# Portable model checks; no jaspTools runner or research-repository files needed.
.ssm_test_data <- function(T, predictor = FALSE, K = 1L) {
  dat <- list(
    T = as.integer(T),
    s = array(rep(1000L, T), dim = T),
    n = array(rep(78L, T), dim = T),
    y = array(rep(c(1L, 5L, 8L, 2L), length.out = T), dim = T),
    prior_theta1_a = 1, prior_theta1_b = 1, prior_sigma_sd = 1
  )
  if (predictor) {
    dat$K <- as.integer(K)
    dat$X <- if (T == 1) matrix(0, nrow = T, ncol = K) else
      scale(outer(seq_len(T), seq_len(K), "^"))
    dat$prior_beta_scale <- 0.707
  }
  dat
}

.ssm_test_model <- function(name) {
  old <- options(jaspAcceptanceSampling.allowStanFallback = FALSE)
  on.exit(options(old))
  getFromNamespace(".asSSM_getStanModel", "jaspAcceptanceSampling")(name)
}

# Solve the mixture's survival function directly in R, independently of C++.
.ssm_test_transport <- function(z, sigma) {
  broad_sd <- sqrt(sigma^2 + 1.5^2)
  vapply(z, function(value) {
    if (value == 0) return(0)
    log_tail <- pnorm(abs(value), lower.tail = FALSE, log.p = TRUE)
    root <- uniroot(function(eta) {
      tail <- 0.95 * pnorm(eta / sigma, lower.tail = FALSE) +
              0.05 * pnorm(eta / broad_sd, lower.tail = FALSE)
      log(tail) - log_tail
    }, interval = c(0, max(1, abs(value) * broad_sd)), tol = 1e-12)$root
    sign(value) * root
  }, numeric(1))
}

.ssm_test_target <- function(parameters, dat) {
  eta <- .ssm_test_transport(parameters$z_raw, parameters$sigma_eta)
  theta1 <- plogis(parameters$mu1)
  mu <- parameters$mu1 + c(0, cumsum(eta))
  target <- dbeta(theta1, dat$prior_theta1_a, dat$prior_theta1_b, log = TRUE) +
            dnorm(parameters$sigma_eta, 0, dat$prior_sigma_sd, log = TRUE) +
            sum(dnorm(parameters$z_raw, log = TRUE))
  if (!is.null(parameters$beta_predictor)) {
    mu <- mu + as.numeric(dat$X %*% parameters$beta_predictor)
    target <- target + sum(dcauchy(parameters$beta_predictor, 0, dat$prior_beta_scale, log = TRUE))
  }
  # Independent probability-scale Beta calculation plus the mu1 and sigma Jacobians.
  target + sum(dbinom(dat$y, dat$n, plogis(mu), log = TRUE)) +
    log(theta1) + log1p(-theta1) + log(parameters$sigma_eta)
}

test_that("mixture initialization covers four scales and the singleton case", {
  initialize <- getFromNamespace(".asSSM_mixtureInit", "jaspAcceptanceSampling")
  for (predictor in c(FALSE, TRUE)) {
    dat <- .ssm_test_data(4, predictor)
    inits <- initialize(dat, chains = 8, predictor = predictor)
    expect_equal(vapply(inits, function(x) x$sigma_eta, numeric(1)),
                 rep(c(0.08, 0.2, 0.5, 1), 2))
    expect_equal(inits[1:4], inits[5:8])
    for (initial in inits) {
      expect_true(is.finite(initial$mu1))
      expect_true(plogis(initial$mu1) > 0 && plogis(initial$mu1) < 1)
      expect_equal(dim(initial$z_raw), 3L)
      expect_true(all(is.finite(initial$z_raw)))
      if (predictor) expect_identical(initial$beta_predictor, array(0, dim = 1L))
      else expect_false("beta_predictor" %in% names(initial))
    }
    singleton <- initialize(.ssm_test_data(1, predictor), chains = 4, predictor = predictor)
    for (initial in singleton) {
      expect_length(initial$z_raw, 0)
      expect_equal(dim(initial$z_raw), 0L)
    }
  }
})

test_that("each numeric coefficient starts at zero without dropping vector dimensions", {
  initialize <- getFromNamespace(".asSSM_mixtureInit", "jaspAcceptanceSampling")
  for (K in c(1L, 2L, 3L)) {
    for (T in c(1L, 8L)) {
      dat <- .ssm_test_data(T, predictor = TRUE, K = K)
      initial <- initialize(dat, chains = 4, predictor = TRUE)
      for (chain in initial) {
        expect_identical(chain$beta_predictor, array(rep(0, K), dim = K))
        expect_length(chain$z_raw, T - 1L)
      }
    }
  }
})

test_that("both precompiled models match independent mixture transport and outputs", {
  z <- c(-8, -3, -0.5, 0, 0.5, 3, 8)
  for (name in c("simple", "complex")) {
    model <- .ssm_test_model(name)
    expect_s4_class(model, "stanmodel")
    dat <- .ssm_test_data(length(z) + 1L, predictor = name == "complex")
    for (sigma in c(0.02, 0.2, 1.2)) {
      initial <- list(mu1 = qlogis(0.13), sigma_eta = sigma, z_raw = array(z, dim = length(z)))
      if (name == "complex") initial$beta_predictor <- array(0.4, dim = 1L)
      fit <- rstan::sampling(model, data = dat, init = list(initial),
                             chains = 1, iter = 1, warmup = 0,
                             algorithm = "Fixed_param", seed = 42, refresh = 0)
      draws <- rstan::extract(fit)
      eta <- .ssm_test_transport(z, sigma)
      nu <- initial$mu1 + c(0, cumsum(eta))
      mu <- if (name == "complex") nu + as.numeric(dat$X %*% initial$beta_predictor) else nu
      expect_equal(as.numeric(draws$eta), eta, tolerance = 1e-8)
      expect_equal(as.numeric(draws$mu), as.numeric(mu), tolerance = 1e-8)
      expect_equal(as.numeric(draws$theta), as.numeric(plogis(mu)), tolerance = 1e-8)
      if (name == "complex") expect_equal(as.numeric(draws$nu), nu, tolerance = 1e-8)

      broad <- 0.05 * dnorm(eta, 0, sqrt(sigma^2 + 1.5^2))
      regular <- 0.95 * dnorm(eta, 0, sigma)
      expect_equal(as.numeric(draws$jump_responsibility), broad / (broad + regular), tolerance = 1e-8)
      expect_true(draws$y_pred >= tail(dat$y, 1))
      expect_true(draws$y_pred <= tail(dat$y + dat$s - dat$n, 1))
    }
  }
})

test_that("both models handle one lot and predict observed defects when sampling is complete", {
  initialize <- getFromNamespace(".asSSM_mixtureInit", "jaspAcceptanceSampling")
  for (name in c("simple", "complex")) {
    dat <- .ssm_test_data(1, predictor = name == "complex")
    dat$s <- dat$n
    initial <- initialize(dat, chains = 1, predictor = name == "complex")
    fit <- rstan::sampling(.ssm_test_model(name), data = dat, init = initial,
                           chains = 1, iter = 1, warmup = 0,
                           algorithm = "Fixed_param", seed = 42, refresh = 0)
    draws <- rstan::extract(fit)
    expect_equal(as.numeric(draws$theta), plogis(initial[[1]]$mu1), tolerance = 1e-10)
    expect_equal(as.numeric(draws$y_pred), as.numeric(dat$y))
    # RStan may omit zero-length outputs from extract(); either representation is empty.
    expect_length(draws$eta, 0)
    expect_length(draws$jump_responsibility, 0)
  }
})

test_that("the transported target has the requested priors and no extra mixture density", {
  for (name in c("simple", "complex")) {
    for (custom_priors in c(FALSE, TRUE)) {
      dat <- .ssm_test_data(4, predictor = name == "complex")
      if (custom_priors) {
        dat$prior_theta1_a <- 2.5
        dat$prior_theta1_b <- 4
        dat$prior_sigma_sd <- 0.7
        if (name == "complex") dat$prior_beta_scale <- 1.3
      }
      # Instantiate the model for log_prob access without drawing a chain.
      fit <- rstan::sampling(.ssm_test_model(name), data = dat,
                             chains = 0, iter = 0, seed = 42, refresh = 0)
      reference <- list(mu1 = qlogis(0.08), sigma_eta = 0.3, z_raw = c(-1.2, 0.25, 2.8))
      if (name == "complex") reference$beta_predictor <- array(0.5, dim = 1L)
      candidates <- list(reference, reference, reference)
      candidates[[1]]$mu1 <- qlogis(0.27)
      candidates[[2]]$sigma_eta <- 0.8
      candidates[[3]]$z_raw <- c(-0.8, -2, 1.4)
      if (name == "complex") {
        candidates[[4]] <- reference
        candidates[[4]]$beta_predictor <- array(-1.1, dim = 1L)
      }
      stan_reference <- rstan::log_prob(fit, rstan::unconstrain_pars(fit, reference),
                                        adjust_transform = TRUE)
      for (candidate in candidates) {
        stan_difference <- rstan::log_prob(fit, rstan::unconstrain_pars(fit, candidate),
                                           adjust_transform = TRUE) - stan_reference
        r_difference <- .ssm_test_target(candidate, dat) - .ssm_test_target(reference, dat)
        expect_equal(as.numeric(stan_difference), as.numeric(r_difference), tolerance = 1e-8)
      }
    }
  }
})

test_that("one matrix column retains the previous scalar-predictor mathematical target", {
  dat <- .ssm_test_data(5, predictor = TRUE)
  fit <- rstan::sampling(.ssm_test_model("complex"), data = dat,
                         chains = 0, iter = 0, seed = 42, refresh = 0)
  parameters <- list(mu1 = qlogis(0.13), sigma_eta = 0.25,
                     z_raw = c(-1.4, 0.3, 1.7, -0.6))
  eta <- .ssm_test_transport(parameters$z_raw, parameters$sigma_eta)
  nu <- parameters$mu1 + c(0, cumsum(eta))
  predictor <- as.numeric(scale(seq_len(dat$T)))
  beta <- c(-1.1, 0, 0.5, 1.7)
  # All terms unrelated to the scalar slope cancel in these differences.
  old_target <- vapply(beta, function(value) {
    sum(dbinom(dat$y, dat$n, plogis(nu + value * predictor), log = TRUE)) +
      dcauchy(value, 0, dat$prior_beta_scale, log = TRUE)
  }, numeric(1))
  matrix_target <- vapply(beta, function(value) {
    parameters$beta_predictor <- array(value, dim = 1L)
    rstan::log_prob(fit, rstan::unconstrain_pars(fit, parameters), adjust_transform = TRUE)
  }, numeric(1))
  expect_equal(matrix_target - matrix_target[1], old_target - old_target[1], tolerance = 1e-8)
})

test_that("multiple numeric coefficients match independent targets and gradients", {
  for (K in c(2L, 3L)) {
    for (prior_scale in c(0.707, 1.3)) {
      dat <- .ssm_test_data(8, predictor = TRUE, K = K)
      dat$prior_theta1_a <- 2.5
      dat$prior_theta1_b <- 4
      dat$prior_sigma_sd <- 0.7
      dat$prior_beta_scale <- prior_scale
      fit <- rstan::sampling(.ssm_test_model("complex"), data = dat,
                             chains = 0, iter = 0, seed = 42, refresh = 0)
      reference <- list(mu1 = qlogis(0.13), sigma_eta = 0.3,
                         z_raw = c(-1.2, 0.25, 2.8, -0.4, 1.1, 0.5, -1.5),
                         beta_predictor = array(c(0.5, -0.3, 0.15)[seq_len(K)], dim = K))
      unconstrained <- rstan::unconstrain_pars(fit, reference)
      stan_reference <- rstan::log_prob(fit, unconstrained, adjust_transform = TRUE)
      for (j in seq_len(K)) {
        candidate <- reference
        candidate$beta_predictor[j] <- reference$beta_predictor[j] + 0.4 * j
        stan_difference <- rstan::log_prob(fit, rstan::unconstrain_pars(fit, candidate),
                                           adjust_transform = TRUE) - stan_reference
        r_difference <- .ssm_test_target(candidate, dat) - .ssm_test_target(reference, dat)
        expect_equal(as.numeric(stan_difference), as.numeric(r_difference), tolerance = 1e-8)
      }

      stan_gradient <- as.numeric(rstan::grad_log_prob(fit, unconstrained, adjust_transform = TRUE))
      eta <- .ssm_test_transport(reference$z_raw, reference$sigma_eta)
      nu <- reference$mu1 + c(0, cumsum(eta))
      theta <- plogis(nu + as.numeric(dat$X %*% reference$beta_predictor))
      beta <- as.numeric(reference$beta_predictor)
      beta_gradient <- as.numeric(crossprod(dat$X, dat$y - dat$n * theta)) -
        2 * beta / (prior_scale^2 + beta^2)
      expect_equal(tail(stan_gradient, K), beta_gradient, tolerance = 1e-8)

      independent_target <- function(u) {
        parameters <- list(
          mu1 = u[1], sigma_eta = exp(u[2]),
          z_raw = u[2L + seq_len(dat$T - 1L)],
          beta_predictor = tail(u, K)
        )
        .ssm_test_target(parameters, dat)
      }
      step <- 1e-5
      finite_difference <- vapply(seq_along(unconstrained), function(j) {
        upper <- lower <- unconstrained
        upper[j] <- upper[j] + step
        lower[j] <- lower[j] - step
        (independent_target(upper) - independent_target(lower)) / (2 * step)
      }, numeric(1))
      expect_equal(stan_gradient, finite_difference, tolerance = 2e-5)
    }
  }
})

test_that("permuting numeric columns and coefficients preserves the target and mapped gradients", {
  dat <- .ssm_test_data(8, predictor = TRUE, K = 3L)
  parameters <- list(mu1 = qlogis(0.13), sigma_eta = 0.3,
                     z_raw = c(-1.2, 0.25, 2.8, -0.4, 1.1, 0.5, -1.5),
                     beta_predictor = array(c(0.5, -0.3, 0.15), dim = 3L))
  fit <- rstan::sampling(.ssm_test_model("complex"), data = dat,
                         chains = 0, iter = 0, seed = 42, refresh = 0)
  unconstrained <- rstan::unconstrain_pars(fit, parameters)
  target <- rstan::log_prob(fit, unconstrained, adjust_transform = TRUE)
  gradient <- as.numeric(rstan::grad_log_prob(fit, unconstrained, adjust_transform = TRUE))

  permutation <- c(3L, 1L, 2L)
  dat$X <- dat$X[, permutation, drop = FALSE]
  parameters$beta_predictor <- array(parameters$beta_predictor[permutation], dim = dat$K)
  permuted_fit <- rstan::sampling(.ssm_test_model("complex"), data = dat,
                                  chains = 0, iter = 0, seed = 42, refresh = 0)
  permuted_u <- rstan::unconstrain_pars(permuted_fit, parameters)
  expect_equal(as.numeric(rstan::log_prob(permuted_fit, permuted_u, adjust_transform = TRUE)),
               as.numeric(target), tolerance = 1e-10)
  permuted_gradient <- as.numeric(rstan::grad_log_prob(permuted_fit, permuted_u, adjust_transform = TRUE))
  expected_gradient <- c(head(gradient, -dat$K), tail(gradient, dat$K)[permutation])
  expect_equal(permuted_gradient, expected_gradient, tolerance = 1e-10)
})

test_that("finite-lot predictions respect zero, partial and complete inspection", {
  initialize <- getFromNamespace(".asSSM_mixtureInit", "jaspAcceptanceSampling")
  predictive_pmf <- getFromNamespace(".asSSM_predictivePMF", "jaspAcceptanceSampling")
  cases <- data.frame(T = c(1L, 3L, 3L), n = c(0L, 12L, 20L), y = c(0L, 4L, 7L))
  configurations <- data.frame(name = c("simple", "complex", "complex", "complex"),
                                K = c(0L, 1L, 2L, 3L))
  for (configuration in seq_len(nrow(configurations))) {
    name <- configurations$name[configuration]
    K <- configurations$K[configuration]
    for (i in seq_len(nrow(cases))) {
      T <- cases$T[i]
      dat <- .ssm_test_data(T, predictor = name == "complex", K = K)
      dat$s[T] <- 20L
      dat$n[T] <- cases$n[i]
      dat$y[T] <- cases$y[i]
      initial <- initialize(dat, chains = 1, predictor = name == "complex")
      initial[[1]]$mu1 <- qlogis(0.35)
      initial[[1]]$z_raw <- array(rep(0, T - 1L), dim = T - 1L)
      if (name == "complex")
        initial[[1]]$beta_predictor <- array(c(0.2, -0.1, 0.3)[seq_len(K)], dim = K)
      fit <- rstan::sampling(.ssm_test_model(name), data = dat, init = initial,
                             chains = 1, iter = 256, warmup = 0,
                             algorithm = "Fixed_param", seed = 42, refresh = 0)
      predicted <- as.numeric(rstan::extract(fit, pars = "y_pred")$y_pred)
      lower <- dat$y[T]
      remaining <- dat$s[T] - dat$n[T]
      upper <- lower + remaining
      expect_true(all(predicted >= lower & predicted <= upper))
      expect_true(all(predicted == floor(predicted)))

      pmf <- predictive_pmf(predicted, dat$s[T], dat$n[T], dat$y[T])
      expect_equal(sum(pmf$prob), 1)
      expect_true(all(pmf$prob >= 0))
      expect_equal(sum(pmf$prob[pmf$defects < lower | pmf$defects > upper]), 0)
      if (remaining == 0) {
        expect_true(all(predicted == lower))
        expect_equal(pmf$prob[pmf$defects == lower], 1)
      } else {
        theta <- if (name == "complex")
          plogis(qlogis(0.35) + sum(dat$X[T, ] * initial[[1]]$beta_predictor)) else 0.35
        # Independent exact Binomial reference for the sum of fixed-parameter draws.
        bounds <- qbinom(c(1e-6, 1 - 1e-6), length(predicted) * remaining, theta)
        expect_true(sum(predicted - lower) >= bounds[1])
        expect_true(sum(predicted - lower) <= bounds[2])
      }
    }
  }
})

test_that("empirical predictive weights do not assign mass below observed defects", {
  predictive_pmf <- getFromNamespace(".asSSM_predictivePMF", "jaspAcceptanceSampling")
  pmf <- predictive_pmf(c(4, 5, 5, 6), sT = 1000, nT = 20, yT = 4)
  expect_equal(pmf$defects, 4:6)
  expect_equal(pmf$prob[pmf$defects %in% 4:6], c(0.25, 0.5, 0.25))
  expect_equal(sum(pmf$prob), 1)
  expect_equal(sum(pmf$prob[pmf$defects < 4]), 0)
  expect_equal(sum(pmf$prob[pmf$defects > 6]), 0)
})

test_that("AQL and RQL masses use strict inequalities at integer and fractional limits", {
  predictive_pmf <- getFromNamespace(".asSSM_predictivePMF", "jaspAcceptanceSampling")
  mass_below <- getFromNamespace(".asSSM_pmfMassBelow", "jaspAcceptanceSampling")
  mass_above <- getFromNamespace(".asSSM_pmfMassAbove", "jaspAcceptanceSampling")
  pmf <- predictive_pmf(c(4, 5, 5, 6), sT = 1000, nT = 20, yT = 4)
  thresholds <- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5)
  expect_equal(vapply(thresholds, function(x) mass_below(pmf, x), numeric(1)),
               c(0, 0, 0.25, 0.25, 0.75, 0.75, 1))
  expect_equal(vapply(thresholds, function(x) mass_above(pmf, x), numeric(1)),
               c(1, 0.75, 0.75, 0.25, 0.25, 0, 0))
})
