# The initial probability remains Beta distributed; sampling its logit avoids
# losing a substantial tail when custom Beta shapes put mass near zero or one.
test_that("initial log odds retain the previous interior starting probabilities", {
  initialize <- getFromNamespace(".asSSM_createMixtureInitialValues", "jaspAcceptanceSampling")
  data <- list(T = 4L, y = c(1L, 5L, 8L, 2L), n = rep(78L, 4), K = 2L)
  expected <- rep(qlogis(c(12, 15, 18, 15) / 236), 2)
  for (predictor in c(FALSE, TRUE)) {
    starts <- initialize(data, 8L, predictor)
    expect_equal(vapply(starts, function(x) x$mu1, numeric(1)), expected)
    expect_false(any(vapply(starts, function(x) "theta1" %in% names(x), logical(1))))
  }
})

test_that("initial-logit targets and gradients match analytic Beta-binomial limits", {
  old <- options(jaspAcceptanceSampling.allowStanFallback = FALSE)
  on.exit(options(old))
  getModel <- getFromNamespace(".asSSM_getStanModel", "jaspAcceptanceSampling")
  softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
  priors <- data.frame(a = c(1, 0.01, 0.1, 2.5, 2.5),
                      b = c(1, 0.01, 2.5, 0.1, 4))
  logits <- c(-1000, -100, -40, -37, -20, 0, 20, 37, 40, 100, 1000)
  for (name in c("simple", "complex")) {
    model <- getModel(name)
    T <- if (name == "simple") 1L else 3L
    for (p in seq_len(nrow(priors))) {
      for (observation in c("none", "interior", "all_defective")) {
        n <- if (observation == "none") 0L else 78L
        y <- switch(observation, none = 0L, interior = 39L, all_defective = 78L)
        dat <- list(T = T, s = array(rep(100L, T), T),
                    n = array(rep(n, T), T), y = array(rep(y, T), T),
                    prior_theta1_a = priors$a[p], prior_theta1_b = priors$b[p],
                    prior_sigma_sd = 1)
        if (name == "complex") {
          dat$K <- 2L
          dat$X <- scale(cbind(c(-1, 0, 1), c(1, -2, 1)))
          dat$prior_beta_scale <- 0.707
        }
        # No posterior sampling: instantiate the density and gradient only.
        fit <- rstan::sampling(model, data = dat, chains = 0, iter = 0,
                               seed = 42, refresh = 0)
        u <- c(0, 0, rep(0, T - 1L), if (name == "complex") c(0, 0))
        anchor <- rstan::log_prob(fit, u, adjust_transform = TRUE)
        a <- dat$prior_theta1_a + sum(dat$y)
        b <- dat$prior_theta1_b + sum(dat$n - dat$y)
        for (value in logits) {
          u[1] <- value
          target <- rstan::log_prob(fit, u, adjust_transform = TRUE)
          gradient <- as.numeric(rstan::grad_log_prob(fit, u, adjust_transform = TRUE))
          expected <- -a * softplus(-value) - b * softplus(value) + (a + b) * log(2)
          expect_equal(as.numeric(target - anchor), expected, tolerance = 1e-9)
          expect_true(all(is.finite(gradient)))
          expect_equal(gradient[1], a - (a + b) * plogis(value), tolerance = 1e-9)
        }
      }
    }
  }
})

test_that("boundary initial probabilities remain outputs without feeding infinities into the model", {
  old <- options(jaspAcceptanceSampling.allowStanFallback = FALSE)
  on.exit(options(old))
  getModel <- getFromNamespace(".asSSM_getStanModel", "jaspAcceptanceSampling")
  for (name in c("simple", "complex")) {
    for (value in c(-1000, -40, 40, 1000)) {
      dat <- list(T = 1L, s = array(100L, 1), n = array(0L, 1), y = array(0L, 1),
                  prior_theta1_a = 0.01, prior_theta1_b = 0.01, prior_sigma_sd = 1)
      initial <- list(mu1 = value, sigma_eta = 1, z_raw = array(numeric(), dim = 0L))
      if (name == "complex") {
        dat$K <- 1L; dat$X <- matrix(0, 1, 1); dat$prior_beta_scale <- 0.707
        initial$beta_predictor <- array(0, dim = 1L)
      }
      fit <- rstan::sampling(getModel(name), data = dat, init = list(initial),
                             chains = 1, iter = 1, warmup = 0,
                             algorithm = "Fixed_param", seed = 42, refresh = 0)
      draws <- rstan::extract(fit)
      expect_equal(as.numeric(draws$mu1), value)
      expect_equal(as.numeric(draws$mu), value)
      expect_equal(as.numeric(draws$theta1), plogis(value))
      expect_equal(as.numeric(draws$theta), plogis(value))
      expect_true(is.finite(draws$y_pred))
      expect_true(draws$y_pred >= 0 && draws$y_pred <= 100)
    }
  }
})

test_that("manual transformed-Beta targets retain strictly positive finite shape requirements", {
  old <- options(jaspAcceptanceSampling.allowStanFallback = FALSE)
  on.exit(options(old))
  getModel <- getFromNamespace(".asSSM_getStanModel", "jaspAcceptanceSampling")
  for (name in c("simple", "complex")) {
    dat <- list(T = 1L, s = array(100L, 1), n = array(0L, 1), y = array(0L, 1),
                prior_theta1_a = 1, prior_theta1_b = 1, prior_sigma_sd = 1)
    if (name == "complex") {
      dat$K <- 1L; dat$X <- matrix(0, 1, 1); dat$prior_beta_scale <- 0.707
    }
    for (field in c("prior_theta1_a", "prior_theta1_b")) {
      for (value in c(0, -0.01, Inf, NaN)) {
        invalid <- dat
        invalid[[field]] <- value
        expect_error({
          fit <- suppressWarnings(rstan::sampling(getModel(name), data = invalid,
                                   chains = 0, iter = 0, seed = 42, refresh = 0))
          rstan::log_prob(fit, c(0, 0, if (name == "complex") 0), adjust_transform = TRUE)
        })
      }
    }
  }
})

test_that("predictor-driven extreme logits have finite binomial targets and correct gradients", {
  old <- options(jaspAcceptanceSampling.allowStanFallback = FALSE)
  on.exit(options(old))
  model <- getFromNamespace(".asSSM_getStanModel", "jaspAcceptanceSampling")("complex")
  softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
  dat <- list(T = 3L, s = array(rep(100L, 3), 3), n = array(c(0L, 78L, 78L), 3),
              y = array(c(0L, 39L, 78L), 3), prior_theta1_a = 0.1,
              prior_theta1_b = 2.5, prior_sigma_sd = 1, prior_beta_scale = 0.707,
              K = 1L, X = matrix(c(-1, 0, 1), 3, 1))
  fit <- rstan::sampling(model, data = dat, chains = 0, iter = 0, seed = 42, refresh = 0)
  mu1 <- qlogis(0.2)
  u <- c(mu1, log(0.5), 0, 0, 0)
  anchor <- rstan::log_prob(fit, u, adjust_transform = TRUE)
  reference <- function(beta) {
    mu <- mu1 + as.numeric(dat$X) * beta
    sum(-dat$y * softplus(-mu) - (dat$n - dat$y) * softplus(mu)) +
      dcauchy(beta, 0, dat$prior_beta_scale, log = TRUE)
  }
  for (beta in c(-1000, -3, -0.4, 0, 0.5, 2, 1000)) {
    u[5] <- beta
    target <- rstan::log_prob(fit, u, adjust_transform = TRUE)
    gradient <- as.numeric(rstan::grad_log_prob(fit, u, adjust_transform = TRUE))
    mu <- mu1 + as.numeric(dat$X) * beta
    expectedBetaGradient <- sum(dat$X[, 1] * (dat$y - dat$n * plogis(mu))) -
      2 * beta / (dat$prior_beta_scale^2 + beta^2)
    expectedInitialGradient <- dat$prior_theta1_a * plogis(-mu1) -
      dat$prior_theta1_b * plogis(mu1) + sum(dat$y - dat$n * plogis(mu))
    expect_equal(as.numeric(target - anchor), reference(beta) - reference(0), tolerance = 1e-9)
    expect_true(all(is.finite(gradient)))
    expect_equal(tail(gradient, 1), expectedBetaGradient, tolerance = 1e-9)
    expect_equal(gradient[1], expectedInitialGradient, tolerance = 1e-9)
  }
})
