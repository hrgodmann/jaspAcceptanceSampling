.ssmDiagnosticApi <- function() {
  list(
    compute = getFromNamespace(".asSSM_computeDiagnostics", "jaspAcceptanceSampling"),
    messages = getFromNamespace(".asSSM_createDiagnosticMessages", "jaspAcceptanceSampling"),
    html = getFromNamespace(".asSSM_createDiagnosticHtml", "jaspAcceptanceSampling")
  )
}

.ssmDiagnosticFixture <- function(n = 2000L, chains = 4L, betaNames = "beta_predictor") {
  set.seed(42)
  variables <- c("mu1", "sigma_eta", "z_raw[1]", "z_raw[2]", betaNames,
                 "theta[1]", "y_pred", "jump_responsibility[1]")
  draws <- array(rnorm(n * chains * length(variables)),
                 dim = c(n, chains, length(variables)),
                 dimnames = list(NULL, paste0("chain:", seq_len(chains)), variables))
  draws[, , "sigma_eta"] <- exp(draws[, , "sigma_eta"])
  # Constant generated quantities must not become sampled-parameter warnings.
  draws[, , "theta[1]"] <- 0
  draws[, , "y_pred"] <- 4
  draws[, , "jump_responsibility[1]"] <- 1
  sampler <- lapply(seq_len(chains), function(j)
    cbind(divergent__ = rep(0, n), treedepth__ = rep(3, n), energy__ = 10 + rnorm(n)))
  list(draws = draws, sampler = sampler)
}

test_that("healthy ordered draws match RStan diagnostics without checking generated quantities", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  expect_identical(d$parameters$parameter,
                   c("mu1", "sigma_eta", "z_raw[1]", "z_raw[2]", "beta_predictor"))
  for (i in seq_len(nrow(d$parameters))) {
    x <- input$draws[, , d$parameters$parameter[i]]
    expect_equal(d$parameters$rhat[i], rstan::Rhat(x), tolerance = 1e-12)
    expect_equal(d$parameters$ess_bulk[i], rstan::ess_bulk(x), tolerance = 1e-12)
    expect_equal(d$parameters$ess_tail[i], rstan::ess_tail(x), tolerance = 1e-12)
  }
  expect_true(all(d$parameters$rhat < 1.01))
  expect_true(all(d$parameters$ess_bulk >= 400 & d$parameters$ess_tail >= 400))
  expect_true(all(d$sampler$bfmi >= 0.3))
  expect_length(api$messages(d), 0)
  expect_match(api$html(d), "do not prove convergence")
})

test_that("divergence and treedepth checks count retained transitions without removing draws", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$sampler[[1]][c(3, 11), "divergent__"] <- 1
  input$sampler[[3]][7, "divergent__"] <- 1
  input$sampler[[2]][c(4, 8), "treedepth__"] <- c(10, 11)
  input$sampler[[4]][2, "treedepth__"] <- 9
  drawsBefore <- input$draws
  samplerBefore <- input$sampler
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  expect_equal(d$sampler$divergences, c(2, 0, 1, 0))
  expect_equal(d$sampler$treedepth_hits, c(0, 2, 0, 0))
  expect_equal(d$sampler$draws, rep(2000, 4))
  expect_identical(input$draws, drawsBefore)
  expect_identical(input$sampler, samplerBefore)
  expect_identical(d$sampler_params, samplerBefore)
  messages <- paste(api$messages(d), collapse = " ")
  expect_match(messages, "3 divergent transitions")
  expect_match(messages, "treedepth.*2 retained")
  expect_match(messages, "No divergent draws were removed")
  expect_match(api$html(d), "including those associated with divergences")
})

test_that("separated chains trigger mixing and effective-sample-size warnings", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$draws[, , "beta_predictor"] <- sweep(input$draws[, , "beta_predictor"] * 0.1,
                                             2, c(-8, -4, 4, 8), "+")
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  beta <- d$parameters[d$parameters$parameter == "beta_predictor", ]
  expect_gt(beta$rhat, 1.01)
  expect_lt(beta$ess_bulk, 400)
  messages <- paste(api$messages(d), collapse = " ")
  expect_match(messages, "R-hat.*beta_predictor")
  expect_match(messages, "effective sample size is below 400")
  expect_match(messages, "Inspect|investigate")
})

test_that("indexed coefficients include both singleton and multivariate predictor fits", {
  api <- .ssmDiagnosticApi()
  for (betaNames in list("beta_predictor[1]", paste0("beta_predictor[", 1:3, "]"))) {
    input <- .ssmDiagnosticFixture(betaNames = betaNames)
    d <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
    expect_identical(d$parameters$parameter,
                     c("mu1", "sigma_eta", "z_raw[1]", "z_raw[2]", betaNames))
    for (name in betaNames) {
      parameter <- d$parameters[d$parameters$parameter == name, ]
      x <- input$draws[, , name]
      expect_equal(parameter$rhat, rstan::Rhat(x), tolerance = 1e-12)
      expect_equal(parameter$ess_bulk, rstan::ess_bulk(x), tolerance = 1e-12)
      expect_equal(parameter$ess_tail, rstan::ess_tail(x), tolerance = 1e-12)
    }
    expect_length(api$messages(d), 0L)
  }
})


test_that("E-BFMI uses consecutive energies separately within each chain", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture(n = 8L, chains = 2L)
  input$sampler[[1]][, "energy__"] <- 1:8
  input$sampler[[2]][, "energy__"] <- c(1, 8, 2, 7, 3, 6, 4, 5)
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  # Both sequences have sample variance 6; mean squared changes are 1 and 20.
  expect_equal(d$sampler$bfmi, c(1 / 6, 10 / 3), tolerance = 1e-12)
  expect_match(paste(api$messages(d), collapse = " "), "E-BFMI is below 0.3 in chain\\(s\\) 1")
  input$sampler[[1]][, "energy__"] <- 100 + 3 * (1:8)
  shifted <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  expect_equal(shifted$sampler$bfmi, d$sampler$bfmi, tolerance = 1e-12)
})

test_that("constant and nonfinite sampled parameters are unavailable rather than reassuring", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$draws[, 1, "z_raw[1]"] <- 0
  input$draws[7, 2, "beta_predictor"] <- Inf
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  z <- d$parameters[d$parameters$parameter == "z_raw[1]", ]
  beta <- d$parameters[d$parameters$parameter == "beta_predictor", ]
  expect_true(z$constant_chain)
  expect_true(beta$nonfinite_draws)
  expect_true(all(is.na(z[c("rhat", "ess_bulk", "ess_tail")])))
  expect_true(all(is.na(beta[c("rhat", "ess_bulk", "ess_tail")])))
  expect_true(all(is.finite(d$parameters$rhat[d$parameters$parameter == "mu1"])))
  messages <- paste(api$messages(d), collapse = " ")
  expect_match(messages, "constant.*z_raw\\[1\\]")
  expect_match(messages, "Non-finite draws")
  expect_match(api$html(d), "Unavailable")
  expect_false(grepl("No diagnostic thresholds were exceeded", api$html(d)))
})

test_that("missing, constant and nonfinite sampler information cannot pass its checks", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$sampler[[1]] <- input$sampler[[1]][, c("divergent__", "treedepth__"), drop = FALSE]
  input$sampler[[2]][, "energy__"] <- 7
  input$sampler[[3]][2, "energy__"] <- NA_real_
  input$sampler[[3]][3, "divergent__"] <- NA_real_
  input$sampler[[4]] <- input$sampler[[4]][-1, , drop = FALSE]
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  expect_true(all(is.na(d$sampler$bfmi)))
  expect_equal(d$sampler$divergences[1:2], c(0, 0))
  expect_true(all(is.na(d$sampler$divergences[3:4])))
  expect_true(is.na(d$sampler$draws[4]))
  messages <- paste(api$messages(d), collapse = " ")
  expect_match(messages, "Sampler diagnostics are unavailable")
  expect_match(messages, "E-BFMI is unavailable")
  expect_match(messages, "refit|Refit")

  missingChain <- api$compute(input$draws, input$sampler[1], maxTreedepth = 10L)
  expect_true(all(is.na(missingChain$sampler$draws[2:4])))
})

test_that("one chain, insufficient draws and missing requested chains are reported", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture(chains = 1L)
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  expect_true(all(is.na(d$parameters$rhat)))
  expect_true(all(is.finite(d$parameters$ess_bulk)))
  expect_match(paste(api$messages(d), collapse = " "), "Only one chain.*multiple chains")

  for (n in c(5L, 6L)) {
    input <- .ssmDiagnosticFixture(n = n)
    d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
    metrics <- unlist(d$parameters[c("rhat", "ess_bulk", "ess_tail")], use.names = FALSE)
    if (n == 5L) {
      expect_true(all(is.na(metrics)))
      expect_true(all(is.na(d$sampler$bfmi)))
      expect_match(paste(api$messages(d), collapse = " "), "Fewer than six")
    } else {
      expect_true(all(is.finite(metrics)))
      expect_true(all(is.finite(d$sampler$bfmi)))
      expect_false(any(grepl("Fewer than six", api$messages(d))))
    }
  }
  input <- .ssmDiagnosticFixture(chains = 2L)
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L, expectedChains = 4L)
  expect_equal(d$chains, 2)
  expect_equal(d$expected_chains, 4)
  expect_match(paste(api$messages(d), collapse = " "), "Only 2 of 4 requested chains")
})

test_that("thinning preserves retained checks but disables consecutive-energy E-BFMI", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$sampler[[1]][2, "divergent__"] <- 1
  input$sampler[[2]][4, "treedepth__"] <- 10
  full <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  thinned <- api$compute(input$draws, input$sampler, maxTreedepth = 10L, thin = 3L)
  expect_identical(thinned$parameters, full$parameters)
  expect_identical(thinned$sampler$divergences, full$sampler$divergences)
  expect_identical(thinned$sampler$treedepth_hits, full$sampler$treedepth_hits)
  expect_true(all(is.na(thinned$sampler$bfmi)))
  expect_equal(thinned$thin, 3L)
  messages <- paste(api$messages(thinned), collapse = " ")
  expect_match(messages, "counts cover only retained transitions")
  expect_match(messages, "Thinning = 1")
})

test_that("diagnostic warning thresholds have explicit inclusive and strict boundaries", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 10L)
  d$parameters$rhat <- 1
  d$parameters$ess_bulk <- d$parameters$ess_tail <- 400
  d$sampler$bfmi <- 0.3
  expect_length(api$messages(d), 0)
  boundary <- d
  boundary$parameters$rhat[1] <- 1.01
  expect_true(any(grepl("R-hat", api$messages(boundary))))
  boundary <- d
  boundary$parameters$ess_tail[1] <- 399.9
  expect_true(any(grepl("effective sample size is below", api$messages(boundary))))
  boundary <- d
  boundary$sampler$bfmi[1] <- 0.299
  expect_true(any(grepl("E-BFMI is below", api$messages(boundary))))
})

test_that("unavailable cached diagnostics and malformed inputs do not imply successful checks", {
  api <- .ssmDiagnosticApi()
  for (unavailable in list(NULL, list(error = "diagnostic extraction failed"))) {
    messages <- paste(api$messages(unavailable), collapse = " ")
    expect_match(messages, "unavailable|could not be calculated")
    expect_match(messages, "Refit|refit")
    expect_false(grepl("No diagnostic thresholds were exceeded", api$html(unavailable)))
  }
  input <- .ssmDiagnosticFixture(n = 6L)
  expect_error(api$compute(matrix(1, 6, 4), input$sampler, 10L), "iterations by chains")
  generated <- input$draws[, , c("theta[1]", "y_pred"), drop = FALSE]
  expect_error(api$compute(generated, input$sampler, 10L), "No sampled parameters")
})


.ssmReportFixture <- function(diagnostics) {
  report <- getFromNamespace(".asSSM_reportDiagnostics", "jaspAcceptanceSampling")
  environment(report) <- list2env(list(createJaspHtml = function(...) list(...)),
                                  parent = environment(report))
  results <- new.env(parent = emptyenv())
  results[["fit"]] <- list(object = list(diagnostics = diagnostics))
  list(report = report, results = results)
}

test_that("a failure confined to a later coefficient produces a visible warning", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture(betaNames = paste0("beta_predictor[", 1:3, "]"))
  input$draws[, , "beta_predictor[3]"] <- sweep(input$draws[, , "beta_predictor[3]"] * 0.1,
                                               2, c(-8, -4, 4, 8), "+")
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
  later <- d$parameters[d$parameters$parameter == "beta_predictor[3]", ]
  earlier <- d$parameters[d$parameters$parameter %in% c("beta_predictor[1]", "beta_predictor[2]"), ]
  expect_gt(later$rhat, 1.01)
  expect_lt(later$ess_bulk, 400)
  expect_true(all(earlier$rhat < 1.01))
  expect_match(paste(api$messages(d), collapse = " "), "R-hat.*beta_predictor\\[3\\]")

  fixture <- .ssmReportFixture(d)
  fixture$report(fixture$results, list(mcmcDiagnosticReport = FALSE))
  warning <- fixture$results[["samplerDiagnostics"]]
  expect_identical(warning$title, "Sampling Warnings")
  expect_match(warning$text, "beta_predictor\\[3\\]")
  expect_match(warning$text, "effective sample size is below 400")
})

test_that("healthy diagnostics are optional and switching them off clears the report", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
  fixture <- .ssmReportFixture(d)
  cachedFit <- fixture$results[["fit"]]

  fixture$report(fixture$results, list(mcmcDiagnosticReport = FALSE))
  expect_null(fixture$results[["samplerDiagnostics"]])
  fixture$report(fixture$results, list(mcmcDiagnosticReport = TRUE))
  full <- fixture$results[["samplerDiagnostics"]]
  expect_identical(full$title, "Computational Diagnostics")
  expect_match(full$text, "No diagnostic thresholds were exceeded")
  expect_match(full$text, "Retained post-warmup draws:")
  expect_match(full$text, "maximum rank-normalized split R-hat")
  expect_match(full$text, "E-BFMI")

  fixture$report(fixture$results, list(mcmcDiagnosticReport = FALSE))
  expect_null(fixture$results[["samplerDiagnostics"]])
  expect_identical(fixture$results[["fit"]], cachedFit)
})

test_that("problematic and unavailable diagnostics warn even when printing is off", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$sampler[[1]][2, "divergent__"] <- 1
  bad <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
  single <- .ssmDiagnosticFixture(chains = 1L)
  oneChain <- api$compute(single$draws, single$sampler, maxTreedepth = 15L)
  cases <- list(bad, oneChain, NULL, list(error = "diagnostic extraction failed"))
  patterns <- c("1 divergent transitions", "Only one chain", "unavailable", "could not be calculated")

  for (i in seq_along(cases)) {
    fixture <- .ssmReportFixture(cases[[i]])
    fixture$report(fixture$results, list(mcmcDiagnosticReport = FALSE))
    warning <- fixture$results[["samplerDiagnostics"]]
    expect_identical(warning$title, "Sampling Warnings")
    expect_match(warning$text, "Sampling requires attention")
    expect_match(warning$text, patterns[i])
    expect_false(grepl("Retained post-warmup draws:", warning$text, fixed = TRUE))
    expect_false(grepl("Across all sampled parameters:", warning$text, fixed = TRUE))
    expect_false(grepl("No diagnostic thresholds were exceeded", warning$text, fixed = TRUE))
  }

  fixture <- .ssmReportFixture(bad)
  fixture$report(fixture$results, list(mcmcDiagnosticReport = TRUE))
  full <- fixture$results[["samplerDiagnostics"]]
  expect_identical(full$title, "Computational Diagnostics")
  expect_match(full$text, "1 divergent transitions")
  expect_match(full$text, "Retained post-warmup draws:")
  expect_match(api$html(bad, details = FALSE), "No divergent draws were removed")
  expect_false(grepl("Divergences:", api$html(bad, details = FALSE), fixed = TRUE))
})

test_that("warnings clear after a healthy fit and stale reports clear without a fit", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  healthy <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
  input$sampler[[1]][2, "divergent__"] <- 1
  bad <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
  fixture <- .ssmReportFixture(bad)
  fixture$report(fixture$results, list(mcmcDiagnosticReport = FALSE))
  expect_false(is.null(fixture$results[["samplerDiagnostics"]]))

  fixture$results[["fit"]] <- list(object = list(diagnostics = healthy))
  fixture$report(fixture$results, list(mcmcDiagnosticReport = FALSE))
  expect_null(fixture$results[["samplerDiagnostics"]])

  fixture$report(fixture$results, list(mcmcDiagnosticReport = TRUE))
  expect_false(is.null(fixture$results[["samplerDiagnostics"]]))
  fixture$results[["fit"]] <- NULL
  fixture$report(fixture$results, list(mcmcDiagnosticReport = TRUE))
  expect_null(fixture$results[["samplerDiagnostics"]])
})

test_that("rounded initial probabilities do not mask mixing of their sampled log odds", {
  api <- .ssmDiagnosticApi()
  input <- .ssmDiagnosticFixture()
  input$draws[, , "mu1"] <- input$draws[, , "mu1"] + 1000
  # Probability-scale output rounds to one, while sampled log odds still vary.
  expect_true(all(plogis(input$draws[, , "mu1"]) == 1))
  d <- api$compute(input$draws, input$sampler, maxTreedepth = 15L)
  first <- d$parameters[d$parameters$parameter == "mu1", ]
  expect_false(first$constant_chain)
  expect_true(is.finite(first$rhat))
  expect_true(is.finite(first$ess_bulk))
  expect_true(is.finite(first$ess_tail))
})
