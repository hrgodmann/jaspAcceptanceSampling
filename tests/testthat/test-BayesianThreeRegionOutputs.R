# Equal-thirds calibration must survive the full planning/inference output path.

.bsThreeOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$priorplan <- "impartial_three"
  options$aqlplan <- .05
  options$rqlplan <- .15
  options$impartialThreeConstraintplan <- "no_U"
  options$max_nplan <- 1
  options$min_bfplan <- 1
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  options$priorPlotplan <- TRUE
  options$inferPosteriorinfer <- TRUE
  options$showInferenceTableinfer <- TRUE
  options$posteriorPlotinfer <- FALSE
  options$ppdPlotinfer <- FALSE
  options
}

.bsThreeCollectionForTest <- function(result, key) result$results[[key]]$collection

.bsThreeQuadratureForTest <- function(alpha, beta, aql, rql) {
  limits <- c(0, aql, rql, 1)
  vapply(seq_len(3L), function(i) {
    width <- limits[i + 1L] - limits[i]
    stats::integrate(function(u) dbeta(limits[i] + width * u, alpha, beta) * width,
                     lower = 0, upper = 1, subdivisions = 2000L,
                     rel.tol = 1e-9, abs.tol = 1e-11)$value
  }, numeric(1))
}

test_that("Bayesian sampling - equal-thirds controls preserve existing defaults", {
  options <- jaspTools::analysisOptions("BayesianSampling")
  expect_identical(options$impartialThreeConstraintplan, "no_U")
  expect_identical(options$priorplan, "impartial")
  expect_identical(options$impartialShapeplan, "unimodal")
  expect_true("impartialThreeConstraintplan" %in% jaspAcceptanceSampling:::.bsPlanningDeps())
  expect_true("impartialThreeConstraintplan" %in% jaspAcceptanceSampling:::.bsInferenceDeps())
})

test_that("Bayesian sampling - feasible equal-thirds priors reach both analysis stages", {
  # QML accepts six decimals. Exact 1/3 and 2/3 (the uniform boundary)
  # are covered by test-BayesianThreeRegionPrior.R; this reachable pair is unimodal.
  cases <- list(
    list(aql = .05, rql = .15, constraints = c("none", "no_U")),
    list(aql = .05, rql = .055, constraints = c("none", "no_U", "both_ge_1")),
    list(aql = .1, rql = .9, constraints = "none"),
    list(aql = .333334, rql = .666666, constraints = c("none", "no_U", "both_ge_1")))
  original_prior <- get(".bsCalculatePriorParams", asNamespace("jaspAcceptanceSampling"))
  for (case in cases) {
    calibrated <- list()
    for (constraint in case$constraints) {
      options <- .bsThreeOptionsForTest()
      options$aqlplan <- case$aql
      options$rqlplan <- case$rql
      options$impartialThreeConstraintplan <- constraint
      captured <- list()
      prior_wrapper <- function(...) {
        params <- original_prior(...)
        captured[[length(captured) + 1L]] <<- params
        params
      }
      result <- testthat::with_mocked_bindings(
        jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
        .bsCalculatePriorParams = prior_wrapper, .package = "jaspAcceptanceSampling")
      expect_identical(result$status, "complete")
      expect_length(captured, 2L)
      expect_equal(captured[[2L]], captured[[1L]])
      params <- unlist(captured[[1L]])
      calibrated[[constraint]] <- params
      expect_true(all(is.finite(params) & params > 0))
      expect_equal(.bsThreeQuadratureForTest(params[1L], params[2L], case$aql, case$rql),
                   rep(1/3, 3L), tolerance = 1e-8)
      if (constraint == "both_ge_1") {
        expect_true(all(params >= 1))
      } else if (constraint == "no_U") {
        expect_true(any(params >= 1))
      }
      planning <- .bsThreeCollectionForTest(result, "planContainer")
      inference <- .bsThreeCollectionForTest(result, "infContainer")
      expect_null(planning$planContainer_priorError)
      expect_null(inference$infContainer_priorError)
      expect_identical(planning$planContainer_planTable$status, "complete")
      expect_identical(planning$planContainer_threeBFTable$status, "complete")
      expect_identical(planning$planContainer_distPlotPrior$status, "complete")
      expect_identical(inference$infContainer_inferenceDecisionTable$status, "complete")
      row <- planning$planContainer_threeBFTable$data[[1L]]
      expect_equal(c(row$prior_good, row$prior_middle, row$prior_bad), rep(1/3, 3L), tolerance = 1e-8)
    }
    for (params in calibrated) expect_equal(params, calibrated[[1L]], tolerance = 1e-8)
  }
})

test_that("Bayesian sampling - incompatible equal-thirds shape constraints show prior errors", {
  cases <- list(
    list(aql = .05, rql = .15, constraint = "both_ge_1"),
    list(aql = .1, rql = .9, constraint = "no_U"),
    list(aql = .1, rql = .9, constraint = "both_ge_1"))
  for (case in cases) {
    options <- .bsThreeOptionsForTest()
    options$aqlplan <- case$aql
    options$rqlplan <- case$rql
    options$impartialThreeConstraintplan <- case$constraint
    result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
    expect_identical(result$status, "complete")
    for (container in c("planContainer", "infContainer")) {
      collection <- .bsThreeCollectionForTest(result, container)
      error <- collection[[paste0(container, "_priorError")]]
      expect_identical(error$status, "error")
      expect_match(paste(error$error$errorMessage, collapse = ""), "constraint", ignore.case = TRUE)
    }
    planning <- .bsThreeCollectionForTest(result, "planContainer")
    inference <- .bsThreeCollectionForTest(result, "infContainer")
    expect_null(planning$planContainer_planTable)
    expect_null(planning$planContainer_threeBFTable)
    expect_null(planning$planContainer_distPlotPrior)
    expect_null(inference$infContainer_inferenceDecisionTable)
    expect_null(inference$infContainer_decisionSummaryText)
  }
})

test_that("Bayesian sampling - changing equal-thirds constraints clears stale outputs and recovers", {
  original_inference <- get(".bsInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  for (case in list(list(aql = .05, rql = .15, valid = "no_U", invalid = "both_ge_1"),
                    list(aql = .1, rql = .9, valid = "none", invalid = "no_U"))) {
    options <- .bsThreeOptionsForTest()
    options$aqlplan <- case$aql
    options$rqlplan <- case$rql
    options$impartialThreeConstraintplan <- case$valid
    options$posteriorPlotinfer <- TRUE
    observed <- list()
    cache_removed <- logical()
    wrapper <- function(jaspResults, options, position) {
      private <- jaspResults$.__enclos_env__$private
      readOutput <- function() {
        private$complete()
        jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      }
      original_inference(jaspResults, options, position)
      observed[[1L]] <<- readOutput()
      for (i in seq_along(c(case$invalid, case$valid))) {
        options$impartialThreeConstraintplan <- c(case$invalid, case$valid)[i]
        private$changeOptions(as.character(jsonlite::toJSON(options)))
        cache_removed <<- c(cache_removed, is.null(jaspResults[["planData"]]))
        jaspBase:::emptyRecomputed()
        jaspAcceptanceSampling:::.bsPlanningAnalysis(jaspResults, options, 1)
        original_inference(jaspResults, options, position)
        observed[[i + 1L]] <<- readOutput()
      }
    }
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsInferenceAnalysis = wrapper, .package = "jaspAcceptanceSampling")
    expect_identical(result$status, "complete")
    expect_length(observed, 3L)
    expect_true(all(cache_removed))
    for (container in c("planContainer", "infContainer")) {
      before <- .bsThreeCollectionForTest(observed[[1L]], container)
      failed <- .bsThreeCollectionForTest(observed[[2L]], container)
      restored <- .bsThreeCollectionForTest(observed[[3L]], container)
      error_key <- paste0(container, "_priorError")
      expect_null(before[[error_key]])
      expect_identical(failed[[error_key]]$status, "error")
      expect_null(restored[[error_key]])
      keys <- if (container == "planContainer") c("planContainer_planTable", "planContainer_threeBFTable") else
        "infContainer_inferenceDecisionTable"
      for (key in keys) {
        expect_gt(length(before[[key]]$data), 0L)
        expect_null(failed[[key]])
        expect_equal(restored[[key]]$data, before[[key]]$data)
      }
    }
    expect_null(.bsThreeCollectionForTest(observed[[2L]], "planContainer")$planContainer_distPlotPrior)
    failed <- .bsThreeCollectionForTest(observed[[2L]], "infContainer")
    expect_null(failed$infContainer_decisionSummaryText)
    expect_null(failed$infContainer_distPlotPosterior)
    expect_null(failed$infContainer_bfText)
  }
})

test_that("Bayesian sampling - old approximate equal-thirds plans are recalibrated", {
  options <- .bsThreeOptionsForTest()
  options$aqlplan <- .05
  options$rqlplan <- .055
  options$impartialThreeConstraintplan <- "none"
  options$max_nplan <- 40
  options$inferPosteriorinfer <- FALSE
  options$priorPlotplan <- FALSE
  ns <- asNamespace("jaspAcceptanceSampling")
  original <- get(".bsPlanningAnalysis", ns)
  original_generator <- get(".bsGeneratePlans", ns)
  # Former penalized optimizer result; its masses were about .378/.244/.379.
  old_params <- c(alpha = 40.482709790714104, beta = 725.36953613152332)
  legacy <- original_generator(.05, .055, 40, 1, old_params[[1L]], old_params[[2L]], add_three_hyp = TRUE)
  expect_identical(attr(legacy, "priorParameters"), old_params)
  generations <- 0L
  observed_generations <- integer()
  observed <- list()
  generator <- function(...) {
    generations <<- generations + 1L
    original_generator(...)
  }
  wrapper <- function(jaspResults, options, position) {
    private <- jaspResults$.__enclos_env__$private
    readTable <- function() {
      private$complete()
      result <- jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      .bsThreeCollectionForTest(result, "planContainer")$planContainer_threeBFTable
    }
    original(jaspResults, options, position)
    observed[[1L]] <<- readTable()
    observed_generations <<- c(observed_generations, generations)
    jaspBase:::emptyRecomputed()
    original(jaspResults, options, position)
    observed_generations <<- c(observed_generations, generations)
    jaspResults[["planData"]]$object <- legacy
    jaspBase:::emptyRecomputed()
    original(jaspResults, options, position)
    observed_generations <<- c(observed_generations, generations)
    observed[[2L]] <<- readTable()
  }
  result <- testthat::with_mocked_bindings(
    jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
    .bsPlanningAnalysis = wrapper, .bsGeneratePlans = generator, .package = "jaspAcceptanceSampling")
  expect_identical(result$status, "complete")
  expect_equal(observed_generations, c(1L, 1L, 2L))
  expect_equal(observed[[2L]]$data, observed[[1L]]$data)
  row <- observed[[2L]]$data[[1L]]
  expect_equal(c(row$prior_good, row$prior_middle, row$prior_bad), rep(1/3, 3L), tolerance = 1e-8)
  expect_gt(max(abs(.bsThreeQuadratureForTest(old_params[1L], old_params[2L], .05, .055) - 1/3)), .05)
})

test_that("Bayesian sampling - exact-thirds planning and inference agree with density quadrature", {
  options <- .bsThreeOptionsForTest()
  options$max_nplan <- 40
  options$min_bfplan <- 30
  options$priorPlotplan <- FALSE
  ns <- asNamespace("jaspAcceptanceSampling")
  original_prior <- get(".bsCalculatePriorParams", ns)
  original_table <- get(".bsInferenceDecisionTable", ns)
  params <- NULL
  inference_values <- NULL
  testthat::local_mocked_bindings(
    .bsCalculatePriorParams = function(...) {
      result <- original_prior(...)
      params <<- c(alpha = result$alpha, beta = result$beta)
      result
    },
    .bsInferenceDecisionTable = function(container, n, d, N, aql, rql, log_bf, ppd, acceptanceNumber, ...) {
      inference_values <<- list(log_bf = log_bf, ppd = ppd, c = acceptanceNumber)
      original_table(container, n, d, N, aql, rql, log_bf, ppd, acceptanceNumber, ...)
    }, .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(result$status, "complete")
  prior <- .bsThreeQuadratureForTest(params[1L], params[2L], .05, .15)
  expect_equal(prior, rep(1/3, 3L), tolerance = 1e-8)
  regions <- function(n, d) .bsThreeQuadratureForTest(params[1L] + d, params[2L] + n - d, .05, .15)
  binary_bf <- function(n, d) {
    post <- regions(n, d)
    ((post[1L] + post[2L]) / post[3L]) / ((prior[1L] + prior[2L]) / prior[3L])
  }
  planning <- .bsThreeCollectionForTest(result, "planContainer")
  plan <- planning$planContainer_planTable
  three <- planning$planContainer_threeBFTable
  expect_identical(plan$status, "complete")
  expect_identical(three$status, "complete")
  final_bf <- vapply(0:40, function(d) binary_bf(40, d), numeric(1))
  max_c <- max((0:40)[final_bf >= 30])
  expect_equal(vapply(plan$data, function(row) row$c, numeric(1)), 0:max_c)
  expect_equal(vapply(three$data, function(row) row$n, numeric(1)),
               vapply(plan$data, function(row) row$n, numeric(1)))
  for (i in seq_along(plan$data)) {
    row <- plan$data[[i]]
    post <- regions(row$n, row$c)
    expect_equal(row$bf_odds, binary_bf(row$n, row$c), tolerance = 1e-8)
    expect_gte(binary_bf(row$n, row$c), 30)
    expect_lt(binary_bf(row$n - 1, row$c), 30)
    expect_lt(binary_bf(row$n, row$c + 1), 30)
    expected_pairwise <- c((post[1L] / post[3L]) / (prior[1L] / prior[3L]),
                           (post[1L] / post[2L]) / (prior[1L] / prior[2L]),
                           (post[2L] / post[3L]) / (prior[2L] / prior[3L]))
    three_row <- three$data[[i]]
    expect_equal(c(three_row$BF_GB, three_row$BF_GM, three_row$BF_MB), expected_pairwise, tolerance = 1e-8)
    expect_equal(c(three_row$post_good, three_row$post_middle, three_row$post_bad), post, tolerance = 1e-8)
  }
  expect_equal(exp(inference_values$log_bf), binary_bf(30, 0), tolerance = 1e-8)
  expected_c <- max((0:30)[vapply(0:30, function(d) binary_bf(30, d), numeric(1)) >= 30])
  expect_equal(inference_values$c, expected_c)
  # Independent mixture integration for the remaining 70 items in this lot.
  remaining_probability <- function(region) {
    stats::integrate(function(theta) {
      probability <- switch(region,
        good = pbinom(5, 70, theta),
        middle = pbinom(15, 70, theta) - pbinom(5, 70, theta),
        bad = pbinom(15, 70, theta, lower.tail = FALSE))
      probability * dbeta(theta, params[1L], params[2L] + 30)
    }, 0, 1, subdivisions = 2000L, rel.tol = 1e-9, abs.tol = 1e-11)$value
  }
  expect_equal(unlist(inference_values$ppd, use.names = FALSE),
               unname(vapply(c("good", "middle", "bad"), remaining_probability, numeric(1))),
               tolerance = 1e-8)
})
