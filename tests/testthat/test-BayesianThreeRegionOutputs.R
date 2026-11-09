# Equal-thirds calibration must survive the full planning/inference output path.

.bsThreeOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$planningPriorDistribution <- "impartialThreeRegion"
  options$planningAcceptableQualityLevel <- .05
  options$planningRejectableQualityLevel <- .15
  options$planningPriorThreeRegionShapeConstraint <- "nonUShaped"
  options$planningMaximumSampleSize <- 1
  options$planningMinimumBayesFactor <- 1
  options$planningPlanTable <- TRUE
  options$planningThreeHypothesisBayesFactorTable <- TRUE
  options$planningPriorPlot <- TRUE
  options$inferencePosterior <- TRUE
  options$inferenceDecisionTable <- TRUE
  options$inferencePosteriorPlot <- FALSE
  options$inferencePosteriorPredictivePlot <- FALSE
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

test_that("Bayesian sampling - equal-thirds constraints are optional by default", {
  options <- jaspTools::analysisOptions("BayesianSampling")
  expect_identical(options$planningPriorThreeRegionShapeConstraint, "none")
  expect_identical(options$planningPriorDistribution, "impartial")
  expect_identical(options$planningPriorImpartialShape, "unimodal")
  expect_true("planningPriorThreeRegionShapeConstraint" %in% jaspAcceptanceSampling:::.bsGetPlanningDependencies())
  expect_true("planningPriorThreeRegionShapeConstraint" %in% jaspAcceptanceSampling:::.bsGetInferenceDependencies())
})

test_that("Bayesian sampling - feasible equal-thirds priors reach both analysis stages", {
  # QML accepts six decimals. Exact 1/3 and 2/3 (the uniform boundary)
  # are covered by test-BayesianThreeRegionPrior.R; this reachable pair is unimodal.
  cases <- list(
    list(aql = .05, rql = .15, constraints = c("none", "nonUShaped")),
    list(aql = .05, rql = .055, constraints = c("none", "nonUShaped", "bothAtLeastOne")),
    list(aql = .1, rql = .9, constraints = "none"),
    list(aql = .333334, rql = .666666, constraints = c("none", "nonUShaped", "bothAtLeastOne")))
  originalPrior <- get(".bsCalculatePriorParameters", asNamespace("jaspAcceptanceSampling"))
  for (case in cases) {
    calibrated <- list()
    for (constraint in case$constraints) {
      options <- .bsThreeOptionsForTest()
      options$planningAcceptableQualityLevel <- case$aql
      options$planningRejectableQualityLevel <- case$rql
      options$planningPriorThreeRegionShapeConstraint <- constraint
      captured <- list()
      priorWrapper <- function(...) {
        params <- originalPrior(...)
        captured[[length(captured) + 1L]] <<- params
        params
      }
      result <- testthat::with_mocked_bindings(
        jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
        .bsCalculatePriorParameters = priorWrapper, .package = "jaspAcceptanceSampling")
      expect_identical(result$status, "complete")
      expect_length(captured, 2L)
      expect_equal(captured[[2L]], captured[[1L]])
      params <- unlist(captured[[1L]])
      calibrated[[constraint]] <- params
      expect_true(all(is.finite(params) & params > 0))
      expect_equal(.bsThreeQuadratureForTest(params[1L], params[2L], case$aql, case$rql),
                   rep(1/3, 3L), tolerance = 1e-8)
      if (constraint == "bothAtLeastOne") {
        expect_true(all(params >= 1))
      } else if (constraint == "nonUShaped") {
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
    list(aql = .05, rql = .15, constraint = "bothAtLeastOne"),
    list(aql = .1, rql = .9, constraint = "nonUShaped"),
    list(aql = .1, rql = .9, constraint = "bothAtLeastOne"))
  for (case in cases) {
    options <- .bsThreeOptionsForTest()
    options$planningAcceptableQualityLevel <- case$aql
    options$planningRejectableQualityLevel <- case$rql
    options$planningPriorThreeRegionShapeConstraint <- case$constraint
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
  originalInference <- get(".bsRunInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  for (case in list(list(aql = .05, rql = .15, valid = "nonUShaped", invalid = "bothAtLeastOne"),
                    list(aql = .1, rql = .9, valid = "none", invalid = "nonUShaped"))) {
    options <- .bsThreeOptionsForTest()
    options$planningAcceptableQualityLevel <- case$aql
    options$planningRejectableQualityLevel <- case$rql
    options$planningPriorThreeRegionShapeConstraint <- case$valid
    options$inferencePosteriorPlot <- TRUE
    observed <- list()
    cacheRemoved <- logical()
    wrapper <- function(jaspResults, options, position) {
      private <- jaspResults$.__enclos_env__$private
      readOutput <- function() {
        private$complete()
        jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      }
      originalInference(jaspResults, options, position)
      observed[[1L]] <<- readOutput()
      for (i in seq_along(c(case$invalid, case$valid))) {
        options$planningPriorThreeRegionShapeConstraint <- c(case$invalid, case$valid)[i]
        private$changeOptions(as.character(jsonlite::toJSON(options)))
        cacheRemoved <<- c(cacheRemoved, is.null(jaspResults[["planData"]]))
        jaspBase:::emptyRecomputed()
        jaspAcceptanceSampling:::.bsRunPlanningAnalysis(jaspResults, options, 1)
        originalInference(jaspResults, options, position)
        observed[[i + 1L]] <<- readOutput()
      }
    }
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsRunInferenceAnalysis = wrapper, .package = "jaspAcceptanceSampling")
    expect_identical(result$status, "complete")
    expect_length(observed, 3L)
    expect_true(all(cacheRemoved))
    for (container in c("planContainer", "infContainer")) {
      before <- .bsThreeCollectionForTest(observed[[1L]], container)
      failed <- .bsThreeCollectionForTest(observed[[2L]], container)
      restored <- .bsThreeCollectionForTest(observed[[3L]], container)
      errorKey <- paste0(container, "_priorError")
      expect_null(before[[errorKey]])
      expect_identical(failed[[errorKey]]$status, "error")
      expect_null(restored[[errorKey]])
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
  options$planningAcceptableQualityLevel <- .05
  options$planningRejectableQualityLevel <- .055
  options$planningPriorThreeRegionShapeConstraint <- "none"
  options$planningMaximumSampleSize <- 40
  options$inferencePosterior <- FALSE
  options$planningPriorPlot <- FALSE
  ns <- asNamespace("jaspAcceptanceSampling")
  original <- get(".bsRunPlanningAnalysis", ns)
  originalGenerator <- get(".bsGeneratePlans", ns)
  # Former penalized optimizer result; its masses were about .378/.244/.379.
  oldParams <- c(alpha = 40.482709790714104, beta = 725.36953613152332)
  legacy <- originalGenerator(.05, .055, 40, 1, oldParams[[1L]], oldParams[[2L]], threeHypothesisComparison = TRUE)
  expect_identical(attr(legacy, "priorParameters"), oldParams)
  generations <- 0L
  observedGenerations <- integer()
  observed <- list()
  generator <- function(...) {
    generations <<- generations + 1L
    originalGenerator(...)
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
    observedGenerations <<- c(observedGenerations, generations)
    jaspBase:::emptyRecomputed()
    original(jaspResults, options, position)
    observedGenerations <<- c(observedGenerations, generations)
    jaspResults[["planData"]]$object <- legacy
    jaspBase:::emptyRecomputed()
    original(jaspResults, options, position)
    observedGenerations <<- c(observedGenerations, generations)
    observed[[2L]] <<- readTable()
  }
  result <- testthat::with_mocked_bindings(
    jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
    .bsRunPlanningAnalysis = wrapper, .bsGeneratePlans = generator, .package = "jaspAcceptanceSampling")
  expect_identical(result$status, "complete")
  expect_equal(observedGenerations, c(1L, 1L, 2L))
  expect_equal(observed[[2L]]$data, observed[[1L]]$data)
  row <- observed[[2L]]$data[[1L]]
  expect_equal(c(row$prior_good, row$prior_middle, row$prior_bad), rep(1/3, 3L), tolerance = 1e-8)
  expect_gt(max(abs(.bsThreeQuadratureForTest(oldParams[1L], oldParams[2L], .05, .055) - 1/3)), .05)
})

test_that("Bayesian sampling - exact-thirds planning and inference agree with density quadrature", {
  options <- .bsThreeOptionsForTest()
  options$planningMaximumSampleSize <- 40
  options$planningMinimumBayesFactor <- 30
  options$planningPriorPlot <- FALSE
  ns <- asNamespace("jaspAcceptanceSampling")
  originalPrior <- get(".bsCalculatePriorParameters", ns)
  originalTable <- get(".bsCreateInferenceDecisionTable", ns)
  params <- NULL
  inferenceValues <- NULL
  testthat::local_mocked_bindings(
    .bsCalculatePriorParameters = function(...) {
      result <- originalPrior(...)
      params <<- c(alpha = result$alpha, beta = result$beta)
      result
    },
    .bsCreateInferenceDecisionTable = function(container, n, d, N, aql, rql, logBayesFactor, ppd, acceptanceNumber, ...) {
      inferenceValues <<- list(log_bf = logBayesFactor, ppd = ppd, c = acceptanceNumber)
      originalTable(container, n, d, N, aql, rql, logBayesFactor, ppd, acceptanceNumber, ...)
    }, .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(result$status, "complete")
  prior <- .bsThreeQuadratureForTest(params[1L], params[2L], .05, .15)
  expect_equal(prior, rep(1/3, 3L), tolerance = 1e-8)
  regions <- function(n, d) .bsThreeQuadratureForTest(params[1L] + d, params[2L] + n - d, .05, .15)
  binaryBf <- function(n, d) {
    post <- regions(n, d)
    ((post[1L] + post[2L]) / post[3L]) / ((prior[1L] + prior[2L]) / prior[3L])
  }
  planning <- .bsThreeCollectionForTest(result, "planContainer")
  plan <- planning$planContainer_planTable
  three <- planning$planContainer_threeBFTable
  expect_identical(plan$status, "complete")
  expect_identical(three$status, "complete")
  finalBf <- vapply(0:40, function(d) binaryBf(40, d), numeric(1))
  maxC <- max((0:40)[finalBf >= 30])
  expect_equal(vapply(plan$data, function(row) row$c, numeric(1)), 0:maxC)
  expect_equal(vapply(three$data, function(row) row$n, numeric(1)),
               vapply(plan$data, function(row) row$n, numeric(1)))
  for (i in seq_along(plan$data)) {
    row <- plan$data[[i]]
    post <- regions(row$n, row$c)
    expect_equal(row$bf_odds, binaryBf(row$n, row$c), tolerance = 1e-8)
    expect_gte(binaryBf(row$n, row$c), 30)
    expect_lt(binaryBf(row$n - 1, row$c), 30)
    expect_lt(binaryBf(row$n, row$c + 1), 30)
    expectedPairwise <- c((post[1L] / post[3L]) / (prior[1L] / prior[3L]),
                           (post[1L] / post[2L]) / (prior[1L] / prior[2L]),
                           (post[2L] / post[3L]) / (prior[2L] / prior[3L]))
    threeRow <- three$data[[i]]
    expect_equal(c(threeRow$BF_GB, threeRow$BF_GM, threeRow$BF_MB), expectedPairwise, tolerance = 1e-8)
    expect_equal(c(threeRow$post_good, threeRow$post_middle, threeRow$post_bad), post, tolerance = 1e-8)
  }
  expect_equal(exp(inferenceValues$log_bf), binaryBf(30, 0), tolerance = 1e-8)
  expectedC <- max((0:30)[vapply(0:30, function(d) binaryBf(30, d), numeric(1)) >= 30])
  expect_equal(inferenceValues$c, expectedC)
  # Independent mixture integration for the remaining 70 items in this lot.
  remainingProbability <- function(region) {
    stats::integrate(function(theta) {
      probability <- switch(region,
        good = pbinom(5, 70, theta),
        middle = pbinom(15, 70, theta) - pbinom(5, 70, theta),
        bad = pbinom(15, 70, theta, lower.tail = FALSE))
      probability * dbeta(theta, params[1L], params[2L] + 30)
    }, 0, 1, subdivisions = 2000L, rel.tol = 1e-9, abs.tol = 1e-11)$value
  }
  expect_equal(unlist(inferenceValues$ppd, use.names = FALSE),
               unname(vapply(c("good", "middle", "bad"), remainingProbability, numeric(1))),
               tolerance = 1e-8)
})
