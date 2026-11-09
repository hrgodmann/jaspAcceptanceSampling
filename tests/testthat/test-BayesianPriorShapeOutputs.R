# Shape selection must reach calibration, cached plans, and visible prior errors.

.bsPriorShapeOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$planningPriorDistribution <- "impartial"
  options$planningAcceptableQualityLevel <- .05
  options$planningRejectableQualityLevel <- .15
  options$planningPriorImpartialShape <- "unimodal"
  options$planningPriorImpartialCustomLocation <- TRUE
  options$planningPriorImpartialLocation <- .05
  options$planningPriorPlot <- TRUE
  options$planningPlanTable <- TRUE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$inferencePosterior <- TRUE
  options$inferenceDecisionTable <- TRUE
  options$inferencePosteriorPlot <- TRUE
  options$inferencePosteriorPredictivePlot <- FALSE
  options
}

.bsPriorShapeCollectionForTest <- function(result, key) {
  result$results[[key]]$collection
}

test_that("Bayesian sampling - parsed impartial defaults preserve the existing location options", {
  options <- jaspTools::analysisOptions("BayesianSampling")
  expect_identical(options$planningPriorDistribution, "impartial")
  expect_identical(options$planningPriorImpartialShape, "unimodal")
  expect_false(options$planningPriorImpartialCustomLocation)
  expect_equal(options$planningPriorImpartialLocation, .05)
  expect_true("planningPriorImpartialShape" %in% jaspAcceptanceSampling:::.bsGetPlanningDependencies())
  expect_true("planningPriorImpartialShape" %in% jaspAcceptanceSampling:::.bsGetInferenceDependencies())
})

test_that("Bayesian sampling - selected prior shape and location reach planning and inference", {
  cases <- list(
    list(shape = "unimodal", rql = .15, custom = FALSE, retained = .95, location = .05),
    list(shape = "uShaped", rql = .15, custom = TRUE, retained = .95, location = .95),
    list(shape = "uShaped", rql = .9, custom = TRUE, retained = .05, location = .05),
    list(shape = "uShaped", rql = .9, custom = FALSE, retained = .95, location = .05))
  originalPlot <- get(".bsCreateDistributionPlot", asNamespace("jaspAcceptanceSampling"))
  for (case in cases) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPlanTable <- FALSE
    options$planningPriorImpartialShape <- case$shape
    options$planningRejectableQualityLevel <- case$rql
    options$planningPriorImpartialCustomLocation <- case$custom
    options$planningPriorImpartialLocation <- case$retained
    plotted <- list()
    plotWrapper <- function(container, aql, rql, alpha, beta, type, ...) {
      plotted[[type]] <<- c(alpha = alpha, beta = beta)
      originalPlot(container, aql, rql, alpha, beta, type, ...)
    }
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsCreateDistributionPlot = plotWrapper, .package = "jaspAcceptanceSampling")
    expect_identical(result$status, "complete")
    expect_named(plotted, c("Prior", "Posterior"))
    params <- plotted$Prior
    expect_true(all(is.finite(params)))
    expect_true(all(params > 0))
    if (case$shape == "unimodal") expect_true(all(params > 1)) else expect_true(all(params < 1))
    expect_equal(unname((params[1L] - 1) / (sum(params) - 2)), case$location,
                 tolerance = 1e-8)
    expect_equal(pbeta(case$rql, params[1L], params[2L]), .5, tolerance = 1e-8)
    expect_equal(plotted$Posterior,
                 params + c(alpha = options$inferenceObservedDefectives, beta = options$inferenceSampleSize - options$inferenceObservedDefectives),
                 tolerance = 1e-8)
    planning <- .bsPriorShapeCollectionForTest(result, "planContainer")
    inference <- .bsPriorShapeCollectionForTest(result, "infContainer")
    expect_null(planning$planContainer_priorError)
    expect_null(inference$infContainer_priorError)
    expect_identical(planning$planContainer_distPlotPrior$status, "complete")
    expect_identical(inference$infContainer_inferenceDecisionTable$status, "complete")
    if (case$shape == "unimodal") {
      bf <- Filter(function(row) identical(row$col_1, "Bayes Factor"),
                   inference$infContainer_inferenceDecisionTable$data)[[1L]]$col_2
      expect_equal(as.numeric(bf), 188.832)
    }
  }
})

test_that("Bayesian sampling - incompatible and unidentified impartial priors show actionable errors", {
  cases <- list(
    list(shape = "unimodal", location = .95, rql = .15),
    list(shape = "uShaped", location = .05, rql = .15),
    list(shape = "unimodal", location = .5, rql = .5),
    list(shape = "uShaped", location = .5, rql = .5),
    list(shape = "unimodal", location = .15, rql = .15))
  for (case in cases) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPriorImpartialShape <- case$shape
    options$planningPriorImpartialLocation <- case$location
    options$planningRejectableQualityLevel <- case$rql
    result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                    options = options, view = FALSE)
    expect_identical(result$status, "complete")
    for (container in c("planContainer", "infContainer")) {
      collection <- .bsPriorShapeCollectionForTest(result, container)
      error <- collection[[paste0(container, "_priorError")]]
      expect_identical(error$status, "error")
      expect_match(paste(error$error$errorMessage, collapse = ""), "Custom", fixed = TRUE)
    }
    expect_null(.bsPriorShapeCollectionForTest(result, "planContainer")$planContainer_distPlotPrior)
    expect_null(.bsPriorShapeCollectionForTest(result, "infContainer")$infContainer_inferenceDecisionTable)
  }
})

test_that("Bayesian sampling - shape-only changes invalidate plans and recover cleanly", {
  originalInference <- get(".bsRunInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  for (initial_shape in c("unimodal", "uShaped")) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPriorImpartialShape <- initial_shape
    options$planningPriorImpartialLocation <- if (initial_shape == "unimodal") .05 else .95
    other <- if (initial_shape == "unimodal") "uShaped" else "unimodal"
    observed <- list()
    cacheRemoved <- logical()
    wrapper <- function(jaspResults, options, position) {
      originalInference(jaspResults, options, position)
      private <- jaspResults$.__enclos_env__$private
      readOutput <- function() {
        private$complete()
        jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      }
      observed[[1L]] <<- readOutput()
      for (i in seq_along(c(other, initial_shape))) {
        options$planningPriorImpartialShape <- c(other, initial_shape)[i]
        private$changeOptions(as.character(jsonlite::toJSON(options)))
        cacheRemoved <<- c(cacheRemoved, is.null(jaspResults[["planData"]]))
        jaspBase:::emptyRecomputed()
        # Call the real builders without re-entering this test wrapper.
        jaspAcceptanceSampling:::.bsCreateProcedure(jaspResults, options, 0)
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
      before <- .bsPriorShapeCollectionForTest(observed[[1L]], container)
      failed <- .bsPriorShapeCollectionForTest(observed[[2L]], container)
      restored <- .bsPriorShapeCollectionForTest(observed[[3L]], container)
      errorKey <- paste0(container, "_priorError")
      expect_null(before[[errorKey]])
      expect_identical(failed[[errorKey]]$status, "error")
      expect_match(paste(failed[[errorKey]]$error$errorMessage, collapse = ""), "Custom", fixed = TRUE)
      expect_null(restored[[errorKey]])
      tableKey <- if (container == "planContainer") "planContainer_planTable" else "infContainer_inferenceDecisionTable"
      expect_gt(length(before[[tableKey]]$data), 0L)
      expect_null(failed[[tableKey]])
      expect_equal(restored[[tableKey]]$data, before[[tableKey]]$data)
    }
    failedInf <- .bsPriorShapeCollectionForTest(observed[[2L]], "infContainer")
    expect_null(failedInf$infContainer_decisionSummaryText)
    expect_null(failedInf$infContainer_distPlotPosterior)
    expect_null(failedInf$infContainer_bfText)
    expect_null(.bsPriorShapeCollectionForTest(observed[[2L]], "planContainer")$planContainer_distPlotPrior)
  }
})

test_that("Bayesian sampling - other priors ignore retained impartial shape and location choices", {
  original <- get(".bsRunInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  for (prior in c("uniform", "custom", "impartialThreeRegion")) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPriorDistribution <- prior
    options$planningPriorAlpha <- 2
    options$planningPriorBeta <- 3
    options$planningPlanTable <- FALSE
    options$planningPriorPlot <- FALSE
    options$inferencePosteriorPlot <- FALSE
    observed <- list()
    wrapper <- function(jaspResults, options, position) {
      private <- jaspResults$.__enclos_env__$private
      readCollection <- function() {
        private$complete()
        result <- jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
        .bsPriorShapeCollectionForTest(result, "infContainer")
      }
      original(jaspResults, options, position)
      observed[[1L]] <<- readCollection()
      # This retained minimum/RQL pair would be incompatible if impartial were selected.
      options$planningPriorImpartialShape <- "uShaped"
      options$planningPriorImpartialCustomLocation <- TRUE
      options$planningPriorImpartialLocation <- .05
      private$changeOptions(as.character(jsonlite::toJSON(options)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      observed[[2L]] <<- readCollection()
    }
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsRunInferenceAnalysis = wrapper, .package = "jaspAcceptanceSampling")
    expect_identical(result$status, "complete")
    expect_length(observed, 2L)
    for (collection in observed) {
      expect_null(collection$infContainer_priorError)
      expect_identical(collection$infContainer_inferenceDecisionTable$status, "complete")
    }
    expect_equal(observed[[2L]]$infContainer_inferenceDecisionTable$data,
                 observed[[1L]]$infContainer_inferenceDecisionTable$data)
  }
})


test_that("Bayesian sampling - a valid concentrated impartial prior reaches planning and finite-lot inference", {
  options <- .bsPriorShapeOptionsForTest()
  options$planningAcceptableQualityLevel <- 1e-6
  options$planningRejectableQualityLevel <- 2e-6
  options$planningPriorImpartialShape <- "unimodal"
  options$planningPriorImpartialCustomLocation <- FALSE
  options$planningPriorImpartialLocation <- .95
  options$planningMaximumSampleSize <- 1
  options$planningMinimumBayesFactor <- 1
  options$planningPriorPlot <- FALSE
  options$inferencePosteriorPlot <- FALSE
  options$inferencePosteriorPredictivePlot <- TRUE
  options$inferenceSampleSize <- 100
  options$inferenceObservedDefectives <- 0
  options$inferenceLotSize <- 100
  ns <- asNamespace("jaspAcceptanceSampling")
  originalPrior <- get(".bsCalculatePriorParameters", ns)
  originalTable <- get(".bsCreateInferenceDecisionTable", ns)
  capturedPriors <- list()
  capturedInference <- NULL
  testthat::local_mocked_bindings(
    .bsCalculatePriorParameters = function(...) {
      params <- originalPrior(...)
      capturedPriors[[length(capturedPriors) + 1L]] <<- params
      params
    },
    .bsCreateInferenceDecisionTable = function(container, n, d, N, aql, rql,
                                         logBayesFactor, ppd, acceptanceNumber, ...) {
      capturedInference <<- list(log_bf = logBayesFactor, ppd = ppd, c = acceptanceNumber)
      originalTable(container, n, d, N, aql, rql, logBayesFactor, ppd, acceptanceNumber, ...)
    }, .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_length(capturedPriors, 2L)
  expect_equal(capturedPriors[[2L]], capturedPriors[[1L]])
  alpha <- capturedPriors[[1L]]$alpha
  beta <- capturedPriors[[1L]]$beta
  expect_equal(alpha, 1.680945757, tolerance = 1e-7)
  expect_gt(beta, 10000)
  expect_lt(abs(beta - 680946.076), .1)
  expect_equal((alpha - 1) / (alpha + beta - 2), 1e-6, tolerance = 1e-10)
  # This reference integrates the density on a rescaled unit interval; it does
  # not call the production log-tail helpers or pbeta().
  integratedProbability <- function(a, b) {
    stats::integrate(function(u) stats::dbeta(options$planningRejectableQualityLevel * u, a, b) * options$planningRejectableQualityLevel,
                     lower = 0, upper = 1, rel.tol = 1e-11, abs.tol = 1e-13,
                     subdivisions = 2000L)$value
  }
  priorP <- integratedProbability(alpha, beta)
  planP <- integratedProbability(alpha, beta + 1)
  posteriorP <- integratedProbability(alpha, beta + 100)
  odds <- function(p) p / (1 - p)
  expectedPlanBf <- odds(planP) / odds(priorP)
  expectedInferenceBf <- odds(posteriorP) / odds(priorP)
  expect_equal(priorP, .5, tolerance = 1e-8)
  expect_gt(expectedPlanBf, 1)
  expect_gt(expectedInferenceBf, 1)
  planning <- .bsPriorShapeCollectionForTest(result, "planContainer")
  inference <- .bsPriorShapeCollectionForTest(result, "infContainer")
  expect_null(planning$planContainer_priorError)
  expect_null(inference$infContainer_priorError)
  expect_null(inference$infContainer_numericalError)
  plan <- planning$planContainer_planTable
  expect_identical(plan$status, "complete")
  expect_length(plan$data, 1L)
  expect_equal(plan$data[[1L]]$n, 1)
  expect_equal(plan$data[[1L]]$c, 0)
  expect_equal(plan$data[[1L]]$bf_odds, expectedPlanBf, tolerance = 1e-8)
  expect_equal(exp(capturedInference$log_bf), expectedInferenceBf, tolerance = 1e-8)
  expect_identical(capturedInference$c, 0L)
  expect_equal(unlist(capturedInference$ppd, use.names = FALSE), c(1, 0, 0), tolerance = 1e-12)
  expect_identical(inference$infContainer_inferenceDecisionTable$status, "complete")
  expect_match(inference$infContainer_decisionSummaryText$rawtext,
               "target of 1 has been reached", fixed = TRUE)
  plotKey <- inference$infContainer_ppdPlot$data
  plot <- result$state$figures[[plotKey]]$obj
  expect_equal(plot$data$d_tot, 0)
  expect_equal(plot$data$pmf, 1)
})
