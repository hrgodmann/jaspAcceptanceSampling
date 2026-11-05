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
  options$planningPlansTable <- TRUE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$inferenceUseData <- TRUE
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
  expect_true("planningPriorImpartialShape" %in% jaspAcceptanceSampling:::.bsPlanningDeps())
  expect_true("planningPriorImpartialShape" %in% jaspAcceptanceSampling:::.bsInferenceDeps())
})

test_that("Bayesian sampling - selected prior shape and location reach planning and inference", {
  cases <- list(
    list(shape = "unimodal", rql = .15, custom = FALSE, retained = .95, location = .05),
    list(shape = "u_shaped", rql = .15, custom = TRUE, retained = .95, location = .95),
    list(shape = "u_shaped", rql = .9, custom = TRUE, retained = .05, location = .05),
    list(shape = "u_shaped", rql = .9, custom = FALSE, retained = .95, location = .05))
  original_plot <- get(".bsDistributionPlot", asNamespace("jaspAcceptanceSampling"))
  for (case in cases) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPlansTable <- FALSE
    options$planningPriorImpartialShape <- case$shape
    options$planningRejectableQualityLevel <- case$rql
    options$planningPriorImpartialCustomLocation <- case$custom
    options$planningPriorImpartialLocation <- case$retained
    plotted <- list()
    plot_wrapper <- function(container, aql, rql, alpha, beta, type, ...) {
      plotted[[type]] <<- c(alpha = alpha, beta = beta)
      original_plot(container, aql, rql, alpha, beta, type, ...)
    }
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsDistributionPlot = plot_wrapper, .package = "jaspAcceptanceSampling")
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
    list(shape = "u_shaped", location = .05, rql = .15),
    list(shape = "unimodal", location = .5, rql = .5),
    list(shape = "u_shaped", location = .5, rql = .5),
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
  original_inference <- get(".bsInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  for (initial_shape in c("unimodal", "u_shaped")) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPriorImpartialShape <- initial_shape
    options$planningPriorImpartialLocation <- if (initial_shape == "unimodal") .05 else .95
    other <- if (initial_shape == "unimodal") "u_shaped" else "unimodal"
    observed <- list()
    cache_removed <- logical()
    wrapper <- function(jaspResults, options, position) {
      original_inference(jaspResults, options, position)
      private <- jaspResults$.__enclos_env__$private
      readOutput <- function() {
        private$complete()
        jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      }
      observed[[1L]] <<- readOutput()
      for (i in seq_along(c(other, initial_shape))) {
        options$planningPriorImpartialShape <- c(other, initial_shape)[i]
        private$changeOptions(as.character(jsonlite::toJSON(options)))
        cache_removed <<- c(cache_removed, is.null(jaspResults[["planData"]]))
        jaspBase:::emptyRecomputed()
        # Call the real builders without re-entering this test wrapper.
        jaspAcceptanceSampling:::.bsProcedure(jaspResults, options, 0)
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
      before <- .bsPriorShapeCollectionForTest(observed[[1L]], container)
      failed <- .bsPriorShapeCollectionForTest(observed[[2L]], container)
      restored <- .bsPriorShapeCollectionForTest(observed[[3L]], container)
      error_key <- paste0(container, "_priorError")
      expect_null(before[[error_key]])
      expect_identical(failed[[error_key]]$status, "error")
      expect_match(paste(failed[[error_key]]$error$errorMessage, collapse = ""), "Custom", fixed = TRUE)
      expect_null(restored[[error_key]])
      table_key <- if (container == "planContainer") "planContainer_planTable" else "infContainer_inferenceDecisionTable"
      expect_gt(length(before[[table_key]]$data), 0L)
      expect_null(failed[[table_key]])
      expect_equal(restored[[table_key]]$data, before[[table_key]]$data)
    }
    failed_inf <- .bsPriorShapeCollectionForTest(observed[[2L]], "infContainer")
    expect_null(failed_inf$infContainer_decisionSummaryText)
    expect_null(failed_inf$infContainer_distPlotPosterior)
    expect_null(failed_inf$infContainer_bfText)
    expect_null(.bsPriorShapeCollectionForTest(observed[[2L]], "planContainer")$planContainer_distPlotPrior)
  }
})

test_that("Bayesian sampling - other priors ignore retained impartial shape and location choices", {
  original <- get(".bsInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  for (prior in c("uniform", "custom", "impartial_three")) {
    options <- .bsPriorShapeOptionsForTest()
    options$planningPriorDistribution <- prior
    options$planningPriorAlpha <- 2
    options$planningPriorBeta <- 3
    options$planningPlansTable <- FALSE
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
      options$planningPriorImpartialShape <- "u_shaped"
      options$planningPriorImpartialCustomLocation <- TRUE
      options$planningPriorImpartialLocation <- .05
      private$changeOptions(as.character(jsonlite::toJSON(options)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      observed[[2L]] <<- readCollection()
    }
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsInferenceAnalysis = wrapper, .package = "jaspAcceptanceSampling")
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
  original_prior <- get(".bsCalculatePriorParams", ns)
  original_table <- get(".bsInferenceDecisionTable", ns)
  captured_priors <- list()
  captured_inference <- NULL
  testthat::local_mocked_bindings(
    .bsCalculatePriorParams = function(...) {
      params <- original_prior(...)
      captured_priors[[length(captured_priors) + 1L]] <<- params
      params
    },
    .bsInferenceDecisionTable = function(container, n, d, N, aql, rql,
                                         log_bf, ppd, acceptanceNumber, ...) {
      captured_inference <<- list(log_bf = log_bf, ppd = ppd, c = acceptanceNumber)
      original_table(container, n, d, N, aql, rql, log_bf, ppd, acceptanceNumber, ...)
    }, .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_length(captured_priors, 2L)
  expect_equal(captured_priors[[2L]], captured_priors[[1L]])
  alpha <- captured_priors[[1L]]$alpha
  beta <- captured_priors[[1L]]$beta
  expect_equal(alpha, 1.680945757, tolerance = 1e-7)
  expect_gt(beta, 10000)
  expect_lt(abs(beta - 680946.076), .1)
  expect_equal((alpha - 1) / (alpha + beta - 2), 1e-6, tolerance = 1e-10)
  # This reference integrates the density on a rescaled unit interval; it does
  # not call the production log-tail helpers or pbeta().
  integrated_probability <- function(a, b) {
    stats::integrate(function(u) stats::dbeta(options$planningRejectableQualityLevel * u, a, b) * options$planningRejectableQualityLevel,
                     lower = 0, upper = 1, rel.tol = 1e-11, abs.tol = 1e-13,
                     subdivisions = 2000L)$value
  }
  prior_p <- integrated_probability(alpha, beta)
  plan_p <- integrated_probability(alpha, beta + 1)
  posterior_p <- integrated_probability(alpha, beta + 100)
  odds <- function(p) p / (1 - p)
  expected_plan_bf <- odds(plan_p) / odds(prior_p)
  expected_inference_bf <- odds(posterior_p) / odds(prior_p)
  expect_equal(prior_p, .5, tolerance = 1e-8)
  expect_gt(expected_plan_bf, 1)
  expect_gt(expected_inference_bf, 1)
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
  expect_equal(plan$data[[1L]]$bf_odds, expected_plan_bf, tolerance = 1e-8)
  expect_equal(exp(captured_inference$log_bf), expected_inference_bf, tolerance = 1e-8)
  expect_identical(captured_inference$c, 0L)
  expect_equal(unlist(captured_inference$ppd, use.names = FALSE), c(1, 0, 0), tolerance = 1e-12)
  expect_identical(inference$infContainer_inferenceDecisionTable$status, "complete")
  expect_match(inference$infContainer_decisionSummaryText$rawtext,
               "target of 1 has been reached", fixed = TRUE)
  plot_key <- inference$infContainer_ppdPlot$data
  plot <- result$state$figures[[plot_key]]$obj
  expect_equal(plot$data$d_tot, 0)
  expect_equal(plot$data$pmf, 1)
})
