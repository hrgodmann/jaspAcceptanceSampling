# Invalid inputs must produce visible errors and recover in the real result engine.
.bsInputErrorOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$planningPlanTable <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$planningPriorPlot <- FALSE
  options$inferencePosterior <- TRUE
  options$inferenceDecisionTable <- TRUE
  options$inferencePosteriorPlot <- TRUE
  options$inferencePosteriorPredictivePlot <- TRUE
  options
}

.bsInputErrorCollectionForTest <- function(result) {
  result$results$infContainer$collection
}

.bsExpectInputErrorForTest <- function(collection, message) {
  error <- collection$infContainer_inputError
  expect_identical(error$status, "error")
  expect_match(paste(error$error$errorMessage, collapse = ""), message, fixed = TRUE)
  for (key in c("inferenceDecisionTable", "decisionSummaryText", "distPlotPosterior", "bfText", "ppdPlot", "priorError", "numericalError"))
    expect_null(collection[[paste0("infContainer_", key)]])
}

.bsInputErrorSequenceForTest <- function(options, changes) {
  original <- get(".bsRunInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  observed <- list()
  wrapper <- function(jaspResults, options, position) {
    private <- jaspResults$.__enclos_env__$private
    capture <- function() {
      private$complete()
      jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
    }
    original(jaspResults, options, position)
    observed[[1L]] <<- capture()
    for (i in seq_along(changes)) {
      options[names(changes[[i]])] <- changes[[i]]
      private$changeOptions(as.character(jsonlite::toJSON(options, digits = NA)))
      jaspBase:::emptyRecomputed()
      jaspAcceptanceSampling:::.bsCreateProcedure(jaspResults, options, 0)
      jaspAcceptanceSampling:::.bsRunPlanningAnalysis(jaspResults, options, 1)
      original(jaspResults, options, position)
      observed[[i + 1L]] <<- capture()
    }
  }
  result <- testthat::with_mocked_bindings(
    jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
    .bsRunInferenceAnalysis = wrapper, .package = "jaspAcceptanceSampling")
  expect_identical(result$status, "complete")
  expect_length(observed, length(changes) + 1L)
  observed
}

test_that("Bayesian sampling - invalid counts and BF targets are rejected by the QML input bounds", {
  cases <- c(inferenceSampleSize = -1, inferenceObservedDefectives = -1, planningMinimumBayesFactor = 0)
  for (option in names(cases)) {
    options <- .bsInputErrorOptionsForTest()
    options[[option]] <- unname(cases[[option]])
    expect_error(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      paste0(option, ": The value must be"), fixed = TRUE)
  }
})

test_that("Bayesian sampling - inconsistent data and lot sizes show visible errors", {
  cases <- list(
    list(change = list(inferenceObservedDefectives = 31), message = "The observed number of defectives (d) cannot exceed the sample size (n)"),
    list(change = list(inferenceLotSize = 29), message = "Lot size (N) must be at least"))
  for (case in cases) {
    options <- .bsInputErrorOptionsForTest()
    options[names(case$change)] <- case$change
    result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
    expect_identical(result$status, "complete")
    .bsExpectInputErrorForTest(.bsInputErrorCollectionForTest(result), case$message)
  }
})

test_that("Bayesian sampling - corrected inputs replace errors with current tables, HTML, and plots", {
  options <- .bsInputErrorOptionsForTest()
  observed <- .bsInputErrorSequenceForTest(options, list(
    list(inferenceObservedDefectives = 31),
    list(inferenceSampleSize = 40, inferenceObservedDefectives = 2),
    list(inferenceLotSize = 39),
    list(inferenceLotSize = 150),
    list(planningMinimumBayesFactor = 0),
    list(planningMinimumBayesFactor = 10)))
  messages <- c("The observed number of defectives (d) cannot exceed the sample size (n)", "Lot size (N) must be at least", "minimum Bayes factor (> 0)")
  for (j in seq_along(messages))
    .bsExpectInputErrorForTest(.bsInputErrorCollectionForTest(observed[[2L * j]]), messages[j])
  for (i in c(1L, 3L, 5L, 7L)) {
    collection <- .bsInputErrorCollectionForTest(observed[[i]])
    for (error in c("inputError", "priorError", "numericalError"))
      expect_null(collection[[paste0("infContainer_", error)]])
    for (key in c("inferenceDecisionTable", "distPlotPosterior", "ppdPlot")) {
      output <- collection[[paste0("infContainer_", key)]]
      expect_identical(output$status, "complete")
      expect_null(output[["error"]])
    }
    for (key in c("decisionSummaryText", "bfText")) {
      output <- collection[[paste0("infContainer_", key)]]
      expect_gt(nchar(paste(output$rawtext, collapse = "")), 0)
      expect_null(output[["error"]])
    }
  }
  # Recovery must represent the new data, rather than merely remove the error banner.
  options$inferenceSampleSize <- 40
  options$inferenceObservedDefectives <- 2
  options$inferenceLotSize <- 150
  options$planningMinimumBayesFactor <- 10
  fresh <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(fresh$status, "complete")
  final <- .bsInputErrorCollectionForTest(observed[[7L]])
  reference <- .bsInputErrorCollectionForTest(fresh)
  expect_equal(final$infContainer_inferenceDecisionTable$data, reference$infContainer_inferenceDecisionTable$data)
  expect_false(identical(final$infContainer_inferenceDecisionTable$data,
                         .bsInputErrorCollectionForTest(observed[[1L]])$infContainer_inferenceDecisionTable$data))
  for (key in c("infContainer_decisionSummaryText", "infContainer_bfText"))
    expect_equal(final[[key]]$rawtext, reference[[key]]$rawtext)
  for (key in c("infContainer_distPlotPosterior", "infContainer_ppdPlot"))
    expect_false(identical(final[[key]]$data, .bsInputErrorCollectionForTest(observed[[1L]])[[key]]$data))
})

test_that("Bayesian sampling - changing between input and prior errors removes the previous message", {
  options <- .bsInputErrorOptionsForTest()
  options$inferenceObservedDefectives <- 31
  observed <- .bsInputErrorSequenceForTest(options, list(
    list(planningAcceptableQualityLevel = .2),
    list(planningAcceptableQualityLevel = .05),
    list(inferenceObservedDefectives = 0)))
  for (i in c(1L, 3L))
    .bsExpectInputErrorForTest(.bsInputErrorCollectionForTest(observed[[i]]), "The observed number of defectives (d) cannot exceed the sample size (n)")
  priorFailure <- .bsInputErrorCollectionForTest(observed[[2L]])
  expect_null(priorFailure$infContainer_inputError)
  expect_null(priorFailure$infContainer_numericalError)
  expect_identical(priorFailure$infContainer_priorError$status, "error")
  expect_match(paste(priorFailure$infContainer_priorError$error$errorMessage, collapse = ""),
               "0 < AQL < RQL < 1", fixed = TRUE)
  for (key in c("inferenceDecisionTable", "decisionSummaryText", "distPlotPosterior", "bfText", "ppdPlot"))
    expect_null(priorFailure[[paste0("infContainer_", key)]])
  recovered <- .bsInputErrorCollectionForTest(observed[[4L]])
  for (key in c("inputError", "priorError", "numericalError"))
    expect_null(recovered[[paste0("infContainer_", key)]])
  expect_identical(recovered$infContainer_inferenceDecisionTable$status, "complete")
  expect_identical(recovered$infContainer_distPlotPosterior$status, "complete")
  expect_identical(recovered$infContainer_ppdPlot$status, "complete")
})

test_that("Bayesian sampling - legacy parent-container errors refresh without locked-state failures", {
  for (section in c("inference", "planning")) {
    builderName <- if (section == "inference") ".bsRunInferenceAnalysis" else ".bsRunPlanningAnalysis"
    containerName <- if (section == "inference") "infContainer" else "planContainer"
    original <- get(builderName, asNamespace("jaspAcceptanceSampling"))
    observed <- list()
    wrapper <- function(jaspResults, options, position) {
      private <- jaspResults$.__enclos_env__$private
      capture <- function() {
        private$complete()
        jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      }
      original(jaspResults, options, position)
      observed[[1L]] <<- capture()
      # Reproduce the old implementation's error on the actual existing container.
      jaspResults[[containerName]]$setError("Legacy input error")
      observed[[2L]] <<- capture()
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      observed[[3L]] <<- capture()
    }
    options <- .bsInputErrorOptionsForTest()
    options$planningPlanTable <- section == "planning"
    options$planningThreeHypothesisBayesFactorTable <- section == "planning"
    options$planningPriorPlot <- section == "planning"
    bindings <- setNames(list(wrapper), builderName)
    result <- do.call(testthat::with_mocked_bindings, c(list(
      quote(jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE))),
      bindings, list(.package = "jaspAcceptanceSampling")))
    expect_identical(result$status, "complete")
    expect_length(observed, 3L)
    before <- observed[[1L]]$results[[containerName]]$collection
    broken <- observed[[2L]]$results[[containerName]]$collection
    recovered <- observed[[3L]]$results[[containerName]]$collection
    tableKey <- paste0(containerName, if (section == "planning") "_planTable" else "_inferenceDecisionTable")
    expect_identical(broken[[tableKey]]$status, "error")
    expect_match(paste(broken[[tableKey]]$error$errorMessage, collapse = ""), "Legacy input error", fixed = TRUE)
    expect_identical(recovered[[tableKey]]$status, "complete")
    expect_null(recovered[[tableKey]][["error"]])
    expect_equal(recovered[[tableKey]]$data, before[[tableKey]]$data)
    for (output in recovered) expect_null(output[["error"]])
    if (section == "planning") {
      expect_identical(recovered$planContainer_distPlotPrior$status, "complete")
      expect_equal(recovered$planContainer_threeBFTable$data, before$planContainer_threeBFTable$data)
    } else {
      expect_identical(recovered$infContainer_distPlotPosterior$status, "complete")
      expect_identical(recovered$infContainer_ppdPlot$status, "complete")
      expect_equal(recovered$infContainer_decisionSummaryText$rawtext, before$infContainer_decisionSummaryText$rawtext)
    }
  }
})

test_that("Bayesian sampling - inconsistent counts recover by correcting either entered value", {
  options <- .bsInputErrorOptionsForTest()
  options$planningPriorDistribution <- "uniform"
  options$inferenceSampleSize <- 100
  options$inferenceObservedDefectives <- 29
  options$inferenceLotSize <- 200
  observed <- .bsInputErrorSequenceForTest(options, list(
    list(inferenceObservedDefectives = 101),
    list(inferenceObservedDefectives = 29),
    list(inferenceObservedDefectives = 101),
    list(inferenceSampleSize = 101),
    list(inferenceLotSize = 100),
    list(inferenceLotSize = 200),
    list(inferenceSampleSize = 100),
    list(inferenceSampleSize = 101)))
  for (i in c(2L, 4L, 8L)) {
    .bsExpectInputErrorForTest(.bsInputErrorCollectionForTest(observed[[i]]),
      "The observed number of defectives (d) cannot exceed the sample size (n).")
  }
  .bsExpectInputErrorForTest(.bsInputErrorCollectionForTest(observed[[6L]]),
    "Lot size (N) must be at least as large as the sample size (n).")
  for (i in c(3L, 5L, 7L, 9L)) {
    collection <- .bsInputErrorCollectionForTest(observed[[i]])
    for (key in c("inputError", "priorError", "numericalError"))
      expect_null(collection[[paste0("infContainer_", key)]])
    for (key in c("inferenceDecisionTable", "distPlotPosterior", "ppdPlot")) {
      output <- collection[[paste0("infContainer_", key)]]
      expect_identical(output$status, "complete")
      expect_null(output[["error"]])
    }
    for (key in c("decisionSummaryText", "bfText"))
      expect_gt(nchar(paste(collection[[paste0("infContainer_", key)]]$rawtext, collapse = "")), 0)
  }
  # Lowering d restores the original data; raising n preserves the entered d=101.
  expect_equal(.bsInputErrorCollectionForTest(observed[[3L]])$infContainer_inferenceDecisionTable$data,
               .bsInputErrorCollectionForTest(observed[[1L]])$infContainer_inferenceDecisionTable$data)
  options$inferenceSampleSize <- 101
  options$inferenceObservedDefectives <- 101
  fresh <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(fresh$status, "complete")
  expected <- .bsInputErrorCollectionForTest(fresh)
  for (i in c(5L, 7L, 9L)) {
    collection <- .bsInputErrorCollectionForTest(observed[[i]])
    expect_equal(collection$infContainer_inferenceDecisionTable$data,
                 expected$infContainer_inferenceDecisionTable$data)
    expect_equal(collection$infContainer_decisionSummaryText$rawtext,
                 expected$infContainer_decisionSummaryText$rawtext)
  }
})
