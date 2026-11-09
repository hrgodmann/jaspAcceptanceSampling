# Tests for Bayesian State-Space Sampling

test_that("BayesianSSMforAS - analysis uses time-sorted data", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$defectCount <- "count"
  options$sampleSize <- "n"
  options$lotSize <- "N"
  options$lotNumber <- "time"
  options$covariates <- ""

  options$plotStateTimeSeries <- FALSE
  options$plotPosteriorPredictive <- FALSE
  options$plotLatestDefectRate <- FALSE
  options$plotPredictorCoefficient <- FALSE
  options$mcmcDiagnosticOverviewTable <- FALSE

  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThinning <- 1
  options$advancedMcmcSeed <- 123
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(3, 1, 2),
    n     = c(20, 20, 20),
    N     = c(100, 100, 100),
    time  = c(30, 10, 20)
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)

  expect_equal(results$status, "complete")
  expect_true("summaryTable" %in% names(results$results))
  expect_equal(results$results$summaryTable$data[[1]]$t, "30")
  expect_equal(results$results$summaryTable$data[[1]]$y, 3)
  expect_false(options$mcmcDiagnosticReport)
  expect_true("samplerDiagnostics" %in% names(results$results))
})

test_that("BayesianSSMforAS - options load", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  expect_true(is.list(options))
  expect_true("defectCount" %in% names(options))
  # An empty optional selector may be omitted from the generated options.
  expect_length(getFromNamespace(".asSSM_getPredictorNames", "jaspAcceptanceSampling")(options), 0L)
  expect_identical(options$mcmcDiagnosticReport, FALSE)
  expect_equal(options$advancedMcmcAdaptDelta, 0.97)
  expect_equal(options$advancedMcmcMaxTreeDepth, 15)
  expect_false("ssm_scaleModel" %in% names(options))
  expect_false(any(c("ssm_postHocCorrection", "ssm_postHocCorrectionWeight") %in% names(options)))
})

test_that("BayesianSSMforAS - an empty predictor selects the base model", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$defectCount <- "count"
  options$sampleSize <- "n"
  options$lotSize <- "N"
  options$lotNumber <- "time"
  options$covariates <- ""
  options$ssm_scaleModel <- "WithPredictor" # Obsolete saved selector is ignored.

  options$plotStateTimeSeries <- FALSE
  options$plotPosteriorPredictive <- FALSE
  options$plotLatestDefectRate <- FALSE
  options$plotPredictorCoefficient <- TRUE
  options$mcmcDiagnosticOverviewTable <- TRUE
  options$mcmcDiagnosticOverviewSigma <- TRUE
  options$mcmcDiagnosticOverviewBeta <- TRUE

  # Keep test runtime short while still exercising Stan sampling and result wiring.
  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThinning <- 1
  options$advancedMcmcSeed <- 123
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(1, 0, 2, 1, 1),
    n     = c(20, 20, 20, 20, 20),
    N     = c(100, 100, 100, 100, 100),
    time  = 1:5,
    temp  = c(20, 21, 19, 22, 20)
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)

  expect_equal(results$status, "complete")
  expect_true("summaryTable" %in% names(results$results))
  expect_false("betaPlot" %in% names(results$results))
  parameters <- vapply(results$results$mcmcSummary$data, function(row) row$parameter, character(1))
  expect_true("sigma_eta" %in% parameters)
  expect_false("beta_predictor" %in% parameters)

  summaryRows <- results$results$summaryTable$data
  expect_equal(length(summaryRows), 1)
  expect_true(summaryRows[[1]]$decision %in% c("Consider to Accept", "Consider to Reject", "Consider to Continue"))
  expect_true(is.numeric(summaryRows[[1]]$massAQL))
  expect_true(is.numeric(summaryRows[[1]]$massRQL))
})

test_that("BayesianSSMforAS - predictive summary is deterministic across reruns", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$defectCount <- "count"
  options$sampleSize <- "n"
  options$lotSize <- "N"
  options$lotNumber <- "time"
  options$covariates <- ""

  options$plotStateTimeSeries <- FALSE
  options$plotPosteriorPredictive <- FALSE
  options$plotLatestDefectRate <- FALSE
  options$plotPredictorCoefficient <- FALSE
  options$mcmcDiagnosticOverviewTable <- FALSE

  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThinning <- 1
  options$advancedMcmcSeed <- 123
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(1, 0, 2, 1, 1),
    n     = c(20, 20, 20, 20, 20),
    N     = c(100, 100, 100, 100, 100),
    time  = 1:5
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)

  results1 <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)
  results2 <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)

  summary1 <- results1$results$summaryTable$data[[1]]
  summary2 <- results2$results$summaryTable$data[[1]]

  expect_equal(summary1$pred, summary2$pred)
  expect_equal(summary1$massAQL, summary2$massAQL)
  expect_equal(summary1$massRQL, summary2$massRQL)
  expect_equal(summary1$decision, summary2$decision)
})

test_that("BayesianSSMforAS - invalid lot inputs fail gracefully before Stan", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  # Keep the intended numeric inputs numeric in the native test runner.
  options$defectCount <- list(value = "count", types = list("scale"))
  options$sampleSize <- list(value = "n", types = list("scale"))
  options$lotSize <- list(value = "N", types = list("scale"))
  options$lotNumber <- list(value = "time", types = list("scale"))
  options$covariates <- ""

  options$plotStateTimeSeries <- FALSE
  options$plotPosteriorPredictive <- FALSE
  options$plotLatestDefectRate <- FALSE
  options$plotPredictorCoefficient <- FALSE
  options$mcmcDiagnosticOverviewTable <- FALSE

  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThinning <- 1
  options$advancedMcmcSeed <- 123
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  invalidData <- data.frame(
    count = c(10, 1),
    n     = c(5, 5),
    N     = c(20, 20),
    time  = 1:2
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(invalidData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)

  expect_equal(results$status, "complete")
  expect_true("placeholder" %in% names(results$results))
  expect_identical(results$results$placeholder$status, "error")
  expect_match(results$results$placeholder$error$errorMessage, "defects cannot exceed")
  expect_false("summaryTable" %in% names(results$results))
})

test_that("BayesianSSMforAS - constant predictor fails gracefully before Stan", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$defectCount <- "count"
  options$sampleSize <- "n"
  options$lotSize <- "N"
  options$lotNumber <- "time"
  options$covariates <- "temp"

  options$plotStateTimeSeries <- FALSE
  options$plotPosteriorPredictive <- FALSE
  options$plotLatestDefectRate <- FALSE
  options$plotPredictorCoefficient <- FALSE
  options$mcmcDiagnosticOverviewTable <- FALSE

  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThinning <- 1
  options$advancedMcmcSeed <- 123
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  invalidData <- data.frame(
    count = c(1, 0, 2, 1, 1),
    n     = c(20, 20, 20, 20, 20),
    N     = c(100, 100, 100, 100, 100),
    time  = 1:5,
    temp  = rep(20, 5)
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(invalidData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)

  expect_equal(results$status, "complete")
  expect_true("placeholder" %in% names(results$results))
  expect_identical(results$results$placeholder$status, "error")
  expect_match(results$results$placeholder$error$errorMessage, "must vary")
  expect_false("summaryTable" %in% names(results$results))
})


test_that("BayesianSSMforAS - assigning a predictor selects the predictor model and outputs", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  options$defectCount <- "count"
  options$sampleSize <- "n"
  options$lotSize <- "N"
  options$lotNumber <- "time"
  options$covariates <- "temp"
  options$ssm_scaleModel <- "WithoutPredictor" # Obsolete saved selector is ignored.

  options$plotStateTimeSeries <- FALSE
  options$plotPosteriorPredictive <- FALSE
  options$plotLatestDefectRate <- FALSE
  options$plotPredictorCoefficient <- TRUE
  options$mcmcDiagnosticOverviewTable <- TRUE
  options$mcmcDiagnosticOverviewBeta <- TRUE

  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThinning <- 1
  options$advancedMcmcSeed <- 123
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(1, 0, 2, 1, 1), n = rep(20, 5), N = rep(100, 5),
    time = 1:5, temp = c(20, 21, 19, 22, 20)
  )
  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options, view = FALSE)

  expect_equal(results$status, "complete")
  expect_true("summaryTable" %in% names(results$results))
  expect_true("betaPlot" %in% names(results$results))
  expect_true("mcmcSummary" %in% names(results$results))
  parameters <- vapply(results$results$mcmcSummary$data, function(row) row$parameter, character(1))
  expect_true("beta_predictor[1] (temp)" %in% parameters)
})
