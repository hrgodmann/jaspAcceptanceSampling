context("Other: BaySSM")

# Generated from the saved JASP analyses, then reviewed for headless execution and completion checks.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("BayesianSSMforAS with temperature results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BaySSM.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", dataset, opts, view = FALSE)
  expect_identical(results$status, "complete")

  plotName <- results[["results"]][["predPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_posterior-predictive")

  table <- results[["results"]][["predictorSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.497402696413993, 0.846355022604599, 0.833704390306833, "temperature",
     1.24517861504467))

  plotName <- results[["results"]][["ssm_statePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_defect-rate-over-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Consider to Reject", 4e-04, 0.878333333333333, 121, 30, 8))

})

test_that("BayesianSSMforAS without predictors results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BaySSM.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", dataset, opts, view = FALSE)
  expect_identical(results$status, "complete")

  plotName <- results[["results"]][["predPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_posterior-predictive")

  plotName <- results[["results"]][["ssm_statePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_defect-rate-over-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Consider to Reject", 0.00126666666666667, 0.902733333333333,
     138, 30, 8))

})

