context("Other: BaySampl")

# Generated from the saved JASP analyses, then reviewed for headless execution and completion checks.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("BayesianSampling two-region unimodal results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BaySampl.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSampling", dataset, opts, view = FALSE)
  expect_identical(results$status, "complete")
  inference <- results[["results"]][["infContainer"]][["collection"]]
  planning <- results[["results"]][["planContainer"]][["collection"]]

  plotName <- inference[["infContainer_distPlotPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_posterior-distribution")

  table <- inference[["infContainer_inferenceDecisionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Sample Size (n)", 30, "Defectives (d)", 0, "Acceptance Number (c)",
     1, "AQL", 0.05, "RQL", 0.15, "Bayes Factor", 188.832, "PPD Good (<unicode> 5 defectives in lot)",
     0.889, "PPD Middle (&gt; 5 &amp; <unicode> 15 defectives)",
     0.11, "PPD Bad (&gt; 15 defectives in lot)", 0.001))

  plotName <- inference[["infContainer_ppdPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_posterior-predictive-distribution")

  plotName <- planning[["planContainer_distPlotPrior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_prior-distribution")

  table <- planning[["planContainer_planTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(33.2464697504423, 0, 19, 0.97079990996774, 0.5, 31.2231195614605,
     1, 30, 0.968966381479836, 0.5, 30.0740526357676, 2, 40, 0.967818809740672,
     0.5))

  table <- planning[["planContainer_threeBFTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(65.2004302801354, 3.71761164551976, 17.5382574881674, 0, 19, 0.0292000900322605,
     0.627462212731796, 0.343337697235943, 0.5, 0.164786992935014,
     0.335213007064986, 45.5992660145122, 1.88770206266272, 24.1559655606837,
     1, 30, 0.031033618520164, 0.4663835177251, 0.502582863754735,
     0.5, 0.164786992935014, 0.335213007064986, 32.5431086410723,
     1.12760842866762, 28.860292113571, 2, 40, 0.0321811902593277,
     0.3451549160052, 0.622663893735472, 0.5, 0.164786992935014,
     0.335213007064986))

})

test_that("BayesianSampling three-region unrestricted results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BaySampl.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSampling", dataset, opts, view = FALSE)
  expect_identical(results$status, "complete")
  inference <- results[["results"]][["infContainer"]][["collection"]]
  planning <- results[["results"]][["planContainer"]][["collection"]]

  plotName <- inference[["infContainer_distPlotPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_posterior-distribution")

  table <- inference[["infContainer_inferenceDecisionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Sample Size (n)", 30, "Defectives (d)", 0, "Acceptance Number (c)",
     0, "AQL", 0.05, "RQL", 0.15, "Bayes Factor", 259.514, "PPD Good (<unicode> 5 defectives in lot)",
     0.943, "PPD Middle (&gt; 5 &amp; <unicode> 15 defectives)",
     0.056, "PPD Bad (&gt; 15 defectives in lot)", 0))

  plotName <- inference[["infContainer_ppdPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_posterior-predictive-distribution")

  plotName <- planning[["planContainer_distPlotPrior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_prior-distribution")

  table <- planning[["planContainer_planTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(33.9867353700176, 0, 18, 0.985501672030264, 0.666666666666667,
     32.8619574712103, 1, 31, 0.985012869810428, 0.666666666666667
    ))

  table <- planning[["planContainer_threeBFTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(53.2647673371837, 3.62130949807972, 14.7087034028515, 0, 18, 0.0144983279697357,
     0.772250066086155, 0.213251605944109, 0.333333333333333, 0.333333333333333,
     0.333333333333333, 40.5568004754366, 1.61149982166775, 25.167114466984,
     1, 31, 0.0149871301895722, 0.607830048797873, 0.377182821012555,
     0.333333333333333, 0.333333333333333, 0.333333333333333))

})

