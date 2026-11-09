# Tests for Bayesian Sampling

test_that("Bayesian sampling - planning table is generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$planningPlanTable <- TRUE
  options$planningPriorPlot <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$inferencePosterior <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  planTable <- results[["results"]][["planContainer"]][["collection"]][["planContainer_planTable"]][["data"]]

  expect_true(length(planTable) >= 3)

  expect_equal(planTable[[1]]$c, 0)
  expect_equal(planTable[[1]]$n, 19)
  expect_equal(round(planTable[[1]]$prior_p, 4), 0.5)
  expect_equal(round(planTable[[1]]$post_p, 4), 0.9708)
  expect_equal(round(planTable[[1]]$bf_odds, 4), 33.2465)

  expect_equal(planTable[[2]]$c, 1)
  expect_equal(planTable[[2]]$n, 30)
  expect_equal(round(planTable[[2]]$prior_p, 4), 0.5)
  expect_equal(round(planTable[[2]]$post_p, 4), 0.9690)
  expect_equal(round(planTable[[2]]$bf_odds, 4), 31.2231)

  expect_equal(planTable[[3]]$c, 2)
  expect_equal(planTable[[3]]$n, 40)
  expect_equal(round(planTable[[3]]$prior_p, 4), 0.5)
  expect_equal(round(planTable[[3]]$post_p, 4), 0.9678)
  expect_equal(round(planTable[[3]]$bf_odds, 4), 30.0741)
})


test_that("Bayesian sampling - inference decision table is generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$planningPlanTable <- FALSE
  options$planningPriorPlot <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE

  options$inferencePosterior <- TRUE
  options$inferenceDecisionTable <- TRUE
  options$inferencePosteriorPlot <- FALSE
  options$inferencePosteriorPredictivePlot <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  infTable <- results[["results"]][["infContainer"]][["collection"]][["infContainer_inferenceDecisionTable"]][["data"]]

  expect_equal(length(infTable), 9)
  jaspTools::expect_equal_tables(infTable[[1]], list("Sample Size (n)", "30"))
  jaspTools::expect_equal_tables(infTable[[2]], list("Defectives (d)", "0"))
  jaspTools::expect_equal_tables(infTable[[3]], list("Acceptance Number (c)", "1"))
  expect_equal(infTable[[4]]$col_1, "AQL")
  expect_equal(as.numeric(infTable[[4]]$col_2), 0.05)
  expect_equal(infTable[[5]]$col_1, "RQL")
  expect_equal(as.numeric(infTable[[5]]$col_2), 0.15)
  expect_equal(infTable[[6]]$col_1, "Bayes Factor")
  expect_equal(as.numeric(infTable[[6]]$col_2), 188.832, tolerance = 1e-6)
  expect_equal(as.numeric(infTable[[7]]$col_2), 0.889, tolerance = 1e-6)
  expect_equal(as.numeric(infTable[[8]]$col_2), 0.110, tolerance = 1e-6)
  expect_equal(as.numeric(infTable[[9]]$col_2), 0.001, tolerance = 1e-6)
})

test_that("Bayesian sampling - invalid impartial prior fails gracefully", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$planningPlanTable <- TRUE
  options$planningPriorPlot <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$inferencePosterior <- FALSE

  options$planningAcceptableQualityLevel <- 0.5
  options$planningRejectableQualityLevel <- 0.9
  options$planningPriorDistribution <- "impartial"
  options$planningPriorImpartialCustomLocation <- TRUE
  options$planningPriorImpartialLocation <- 0.8

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  expect_equal(results$status, "complete")
  expect_true("planContainer" %in% names(results$results))
  expect_false("planData" %in% names(results$results))
})

test_that("Bayesian sampling - extreme custom prior values do not crash plan generation", {
  plans <- jaspAcceptanceSampling:::.bsGeneratePlans(
    aql = 0.05,
    rql = 0.10,
    maximumSampleSize = 5,
    minimumBayesFactor = 30,
    alpha = 100000,
    beta = 100000
  )

  expect_true(is.data.frame(plans))
})

test_that("Bayesian sampling - uniform prior ignores stale custom mode values", {
  priorParams <- jaspAcceptanceSampling:::.bsCalculatePriorParameters(
    prior = "uniform",
    aql = 0.05,
    rql = 0.10,
    impartialCustomLocation = TRUE,
    impartialLocation = 0.50
  )

  expect_equal(priorParams$alpha, 1)
  expect_equal(priorParams$beta, 1)
})

test_that("Bayesian sampling - posterior predictive x-axis covers full support", {
  xScale <- jaspAcceptanceSampling:::.bsCreatePredictiveXAxisScale(0:67)

  expect_true(all(diff(xScale$breaks) >= 0))
  expect_lte(xScale$limits[1], 0)
  expect_gte(xScale$limits[2], 67)
})

test_that("Bayesian sampling - y-axis scale covers the full plotted range", {
  yScale <- jaspAcceptanceSampling:::.bsCreateYAxisScale(c(0, 12.5))

  expect_true(all(diff(yScale$breaks) >= 0))
  expect_equal(yScale$limits[1], 0)
  expect_gte(yScale$limits[2], 12.5)
  expect_lt(yScale$label_y, yScale$limits[2])
})

test_that("Bayesian sampling - off-range predictive thresholds are hidden", {
  expect_null(
    jaspAcceptanceSampling:::.bsCreateVisibleThreshold(
      x = 10,
      label = "AQL",
      side = "left",
      lower = 39.5,
      upper = 95.5,
      minimumOffset = 0.75
    )
  )
})

test_that("Bayesian sampling - errored containers are refreshed before reuse", {
  staleContainer <- list(getError = function() TRUE)

  refreshed <- jaspAcceptanceSampling:::.bsReuseOrRefreshContainer(
    staleContainer,
    "Planning",
    jaspAcceptanceSampling:::.bsGetPlanningDependencies()
  )

  cleanContainer <- list(getError = function() FALSE)
  reused <- jaspAcceptanceSampling:::.bsReuseOrRefreshContainer(
    cleanContainer,
    "Planning",
    jaspAcceptanceSampling:::.bsGetPlanningDependencies()
  )

  expect_false(identical(refreshed, staleContainer))
  expect_identical(reused, cleanContainer)
})

test_that("Bayesian sampling - plot outputs are generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$planningPlanTable <- FALSE
  options$planningPriorPlot <- TRUE
  options$planningThreeHypothesisBayesFactorTable <- FALSE

  options$inferencePosterior <- TRUE
  options$inferenceDecisionTable <- FALSE
  options$inferencePosteriorPlot <- TRUE
  options$inferencePosteriorPredictivePlot <- TRUE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  priorPlotName <- results[["results"]][["planContainer"]][["collection"]][["planContainer_distPlotPrior"]][["data"]]
  posteriorPlotName <- results[["results"]][["infContainer"]][["collection"]][["infContainer_distPlotPosterior"]][["data"]]
  ppdPlotName <- results[["results"]][["infContainer"]][["collection"]][["infContainer_ppdPlot"]][["data"]]

  expect_true(priorPlotName %in% names(results[["state"]][["figures"]]))
  expect_true(posteriorPlotName %in% names(results[["state"]][["figures"]]))
  expect_true(ppdPlotName %in% names(results[["state"]][["figures"]]))
})

test_that("Bayesian sampling - three-hypothesis probabilities remain a valid partition", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$planningPlanTable <- FALSE
  options$planningPriorPlot <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- TRUE
  options$inferencePosterior <- FALSE
  options$planningPriorDistribution <- "impartialThreeRegion"
  options$planningAcceptableQualityLevel <- 0.05
  options$planningRejectableQualityLevel <- 0.15

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  expect_equal(results$status, "complete")

  threeBfTable <- results[["results"]][["planContainer"]][["collection"]][["planContainer_threeBFTable"]][["data"]]
  expect_true(length(threeBfTable) >= 1)

  firstRow <- threeBfTable[[1]]
  expect_equal(
    as.numeric(firstRow$prior_good) + as.numeric(firstRow$prior_middle) + as.numeric(firstRow$prior_bad),
    1,
    tolerance = 1e-10
  )
  expect_equal(
    as.numeric(firstRow$post_good) + as.numeric(firstRow$post_middle) + as.numeric(firstRow$post_bad),
    1,
    tolerance = 1e-10
  )
  expect_true(is.finite(as.numeric(firstRow$BF_GB)))
  expect_true(is.finite(as.numeric(firstRow$BF_GM)))
  expect_true(is.finite(as.numeric(firstRow$BF_MB)))
})
