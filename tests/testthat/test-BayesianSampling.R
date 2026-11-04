# Tests for Bayesian Sampling

test_that("Bayesian sampling - planning table is generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$showPlansplan <- TRUE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$inferPosteriorinfer <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  plan_table <- results[["results"]][["planContainer"]][["collection"]][["planContainer_planTable"]][["data"]]

  expect_true(length(plan_table) >= 3)

  expect_equal(plan_table[[1]]$c, 0)
  expect_equal(plan_table[[1]]$n, 19)
  expect_equal(round(plan_table[[1]]$prior_p, 4), 0.5)
  expect_equal(round(plan_table[[1]]$post_p, 4), 0.9708)
  expect_equal(round(plan_table[[1]]$bf_odds, 4), 33.2465)

  expect_equal(plan_table[[2]]$c, 1)
  expect_equal(plan_table[[2]]$n, 30)
  expect_equal(round(plan_table[[2]]$prior_p, 4), 0.5)
  expect_equal(round(plan_table[[2]]$post_p, 4), 0.9690)
  expect_equal(round(plan_table[[2]]$bf_odds, 4), 31.2231)

  expect_equal(plan_table[[3]]$c, 2)
  expect_equal(plan_table[[3]]$n, 40)
  expect_equal(round(plan_table[[3]]$prior_p, 4), 0.5)
  expect_equal(round(plan_table[[3]]$post_p, 4), 0.9678)
  expect_equal(round(plan_table[[3]]$bf_odds, 4), 30.0741)
})


test_that("Bayesian sampling - inference decision table is generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$showPlansplan <- FALSE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE

  options$inferPosteriorinfer <- TRUE
  options$showInferenceTableinfer <- TRUE
  options$posteriorPlotinfer <- FALSE
  options$ppdPlotinfer <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  inf_table <- results[["results"]][["infContainer"]][["collection"]][["infContainer_inferenceDecisionTable"]][["data"]]

  expect_equal(length(inf_table), 9)
  jaspTools::expect_equal_tables(inf_table[[1]], list("Sample Size (n)", "30"))
  jaspTools::expect_equal_tables(inf_table[[2]], list("Defectives (d)", "0"))
  jaspTools::expect_equal_tables(inf_table[[3]], list("Acceptance number (c)", "1"))
  expect_equal(inf_table[[4]]$col_1, "AQL")
  expect_equal(as.numeric(inf_table[[4]]$col_2), 0.05)
  expect_equal(inf_table[[5]]$col_1, "RQL")
  expect_equal(as.numeric(inf_table[[5]]$col_2), 0.15)
  expect_equal(inf_table[[6]]$col_1, "Bayes Factor")
  expect_equal(as.numeric(inf_table[[6]]$col_2), 188.832, tolerance = 1e-6)
  expect_equal(as.numeric(inf_table[[7]]$col_2), 0.889, tolerance = 1e-6)
  expect_equal(as.numeric(inf_table[[8]]$col_2), 0.110, tolerance = 1e-6)
  expect_equal(as.numeric(inf_table[[9]]$col_2), 0.001, tolerance = 1e-6)
})

test_that("Bayesian sampling - invalid impartial prior fails gracefully", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$showPlansplan <- TRUE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$inferPosteriorinfer <- FALSE

  options$aqlplan <- 0.5
  options$rqlplan <- 0.9
  options$priorplan <- "impartial"
  options$impartialCustomModeplan <- TRUE
  options$impartialModeplan <- 0.8

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  expect_equal(results$status, "complete")
  expect_true("planContainer" %in% names(results$results))
  expect_false("planData" %in% names(results$results))
})

test_that("Bayesian sampling - extreme custom prior values do not crash plan generation", {
  plans <- jaspAcceptanceSampling:::.bsGeneratePlans(
    aql = 0.05,
    rql = 0.10,
    max_n = 5,
    min_bf = 30,
    alpha = 100000,
    beta = 100000
  )

  expect_true(is.data.frame(plans))
})

test_that("Bayesian sampling - uniform prior ignores stale custom mode values", {
  priorParams <- jaspAcceptanceSampling:::.bsCalculatePriorParams(
    prior = "uniform",
    aql = 0.05,
    rql = 0.10,
    impartialCustomMode = TRUE,
    impartialMode = 0.50
  )

  expect_equal(priorParams$alpha, 1)
  expect_equal(priorParams$beta, 1)
})

test_that("Bayesian sampling - posterior predictive x-axis covers full support", {
  xScale <- jaspAcceptanceSampling:::.bsPPDXScale(0:67)

  expect_true(all(diff(xScale$breaks) >= 0))
  expect_lte(xScale$limits[1], 0)
  expect_gte(xScale$limits[2], 67)
})

test_that("Bayesian sampling - y-axis scale covers the full plotted range", {
  yScale <- jaspAcceptanceSampling:::.bsYScale(c(0, 12.5))

  expect_true(all(diff(yScale$breaks) >= 0))
  expect_equal(yScale$limits[1], 0)
  expect_gte(yScale$limits[2], 12.5)
  expect_lt(yScale$label_y, yScale$limits[2])
})

test_that("Bayesian sampling - off-range predictive thresholds are hidden", {
  expect_null(
    jaspAcceptanceSampling:::.bsVisibleThreshold(
      x = 10,
      label = "AQL",
      side = "left",
      lower = 39.5,
      upper = 95.5,
      min_offset = 0.75
    )
  )
})

test_that("Bayesian sampling - errored containers are refreshed before reuse", {
  staleContainer <- list(getError = function() TRUE)

  refreshed <- jaspAcceptanceSampling:::.bsReuseOrRefreshContainer(
    staleContainer,
    "Planning",
    jaspAcceptanceSampling:::.bsPlanningDeps()
  )

  cleanContainer <- list(getError = function() FALSE)
  reused <- jaspAcceptanceSampling:::.bsReuseOrRefreshContainer(
    cleanContainer,
    "Planning",
    jaspAcceptanceSampling:::.bsPlanningDeps()
  )

  expect_false(identical(refreshed, staleContainer))
  expect_identical(reused, cleanContainer)
})

test_that("Bayesian sampling - plot outputs are generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$showPlansplan <- FALSE
  options$priorPlotplan <- TRUE
  options$showThreeBFplan <- FALSE

  options$inferPosteriorinfer <- TRUE
  options$showInferenceTableinfer <- FALSE
  options$posteriorPlotinfer <- TRUE
  options$ppdPlotinfer <- TRUE

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

  options$showPlansplan <- FALSE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- TRUE
  options$inferPosteriorinfer <- FALSE
  options$priorplan <- "impartial_three"
  options$aqlplan <- 0.05
  options$rqlplan <- 0.15

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  expect_equal(results$status, "complete")

  three_bf_table <- results[["results"]][["planContainer"]][["collection"]][["planContainer_threeBFTable"]][["data"]]
  expect_true(length(three_bf_table) >= 1)

  first_row <- three_bf_table[[1]]
  expect_equal(
    as.numeric(first_row$prior_good) + as.numeric(first_row$prior_middle) + as.numeric(first_row$prior_bad),
    1,
    tolerance = 1e-10
  )
  expect_equal(
    as.numeric(first_row$post_good) + as.numeric(first_row$post_middle) + as.numeric(first_row$post_bad),
    1,
    tolerance = 1e-10
  )
  expect_true(is.finite(as.numeric(first_row$BF_GB)))
  expect_true(is.finite(as.numeric(first_row$BF_GM)))
  expect_true(is.finite(as.numeric(first_row$BF_MB)))
})
