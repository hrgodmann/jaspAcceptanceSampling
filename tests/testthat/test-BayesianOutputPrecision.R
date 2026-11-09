test_that("Bayesian predictive plots label a fully inspected non-round count", {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$planningPriorDistribution <- "uniform"
  options$planningPlanTable <- FALSE
  options$planningPriorPlot <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$inferencePosterior <- TRUE
  options$inferenceDecisionTable <- FALSE
  options$inferencePosteriorPlot <- FALSE
  options$inferencePosteriorPredictivePlot <- TRUE
  options$inferenceSampleSize <- 100
  options$inferenceObservedDefectives <- 29
  options$inferenceLotSize <- 100

  results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                    options = options, view = FALSE, quiet = TRUE)
  expect_equal(results$status, "complete")
  plotName <- results$results$infContainer$collection$infContainer_ppdPlot$data
  plot <- results$state$figures[[plotName]]$obj
  expect_equal(plot$data$d_tot, 29)
  expect_equal(plot$data$pmf, 1)
  expect_warning(ggplot2::ggplotGrob(plot), NA)

  axis <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x
  expect_true(29 %in% axis$get_breaks())
  expect_true("29" %in% axis$get_labels())
  expect_equal(plot$scales$get_scales("x")$limits, c(28.5, 29.5))
})

test_that("Bayesian predictive plots preserve their regular count axis", {
  # Preserve the existing axis for an ordinary non-degenerate support.
  scale <- jaspAcceptanceSampling:::.bsCreatePredictiveXAxisScale(0:67)
  expect_equal(scale$breaks, seq(0, 70, by = 10))
  expect_equal(scale$limits, c(-0.5, 67.5))
})

test_that("Bayesian quality thresholds retain input precision in tables and text", {
  cases <- data.frame(
    aql = c(.0001, .000001, .123456),
    rql = c(.0002, .000002, .123457),
    aql_text = c("0.0001", "0.000001", "0.123456"),
    rql_text = c("0.0002", "0.000002", "0.123457"),
    aql_percent = c("0.01", "0.0001", "12.3456"),
    rql_percent = c("0.02", "0.0002", "12.3457")
  )
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    options <- jaspTools::analysisOptions("BayesianSampling")
    options$planningPriorDistribution <- "uniform"
    options$planningAcceptableQualityLevel <- case$aql
    options$planningRejectableQualityLevel <- case$rql
    options$planningPlanTable <- FALSE
    options$planningPriorPlot <- FALSE
    options$planningThreeHypothesisBayesFactorTable <- FALSE
    options$inferencePosterior <- TRUE
    options$inferenceDecisionTable <- TRUE
    options$inferencePosteriorPlot <- TRUE
    options$inferencePosteriorPredictivePlot <- FALSE
    options$inferenceSampleSize <- 30
    options$inferenceObservedDefectives <- 0
    options$inferenceLotSize <- 100

    results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                      options = options, view = FALSE, quiet = TRUE)
    expect_equal(results$status, "complete")
    collection <- results$results$infContainer$collection
    table <- collection$infContainer_inferenceDecisionTable$data
    labels <- vapply(table, `[[`, character(1), "col_1")
    values <- vapply(table, `[[`, character(1), "col_2")
    expect_equal(values[match("AQL", labels)], case$aql_text)
    expect_equal(values[match("RQL", labels)], case$rql_text)
    expect_match(collection$infContainer_decisionSummaryText$rawtext,
                 paste0("RQL of <b>", case$rql_text, "</b>"), fixed = TRUE)
    expect_match(collection$infContainer_bfText$rawtext,
                 paste0("< <u>", case$rql_text, "</u>"), fixed = TRUE)

    procedure <- results$results$procedureContainer$collection$procedureContainer_intro$rawtext
    expect_match(procedure, paste0("set at <b>", case$aql_percent, "%</b>"), fixed = TRUE)
    expect_match(procedure, paste0("set at <b>", case$rql_percent, "%</b>"), fixed = TRUE)
    expect_match(procedure, paste0("&ge; ", case$rql_percent, "%"), fixed = TRUE)
    expect_match(procedure, paste0("&theta; < ", case$rql_percent, "%"), fixed = TRUE)

    # These presentation changes must retain the numerical results. With a
    # uniform prior and zero defectives among 30, the posterior is Beta(1,31).
    expectedBf <- expm1(-31 * log1p(-case$rql)) * (1 - case$rql) / case$rql
    expect_equal(values[match("Bayes Factor", labels)], sprintf("%.3f", expectedBf))

    # Independent combinatorial form of the finite-lot posterior prediction:
    # P(D=k | d=0,n=30,N=100) = choose(100-k,30) / choose(101,31).
    totals <- 0:70
    pmf <- choose(100 - totals, 30) / choose(101, 31)
    acceptableQualityCountLimit <- floor(case$aql * 100)
    rejectableQualityCountLimit <- floor(case$rql * 100)
    expectedPpd <- c(sum(pmf[totals <= acceptableQualityCountLimit]),
                      sum(pmf[totals > acceptableQualityCountLimit & totals <= rejectableQualityCountLimit]),
                      sum(pmf[totals > rejectableQualityCountLimit]))
    expect_equal(values[startsWith(labels, "PPD ")], sprintf("%.3f", expectedPpd))
  }
})
