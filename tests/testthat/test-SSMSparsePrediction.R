test_that("sparse empirical support preserves dense-reference masses and lower discrete medians", {
  pmf <- getFromNamespace(".asSSM_computePredictivePmf", "jaspAcceptanceSampling")
  below <- getFromNamespace(".asSSM_computePmfMassBelow", "jaspAcceptanceSampling")
  above <- getFromNamespace(".asSSM_computePmfMassAbove", "jaspAcceptanceSampling")
  median <- getFromNamespace(".asSSM_computePmfMedian", "jaspAcceptanceSampling")
  cases <- list(
    list(draws = c(9, 2, 9, 2), s = 10, n = 0, y = 0),
    list(draws = c(6, 4, 5, 5, 4, 6, 6), s = 10, n = 8, y = 4),
    list(draws = c(0, 20, 0, 1, 20), s = 20, n = 0, y = 0),
    list(draws = rep(4, 3), s = 20, n = 20, y = 4),
    list(draws = c(0, 0), s = 0, n = 0, y = 0))
  for (case in cases) {
    actual <- pmf(case$draws, case$s, case$n, case$y)
    dense <- data.frame(defects = 0:case$s,
      prob = tabulate(case$draws + 1L, nbins = case$s + 1L) / length(case$draws))
    positive <- dense[dense$prob > 0, , drop = FALSE]
    rownames(positive) <- NULL
    expect_equal(actual, positive)
    expect_equal(median(actual), dense$defects[which(cumsum(dense$prob) >= .5)[1L]])
    expect_equal(median(actual), sort(case$draws)[ceiling(length(case$draws) / 2)])
    # Include exact integer thresholds, fractional thresholds, and both exterior tails.
    for (threshold in seq(-.5, case$s + .5, by = .5)) {
      expect_equal(below(actual, threshold), sum(dense$prob[dense$defects < threshold]))
      expect_equal(above(actual, threshold), sum(dense$prob[dense$defects > threshold]))
      expect_equal(below(actual, threshold), mean(case$draws < threshold))
      expect_equal(above(actual, threshold), mean(case$draws > threshold))
    }
  }
})

test_that("large allowed lots use memory proportional to observed predictive support", {
  pmf <- getFromNamespace(".asSSM_computePredictivePmf", "jaspAcceptanceSampling")
  below <- getFromNamespace(".asSSM_computePmfMassBelow", "jaspAcceptanceSampling")
  above <- getFromNamespace(".asSSM_computePmfMassAbove", "jaspAcceptanceSampling")
  median <- getFromNamespace(".asSSM_computePmfMedian", "jaspAcceptanceSampling")
  # Fail safely before the large case if dense allocation is accidentally restored.
  if (nrow(pmf(c(0, 1), 100, 0, 0)) != 2L)
    stop("Sparse support is required before testing the maximum allowed lot size.")
  largest <- .Machine$integer.max - 1L
  draws <- c(largest, 4L, 4L, largest - 1L)
  actual <- pmf(draws, sT = largest, nT = 4L, yT = 4L)
  expect_identical(actual$defects, c(4L, largest - 1L, largest))
  expect_equal(actual$prob, c(.5, .25, .25))
  expect_lt(as.numeric(utils::object.size(actual)), 4096)
  expect_equal(median(actual), 4)
  expect_equal(below(actual, largest), .75)
  expect_equal(above(actual, largest), 0)
  expect_equal(above(actual, largest - 1L), .25)
  census <- pmf(rep(largest - 1L, 3L), largest, largest, largest - 1L)
  expect_equal(nrow(census), 1L)
  expect_equal(census$defects, largest - 1L)
  expect_equal(census$prob, 1)
  expect_equal(median(census), largest - 1L)
})

test_that("predictive plots retain empty bins and exact weights for sparse and census support", {
  ns <- asNamespace("jaspAcceptanceSampling")
  fixture <- new.env(parent = emptyenv())
  fixture$draws <- NULL
  fitFixture <- function(jaspResults, dataset, options) {
    error <- get(".asSSM_validateInputs", ns)(dataset, options)
    if (!is.null(error)) {
      get(".asSSM_setAnalysisError", ns)(jaspResults, error)
      return()
    }
    state <- jaspBase::createJaspState()
    state$dependOn(get(".asSSM_getFitDependencies", ns)())
    jaspResults[["fit"]] <- state
    jaspResults[["fit"]]$object <- list(samples = list(y_pred = fixture$draws))
  }
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fitFixture,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("Predictive plotting fixtures must not load Stan."),
    .package = "jaspAcceptanceSampling")
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(defectCount = "y", sampleSize = "n", lotSize = "s",
                      lotNumber = "lot", covariates = character())
  # Specify Scale for the numeric fixture columns.
  options[names(assignments)] <- lapply(assignments, function(columns)
    if (length(columns)) list(value = columns, types = as.list(rep("scale", length(columns)))) else columns)
  for (name in c("plotStateTimeSeries", "plotLatestDefectRate", "plotPredictorCoefficient",
                 "mcmcDiagnosticOverviewTable", "mcmcDiagnosticReport"))
    options[[name]] <- FALSE
  options$plotPosteriorPredictive <- TRUE
  options$settingsAcceptableQualityLevel <- .025
  options$settingsRejectableQualityLevel <- .1
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  if (nrow(get(".asSSM_computePredictivePmf", ns)(c(0, 1), 100, 0, 0)) != 2L)
    stop("Sparse support is required before plotting the maximum allowed lot size.")
  largest <- .Machine$integer.max - 1L
  cases <- list(
    list(draws = c(2, 2, 50, 98), s = 100, n = 0, y = 0),
    list(draws = c(0, largest, largest), s = largest, n = 0, y = 0),
    list(draws = rep(7, 4), s = 100, n = 100, y = 7))
  for (case in cases) {
    fixture$draws <- case$draws
    utils::write.csv(data.frame(y = case$y, n = case$n, s = case$s, lot = 1),
                      csv, row.names = FALSE)
    result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)
    expect_identical(result$status, "complete")
    expect_null(result$results$predPlot[["error"]])
    plot <- result$state$figures[[result$results$predPlot$data]]$obj
    expect_s3_class(plot, "ggplot")
    bars <- plot$data
    expect_lte(nrow(bars), 30L)
    expect_gte(nrow(bars), 1L)
    expect_true(all(is.finite(bars$prob) & bars$prob >= 0))
    expect_true(any(bars$prob == 0))
    expect_equal(sum(bars$prob), 1)
    # Sum the raw draws independently over each rendered bar's integer support.
    built <- ggplot2::ggplot_build(plot)$data[[1L]]
    expect_true(all(is.finite(built$xmin) & is.finite(built$xmax)))
    widths <- built$xmax - built$xmin
    expect_equal(widths, round(widths))
    expect_equal(built$xmin %% 1, rep(.5, nrow(built)))
    expect_equal(built$xmax %% 1, rep(.5, nrow(built)))
    expected <- vapply(seq_len(nrow(built)), function(i)
      mean(case$draws > built$xmin[i] & case$draws < built$xmax[i]), numeric(1))
    expect_equal(bars$prob, expected)
    expect_warning(ggplot2::ggplotGrob(plot), NA)
    row <- result$results$summaryTable$data[[1L]]
    expect_equal(row$pred, sort(case$draws)[ceiling(length(case$draws) / 2)])
    expect_equal(row$massAQL, mean(case$draws < .025 * case$s))
    expect_equal(row$massRQL, mean(case$draws > .1 * case$s))
  }
})
