.ssmPredictiveBinPlot <- function(draws, lotSize, inspected = 0, defects = 0) {
  ns <- asNamespace("jaspAcceptanceSampling")
  fitFixture <- function(jaspResults, dataset, options) {
    state <- jaspBase::createJaspState()
    state$dependOn(get(".asSSM_getFitDependencies", ns)())
    jaspResults[["fit"]] <- state
    state$object <- list(samples = list(y_pred = draws))
  }
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fitFixture,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("Predictive bin fixtures must not load Stan."),
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
  options$settingsAcceptableQualityLevel <- .05
  options$settingsRejectableQualityLevel <- .1
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(y = defects, n = inspected, s = lotSize, lot = 1),
                    csv, row.names = FALSE)
  result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_null(result$results$predPlot[["error"]])
  plot <- result$state$figures[[result$results$predPlot$data]]$obj
  expect_s3_class(plot, "ggplot")
  plot
}

test_that("short predictive ranges show one complete bar per integer count", {
  draws <- rep(0:20, each = 100)
  plot <- .ssmPredictiveBinPlot(draws, 60)
  bars <- ggplot2::ggplot_build(plot)$data[[1L]]
  expect_equal(bars$x, 0:20)
  expect_equal(bars$y, rep(1 / 21, 21))
  expect_equal(bars$xmin, (0:20) - .5)
  expect_equal(bars$xmax, (0:20) + .5)
  expect_warning(ggplot2::ggplotGrob(plot), NA)
})

test_that("grouped predictive bins represent equal numbers of counts without artificial peaks", {
  # Stratified quantiles provide a smooth binomial PMF without simulation noise.
  draws <- stats::qbinom((seq_len(20000) - .5) / 20000, 400, .05)
  plot <- .ssmPredictiveBinPlot(draws, 400)
  bars <- ggplot2::ggplot_build(plot)$data[[1L]]
  expect_lte(nrow(bars), 30L)
  widths <- bars$xmax - bars$xmin
  expect_true(all(is.finite(widths)))
  expect_true(all(widths == widths[1L] & widths >= 1 & widths == round(widths)))
  expect_equal(bars$xmin %% 1, rep(.5, nrow(bars)))
  expect_equal(bars$xmax %% 1, rep(.5, nrow(bars)))
  expected <- vapply(seq_len(nrow(bars)), function(i)
    mean(draws > bars$xmin[i] & draws < bars$xmax[i]), numeric(1))
  expect_equal(bars$y, expected)
  peak <- which.max(bars$y)
  expect_true(all(diff(bars$y[seq_len(peak)]) >= -1e-12))
  expect_true(all(diff(bars$y[seq.int(peak, nrow(bars))]) <= 1e-12))
  expect_warning(ggplot2::ggplotGrob(plot), NA)
})

test_that("displayed predictive mass is not renormalized when extreme tails are outside the plot", {
  draws <- c(rep(20, 9999), 1000)
  plot <- .ssmPredictiveBinPlot(draws, 1000)
  bars <- ggplot2::ggplot_build(plot)$data[[1L]]
  expect_equal(sum(bars$y), .9999)
  expect_true(all(is.finite(bars$xmin) & is.finite(bars$xmax)))
  expect_warning(ggplot2::ggplotGrob(plot), NA)
})

test_that("zero counts and complete inspection keep their full probability bars", {
  cases <- list(c(s = 0, n = 0, y = 0), c(s = 100, n = 0, y = 0),
                c(s = 100, n = 100, y = 7), c(s = 100, n = 100, y = 100))
  for (case in cases) {
    plot <- .ssmPredictiveBinPlot(rep(case[["y"]], 100),
      case[["s"]], case[["n"]], case[["y"]])
    bars <- ggplot2::ggplot_build(plot)$data[[1L]]
    expect_lte(nrow(bars), 30L)
    expect_equal(sum(bars$y), 1)
    expect_true(all(is.finite(bars$xmin) & is.finite(bars$xmax)))
    nonzero <- bars[bars$y > 0, ]
    expect_equal(nrow(nonzero), 1L)
    expect_true(nonzero$xmin < case[["y"]] && nonzero$xmax > case[["y"]])
    expect_warning(ggplot2::ggplotGrob(plot), NA)
  }
})
