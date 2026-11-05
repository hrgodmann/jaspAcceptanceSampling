.ssm_plot_boundary_data <- function(lots = 1L) {
  data.frame(count = rep(2, lots), n = rep(20, lots), s = rep(100, lots),
             lot = seq_len(lots), x1 = seq_len(lots), x2 = seq_len(lots)^2)
}

.ssm_plot_boundary_run <- function(data, theta, beta = NULL, y_pred = NULL,
                                   predictors = character(), plots = character()) {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(defectCount = "count", sampleSize = "n", lotSize = "s",
                      lotNumber = "lot", covariates = predictors)
  # Specify Scale for the numeric fixture columns.
  options[names(assignments)] <- lapply(assignments, function(columns)
    if (length(columns)) list(value = columns, types = as.list(rep("scale", length(columns)))) else columns)
  for (name in c("plotsStateTimeSeries", "plotsPosteriorPredictive", "plotsLatestDefectRate",
                 "plotsPredictorCoefficients", "mcmcDiagnosticsOverviewTable", "mcmcDiagnosticsPrint"))
    options[[name]] <- name %in% plots
  if (is.null(y_pred)) y_pred <- rep(tail(data$count, 1L), nrow(theta))
  ns <- asNamespace("jaspAcceptanceSampling")
  fixture <- function(jaspResults, dataset, options) {
    error <- get(".asSSM_validateInputs", ns)(dataset, options)
    if (!is.null(error)) {
      get(".asSSM_setAnalysisError", ns)(jaspResults, error)
      return()
    }
    state <- jaspBase::createJaspState()
    state$dependOn(get(".asSSM_fitDependencies", ns)())
    jaspResults[["fit"]] <- state
    state$object <- list(
      samples = list(theta = theta, beta_predictor = beta, y_pred = y_pred),
      predictor_design = get(".asSSM_predictorDesign", ns)(dataset, options))
  }
  # The real runner and output builders are used; only sampling is replaced.
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fixture,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("Plot fixtures must not load Stan."),
    .package = "jaspAcceptanceSampling")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data, csv, row.names = FALSE)
  jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)
}

.ssm_plot_boundary_object <- function(result, name) {
  result$state$figures[[result$results[[name]]$data]]$obj
}

test_that("a single lot shows its posterior mean and interval", {
  theta <- matrix(seq(.01, .20, length.out = 1000), ncol = 1L)
  result <- .ssm_plot_boundary_run(.ssm_plot_boundary_data(), theta,
                                  plots = "plotsStateTimeSeries")
  expect_identical(result$status, "complete")
  plot <- .ssm_plot_boundary_object(result, "ssm_statePlot")
  expect_s3_class(plot, "ggplot")
  expect_warning(ggplot2::ggplotGrob(plot), NA)
  built <- ggplot2::ggplot_build(plot)
  intervals <- which(vapply(plot$layers, function(x) inherits(x$geom, "GeomLinerange"), logical(1)))
  points <- which(vapply(plot$layers, function(x) inherits(x$geom, "GeomPoint"), logical(1)))
  expect_length(intervals, 1L)
  expect_length(points, 1L)
  if (length(intervals) && length(points)) {
    expect_equal(built$data[[points]]$x, 1)
    expect_equal(built$data[[points]]$y, mean(theta) * 100)
    expect_equal(built$data[[intervals]]$ymin, unname(stats::quantile(theta, .025)) * 100)
    expect_equal(built$data[[intervals]]$ymax, unname(stats::quantile(theta, .975)) * 100)
  }
})

test_that("several lots retain their trajectory and uncertainty ribbon", {
  theta <- cbind(seq(.01, .20, length.out = 1000),
                 seq(.10, .30, length.out = 1000),
                 seq(.20, .40, length.out = 1000))
  result <- .ssm_plot_boundary_run(.ssm_plot_boundary_data(3L), theta,
                                  plots = "plotsStateTimeSeries")
  plot <- .ssm_plot_boundary_object(result, "ssm_statePlot")
  expect_s3_class(plot$layers[[1L]]$geom, "GeomRibbon")
  expect_s3_class(plot$layers[[2L]]$geom, "GeomLine")
  expect_equal(plot$data$time, 1:3)
  expect_equal(plot$data$mean, colMeans(theta) * 100)
  expect_equal(plot$data$lower, apply(theta, 2, stats::quantile, .025) * 100)
  expect_equal(plot$data$upper, apply(theta, 2, stats::quantile, .975) * 100)
})

test_that("constant defect-rate draws retain state output but cannot manufacture a density", {
  for (value in c(0, .05, 1)) {
    theta <- matrix(rep(value, 1000), ncol = 1L)
    result <- .ssm_plot_boundary_run(.ssm_plot_boundary_data(), theta,
      plots = c("plotsStateTimeSeries", "plotsLatestDefectRate"))
    expect_identical(result$status, "complete")
    expect_identical(result$results$summaryTable$status, "complete")
    state <- .ssm_plot_boundary_object(result, "ssm_statePlot")
    expect_equal(state$data$mean, value * 100)
    expect_equal(state$data$lower, state$data$upper)
    expect_warning(ggplot2::ggplotGrob(state), NA)
    breaks <- ggplot2::ggplot_build(state)$layout$panel_params[[1L]]$y$breaks
    expect_true(all(breaks[!is.na(breaks)] >= 0))
    expect_identical(result$results$postDist$status, "error")
    expect_match(paste(unlist(result$results$postDist), collapse = " "),
                 "no variation.*density cannot be estimated")
  }
})

test_that("predictive plots preserve unit mass at zero and after full inspection", {
  cases <- list(c(y = 0, n = 0, s = 100), c(y = 7, n = 100, s = 100),
                c(y = 0, n = 0, s = 0))
  for (counts in cases) {
    data <- .ssm_plot_boundary_data()
    data$count <- counts[["y"]]
    data$n <- counts[["n"]]
    data$s <- counts[["s"]]
    result <- .ssm_plot_boundary_run(data, matrix(seq(.01, .20, length.out = 1000), ncol = 1L),
      y_pred = rep(counts[["y"]], 1000), plots = "plotsPosteriorPredictive")
    expect_identical(result$status, "complete")
    expect_equal(result$results$summaryTable$data[[1L]]$pred, counts[["y"]])
    plot <- .ssm_plot_boundary_object(result, "predPlot")
    expect_s3_class(plot, "ggplot")
    expect_equal(sum(plot$data$prob), 1)
    expect_true(all(is.finite(plot$data$x) & is.finite(plot$data$prob)))
    expect_equal(max(plot$data$prob), 1)
    expect_warning(ggplot2::ggplotGrob(plot), NA)
  }
})

test_that("malformed coefficient arrays fail instead of being reshaped into densities", {
  data <- .ssm_plot_boundary_data(4L)
  theta <- matrix(rep(.05, 4000), ncol = 4L)
  invalid <- list(matrix(1:6, ncol = 1L), matrix(1:12, ncol = 3L), 1:12,
                  matrix(letters[1:8], ncol = 2L), matrix(complex(real = 1:8), ncol = 2L),
                  matrix(numeric(), ncol = 2L), matrix(c(1, 2), nrow = 1L),
                  matrix(c(1, NA, 3, 4), ncol = 2L), matrix(c(1, Inf, 3, 4), ncol = 2L))
  for (beta in invalid) {
    result <- .ssm_plot_boundary_run(data, theta, beta = beta,
      predictors = c("x1", "x2"), plots = "plotsPredictorCoefficients")
    expect_identical(result$status, "complete")
    expect_identical(result$results$betaPlot$status, "error")
    expect_match(paste(unlist(result$results$betaPlot), collapse = " "),
                 "Finite posterior draws for every predictor")
  }
})

test_that("constant coefficient draws retain their summary but cannot manufacture density spread", {
  data <- .ssm_plot_boundary_data(4L)
  theta <- matrix(rep(.05, 4000), ncol = 4L)
  for (value in c(0, .5, -.5)) {
    beta <- cbind(seq(-.2, .2, length.out = 1000), rep(value, 1000))
    result <- .ssm_plot_boundary_run(data, theta, beta = beta,
      predictors = c("x1", "x2"), plots = "plotsPredictorCoefficients")
    expect_identical(result$status, "complete")
    expect_identical(result$results$predictorSummary$status, "complete")
    expect_equal(result$results$predictorSummary$data[[2L]]$mean, value)
    expect_equal(result$results$predictorSummary$data[[2L]]$lower, value)
    expect_equal(result$results$predictorSummary$data[[2L]]$upper, value)
    expect_identical(result$results$betaPlot$status, "error")
    expect_match(paste(unlist(result$results$betaPlot), collapse = " "),
                 "no variation.*densities cannot be estimated")
  }
})
