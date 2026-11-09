.ssmBetaPlotData <- function() {
  data.frame(
    count = c(1, 0, 2, 1, 0, 3, 1, 2), n = rep(20, 8), s = rep(100, 8),
    lot = 1:8,
    x1 = c(-2, -1, 0, 1, 2, 0, -1, 1),
    x2 = c(1, 0, -1, 2, -2, 1, 0, -1),
    x3 = c(0, 1, 0, -1, 0, 2, -2, 1))
}

.ssmBetaPlotFixture <- function() {
  fixture <- new.env(parent = emptyenv())
  fixture$generations <- 0L
  fixture$labels <- c(x1 = "Positive", x2 = "Negative", x3 = "Crossing zero")
  # Deterministic, smooth draws with observations beyond each old 99% boundary.
  z <- stats::qnorm(stats::ppoints(4000))
  fixture$draws <- cbind(x1 = .5 + .07 * z, x2 = -.5 + .07 * z, x3 = .07 * z)
  ns <- asNamespace("jaspAcceptanceSampling")
  fixture$fit <- function(jaspResults, dataset, options) {
    error <- get(".asSSM_validateInputs", ns)(dataset, options)
    if (!is.null(error)) {
      get(".asSSM_setAnalysisError", ns)(jaspResults, error)
      return()
    }
    if (is.null(jaspResults[["fit"]])) {
      state <- jaspBase::createJaspState()
      state$dependOn(get(".asSSM_getFitDependencies", ns)())
      jaspResults[["fit"]] <- state
    }
    if (!is.null(jaspResults[["fit"]]$object)) return()
    fixture$generations <- fixture$generations + 1L
    design <- get(".asSSM_createPredictorDesign", ns)(dataset, options)
    fixtureNames <- jaspSyntax::decodeColumnNames(design$names)
    design$labels <- unname(fixture$labels[fixtureNames])
    beta <- fixture$draws[, fixtureNames, drop = FALSE]
    if (length(design$names) == 1L) beta <- as.numeric(beta)
    y <- tail(dataset[[options$defectCount]], 1L)
    jaspResults[["fit"]]$object <- list(
      samples = list(y_pred = y + c(0, 1, 1, 2, 2, 3), beta_predictor = beta),
      predictor_design = design)
  }
  fixture
}

.ssmBetaPlotOptions <- function(predictors) {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(defectCount = "count", sampleSize = "n", lotSize = "s",
                      lotNumber = "lot", covariates = predictors)
  options[names(assignments)] <- assignments
  for (name in c("plotStateTimeSeries", "plotPosteriorPredictive", "plotLatestDefectRate",
                 "mcmcDiagnosticOverviewTable", "mcmcDiagnosticReport"))
    options[[name]] <- FALSE
  options$plotPredictorCoefficient <- TRUE
  options
}

test_that("coefficient curves retain their tails when axes are extended", {
  fixture <- .ssmBetaPlotFixture()
  # Replace fitting only; the normal runner creates and serializes the real plot.
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fixture$fit,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("The density fixture must not load Stan."),
    .package = "jaspAcceptanceSampling")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(.ssmBetaPlotData(), csv, row.names = FALSE)

  # Cover legacy scalar draws and several predictors in a non-default order.
  for (predictors in list("x1", c("x3", "x1", "x2"))) {
    result <- jaspTools::runAnalysis("BayesianSSMforAS", csv,
      .ssmBetaPlotOptions(predictors), view = FALSE)
    expect_identical(result$status, "complete")
    expect_null(result$results$betaPlot[["error"]])
    plot <- result$state$figures[[result$results$betaPlot$data]]$obj
    expect_s3_class(plot, "ggplot")
    expect_identical(levels(plot$data$covariate), unname(fixture$labels[predictors]))

    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$layout$layout), length(predictors))
    for (predictor in predictors) {
      curve <- plot$data[plot$data$covariate == fixture$labels[[predictor]], ]
      draws <- fixture$draws[, predictor]
      oldBounds <- stats::quantile(draws, c(.005, .995))
      expect_lt(min(curve$x), oldBounds[[1L]])
      expect_gt(max(curve$x), oldBounds[[2L]])
      expect_lt(min(curve$x), min(draws))
      expect_gt(max(curve$x), max(draws))
      expect_true(all(is.finite(curve$density) & curve$density >= 0))
      # The filled curve should approach its baseline, not end at substantial height.
      expect_lt(curve$density[1L] / max(curve$density), .001)
      expect_lt(tail(curve$density, 1L) / max(curve$density), .001)
    }
    for (panel in built$layout$panel_params) {
      expect_lte(panel$x.range[1L], 0)
      expect_gte(panel$x.range[2L], 0)
    }

    # An expanded display axis must leave the existing full curve unchanged.
    expanded <- ggplot2::ggplot_build(
      plot + ggplot2::scale_x_continuous(limits = c(-1, 1)))
    expect_equal(expanded$data[[2L]][c("x", "y", "PANEL")],
                 built$data[[2L]][c("x", "y", "PANEL")])
    expect_false(grepl("99%", paste(plot$labels$caption, collapse = " "), fixed = TRUE))
  }
  expect_equal(fixture$generations, 2L)
})
