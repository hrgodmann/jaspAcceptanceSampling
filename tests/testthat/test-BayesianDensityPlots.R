# The actual result-engine plots must retain narrow peaks and both endpoint tails.
.bsDensityOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$priorplan <- "custom"
  options$alphaplan <- 1
  options$betaplan <- 1
  options$showPlansplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$priorPlotplan <- TRUE
  options$inferPosteriorinfer <- TRUE
  options$showInferenceTableinfer <- TRUE
  options$posteriorPlotinfer <- TRUE
  options$ppdPlotinfer <- FALSE
  options
}

.bsDensityPlotForTest <- function(result, type) {
  container <- if (type == "Prior") "planContainer" else "infContainer"
  output <- result$results[[container]]$collection[[paste0(container, "_distPlot", type)]]
  expect_identical(output$status, "complete")
  expect_null(output[["error"]])
  plot <- result$state$figures[[output$data]]$obj
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$scales$get_scales("x")$limits, c(0, 1))
  plot
}

.bsDensityLayerForTest <- function(plot, geom) {
  which(vapply(plot$layers, function(layer) inherits(layer$geom, geom), logical(1)))
}

.bsDensityPeakForTest <- function(plot, alpha, beta, type) {
  layer <- .bsDensityLayerForTest(plot, "GeomLine")
  raw <- subset(plot$layers[[layer]]$data, dist == type)
  mode <- if (alpha == 1) 0 else if (beta == 1) 1 else (alpha - 1) / (alpha + beta - 2)
  peak <- dbeta(mode, alpha, beta)
  expect_equal(range(raw$x), c(0, 1))
  expect_equal(max(raw$dens), peak, tolerance = 1e-7)
  expect_equal(raw$dens, dbeta(raw$x, alpha, beta))
  built <- ggplot2::ggplot_build(plot)
  line <- built$data[[layer]]
  # Prior has a dashed overlay only in the posterior plot.
  style <- if (type == "Prior" && length(unique(plot$layers[[layer]]$data$dist)) > 1L) "dashed" else "solid"
  expect_equal(max(line$y[line$linetype == style]), peak, tolerance = 1e-7)
  expect_gte(plot$scales$get_scales("y")$limits[2L], peak)
  if (type == "Posterior") {
    area <- built$data[[.bsDensityLayerForTest(plot, "GeomArea")]]
    expect_equal(max(area$y), peak, tolerance = 1e-7)
  }
  invisible(plot)
}

test_that("Bayesian sampling - adaptive grids retain finite endpoints, modes, and density mass", {
  cases <- list(c(1, 1), c(1, 1e6), c(1e6, 1), c(1e5, 1e5), c(9e10, 9e10))
  for (params in cases) {
    grid <- jaspAcceptanceSampling:::.bsBetaDensityGrid(params[1L], params[2L])
    expect_equal(range(grid$x), c(0, 1))
    expect_true(all(diff(grid$x) > 0))
    expect_equal(grid$density, dbeta(grid$x, params[1L], params[2L]))
    expect_true(all(is.finite(grid$density)))
    integral <- sum(diff(grid$x) * (head(grid$density, -1L) + tail(grid$density, -1L)) / 2)
    expect_equal(integral, 1, tolerance = .001)
    if (all(params > 1)) {
      mode <- (params[1L] - 1) / (sum(params) - 2)
      expect_equal(max(grid$density), dbeta(mode, params[1L], params[2L]))
    } else {
      expect_equal(grid$density[c(1L, length(grid$density))], dbeta(c(0, 1), params[1L], params[2L]))
    }
  }
})

test_that("Bayesian sampling - actual prior and posterior layers preserve distinct narrow peaks", {
  options <- .bsDensityOptionsForTest()
  options$alphaplan <- options$betaplan <- 1e5
  options$aqlplan <- .4
  options$rqlplan <- .5
  options$data_ninfer <- options$lot_sizeinfer <- 10000
  options$data_dinfer <- 0
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(result$status, "complete")
  prior <- .bsDensityPlotForTest(result, "Prior")
  posterior <- .bsDensityPlotForTest(result, "Posterior")
  .bsDensityPeakForTest(prior, 1e5, 1e5, "Prior")
  .bsDensityPeakForTest(posterior, 1e5, 1e5, "Prior")
  .bsDensityPeakForTest(posterior, 1e5, 110000, "Posterior")
  expect_warning(ggplot2::ggplotGrob(prior), NA)
  expect_warning(ggplot2::ggplotGrob(posterior), NA)
})

test_that("Bayesian sampling - a reachable sharply calibrated prior is resolved in the rendered plot", {
  # Preserve six-decimal inputs through jaspTools' otherwise four-digit JSON.
  fetch_args <- getFromNamespace("fetchRunArgs", "jaspTools")
  testthat::local_mocked_bindings(fetchRunArgs = function(name, options) {
    args <- fetch_args(name, options)
    args$options <- jsonlite::toJSON(options, digits = NA)
    args
  }, .package = "jaspTools")
  options <- .bsDensityOptionsForTest()
  options$priorplan <- "impartial_three"
  options$aqlplan <- .499999
  options$rqlplan <- .5
  options$impartialThreeConstraintplan <- "both_ge_1"
  options$inferPosteriorinfer <- FALSE
  original <- get(".bsDistributionPlot", asNamespace("jaspAcceptanceSampling"))
  params <- NULL
  wrapper <- function(container, aql, rql, alpha, beta, type, ...) {
    params <<- c(alpha, beta)
    original(container, aql, rql, alpha, beta, type, ...)
  }
  result <- testthat::with_mocked_bindings(
    jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
    .bsDistributionPlot = wrapper, .package = "jaspAcceptanceSampling")
  expect_identical(result$status, "complete")
  expect_true(all(params > 1e10))
  plot <- .bsDensityPlotForTest(result, "Prior")
  .bsDensityPeakForTest(plot, params[1L], params[2L], "Prior")
  area <- ggplot2::ggplot_build(plot)$data[[.bsDensityLayerForTest(plot, "GeomArea")]]
  expect_equal(max(area$y), dbeta((params[1L] - 1) / (sum(params) - 2), params[1L], params[2L]), tolerance = 1e-7)
  expect_warning(ggplot2::ggplotGrob(plot), NA)
})

test_that("Bayesian sampling - actual U-shaped tails are mirrored and retained when expanding axes", {
  options <- .bsDensityOptionsForTest()
  options$alphaplan <- options$betaplan <- .5
  options$inferPosteriorinfer <- FALSE
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(result$status, "complete")
  plot <- .bsDensityPlotForTest(result, "Prior")
  index <- .bsDensityLayerForTest(plot, "GeomLine")
  raw <- plot$layers[[index]]$data
  expect_true(all(is.infinite(raw$dens[raw$x %in% c(0, 1)])))
  finite <- raw[is.finite(raw$dens), ]
  probes <- c(1e-6, 1e-5, 1e-4, .001, .01, .05, .25)
  left <- approx(finite$x, finite$dens, probes)$y
  right <- approx(finite$x, finite$dens, 1 - probes)$y
  expect_equal(left, right, tolerance = .002)
  built <- ggplot2::ggplot_build(plot)$data[[index]]
  upper <- plot$scales$get_scales("y")$limits[2L]
  expect_true(any(is.finite(built$y) & built$y > upper))
  expect_equal(sum(is.infinite(built$y)), 2L)
  expect_warning(ggplot2::ggplotGrob(plot), NA)
  # Use the installed scale's existing out-of-bounds behavior as a manual edit does.
  y_scale <- plot$scales$get_scales("y")
  y_scale$limits <- c(0, upper * 10)
  expanded <- ggplot2::ggplot_build(plot)$data[[index]]
  expect_equal(plot$layers[[index]]$data, raw)
  expect_equal(expanded$x, built$x)
  visible_before <- is.finite(raw$dens) & raw$dens <= upper
  visible_after <- is.finite(raw$dens) & raw$dens <= upper * 10
  expect_equal(built$y[visible_before], raw$dens[visible_before])
  expect_equal(expanded$y[visible_after], raw$dens[visible_after])
  expect_true(any(visible_after & !visible_before))
  expect_true(any(expanded$y[is.finite(expanded$y)] > upper * 10))
  expect_equal(sum(is.infinite(expanded$y)), 2L)
  expect_warning(ggplot2::ggplotGrob(plot), NA)
})

test_that("Bayesian sampling - finite endpoint peaks and uniform overlays survive rendering", {
  options <- .bsDensityOptionsForTest()
  options$data_ninfer <- options$data_dinfer <- options$lot_sizeinfer <- 10000
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(result$status, "complete")
  prior <- .bsDensityPlotForTest(result, "Prior")
  raw <- prior$layers[[.bsDensityLayerForTest(prior, "GeomLine")]]$data
  expect_equal(range(raw$x), c(0, 1))
  expect_true(all(raw$dens == 1))
  posterior <- .bsDensityPlotForTest(result, "Posterior")
  .bsDensityPeakForTest(posterior, 10001, 1, "Posterior")
  expect_warning(ggplot2::ggplotGrob(prior), NA)
  expect_warning(ggplot2::ggplotGrob(posterior), NA)
})

test_that("Bayesian sampling - plotting leaves planning and inference numbers unchanged", {
  options <- .bsDensityOptionsForTest()
  options$priorplan <- "impartial"
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  options$max_nplan <- 40
  with_plots <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  options$priorPlotplan <- options$posteriorPlotinfer <- FALSE
  without_plots <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(with_plots$status, "complete")
  expect_identical(without_plots$status, "complete")
  for (key in c("planTable", "threeBFTable")) {
    name <- paste0("planContainer_", key)
    before <- with_plots$results$planContainer$collection[[name]]$data
    expect_gt(length(before), 0L)
    expect_equal(without_plots$results$planContainer$collection[[name]]$data, before)
  }
  before <- with_plots$results$infContainer$collection$infContainer_inferenceDecisionTable$data
  expect_gt(length(before), 0L)
  expect_equal(without_plots$results$infContainer$collection$infContainer_inferenceDecisionTable$data, before)
})


test_that("Bayesian sampling - off-panel density coordinates stay bounded without changing visible values", {
  x <- c(-Inf, -1e20, -10, 0, 1, 4, 10, 11, 20, 1e21, Inf, NA_real_)
  mapped <- jaspAcceptanceSampling:::.bsKeepDensityTails(x, c(0, 10))
  inside <- is.finite(x) & x >= 0 & x <= 10
  expect_identical(mapped[inside], x[inside])
  expect_true(all(mapped[is.finite(mapped)] >= -10))
  expect_true(all(mapped[is.finite(mapped)] <= 20))
  expect_true(any(mapped[is.finite(mapped)] > 10))
  expect_identical(mapped[is.infinite(x)], x[is.infinite(x)])
  expect_identical(is.na(mapped), is.na(x))
})

test_that("Bayesian sampling - an asymmetric U prior renders despite extremely large off-panel densities", {
  options <- .bsDensityOptionsForTest()
  options$priorplan <- "impartial"
  options$impartialShapeplan <- "u_shaped"
  options$impartialCustomModeplan <- TRUE
  options$impartialModeplan <- .95
  options$rqlplan <- .15
  options$inferPosteriorinfer <- FALSE
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE)
  expect_identical(result$status, "complete")
  plot <- .bsDensityPlotForTest(result, "Prior")
  index <- .bsDensityLayerForTest(plot, "GeomLine")
  raw <- plot$layers[[index]]$data
  expect_gt(max(raw$dens[is.finite(raw$dens)]), 1e20)
  built <- ggplot2::ggplot_build(plot)$data[[index]]
  upper <- plot$scales$get_scales("y")$limits[2L]
  expect_lte(max(built$y[is.finite(built$y)]), 2 * upper)
  expect_equal(sum(is.infinite(built$y)), 2L)
  path <- tempfile(fileext = ".png")
  withr::defer(unlink(path))
  expect_warning(ggplot2::ggsave(path, plot, width = 5, height = 4, dpi = 100, bg = "white"), NA)
  expect_gt(file.info(path)$size, 0)
  expect_equal(plot$layers[[index]]$data, raw)
})
