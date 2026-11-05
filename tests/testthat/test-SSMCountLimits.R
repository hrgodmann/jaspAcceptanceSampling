test_that("decimal quality limits retain mathematically integer count boundaries", {
  limit <- getFromNamespace(".asSSM_countLimit", "jaspAcceptanceSampling")
  # Independent integer arithmetic identifies exact boundaries for decimal inputs.
  sizes <- seq_len(2000L)
  actual <- expected <- numeric()
  for (numerator in seq_len(999L)) {
    selected <- sizes[(numerator * sizes) %% 1000L == 0L]
    actual <- c(actual, vapply(selected, function(n) limit(numerator / 1000, n), numeric(1)))
    expected <- c(expected, numerator * selected / 1000)
  }
  expect_length(actual, 15000L)
  expect_identical(actual, expected)
  expect_identical(limit(.29, 100), 29)
  expect_identical(limit(.07, 100), 7)

  # Decimal inputs also occur with large allowed lots, not just small examples.
  large <- 2147483600
  expect_identical(vapply(1:99, function(k) limit(k / 100, large), numeric(1)),
                   (1:99) * (large / 100))
  largest <- .Machine$integer.max - 1L
  expect_equal(limit(.5, largest), largest / 2)
  expect_equal(limit(1, largest), largest)
})

test_that("fractional and genuinely nearby quality limits are not rounded", {
  limit <- getFromNamespace(".asSSM_countLimit", "jaspAcceptanceSampling")
  for (pair in list(c(.025, 30), c(.15, 7), c(1e-300, 100), c(.Machine$double.xmin, 1)))
    expect_identical(limit(pair[1L], pair[2L]), pair[1L] * pair[2L])
  for (pair in list(c(29, 100), c(7, 100), c(622770244, 2147483600))) {
    for (offset in c(-1e-5, 1e-5)) {
      proportion <- (pair[1L] + offset) / pair[2L]
      actual <- limit(proportion, pair[2L])
      expect_identical(actual, proportion * pair[2L])
      expect_true(if (offset < 0) actual < pair[1L] else actual > pair[1L])
    }
  }
  expect_identical(limit(0, 100), 0)
  expect_identical(limit(.29, 0), 0)
})

test_that("table decisions and plot limits share strict decimal count boundaries", {
  ns <- asNamespace("jaspAcceptanceSampling")
  fixture <- new.env(parent = emptyenv())
  fit_fixture <- function(jaspResults, dataset, options) {
    error <- get(".asSSM_validateInputs", ns)(dataset, options)
    if (!is.null(error)) {
      get(".asSSM_setAnalysisError", ns)(jaspResults, error)
      return()
    }
    state <- jaspBase::createJaspState()
    state$dependOn(get(".asSSM_fitDependencies", ns)())
    jaspResults[["fit"]] <- state
    state$object <- list(samples = list(y_pred = fixture$draws))
  }
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fit_fixture,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("Count-limit fixtures must not load Stan."),
    .package = "jaspAcceptanceSampling")
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(defectCount = "y", sampleSize = "n", lotSize = "s",
                      lotNumber = "lot", covariates = character())
  # Specify Scale for the numeric fixture columns.
  options[names(assignments)] <- lapply(assignments, function(columns)
    if (length(columns)) list(value = columns, types = as.list(rep("scale", length(columns)))) else columns)
  for (name in c("plotsStateTimeSeries", "plotsLatestDefectRate", "plotsPredictorCoefficients",
                 "mcmcDiagnosticsOverviewTable", "mcmcDiagnosticsPrint"))
    options[[name]] <- FALSE
  options$plotsPosteriorPredictive <- TRUE
  options$settingsAcceptanceThreshold <- .10
  options$settingsRejectionThreshold <- .05
  cases <- list(
    list(s = 100, n = 100, y = 29, draws = rep(29, 4), aql = .05, rql = .29,
         limits = c(5, 29), below = 0, above = 0, decision = "Accept"),
    list(s = 100, n = 100, y = 7, draws = rep(7, 4), aql = .07, rql = .10,
         limits = c(7, 10), below = 0, above = 0, decision = "Accept"),
    # Use neighboring limits accepted by the QML controls.
    list(s = 100, n = 100, y = 29, draws = rep(29, 4), aql = .05, rql = .289,
         limits = c(5, (.289) * 100), below = 0, above = 1, decision = "Reject"),
    list(s = 100, n = 100, y = 29, draws = rep(29, 4), aql = .05, rql = .291,
         limits = c(5, (.291) * 100), below = 0, above = 0, decision = "Accept"),
    list(s = 100, n = 100, y = 7, draws = rep(7, 4), aql = .069, rql = .10,
         limits = c((.069) * 100, 10), below = 0, above = 0, decision = "Accept"),
    list(s = 100, n = 100, y = 7, draws = rep(7, 4), aql = .071, rql = .10,
         limits = c((.071) * 100, 10), below = 1, above = 0, decision = "Accept"),
    # Exact probability ties must survive aggregation over many supported counts.
    list(s = 1000, n = 0, y = 0, draws = c(rep(80, 900), 101:200), aql = .05, rql = .10,
         limits = c(50, 100), below = 0, above = .10, decision = "Continue"),
    list(s = 1000, n = 0, y = 0, draws = c(1, rep(2, 9), rep(200, 70), rep(900, 20)),
         aql = .05, rql = .80, reject = .10,
         limits = c(50, 800), below = .10, above = .20, decision = "Continue"),
    list(s = 30, n = 0, y = 0, draws = 0:3, aql = .025, rql = .05,
         limits = c(.75, 1.5), below = .25, above = .5, decision = "Continue"),
    list(s = 0, n = 0, y = 0, draws = rep(0, 4), aql = .025, rql = .10,
         limits = c(0, 0), below = 0, above = 0, decision = "Accept"),
    list(s = 2147483600, n = 2147483600, y = 622770244, draws = rep(622770244, 4),
         aql = .05, rql = .29, limits = c(107374180, 622770244),
         below = 0, above = 0, decision = "Accept"))
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  for (case in cases) {
    fixture$draws <- case$draws
    options$settingsRejectionThreshold <- if (is.null(case$reject)) .05 else case$reject
    options$settingsAcceptableQualityLevel <- case$aql
    options$settingsRejectableQualityLevel <- case$rql
    utils::write.csv(data.frame(y = case$y, n = case$n, s = case$s, lot = 1),
                      csv, row.names = FALSE)
    result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)
    expect_identical(result$status, "complete")
    row <- result$results$summaryTable$data[[1L]]
    expect_equal(row$massAQL, case$below)
    expect_equal(row$massRQL, case$above)
    expect_identical(row$decision, paste("Consider to", case$decision))
    expect_null(result$results$predPlot[["error"]])
    plot <- result$state$figures[[result$results$predPlot$data]]$obj
    lines <- Filter(function(layer) inherits(layer$geom, "GeomVline"), plot$layers)
    expect_length(lines, 2L)
    intercepts <- vapply(lines, function(layer) layer$data$xintercept, numeric(1))
    expect_identical(unname(intercepts), case$limits)
  }
})
