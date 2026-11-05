.ssm_numeric_api <- function() {
  list(
    names = getFromNamespace(".asSSM_predictorNames", "jaspAcceptanceSampling"),
    design = getFromNamespace(".asSSM_predictorDesign", "jaspAcceptanceSampling"),
    validate = getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling"),
    read = getFromNamespace(".asSSM_readData", "jaspAcceptanceSampling")
  )
}

.ssm_numeric_options <- function(predictors = character()) {
  list(defectCount = "count", sampleSize = "n", lotSize = "s", lotNumber = "lot",
       covariates = predictors)
}

.ssm_numeric_data <- function() {
  data.frame(
    count = c(1, 0, 2, 1, 0, 3, 1, 2), n = rep(20, 8), s = rep(100, 8),
    lot = 1:8,
    x1 = c(-2, -1, 0, 1, 2, 0, -1, 1),
    x2 = c(1, 0, -1, 2, -2, 1, 0, -1),
    x3 = c(0, 1, 0, -1, 0, 2, -2, 1)
  )
}

test_that("numeric predictor assignments preserve old scalar and new vector options", {
  api <- .ssm_numeric_api()
  for (empty in list("", character(), NULL)) {
    expect_identical(api$names(.ssm_numeric_options(empty)), character())
  }
  expect_identical(api$names(.ssm_numeric_options("x1")), "x1")
  expect_identical(api$names(.ssm_numeric_options(c("x3", "x1", "x2"))),
                   c("x3", "x1", "x2"))
})

test_that("numeric design preserves the original one-predictor standardization and dimensions", {
  api <- .ssm_numeric_api()
  data <- .ssm_numeric_data()
  before <- data
  design <- api$design(data, .ssm_numeric_options("x1"))
  expect_identical(design$names, "x1")
  expect_identical(dim(design$X), c(nrow(data), 1L))
  expect_equal(unname(design$center), mean(data$x1), tolerance = 1e-14)
  expect_equal(unname(design$scale), stats::sd(data$x1), tolerance = 1e-14)
  expect_equal(as.numeric(design$X), as.numeric(scale(data$x1)), tolerance = 1e-14)
  expect_identical(data, before)

  none <- api$design(data, .ssm_numeric_options())
  expect_identical(none$names, character())
  expect_length(none$center, 0L)
  expect_length(none$scale, 0L)
  expect_identical(dim(none$X), c(nrow(data), 0L))
})

test_that("two and three numerical predictors are scaled separately in assignment order", {
  api <- .ssm_numeric_api()
  data <- .ssm_numeric_data()
  for (selected in list(c("x1", "x2"), c("x3", "x1", "x2"))) {
    options <- .ssm_numeric_options(selected)
    expect_null(api$validate(data, options))
    design <- api$design(data, options)
    expect_identical(design$names, selected)
    expect_identical(dim(design$X), c(nrow(data), length(selected)))
    for (j in seq_along(selected)) {
      expected <- (data[[selected[j]]] - mean(data[[selected[j]]])) /
        stats::sd(data[[selected[j]]])
      expect_equal(as.numeric(design$X[, j]), expected, tolerance = 1e-14)
      expect_equal(unname(design$center[j]), mean(data[[selected[j]]]), tolerance = 1e-14)
      expect_equal(unname(design$scale[j]), stats::sd(data[[selected[j]]]), tolerance = 1e-14)
      expect_equal(mean(design$X[, j]), 0, tolerance = 1e-14)
      expect_equal(stats::sd(design$X[, j]), 1, tolerance = 1e-14)
    }
  }
  ordered <- api$design(data, .ssm_numeric_options(c("x1", "x2", "x3")))
  reordered <- api$design(data, .ssm_numeric_options(c("x3", "x1", "x2")))
  expect_equal(as.numeric(reordered$X), as.numeric(ordered$X[, c(3, 1, 2), drop = FALSE]),
               tolerance = 1e-14)
  beta <- c(0.8, -0.4, 0.2)
  expect_equal(as.numeric(ordered$X %*% beta),
               as.numeric(reordered$X %*% beta[c(3, 1, 2)]), tolerance = 1e-14)
})

test_that("sorting keeps all predictor values with their lot and never drops a row", {
  api <- .ssm_numeric_api()
  data <- .ssm_numeric_data()[c(8, 3, 6, 2, 5, 1, 7, 4), ]
  data$row_id <- letters[1:nrow(data)]
  before <- data
  options <- .ssm_numeric_options(c("x1", "x2", "x3"))
  sorted <- api$read(data, options)
  expect_identical(sorted, data[order(data$lot), , drop = FALSE])
  expect_equal(nrow(sorted), nrow(data))
  expect_null(api$validate(sorted, options))
  expect_identical(data, before)
  expect_equal(as.numeric(api$design(sorted, options)$X),
               as.numeric(api$design(data, options)$X[order(data$lot), , drop = FALSE]),
               tolerance = 1e-14)
})

test_that("every selected predictor is checked for missing columns and numeric finite values", {
  api <- .ssm_numeric_api()
  options <- .ssm_numeric_options(c("x1", "x2", "x3"))
  for (name in options$covariates) {
    missing <- .ssm_numeric_data()
    missing[[name]] <- NULL
    expect_match(api$validate(missing, options), "missing", ignore.case = TRUE)

    for (value in c(NA_real_, NaN, Inf, -Inf)) {
      invalid <- .ssm_numeric_data()
      invalid[[name]][4] <- value
      before <- invalid
      expect_match(api$validate(invalid, options), "missing|finite", ignore.case = TRUE)
      expect_identical(invalid, before)
      expect_equal(nrow(invalid), 8L)
    }
    invalidColumns <- list(as.character(1:8), factor(rep(c("a", "b"), 4)),
                           rep(c(TRUE, FALSE), 4), as.list(1:8), as.complex(1:8))
    for (values in invalidColumns) {
      invalid <- .ssm_numeric_data()
      invalid[[name]] <- values
      expect_match(api$validate(invalid, options), "numeric|number|real", ignore.case = TRUE)
    }
    invalid <- .ssm_numeric_data()
    invalid[[name]] <- rep(5, nrow(invalid))
    expect_match(api$validate(invalid, options), "vary|variation|constant", ignore.case = TRUE)
  }
  inactive <- .ssm_numeric_data()
  inactive$x3 <- factor(rep(c("a", "b"), 4))
  expect_null(api$validate(inactive, .ssm_numeric_options(c("x1", "x2"))))
})

test_that("duplicate and exactly redundant numeric predictors are rejected before fitting", {
  api <- .ssm_numeric_api()
  data <- .ssm_numeric_data()
  expect_match(api$validate(data, .ssm_numeric_options(c("x1", "x1"))),
               "duplicate|once|repeated", ignore.case = TRUE)

  proportional <- data
  proportional$x2 <- 3 * proportional$x1 + 12
  expect_match(api$validate(proportional, .ssm_numeric_options(c("x1", "x2"))),
               "redundan|linear|collinear|rank", ignore.case = TRUE)
  combination <- data
  combination$x3 <- combination$x1 + 2 * combination$x2 + 9
  expect_match(api$validate(combination, .ssm_numeric_options(c("x1", "x2", "x3"))),
               "redundan|linear|collinear|rank", ignore.case = TRUE)
  expect_null(api$validate(data, .ssm_numeric_options(c("x1", "x2", "x3"))))
})

test_that("high correlation and nonlinear transformations alone do not block predictors", {
  api <- .ssm_numeric_api()
  data <- .ssm_numeric_data()
  data$x1 <- seq_len(nrow(data))
  data$x2 <- 2 * data$x1 + c(0.01, -0.01, 0, 0.02, -0.02, 0, 0.01, -0.01)
  expect_gt(stats::cor(data$x1, data$x2), 0.999)
  expect_null(api$validate(data, .ssm_numeric_options(c("x1", "x2"))))
  data$x2 <- data$x1^2
  expect_null(api$validate(data, .ssm_numeric_options(c("x1", "x2"))))
})

test_that("redundant predictors produce a renderable actionable error before loading Stan", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- .ssm_numeric_options(c("x1", "x2"))
  options[names(assignments)] <- assignments
  data <- .ssm_numeric_data()
  data$x2 <- 2 * data$x1 + 10
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data, csv, row.names = FALSE)
  loads <- 0L
  testthat::local_mocked_bindings(
    .asSSM_getStanModel = function(...) {
      loads <<- loads + 1L
      stop("Stan must not load for invalid covariates.")
    },
    .package = "jaspAcceptanceSampling"
  )
  result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)
  expect_equal(loads, 0L)
  meta <- Filter(function(x) identical(x$name, "placeholder"), result$results$.meta)
  expect_length(meta, 1L)
  expect_identical(meta[[1]]$type, "table")
  error <- result$results$placeholder
  expect_identical(error$status, "error")
  expect_identical(error$error$type, "badData")
  expect_match(error$error$errorMessage, "linearly dependent")
  expect_match(error$error$errorMessage, "rescaled or shifted")
  expect_match(error$error$errorMessage, "Remove redundant covariates")
  expect_false("summaryTable" %in% names(result$results))
})

test_that("sample and lot boundaries remain valid with several predictors", {
  api <- .ssm_numeric_api()
  data <- .ssm_numeric_data()
  data$count[1:4] <- c(0, 0, 20, 0)
  data$n[1:4] <- c(0, 100, 20, 0)
  data$s[4] <- 0
  options <- .ssm_numeric_options(c("x1", "x2"))
  expect_null(api$validate(data, options))
  expect_equal(nrow(api$design(data, options)$X), nrow(data))

  invalid <- data
  invalid$count[1] <- 1
  expect_match(api$validate(invalid, options), "defect.*sample|count.*inspected",
               ignore.case = TRUE)
  invalid <- data
  invalid$lot[8] <- invalid$lot[1]
  expect_match(api$validate(invalid, options), "unique|duplicate|one row|only once",
               ignore.case = TRUE)
  expect_null(api$validate(data[1, , drop = FALSE], .ssm_numeric_options()))
  expect_match(api$validate(data[1, , drop = FALSE], .ssm_numeric_options("x1")),
               "two|multiple|more than one", ignore.case = TRUE)
})

test_that("two numeric predictors reach the coefficient table, density facets and predictive output", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- .ssm_numeric_options(c("x1", "x2"))
  for (name in names(assignments)) options[[name]] <- assignments[[name]]
  options$plotsStateTimeSeries <- FALSE
  options$plotsPosteriorPredictive <- TRUE
  options$plotsLatestDefectRate <- FALSE
  options$plotsPredictorCoefficients <- TRUE
  options$mcmcDiagnosticsOverviewTable <- TRUE
  options$mcmcDiagnosticsOverviewTheta <- FALSE
  options$mcmcDiagnosticsOverviewSigma <- FALSE
  options$mcmcDiagnosticsOverviewPrediction <- FALSE
  options$mcmcDiagnosticsOverviewBeta <- TRUE
  options$mcmcDiagnosticsPrint <- FALSE

  # This short chain tests output wiring, not convergence or parameter recovery.
  options$advancedMcmcBurnin <- 100
  options$advancedMcmcSamples <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcThin <- 1
  options$advancedMcmcSeed <- 42
  options$advancedMcmcAdaptDelta <- 0.9
  options$advancedMcmcMaxTreeDepth <- 8

  data <- .ssm_numeric_data()
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data, csv, row.names = FALSE)
  result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)

  expect_identical(result$status, "complete")
  expect_true(all(c("summaryTable", "mcmcSummary", "betaPlot", "predPlot",
                    "samplerDiagnostics") %in% names(result$results)))
  parameters <- vapply(result$results$mcmcSummary$data, function(row) row$parameter,
                       character(1))
  expect_identical(parameters, c("beta_predictor[1] (x1)", "beta_predictor[2] (x2)"))
  row <- result$results$summaryTable$data[[1L]]
  expect_identical(row$t, "8")
  expect_equal(row$y, tail(data$count, 1L))
  expect_gte(row$pred, tail(data$count, 1L))
  expect_lte(row$pred, tail(data$count + data$s - data$n, 1L))
  expect_true(row$decision %in%
                c("Consider to Accept", "Consider to Reject", "Consider to Continue"))
  expect_true(is.finite(row$massAQL) && row$massAQL >= 0 && row$massAQL <= 1)
  expect_true(is.finite(row$massRQL) && row$massRQL >= 0 && row$massRQL <= 1)
  warningText <- paste(unlist(result$results$samplerDiagnostics, use.names = FALSE),
                       collapse = " ")
  expect_match(warningText, "Only one chain")

  plot <- result$state$figures[[result$results$betaPlot$data]]$obj
  expect_s3_class(plot, "ggplot")
  layout <- ggplot2::ggplot_build(plot)$layout$layout
  expect_equal(length(unique(layout$PANEL)), 2L)
  labels <- unlist(lapply(layout, as.character), use.names = FALSE)
  expect_true(all(c("x1", "x2") %in% labels))
  predictivePlot <- result$state$figures[[result$results$predPlot$data]]$obj
  expect_s3_class(predictivePlot, "ggplot")
})

test_that("valid inputs silently replace old errors and a loader failure shows a new error", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- .ssm_numeric_options(c("x1", "x2"))
  for (name in names(assignments)) options[[name]] <- assignments[[name]]
  for (name in c("plotsStateTimeSeries", "plotsPosteriorPredictive", "plotsLatestDefectRate",
                 "plotsPredictorCoefficients", "mcmcDiagnosticsOverviewTable", "mcmcDiagnosticsPrint"))
    options[[name]] <- FALSE

  captured <- NULL
  loader <- function(name) {
    results <- get("jaspResults", envir = parent.frame(), inherits = TRUE)
    pending <- results[["placeholder"]]
    captured <<- list(
      present = !is.null(pending),
      error = if (is.null(pending)) NA else pending$getError(),
      json = jaspTools:::getJsonResultsFromJaspResults(results)
    )
    stop("Deliberate model-loader failure for the output lifecycle test.", call. = FALSE)
  }
  testthat::local_mocked_bindings(.asSSM_getStanModel = loader,
                                 .package = "jaspAcceptanceSampling")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(.ssm_numeric_data(), csv, row.names = FALSE)
  result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)

  expect_true(captured$present)
  expect_identical(captured$error, FALSE)
  pending <- jsonlite::fromJSON(captured$json, simplifyVector = FALSE)$results$placeholder
  expect_identical(pending$rawtext, "")
  expect_identical(pending$text, "")
  expect_identical(pending$elementType, "")
  expect_identical(pending$title, "hide me")
  expect_null(pending$error)
  expect_identical(result$status, "complete")
  expect_true("placeholder" %in% names(result$results))
  final <- as.character(jsonlite::toJSON(result$results$placeholder, auto_unbox = TRUE))
  expect_match(final, "Deliberate model-loader failure", fixed = TRUE)
  expect_false(grepl("Inputs validated. Estimating the model.", final, fixed = TRUE))
  expect_false(any(c("summaryTable", "mcmcSummary", "betaPlot", "predPlot") %in%
                     names(result$results)))
})

.ssm_coefficient_fixture <- function() {
  fixture <- new.env(parent = emptyenv())
  fixture$generations <- 0L
  fixture$labels <- c(x1 = "Temperature \u00b0C", x2 = "Pressure level", x3 = "Flow / volume")
  fixture$draws <- cbind(x1 = c(-2, -1, 0, 1, 2, 20),
                         x2 = c(-9, -4, -1, 0, 0, 2),
                         x3 = c(.25, .5, .75, 1, 1.25, 4))
  fixture$expected <- data.frame(
    mean = c(10 / 3, -2, 31 / 24), median = c(.5, -.5, .875),
    lower = c(-1.875, -8.375, .28125), upper = c(17.75, 1.75, 3.65625),
    row.names = c("x1", "x2", "x3"))
  ns <- asNamespace("jaspAcceptanceSampling")
  fixture$fit <- function(jaspResults, dataset, options) {
    error <- get(".asSSM_validateInputs", ns)(dataset, options)
    if (!is.null(error)) {
      get(".asSSM_setAnalysisError", ns)(jaspResults, error)
      return()
    }
    if (is.null(jaspResults[["fit"]])) {
      state <- jaspBase::createJaspState()
      state$dependOn(get(".asSSM_fitDependencies", ns)())
      jaspResults[["fit"]] <- state
    }
    if (!is.null(jaspResults[["fit"]]$object)) return()
    fixture$generations <- fixture$generations + 1L
    design <- get(".asSSM_predictorDesign", ns)(dataset, options)
    fixtureNames <- jaspSyntax::decodeColumnNames(design$names)
    design$labels <- unname(fixture$labels[fixtureNames])
    beta <- fixture$draws[, fixtureNames, drop = FALSE]
    # Exercise the accepted legacy scalar-extraction shape as well as matrices.
    if (length(design$names) == 1L) beta <- as.numeric(beta)
    y <- tail(dataset[[options$defectCount]], 1L)
    jaspResults[["fit"]]$object <- list(
      samples = list(y_pred = y + c(0, 1, 1, 2, 2, 3),
                     beta_predictor = if (length(design$names)) beta else NULL),
      predictor_design = design)
  }
  fixture
}

.ssm_coefficient_options <- function(predictors) {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- .ssm_numeric_options(predictors)
  options[names(assignments)] <- assignments
  for (name in c("plotsStateTimeSeries", "plotsPosteriorPredictive", "plotsLatestDefectRate",
                 "plotsPredictorCoefficients", "mcmcDiagnosticsOverviewTable", "mcmcDiagnosticsPrint"))
    options[[name]] <- FALSE
  options
}

.ssm_expect_coefficients <- function(result, predictors, fixture) {
  table <- result$results$predictorSummary
  if (!length(predictors)) {
    expect_null(table)
    return(invisible(NULL))
  }
  expect_identical(table$title, "Predictor Coefficients")
  expect_null(table$error)
  rows <- table$data
  expect_length(rows, length(predictors))
  expect_identical(vapply(rows, function(row) row$predictor, character(1)),
                   unname(fixture$labels[predictors]))
  for (column in c("mean", "median", "lower", "upper")) {
    values <- vapply(rows, function(row) row[[column]], numeric(1))
    expect_equal(values, fixture$expected[predictors, column], tolerance = 1e-12)
  }
  fields <- vapply(table$schema$fields, function(field) field$name, character(1))
  expect_identical(fields, c("predictor", "mean", "median", "lower", "upper"))
  order <- vapply(result$results$.meta, function(item) item$name, character(1))
  expect_equal(match("predictorSummary", order), match("summaryTable", order) + 1L)
}

test_that("automatic coefficient summaries use all draws and saved labels for one to three predictors", {
  fixture <- .ssm_coefficient_fixture()
  # Only fitting and unrelated diagnostics are replaced; result construction and
  # table serialization use the normal JASP runner and production builders.
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fixture$fit,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("This output fixture must not load Stan."),
    .package = "jaspAcceptanceSampling")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(.ssm_numeric_data(), csv, row.names = FALSE)
  for (predictors in list("x1", c("x2", "x1"), c("x3", "x1", "x2"), character())) {
    result <- jaspTools::runAnalysis("BayesianSSMforAS", csv,
      .ssm_coefficient_options(predictors), view = FALSE)
    expect_identical(result$status, "complete")
    .ssm_expect_coefficients(result, predictors, fixture)
  }
  expect_equal(fixture$generations, 4L)
})

