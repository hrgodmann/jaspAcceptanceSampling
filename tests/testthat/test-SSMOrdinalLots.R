.ssm_ordinal_options <- function() {
  list(ssm_count = "count", ssm_sampleSize = "n", ssm_total = "s",
       ssm_time = "lot", ssm_predictor = "")
}

.ssm_ordinal_data <- function() {
  data.frame(count = c(13, 2, 7), n = c(100, 100, 100), s = c(1000, 1000, 1000),
             lot = c(120, 10, 40), row_id = c("last", "first", "middle"))
}

test_that("ordered numeric lot labels preserve values and sort their complete rows", {
  read <- getFromNamespace(".asSSM_readData", "jaspAcceptanceSampling")
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  numeric <- .ssm_ordinal_data()
  ordinal <- numeric
  # Deliberately disagree with the numeric label order; an unused text level is harmless.
  ordinal$lot <- ordered(as.character(numeric$lot), levels = c("120", "40", "10", "unused"))
  original <- ordinal
  result <- read(ordinal, .ssm_ordinal_options())
  expect_identical(result, read(numeric, .ssm_ordinal_options()))
  expect_identical(result$lot, c(10, 40, 120))
  expect_identical(result$row_id, c("first", "middle", "last"))
  expect_null(validate(result, .ssm_ordinal_options()))
  expect_identical(ordinal, original)
})

test_that("ordered numeric lot labels need not be positive consecutive integers", {
  read <- getFromNamespace(".asSSM_readData", "jaspAcceptanceSampling")
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- .ssm_ordinal_data()
  data$lot <- ordered(c("1e3", "-2.5", "0.25"), levels = c("1e3", "0.25", "-2.5"))
  result <- read(data, .ssm_ordinal_options())
  expect_identical(result$lot, c(-2.5, 0.25, 1000))
  expect_identical(result$row_id, c("first", "middle", "last"))
  expect_null(validate(result, .ssm_ordinal_options()))
})

test_that("ordinal conversion preserves missing lots and rejects numeric aliases", {
  read <- getFromNamespace(".asSSM_readData", "jaspAcceptanceSampling")
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- .ssm_ordinal_data()
  data$lot <- ordered(c(NA, "10", "40"))
  result <- read(data, .ssm_ordinal_options())
  expect_equal(nrow(result), nrow(data))
  expect_identical(result$lot, c(10, 40, NA_real_))
  expect_identical(result$row_id, c("first", "middle", "last"))
  expect_match(validate(result, .ssm_ordinal_options()), "Lot Number.*missing", ignore.case = TRUE)

  data$lot <- ordered(rep(NA_character_, 3), levels = c("10", "40", "120"))
  result <- read(data, .ssm_ordinal_options())
  expect_equal(nrow(result), 3)
  expect_true(is.numeric(result$lot))
  expect_true(all(is.na(result$lot)))
  expect_match(validate(result, .ssm_ordinal_options()), "Lot Number.*missing", ignore.case = TRUE)

  data$lot <- ordered(c("01", "1", "2"))
  result <- read(data, .ssm_ordinal_options())
  expect_identical(result$lot, c(1, 1, 2))
  expect_match(validate(result, .ssm_ordinal_options()), "lot number.*only once", ignore.case = TRUE)
})

test_that("non-numeric ordered labels and unordered factors are not replaced with codes", {
  read <- getFromNamespace(".asSSM_readData", "jaspAcceptanceSampling")
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  for (labels in list(ordered(c("Lot C", "Lot A", "Lot B")),
                      ordered(c("120", "not a number", "40")),
                      factor(c("120", "10", "40")), c("120", "10", "40"))) {
    data <- .ssm_ordinal_data()
    data$lot <- labels
    result <- read(data, .ssm_ordinal_options())
    expect_false(is.numeric(result$lot))
    expect_equal(nrow(result), nrow(data))
    expect_match(validate(result, .ssm_ordinal_options()), "Lot Number.*numeric", ignore.case = TRUE)
  }
})

test_that("preloaded ordinal lots give the same current-lot output and retain missing rows", {
  ns <- asNamespace("jaspAcceptanceSampling")
  original <- get("bayesianSSMforASInternal", ns)
  incoming <- list()
  normalized <- list()
  generations <- 0L
  wrapper <- function(jaspResults, dataset, options) {
    captured <- dataset
    names(captured) <- jaspSyntax::decodeColumnNames(names(captured))
    incoming[[length(incoming) + 1L]] <<- captured
    original(jaspResults, dataset, options)
  }
  fit <- function(jaspResults, dataset, options) {
    captured <- dataset
    names(captured) <- jaspSyntax::decodeColumnNames(names(captured))
    normalized[[length(normalized) + 1L]] <<- captured
    error <- get(".asSSM_validateInputs", ns)(dataset, options)
    if (!is.null(error)) {
      get(".asSSM_setAnalysisError", ns)(jaspResults, error)
      return()
    }
    generations <<- generations + 1L
    state <- jaspBase::createJaspState()
    state$dependOn(get(".asSSM_fitDependencies", ns)())
    state$object <- list(samples = list(y_pred = tail(dataset[[options$ssm_count]], 1L) + c(0, 1, 1, 2)))
    jaspResults[["fit"]] <- state
  }
  # The normal preloaded runner, data reader, validator and table builder remain real.
  # Deterministic posterior draws isolate input handling from Monte Carlo variation.
  testthat::local_mocked_bindings(
    bayesianSSMforASInternal = wrapper,
    .asSSM_fitModel = fit,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("Ordinal input fixtures must not load Stan."),
    .package = "jaspAcceptanceSampling")
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- .ssm_ordinal_options()
  options[names(assignments)] <- assignments
  for (name in c("ssm_count", "ssm_sampleSize", "ssm_total", "ssm_time"))
    options[[name]] <- list(value = assignments[[name]], types = list("scale"))
  for (name in c("ssm_statePlot", "ssm_predictionPlot", "ssm_posteriorDistPlot",
                 "ssm_plotBeta", "ssm_showMcmcSummary", "ssm_printDiagnostics"))
    options[[name]] <- FALSE

  numeric <- .ssm_ordinal_data()
  ordinal <- numeric
  ordinal$lot <- ordered(as.character(numeric$lot), levels = c("120", "40", "10"))
  # Exercise native input handling with both numeric and ordered lot labels.
  baseline <- jaspTools::runAnalysis("BayesianSSMforAS", numeric, options, view = FALSE)
  options$ssm_time$types <- list("ordinal")
  actual <- jaspTools::runAnalysis("BayesianSSMforAS", ordinal, options, view = FALSE)
  expect_identical(baseline$status, "complete")
  expect_identical(actual$status, "complete")
  expect_true(is.numeric(incoming[[1L]]$lot))
  expect_true(is.ordered(incoming[[2L]]$lot))
  expect_identical(normalized[[1L]], normalized[[2L]])
  expect_null(actual$results$placeholder)
  expect_identical(actual$results$summaryTable$data, baseline$results$summaryTable$data)
  expect_identical(actual$results$summaryTable$data[[1L]]$t, "120")
  expect_equal(actual$results$summaryTable$data[[1L]]$y, 13)
  expect_equal(generations, 2L)

  for (column in c("lot", "count")) {
    missing <- ordinal
    missing[[column]][1L] <- NA
    result <- jaspTools::runAnalysis("BayesianSSMforAS", missing, options, view = FALSE)
    expect_identical(result$status, "complete")
    expect_equal(nrow(tail(incoming, 1L)[[1L]]), 3L)
    expect_equal(nrow(tail(normalized, 1L)[[1L]]), 3L)
    expect_true(anyNA(tail(normalized, 1L)[[1L]][[column]]))
    expect_null(result$results$summaryTable)
    error <- result$results$placeholder$error
    expect_match(paste(unlist(error), collapse = " "),
                 if (column == "lot") "Lot Number.*missing" else "Defect Count.*missing", ignore.case = TRUE)
    expect_equal(generations, 2L)
  }
})
