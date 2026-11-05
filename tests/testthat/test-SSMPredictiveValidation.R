.ssm_predictive_api <- function() {
  list(
    pmf = getFromNamespace(".asSSM_predictivePMF", "jaspAcceptanceSampling"),
    validate = getFromNamespace(".asSSM_validatePMF", "jaspAcceptanceSampling"),
    below = getFromNamespace(".asSSM_pmfMassBelow", "jaspAcceptanceSampling"),
    above = getFromNamespace(".asSSM_pmfMassAbove", "jaspAcceptanceSampling"),
    median = getFromNamespace(".asSSM_pmfMedian", "jaspAcceptanceSampling")
  )
}

test_that("valid predictive draws retain their exact empirical weights and finite-lot support", {
  api <- .ssm_predictive_api()
  pmf <- api$pmf(c(4, 5, 5, 6), sT = 1000, nT = 998, yT = 4)
  expect_equal(pmf$defects, 4:6)
  expect_equal(pmf$prob[pmf$defects %in% 4:6], c(0.25, 0.5, 0.25))
  expect_equal(sum(pmf$prob[pmf$defects < 4 | pmf$defects > 6]), 0)
  expect_equal(sum(pmf$prob), 1)
  expect_equal(api$median(pmf), 5)
  expect_silent(api$validate(pmf))
  expect_equal(api$pmf(array(c(4L, 5L, 5L, 6L), dim = c(2, 2)), 1000L, 998L, 4L), pmf)
})

test_that("zero inspection, full inspection and a zero-sized lot have valid predictive PMFs", {
  api <- .ssm_predictive_api()
  uninspected <- api$pmf(c(0, 0, 1, 2), sT = 2, nT = 0, yT = 0)
  expect_equal(uninspected$defects, 0:2)
  expect_equal(uninspected$prob, c(0.5, 0.25, 0.25))
  census <- api$pmf(rep(4, 3), sT = 20, nT = 20, yT = 4)
  expect_equal(census$prob[census$defects == 4], 1)
  expect_equal(sum(census$prob[census$defects != 4]), 0)
  zero <- api$pmf(c(0, 0), sT = 0, nT = 0, yT = 0)
  expect_equal(zero$defects, 0)
  expect_equal(zero$prob, 1)
  expect_equal(api$median(zero), 0)
})

test_that("empty, nonfinite and nonnumeric draws are rejected without dropping or coercion", {
  api <- .ssm_predictive_api()
  invalid <- list(
    NULL, numeric(0), c(NA_real_, NA_real_), c(4, NA_real_, 5),
    c(4, NaN, 5), c(4, Inf), c(4, -Inf), c("4", "5"),
    factor(c(4, 5)), list(4, 5), c(TRUE, FALSE), c(4 + 0i, 5 + 0i)
  )
  for (draws in invalid)
    expect_error(api$pmf(draws, sT = 1000, nT = 78, yT = 4))
})

test_that("fractional and impossible predictive counts are rejected rather than repaired", {
  api <- .ssm_predictive_api()
  # Four observed defects plus two uninspected items imply support exactly 4:6.
  for (draws in list(c(4, 4.5, 5), c(4, 5 + 1e-10), c(3, 4), c(6, 7), c(-1, 4), c(4, 1001)))
    expect_error(api$pmf(draws, sT = 1000, nT = 998, yT = 4))
  expect_error(api$pmf(c(3, 4), sT = 20, nT = 20, yT = 4))
  expect_error(api$pmf(c(4, 5), sT = 20, nT = 20, yT = 4))
  expect_error(api$pmf(c(0, 1), sT = 0, nT = 0, yT = 0))
})

test_that("lot information must be supplied as valid scalar integer counts", {
  api <- .ssm_predictive_api()
  expect_error(api$pmf(c(4, 5), sT = 1000))
  for (field in c("sT", "nT", "yT")) {
    for (value in list(NULL, numeric(0), c(1, 2), NA_real_, Inf, -1, 0.5, "1", TRUE, 1 + 0i)) {
      arguments <- list(y_pred_draws = c(4, 5), sT = 1000, nT = 78, yT = 4)
      arguments[field] <- list(value)
      expect_error(do.call(api$pmf, arguments))
    }
  }
  expect_error(api$pmf(5, sT = 10, nT = 4, yT = 5))
  expect_error(api$pmf(4, sT = 10, nT = 11, yT = 4))
  expect_error(api$pmf(4, sT = .Machine$integer.max, nT = 78, yT = 4))
  expect_error(api$pmf(4, sT = .Machine$integer.max + 1, nT = 78, yT = 4))
})

test_that("PMF validation rejects malformed support or invalid probability measures", {
  api <- .ssm_predictive_api()
  invalid <- list(
    NULL,
    data.frame(defects = numeric(0), prob = numeric(0)),
    c(0.5, 0.5),
    data.frame(defects = 0:1),
    data.frame(prob = c(0.5, 0.5)),
    data.frame(defects = c(0, 0), prob = c(0.5, 0.5)),
    data.frame(defects = c(-1, 0), prob = c(0.5, 0.5)),
    data.frame(defects = c(0, 0.5), prob = c(0.5, 0.5)),
    data.frame(defects = c(0, NA_real_), prob = c(0.5, 0.5)),
    data.frame(defects = c(0, Inf), prob = c(0.5, 0.5)),
    data.frame(defects = c("0", "1"), prob = c(0.5, 0.5)),
    data.frame(defects = c(0 + 0i, 1 + 0i), prob = c(0.5, 0.5)),
    data.frame(defects = 0:1, prob = c("0.5", "0.5")),
    data.frame(defects = 0:1, prob = c(0.5 + 0i, 0.5 + 0i)),
    data.frame(defects = 0:1, prob = c(1.1, -0.1)),
    data.frame(defects = 0:1, prob = c(0.5, NA_real_)),
    data.frame(defects = 0:1, prob = c(0.5, Inf)),
    data.frame(defects = 0:1, prob = c(0, 0)),
    data.frame(defects = 0:1, prob = c(0.2, 0.3)),
    data.frame(defects = 0:1, prob = c(0.6, 0.6))
  )
  for (pmf in invalid) expect_error(api$validate(pmf))
  expect_silent(api$validate(data.frame(defects = 4:6, prob = c(0.25, 0.5, 0.25))))
  expect_silent(api$validate(data.frame(defects = 0:1, prob = c(0.5, 0.5 + 5e-9))))
  expect_error(api$validate(data.frame(defects = 0:1, prob = c(0.5, 0.5 + 2e-8))))
  expect_error(api$validate(data.frame(defects = 0:1, prob = c(0.5, 0.5 - 2e-8))))
})

test_that("validated predictive masses retain strict AQL and RQL boundaries", {
  api <- .ssm_predictive_api()
  pmf <- api$pmf(c(4, 5, 5, 6), sT = 1000, nT = 998, yT = 4)
  thresholds <- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5)
  expect_equal(vapply(thresholds, function(x) api$below(pmf, x), numeric(1)),
               c(0, 0, 0.25, 0.25, 0.75, 0.75, 1))
  expect_equal(vapply(thresholds, function(x) api$above(pmf, x), numeric(1)),
               c(1, 0.75, 0.75, 0.25, 0.25, 0, 0))
  invalid <- data.frame(defects = 0:1, prob = c(0, 0))
  expect_error(api$below(invalid, 1))
  expect_error(api$above(invalid, 1))
  expect_error(api$median(invalid))
})

test_that("decision notes follow strict rules and changed thresholds without refitting or accumulation", {
  ns <- asNamespace("jaspAcceptanceSampling")
  original <- get("bayesianSSMforASInternal", ns)
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(ssm_count = "y", ssm_sampleSize = "n", ssm_total = "s",
                      ssm_time = "lot", ssm_predictor = character())
  options[names(assignments)] <- lapply(assignments, function(columns) {
    if (!length(columns)) return(columns)
    list(value = columns, types = as.list(rep("scale", length(columns))))
  })
  for (name in c("ssm_statePlot", "ssm_predictionPlot", "ssm_posteriorDistPlot",
                 "ssm_plotBeta", "ssm_showMcmcSummary", "ssm_printDiagnostics"))
    options[[name]] <- FALSE
  options$ssm_controlLimitsLower <- .04
  options$ssm_controlLimitsUpper <- .06
  options$ssm_decisionAcceptThreshold <- .25
  options$ssm_decisionRejectThreshold <- .15
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  fixture_data <- data.frame(y = 0, n = 0, s = 100, lot = 1)
  utils::write.csv(fixture_data, csv, row.names = FALSE)
  generations <- 0L
  visited <- 0L
  fit_fixture <- function(jaspResults, dataset, options) {
    if (is.null(jaspResults[["fit"]])) {
      state <- jaspBase::createJaspState()
      state$dependOn(get(".asSSM_fitDependencies", ns)())
      jaspResults[["fit"]] <- state
    }
    if (!is.null(jaspResults[["fit"]]$object)) return()
    generations <<- generations + 1L
    # Counts exactly at AQL=4 and RQL=6 do not belong to either strict tail.
    jaspResults[["fit"]]$object <- list(samples = list(y_pred = c(3, 4, 5, 6, 7)))
  }
  wrapper <- function(jaspResults, dataset, options) {
    original(jaspResults, dataset, options)
    cached <- jaspResults[["fit"]]$object
    jaspResults$.__enclos_env__$private$complete()
    cases <- data.frame(
      accept = c(.25, .15, .15, .20, .15, .125, .125),
      reject = c(.15, .25, .15, .25, .20, .375, .375),
      decision = c("Accept", "Reject", "Continue", "Continue", "Continue", "Reject", "Reject"),
      prefix = c("Acceptance", "Rejection", "Further sampling", "Further sampling",
                 "Further sampling", "Rejection", "Rejection"))
    percentages <- list("25%", c("15%", "25%"), "15%", c("20%", "25%"),
                        c("15%", "20%"), c("12.5%", "37.5%"), c("12.5%", "37.5%"))
    for (i in seq_len(nrow(cases))) {
      options$ssm_decisionAcceptThreshold <- cases$accept[i]
      options$ssm_decisionRejectThreshold <- cases$reject[i]
      # Preserve scalar, array and integer/double formats for unchanged options.
      nativeOptions <- options
      typeNames <- names(nativeOptions)[endsWith(names(nativeOptions), ".types")]
      nativeOptions[typeNames] <- lapply(nativeOptions[typeNames], as.list)
      jaspResults$.__enclos_env__$private$changeOptions(as.character(jsonlite::toJSON(
        nativeOptions, auto_unbox = TRUE, digits = NA, null = "null", always_decimal = TRUE)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, dataset, options)
      # Repeated builder calls on the same table must not append duplicate notes.
      get(".asSSM_createSummaryTable", ns)(jaspResults, dataset, options)
      get(".asSSM_createSummaryTable", ns)(jaspResults, dataset, options)
      jaspResults$.__enclos_env__$private$complete()
      result <- jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults),
                                   simplifyVector = FALSE)
      table <- result$results$summaryTable
      row <- table$data[[1L]]
      expect_equal(row$massAQL, .20)
      expect_equal(row$massRQL, .20)
      expect_identical(row$decision, paste("Consider to", cases$decision[i]))
      expect_length(table$footnotes, 1L)
      note <- table$footnotes[[1L]]$text
      expect_true(startsWith(note, cases$prefix[i]))
      for (percentage in percentages[[i]]) expect_match(note, percentage, fixed = TRUE)
      if (!(.20 %in% c(cases$accept[i], cases$reject[i])))
        expect_false(grepl("20%", note, fixed = TRUE))
      expect_false(grepl("0.2", note, fixed = TRUE))
      expect_identical(jaspResults[["fit"]]$object, cached)
      expect_equal(generations, 1L)
      visited <<- visited + 1L
    }
  }
  testthat::local_mocked_bindings(
    .asSSM_fitModel = fit_fixture,
    .asSSM_reportDiagnostics = function(...) invisible(NULL),
    .asSSM_getStanModel = function(...) stop("Decision-note fixtures must not load Stan."),
    bayesianSSMforASInternal = wrapper,
    .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_equal(visited, 7L)
  expect_equal(generations, 1L)
})
