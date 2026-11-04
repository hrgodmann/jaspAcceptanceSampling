# Exercise stable Bayes factors through actual JASP tables and HTML output.

.bsStableBFOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$showPlansplan <- FALSE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$priorplan <- "custom"
  options$alphaplan <- 1
  options$betaplan <- 100
  options$aqlplan <- .05
  options$rqlplan <- .5
  options$min_bfplan <- 1.5
  options$max_nplan <- 1
  options$inferPosteriorinfer <- TRUE
  options$data_ninfer <- 1
  options$data_dinfer <- 0
  options$lot_sizeinfer <- 100
  options$showInferenceTableinfer <- TRUE
  options$posteriorPlotinfer <- TRUE
  options$ppdPlotinfer <- FALSE
  options
}

.bsStableBFRowForTest <- function(table, label) {
  rows <- Filter(function(row) identical(row$col_1, label), table$data)
  if (length(rows) != 1L) return(NA_character_)
  rows[[1L]]$col_2
}

.bsLogFromDisplayedBFForTest <- function(value) {
  if (length(value) != 1L || is.na(value)) return(NA_real_)
  pieces <- strsplit(as.character(value), "[eE]")[[1L]]
  if (length(pieces) > 2L) return(NA_real_)
  mantissa <- suppressWarnings(as.numeric(pieces[1L]))
  exponent <- if (length(pieces) == 2L) suppressWarnings(as.numeric(pieces[2L])) else 0
  if (!is.finite(mantissa) || mantissa <= 0 || !is.finite(exponent)) return(NA_real_)
  log(mantissa) + exponent * log(10)
}

.bsHTMLLogBFForTest <- function(output) {
  text <- paste(output$rawtext, collapse = "")
  found <- regmatches(text, regexec("is approximately <b>([^<]+)</b>", text))[[1L]]
  if (length(found) != 2L) return(NA_real_)
  .bsLogFromDisplayedBFForTest(found[2L])
}

.bsStableBFSequenceForTest <- function(options, changes) {
  original <- get(".bsInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  observed <- list()
  wrapper <- function(jaspResults, options, position) {
    original(jaspResults, options, position)
    private <- jaspResults$.__enclos_env__$private
    readCollection <- function() {
      private$complete()
      result <- jsonlite::fromJSON(
        jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      result$results$infContainer$collection
    }
    observed[[1L]] <<- readCollection()
    for (i in seq_along(changes)) {
      options[names(changes[[i]])] <- changes[[i]]
      private$changeOptions(as.character(jsonlite::toJSON(options)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      observed[[i + 1L]] <<- readCollection()
    }
  }
  testthat::local_mocked_bindings(.bsInferenceAnalysis = wrapper,
                                 .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_length(observed, length(changes) + 1L)
  observed
}

test_that("Bayesian sampling - extreme prior odds preserve finite evidence and acceptance numbers", {
  options <- .bsStableBFOptionsForTest()
  observed <- .bsStableBFSequenceForTest(options, list(
    list(alphaplan = 100, betaplan = 1, data_dinfer = 1),
    list(data_dinfer = 0)))
  # Beta(1,b) and Beta(a,1) give these limits analytically at RQL=.5.
  expected_bf <- c(2, .5, 51)
  for (i in seq_along(observed)) {
    collection <- observed[[i]]
    table <- collection$infContainer_inferenceDecisionTable
    expect_null(table[["error"]])
    expect_equal(as.numeric(.bsStableBFRowForTest(table, "Bayes Factor")), expected_bf[i])
    expect_identical(.bsStableBFRowForTest(table, "Acceptance number (c)"), "0")
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- collection[[key]]$rawtext
      expect_equal(.bsHTMLLogBFForTest(collection[[key]]), log(expected_bf[i]), tolerance = 1e-10)
      expected_status <- if (i == 2L) "has not been reached" else "has been reached"
      expect_match(text, expected_status, fixed = TRUE)
    }
  }
})

test_that("Bayesian sampling - prior tail saturation does not remove a valid planning row", {
  options <- .bsStableBFOptionsForTest()
  options$inferPosteriorinfer <- FALSE
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  collection <- result$results$planContainer$collection
  plan <- collection$planContainer_planTable
  three <- collection$planContainer_threeBFTable
  expect_null(plan[["error"]])
  expect_null(three[["error"]])
  expect_length(plan$data, 1L)
  expect_length(three$data, 1L)
  row <- plan$data[[1L]]
  expect_equal(row$n, 1)
  expect_equal(row$c, 0)
  expect_true(is.numeric(row$bf_odds))
  expect_equal(row$bf_odds, 2, tolerance = 1e-12)
  region_logs <- function(b) {
    la <- b * log(.95)
    lr <- b * log(.5)
    c(good = log1p(-exp(la)), middle = la + log1p(-exp(lr - la)), bad = lr)
  }
  prior <- region_logs(100)
  post <- region_logs(101)
  expected <- c(BF_GB = (post[1L] - post[3L]) - (prior[1L] - prior[3L]),
                BF_GM = (post[1L] - post[2L]) - (prior[1L] - prior[2L]),
                BF_MB = (post[2L] - post[3L]) - (prior[2L] - prior[3L]))
  row <- three$data[[1L]]
  for (key in c("BF_GB", "BF_GM", "BF_MB")) {
    expect_true(is.numeric(row[[key]]))
    expect_equal(log(row[[key]]), unname(expected[startsWith(names(expected), key)]),
                 tolerance = 1e-10)
  }
})

test_that("Bayesian sampling - infeasible plans report the actual maximum Bayes factor", {
  options <- .bsStableBFOptionsForTest()
  options$inferPosteriorinfer <- FALSE
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  options$min_bfplan <- 3
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  collection <- result$results$planContainer$collection
  for (key in c("planContainer_planTable", "planContainer_threeBFTable")) {
    output <- collection[[key]]
    expect_identical(output$status, "error")
    text <- paste(output$error$errorMessage, collapse = "")
    found <- regmatches(text, regexec("maximum attainable Bayes factor is ([0-9.eE+-]+)", text))[[1L]]
    expect_length(found, 2L)
    if (length(found) == 2L)
      expect_equal(.bsLogFromDisplayedBFForTest(sub("\\.$", "", found[2L])), log(2), tolerance = 1e-10)
  }
})

test_that("Bayesian sampling - overflowing and underflowing Bayes factors remain informative", {
  options <- .bsStableBFOptionsForTest()
  options$priorplan <- "uniform"
  options$data_ninfer <- 10000
  options$lot_sizeinfer <- 10000
  options$min_bfplan <- 30
  observed <- .bsStableBFSequenceForTest(options,
                                        list(list(data_dinfer = 10000), list(data_dinfer = 0)))
  # Posterior odds are 2^(n+1)-1 at d=0, and their reciprocal at d=n.
  expected_log_bf <- c(1, -1, 1) * 10001 * log(2)
  for (i in seq_along(observed)) {
    collection <- observed[[i]]
    table <- collection$infContainer_inferenceDecisionTable
    expect_null(table[["error"]])
    value <- .bsStableBFRowForTest(table, "Bayes Factor")
    expect_true(is.finite(.bsLogFromDisplayedBFForTest(value)))
    expect_lt(abs(.bsLogFromDisplayedBFForTest(value) - expected_log_bf[i]), .001)
    # The binomial-tail identity gives the largest qualifying count as 4907.
    expect_identical(.bsStableBFRowForTest(table, "Acceptance number (c)"), "4907")
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      output <- collection[[key]]
      expect_true(is.finite(.bsHTMLLogBFForTest(output)))
      expect_lt(abs(.bsHTMLLogBFForTest(output) - expected_log_bf[i]), .003)
      direction <- if (i == 2L) "at or above the RQL" else "below the RQL"
      expect_match(output$rawtext, paste("Strong evidence that the proportion defective is", direction), fixed = TRUE)
      expected_status <- if (i == 2L) "has not been reached" else "has been reached"
      expect_match(output$rawtext, expected_status, fixed = TRUE)
    }
  }
})

test_that("Bayesian sampling - three-region underflow neither drops plans nor prints zero Bayes factors", {
  options <- .bsStableBFOptionsForTest()
  options$inferPosteriorinfer <- FALSE
  options$priorplan <- "uniform"
  options$aqlplan <- 1e-6
  options$rqlplan <- .9
  options$max_nplan <- 100
  options$min_bfplan <- 30
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  reference_rows <- lapply(seq_len(100), function(n) {
    d <- 0:n
    log_bf <- pbeta(.9, d + 1, n - d + 1, log.p = TRUE) -
      pbeta(.9, d + 1, n - d + 1, lower.tail = FALSE, log.p = TRUE) - log(9)
    qualifying <- d[log_bf >= log(30)]
    if (!length(qualifying)) return(NULL)
    data.frame(n = n, c = max(qualifying))
  })
  reference <- do.call(rbind, reference_rows)
  reference <- reference[!duplicated(reference$c), ]
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  collection <- result$results$planContainer$collection
  plan <- collection$planContainer_planTable
  three <- collection$planContainer_threeBFTable
  expect_null(plan[["error"]])
  expect_null(three[["error"]])
  expect_length(three$data, 82L)
  expect_length(plan$data, 82L)
  for (output in list(plan, three)) {
    expect_equal(vapply(output$data, function(row) row$n, numeric(1)), reference$n)
    expect_equal(vapply(output$data, function(row) row$c, numeric(1)), reference$c)
  }
  lg <- with(reference, pbeta(1e-6, c + 1, n - c + 1, log.p = TRUE))
  lr <- with(reference, pbeta(.9, c + 1, n - c + 1, log.p = TRUE))
  lb <- with(reference, pbeta(.9, c + 1, n - c + 1, lower.tail = FALSE, log.p = TRUE))
  lm <- lr + log1p(-exp(lg - lr))
  expected_logs <- list(BF_GB = lg - lb - log(1e-6 / .1),
                        BF_GM = lg - lm - log(1e-6 / (.9 - 1e-6)),
                        BF_MB = lm - lb - log((.9 - 1e-6) / .1))
  for (key in names(expected_logs)) {
    values <- lapply(three$data, function(row) row[[key]])
    actual_logs <- vapply(values, .bsLogFromDisplayedBFForTest, numeric(1))
    expect_true(all(is.finite(actual_logs)))
    scientific <- vapply(values, function(value) is.character(value) &&
      grepl("[eE]", value), logical(1))
    if (any(scientific))
      expect_lt(max(abs(actual_logs[scientific] - expected_logs[[key]][scientific])), .001)
    if (any(!scientific)) {
      ordinary <- vapply(values[!scientific], as.numeric, numeric(1))
      # Fixed-point strings are rounded to three decimal places by design.
      expect_lt(max(abs(ordinary - exp(expected_logs[[key]][!scientific]))), .00051)
    }
    field <- Filter(function(field) identical(field$name, key), three$schema$fields)[[1L]]
    expected_type <- if (key == "BF_MB") "number" else "string"
    expect_identical(field$type, expected_type)
    expect_true(all(vapply(values, if (key == "BF_MB") is.numeric else is.character, logical(1))))
  }
  notes <- paste(vapply(three$footnotes, function(note) note$text, character(1)), collapse = " ")
  expect_match(notes, "log probabilities", fixed = TRUE)
})


test_that("Bayesian sampling - cached pre-logarithmic plans are recomputed", {
  options <- .bsStableBFOptionsForTest()
  options$inferPosteriorinfer <- FALSE
  options$showPlansplan <- TRUE
  options$min_bfplan <- .75
  ns <- asNamespace("jaspAcceptanceSampling")
  original <- get(".bsPlanningAnalysis", ns)
  original_generator <- get(".bsGeneratePlans", ns)
  generations <- 0L
  observed_generations <- integer()
  generator <- function(...) {
    generations <<- generations + 1L
    original_generator(...)
  }
  wrapper <- function(jaspResults, options, position) {
    original(jaspResults, options, position)
    observed_generations <<- c(observed_generations, generations)
    jaspBase:::emptyRecomputed()
    original(jaspResults, options, position)
    observed_generations <<- c(observed_generations, generations)
    # This is the actual old clipped-BF result for these inputs: c=1, BF=1.
    # Saved states from that implementation have no log-BF error tolerance.
    jaspResults[["planData"]]$object <- data.frame(n = 1L, c = 1L, bf = 1, log_bf = 0)
    jaspBase:::emptyRecomputed()
    original(jaspResults, options, position)
    observed_generations <<- c(observed_generations, generations)
  }
  testthat::local_mocked_bindings(.bsPlanningAnalysis = wrapper,
                                 .bsGeneratePlans = generator,
                                 .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_equal(generations, 2L)
  expect_equal(observed_generations, c(1L, 1L, 2L))
  table <- result$results$planContainer$collection$planContainer_planTable
  expect_null(table[["error"]])
  expect_length(table$data, 1L)
  expect_equal(table$data[[1L]]$n, 1)
  expect_equal(table$data[[1L]]$c, 0)
  expect_equal(table$data[[1L]]$bf_odds, 2, tolerance = 1e-12)
})


test_that("Bayesian sampling - tiny representable and subnormal Bayes factors use suitable columns", {
  for (max_n in c(30L, 70L)) {
    options <- .bsStableBFOptionsForTest()
    options$inferPosteriorinfer <- FALSE
    options$priorplan <- "uniform"
    options$aqlplan <- 1e-6
    options$rqlplan <- .9
    options$max_nplan <- max_n
    options$min_bfplan <- 30
    options$showThreeBFplan <- TRUE
    result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                    options = options, view = FALSE)
    expect_identical(result$status, "complete")
    table <- result$results$planContainer$collection$planContainer_threeBFTable
    expect_null(table[["error"]])
    final <- tail(table$data, 1L)[[1L]]
    # Beta-binomial identity: the largest qualifying count is one below this quantile.
    expected_c <- stats::qbinom(1/271, max_n + 1, .9) - 1
    candidates <- seq.int(max(1, expected_c), max_n)
    binary <- pbeta(.9, expected_c + 1, candidates - expected_c + 1, log.p = TRUE) -
      pbeta(.9, expected_c + 1, candidates - expected_c + 1, lower.tail = FALSE, log.p = TRUE) - log(9)
    expected_n <- min(candidates[binary >= log(30)])
    expected_log <- pbeta(1e-6, expected_c + 1, expected_n - expected_c + 1, log.p = TRUE) -
      pbeta(.9, expected_c + 1, expected_n - expected_c + 1, lower.tail = FALSE, log.p = TRUE) -
      log(1e-6 / .1)
    expect_equal(final$n, expected_n)
    expect_equal(final$c, expected_c)
    field <- Filter(function(field) identical(field$name, "BF_GB"), table$schema$fields)[[1L]]
    if (max_n == 30L) {
      expect_identical(field$type, "number")
      expect_identical(field$format, "sf:3")
      expect_true(is.numeric(final$BF_GB))
      expect_gt(final$BF_GB, .Machine$double.xmin)
      expect_equal(log(final$BF_GB), expected_log, tolerance = 1e-10)
    } else {
      # This nonzero subnormal BF has reduced numerical precision as a double.
      expect_gt(exp(expected_log), 0)
      expect_lt(exp(expected_log), .Machine$double.xmin)
      expect_identical(field$type, "string")
      expect_type(final$BF_GB, "character")
      expect_lt(abs(.bsLogFromDisplayedBFForTest(final$BF_GB) - expected_log), .001)
    }
  }
})


test_that("Bayesian sampling - numerical planning failures remain explicit and preserve the prior", {
  options <- .bsStableBFOptionsForTest()
  options$inferPosteriorinfer <- FALSE
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  options$priorPlotplan <- TRUE
  original_binary <- get(".bsLogBF", asNamespace("jaspAcceptanceSampling"))
  original_three <- get(".bsLogBFThreeHyp", asNamespace("jaspAcceptanceSampling"))
  failure <- function(...) jaspAcceptanceSampling:::.bsNumericalError()
  for (failed in c("binary", "three")) {
    result <- testthat::with_mocked_bindings(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      .bsLogBF = if (failed == "binary") failure else original_binary,
      .bsLogBFThreeHyp = if (failed == "three") failure else original_three,
      .package = "jaspAcceptanceSampling")
    expect_identical(result$status, "complete")
    collection <- result$results$planContainer$collection
    prior <- collection$planContainer_distPlotPrior
    expect_identical(prior$status, "complete")
    expect_null(prior[["error"]])
    expect_true(prior$data %in% names(result$state$figures))
    for (key in c("planContainer_planTable", "planContainer_threeBFTable")) {
      output <- collection[[key]]
      expect_identical(output$status, "error")
      message <- paste(output$error$errorMessage, collapse = "")
      expect_match(message, "Bayes factor could not be calculated reliably", fixed = TRUE)
      expect_false(grepl("No sampling plan|maximum attainable", message))
      expect_length(output$data, 0L)
    }
  }
})

test_that("Bayesian sampling - numerical inference errors are visible and recover without stale output", {
  options <- .bsStableBFOptionsForTest()
  options$data_dinfer <- 1
  ns <- asNamespace("jaspAcceptanceSampling")
  original <- get(".bsInferenceAnalysis", ns)
  original_binary <- get(".bsLogBF", ns)
  fail_now <- TRUE
  observed <- list()
  binary <- function(...) {
    if (fail_now) jaspAcceptanceSampling:::.bsNumericalError()
    original_binary(...)
  }
  wrapper <- function(jaspResults, options, position) {
    private <- jaspResults$.__enclos_env__$private
    readCollection <- function() {
      private$complete()
      result <- jsonlite::fromJSON(
        jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      result$results$infContainer$collection
    }
    original(jaspResults, options, position)
    observed[[1L]] <<- readCollection()
    for (i in seq_along(c(FALSE, TRUE, FALSE))) {
      fail_now <<- c(FALSE, TRUE, FALSE)[i]
      options$data_dinfer <- if (fail_now) 1 else 0
      private$changeOptions(as.character(jsonlite::toJSON(options)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      observed[[i + 1L]] <<- readCollection()
    }
  }
  testthat::local_mocked_bindings(.bsInferenceAnalysis = wrapper, .bsLogBF = binary,
                                 .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_length(observed, 4L)
  for (i in c(1L, 3L)) {
    collection <- observed[[i]]
    error <- collection$infContainer_numericalError
    expect_identical(error$status, "error")
    expect_match(paste(error$error$errorMessage, collapse = ""),
                 "Bayes factor could not be calculated reliably", fixed = TRUE)
    for (key in c("inferenceDecisionTable", "decisionSummaryText", "distPlotPosterior", "bfText", "ppdPlot"))
      expect_null(collection[[paste0("infContainer_", key)]])
  }
  for (i in c(2L, 4L)) {
    collection <- observed[[i]]
    expect_null(collection$infContainer_numericalError)
    table <- collection$infContainer_inferenceDecisionTable
    expect_identical(table$status, "complete")
    expect_null(table[["error"]])
    expect_equal(as.numeric(.bsStableBFRowForTest(table, "Bayes Factor")), 2)
    expect_identical(.bsStableBFRowForTest(table, "Acceptance number (c)"), "0")
    expect_match(collection$infContainer_decisionSummaryText$rawtext,
                 "has been reached", fixed = TRUE)
  }
})
