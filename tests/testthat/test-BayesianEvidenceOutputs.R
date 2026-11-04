# Evidence strength and meeting the selected planning target are distinct outputs.

.bsEvidenceOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$showPlansplan <- FALSE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$priorplan <- "impartial"
  options$impartialCustomModeplan <- FALSE
  options$aqlplan <- .05
  options$rqlplan <- .15
  options$min_bfplan <- 30
  options$inferPosteriorinfer <- TRUE
  options$data_ninfer <- 30
  options$data_dinfer <- 0
  options$lot_sizeinfer <- 100
  options$showInferenceTableinfer <- TRUE
  options$posteriorPlotinfer <- TRUE
  options$ppdPlotinfer <- FALSE
  options
}

.bsEvidenceCollectionForTest <- function(result) {
  result$results$infContainer$collection
}

.bsEvidenceTextForTest <- function(output) {
  gsub("<[^>]+>", "", paste(output$rawtext, collapse = ""))
}

.bsEvidenceRowForTest <- function(table, label) {
  rows <- Filter(function(row) identical(row$col_1, label), table$data)
  if (length(rows) != 1L) return(NA_character_)
  rows[[1L]]$col_2
}

.bsEvidenceSequenceForTest <- function(options, changes) {
  original <- get(".bsInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
  observed <- list()
  wrapper <- function(jaspResults, options, position) {
    original(jaspResults, options, position)
    private <- jaspResults$.__enclos_env__$private
    private$complete()
    readCollection <- function() {
      result <- jsonlite::fromJSON(
        jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      .bsEvidenceCollectionForTest(result)
    }
    observed[[1L]] <<- readCollection()
    for (i in seq_along(changes)) {
      options[names(changes[[i]])] <- changes[[i]]
      private$changeOptions(as.character(jsonlite::toJSON(options)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      private$complete()
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

test_that("Bayesian sampling - changing the target updates status without changing evidence", {
  options <- .bsEvidenceOptionsForTest()
  observed <- .bsEvidenceSequenceForTest(options,
                                        list(list(min_bfplan = 1000), list(min_bfplan = 30)))
  expected_c <- c("1", "Not available", "1")
  expected_status <- c(
    "The selected Bayes-factor target of 30 has been reached.",
    "The selected Bayes-factor target of 1000 has not been reached.",
    "The selected Bayes-factor target of 30 has been reached.")
  for (i in seq_along(observed)) {
    collection <- observed[[i]]
    table <- collection$infContainer_inferenceDecisionTable
    expect_equal(as.numeric(.bsEvidenceRowForTest(table, "Bayes Factor")), 188.832)
    expect_identical(.bsEvidenceRowForTest(table, "Acceptance number (c)"), expected_c[i])
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- .bsEvidenceTextForTest(collection[[key]])
      expect_match(text, "Strong evidence that the proportion defective is below the RQL.", fixed = TRUE)
      expect_match(text, expected_status[i], fixed = TRUE)
      expect_match(text, "188.83", fixed = TRUE)
      expect_match(text, "use the unrounded Bayes factor", fixed = TRUE)
      expect_false(grepl("accepting the lot|rejecting the lot", text))
    }
  }
})

test_that("Bayesian sampling - evidence descriptions follow data in both inference outputs", {
  options <- .bsEvidenceOptionsForTest()
  observed <- .bsEvidenceSequenceForTest(options,
    lapply(c(1, 2, 4, 7, 8), function(d) list(data_dinfer = d)))
  expected_bf <- c(188.832, 31.223, 8.881, 1.478, .157, .069)
  expected_evidence <- c(
    "Strong evidence that the proportion defective is below the RQL.",
    "Strong evidence that the proportion defective is below the RQL.",
    "Moderate evidence that the proportion defective is below the RQL.",
    "Inconclusive evidence about whether the proportion defective is below the RQL.",
    "Moderate evidence that the proportion defective is at or above the RQL.",
    "Strong evidence that the proportion defective is at or above the RQL.")
  expected_status <- c(
    rep("The selected Bayes-factor target of 30 has been reached.", 2L),
    rep("The selected Bayes-factor target of 30 has not been reached.", 4L))
  for (i in seq_along(observed)) {
    collection <- observed[[i]]
    table <- collection$infContainer_inferenceDecisionTable
    expect_equal(as.numeric(.bsEvidenceRowForTest(table, "Bayes Factor")), expected_bf[i])
    expect_identical(.bsEvidenceRowForTest(table, "Acceptance number (c)"), "1")
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- .bsEvidenceTextForTest(collection[[key]])
      expect_match(text, expected_evidence[i], fixed = TRUE)
      expect_match(text, expected_status[i], fixed = TRUE)
      expect_false(grepl("accepting the lot|rejecting the lot", text))
    }
  }
})

test_that("Bayesian sampling - posterior-only output reports evidence and the selected target", {
  options <- .bsEvidenceOptionsForTest()
  options$showInferenceTableinfer <- FALSE
  options$min_bfplan <- 1000
  result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                  options = options, view = FALSE)
  collection <- .bsEvidenceCollectionForTest(result)
  expect_identical(result$status, "complete")
  expect_null(collection$infContainer_inferenceDecisionTable)
  expect_null(collection$infContainer_decisionSummaryText)
  plot <- collection$infContainer_distPlotPosterior
  expect_false(is.null(plot))
  expect_null(plot[["error"]])
  expect_identical(plot$status, "complete")
  text <- .bsEvidenceTextForTest(collection$infContainer_bfText)
  expect_match(text, "Strong evidence that the proportion defective is below the RQL.", fixed = TRUE)
  expect_match(text, "The selected Bayes-factor target of 1000 has not been reached.", fixed = TRUE)
  expect_match(text, "188.83", fixed = TRUE)
  expect_match(text, "use the unrounded Bayes factor", fixed = TRUE)
  expect_false(grepl("accepting the lot|rejecting the lot", text))
})


test_that("Bayesian sampling - target comparison allows equality and explains targets below one", {
  low_target <- .bsEvidenceOptionsForTest()
  low_target$data_dinfer <- 7
  low_target$min_bfplan <- .1
  equal_target <- .bsEvidenceOptionsForTest()
  equal_target$priorplan <- "uniform"
  equal_target$rqlplan <- .5
  equal_target$data_ninfer <- 1
  equal_target$min_bfplan <- 3
  cases <- list(low_target, equal_target)
  expected_bf <- c(.157, 3)
  expected_c <- c("7", "0")
  expected_evidence <- c(
    "Moderate evidence that the proportion defective is at or above the RQL.",
    "Moderate evidence that the proportion defective is below the RQL.")
  expected_target <- c(
    "The selected Bayes-factor target of 0.1 has been reached.",
    "The selected Bayes-factor target of 3 has been reached.")
  caveat <- paste("A target at or below 1 does not require evidence favoring",
                  "a proportion defective below the RQL.")
  for (i in seq_along(cases)) {
    result <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                    options = cases[[i]], view = FALSE)
    expect_identical(result$status, "complete")
    collection <- .bsEvidenceCollectionForTest(result)
    table <- collection$infContainer_inferenceDecisionTable
    expect_equal(as.numeric(.bsEvidenceRowForTest(table, "Bayes Factor")), expected_bf[i])
    expect_identical(.bsEvidenceRowForTest(table, "Acceptance number (c)"), expected_c[i])
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- .bsEvidenceTextForTest(collection[[key]])
      expect_match(text, expected_evidence[i], fixed = TRUE)
      expect_match(text, expected_target[i], fixed = TRUE)
      expect_identical(grepl(caveat, text, fixed = TRUE), i == 1L)
      expect_false(grepl("accepting the lot|rejecting the lot", text))
    }
  }
})
