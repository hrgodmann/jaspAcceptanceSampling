# Evidence strength and meeting the selected planning target are distinct outputs.

.bsEvidenceOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$planningPlanTable <- FALSE
  options$planningPriorPlot <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$planningPriorDistribution <- "impartial"
  options$planningPriorImpartialCustomLocation <- FALSE
  options$planningAcceptableQualityLevel <- .05
  options$planningRejectableQualityLevel <- .15
  options$planningMinimumBayesFactor <- 30
  options$inferencePosterior <- TRUE
  options$inferenceSampleSize <- 30
  options$inferenceObservedDefectives <- 0
  options$inferenceLotSize <- 100
  options$inferenceDecisionTable <- TRUE
  options$inferencePosteriorPlot <- TRUE
  options$inferencePosteriorPredictivePlot <- FALSE
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
  original <- get(".bsRunInferenceAnalysis", asNamespace("jaspAcceptanceSampling"))
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
  testthat::local_mocked_bindings(.bsRunInferenceAnalysis = wrapper,
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
                                        list(list(planningMinimumBayesFactor = 1000), list(planningMinimumBayesFactor = 30)))
  expectedC <- c("1", "Not available", "1")
  expectedStatus <- c(
    "The selected Bayes-factor target of 30 has been reached.",
    "The selected Bayes-factor target of 1000 has not been reached.",
    "The selected Bayes-factor target of 30 has been reached.")
  for (i in seq_along(observed)) {
    collection <- observed[[i]]
    table <- collection$infContainer_inferenceDecisionTable
    expect_equal(as.numeric(.bsEvidenceRowForTest(table, "Bayes Factor")), 188.832)
    expect_identical(.bsEvidenceRowForTest(table, "Acceptance Number (c)"), expectedC[i])
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- .bsEvidenceTextForTest(collection[[key]])
      expect_match(text, "Strong evidence that the proportion defective is below the RQL.", fixed = TRUE)
      expect_match(text, expectedStatus[i], fixed = TRUE)
      expect_match(text, "188.83", fixed = TRUE)
      expect_match(text, "use the unrounded Bayes factor", fixed = TRUE)
      expect_false(grepl("accepting the lot|rejecting the lot", text))
    }
  }
})

test_that("Bayesian sampling - evidence descriptions follow data in both inference outputs", {
  options <- .bsEvidenceOptionsForTest()
  observed <- .bsEvidenceSequenceForTest(options,
    lapply(c(1, 2, 4, 7, 8), function(d) list(inferenceObservedDefectives = d)))
  expectedBf <- c(188.832, 31.223, 8.881, 1.478, .157, .069)
  expectedEvidence <- c(
    "Strong evidence that the proportion defective is below the RQL.",
    "Strong evidence that the proportion defective is below the RQL.",
    "Moderate evidence that the proportion defective is below the RQL.",
    "Inconclusive evidence about whether the proportion defective is below the RQL.",
    "Moderate evidence that the proportion defective is at or above the RQL.",
    "Strong evidence that the proportion defective is at or above the RQL.")
  expectedStatus <- c(
    rep("The selected Bayes-factor target of 30 has been reached.", 2L),
    rep("The selected Bayes-factor target of 30 has not been reached.", 4L))
  for (i in seq_along(observed)) {
    collection <- observed[[i]]
    table <- collection$infContainer_inferenceDecisionTable
    expect_equal(as.numeric(.bsEvidenceRowForTest(table, "Bayes Factor")), expectedBf[i])
    expect_identical(.bsEvidenceRowForTest(table, "Acceptance Number (c)"), "1")
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- .bsEvidenceTextForTest(collection[[key]])
      expect_match(text, expectedEvidence[i], fixed = TRUE)
      expect_match(text, expectedStatus[i], fixed = TRUE)
      expect_false(grepl("accepting the lot|rejecting the lot", text))
    }
  }
})

test_that("Bayesian sampling - posterior-only output reports evidence and the selected target", {
  options <- .bsEvidenceOptionsForTest()
  options$inferenceDecisionTable <- FALSE
  options$planningMinimumBayesFactor <- 1000
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
  lowTarget <- .bsEvidenceOptionsForTest()
  lowTarget$inferenceObservedDefectives <- 7
  lowTarget$planningMinimumBayesFactor <- .1
  equalTarget <- .bsEvidenceOptionsForTest()
  equalTarget$planningPriorDistribution <- "uniform"
  equalTarget$planningRejectableQualityLevel <- .5
  equalTarget$inferenceSampleSize <- 1
  equalTarget$planningMinimumBayesFactor <- 3
  cases <- list(lowTarget, equalTarget)
  expectedBf <- c(.157, 3)
  expectedC <- c("7", "0")
  expectedEvidence <- c(
    "Moderate evidence that the proportion defective is at or above the RQL.",
    "Moderate evidence that the proportion defective is below the RQL.")
  expectedTarget <- c(
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
    expect_equal(as.numeric(.bsEvidenceRowForTest(table, "Bayes Factor")), expectedBf[i])
    expect_identical(.bsEvidenceRowForTest(table, "Acceptance Number (c)"), expectedC[i])
    for (key in c("infContainer_decisionSummaryText", "infContainer_bfText")) {
      text <- .bsEvidenceTextForTest(collection[[key]])
      expect_match(text, expectedEvidence[i], fixed = TRUE)
      expect_match(text, expectedTarget[i], fixed = TRUE)
      expect_identical(grepl(caveat, text, fixed = TRUE), i == 1L)
      expect_false(grepl("accepting the lot|rejecting the lot", text))
    }
  }
})
