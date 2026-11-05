# Prior inspection must not depend on finding an acceptable sampling plan.

.bsPriorPlanningOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$planningPriorPlot <- TRUE
  options$planningPlansTable <- FALSE
  options$planningThreeHypothesisBayesFactorTable <- FALSE
  options$inferenceUseData <- FALSE
  options$planningMaximumSampleSize <- 1
  options$planningMinimumBayesFactor <- 30
  options
}

.bsPlanningCollectionForTest <- function(results) {
  results$results$planContainer$collection
}

test_that("Bayesian sampling - prior-only output does not search for a sampling plan", {
  options <- .bsPriorPlanningOptionsForTest()
  testthat::local_mocked_bindings(
    .bsGeneratePlans = function(...) stop("Prior inspection must not search for a plan."),
    .package = "jaspAcceptanceSampling")
  results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                   options = options, view = FALSE)
  collection <- .bsPlanningCollectionForTest(results)
  prior <- collection$planContainer_distPlotPrior
  expect_identical(results$status, "complete")
  expect_false(is.null(prior))
  expect_null(prior[["error"]])
  expect_identical(prior$status, "complete")
  expect_true(prior$data %in% names(results$state$figures))
  expect_null(collection$planContainer_planTable)
  expect_null(collection$planContainer_threeBFTable)
})

test_that("Bayesian sampling - infeasible requested tables do not suppress a valid prior", {
  for (requested in list(c(TRUE, FALSE), c(FALSE, TRUE), c(TRUE, TRUE))) {
    options <- .bsPriorPlanningOptionsForTest()
    options$planningPlansTable <- requested[1L]
    options$planningThreeHypothesisBayesFactorTable <- requested[2L]
    results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                     options = options, view = FALSE)
    collection <- .bsPlanningCollectionForTest(results)
    prior <- collection$planContainer_distPlotPrior
    expect_identical(results$status, "complete")
    expect_false(is.null(prior))
    expect_null(prior[["error"]])
  expect_identical(prior$status, "complete")
    expect_true(prior$data %in% names(results$state$figures))
    keys <- c("planContainer_planTable", "planContainer_threeBFTable")
    for (i in seq_along(keys)) {
      output <- collection[[keys[i]]]
      if (requested[i]) {
        expect_false(is.null(output))
        expect_match(paste(output$error$errorMessage, collapse = ""),
                     "No sampling plan found", fixed = TRUE)
        expect_identical(output$status, "error")
      } else {
        expect_null(output)
      }
    }
  }
})

test_that("Bayesian sampling - invalid planning controls are rejected by the QML input bounds", {
  for (option in c("planningMaximumSampleSize", "planningMinimumBayesFactor")) {
    options <- .bsPriorPlanningOptionsForTest()
    options$planningPlansTable <- TRUE
    options$planningThreeHypothesisBayesFactorTable <- TRUE
    options[[option]] <- 0
    expect_error(
      jaspTools::runAnalysis("BayesianSampling", dataset = NULL, options = options, view = FALSE),
      paste0(option, ": The value must be"), fixed = TRUE)
  }
})

test_that("Bayesian sampling - prior survives plan infeasibility, invalid BF targets, and recovery", {
  options <- .bsPriorPlanningOptionsForTest()
  options$planningPlansTable <- TRUE
  options$planningThreeHypothesisBayesFactorTable <- TRUE
  options$planningMaximumSampleSize <- 40
  original <- get(".bsPlanningAnalysis", asNamespace("jaspAcceptanceSampling"))
  visited <- 0L
  observed <- list()
  wrapper <- function(jaspResults, options, position) {
    original(jaspResults, options, position)
    private <- jaspResults$.__enclos_env__$private
    private$complete()
    readCollection <- function() {
      result <- jsonlite::fromJSON(
        jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      .bsPlanningCollectionForTest(result)
    }
    observed[[1L]] <<- readCollection()
    changes <- list(list(planningMaximumSampleSize = 1), list(planningMaximumSampleSize = 40),
                    list(planningMinimumBayesFactor = 0), list(planningMinimumBayesFactor = 30))
    for (change in changes) {
      options[names(change)] <- change
      # Exercise the real engine's dependency invalidation on the same result.
      private$changeOptions(as.character(jsonlite::toJSON(options)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, options, position)
      private$complete()
      visited <<- visited + 1L
      observed[[visited + 1L]] <<- readCollection()
    }
  }
  testthat::local_mocked_bindings(.bsPlanningAnalysis = wrapper,
                                 .package = "jaspAcceptanceSampling")
  results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                   options = options, view = FALSE)
  expect_identical(results$status, "complete")
  expect_equal(visited, 4L)
  expect_length(observed, 5L)
  for (collection in observed) {
    prior <- collection$planContainer_distPlotPrior
    expect_false(is.null(prior))
    expect_null(prior[["error"]])
    expect_identical(prior$status, "complete")
    expect_type(prior$data, "character")
    expect_length(prior$data, 1L)
  }
  for (key in c("planContainer_planTable", "planContainer_threeBFTable")) {
    expect_null(observed[[1L]][[key]]$error)
    expect_gt(length(observed[[1L]][[key]]$data), 0L)
    expect_match(paste(observed[[2L]][[key]]$error$errorMessage, collapse = ""),
                 "No sampling plan found", fixed = TRUE)
    expect_null(observed[[3L]][[key]]$error)
    expect_equal(observed[[3L]][[key]]$data, observed[[1L]][[key]]$data)
    expect_identical(observed[[4L]][[key]]$status, "error")
    expect_match(paste(observed[[4L]][[key]]$error$errorMessage, collapse = ""),
                 "minimum Bayes factor", fixed = TRUE)
    expect_identical(observed[[5L]][[key]]$status, "complete")
    expect_null(observed[[5L]][[key]]$error)
    expect_equal(observed[[5L]][[key]]$data, observed[[1L]][[key]]$data)
  }
})
