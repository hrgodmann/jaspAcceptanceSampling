# Prior inspection must not depend on finding an acceptable sampling plan.

.bsPriorPlanningOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$priorPlotplan <- TRUE
  options$showPlansplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$inferPosteriorinfer <- FALSE
  options$max_nplan <- 1
  options$min_bfplan <- 30
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
    options$showPlansplan <- requested[1L]
    options$showThreeBFplan <- requested[2L]
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

test_that("Bayesian sampling - invalid planning controls leave the selected prior visible", {
  cases <- list(
    list(option = "max_nplan", value = 0, message = "maximum sample size"),
    list(option = "min_bfplan", value = 0, message = "minimum Bayes factor"))
  for (case in cases) {
    options <- .bsPriorPlanningOptionsForTest()
    options$showPlansplan <- TRUE
    options$showThreeBFplan <- TRUE
    options[[case$option]] <- case$value
    results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                     options = options, view = FALSE)
    collection <- .bsPlanningCollectionForTest(results)
    prior <- collection$planContainer_distPlotPrior
    expect_identical(results$status, "complete")
    expect_false(is.null(prior))
    expect_null(prior[["error"]])
    expect_identical(prior$status, "complete")
    expect_true(prior$data %in% names(results$state$figures))
    for (key in c("planContainer_planTable", "planContainer_threeBFTable")) {
      expect_match(paste(collection[[key]]$error$errorMessage, collapse = ""),
                   case$message, fixed = TRUE)
      expect_identical(collection[[key]]$status, "error")
    }
  }
})

test_that("Bayesian sampling - prior survives plan infeasibility and table recovery", {
  options <- .bsPriorPlanningOptionsForTest()
  options$showPlansplan <- TRUE
  options$showThreeBFplan <- TRUE
  options$max_nplan <- 40
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
    for (max_n in c(1, 40)) {
      options$max_nplan <- max_n
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
  expect_equal(visited, 2L)
  expect_length(observed, 3L)
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
  }
})
