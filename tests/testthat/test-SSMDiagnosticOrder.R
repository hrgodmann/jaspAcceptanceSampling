test_that("computational diagnostics follow all plots and precede a reused MCMC table", {
  ns <- asNamespace("jaspAcceptanceSampling")
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(defectCount = "count", sampleSize = "n", lotSize = "s",
                      lotNumber = "lot", covariates = c("temperature", "humidity"))
  options[names(assignments)] <- lapply(assignments, function(columns) {
    if (!length(columns)) return(columns)
    list(value = columns, types = as.list(rep("scale", length(columns))))
  })
  for (name in c("plotsStateTimeSeries", "plotsPosteriorPredictive", "plotsLatestDefectRate",
                 "plotsPredictorCoefficients", "mcmcDiagnosticsPrint", "mcmcDiagnosticsOverviewTable",
                 "mcmcDiagnosticsOverviewTheta", "mcmcDiagnosticsOverviewSigma", "mcmcDiagnosticsOverviewPrediction", "mcmcDiagnosticsOverviewBeta"))
    options[[name]] <- TRUE
  dataset <- data.frame(count = c(1, 0, 2, 1), n = 20, s = 100, lot = 1:4,
                        temperature = c(-2, -1, 0, 1), humidity = c(1, 0, -1, 2))
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(dataset, csv, row.names = FALSE)

  # Only model fitting is substituted. All output objects and sorting are real.
  generations <- 0L
  generationsByUpdate <- integer()
  fit <- function(jaspResults, dataset, options) {
    if (is.null(jaspResults[["fit"]])) {
      state <- jaspBase::createJaspState()
      state$dependOn(get(".asSSM_fitDependencies", ns)())
      jaspResults[["fit"]] <- state
    }
    if (!is.null(jaspResults[["fit"]]$object)) return()
    generations <<- generations + 1L
    z <- stats::qnorm(stats::ppoints(1000))
    design <- get(".asSSM_predictorDesign", ns)(dataset, options)
    pars <- c(paste0("theta[", seq_len(nrow(dataset)), "]"), "sigma_eta", "y_pred",
              "beta_predictor[1]", "beta_predictor[2]")
    sm <- matrix(rep(c(.1, .02, 1000, 1), each = length(pars)),
                 nrow = length(pars), dimnames = list(pars, c("mean", "sd", "n_eff", "Rhat")))
    diagnostics <- list(chains = 2L, expected_chains = 2L, draws_per_chain = 500L,
      thin = 1L, max_treedepth = 15L,
      parameters = data.frame(parameter = c("sigma_eta", "beta_predictor[1]", "beta_predictor[2]"),
        rhat = 1, ess_bulk = 1000, ess_tail = 1000, constant_chain = FALSE, nonfinite_draws = FALSE),
      sampler = data.frame(chain = 1:2, draws = 500L, divergences = c(4L, 0L),
                           treedepth_hits = 0L, bfmi = 1))
    jaspResults[["fit"]]$object <- list(
      samples = list(theta = matrix(rep(stats::plogis(-2 + .1 * z), nrow(dataset)), ncol = nrow(dataset)),
                     y_pred = rep(5:14, 100),
                     beta_predictor = cbind(.5 + .1 * z, -.5 + .1 * z)),
      predictor_design = design, summary = sm, diagnostics = diagnostics)
  }
  original <- get("bayesianSSMforASInternal", ns)
  observed <- list()
  reused <- FALSE
  reusedPosition <- NULL
  wrapper <- function(jaspResults, dataset = NULL, options) {
    private <- jaspResults$.__enclos_env__$private
    capture <- function() {
      private$complete()
      jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
    }
    original(jaspResults, dataset, options)
    observed$full <<- capture()

    # Previously cached tables used position 5: refreshing must update that object.
    oldTable <- jaspResults[["mcmcSummary"]]
    oldTable$position <- 5
    get(".asSSM_createMcmcSummaryTable", ns)(jaspResults, dataset, options)
    reused <<- identical(oldTable, jaspResults[["mcmcSummary"]])
    reusedPosition <<- oldTable$position
    observed$reused <<- capture()

    for (name in c("warnings", "fullAgain", "healthyOff", "priorChange")) {
      # Display changes reuse the fit; changing its prior must invalidate it.
      if (identical(name, "priorChange")) options$modelInnovationPriorScale <- 2
      options$mcmcDiagnosticsPrint <- identical(name, "fullAgain")
      if (identical(name, "healthyOff")) {
        stored <- jaspResults[["fit"]]$object
        stored$diagnostics$sampler$divergences[] <- 0L
        jaspResults[["fit"]]$object <- stored
      }
      # Preserve scalar, array and integer/double formats for unchanged options.
      nativeOptions <- options
      typeNames <- names(nativeOptions)[endsWith(names(nativeOptions), ".types")]
      nativeOptions[typeNames] <- lapply(nativeOptions[typeNames], as.list)
      private$changeOptions(as.character(jsonlite::toJSON(
        nativeOptions, auto_unbox = TRUE, digits = NA, null = "null", always_decimal = TRUE)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, dataset, options)
      observed[[name]] <<- capture()
      generationsByUpdate[[name]] <<- generations
    }
  }
  result <- testthat::with_mocked_bindings(
    jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE),
    .asSSM_fitModel = fit, bayesianSSMforASInternal = wrapper,
    .asSSM_getStanModel = function(...) stop("The output fixture must not load Stan."),
    .package = "jaspAcceptanceSampling")
  expect_identical(result$status, "complete")
  expect_true(reused)
  expect_equal(reusedPosition, 6)
  expect_equal(generations, 2L)
  expect_identical(generationsByUpdate,
                   c(warnings = 1L, fullAgain = 1L, healthyOff = 1L, priorChange = 2L))
  keys <- function(x) vapply(x$results$.meta, function(item) item$name, character(1))
  allOutputs <- c("summaryTable", "predictorSummary", "ssm_statePlot", "predPlot",
                  "postDist", "betaPlot", "samplerDiagnostics", "mcmcSummary")
  for (name in c("full", "reused", "fullAgain")) {
    expect_identical(keys(observed[[name]]), allOutputs)
    expect_identical(observed[[name]]$results$samplerDiagnostics$title, "Computational Diagnostics")
    for (key in allOutputs)
      expect_null(observed[[name]]$results[[key]][["error"]])
  }
  expect_identical(keys(observed$warnings), c("samplerDiagnostics", allOutputs[allOutputs != "samplerDiagnostics"]))
  expect_identical(observed$warnings$results$samplerDiagnostics$title, "Sampling Warnings")
  expect_match(observed$warnings$results$samplerDiagnostics$text, "4 divergent transitions", fixed = TRUE)
  expect_identical(keys(observed$healthyOff), allOutputs[allOutputs != "samplerDiagnostics"])
  expect_null(observed$healthyOff$results$samplerDiagnostics)
})
