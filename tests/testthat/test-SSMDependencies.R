.ssmExpectedFitDependencies <- function() {
  c("defectCount", "lotSize", "sampleSize", "lotNumber",
    "covariates",
    "modelInitialBaselineAlpha", "modelInitialBaselineBeta",
    "modelInnovationPriorScale", "modelPredictorCauchyScale",
    "advancedMcmcBurnin", "advancedMcmcChains",
    "advancedMcmcSamples", "advancedMcmcThinning",
    "advancedMcmcSeed", "advancedMcmcAdaptDelta",
    "advancedMcmcMaxTreeDepth")
}

.ssmCaptureDependencies <- function(functionName) {
  # Capture the real builder's declaration before creating Rcpp outputs or fitting.
  capture <- function(keys) {
    stop(structure(list(message = "Dependencies captured", call = NULL, keys = keys),
                   class = c("ssm_dependencies_captured", "condition")))
  }
  factory <- function(...) list(dependOn = capture)
  fun <- getFromNamespace(functionName, "jaspAcceptanceSampling")
  environment(fun) <- list2env(list(
    createJaspState = factory, createJaspTable = factory, createJaspPlot = factory,
    createJaspHtml = function(..., dependencies) capture(dependencies)
  ), parent = environment(fun))
  options <- list(covariates = "x", mcmcDiagnosticOverviewTable = TRUE,
                  mcmcDiagnosticReport = TRUE,
                  plotStateTimeSeries = TRUE, plotPosteriorPredictive = TRUE,
                  plotLatestDefectRate = TRUE, plotPredictorCoefficient = TRUE)
  results <- if (functionName == ".asSSM_reportDiagnostics")
    list(fit = list(object = list())) else list()
  arguments <- list(jaspResults = results, options = options)
  if ("dataset" %in% names(formals(fun))) arguments["dataset"] <- list(NULL)
  captured <- tryCatch(do.call(fun, arguments), ssm_dependencies_captured = base::identity)
  if (!inherits(captured, "ssm_dependencies_captured"))
    stop("The builder did not declare its dependencies: ", functionName)
  captured$keys
}

test_that("the fit depends on every data, prior and sampler setting", {
  dependencies <- getFromNamespace(".asSSM_getFitDependencies", "jaspAcceptanceSampling")()
  expected <- .ssmExpectedFitDependencies()
  expect_setequal(dependencies, expected)
  expect_length(dependencies, length(expected))
  expect_setequal(.ssmCaptureDependencies(".asSSM_fitModel"), expected)
})

test_that("every fitted output includes all model dependencies and its display controls", {
  extras <- list(
    .asSSM_createSummaryTable = c("settingsAcceptableQualityLevel", "settingsRejectableQualityLevel",
      "settingsAcceptanceThreshold", "settingsRejectionThreshold"),
    .asSSM_createPredictorSummaryTable = character(),
    .asSSM_plotState = "plotStateTimeSeries",
    .asSSM_plotPrediction = c("plotPosteriorPredictive", "settingsAcceptableQualityLevel", "settingsRejectableQualityLevel"),
    .asSSM_plotPosteriorDistribution = "plotLatestDefectRate",
    .asSSM_plotBetaCoefficient = "plotPredictorCoefficient",
    .asSSM_createMcmcSummaryTable = c("mcmcDiagnosticOverviewTable", "mcmcDiagnosticOverviewTheta",
      "mcmcDiagnosticOverviewSigma", "mcmcDiagnosticOverviewPrediction", "mcmcDiagnosticOverviewBeta"),
    .asSSM_reportDiagnostics = "mcmcDiagnosticReport"
  )
  for (name in names(extras)) {
    declared <- .ssmCaptureDependencies(name)
    expected <- c(.ssmExpectedFitDependencies(), extras[[name]])
    expect_equal(sort(declared), sort(expected), info = name)
  }
})

test_that("changing decisions or output visibility does not invalidate the fit", {
  displayControls <- c("settingsAcceptableQualityLevel", "settingsRejectableQualityLevel",
    "settingsAcceptanceThreshold", "settingsRejectionThreshold",
    "plotStateTimeSeries", "plotPosteriorPredictive", "plotLatestDefectRate", "plotPredictorCoefficient",
    "mcmcDiagnosticOverviewTable", "mcmcDiagnosticOverviewTheta", "mcmcDiagnosticOverviewSigma", "mcmcDiagnosticOverviewPrediction", "mcmcDiagnosticOverviewBeta",
    "mcmcDiagnosticReport")
  declared <- .ssmCaptureDependencies(".asSSM_fitModel")
  expect_length(intersect(declared, displayControls), 0L)
})

test_that("the real fit cache signature retains every renamed sampler setting", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")
  assignments <- list(defectCount = "defects", sampleSize = "tested",
                      lotSize = "total", lotNumber = "lot")
  options[names(assignments)] <- lapply(assignments, function(column) {
    list(value = column, types = list("scale"))
  })
  options$covariates <- character()
  expected <- list(
    advancedMcmcBurnin = 100, advancedMcmcChains = 2,
    advancedMcmcSamples = 300, advancedMcmcThinning = 3,
    advancedMcmcSeed = 456, advancedMcmcAdaptDelta = .91,
    advancedMcmcMaxTreeDepth = 11
  )
  expect_true(all(names(expected) %in% names(options)))
  options[names(expected)] <- expected

  captured <- NULL
  calls <- 0L
  loader <- function(name) {
    calls <<- calls + 1L
    # Inspect the actual signature built before model loading; no Stan fit is needed.
    captured <<- get("fitInputs", envir = parent.frame(), inherits = TRUE)
    stop("Deliberate model-loader stop after capturing sampler settings.", call. = FALSE)
  }
  testthat::local_mocked_bindings(.asSSM_getStanModel = loader,
                                 .package = "jaspAcceptanceSampling")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(defects = c(1, 0, 2), tested = c(20, 20, 20),
                             total = c(100, 100, 100), lot = c(10, 20, 30)),
                   csv, row.names = FALSE)
  result <- jaspTools::runAnalysis("BayesianSSMforAS", csv, options, view = FALSE)

  expect_identical(calls, 1L)
  expect_type(captured, "list")
  expect_length(captured$settings, 7L)
  expect_setequal(names(captured$settings), names(expected))
  expect_equal(captured$settings, expected)
  expect_identical(result$status, "complete")
  final <- as.character(jsonlite::toJSON(result$results$placeholder, auto_unbox = TRUE))
  expect_match(final, "Deliberate model-loader stop", fixed = TRUE)
})
