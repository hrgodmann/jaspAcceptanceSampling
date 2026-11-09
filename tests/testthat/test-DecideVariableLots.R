# Tests for Decide variable lots

##----------------------------------------------------------------
##                      1. No dataset, LSL                      --
##----------------------------------------------------------------
test_that("Decide lots - no dataset, lsl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStatistics <- TRUE
  options$sampleSize <- 24
  options$sampleMean <- 1.5
  options$sampleSd <- 1
  options$kValue <- 1.309
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- FALSE
  
  # 1.1 Historical SD unknown
  options$historicalSdKnown <- FALSE
  options$lowerSpecificationLimitValue <- 0.1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(24, 1.5, 1, 0.1, 1.4, 1.309))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 1.2 Historical SD known
  options$historicalSdKnown <- TRUE
  options$historicalSd <- 2.4
  options$lowerSpecificationLimitValue <- 0.5
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(24, 1.5, 1, 2.4, 0.5, 0.4167, 1.309))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
})

##----------------------------------------------------------------
##                      2. No dataset, USL                      --
##----------------------------------------------------------------
test_that("Decide lots - no dataset, usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStatistics <- TRUE
  options$sampleSize <- 125
  options$sampleMean <- 172.3
  options$sampleSd <- 23.7
  options$kValue <- 2.5
  options$lowerSpecificationLimit <- FALSE
  options$upperSpecificationLimit <- TRUE
  
  # 2.1 Historical SD unknown
  options$historicalSdKnown <- FALSE

  # 2.1.1 Reject
  options$upperSpecificationLimitValue <- 230
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 230, 2.435, 2.5))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 2.1.2 Accept
  options$upperSpecificationLimitValue <- 232
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 232, 2.519, 2.5))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 2.2 Historical SD known
  options$historicalSdKnown <- TRUE
  options$historicalSd <- 19.5

  # 2.2.1 Reject
  options$upperSpecificationLimitValue <- 221
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(125, 172.3, 23.7, 19.5, 221, 2.497, 2.5))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 2.2.2 Accept
  options$upperSpecificationLimitValue <- 222
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 19.5, 222, 2.549, 2.5))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

##---------------------------------------------------------------
##               3. No dataset, both LSL and USL               --
##---------------------------------------------------------------
test_that("Decide lots - no dataset, lsl & usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStatistics <- TRUE
  options$sampleSize <- 785
  options$sampleMean <- 35.2
  options$sampleSd <- 8.92
  options$kValue <- 2
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 60
  
  # 3.1 Historical SD unknown
  options$historicalSdKnown <- FALSE
  
  # 3.1.1 Reject
  options$lowerSpecificationLimitValue <- 17
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 17, 60, 2.04, 2.78, 2))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.1.2 Accept
  options$lowerSpecificationLimitValue <- 16
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 16, 60, 2.152, 2.78, 2))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 3.2 Historical SD known
  options$historicalSdKnown <- TRUE
  options$historicalSd <- 10.12
  options$acceptableQualityLevel <- 0.05
  options$rejectableQualityLevel <- 0.15
  options$lowerSpecificationLimitValue <- 10
  
  # 3.2.1 Reject
  options$upperSpecificationLimitValue <- 55
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(785, 35.2, 8.92, 10.12, 10, 55, 2.49, 1.956, 2))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.2.2 Accept
  options$upperSpecificationLimitValue <- 56
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 10.12, 10, 56, 2.49, 2.055, 2))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

##---------------------------------------------------------------
##                       4. Dataset, LSL                       --
##---------------------------------------------------------------
test_that("Decide lots - dataset, lsl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$measurement <- "contNormal"
  options$sampleStatistics <- FALSE
  options$kValue <- 1.309
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- FALSE
  
  # 4.1 Historical SD unknown
  options$historicalSdKnown <- FALSE
  options$lowerSpecificationLimitValue <- 0.1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, -0.1887, 1.058, 0.1, -0.2728, 1.309))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 4.2 Historical SD known
  options$historicalSdKnown <- TRUE
  options$historicalSd <- 0.1
  options$lowerSpecificationLimitValue <- 0
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(100, -0.1887, 1.058, 0.1, 0, -1.888, 1.309))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Reject</b> lot."))
})

##---------------------------------------------------------------
##                       5. Dataset, USL                       --
##---------------------------------------------------------------
test_that("Decide lots - no dataset, usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$measurement <- "contOutlier"
  options$sampleStatistics <- FALSE
  options$kValue <- 2.5
  options$lowerSpecificationLimit <- FALSE
  options$upperSpecificationLimit <- TRUE
  
  # 5.1 Historical SD unknown
  options$historicalSdKnown <- FALSE

  # 5.1.1 Reject
  options$upperSpecificationLimitValue <- 7
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 0.0355, 2.903, 7, 2.399, 2.5))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 5.1.2 Accept
  options$upperSpecificationLimitValue <- 8
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 0.0355, 2.903, 8, 2.744, 2.5))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 5.2 Historical SD known
  options$historicalSdKnown <- TRUE
  options$historicalSd <- 2.5
  options$acceptableQualityLevel <- 0.05
  options$rejectableQualityLevel <- 0.15

  # 5.2.1 Reject
  options$upperSpecificationLimitValue <- 6
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(100, 0.0355, 2.903, 2.5, 6, 2.386, 2.5))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 5.2.2 Accept
  options$upperSpecificationLimitValue <- 7
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 0.0355, 2.903, 2.5, 7, 2.786, 2.5))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

##----------------------------------------------------------------
##                 6. Dataset, both LSL and USL                 --
##----------------------------------------------------------------
test_that("Decide lots - dataset, lsl & usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$measurement <- "contGamma"
  options$sampleStatistics <- FALSE
  options$kValue <- 1.309
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0
  
  # 6.1 Historical SD unknown
  options$historicalSdKnown <- FALSE
  
  # 6.1.1 Reject
  options$upperSpecificationLimitValue <- 6
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 2.033, 1.532, 0, 6, 1.327, 2.589, 1.309))
  lotDecisionUnknownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 6.1.2 Accept
  options$upperSpecificationLimitValue <- 7
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 2.033, 1.532, 0, 7, 1.327, 3.241, 1.309))
  lotDecisionUnknownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 6.2 Historical SD known
  options$historicalSdKnown <- TRUE
  options$historicalSd <- 1.42
  options$acceptableQualityLevel <- 0.05
  options$rejectableQualityLevel <- 0.15
  options$upperSpecificationLimitValue <- 6
  
  # 6.2.1 Reject
  options$lowerSpecificationLimitValue <- 1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableKnown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(100, 2.033, 1.532, 1.42, 1, 6, 0.7274, 2.794, 1.309))
  lotDecisionKnownReject <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 6.2.2 Accept
  options$lowerSpecificationLimitValue <- 0
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(100, 2.033, 1.532, 1.42, 0, 6, 1.432, 2.794, 1.309))
  lotDecisionKnownAccept <- results[["results"]][["lotContainer"]][["collection"]][["lotContainer_decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})
# Verify the actual declaration against parsed QML options and exercise the real
# result engine. Native variable handling also invalidates assignments, so the
# transition alone would not detect an obsolete explicit dependency name.
test_that("Decide lots - measurement assignment invalidates the lot container", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$measurement <- "contGamma"
  options$sampleStatistics <- FALSE
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- FALSE
  options$lowerSpecificationLimitValue <- 0
  options$historicalSdKnown <- FALSE

  original <- getFromNamespace("DecideVariableLots", "jaspAcceptanceSampling")
  createContainer <- get("createJaspContainer", asNamespace("jaspAcceptanceSampling"))
  declaredDependencies <- character()
  parsedOptionNames <- character()
  testEnvironment <- environment()
  factory <- function(...) {
    container <- createContainer(...)
    dependOn <- container$dependOn
    # Restore the exact original method and binding lock when this test ends.
    testthat:::local_bindings_rebind(
      dependOn = function(options = NULL, ...) {
        declaredDependencies <<- union(declaredDependencies, options)
        dependOn(options = options, ...)
      }, .env = container, .frame = testEnvironment)
    container
  }
  invalidated <- NULL
  wrapper <- function(jaspResults, dataset = NULL, options, ...) {
    parsedOptionNames <<- names(options)
    original(jaspResults, dataset, options, ...)
    expect_false(is.null(jaspResults[["lotContainer"]]))
    private <- jaspResults$.__enclos_env__$private
    private$complete()

    cleared <- options
    if (is.list(cleared$measurement) && "value" %in% names(cleared$measurement))
      cleared$measurement$value <- character()
    else
      cleared$measurement <- character()
    private$changeOptions(as.character(jsonlite::toJSON(cleared, digits = NA)))
    jaspBase:::emptyRecomputed()
    invalidated <<- is.null(jaspResults[["lotContainer"]])

    # Leave the runner with the original valid analysis after the transition probe.
    private$changeOptions(as.character(jsonlite::toJSON(options, digits = NA)))
    jaspBase:::emptyRecomputed()
    original(jaspResults, dataset, options, ...)
  }
  result <- testthat::with_mocked_bindings(
    jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, view = FALSE),
    DecideVariableLots = wrapper, createJaspContainer = factory,
    .package = "jaspAcceptanceSampling")
  expect_identical(result$status, "complete")
  expect_identical(list(
    measurementDeclared = "measurement" %in% declaredDependencies,
    obsoleteVarsDeclared = "vars" %in% declaredDependencies,
    unknownOptions = setdiff(declaredDependencies, parsedOptionNames)),
    list(measurementDeclared = TRUE, obsoleteVarsDeclared = FALSE,
         unknownOptions = character()))
  expect_identical(invalidated, TRUE)
})
