context("Example: BaySSM")

# Recalculate the saved example using its data and applicable settings.

test_that("BayesianSSMforAS runs without error", {
  oldModuleDirs <- jaspTools:::getPkgOption("module.dirs")
  oldReinstallModules <- jaspTools:::getPkgOption("reinstall.modules")
  on.exit({
    jaspTools::setPkgOption("module.dirs", oldModuleDirs)
    jaspTools::setPkgOption("reinstall.modules", oldReinstallModules)
  }, add = TRUE)

  jaspTools::setPkgOption("module.dirs", normalizePath(testthat::test_path("..", "..")))
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "BaySSM.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  opts$mcmcDiagnosticReport <- TRUE
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Let the public runner encode the data and options.
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", dataset, opts, view = FALSE)

  errorInfo <- paste(c(results$errorMessage, results$results$errorMessage,
                       unlist(results$results$placeholder$error, use.names = FALSE)),
                     collapse = "\n")
  expect_equal(results$status, "complete", info = errorInfo)
  expect_false("placeholder" %in% names(results$results), info = errorInfo)
  expect_true(all(c("summaryTable", "samplerDiagnostics") %in% names(results$results)))
  expect_false("predPlotNote" %in% names(results$results))

  lastLot <- dataset[which.max(dataset[[opts$lotNumber]]), , drop = FALSE]
  summary <- results$results$summaryTable$data[[1]]
  expect_equal(summary$t, format(lastLot[[opts$lotNumber]], trim = TRUE, scientific = FALSE, digits = 15))
  expect_equal(summary$y, lastLot[[opts$defectCount]])
  expect_true(is.finite(summary$massAQL) && summary$massAQL >= 0 && summary$massAQL <= 1)
  expect_true(is.finite(summary$massRQL) && summary$massRQL >= 0 && summary$massRQL <= 1)
  expect_true(summary$decision %in% c("Consider to Accept", "Consider to Reject", "Consider to Continue"))
})
