context("Example: BaySSM")

# Recalculate the saved example using its data and applicable settings.

test_that("BayesianSSMforAS runs without error", {
  old_module_dirs <- jaspTools:::getPkgOption("module.dirs")
  old_reinstall_modules <- jaspTools:::getPkgOption("reinstall.modules")
  on.exit({
    jaspTools::setPkgOption("module.dirs", old_module_dirs)
    jaspTools::setPkgOption("reinstall.modules", old_reinstall_modules)
  }, add = TRUE)

  jaspTools::setPkgOption("module.dirs", normalizePath(testthat::test_path("..", "..")))
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "BaySSM.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  opts$ssm_printDiagnostics <- TRUE
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Let the public runner encode the data and options.
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", dataset, opts, view = FALSE)

  expect_equal(results$status, "complete")
  expect_false("placeholder" %in% names(results$results))
  expect_true(all(c("summaryTable", "samplerDiagnostics") %in% names(results$results)))
  expect_false("predPlotNote" %in% names(results$results))

  lastLot <- dataset[which.max(dataset[[opts$ssm_time]]), , drop = FALSE]
  summary <- results$results$summaryTable$data[[1]]
  expect_equal(summary$t, format(lastLot[[opts$ssm_time]], trim = TRUE, scientific = FALSE, digits = 15))
  expect_equal(summary$y, lastLot[[opts$ssm_count]])
  expect_true(is.finite(summary$massAQL) && summary$massAQL >= 0 && summary$massAQL <= 1)
  expect_true(is.finite(summary$massRQL) && summary$massRQL >= 0 && summary$massRQL <= 1)
  expect_true(summary$decision %in% c("Consider to Accept", "Consider to Reject", "Consider to Continue"))
})
