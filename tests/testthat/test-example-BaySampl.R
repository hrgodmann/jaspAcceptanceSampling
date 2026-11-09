context("Example: BaySampl")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("BayesianSampling runs without error", {
  oldModuleDirs <- jaspTools:::getPkgOption("module.dirs")
  oldReinstallModules <- jaspTools:::getPkgOption("reinstall.modules")
  on.exit({
    jaspTools::setPkgOption("module.dirs", oldModuleDirs)
    jaspTools::setPkgOption("reinstall.modules", oldReinstallModules)
  }, add = TRUE)

  jaspTools::setPkgOption("module.dirs", normalizePath(testthat::test_path("..", "..")))
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "BaySampl.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Let the public runner encode the data and options.
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSampling", dataset, opts, view = FALSE)

  # Check analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})
