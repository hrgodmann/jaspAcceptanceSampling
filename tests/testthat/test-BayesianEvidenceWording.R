test_that("Bayesian evidence wording preserves the existing exact boundary conventions", {
  values <- c(.05, .1 - 1e-12, .1, .1 + 1e-12,
              1/3 - 1e-12, 1/3, 1/3 + 1e-12, 1,
              3 - 1e-12, 3, 3 + 1e-12,
              10 - 1e-12, 10, 10 + 1e-12, 100)
  strong_above <- "Strong evidence that the proportion defective is at or above the RQL."
  moderate_above <- "Moderate evidence that the proportion defective is at or above the RQL."
  inconclusive <- "Inconclusive evidence about whether the proportion defective is below the RQL."
  moderate_below <- "Moderate evidence that the proportion defective is below the RQL."
  strong_below <- "Strong evidence that the proportion defective is below the RQL."
  expected <- c(strong_above, strong_above,
                moderate_above, moderate_above, moderate_above,
                inconclusive, inconclusive, inconclusive, inconclusive,
                moderate_below, moderate_below, moderate_below,
                strong_below, strong_below, strong_below)
  actual <- vapply(log(values), jaspAcceptanceSampling:::.bsBFEvidenceText,
                   character(1), tolerance = 0)
  expect_equal(actual, expected)
  expect_false(any(grepl("accept|reject", actual, ignore.case = TRUE)))
})

test_that("Bayesian target wording compares unrounded values and includes equality", {
  for (target in c(.0001, .1, 1/3, 1, 3, 10, 30, 10000)) {
    values <- target * c(1 - 1e-12, 1, 1 + 1e-12)
    actual <- vapply(log(values), jaspAcceptanceSampling:::.bsBFTargetText,
                     character(1), min_bf = target, tolerance = 0)
    expect_equal(grepl("has not been reached.", actual, fixed = TRUE), c(TRUE, FALSE, FALSE))
    expect_equal(grepl("has been reached.", actual, fixed = TRUE), c(FALSE, TRUE, TRUE))
    expect_true(all(grepl("Evidence labels and the target comparison use the unrounded Bayes factor.",
                          actual, fixed = TRUE)))
    expect_equal(grepl("A target at or below 1 does not require evidence favoring a proportion defective below the RQL.",
                       actual, fixed = TRUE), rep(target <= 1, 3))
    expect_false(any(grepl("accept|reject", actual, ignore.case = TRUE)))
  }
  expect_match(jaspAcceptanceSampling:::.bsBFTargetText(log_bf = log(.0001), min_bf = .0001, tolerance = 0),
               "target of 0.0001 has been reached.", fixed = TRUE)
})

test_that("Bayesian evidence strength and selected-target status remain independent", {
  # Strong favorable evidence can still fall short of the selected target.
  strong_unmet <- c(jaspAcceptanceSampling:::.bsBFEvidenceText(log(20), tolerance = 0),
                    jaspAcceptanceSampling:::.bsBFTargetText(log_bf = log(20), min_bf = 30, tolerance = 0))
  expect_match(strong_unmet[1], "Strong evidence", fixed = TRUE)
  expect_match(strong_unmet[2], "has not been reached.", fixed = TRUE)

  # A permitted low target can be reached despite evidence in the other direction.
  adverse_reached <- c(jaspAcceptanceSampling:::.bsBFEvidenceText(log(.05), tolerance = 0),
                       jaspAcceptanceSampling:::.bsBFTargetText(log_bf = log(.05), min_bf = .01, tolerance = 0))
  expect_match(adverse_reached[1], "Strong evidence that the proportion defective is at or above", fixed = TRUE)
  expect_match(adverse_reached[2], "has been reached.", fixed = TRUE)
  expect_match(adverse_reached[2], "does not require evidence favoring", fixed = TRUE)

  # BF=1 meets a target of 1 without favoring either hypothesis.
  neutral <- c(jaspAcceptanceSampling:::.bsBFEvidenceText(log(1), tolerance = 0),
               jaspAcceptanceSampling:::.bsBFTargetText(log_bf = log(1), min_bf = 1, tolerance = 0))
  expect_match(neutral[1], "Inconclusive", fixed = TRUE)
  expect_match(neutral[2], "has been reached.", fixed = TRUE)
})
