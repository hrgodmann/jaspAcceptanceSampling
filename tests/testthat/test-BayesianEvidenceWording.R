test_that("Bayesian evidence wording preserves the existing exact boundary conventions", {
  values <- c(.05, .1 - 1e-12, .1, .1 + 1e-12,
              1/3 - 1e-12, 1/3, 1/3 + 1e-12, 1,
              3 - 1e-12, 3, 3 + 1e-12,
              10 - 1e-12, 10, 10 + 1e-12, 100)
  strongAbove <- "Strong evidence that the proportion defective is at or above the RQL."
  moderateAbove <- "Moderate evidence that the proportion defective is at or above the RQL."
  inconclusive <- "Inconclusive evidence about whether the proportion defective is below the RQL."
  moderateBelow <- "Moderate evidence that the proportion defective is below the RQL."
  strongBelow <- "Strong evidence that the proportion defective is below the RQL."
  expected <- c(strongAbove, strongAbove,
                moderateAbove, moderateAbove, moderateAbove,
                inconclusive, inconclusive, inconclusive, inconclusive,
                moderateBelow, moderateBelow, moderateBelow,
                strongBelow, strongBelow, strongBelow)
  actual <- vapply(log(values), jaspAcceptanceSampling:::.bsCreateBayesFactorEvidenceText,
                   character(1), tolerance = 0)
  expect_equal(actual, expected)
  expect_false(any(grepl("accept|reject", actual, ignore.case = TRUE)))
})

test_that("Bayesian target wording compares unrounded values and includes equality", {
  for (target in c(.0001, .1, 1/3, 1, 3, 10, 30, 10000)) {
    values <- target * c(1 - 1e-12, 1, 1 + 1e-12)
    actual <- vapply(log(values), jaspAcceptanceSampling:::.bsCreateBayesFactorTargetText,
                     character(1), minimumBayesFactor = target, tolerance = 0)
    expect_equal(grepl("has not been reached.", actual, fixed = TRUE), c(TRUE, FALSE, FALSE))
    expect_equal(grepl("has been reached.", actual, fixed = TRUE), c(FALSE, TRUE, TRUE))
    expect_true(all(grepl("Evidence labels and the target comparison use the unrounded Bayes factor.",
                          actual, fixed = TRUE)))
    expect_equal(grepl("A target at or below 1 does not require evidence favoring a proportion defective below the RQL.",
                       actual, fixed = TRUE), rep(target <= 1, 3))
    expect_false(any(grepl("accept|reject", actual, ignore.case = TRUE)))
  }
  expect_match(jaspAcceptanceSampling:::.bsCreateBayesFactorTargetText(logBayesFactor = log(.0001), minimumBayesFactor = .0001, tolerance = 0),
               "target of 0.0001 has been reached.", fixed = TRUE)
})

test_that("Bayesian evidence strength and selected-target status remain independent", {
  # Strong favorable evidence can still fall short of the selected target.
  strongUnmet <- c(jaspAcceptanceSampling:::.bsCreateBayesFactorEvidenceText(log(20), tolerance = 0),
                    jaspAcceptanceSampling:::.bsCreateBayesFactorTargetText(logBayesFactor = log(20), minimumBayesFactor = 30, tolerance = 0))
  expect_match(strongUnmet[1], "Strong evidence", fixed = TRUE)
  expect_match(strongUnmet[2], "has not been reached.", fixed = TRUE)

  # A permitted low target can be reached despite evidence in the other direction.
  adverseReached <- c(jaspAcceptanceSampling:::.bsCreateBayesFactorEvidenceText(log(.05), tolerance = 0),
                       jaspAcceptanceSampling:::.bsCreateBayesFactorTargetText(logBayesFactor = log(.05), minimumBayesFactor = .01, tolerance = 0))
  expect_match(adverseReached[1], "Strong evidence that the proportion defective is at or above", fixed = TRUE)
  expect_match(adverseReached[2], "has been reached.", fixed = TRUE)
  expect_match(adverseReached[2], "does not require evidence favoring", fixed = TRUE)

  # BF=1 meets a target of 1 without favoring either hypothesis.
  neutral <- c(jaspAcceptanceSampling:::.bsCreateBayesFactorEvidenceText(log(1), tolerance = 0),
               jaspAcceptanceSampling:::.bsCreateBayesFactorTargetText(logBayesFactor = log(1), minimumBayesFactor = 1, tolerance = 0))
  expect_match(neutral[1], "Inconclusive", fixed = TRUE)
  expect_match(neutral[2], "has been reached.", fixed = TRUE)
})
