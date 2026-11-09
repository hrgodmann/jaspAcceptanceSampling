test_that("three-region calibration retains the exact uniform boundary", {
  # The GUI accepts six decimals, so test the exact closed-form boundary here.
  for (constraint in c("none", "nonUShaped", "bothAtLeastOne")) {
    fit <- jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(1/3, 2/3, constraint)
    expect_equal(unlist(fit, use.names = FALSE), c(1, 1), tolerance = 1e-8)
  }
})

test_that("three-region calibration matches independent high-precision Beta references", {
  reference <- read.csv(test_path("fixtures", "bayesian-three-region-priors.csv"))
  for (i in seq_len(nrow(reference))) {
    case <- reference[i, ]
    fit <- jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(case$aql, case$rql, "none")
    actual <- unlist(fit, use.names = FALSE)
    expect_true(all(is.finite(actual) & actual > 0), label = case$case)
    expect_lt(max(abs(log(actual / c(case$alpha, case$beta)))), 1e-6, label = case$case)
    # Check all three masses on the returned parameters, not merely optimizer loss.
    cdf <- stats::pbeta(c(case$aql, case$rql), actual[1], actual[2])
    masses <- c(cdf[1], diff(cdf), stats::pbeta(case$rql, actual[1], actual[2], lower.tail = FALSE))
    expect_lte(max(abs(masses - 1/3)), 1e-8, label = case$case)
  }
})

test_that("three-region shape constraints retain true boundaries and reject incompatible priors", {
  reference <- read.csv(test_path("fixtures", "bayesian-three-region-priors.csv"))
  for (i in seq_len(nrow(reference))) {
    case <- reference[i, ]
    for (constraint in c("nonUShaped", "bothAtLeastOne")) {
      allowed <- if (constraint == "nonUShaped") any(c(case$alpha, case$beta) >= 1) else
        all(c(case$alpha, case$beta) >= 1)
      if (!allowed) {
        expect_error(jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(case$aql, case$rql, constraint),
                     "shape constraint", label = paste(case$case, constraint))
      } else {
        fit <- jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(case$aql, case$rql, constraint)
        parameters <- unlist(fit, use.names = FALSE)
        expect_true(if (constraint == "nonUShaped") any(parameters >= 1) else all(parameters >= 1),
                    label = paste(case$case, constraint))
        cdf <- stats::pbeta(c(case$aql, case$rql), parameters[1], parameters[2])
        expect_lte(max(abs(c(cdf[1], diff(cdf), 1-cdf[2]) - 1/3)), 1e-8,
                   label = paste(case$case, constraint))
      }
    }
  }
})

test_that("three-region calibration remains accurate under reflection and endpoint limits", {
  pairs <- list(c(.000001, .000002), c(.000001, .999999), c(.000001, .05),
                c(.49, .51), c(.499999, .5), c(.333333, .666667),
                c(.8, .800001), c(.999998, .999999))
  for (pair in pairs) {
    fit <- jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(pair[1], pair[2], "none")
    mirror <- jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(1-pair[2], 1-pair[1], "none")
    p <- unlist(fit, use.names = FALSE)
    reflected <- rev(unlist(mirror, use.names = FALSE))
    expect_lt(max(abs(log(p / reflected))), 1e-6)
    cdf <- stats::pbeta(pair, p[1], p[2])
    expect_lte(max(abs(c(cdf[1], diff(cdf), 1-cdf[2]) - 1/3)), 1e-8)
    for (constraint in c("nonUShaped", "bothAtLeastOne")) {
      # These cases are separated from a shape boundary, including the rounded
      # one-third/two-thirds inputs, whose exact solution is slightly U-shaped.
      allowed <- if (constraint == "nonUShaped") any(p >= 1) else all(p >= 1)
      if (allowed) {
        constrained <- unlist(jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(pair[1], pair[2], constraint))
        expect_true(if (constraint == "nonUShaped") any(constrained >= 1) else all(constrained >= 1))
      } else {
        expect_error(jaspAcceptanceSampling:::.bsSolveImpartialThreeRegion(pair[1], pair[2], constraint),
                     "shape constraint")
      }
    }
  }
})

test_that("highly concentrated calibrated priors retain accurate downstream Bayes factors", {
  priors <- read.csv(test_path("fixtures", "bayesian-three-region-priors.csv"))
  reference <- read.csv(test_path("fixtures", "bayesian-three-region-bf.csv"))
  for (i in seq_len(nrow(reference))) {
    case <- reference[i, ]
    prior <- priors[match(case$case, priors$case), ]
    actual <- jaspAcceptanceSampling:::.bsComputeLogBayesFactor(prior$rql, case$n, case$d, prior$alpha, prior$beta)$log_bf
    expect_lt(abs(actual-case$log_bf), 5e-8, label = paste(case$case, case$d))
    three <- jaspAcceptanceSampling:::.bsComputeThreeHypothesisLogBayesFactor(prior$aql, prior$rql, case$n, case$d,
                                                      prior$alpha, prior$beta)
    expected <- unlist(case[c("logBF_GB", "logBF_GM", "logBF_MB")], use.names = FALSE)
    expect_lt(max(abs(unname(three)-expected)), 5e-8, label = paste(case$case, case$d))
  }
})

test_that("beta-binomial predictions match high-precision references across concentration extremes", {
  reference <- read.csv(test_path("fixtures", "bayesian-three-region-pmf.csv"))
  for (case in split(reference, reference$case)) {
    actual <- jaspAcceptanceSampling:::.bsComputeBetaBinomialPmf(case$x, case$n[1], case$alpha[1], case$beta[1])
    expect_lt(max(abs(actual-case$pmf)), 2e-12, label = case$case[1])
    expect_equal(sum(actual), 1, tolerance = 1e-12, label = case$case[1])
    normal <- case$log_pmf > -700
    expect_lt(max(abs(log(actual[normal])-case$log_pmf[normal])), 1e-10, label = case$case[1])
  }
})

test_that("beta-binomial subsets preserve support and zero remaining items", {
  expected <- jaspAcceptanceSampling:::.bsComputeBetaBinomialPmf(0:10, 10, 2, 3)
  actual <- jaspAcceptanceSampling:::.bsComputeBetaBinomialPmf(c(10, 0, 4, 4, -1, 11, .5, Inf, -Inf, NA, NaN), 10, 2, 3)
  expect_equal(actual, c(expected[c(11, 1, 5, 5)], 0, 0, 0, 0, 0, NA, NA))
  expect_equal(jaspAcceptanceSampling:::.bsComputeBetaBinomialPmf(c(0, 1), 0, 1e-20, 1e-20), c(1, 0))
})

test_that("three-region default permits a calibrated U-shaped prior", {
  parameters <- jaspAcceptanceSampling:::.bsCalculatePriorParameters(
    "impartialThreeRegion", aql = .1, rql = .9)

  expect_true(parameters$alpha > 0 && parameters$alpha < 1)
  expect_true(parameters$beta > 0 && parameters$beta < 1)
  expect_equal(stats::pbeta(c(.1, .9), parameters$alpha, parameters$beta),
               c(1/3, 2/3), tolerance = 1e-8)
})
