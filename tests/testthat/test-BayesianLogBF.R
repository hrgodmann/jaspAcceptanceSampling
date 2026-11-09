# References use 85-digit arithmetic: closed-form beta tails, the beta/binomial
# identity, and direct integration of the density for interval probabilities.
# They were generated independently of the production R implementation.
test_that("Bayesian log probabilities and Bayes factors match high-precision references", {
  fixtures <- read.csv(testthat::test_path("fixtures", "bayesian-log-bf-reference.csv"),
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(fixtures))) {
    z <- fixtures[i, ]
    prior <- jaspAcceptanceSampling:::.bsComputeLogThreeRegionProbabilities(z$aql, z$rql, z$alpha, z$beta)
    post <- jaspAcceptanceSampling:::.bsComputeLogThreeRegionProbabilities(z$aql, z$rql,
                                                          z$alpha + z$d, z$beta + z$n - z$d)
    binary <- jaspAcceptanceSampling:::.bsComputeLogBayesFactor(rql = z$rql, n = z$n, c = z$d,
                                               alpha = z$alpha, beta = z$beta)
    three <- jaspAcceptanceSampling:::.bsComputeThreeHypothesisLogBayesFactor(aql = z$aql, rql = z$rql,
                                                       n = z$n, d = z$d,
                                                       alpha = z$alpha, beta = z$beta)
    actual <- c(unname(prior), unname(post), binary$log_bf, unname(three))
    expected <- unlist(z[c("prior_good", "prior_middle", "prior_bad",
                           "post_good", "post_middle", "post_bad", "log_bf",
                           "log_bf_gb", "log_bf_gm", "log_bf_mb")], use.names = FALSE)
    expect_true(max(abs(actual - expected)) < 1e-8,
                info = sprintf("%s: maximum absolute error %.17g", z$case,
                               max(abs(actual - expected))))
  }
})

.bsTestBinomialLogTails <- function(n, q) {
  # F_Beta(q; a,b) = P[Binomial(a+b-1,q) >= a]. Sum log PMFs directly,
  # rather than using pbinom(), which can call the same beta-tail routine.
  logMass <- stats::dbinom(0:n, n, q, log = TRUE)
  logAdd <- function(a, b) {
    high <- max(a, b)
    high + log1p(exp(min(a, b) - high))
  }
  cumulative <- reverseCumulative <- numeric(n + 1L)
  cumulative[1] <- logMass[1]
  reverseCumulative[n + 1L] <- logMass[n + 1L]
  for (i in 2:(n + 1L))
    cumulative[i] <- logAdd(cumulative[i - 1L], logMass[i])
  for (i in n:1)
    reverseCumulative[i] <- logAdd(reverseCumulative[i + 1L], logMass[i])
  list(lower = reverseCumulative[2:(n + 1L)], upper = cumulative[1:n])
}

test_that("all uniform-prior posterior counts have accurate finite log tails", {
  # Includes pbeta's finite-but-wrong and -Inf cases around Beta(9965,37),
  # plus both extreme quality limits permitted by the six-decimal controls.
  for (q in c(.000001, .1, .5, .999999)) {
    actual <- jaspAcceptanceSampling:::.bsComputeLogBetaTails(q, 1:10001, 10001:1)
    expected <- .bsTestBinomialLogTails(10001, q)
    expect_true(all(is.finite(actual$lower) & is.finite(actual$upper)))
    expect_true(max(abs(actual$lower - expected$lower), abs(actual$upper - expected$upper)) <
                 1e-8, info = paste("q =", q))
    expect_lt(max(abs(exp(actual$lower) + exp(actual$upper) - 1)), 1e-14)
  }
  reference <- .bsTestBinomialLogTails(10001, .5)
  counts <- 0:10000
  expectedC <- max(counts[reference$lower - reference$upper >= log(30)])
  expect_equal(jaspAcceptanceSampling:::.bsComputeAcceptanceNumber(
    n = 10000, rql = .5, minimumBayesFactor = 30, alpha = 1, beta = 1), expectedC)
})

test_that("Bayesian plans recover evidence hidden by clipped extreme priors", {
  for (beta in c(100, 10000)) {
    actual <- jaspAcceptanceSampling:::.bsComputeLogBayesFactor(rql = .5, n = 1, c = 0,
                                               alpha = 1, beta = beta)
    # Exact BF is (2^(b+1)-1)/(2^b-1), indistinguishable from 2 here.
    expect_equal(actual$log_bf, log(2), tolerance = 1e-10)
    expect_equal(jaspAcceptanceSampling:::.bsComputeAcceptanceNumber(
      n = 1, rql = .5, minimumBayesFactor = 1.5, alpha = 1, beta = beta), 0)
    plans <- jaspAcceptanceSampling:::.bsGeneratePlans(aql = .05, rql = .5,
                maximumSampleSize = 1, minimumBayesFactor = 1.5, alpha = 1, beta = beta)
    expect_equal(plans$n, 1)
    expect_equal(plans$c, 0)
    expect_equal(plans$log_bf, log(2), tolerance = 1e-10)
  }
})

test_that("exact acceptance ties survive roundoff while resolved differences remain distinct", {
  # With a uniform prior, q=.5 and zero defectives among n inspected items,
  # the exact BF is the integer 2^(n+1)-1 (all targets below are UI-valid).
  for (n in 1:12) {
    target <- 2^(n + 1) - 1
    evidence <- jaspAcceptanceSampling:::.bsComputeLogBayesFactor(rql = .5, n = n, c = 0,
                                                alpha = 1, beta = 1)
    # The decision-tie allowance is not a general error bound for beta tails.
    # Check it here against exact analytic ties, separately from reference accuracy.
    expect_true(abs(evidence$log_bf - log(target)) <= evidence$tolerance,
                info = sprintf("n = %d: tie error %.17g, allowance %.17g", n,
                               abs(evidence$log_bf - log(target)), evidence$tolerance))
    expect_equal(jaspAcceptanceSampling:::.bsComputeAcceptanceNumber(
      n = n, rql = .5, minimumBayesFactor = target, alpha = 1, beta = 1), 0)
    reached <- jaspAcceptanceSampling:::.bsMeetsLogBayesFactor(
      evidence$log_bf, log(target) + c(-1e-8, 0, 1e-8), evidence$tolerance)
    expect_equal(reached, c(TRUE, TRUE, FALSE))
  }
})
