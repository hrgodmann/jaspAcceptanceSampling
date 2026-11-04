test_that("impartial prior shapes match independent high-precision references", {
  references <- read.csv(testthat::test_path("fixtures", "bayesian-prior-shape-reference.csv"),
                         stringsAsFactors = FALSE)
  for (i in seq_len(nrow(references))) {
    z <- references[i, ]
    actual <- unlist(jaspAcceptanceSampling:::.bsSolveImpartial(z$location, z$rql, z$shape))
    expected <- c(alpha = z$alpha, beta = z$beta)
    expect_true(max(abs(actual - expected) / expected) < 1e-8, info = z$case)
    departure <- if (z$shape == "unimodal") actual - 1 else 1 - actual
    expect_true(abs(departure[1] / sum(departure) - z$location) <= 1e-8, info = z$case)
    expect_true(abs(pbeta(z$rql, actual[1], actual[2]) - .5) <= 1e-8, info = z$case)
  }
  # Omitting the selector preserves the previous ordinary default calibration.
  default <- jaspAcceptanceSampling:::.bsSolveImpartial(.05, .15)
  expect_equal(default, list(alpha = 1.242247034950371, beta = 5.602693664057053),
               tolerance = 1e-12)
})

test_that("impartial calibration covers both orientations and six-decimal boundary cases", {
  i <- seq_len(64)
  locations <- 1 + (i * 7919) %% 499998
  rqls <- locations + 1 + (i * 3571) %% (499999 - locations)
  peak <- data.frame(m = locations / 1e6, r = rqls / 1e6, shape = "unimodal")
  minimum <- data.frame(m = (1 + (i * 7919) %% 499999) / 1e6,
                        r = (500001 + (i * 3571) %% 499999) / 1e6, shape = "u_shaped")
  edgeLocations <- c(1, 2, 10, 1000, 50000, 490000, 499000, 499998) / 1e6
  peakEdges <- unique(rbind(data.frame(m = edgeLocations, r = edgeLocations + 1e-6),
                            data.frame(m = edgeLocations, r = .499999)))
  peakEdges$shape <- "unimodal"
  minimumEdges <- expand.grid(m = c(edgeLocations, .499999),
                              r = c(.500001, .9, .999999))
  minimumEdges$shape <- "u_shaped"
  left <- rbind(peak, minimum, peakEdges, minimumEdges)
  cases <- rbind(left, transform(left, m = 1 - m, r = 1 - r))
  parameters <- t(vapply(seq_len(nrow(cases)), function(i) {
    unlist(jaspAcceptanceSampling:::.bsSolveImpartial(cases$m[i], cases$r[i], cases$shape[i]))
  }, numeric(2)))
  isPeak <- cases$shape == "unimodal"
  expect_true(all(parameters[isPeak, ] > 1))
  expect_true(all(parameters[!isPeak, ] > 0 & parameters[!isPeak, ] < 1))
  departure <- abs(parameters - 1)
  actualLocation <- departure[, 1] / rowSums(departure)
  actualMass <- pbeta(cases$r, parameters[, 1], parameters[, 2])
  expect_lt(max(abs(actualLocation - cases$m)), 1e-8)
  expect_lt(max(abs(actualMass - .5)), 1e-8)
  expect_gt(max(parameters), 100000) # No legacy 10000 parameter ceiling.
})

test_that("non-unique, incompatible and uniform-boundary prior requests are rejected", {
  for (shape in c("unimodal", "u_shaped")) {
    expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(.5, .5, shape),
                  "do not determine a unique.*Custom")
    for (m in c(.05, .95))
      expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(m, .5, shape), "Custom")
    for (r in c(.1, .9))
      expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(.5, r, shape), "Custom")
  }
  for (pair in list(c(.05, .9), c(.95, .1), c(.05, .05), c(.95, .95)))
    expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(pair[1], pair[2], "unimodal"),
                  "RQL must lie strictly between the mode and 0.5")
  for (pair in list(c(.05, .15), c(.95, .85)))
    expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(pair[1], pair[2], "u_shaped"),
                  "minimum and RQL must lie on opposite sides of 0.5")
})

test_that("numerically unresolved near-uniform requests give explicit Custom guidance", {
  # These are closer to uniform than the six-decimal controls permit; a shape
  # rounds to exactly 1, so returning it would falsely claim a mode or minimum.
  expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(
    .05, .5 - .Machine$double.eps / 4, "unimodal"), "Could not reliably calibrate.*Custom")
  expect_error(jaspAcceptanceSampling:::.bsSolveImpartial(
    .05, .5 + .Machine$double.eps / 2, "u_shaped"), "Could not reliably calibrate.*Custom")
})

test_that("the impartial shape selector does not restrict a Custom U-shaped prior", {
  actual <- jaspAcceptanceSampling:::.bsCalculatePriorParams(
    prior = "custom", aql = .05, rql = .15, alpha = .2, beta = .4,
    impartialShape = "unimodal"
  )
  expect_equal(actual, list(alpha = .2, beta = .4))
})

test_that("a Custom minimum may lie above the RQL for an impartial U-shaped prior", {
  actual <- jaspAcceptanceSampling:::.bsCalculatePriorParams(
    prior = "impartial", aql = .05, rql = .1,
    impartialCustomMode = TRUE, impartialMode = .95, impartialShape = "u_shaped"
  )
  expect_equal(actual, list(alpha = .294771218818453057, beta = .962882695727286968),
               tolerance = 1e-10)
})
