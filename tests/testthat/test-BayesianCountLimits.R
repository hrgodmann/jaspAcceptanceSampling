test_that("Bayesian count limits match decimal rational floors throughout the input range", {
  # Products of these integer numerators and lot sizes are exact doubles (< 2^53),
  # so rational division is an independent reference without proportion * N.
  numerators <- unique(c(1:9, 70000, 290000, 580000, 999990:999999,
                         (seq_len(500) * 7919) %% 1000000))
  grid <- expand.grid(k = numerators,
                      N = c(1, 2, 3, 7, 10, 29, 99, 100, 101, 1000, 1001,
                            10000, 10001, 99991, 99999, 100000))
  exact_products <- grid$k * grid$N
  expected <- (exact_products - exact_products %% 1000000) / 1000000
  actual <- mapply(jaspAcceptanceSampling:::.bsCountLimit,
                   proportion = grid$k / 1000000, lotSize = grid$N)
  expect_equal(actual, expected)

  # A dense percentage grid includes small lots and familiar decimal boundaries.
  percentages <- expand.grid(k = 1:99, N = 1:1000)
  exact_products <- percentages$k * percentages$N
  expected <- (exact_products - exact_products %% 100) / 100
  actual <- mapply(jaspAcceptanceSampling:::.bsCountLimit,
                   proportion = percentages$k / 100, lotSize = percentages$N)
  expect_equal(actual, expected)
})

test_that("Bayesian count limits preserve genuinely fractional boundaries", {
  proportions <- c(.289999, .29, .290001, .29 - 1e-10, .29 + 1e-10)
  actual <- vapply(proportions, jaspAcceptanceSampling:::.bsCountLimit,
                   numeric(1), lotSize = 100)
  expect_equal(actual, c(28, 29, 29, 28, 29))
  expect_equal(jaspAcceptanceSampling:::.bsCountLimit(.290001, 100000), 29000)
  expect_equal(jaspAcceptanceSampling:::.bsCountLimit(.289999, 100000), 28999)
})

test_that("fully inspected Bayesian lots retain inclusive AQL and RQL boundaries", {
  counts <- c(6, 7, 8, 28, 29, 30)
  regions <- c("good", "good", "middle", "middle", "middle", "bad")
  actual <- vapply(counts, function(d) {
    unlist(jaspAcceptanceSampling:::.bsPPDRegionProbs(
      n = 100, d = d, N = 100, aql = .07, rql = .29, alpha = 1, beta = 1
    ))
  }, numeric(3))
  expected <- vapply(regions, function(region) {
    setNames(as.numeric(c("good", "middle", "bad") == region),
             c("good", "middle", "bad"))
  }, numeric(3))
  dimnames(actual) <- dimnames(expected) <- NULL
  expect_equal(actual, expected)

  # .29 * 100 rounds below 29: this explicitly catches the AQL-side regression.
  equality <- jaspAcceptanceSampling:::.bsPPDRegionProbs(
    n = 100, d = 29, N = 100, aql = .29, rql = .5, alpha = 1, beta = 1
  )
  expect_equal(equality, list(good = 1, middle = 0, bad = 0))
})

test_that("Bayesian predictive probabilities, labels, summary and plot share count boundaries", {
  options <- jaspTools::analysisOptions("BayesianSampling")
  options$priorplan <- "uniform"
  options$aqlplan <- .07
  options$rqlplan <- .29
  options$data_ninfer <- 20
  options$data_dinfer <- 3
  options$lot_sizeinfer <- 100
  options$inferPosteriorinfer <- TRUE
  options$showInferenceTableinfer <- TRUE
  options$ppdPlotinfer <- TRUE
  options$showPlansplan <- FALSE
  options$priorPlotplan <- FALSE
  options$posteriorPlotinfer <- FALSE
  options$showThreeBFplan <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", dataset = NULL,
                                    options = options, view = FALSE, quiet = TRUE)
  expect_equal(results$status, "complete")
  collection <- results$results$infContainer$collection
  table <- collection$infContainer_inferenceDecisionTable$data

  # Independent predictive reference: integrate Binomial(80,theta) against
  # the analytically known Beta(4,18) posterior after 3 defectives among 20.
  totals <- 3:83
  pmf <- vapply(0:80, function(k) {
    stats::integrate(function(theta) {
      stats::dbinom(k, 80, theta) * stats::dbeta(theta, 4, 18)
    }, lower = 0, upper = 1, rel.tol = 1e-10)$value
  }, numeric(1))
  expected <- c(good = sum(pmf[totals <= 7]),
                middle = sum(pmf[totals > 7 & totals <= 29]),
                bad = sum(pmf[totals > 29]))
  actual <- unlist(jaspAcceptanceSampling:::.bsPPDRegionProbs(
    n = 20, d = 3, N = 100, aql = .07, rql = .29, alpha = 1, beta = 1
  ))
  expect_equal(actual, expected, tolerance = 1e-9)
  expect_equal(vapply(table[7:9], `[[`, character(1), "col_2"),
               unname(sprintf("%.3f", expected)))

  labels <- vapply(table[7:9], `[[`, character(1), "col_1")
  labels <- gsub("&gt;", ">", gsub("&amp;", "&", labels, fixed = TRUE), fixed = TRUE)
  expect_equal(labels, c("PPD Good (\u2264 7 defectives in Lot)",
                         "PPD Middle (> 7 & \u2264 29 defectives)",
                         "PPD Bad (> 29 defectives in Lot)"))
  summary <- collection$infContainer_decisionSummaryText$rawtext
  expect_match(summary, "at most 29 total defectives", fixed = TRUE)
  expect_match(summary, sprintf("%.1f%%</b>", 100 * sum(expected[1:2])), fixed = TRUE)

  plot_name <- collection$infContainer_ppdPlot$data
  plot <- results$state$figures[[plot_name]]$obj
  expect_equal(plot$data$d_tot, totals)
  expect_equal(plot$data$pmf, pmf, tolerance = 1e-9)
  selected <- match(c(6, 7, 8, 28, 29, 30), plot$data$d_tot)
  expect_equal(as.character(plot$data$region[selected]),
               c("Good", "Good", "Middle", "Middle", "Middle", "Bad"))
  plotted_masses <- tapply(plot$data$pmf, plot$data$region, sum)
  expect_equal(unname(as.vector(plotted_masses)), unname(expected), tolerance = 1e-9)
  line_layer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomVline"), logical(1)))
  expect_length(line_layer, 1)
  expect_equal(sort(plot$layers[[line_layer]]$data$x), c(7.5, 29.5))
})
