# Copyright (C) 2018-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#' @importFrom jaspBase jaspDeps %setOrRetrieve%
#' @importFrom rlang .data
#' @export


BayesianSampling <- function(jaspResults, dataset = NULL, options, ...) {

  # 0. Introductory Procedure Text (Always shown up front)
  .bsProcedure(jaspResults, options, position = 0)

  # 1. Planning
  .bsPlanningAnalysis(jaspResults, options, position = 1)

  # 2. Inference
  .bsInferenceAnalysis(jaspResults, options, position = 10)
}

# Dependency helpers
.bsPlanningDeps <- function() {
  c(
    "max_nplan", "min_bfplan", "aqlplan", "rqlplan", "priorplan",
    "alphaplan", "betaplan",
    "impartialShapeplan", "impartialCustomModeplan", "impartialModeplan",
    "impartialThreeConstraintplan",   # NEW
    "showPlansplan", "priorPlotplan",
    "showThreeBFplan"
  )
}

.bsInferenceDeps <- function() {
  c(
    "inferPosteriorinfer",

    "aqlplan", "rqlplan", "min_bfplan", "priorplan", "alphaplan", "betaplan",
    "impartialShapeplan", "impartialCustomModeplan", "impartialModeplan",
    "impartialThreeConstraintplan",

    "data_ninfer", "data_dinfer", "lot_sizeinfer",
    "showInferenceTableinfer", "posteriorPlotinfer", "ppdPlotinfer"
  )
}


.bsNumericalError <- function() {
  stop(gettext("The Bayes factor could not be calculated reliably for these inputs. Please adjust the quality limits or prior parameters."), call. = FALSE)
}

.bsLogBetaTailCF <- function(q, alpha, beta) {
  # Incomplete-beta continued fraction (DLMF 8.17.22-23), with modified Lentz updates.
  # https://dlmf.nist.gov/8.17#v
  # This is called for the small tail, where direct probability-scale beta
  # series can underflow internally even when pbeta(log.p = TRUE) is requested.
  # Guard recurrence denominators only; never clip a probability.
  tiny <- 1e-300
  guard <- function(value) {
    if (!is.finite(value))
      .bsNumericalError()
    if (abs(value) < tiny) {
      if (value < 0) -tiny else tiny
    } else value
  }
  total <- alpha + beta
  alphaPlus <- alpha + 1
  alphaMinus <- alpha - 1
  c <- 1
  d <- 1 / guard(1 - total * q / alphaPlus)
  fraction <- d
  converged <- FALSE
  for (m in seq_len(10000L)) {
    twiceM <- 2 * m
    term <- m * (beta - m) * q / ((alphaMinus + twiceM) * (alpha + twiceM))
    d <- 1 / guard(1 + term * d)
    c <- guard(1 + term / c)
    fraction <- fraction * d * c
    term <- -(alpha + m) * (total + m) * q / ((alpha + twiceM) * (alphaPlus + twiceM))
    d <- 1 / guard(1 + term * d)
    c <- guard(1 + term / c)
    change <- d * c
    fraction <- fraction * change
    if (abs(change - 1) <= 8 * .Machine$double.eps) {
      converged <- TRUE
      break
    }
  }
  if (!converged || !is.finite(fraction) || fraction <= 0)
    .bsNumericalError()
  result <- alpha * log(q) + beta * log1p(-q) - lbeta(alpha, beta) +
    log(fraction) - log(alpha)
  if (!is.finite(result) || result > 0)
    .bsNumericalError()
  result
}

.bsLogBetaTails <- function(q, alpha, beta) {
  size <- max(length(q), length(alpha), length(beta))
  q <- rep_len(q, size)
  alpha <- rep_len(alpha, size)
  beta <- rep_len(beta, size)
  if (any(!is.finite(q) | q <= 0 | q >= 1 |
          !is.finite(alpha) | alpha <= 0 | !is.finite(beta) | beta <= 0))
    .bsNumericalError()
  lower <- suppressWarnings(pbeta(q, alpha, beta, log.p = TRUE))
  upper <- suppressWarnings(pbeta(q, alpha, beta, lower.tail = FALSE, log.p = TRUE))
  # R's beta series can also return inaccurate *finite* logs below the normal
  # probability range. Switch before that underflow boundary, not just at Inf.
  repair <- which(!is.finite(lower) | !is.finite(upper) |
                    lower > 0 | upper > 0 | pmin(lower, upper) < -700)
  for (i in repair) {
    lowerIsSmall <- if (is.na(lower[i]) || is.na(upper[i])) {
      q[i] < (alpha[i] + 1) / (alpha[i] + beta[i] + 2)
    } else lower[i] < upper[i]
    small <- if (lowerIsSmall) {
      .bsLogBetaTailCF(q[i], alpha[i], beta[i])
    } else {
      .bsLogBetaTailCF(1 - q[i], beta[i], alpha[i])
    }
    large <- if (small < -log(2)) log1p(-exp(small)) else log(-expm1(small))
    lower[i] <- if (lowerIsSmall) small else large
    upper[i] <- if (lowerIsSmall) large else small
  }
  if (any(!is.finite(lower) | !is.finite(upper) | lower > 0 | upper > 0))
    .bsNumericalError()
  list(lower = lower, upper = upper)
}

.bsLogBetaInterval <- function(lower, upper, alpha, beta, lowerTails, upperTails) {
  # Subtract in the better-separated tail using expm1, avoiding 1 - P.
  gaps <- c(upperTails$lower - lowerTails$lower,
            lowerTails$upper - upperTails$upper)
  side <- which.max(gaps)
  if (length(side) && gaps[side] > 1e-4) {
    large <- if (side == 1L) upperTails$lower else lowerTails$upper
    return(large + log(-expm1(-gaps[side])))
  }

  # Nearly equal log-CDFs can lose even the relative size of a narrow interval.
  # Integrate on [0,1], scaling both the interval width and the density peak.
  # This switch is an accuracy safeguard, not a floor on the probability.
  width <- upper - lower
  peakPoints <- c(lower, upper)
  if (alpha > 1 && beta > 1) {
    mode <- (alpha - 1) / (alpha + beta - 2)
    if (mode > lower && mode < upper)
      peakPoints <- c(peakPoints, mode)
  }
  logPeak <- max(dbeta(peakPoints, alpha, beta, log = TRUE))
  integral <- tryCatch(integrate(
    function(t) exp(dbeta(lower + width * t, alpha, beta, log = TRUE) - logPeak),
    0, 1, rel.tol = 1e-10, abs.tol = 1e-12, subdivisions = 1000L,
    stop.on.error = FALSE
  ), error = function(e) NULL)
  if (is.null(integral) || integral$message != "OK" ||
      !is.finite(integral$value) || integral$value <= 0)
    .bsNumericalError()
  result <- log(width) + logPeak + log(integral$value)
  if (!is.finite(result) || result > 0)
    .bsNumericalError()
  result
}

.bsLogThreeRegionProbs <- function(aql, rql, alpha, beta) {
  atAQL <- .bsLogBetaTails(aql, alpha, beta)
  atRQL <- .bsLogBetaTails(rql, alpha, beta)
  c(good = atAQL$lower,
    middle = .bsLogBetaInterval(aql, rql, alpha, beta, atAQL, atRQL),
    bad = atRQL$upper)
}

.bsLogBF <- function(rql, n, c, alpha, beta, prior_log_tails = NULL) {
  prior <- if (is.null(prior_log_tails)) .bsLogBetaTails(rql, alpha, beta) else prior_log_tails
  post <- .bsLogBetaTails(rql, alpha + c, beta + n - c)
  logBF <- (post$lower - post$upper) - (prior$lower - prior$upper)
  # Round-off allowance for subtracting four log probabilities. This is a
  # numerical tie convention, not a change to the selected evidence target.
  tolerance <- 16 * .Machine$double.eps * pmax(
    1, abs(post$lower) + abs(post$upper) + abs(prior$lower) + abs(prior$upper)
  )
  if (any(!is.finite(logBF) | !is.finite(tolerance)))
    .bsNumericalError()
  list(log_bf = logBF, tolerance = tolerance)
}

.bsMeetsLogBF <- function(log_bf, log_target, tolerance = 0) {
  log_bf >= log_target - tolerance
}

.bsFormatBF <- function(log_bf, digits = 2L) {
  vapply(log_bf, function(value) {
    if (!is.finite(value))
      .bsNumericalError()
    if (value >= log(0.5 * 10^(-digits)) && value < log(1e6)) {
      displayed <- sprintf(paste0("%.", digits, "f"), exp(value))
      if (as.numeric(displayed) > 0)
        return(displayed)
    }
    exponent <- floor(value / log(10))
    mantissa <- exp(value - exponent * log(10))
    if (round(mantissa, digits) >= 10) {
      mantissa <- 1
      exponent <- exponent + 1
    }
    paste0(sprintf(paste0("%.", digits, "f"), mantissa), "e",
           if (exponent >= 0) "+" else "-", sprintf("%.0f", abs(exponent)))
  }, character(1), USE.NAMES = FALSE)
}

.bsBFColumn <- function(table, name, title, log_bf) {
  values <- exp(log_bf)
  if (all(is.finite(values) & values >= .Machine$double.xmin)) {
    table$addColumnInfo(name = name, title = title, type = "number", format = "sf:3")
    values
  } else {
    # Avoid overflow, underflow and the loss of significant digits in subnormal
    # doubles. Keep the whole column consistently typed and show its magnitude.
    table$addColumnInfo(name = name, title = title, type = "string")
    .bsFormatBF(log_bf, digits = 3L)
  }
}


# Beta-Binomial PMF: P(X = x | n_trials, alpha, beta)
.bsBetaBinomPMF <- function(x, n_trials, alpha, beta) {
  # Adjacent-count ratio p(k)/p(k-1) gives the same beta-binomial PMF
  # without subtracting large, nearly equal log-Beta values. Such subtraction
  # loses precision for the concentrated priors needed by close quality limits.
  k <- seq_len(n_trials)
  logRatio <- log(n_trials - k + 1) - log(k) +
    log(alpha + (k - 1)) - log(beta + (n_trials - k))
  logWeights <- c(0, cumsum(logRatio))
  weights <- exp(logWeights - max(logWeights))
  probabilities <- weights / sum(weights)

  result <- numeric(length(x))
  valid <- is.finite(x) & x >= 0 & x <= n_trials & x == floor(x)
  result[valid] <- probabilities[x[valid] + 1]
  result[is.na(x)] <- NA_real_
  result
}

.bsCountLimit <- function(proportion, lotSize) {
  limit <- proportion * lotSize
  integerLimit <- round(limit)
  # Decimal input and multiplication can move an integer boundary by a few ulps.
  # Snap only floating-point noise before flooring; retain fractional limits.
  tolerance <- 2 * .Machine$double.eps * abs(limit)
  if (abs(limit - integerLimit) <= tolerance)
    limit <- integerLimit
  floor(limit)
}

# PPD region probabilities based on posterior predictive distribution
# D_rem | k,n ~ BetaBinomial(N-n, a+k, b+n-k)
# D_tot = k + D_rem
# Good:   D_tot <= floor(AQL * N)
# Middle: floor(AQL * N) < D_tot <= floor(RQL * N)
# Bad:    D_tot > floor(RQL * N)
.bsPPDRegionProbs <- function(n, d, N, aql, rql, alpha, beta) {
  alpha_post <- alpha + d
  beta_post  <- beta + n - d
  n_rem      <- N - n

  aql_count <- .bsCountLimit(aql, N)
  rql_count <- .bsCountLimit(rql, N)

  # D_rem ranges from 0 to n_rem; D_tot = d + D_rem
  d_rem_vals <- 0:n_rem
  pmf <- .bsBetaBinomPMF(d_rem_vals, n_rem, alpha_post, beta_post)

  d_tot <- d + d_rem_vals

  p_good   <- sum(pmf[d_tot <= aql_count])
  p_middle <- sum(pmf[d_tot > aql_count & d_tot <= rql_count])
  p_bad    <- sum(pmf[d_tot > rql_count])

  list(good = p_good, middle = p_middle, bad = p_bad)
}

# Full PPD PMF for D_tot
.bsPPDFull <- function(n, d, N, alpha_post, beta_post) {
  n_rem <- N - n
  d_rem_vals <- 0:n_rem
  pmf <- .bsBetaBinomPMF(d_rem_vals, n_rem, alpha_post, beta_post)
  d_tot <- d + d_rem_vals
  data.frame(d_tot = d_tot, pmf = pmf)
}


.bsAcceptanceNumber <- function(n, rql, min_bf, alpha, beta) {
  if (!is.finite(n) || !is.finite(rql) || !is.finite(min_bf) ||
      !is.finite(alpha) || !is.finite(beta) || n < 0 || min_bf <= 0) {
    return(NA_integer_)
  }
  counts <- seq.int(0L, as.integer(floor(n)))
  evidence <- .bsLogBF(rql, n, counts, alpha, beta)
  accepted <- counts[.bsMeetsLogBF(evidence$log_bf, log(min_bf), evidence$tolerance)]
  if (length(accepted)) max(accepted) else NA_integer_
}

# Utility functions
.bsCalculatePriorParams <- function(prior, aql, rql,
                                    alpha = NULL, beta = NULL,
                                    impartialCustomMode = FALSE,
                                    impartialMode = NULL,
                                    impartialThreeConstraint = "no_U",
                                    impartialShape = "unimodal") {

  if (prior == "impartial") {
    location <- if (isTRUE(impartialCustomMode) && !is.null(impartialMode)) impartialMode else aql
    params <- .bsSolveImpartial(location, rql, shape = impartialShape)
    alpha  <- params$alpha
    beta   <- params$beta

  } else if (prior == "impartial_three") {
    params <- .bsSolveImpartialThree(aql, rql, constraint = impartialThreeConstraint)
    alpha <- params$alpha
    beta  <- params$beta

  } else if (prior == "uniform") {
    alpha <- beta <- 1

  } else if (prior == "custom") {
  }

  list(alpha = alpha, beta = beta)
}

.bsSafePbeta <- function(x, a, b) {
  out <- suppressWarnings(
    tryCatch(pbeta(x, a, b), error = function(e) NA_real_)
  )
  if (!is.finite(out)) NA_real_ else out
}

.bsIncreasingLogRoot <- function(f, failure, start = 0) {
  # The bounds are the range of positive representable doubles, not prior limits.
  lowerLimit <- log(.Machine$double.xmin) + log(.Machine$double.eps)
  upperLimit <- log(.Machine$double.xmax)
  start <- min(upperLimit, max(lowerLimit, start))
  atStart <- f(start)
  if (!is.finite(atStart)) failure()
  if (atStart == 0) return(start)
  direction <- if (atStart < 0) 1 else -1
  step <- 1
  repeat {
    bound <- min(upperLimit, max(lowerLimit, start + direction * step))
    atBound <- f(bound)
    if (!is.finite(atBound)) failure()
    if (atBound == 0) return(bound)
    if (sign(atStart) != sign(atBound)) break
    if (bound == lowerLimit || bound == upperLimit)
      failure()
    step <- 2 * step
  }
  if (direction > 0) {
    stats::uniroot(f, c(start, bound), f.lower = atStart, f.upper = atBound,
                   tol = .Machine$double.eps, maxiter = 1000L, check.conv = TRUE)$root
  } else {
    stats::uniroot(f, c(bound, start), f.lower = atBound, f.upper = atStart,
                   tol = .Machine$double.eps, maxiter = 1000L, check.conv = TRUE)$root
  }
}

.bsSolveThreeRegionBeta <- function(c1, c2,
                                    pi = c(1/3, 1/3, 1/3),
                                    constraint = c("none", "no_U", "both_ge_1")) {
  constraint <- tryCatch(match.arg(constraint), error = function(e) {
    stop(gettext("Please select a valid shape constraint for the three-region prior."), call. = FALSE)
  })
  if (length(c1) != 1L || !is.numeric(c1) || !is.finite(c1) ||
      length(c2) != 1L || !is.numeric(c2) || !is.finite(c2) ||
      c1 <= 0 || c1 >= c2 || c2 >= 1)
    stop(gettext("Please ensure 0 < AQL < RQL < 1."), call. = FALSE)
  if (!is.numeric(pi) || length(pi) != 3L || any(!is.finite(pi) | pi <= 0) ||
      abs(sum(pi) - 1) > 1e-12)
    stop(gettext("Target region probabilities must be positive and sum to 1."), call. = FALSE)

  failure <- function() {
    stop(gettextf("Could not reliably calibrate a three-region Beta prior for AQL %1$s and RQL %2$s. Change the quality limits or choose a Custom prior.",
                  .bsFormatQuality(c1), .bsFormatQuality(c2)), call. = FALSE)
  }
  allowed <- function(p) {
    switch(constraint, none = TRUE, no_U = any(p >= 1), both_ge_1 = all(p >= 1))
  }
  calibrated <- function(p) {
    if (any(!is.finite(p) | p <= 0)) return(FALSE)
    probabilities <- exp(.bsLogThreeRegionProbs(c1, c2, p[1], p[2]))
    all(is.finite(probabilities)) && all(abs(probabilities - pi) <= 1e-8)
  }
  p1 <- pi[1]
  p2 <- sum(pi[1:2])
  if (!(p1 < p2 && p2 < 1)) failure()
  parameters <- tryCatch({
    # For fixed alpha, F(AQL) strictly increases with beta. Along this first
    # quantile, F(RQL) strictly increases with alpha from p1 to 1, so the
    # unconstrained solution is unique. Shape constraints are checked afterward.
    betaForAlpha <- function(logAlpha) {
      alpha <- exp(logAlpha)
      if (!is.finite(alpha) || alpha <= 0) failure()
      objective <- function(logBeta) {
        beta <- exp(logBeta)
        if (!is.finite(beta) || beta <= 0) failure()
        # Bracketing only needs a sign; probability-scale saturation at 0 or 1
        # is safe here because the target is interior. No BF uses these values.
        value <- .bsSafePbeta(c1, alpha, beta)
        if (!is.finite(value)) failure()
        value - p1
      }
      .bsIncreasingLogRoot(objective, failure = failure,
                            start = logAlpha + log1p(-c1) - log(c1))
    }
    objective <- function(logAlpha) {
      logBeta <- betaForAlpha(logAlpha)
      value <- .bsSafePbeta(c2, exp(logAlpha), exp(logBeta))
      if (!is.finite(value)) failure()
      value - p2
    }
    logAlpha <- .bsIncreasingLogRoot(objective, failure = failure)
    logBeta <- betaForAlpha(logAlpha)
    p <- c(alpha = exp(logAlpha), beta = exp(logBeta))
    if (!calibrated(p)) failure()
    p
  }, error = function(e) failure())

  if (!allowed(parameters)) {
    # Check true shape-boundary candidates instead of adding a parameter epsilon
    # or optimizing away from the calibrated prior. Every region must still fit.
    candidates <- list(
      c(alpha = 1, beta = log1p(-p1) / log1p(-c1)),
      c(alpha = log(p1) / log(c1), beta = 1),
      c(alpha = 1, beta = 1)
    )
    valid <- tryCatch(vapply(candidates, function(p) allowed(p) && calibrated(p), logical(1)),
                       error = function(e) failure())
    if (any(valid)) {
      candidates <- candidates[valid]
      distances <- vapply(candidates, function(p) sum((log(p) - log(parameters))^2), numeric(1))
      parameters <- candidates[[which.min(distances)]]
    } else {
      message <- if (constraint == "no_U") {
        gettext("The calibrated three-region Beta prior is U-shaped, which the selected shape constraint excludes. Change AQL/RQL, allow a U-shaped prior, or choose a Custom prior.")
      } else {
        gettext("The calibrated three-region Beta prior does not have both shape parameters at least 1, as required by the selected shape constraint. Change AQL/RQL, choose a different shape constraint, or choose a Custom prior.")
      }
      stop(message, call. = FALSE)
    }
  }
  list(alpha = unname(parameters[1]), beta = unname(parameters[2]))
}

.bsSolveImpartial <- function(location, rql, shape = "unimodal") {
  if (length(shape) != 1L || is.na(shape) || !shape %in% c("unimodal", "u_shaped"))
    stop(gettext("Please select a valid shape for the impartial prior."), call. = FALSE)
  if (length(location) != 1L || !is.numeric(location) || !is.finite(location) ||
      location <= 0 || location >= 1 || length(rql) != 1L || !is.numeric(rql) ||
      !is.finite(rql) || rql <= 0 || rql >= 1)
    stop(gettext("The prior location and RQL must both be strictly between 0 and 1."), call. = FALSE)

  if (location == 0.5 && rql == 0.5)
    stop(gettext("A location and RQL of 0.5 do not determine a unique Beta prior of the selected shape. Choose a Custom prior to specify its parameters."), call. = FALSE)

  feasible <- if (shape == "unimodal") {
    (location < rql && rql < 0.5) || (0.5 < rql && rql < location)
  } else {
    (location < 0.5 && rql > 0.5) || (location > 0.5 && rql < 0.5)
  }
  if (!feasible) {
    message <- if (shape == "unimodal") {
      gettextf("A unimodal Beta prior cannot have mode %1$s and 50%% probability below RQL %2$s. The RQL must lie strictly between the mode and 0.5. Change the location, shape or RQL, or choose a Custom prior.",
               .bsFormatQuality(location), .bsFormatQuality(rql))
    } else {
      gettextf("A U-shaped Beta prior cannot have minimum %1$s and 50%% probability below RQL %2$s. The minimum and RQL must lie on opposite sides of 0.5. Change the location, shape or RQL, or choose a Custom prior.",
               .bsFormatQuality(location), .bsFormatQuality(rql))
    }
    stop(message, call. = FALSE)
  }

  # alpha = 1 + k*location, beta = 1 + k*(1-location) fixes the stationary
  # point. k > 0 makes it a mode; -1/max(location,1-location) < k < 0 a minimum.
  # Transform each shape's search so bracketing cannot cross into the other shape.
  parameters <- if (shape == "unimodal") {
    function(t) {
      k <- expm1(t)
      c(alpha = 1 + k * location, beta = 1 + k * (1 - location))
    }
  } else {
    function(t) {
      smallest <- exp(-t)
      m <- min(location, 1 - location)
      ratio <- m / (1 - m)
      # Near uniform, expm1 retains the small departure from 1. Near the other
      # boundary, form the remaining positive shape without subtracting from 1.
      other <- if (t <= log(2)) 1 - ratio * (-expm1(-t)) else
        (1 - 2 * m) / (1 - m) + ratio * smallest
      if (location < 0.5) c(alpha = other, beta = smallest) else
        c(alpha = smallest, beta = other)
    }
  }

  failure <- function() {
    stop(gettextf("Could not reliably calibrate the selected prior for location %1$s and RQL %2$s. Change the location or RQL, or choose a Custom prior.",
                  .bsFormatQuality(location), .bsFormatQuality(rql)), call. = FALSE)
  }
  tryCatch({
    objective <- function(t) {
      p <- parameters(t)
      if (any(!is.finite(p) | p <= 0))
        failure()
      .bsLogBetaTails(rql, p[1], p[2])$lower + log(2)
    }
    # t=0 is the uniform boundary, used only to bracket, never as a valid answer.
    atZero <- log(rql) + log(2)
    upper <- 1
    atUpper <- objective(upper)
    while (sign(atZero) == sign(atUpper)) {
      upper <- 2 * upper
      atUpper <- objective(upper)
    }
    result <- stats::uniroot(objective, c(0, upper), f.lower = atZero, f.upper = atUpper,
                             tol = .Machine$double.eps, maxiter = 1000L, check.conv = TRUE)
    p <- parameters(result$root)
    validShape <- if (shape == "unimodal") all(p > 1) else all(p > 0 & p < 1)
    differences <- if (shape == "unimodal") p - 1 else 1 - p
    actualLocation <- differences[1] / sum(differences)
    probability <- exp(.bsLogBetaTails(rql, p[1], p[2])$lower)
    # Agreed absolute calibration accuracy, checked on the returned parameters.
    if (!validShape || !is.finite(actualLocation) || abs(actualLocation - location) > 1e-8 ||
        abs(probability - 0.5) > 1e-8)
      failure()
    list(alpha = unname(p[1]), beta = unname(p[2]))
  }, error = function(e) failure())
}

.bsSolveImpartialThree <- function(aql, rql,
                                   constraint = c("none", "no_U", "both_ge_1")) {
  constraint <- match.arg(constraint)
  fit <- .bsSolveThreeRegionBeta(aql, rql,
                                 pi = c(1/3, 1/3, 1/3),
                                 constraint = constraint)
  list(alpha = fit$alpha, beta = fit$beta)
}



.bsThreeRegionProbs <- function(aql, rql, alpha, beta) {
  # Conversion is only for displayed probabilities; BFs use the log masses.
  exp(.bsLogThreeRegionProbs(aql, rql, alpha, beta))
}

.bsLogBFThreeHyp <- function(aql, rql, n, d, alpha, beta) {
  prior <- .bsLogThreeRegionProbs(aql, rql, alpha, beta)
  post <- .bsLogThreeRegionProbs(aql, rql, alpha + d, beta + n - d)
  c(logBF_GB = unname((post["good"] - post["bad"]) - (prior["good"] - prior["bad"])),
    logBF_GM = unname((post["good"] - post["middle"]) - (prior["good"] - prior["middle"])),
    logBF_MB = unname((post["middle"] - post["bad"]) - (prior["middle"] - prior["bad"])))
}

.bsGeneratePlans <- function(aql, rql, max_n, min_bf, alpha, beta,
                             add_three_hyp = FALSE) {
  prior <- .bsLogBetaTails(rql, alpha, beta)
  plans <- data.frame(n = integer(), c = integer(), log_bf = numeric(),
                      log_bf_tolerance = numeric())
  for (n in seq_len(max_n)) {
    counts <- seq.int(0L, n)
    evidence <- .bsLogBF(rql, n, counts, alpha, beta, prior)
    accepted <- which(.bsMeetsLogBF(evidence$log_bf, log(min_bf), evidence$tolerance))
    if (length(accepted)) {
      i <- max(accepted)
      plans <- rbind(plans, data.frame(n = n, c = counts[i],
                     log_bf = evidence$log_bf[i], log_bf_tolerance = evidence$tolerance[i]))
    }
  }
  if (nrow(plans) && isTRUE(add_three_hyp)) {
    lbf <- t(mapply(function(n, c) .bsLogBFThreeHyp(aql, rql, n, c, alpha, beta),
                    n = plans$n, c = plans$c))
    plans <- cbind(plans, as.data.frame(lbf))
  }
  # Numerical failures must surface as errors, never disappear as omitted rows.
  attr(plans, "priorParameters") <- c(alpha = alpha, beta = beta)
  plans
}

.bsCreateStateContainer <- function(title, deps) {
  createJaspContainer(title = title) |>
    createJaspState(jaspDeps(deps))
}

.bsReuseOrRefreshContainer <- function(container, title, deps) {
  if (is.null(container) || !isTRUE(container$getError()))
    return(container)

  # Callers populate a container directly; a state wrapper would be a locked
  # R6 environment here. Register dependencies on the replacement container.
  createJaspContainer(title = title, dependencies = jaspDeps(deps))
}

.bsPPDXScale <- function(d_tot) {
  xMin <- min(d_tot, na.rm = TRUE)
  xMax <- max(d_tot, na.rm = TRUE)
  breaks <- if (xMin == xMax) xMin else jaspGraphs::getPrettyAxisBreaks(c(xMin, xMax))

  list(
    breaks = breaks,
    limits = c(xMin - 0.5, xMax + 0.5)
  )
}

.bsYScale <- function(values, expansion = 1.08) {
  yMax <- max(values, na.rm = TRUE)
  if (!is.finite(yMax) || yMax <= 0)
    yMax <- 1

  upperTarget <- yMax * expansion
  breaks <- jaspGraphs::getPrettyAxisBreaks(c(0, upperTarget))
  limits <- c(0, max(c(breaks, upperTarget), na.rm = TRUE))

  list(
    breaks = breaks,
    limits = limits,
    label_y = limits[2] * 0.97
  )
}

.bsThresholdLabelX <- function(x, side = c("left", "right"), lower, upper, min_offset = 0) {
  side <- match.arg(side)
  offset <- max((upper - lower) * 0.02, min_offset)

  if (side == "left")
    return(max(lower, x - offset))

  min(upper, x + offset)
}

.bsVisibleThreshold <- function(x, label, side, lower, upper, min_offset = 0) {
  if (!is.finite(x) || x < lower || x > upper)
    return(NULL)

  data.frame(
    x = x,
    label = label,
    label_x = .bsThresholdLabelX(x, side, lower, upper, min_offset = min_offset),
    hjust = if (identical(side, "left")) 1 else 0
  )
}


.bsFormatQuality <- function(value) {
  # Quality controls accept six decimal places; retain them in proportions and percentages.
  format(value, digits = 6, scientific = FALSE, trim = TRUE)
}

.bsPlanError <- function(container, options, message) {
  addError <- function(name, title, position, option) {
    if (isTRUE(options[[option]])) {
      container[[name]] <- createJaspTable(
        title = title, position = position, dependencies = jaspDeps(option)
      )
      container[[name]]$setError(message)
    }
  }
  addError("planTable", gettext("Sampling Plans"), 1, "showPlansplan")
  addError("threeBFTable", gettext("Three-Hypothesis Bayes Factors (at d = c)"),
           3, "showThreeBFplan")
}

.bsInferenceError <- function(container, message, title = gettext("Inference"), name = "inputError") {
  # Clear dependent results and previous errors before adding a visible child.
  # An error on an empty parent container alone is not displayed.
  for (output in c("inferenceDecisionTable", "decisionSummaryText", "distPlotPosterior",
                    "bfText", "ppdPlot", "priorError", "inputError", "numericalError"))
    container[[output]] <- NULL
  table <- createJaspTable(title = title, position = 0,
                           dependencies = jaspDeps(.bsInferenceDeps()))
  table$setError(message)
  container[[name]] <- table
}

.bsPriorError <- function(container, message, planning) {
  if (!planning)
    return(.bsInferenceError(container, message, gettext("Prior Distribution"), "priorError"))

  for (name in c("planTable", "threeBFTable", "distPlotPrior"))
    container[[name]] <- NULL
  table <- createJaspTable(title = gettext("Prior Distribution"), position = 0,
                           dependencies = jaspDeps(.bsPlanningDeps()))
  table$setError(message)
  container[["priorError"]] <- table
}

# Section 1: Planning
.bsPlanningAnalysis <- function(jaspResults, options, position) {

  if (!isTRUE(options$showPlansplan) &&
      !isTRUE(options$priorPlotplan) &&
      !isTRUE(options$showThreeBFplan)) {
    return()
  }

  container <- jaspResults[["planContainer"]] %setOrRetrieve% (
    .bsCreateStateContainer(gettext("Planning"), .bsPlanningDeps())
  )

  if (is.null(container))
    return()

  # JASP can reuse an errored container across UI updates; rebuild it so a
  # corrected AQL/RQL pair can render fresh outputs instead of keeping a stale
  # error banner.
  container <- .bsReuseOrRefreshContainer(container, gettext("Planning"), .bsPlanningDeps())

  on.exit({
    jaspResults[["planContainer"]] <- container
  }, add = TRUE)

  aql    <- options$aqlplan
  rql    <- options$rqlplan
  max_n  <- options$max_nplan
  min_bf <- options$min_bfplan

  if (!is.finite(aql) || !is.finite(rql) || aql <= 0 || rql <= 0 || aql >= rql || rql >= 1) {
    .bsPriorError(container, gettext("Please ensure 0 < AQL < RQL < 1."), planning = TRUE)
    return()
  }

  priorParams <- tryCatch(
    .bsCalculatePriorParams(
      options$priorplan, aql, rql,
      options$alphaplan, options$betaplan,
      impartialCustomMode = options$impartialCustomModeplan,
      impartialMode = options$impartialModeplan,
      impartialThreeConstraint = options$impartialThreeConstraintplan,
      impartialShape = options$impartialShapeplan
    ),
    error = function(e) {
      .bsPriorError(container, conditionMessage(e), planning = TRUE)
      jaspResults[["planData"]] <- NULL
      return(NULL)
    }
  )

  if (is.null(priorParams))
    return()

  if (!is.finite(priorParams$alpha) || !is.finite(priorParams$beta) ||
      priorParams$alpha <= 0 || priorParams$beta <= 0) {
    .bsPriorError(container, gettext("The selected prior results in invalid Beta parameters. Please adjust AQL/RQL or choose a different prior."), planning = TRUE)
    return()
  }

  container[["priorError"]] <- NULL

  if (isTRUE(options$priorPlotplan)) {
    .bsDistributionPlot(container, aql, rql, priorParams$alpha, priorParams$beta,
                        "Prior", position = 2, base = "priorPlotplan")
  }

  # The prior is defined independently of the requested sampling plan.
  if (!isTRUE(options$showPlansplan) && !isTRUE(options$showThreeBFplan))
    return()

  if (!is.finite(max_n) || max_n <= 0) {
    .bsPlanError(container, options, gettext("Please provide a valid maximum sample size (> 0)."))
    return()
  }

  if (!is.finite(min_bf) || min_bf <= 0) {
    .bsPlanError(container, options, gettext("Please provide a valid minimum Bayes factor (> 0)."))
    return()
  }

  # Also check the calibrated prior: older saved states do not depend on shape.
  cachedPlans <- jaspResults[["planData"]]
  if (!is.null(cachedPlans) &&
      (!"log_bf_tolerance" %in% names(cachedPlans$object) ||
       !identical(attr(cachedPlans$object, "priorParameters"),
                  c(alpha = priorParams$alpha, beta = priorParams$beta))))
    jaspResults[["planData"]] <- NULL

  plans <- tryCatch(jaspResults[["planData"]] %setOrRetrieve% (
    .bsGeneratePlans(aql, rql, max_n, min_bf,
                     priorParams$alpha, priorParams$beta,
                     add_three_hyp = isTRUE(options$showThreeBFplan)) |>
      createJaspState(jaspDeps(.bsPlanningDeps()))
  ), error = function(e) {
    .bsPlanError(container, options, conditionMessage(e))
    NULL
  })
  if (is.null(plans))
    return()

  if (nrow(plans) == 0) {
    # Compute the maximum attainable BF (best case: n = max_n, d = 0)
    maxEvidence <- .bsLogBF(rql, max_n, 0, priorParams$alpha, priorParams$beta)

    .bsPlanError(container, options, gettextf(
      "No sampling plan found. With the current prior (Beta(%1$.2f, %2$.2f)) and maximum sample size (n = %3$d), the maximum attainable Bayes factor is %4$s. Consider increasing the maximum sample size, lowering the required Bayes factor, or adjusting the prior.",
      priorParams$alpha, priorParams$beta, as.integer(max_n), .bsFormatBF(maxEvidence$log_bf)
    ))
    return()
  }

  if (isTRUE(options$showPlansplan)) {
    .bsPlanTable(container, plans, position = 1, base = "showPlansplan",
                 rql = rql, alpha = priorParams$alpha, beta = priorParams$beta)
  }

  if (isTRUE(options$showThreeBFplan)) {
    .bsThreeBFTable(container, plans, position = 3, base = "showThreeBFplan",
                    aql = aql, rql = rql, alpha = priorParams$alpha, beta = priorParams$beta)
  }
}

.bsBFEvidenceText <- function(log_bf, tolerance = 0) {
  if (.bsMeetsLogBF(log_bf, log(10), tolerance)) {
    gettext("Strong evidence that the proportion defective is below the RQL.")
  } else if (.bsMeetsLogBF(log_bf, log(3), tolerance)) {
    gettext("Moderate evidence that the proportion defective is below the RQL.")
  } else if (.bsMeetsLogBF(log_bf, log(1/3), tolerance)) {
    gettext("Inconclusive evidence about whether the proportion defective is below the RQL.")
  } else if (.bsMeetsLogBF(log_bf, log(1/10), tolerance)) {
    gettext("Moderate evidence that the proportion defective is at or above the RQL.")
  } else {
    gettext("Strong evidence that the proportion defective is at or above the RQL.")
  }
}

.bsBFTargetText <- function(log_bf, min_bf, tolerance = 0) {
  target <- format(min_bf, digits = 15, scientific = FALSE, trim = TRUE)
  text <- if (.bsMeetsLogBF(log_bf, log(min_bf), tolerance)) {
    gettextf("The selected Bayes-factor target of %1$s has been reached.", target)
  } else {
    gettextf("The selected Bayes-factor target of %1$s has not been reached.", target)
  }
  text <- paste(text, gettext("Evidence labels and the target comparison use the unrounded Bayes factor."))
  if (min_bf <= 1) {
    text <- paste(text, gettext("A target at or below 1 does not require evidence favoring a proportion defective below the RQL."))
  }
  text
}

# Section 2: Inference
.bsInferenceAnalysis <- function(jaspResults, options, position) {
  if (!isTRUE(options$inferPosteriorinfer))
    return()

  # Create/retrieve container + state
  container <- jaspResults[["infContainer"]] %setOrRetrieve% (
    .bsCreateStateContainer(gettext("Inference"), .bsInferenceDeps())
  )

  if (is.null(container))
    return()

  container <- .bsReuseOrRefreshContainer(container, gettext("Inference"), .bsInferenceDeps())

  # Always write container back, even if we return early later
  on.exit({
    jaspResults[["infContainer"]] <- container
  }, add = TRUE)

  # Always use prior from planning stage
  aql <- options$aqlplan
  rql <- options$rqlplan
  if (!is.finite(aql) || !is.finite(rql) || aql <= 0 || rql <= 0 || aql >= rql || rql >= 1) {
    .bsPriorError(container, gettext("Please ensure 0 < AQL < RQL < 1."), planning = FALSE)
    return()
  }

  priorParams <- tryCatch(
    .bsCalculatePriorParams(
      options$priorplan, aql, rql,
      options$alphaplan, options$betaplan,
      impartialCustomMode = options$impartialCustomModeplan,
      impartialMode = options$impartialModeplan,
      impartialThreeConstraint = options$impartialThreeConstraintplan,
      impartialShape = options$impartialShapeplan
    ),
    error = function(e) {
      .bsPriorError(container, conditionMessage(e), planning = FALSE)
      return(NULL)
    }
  )

  if (is.null(priorParams))
    return()

  if (!is.finite(priorParams$alpha) || !is.finite(priorParams$beta) ||
      priorParams$alpha <= 0 || priorParams$beta <= 0) {
    .bsPriorError(container, gettext("The selected prior results in invalid Beta parameters. Please adjust AQL/RQL or choose a different prior."), planning = FALSE)
    return()
  }

  container[["priorError"]] <- NULL

  # Data
  n <- options$data_ninfer
  d <- options$data_dinfer
  min_bf <- options$min_bfplan

  if (!is.finite(n) || !is.finite(d) || n < 0 || d < 0) {
    .bsInferenceError(container, gettext("Please ensure the data are valid: n >= 0 and 0 <= d <= n."))
    return()
  }

  if (d > n) {
    .bsInferenceError(container, gettext("The observed number of defectives (d) cannot exceed the sample size (n). Check the defect count and sample size."))
    return()
  }

  if (!is.finite(min_bf) || min_bf <= 0) {
    .bsInferenceError(container, gettext("Please provide a valid minimum Bayes factor (> 0)."))
    return()
  }

  # Posterior
  alpha_post <- priorParams$alpha + d
  beta_post  <- priorParams$beta + n - d

  # Outputs
  # Lot size for PPD
  N <- options$lot_sizeinfer
  if (!is.finite(N) || N < n) {
    .bsInferenceError(container, gettext("Lot size (N) must be at least as large as the sample size (n)."))
    return()
  }

  container[["inputError"]] <- NULL

  # Compute PPD region probabilities (Beta-Binomial)
  ppd <- .bsPPDRegionProbs(n, d, N, aql, rql, priorParams$alpha, priorParams$beta)
  numerical <- tryCatch(list(
    evidence = .bsLogBF(rql, n, d, priorParams$alpha, priorParams$beta),
    acceptanceNumber = .bsAcceptanceNumber(n, rql, min_bf, priorParams$alpha, priorParams$beta)
  ), error = function(e) {
    .bsInferenceError(container, conditionMessage(e), gettext("Bayes Factor"), "numericalError")
    NULL
  })
  if (is.null(numerical))
    return()
  container[["numericalError"]] <- NULL
  evidence <- numerical$evidence
  log_bf <- evidence$log_bf
  acceptanceNumber <- numerical$acceptanceNumber
  bfInterpretation <- .bsBFEvidenceText(log_bf, evidence$tolerance)
  targetInterpretation <- .bsBFTargetText(log_bf, min_bf, evidence$tolerance)

  if (isTRUE(options$showInferenceTableinfer)) {
    .bsInferenceDecisionTable(container, n, d, N, aql, rql,
                              log_bf, ppd, acceptanceNumber,
                              position = 0, base = "showInferenceTableinfer")

    # Evidence strength and the selected planning target are separate criteria.
    rql_count <- .bsCountLimit(rql, N)
    ppdGoodMiddle <- ppd$good + ppd$middle

    summaryText <- createJaspHtml(
      text = gettextf(
        "The <b>Bayes factor</b> in favor of the lot proportion defective being below the RQL of <b>%1$s</b> is approximately <b>%2$s</b>. %3$s %4$s<br>Based on the observed sample, there is a <b>%5$.1f%%</b> predicted probability that the current lot of %6$d items contains at most %7$d total defectives.",
        .bsFormatQuality(rql), .bsFormatBF(log_bf), bfInterpretation, targetInterpretation,
        ppdGoodMiddle * 100, as.integer(N), rql_count
      ),
      position = 0.5,
      dependencies = jaspDeps(.bsInferenceDeps())
    )
    container[["decisionSummaryText"]] <- summaryText
  } else {
    container[["inferenceDecisionTable"]] <- NULL
    container[["decisionSummaryText"]] <- NULL
  }

  if (isTRUE(options$posteriorPlotinfer)) {
    .bsDistributionPlot(container, aql, rql, alpha_post, beta_post,
                        "Posterior", position = 1, base = "posteriorPlotinfer",
                        alpha_prior = priorParams$alpha, beta_prior = priorParams$beta)

    bfText <- createJaspHtml(
      text = paste(
        gettextf(
          "<u>Bayes factor</u> in favor of proportion defective < <u>%1$s</u> is approximately <b>%2$s</b>.",
          .bsFormatQuality(rql), .bsFormatBF(log_bf)
        ),
        bfInterpretation, targetInterpretation
      ),
      position = 2,
      dependencies = jaspDeps(.bsInferenceDeps())
    )
    container[["bfText"]] <- bfText
  } else {
    container[["distPlotPosterior"]] <- NULL
    container[["bfText"]] <- NULL
  }

  if (isTRUE(options$ppdPlotinfer)) {
    .bsPPDPlot(container, n, d, N, aql, rql,
               alpha_post, beta_post,
               position = 3, base = "ppdPlotinfer")
  } else {
    container[["ppdPlot"]] <- NULL
  }

}


# Tables
.bsPlanTable <- function(container, plans, position, base = "showPlansplan",
                         rql, alpha, beta) {


  # Keep only the lowest n for each acceptance number c
  plans2 <- plans[order(plans$c, plans$n), , drop = FALSE]
  plans2 <- plans2[!duplicated(plans2$c), , drop = FALSE]

  plans2$prior_p <- exp(.bsLogBetaTails(rql, alpha, beta)$lower)
  plans2$post_p <- exp(.bsLogBetaTails(rql, alpha + plans2$c, beta + plans2$n - plans2$c)$lower)

  table <- createJaspTable(
    title = gettext("Sampling Plans"),
    position = position,
    dependencies = jaspDeps(base)
  )

  table$addColumnInfo(name = "c",       title = gettext("Acceptance number (c)"),  type = "integer")
  table$addColumnInfo(name = "n",       title = gettext("Lowest sample size (n)"), type = "integer")
  table$addColumnInfo(name = "prior_p", title = gettext("Prior P(θ < RQL)"),       type = "number")
  table$addColumnInfo(name = "post_p",  title = gettext("Posterior P(θ < RQL)"),   type = "number")
  plans2$bf_odds <- .bsBFColumn(table, "bf_odds", gettext("Bayes factor"), plans2$log_bf)

  table$setData(plans2[c("c", "n", "prior_p", "post_p", "bf_odds")])

  table$addFootnote(gettextf("Prior distribution: Beta(%1$.3f, %2$.3f).", alpha, beta))

  table$addFootnote(gettext("Bayes factors are calculated from log probabilities; displayed probabilities may round to 0 or 1."))
  container[["planTable"]] <- table
}


.bsThreeBFTable <- function(container, plans, position, base = "showThreeBFplan",
                            aql, rql, alpha, beta) {

  needed <- c("n", "c", "logBF_GB", "logBF_GM", "logBF_MB")
  if (!all(needed %in% names(plans))) return()

  # Keep only the lowest n for each acceptance number c
  plans2 <- plans[order(plans$c, plans$n), , drop = FALSE]
  plans2 <- plans2[!duplicated(plans2$c), , drop = FALSE]

  # Prior region probs (constant)
  prior <- .bsThreeRegionProbs(aql, rql, alpha, beta)
  plans2$prior_good   <- unname(prior["good"])
  plans2$prior_middle <- unname(prior["middle"])
  plans2$prior_bad    <- unname(prior["bad"])

  # Posterior region probs at d = c (row-wise)
  post_matrix <- t(mapply(
    FUN = function(a_post, b_post) .bsThreeRegionProbs(aql, rql, a_post, b_post),
    a_post = alpha + plans2$c,
    b_post = beta + plans2$n - plans2$c
  ))

  plans2$post_good   <- unname(post_matrix[, "good"])
  plans2$post_middle <- unname(post_matrix[, "middle"])
  plans2$post_bad    <- unname(post_matrix[, "bad"])

  table <- createJaspTable(
    title = gettext("Three-Hypothesis Bayes Factors (at d = c)"),
    position = position,
    dependencies = jaspDeps(base)
  )

  table$addColumnInfo(name = "c", title = gettext("Acceptance number (c)"),  type = "integer")
  table$addColumnInfo(name = "n", title = gettext("Lowest sample size (n)"), type = "integer")

  table$addColumnInfo(name = "prior_good",   title = gettext("Prior P(Good)"),   type = "number")
  table$addColumnInfo(name = "post_good",    title = gettext("Post P(Good)"),    type = "number")
  table$addColumnInfo(name = "prior_middle", title = gettext("Prior P(Middle)"), type = "number")
  table$addColumnInfo(name = "post_middle",  title = gettext("Post P(Middle)"),  type = "number")
  table$addColumnInfo(name = "prior_bad",    title = gettext("Prior P(Bad)"),    type = "number")
  table$addColumnInfo(name = "post_bad",     title = gettext("Post P(Bad)"),     type = "number")

  plans2$BF_GB <- .bsBFColumn(table, "BF_GB", gettext("BF (Good vs Bad)"), plans2$logBF_GB)
  plans2$BF_GM <- .bsBFColumn(table, "BF_GM", gettext("BF (Good vs Middle)"), plans2$logBF_GM)
  plans2$BF_MB <- .bsBFColumn(table, "BF_MB", gettext("BF (Middle vs Bad)"), plans2$logBF_MB)

  table$setData(plans2[c(
    "c", "n",
    "prior_good", "post_good",
    "prior_middle", "post_middle",
    "prior_bad", "post_bad",
    "BF_GB", "BF_GM", "BF_MB"
  )])

  table$addFootnote(gettext("Bayes factors are calculated from log probabilities; displayed probabilities may round to 0 or 1."))
  container[["threeBFTable"]] <- table
}


# Plots
.bsBetaDensityGrid <- function(alpha, beta) {
  # Combine full-support coverage with probability-spaced points and the exact
  # interior mode. Each curve gets its own grid, including a narrow posterior
  # over a broad prior. Log-spaced edge points resolve both tails symmetrically.
  edge <- 10^seq(-12, -3, length.out = 91L)
  probabilities <- sort(unique(c(edge, seq(.001, .999, length.out = 501L), 1 - edge)))
  mode <- if (alpha > 1 && beta > 1) (alpha - 1) / (alpha + beta - 2) else numeric(0)
  x <- sort(unique(c(seq(0, 1, length.out = 1001L), edge, 1 - edge,
                     stats::qbeta(probabilities, alpha, beta), mode)))
  density <- stats::dbeta(x, alpha, beta)

  # An unbounded endpoint cannot determine a finite automatic y-axis. Retain
  # the former 1e-6 reference distance only for that axis choice, not as a cut
  # in the curve: all tail points and endpoint infinities remain in the data.
  reference <- x >= (if (alpha < 1) 1e-6 else 0) &
    x <= (if (beta < 1) 1 - 1e-6 else 1)
  list(x = x, density = density, scale = density[reference & is.finite(density)])
}

.bsKeepDensityTails <- function(x, range) {
  # Huge finite coordinates can stall graphics devices. Bound only drawing
  # coordinates outside the panel (not at its edge); keep the raw curve data
  # and endpoint infinities, and retain every value in the displayed range.
  span <- diff(range)
  finite <- is.finite(x)
  x[finite] <- pmin(pmax(x[finite], range[1] - span), range[2] + span)
  x
}

.bsDistributionPlot <- function(container, aql, rql, alpha, beta, type,
                                position, base, alpha_prior = NULL, beta_prior = NULL) {
  plotName <- paste0("distPlot", type)
  hasPriorOverlay <- identical(type, "Posterior") && !is.null(alpha_prior) && !is.null(beta_prior)

  plot <- createJaspPlot(
    title = if (type == "Posterior") gettext("Posterior Distribution") else gettext("Prior Distribution"),
    width = 500,
    height = 400,
    position = position,
    dependencies = jaspDeps(base)
  )

  postGrid <- .bsBetaDensityGrid(alpha, beta)
  df_post <- data.frame(x = postGrid$x, dens = postGrid$density, dist = type)
  scaleDensities <- postGrid$scale

  df <- df_post
  if (hasPriorOverlay) {
    priorGrid <- .bsBetaDensityGrid(alpha_prior, beta_prior)
    df_prior <- data.frame(x = priorGrid$x, dens = priorGrid$density, dist = "Prior")
    scaleDensities <- c(scaleDensities, priorGrid$scale)
    df <- rbind(df_prior, df_post)
    df$dist <- factor(df$dist, levels = c("Posterior", "Prior"))
  } else {
    df$dist <- factor(df$dist)
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  yScale <- .bsYScale(scaleDensities)
  aqlLabelX <- .bsThresholdLabelX(aql, "left", 0, 1, min_offset = 0.015)
  rqlLabelX <- .bsThresholdLabelX(rql, "right", 0, 1, min_offset = 0.015)

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = aql, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = rql, linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = aqlLabelX, y = yScale$label_y, label = "AQL", vjust = 1, hjust = 1, size = 3) +
    ggplot2::annotate("text", x = rqlLabelX, y = yScale$label_y, label = "RQL", vjust = 1, hjust = 0, size = 3) +
    ggplot2::geom_area(
      data = df_post,
      ggplot2::aes(x = .data$x, y = .data$dens),
      fill = "#b3b3b3",
      alpha = 0.8,
      stat = "identity",
      position = "identity"
    ) +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$dens, linetype = .data$dist),
      color = "black",
      linewidth = 0.9,
      show.legend = hasPriorOverlay
    ) +
    ggplot2::scale_linetype_manual(values = if (hasPriorOverlay) c(Posterior = "solid", Prior = "dashed") else setNames("solid", type)) +
    ggplot2::labs(
      x = gettext("Lot Proportion Defective"),
      y = gettext("Density"),
      linetype = NULL
    ) +
    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (hasPriorOverlay) c(0.98, 0.98) else "none") +
    ggplot2::theme(
      legend.justification = c(1, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    # Keep tail values when limiting the display, so they are clipped at the
    # panel boundary rather than deleted or turned into an artificial plateau.
    ggplot2::scale_y_continuous(breaks = yScale$breaks, limits = yScale$limits,
                              oob = .bsKeepDensityTails)

  plot$plotObject <- p
  container[[plotName]] <- plot
}

# Posterior Predictive Distribution Plot
.bsPPDPlot <- function(container, n, d, N, aql, rql,
                       alpha_post, beta_post,
                       position, base) {

  plot <- createJaspPlot(
    title = gettext("Posterior Predictive Distribution"),
    width = 500,
    height = 400,
    position = position,
    dependencies = jaspDeps(base)
  )

  ppd_df <- .bsPPDFull(n, d, N, alpha_post, beta_post)

  aql_count <- .bsCountLimit(aql, N)
  rql_count <- .bsCountLimit(rql, N)

  ppd_df$region <- ifelse(ppd_df$d_tot <= aql_count, "Good",
                   ifelse(ppd_df$d_tot <= rql_count, "Middle", "Bad"))
  ppd_df$region <- factor(ppd_df$region, levels = c("Good", "Middle", "Bad"))

  xScale <- .bsPPDXScale(ppd_df$d_tot)
  xBreaks <- xScale$breaks
  yScale <- .bsYScale(ppd_df$pmf)
  visibleThresholds <- do.call(
    rbind,
    Filter(Negate(is.null), list(
      .bsVisibleThreshold(aql_count + 0.5, "AQL", "left", xScale$limits[1], xScale$limits[2], min_offset = 0.75),
      .bsVisibleThreshold(rql_count + 0.5, "RQL", "right", xScale$limits[1], xScale$limits[2], min_offset = 0.75)
    ))
  )
  if (is.null(visibleThresholds))
    visibleThresholds <- data.frame(x = numeric(0), label = character(0), label_x = numeric(0), hjust = numeric(0))

  p <- ggplot2::ggplot(ppd_df, ggplot2::aes(x = .data$d_tot, y = .data$pmf, fill = .data$region)) +
    ggplot2::geom_col(width = 0.8, color = "black", linewidth = 0.2) +
    ggplot2::geom_vline(
      data = visibleThresholds,
      ggplot2::aes(xintercept = .data$x),
      linetype = "dashed",
      linewidth = 0.6,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = visibleThresholds,
      ggplot2::aes(x = .data$label_x, y = yScale$label_y, label = .data$label, hjust = .data$hjust),
      vjust = 1,
      size = 3,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(Good = "grey75", Middle = "grey55", Bad = "grey35"),
      guide = "none"
    ) +
    ggplot2::labs(
      x = gettext("Total Defectives in Lot"),
      y = gettext("Probability")
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xScale$limits) +
    ggplot2::scale_y_continuous(breaks = yScale$breaks, limits = yScale$limits)

  plot$plotObject <- p
  container[["ppdPlot"]] <- plot
}

.bsInferenceDecisionTable <- function(container, n, d, N, aql, rql,
                                       log_bf, ppd, acceptanceNumber,
                                       position, base) {
  
  table <- createJaspTable(
    title = gettext("Inference Decision Table"),
    position = position,
    dependencies = jaspDeps(base)
  )
  
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = gettext("Value"), type = "string")
  
  # PPD region labels with count thresholds
  aql_count <- .bsCountLimit(aql, N)
  rql_count <- .bsCountLimit(rql, N)
  
  ppd_good_label   <- gettextf("PPD Good (\u2264 %1$d defectives in Lot)", aql_count)
  ppd_middle_label  <- gettextf("PPD Middle (> %1$d & \u2264 %2$d defectives)", aql_count, rql_count)
  ppd_bad_label     <- gettextf("PPD Bad (> %1$d defectives in Lot)", rql_count)
  
  table[["col_1"]] <- c(
    gettext("Sample Size (n)"),
    gettext("Defectives (d)"),
    gettext("Acceptance number (c)"),
    gettext("AQL"),
    gettext("RQL"),
    gettext("Bayes Factor"),
    ppd_good_label,
    ppd_middle_label,
    ppd_bad_label
  )
  
  table[["col_2"]] <- c(
    as.character(n),
    as.character(d),
    if (is.na(acceptanceNumber)) gettext("Not available") else as.character(acceptanceNumber),
    .bsFormatQuality(aql),
    .bsFormatQuality(rql),
    .bsFormatBF(log_bf, digits = 3L),
    sprintf("%.3f", ppd$good),
    sprintf("%.3f", ppd$middle),
    sprintf("%.3f", ppd$bad)
  )
  
  table$addFootnote(gettext("Bayes factor interpretation: H\u2212 denotes a proportion defective below the RQL; H+ denotes a proportion defective at or above the RQL. BF \u2265 10 (strong evidence for H\u2212), 3 \u2264 BF < 10 (moderate evidence for H\u2212), 1/3 \u2264 BF < 3 (inconclusive), 0.10 \u2264 BF < 1/3 (moderate evidence for H+), BF < 0.10 (strong evidence for H+). Evidence labels use the unrounded Bayes factor and are separate from whether the selected planning target is reached."))
  
  container[["inferenceDecisionTable"]] <- table
}

.bsProcedure <- function(jaspResults, options, position) {

  # Retrieve values for dynamic text
  aql <- options$aqlplan * 100
  rql <- options$rqlplan * 100

  container <- jaspResults[["procedureContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Procedure"))

  introText <- gettextf(
    "<div style='line-height: 1.2;'>
    <p style='margin: 0 0 8px 0;'>The <b>main objective</b> of this Bayesian sampling procedure is to evaluate the quality of a lot by determining if the proportion defective is lower than the <b>Rejectable Quality Level (RQL)</b>, which is currently set at <b>%1$s%%</b>.</p>

    <p style='margin: 0 0 4px 0;'>Two primary statistical hypotheses are formulated about the lot proportion defective (&theta;):</p>
    <ul style='margin: 0 0 8px 20px;'>
      <li>The (null) hypothesis of intolerable quality H<sub>+</sub>: &theta; &ge; %1$s%% (user-defined)</li>
      <li>The (alternative) hypothesis of tolerable quality H<sub>-</sub>: &theta; < %1$s%% (user-defined)</li>
    </ul>

    <p style='margin: 0 0 8px 0;'>Additionally, users have the option to perform a <b>Three-Hypothesis Test</b>, which partitions the quality levels into 'Good' (&le; AQL), 'Middle' (AQL < &theta; &le; RQL), and 'Bad' (> RQL) regions.</p>

    <p style='margin: 0 0 8px 0;'>The <b>Acceptable Quality Level (AQL)</b> is currently set at <b>%2$s%%</b>. This value represents the
    quality level aimed for acceptance with high probability. These quality constraints can be adjusted in the Planning phase to reflect specific requirements.</p>

    <p style='margin: 0 0 6px 0;'><b>Stages</b></p>
    <p style='margin: 0 0 6px 0;'><b>1. Planning:</b> Determine a minimum sample size such that the sample provides sufficient evidence (a target Bayes factor) to achieve the specified sampling objectives.</p>

    <p style='margin: 0;'><b>2. Inference:</b> Evaluate observed data to update the prior belief into
    a posterior distribution. In this section, <b>n</b> denotes the sample size, <b>d</b> the observed number of defectives in the sample, and <b>N</b> the lot size.</p></div>",
    .bsFormatQuality(rql), .bsFormatQuality(aql)
  )

  container[["intro"]] <- createJaspHtml(text = introText, position = 1)

  jaspResults[["procedureContainer"]] <- container
}
