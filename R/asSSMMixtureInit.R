.asSSM_createMixtureInitialValues <- function(dat, chains, predictor = FALSE) {
  rate <- (dat$y + 0.5) / (dat$n + 1)
  firstLots <- seq_len(min(dat$T, 3))
  firstRate <- (sum(dat$y[firstLots]) + 1) / (sum(dat$n[firstLots]) + 2)

  lapply(seq_len(chains), function(chain) {
    i <- (chain - 1L) %% 4L + 1L
    sigma <- c(0.08, 0.2, 0.5, 1)[i]
    theta1 <- min(0.99, max(0.001, firstRate * c(0.8, 1, 1.2, 1)[i]))
    mu <- stats::qlogis(theta1) + c(0, 0.35, 0.7, 1)[i] * (stats::qlogis(rate) - stats::qlogis(rate[1]))
    eta <- diff(mu)

    regular <- log(0.95) + stats::pnorm(abs(eta) / sigma, lower.tail = FALSE, log.p = TRUE)
    broad <- log(0.05) + stats::pnorm(abs(eta) / sqrt(sigma^2 + 1.5^2),
                              lower.tail = FALSE, log.p = TRUE)
    logTail <- pmax(regular, broad) + log1p(exp(pmin(regular, broad) - pmax(regular, broad)))
    standardizedInnovations <- sign(eta) * stats::qnorm(logTail, lower.tail = FALSE, log.p = TRUE)

    initial <- list(
      mu1 = stats::qlogis(theta1),
      sigma_eta = sigma,
      z_raw = array(as.numeric(standardizedInnovations), dim = length(standardizedInnovations))
    )
    if (predictor) initial$beta_predictor <- array(rep(0, dat$K), dim = dat$K)
    initial
  })
}
