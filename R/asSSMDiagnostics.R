.asSSM_computeDiagnostics <- function(draws, samplerParams, maxTreedepth, thin = 1L, expectedChains = dim(draws)[2L]) {
  if (length(dim(draws)) != 3L)
    stop("Expected iterations by chains by parameters.")
  n <- dim(draws)[1L]
  chains <- dim(draws)[2L]
  names <- dimnames(draws)[[3L]]
  sampled <- grep("^(mu1|sigma_eta|beta_predictor(\\[[0-9]+\\])?|z_raw\\[[0-9]+\\])$", names)
  if (!length(sampled)) stop("No sampled parameters found for diagnostics.")

  parameters <- data.frame(parameter = names[sampled], rhat = NA_real_,
                           ess_bulk = NA_real_, ess_tail = NA_real_,
                           constant_chain = FALSE, nonfinite_draws = FALSE)
  for (i in seq_along(sampled)) {
    x <- matrix(draws[, , sampled[i]], nrow = n, ncol = chains)
    parameters$nonfinite_draws[i] <- any(!is.finite(x))
    if (n < 6L || parameters$nonfinite_draws[i]) next
    parameters$constant_chain[i] <- any(vapply(seq_len(chains), function(j)
      all(x[, j] == x[1L, j]), logical(1)))
    if (parameters$constant_chain[i]) next
    if (chains > 1L) parameters$rhat[i] <- rstan::Rhat(x)
    parameters$ess_bulk[i] <- rstan::ess_bulk(x)
    parameters$ess_tail[i] <- rstan::ess_tail(x)
  }

  sampler <- data.frame(chain = seq_len(chains), draws = NA_integer_,
                        divergences = NA_integer_, treedepth_hits = NA_integer_,
                        bfmi = NA_real_)
  for (j in seq_len(chains)) {
    if (j > length(samplerParams)) next
    x <- samplerParams[[j]]
    if (!is.matrix(x) || nrow(x) != n || n == 0L) next
    sampler$draws[j] <- nrow(x)
    if ("divergent__" %in% colnames(x) && all(is.finite(x[, "divergent__"])))
      sampler$divergences[j] <- sum(x[, "divergent__"] != 0)
    if ("treedepth__" %in% colnames(x) && all(is.finite(x[, "treedepth__"])))
      sampler$treedepth_hits[j] <- sum(x[, "treedepth__"] >= maxTreedepth)
    if (thin == 1L && n >= 6L && "energy__" %in% colnames(x)) {
      energy <- x[, "energy__"]
      if (all(is.finite(energy)) && stats::var(energy) > 0)
        sampler$bfmi[j] <- mean(diff(energy)^2) / stats::var(energy)
    }
  }

  list(chains = chains, expected_chains = expectedChains, draws_per_chain = n, thin = thin,
       max_treedepth = maxTreedepth, parameters = parameters,
       sampler = sampler, sampler_params = samplerParams)
}

.asSSM_extractDiagnostics <- function(stanFit, lots, predictor, maxTreedepth, thin, expectedChains) {
  pars <- c("mu1", "sigma_eta", if (lots > 1L) "z_raw",
            if (predictor) "beta_predictor")
  samplerParams <- rstan::get_sampler_params(stanFit, inc_warmup = FALSE)
  tryCatch({
    draws <- rstan::extract(stanFit, pars = pars, permuted = FALSE, inc_warmup = FALSE)
    .asSSM_computeDiagnostics(draws, samplerParams, maxTreedepth, thin, expectedChains)
  }, error = function(e) list(error = conditionMessage(e), sampler_params = samplerParams))
}

.asSSM_diagnosticMessages <- function(diagnostics) {
  if (is.null(diagnostics))
    return(gettext("Computational diagnostics are unavailable for this cached fit. Refit the analysis before relying on its decisions."))
  if (!is.null(diagnostics$error))
    return(gettext("Computational diagnostics could not be calculated. The posterior draws are retained, but computational reliability has not been assessed. Refit the analysis and inspect the sampler output."))

  d <- diagnostics
  p <- d$parameters
  s <- d$sampler
  messages <- character()
  if (d$chains != d$expected_chains)
    messages <- c(messages, gettextf("Only %1$d of %2$d requested chains are available. Inspect the sampler output for failed chains and refit before relying on the results.", d$chains, d$expected_chains))
  if (d$thin > 1L)
    messages <- c(messages, gettext("Thinning is enabled. Divergence and treedepth counts cover only retained transitions, so other events may be missed; E-BFMI is unavailable. Rerun with Thin = 1 for complete sampler checks."))
  if (anyNA(s$divergences) || anyNA(s$treedepth_hits))
    messages <- c(messages, gettext("Sampler diagnostics are unavailable for one or more chains. Missing checks are not evidence of successful sampling; refit and inspect the sampler output."))
  if (any(s$divergences > 0, na.rm = TRUE))
    messages <- c(messages, gettextf("Detected %1$d divergent transitions in the available retained post-warmup records. Estimates may be biased. Review the model, priors and sampler settings before relying on the results; increasing Adapt Delta may help. No divergent draws were removed.", sum(s$divergences, na.rm = TRUE)))
  if (any(s$treedepth_hits > 0, na.rm = TRUE))
    messages <- c(messages, gettextf("The maximum treedepth was reached in %1$d retained post-warmup iterations. This can reduce sampling efficiency. Inspect mixing and the model before considering a higher treedepth limit.", sum(s$treedepth_hits, na.rm = TRUE)))
  if (d$chains < 2L)
    messages <- c(messages, gettext("Only one chain was run, so between-chain mixing cannot be assessed and automatic R-hat is unavailable. Run multiple chains, preferably at least four."))
  if (d$draws_per_chain < 6L)
    messages <- c(messages, gettext("Fewer than six post-warmup draws per chain are available. R-hat, effective sample sizes and E-BFMI cannot be assessed reliably. Increase the number of retained samples."))
  if (any(p$constant_chain))
    messages <- c(messages, gettextf("At least one chain is constant for %1$d sampled parameter(s), including %2$s. Mixing and effective sample sizes are unavailable for these parameters. Inspect the chains and refit before relying on the results.", sum(p$constant_chain), p$parameter[which(p$constant_chain)[1L]]))
  if (any(p$nonfinite_draws))
    messages <- c(messages, gettext("Non-finite draws occur in sampled parameters. Their mixing and effective sample sizes are unavailable; investigate the model and sampler output before using the results."))

  if (d$chains > 1L && d$draws_per_chain >= 6L) {
    if (any(p$rhat >= 1.01, na.rm = TRUE)) {
      i <- which.max(p$rhat)
      messages <- c(messages, gettextf("The maximum rank-normalized split R-hat is %1$.3f (%2$s), at or above 1.01. The chains may not have mixed. Inspect their traces, run longer chains, and review the model if the problem persists.", p$rhat[i], p$parameter[i]))
    }
    if (any(!is.finite(p$rhat)) && !any(p$constant_chain | p$nonfinite_draws))
      messages <- c(messages, gettext("R-hat is unavailable for one or more sampled parameters. Inspect the chains and refit; this is not a successful mixing check."))
  }
  if (d$draws_per_chain >= 6L) {
    threshold <- 100 * d$chains
    if (any(p$ess_bulk < threshold | p$ess_tail < threshold, na.rm = TRUE))
      messages <- c(messages, gettextf("Bulk or tail effective sample size is below %1$d (100 times the number of chains) for one or more sampled parameters. Monte Carlo estimates may be imprecise. If the chains mix well, increase the number of retained samples; otherwise investigate mixing first.", threshold))
    if (any(!is.finite(p$ess_bulk) | !is.finite(p$ess_tail)) && !any(p$constant_chain | p$nonfinite_draws))
      messages <- c(messages, gettext("Bulk or tail effective sample size is unavailable for one or more sampled parameters. Inspect the chains and obtain more usable draws before relying on the results."))
  }
  if (d$thin == 1L && d$draws_per_chain >= 6L) {
    if (any(s$bfmi < 0.3, na.rm = TRUE))
      messages <- c(messages, gettextf("E-BFMI is below 0.3 in chain(s) %1$s, indicating poor exploration of the energy distribution. Review the model and priors and consider longer warmup before relying on the results.", paste(s$chain[which(s$bfmi < 0.3)], collapse = ", ")))
    if (any(!is.finite(s$bfmi)))
      messages <- c(messages, gettext("E-BFMI is unavailable for one or more chains because their energy diagnostics are missing, non-finite or constant. Refit with usable energy diagnostics; missing values are not a successful check."))
  }
  messages
}

.asSSM_diagnosticHtml <- function(diagnostics, details = TRUE) {
  messages <- .asSSM_diagnosticMessages(diagnostics)
  text <- if (length(messages)) paste0("<p><b>", gettext("Sampling requires attention."),
    "</b></p><ul><li>", paste(messages, collapse = "</li><li>"), "</li></ul>") else
    paste0("<p>", gettext("No diagnostic thresholds were exceeded. These checks do not prove convergence."), "</p>")
  if (!details || is.null(diagnostics) || !is.null(diagnostics$error)) return(text)

  d <- diagnostics
  p <- d$parameters
  s <- d$sampler
  number <- function(x, digits = 3L) {
    if (length(x) != 1L || !is.finite(x)) return(gettext("Unavailable"))
    formatC(x, digits = digits, format = "f")
  }
  extreme <- function(x, fun) {
    if (length(x) == 0L || any(!is.finite(x))) return(NA_real_)
    fun(x)
  }
  counts <- function(x) if (anyNA(x)) gettext("Unavailable") else as.character(sum(x))
  text <- paste0(text, "<p>", gettextf("Retained post-warmup draws: %1$d per chain across %2$d chain(s). All retained draws are used, including those associated with divergences.", d$draws_per_chain, d$chains), "</p>")
  text <- paste0(text, "<p>", gettextf("Divergences: %1$s; maximum treedepth hits: %2$s (limit %3$d).", counts(s$divergences), counts(s$treedepth_hits), d$max_treedepth), "</p>")
  text <- paste0(text, "<p>", gettextf("Across all sampled parameters: maximum rank-normalized split R-hat = %1$s; minimum bulk ESS = %2$s; minimum tail ESS = %3$s.", number(extreme(p$rhat, max)), number(extreme(p$ess_bulk, min), 0L), number(extreme(p$ess_tail, min), 0L)), "</p>")
  bfmi <- vapply(seq_len(nrow(s)), function(i)
    gettextf("chain %1$d: %2$s", s$chain[i], number(s$bfmi[i])), character(1))
  paste0(text, "<p>", gettextf("E-BFMI (%1$s).", paste(bfmi, collapse = "; ")), "</p>")
}

.asSSM_reportDiagnostics <- function(jaspResults, options) {
  jaspResults[["samplerDiagnostics"]] <- NULL
  fit <- jaspResults[["fit"]]
  if (is.null(fit) || is.null(fit$object)) return()

  printDiagnostics <- isTRUE(options[["ssm_printDiagnostics"]])
  diagnostics <- fit$object$diagnostics
  if (!printDiagnostics && !length(.asSSM_diagnosticMessages(diagnostics))) return()

  jaspResults[["samplerDiagnostics"]] <- createJaspHtml(
    text = .asSSM_diagnosticHtml(diagnostics, details = printDiagnostics), elementType = "div",
    title = if (printDiagnostics) gettext("Computational Diagnostics") else gettext("Sampling Warnings"),
    position = if (printDiagnostics) 4.5 else -0.5,
    dependencies = c(.asSSM_fitDependencies(), "ssm_printDiagnostics"))
}
