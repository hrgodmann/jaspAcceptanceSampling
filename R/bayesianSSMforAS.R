
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BayesianSSMforAS <- function(jaspResults, dataset = NULL, options, ...) {
  bayesianSSMforASInternal(jaspResults, dataset, options)
}

# Backward-compatible alias.
bayesianSSMforAS <- BayesianSSMforAS

bayesianSSMforASInternal <- function(jaspResults, dataset = NULL, options) {

  # Clear the obsolete correction note from previously calculated results.
  jaspResults[["predPlotNote"]] <- NULL
  jaspResults[["samplerDiagnostics"]] <- NULL
  jaspResults[["modelChangeNote"]] <- NULL

  dataset <- .asSSM_readData(dataset, options)

  ready <- .asSSMReady(options, dataset)

  jaspResults[["placeholder"]] <- NULL

  if (!ready) {
    jaspResults[["fit"]] <- NULL
    .asSSM_clearOutputs(jaspResults)
    .asSSM_createSummaryTable(jaspResults, dataset, options)
    return()
  }

  .asSSM_fitModel(jaspResults, dataset, options)

  if (!is.null(jaspResults[["placeholder"]]) &&
      jaspResults[["placeholder"]]$getError())
    return()

  T_idx <- nrow(dataset)
  predictiveError <- tryCatch({
    .asSSM_predictivePMF(jaspResults[["fit"]]$object$samples$y_pred,
      dataset[[options[["ssm_total"]]]][T_idx],
      dataset[[options[["ssm_sampleSize"]]]][T_idx],
      dataset[[options[["ssm_count"]]]][T_idx])
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(predictiveError)) {
    .asSSM_setAnalysisError(jaspResults, predictiveError)
    return()
  }
  .asSSM_reportDiagnostics(jaspResults, options)

  .asSSM_createSummaryTable(jaspResults, dataset, options)
  .asSSM_createPredictorSummaryTable(jaspResults, options)
  .asSSM_createMcmcSummaryTable(jaspResults, dataset, options)

  if (isTRUE(options[["ssm_statePlot"]]))
    .asSSM_plotState(jaspResults, dataset, options)
  else
    jaspResults[["ssm_statePlot"]] <- NULL

  if (isTRUE(options[["ssm_posteriorDistPlot"]]))
    .asSSM_plotPosteriorDist(jaspResults, dataset, options)
  else
    jaspResults[["postDist"]] <- NULL

  if (isTRUE(options[["ssm_predictionPlot"]]))
    .asSSM_plotPrediction(jaspResults, dataset, options)
  else
    jaspResults[["predPlot"]] <- NULL

  if (length(.asSSM_predictorNames(options)) && isTRUE(options[["ssm_plotBeta"]]))
    .asSSM_plotBetaCoefficient(jaspResults, dataset, options)
  else
    jaspResults[["betaPlot"]] <- NULL

  jaspResults[["placeholder"]] <- NULL
}

.asSSM_clearOutputs <- function(jaspResults) {
  for (key in c("summaryTable", "predictorSummary", "mcmcSummary", "ssm_statePlot", "predPlot",
                "postDist", "betaPlot", "samplerDiagnostics"))
    jaspResults[[key]] <- NULL
}

.asSSM_setAnalysisError <- function(jaspResults, message) {
  .asSSM_clearOutputs(jaspResults)
  errorTable <- createJaspTable(title = gettext("Analysis Error"))
  errorTable$position <- 0
  jaspResults[["placeholder"]] <- errorTable
  errorTable$setError(message)
}

.asSSMReady <- function(options, dataset) {
  if (options[["ssm_count"]] == "" || options[["ssm_total"]] == "" ||
      options[["ssm_sampleSize"]] == "" || options[["ssm_time"]] == "") return(FALSE)
  return(TRUE)
}

.asSSM_readData <- function(dataset, options) {

  if (options[["ssm_count"]] == "" ||
      options[["ssm_total"]] == "" ||
      options[["ssm_sampleSize"]] == "" ||
      options[["ssm_time"]] == "") {
    return(dataset)
  }

  colsNumeric <- c()
  if (options[["ssm_count"]]      != "") colsNumeric <- c(colsNumeric, options[["ssm_count"]])
  if (options[["ssm_total"]]      != "") colsNumeric <- c(colsNumeric, options[["ssm_total"]])
  if (options[["ssm_sampleSize"]] != "") colsNumeric <- c(colsNumeric, options[["ssm_sampleSize"]])
  if (options[["ssm_time"]]       != "") colsNumeric <- c(colsNumeric, options[["ssm_time"]])
  colsNumeric <- unique(c(colsNumeric, .asSSM_predictorNames(options)))

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = colsNumeric)
  }

  if (!is.null(dataset) && options[["ssm_time"]] != "" && options[["ssm_time"]] %in% colnames(dataset)) {
    lot <- dataset[[options[["ssm_time"]]]]
    if (is.ordered(lot)) {
      # Preloaded Ordinal columns are ordered factors: preserve labels, not level codes.
      numericLot <- suppressWarnings(as.numeric(as.character(lot)))
      if (all(is.na(lot) | !is.na(numericLot)))
        dataset[[options[["ssm_time"]]]] <- numericLot
    }
    dataset <- dataset[order(dataset[[options[["ssm_time"]]]]), , drop = FALSE]
  }

  return(dataset)
}

.asSSM_isWholeNumber <- function(x) {
  x == floor(x)
}

.asSSM_validateInputs <- function(dataset, options) {
  if (is.null(dataset) || nrow(dataset) == 0L) {
    return(gettext("The dataset is empty."))
  }

  predictors <- .asSSM_predictorNames(options)
  usePredictor <- length(predictors) > 0L
  requiredCols <- c(
    options[["ssm_count"]],
    options[["ssm_sampleSize"]],
    options[["ssm_total"]],
    options[["ssm_time"]]
  )
  if (usePredictor)
    requiredCols <- c(requiredCols, predictors)
  requiredCols <- unique(requiredCols[nzchar(requiredCols)])

  missingCols <- setdiff(requiredCols, names(dataset))
  if (length(missingCols) > 0L) {
    return(gettextf(
      "The following assigned variables are missing from the dataset: %1$s.",
      paste(missingCols, collapse = ", ")
    ))
  }

  count <- dataset[[options[["ssm_count"]]]]
  sampleSize <- dataset[[options[["ssm_sampleSize"]]]]
  lotSize <- dataset[[options[["ssm_total"]]]]
  time <- dataset[[options[["ssm_time"]]]]

  validateNumeric <- function(x, label, mustBeWhole = FALSE, mustBeNonNegative = FALSE) {
    if (!is.numeric(x) || is.complex(x)) {
      return(gettextf("%1$s must contain numeric values.", label))
    }
    if (any(!is.finite(x))) {
      return(gettextf("%1$s cannot contain missing or non-finite values.", label))
    }
    if (mustBeWhole && any(!.asSSM_isWholeNumber(x))) {
      return(gettextf("%1$s must contain whole numbers only.", label))
    }
    if (mustBeNonNegative && any(x < 0)) {
      return(gettextf("%1$s cannot contain negative values.", label))
    }
    if (mustBeWhole && any(x >= .Machine$integer.max)) {
      return(gettextf("%1$s must be smaller than %2$d for this analysis.", label, .Machine$integer.max))
    }
    NULL
  }

  msg <- validateNumeric(count, gettext("Defect Count"), mustBeWhole = TRUE, mustBeNonNegative = TRUE)
  if (!is.null(msg)) return(msg)

  msg <- validateNumeric(sampleSize, gettext("Sample Size"), mustBeWhole = TRUE, mustBeNonNegative = TRUE)
  if (!is.null(msg)) return(msg)

  msg <- validateNumeric(lotSize, gettext("Lot Size"), mustBeWhole = TRUE, mustBeNonNegative = TRUE)
  if (!is.null(msg)) return(msg)

  msg <- validateNumeric(time, gettext("Lot Number"))
  if (!is.null(msg)) return(msg)
  if (anyDuplicated(time)) {
    return(gettext("Each lot number must occur only once. When inspecting more items from an existing lot, update that lot's sample size and defect count instead of adding another row."))
  }

  if (usePredictor) {
    if (anyDuplicated(predictors))
      return(gettext("Assign each covariate only once."))
    if (nrow(dataset) < 2L) {
      return(gettext("At least two lots with varying predictor values are required when using the predictor model."))
    }
    for (name in predictors) {
      predictor <- dataset[[name]]
      label <- gettextf("Covariate %s", jaspBase::decodeColNames(name))
      msg <- validateNumeric(predictor, label)
      if (!is.null(msg)) return(msg)
      if (length(unique(predictor)) < 2L)
        return(gettextf("Covariate %s must vary across lots.", jaspBase::decodeColNames(name)))
      predictorSD <- stats::sd(predictor)
      if (!is.finite(predictorSD) || predictorSD <= 0)
        return(gettextf("Covariate %s cannot be standardized numerically. Rescale its values and try again.", jaspBase::decodeColNames(name)))
    }
    design <- .asSSM_predictorDesign(dataset, options)
    if (any(!is.finite(design$X)) || any(!is.finite(design$scale)) || any(design$scale <= 0))
      return(gettext("The covariates cannot be standardized numerically. Rescale their values and try again."))
    if (qr(cbind(1, design$X), tol = 1e-10)$rank < length(predictors) + 1L)
      return(gettext("The covariates are linearly dependent (for example, one is a rescaled or shifted copy of another), or there are too few lots to distinguish their effects. Remove redundant covariates or add lots with distinct covariate values."))
  }

  if (any(count > sampleSize)) {
    return(gettext("Observed defects cannot exceed the sample size for any lot."))
  }

  if (any(sampleSize > lotSize)) {
    return(gettext("Sample size cannot exceed the lot size for any lot."))
  }

  NULL
}

.asSSM_fitDependencies <- function() {
  c(
    "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
    "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcBurnin",
    "ssm_advancedMcmcChains", "ssm_advancedMcmcSamples",
    "ssm_advancedMcmcThin", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  )
}

.asSSM_modelVersion <- function() "innovation-mixture-numeric-v4"

.asSSM_fitModel <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["fit"]])) {
    fitContainer <- createJaspState()
    fitContainer$dependOn(.asSSM_fitDependencies())
    jaspResults[["fit"]] <- fitContainer
  }

  validationError <- .asSSM_validateInputs(dataset, options)
  if (!is.null(validationError)) {
    .asSSM_setAnalysisError(jaspResults, validationError)
    return()
  }

  stanData <- list(
    T = nrow(dataset),
    n = array(as.integer(dataset[[options[["ssm_sampleSize"]]]]), dim = nrow(dataset)),  # tested items
    s = array(as.integer(dataset[[options[["ssm_total"]]]]), dim = nrow(dataset)),       # lot size
    y = array(as.integer(dataset[[options[["ssm_count"]]]]), dim = nrow(dataset)),       # defects in sample
    prior_theta1_a = options[["ssm_priorTheta1Shape1"]],
    prior_theta1_b = options[["ssm_priorTheta1Shape2"]],
    prior_sigma_sd = options[["ssm_priorSigmaSD"]]
  )

  design <- .asSSM_predictorDesign(dataset, options)
  usePredictor <- length(design$names) > 0L

  if (usePredictor) {
    stanData$K <- ncol(design$X)
    stanData$X <- design$X
    stanData$prior_beta_scale <- options[["ssm_priorBetaScale"]]
    modelName <- "complex"
  } else {
    modelName <- "simple"
  }

  fitInputs <- list(
    data = stanData, predictors = design,
    lots = dataset[[options[["ssm_time"]]]],
    settings = options[grep("^ssm_advancedMcmc", .asSSM_fitDependencies(), value = TRUE)]
  )
  cachedFit <- jaspResults[["fit"]]$object
  if (!is.null(cachedFit)) {
    currentVersion <- is.list(cachedFit) &&
      identical(cachedFit$model_version, .asSSM_modelVersion())
    if (currentVersion && identical(cachedFit$fit_inputs, fitInputs))
      return()

    jaspResults[["fit"]]$object <- NULL
    if (!currentVersion) {
      modelNote <- createJaspHtml(
        title = gettext("Model update"),
        text = gettext("The saved fit uses a different model version. Recalculation uses the current mixture model, so results may differ from those previously saved.")
      )
      modelNote$position <- -0.75
      modelNote$dependOn(.asSSM_fitDependencies())
      jaspResults[["modelChangeNote"]] <- modelNote
    }
  }
  .asSSM_clearOutputs(jaspResults)
  # Silently replace any previous error while JASP retains old results.
  jaspResults[["placeholder"]] <- createJaspHtml(
    text = "", elementType = "",
    position = 0, dependencies = .asSSM_fitDependencies()
  )

  thisSeed  <- options[["ssm_advancedMcmcSeed"]]
  thisDelta <- options[["ssm_advancedMcmcAdaptDelta"]]
  thisDepth <- options[["ssm_advancedMcmcMaxTreeDepth"]]
  thisChains <- as.integer(options[["ssm_advancedMcmcChains"]])

  availableCores <- suppressWarnings(parallel::detectCores())
  if (!is.finite(availableCores) || availableCores < 1)
    availableCores <- 1L
  thisCores <- min(thisChains, as.integer(availableCores))

  controlList <- list()
  if (is.numeric(thisDelta) && length(thisDelta) == 1L &&
      is.finite(thisDelta) && thisDelta > 0 && thisDelta < 1)
    controlList$adapt_delta <- thisDelta

  if (is.numeric(thisDepth) && length(thisDepth) == 1L &&
      is.finite(thisDepth) && thisDepth > 0)
    controlList$max_treedepth <- as.integer(thisDepth)

  jaspBase::startProgressbar(1, gettext("Sampling..."))

  tryCatch({
    model_obj <- .asSSM_getStanModel(modelName)
    stanFit <- rstan::sampling(
      object     = model_obj,
      data       = stanData,
      init       = .asSSM_mixtureInit(stanData, thisChains, predictor = usePredictor),
      chains     = thisChains,
      iter       = options[["ssm_advancedMcmcBurnin"]] + options[["ssm_advancedMcmcSamples"]],
      warmup     = options[["ssm_advancedMcmcBurnin"]],
      thin       = options[["ssm_advancedMcmcThin"]],
      cores      = thisCores,
      refresh    = 0,
      seed       = thisSeed,
      control    = controlList
    )

    samples <- rstan::extract(stanFit)
    if (length(samples$y_pred) == 0L)
      stop(gettext("Sampling produced no posterior predictive draws. Check the sampler output and MCMC settings, then refit. An acceptance decision cannot be calculated."), call. = FALSE)
    summary <- rstan::summary(stanFit)$summary
    maxTreedepth <- if (is.null(controlList$max_treedepth)) 10L else controlList$max_treedepth
    diagnostics <- tryCatch(
      .asSSM_extractDiagnostics(stanFit, stanData$T, usePredictor, maxTreedepth,
                               options[["ssm_advancedMcmcThin"]], thisChains),
      error = function(e) list(error = conditionMessage(e))
    )

    jaspResults[["fit"]]$object <- list(
      model_version = .asSSM_modelVersion(),
      fit_inputs = fitInputs,
      predictor_design = design,
      samples = samples,
      summary = summary,
      diagnostics = diagnostics
    )
  }, error = function(e) {
    msg <- gettextf("Estimation failed: %s", e$message)
    .asSSM_setAnalysisError(jaspResults, msg)
  })

  jaspBase::progressbarTick()
}


.asSSM_createSummaryTable <- function(jaspResults, dataset, options) {
  table <- jaspResults[["summaryTable"]]
  if (is.null(table)) {
    table <- createJaspTable(title = gettext("Posterior Summary (Current Lot)"))
    table$dependOn(c(
      .asSSM_fitDependencies(),
      "ssm_controlLimitsLower", "ssm_controlLimitsUpper", "ssm_decisionAcceptThreshold", "ssm_decisionRejectThreshold"
    ))
    table$position <- 0

    table$addColumnInfo(name = "t",        title = gettext("Lot Nr."),              type = "string")
    table$addColumnInfo(name = "y",        title = gettext("Obs. Defects"),         type = "integer")
    table$addColumnInfo(name = "pred",     title = gettext("Median Pred. Defects"), type = "integer")
    table$addColumnInfo(name = "massAQL",  title = gettext("Mass < AQL"),           type = "number", format = "dp:3")
    table$addColumnInfo(name = "massRQL",  title = gettext("Mass > RQL"),           type = "number", format = "dp:3")
    table$addColumnInfo(name = "decision", title = gettext("Decision"),                 type = "string")

    jaspResults[["summaryTable"]] <- table
  }

  table$setData(list(
    t = character(0),
    y = integer(0),
    pred = numeric(0),
    massAQL = numeric(0),
    massRQL = numeric(0),
    decision = character(0)
  ))

  fit <- jaspResults[["fit"]]
  if (is.null(fit) || is.null(fit$object)) return()

  samples <- fit$object$samples

  T_idx <- nrow(dataset)
  tT <- dataset[[options[["ssm_time"]]]][T_idx]
  sT <- dataset[[options[["ssm_total"]]]][T_idx]        # total lot size
  nT <- dataset[[options[["ssm_sampleSize"]]]][T_idx]   # sample size
  yT <- dataset[[options[["ssm_count"]]]][T_idx]

  if (is.na(tT) || is.na(sT) || is.na(yT)) {
    table$setError(gettext("The Lot Number, Lot Size, or Defect Count is missing (NA) for the last lot."))
    return()
  }

  y_pred <- samples$y_pred
  pmf <- .asSSM_predictivePMF(y_pred, sT, nT, yT)

  limit_AQL <- .asSSM_countLimit(options[["ssm_controlLimitsLower"]], sT)
  limit_RQL <- .asSSM_countLimit(options[["ssm_controlLimitsUpper"]], sT)

  # Tally membership before division so exact empirical threshold ties remain ties.
  mass_below_AQL <- sum(y_pred < limit_AQL) / length(y_pred)
  mass_above_RQL <- sum(y_pred > limit_RQL) / length(y_pred)

  acceptThreshold <- options[["ssm_decisionAcceptThreshold"]]
  rejectThreshold <- options[["ssm_decisionRejectThreshold"]]
  acceptPercent <- format(100 * acceptThreshold, trim = TRUE, scientific = FALSE, digits = 10)
  rejectPercent <- format(100 * rejectThreshold, trim = TRUE, scientific = FALSE, digits = 10)

  decision <- gettext("Consider to Continue")
  decisionNote <- gettextf("Further sampling is suggested because neither decision criterion is met at the selected thresholds: %1$s%% for acceptance and %2$s%% for rejection.",
                          acceptPercent, rejectPercent)
  if (!is.na(mass_above_RQL) && mass_above_RQL < acceptThreshold) {
    decision <- gettext("Consider to Accept")
    decisionNote <- gettextf("Acceptance is suggested because the posterior predictive probability that the lot's defect proportion exceeds the RQL is below the acceptance threshold of %s%%.",
                            acceptPercent)
  } else if (!is.na(mass_above_RQL) && !is.na(mass_below_AQL) &&
             mass_above_RQL > acceptThreshold && mass_below_AQL < rejectThreshold) {
    decision <- gettext("Consider to Reject")
    decisionNote <- gettextf("Rejection is suggested because the posterior predictive probability of exceeding the RQL is greater than %1$s%%, while the probability of falling below the AQL is less than %2$s%%.",
                            acceptPercent, rejectPercent)
  }

  table$setData(list(
    t = format(tT, trim = TRUE, scientific = FALSE, digits = 15),
    y = yT,
    pred = .asSSM_pmfMedian(pmf),
    massAQL = mass_below_AQL,
    massRQL = mass_above_RQL,
    decision = decision
  ))
  table$addFootnote(decisionNote)
}

.asSSM_createPredictorSummaryTable <- function(jaspResults, options) {
  number <- length(.asSSM_predictorNames(options))
  if (!number) {
    jaspResults[["predictorSummary"]] <- NULL
    return()
  }

  table <- jaspResults[["predictorSummary"]]
  if (is.null(table)) {
    table <- createJaspTable(title = gettext("Predictor Coefficients"))
    table$dependOn(.asSSM_fitDependencies())
    table$position <- 0.5
    table$addColumnInfo(name = "predictor", title = gettext("Predictor"), type = "string")
    table$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number", format = "dp:3")
    table$addColumnInfo(name = "median", title = gettext("Median"), type = "number", format = "dp:3")
    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", format = "dp:3",
                       overtitle = gettextf("95%% Credible Interval"))
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", format = "dp:3",
                       overtitle = gettextf("95%% Credible Interval"))
    table$addFootnote(gettext("Coefficients describe the change in log-odds of a defect per one SD increase in the covariate, holding the latent baseline and other covariates fixed. Intervals are equal-tailed."))
    jaspResults[["predictorSummary"]] <- table
  }

  table$setData(list(predictor = character(), mean = numeric(), median = numeric(),
                     lower = numeric(), upper = numeric()))
  fit <- jaspResults[["fit"]]
  if (is.null(fit) || is.null(fit$object)) return()

  beta <- fit$object$samples$beta_predictor
  if (is.numeric(beta) && is.null(dim(beta)) && number == 1L)
    beta <- matrix(beta, ncol = 1L)
  if (!is.matrix(beta) || !is.numeric(beta) || is.complex(beta) ||
      nrow(beta) == 0L || ncol(beta) != number || any(!is.finite(beta))) {
    table$setError(gettext("Finite posterior draws for every predictor are required to summarize the coefficients. Inspect the sampling diagnostics."))
    return()
  }

  intervals <- apply(beta, 2L, stats::quantile, probs = c(0.025, 0.975), names = FALSE)
  table$setData(list(
    predictor = fit$object$predictor_design$labels,
    mean = colMeans(beta),
    median = apply(beta, 2L, stats::median),
    lower = intervals[1L, ],
    upper = intervals[2L, ]
  ))
}

.asSSM_plotState <- function(jaspResults, dataset, options) {
  plot <- jaspResults[["ssm_statePlot"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Defect Rate Over Time"), width = 600, height = 400)
    plot$dependOn(c(
      .asSSM_fitDependencies(),
      "ssm_statePlot"
    ))
    plot$position <- 1
    jaspResults[["ssm_statePlot"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  theta <- samples$theta * 100
  timeValues <- dataset[[options[["ssm_time"]]]]

  df <- data.frame(
    time  = timeValues,
    mean  = colMeans(theta),
    lower = apply(theta, 2, quantile, 0.025),
    upper = apply(theta, 2, quantile, 0.975)
  )

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$time)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(df$lower, df$upper))
  if (all(df$lower == 0 & df$upper == 0)) yBreaks <- c(0, 1)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$mean))
  if (nrow(df) == 1L) {
    p <- p +
      ggplot2::geom_linerange(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                             colour = "#7f7f7f", linewidth = 0.6) +
      ggplot2::geom_point(size = 2)
  } else {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                           fill = "#c2c2c2", alpha = 0.6) +
      ggplot2::geom_line(linewidth = 1)
  }
  p <- p +
    ggplot2::labs(
      x = gettext("Lot"),
      y = gettextf("Defect Rate (%%)")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_plotPrediction <- function(jaspResults, dataset, options) {
  depVars <- c(.asSSM_fitDependencies(), "ssm_predictionPlot",
               "ssm_controlLimitsLower", "ssm_controlLimitsUpper")

  plot <- jaspResults[["predPlot"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Posterior Predictive"), width = 500, height = 400)
    plot$dependOn(depVars)
    plot$position <- 2
    jaspResults[["predPlot"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  y_pred <- samples$y_pred

  sT <- dataset[[options[["ssm_total"]]]][nrow(dataset)]
  nT <- dataset[[options[["ssm_sampleSize"]]]][nrow(dataset)]
  yT <- dataset[[options[["ssm_count"]]]][nrow(dataset)]
  pmf <- .asSSM_predictivePMF(y_pred, sT, nT, yT)

  limit_AQL <- .asSSM_countLimit(options[["ssm_controlLimitsLower"]], sT)
  limit_RQL <- .asSSM_countLimit(options[["ssm_controlLimitsUpper"]], sT)

  # Posterior probabilities for AQL / RQL regions
  # Tally membership before division so exact empirical threshold ties remain ties.
  mass_below_AQL <- sum(y_pred < limit_AQL) / length(y_pred)
  mass_above_RQL <- sum(y_pred > limit_RQL) / length(y_pred)

  # Show the central predictive range and both quality limits.
  pred_q <- stats::quantile(y_pred, probs = c(0.001, 0.999), na.rm = TRUE, names = FALSE)
  x_min <- floor(min(pred_q[1], limit_AQL, limit_RQL))
  x_max <- ceiling(max(pred_q[2], limit_AQL, limit_RQL))
  x_min <- max(0, x_min)
  if (!is.finite(x_max) || x_max <= x_min)
    x_max <- x_min + 1

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(x_min, x_max, limit_AQL, limit_RQL))
  if (length(xBreaks) < 2L)
    xBreaks <- c(x_min, x_max)
  xLower <- min(xBreaks)
  xUpper <- max(xBreaks)

  # Group whole counts rather than cutting between uneven numbers of integers.
  # Short ranges show one bar per count; wide ranges use at most 30 bins.
  pmf_plot <- pmf[pmf$defects >= xLower & pmf$defects <= xUpper, , drop = FALSE]
  if (nrow(pmf_plot) == 0L)
    pmf_plot <- pmf

  firstCount <- ceiling(xLower)
  countSpan <- floor(xUpper) - firstCount + 1
  binWidth <- max(1, ceiling(countSpan / 30))
  breaks <- firstCount - 0.5 + seq.int(0, ceiling(countSpan / binWidth)) * binWidth
  bin_ids <- cut(pmf_plot$defects, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
  mids <- head(breaks, -1L) + diff(breaks) / 2
  probs <- tapply(pmf_plot$prob, bin_ids, sum)
  df_hist <- data.frame(
    x = mids,
    prob = 0
  )
  if (length(probs) > 0L)
    df_hist$prob[as.integer(names(probs))] <- as.numeric(probs)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(df_hist$prob)))

  p <- ggplot2::ggplot(df_hist, ggplot2::aes(x = .data$x, y = .data$prob)) +
    ggplot2::geom_col(width = binWidth, fill = "#595959", colour = "white", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = limit_AQL, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = limit_RQL, linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = limit_AQL, y = max(df_hist$prob),
                      label = "AQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::annotate("text", x = limit_RQL, y = max(df_hist$prob),
                      label = "RQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::labs(
      x = gettext("Defects in Current Lot"),
      y = gettext("Posterior Predictive Probability")
    ) +
    ggplot2::scale_x_continuous(
      breaks = xBreaks,
      # Keep the complete boundary bars; scale limits otherwise censor them.
      limits = range(xBreaks, breaks)
    ) +
    ggplot2::scale_y_continuous(
      breaks = yBreaks,
      limits = range(yBreaks)
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p


}


.asSSM_plotPosteriorDist <- function(jaspResults, dataset, options) {
  plot <- jaspResults[["postDist"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Posterior Defect Rate"), width = 500, height = 400)
    plot$dependOn(c(
      .asSSM_fitDependencies(),
      "ssm_posteriorDistPlot"
    ))
    plot$position <- 3
    jaspResults[["postDist"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  theta   <- samples$theta
  df    <- data.frame(x = theta[, ncol(theta)] * 100)
  if (length(df$x) < 2L || any(!is.finite(df$x))) {
    plot$setError(gettext("Finite posterior draws are required to plot the defect-rate density. Inspect the sampling diagnostics."))
    return()
  }
  if (all(df$x == df$x[1L])) {
    plot$setError(gettext("The posterior defect-rate draws have no variation, so a density cannot be estimated. Inspect the sampling diagnostics."))
    return()
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)

  densVals <- stats::density(df$x)
  yBreaks  <- jaspGraphs::getPrettyAxisBreaks(c(0, max(densVals$y)))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_density(fill = "#b3b3b3") +
    ggplot2::labs(
      x = gettextf("Defect Rate (%%)"),
      y = gettext("Density")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_plotBetaCoefficient <- function(jaspResults, dataset, options) {
  if (!length(.asSSM_predictorNames(options))) return()
  plot <- jaspResults[["betaPlot"]]
  if (is.null(plot)) {
    number <- length(.asSSM_predictorNames(options))
    plot <- createJaspPlot(title = gettext("Predictor Coefficients"),
                          width = 500, height = 350 * number)
    plot$dependOn(c(
      .asSSM_fitDependencies(),
      "ssm_plotBeta"
    ))

    plot$position <- 4
    jaspResults[["betaPlot"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  fit <- jaspResults[["fit"]]$object
  design <- fit$predictor_design
  number <- length(design$names)
  beta <- fit$samples$beta_predictor
  if (is.numeric(beta) && is.null(dim(beta)) && number == 1L)
    beta <- matrix(beta, ncol = 1L)
  if (!is.matrix(beta) || !is.numeric(beta) || is.complex(beta) ||
      nrow(beta) < 2L || ncol(beta) != number || any(!is.finite(beta))) {
    plot$setError(gettext("Finite posterior draws for every predictor are required to plot the coefficients. Inspect the sampling diagnostics."))
    return()
  }
  if (any(vapply(seq_len(number), function(i) all(beta[, i] == beta[1L, i]), logical(1)))) {
    plot$setError(gettext("The posterior draws for one or more predictor coefficients have no variation, so their densities cannot be estimated. Inspect the sampling diagnostics."))
    return()
  }
  densities <- lapply(seq_along(design$names), function(i) {
    # Extend the grid beyond the sampled range so both density tails approach zero.
    density <- stats::density(beta[, i])
    data.frame(x = density$x, density = density$y, covariate = design$labels[i])
  })
  df <- do.call(rbind, densities)
  df$covariate <- factor(df$covariate, levels = design$labels)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$density)) +
    ggplot2::geom_area(fill = "#b3b3b3") +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
    ggplot2::facet_wrap(~ covariate, ncol = 1, scales = "free") +
    ggplot2::labs(
      x = gettext("Coefficient \u03B2 (per SD)"),
      y = gettext("Density")
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_countLimit <- function(proportion, lotSize) {
  limit <- proportion * lotSize
  integerLimit <- round(limit)
  # Decimal input and multiplication can move an integer boundary by a few ulps.
  # A relative tolerance preserves genuine fractional limits, including near zero.
  tolerance <- 2 * .Machine$double.eps * abs(limit)
  if (abs(limit - integerLimit) <= tolerance) integerLimit else limit
}

.asSSM_predictivePMF <- function(y_pred_draws, sT, nT, yT) {
  validCount <- function(x) is.numeric(x) && !is.complex(x) && length(x) == 1L &&
    is.finite(x) && x >= 0 && x == floor(x)
  if (!validCount(sT) || !validCount(nT) || !validCount(yT) ||
      yT > nT || nT > sT || sT >= .Machine$integer.max)
    stop(gettext("Invalid lot counts for the predictive distribution. Check the lot size, inspected count and observed defects. An acceptance decision cannot be calculated."), call. = FALSE)
  if (length(y_pred_draws) == 0L)
    stop(gettext("No posterior predictive draws are available. Check the sampler output and MCMC settings, then refit. An acceptance decision cannot be calculated."), call. = FALSE)
  if (!is.numeric(y_pred_draws) || is.complex(y_pred_draws) || any(!is.finite(y_pred_draws)))
    stop(gettext("Posterior predictive draws contain non-numeric, missing or non-finite values. Inspect the sampler output and refit. An acceptance decision cannot be calculated."), call. = FALSE)
  if (any(y_pred_draws != floor(y_pred_draws)))
    stop(gettext("Posterior predictive defect counts must be whole numbers. Inspect the model output and refit. An acceptance decision cannot be calculated."), call. = FALSE)
  if (any(y_pred_draws < yT | y_pred_draws > yT + (sT - nT)))
    stop(gettext("Posterior predictive defect counts fall outside the possible total given the inspected items. Inspect the model output and refit. An acceptance decision cannot be calculated."), call. = FALSE)

  draws <- as.integer(y_pred_draws)
  # Unobserved counts have exactly zero empirical mass. Store only the sorted
  # observed support so memory depends on the draws, not on the lot size.
  support <- sort(unique(draws))
  probs <- tabulate(match(draws, support), nbins = length(support))
  probs <- probs / sum(probs)

  pmf <- data.frame(defects = support, prob = probs)
  .asSSM_validatePMF(pmf)
  pmf
}

.asSSM_validatePMF <- function(pmf) {
  if (!is.data.frame(pmf) || !all(c("defects", "prob") %in% names(pmf)) || nrow(pmf) == 0L ||
      !is.numeric(pmf$defects) || is.complex(pmf$defects) ||
      !is.numeric(pmf$prob) || is.complex(pmf$prob) ||
      any(!is.finite(pmf$defects)) || any(!is.finite(pmf$prob)) ||
      any(pmf$defects < 0 | pmf$defects != floor(pmf$defects)) || anyDuplicated(pmf$defects) ||
      any(pmf$prob < 0) || abs(sum(pmf$prob) - 1) > 1e-8)
    stop(gettext("The posterior predictive probabilities are invalid or do not sum to one. Inspect the model output and refit. An acceptance decision cannot be calculated."), call. = FALSE)
  invisible(NULL)
}

.asSSM_pmfMassBelow <- function(pmf, threshold) {
  .asSSM_validatePMF(pmf)
  sum(pmf$prob[pmf$defects < threshold])
}

.asSSM_pmfMassAbove <- function(pmf, threshold) {
  .asSSM_validatePMF(pmf)
  sum(pmf$prob[pmf$defects > threshold])
}

.asSSM_pmfMedian <- function(pmf) {
  .asSSM_validatePMF(pmf)

  cumProb <- cumsum(pmf$prob)
  idx <- which(cumProb >= 0.5)[1]
  if (is.na(idx))
    return(NA_real_)

  pmf$defects[idx]
}

.asSSM_createMcmcSummaryTable <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["ssm_showMcmcSummary"]])) {
    jaspResults[["mcmcSummary"]] <- NULL
    return()
  }

  table <- jaspResults[["mcmcSummary"]]
  if (is.null(table)) {
    table <- createJaspTable(title = gettext("MCMC Diagnostics"))
    table$dependOn(c(
      .asSSM_fitDependencies(),
      "ssm_showMcmcSummary", "ssm_showTheta", "ssm_showSigma", "ssm_showYpred", "ssm_showBeta"
    ))
    table$position <- 5   # after the other tables/plots
    table$addColumnInfo(name = "parameter", title = gettext("Parameter"), type = "string")
    table$addColumnInfo(name = "mean",      title = gettext("Mean"),      type = "number")
    table$addColumnInfo(name = "sd",        title = gettext("SD"),        type = "number")
    table$addColumnInfo(name = "n_eff",     title = gettext("ESS"),       type = "number")
    table$addColumnInfo(name = "Rhat",      title = gettext("R-hat"),     type = "number")
    table$addFootnote(gettext("ESS and R-hat are RStan's traditional raw-draw ESS and split R-hat summaries. The automatic computational checks use rank-normalized split R-hat and bulk/tail ESS for all sampled parameters. Diagnostics can be undefined for constant generated quantities, such as predicted defects after complete inspection."))

    jaspResults[["mcmcSummary"]] <- table
  }

  table$setData(list(
    parameter = character(0),
    mean = numeric(0),
    sd = numeric(0),
    n_eff = numeric(0),
    Rhat = numeric(0)
  ))

  fit <- jaspResults[["fit"]]
  if (is.null(fit) || is.null(fit$object)) return()

  sm <- fit$object$summary
  allNames <- rownames(sm)
  pars <- character(0)

  if (isTRUE(options[["ssm_showTheta"]])) {
    pars <- c(pars, grep("^theta\\[", allNames, value = TRUE))
  }
  if (isTRUE(options[["ssm_showSigma"]])) {
    if ("sigma_eta" %in% allNames)
      pars <- c(pars, "sigma_eta")
  }
  if (isTRUE(options[["ssm_showYpred"]])) {
    if ("y_pred" %in% allNames)
      pars <- c(pars, "y_pred")
  }
  if (length(.asSSM_predictorNames(options)) && isTRUE(options[["ssm_showBeta"]])) {
    pars <- c(pars, grep("^beta_predictor\\[[0-9]+\\]$", allNames, value = TRUE))
  }

  if (length(pars) == 0L) {
    table$setError(gettext("No parameters selected for the MCMC diagnostics table."))
    return()
  }

  smSel <- sm[pars, , drop = FALSE]

  table$setData(list(
    parameter = .asSSM_betaLabels(rownames(smSel), jaspResults[["fit"]]$object$predictor_design),
    mean = smSel[, "mean"],
    sd = smSel[, "sd"],
    n_eff = smSel[, "n_eff"],
    Rhat = smSel[, "Rhat"]
  ))
}
