.asSSM_predictorNames <- function(options) {
  # A saved single selection is a string; multiple selections are a character vector.
  predictors <- options[["covariates"]]
  if (!length(predictors)) return(character())
  predictors[nzchar(predictors)]
}

.asSSM_predictorDesign <- function(dataset, options) {
  predictors <- .asSSM_predictorNames(options)
  if (!length(predictors))
    return(list(names = character(), labels = character(), center = numeric(),
                scale = numeric(), X = matrix(numeric(), nrow(dataset), 0L)))

  scaled <- scale(as.matrix(dataset[, predictors, drop = FALSE]))
  list(
    names = predictors,
    labels = jaspBase::decodeColNames(predictors),
    center = attr(scaled, "scaled:center"),
    scale = attr(scaled, "scaled:scale"),
    X = matrix(as.numeric(scaled), nrow(dataset), length(predictors))
  )
}

.asSSM_betaLabels <- function(parameters, design) {
  labels <- parameters
  beta <- grep("^beta_predictor\\[[0-9]+\\]$", parameters)
  if (length(beta)) {
    index <- as.integer(sub("^beta_predictor\\[([0-9]+)\\]$", "\\1", parameters[beta]))
    labels[beta] <- paste0(parameters[beta], " (", design$labels[index], ")")
  }
  labels
}
