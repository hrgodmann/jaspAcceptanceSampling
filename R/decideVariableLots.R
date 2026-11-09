#
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
#

##----------------------------------------------------------------
##            Decision table for variable plan lots.            --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
#' @importFrom stats pnorm pbeta
##----------------------------------------------------------------
DecideVariableLots <- function(jaspResults, dataset = NULL, options, ...) {
  dependencies <- c("measurement", "sampleStatistics", "sampleSize", "sampleMean", "sampleSd", "kValue", "lowerSpecificationLimit", "lowerSpecificationLimitValue", "upperSpecificationLimit", "upperSpecificationLimitValue", "historicalSdKnown", "historicalSd")
  riskVariables <- c("acceptableQualityLevel", "rejectableQualityLevel")

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["lotContainer"]]) || jaspResults[["lotContainer"]]$getError()) {
    lotContainer <- createJaspContainer(title = "")
    lotContainer$dependOn(c(dependencies, riskVariables)) # Common dependencies
    jaspResults[["lotContainer"]] <- lotContainer
  } else {
    lotContainer <- jaspResults[["lotContainer"]]
  }

  # Plan variables
  n <- NULL
  sampleMean <- NULL
  sampleSd <- NULL
  k <- options$kValue
  variableName <- NULL

  vars <- unlist(options$measurement)
  if (options$sampleStatistics) {
    # Take sample size, sample mean and sample SD directly from sample statistics option values.
    n <- options$sampleSize
    sampleMean <- options$sampleMean
    sampleSd <- options$sampleSd
  } else {
    if (length(vars) == 1) {
      # Dataset is available. Process it.
      if (is.null(dataset)) {
        dataset <- .readDataSetToEnd(columns.as.numeric=vars)
        # Proceed with calculations for the data sample.
        data <- na.omit(dataset[[.v(vars)]])
        # Error checking for infinite/missing values.
        .hasErrors(dataset=dataset, type = c("infinity", "missingValues"), target = vars, exitAnalysisIfErrors = TRUE)
        n <- length(data)
        sampleMean <- mean(data)
        sampleSd <- sd(data)
        variableName <- vars[1]
      }
    }
  }

  # Initializing the lot decision table
  decisionTable <- createJaspTable(title = gettextf("Accept or Reject Lot %s", ifelse(!is.null(variableName), paste0("(Variable: <b>", variableName, "</b>)"), "")))
  decisionTable$transpose <- TRUE
  decisionTable$transposeWithOvertitle <- FALSE
  decisionTable$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row for title. Add title if needed.
  decisionTable$addColumnInfo(name = "col_1", title = gettext("Sample Size"), type = "integer")
  decisionTable$addColumnInfo(name = "col_2", title = gettext("Sample Mean"), type = "number")
  decisionTable$addColumnInfo(name = "col_3", title = gettext("Sample Standard Deviation"), type = "number")

  # The below rows (tranposed columns) are to be added only if the corresponding options have been selected.
  if (options$historicalSdKnown) {
    decisionTable$addColumnInfo(name = "col_4", title = gettext("Historical Standard Deviation"), type = "number")
  }
  if (options$lowerSpecificationLimit) {
    decisionTable$addColumnInfo(name = "col_5", title = gettext("Lower Specification Limit (LSL)"), type = "number")
  }
  if (options$upperSpecificationLimit) {
    decisionTable$addColumnInfo(name = "col_6", title = gettext("Upper Specification Limit (USL)"), type = "number")
  }
  if (options$lowerSpecificationLimit) {
    decisionTable$addColumnInfo(name = "col_7", title = gettext("Z.LSL"), type = "number")
  }
  if (options$upperSpecificationLimit) {
    decisionTable$addColumnInfo(name = "col_8", title = gettext("Z.USL"), type = "number")
  }
  decisionTable$addColumnInfo(name = "col_9", title = gettext("Critical Distance (k)"), type = "number")

  lotContainer[["decision_table"]] <- decisionTable
  # Sanity checks for sample statistics
  if (sampleSd <= 0) {
    lotContainer$setError(gettext("Sample standard deviation has to be greater than 0."))
    return ()
  }

  if (is.null(sampleMean)) {
    lotContainer$setError(gettext("Sample mean is invalid."))
    return ()
  }
  # We always have a sample standard deviation.
  # We might or might not have a historical standard deviation, so comparison is to be done accordingly.
  comparisonSd <- sampleSd
  sd <- "unknown"
  historicalSd <- 0
  if (options$historicalSdKnown) {
    # Historical standard deviation is known. We'll use it for comparison.
    sd <- "known"
    historicalSd <- options$historicalSd
    comparisonSd <- historicalSd
  }

  lowerSpecificationZ <- NULL
  upperSpecificationZ <- NULL
  decision <- NULL

  if (!options$lowerSpecificationLimit && !options$upperSpecificationLimit) {
    decision <- NULL
  }

  # Decision for the lot is made based on the available specification limits.
  # 1. LSL is available.
  if (options$lowerSpecificationLimit) {
    lowerSpecificationZ <- ((sampleMean - options$lowerSpecificationLimitValue) / comparisonSd)
    lowerSpecificationZ <- lowerSpecificationZ
    if (!options$upperSpecificationLimit) {
      # Only LSL available. Accept/reject lot based on Z.LSL.
      decision <- lowerSpecificationZ >= k
    }
  }
  # 2. USL is available.
  if (options$upperSpecificationLimit) {
    upperSpecificationZ <- ((options$upperSpecificationLimitValue - sampleMean) / comparisonSd)
    upperSpecificationZ <- upperSpecificationZ
    if (!options$lowerSpecificationLimit) {
      # Only USL available. Accept/reject lot based on Z.USL.
      decision <- upperSpecificationZ >= k
    }
  }
  # 3. Both LSL and USL are available.
  if (options$lowerSpecificationLimit && options$upperSpecificationLimit) {
    # When both LSL and USL are specified, we need to decide based on standard deviation.
    # Historical standard deviation known
    if (options$historicalSdKnown) {
      # Error handling for AQL/RQL
      aql <- options$acceptableQualityLevel
      rql <- options$rejectableQualityLevel
      if (aql >= rql) {
        lotContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
        return ()
      }
      halfSpecificationWidthZ <- (options$lowerSpecificationLimitValue - options$upperSpecificationLimitValue) / (2 * historicalSd)
      p <- pnorm(halfSpecificationWidthZ)
      if (2*p >= rql) {
        decision <- FALSE
      } else if (2*p <= aql) {
        decision <- (lowerSpecificationZ >= k) && (upperSpecificationZ >= k)
      } else {
        if (n <= 1) {
          lotContainer$setError(gettext("Can not accept or reject lot: sample size has to be greater than 1."))
          return ()
        } else {
          lowerSpecificationQuantile <- lowerSpecificationZ * sqrt(n/(n-1))
          lowerTailProbability <- pnorm(lowerSpecificationQuantile, lower.tail = FALSE)
          upperSpecificationQuantile <- upperSpecificationZ * sqrt(n/(n-1))
          upperTailProbability <- pnorm(upperSpecificationQuantile, lower.tail = FALSE)
          combinedTailProbability <- lowerTailProbability + upperTailProbability
          acceptanceThresholdZ <- k * sqrt(n/(n-1))
          m <- pnorm(acceptanceThresholdZ, lower.tail = FALSE)
          decision <- combinedTailProbability <= m
        }
      }
    } else {
      # Historical standard deviation unknown
      if (n <= 1) {
        lotContainer$setError(gettextf("Sample size has to be <b>> 1</b> if <b>both</b> LSL and USL are provided, and historical standard deviation is <b>unknown</b>."))
        return ()
      } else {
        a <- (n - 2) / 2
        b <- (n - 2) / 2
        lowerBetaQuantile <- max(0, 0.5 - (0.5 * lowerSpecificationZ * sqrt(n)/(n-1)))
        lowerTailProbability <- pbeta(lowerBetaQuantile, a, b)
        upperBetaQuantile <- max(0, 0.5 - (0.5 * upperSpecificationZ * sqrt(n)/(n-1)))
        upperTailProbability <- pbeta(upperBetaQuantile, a, b)
        combinedTailProbability <- lowerTailProbability + upperTailProbability
        acceptanceThresholdBetaQuantile <- 0.5 * (1 - k * sqrt(n)/(n-1))
        m <- pbeta(acceptanceThresholdBetaQuantile, a, b)
        decision <- combinedTailProbability <= m
      }
    }
  }
  # Fill up the decision table
  row = list("col_1" = n, "col_2" = sampleMean, "col_3" = sampleSd, "col_4" = comparisonSd)
  # The below values are to filled only when the corresponding options have been selected.
  if (options$lowerSpecificationLimit) {
    row = append(row, list("col_5" = options$lowerSpecificationLimitValue))
  }
  if (options$upperSpecificationLimit) {
    row = append(row, list("col_6" = options$upperSpecificationLimitValue))
  }
  if (options$lowerSpecificationLimit) {
    row = append(row, list("col_7" = lowerSpecificationZ))
  }
  if (options$upperSpecificationLimit) {
    row = append(row, list("col_8" = upperSpecificationZ))
  }
  row = append(row, list("col_9" = k))

  decisionTable$addRows(row)
  decisionTable$showSpecifiedColumnsOnly <- TRUE
  decisionTable$position <- 1

  # Show the decision for the lot.
  if (!is.null(decision)) {
    if (is.null(lotContainer[["decision_output"]])) {
      decisionOutput <- createJaspHtml(text = gettextf("<u>Decision:</u> <b>%s</b> lot.", ifelse(decision == TRUE, "Accept", "Reject")), position = 2)
      decisionOutput$position <- 2
      lotContainer[["decision_output"]] <- decisionOutput
    }
  }
}