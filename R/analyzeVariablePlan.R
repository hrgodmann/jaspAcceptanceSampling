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
##                    Analyze variable plan.                    --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##----------------------------------------------------------------
AnalyzeVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Dependency variables
  planVariables <- c("sampleSize", "kValue")

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["analyzeVarContainer"]]) || jaspResults[["analyzeVarContainer"]]$getError()) {
    analyzeVarContainer <- createJaspContainer(title = gettext("Analyze Variable Plan"))
    analyzeVarContainer$dependOn(planVariables) # Common dependencies
    jaspResults[["analyzeVarContainer"]] <- analyzeVarContainer
  } else {
    analyzeVarContainer <- jaspResults[["analyzeVarContainer"]]
  }
  # Plan variables
  N <- options$lotSize
  n <- options$sampleSize
  k <- options$kValue
  sd <- if (options[["historicalSdKnown"]]) "known" else "unknown"

  # Initialize the plan table
  planTable <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  planTable$transpose <- TRUE
  planTable$transposeWithOvertitle <- FALSE
  planTable$dependOn(c(planVariables, "historicalSdKnown"))
  planTable$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  planTable$addColumnInfo(name = "col_1", title = gettext("Sample Size"), type = "integer")
  planTable$addColumnInfo(name = "col_2", title = gettext("Critical Distance (k)"), type = "number")
  planTable$addRows(list("col_1" = n, "col_2" = k))
  planTable$showSpecifiedColumnsOnly <- TRUE
  planTable$position <- 1
  analyzeVarContainer[["plan_table"]] <- planTable

  # Error check for n (sample size)
  if (!options$historicalSdKnown && (n <= 1)) {
    analyzeVarContainer$setError(gettext("If historical standard deviation is unknown, sample size has to be > 1."))
    return ()
  }

  # Error check for N (lot size)
  if (N < n) {
    analyzeVarContainer$setError(gettextf("Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan.", N, n))
    return ()
  }

  plan <- getPlan(analyzeVarContainer, options, "", n, k=k, sd=sd)
  if (analyzeVarContainer$getError()) {
    return ()
  }

  # Plan dataframe
  planData <- plan$df_plan
  operatingCharacteristic <- plan$oc_plan

  riskVariables <- c("planAssessmentAcceptableQualityLevel", "planAssessmentProducerRisk", "planAssessmentRejectableQualityLevel", "planAssessmentConsumerRisk")
  proportionVariables <- c("proportionNonconformingFrom", "proportionNonconformingTo", "proportionNonconformingStep")

  # 1. Assess plan
  if (options$planAssessment) {
    assessPlan(analyzeVarContainer, pos=2, c(riskVariables, proportionVariables, "planAssessment"), operatingCharacteristic, options, "")
    if (analyzeVarContainer$getError()) {
      return ()
    }
  }

  # 2. Plan summary
  if (options$summaryTable) {
    # Assess plan generates 2 output elements, so position of next element is previous + 2.
    getSummary(analyzeVarContainer, pos=4, c(proportionVariables, "summaryTable"), planData)
  }
  # 3. OC Curve
  if (options$operatingCharacteristicCurve) {
    getOCCurve(analyzeVarContainer, pos=5, c(proportionVariables, "operatingCharacteristicCurve"), planData)
  }
  # 4. AOQ Curve
  if (options$averageOutgoingQualityCurve) {
    getAOQCurve(analyzeVarContainer, pos=6, c(proportionVariables, "averageOutgoingQualityCurve", "lotSize"), planData, options, "", n)
    if (analyzeVarContainer$getError()) {
      return ()
    }
  }
  # 5. ATI Curve
  if (options$averageTotalInspectionCurve) {
    getATICurve(analyzeVarContainer, pos=7, c(proportionVariables, "averageTotalInspectionCurve", "lotSize"), planData, options, "", n)
  }
}