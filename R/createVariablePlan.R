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

##---------------------------------------------------------------
##                    Create variable plan.                    --
##---------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##---------------------------------------------------------------
CreateVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Dependency variables
  riskVariables <- c("acceptableQualityLevel", "producerRisk", "rejectableQualityLevel", "consumerRisk")
  proportionVariables <- c("proportionNonconformingFrom", "proportionNonconformingTo", "proportionNonconformingStep")

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["createVarContainer"]]) || jaspResults[["createVarContainer"]]$getError()) {
    createVarContainer <- createJaspContainer(title = gettext("Create Variable Plan"))
    # Common dependencies
    createVarContainer$dependOn(riskVariables)
    jaspResults[["createVarContainer"]] <- createVarContainer
  } else {
    createVarContainer <- jaspResults[["createVarContainer"]]
  }

  # Description of the plan
  planDescription <- createJaspHtml(text = gettext("Z.LSL = (mean - LSL) / historical standard deviation\n\nAccept lot if Z.LSL >= k, otherwise reject."),
                           dependencies=riskVariables, position=1)
  createVarContainer[["decision_info"]] <- planDescription

  sd <- "unknown"
  if (options$historicalSdKnown) {
    sd <- "known"
  }

  # Initialize the plan table
  planTable <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  planTable$transpose <- TRUE
  planTable$transposeWithOvertitle <- FALSE
  planTable$dependOn(c(riskVariables, "historicalSdKnown"))
  planTable$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  planTable$addColumnInfo(name = "col_1", title = gettext("Sample Size"), type = "integer")
  planTable$addColumnInfo(name = "col_2", title = gettext("Critical Distance (k)"), type = "number")
  planTable$showSpecifiedColumnsOnly <- TRUE
  planTable$position <- 2
  createVarContainer[["plan_table"]] <- planTable

  # Plan constraints
  aql <- options$acceptableQualityLevel
  rql <- options$rejectableQualityLevel
  producerAcceptanceProbability <- 1 - options$producerRisk
  consumerAcceptanceProbability <- options$consumerRisk

  # Error handling for AQL/RQL
  if (aql >= rql) {
    createVarContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }
  # Error handling for Producer's and Consumer's Risk
  if (producerAcceptanceProbability <= consumerAcceptanceProbability) {
    createVarContainer$setError(gettext("1 - \u03B1 (producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }

  N <- options$lotSize
  # Quality levels
  proportionFrom <- options$proportionNonconformingFrom
  proportionTo <- options$proportionNonconformingTo
  proportionStep <- options$proportionNonconformingStep
  checkPdErrors(createVarContainer, proportionFrom, proportionTo, proportionStep)
  if (createVarContainer$getError()) {
    return ()
  }
  pd <- seq(proportionFrom, proportionTo, proportionStep)
  # Add AQL and RQL to quality range
  pd <- c(pd, aql, rql)
  pd <- sort(pd)
  pd <- pd[!duplicated(pd)]

  # Sanity checks done. Let's find a plan that satisfies the constraints.
  variablePlan <- tryCatch(AcceptanceSampling::find.plan(PRP = c(aql, producerAcceptanceProbability), CRP = c(rql, consumerAcceptanceProbability), type = "normal", s.type = sd), error = function(x) "error")
  # find.plan can result in invalid sampling plans for certain quality constraints.
  # We want to check for such outputs.
  if (class(variablePlan) == "character" && variablePlan == "error") {
    createVarContainer$setError(gettext("Variable plan generated for the current quality constraints is invalid. Modify the quality constraints."))
    return ()
  }
  n <- variablePlan$n
  k <- variablePlan$k
  # Error checks for n
  if (is.null(n) || is.na(n) || is.infinite(n) || is.nan(n) || (n <= 0) || (!options$historicalSdKnown && (n <= 1))) {
    createVarContainer$setError(gettext("Variable plan generated for the current quality constraints has an invalid sample size (n). Modify the quality constraints."))
    return ()
  }
  # Error checks for k
  if (is.na(k) || is.null(k) || is.infinite(k) || is.nan(k) || k <= 0) {
    createVarContainer$setError(gettext("Variable plan generated for the current quality constraints has an invalid k value. Modify the quality constraints."))
    return ()
  }
  variableOperatingCharacteristic <- AcceptanceSampling::OCvar(n = n, k = k, type = "normal", s.type = sd, pd = pd)

  # Error check for lot size
  if (N < n) {
	createVarContainer$setError(gettextf("Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan.", N, n))
    return ()
  }

  # Plan dataframe
  planData <- data.frame(PD = pd, PA = variableOperatingCharacteristic@paccept)
  planData <- na.omit(planData)
  if (nrow(planData) == 0) {
    createVarContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }

  # Output options
  outputVariables <- c("summaryTable", "operatingCharacteristicCurve", "averageOutgoingQualityCurve", "averageTotalInspectionCurve")

  # 0. Fill the variable plan table
  planTable$addRows(list("col_1" = n, "col_2" = k))

  # 1. Plan summary
  if (options$summaryTable) {
    getSummary(createVarContainer, pos=3, c(proportionVariables, outputVariables[1]), planData)
  }
  # 2. OC Curve
  if (options$operatingCharacteristicCurve) {
    getOCCurve(createVarContainer, pos=4, c(proportionVariables, outputVariables[2]), planData)
  }
  # 3. AOQ Curve
  if (options$averageOutgoingQualityCurve) {
    getAOQCurve(createVarContainer, pos=5, c(proportionVariables, outputVariables[3], "lotSize"), planData, options, "", n)
    if (createVarContainer$getError()) {
      return ()
    }
  }
  # 4. ATI Curve
  if (options$averageTotalInspectionCurve) {
    getATICurve(createVarContainer, pos=6, c(proportionVariables, outputVariables[4], "lotSize"), planData, options, "", n)
  }
}
