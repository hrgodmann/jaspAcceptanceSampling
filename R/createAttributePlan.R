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
##                    Create attribute plan                    --
##---------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
#' @seealso .findPlan()
##---------------------------------------------------------------
CreateAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Constraints to create a plan
  riskVariables <- c("acceptableQualityLevel", "producerRisk", "rejectableQualityLevel", "consumerRisk")
  # Quality levels
  proportionVariables <- c("proportionNonconformingFrom", "proportionNonconformingTo", "proportionNonconformingStep")
  dependencies <- c(proportionVariables, riskVariables, "lotSize", "distribution")

  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["createContainer"]]) || jaspResults[["createContainer"]]$getError()) {
    createContainer <- createJaspContainer(title = "")
    createContainer$dependOn(dependencies)
    jaspResults[["createContainer"]] <- createContainer
  } else {
    createContainer <- jaspResults[["createContainer"]]
  }

  # Plan table outline
  planTable <- createJaspTable(title = gettext("Generated Sampling Plan"))
  planTable$addColumnInfo(name = "col_1", title = "", type = "string")
  planTable$addColumnInfo(name = "col_2", title = gettext("Value"), type = "integer")
  planTable[["col_1"]] <- c("Sample Size", "Acceptance Number")
  planTable$position <- 1
  createContainer[["findPlanTable"]] <- planTable

  # Probability table outline
  probabilityTable <- createJaspTable(title = gettext("Acceptance Probabilities at AQL and RQL"))
  probabilityTable$addColumnInfo(name = "col_1", title = "", type = "string")
  probabilityTable$addColumnInfo(name = "col_2", title = gettext("Proportion Non-conforming"), type = "number")
  probabilityTable$addColumnInfo(name = "col_3", title = gettext("Acceptance Probability"), type = "number")
  probabilityTable$addColumnInfo(name = "col_4", title = gettext("Rejection Probability"), type = "number")
  probabilityTable[["col_1"]] <- c("AQL", "RQL")
  probabilityTable$position <- 2
  createContainer[["findProbTable"]] <- probabilityTable

  # Error handling for hypergeometric distribution
  aql <- options$acceptableQualityLevel
  rql <- options$rejectableQualityLevel
  checkHypergeom(createContainer, proportionVariables, options, type="", aql, rql)
  if (createContainer$getError()) {
    return ()
  }

  # Error handling for AQL/RQL
  if (aql >= rql) {
    createContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  producerAcceptanceProbability <- (1 - options$producerRisk)
  consumerAcceptanceProbability <- options$consumerRisk
  if (producerAcceptanceProbability <= consumerAcceptanceProbability) {
    createContainer$setError(gettext("1 - \u03B1 (producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }
  # Sanity checks done. Let's find a plan that satisfies the constraints.
  .findPlan(createContainer, options, dependencies, aql, rql, producerAcceptanceProbability, consumerAcceptanceProbability)
}

##----------------------------------------------------------------------------------
##  Find the sampling plan that satisfies the specified AQL and RQL constraints.  --
##----------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param dependencies {vector} Names of variables on which the output element depends.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param producerAcceptanceProbability {numeric} Minimum probability (0 to 1) of accepting the lot at Acceptable Quality Level.
#' @param consumerAcceptanceProbability {numeric} Maximum probability (0 to 1) of accepting the lot at Rejectable Quality Level.
#' @seealso CreateAttributePlan()
##----------------------------------------------------------------------------------
.findPlan <- function(jaspContainer, options, dependencies, aql, rql, producerAcceptanceProbability, consumerAcceptanceProbability) {
  proportionFrom <- options$proportionNonconformingFrom
  proportionTo <- options$proportionNonconformingTo
  proportionStep <- options$proportionNonconformingStep
  pd <- seq(proportionFrom, proportionTo, proportionStep)
  # Add AQL and RQL to quality range.
  pd <- c(pd, aql, rql)
  pd <- pd[!duplicated(pd)]
  pd <- sort(pd)
  dist <- .getSamplingDistributionType(options$distribution)
  planValues <- NULL
  plan <- NULL

  # # Create sampling plan with the specified values
  if (dist == "hypergeom") {
    # Need to provide the lot size (N) for hypergeometric distribution.
    planValues <- AcceptanceSampling::find.plan(PRP = c(aql, producerAcceptanceProbability), CRP = c(rql, consumerAcceptanceProbability), type = dist, N = options$lotSize)
    plan <- AcceptanceSampling::OC2c(N = options$lotSize, n = planValues$n, c = planValues$c, r = planValues$r, type = dist, pd = pd)
  } else {
    # Binomial and Poisson distributions don't require lot size (N) or standard deviation.
    planValues <- AcceptanceSampling::find.plan(PRP = c(aql, producerAcceptanceProbability), CRP = c(rql, consumerAcceptanceProbability), type = dist)
    plan <- AcceptanceSampling::OC2c(n = planValues$n, c = planValues$c, r = planValues$r, type = dist, pd = pd)
  }
  n <- planValues$n
  c <- planValues$c
  r <- planValues$r

  planData <- data.frame(PD = pd, PA = plan@paccept)
  planData <- na.omit(planData)
  if (nrow(planData) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  # Fill the output tables for the created plan.
  .attributePlanTable(jaspContainer, dependencies, aql, rql, planData$PA[planData$PD == aql], planData$PA[planData$PD == rql], n, c, r)

  # Plan summary
  if (options$summaryTable) {
    getSummary(jaspContainer, pos=4, c(dependencies, "summaryTable"), planData)
  }

  # OC Curve
  if (options$operatingCharacteristicCurve) {
    getOCCurve(jaspContainer, pos=5, c(dependencies, "operatingCharacteristicCurve"), planData)
  }

  # AOQ Curve (for plans with rectification)
  if (options$averageOutgoingQualityCurve) {
    getAOQCurve(jaspContainer, pos=6, "averageOutgoingQualityCurve", planData, options, "", n)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ATI Curve (for plans with rectification)
  if (options$averageTotalInspectionCurve) {
    getATICurve(jaspContainer, pos=7, "averageTotalInspectionCurve", planData, options, "", n)
    if (jaspContainer$getError()) {
      return ()
    }
  }
}

##----------------------------------------------------------------
##             Create and fill the output table(s).             --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param dependencies {vector} Names of variables on which the output element depends.
#' @param aql {numeric} Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param producerAcceptanceProbability {numeric} Minimum probability (0 to 1) of accepting the lot at Acceptable Quality Level.
#' @param consumerAcceptanceProbability {numeric} Maximum probability (0 to 1) of accepting the lot at Rejectable Quality Level.
#' @param n {numeric} Sample size for the plan.
#' @param c {numeric} Acceptance number for the plan.
#' @param r {numeric} Rejection number for the plan.
##----------------------------------------------------------------
.attributePlanTable <- function(jaspContainer, dependencies, aql, rql, producerAcceptanceProbability, consumerAcceptanceProbability, n, c, r) {
  # Fill the table with sample size and acceptance number
  planTable <- jaspContainer[["findPlanTable"]]
  planTable[["col_2"]] <- c(n, c)

  # Fill the table with acceptance and rejection probabilities for AQL and RQL
  probabilityTable <- jaspContainer[["findProbTable"]]
  probabilityTable[["col_2"]] <- c(aql, rql)
  probabilityTable[["col_3"]] <- c(producerAcceptanceProbability, consumerAcceptanceProbability)
  probabilityTable[["col_4"]] <- c(1-producerAcceptanceProbability, 1-consumerAcceptanceProbability)

  # Description of the sampling plan:
  if (is.null(jaspContainer[["description"]])) {
    description <- createJaspHtml(text = gettextf("If the number of defective items out of %1$d sampled is <= %2$d, accept the lot. Reject otherwise.", n, c), position = 3)
    jaspContainer[["description"]] <- description
  }
}