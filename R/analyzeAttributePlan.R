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
##   Analyze single stage and multiple stage attribute plans   --
##---------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
#' @seealso .handleAttributePlan()
##---------------------------------------------------------------
AnalyzeAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  planVariables <- c("lotSize", "distribution")
  proportionVariables <- c("proportionNonconformingFrom", "proportionNonconformingTo", "proportionNonconformingStep")

  # Single sampling plan
  if ((options$singlePlanSampleSize > 0) && (options$singlePlanAcceptanceNumber >= 0)) {
    singlePlanVariables <- c(planVariables, "sampleSize", "acceptanceNumber", "rejectionNumber")
    singlePlanVariables <- .getPlanOptionNames(singlePlanVariables, "Single")
    singleProportionVariables <- .getPlanOptionNames(proportionVariables, "Single")

    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["singleContainer"]]) || jaspResults[["singleContainer"]]$getError()) {
      singleContainer <- createJaspContainer(title = gettext("Single Sampling Plan"))
      # Common dependencies for all single plans
      singleContainer$dependOn(c(singlePlanVariables, singleProportionVariables))
      jaspResults[["singleContainer"]] <- singleContainer
    } else {
      singleContainer <- jaspResults[["singleContainer"]]
    }
    .handleAttributePlan(singleContainer, pos=0, singlePlanVariables, singleProportionVariables, options, "Single")
  }

  # Multiple sampling plan
  if (length(options$multiplePlanStages) > 1) {
    multiplePlanVariables <- .getPlanOptionNames(planVariables, "Mult")
    multiplePlanVariables <- c(multiplePlanVariables, "multiplePlanStages", "multiplePlanStagesCount")
    multipleProportionVariables <- .getPlanOptionNames(proportionVariables, "Mult")
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["multContainer"]]) || jaspResults[["multContainer"]]$getError()) {
      multContainer <- createJaspContainer(title = gettext("Multiple Sampling Plan"))
      # Common dependencies for all multiple plans
      multContainer$dependOn(c(multiplePlanVariables, multipleProportionVariables))
      jaspResults[["multContainer"]] <- multContainer
    } else {
      multContainer <- jaspResults[["multContainer"]]
    }
    .handleAttributePlan(multContainer, pos=100, multiplePlanVariables, multipleProportionVariables, options, "Mult")
  }
}

##---------------------------------------------------------------
##                   Analyze attribute plans                   --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param planVariables {vector} Names of variables that determine a sampling plan. Used to set dependencies for outputs.
#' @param proportionVariables {vector} Variables to generate a sequence of quality levels. Includes the lower proportion, upper proportion, and step size.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @seealso AnalyzeAttributePlan()
##---------------------------------------------------------------
.handleAttributePlan <- function(jaspContainer, pos, planVariables, proportionVariables, options, type) {
  planTable <- createJaspTable(title = gettext("Acceptance Sampling Plan"))
  if (type == "Single") {
    planTable$dependOn(.getPlanOptionNames(c("sampleSize", "acceptanceNumber", "rejectionNumber"), type))
  } else {
    planTable$dependOn(c("multiplePlanStages"))
  }
  planTable$showSpecifiedColumnsOnly <- TRUE
  planTable$position <- pos
  jaspContainer[["plan_table"]] <- planTable

  planValues <- getPlanValues(jaspContainer, options, type)
  # Error handling for plan values
  if (jaspContainer$getError()) {
    return ()
  }

  # Error handling for hypergeometric distribution
  checkHypergeom(jaspContainer, proportionVariables, options, type)
  if (jaspContainer$getError()) {
    return ()
  }

  n <- planValues$n
  c <- planValues$c
  r <- planValues$r

  # Sampling plan table
  if (type == "Single") {
    # Single plan table
    planTable$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
    planTable$addColumnInfo(name = "table_1_col_2", title = gettext("Value"), type = "integer")
    planTable$addRows(list("table_1_col_1" = "Sample Size", "table_1_col_2" = n))
    planTable$addRows(list("table_1_col_1" = "Acceptance Number", "table_1_col_2" = c))
  } else {
    # Multiple plan table
    stages <- options[["multiplePlanStages"]]
    planTable$addColumnInfo(name = "table_1_col_1", title = gettext("Sample"), type = "integer")
    planTable$addColumnInfo(name = "table_1_col_2", title = gettext("Sample Size"), type = "integer")
    planTable$addColumnInfo(name = "table_1_col_3", title = gettext("Cum. Sample Size"), type = "integer")
    planTable$addColumnInfo(name = "table_1_col_4", title = gettext("Acc. Number"), type = "integer")
    planTable$addColumnInfo(name = "table_1_col_5", title = gettext("Rej. Number"), type = "integer")
    planTable$setData(list(table_1_col_1 = 1:length(stages), table_1_col_2 = n, table_1_col_3 = cumsum(n),
                            table_1_col_4 = c, table_1_col_5 = r))
  }
  # Assess plan options
  riskVariables <- .getPlanOptionNames(c("acceptableQualityLevel", "producerRisk", "rejectableQualityLevel", "consumerRisk"), type, assessment = TRUE)
  # Output options
  outputVariables <- .getPlanOptionNames(c("assessment", "summaryTable", "operatingCharacteristicCurve", "averageOutgoingQualityCurve", "averageTotalInspectionCurve", "averageSampleNumberCurve"), type)

  # Plan data
  plan <- getPlan(jaspContainer, options, type, n, c, r)
  if (jaspContainer$getError()) {
    return ()
  }
  planData <- plan$df_plan
  operatingCharacteristic <- plan$oc_plan

  # Assess plan
  if (options[[outputVariables[1]]]) {
    assessPlan(jaspContainer, pos=pos+1, c(outputVariables[1], riskVariables), operatingCharacteristic, options, type)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # Summary table
  if (options[[outputVariables[2]]]) {
    # Assess plan generates 2 output elements, so position of next element is previous + 2.
    getSummary(jaspContainer, pos=pos+3, outputVariables[2], planData)
  }

  # OC Curve
  if (options[[outputVariables[3]]]) {
    getOCCurve(jaspContainer, pos=pos+4, outputVariables[3], planData)
  }

  # AOQ Curve (for plans with rectification)
  if (options[[outputVariables[4]]]) {
    getAOQCurve(jaspContainer, pos=pos+5, outputVariables[4], planData, options, type, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ATI Curve (for plans with rectification)
  if (options[[outputVariables[5]]]) {
    getATICurve(jaspContainer, pos=pos+6, outputVariables[5], planData, options, type, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ASN Curve (only for multiple sampling plan)
  if (options[[outputVariables[6]]]) {
    getASNCurve(jaspContainer, pos=pos+7, outputVariables[6], planData, options, n, c, r)
  }
}