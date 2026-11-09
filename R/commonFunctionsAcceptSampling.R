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

# Exported helpers retain their established argument and return-field names for
# callers outside this module. Their implementations use camelCase local names.

# Build group-first names for the flat single/multiple-plan option contract.
.getPlanOptionNames <- function(optionNames, type, assessment = FALSE) {
  prefix <- switch(type, Single = "singlePlan", Mult = "multiplePlan", "")
  if (assessment)
    prefix <- if (nzchar(prefix)) paste0(prefix, "Assessment") else "planAssessment"
  if (!nzchar(prefix)) return(optionNames)
  paste0(prefix, toupper(substr(optionNames, 1L, 1L)), substring(optionNames, 2L))
}

# UI values are descriptive; AcceptanceSampling retains its own type vocabulary.
.getSamplingDistributionType <- function(distribution) {
  switch(distribution, binomial = "binom", hypergeometric = "hypergeom", poisson = "poisson",
         stop("Unknown sampling distribution: ", distribution, call. = FALSE))
}

##---------------------------------------------------------------
##         Check if values specified for PD are valid.         --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pd_lower {numeric} lower limit for the proportion of non-conforming items.
#' @param pd_upper {numeric} upper limit for the proportion of non-conforming items.
#' @param pd_step {numeric} step size for the sequence from proportionFrom to proportionTo.
#' @seealso checkHypergeom()
#' @examples
#' checkPdErrors(jaspContainer, 0.1, 0.5, 0.01)
##---------------------------------------------------------------
checkPdErrors <- function(jaspContainer, pd_lower, pd_upper, pd_step) {
  proportionFrom <- pd_lower
  proportionTo <- pd_upper
  proportionStep <- pd_step

  if (proportionFrom > proportionTo) {
    jaspContainer$setError(gettext("Lower limit for proportion non-conforming items needs to be smaller than the upper limit."))
    return ()
  }
  if (proportionStep > (proportionTo - proportionFrom)) {
    jaspContainer$setError(gettext("Step size for proportion non-conforming items needs to be smaller than the difference between the upper and lower limits."))
    return ()
  }
  isZero <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - 0) < tol
  }
  if (isZero(proportionStep) && (proportionTo != proportionFrom)) {
    jaspContainer$setError(gettext("Step size of 0 is allowed only if the lower and upper limits of proportion non-conforming items are identical."))
    return ()
  }
}

##--------------------------------------------------------------------------------
##  Check if D*pd values for the hypergeomtric distribution are whole numbers.  --
##--------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pd_vars {vector} Variables to generate a sequence of quality levels. Includes the lower proportion, upper proportion, and step size.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param aql {numeric} (optional) Acceptable Quality Level (AQL), specified as the proportion (0 to 1) of non-conforming items.
#' @param rql {numeric} (optional) Rejectable Quality Level (RQL), specified as the proportion (0 to 1) of non-conforming items.
#' @seealso checkPdErrors()
#' @examples
#' checkHypergeom(jaspContainer, pd_vars, options, "Single", 0.05, 0.15)
##--------------------------------------------------------------------------------
checkHypergeom <- function(jaspContainer, pd_vars, options, type, aql=NULL, rql=NULL) {
  proportionVariables <- pd_vars

  proportionFrom <- options[[proportionVariables[1]]]
  proportionTo <- options[[proportionVariables[2]]]
  proportionStep <- options[[proportionVariables[3]]]
  checkPdErrors(jaspContainer, proportionFrom, proportionTo, proportionStep)
  if (jaspContainer$getError()) {
    return ()
  }
  # This check is only required for hypergeometric distribution.
  if (.getSamplingDistributionType(options[[.getPlanOptionNames("distribution", type)]]) == "hypergeom") {
    pd <- seq(proportionFrom, proportionTo, proportionStep)
    # Add AQL and RQL to the quality range.
    if (!is.null(aql) && !is.null(rql)) {
      pd <- c(pd, aql, rql)
    }

    # Function to check for whole numbers. From R package AcceptanceSampling.
    isWholeNumber <- function(x, tol = .Machine$double.eps^0.5) {
      abs(x - round(x)) < tol
    }

    N <- options[[.getPlanOptionNames("lotSize", type)]]
    D <- N * pd
    if (!all(isWholeNumber(N), isWholeNumber(D))) {
      jaspContainer$setError(gettext("For hypergeometric distribution, N * proportion non-conforming should all be integer values.\n\nCheck the values of N and proportion non-conforming."))
    }
  }
}

##---------------------------------------------------------------
##  Check for errors in single stage attribute sampling plan.  --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param N {numeric} Total size of the lot.
#' @param n {numeric} Sample size for the plan.
#' @param c {numeric} Acceptance number for the plan.
#' @param r {numeric} Rejection number for the plan.
#' @seealso checkErrorsMultiplePlan()
#' @examples
#' checkErrorsSinglePlan(jaspContainer, 1000, 100, 4, 5)
##---------------------------------------------------------------
checkErrorsSinglePlan <- function(jaspContainer, N, n, c, r) {
  if (n > N) {
    jaspContainer$setError(gettext("Sample size (n) cannot be larger than the lot size (N)."))
    return ()
  }
  if (c > n) {
    jaspContainer$setError(gettext("Acceptance number (c) cannot be larger than the sample size (n)."))
    return ()
  }
  if (r > n) {
    jaspContainer$setError(gettext("Rejection number (r) cannot be larger than the sample size (n)."))
    return ()
  }
  if (r <= c) {
    jaspContainer$setError(gettext("Rejection number (r) has to be larger than the acceptance number (c)."))
  }
}

##-----------------------------------------------------------------
##  Check for errors in multiple stage attribute sampling plan.  --
##-----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param N {numeric} Total size of the lot.
#' @param n {vector} Sample sizes for the plan.
#' @param c {vector} Acceptance numbers for the plan.
#' @param r {vector} Rejection numbers for the plan.
#' @seealso checkErrorsSinglePlan()
#' @examples
#' checkErrorsMultiplePlan(jaspContainer, 1000, c(10,20), c(0,4), c(2,5))
##-----------------------------------------------------------------
checkErrorsMultiplePlan <- function(jaspContainer, N, n, c, r) {
  totalSampleSize <- sum(n) # Total sample size for all stages combined
  cumulativeSampleSize <- cumsum(n) # Cumulative sample size at each stage
  stageCount <- length(n)
  if (totalSampleSize > N) {
    jaspContainer$setError(gettext("Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N)."))
    return ()
  }
  if (r[stageCount] != c[stageCount] + 1) {
    jaspContainer$setError(gettext("Final rejection number (r) needs to be 1 more than the final acceptance number (c)."))
    return ()
  }
  if (any(c > cumulativeSampleSize)) {
    jaspContainer$setError(gettext("Acceptance number (c) cannot be larger than the sample size (n)."))
    return ()
  }
  if (any(r > cumulativeSampleSize)) {
    jaspContainer$setError(gettext("Rejection number (r) cannot be larger than the sample size (n)."))
    return ()
  }
  if (any(r <= c)) {
    jaspContainer$setError(gettext("Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c)."))
    return ()
  }
  if (any(c[1:(stageCount-1)] > r[1:(stageCount-1)] - 2)) {
    # Check for r[i] > c[i] + 1 for i in 1:stages-1
    jaspContainer$setError(gettext("For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).\n
                                   Else, subsequent stages become redundant."))
    return ()
  }
  if (any(c != sort(c))) {
    # Check for non-decreasing seqeuence of c
    jaspContainer$setError(gettext("Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence."))
    return ()
  }
  if (any(r != sort(r))) {
    # Check for non-decreasing seqeuence of r
    jaspContainer$setError(gettext("Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence."))
  }
}

##----------------------------------------------------------------
##            Get the plan variables - n, c, and r.             --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @returns list
#' @seealso getPlan()
#' @examples
#' getPlanValues(jaspContainer, options, "Mult")
##----------------------------------------------------------------
getPlanValues <- function(jaspContainer, options, type) {
  n <- c <- r <- NULL
  N <- options[[.getPlanOptionNames("lotSize", type)]]
  if (type == "Single") {
    # Single sampling plan
    n <- options[["singlePlanSampleSize"]]
    c <- options[["singlePlanAcceptanceNumber"]]
    r <- options[["singlePlanRejectionNumber"]]
    # Error checking for single stage plan
    checkErrorsSinglePlan(jaspContainer, N, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  } else {
    # Multiple sampling plan
    stages <- options[["multiplePlanStages"]]
    for (i in 1:length(stages)) {
      n[i] <- stages[[i]]$sampleSize
      c[i] <- stages[[i]]$acceptanceNumber
      r[i] <- stages[[i]]$rejectionNumber
    }
    # Error checking for multiple stage plan
    checkErrorsMultiplePlan(jaspContainer, N, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }
  return (list(n=n,c=c,r=r))
}

##---------------------------------------------------------------
##            Create and return a plan and its data            --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} (optional) Acceptance number(s) for the plan.
#' @param r {vector} (optional) Rejection number(s) for the plan.
#' @param k {numeric} (optional) The required distance in terms of standard deviation, between acceptance limits and the sample mean for a variable sampling plan.
#' @param sd {string} (optional) Status of the historical standard deviation. Possible values are "known" and "unknown".
#' @returns list
#' @seealso getPlanValues()
#' @examples
#' getPlan(jaspContainer, options, "Single", n=20, c=2, r=4)
#' getPlan(jaspContainer, options, "Single", n=8, k=1.2, sd="known")
##---------------------------------------------------------------
getPlan <- function(jaspContainer, options, type, n, c=NULL, r=NULL, k=NULL, sd=NULL) {
  proportionFrom <- options[[.getPlanOptionNames("proportionNonconformingFrom", type)]]
  proportionTo <- options[[.getPlanOptionNames("proportionNonconformingTo", type)]]
  proportionStep <- options[[.getPlanOptionNames("proportionNonconformingStep", type)]]
  # Check for errors in the quality range
  checkPdErrors(jaspContainer, proportionFrom, proportionTo, proportionStep)
  if (jaspContainer$getError()) {
    return ()
  }
  pd <- seq(proportionFrom, proportionTo, proportionStep)

  # If assess plan option is specified, add the AQL and RQL values to quality range.
  if (options[[.getPlanOptionNames("", type, assessment = TRUE)]]) {
    pd <- c(pd, options[[.getPlanOptionNames("acceptableQualityLevel", type, assessment = TRUE)]], options[[.getPlanOptionNames("rejectableQualityLevel", type, assessment = TRUE)]])
    pd <- sort(pd)
    pd <- pd[!duplicated(pd)]
  }

  # If PD has only <= 1 point(s), add 0 and 1 to the range to make the output more interpretable.
  if (length(pd) <= 1) {
    pd <- c(pd, 0, 1)
    pd <- sort(pd)
    pd <- pd[!duplicated(pd)]
  }
  # For variable plans, sd (whether historical stdev is known) will have a non-null value. Use normal distribution then.
  if (!is.null(sd)) {
    dist <- "normal"
  } else {
    dist <- .getSamplingDistributionType(options[[.getPlanOptionNames("distribution", type)]])
  }
  # Create an OC2c or OCvar object for the plan using the AcceptanceSampling package
  operatingCharacteristic <- NULL
  if (dist == "hypergeom") {
    N <- options[[.getPlanOptionNames("lotSize", type)]]
    operatingCharacteristic <- AcceptanceSampling::OC2c(N = N, n = n, c = c, r = r, type = dist, pd = pd)
  } else if (dist == "binom" || dist == "poisson") {
    operatingCharacteristic <- AcceptanceSampling::OC2c(n = n, c = c, r = r, type = dist, pd = pd)
  } else if (dist == "normal") {
    operatingCharacteristic <- AcceptanceSampling::OCvar(n = n, k = k, type = dist, s.type = sd, pd = pd)
  }
  planData <- data.frame(PD = operatingCharacteristic@pd, PA = operatingCharacteristic@paccept)
  planData <- na.omit(planData)
  if (nrow(planData) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  return (list(oc_plan=operatingCharacteristic, df_plan=planData))
}

##---------------------------------------------------------------------------------------
##  Check if the plan can satisfy the AQL and RQL constraints. Create tabular output.  --
##---------------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param oc_plan {object} An object from the AcceptanceSampling::OC2c class family (OCbinomial / OChypergeom / OCpoisson).
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
##---------------------------------------------------------------------------------------
assessPlan <- function(jaspContainer, pos, depend_vars, oc_plan, options, type) {
  dependencies <- depend_vars
  operatingCharacteristic <- oc_plan

  aql <- options[[.getPlanOptionNames("acceptableQualityLevel", type, assessment = TRUE)]]
  producerAcceptanceProbability <- 1 - options[[.getPlanOptionNames("producerRisk", type, assessment = TRUE)]]
  rql <- options[[.getPlanOptionNames("rejectableQualityLevel", type, assessment = TRUE)]]
  consumerAcceptanceProbability <- options[[.getPlanOptionNames("consumerRisk", type, assessment = TRUE)]]

  # Error handling for AQL/RQL
  if (aql >= rql) {
    jaspContainer$setError(gettext("AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  if (producerAcceptanceProbability <= consumerAcceptanceProbability) {
    jaspContainer$setError(gettext("1 - \u03B1 (producer's risk) has to be greater than \u03B2 (consumer's risk)."))
    return ()
  }

  # Assessment of the sampling plan
  assess <- AcceptanceSampling::assess(operatingCharacteristic, PRP = c(aql, producerAcceptanceProbability), CRP = c(rql, consumerAcceptanceProbability))
  producerActualAcceptanceProbability <- assess$PRP[3]
  consumerActualAcceptanceProbability <- assess$CRP[3]
  # Table with the specified and actual acceptance probabilities
  if (!is.null(jaspContainer[["riskTable"]])) {
    return ()
  }
  table <- createJaspTable(title = gettextf("Current plan <b>CAN %s</b> meet the specified risk point(s).", ifelse(assess$OK, "", "NOT")))
  table$dependOn(dependencies)
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = gettext("Proportion Non-conforming"), type = "number")
  table$addColumnInfo(name = "col_3", title = gettext("Required P(accept)"), type = "number")
  table$addColumnInfo(name = "col_4", title = gettext("Actual P(accept)"), type = "number")
  table$addRows(list("col_1" = "AQL", "col_2" = aql, "col_3" = producerAcceptanceProbability, "col_4" = producerActualAcceptanceProbability))
  table$addRows(list("col_1" = "RQL", "col_2" = rql, "col_3" = consumerAcceptanceProbability, "col_4" = consumerActualAcceptanceProbability))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- pos
  jaspContainer[["riskTable"]] <- table

  # If the current plan CANNOT satisfy the constraints, provide explanation.
  if (!assess$OK) {
    if (producerActualAcceptanceProbability < producerAcceptanceProbability) {
	  text <- gettextf("Probability of acceptance (%1$.3f) at AQL (%2$.3f) is <b>lower</b> than the required probability of acceptance (%3$.3f) at AQL.", producerActualAcceptanceProbability, aql, producerAcceptanceProbability)
    } else if (consumerActualAcceptanceProbability > consumerAcceptanceProbability) {
	  text <- gettextf("Probability of acceptance (%1$.3f) at RQL (%2$.3f) is <b>higher</b> than the required probability of acceptance (%3$.3f) at RQL.", consumerActualAcceptanceProbability, rql, consumerAcceptanceProbability)
    }
    explanation <- createJaspHtml(text = text, position=pos+1, dependencies=dependencies)
    if (is.null(jaspContainer[["explanation"]])) {
      jaspContainer[["explanation"]] <- explanation
    }
  }
}

##---------------------------------------------------------------
##            Generate a summary table for the plan            --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
##---------------------------------------------------------------
getSummary <- function(jaspContainer, pos, depend_vars, df_plan) {
  dependencies <- depend_vars
  planData <- df_plan

  if (!is.null(jaspContainer[["summaryTable"]])) {
    return()
  }
  summaryTable <- createJaspTable(title = gettext("Acceptance Probabilities"))
  summaryTable$dependOn(dependencies)
  summaryTable$addColumnInfo(name = "col_1", title = gettext("Prop. Non-conforming"), type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = gettext("P(accept)"), type = "number")
  summaryTable$setData(list(col_1 = round(planData$PD,6), col_2 = round(planData$PA,6)))
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- pos
  jaspContainer[["summaryTable"]] <- summaryTable
}

##----------------------------------------------------------------
##  Generate the operating characteristics curve for the plan.  --
##----------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
##----------------------------------------------------------------
getOCCurve <- function(jaspContainer, pos, depend_vars, df_plan) {
  dependencies <- depend_vars
  planData <- df_plan

  if (!is.null(jaspContainer[["ocCurve"]])) {
    return()
  }
  planData$PD <- round(planData$PD, 3)
  planData$PA <- round(planData$PA, 3)
  ocCurve <- createJaspPlot(title = gettext("OC (Operating Characteristics) Curve"),  width = 480, height = 320)
  ocCurve$dependOn(dependencies)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(planData$PD), max(planData$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(planData$PA), max(planData$PA)))
  plt <- ggplot2::ggplot(data = planData, ggplot2::aes(x = PD, y = PA)) +
                  ggplot2::geom_point(colour = "black", shape = 19) +
                  ggplot2::geom_line(colour = "black", linetype = "dashed") +
                  ggplot2::labs(x = gettext("Proportion Non-conforming"), y = gettext("Probability of Acceptance")) +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  ocCurve$plotObject <- plt
  ocCurve$position <- pos
  jaspContainer[["ocCurve"]] <- ocCurve
}

##------------------------------------------------------------------------------
##  Generate the average outgoing quality curve for plan with rectification.  --
##------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} (optional) Acceptance number(s) for the plan.
#' @param r {vector} (optional) Rejection number(s) for the plan.
#' @seealso getATICurve()
##------------------------------------------------------------------------------
getAOQCurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n, c=NULL, r=NULL) {
  dependencies <- depend_vars
  planData <- df_plan

  if (!is.null(jaspContainer[["aoqCurve"]])) {
    return ()
  }
  aoqCurve <- createJaspPlot(title = gettext("AOQ (Average Outgoing Quality) Curve"), width = 480, height = 320)
  aoqCurve$dependOn(dependencies)
  jaspContainer[["aoqCurve"]] <- aoqCurve

  N <- options[[.getPlanOptionNames("lotSize", type)]]
  pd <- planData$PD
  AOQ <- numeric(length(pd))

  # Straightforward calculation for single stage plan.
  if (type == "Single" || type == "") {
    AOQ <- planData$PA * pd * (N-n) / N
  } else {
    # Multiple plan - need to compute stagewise probabilities.
    dist <- .getSamplingDistributionType(options[[.getPlanOptionNames("distribution", type)]])
    stages <- length(n) # Number of sampling stages
    totalSampleSize <- cumsum(n)
    stageProbabilities <- getStageProbability(pd, n, c, r, dist, N)
    if (is.null(stageProbabilities)) {
      aoqCurve$setError(gettext("Can not calculate AOQ. Check the plan parameters."))
      return ()
    }
    stageProbabilities <- stageProbabilities[[1]] # We only need acceptance probability.
    for (i in 1:stages) {
      stageAcceptanceProbability <- stageProbabilities[i,] # Acceptance probability for ith stage
      AOQ <- AOQ + (stageAcceptanceProbability * (N - totalSampleSize[i])) # Incremental AOQ till ith stage
    }
    # This part is common for every stage, so do it at the end.
    AOQ <- AOQ * pd / N
  }
  AOQ <- round(AOQ, 3)
  planData$AOQ <- AOQ
  planData$PD <- round(planData$PD, 3)
  planData <- na.omit(planData)
  if (nrow(planData) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  # AOQL (Average Outgoing Quality Limit)
  aoql <- max(planData$AOQ)
  # pd_aoql <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(planData$PD), max(planData$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(planData$AOQ), 1.2*aoql))
  plt <- ggplot2::ggplot(data = planData, ggplot2::aes(x = PD, y = AOQ)) +
         ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = gettext("(Incoming) Proportion Non-conforming"), y = gettext("Average Outgoing Quality")) +
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::geom_hline(yintercept = aoql, linetype = "dotted") +
         ggplot2::annotate("text", label = gettextf("AOQL: %.3f", aoql),
                           x = (min(planData$PD) + max(planData$PD)) / 2, y = aoql*1.1, color = "black", size = 6) +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
        # ggplot2::ylim(0.0,round(aoql*1.2, 2))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  aoqCurve$plotObject <- plt
}

##---------------------------------------------------------------
##  Generate the average total inspection curve for the plan.  --
##---------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
#' @param options {list} A named list of interface options selected by the user.
#' @param type {string} Sampling plan type. Possible values are "Single", "", and "Mult".
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} (optional) Acceptance number(s) for the plan.
#' @param r {vector} (optional) Rejection number(s) for the plan.
#' @seealso getAOQCurve()
##---------------------------------------------------------------
getATICurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n, c=NULL, r=NULL) {
  dependencies <- depend_vars
  planData <- df_plan

  if (!is.null(jaspContainer[["atiCurve"]])) {
    return ()
  }
  atiCurve <- createJaspPlot(title = gettext("ATI (Average Total Inspection) Curve"), width = 480, height = 320)
  atiCurve$dependOn(dependencies)
  jaspContainer[["atiCurve"]] <- atiCurve

  N <- options[[.getPlanOptionNames("lotSize", type)]]
  pd <- planData$PD
  ATI <- numeric(length(pd))

  # Straightforward calculation for single stage plan.
  if (type == "Single" || type == "") {
    ATI <- planData$PA * n + (1 - planData$PA) * N
  } else {
    # Multiple plan - need to compute stagewise probabilities.
    dist <- .getSamplingDistributionType(options[[.getPlanOptionNames("distribution", type)]])
    stages <- length(n) # Number of sampling stages
    totalSampleSize <- cumsum(n)
    stageProbabilities <- getStageProbability(pd, n, c, r, dist, N)
    if (is.null(stageProbabilities)) {
      atiCurve$setError(gettext("Can not calculate ATI. Check the plan parameters."))
      return ()
    }
    acceptanceProbabilities <- stageProbabilities[[1]] # Acceptance probabilities
    rejectionProbabilities <- stageProbabilities[[2]] # Rejection probabilities
    for (i in 1:stages) {
      stageAcceptanceProbability <- acceptanceProbabilities[i,] # Acceptance probability for ith stage
      ATI <- ATI + (stageAcceptanceProbability * totalSampleSize[i]) # Incremental ATI till ith stage
    }
    # This part is common for every stage, so do it at the end.
    # For any stage, if lot gets rejected, all N items are inspected under rectification plan.
    ATI <- ATI + N * colSums(rejectionProbabilities)
  }
  ATI <- round(ATI, 3)
  planData$ATI <- ATI
  planData$PD <- round(planData$PD, 3)
  planData <- na.omit(planData)
  if (nrow(planData) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
    return ()
  }
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(planData$PD)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(planData$ATI)
  plt <- ggplot2::ggplot(data = planData, ggplot2::aes(x = PD, y = ATI)) +
         ggplot2::geom_point(colour = "black", shape = 19) +
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = gettext("Proportion Non-conforming"), y = gettext("Average Total Inspection")) +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
         # ggplot2::scale_y_continuous(breaks = pretty(df_plan$ATI))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  atiCurve$plotObject <- plt
}

##---------------------------------------------------------------------------------------------------------
##  Generate the average sample number curve for the plan. Only applicable for multiple sampling plans.  --
##---------------------------------------------------------------------------------------------------------
#' @param jaspContainer {list} A functional grouping of different output elements such as plots, tables, etc.
#' @param pos {numeric} Position of the output element in the output display.
#' @param depend_vars {vector} Names of variables on which the output element depends.
#' @param df_plan {data.frame} Dataframe for a plan with quality levels (PD) and the corresponding probabilities of acceptance (PA).
#' @param options {list} A named list of interface options selected by the user.
#' @param n {vector} Sample sizes for the plan.
#' @param c {vector} Acceptance numbers for the plan.
#' @param r {vector} Rejection numbers for the plan.
##---------------------------------------------------------------------------------------------------------
getASNCurve <- function(jaspContainer, pos, depend_vars, df_plan, options, n, c, r) {
  dependencies <- depend_vars
  planData <- df_plan

  if (!is.null(jaspContainer[["asnCurve"]])) {
    return ()
  }
  asnCurve <- createJaspPlot(title = gettext("ASN (Average Sample Number) Curve"), width = 480, height = 320)
  asnCurve$dependOn(dependencies)
  jaspContainer[["asnCurve"]] <- asnCurve

  # Parse option values
  dist <- .getSamplingDistributionType(options$multiplePlanDistribution)
  N <- options$multiplePlanLotSize
  pd <- planData$PD
  stages <- length(n)
  valueCount <- length(pd)
  ASN <- numeric(valueCount)
  totalSampleSize <- cumsum(n)
  stageProbabilities <- getStageProbability(pd, n, c, r, dist, N)
  if (is.null(stageProbabilities)) {
    jaspContainer$setError(gettext("Can not calculate ASN. Check the plan parameters."))
    return ()
  }
  stageProbabilities <- stageProbabilities[[1]] + stageProbabilities[[2]] # Decision prob = p_acc + p_rej
  for (i in 1:stages) {
    stageDecisionProbability <- stageProbabilities[i,]
    ASN <- ASN + stageDecisionProbability * totalSampleSize[i]
  }
  ASN <- round(ASN, 3)
  planData$ASN <- ASN
  planData$PD <- round(planData$PD, 3)
  planData <- na.omit(planData)
  if (nrow(planData) == 0) {
    jaspContainer$setError(gettext("No valid values found in the plan. Check the inputs."))
  }

  # Draw ASN plot
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(planData$PD), max(planData$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(planData$ASN), max(planData$ASN)))
  plt <- ggplot2::ggplot(data = planData, ggplot2::aes(x = PD, y = ASN)) +
         ggplot2::geom_point(colour = "black", shape = 19) +
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = gettext("Proportion Non-conforming"), y = gettext("Average Sample Number")) +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  asnCurve$plotObject <- plt
}

##-------------------------------------------------------------------------------------------
##  Helper function to get stagewise acceptance and rejection probabilities for the plan.  --
##-------------------------------------------------------------------------------------------
#' @param pd {vector} Range of quality levels over which to evaluate the plan, each value indicating a proportion (0 to 1) of non-conforming items.
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} Acceptance number(s) for the plan.
#' @param r {vector} Rejection number(s) for the plan.
#' @param dist {string} The type of distribution of non-conforming items in the sample. Possible values are "binom", "hypergeom", and "poisson".
#' @param N {numeric} (optional) Total size of the lot.
#' @returns list
#' @seealso getStageProbability()
#' @examples
#' getStageProbabilityHelper(pd, n=c(20,30), c=c(4,10), r=c(7,11), dist="hypergeom", N=500)
#' @notes
#' Part of the code in this function has been re-used from the R package AcceptanceSampling written by Andreas Kiermeier.
#' Specifically, from the following functions:
#' 1) calc.OCbinomial.pdi 2) calc.OChypergeom.pdi 3) calc.OCpoisson.pdi
#' https://github.com/cran/AcceptanceSampling/blob/master/R/code_twoclass.R
#' @importFrom stats dbinom pbinom dpois ppois dhyper phyper
##-------------------------------------------------------------------------------------------
getStageProbabilityHelper <- function(pd, n, c, r, dist, N=10000) {
  D <- pd * N # Number of defects = quality level * lot size
  stageCount <- length(n)
  acceptanceProbabilities <- matrix(nrow = stageCount, ncol = length(pd))
  rejectionProbabilities <- matrix(nrow = stageCount, ncol = length(pd))
  stageCount <- stageCount # number of stages in this sampling

  # Function to calculate acceptance probability
  getAcceptanceProbability <- function(x, n, p, dist, N=1000) {
    k <- length(x)
    k1 <- k-2
    if (dist == "binom") {
      # k = number of this stage + 2.
      # x[1:k1] will give all values until the last stage before this one.
      # n[1:k1] does the same for n.
      prob <- prod(dbinom(x[1:k1], n[1:k1], p)) * pbinom(x[k-1], n[k-1], p)
    } else if (dist == "poisson") {
      prob <- prod(dpois(x[1:k1], n[1:k1]*p)) * ppois(x[k-1], n[k-1]*p)
    } else if (dist == "hypergeom") {
      cumulativeDefects <- cumsum(x[1:(k-1)])
      cumulativeSampleSize <- cumsum(n)
      remainingLotSize <- N - c(0, cumulativeSampleSize[1:(k-1)])
      remainingDefects <- D - c(0, cumulativeDefects[1:(k-1)])

      prob <- prod(dhyper(x=x[1:k1], m=pmax(remainingDefects[1:k1], 0), n = remainingLotSize[1:k1] - pmax(remainingDefects[1:k1], 0), k=n[1:k1])) *
                   phyper(q=x[k-1], m=pmax(remainingDefects[k-1], 0), n = remainingLotSize[k-1] - pmax(remainingDefects[k-1], 0), k=n[k-1])
    }
    return (prob)
  }
  # Function to calculate rejection probability
  getRejectionProbability <- function(x, n, p, dist, N=1000) {
    k <- length(x)
    k1 <- k-2
    if (dist == "binom") {
      prob <- prod(dbinom(x[1:k1], n[1:k1], p)) * pbinom(x[k], n[k-1], p, lower.tail = FALSE)
    } else if (dist == "poisson") {
      prob <- prod(dpois(x[1:k1], n[1:k1]*p)) * ppois(x[k], n[k-1]*p, lower.tail = FALSE)
    } else if (dist == "hypergeom") {
      cumulativeDefects <- cumsum(x[c(1:(k-2), k)])
      cumulativeSampleSize <- cumsum(n)
      remainingLotSize <- N - c(0,cumulativeSampleSize[1:(k-2)])
      remainingDefects <- D - c(0,cumulativeDefects[1:(k-2)])
      prob <- prod(dhyper(x=x[1:k1], m=pmax(remainingDefects[1:k1], 0), n = remainingLotSize[1:k1] - pmax(remainingDefects[1:k1], 0), k=n[1:k1])) *
                   phyper(q=x[k], m=pmax(remainingDefects[k-1], 0), n = remainingLotSize[k-1] - pmax(remainingDefects[k-1], 0), k=n[k-1], lower.tail = FALSE)
    }
    return (prob)
  }

  for (k in 1:stageCount) {
    # For each stage, find out all the possibilities which could lead to still not having made a decision,
    # and then calculate the appropriate probabilities.

    if (k == 1) {
      # Only a single sampling stage
      acceptanceProbability <- sapply(pd, FUN = function(el) {
        if (dist == "binom") {
          return (pbinom(q=c[1], size=n[1], prob=el))
        } else if (dist == "poisson") {
          return (ppois(q=c[1], lambda=n[1]*el))
        } else if (dist == "hypergeom") {
          el <- el * N
          return (phyper(q=c[1], m=el, n=N-el, k=n[1]))
        }
      })
      acceptanceProbabilities[k,] = acceptanceProbability
      rejectionProbability <- sapply(pd, FUN = function(el) {
        if (dist == "binom") {
          return (pbinom(q=r[1]-1, size=n[1], prob=el, lower.tail = FALSE))
        } else if (dist == "poisson") {
          return (ppois(q=r[1]-1, lambda = n[1]*el, lower.tail = FALSE))
        } else if (dist == "hypergeom") {
          el <- el * N
          return (phyper(q=r[1]-1, m=el, n=N-el, k=n[1], lower.tail = FALSE))
        }
      })
      rejectionProbabilities[k,] <- rejectionProbability
      # p.acc and p.rej now exist and can be used in the following stages.
    }
    else if (k == 2) {
      # Two sampling stages. Needs to be handled separately from
      # more stages due to matrix dimensions
      continuationLower <- c+1 # Use to calculate limits
      continuationUpper <- r-1 # Use to calculate limits
      # The possibilities which lead to a decision to be made at the second stage
      x <- data.frame(X1 = seq(continuationLower[1], continuationUpper[1], by=1),
                      acceptanceRemainder = c[2]-seq(continuationLower[1], continuationUpper[1], by=1),
                      rejectionRemainder = r[2]-1-seq(continuationLower[1], continuationUpper[1], by=1))
      secondStageAcceptanceProbability <- sum(apply(x, 1, FUN=getAcceptanceProbability, n=n, p=pd, dist=dist, N=N))
      acceptanceProbability <- acceptanceProbability + secondStageAcceptanceProbability
      acceptanceProbabilities[k,] <- secondStageAcceptanceProbability
      secondStageRejectionProbability <- sum(apply(x, 1, FUN=getRejectionProbability, n=n, p=pd, dist=dist, N=N))
      rejectionProbability <- rejectionProbability + secondStageRejectionProbability
      rejectionProbabilities[k,] <- secondStageRejectionProbability
    }
    else {
      # More than two sampling stages.
      continuationLower <- c+1 # Use to calculate limits
      continuationUpper <- r-1 # Use to calculate limits
      expansionCall <- "expand.grid(continuationLower[k-1]:continuationUpper[k-1]"
      for (i in 2:(k-1)) {
        expansionCall <- paste(expansionCall, paste("continuationLower[k-",i,"]:continuationUpper[k-",i,"]", sep=""), sep=",")
      }
      expansionCall <- paste(expansionCall,")", sep="")
      x <- eval(parse(text=expansionCall)[[1]])
      x <- x[,(k-1):1] # Reverses the order of columns in dataframe x
      names(x) <- paste("X", 1:(k-1), sep="")

      for (i in ncol(x):2) {
        x[,i] <- x[,i] - x[,i-1]
      }
      x <- cbind(x, acceptanceRemainder = c[k] - rowSums(x[,1:(k-1)]))
      x <- cbind(x, rejectionRemainder = r[k]-1 - rowSums(x[,1:(k-1)]))
      stageAcceptanceProbability <- sum(apply(x, 1, FUN=getAcceptanceProbability, n=n, p=pd, dist=dist, N=N))
      acceptanceProbability <- acceptanceProbability + stageAcceptanceProbability
      acceptanceProbabilities[k,] <- stageAcceptanceProbability # Acceptance probability for the kth stage
      stageRejectionProbability <- sum(apply(x, 1, FUN=getRejectionProbability, n=n, p=pd, dist=dist, N=N))
      rejectionProbability <- rejectionProbability + stageRejectionProbability
      rejectionProbabilities[k,] <- stageRejectionProbability # Rejection probability for the kth stage
    }
  }
  return(list(acceptanceProbabilities,rejectionProbabilities))
}

##------------------------------------------------------------------------
##  Get stagewise acceptance and rejection probabilities for the plan.  --
##------------------------------------------------------------------------
#' @param pd {vector} Range of quality levels over which to evaluate the plan, each value indicating a proportion (0 to 1) of non-conforming items.
#' @param n {vector} Sample size(s) for the plan.
#' @param c {vector} Acceptance number(s) for the plan.
#' @param r {vector} Rejection number(s) for the plan.
#' @param dist {string} The type of distribution of non-conforming items in the sample. Possible values are "binom", "hypergeom", and "poisson".
#' @param N {numeric} (optional) Total size of the lot.
#' @returns list
#' @seealso getStageProbabilityHelper()
#' @examples
#' getStageProbability(pd, n=c(10,20,30), c=c(2,5,8), r=c(4,7,9), dist="binom", N=1000)
#' @notes
#' The calculation of stagewise probability is done independently for every value in the vector 'pd', which has a range of quality levels.
##------------------------------------------------------------------------
getStageProbability <- function(pd, n, c, r, dist, N=10000) {
  stageProbabilities <- vapply(pd, FUN=getStageProbabilityHelper, n=n, c=c, r=r, dist=dist, N=N, FUN.VALUE=list(matrix,matrix))
  acc <- matrix(unlist(stageProbabilities[1,]), byrow=FALSE, nrow=length(n))
  rej <- matrix(unlist(stageProbabilities[2,]), byrow=FALSE, nrow=length(n))
  return (list(acc,rej))
}
