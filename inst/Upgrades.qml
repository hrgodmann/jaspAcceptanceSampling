import QtQuick
import JASP.Module

Upgrades
{
	// The upgrader selects this step for saved module versions 0.96.4 or older.
	// Keep newer/current option values if a file already contains both names.
	Upgrade
	{
		functionName: "CreateAttributePlan"
		fromVersion: "0.96.4"
		toVersion: "0.96.5"

		ChangeRename
		{
			from: "aql"; to: "acceptableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("aql") && !options.hasOwnProperty("acceptableQualityLevel"); }
		}
		ChangeRename
		{
			from: "rql"; to: "rejectableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("rql") && !options.hasOwnProperty("rejectableQualityLevel"); }
		}
		ChangeRename
		{
			from: "prod_risk"; to: "producerRisk"
			condition: function(options) { return options.hasOwnProperty("prod_risk") && !options.hasOwnProperty("producerRisk"); }
		}
		ChangeRename
		{
			from: "cons_risk"; to: "consumerRisk"
			condition: function(options) { return options.hasOwnProperty("cons_risk") && !options.hasOwnProperty("consumerRisk"); }
		}
		ChangeRename
		{
			from: "pd_lower"; to: "proportionNonconformingFrom"
			condition: function(options) { return options.hasOwnProperty("pd_lower") && !options.hasOwnProperty("proportionNonconformingFrom"); }
		}
		ChangeRename
		{
			from: "pd_upper"; to: "proportionNonconformingTo"
			condition: function(options) { return options.hasOwnProperty("pd_upper") && !options.hasOwnProperty("proportionNonconformingTo"); }
		}
		ChangeRename
		{
			from: "pd_step"; to: "proportionNonconformingStep"
			condition: function(options) { return options.hasOwnProperty("pd_step") && !options.hasOwnProperty("proportionNonconformingStep"); }
		}
		ChangeRename
		{
			from: "showSummary"; to: "summaryTable"
			condition: function(options) { return options.hasOwnProperty("showSummary") && !options.hasOwnProperty("summaryTable"); }
		}
		ChangeRename
		{
			from: "showOCCurve"; to: "operatingCharacteristicCurve"
			condition: function(options) { return options.hasOwnProperty("showOCCurve") && !options.hasOwnProperty("operatingCharacteristicCurve"); }
		}
		ChangeRename
		{
			from: "showAOQCurve"; to: "averageOutgoingQualityCurve"
			condition: function(options) { return options.hasOwnProperty("showAOQCurve") && !options.hasOwnProperty("averageOutgoingQualityCurve"); }
		}
		ChangeRename
		{
			from: "showATICurve"; to: "averageTotalInspectionCurve"
			condition: function(options) { return options.hasOwnProperty("showATICurve") && !options.hasOwnProperty("averageTotalInspectionCurve"); }
		}
		ChangeRename
		{
			from: "showASNCurve"; to: "averageSampleNumberCurve"
			condition: function(options) { return options.hasOwnProperty("showASNCurve") && !options.hasOwnProperty("averageSampleNumberCurve"); }
		}

		ChangeJS
		{
			name: "distribution"
			condition: function(options) { return options.hasOwnProperty("distribution"); }
			jsFunction: function(options)
			{
				// Metadata objects pass through unchanged when JASP also applies this change to .meta.
				switch (options["distribution"])
				{
					case "binom": return "binomial";
					case "hypergeom": return "hypergeometric";
					default: return options["distribution"];
				}
			}
		}
	}

	Upgrade
	{
		functionName: "CreateVariablePlan"
		fromVersion: "0.96.4"
		toVersion: "0.96.5"

		ChangeRename
		{
			from: "aql"; to: "acceptableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("aql") && !options.hasOwnProperty("acceptableQualityLevel"); }
		}
		ChangeRename
		{
			from: "rql"; to: "rejectableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("rql") && !options.hasOwnProperty("rejectableQualityLevel"); }
		}
		ChangeRename
		{
			from: "prod_risk"; to: "producerRisk"
			condition: function(options) { return options.hasOwnProperty("prod_risk") && !options.hasOwnProperty("producerRisk"); }
		}
		ChangeRename
		{
			from: "cons_risk"; to: "consumerRisk"
			condition: function(options) { return options.hasOwnProperty("cons_risk") && !options.hasOwnProperty("consumerRisk"); }
		}
		ChangeRename
		{
			from: "pd_lower"; to: "proportionNonconformingFrom"
			condition: function(options) { return options.hasOwnProperty("pd_lower") && !options.hasOwnProperty("proportionNonconformingFrom"); }
		}
		ChangeRename
		{
			from: "pd_upper"; to: "proportionNonconformingTo"
			condition: function(options) { return options.hasOwnProperty("pd_upper") && !options.hasOwnProperty("proportionNonconformingTo"); }
		}
		ChangeRename
		{
			from: "pd_step"; to: "proportionNonconformingStep"
			condition: function(options) { return options.hasOwnProperty("pd_step") && !options.hasOwnProperty("proportionNonconformingStep"); }
		}
		ChangeRename
		{
			from: "showSummary"; to: "summaryTable"
			condition: function(options) { return options.hasOwnProperty("showSummary") && !options.hasOwnProperty("summaryTable"); }
		}
		ChangeRename
		{
			from: "showOCCurve"; to: "operatingCharacteristicCurve"
			condition: function(options) { return options.hasOwnProperty("showOCCurve") && !options.hasOwnProperty("operatingCharacteristicCurve"); }
		}
		ChangeRename
		{
			from: "showAOQCurve"; to: "averageOutgoingQualityCurve"
			condition: function(options) { return options.hasOwnProperty("showAOQCurve") && !options.hasOwnProperty("averageOutgoingQualityCurve"); }
		}
		ChangeRename
		{
			from: "showATICurve"; to: "averageTotalInspectionCurve"
			condition: function(options) { return options.hasOwnProperty("showATICurve") && !options.hasOwnProperty("averageTotalInspectionCurve"); }
		}
		ChangeRename
		{
			from: "showASNCurve"; to: "averageSampleNumberCurve"
			condition: function(options) { return options.hasOwnProperty("showASNCurve") && !options.hasOwnProperty("averageSampleNumberCurve"); }
		}
		ChangeRename
		{
			from: "sd"; to: "historicalSdKnown"
			condition: function(options) { return options.hasOwnProperty("sd") && !options.hasOwnProperty("historicalSdKnown"); }
		}
	}

	Upgrade
	{
		functionName: "AnalyzeVariablePlan"
		fromVersion: "0.96.4"
		toVersion: "0.96.5"

		ChangeRename
		{
			from: "aql"; to: "planAssessmentAcceptableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("aql") && !options.hasOwnProperty("planAssessmentAcceptableQualityLevel"); }
		}
		ChangeRename
		{
			from: "rql"; to: "planAssessmentRejectableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("rql") && !options.hasOwnProperty("planAssessmentRejectableQualityLevel"); }
		}
		ChangeRename
		{
			from: "prod_risk"; to: "planAssessmentProducerRisk"
			condition: function(options) { return options.hasOwnProperty("prod_risk") && !options.hasOwnProperty("planAssessmentProducerRisk"); }
		}
		ChangeRename
		{
			from: "cons_risk"; to: "planAssessmentConsumerRisk"
			condition: function(options) { return options.hasOwnProperty("cons_risk") && !options.hasOwnProperty("planAssessmentConsumerRisk"); }
		}
		ChangeRename
		{
			from: "pd_lower"; to: "proportionNonconformingFrom"
			condition: function(options) { return options.hasOwnProperty("pd_lower") && !options.hasOwnProperty("proportionNonconformingFrom"); }
		}
		ChangeRename
		{
			from: "pd_upper"; to: "proportionNonconformingTo"
			condition: function(options) { return options.hasOwnProperty("pd_upper") && !options.hasOwnProperty("proportionNonconformingTo"); }
		}
		ChangeRename
		{
			from: "pd_step"; to: "proportionNonconformingStep"
			condition: function(options) { return options.hasOwnProperty("pd_step") && !options.hasOwnProperty("proportionNonconformingStep"); }
		}
		ChangeRename
		{
			from: "showSummary"; to: "summaryTable"
			condition: function(options) { return options.hasOwnProperty("showSummary") && !options.hasOwnProperty("summaryTable"); }
		}
		ChangeRename
		{
			from: "showOCCurve"; to: "operatingCharacteristicCurve"
			condition: function(options) { return options.hasOwnProperty("showOCCurve") && !options.hasOwnProperty("operatingCharacteristicCurve"); }
		}
		ChangeRename
		{
			from: "showAOQCurve"; to: "averageOutgoingQualityCurve"
			condition: function(options) { return options.hasOwnProperty("showAOQCurve") && !options.hasOwnProperty("averageOutgoingQualityCurve"); }
		}
		ChangeRename
		{
			from: "showATICurve"; to: "averageTotalInspectionCurve"
			condition: function(options) { return options.hasOwnProperty("showATICurve") && !options.hasOwnProperty("averageTotalInspectionCurve"); }
		}
		ChangeRename
		{
			from: "showASNCurve"; to: "averageSampleNumberCurve"
			condition: function(options) { return options.hasOwnProperty("showASNCurve") && !options.hasOwnProperty("averageSampleNumberCurve"); }
		}
		ChangeRename
		{
			from: "sd"; to: "historicalSdKnown"
			condition: function(options) { return options.hasOwnProperty("sd") && !options.hasOwnProperty("historicalSdKnown"); }
		}
		ChangeRename
		{
			from: "assessPlan"; to: "planAssessment"
			condition: function(options) { return options.hasOwnProperty("assessPlan") && !options.hasOwnProperty("planAssessment"); }
		}
	}

	Upgrade
	{
		functionName: "DecideVariableLots"
		fromVersion: "0.96.4"
		toVersion: "0.96.5"

		ChangeRename
		{
			from: "aql"; to: "acceptableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("aql") && !options.hasOwnProperty("acceptableQualityLevel"); }
		}
		ChangeRename
		{
			from: "rql"; to: "rejectableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("rql") && !options.hasOwnProperty("rejectableQualityLevel"); }
		}
		ChangeRename
		{
			from: "variables"; to: "measurement"
			condition: function(options) { return options.hasOwnProperty("variables") && !options.hasOwnProperty("measurement"); }
		}
		ChangeRename
		{
			from: "sampleStats"; to: "sampleStatistics"
			condition: function(options) { return options.hasOwnProperty("sampleStats") && !options.hasOwnProperty("sampleStatistics"); }
		}
		ChangeRename
		{
			from: "sampleSD"; to: "sampleSd"
			condition: function(options) { return options.hasOwnProperty("sampleSD") && !options.hasOwnProperty("sampleSd"); }
		}
		ChangeRename
		{
			from: "lsl"; to: "lowerSpecificationLimit"
			condition: function(options) { return options.hasOwnProperty("lsl") && !options.hasOwnProperty("lowerSpecificationLimit"); }
		}
		ChangeRename
		{
			from: "lower_spec"; to: "lowerSpecificationLimitValue"
			condition: function(options) { return options.hasOwnProperty("lower_spec") && !options.hasOwnProperty("lowerSpecificationLimitValue"); }
		}
		ChangeRename
		{
			from: "usl"; to: "upperSpecificationLimit"
			condition: function(options) { return options.hasOwnProperty("usl") && !options.hasOwnProperty("upperSpecificationLimit"); }
		}
		ChangeRename
		{
			from: "upper_spec"; to: "upperSpecificationLimitValue"
			condition: function(options) { return options.hasOwnProperty("upper_spec") && !options.hasOwnProperty("upperSpecificationLimitValue"); }
		}
		ChangeRename
		{
			from: "sd"; to: "historicalSdKnown"
			condition: function(options) { return options.hasOwnProperty("sd") && !options.hasOwnProperty("historicalSdKnown"); }
		}
		ChangeRename
		{
			from: "stdev"; to: "historicalSd"
			condition: function(options) { return options.hasOwnProperty("stdev") && !options.hasOwnProperty("historicalSd"); }
		}
	}

	Upgrade
	{
		functionName: "AnalyzeAttributePlan"
		fromVersion: "0.96.4"
		toVersion: "0.96.5"

		ChangeRename
		{
			from: "pd_lowerSingle"; to: "singlePlanProportionNonconformingFrom"
			condition: function(options) { return options.hasOwnProperty("pd_lowerSingle") && !options.hasOwnProperty("singlePlanProportionNonconformingFrom"); }
		}
		ChangeRename
		{
			from: "pd_upperSingle"; to: "singlePlanProportionNonconformingTo"
			condition: function(options) { return options.hasOwnProperty("pd_upperSingle") && !options.hasOwnProperty("singlePlanProportionNonconformingTo"); }
		}
		ChangeRename
		{
			from: "pd_stepSingle"; to: "singlePlanProportionNonconformingStep"
			condition: function(options) { return options.hasOwnProperty("pd_stepSingle") && !options.hasOwnProperty("singlePlanProportionNonconformingStep"); }
		}
		ChangeRename
		{
			from: "showSummarySingle"; to: "singlePlanSummaryTable"
			condition: function(options) { return options.hasOwnProperty("showSummarySingle") && !options.hasOwnProperty("singlePlanSummaryTable"); }
		}
		ChangeRename
		{
			from: "showOCCurveSingle"; to: "singlePlanOperatingCharacteristicCurve"
			condition: function(options) { return options.hasOwnProperty("showOCCurveSingle") && !options.hasOwnProperty("singlePlanOperatingCharacteristicCurve"); }
		}
		ChangeRename
		{
			from: "showAOQCurveSingle"; to: "singlePlanAverageOutgoingQualityCurve"
			condition: function(options) { return options.hasOwnProperty("showAOQCurveSingle") && !options.hasOwnProperty("singlePlanAverageOutgoingQualityCurve"); }
		}
		ChangeRename
		{
			from: "showATICurveSingle"; to: "singlePlanAverageTotalInspectionCurve"
			condition: function(options) { return options.hasOwnProperty("showATICurveSingle") && !options.hasOwnProperty("singlePlanAverageTotalInspectionCurve"); }
		}
		ChangeRename
		{
			from: "showASNCurveSingle"; to: "singlePlanAverageSampleNumberCurve"
			condition: function(options) { return options.hasOwnProperty("showASNCurveSingle") && !options.hasOwnProperty("singlePlanAverageSampleNumberCurve"); }
		}
		ChangeRename
		{
			from: "lotSizeSingle"; to: "singlePlanLotSize"
			condition: function(options) { return options.hasOwnProperty("lotSizeSingle") && !options.hasOwnProperty("singlePlanLotSize"); }
		}
		ChangeRename
		{
			from: "distributionSingle"; to: "singlePlanDistribution"
			condition: function(options) { return options.hasOwnProperty("distributionSingle") && !options.hasOwnProperty("singlePlanDistribution"); }
		}
		ChangeRename
		{
			from: "assessPlanSingle"; to: "singlePlanAssessment"
			condition: function(options) { return options.hasOwnProperty("assessPlanSingle") && !options.hasOwnProperty("singlePlanAssessment"); }
		}
		ChangeRename
		{
			from: "aqlSingle"; to: "singlePlanAssessmentAcceptableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("aqlSingle") && !options.hasOwnProperty("singlePlanAssessmentAcceptableQualityLevel"); }
		}
		ChangeRename
		{
			from: "rqlSingle"; to: "singlePlanAssessmentRejectableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("rqlSingle") && !options.hasOwnProperty("singlePlanAssessmentRejectableQualityLevel"); }
		}
		ChangeRename
		{
			from: "prod_riskSingle"; to: "singlePlanAssessmentProducerRisk"
			condition: function(options) { return options.hasOwnProperty("prod_riskSingle") && !options.hasOwnProperty("singlePlanAssessmentProducerRisk"); }
		}
		ChangeRename
		{
			from: "cons_riskSingle"; to: "singlePlanAssessmentConsumerRisk"
			condition: function(options) { return options.hasOwnProperty("cons_riskSingle") && !options.hasOwnProperty("singlePlanAssessmentConsumerRisk"); }
		}
		ChangeRename
		{
			from: "sampleSizeSingle"; to: "singlePlanSampleSize"
			condition: function(options) { return options.hasOwnProperty("sampleSizeSingle") && !options.hasOwnProperty("singlePlanSampleSize"); }
		}
		ChangeRename
		{
			from: "acceptNumberSingle"; to: "singlePlanAcceptanceNumber"
			condition: function(options) { return options.hasOwnProperty("acceptNumberSingle") && !options.hasOwnProperty("singlePlanAcceptanceNumber"); }
		}
		ChangeRename
		{
			from: "rejectNumberSingle"; to: "singlePlanRejectionNumber"
			condition: function(options) { return options.hasOwnProperty("rejectNumberSingle") && !options.hasOwnProperty("singlePlanRejectionNumber"); }
		}
		ChangeRename
		{
			from: "pd_lowerMult"; to: "multiplePlanProportionNonconformingFrom"
			condition: function(options) { return options.hasOwnProperty("pd_lowerMult") && !options.hasOwnProperty("multiplePlanProportionNonconformingFrom"); }
		}
		ChangeRename
		{
			from: "pd_upperMult"; to: "multiplePlanProportionNonconformingTo"
			condition: function(options) { return options.hasOwnProperty("pd_upperMult") && !options.hasOwnProperty("multiplePlanProportionNonconformingTo"); }
		}
		ChangeRename
		{
			from: "pd_stepMult"; to: "multiplePlanProportionNonconformingStep"
			condition: function(options) { return options.hasOwnProperty("pd_stepMult") && !options.hasOwnProperty("multiplePlanProportionNonconformingStep"); }
		}
		ChangeRename
		{
			from: "showSummaryMult"; to: "multiplePlanSummaryTable"
			condition: function(options) { return options.hasOwnProperty("showSummaryMult") && !options.hasOwnProperty("multiplePlanSummaryTable"); }
		}
		ChangeRename
		{
			from: "showOCCurveMult"; to: "multiplePlanOperatingCharacteristicCurve"
			condition: function(options) { return options.hasOwnProperty("showOCCurveMult") && !options.hasOwnProperty("multiplePlanOperatingCharacteristicCurve"); }
		}
		ChangeRename
		{
			from: "showAOQCurveMult"; to: "multiplePlanAverageOutgoingQualityCurve"
			condition: function(options) { return options.hasOwnProperty("showAOQCurveMult") && !options.hasOwnProperty("multiplePlanAverageOutgoingQualityCurve"); }
		}
		ChangeRename
		{
			from: "showATICurveMult"; to: "multiplePlanAverageTotalInspectionCurve"
			condition: function(options) { return options.hasOwnProperty("showATICurveMult") && !options.hasOwnProperty("multiplePlanAverageTotalInspectionCurve"); }
		}
		ChangeRename
		{
			from: "showASNCurveMult"; to: "multiplePlanAverageSampleNumberCurve"
			condition: function(options) { return options.hasOwnProperty("showASNCurveMult") && !options.hasOwnProperty("multiplePlanAverageSampleNumberCurve"); }
		}
		ChangeRename
		{
			from: "lotSizeMult"; to: "multiplePlanLotSize"
			condition: function(options) { return options.hasOwnProperty("lotSizeMult") && !options.hasOwnProperty("multiplePlanLotSize"); }
		}
		ChangeRename
		{
			from: "distributionMult"; to: "multiplePlanDistribution"
			condition: function(options) { return options.hasOwnProperty("distributionMult") && !options.hasOwnProperty("multiplePlanDistribution"); }
		}
		ChangeRename
		{
			from: "assessPlanMult"; to: "multiplePlanAssessment"
			condition: function(options) { return options.hasOwnProperty("assessPlanMult") && !options.hasOwnProperty("multiplePlanAssessment"); }
		}
		ChangeRename
		{
			from: "aqlMult"; to: "multiplePlanAssessmentAcceptableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("aqlMult") && !options.hasOwnProperty("multiplePlanAssessmentAcceptableQualityLevel"); }
		}
		ChangeRename
		{
			from: "rqlMult"; to: "multiplePlanAssessmentRejectableQualityLevel"
			condition: function(options) { return options.hasOwnProperty("rqlMult") && !options.hasOwnProperty("multiplePlanAssessmentRejectableQualityLevel"); }
		}
		ChangeRename
		{
			from: "prod_riskMult"; to: "multiplePlanAssessmentProducerRisk"
			condition: function(options) { return options.hasOwnProperty("prod_riskMult") && !options.hasOwnProperty("multiplePlanAssessmentProducerRisk"); }
		}
		ChangeRename
		{
			from: "cons_riskMult"; to: "multiplePlanAssessmentConsumerRisk"
			condition: function(options) { return options.hasOwnProperty("cons_riskMult") && !options.hasOwnProperty("multiplePlanAssessmentConsumerRisk"); }
		}
		ChangeRename
		{
			from: "numberOfStages"; to: "multiplePlanStagesCount"
			condition: function(options) { return options.hasOwnProperty("numberOfStages") && !options.hasOwnProperty("multiplePlanStagesCount"); }
		}
		ChangeRename
		{
			from: "stages"; to: "multiplePlanStages"
			condition: function(options) { return options.hasOwnProperty("stages") && !options.hasOwnProperty("multiplePlanStages"); }
		}

		ChangeJS
		{
			name: "singlePlanDistribution"
			condition: function(options) { return options.hasOwnProperty("singlePlanDistribution"); }
			jsFunction: function(options)
			{
				// Metadata objects pass through unchanged when JASP also applies this change to .meta.
				switch (options["singlePlanDistribution"])
				{
					case "binom": return "binomial";
					case "hypergeom": return "hypergeometric";
					default: return options["singlePlanDistribution"];
				}
			}
		}

		ChangeJS
		{
			name: "multiplePlanDistribution"
			condition: function(options) { return options.hasOwnProperty("multiplePlanDistribution"); }
			jsFunction: function(options)
			{
				// Metadata objects pass through unchanged when JASP also applies this change to .meta.
				switch (options["multiplePlanDistribution"])
				{
					case "binom": return "binomial";
					case "hypergeom": return "hypergeometric";
					default: return options["multiplePlanDistribution"];
				}
			}
		}

		ChangeJS
		{
			name: "multiplePlanStages"
			condition: function(options) { return options.hasOwnProperty("multiplePlanStages"); }
			jsFunction: function(options)
			{
				var stages = options["multiplePlanStages"];
				// A metadata descriptor is an object, not a row array.
				if (!Array.isArray(stages))
					return stages;
				return stages.map(function(stage)
				{
					if (stage === null || typeof stage !== "object" || Array.isArray(stage))
						return stage;
					var renamed = {};
					Object.keys(stage).forEach(function(key) { renamed[key] = stage[key]; });
					var names = {
						"sampleSizeMult": "sampleSize",
						"acceptNumberMult": "acceptanceNumber",
						"rejectNumberMult": "rejectionNumber"
					};
					Object.keys(names).forEach(function(oldName)
					{
						if (!renamed.hasOwnProperty(oldName))
							return;
						var newName = names[oldName];
						if (!renamed.hasOwnProperty(newName))
							renamed[newName] = renamed[oldName];
						delete renamed[oldName];
					});
					return renamed;
				});
			}
		}
	}

}
