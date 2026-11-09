// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	info: qsTr("Bayesian Sampling supports Bayesian planning and inference for acceptance sampling with a Beta prior " +
		"on the lot proportion defective. The procedure text describes the hypotheses and workflow. Planning " +
		"finds candidate sampling plans; inference updates the selected prior using the observed sample and " +
		"predicts defects in the lot.")
	infoBottom: "## " + qsTr("Numerical Details") + "\n\n" +
		qsTr("Bayes factors are calculated from log probabilities without clipping the hypothesis-region " +
		"probabilities. Planning, acceptance numbers and evidence labels use these log-scale values. " +
		"Differences within a small floating-point round-off allowance are treated as ties, so a numerical " +
		"tie meets the selected target. Very large or small Bayes factors are displayed in scientific " +
		"notation; displayed region probabilities may round to 0 or 1 without affecting the Bayes-factor " +
		"calculation.") + "\n\n" +
		qsTr("For the two-region impartial prior, calibration checks the returned Beta parameters: the selected " +
		"shape must hold strictly, and the location and probability below RQL must match their requested " +
		"values within an absolute error of 1e-8. A failure is reported rather than using an approximate " +
		"prior outside that tolerance.") + "\n\n" +
		qsTr("For the three-region impartial prior, two nested root searches match the cumulative probabilities at " +
		"AQL and RQL to 1/3 and 2/3. All three resulting region probabilities must be within an absolute " +
		"error of 1e-8 of 1/3. This is a numerical accuracy check, not a statistical uncertainty interval. " +
		"The search has no fixed upper bound on the Beta parameters; very close quality limits can imply a " +
		"highly concentrated prior. If numerical accuracy cannot be verified, an explicit calibration error " +
		"is shown. Shape boundaries at alpha = 1 or beta = 1 are allowed only when the resulting prior also " +
		"passes the same probability checks.") + "\n\n" +
		qsTr("Beta-binomial count probabilities are evaluated using adjacent-count ratios and normalized over " +
		"their full support. This avoids loss of precision for highly concentrated priors without changing " +
		"the posterior predictive model.")

	// -----------------------------------------------------------------------------
	// 1. Planning
	// -----------------------------------------------------------------------------
	Section
	{
		title: qsTr("1. Planning")
		info: qsTr("Choose the quality limits, prior distribution, maximum sample size and minimum Bayes-factor target. " +
			"In the standard two-hypothesis workflow, planning targets evidence about whether the lot proportion " +
			"defective is below the RQL.")
		columns: 1
		expanded: true

		Group
		{
			title: qsTr("Quality Levels")

			DoubleField {
				name: "planningAcceptableQualityLevel"
				label: qsTr("Acceptable quality level (AQL)")
				info: qsTr("Acceptable quality level (AQL), expressed as a proportion defective between 0 and 1.") + "\n\n" +
					qsTr("For the default unimodal two-hypothesis impartial prior, AQL sets the mode (peak) unless a custom mode " +
					"is specified. The prior assigns 50% probability below RQL and 50% above it. For a U-shaped impartial " +
					"prior, AQL instead sets the interior minimum unless a custom minimum is specified. In the " +
					"three-hypothesis workflow, AQL separates the Good and Middle regions.")
				defaultValue: 0.05
				min: 0
				max: 1
				inclusive: JASP.MaxOnly
				decimals: 6
			}

			DoubleField {
				name: "planningRejectableQualityLevel"
				label: qsTr("Rejectable quality level (RQL or LTPD)")
				info: qsTr("Proportion-defective threshold for unacceptable quality and the main threshold used in " +
					"two-hypothesis planning and inference.") + "\n\n" +
					qsTr("Also called the lot tolerance percent defective (LTPD). Enter a proportion between 0 and 1 " +
						"(for example, 0.10 means 10%).")
				defaultValue: 0.15
				min: 0
				max: 1
				inclusive: JASP.MaxOnly
				decimals: 6
			}
		}

		RadioButtonGroup
		{
			id:		priorPlanGroup
			title:	qsTr("Beta Prior")
			info:	qsTr("Select a Beta prior for the lot proportion defective. The selected prior is used for both planning " +
				"and inference.")
			name:	"planningPriorDistribution"

			RadioButton
			{
				value:		"impartial"
				label:		qsTr("Impartial (two-region)")
				info:		qsTr("Assigns 50% prior probability below RQL and 50% above it. Choose a unimodal prior with an interior " +
					"peak or a U-shaped prior with an interior minimum between the endpoint peaks. The location defaults " +
					"to AQL. Incompatible combinations produce an error; the analysis does not automatically change the " +
					"shape, location or probability split.")
				checked:	true

				Group
				{
					id:					priorTwoRegionSettings
					visible:			priorPlanGroup.value === "impartial"
					enabled:			visible

					RadioButtonGroup
					{
						id:			impartialShape
						name:		"planningPriorImpartialShape"
						title:		qsTr("Prior Shape")
						infoLabel:	qsTr("Two-region prior shape")
						info:		qsTr("Both shapes assign half of the prior probability below RQL. For Unimodal (the default), the location " +
							"is the peak and RQL must lie strictly between that mode and 0.5. For U-shaped, the location is the " +
							"interior minimum and the minimum and RQL must lie on opposite sides of 0.5. Both the location and " +
							"RQL must be strictly between 0 and 1.") + "\n\n" +
							qsTr("A location and RQL both equal to 0.5 leave infinitely many priors of either shape possible. Choose " +
							"Custom to specify the desired parameters.")

						RadioButton
						{
							value:		"unimodal"
							label:		qsTr("Unimodal")
							checked:	true
						}
						RadioButton
						{
							value:	"uShaped"
							label:	qsTr("U-shaped")
						}
					}

					CheckBox
					{
						id:					impartialCustomLocation
						name:				"planningPriorImpartialCustomLocation"
						label:				impartialShape.value === "uShaped" ? qsTr("Custom minimum") : qsTr("Custom mode")
						infoLabel:			qsTr("Two-region custom location")
						info:				qsTr("When unchecked, AQL is used as the location. Changing shape retains the location; incompatible combinations show an error.")
						checked:			false
						childrenOnSameRow:	true

						DoubleField
						{
							name:			"planningPriorImpartialLocation"
							label:			""
							infoLabel:		qsTr("Prior location")
							afterLabel:		impartialCustomLocation.checked ? "" : qsTr("(default: AQL)")
							info:			qsTr("Location of the interior peak for Unimodal or the interior minimum for U-shaped, between 0 and 1. " +
								"Switching shape retains this value.")
							defaultValue:	0.05
							min:			0
							max:			1
							inclusive:		JASP.None
							decimals:		6
						}
					}
				}
			}

			RadioButton
			{
				value:	"impartialThreeRegion"
				label:	qsTr("Impartial (three-region)")
				info:	qsTr("Assigns one third of the prior probability to each of Good (below AQL), Middle (between AQL and " +
					"RQL), and Bad (above RQL). The limits must satisfy 0 < AQL < RQL < 1.")

				Group
				{
					id:					priorThreeRegionSettings
					visible:			priorPlanGroup.value === "impartialThreeRegion"
					enabled:			visible

					RadioButtonGroup
					{
						id:						priorThreeRegionShapeConstraint
						name:					"planningPriorThreeRegionShapeConstraint"
						title:					qsTr("Shape Constraint (Optional)")
						infoLabel:				qsTr("Three-region shape constraint")
						info:					qsTr("Leave both choices unchecked to allow any Beta shape (the default). Select at most one constraint: " +
							"exclude U-shapes (alpha >= 1 or beta >= 1), or require both parameters to be " +
							"at least 1. The equal-thirds prior must satisfy the selected constraint. Incompatible quality limits " +
							"and shape constraints produce an error. Change the limits or constraint; the probability split is " +
							"not changed.")

						// Preserve the saved string option while allowing both visible choices to be unchecked.
						function synchronizeChoices()
						{
							threeRegionExcludeUShaped.checked = value === "nonUShaped"
							threeRegionBothAtLeastOne.checked = value === "bothAtLeastOne"
						}

						onValueChanged:			synchronizeChoices()
						onInitializedChanged:	if (initialized) synchronizeChoices()

						RadioButton
						{
							id:			priorThreeRegionNone
							value:		"none"
							visible:	false
							checked:	true
						}
						RadioButton
						{
							id:			priorThreeRegionNonUShaped
							value:		"nonUShaped"
							visible:	false
						}
						RadioButton
						{
							id:			priorThreeRegionBothAtLeastOne
							value:		"bothAtLeastOne"
							visible:	false
						}

						CheckBox
						{
							id:			threeRegionExcludeUShaped
							label:		qsTr("Exclude U-shaped")
							info:		qsTr("Requires at least one of alpha or beta to be at least 1, allowing monotone priors but excluding U-shaped priors.")
							isBound:	false
							onCheckedChanged:
							{
								if (priorThreeRegionShapeConstraint.initialized)
								{
									if (checked && priorThreeRegionShapeConstraint.value !== "nonUShaped")
										priorThreeRegionNonUShaped.click()
									else if (!checked && priorThreeRegionShapeConstraint.value === "nonUShaped")
										priorThreeRegionNone.click()
								}
							}
						}
						CheckBox
						{
							id:			threeRegionBothAtLeastOne
							label:		qsTr("\u03B1 \u2265 1 and \u03B2 \u2265 1")
							info:		qsTr("Requires both alpha and beta to be at least 1, also excluding monotone priors that have either parameter below 1.")
							isBound:	false
							onCheckedChanged:
							{
								if (priorThreeRegionShapeConstraint.initialized)
								{
									if (checked && priorThreeRegionShapeConstraint.value !== "bothAtLeastOne")
										priorThreeRegionBothAtLeastOne.click()
									else if (!checked && priorThreeRegionShapeConstraint.value === "bothAtLeastOne")
										priorThreeRegionNone.click()
								}
							}
						}
					}
				}
			}

			RadioButton
			{
				value:	"uniform"
				label:	qsTr("Uniform")
				info:	qsTr("Uses a Beta(1, 1) prior.")
			}

			RadioButton
			{
				value:	"custom"
				label:	qsTr("Custom")
				info:	qsTr("Specify the two positive shape parameters of the Beta prior.")

				Group
				{
					id:					priorCustomSettings
					visible:			priorPlanGroup.value === "custom"
					enabled:			visible
					columns:			2

					DoubleField
					{
						name:			"planningPriorAlpha"
						label:			qsTr("\u03B1")
						info:			qsTr("First shape parameter (alpha) of the custom Beta prior.")
						defaultValue:	1
						min:			0
						max:			10000
					}

					DoubleField
					{
						name:			"planningPriorBeta"
						label:			qsTr("\u03B2")
						info:			qsTr("Second shape parameter (beta) of the custom Beta prior.")
						defaultValue:	1
						min:			0
						max:			10000
					}
				}
			}
		}

		Group
		{
			title: qsTr("Planning Criteria")

			IntegerField {
				name: "planningMaximumSampleSize"
				label: qsTr("(Max) sample size (n)")
				info: qsTr("Maximum sample size searched when finding candidate sampling plans.")
				defaultValue: 40
				min: 1
				max: 1000
			}

			DoubleField {
				name: "planningMinimumBayesFactor"
				label: qsTr("Bayes factor (min)")
				info: qsTr("Minimum target Bayes factor in favor of a proportion defective below the RQL. This target is used " +
					"for planning and for the acceptance number and target status reported under Inference. Equality " +
					"meets the target; not reaching it does not by itself imply rejection. A target at or below 1 can be " +
					"reached without evidence favoring a proportion defective below the RQL.")
				defaultValue: 30
				min: 1/10000
				max: 10000
			}
		}

		Group
		{
			title: qsTr("Output Options")
			CheckBox
			{
				name: "planningPlanTable"
				label: qsTr("Plan table")
				info: qsTr("Show candidate sampling plans meeting the selected minimum Bayes-factor target within the maximum " +
					"sample size.")
			}
			CheckBox
			{
				name: "planningPriorPlot"
				label: qsTr("Prior plot")
				info: qsTr("Plot the selected Beta prior. The plot remains available even if the sample-size limit and " +
					"Bayes-factor target yield no feasible plan. Adaptive grids resolve narrow peaks and both tails. If " +
					"the density diverges at 0 or 1, its curve continues beyond the finite vertical display range; this " +
					"affects only the display, not probabilities or Bayes factors.")
			}
			CheckBox
			{
				name: "planningThreeHypothesisBayesFactorTable"
				label: qsTr("Three-hypothesis BF table")
				info: qsTr("Show Bayes-factor comparisons for the Good, Middle and Bad regions defined by AQL and RQL.")
			}
		}
	}

	// -----------------------------------------------------------------------------
	// 2. Inference
	// -----------------------------------------------------------------------------
	Section
	{
		title: qsTr("2. Inference")
		info: qsTr("Use the selected prior and observed sample to compute the posterior distribution, Bayes-factor " +
			"evidence and posterior predictive lot-quality probabilities. Invalid sample counts, lot sizes or " +
			"Bayes-factor targets produce an explanatory error; correcting the inputs restores the results.")
		columns: 1
		id: infer

		Group
		{
			CheckBox
			{
				id: inferPosterior
				name: "inferencePosterior"
				label: qsTr("Posterior distribution")
				info: qsTr("Update the selected Beta prior using the sample size n and observed defectives d. Counts must " +
					"satisfy 0 <= d <= n <= N. The input fields retain inconsistent counts so that the analysis can " +
					"explain the problem. If d exceeds n or n exceeds N, correct whichever entry was mistaken; dependent " +
					"inference results return after the inputs are valid.")
			}

			Group
			{
				enabled: inferPosterior.checked

				IntegerField {
					id: inferenceSampleSize
					name: "inferenceSampleSize"
					label: qsTr("Sample size (n)")
					info: qsTr("Number of inspected items in the sample. This must not exceed the lot size.")
					defaultValue: 30
					min: 1
					max: 10000
				}

				IntegerField {
					name: "inferenceObservedDefectives"
					label: qsTr("Observed number of defectives (d)")
					info: qsTr("Number of defective items observed in the inspected sample. This must not exceed the sample size.")
					defaultValue: 0
					min: 0
					// Let the analysis explain d > n without rejecting the entered count.
					max: 100000
				}

				IntegerField {
					id: inferenceLotSize
					name: "inferenceLotSize"
					label: qsTr("Lot size (N)")
					info: qsTr("Total number of items in the lot, including inspected and uninspected items.")
					defaultValue: 100
					// Validate N >= n in the analysis; either input may need correction.
					min: 1
					max: 100000
				}
			}

			Group
			{
				title: qsTr("Output Options")
				enabled: inferPosterior.checked
				CheckBox
				{
					name: "inferenceDecisionTable"
					label: qsTr("Inference decision table")
					info: qsTr("Show posterior Bayes-factor evidence and target status. Evidence labels describe support for a " +
						"proportion defective below versus at or above the RQL; whether the selected target is reached is " +
						"reported separately. Both use the unrounded Bayes factor. The acceptance number c is the largest " +
						"observed defect count at the specified inference sample size that meets the target. \"Not available\" " +
						"means that no count meets it at that sample size.")
				}
				CheckBox
				{
					name: "inferencePosteriorPlot"
					label: qsTr("Posterior plot")
					info: qsTr("Plot the updated Beta posterior and its Bayes-factor evidence and target status. Adaptive grids " +
						"resolve narrow peaks and both tails. If the density diverges at 0 or 1, its curve continues beyond " +
						"the finite vertical display range; this affects only the display, not probabilities or Bayes " +
						"factors.")
				}
				CheckBox
				{
					name: "inferencePosteriorPredictivePlot"
					label: qsTr("Posterior predictive distribution plot")
					info: qsTr("Plot the posterior predictive distribution of lot-level defectives, conditional on the observed " +
						"sample data.")
				}
			}
		}
	}
}
