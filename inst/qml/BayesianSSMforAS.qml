import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	columns: 1

	// Keep help notation readable without MathJax: use HTML and Unicode, not LaTeX.
	info: qsTr("*Bayesian State-Space Sampling* performs acceptance sampling with a Bayesian state-space model for defect " +
			"rates over lots.")

	infoBottom: "## " + qsTr("Data Requirements") + "\n\n" +
		qsTr("Use one row per lot. Rows are sorted by Lot Number, and the current lot is the one with the largest number. " +
			"Duplicate lot numbers are rejected. If further items from the same lot are inspected, update its existing " +
			"Sample Size and Defect Count rather than appending another cumulative-inspection row.") + "\n\n" +
		qsTr("Lot numbers determine order only. Their spacing does not change the state-transition variance: lots " +
			"numbered 1, 2, 3 and lots numbered 1, 10, 100 have the same model transitions when their data are otherwise " +
			"identical.") + "\n\n" +
		qsTr("Defect Count, Sample Size and Lot Size must be finite, nonnegative whole numbers, with Defect Count \u2264 " +
			"Sample Size \u2264 Lot Size. Fractional values are rejected rather than truncated; counts must be smaller than " +
			"2,147,483,647 to fit the implementation's integer indexing. Zero inspected items, zero observed defects, " +
			"all inspected items defective, and full inspection are supported. A zero-size lot is allowed only with zero " +
			"inspected items and zero observed defects; it still occupies a position in the state sequence. A single lot " +
			"is supported without a predictor.") + "\n\n" +
		qsTr("Every assigned column must be complete. A missing or non-finite value in Lot Number, Defect Count, Sample " +
			"Size, Lot Size, or an assigned covariate stops the analysis with an error naming the affected field; no " +
			"acceptance decision is produced. Lots are not automatically dropped, and missing values are neither imputed " +
			"nor treated as zero. Unassigned columns do not affect the analysis. For a genuinely uninspected lot, enter " +
			"Sample Size = 0 and Defect Count = 0; do not replace an unknown count from an inspected sample with zero.") + "\n\n" +
		qsTr("With covariates, at least two lots are required, and every selected covariate must contain varying, finite " +
			"numeric values. Each covariate is centered at its own mean and divided by its own sample standard deviation " +
			"over all supplied rows, without weighting by sample size. Appending lots can change this scaling and hence " +
			"the meaning of a one-SD effect. The Model section explains the coefficients and their priors.") + "\n\n" +
		qsTr("The analysis rejects linearly dependent covariates, such as duplicate variables or a variable that is an " +
			"exact linear combination of the others after centering. Remove redundant covariates before fitting. " +
			"Strongly correlated covariates can still be difficult to distinguish, and covariates that track time can be " +
			"difficult to distinguish from changes in the latent baseline. For short series, use a small number of " +
			"covariates with a clear substantive purpose.") + "\n\n" +
		"## " + qsTr("Updating Data and Refitting") + "\n\n" +
		qsTr("Each fit uses all supplied lots jointly. To inspect more items from the current lot, update its cumulative " +
			"Sample Size and Defect Count in the existing row and rerun the analysis. Append a row only for a new lot. " +
			"The interface does not automatically inspect items or run an item-by-item optional-stopping loop.") + "\n\n" +
		qsTr("Changing the supplied data, covariate assignments, priors or MCMC settings requires a new fit of the " +
			"supplied series. Changing quality limits, decision thresholds or displayed outputs reuses the fitted " +
			"posterior samples; it does not change the model posterior.") + "\n\n" +
		"## " + qsTr("Output") + "\n\n" +
		qsTr("- **Posterior Summary (Current Lot)**: observed defects, posterior predictive summary, and acceptance " +
			"decision. A decision-specific note explains the proposal using the selected probability thresholds; the " +
			"calculated probabilities remain in the table.\n- **Predictor Coefficients** (automatically shown when " +
			"covariates are included): posterior mean, median, and equal-tailed 95% credible interval (2.5th and 97.5th " +
			"percentiles) for each coefficient. These describe changes in the log-odds of a defect per one SD increase " +
			"in the covariate, holding the latent baseline and other covariates fixed.")

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList
		{
			name: "defectCount"
			title: qsTr("Defect Count")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Observed defects in each sampled lot.")
		}
		AssignedVariablesList
		{
			name: "sampleSize"
			title: qsTr("Sample Size (Tested Items)")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Number of tested items per lot.")
		}
		AssignedVariablesList
		{
			name: "lotSize"
			title: qsTr("Lot Size (Total Items)")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Total items per lot.")
		}
		AssignedVariablesList
		{
			name: "lotNumber"
			title: qsTr("Lot Number")
			allowedColumns: ["scale", "ordinal"]
			singleVariable: true
			info: qsTr("Unique numeric sequence/order index for each lot, with measurement type **Scale** or **Ordinal**. Ordinal " +
					"columns must have numeric labels; their numeric values determine the order, regardless of category-level " +
					"order. Ordered text labels are not supported.")
		}
		AssignedVariablesList
		{
			id: predictor
			name: "covariates"
			title: qsTr("Covariates (Optional)")
			allowedColumns: ["scale"]
			info: qsTr("Assign one or more numeric covariates to include their additive effects in the model. Leave this field " +
					"empty to fit the model without predictors. Adding or removing a covariate refits the model. The common " +
					"coefficient prior scale can be set under Model before assigning covariates; coefficient plots and " +
					"diagnostic controls are shown when covariates are assigned. Categorical predictors and interaction terms " +
					"are not supported.")
		}
	}

	Group
	{
		columns: 1
		rowSpacing: jaspTheme.rowGridSpacing
		info: qsTr("AQL and RQL are defect **proportions**, entered between 0 and 1. For example, an AQL of 0.025 " +
			"corresponds to 25 defects in a lot of size 1,000. Let %1 be the total number of defects in the " +
			"current lot, %2 its size, and %3 all supplied observations. The decision uses the posterior " +
			"predictive probabilities").arg("D<sub>T</sub>").arg("s<sub>T</sub>").arg("<b>D</b>") + "\n\n" +
			"<p>L = P(D<sub>T</sub> &lt; AQL \u00d7 s<sub>T</sub> | <b>D</b>)<br>" +
				"H = P(D<sub>T</sub> &gt; RQL \u00d7 s<sub>T</sub> | <b>D</b>).</p>\n\n" +
			qsTr("Let a be the **Acceptance threshold** (default 0.10) and r the **Rejection threshold** (default 0.05). " +
				"The result is:") + "\n\n" +
			qsTr("- **Consider to Accept** if H &lt; a.\n- **Consider to Reject** if H &gt; a and L &lt; r.\n- **Consider to " +
				"Continue** otherwise.") + "\n\n" +
			qsTr("The fields beside **Accept if** and **Reject if** set a and r, respectively. The rejection label also " +
				"displays the current value of a, so both required conditions are visible.") + "\n\n" +
			qsTr("All inequalities are strict. In particular, H = a gives Continue. Counts exactly at a quality limit are " +
				"excluded from its corresponding strict tail. Products within floating-point precision of an integer are " +
				"treated as that integer; genuinely fractional count limits are not rounded. These probabilities concern the " +
				"total defects in the finite lot, including observed and uninspected items, rather than the latent defect " +
				"probability alone. The count-based expressions also apply to a zero-size lot, for which a defect proportion " +
				"is undefined.") + "\n\n" +
			qsTr("The thresholds express model-based posterior decision criteria. They do not by themselves guarantee " +
				"frequentist producer's or consumer's error rates across repeated samples.")

		Group
		{
			title: qsTr("Quality Levels")
			DoubleField
			{
				id: lowerLimit
				name: "settingsAcceptableQualityLevel"
				label: qsTr("Acceptable quality level (AQL)")
				defaultValue: 0.05
				min: 0
				max: upperLimit.value
				info: qsTr("Acceptable quality level (AQL), expressed as a proportion defective between 0 and 1.")
			}
			DoubleField
			{
				id: upperLimit
				name: "settingsRejectableQualityLevel"
				label: qsTr("Rejectable quality level (RQL or LTPD)")
				defaultValue: 0.10
				min: lowerLimit.value
				max: 1
				info: qsTr("Rejectable quality level (RQL), also called the lot tolerance percent defective (LTPD). " +
					"Enter a proportion between 0 and 1 (for example, 0.10 means 10%).")
			}
		}

		Group
		{
			title: qsTr("Decision Rule")
			DoubleField
			{
				id: acceptThreshold
				name: "settingsAcceptanceThreshold"
				label: qsTr("Accept if P(above RQL) <")
				info: qsTr("Acceptance threshold a (default 0.10). Consider to Accept when H &lt; a, where H is the posterior " +
						"predictive probability that the total defects in the current lot exceed RQL times its size.")
				defaultValue: 0.10; min: 0; max: 1
			}
			DoubleField
			{
				name: "settingsRejectionThreshold"
				label: qsTr("Reject if P(above RQL) > %1 and P(below AQL) <").arg(acceptThreshold.value)
				info: qsTr("Rejection threshold r (default 0.05). Consider to Reject when H &gt; a and L &lt; r, where H is the " +
						"posterior predictive probability that total defects exceed RQL times the lot size and L is the " +
						"probability that they are below AQL times the lot size. Equality to the acceptance threshold gives Continue.")
				defaultValue: 0.05; min: 0; max: 1
			}
			Label
			{
				text: qsTr("Continue sampling if neither rule is satisfied.")
			}
		}
	}

	Section
	{
		title: qsTr("Plots")
		columns: 1
		info: "### " + qsTr("Reading the State Trajectory") + "\n\n" +
			qsTr("The trajectory shows the lot-specific defect probabilities, including any covariate contributions. It " +
				"conditions on all supplied observations. Estimates for earlier lots therefore use later supplied " +
				"observations as well: this is **smoothing**. They can change when more lots are added and should not be " +
				"read as the estimates or decisions that would have been available when each earlier lot was inspected. The " +
				"current lot is the last lot in the supplied sequence; its prediction uses observations available in that " +
				"dataset.") + "\n\n" +
			"### " + qsTr("Prediction for the Current Lot") + "\n\n" +
			qsTr("For current lot %1, with %2 total items, %3 inspected and %4 observed defects, the model predicts " +
				"the remaining defects %5 and total %6 as").arg("T").arg("s<sub>T</sub>").arg("n<sub>T</sub>").arg("y<sub>T</sub>").arg("Y<sub>rem,T</sub>").arg("D<sub>T</sub>") + "\n\n" +
			"<p>Y<sub>rem,T</sub> | \u03b8<sub>T</sub> \u223c Binomial(s<sub>T</sub> \u2212 n<sub>T</sub>, \u03b8<sub>T</sub>)<br>" +
				"D<sub>T</sub> = y<sub>T</sub> + Y<sub>rem,T</sub>.</p>\n\n" +
			qsTr("The posterior predictive distribution averages this conditional distribution over the posterior " +
				"uncertainty in %1. It combines uncertainty about the defect probability with binomial variability " +
				"among the uninspected items.").arg("\u03b8<sub>T</sub>") + "\n\n" +
			qsTr("The predictive plot displays approximately the central 99.8% range (0.1st\u201399.9th percentiles), expanded to " +
				"include AQL/RQL count limits and rounded axis limits. Very small tails may fall outside the displayed " +
				"range. The bins contain equal numbers of whole defect counts, with one bar per count for short ranges. Bars " +
				"show the probability mass in each bin and are **not renormalized** after selecting the display range. The " +
				"summary probabilities and decision always use the full predictive distribution, including any undisplayed " +
				"tails.") + "\n\n" +
			qsTr("Valid predictions range from the observed defect count to that count plus the number of uninspected items; " +
				"a fully inspected lot has probability one at its observed total. If predictive draws are missing, " +
				"non-finite, noninteger or outside this range, or their probabilities do not sum to one, the analysis " +
				"reports an error instead of an acceptance decision. A divergent transition alone does not make a predictive " +
				"draw invalid; these draws are retained and their computational warnings should be investigated.") + "\n\n" +
			"### " + qsTr("Predictor Coefficient Plots") + "\n\n" +
			qsTr("The coefficient density plots use all posterior draws and extend beyond the sampled range so that the tails " +
				"approach zero. The default axes also include zero as a reference. Adjusting the axes changes the display, " +
				"not the posterior draws or coefficient summaries. With weakly identified effects, Cauchy tails can make " +
				"coefficient means and SDs unstable or undefined. Inspect the computational diagnostics and assess prior " +
				"sensitivity before interpreting such summaries.")

		CheckBox
		{
			name: "plotStateTimeSeries"
			label: qsTr("State time series (defect rate)")
			checked: true
			info: qsTr("Defect-rate state trajectory over lots: posterior means with pointwise 95% credible intervals.")
		}
		CheckBox
		{
			name: "plotPosteriorPredictive"
			label: qsTr("Posterior predictive for current lot")
			checked: true
			info: qsTr("Posterior predictive distribution of total defects in the current lot: observed defects plus predicted " +
					"defects among its uninspected items. It cannot contain fewer defects than already observed; after " +
					"inspecting the entire lot, its total defect count is known.")
		}
		CheckBox
		{
			name: "plotLatestDefectRate"
			label: qsTr("Posterior for latest defect rate")
			info: qsTr("Posterior distribution of latest defect rate.")
		}
		CheckBox
		{
			name: "plotPredictorCoefficient"
			label: qsTr("Posteriors for predictor coefficients")
			visible: predictor.count > 0
			info: qsTr("Posterior distributions of predictor coefficients, labeled by covariate.")
		}
	}

	Section
	{
		title: qsTr("Model")
		columns: 1
		info: qsTr("For lot %1, let %2 be its observed defects, %3 its inspected items and %4 its latent defect " +
			"probability. The observation model is").arg("\u2113").arg("y<sub>\u2113</sub>").arg("n<sub>\u2113</sub>").arg("\u03b8<sub>\u2113</sub>") + "\n\n" +
			"<p>y<sub>\u2113</sub> | \u03b8<sub>\u2113</sub> \u223c Binomial(n<sub>\u2113</sub>, \u03b8<sub>\u2113</sub>).</p>\n\n" +
			qsTr("Let %1 denote the initial baseline probability (the %2 prior controls in the interface) and %3 the " +
				"baseline log odds. Across lots,").arg("p<sub>0</sub>").arg("\u03b8<sub>1</sub>").arg("\u03bd<sub>\u2113</sub>") + "\n\n" +
			"<p>\u03bd<sub>1</sub> = logit(p<sub>0</sub>)<br>" +
				"\u03bd<sub>\u2113</sub> = \u03bd<sub>\u2113\u22121</sub> + \u03b7<sub>\u2113</sub>, \u2113 = 2, \u2026, T,</p>\n\n" +
			qsTr("with innovations") + "\n\n" +
			"<p>\u03b7<sub>\u2113</sub> | \u03c3<sub>\u03b7</sub> \u223c " +
				"(1 \u2212 q) N(0, \u03c3<sub>\u03b7</sub><sup>2</sup>) + q N(0, \u03c3<sub>\u03b7</sub><sup>2</sup> + \u03b4<sup>2</sup>)<br>" +
				"q = 0.05, \u03b4 = 1.5.</p>\n\n" +
			qsTr("The second argument of %1 is its variance. The regular component has SD %2, the **baseline " +
				"innovation scale** estimated from the data. The broad component has SD %3 and accommodates larger " +
				"changes. Both are centered at zero, allowing quality to improve or deteriorate. The baseline scale " +
				"is not the SD of the entire mixture.").arg("N").arg("\u03c3<sub>\u03b7</sub>").arg("\u221a(\u03c3<sub>\u03b7</sub><sup>2</sup> + 1.5<sup>2</sup>)") + "\n\n" +
			qsTr("The mixture weight %1 and additional spread %2 are fixed model settings; %2 is on the logit scale. " +
				"Conditional on the shared baseline scale, innovations are independent draws from this mixture. A " +
				"particular transition can receive stronger posterior support for the broad component, but the shared " +
				"prior weight stays at 0.05. The model does not estimate probabilities of switching between " +
				"persistent regimes.").arg("q").arg("\u03b4") + "\n\n" +
			qsTr("Without predictors, %1. With %2 numeric covariates,").arg("\u03b8<sub>\u2113</sub> = logit<sup>\u22121</sup>(\u03bd<sub>\u2113</sub>)").arg("K") + "\n\n" +
			"<p>\u03b8<sub>\u2113</sub> = logit<sup>\u22121</sup>(\u03bd<sub>\u2113</sub> + " +
				"\u2211<sub>k=1</sub><sup>K</sup> \u03b2<sub>k</sub> X<sub>\u2113k</sub><sup>*</sup>)<br>" +
				"X<sub>\u2113k</sub><sup>*</sup> = (X<sub>\u2113k</sub> \u2212 mean(X<sub>k</sub>)) / SD(X<sub>k</sub>),</p>\n\n" +
			qsTr("where %1 is covariate %2's sample SD over the supplied lots and each coefficient %3 is shared across " +
				"lots. Equivalently, the vector of log odds is %4, where %5 contains the standardized covariate " +
				"columns. No additional intercept is included because the latent baseline already supplies it.").arg("SD(X<sub>k</sub>)").arg("k").arg("\u03b2<sub>k</sub>").arg("<b>\u03bc</b> = <b>\u03bd</b> + X<b>\u03b2</b>").arg("X") + "\n\n" +
			qsTr("A one-SD increase in covariate %1 changes the log odds by %2, holding the other covariates and the " +
				"baseline fixed; it multiplies the odds by %3. In this mode, %4 is the first lot's baseline " +
				"probability when all covariates are at their means, and its actual defect probability also includes " +
				"the combined covariate contribution.").arg("k").arg("\u03b2<sub>k</sub>").arg("exp(\u03b2<sub>k</sub>)").arg("p<sub>0</sub>") + "\n\n" +
			"### " + qsTr("Priors") + "\n\n" +
			qsTr("The following priors are independent. Their hyperparameters are adjustable in the Model section.") + "\n\n" +
			qsTr("| Parameter | Prior | Default settings |\n" +
				"|---|---|---|\n" +
				"| Initial baseline probability %1 | Beta | Shape parameters 1 and 1 (uniform) |\n" +
				"| Baseline innovation scale %2 | Half-normal | Scale 1 |\n" +
				"| Each predictor coefficient %3 | Independent Cauchy, centered at zero | Common scale 0.707 |").arg("p<sub>0</sub>").arg("\u03c3<sub>\u03b7</sub>").arg("\u03b2<sub>k</sub>") + "\n\n" +
			qsTr("Adding covariates can broaden the prior on their combined contribution to the log odds even when the " +
				"individual coefficient scale stays unchanged. The model does not automatically increase shrinkage as more " +
				"covariates are added. Choose covariates and priors using relevant process knowledge and assess sensitivity " +
				"when the series is short or effects are weakly identified.")

		Group
		{
			title: qsTr("Initial Baseline Probability")
			info: qsTr("Beta prior for the first lot's defect probability without predictors, or its baseline probability when all covariates are at their means.")
			DoubleField
			{
				name: "modelInitialBaselineAlpha"
				label: qsTr("\u03B8\u2081 alpha")
				defaultValue: 1
				min: 0.01
				info: qsTr("First shape parameter of the beta prior for the initial baseline probability. The default shape parameters " +
						"are 1 and 1, giving a uniform prior.")
			}
			DoubleField
			{
				name: "modelInitialBaselineBeta"
				label: qsTr("\u03B8\u2081 beta")
				defaultValue: 1
				min: 0.01
				info: qsTr("Second shape parameter of the beta prior for the initial baseline probability. The default shape parameters " +
						"are 1 and 1, giving a uniform prior.")
			}
		}

		Group
		{
			title: qsTr("Baseline Innovation Scale")
			DoubleField
			{
				name: "modelInnovationPriorScale"
				label: qsTr("Half-normal prior scale")
				info: qsTr("The **Half-normal prior scale** is the SD of the underlying zero-centered normal distribution before " +
					"restricting it to positive values; it is not the SD of the resulting half-normal distribution. This " +
					"control sets the width of the prior for %1, which is estimated from the data. The half-normal prior " +
					"favors smaller baseline scales while allowing larger values.").arg("\u03c3<sub>\u03b7</sub>")
				defaultValue: 1; min: 0.01
			}
		}

		Group
		{
			title: qsTr("Predictor Coefficients")
			DoubleField
			{
				name: "modelPredictorCauchyScale"
				label: qsTr("Cauchy scale")
				info: qsTr("Common scale of the independent zero-centered Cauchy priors for the predictor coefficients. You can set " +
						"this before assigning covariates. Each coefficient is the change in log odds per one standard deviation " +
						"increase in that covariate, holding the other covariates and baseline fixed.") + "\n\n" +
					qsTr("The **Cauchy scale** control applies separately to every coefficient; the coefficients are not " +
						"constrained to be equal. Each prior is expressed per one SD of its covariate. In original covariate " +
						"units, the coefficient is %1, and its prior scale is the chosen Cauchy scale divided by %2. Because " +
						"covariates are standardized over the supplied dataset at each fit, appending lots can change these " +
						"original-unit priors.").arg("\u03b2<sub>k</sub> / SD(X<sub>k</sub>)").arg("SD(X<sub>k</sub>)")
				defaultValue: 0.707; min: 0.01
			}
		}
	}

	Section
	{
		title: qsTr("MCMC Diagnostics")
		columns: 1
		info: qsTr("Check computational diagnostics before interpreting the results. Select **MCMC Diagnostics \u2192 Diagnostic " +
				"report** to see the full report. The checks run even when this option is off, and any warnings remain " +
				"visible alongside the results. All retained post-warmup draws are used, including those associated with " +
				"divergent transitions. Warnings qualify the posterior summaries and decisions; they do not remove draws or " +
				"suppress these outputs.") + "\n\n" +
			qsTr("The checks assess all sampled parameters: the initial baseline log odds (`mu1`), baseline innovation scale, " +
				"variables used to construct the innovations (`z_raw`), and every predictor coefficient when present. They " +
				"report divergences, maximum-treedepth hits, rank-normalized split R-hat (the maximum of its folded and " +
				"unfolded versions), bulk ESS, tail ESS, and per-chain E-BFMI. RStan's `Rhat()`, `ess_bulk()` and " +
				"`ess_tail()` functions calculate the mixing diagnostics from iteration-ordered chains. E-BFMI is " +
				"`mean(diff(energy)^2) / var(energy)`, calculated separately for each chain.") + "\n\n" +
			qsTr("Warnings flag any retained divergence or treedepth hit, R-hat at or above 1.01, bulk or tail ESS below 100 " +
				"times the number of chains, and E-BFMI below 0.3. These are screening thresholds, not guarantees of " +
				"convergence. Divergences and poor mixing can compromise estimates; treedepth saturation can reduce " +
				"efficiency. See [Stan's diagnostic guidance](https://mc-stan.org/learn-stan/diagnostics-warnings.html) for " +
				"investigation and remedies.") + "\n\n" +
			qsTr("A single chain cannot establish between-chain agreement, so the rank-normalized split R-hat check is " +
				"unavailable. At least six retained draws per chain are required before computing the mixing diagnostics; " +
				"this is only a computational minimum. Constant or non-finite sampled-parameter chains and missing " +
				"diagnostics trigger explicit qualifications. Constant generated quantities, such as a known total after " +
				"complete inspection, do not by themselves indicate sampler failure. Unavailable checks should not be " +
				"interpreted as evidence of convergence.")

		CheckBox
		{
			name: "mcmcDiagnosticReport"
			label: qsTr("Diagnostic report")
			checked: false
			info: qsTr("Show sampler and convergence checks for all sampled parameters.") + "\n\n" +
				qsTr("Under **MCMC Diagnostics**, **Diagnostic report** displays the full computational diagnostic report and is " +
					"off by default. The checks always run, and warnings about problems or unavailable checks are shown " +
					"regardless of this option. **Overview table** separately displays posterior summaries and traditional MCMC " +
					"diagnostics for the selected parameters.")
		}

		CheckBox
		{
			id: showMcmcSummary
			name: "mcmcDiagnosticOverviewTable"
			label: qsTr("Overview table")
			info: qsTr("Show the mean, SD, ESS, and R-hat for the selected parameters.") + "\n\n" +
				qsTr("The **Overview table** summarizes the parameters selected below it. **Sigma (state noise)** refers " +
					"to the baseline innovation scale %1. This table's ESS (`n_eff`) and R-hat columns use RStan's " +
					"traditional raw-draw ESS and unranked split R-hat, which differ from the rank-based diagnostics in " +
					"the full computational report. Undefined diagnostics for constant generated quantities should not be " +
					"read as successful convergence checks.").arg("\u03c3<sub>\u03b7</sub>")
		}

		Group
		{
			title: qsTr("Selected Parameters")
			enabled: showMcmcSummary.checked
			CheckBox
			{
				name: "mcmcDiagnosticOverviewTheta"
				label: qsTr("Theta[t]")
				info: qsTr("Include the defect probability for every lot in the overview table.")
			}
			CheckBox
			{
				name: "mcmcDiagnosticOverviewSigma"
				label: qsTr("Sigma (state noise)")
				info: qsTr("Include the baseline innovation scale %1 in the overview table.").arg("\u03c3<sub>\u03b7</sub>")
			}
			CheckBox
			{
				name: "mcmcDiagnosticOverviewPrediction"
				label: qsTr("y_pred (prediction)")
				info: qsTr("Include the prediction for the total defects in the current lot in the overview table.")
			}
			CheckBox
			{
				name: "mcmcDiagnosticOverviewBeta"
				label: qsTr("Beta (predictor coefficients)")
				visible: predictor.count > 0
				info: qsTr("Include the predictor coefficients in the overview table.")
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")
		columns: 1

		Group
		{
			title: qsTr("MCMC")
			IntegerField
			{
				name: "advancedMcmcBurnin"
				label: qsTr("Burnin")
				defaultValue: 2000
				min: 100
				info: qsTr("Warmup iterations per chain.")
			}
			IntegerField
			{
				name: "advancedMcmcSamples"
				label: qsTr("Samples")
				defaultValue: 5000
				min: 100
				info: qsTr("Post-warmup iterations per chain, before thinning.")
			}
			IntegerField
			{
				name: "advancedMcmcChains"
				label: qsTr("Chains")
				defaultValue: 3
				min: 1
				info: qsTr("Number of parallel chains.")
			}
			IntegerField
			{
				name: "advancedMcmcThinning"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				info: qsTr("Thinning interval.") + "\n\n" +
					qsTr("With Thinning greater than 1, divergence and treedepth counts cover only retained transitions; events in " +
						"omitted iterations cannot be counted. E-BFMI is unavailable because saved energies are not consecutive. Use " +
						"Thinning = 1 for complete sampler checks.")
			}
			IntegerField
			{
				name: "advancedMcmcSeed"
				label: qsTr("Seed")
				defaultValue: 1
				min: 1
				info: qsTr("Random seed.")
			}
			DoubleField
			{
				name: "advancedMcmcAdaptDelta"
				label: qsTr("Adapt delta")
				defaultValue: 0.97
				min: 0.5
				max: 0.999
				info: qsTr("Target acceptance probability for HMC adaptation (default 0.97). A higher value generally produces smaller " +
						"integration steps and may help resolve divergences, at the cost of longer computation.")
			}
			IntegerField
			{
				name: "advancedMcmcMaxTreeDepth"
				label: qsTr("Max tree depth")
				defaultValue: 15
				min: 5
				max: 20
				info: qsTr("Limit on the depth of HMC's trajectory construction (default 15). Reaching this limit can reduce sampling " +
						"efficiency; increasing it permits longer trajectories and can increase computation time.")
			}
		}
	}
}
