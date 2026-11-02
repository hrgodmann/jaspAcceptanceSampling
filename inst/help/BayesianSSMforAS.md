Bayesian State-Space Sampling
=============================
*Bayesian State-Space Sampling* performs acceptance sampling with a Bayesian state-space model for defect rates over lots.

## Input
-------
### Data assignment
- **Defect Count**: observed defects in each sampled lot.
- **Sample Size**: number of tested items per lot.
- **Lot Size**: total items per lot.
- **Lot Number**: unique numeric sequence/order index for each lot, with measurement type **Scale** or **Ordinal**. Ordinal columns must have numeric labels; their numeric values determine the order, regardless of category-level order. Ordered text labels are not supported.
- **Covariates (Optional)**: assign one or more numeric covariates to include their additive effects in the model. Leave this field empty to fit the model without predictors. Adding or removing a covariate refits the model. The common coefficient prior scale can be set under Model before assigning covariates; coefficient plots and diagnostic controls are shown when covariates are assigned. Categorical predictors and interaction terms are not supported.

Use one row per lot. Rows are sorted by Lot Number, and the current lot is the one with the largest number. Duplicate lot numbers are rejected. If further items from the same lot are inspected, update its existing Sample Size and Defect Count rather than appending another cumulative-inspection row.

Lot numbers determine order only. Their spacing does not change the state-transition variance: lots numbered 1, 2, 3 and lots numbered 1, 10, 100 have the same model transitions when their data are otherwise identical.

Defect Count, Sample Size and Lot Size must be finite, nonnegative whole numbers, with Defect Count ≤ Sample Size ≤ Lot Size. Fractional values are rejected rather than truncated; counts must be smaller than 2,147,483,647 to fit the implementation's integer indexing. Zero inspected items, zero observed defects, all inspected items defective, and full inspection are supported. A zero-size lot is allowed only with zero inspected items and zero observed defects; it still occupies a position in the state sequence. A single lot is supported without a predictor.

Every assigned column must be complete. A missing or non-finite value in Lot Number, Defect Count, Sample Size, Lot Size, or an assigned covariate stops the analysis with an error naming the affected field; no acceptance decision is produced. Lots are not automatically dropped, and missing values are neither imputed nor treated as zero. Unassigned columns do not affect the analysis. For a genuinely uninspected lot, enter Sample Size = 0 and Defect Count = 0; do not replace an unknown count from an inspected sample with zero.

With covariates, at least two lots are required, and every selected covariate must contain varying, finite numeric values. Each covariate is centered at its own mean and divided by its own sample standard deviation over all supplied rows, without weighting by sample size. Appending lots can change this scaling and hence the meaning of a one-SD effect. The Model and Priors sections explain the coefficients and their priors.

The analysis rejects linearly dependent covariates, such as duplicate variables or a variable that is an exact linear combination of the others after centering. Remove redundant covariates before fitting. Strongly correlated covariates can still be difficult to distinguish, and covariates that track time can be difficult to distinguish from changes in the latent baseline. For short series, use a small number of covariates with a clear substantive purpose.

### Updating data and refitting
Each fit uses all supplied lots jointly. To inspect more items from the current lot, update its cumulative Sample Size and Defect Count in the existing row and rerun the analysis. Append a row only for a new lot. The interface does not automatically inspect items or run an item-by-item optional-stopping loop.

Changing the supplied data, covariate assignments, priors or MCMC settings requires a new fit of the supplied series. Changing quality limits, decision thresholds or displayed outputs reuses the fitted posterior samples; it does not change the model posterior.

### Settings and decision rule
AQL and RQL are defect **proportions**, entered between 0 and 1. For example, an AQL of 0.025 corresponds to 25 defects in a lot of size 1,000. Let $D_T$ be the total number of defects in the current lot, $s_T$ its size, and $\mathcal D$ all supplied observations. The decision uses the posterior predictive probabilities

$$
L = \Pr(D_T < \mathrm{AQL} s_T \mid \mathcal D),
\qquad
H = \Pr(D_T > \mathrm{RQL} s_T \mid \mathcal D).
$$

Let $a$ be the **Acceptance threshold** (default 0.10) and $r$ the **Rejection threshold** (default 0.05). The result is:

- **Consider to Accept** if $H < a$.
- **Consider to Reject** if $H > a$ and $L < r$.
- **Consider to Continue** otherwise.

The fields beside **Accept if** and **Reject if** set $a$ and $r$, respectively. The rejection label also displays the current value of $a$, so both required conditions are visible.

All inequalities are strict. In particular, $H=a$ gives Continue. Counts exactly at a quality limit are excluded from its corresponding strict tail. Products within floating-point precision of an integer are treated as that integer; genuinely fractional count limits are not rounded. These probabilities concern the total defects in the finite lot, including observed and uninspected items, rather than the latent defect probability alone. The count-based expressions also apply to a zero-size lot, for which a defect proportion is undefined.

The thresholds express model-based posterior decision criteria. They do not by themselves guarantee frequentist producer's or consumer's error rates across repeated samples.

### Model
For lot $\ell$, let $y_\ell$ be its observed defects, $n_\ell$ its inspected items and $\theta_\ell$ its latent defect probability. The observation model is

$$
y_\ell \mid \theta_\ell \sim \mathrm{Binomial}(n_\ell,\theta_\ell).
$$

Let $p_0$ denote the initial baseline probability (the $\theta_1$ prior controls in the interface) and $\nu_\ell$ the baseline log odds. Across lots,

$$
\nu_1=\operatorname{logit}(p_0),
\qquad
\nu_\ell=\nu_{\ell-1}+\eta_\ell,\quad \ell=2,\ldots,T,
$$

with innovations

$$
\eta_\ell\mid\sigma_\eta \sim
(1-q) \mathcal N(0,\sigma_\eta^2)
+q \mathcal N(0,\sigma_\eta^2+\delta^2),
\qquad q=0.05,\quad\delta=1.5.
$$

The second argument of $\mathcal N$ is its variance. The regular component has SD $\sigma_\eta$, the **baseline innovation scale** estimated from the data. The broad component has SD $\sqrt{\sigma_\eta^2+1.5^2}$ and accommodates larger changes. Both are centered at zero, allowing quality to improve or deteriorate. The baseline scale is not the SD of the entire mixture.

The mixture weight $q$ and additional spread $\delta$ are fixed model settings; $\delta$ is on the logit scale. Conditional on the shared baseline scale, innovations are independent draws from this mixture. A particular transition can receive stronger posterior support for the broad component, but the shared prior weight stays at 0.05. The model does not estimate probabilities of switching between persistent regimes.

Without predictors, $\theta_\ell=\operatorname{logit}^{-1}(\nu_\ell)$. With $K$ numeric covariates,

$$
\theta_\ell=\operatorname{logit}^{-1}\left(\nu_\ell+\sum_{k=1}^{K}\beta_k X_{\ell k}^{\ast}\right),
\qquad
X_{\ell k}^{\ast}=\frac{X_{\ell k}-\bar X_k}{s_{X_k}},
$$

where $s_{X_k}$ is covariate $k$'s sample SD over the supplied lots and each coefficient $\beta_k$ is shared across lots. Equivalently, the vector of log odds is $\boldsymbol\mu=\boldsymbol\nu+X\boldsymbol\beta$, where $X$ contains the standardized covariate columns. No additional intercept is included because the latent baseline already supplies it.

A one-SD increase in covariate $k$ changes the log odds by $\beta_k$, holding the other covariates and the baseline fixed; it multiplies the odds by $\exp(\beta_k)$. In this mode, $p_0$ is the first lot's baseline probability when all covariates are at their means, and its actual defect probability also includes the combined covariate contribution.

### Priors
The following priors are independent. Their hyperparameters are adjustable in the Model section.

| Parameter | Prior | Default settings |
|---|---|---|
| Initial baseline probability $p_0$ | Beta | Shape parameters 1 and 1 (uniform) |
| Baseline innovation scale $\sigma_\eta$ | Half-normal | Scale 1 |
| Each predictor coefficient $\beta_k$ | Independent Cauchy, centered at zero | Common scale 0.707 |

The **Half-normal prior scale** is the SD of the underlying zero-centered normal distribution before restricting it to positive values; it is not the SD of the resulting half-normal distribution. This control sets the width of the prior for $\sigma_\eta$, which is estimated from the data. The half-normal prior favors smaller baseline scales while allowing larger values.

The **Cauchy scale** control applies separately to every coefficient; the coefficients are not constrained to be equal. Each prior is expressed per one SD of its covariate. In original covariate units, the coefficient is $\beta_k/s_{X_k}$, and its prior scale is the chosen Cauchy scale divided by $s_{X_k}$. Because covariates are standardized over the supplied dataset at each fit, appending lots can change these original-unit priors.

Adding covariates can broaden the prior on their combined contribution to the log odds even when the individual coefficient scale stays unchanged. The model does not automatically increase shrinkage as more covariates are added. Choose covariates and priors using relevant process knowledge and assess sensitivity when the series is short or effects are weakly identified.

### MCMC
- **Burnin**: warmup iterations per chain.
- **Samples**: post-warmup iterations per chain, before thinning.
- **Chains**, **Thin** and **Seed**: parallel chains, thinning interval and random seed.
- **Adapt Delta**: target acceptance probability for HMC adaptation (default 0.97). A higher value generally produces smaller integration steps and may help resolve divergences, at the cost of longer computation.
- **Max tree depth**: limit on the depth of HMC's trajectory construction (default 15). Reaching this limit can reduce sampling efficiency; increasing it permits longer trajectories and can increase computation time.

Under **MCMC Diagnostics**, **Print Diagnostics** displays the full computational diagnostic report and is off by default. The checks always run, and warnings about problems or unavailable checks are shown regardless of this option. **Overview Table** separately displays posterior summaries and traditional MCMC diagnostics for the selected parameters.

## Output
-------
- **Posterior Summary (Current Lot)**: observed defects, posterior predictive summary, and acceptance decision. A decision-specific note explains the proposal using the selected probability thresholds; the calculated probabilities remain in the table.
- **Predictor Coefficients** (automatically shown when covariates are included): posterior mean, median, and equal-tailed 95% credible interval (2.5th and 97.5th percentiles) for each coefficient. These describe changes in the log-odds of a defect per one SD increase in the covariate, holding the latent baseline and other covariates fixed.
- **Computational Diagnostics** (when Print Diagnostics is selected): sampler and convergence checks for all sampled parameters.
- **Overview Table** (optional): mean, SD, ESS, and R-hat for selected parameters.
- **Plots** (as selected):
 - Defect-rate state trajectory over lots: posterior means with pointwise 95% credible intervals.
 - Posterior predictive distribution of total defects in the current lot: observed defects plus predicted defects among its uninspected items. It cannot contain fewer defects than already observed; after inspecting the entire lot, its total defect count is known.
 - Posterior distribution of latest defect rate.
 - Posterior distributions of predictor coefficients, labeled by covariate.

The coefficient density plots use all posterior draws and extend beyond the sampled range so that the tails approach zero. The default axes also include zero as a reference. Adjusting the axes changes the display, not the posterior draws or coefficient summaries. With weakly identified effects, Cauchy tails can make coefficient means and SDs unstable or undefined. Inspect the computational diagnostics and assess prior sensitivity before interpreting such summaries.

### Reading the state trajectory
The trajectory shows the lot-specific defect probabilities, including any covariate contributions. It conditions on all supplied observations. Estimates for earlier lots therefore use later supplied observations as well: this is **smoothing**. They can change when more lots are added and should not be read as the estimates or decisions that would have been available when each earlier lot was inspected. The current lot is the last lot in the supplied sequence; its prediction uses observations available in that dataset.

### Prediction for the current lot
For current lot $T$, with $s_T$ total items, $n_T$ inspected and $y_T$ observed defects, the model predicts the remaining defects $Y_{\mathrm{rem},T}$ and total $D_T$ as

$$
Y_{\mathrm{rem},T}\mid\theta_T
\sim \mathrm{Binomial}(s_T-n_T,\theta_T),
\qquad D_T=y_T+Y_{\mathrm{rem},T}.
$$

The posterior predictive distribution averages this conditional distribution over the posterior uncertainty in $\theta_T$. It combines uncertainty about the defect probability with binomial variability among the uninspected items.

The predictive plot displays approximately the central 99.8% range (0.1st–99.9th percentiles), expanded to include AQL/RQL count limits and rounded axis limits. Very small tails may fall outside the displayed range. The bins contain equal numbers of whole defect counts, with one bar per count for short ranges. Bars show the probability mass in each bin and are **not renormalized** after selecting the display range. The summary probabilities and decision always use the full predictive distribution, including any undisplayed tails.

Valid predictions range from the observed defect count to that count plus the number of uninspected items; a fully inspected lot has probability one at its observed total. If predictive draws are missing, non-finite, noninteger or outside this range, or their probabilities do not sum to one, the analysis reports an error instead of an acceptance decision. A divergent transition alone does not make a predictive draw invalid; these draws are retained and their computational warnings should be investigated.

### Computational checks
Check computational diagnostics before interpreting the results. Select **MCMC Diagnostics → Print Diagnostics** to see the full report. The checks run even when this option is off, and any warnings remain visible alongside the results. All retained post-warmup draws are used, including those associated with divergent transitions. Warnings qualify the posterior summaries and decisions; they do not remove draws or suppress these outputs.

The checks assess all sampled parameters: the initial baseline log odds (`mu1`), baseline innovation scale, variables used to construct the innovations (`z_raw`), and every predictor coefficient when present. They report divergences, maximum-treedepth hits, rank-normalized split R-hat (the maximum of its folded and unfolded versions), bulk ESS, tail ESS, and per-chain E-BFMI. RStan's `Rhat()`, `ess_bulk()` and `ess_tail()` functions calculate the mixing diagnostics from iteration-ordered chains. E-BFMI is `mean(diff(energy)^2) / var(energy)`, calculated separately for each chain.

Warnings flag any retained divergence or treedepth hit, R-hat at or above 1.01, bulk or tail ESS below 100 times the number of chains, and E-BFMI below 0.3. These are screening thresholds, not guarantees of convergence. Divergences and poor mixing can compromise estimates; treedepth saturation can reduce efficiency. See [Stan's diagnostic guidance](https://mc-stan.org/learn-stan/diagnostics-warnings.html) for investigation and remedies.

A single chain cannot establish between-chain agreement, so the rank-normalized split R-hat check is unavailable. At least six retained draws per chain are required before computing the mixing diagnostics; this is only a computational minimum. Constant or non-finite sampled-parameter chains and missing diagnostics trigger explicit qualifications. Constant generated quantities, such as a known total after complete inspection, do not by themselves indicate sampler failure. Unavailable checks should not be interpreted as evidence of convergence.

With Thin greater than 1, divergence and treedepth counts cover only retained transitions; events in omitted iterations cannot be counted. E-BFMI is unavailable because saved energies are not consecutive. Use Thin = 1 for complete sampler checks.

The **Overview Table** summarizes the parameters selected below it. **Sigma (state noise)** refers to the baseline innovation scale $\sigma_\eta$. This table's ESS (`n_eff`) and R-hat columns use RStan's traditional raw-draw ESS and unranked split R-hat, which differ from the rank-based diagnostics in the full computational report. Undefined diagnostics for constant generated quantities should not be read as successful convergence checks.
