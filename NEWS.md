# jaspAcceptanceSampling Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspAcceptanceSampling (development version)

## Changed
* Bayesian State-Space Sampling accepts multiple numeric covariates with additive effects, separate standardization and independent coefficient priors using one adjustable Cauchy scale. Coefficient outputs identify each covariate, and redundant covariates are rejected.
* Clarified state-space prior and decision-threshold labels and expanded the help with model equations, predictor interpretation, strict decision rules, smoothing/refitting behavior and predictive plot limits. These wording and help changes do not alter calculations.
* Bayesian State-Space Sampling now uses a two-component innovation mixture instead of the Gaussian state model and post-hoc predictive correction supplied in the earlier beta. Recalculating saved beta analyses fits the new model, so results may differ. Applicable data assignments, priors, MCMC settings and decision thresholds are preserved; the removed correction settings are no longer used.

* New state-space analyses default to an adaptation target of 0.97 and a maximum tree depth of 15. Saved explicit sampler settings are retained.
* State-space model selection now follows the covariate assignments. A saved beta analysis with a retained covariate assignment uses that covariate on recalculation, even if its old model selector was set to exclude predictors. Clear the assignment to fit the model without covariates.

## Fixed
* State-space Lot Number accepts numeric Ordinal labels as well as Scale data, preserving numeric lot values and ordering instead of rejecting valid Ordinal input. Clarified that missing values in assigned columns stop the analysis without dropping or imputing lots.
* Pinned the complete Stan toolchain dependency set and added a pre-restore CI check for missing pins. The existing generator versions reproduce the checked-in model headers. The Linux generation job builds the pinned RStan version from source against its locked dependencies to avoid incompatible TBB binaries.
* Strict state-space quality limits correct floating-point noise near whole defect counts. Decision probabilities are tallied directly from predictive draws so exact equality to a decision threshold remains equality.
* State-space predictive plots use bins aligned to whole defect counts, preventing artificial gaps and spikes caused by uneven numbers of counts per bin.
* Escaped percent signs in coefficient credible-interval headings so the translation check passes without changing the displayed labels.
* The initial state is calculated directly on the logit scale with the same Beta prior on its probability, and the binomial likelihood uses stable log-probability terms. This prevents rounding near zero or one from corrupting the target density under extreme custom priors or predictor effects. Saved fits are recalculated for this implementation update.
* State-space predictive probabilities use memory proportional to the observed predictive support rather than the total lot size, preserving empirical probabilities and decision rules.
* Varying numeric covariates are accepted regardless of their units when they can be standardized numerically; values outside the numerical range receive an actionable rescaling error.
* Single-lot state plots show a point and credible interval. Degenerate posterior draws and malformed coefficient draw arrays produce explicit plot errors instead of misleading density curves.
* State-space coefficient density curves extend beyond the sampled range instead of stopping at the central 99% interval, preventing visibly cut-off tails when plot axes are expanded. Posterior draws and coefficient summaries are unchanged.
* Saved state-space fits without the current model-version marker are discarded before recalculation, preventing reuse of an older model's posterior samples.



---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
