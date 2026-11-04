Bayesian Sampling
==========================
*Bayesian Sampling* supports Bayesian planning and inference for acceptance sampling with a Beta prior on the lot proportion defective.

## Input
-------
### 1. Planning
- **Quality constraints**
 - *Acceptable Quality Level (AQL)*: used for the Good/Middle/Bad partition in the three-hypothesis workflow and as the default mode anchor for the impartial two-hypothesis prior.
 - *Rejectable Quality Level (RQL)*: proportion-defective threshold for unacceptable quality and the main threshold used in the two-hypothesis planning and inference workflow.
- **Prior distribution (Beta)**
 - *Impartial (Around RQL)*: assigns 50% prior probability below RQL and 50% above it. Choose *Unimodal* (the default) for an interior peak, or *U-shaped* for an interior minimum between the two endpoint peaks. The location defaults to AQL; enable *Custom prior mode* or *Custom prior minimum* to enter another location. Switching shape retains the location value.
 - For *Unimodal*, RQL must lie strictly between the mode and 0.5. For *U-shaped*, the minimum and RQL must lie on opposite sides of 0.5. Both the location and RQL must be strictly between 0 and 1.
 - A location and RQL both equal to 0.5 leave infinitely many priors of either shape possible. Choose *Custom* to specify the desired parameters. Other incompatible requests also produce an error; the analysis does not automatically change the shape, location or probability split.
 - *Uniform*: Beta(1, 1).
 - *Custom*: user-defined `alpha` and `beta`.
 - *Impartial (Three-region)*: assigns one third of the prior probability to each of Good (below AQL), Middle (between AQL and RQL), and Bad (above RQL). AQL and RQL must satisfy `0 < AQL < RQL < 1`.
 - The three-region shape constraint either allows any Beta shape, excludes U-shapes (`alpha >= 1` or `beta >= 1`), or requires both parameters to be at least 1. If the equal-thirds prior is incompatible with the selected constraint, the analysis reports an error. Change the quality limits or the constraint; it does not silently substitute a different probability split.
- **Planning controls**
 - *(Max) sample size (n)*: search limit for candidate plans.
 - *Bayes factor (min)*: minimum target Bayes factor.
- **Output options**
 - *Plans table*
 - *Prior plot*: available even when the selected sample-size limit and Bayes-factor target yield no feasible plan.
 - *Three-hypothesis BF table*

### 2. Inference
- **Use data to create posterior distribution**
 - *Sample size (n)*
 - *Observed defectives in the sample (d)*
 - *Lot size (N)*
- Counts must satisfy `0 <= d <= n <= N`. The input fields retain inconsistent counts so that the analysis can explain the problem. If defectives exceed the sample size, or the sample size exceeds the lot size, correct whichever entry was mistaken; dependent inference results return after the inputs are valid.
- **Output options**
 - *Inference decision table*
 - *Posterior plot*
 - *Posterior predictive distribution plot*

## Output
-------
- **Procedure text**: describes hypotheses and workflow.
- **Planning outputs**: candidate plans and optional prior/three-hypothesis summaries.
- **Inference outputs**: posterior-based Bayes factor evidence and posterior predictive lot-quality probabilities. Invalid sample counts, lot sizes or Bayes-factor targets produce an explanatory error; correcting the inputs restores the results.
- **Density plots**: adaptive grids resolve narrow peaks and both tails of the prior and posterior. If a density diverges at 0 or 1, its curve continues beyond the finite vertical display range. This affects only the visualization, not the probabilities or Bayes factors.
- **Acceptance number (c)**: the largest number of observed defectives at the specified inference sample size that meets the minimum Bayes factor selected under Planning. "Not available" means that no count meets this target at that sample size.
- **Evidence and target status**: the inference summary and posterior-plot text describe support for a proportion defective below versus at or above the RQL, then separately state whether the minimum Bayes-factor target from Planning was reached. Equality meets the target; not reaching it does not by itself imply rejection. Evidence labels and target comparisons use the unrounded Bayes factor. A target at or below 1 can be reached without evidence favoring a proportion defective below the RQL.

## Notes
-------
- The posterior uses Beta-Binomial updating from the selected prior and observed `(n, d)`.
- Posterior predictive results are computed for lot-level defectives conditional on observed sample data. Beta-binomial count probabilities are evaluated using adjacent-count ratios and normalized over their full support; this avoids loss of precision for highly concentrated priors without changing the predictive model.
- In the standard two-hypothesis workflow, planning targets evidence about whether the lot proportion defective is below the RQL.
- AQL directly affects the three-hypothesis summaries and can also affect the two-hypothesis analysis indirectly through the default impartial prior mode.

Bayes factors are calculated from log probabilities without clipping the hypothesis-region probabilities. Planning, acceptance numbers and evidence labels use these log-scale values. Differences within a small floating-point round-off allowance are treated as ties, so a numerical tie meets the selected target. Very large or small Bayes factors are displayed in scientific notation; the displayed region probabilities may round to 0 or 1 without affecting the Bayes-factor calculation.

For the two-region impartial prior, calibration checks the returned Beta parameters: the selected shape must hold strictly, and the location and probability below RQL must match their requested values within an absolute error of 1e-8. A failure is reported rather than using an approximate prior outside that tolerance. For the three-region impartial prior, two nested root searches match the cumulative probabilities at AQL and RQL to 1/3 and 2/3. All three resulting region probabilities must be within an absolute error of 1e-8 of 1/3. This is a numerical accuracy check, not a statistical uncertainty interval. The search has no fixed upper bound on the Beta parameters; very close quality limits can imply a highly concentrated prior. If numerical accuracy cannot be verified, an explicit calibration error is shown. Shape boundaries at `alpha = 1` or `beta = 1` are allowed only when the resulting prior also passes the same probability checks.
