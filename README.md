# The Acceptance Sampling Module

## Overview

The JASP Acceptance Sampling module is an add-on module for JASP that provides comprehensive tools for lot sampling and quality control decisions. The module supports both classical and Bayesian approaches to acceptance sampling, enabling practitioners to create, analyze, and apply sampling plans for attribute and variable data. It offers single and multiple stage attribute sampling plans, variable sampling plans with accept/reject decision support, Bayesian planning and inference using Beta-Binomial models, and a Bayesian state-space model for tracking defect rates over time. The module integrates operating characteristic (OC) curves, average outgoing quality (AOQ) curves, average total inspection (ATI) curves, and posterior predictive visualizations to help quality engineers design and evaluate sampling strategies.

## R Packages

The acceptance sampling functionality is served by several R packages:

- **AcceptanceSampling** — The primary package for classical sampling plan creation and analysis ([AcceptanceSampling on CRAN](https://cran.r-project.org/package=AcceptanceSampling))
- **abtest** — Used for Bayesian hypothesis testing in sampling decisions ([abtest on CRAN](https://cran.r-project.org/package=abtest))
- **rstan** — Stan interface for Bayesian state-space model estimation ([rstan on CRAN](https://cran.r-project.org/package=rstan))

## Analyses

The organization of the analyses within the Acceptance Sampling module in JASP is as follows:

```
--- Acceptance Sampling
    -- Attribute Sampling
       - Create Attribute Plan
       - Analyze Attribute Plan
    -- Variable Sampling
       - Create Variable Plan
       - Analyze Variable Plan
       - Accept/Reject Lots
    -- Bayesian Sampling
       - Bayesian Sampling
       - Bayesian State-Space Sampling
```

## Key Features

**Attribute Sampling Plans:** Create single stage sampling plans that satisfy producer and consumer risk constraints (AQL/RQL). Analyze single and multiple stage plans with support for binomial, hypergeometric, and Poisson distributions.

**Variable Sampling Plans:** Design sampling plans based on continuous measurements with normal distribution assumptions. Evaluate plans against quality levels and make accept/reject decisions for individual lots.

**Bayesian Planning and Inference:** Plan acceptance sampling studies using Beta-Binomial models with customizable priors (uniform, custom, impartial). Perform Bayesian inference on observed lot data with posterior updating, Bayes factor evidence assessment, and posterior predictive distributions.

**Bayesian State-Space Model:** Track defect rates across lots with a two-component innovation mixture that allows ordinary variation and occasional larger changes. Supports multiple separately standardized numeric covariates, configurable priors, automatic computational diagnostics and prediction of the total defects in the current lot. Open the analysis help in JASP for the model, decision rule and updating behavior.

**Visualization:** OC curves, AOQ curves, ATI curves, ASN curves for classical plans. Prior/posterior density plots, posterior predictive distributions, and state trajectory plots for Bayesian analyses.

## Maintainers

- Henrik Godmann (Bayesian analyses)
- JASP Team (Classical analyses)

## Developing the Stan models

Edit `inst/stan/simple.stan` or `inst/stan/complex.stan`. A normal source-package
installation runs `configure` (`configure.win` on Windows), which regenerates the
model C++ before compilation. No hand-editing of generated model headers is needed.
The generation uses the public
[`rstantools::rstan_config()` API](https://mc-stan.org/rstantools/reference/rstan_config.html)
in a temporary directory, preserving this package's custom loader and compiler settings.
Changes to the transport or its include header also trigger model recompilation.

To regenerate the files for a commit, run from the package root:

```sh
Rscript --vanilla tools/check-stan-lockfile.R
Rscript --vanilla tools/update-stan.R --write
Rscript --vanilla tools/update-stan.R --check
Rscript --vanilla tools/test-stan-build.R
R CMD INSTALL --preclean .
```

Use the project's `renv.lock` toolchain for checked-in generated code. The lockfile
check validates the selected Stan packages and their recorded hard dependencies
without loading Stan. Each dependency record needs `Requirements` metadata; use
an empty array (`[]`) for packages with no hard dependencies. Restore that complete
dependency closure into an isolated library before checking or regenerating the
headers; avoid capturing unrelated library upgrades. Include the
regenerated `src/stanExports_*.h`, `src/stanExports_*.cc` and `src/RcppExports.cpp`
with the source change when they differ. The CI generation check runs **before**
installation, so automatic regeneration cannot hide outdated checked-in files.
It fails with a repair command when the files differ. A deliberate generator
upgrade can also change generated output; regenerate and review it with that
updated toolchain.
The Linux generation job restores the locked dependencies first and builds the
locked RStan version from source against them. This avoids mixing a repository
RStan binary with a different TBB ABI. The remaining dependencies can use binaries.

Restart R or the JASP engine after installing a rebuilt package: an existing
process can still hold its previous DLL and cached Stan objects. `--preclean`
also avoids reusing objects after a compiler or dependency upgrade. The numerical
model tests exercise both installed models with the development fallback disabled.
These build checks supplement those tests; they do not establish statistical
validity by themselves.

Changing a prior's numeric settings in JASP only refits the model. It does not
require regeneration or compilation.
