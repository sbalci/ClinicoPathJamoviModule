# Mendelian Randomization

Mendelian Randomization (MR) uses genetic variants as instrumental
variables to infer causal relationships between exposures and outcomes.

## Usage

``` r
mendelianrandomization(
  exposure_data,
  outcome_data,
  use_same_dataset = TRUE,
  snp_column,
  beta_exposure,
  se_exposure,
  pval_exposure = NULL,
  beta_outcome,
  se_outcome,
  pval_outcome = NULL,
  eaf_column = NULL,
  pval_threshold = 5e-08,
  clump_r2 = 0.001,
  clump_kb = 10000,
  mr_methods = "main_three",
  ivw_model = "random",
  egger_bootstrap = FALSE,
  bootstrap_samples = 1000,
  heterogeneity_test = TRUE,
  pleiotropy_test = TRUE,
  leave_one_out = TRUE,
  single_snp_analysis = FALSE,
  mr_presso = FALSE,
  presso_threshold = 0.05,
  plot_forest = TRUE,
  plot_funnel = TRUE,
  plot_scatter = TRUE,
  plot_loo = TRUE,
  harmonize_alleles = TRUE,
  remove_palindromic = TRUE,
  steiger_filtering = FALSE,
  min_snps = 3,
  conf_level = 0.95,
  random_seed = 42
)
```

## Arguments

- exposure_data:

  GWAS summary statistics for exposure variable

- outcome_data:

  GWAS summary statistics for outcome variable (optional if same
  dataset)

- use_same_dataset:

  Whether exposure and outcome are in the same dataset

- snp_column:

  Column containing SNP identifiers (e.g., rsID)

- beta_exposure:

  Effect size (beta) for exposure

- se_exposure:

  Standard error for exposure beta

- pval_exposure:

  P-value for exposure association

- beta_outcome:

  Effect size (beta) for outcome

- se_outcome:

  Standard error for outcome beta

- pval_outcome:

  P-value for outcome association

- eaf_column:

  Effect allele frequency (optional)

- pval_threshold:

  Genome-wide significance threshold (default 5e-8)

- clump_r2:

  R² threshold for LD clumping

- clump_kb:

  Distance window for LD clumping in kilobases

- mr_methods:

  Which MR methods to perform

- ivw_model:

  Model specification for IVW method

- egger_bootstrap:

  Use bootstrap for MR-Egger confidence intervals

- bootstrap_samples:

  Number of bootstrap samples

- heterogeneity_test:

  Perform Cochran Q heterogeneity test

- pleiotropy_test:

  Test for horizontal pleiotropy (MR-Egger intercept)

- leave_one_out:

  Perform leave-one-out sensitivity analysis

- single_snp_analysis:

  Analyze each SNP individually

- mr_presso:

  Detect and correct for outliers using MR-PRESSO

- presso_threshold:

  P-value threshold for MR-PRESSO outlier detection

- plot_forest:

  Generate forest plot of MR estimates

- plot_funnel:

  Generate funnel plot for asymmetry assessment

- plot_scatter:

  Generate scatter plot with method lines

- plot_loo:

  Plot leave-one-out analysis results

- harmonize_alleles:

  Harmonize exposure and outcome effect alleles

- remove_palindromic:

  Remove palindromic SNPs with ambiguous strand

- steiger_filtering:

  Filter SNPs using Steiger test for directionality

- min_snps:

  Minimum number of SNPs required for analysis

- conf_level:

  Confidence level for intervals

- random_seed:

  Random seed for reproducibility

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$instructions`       |     |     |     |     | a html         |
| `results$dataInfo`           |     |     |     |     | a preformatted |
| `results$snpSelection`       |     |     |     |     | a html         |
| `results$selectedSNPs`       |     |     |     |     | a table        |
| `results$mrResults`          |     |     |     |     | a table        |
| `results$heterogeneityTable` |     |     |     |     | a table        |
| `results$pleiotropyTable`    |     |     |     |     | a table        |
| `results$looTable`           |     |     |     |     | a table        |
| `results$pressoResults`      |     |     |     |     | a html         |
| `results$forestPlot`         |     |     |     |     | an image       |
| `results$funnelPlot`         |     |     |     |     | an image       |
| `results$scatterPlot`        |     |     |     |     | an image       |
| `results$looPlot`            |     |     |     |     | an image       |
| `results$interpretation`     |     |     |     |     | a html         |
| `results$assumptions`        |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$selectedSNPs$asDF`

`as.data.frame(results$selectedSNPs)`

## Details

This module supports GWAS summary statistics (NOT individual-level data)
and provides multiple MR methods with comprehensive sensitivity
analysis.

Applications: Genetic epidemiology, drug target validation, causal
inference in observational studies.
