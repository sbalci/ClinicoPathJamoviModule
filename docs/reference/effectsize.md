# Comprehensive Effect Size Analysis

Comprehensive effect size analysis toolkit with BlueSky R integration.
Includes Cohen's d, Hedges' g, Glass' delta, eta-squared, omega-squared,
Cramér's V, phi coefficient, rank-based effect sizes, and more.
Essential for clinical significance assessment, meta-analyses, power
analyses, and reproducible research in medical and pathological studies.

## Usage

``` r
effectsize(
  data,
  dep,
  group,
  analysisType = "two_sample",
  measures_cohens_d = TRUE,
  measures_hedges_g = TRUE,
  measures_glass_delta = FALSE,
  testValue = 0,
  group1Value = "",
  group2Value = "",
  ciWidth = 95,
  interpretation = TRUE,
  descriptives = TRUE,
  testDetails = TRUE,
  assumptionChecks = FALSE,
  plotEffects = TRUE,
  plotType = "forest",
  plotWidth = 600,
  plotHeight = 400,
  measures_eta_squared = FALSE,
  measures_partial_eta_squared = FALSE,
  measures_omega_squared = FALSE,
  measures_epsilon_squared = FALSE,
  measures_cramers_v = FALSE,
  measures_phi_coefficient = FALSE,
  measures_cohens_w = FALSE,
  measures_rank_biserial = FALSE,
  measures_cliff_delta = FALSE,
  measures_vargha_delaney_a = FALSE,
  measures_common_language = FALSE,
  measures_cohens_u3 = FALSE,
  measures_probability_superiority = FALSE,
  analysis_context = "ttest",
  correction_method = "none",
  bootstrap_ci = FALSE,
  bootstrap_samples = 1000,
  pooled_sd = TRUE,
  welch_correction = FALSE,
  paired_analysis = FALSE,
  pairing_variable,
  clinical_thresholds = FALSE,
  small_effect_threshold = 0.2,
  medium_effect_threshold = 0.5,
  large_effect_threshold = 0.8,
  power_analysis = FALSE,
  alpha_level = 0.05,
  desired_power = 0.8,
  meta_analysis_format = FALSE,
  study_weights,
  bluesky_integration = TRUE,
  comprehensive_output = FALSE,
  effect_size_family = "smd",
  forest_plot_advanced = FALSE,
  effect_size_distribution = FALSE,
  comparison_plot = FALSE,
  export_format = "standard"
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  The continuous outcome variable

- group:

  Variable to define groups for comparison (optional for one-sample
  analysis)

- analysisType:

  Type of effect size analysis

- measures_cohens_d:

  Calculate Cohen's d effect size

- measures_hedges_g:

  Calculate Hedges' g effect size (bias-corrected)

- measures_glass_delta:

  Calculate Glass' delta effect size

- testValue:

  Reference value for one-sample analysis

- group1Value:

  Specific value for first group (optional)

- group2Value:

  Specific value for second group (optional)

- ciWidth:

  Confidence level for effect size confidence intervals

- interpretation:

  Provide Cohen's (1988) interpretation guidelines

- descriptives:

  Show group descriptive statistics

- testDetails:

  Show t-test statistics and p-values

- assumptionChecks:

  Perform normality and homogeneity of variance tests

- plotEffects:

  Create visualization of effect sizes with confidence intervals

- plotType:

  Type of effect size visualization

- plotWidth:

  Width of effect size plot

- plotHeight:

  Height of effect size plot

- measures_eta_squared:

  Calculate eta-squared effect size (ANOVA)

- measures_partial_eta_squared:

  Calculate partial eta-squared effect size

- measures_omega_squared:

  Calculate omega-squared effect size (unbiased)

- measures_epsilon_squared:

  Calculate epsilon-squared effect size

- measures_cramers_v:

  Calculate Cramér's V for categorical associations

- measures_phi_coefficient:

  Calculate phi coefficient for 2x2 contingency tables

- measures_cohens_w:

  Calculate Cohen's w for chi-square goodness of fit

- measures_rank_biserial:

  Calculate rank-biserial correlation for Mann-Whitney U

- measures_cliff_delta:

  Calculate Cliff's delta for ordinal data

- measures_vargha_delaney_a:

  Calculate Vargha-Delaney A measure

- measures_common_language:

  Calculate common language effect size (CLES)

- measures_cohens_u3:

  Calculate Cohen's U3 statistic

- measures_probability_superiority:

  Calculate probability of superiority

- analysis_context:

  Statistical context for effect size calculation

- correction_method:

  Correction method for multiple effect sizes

- bootstrap_ci:

  Use bootstrap methods for confidence intervals

- bootstrap_samples:

  Number of bootstrap samples

- pooled_sd:

  Use pooled standard deviation for effect size calculations

- welch_correction:

  Apply Welch correction for unequal variances

- paired_analysis:

  Analyze paired/matched data

- pairing_variable:

  Variable indicating pairs/matches

- clinical_thresholds:

  Apply clinical significance thresholds

- small_effect_threshold:

  Threshold for small effect size

- medium_effect_threshold:

  Threshold for medium effect size

- large_effect_threshold:

  Threshold for large effect size

- power_analysis:

  Include post-hoc power analysis

- alpha_level:

  Significance level for power calculations

- desired_power:

  Desired statistical power

- meta_analysis_format:

  Format output for meta-analysis software

- study_weights:

  Variable containing study weights for meta-analysis

- bluesky_integration:

  Use BlueSky R statistical environment features

- comprehensive_output:

  Include comprehensive statistical details

- effect_size_family:

  Focus on specific family of effect sizes

- forest_plot_advanced:

  Create publication-ready forest plot

- effect_size_distribution:

  Show sampling distribution of effect sizes

- comparison_plot:

  Compare multiple effect size measures

- export_format:

  Output formatting style

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$results$instructions` |  |  |  |  | a html |
| `results$results$effectSizes` |  |  |  |  | Effect size measures with confidence intervals |
| `results$results$descriptives` |  |  |  |  | Group descriptive statistics |
| `results$results$testDetails` |  |  |  |  | t-test statistics and significance |
| `results$results$assumptions` |  |  |  |  | Tests of normality and variance homogeneity |
| `results$results$comprehensiveEffectSizes` |  |  |  |  | All requested effect size measures with interpretations |
| `results$results$varianceExplained` |  |  |  |  | Eta-squared, omega-squared, and related measures |
| `results$results$associationMeasures` |  |  |  |  | Cramér's V, phi coefficient, and association measures |
| `results$results$rankBasedEffects` |  |  |  |  | Non-parametric effect size measures |
| `results$results$commonLanguageEffects` |  |  |  |  | Intuitive probability-based effect sizes |
| `results$results$powerAnalysisResults` |  |  |  |  | Statistical power calculations for observed effects |
| `results$results$metaAnalysisFormat` |  |  |  |  | Formatted output for meta-analysis software |
| `results$results$clinicalSignificance` |  |  |  |  | Clinical significance evaluation using thresholds |
| `results$results$bootstrapResults` |  |  |  |  | Bootstrap-based confidence intervals for effect sizes |
| `results$results$effectSizeGuide` |  |  |  |  | a html |
| `results$results$clinicalGuidance` |  |  |  |  | a html |
| `results$results$methodsExplanation` |  |  |  |  | a html |
| `results$results$effectPlot` |  |  |  |  | Plot showing effect sizes with confidence intervals |
| `results$results$advancedForestPlot` |  |  |  |  | Publication-ready forest plot of effect sizes |
| `results$results$effectDistributionPlot` |  |  |  |  | Sampling distribution of effect sizes |
| `results$results$comparisonPlot` |  |  |  |  | Comparison of multiple effect size measures |
| `results$results$powerCurvePlot` |  |  |  |  | Power curves for different effect sizes and sample sizes |
