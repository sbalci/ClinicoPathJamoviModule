# Non-Parametric Statistical Methods

Comprehensive non-parametric statistical methods including
Kruskal-Wallis test, Friedman test, Mann-Whitney U test, and robust
alternatives to classical procedures. Designed specifically for clinical
research where normality assumptions are violated. This enhanced module
combines distribution-free methods with proper effect size calculations,
assumption checking, and post hoc analysis - addressing critical gaps
identified in 30 percent of pathology studies that use these methods.

## Usage

``` r
nonparametric(
  data,
  deps,
  outcome,
  groups,
  paired_variable = NULL,
  blocking_variable = NULL,
  test_type = "mann_whitney",
  effect_size = TRUE,
  effect_size_method = "eta_squared",
  confidence_intervals = TRUE,
  confidence_level = 0.95,
  post_hoc = TRUE,
  post_hoc_method = "dunn",
  p_adjustment = "holm",
  globalTestCount = 1,
  robust_method = "standard",
  trim_proportion = 0.1,
  winsorize_proportion = 0.1,
  bootstrap_ci = FALSE,
  bootstrap_samples = 1000,
  test_assumptions = TRUE,
  normality_tests = TRUE,
  assumption_checks = TRUE,
  homogeneity_test = "levene",
  exact_test = FALSE,
  exact_p_values = TRUE,
  continuity_correction = TRUE,
  tie_correction = TRUE,
  ties_method = "average",
  show_boxplots = TRUE,
  show_violin_plots = FALSE,
  show_rank_plots = FALSE,
  show_effect_plots = TRUE,
  descriptive_plots = TRUE,
  show_qqplots = FALSE,
  show_descriptives = TRUE,
  show_test_statistics = TRUE,
  show_post_hoc_table = TRUE,
  show_effect_sizes = TRUE,
  show_assumptions = TRUE,
  show_robust_statistics = FALSE,
  show_power_analysis = FALSE,
  show_instructions = TRUE,
  show_explanations = FALSE,
  show_interpretation = FALSE,
  show_recommendations = FALSE,
  clinical_context = "general",
  set_seed = TRUE,
  seed_value = 42,
  missing_data_handling = "listwise",
  alpha_level = 0.05,
  minimum_sample_size = 5,
  outlier_method = "iqr",
  small_sample_exact = TRUE,
  report_standardized_statistics = FALSE
)
```

## Arguments

- data:

  The dataset to be analyzed, provided as a data frame.

- deps:

  Multiple continuous variables to be analyzed. These should be numeric
  variables representing biomarker expression levels, cell counts,
  morphometric measurements, or other quantitative pathology data.

- outcome:

  Single continuous outcome variable. Use either this OR 'deps' for
  multiple variables.

- groups:

  Categorical variable defining the groups to be compared. For
  Mann-Whitney U test, this should have exactly 2 levels. For
  Kruskal-Wallis test, can have 2+ levels.

- paired_variable:

  Variable identifying paired observations for repeated measures
  designs.

- blocking_variable:

  Variable identifying blocks for Friedman test designs.

- test_type:

  Select the appropriate non-parametric test based on your study design.

- effect_size:

  Calculate appropriate effect sizes for non-parametric tests with
  confidence intervals.

- effect_size_method:

  Method for calculating effect sizes appropriate to the chosen test: -
  Eta-squared/Epsilon-squared: For Kruskal-Wallis (multi-group) -
  Rank-biserial correlation: For Wilcoxon tests (paired data)

  - Cliff's Delta: For Mann-Whitney U tests (two independent groups) -
    CLES/Vargha-Delaney A: Probability-based effect sizes - Kendall's W:
    For Friedman test (repeated measures concordance)

- confidence_intervals:

  Calculate bootstrap confidence intervals for effect sizes using BCa
  method.

- confidence_level:

  Confidence level for confidence intervals and descriptive statistics.

- post_hoc:

  Perform post-hoc pairwise comparisons when overall test is
  significant.

- post_hoc_method:

  Method for post hoc pairwise comparisons.

- p_adjustment:

  Method for correcting p-values in multiple comparisons.

- globalTestCount:

  Total number of independent tests performed across the entire study
  (for global familywise error rate control). When \> 1, all reported
  p-values are additionally multiplied by this count (Bonferroni). Set
  to 1 (default) to disable global correction.

- robust_method:

  Method for robust rank-based estimation.

- trim_proportion:

  Proportion of observations to trim from each end for robust
  estimation.

- winsorize_proportion:

  Proportion of observations to winsorize from each end.

- bootstrap_ci:

  Use bootstrap methods for confidence interval estimation.

- bootstrap_samples:

  Number of bootstrap resamples for confidence interval estimation.

- test_assumptions:

  Perform comprehensive assumption checking for non-parametric tests.

- normality_tests:

  Test normality of data to justify use of non-parametric methods.

- assumption_checks:

  Perform detailed assumption checks including independence,
  distribution shape, and outlier assessment.

- homogeneity_test:

  Test for homogeneity of variance between groups.

- exact_test:

  Use exact distribution calculations instead of asymptotic
  approximations.

- exact_p_values:

  Use exact p-values when computationally feasible.

- continuity_correction:

  Apply continuity correction for better normal approximation.

- tie_correction:

  Apply correction for tied observations in rank-based tests.

- ties_method:

  Method for handling tied ranks in calculations.

- show_boxplots:

  Generate box plots for visual group comparison.

- show_violin_plots:

  Generate violin plots showing distribution shapes.

- show_rank_plots:

  Generate plots showing rank distributions.

- show_effect_plots:

  Generate visualization of effect sizes with confidence intervals.

- descriptive_plots:

  Generate comprehensive set of descriptive plots including box plots,
  violin plots, and distribution comparisons.

- show_qqplots:

  Generate Q-Q plots for normality assessment.

- show_descriptives:

  Display comprehensive descriptive statistics.

- show_test_statistics:

  Display main non-parametric test results table.

- show_post_hoc_table:

  Display post-hoc pairwise comparison results.

- show_effect_sizes:

  Display detailed effect size calculations and interpretations.

- show_assumptions:

  Display assumption testing results and implications.

- show_robust_statistics:

  Display robust statistical estimates using alternative methods.

- show_power_analysis:

  Perform post-hoc power analysis and sample size recommendations.

- show_instructions:

  Display comprehensive instructions for using non-parametric methods.

- show_explanations:

  Display detailed explanations of non-parametric methods and
  assumptions.

- show_interpretation:

  Provide plain-language interpretation of statistical results.

- show_recommendations:

  Provide recommendations for follow-up analyses and study design.

- clinical_context:

  Clinical context for tailored interpretation and recommendations.

- set_seed:

  Set random seed for reproducible bootstrap and permutation results.

- seed_value:

  Specific seed value for reproducible random number generation.

- missing_data_handling:

  Method for handling missing data in non-parametric analyses.

- alpha_level:

  Significance level for hypothesis testing (default: 0.05).

- minimum_sample_size:

  Minimum sample size per group before showing adequacy warnings.

- outlier_method:

  Method for identifying outliers in the data.

- small_sample_exact:

  Automatically use exact methods when sample sizes are small (n \< 20
  per group).

- report_standardized_statistics:

  Report standardized versions of test statistics for better
  comparability.

## Value

A results object containing:

|                                      |     |     |     |     |                |
|--------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                    |     |     |     |     | a preformatted |
| `results$instructions`               |     |     |     |     | a html         |
| `results$descriptives`               |     |     |     |     | a table        |
| `results$normality`                  |     |     |     |     | a table        |
| `results$assumptions`                |     |     |     |     | a table        |
| `results$tests`                      |     |     |     |     | a table        |
| `results$effectsizes`                |     |     |     |     | a table        |
| `results$posthoc`                    |     |     |     |     | a table        |
| `results$robustStatistics`           |     |     |     |     | a table        |
| `results$powerAnalysis`              |     |     |     |     | a table        |
| `results$distributionplot`           |     |     |     |     | an image       |
| `results$boxplots`                   |     |     |     |     | an image       |
| `results$violinplots`                |     |     |     |     | an image       |
| `results$rankplots`                  |     |     |     |     | an image       |
| `results$effectsizeplots`            |     |     |     |     | an image       |
| `results$qqplots`                    |     |     |     |     | an image       |
| `results$methodExplanation`          |     |     |     |     | a html         |
| `results$effectSizeExplanation`      |     |     |     |     | a html         |
| `results$postHocExplanation`         |     |     |     |     | a html         |
| `results$assumptionExplanation`      |     |     |     |     | a html         |
| `results$robustMethodsExplanation`   |     |     |     |     | a html         |
| `results$resultInterpretation`       |     |     |     |     | a html         |
| `results$clinicalInterpretation`     |     |     |     |     | a html         |
| `results$methodsExplanation`         |     |     |     |     | a html         |
| `results$statisticalRecommendations` |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$descriptives$asDF`

`as.data.frame(results$descriptives)`
