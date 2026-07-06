# Advanced Statistical Tests

Advanced statistical tests using functions from the DescTools package.
Includes effect size calculations (Cohen's D), goodness-of-fit tests
(Hosmer-Lemeshow, Anderson-Darling), agreement tests (Barnard,
Breslow-Day), and trend tests (Cochran-Armitage). Essential for clinical
research, epidemiological studies, and advanced statistical analysis.

## Usage

``` r
desctools(
  data,
  effect_size_analysis = FALSE,
  group_var = NULL,
  continuous_var = NULL,
  pooled_sd = TRUE,
  hedges_correction = FALSE,
  effect_ci_level = 0.95,
  goodness_of_fit = FALSE,
  fitted_probs = NULL,
  observed_outcomes = NULL,
  hl_groups = 10,
  normality_var = NULL,
  categorical_tests = FALSE,
  cat_var1 = NULL,
  cat_var2 = NULL,
  stratum_var = NULL,
  ordered_exposure = NULL,
  binary_outcome = NULL,
  multiple_testing = "none",
  show_effect_sizes = TRUE,
  show_goodness_tests = TRUE,
  show_categorical_tests = TRUE,
  show_interpretations = TRUE
)
```

## Arguments

- data:

  The data as a data frame for statistical analysis.

- effect_size_analysis:

  Calculate effect sizes including Cohen's D for comparing group means.
  Essential for determining practical significance in clinical studies.

- group_var:

  Categorical variable defining groups for effect size comparison.
  Should have exactly 2 levels for Cohen's D calculation.

- continuous_var:

  Continuous variable for effect size calculation (outcome measure).

- pooled_sd:

  Use pooled standard deviation for Cohen's D calculation. Recommended
  when group variances are similar.

- hedges_correction:

  Apply Hedges correction for small sample bias in effect size
  calculation.

- effect_ci_level:

  Confidence level for effect size confidence intervals.

- goodness_of_fit:

  Perform goodness of fit tests including Hosmer-Lemeshow test for
  logistic regression models and normality tests.

- fitted_probs:

  Fitted probabilities from a logistic regression model for
  Hosmer-Lemeshow goodness of fit test.

- observed_outcomes:

  Observed binary outcomes (0/1) for Hosmer-Lemeshow test.

- hl_groups:

  Number of groups for Hosmer-Lemeshow test. Usually 10 groups provide
  good balance between power and stability.

- normality_var:

  Continuous variable for normality testing using Anderson-Darling and
  other goodness of fit tests.

- categorical_tests:

  Perform advanced tests for categorical data including Barnard's test,
  Breslow-Day test, and Cochran-Armitage trend test.

- cat_var1:

  First categorical variable for contingency table analysis.

- cat_var2:

  Second categorical variable for contingency table analysis.

- stratum_var:

  Stratification variable for Breslow-Day test of homogeneity of odds
  ratios across strata.

- ordered_exposure:

  Ordered exposure variable for Cochran-Armitage trend test. Should
  represent ordered dose or exposure levels.

- binary_outcome:

  Binary outcome variable for Cochran-Armitage trend test.

- multiple_testing:

  Method for correcting p-values when multiple tests are performed. FDR
  (Benjamini-Hochberg) is recommended for most clinical studies.

- show_effect_sizes:

  Display effect size calculations including Cohen's D with confidence
  intervals and interpretation.

- show_goodness_tests:

  Display goodness of fit test results including Hosmer-Lemeshow and
  normality tests.

- show_categorical_tests:

  Display results of advanced categorical data tests.

- show_interpretations:

  Provide clinical interpretations and guidelines for statistical
  results to aid in medical decision making.

## Value

A results object containing:

|                                |     |     |     |     |        |
|--------------------------------|-----|-----|-----|-----|--------|
| `results$instructions`         |     |     |     |     | a html |
| `results$effect_size_results`  |     |     |     |     | a html |
| `results$goodness_fit_results` |     |     |     |     | a html |
| `results$categorical_results`  |     |     |     |     | a html |
