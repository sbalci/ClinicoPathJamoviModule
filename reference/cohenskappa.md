# Cohen's Kappa Agreement

Cohen's kappa and weighted kappa for two-rater agreement analysis.
Essential for assessing agreement between two pathologists, diagnostic
reproducibility, and quality assurance in clinical pathology practice.

## Usage

``` r
cohenskappa(
  data,
  rater1,
  rater2,
  kappa_type = "cohen",
  confidence_level = 0.95,
  ci_method = "asymptotic",
  bootstrap_samples = 1000,
  exact_agreement = TRUE,
  marginal_homogeneity = TRUE,
  agreement_plot = TRUE,
  confusion_matrix = TRUE,
  category_analysis = FALSE,
  interpretation_guide = TRUE,
  missing_treatment = "listwise",
  minimum_categories = 2,
  rater3 = NULL,
  rater4 = NULL,
  rater5 = NULL,
  multi_rater_method = "auto",
  show_pairwise_kappa = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- rater1:

  .

- rater2:

  .

- kappa_type:

  Type of kappa statistic to calculate

- confidence_level:

  Confidence level for confidence intervals

- ci_method:

  Method for calculating confidence intervals

- bootstrap_samples:

  Number of bootstrap samples for CI estimation

- exact_agreement:

  Calculate overall and category-specific agreement percentages

- marginal_homogeneity:

  Test whether marginal distributions are equal (Stuart-Maxwell test)

- agreement_plot:

  Create agreement plot showing observed vs expected agreement

- confusion_matrix:

  Display confusion matrix with agreement patterns

- category_analysis:

  Detailed analysis of agreement for each category

- interpretation_guide:

  Provide clinical interpretation of kappa values

- missing_treatment:

  How to handle missing data

- minimum_categories:

  Minimum number of categories required for analysis

- rater3:

  Third rater for multi-rater agreement (Fleiss kappa)

- rater4:

  Fourth rater for multi-rater agreement

- rater5:

  Fifth rater for multi-rater agreement

- multi_rater_method:

  Method for calculating multi-rater agreement (\>=3 raters)

- show_pairwise_kappa:

  Show all pairwise kappa values between raters (multi-rater only)

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$summary`             |     |     |     |     | a html   |
| `results$kappaTable`          |     |     |     |     | a table  |
| `results$agreementStats`      |     |     |     |     | a table  |
| `results$confusionMatrix`     |     |     |     |     | a table  |
| `results$categoryStats`       |     |     |     |     | a table  |
| `results$marginalTest`        |     |     |     |     | a table  |
| `results$bootstrapResults`    |     |     |     |     | a table  |
| `results$agreementPlot`       |     |     |     |     | an image |
| `results$confusionHeatmap`    |     |     |     |     | an image |
| `results$interpretationGuide` |     |     |     |     | a html   |
| `results$technicalNotes`      |     |     |     |     | a html   |
| `results$multiRaterKappa`     |     |     |     |     | a table  |
| `results$pairwiseKappaMatrix` |     |     |     |     | a table  |
| `results$multiRaterSummary`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$kappaTable$asDF`

`as.data.frame(results$kappaTable)`
