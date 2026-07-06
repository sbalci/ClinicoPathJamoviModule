# Lasso-Cox Regression

Performs Lasso-penalized Cox regression for variable selection in
survival analysis.

## Usage

``` r
lassocox(
  data,
  elapsedtime,
  outcome,
  outcomeLevel,
  censorLevel,
  explanatory,
  lambda = "lambda.1se",
  nfolds = 10,
  random_seed = 123456,
  standardize = TRUE,
  suitabilityCheck = TRUE,
  cv_plot = TRUE,
  coef_plot = TRUE,
  survival_plot = TRUE,
  showSummary = FALSE,
  showExplanations = FALSE,
  showMethodologyNotes = FALSE,
  includeClinicalGuidance = FALSE,
  showVariableImportance = FALSE,
  showModelComparison = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  The numeric variable representing follow-up time until the event or
  last observation.

- outcome:

  Binary event indicator variable (event vs censored). Can be factor or
  numeric with exactly two observed values.

- outcomeLevel:

  Level of `outcome` considered as the event. For binary factor
  outcomes, if left empty the second observed level is used; for numeric
  binary outcomes, the larger observed value is used (or 1 for 0/1
  coding).

- censorLevel:

  Level of `outcome` considered as censored (no event). Together with
  `outcomeLevel`, this defines a strict two-level encoding: rows whose
  outcome matches neither level are treated as missing and excluded.

- explanatory:

  Variables to be considered for selection in the Lasso-Cox regression.
  Constant variables are removed automatically before fitting.

- lambda:

  Method for selecting the optimal lambda parameter from
  cross-validation.

- nfolds:

  Number of folds for cross-validation. Fold count is reduced
  automatically when sample size is limited.

- random_seed:

  Random seed for reproducible cross-validation fold assignment.

- standardize:

  Whether to standardize predictor variables before fitting. If enabled,
  reported coefficients are on the standardized predictor scale.

- suitabilityCheck:

  Run a comprehensive data suitability assessment before LASSO analysis.
  Checks sample size, events-per-variable ratio, multicollinearity, and
  whether regularization is needed.

- cv_plot:

  Whether to show the cross-validation plot.

- coef_plot:

  Whether to show a bar plot of selected-variable coefficients at the
  chosen lambda value.

- survival_plot:

  Whether to show survival curves by risk groups. Uses `survminer` if
  available, with a base-R fallback otherwise.

- showSummary:

  Display a natural-language summary paragraph of the main results,
  suitable for copying into reports or manuscripts.

- showExplanations:

  Display detailed explanations of LASSO Cox regression methodology,
  including regularization concepts and interpretation guidance.

- showMethodologyNotes:

  Show comprehensive technical notes about LASSO regularization,
  cross-validation, and variable selection process.

- includeClinicalGuidance:

  Include guidance for clinical interpretation of LASSO Cox regression
  results, risk scores, and variable selection outcomes.

- showVariableImportance:

  Display analysis of variable importance rankings and selection
  patterns across different lambda values.

- showModelComparison:

  Compare post-LASSO Cox refit (selected variables) with standard Cox
  using all encoded predictors. Intended as an exploratory comparison.

## Value

A results object containing:

|                                         |     |     |     |     |           |
|-----------------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                          |     |     |     |     | a html    |
| `results$suitabilityReport`             |     |     |     |     | a html    |
| `results$modelSummary`                  |     |     |     |     | a table   |
| `results$coefficients`                  |     |     |     |     | a table   |
| `results$performance`                   |     |     |     |     | a table   |
| `results$cv_plot`                       |     |     |     |     | an image  |
| `results$coef_plot`                     |     |     |     |     | an image  |
| `results$survival_plot`                 |     |     |     |     | an image  |
| `results$riskScore`                     |     |     |     |     | an output |
| `results$summaryText`                   |     |     |     |     | a html    |
| `results$lassoExplanation`              |     |     |     |     | a html    |
| `results$methodologyNotes`              |     |     |     |     | a html    |
| `results$clinicalGuidance`              |     |     |     |     | a html    |
| `results$variableImportance`            |     |     |     |     | a table   |
| `results$modelComparison`               |     |     |     |     | a table   |
| `results$regularizationPathExplanation` |     |     |     |     | a html    |
| `results$crossValidationExplanation`    |     |     |     |     | a html    |
| `results$riskScoreExplanation`          |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
