# LASSO Logistic Regression

Performs LASSO-penalized logistic regression for variable selection in
binary classification problems. Ideal for diagnostic pathology studies
that build classifiers (e.g., tumor type A vs B) with automatic feature
selection.

## Usage

``` r
lassologistic(
  data,
  outcome,
  outcomeLevel,
  explanatory,
  penalty = "lasso",
  alpha = 0.5,
  lambda = "lambda.1se",
  nfolds = 10,
  random_seed = 123456,
  standardize = TRUE,
  suitabilityCheck = TRUE,
  bootstrapValidation = FALSE,
  bootstrapN = 200,
  cv_plot = TRUE,
  coef_plot = TRUE,
  roc_plot = TRUE,
  scoringSystem = FALSE,
  scoringMethod = "schneeweiss",
  scoringMaxPoints = 10,
  scoreCutMethod = "median",
  scoreCutPoints = "",
  scoreLookupTable = TRUE,
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

- outcome:

  Binary outcome variable to classify. Can be factor or numeric with
  exactly two observed values.

- outcomeLevel:

  Level of outcome considered as the positive class (event). For factor
  outcomes, if left empty the second level is used.

- explanatory:

  Variables to be considered for selection. At least 2 required.
  Constant variables are removed automatically.

- penalty:

  Type of regularization penalty. LASSO (L1) produces sparse models by
  setting coefficients to zero. Ridge (L2) shrinks but retains all.
  Elastic Net combines both.

- alpha:

  Mixing parameter for elastic net. Only used when penalty is
  elasticnet.

- lambda:

  Method for selecting the optimal lambda from cross-validation. 1SE
  rule tends to select fewer variables (more conservative).

- nfolds:

  Number of folds for cross-validation. Automatically reduced when
  sample size is limited.

- random_seed:

  Random seed for reproducible cross-validation fold assignment.

- standardize:

  Whether to standardize predictor variables before fitting.

- suitabilityCheck:

  Check sample size, events-per-variable ratio, multicollinearity, and
  whether regularization is needed.

- bootstrapValidation:

  Perform bootstrap optimism-corrected validation (Harrell method).
  Reports corrected AUC, calibration slope, and Brier score.

- bootstrapN:

  Number of bootstrap iterations for internal validation.

- cv_plot:

  Show cross-validation deviance vs lambda plot.

- coef_plot:

  Show bar plot of selected variable coefficients.

- roc_plot:

  Show ROC curve with AUC and confidence interval.

- scoringSystem:

  Convert model coefficients to an integer-point scoring system.
  Multiple methods available following published standards.

- scoringMethod:

  Method for converting regression coefficients to integer points.
  Beta10: multiply by 10, round (Zhang et al. 2017). Schneeweiss: divide
  by smallest absolute coefficient (Mehta et al. 2016). Sullivan:
  reference-variable scaling (Sullivan et al. 2004, Framingham method).
  Compare: generate all three and show performance comparison.

- scoringMaxPoints:

  Points assigned to the strongest predictor in the reference-scaled
  Sullivan method. The Beta10 method uses a fixed x10 coefficient
  scaling and ignores this value; the Schneeweiss method scales by the
  smallest coefficient and also ignores it.

- scoreCutMethod:

  How continuous predictors are dichotomized when awarding points. A
  predictor earns its points when the patient's value exceeds this cut.
  'median' (the default) splits at the sample median, which is
  data-derived and will differ in another cohort. 'manual' lets you
  enter established clinical thresholds, which is what makes a score
  portable.

- scoreCutPoints:

  Cut points on the ORIGINAL measurement scale, as 'variable=value'
  pairs separated by commas or semicolons, e.g. 'ki67=20, age=65'. Used
  when Cut Point method is 'manual'. Any continuous predictor not listed
  falls back to the sample median, and that fallback is reported. Binary
  predictors ignore this - they score when present.

- scoreLookupTable:

  Generate a lookup table mapping each possible total score to predicted
  probability of the positive class.

- showSummary:

  Display a natural-language summary of the main results.

- showExplanations:

  Display explanations of LASSO logistic regression methodology.

- showMethodologyNotes:

  Show technical notes about regularization and validation.

- includeClinicalGuidance:

  Include guidance for clinical interpretation of results.

- showVariableImportance:

  Display variable importance rankings across lambda values.

- showModelComparison:

  Compare LASSO vs standard logistic using selected variables.

## Value

A results object containing:

|                              |     |     |     |     |           |
|------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`               |     |     |     |     | a html    |
| `results$notices`            |     |     |     |     | a html    |
| `results$suitabilityReport`  |     |     |     |     | a html    |
| `results$modelSummary`       |     |     |     |     | a table   |
| `results$coefficients`       |     |     |     |     | a table   |
| `results$performance`        |     |     |     |     | a table   |
| `results$scoringTable`       |     |     |     |     | a table   |
| `results$scoringPerformance` |     |     |     |     | a table   |
| `results$methodComparison`   |     |     |     |     | a table   |
| `results$lookupTable`        |     |     |     |     | a table   |
| `results$validationTable`    |     |     |     |     | a table   |
| `results$cv_plot`            |     |     |     |     | an image  |
| `results$coef_plot`          |     |     |     |     | an image  |
| `results$roc_plot`           |     |     |     |     | an image  |
| `results$predictions`        |     |     |     |     | an output |
| `results$summaryText`        |     |     |     |     | a html    |
| `results$lassoExplanation`   |     |     |     |     | a html    |
| `results$methodologyNotes`   |     |     |     |     | a html    |
| `results$clinicalGuidance`   |     |     |     |     | a html    |
| `results$variableImportance` |     |     |     |     | a table   |
| `results$modelComparison`    |     |     |     |     | a table   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
