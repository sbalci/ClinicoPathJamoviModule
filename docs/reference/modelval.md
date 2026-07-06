# Model Validation Dashboard

Comprehensive validation dashboard for clinical prediction models.
Assess calibration, discrimination, and clinical utility using predicted
probabilities from existing models. Supports external validation,
temporal validation, and subgroup analysis.

## Usage

``` r
modelval(
  data,
  outcome,
  outcomePositive,
  predicted,
  validationType = "general",
  subgroup,
  showCalibrationLarge = TRUE,
  showCalibrationSlope = TRUE,
  showFlexibleCalibration = TRUE,
  calibrationGroups = 10,
  showDiscrimination = TRUE,
  showNetBenefit = TRUE,
  thresholdRange = "0.01, 0.99",
  showSubgroupAnalysis = FALSE,
  ciLevel = 0.95
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome variable (observed events: yes/no).

- outcomePositive:

  The factor level of the outcome variable that represents the positive
  case (event = 1). Required to avoid relying on alphabetic level order,
  which can silently invert calibration and AUC interpretation.

- predicted:

  Predicted probabilities from the existing model (0-1 scale).

- validationType:

  Type of validation being performed.

- subgroup:

  Variable for subgroup analysis (e.g., hospital, time period, sex).

- showCalibrationLarge:

  Test if predicted probabilities are systematically too high or low.

- showCalibrationSlope:

  Assess if model predictions are too extreme (slope != 1).

- showFlexibleCalibration:

  Display smooth calibration curve using loess/splines.

- calibrationGroups:

  Number of groups for calibration plot (typically 10).

- showDiscrimination:

  Display AUC, Brier score, and scaled Brier score.

- showNetBenefit:

  Perform decision curve analysis for clinical utility.

- thresholdRange:

  Min and max threshold probabilities (comma-separated).

- showSubgroupAnalysis:

  Compare model performance across subgroups.

- ciLevel:

  Confidence level for intervals (0.80-0.99).

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$validationSummary`       |     |     |     |     | a html   |
| `results$calibrationMetrics`      |     |     |     |     | a table  |
| `results$discriminationMetrics`   |     |     |     |     | a table  |
| `results$netBenefitSummary`       |     |     |     |     | a table  |
| `results$subgroupPerformance`     |     |     |     |     | a table  |
| `results$calibrationPlot`         |     |     |     |     | an image |
| `results$rocPlot`                 |     |     |     |     | an image |
| `results$dcaPlot`                 |     |     |     |     | an image |
| `results$subgroupCalibrationPlot` |     |     |     |     | an image |
| `results$recommendations`         |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$calibrationMetrics$asDF`

`as.data.frame(results$calibrationMetrics)`
