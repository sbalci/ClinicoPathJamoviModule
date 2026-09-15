# Continuous Variable Relationship Analysis

Analyzes relationships between continuous variables in clinical research
using evidence-based approaches from BMJ best practices. Avoids common
pitfalls like arbitrary categorization and linear assumptions.

## Usage

``` r
continuousrelationship(
  data,
  outcome,
  predictor,
  covariates = NULL,
  modelType = "spline",
  nKnots = "4",
  knotPositions = "quantile",
  plotType = "doseresponse",
  referenceValue,
  showCI = TRUE,
  showRug = TRUE,
  showGuidance = TRUE,
  compareWithLinear = TRUE,
  showCategorizedPitfall = FALSE,
  testLinearity = TRUE,
  showModelFit = TRUE,
  showModel = FALSE
)
```

## Arguments

- data:

  The dataset containing clinical variables

- outcome:

  The outcome/dependent variable (can be continuous or binary)

- predictor:

  The continuous predictor variable to analyze

- covariates:

  Variables to adjust for in the analysis

- modelType:

  Statistical approach for modeling the relationship. Splines are
  recommended for most clinical applications.

- nKnots:

  Number of knots for spline models. More knots = more flexibility

- knotPositions:

  How to position knots across the predictor range

- plotType:

  Type of visualization to create

- referenceValue:

  Reference value for the predictor (e.g., BMI=25). If not specified,
  uses median value.

- showCI:

  Display 95 percent confidence intervals on plots

- showRug:

  Show rug plot of actual data points

- showGuidance:

  Display educational guidance based on BMJ best practices to help
  interpret results and avoid common mistakes

- compareWithLinear:

  Show comparison with simple linear model to demonstrate the importance
  of flexible modeling

- showCategorizedPitfall:

  Educational: Show what happens when continuous variables are
  inappropriately categorized (with warning)

- testLinearity:

  Perform statistical test for non-linear relationship

- showModelFit:

  Display AIC, BIC, and other fit statistics

- showModel:

  Display the model formula as HTML in the results

## Value

A results object containing:

|                               |     |     |     |     |                |
|-------------------------------|-----|-----|-----|-----|----------------|
| `results$guidance`            |     |     |     |     | a html         |
| `results$warningMessage`      |     |     |     |     | a html         |
| `results$modelFormula`        |     |     |     |     | a html         |
| `results$linearityTest`       |     |     |     |     | a table        |
| `results$modelFit`            |     |     |     |     | a table        |
| `results$doseResponsePlot`    |     |     |     |     | an image       |
| `results$partialEffectPlot`   |     |     |     |     | an image       |
| `results$comparisonPlot`      |     |     |     |     | an image       |
| `results$categorizedPlot`     |     |     |     |     | an image       |
| `results$interpretationGuide` |     |     |     |     | a preformatted |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$linearityTest$asDF`

`as.data.frame(results$linearityTest)`
