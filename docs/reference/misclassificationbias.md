# Misclassification Bias Sensitivity Analysis

Assesses how non-differential or differential misclassification of an
exposure or outcome affects observed effect estimates (OR, RR, RD).
Essential for studies relying on subjective classification methods
(e.g., Ki-67 visual estimation, morphologic grading, IHC scoring).

## Usage

``` r
misclassificationbias(
  data,
  outcome,
  outcomeLevel,
  exposure,
  exposureLevel,
  misclassType = "nondifferential",
  senExposure = 0.85,
  specExposure = 0.9,
  senExposureCase = 0.85,
  specExposureCase = 0.9,
  senExposureControl = 0.85,
  specExposureControl = 0.9,
  effectMeasure = "or",
  rangeAnalysis = TRUE,
  senRange = "0.70,0.75,0.80,0.85,0.90,0.95",
  specRange = "0.70,0.75,0.80,0.85,0.90,0.95",
  nSimulations = 10000,
  random_seed = 42
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome variable (e.g., recurrence, death, disease status).

- outcomeLevel:

  Level of the outcome considered as the event.

- exposure:

  Binary exposure or classification variable subject to
  misclassification (e.g., Ki-67 grade, morphologic classification, IHC
  scoring).

- exposureLevel:

  Level of the exposure considered as positive/exposed.

- misclassType:

  Non-differential: sensitivity/specificity of the classifier are the
  same regardless of the true outcome. Differential: error rates may
  differ between outcome groups.

- senExposure:

  Sensitivity of the classification method (probability of correctly
  classifying a truly positive case). For Ki-67 visual estimation,
  typically 0.70-0.90.

- specExposure:

  Specificity of the classification method (probability of correctly
  classifying a truly negative case).

- senExposureCase:

  Sensitivity of the classifier among cases (outcome positive). Used for
  differential misclassification.

- specExposureCase:

  Specificity of the classifier among cases. Used for differential
  misclassification.

- senExposureControl:

  Sensitivity of the classifier among controls (outcome negative). Used
  for differential misclassification.

- specExposureControl:

  Specificity of the classifier among controls. Used for differential
  misclassification.

- effectMeasure:

  Effect measure to compute and bias-adjust.

- rangeAnalysis:

  Vary sensitivity and specificity across a plausible range to show how
  the bias-adjusted estimate changes. Produces a contour plot.

- senRange:

  Comma-separated sensitivity values for range analysis.

- specRange:

  Comma-separated specificity values for range analysis.

- nSimulations:

  Number of Monte Carlo simulations for probabilistic bias analysis.
  Higher values give more stable estimates but take longer.

- random_seed:

  Random seed for reproducible simulation results.

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$notices`        |     |     |     |     | a preformatted |
| `results$todo`           |     |     |     |     | a html         |
| `results$observedTable`  |     |     |     |     | a table        |
| `results$biasAnalysis`   |     |     |     |     | a table        |
| `results$correctedTable` |     |     |     |     | a table        |
| `results$rangeTable`     |     |     |     |     | a table        |
| `results$rangeplot`      |     |     |     |     | an image       |
| `results$interpretation` |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$observedTable$asDF`

`as.data.frame(results$observedTable)`
