# Variable Importance Biplot

Create biplots showing variable contributions to group separation using
PCA, PLS-DA, or LDA. Displays patients as points and variables as
vectors, with vector length indicating contribution strength. Inspired
by Orange Data Mining's contribution analysis, adapted for clinical
research with comprehensive variable importance metrics.

## Usage

``` r
variablebiplot(
  data,
  groupVar = NULL,
  features = NULL,
  method = "pca",
  showLoadings = FALSE,
  loadingScale = 1.5,
  topContributors = 10,
  pc1 = 1,
  pc2 = 2,
  showContribTable = FALSE,
  showVarianceExplained = FALSE,
  contributionMetric = "squared",
  showSeparation = FALSE,
  separationMetric = "silhouette",
  showConfidenceEllipse = FALSE,
  pointSize = 3,
  labelPoints = "none",
  centerScale = FALSE,
  minVariance = 70,
  biplotType = "covariance",
  showSummary = FALSE,
  showReport = FALSE,
  showInterpretation = FALSE,
  showRCode = FALSE
)
```

## Arguments

- data:

  .

- groupVar:

  .

- features:

  .

- method:

  .

- showLoadings:

  .

- loadingScale:

  .

- topContributors:

  .

- pc1:

  .

- pc2:

  .

- showContribTable:

  .

- showVarianceExplained:

  .

- contributionMetric:

  .

- showSeparation:

  .

- separationMetric:

  .

- showConfidenceEllipse:

  .

- pointSize:

  .

- labelPoints:

  .

- centerScale:

  .

- minVariance:

  .

- biplotType:

  .

- showSummary:

  Display a plain-language summary paragraph with key findings and
  clinical interpretation. Useful for understanding results and creating
  reports.

- showReport:

  Generate a copy-ready paragraph with all statistics filled in,
  suitable for pasting into manuscripts or clinical reports.

- showInterpretation:

  Display guidance on how to read and interpret the biplot, including
  what arrows, points, and distances represent.

- showRCode:

  Generate copy-ready R code using upstream packages (stats, mixOmics,
  MASS) instead of jamovi wrappers. Useful for reproducing analysis in R
  scripts, learning the underlying implementation, and sharing code with
  non-jamovi users.

## Value

A results object containing:

|                                      |     |     |     |     |          |
|--------------------------------------|-----|-----|-----|-----|----------|
| `results$about`                      |     |     |     |     | a html   |
| `results$todo`                       |     |     |     |     | a html   |
| `results$assumptions`                |     |     |     |     | a html   |
| `results$summary`                    |     |     |     |     | a html   |
| `results$interpretation`             |     |     |     |     | a html   |
| `results$report`                     |     |     |     |     | a html   |
| `results$componentVariance`          |     |     |     |     | a table  |
| `results$contributionTable`          |     |     |     |     | a table  |
| `results$separationAnalysis`         |     |     |     |     | a table  |
| `results$biplot`                     |     |     |     |     | an image |
| `results$contributionInterpretation` |     |     |     |     | a html   |
| `results$rCode`                      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$componentVariance$asDF`

`as.data.frame(results$componentVariance)`
