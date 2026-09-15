# Residual Cancer Burden (RCB)

Computes the Residual Cancer Burden (RCB) index and class (Symmans et
al., 2007) from the standard post-neoadjuvant pathology variables: the
two dimensions of the primary tumour bed, overall cancer cellularity,
the in-situ fraction, the number of positive lymph nodes, and the
diameter of the largest nodal metastasis. The continuous index is
classified into RCB-0 (pathologic complete response), RCB-I (minimal),
RCB-II (moderate), and RCB-III (extensive) residual disease using the
established cut points (1.36 and 3.28). Works on a cohort (one row per
patient) or as a single-case calculator, and can optionally relate RCB
class to a survival outcome.

## Usage

``` r
residualcancerburden(
  data,
  inputMode = "data",
  d1 = NULL,
  d2 = NULL,
  cellularity = NULL,
  cis = NULL,
  positiveNodes = NULL,
  metSize = NULL,
  mD1 = 20,
  mD2 = 15,
  mCellularity = 10,
  mCis = 0,
  mNodes = 0,
  mMetSize = 0,
  survivalTime = NULL,
  survivalStatus = NULL,
  eventLevel,
  showDistribution = TRUE,
  showPlot = TRUE,
  survivalLink = FALSE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per patient in cohort mode).

- inputMode:

  Compute RCB for every row using data variables, or for a single
  entered case.

- d1:

  First bidimensional diameter of the primary tumour bed (mm).

- d2:

  Second bidimensional diameter of the primary tumour bed (mm).

- cellularity:

  Overall percentage of the tumour bed area that is carcinoma (0-100).

- cis:

  Percentage of the carcinoma that is in-situ disease (0-100). Defaults
  to 0.

- positiveNodes:

  Number of regional lymph nodes containing metastatic carcinoma.

- metSize:

  Diameter of the largest nodal metastasis (mm). Defaults to 0.

- mD1:

  Single-case first tumour-bed dimension (mm).

- mD2:

  Single-case second tumour-bed dimension (mm).

- mCellularity:

  Single-case overall cellularity (percent).

- mCis:

  Single-case in-situ fraction (percent).

- mNodes:

  Single-case number of positive nodes.

- mMetSize:

  Single-case largest nodal metastasis (mm).

- survivalTime:

  Follow-up time, to relate RCB class to outcome (cohort mode).

- survivalStatus:

  Event indicator for the survival linkage.

- eventLevel:

  Level of the status variable representing the event.

- showDistribution:

  Tabulate the RCB class distribution across the cohort.

- showPlot:

  Plot the RCB class distribution (cohort) or the case position (single
  case).

- survivalLink:

  If survival time and status are supplied, show a log-rank test across
  classes.

- showSummary:

  Plain-language summary of the result.

- showExplanation:

  Explanation of the RCB formula and classes.

## Value

A results object containing:

|                             |     |     |     |     |           |
|-----------------------------|-----|-----|-----|-----|-----------|
| `results$todo`              |     |     |     |     | a html    |
| `results$caseTable`         |     |     |     |     | a table   |
| `results$distributionTable` |     |     |     |     | a table   |
| `results$survivalTable`     |     |     |     |     | a table   |
| `results$plot`              |     |     |     |     | an image  |
| `results$summary`           |     |     |     |     | a html    |
| `results$explanation`       |     |     |     |     | a html    |
| `results$addIndexToData`    |     |     |     |     | an output |
| `results$addClassToData`    |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$caseTable$asDF`

`as.data.frame(results$caseTable)`

## Examples

``` r
# \donttest{
residualcancerburden(
    data = mydata,
    d1 = "tumor_dim1", d2 = "tumor_dim2",
    cellularity = "pct_cellularity", cis = "pct_insitu",
    positiveNodes = "n_pos_nodes", metSize = "largest_met_mm")
#> Error in residualcancerburden(data = mydata, d1 = "tumor_dim1", d2 = "tumor_dim2",     cellularity = "pct_cellularity", cis = "pct_insitu", positiveNodes = "n_pos_nodes",     metSize = "largest_met_mm"): argument "eventLevel" is missing, with no default
# }
```
