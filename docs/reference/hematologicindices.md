# Hematologic Prognostic Indices

Derives the commonly reported blood-count and inflammation-based
prognostic indices from routine complete-blood-count and biochemistry
variables: the neutrophil-to-lymphocyte ratio (NLR),
platelet-to-lymphocyte ratio (PLR), lymphocyte-to-monocyte ratio (LMR),
systemic immune-inflammation index (SII), Onodera's Prognostic
Nutritional Index (PNI), the C-reactive-protein-to-albumin ratio (CAR),
and the (modified) Glasgow Prognostic Score (GPS / mGPS). Each index is
computed per patient, summarized across the cohort, optionally
dichotomized at a literature or data-driven cutpoint, and optionally
related to a survival outcome.

## Usage

``` r
hematologicindices(
  data,
  neutrophils,
  lymphocytes,
  platelets,
  monocytes = NULL,
  albumin = NULL,
  albuminUnit = "gdl",
  crp = NULL,
  indices = list("nlr", "plr", "sii", "pni"),
  gpsType = "modified",
  survivalTime = NULL,
  survivalStatus = NULL,
  eventLevel,
  survivalIndex = "nlr",
  splitMethod = "median",
  showIndicesTable = TRUE,
  showSurvival = FALSE,
  showPlot = TRUE,
  showSummary = FALSE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame (one row per patient).

- neutrophils:

  Absolute neutrophil count (10^9/L).

- lymphocytes:

  Absolute lymphocyte count (10^9/L).

- platelets:

  Platelet count (10^9/L).

- monocytes:

  Absolute monocyte count (10^9/L), for LMR.

- albumin:

  Serum albumin in g/dL (for PNI). If your data are in g/L, set the
  albumin unit.

- albuminUnit:

  Unit of the albumin variable.

- crp:

  C-reactive protein in mg/L (for CAR and GPS).

- indices:

  Which indices to compute (subject to the required inputs being
  supplied).

- gpsType:

  Original GPS scores low albumin even with normal CRP; mGPS requires
  elevated CRP.

- survivalTime:

  Follow-up time, to relate an index to outcome.

- survivalStatus:

  Event indicator for the survival linkage.

- eventLevel:

  Level of the status variable representing the event.

- survivalIndex:

  Which continuous index to dichotomize for the survival comparison.

- splitMethod:

  How to dichotomize the chosen index for the survival split.

- showIndicesTable:

  Summary statistics for each computed index.

- showSurvival:

  Compare survival above vs below the cutpoint of the chosen index.

- showPlot:

  Plot the index distributions (or survival split).

- showSummary:

  Plain-language summary.

- showExplanation:

  Formulas and cut points.

## Value

A results object containing:

|                          |     |     |     |     |           |
|--------------------------|-----|-----|-----|-----|-----------|
| `results$todo`           |     |     |     |     | a html    |
| `results$indicesTable`   |     |     |     |     | a table   |
| `results$gpsTable`       |     |     |     |     | a table   |
| `results$survivalTable`  |     |     |     |     | a table   |
| `results$plot`           |     |     |     |     | an image  |
| `results$summary`        |     |     |     |     | a html    |
| `results$explanation`    |     |     |     |     | a html    |
| `results$addIndexToData` |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$indicesTable$asDF`

`as.data.frame(results$indicesTable)`

## Examples

``` r
# \donttest{
hematologicindices(
    data = mydata,
    neutrophils = "neut", lymphocytes = "lymph",
    platelets = "plt", albumin = "alb", crp = "crp")
#> Error in hematologicindices(data = mydata, neutrophils = "neut", lymphocytes = "lymph",     platelets = "plt", albumin = "alb", crp = "crp"): argument "eventLevel" is missing, with no default
# }
```
