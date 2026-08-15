# Single Variable Quality Check

Single Variable Quality Check

## Usage

``` r
checkdata(
  data,
  var = NULL,
  showOutliers = TRUE,
  showDistribution = FALSE,
  showDuplicates = FALSE,
  showPatterns = FALSE,
  rareCategoryThreshold = 5,
  clinicalValidation = TRUE,
  unitSystem = "auto",
  outlierTransform = "none",
  mcarTest = FALSE,
  cvMinMean = 0.01,
  showSummary = FALSE,
  showAbout = FALSE,
  showCaveats = FALSE
)
```

## Arguments

- data:

  .

- var:

  .

- showOutliers:

  Detect potential outliers using three methods - z-score (\|z\| \> 3),
  the IQR fence (1.5 x IQR), and the modified Z-score (\|M\| \> 3.5,
  MAD-based) - and report points flagged by at least two of them. Below
  n = 10 single-method flags are shown and labelled informative-only.

- showDistribution:

  Display descriptive statistics and distribution characteristics.

- showDuplicates:

  Identify and count duplicate values in the dataset.

- showPatterns:

  Analyze patterns in missing data and value distributions.

- rareCategoryThreshold:

  Percentage threshold for flagging rare categories (important for
  chi-squared assumptions and modeling).

- clinicalValidation:

  Perform context-specific validation for clinical variables (age, lab
  values, etc.). Heuristic ranges may need adjustment.

- unitSystem:

  Unit system for clinical plausibility checks. Auto-detect attempts to
  infer from data ranges.

- outlierTransform:

  Apply transformation before outlier detection to handle skewed
  distributions (especially right-skewed lab values).

- mcarTest:

  Explain whether the missingness mechanism can be tested formally for
  this variable. Little's MCAR test is multivariate and cannot be
  computed from a single variable, so this analysis reports heuristics
  about where the missing values sit rather than a test of the
  mechanism; enable this to state that limitation explicitly in the
  output.

- cvMinMean:

  Suppress coefficient of variation when absolute mean is below this
  threshold (avoids instability).

- showSummary:

  Display a plain-language summary of data quality suitable for copying
  to reports.

- showAbout:

  Display information about data quality assessment methodology.

- showCaveats:

  Display important limitations and assumptions of the quality
  assessment.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a html |
| `results$todo` |  |  |  |  | a html |
| `results$qualityText` |  |  |  |  | a preformatted |
| `results$missingVals` |  |  |  |  | a table |
| `results$noOutliers` |  |  |  |  | a html |
| `results$outliers` |  |  |  |  | Shows outliers detected by at least 2 of 3 methods: Z-score (\|z\|\>3), IQR (1.5×IQR rule), Modified Z-score (MAD-based \|z\|\>3.5). Points flagged by only 1 method are NOT shown. |
| `results$outlierMethodSummary` |  |  |  |  | Summary of each outlier detection method. These are heuristic approaches; consider skewness and sample size when interpreting. |
| `results$distribution` |  |  |  |  | a table |
| `results$duplicates` |  |  |  |  | a table |
| `results$patterns` |  |  |  |  | a table |
| `results$naturalSummary` |  |  |  |  | a html |
| `results$aboutAnalysis` |  |  |  |  | a html |
| `results$caveatsAssumptions` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$missingVals$asDF`

`as.data.frame(results$missingVals)`
