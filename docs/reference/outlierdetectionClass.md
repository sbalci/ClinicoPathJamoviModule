# Outlier Detection with easystats

Advanced outlier detection using multiple statistical methods from the
easystats performance package. This function provides comprehensive
outlier detection through univariate methods (Z-scores, IQR, confidence
intervals), multivariate methods (Mahalanobis distance, MCD, OPTICS,
LOF), and composite scoring across multiple algorithms. Complements
existing data quality assessment modules with state-of-the-art outlier
detection capabilities. Perfect for clinical research data quality
control and preprocessing.

## Value

A jamovi analysis object containing outlier detection results with
tables, plots, and interpretation based on selected options

## Details

The outlier detection module supports four main categories of methods:

**Univariate Methods:**

- **Robust Z-Score (MAD-based):** Uses median absolute deviation for
  robust standardization

- **Standard Z-Score:** Classical z-score based on mean and standard
  deviation

- **Interquartile Range (IQR):** Tukey's method using quartiles and IQR
  multiplier

- **Equal-Tailed Interval (ETI):** Symmetric confidence interval
  approach

- **Highest Density Interval (HDI):** Bayesian credible interval method

**Multivariate Methods:**

- **Mahalanobis Distance:** Classical multivariate distance accounting
  for covariance

- **Robust Mahalanobis Distance:** Robust version using minimum
  covariance determinant

- **Minimum Covariance Determinant (MCD):** Robust covariance estimation

- **OPTICS Clustering:** Density-based clustering approach

- **Local Outlier Factor (LOF):** Local density deviation method

**Composite Methods:** Combine multiple algorithms for robust detection
with adjustable thresholds

**All Methods:** Comprehensive analysis using all available techniques

## Method Selection Guidelines

- **Univariate:** When analyzing variables independently, simple
  interpretation needed

- **Multivariate:** When variable relationships matter, detecting
  complex outlier patterns

- **Composite:** When robust detection across different data patterns is
  needed

- **All:** For comprehensive analysis and method comparison

## Threshold Recommendations

- **Z-Score:** 3.29 (99.9 percent confidence, ~0.1 percent outliers)

- **IQR Multiplier:** 1.7 (more conservative than Tukey's 1.5)

- **Confidence Level:** 0.999 (99.9 percent for interval methods)

- **Composite Threshold:** 0.5 (outliers detected by 50 percent or more
  of methods)

## Clinical Applications

- **Laboratory Data:** CBC, chemistry panels, liver function tests

- **Anthropometric Data:** Height, weight, BMI measurements

- **Physiological Data:** Blood pressure, heart rate, temperature

- **Biomarker Data:** Protein levels, genetic markers, metabolites

- **Quality Control:** Data entry errors, instrument malfunctions

## Output Components

- **Outlier Table:** Detailed results with outlier scores and
  classifications

- **Method Comparison:** Performance across different detection
  algorithms

- **Exclusion Summary:** Recommendations for data cleaning procedures

- **Visualization:** Plots showing outlier patterns and distributions

- **Interpretation:** Detailed guidance on results and methodology

## Statistical Considerations

- **Sample Size:** Minimum 30 observations recommended for robust
  results

- **Distribution:** Robust methods handle non-normal distributions
  better

- **Missing Data:** Complete cases analysis performed automatically

- **Correlations:** Multivariate methods account for variable
  relationships

- **False Positives:** Conservative thresholds reduce over-detection

## References

- Ludecke, D., Ben-Shachar, M., Patil, I., Waggoner, P., & Makowski, D.
  (2021). performance: An R Package for Assessment, Comparison and
  Testing of Statistical Models. Journal of Open Source Software,
  6(60), 3139. https://doi.org/10.21105/joss.03139

- Rousseeuw, P. J., & Hubert, M. (2018). Anomaly detection by robust
  statistics. Wiley Interdisciplinary Reviews: Data Mining and Knowledge
  Discovery, 8(2), e1236.

- Breunig, M. M., Kriegel, H. P., Ng, R. T., & Sander, J. (2000). LOF:
  identifying density-based local outliers. ACM sigmod record, 29(2),
  93-104.

## See also

[`check_outliers`](https://easystats.github.io/performance/reference/check_outliers.html)
for the underlying outlier detection functions

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`outlierdetectionBase` -\> `outlierdetectionClass`

## Methods

### Public methods

- [`outlierdetectionClass$clone()`](#method-outlierdetectionClass-clone)

Inherited methods

- [`jmvcore::Analysis$.createImage()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImage)
- [`jmvcore::Analysis$.createImages()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImages)
- [`jmvcore::Analysis$.createPlotObject()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createPlotObject)
- [`jmvcore::Analysis$.getSessionTemp()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.getSessionTemp)
- [`jmvcore::Analysis$.load()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.load)
- [`jmvcore::Analysis$.render()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.render)
- [`jmvcore::Analysis$.save()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.save)
- [`jmvcore::Analysis$.savePart()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.savePart)
- [`jmvcore::Analysis$.setCheckpoint()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setCheckpoint)
- [`jmvcore::Analysis$.setParent()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setParent)
- [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetHeaderSource)
- [`jmvcore::Analysis$.setReadDatasetSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetSource)
- [`jmvcore::Analysis$.setResourcesPathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setResourcesPathSource)
- [`jmvcore::Analysis$.setStatePathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setStatePathSource)
- [`jmvcore::Analysis$addAddon()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-addAddon)
- [`jmvcore::Analysis$asProtoBuf()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asProtoBuf)
- [`jmvcore::Analysis$asSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asSource)
- [`jmvcore::Analysis$check()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-check)
- [`jmvcore::Analysis$init()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-init)
- [`jmvcore::Analysis$optionsChangedHandler()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-optionsChangedHandler)
- [`jmvcore::Analysis$postInit()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-postInit)
- [`jmvcore::Analysis$print()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-print)
- [`jmvcore::Analysis$readDataset()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-readDataset)
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `outlierdetectionBase$initialize()`

------------------------------------------------------------------------

### `outlierdetectionClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    outlierdetectionClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic univariate outlier detection
# Load clinical data
data(clinical_data)

# Detect outliers using robust z-score
outlierdetection(
  data = clinical_data,
  vars = c("hemoglobin", "glucose", "creatinine"),
  method_category = "univariate",
  univariate_methods = "zscore_robust",
  zscore_threshold = 3.29,
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# Example 2: Multivariate outlier detection
# Detect multivariate outliers in biomarker data
outlierdetection(
  data = biomarker_data,
  vars = c("protein_1", "protein_2", "protein_3"),
  method_category = "multivariate",
  multivariate_methods = "mahalanobis",
  show_method_comparison = TRUE,
  show_exclusion_summary = TRUE
)

# Example 3: Composite outlier detection
# Robust detection using multiple methods
outlierdetection(
  data = patient_data,
  vars = c("age", "weight", "height", "bmi"),
  method_category = "composite",
  composite_threshold = 0.6,
  show_outlier_table = TRUE,
  show_interpretation = TRUE
)

# Example 4: Comprehensive analysis
# Compare all available methods
outlierdetection(
  data = lab_data,
  vars = c("alt", "ast", "bilirubin", "albumin"),
  method_category = "all",
  show_method_comparison = TRUE,
  show_exclusion_summary = TRUE,
  show_visualization = TRUE
)
} # }
```
