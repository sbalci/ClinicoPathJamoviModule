# IHC Heterogeneity Analysis

IHC Heterogeneity Analysis

## Usage

``` r
ihcheterogeneity(
  data,
  wholesection = NULL,
  biopsy1,
  biopsy2 = NULL,
  biopsy3 = NULL,
  biopsy4 = NULL,
  biopsies = NULL,
  spatial_id = NULL,
  compareCompartments = FALSE,
  compartmentTests = FALSE,
  analysis_type = "comprehensive",
  sampling_strategy = "unknown",
  cv_threshold = 20,
  correlation_threshold = 0.8,
  show_variability_plots = FALSE,
  variance_components = FALSE,
  power_analysis = FALSE,
  generate_recommendations = FALSE,
  showSummary = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- wholesection:

  Optional reference measurement for comparison with regional
  measurements. Can be whole section average, hotspot area, or overall
  tumor measurement. Leave empty for inter-regional comparison studies.
  Example: Ki67 proliferation index (0-100 percent), ER H-score (0-300),
  PR percentage (0-100 percent).

- biopsy1:

  Continuous biomarker measurement from first tissue region or area.
  Should represent same biomarker as reference measurement for
  heterogeneity comparison. Example: Ki67 percent from tumor periphery,
  ER H-score from invasive front.

- biopsy2:

  Second tissue region biomarker measurement for heterogeneity analysis

- biopsy3:

  Third tissue region biomarker measurement

- biopsy4:

  Fourth tissue region biomarker measurement

- biopsies:

  additional simulated biopsy measurements

- spatial_id:

  identifier for spatial regions or tissue areas (e.g.,
  Central/Invasive, Preinvasive/Invasive)

- compareCompartments:

  Perform statistical comparison of heterogeneity patterns between
  spatial compartments. Requires spatial_id variable. Compares ICC, CV,
  and bias across compartments.

- compartmentTests:

  Perform statistical tests to determine if heterogeneity differs
  significantly between compartments. Uses Levene's test for variance
  equality and Kruskal-Wallis for distributional differences.

- analysis_type:

  primary focus of biopsy simulation analysis

- sampling_strategy:

  biopsy sampling strategy used

- cv_threshold:

  Coefficient of variation threshold for acceptable sampling
  variability. Typical clinical values: 15-25 percent for
  immunohistochemistry (Ki67, ER, PR), 10-20 percent for molecular
  assays, 20-30 percent for heterogeneous markers (HER2, PD-L1). Lower
  values indicate more stringent quality requirements.

- correlation_threshold:

  Minimum Spearman correlation between biopsy and whole section
  measurements. Clinical guidelines: \>=0.80 excellent agreement,
  \>=0.70 good agreement, \>=0.60 moderate agreement, \<0.60 poor
  agreement. Higher values indicate better representativeness of biopsy
  samples.

- show_variability_plots:

  display plots showing sampling variability

- variance_components:

  perform variance component decomposition

- power_analysis:

  perform power analysis for sample size recommendations

- generate_recommendations:

  provide recommendations for optimal heterogeneity assessment strategy

- showSummary:

  Display natural-language summary of heterogeneity analysis results

- showGlossary:

  Display definitions of statistical terms (ICC, CV, correlation)

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$welcome` |  |  |  |  | Welcome screen shown when no variables selected |
| `results$interpretation` |  |  |  |  | a html |
| `results$report_sentences` |  |  |  |  | Pre-formatted sentences ready for clinical reports and publications |
| `results$assumptions` |  |  |  |  | Analysis assumptions, data requirements, and methodological considerations |
| `results$summary` |  |  |  |  | Natural-language summary of heterogeneity analysis results |
| `results$glossary` |  |  |  |  | Definitions of key statistical terms used in the analysis |
| `results$reproducibilitytable` |  |  |  |  | Correlation and reliability metrics |
| `results$samplingbiastable` |  |  |  |  | Systematic bias assessment between methods |
| `results$variancetable` |  |  |  |  | Sources of measurement variability |
| `results$poweranalysistable` |  |  |  |  | Sample size recommendations and power calculations |
| `results$spatialanalysistable` |  |  |  |  | Variability across spatial regions |
| `results$compartmentComparison` |  |  |  |  | Statistical comparison of heterogeneity metrics between compartments |
| `results$compartmentTests` |  |  |  |  | Formal statistical tests comparing heterogeneity across compartments |
| `results$biopsyplot` |  |  |  |  | Distribution comparison across regional measurements and reference (if provided) |
| `results$variabilityplot` |  |  |  |  | Coefficient of variation by case |
| `results$spatialplot` |  |  |  |  | Spatial distribution of biomarker values |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$reproducibilitytable$asDF`

`as.data.frame(results$reproducibilitytable)`
