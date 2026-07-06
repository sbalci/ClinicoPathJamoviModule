# IHC Scoring Standardization

IHC Scoring Standardization

## Usage

``` r
ihcscoring(
  data,
  guided_biomarker = "manual",
  intensity_var,
  proportion_var,
  sample_id_var = NULL,
  group_var = NULL,
  scoring_method = "both",
  binary_cutpoint = 100,
  allred_cutpoint = 3,
  intensity_scale = "standard",
  biomarker_type = "other",
  show_plots = TRUE,
  show_agreement_plots = TRUE,
  include_statistics = TRUE,
  include_digital_validation = FALSE,
  agreement_analysis = TRUE,
  quality_control = TRUE,
  clinical_interpretation = TRUE,
  export_results = FALSE,
  multiple_cutoffs = FALSE,
  cutoff_values = "1, 5, 10, 25, 50",
  cps_analysis = FALSE,
  immune_cells_var = NULL,
  tumor_cells_var = NULL,
  cutoff_comparison = TRUE,
  confidence_level = 0.95,
  bootstrap_n = 1000,
  automated_analysis = FALSE,
  segmentation_method = "manual",
  color_deconvolution = TRUE,
  dab_thresholds = "0.1, 0.3, 0.6",
  minimum_nuclear_area = 50,
  maximum_nuclear_area = 2000,
  batch_processing = FALSE,
  image_format = "tiff",
  validation_metrics = TRUE,
  molecular_classification = FALSE,
  classification_system = "bladder_mibc",
  primary_marker1 = NULL,
  primary_marker2 = NULL,
  secondary_marker = NULL,
  classification_cutoffs = "20, 20, 70",
  pd1_marker = NULL,
  pdl1_marker = NULL,
  checkpoint_cutoffs = "1, 10",
  subtype_statistics = TRUE,
  subtype_visualization = TRUE,
  language = "english",
  colorblind_safe = TRUE,
  high_contrast = FALSE,
  font_size = "normal"
)
```

## Arguments

- data:

  the data as a data frame

- guided_biomarker:

  guided configuration for common biomarkers with recommended settings

- intensity_var:

  staining intensity scores (typically 0-3 scale)

- proportion_var:

  percentage of positive cells (0-100 percent)

- sample_id_var:

  unique identifier for each sample

- group_var:

  grouping variable for comparative analysis

- scoring_method:

  primary scoring methodology to emphasize

- binary_cutpoint:

  H-score cutpoint for positive/negative classification

- allred_cutpoint:

  Allred score cutpoint for positive/negative classification

- intensity_scale:

  intensity scoring scale used

- biomarker_type:

  specific biomarker being analyzed

- show_plots:

  display scoring distribution and correlation plots

- show_agreement_plots:

  display method agreement analysis plots

- include_statistics:

  calculate comprehensive descriptive statistics

- include_digital_validation:

  include digital pathology validation metrics

- agreement_analysis:

  calculate inter-method agreement statistics

- quality_control:

  perform quality control checks and outlier detection

- clinical_interpretation:

  provide clinical context and interpretation

- export_results:

  format results for external analysis or reporting

- multiple_cutoffs:

  analyze biomarker positivity across multiple pre-specified cut-off
  values

- cutoff_values:

  comma-separated list of percentage cut-off values for positivity
  analysis

- cps_analysis:

  calculate Combined Positive Score for PD-L1 assessment

- immune_cells_var:

  percentage of PD-L1+ immune cells (required for CPS calculation)

- tumor_cells_var:

  percentage of PD-L1+ tumor cells (required for CPS calculation)

- cutoff_comparison:

  perform statistical comparisons across different cut-off values

- confidence_level:

  confidence level for statistical intervals

- bootstrap_n:

  number of bootstrap replicates for confidence intervals

- automated_analysis:

  enable automated quantification from histological images

- segmentation_method:

  method for nuclear segmentation in automated analysis

- color_deconvolution:

  separate DAB and hematoxylin channels for better quantification

- dab_thresholds:

  optical density thresholds for weak, moderate, and strong DAB staining
  (comma-separated)

- minimum_nuclear_area:

  minimum area threshold for nuclear detection

- maximum_nuclear_area:

  maximum area threshold for nuclear detection

- batch_processing:

  process multiple image files in batch mode

- image_format:

  format of input histological images

- validation_metrics:

  compare automated results with manual scoring when available

- molecular_classification:

  perform molecular subtype classification based on marker combinations

- classification_system:

  molecular classification system to apply

- primary_marker1:

  first primary marker for molecular classification

- primary_marker2:

  second primary marker for molecular classification

- secondary_marker:

  secondary marker for subtype refinement

- classification_cutoffs:

  comma-separated cut-off values for each marker in order (primary1,
  primary2, secondary)

- pd1_marker:

  PD-1 expression scores for immune checkpoint analysis

- pdl1_marker:

  PD-L1 expression scores for immune checkpoint analysis

- checkpoint_cutoffs:

  comma-separated cut-off values for PD-1/PD-L1 positivity analysis

- subtype_statistics:

  calculate statistical comparisons between molecular subtypes

- subtype_visualization:

  create plots showing molecular subtype distributions and associations

- language:

  language for interface elements and clinical interpretations

- colorblind_safe:

  use colorblind-safe palette for all visualizations

- high_contrast:

  enable high contrast mode for better accessibility

- font_size:

  base font size for improved readability

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$interpretation` |  |  |  |  | a html |
| `results$clinicalSummary` |  |  |  |  | a html |
| `results$aboutAnalysis` |  |  |  |  | a html |
| `results$clinicalReport` |  |  |  |  | a preformatted |
| `results$assumptions` |  |  |  |  | a html |
| `results$scorestable` |  |  |  |  | Calculated H-scores, Allred scores, and binary classifications |
| `results$statisticstable` |  |  |  |  | Comprehensive statistical summary for each scoring method |
| `results$agreementtable` |  |  |  |  | Correlation and agreement statistics between scoring methods |
| `results$qualitycontroltable` |  |  |  |  | Outlier detection and data quality assessment |
| `results$distributionplot` |  |  |  |  | Visual distribution of H-scores and Allred scores |
| `results$correlationplot` |  |  |  |  | Correlation between H-score and Allred score methods |
| `results$agreementplot` |  |  |  |  | Bland-Altman style agreement analysis between methods |
| `results$cutpointplot` |  |  |  |  | ROC analysis for optimal cutpoint determination |
| `results$biomarkerspecific$biomarkerresults` |  |  |  |  | Analysis tailored to specific biomarker characteristics |
| `results$biomarkerspecific$clinicalcutpoints` |  |  |  |  | Established clinical thresholds and their performance |
| `results$digitalvalidation$algorithmcomparison` |  |  |  |  | Statistical comparison of digital vs manual scoring |
| `results$digitalvalidation$batcheffects` |  |  |  |  | Analysis of systematic differences across batches |
| `results$digitalvalidation$validationplot` |  |  |  |  | an image |
| `results$automatedanalysis$segmentationresults` |  |  |  |  | Summary of automated nuclear segmentation |
| `results$automatedanalysis$intensityanalysis` |  |  |  |  | Distribution of DAB intensity levels |
| `results$automatedanalysis$validationcomparison` |  |  |  |  | Comparison between manual and automated scores |
| `results$automatedanalysis$segmentationplot` |  |  |  |  | Overlay of detected nuclei on original image |
| `results$automatedanalysis$intensityplot` |  |  |  |  | Histogram of DAB optical density values |
| `results$automatedanalysis$validationplot` |  |  |  |  | Scatter plot of manual vs automated scores |
| `results$advancedmetrics$reliabilitymetrics` |  |  |  |  | Comprehensive reliability and consistency metrics |
| `results$advancedmetrics$distributionanalysis` |  |  |  |  | Normality testing and distribution characteristics |
| `results$multiplecutoffs$cutoffresults` |  |  |  |  | Positivity rates across different cut-off thresholds |
| `results$multiplecutoffs$comparisonstats` |  |  |  |  | Statistical tests comparing groups across different cut-off values |
| `results$cutoffplot` |  |  |  |  | Biomarker positivity rates across different thresholds |
| `results$cpsanalysis$cpsresults` |  |  |  |  | Combined Positive Score analysis for PD-L1 assessment |
| `results$cpsanalysis$cpsstatistics` |  |  |  |  | Summary statistics for Combined Positive Score |
| `results$cpsanalysis$cpscomparison` |  |  |  |  | Statistical comparison of CPS between groups |
| `results$cpsplot` |  |  |  |  | Distribution of CPS scores with clinical thresholds |
| `results$molecularclassification$classificationtable` |  |  |  |  | Molecular subtype classification based on marker combinations |
| `results$molecularclassification$subtypedistribution` |  |  |  |  | Frequency and percentage of each molecular subtype |
| `results$molecularclassification$subtypestatistics` |  |  |  |  | Statistical comparisons between molecular subtypes |
| `results$molecularclassification$checkpointanalysis` |  |  |  |  | PD-1/PD-L1 expression patterns across molecular subtypes |
| `results$subtypeplot` |  |  |  |  | Visual representation of molecular subtype frequencies |
| `results$checkpointplot` |  |  |  |  | PD-1/PD-L1 expression patterns across molecular subtypes |
| `results$exportdata` |  |  |  |  | Formatted results for external analysis or reporting |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$scorestable$asDF`

`as.data.frame(results$scorestable)`
