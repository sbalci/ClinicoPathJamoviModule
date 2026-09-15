# Explainable AI Analysis

Comprehensive explainable AI toolkit for interpreting machine learning
models in medical and clinical applications. Provides attention maps,
feature importance, SHAP values, and other interpretability methods.

## Usage

``` r
explainableai(
  data,
  analysis_type = "feature_importance",
  features,
  target_var,
  model_predictions,
  image_paths,
  attention_maps,
  shap_method = "kernel_explainer",
  lime_method = "tabular",
  n_samples = 100,
  n_features = 20,
  plot_type = "summary",
  overlay_original = TRUE,
  confidence_level = 0.95,
  background_samples = 100,
  perturbation_method = "random",
  clustering_method = "none",
  interaction_analysis = FALSE,
  local_explanations = TRUE,
  global_explanations = TRUE,
  save_explanations = FALSE,
  explanation_path = "",
  attention_threshold = 0.1,
  colormap = "viridis"
)
```

## Arguments

- data:

  the data as a data frame

- analysis_type:

  type of explainability analysis to perform

- features:

  features/variables for importance analysis

- target_var:

  target variable or model predictions

- model_predictions:

  variable containing model predictions or probabilities

- image_paths:

  variable containing paths to image files

- attention_maps:

  variable containing attention map data or file paths

- shap_method:

  SHAP explainer method based on model type

- lime_method:

  LIME explanation method for different data types

- n_samples:

  number of samples to use for explanation analysis

- n_features:

  number of top important features to display

- plot_type:

  type of visualization for explanations

- overlay_original:

  overlay attention maps on original images

- confidence_level:

  confidence level for statistical intervals

- background_samples:

  number of background samples for SHAP baseline

- perturbation_method:

  method for perturbing features in permutation importance

- clustering_method:

  method for grouping similar features

- interaction_analysis:

  analyze pairwise feature interactions

- local_explanations:

  create individual sample explanations

- global_explanations:

  create overall model explanations

- save_explanations:

  save explanation data for external use

- explanation_path:

  file path to save explanation results

- attention_threshold:

  minimum attention value to display in visualizations

- colormap:

  color palette for explanation visualizations

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$overview` |  |  |  |  | a html |
| `results$featureimportance$importancetable` |  |  |  |  | Ranked list of feature importance scores |
| `results$featureimportance$importanceplot` |  |  |  |  | Bar plot of feature importance scores |
| `results$shapanalysis$shapvaluestable` |  |  |  |  | Mean absolute SHAP values per feature |
| `results$shapanalysis$shapwaterfalltable` |  |  |  |  | Individual prediction explanations |
| `results$shapanalysis$shapinteractiontable` |  |  |  |  | Feature interaction effects |
| `results$shapanalysis$shapsummaryplot` |  |  |  |  | Overview of SHAP values for all features |
| `results$shapanalysis$shapwaterfallplot` |  |  |  |  | Individual prediction explanation |
| `results$shapanalysis$shapinteractionplot` |  |  |  |  | Feature interaction heatmap |
| `results$limeanalysis$limeexplanationtable` |  |  |  |  | Local explanations for individual predictions |
| `results$limeanalysis$limeplot` |  |  |  |  | Local explanation visualization |
| `results$attentionanalysis$attentionstatstable` |  |  |  |  | Summary statistics of attention patterns |
| `results$attentionanalysis$attentionpeakstable` |  |  |  |  | Highest attention regions across samples |
| `results$attentionanalysis$attentionheatmapplot` |  |  |  |  | Attention map overlays on original images |
| `results$attentionanalysis$attentiondistributionplot` |  |  |  |  | Distribution of attention values |
| `results$partialdependence$pdptable` |  |  |  |  | Partial dependence effect sizes |
| `results$partialdependence$pdpplot` |  |  |  |  | Effect of individual features on predictions |
| `results$partialdependence$iceplot` |  |  |  |  | Individual conditional expectation curves |
| `results$globalexplanations$modelinsightstable` |  |  |  |  | Overall model behavior insights |
| `results$globalexplanations$featureclusteringtable` |  |  |  |  | Groups of similar features |
| `results$globalexplanations$globalinsightplot` |  |  |  |  | Overall model behavior visualization |
| `results$globalexplanations$featureclusterplot` |  |  |  |  | Dendrogram or cluster plot of features |
| `results$localexplanations$samplewiseexplanationtable` |  |  |  |  | Detailed explanations for individual samples |
| `results$localexplanations$localexplanationplot` |  |  |  |  | Individual sample explanation examples |
| `results$validationmetrics$validationtable` |  |  |  |  | Quality metrics for explanations |
| `results$validationmetrics$stabilityanalysistable` |  |  |  |  | Consistency of explanations across perturbations |
| `results$validationmetrics$validationplot` |  |  |  |  | Validation metrics visualization |
