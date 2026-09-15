# Deep Learning Image Prediction

Advanced deep learning prediction module for histopathological image
analysis. Implements Vision Transformer (ViT), ResNet, and other CNN
architectures for biomarker prediction from H&E-stained tissue images.

## Usage

``` r
deeplearningprediction(
  data,
  image_path_var,
  target_var,
  model_type = "vision_transformer",
  prediction_type = "classification",
  patch_size = 384,
  batch_size = 8,
  validation_split = 0.2,
  learning_rate = 1e-04,
  epochs = 10,
  attention_maps = TRUE,
  data_augmentation = TRUE,
  gray_zone_width = 5,
  thresholds = "",
  pretrained_weights = TRUE,
  freeze_backbone = FALSE,
  dropout_rate = 0.1,
  weight_decay = 1e-04,
  early_stopping = TRUE,
  save_model = FALSE,
  model_path = "",
  gpu_acceleration = TRUE,
  cross_validation = FALSE,
  cv_folds = 5,
  class_weights = TRUE,
  confidence_threshold = 0.8
)
```

## Arguments

- data:

  the data as a data frame

- image_path_var:

  variable containing file paths to histological images

- target_var:

  dependent variable to predict (biomarker status, classification)

- model_type:

  deep learning architecture for image analysis

- prediction_type:

  type of prediction task and output format

- patch_size:

  size of image patches for processing (pixels)

- batch_size:

  number of images processed simultaneously

- validation_split:

  proportion of data used for validation

- learning_rate:

  learning rate for model training

- epochs:

  number of training epochs

- attention_maps:

  create attention/saliency maps for model explainability

- data_augmentation:

  apply image transformations during training

- gray_zone_width:

  width of gray zone for cumulative logit models

- thresholds:

  custom thresholds for ordinal prediction (comma-separated numbers)

- pretrained_weights:

  initialize with ImageNet pretrained weights

- freeze_backbone:

  freeze pretrained backbone for fine-tuning

- dropout_rate:

  dropout rate for regularization

- weight_decay:

  L2 regularization weight decay

- early_stopping:

  stop training when validation loss stops improving

- save_model:

  save trained model for future use

- model_path:

  file path to save trained model

- gpu_acceleration:

  utilize GPU for faster training if available

- cross_validation:

  perform k-fold cross-validation

- cv_folds:

  number of cross-validation folds

- class_weights:

  apply class balancing weights for imbalanced datasets

- confidence_threshold:

  minimum confidence threshold for predictions

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$modelSummary` |  |  |  |  | a html |
| `results$dataInfo` |  |  |  |  | Summary of input data and preprocessing |
| `results$trainingMetrics$trainingHistory` |  |  |  |  | Epoch-by-epoch training progress |
| `results$trainingMetrics$finalMetrics` |  |  |  |  | Overall model performance assessment |
| `results$predictionResults$classificationResults` |  |  |  |  | a table |
| `results$predictionResults$cumulativeLogitResults` |  |  |  |  | a table |
| `results$performanceAnalysis$confusionMatrix` |  |  |  |  | Classification confusion matrix |
| `results$performanceAnalysis$classificationReport` |  |  |  |  | Precision, recall, F1-score per class |
| `results$performanceAnalysis$rocAnalysis` |  |  |  |  | ROC AUC scores for each class |
| `results$explainabilityAnalysis$attentionSummary` |  |  |  |  | Summary of attention patterns across samples |
| `results$explainabilityAnalysis$featureImportance` |  |  |  |  | Most important image features for prediction |
| `results$crossValidationResults$cvMetrics` |  |  |  |  | Performance across CV folds |
| `results$crossValidationResults$cvSummary` |  |  |  |  | Mean and standard deviation of CV metrics |
| `results$trainingCurvePlot` |  |  |  |  | Training and validation loss/accuracy over epochs |
| `results$confusionMatrixPlot` |  |  |  |  | Heatmap of confusion matrix |
| `results$rocCurvePlot` |  |  |  |  | ROC curves for each class |
| `results$attentionHeatmapPlot` |  |  |  |  | Sample attention maps showing model focus areas |
| `results$predictionDistributionPlot` |  |  |  |  | Distribution of prediction confidence scores |
| `results$cumulativeLogitPlot` |  |  |  |  | Cumulative probability plots for ordinal prediction |
| `results$crossValidationPlot` |  |  |  |  | Box plots of CV performance metrics |
| `results$modelArchitecturePlot` |  |  |  |  | Visual representation of the neural network architecture |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
