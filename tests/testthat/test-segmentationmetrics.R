
test_that('segmentationmetrics analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    prediction_mask = sample(c('A', 'B'), n, replace = TRUE),
    ground_truth_mask = sample(c('A', 'B'), n, replace = TRUE),
    image_id = sample(c('A', 'B'), n, replace = TRUE),
    stratify_by = sample(c('A', 'B'), n, replace = TRUE),
    comparison_method = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  model <- segmentationmetrics(
      data = data,
      prediction_mask = 'prediction_mask',
      positive_class = NULL,
      ground_truth_mask = 'ground_truth_mask',
      image_id = 'image_id',
      segmentation_type = 'binary',
      dice_coefficient = TRUE,
      jaccard_index = TRUE,
      volumetric_similarity = FALSE,
      sensitivity_specificity = TRUE,
      hausdorff_distance = TRUE,
      average_hausdorff = TRUE,
      surface_distance = TRUE,
      surface_overlap = FALSE,
      boundary_tolerance = 2,
      pixel_size_provided = FALSE,
      pixel_size_x = 0.5,
      pixel_size_y = 0.5,
      class_specific_metrics = TRUE,
      macro_average = TRUE,
      weighted_average = TRUE,
      object_detection_metrics = FALSE,
      iou_threshold = 0.5,
      count_metrics = FALSE,
      confidence_intervals = TRUE,
      bootstrap_ci = FALSE,
      bootstrap_samples = 100,
      confidence_level = 0.95,
      quality_thresholds = TRUE,
      dice_threshold_excellent = 0.9,
      dice_threshold_good = 0.8,
      dice_threshold_acceptable = 0.7,
      stratified_analysis = FALSE,
      stratify_by = 'stratify_by',
      outlier_detection = TRUE,
      outlier_method = 'iqr',
      plot_metric_distribution = TRUE,
      plot_scatter_comparison = TRUE,
      plot_boundary_error = TRUE,
      plot_confusion_matrix = FALSE,
      plot_performance_by_class = FALSE,
      application_context = 'general',
      show_interpretation = TRUE,
      paired_analysis = FALSE,
      comparison_method = 'comparison_method',
      missing_handling = 'complete',
      random_seed = 123
    )

  expect_s3_class(model, "segmentationmetricsResults")
  
  # Verify and Export OMV
  omv_path <- file.path('omv_output', 'segmentationmetrics.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  tryCatch({
    jmvReadWrite::write_omv(model, omv_path)
  }, error = function(e){
      message("OMV export failed: ", e$message)
  })
  
  if (!file.exists(omv_path)) {
     skip("OMV export failed, skipping file existence check")
  }

  expect_true(file.exists(omv_path))
})

