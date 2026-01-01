
test_that('deeplearningprediction analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    image_path_var = sample(c('A', 'B'), n, replace = TRUE),
    target_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- deeplearningprediction(
      data = data,
    image_path_var = 'image_path_var',
    target_var = 'target_var',
    model_type = 'vision_transformer',
    prediction_type = 'classification',
    patch_size = 384,
    batch_size = 8,
    validation_split = 0.2,
    learning_rate = 1e-04,
    epochs = 10,
    attention_maps = TRUE,
    data_augmentation = TRUE,
    gray_zone_width = 5,
    pretrained_weights = TRUE,
    freeze_backbone = FALSE,
    dropout_rate = 0.1,
    weight_decay = 1e-04,
    early_stopping = TRUE,
    save_model = FALSE,
    gpu_acceleration = TRUE,
    cross_validation = FALSE,
    cv_folds = 5,
    class_weights = TRUE,
    confidence_threshold = 0.8
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'deeplearningprediction.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

