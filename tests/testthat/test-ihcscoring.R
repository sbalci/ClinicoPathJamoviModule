
test_that('ihcscoring analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    intensity_var = runif(n, 1, 100),
    proportion_var = runif(n, 1, 100),
    sample_id_var = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    immune_cells_var = runif(n, 1, 100),
    tumor_cells_var = runif(n, 1, 100),
    primary_marker1 = runif(n, 1, 100),
    primary_marker2 = runif(n, 1, 100),
    secondary_marker = runif(n, 1, 100),
    pd1_marker = runif(n, 1, 100),
    pdl1_marker = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- ihcscoring(
      data = data,
    guided_biomarker = 'manual',
    intensity_var = 'intensity_var',
    proportion_var = 'proportion_var',
    sample_id_var = 'sample_id_var',
    group_var = 'group_var',
    scoring_method = 'both',
    binary_cutpoint = 100,
    allred_cutpoint = 3,
    intensity_scale = 'standard',
    biomarker_type = 'other',
    show_plots = TRUE,
    show_agreement_plots = TRUE,
    include_statistics = TRUE,
    include_digital_validation = FALSE,
    agreement_analysis = TRUE,
    quality_control = TRUE,
    clinical_interpretation = TRUE,
    export_results = FALSE,
    multiple_cutoffs = FALSE,
    cps_analysis = FALSE,
    immune_cells_var = 'immune_cells_var',
    tumor_cells_var = 'tumor_cells_var',
    cutoff_comparison = TRUE,
    confidence_level = 0.95,
    bootstrap_n = 1000,
    automated_analysis = FALSE,
    segmentation_method = 'manual',
    color_deconvolution = TRUE,
    minimum_nuclear_area = 50,
    maximum_nuclear_area = 2000,
    batch_processing = FALSE,
    image_format = 'tiff',
    validation_metrics = TRUE,
    molecular_classification = FALSE,
    classification_system = 'bladder_mibc',
    primary_marker1 = 'primary_marker1',
    primary_marker2 = 'primary_marker2',
    secondary_marker = 'secondary_marker',
    pd1_marker = 'pd1_marker',
    pdl1_marker = 'pdl1_marker',
    subtype_statistics = TRUE,
    subtype_visualization = TRUE,
    language = 'english',
    colorblind_safe = TRUE,
    high_contrast = FALSE,
    font_size = 'normal'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcscoring.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})



test_that("ihcscoring finds a binary-outcome optimal cutpoint (Youden)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("cutpointr")

  set.seed(11); n <- 220
  intensity  <- sample(0:3, n, TRUE)
  proportion <- round(runif(n, 0, 100))
  hscore <- intensity * proportion
  outcome <- factor(ifelse(rbinom(n, 1, plogis((hscore - 120) / 60)) == 1, "Pos", "Neg"),
                    levels = c("Neg", "Pos"))
  data <- data.frame(intensity_var = intensity, proportion_var = proportion,
                     outcome = outcome)

  expect_no_error({
    model <- ihcscoring(
      data = data, intensity_var = "intensity_var", proportion_var = "proportion_var",
      optimal_cutpoint = TRUE, optimize_score = "hscore",
      outcome_type = "binary", outcome_var = "outcome", outcome_positive = "Pos")
  })
  expect_true(inherits(model, "jmvcoreClass"))

  ct <- model$results$optimalCutpointTable$asDF
  expect_true(nrow(ct) >= 5)
  expect_true("Optimal cutpoint" %in% ct$quantity)
  expect_true("AUC" %in% ct$quantity)
})

test_that("ihcscoring finds a survival-outcome optimal cutpoint (maxstat)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("maxstat")

  set.seed(11); n <- 220
  intensity  <- sample(0:3, n, TRUE)
  proportion <- round(runif(n, 0, 100))
  hscore <- intensity * proportion
  time   <- rexp(n, 0.02 * (0.5 + hscore / 150))
  status <- rbinom(n, 1, 0.7)
  data <- data.frame(intensity_var = intensity, proportion_var = proportion,
                     time = time, event = status)

  expect_no_error({
    model <- ihcscoring(
      data = data, intensity_var = "intensity_var", proportion_var = "proportion_var",
      optimal_cutpoint = TRUE, optimize_score = "hscore",
      outcome_type = "survival", outcome_var = "event", outcome_positive = "1",
      cutpoint_time_var = "time")
  })
  expect_true(inherits(model, "jmvcoreClass"))

  ct <- model$results$optimalCutpointTable$asDF
  expect_true("Optimal cutpoint" %in% ct$quantity)
  expect_true(any(grepl("log-rank", ct$quantity, ignore.case = TRUE)))
})
