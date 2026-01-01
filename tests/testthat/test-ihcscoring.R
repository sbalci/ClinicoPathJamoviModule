
test_that('ihcscoring analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

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

