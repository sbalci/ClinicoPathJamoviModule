
test_that('imagingcorrelation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    imagingFindings1 = sample(c('A', 'B'), n, replace = TRUE),
    imagingFindings2 = sample(c('A', 'B'), n, replace = TRUE),
    imagingFindings3 = sample(c('A', 'B'), n, replace = TRUE),
    labResults1 = runif(n, 1, 100),
    labResults2 = runif(n, 1, 100),
    labResults3 = runif(n, 1, 100),
    clinicalVars1 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalVars2 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalVars3 = sample(c('A', 'B'), n, replace = TRUE),
    pathologyData1 = sample(c('A', 'B'), n, replace = TRUE),
    pathologyData2 = sample(c('A', 'B'), n, replace = TRUE),
    pathologyData3 = sample(c('A', 'B'), n, replace = TRUE),
    imagingModality1 = sample(c('A', 'B'), n, replace = TRUE),
    imagingModality2 = sample(c('A', 'B'), n, replace = TRUE),
    imagingModality3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- imagingcorrelation(
      data = data,
    imagingFindings = c('imagingFindings1', 'imagingFindings2', 'imagingFindings3'),
    labResults = c('labResults1', 'labResults2', 'labResults3'),
    clinicalVars = c('clinicalVars1', 'clinicalVars2', 'clinicalVars3'),
    pathologyData = c('pathologyData1', 'pathologyData2', 'pathologyData3'),
    imagingModality = c('imagingModality1', 'imagingModality2', 'imagingModality3'),
    correlation_analysis = TRUE,
    concordance_assessment = TRUE,
    pattern_recognition = TRUE,
    sensitivity_specificity = TRUE,
    integration_method = 'weighted_fusion',
    correlation_method = 'spearman',
    confidence_level = 0.95,
    minimum_correlation = 0.3,
    radiomics_analysis = FALSE,
    texture_analysis = FALSE,
    lesion_characterization = TRUE,
    temporal_analysis = FALSE,
    anatomical_correlation = TRUE,
    staging_correlation = TRUE,
    treatment_response = FALSE,
    ai_assisted = FALSE,
    correlation_plots = TRUE,
    concordance_plots = TRUE,
    heatmap_visualization = TRUE,
    network_diagram = FALSE,
    roc_curves = TRUE,
    clinical_guidelines = TRUE,
    diagnostic_confidence = TRUE,
    report_generation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'imagingcorrelation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

