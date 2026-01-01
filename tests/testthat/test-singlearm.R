
test_that('singlearm analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    dxdate = sample(c('A', 'B'), n, replace = TRUE),
    fudate = sample(c('A', 'B'), n, replace = TRUE),
    outcome = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- singlearm(
      data = data,
    elapsedtime = 'elapsedtime',
    tint = FALSE,
    dxdate = 'dxdate',
    fudate = 'fudate',
    outcome = 'outcome',
    analysistype = 'overall',
    timetypedata = 'ymd',
    timetypeoutput = 'months',
    uselandmark = FALSE,
    landmark = 3,
    sc = FALSE,
    kmunicate = FALSE,
    ce = FALSE,
    ch = FALSE,
    endplot = 60,
    ybegin_plot = 0,
    yend_plot = 1,
    byplot = 12,
    multievent = FALSE,
    ci95 = FALSE,
    risktable = FALSE,
    censored = FALSE,
    medianline = 'none',
    person_time = FALSE,
    rate_multiplier = 100,
    baseline_hazard = FALSE,
    hazard_smoothing = FALSE,
    showExplanations = FALSE,
    showSummaries = FALSE,
    advancedDiagnostics = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'singlearm.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

