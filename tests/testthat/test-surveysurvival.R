
test_that('surveysurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    dxdate = runif(n, 1, 100),
    fudate = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    weights = runif(n, 1, 100),
    strata = sample(c('A', 'B'), n, replace = TRUE),
    cluster = sample(c('A', 'B'), n, replace = TRUE),
    fpc = runif(n, 1, 100),
    explanatory1 = sample(c('A', 'B'), n, replace = TRUE),
    explanatory2 = sample(c('A', 'B'), n, replace = TRUE),
    explanatory3 = sample(c('A', 'B'), n, replace = TRUE),
    contexpl1 = runif(n, 1, 100),
    contexpl2 = runif(n, 1, 100),
    contexpl3 = runif(n, 1, 100),
    subpopulation = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- surveysurvival(
      data = data,
    elapsedtime = 'elapsedtime',
    tint = FALSE,
    dxdate = 'dxdate',
    fudate = 'fudate',
    timetypedata = 'ymd',
    timetypeoutput = 'months',
    outcome = 'outcome',
    weights = 'weights',
    strata = 'strata',
    cluster = 'cluster',
    fpc = 'fpc',
    design_type = 'srs',
    nest_clusters = FALSE,
    explanatory = c('explanatory1', 'explanatory2', 'explanatory3'),
    contexpl = c('contexpl1', 'contexpl2', 'contexpl3'),
    km_weighted = TRUE,
    cox_weighted = FALSE,
    robust_se = TRUE,
    ci_level = 0.95,
    population_totals = FALSE,
    subpopulation = 'subpopulation',
    km_plot = TRUE,
    endplot = 60,
    byplot = 12,
    ci95 = TRUE,
    risktable = FALSE,
    design_summary = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'surveysurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

