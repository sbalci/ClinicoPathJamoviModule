
test_that('singlearm analysis works', {
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
    outcomeLevel = 'B',
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
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

  # The R wrapper returns the analysis result group, not a generic list and not
  # an entire jamovi dataset suitable for write_omv().
  expect_true(inherits(model, 'singlearmResults'))
  expect_true(inherits(model, 'Group'))
  expect_equal(model$medianTable$rowCount, 1)
})
