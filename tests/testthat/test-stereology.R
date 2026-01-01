
test_that('stereology analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    intersections = runif(n, 1, 100),
    totalPoints = runif(n, 1, 100),
    boundaryIntersections = runif(n, 1, 100),
    objectCount = runif(n, 1, 100),
    referenceArea = runif(n, 1, 100),
    gridSpacing = runif(n, 1, 100),
    groupVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- stereology(
      data = data,
    intersections = 'intersections',
    totalPoints = 'totalPoints',
    boundaryIntersections = 'boundaryIntersections',
    objectCount = 'objectCount',
    referenceArea = 'referenceArea',
    sectionThickness = 5,
    gridSpacing = 'gridSpacing',
    tissueType = 'vessels',
    groupVar = 'groupVar',
    calculateAa = TRUE,
    calculateVv = TRUE,
    calculateBa = FALSE,
    calculateNa = FALSE,
    calculateSv = FALSE,
    showConfidenceIntervals = TRUE,
    bootstrapIterations = 1000,
    showGroupComparison = FALSE,
    showDescriptives = TRUE,
    showPlot = TRUE,
    plotType = 'bars',
    showMethodology = TRUE,
    showReferences = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'stereology.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

