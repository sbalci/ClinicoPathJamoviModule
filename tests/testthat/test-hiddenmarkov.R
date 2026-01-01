
test_that('hiddenmarkov analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    subject = sample(c('A', 'B'), n, replace = TRUE),
    state = sample(c('A', 'B'), n, replace = TRUE),
    time = runif(n, 1, 100),
    covs1 = sample(c('A', 'B'), n, replace = TRUE),
    covs2 = sample(c('A', 'B'), n, replace = TRUE),
    covs3 = sample(c('A', 'B'), n, replace = TRUE),
    hiddenCovs1 = sample(c('A', 'B'), n, replace = TRUE),
    hiddenCovs2 = sample(c('A', 'B'), n, replace = TRUE),
    hiddenCovs3 = sample(c('A', 'B'), n, replace = TRUE),
    censor = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- hiddenmarkov(
      data = data,
    subject = 'subject',
    state = 'state',
    time = 'time',
    covs = c('covs1', 'covs2', 'covs3'),
    hiddenCovs = c('hiddenCovs1', 'hiddenCovs2', 'hiddenCovs3'),
    nstates = 3,
    qmatrix = 'irreversible',
    censor = 'censor',
    pci = FALSE,
    obstype = 'exact',
    ematrix = 'identity',
    method = 'BFGS',
    showModel = TRUE,
    showTransitions = TRUE,
    showPrevalence = TRUE,
    showResiduals = FALSE,
    showViterbi = FALSE,
    bootstrap = FALSE,
    nboot = 500,
    plotTransitions = TRUE,
    plotPrevalence = TRUE,
    plotResiduals = FALSE,
    showEducational = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'hiddenmarkov.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

