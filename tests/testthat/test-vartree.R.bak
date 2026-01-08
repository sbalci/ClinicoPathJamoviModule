
test_that('vartree analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = sample(c('A', 'B'), n, replace = TRUE),
    vars2 = sample(c('A', 'B'), n, replace = TRUE),
    vars3 = sample(c('A', 'B'), n, replace = TRUE),
    percvar = sample(c('A', 'B'), n, replace = TRUE),
    summaryvar = runif(n, 1, 100),
    prunebelow = sample(c('A', 'B'), n, replace = TRUE),
    follow = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- vartree(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    percvar = 'percvar',
    summaryvar = 'summaryvar',
    summarylocation = 'leafonly',
    style = 'default',
    prunebelow = 'prunebelow',
    follow = 'follow',
    excl = FALSE,
    vp = TRUE,
    horizontal = FALSE,
    sline = TRUE,
    varnames = FALSE,
    nodelabel = TRUE,
    pct = FALSE,
    showcount = TRUE,
    legend = FALSE,
    pattern = FALSE,
    sequence = FALSE,
    ptable = FALSE,
    useprunesmaller = FALSE,
    prunesmaller = 5,
    showInterpretation = TRUE,
    maxwidth = 600
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'vartree.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

