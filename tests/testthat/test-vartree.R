
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
  result <- vartree(
    data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    percvar = 'percvar',
    percvarLevel = 'A',
    summaryvar = 'summaryvar',
    summarylocation = 'leafonly',
    style = 'default',
    prunebelow = 'prunebelow',
    pruneLevel1 = 'A',
    pruneLevel2 = NULL,
    follow = 'follow',
    followLevel1 = 'A',
    followLevel2 = NULL,
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

  # Verify result object
  expect_s3_class(result, 'vartreeResults')

  # Check that main outputs exist
  # vartree uses text1 (Html) for the tree visualization, not a plot
  expect_true(!is.null(result$text1))
  expect_true(!is.null(result$interpretation))
})

