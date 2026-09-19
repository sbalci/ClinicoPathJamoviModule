
test_that('distributionfit analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    explanatory1 = sample(c('A', 'B'), n, replace = TRUE),
    explanatory2 = sample(c('A', 'B'), n, replace = TRUE),
    explanatory3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- distributionfit(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = c('explanatory1', 'explanatory2', 'explanatory3'),
    test_weibull = TRUE,
    test_exponential = TRUE,
    test_lognormal = TRUE,
    test_loglogistic = TRUE,
    test_gamma = TRUE,
    test_gengamma = FALSE,
    test_genf = FALSE,
    selection_method = 'aic',
    gof_tests = 'all_tests',
    confidence_level = 0.95,
    bootstrap_gof = TRUE,
    bootstrap_samples = 100,
    show_comparison_table = TRUE,
    show_gof_table = TRUE,
    show_parameter_table = TRUE,
    show_survival_plot = TRUE,
    show_hazard_plot = TRUE,
    show_pp_plot = TRUE,
    show_qq_plot = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'distributionfit.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})


test_that("the analysis constructs and reads the Weibull shape on its natural scale", {
  # A custom initialize() passed arguments distributionfitBase does not take, so the class
  # could never be created; and .getHazardShape() compared log(shape) with 1.
  set.seed(3); t <- rweibull(400, shape = 1.6, scale = 20)
  expect_no_error(ClinicoPath:::distributionfitClass$new(
    options = ClinicoPath:::distributionfitOptions$new(), data = data.frame(t = t)))
  m <- flexsurv::flexsurvreg(survival::Surv(t, rep(1, 400)) ~ 1, dist = "weibull")
  f <- ClinicoPath:::distributionfitClass$private_methods$.getHazardShape
  expect_equal(f("weibull", m), "Increasing")   # log(1.67) = 0.51 < 1 used to read "Decreasing"
})
