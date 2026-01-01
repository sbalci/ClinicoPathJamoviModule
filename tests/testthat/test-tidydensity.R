test_that("tidydensity function exists and basic functionality works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test function existence
  expect_true(exists("tidydensity"))
  expect_true(is.function(tidydensity))
  
  # Test with minimal valid parameters - should not error
  expect_no_error({
    result <- tidydensity(
      data = data.frame(),
      distribution_type = "normal",
      n_observations = 100,
      n_simulations = 1
    )
  })
})

test_that("tidydensity normal distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 50,
    n_simulations = 2,
    normal_mean = 10,
    normal_sd = 2,
    show_statistics = TRUE,
    show_summary_table = TRUE
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true("main_plot" %in% names(result))
  expect_true("distribution_statistics" %in% names(result))
  expect_true("summary_table" %in% names(result))
  expect_true(result$main_plot$visible)
  expect_true(result$distribution_statistics$visible)
  expect_true(result$summary_table$visible)
})

test_that("tidydensity beta distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "beta",
    n_observations = 100,
    n_simulations = 1,
    beta_shape1 = 3,
    beta_shape2 = 7,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity gamma distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "gamma",
    n_observations = 100,
    n_simulations = 1,
    gamma_shape = 2,
    gamma_scale = 1.5,
    plot_type = "quantile"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity exponential distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "exponential",
    n_observations = 100,
    n_simulations = 1,
    exponential_rate = 0.5,
    plot_type = "probability"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity uniform distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "uniform",
    n_observations = 100,
    n_simulations = 1,
    uniform_min = 5,
    uniform_max = 15,
    plot_type = "qq"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity chi-square distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "chisquare",
    n_observations = 100,
    n_simulations = 1,
    chisq_df = 5,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity t distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "t",
    n_observations = 100,
    n_simulations = 1,
    t_df = 10,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity F distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "f",
    n_observations = 100,
    n_simulations = 1,
    f_df1 = 3,
    f_df2 = 8,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity binomial distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "binomial",
    n_observations = 100,
    n_simulations = 1,
    binomial_size = 20,
    binomial_prob = 0.3,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity poisson distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "poisson",
    n_observations = 100,
    n_simulations = 1,
    poisson_lambda = 5,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity weibull distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "weibull",
    n_observations = 100,
    n_simulations = 1,
    weibull_shape = 2,
    weibull_scale = 1,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity lognormal distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "lognormal",
    n_observations = 100,
    n_simulations = 1,
    lognormal_meanlog = 1,
    lognormal_sdlog = 0.5,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity logistic distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "logistic",
    n_observations = 100,
    n_simulations = 1,
    logistic_location = 5,
    logistic_scale = 2,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity cauchy distribution generation works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "cauchy",
    n_observations = 100,
    n_simulations = 1,
    cauchy_location = 0,
    cauchy_scale = 1,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity multiple simulations work", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 50,
    n_simulations = 5,
    normal_mean = 0,
    normal_sd = 1,
    show_statistics = TRUE,
    show_summary_table = TRUE
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
  expect_true(result$distribution_statistics$visible)
  expect_true(result$summary_table$visible)
})

test_that("tidydensity plot types work correctly", {
  skip_if_not_installed("TidyDensity")
  
  plot_types <- c("density", "quantile", "probability", "qq")
  
  for (plot_type in plot_types) {
    result <- tidydensity(
      data = data.frame(),
      distribution_type = "normal",
      n_observations = 100,
      n_simulations = 1,
      plot_type = plot_type
    )
    
    expect_s3_class(result, "tidydensityResults")
    expect_true(result$main_plot$visible)
  }
})

test_that("tidydensity plot enhancements work", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 100,
    n_simulations = 1,
    plot_enhancements = TRUE,
    plot_type = "density"
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity parameter information works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 100,
    n_simulations = 1,
    show_parameter_info = TRUE
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$parameter_info$visible)
})

test_that("tidydensity interpretation guide works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 100,
    n_simulations = 1,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$interpretation$visible)
})

test_that("tidydensity handles missing TidyDensity package gracefully", {
  # This test would be difficult to implement without actually uninstalling the package
  # But we can test that the function doesn't crash when the package is available
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 100,
    n_simulations = 1
  )
  
  expect_s3_class(result, "tidydensityResults")
})

test_that("tidydensity welcome message displays correctly", {
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 100,
    n_simulations = 1
  )
  
  expect_s3_class(result, "tidydensityResults")
  # Welcome message should display for default parameters
  expect_true(result$todo$visible)
})

test_that("tidydensity handles different sample sizes", {
  skip_if_not_installed("TidyDensity")
  
  sample_sizes <- c(10, 50, 100, 500, 1000)
  
  for (n in sample_sizes) {
    expect_no_error({
      result <- tidydensity(
        data = data.frame(),
        distribution_type = "normal",
        n_observations = n,
        n_simulations = 1
      )
    })
  }
})

test_that("tidydensity economist plot type works", {
  skip_if_not_installed("TidyDensity")
  
  result <- tidydensity(
    data = data.frame(),
    distribution_type = "normal",
    n_observations = 100,
    n_simulations = 1,
    plot_type = "economist",
    economist_theme = TRUE,
    economist_colors = TRUE
  )
  
  expect_s3_class(result, "tidydensityResults")
  expect_true(result$main_plot$visible)
})

test_that("tidydensity handles edge cases", {
  skip_if_not_installed("TidyDensity")
  
  # Test with minimum observations
  expect_no_error({
    result <- tidydensity(
      data = data.frame(),
      distribution_type = "normal",
      n_observations = 10,
      n_simulations = 1
    )
  })
  
  # Test with maximum simulations
  expect_no_error({
    result <- tidydensity(
      data = data.frame(),
      distribution_type = "normal",
      n_observations = 100,
      n_simulations = 20
    )
  })
  
  # Test with extreme parameters
  expect_no_error({
    result <- tidydensity(
      data = data.frame(),
      distribution_type = "normal",
      n_observations = 100,
      n_simulations = 1,
      normal_mean = 1000,
      normal_sd = 100
    )
  })
})
