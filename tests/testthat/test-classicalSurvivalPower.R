
test_that('classicalSurvivalPower analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- classicalSurvivalPower(
    calculation_type = 'sample_size',
    method = 'lachin_foulkes',
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    hazard_ratio = 0.6,
    study_duration = 24,
    accrual_duration = 12,
    dropout_rate = 0,
    allocation_ratio = 1,
    alpha = 0.025,
    beta = 0.1,
    power = 0.9,
    sided = 'one_sided',
    entry_type = 'unif',
    gamma = 0,
    sample_size_input = 100,
    events_input = 50,
    show_summary = TRUE,
    show_formulas = FALSE,
    show_interpretation = TRUE,
    show_power_plot = FALSE,
    show_timeline_plot = FALSE,
    export_results = FALSE,
    export_power_curve = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'classicalSurvivalPower.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

lf_design <- function(sided = 1, entry = "unif", gamma = NA_real_) {
  list(lambda1 = 0.083, lambda2 = 0.042, Ts = 24, Tr = 12,
       eta = 0, ratio = 1, alpha = 0.025, sided = sided,
       entry = entry, gamma = gamma)
}

test_that("Lachin-Foulkes sample size and solved power round trip", {
  skip_if_not_installed("gsDesign")
  for (design in list(lf_design(), lf_design(sided = 2),
                      lf_design(entry = "expo", gamma = 0.1))) {
    for (target_beta in c(0.2, 0.1, 0.01)) {
      required <- do.call(gsDesign::nSurvival, c(design, list(beta = target_beta)))
      actual <- .classicalSurvivalPower_lf_power(required$n, design)
      expect_equal(actual$power, 1 - target_beta, tolerance = 1e-7)
      expect_equal(actual$result$n, required$n, tolerance = 1e-5)
      expect_equal(actual$events, required$nEvents, tolerance = 1e-5)
    }
  }
})

test_that("Lachin-Foulkes solved power and the curve rise with sample size", {
  skip_if_not_installed("gsDesign")
  design <- lf_design()
  n80 <- do.call(gsDesign::nSurvival, c(design, list(beta = 0.2)))$n
  sizes <- seq(n80 / 2, 2 * n80, length.out = 50)
  powers <- vapply(sizes, function(n) {
    .classicalSurvivalPower_lf_power(n, design)$power
  }, numeric(1))
  expect_true(all(diff(powers) > 0))
  expect_equal(.classicalSurvivalPower_lf_power(n80, design)$power, 0.8,
               tolerance = 1e-7)
  expect_gt(powers[50], 0.8)
  expect_lt(powers[1], 0.8)
})

test_that("Lachin-Foulkes inverse handles low power and invalid boundaries", {
  skip_if_not_installed("gsDesign")
  design <- lf_design()
  n40 <- do.call(gsDesign::nSurvival, c(design, list(beta = 0.6)))$n
  expect_equal(.classicalSurvivalPower_lf_power(n40, design)$power, 0.4,
               tolerance = 1e-7)
  expect_error(.classicalSurvivalPower_lf_power(0, design), "positive finite")
  expect_error(.classicalSurvivalPower_lf_power(Inf, design), "positive finite")
  expect_error(.classicalSurvivalPower_lf_power(10, modifyList(design,
    list(lambda2 = design$lambda1))), "Invalid Lachin-Foulkes")
  expect_error(.classicalSurvivalPower_lf_power(10, modifyList(design,
    list(Ts = 5))), "Invalid Lachin-Foulkes")
  expect_error(.classicalSurvivalPower_lf_power(10, modifyList(design,
    list(entry = "expo", gamma = 0))), "Invalid Lachin-Foulkes")
  numerical_limit <- do.call(gsDesign::nSurvival,
    c(design, list(beta = .Machine$double.eps)))$n
  expect_error(.classicalSurvivalPower_lf_power(2 * numerical_limit, design),
               "outside the numerically solvable")
})

test_that("Lachin-Foulkes jamovi power uses the inverse and Schoenfeld stays intact", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("jmvcore")
  design <- lf_design()
  required <- do.call(gsDesign::nSurvival, c(design, list(beta = 0.1)))
  model <- classicalSurvivalPower(
    calculation_type = "power", method = "lachin_foulkes",
    sample_size_input = required$n, show_interpretation = FALSE,
    export_power_curve = TRUE
  )
  expect_match(model$results$power_results$content, "Statistical Power:</strong> 90")
  expect_match(model$results$power_results$content, "Expected Events")
  expect_false(grepl("Power (Estimated)", model$results$power_results$content,
                     fixed = TRUE))
  expect_match(model$results$export_summary$content, "Power curve:</strong> 50 points")

  schoenfeld <- classicalSurvivalPower(
    calculation_type = "power", method = "schoenfeld",
    events_input = 50, show_interpretation = FALSE
  )
  expected <- gsDesign::nEvents(hr = 0.6, alpha = 0.025, ratio = 1,
                                sided = 1, n = 50, tbl = TRUE)$Power[1]
  expect_match(schoenfeld$results$power_results$content,
               paste0("Statistical Power:</strong> ", round(expected * 100, 1)),
               fixed = TRUE)
})

test_that("Schoenfeld curve agrees with the entered events and increases", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("jmvcore")
  model <- classicalSurvivalPower(
    calculation_type = "power", method = "schoenfeld", events_input = 50,
    power_plot_range = "25,100", export_power_curve = TRUE,
    show_interpretation = FALSE
  )
  private <- model$.__enclos_env__$private
  curve <- private$.generate_power_curve_data()
  main_power <- private$.results_data$power
  expect_equal(nrow(curve), 50L)
  expect_true(any(curve$amount == 50))
  expect_equal(curve$power[curve$amount == 50], main_power, tolerance = 1e-10)
  expect_true(all(diff(curve$power) > 0))
  expect_match(model$results$export_summary$content, "Number of events versus power")
})
