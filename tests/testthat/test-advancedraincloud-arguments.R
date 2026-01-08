# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: advancedraincloud
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("advancedraincloud handles clinical significance options", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    clinical_cutoff = 50,
    reference_range_min = 0,
    reference_range_max = 30,
    show_mcid = TRUE,
    mcid_value = 10
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles effect size calculations", {
  devtools::load_all()

  data(advancedraincloud_test)

  effect_types <- c("cohens_d", "hedges_g", "glass_delta")

  for (etype in effect_types) {
    result <- advancedraincloud(
      data = advancedraincloud_test,
      y_var = "pain_score",
      x_var = "timepoint",
      fill_var = "treatment",
      show_effect_size = TRUE,
      effect_size_type = etype
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for effect size type:", etype))
  }
})

test_that("advancedraincloud handles change score analysis", {
  devtools::load_all()

  data(advancedraincloud_test)

  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    show_change_scores = TRUE,
    baseline_group = "Baseline",
    responder_threshold = 20
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles sample size annotations", {
  devtools::load_all()

  data(advancedraincloud_test)

  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment",
    show_sample_size = TRUE,
    show_missing_info = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles clinical trial features", {
  devtools::load_all()

  data(advancedraincloud_test)

  populations <- c("itt", "pp", "mitt", "at")

  for (pop in populations) {
    result <- advancedraincloud(
      data = advancedraincloud_test,
      y_var = "pain_score",
      x_var = "timepoint",
      fill_var = "treatment",
      trial_arms = "Placebo,Low Dose,High Dose",
      time_labels = "Baseline,Week 4,Week 12",
      population_type = pop
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for population type:", pop))
  }
})

test_that("advancedraincloud handles biomarker features", {
  devtools::load_all()

  data(advancedraincloud_test)
  week12_data <- subset(advancedraincloud_test, timepoint == "Week 12")

  result <- advancedraincloud(
    data = week12_data,
    y_var = "crp_level",
    x_var = "treatment",
    log_transform = TRUE,
    show_cv_bands = TRUE,
    cv_band_1 = 15,
    cv_band_2 = 20
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles outlier methods", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  outlier_methods <- c("none", "winsorize", "trim", "iqr")

  for (method in outlier_methods) {
    result <- advancedraincloud(
      data = baseline_data,
      y_var = "pain_score",
      x_var = "treatment",
      outlier_method = method
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for outlier method:", method))
  }
})

test_that("advancedraincloud handles p-value positions", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  p_positions <- c("above", "legend", "table", "none")

  for (pos in p_positions) {
    result <- advancedraincloud(
      data = baseline_data,
      y_var = "pain_score",
      x_var = "treatment",
      show_comparisons = TRUE,
      p_value_position = pos
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for p-value position:", pos))
  }
})

test_that("advancedraincloud handles journal styles", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  journal_styles <- c("default", "nature", "nejm", "lancet", "jama")

  for (style in journal_styles) {
    result <- advancedraincloud(
      data = baseline_data,
      y_var = "pain_score",
      x_var = "treatment",
      journal_style = style
    )

    expect_s3_class(result, "advancedraincloudResults",
                   info = paste("Failed for journal style:", style))
  }
})

test_that("advancedraincloud handles report generation options", {
  devtools::load_all()

  data(advancedraincloud_test)

  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    generate_report = TRUE,
    include_methods = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles combined advanced features", {
  devtools::load_all()

  data(advancedraincloud_test)

  # Test comprehensive clinical trial scenario
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    id_var = "patient_id",
    cov_var = "age",
    show_longitudinal = TRUE,
    rain_side = "f",
    show_statistics = TRUE,
    show_comparisons = TRUE,
    show_effect_size = TRUE,
    effect_size_type = "cohens_d",
    show_change_scores = TRUE,
    baseline_group = "Baseline",
    clinical_cutoff = 50,
    show_mcid = TRUE,
    mcid_value = 10,
    show_sample_size = TRUE,
    trial_arms = "Placebo,Low Dose,High Dose",
    time_labels = "Baseline,Week 4,Week 12",
    population_type = "itt",
    journal_style = "nejm"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles optional variables set to NULL", {
  devtools::load_all()

  data(advancedraincloud_test)
  baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

  # All optional variables explicitly NULL
  result <- advancedraincloud(
    data = baseline_data,
    y_var = "pain_score",
    x_var = "treatment",
    fill_var = NULL,
    id_var = NULL,
    cov_var = NULL
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles show/hide options", {
  devtools::load_all()

  data(advancedraincloud_test)

  # All show options FALSE
  result1 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment",
    show_statistics = FALSE,
    show_comparisons = FALSE,
    show_interpretation = FALSE,
    show_effect_size = FALSE,
    show_change_scores = FALSE,
    show_sample_size = FALSE,
    show_missing_info = FALSE,
    show_mcid = FALSE,
    show_cv_bands = FALSE
  )
  expect_s3_class(result1, "advancedraincloudResults")

  # All show options TRUE
  result2 <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "timepoint",
    fill_var = "treatment",
    show_statistics = TRUE,
    show_comparisons = TRUE,
    show_interpretation = TRUE,
    show_effect_size = TRUE,
    show_sample_size = TRUE,
    show_missing_info = TRUE
  )
  expect_s3_class(result2, "advancedraincloudResults")
})
