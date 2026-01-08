# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: statsplot2
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("statsplot2 handles all distribution types", {
  devtools::load_all()

  data(statsplot2_test)

  distributions <- c("p", "np", "r", "bf")

  for (dist in distributions) {
    result <- statsplot2(
      data = statsplot2_test,
      dep = "tumor_reduction",
      group = "treatment",
      distribution = dist
    )

    expect_s3_class(result, "statsplot2Results",
                   info = paste("Failed for distribution:", dist))
  }
})

test_that("statsplot2 handles all direction types", {
  devtools::load_all()

  data(statsplot2_test)
  data(statsplot2_repeated)

  # Independent samples
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    direction = "independent"
  )
  expect_s3_class(result1, "statsplot2Results")

  # Repeated measures
  result2 <- statsplot2(
    data = statsplot2_repeated,
    dep = "symptom_severity",
    group = "timepoint",
    direction = "repeated"
  )
  expect_s3_class(result2, "statsplot2Results")
})

test_that("statsplot2 handles plot type combinations", {
  devtools::load_all()

  data(statsplot2_test)

  # Continuous dep + categorical group → violin/box
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )
  expect_s3_class(result1, "statsplot2Results")

  # Categorical dep + categorical group → bar chart
  result2 <- statsplot2(
    data = statsplot2_test,
    dep = "response_status",
    group = "treatment"
  )
  expect_s3_class(result2, "statsplot2Results")

  # Continuous dep + continuous group → scatter
  result3 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "biomarker_level"
  )
  expect_s3_class(result3, "statsplot2Results")
})

test_that("statsplot2 handles split-by variable combinations", {
  devtools::load_all()

  data(statsplot2_test)

  split_vars <- c("sex", "tumor_stage", "age_group")

  for (grvar in split_vars) {
    result <- statsplot2(
      data = statsplot2_test,
      dep = "tumor_reduction",
      group = "treatment",
      grvar = grvar
    )

    expect_s3_class(result, "statsplot2Results",
                   info = paste("Failed for grvar:", grvar))
  }
})

test_that("statsplot2 handles label customizations", {
  devtools::load_all()

  data(statsplot2_test)

  # All labels provided
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    plotTitle = "Primary Outcome Analysis",
    xlab = "Treatment Arm",
    ylab = "Tumor Size Reduction (mm)"
  )
  expect_s3_class(result1, "statsplot2Results")

  # No labels (use defaults)
  result2 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )
  expect_s3_class(result2, "statsplot2Results")

  # Empty labels
  result3 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    plotTitle = "",
    xlab = "",
    ylab = ""
  )
  expect_s3_class(result3, "statsplot2Results")
})

test_that("statsplot2 handles repeated measures with split variable", {
  devtools::load_all()

  data(statsplot2_repeated)

  result <- statsplot2(
    data = statsplot2_repeated,
    dep = "symptom_severity",
    group = "timepoint",
    grvar = "treatment_arm",
    direction = "repeated",
    distribution = "p"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles comprehensive parameter combinations", {
  devtools::load_all()

  data(statsplot2_test)

  # Full feature set
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "tumor_stage",
    direction = "independent",
    distribution = "p",
    plotTitle = "Comprehensive Analysis",
    xlab = "Treatment",
    ylab = "Reduction (mm)"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles different continuous outcomes", {
  devtools::load_all()

  data(statsplot2_test)

  outcomes <- c("tumor_reduction", "pain_score", "qol_score",
                "biomarker_level", "age", "bmi")

  for (outcome in outcomes) {
    result <- statsplot2(
      data = statsplot2_test,
      dep = outcome,
      group = "treatment",
      distribution = "p"
    )

    expect_s3_class(result, "statsplot2Results",
                   info = paste("Failed for outcome:", outcome))
  }
})

test_that("statsplot2 handles different categorical outcomes", {
  devtools::load_all()

  data(statsplot2_test)

  # response_status as outcome
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "response_status",
    group = "treatment"
  )
  expect_s3_class(result1, "statsplot2Results")

  # Data from repeated measures
  data(statsplot2_repeated)

  # disease_status as outcome
  result2 <- statsplot2(
    data = statsplot2_repeated,
    dep = "disease_status",
    group = "treatment_arm"
  )
  expect_s3_class(result2, "statsplot2Results")
})

test_that("statsplot2 handles different grouping variables", {
  devtools::load_all()

  data(statsplot2_test)

  groups <- c("treatment", "tumor_stage", "sex", "age_group")

  for (group_var in groups) {
    result <- statsplot2(
      data = statsplot2_test,
      dep = "tumor_reduction",
      group = group_var
    )

    expect_s3_class(result, "statsplot2Results",
                   info = paste("Failed for group:", group_var))
  }
})

test_that("statsplot2 handles NULL optional parameters", {
  devtools::load_all()

  data(statsplot2_test)

  # Explicit NULL for optional parameters
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = NULL,
    plotTitle = NULL,
    xlab = NULL,
    ylab = NULL
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles factor vs character variables", {
  devtools::load_all()

  data(statsplot2_test)

  # Convert to character
  char_data <- statsplot2_test
  char_data$treatment <- as.character(char_data$treatment)

  result_char <- statsplot2(
    data = char_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Keep as factor
  result_factor <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Both should work
  expect_s3_class(result_char, "statsplot2Results")
  expect_s3_class(result_factor, "statsplot2Results")
})

test_that("statsplot2 handles multiple special datasets", {
  devtools::load_all()

  # Clinical trial data
  data(statsplot2_clinical)
  result1 <- statsplot2(
    data = statsplot2_clinical,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "p"
  )
  expect_s3_class(result1, "statsplot2Results")

  # Repeated measures data
  data(statsplot2_repeated)
  result2 <- statsplot2(
    data = statsplot2_repeated,
    dep = "symptom_severity",
    group = "timepoint",
    direction = "repeated"
  )
  expect_s3_class(result2, "statsplot2Results")

  # Outliers data
  data(statsplot2_outliers)
  result3 <- statsplot2(
    data = statsplot2_outliers,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "r"
  )
  expect_s3_class(result3, "statsplot2Results")

  # Skewed data
  data(statsplot2_skewed)
  result4 <- statsplot2(
    data = statsplot2_skewed,
    dep = "tumor_reduction",
    group = "treatment",
    distribution = "np"
  )
  expect_s3_class(result4, "statsplot2Results")
})
