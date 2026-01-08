# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: statsplot2
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("statsplot2 errors on missing required arguments", {
  devtools::load_all()

  data(statsplot2_test)

  # Missing dep
  expect_error(
    statsplot2(
      data = statsplot2_test,
      group = "treatment"
    )
  )

  # Missing group
  expect_error(
    statsplot2(
      data = statsplot2_test,
      dep = "tumor_reduction"
    )
  )

  # Missing data
  expect_error(
    statsplot2(
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles missing data correctly", {
  devtools::load_all()

  data(statsplot2_test)
  test_data_na <- statsplot2_test
  test_data_na$tumor_reduction[1:10] <- NA

  # Should handle NA values (either drop or warn)
  expect_condition(
    statsplot2(
      data = test_data_na,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles all NA in dependent variable", {
  devtools::load_all()

  data(statsplot2_test)
  test_data_all_na <- statsplot2_test
  test_data_all_na$tumor_reduction <- NA_real_

  # Should error with informative message
  expect_error(
    statsplot2(
      data = test_data_all_na,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles missing grouping variable values", {
  devtools::load_all()

  data(statsplot2_test)
  test_data_na_group <- statsplot2_test
  test_data_na_group$treatment[1:5] <- NA

  # Should handle NA groups (drop or warn)
  expect_condition(
    statsplot2(
      data = test_data_na_group,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles small sample sizes", {
  devtools::load_all()

  data(statsplot2_test)
  small_data <- statsplot2_test[1:15, ]

  result <- statsplot2(
    data = small_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Should complete but may warn
  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles very small sample sizes", {
  devtools::load_all()

  data(statsplot2_test)
  tiny_data <- statsplot2_test[1:6, ]

  # Should either complete or error with informative message
  expect_condition(
    statsplot2(
      data = tiny_data,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles single group", {
  devtools::load_all()

  data(statsplot2_test)
  single_group <- subset(statsplot2_test, treatment == "Placebo")

  # Should error as cannot compare groups
  expect_error(
    statsplot2(
      data = single_group,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles constant dependent variable", {
  devtools::load_all()

  data(statsplot2_test)
  const_data <- statsplot2_test
  const_data$tumor_reduction <- 50

  # Should error or warn about no variance
  expect_condition(
    statsplot2(
      data = const_data,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles constant grouping variable", {
  devtools::load_all()

  data(statsplot2_test)
  const_group <- statsplot2_test
  const_group$treatment <- "Placebo"

  # Should error as only one group
  expect_error(
    statsplot2(
      data = const_group,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles variables with special characters", {
  devtools::load_all()

  data(statsplot2_test)
  special_data <- statsplot2_test
  names(special_data)[names(special_data) == "tumor_reduction"] <- "tumor reduction (mm)"

  result <- statsplot2(
    data = special_data,
    dep = "tumor reduction (mm)",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles very long variable names", {
  devtools::load_all()

  data(statsplot2_test)
  long_name_data <- statsplot2_test
  names(long_name_data)[names(long_name_data) == "tumor_reduction"] <-
    "VeryLongVariableName_TumorSizeReduction_MeasuredInMillimeters_AtWeek12"

  result <- statsplot2(
    data = long_name_data,
    dep = "VeryLongVariableName_TumorSizeReduction_MeasuredInMillimeters_AtWeek12",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles extreme values", {
  devtools::load_all()

  data(statsplot2_test)
  extreme_data <- statsplot2_test
  extreme_data$tumor_reduction[1] <- 10000

  result <- statsplot2(
    data = extreme_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles negative values", {
  devtools::load_all()

  data(statsplot2_test)
  negative_data <- statsplot2_test
  negative_data$tumor_reduction <- negative_data$tumor_reduction - 50

  result <- statsplot2(
    data = negative_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles zero values", {
  devtools::load_all()

  data(statsplot2_test)
  zero_data <- statsplot2_test
  zero_data$tumor_reduction[1:10] <- 0

  result <- statsplot2(
    data = zero_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles unbalanced groups", {
  devtools::load_all()

  data(statsplot2_test)

  # Create highly unbalanced groups (90% in one group)
  unbalanced_data <- statsplot2_test[1:90, ]
  unbalanced_data$treatment <- c(rep("Placebo", 85), rep("High Dose", 5))

  result <- statsplot2(
    data = unbalanced_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles integer vs numeric variables", {
  devtools::load_all()

  data(statsplot2_test)

  # Integer outcome
  result1 <- statsplot2(
    data = statsplot2_test,
    dep = "age",
    group = "treatment"
  )
  expect_s3_class(result1, "statsplot2Results")

  # Numeric outcome
  result2 <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )
  expect_s3_class(result2, "statsplot2Results")
})

test_that("statsplot2 handles empty plot title", {
  devtools::load_all()

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    plotTitle = ""
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles very long plot title", {
  devtools::load_all()

  data(statsplot2_test)

  long_title <- paste(rep("Very Long Title", 30), collapse = " ")

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment",
    plotTitle = long_title
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles factor levels in different orders", {
  devtools::load_all()

  data(statsplot2_test)

  # Reorder factor levels
  reordered_data <- statsplot2_test
  reordered_data$treatment <- factor(
    reordered_data$treatment,
    levels = c("High Dose", "Low Dose", "Placebo")
  )

  result <- statsplot2(
    data = reordered_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles unused factor levels", {
  devtools::load_all()

  data(statsplot2_test)

  # Add unused factor level
  unused_level_data <- statsplot2_test
  unused_level_data$treatment <- factor(
    unused_level_data$treatment,
    levels = c(levels(unused_level_data$treatment), "Ultra High Dose")
  )

  result <- statsplot2(
    data = unused_level_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles numeric grouping variable as continuous", {
  devtools::load_all()

  data(statsplot2_test)

  # Use numeric variable as group (should create scatter plot)
  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "age"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles repeated measures with incomplete IDs", {
  devtools::load_all()

  data(statsplot2_repeated)

  # Remove some observations to create incomplete trajectories
  incomplete_data <- statsplot2_repeated[-c(1, 50, 100), ]

  result <- statsplot2(
    data = incomplete_data,
    dep = "symptom_severity",
    group = "timepoint",
    direction = "repeated"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles missing split-by variable values", {
  devtools::load_all()

  data(statsplot2_test)
  test_data_na_split <- statsplot2_test
  test_data_na_split$sex[1:5] <- NA

  # Should handle NA in split variable
  result <- statsplot2(
    data = test_data_na_split,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "sex"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles single level in split variable", {
  devtools::load_all()

  data(statsplot2_test)
  single_split <- statsplot2_test
  single_split$sex <- "Male"

  result <- statsplot2(
    data = single_split,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "sex"
  )

  expect_s3_class(result, "statsplot2Results")
})
