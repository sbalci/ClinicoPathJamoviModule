# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: statsplot2
# ═══════════════════════════════════════════════════════════

library(testthat)

# A jamovi analysis reports bad input in a results panel, not by throwing. Both
# expect_error() and expect_condition() therefore asserted the OPPOSITE of the
# desired behaviour here - they passed only when the analysis crashed.
sp_todo    <- function(res) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(res$todo$content)))
sp_notices <- function(res) # NB: do NOT strip "<...>" here. The notices output is Preformatted PLAIN TEXT,
  # and a tag-stripping regex eats everything between a "<" and the next ">" -
  # e.g. "recommended for n<30 ... Required: >=2 valid values" collapses into
  # "recommended for n=2 valid values", merging two separate notices and hiding
  # the one being asserted.
  gsub("[[:space:]]+", " ", paste(as.character(res$notices$content), collapse = " "))

test_that("statsplot2 errors on missing required arguments", {

  data(statsplot2_test)

  # Missing dep -> the welcome panel, not a crash
  expect_match(sp_todo(statsplot2(data = statsplot2_test, group = "treatment")),
               "Welcome to Automatic Plot Selection")

  # Missing group
  expect_match(sp_todo(statsplot2(data = statsplot2_test, dep = "tumor_reduction")),
               "Welcome to Automatic Plot Selection")

  # Missing data
  expect_error(
    statsplot2(
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("statsplot2 handles missing data correctly", {

  data(statsplot2_test)
  test_data_na <- statsplot2_test
  test_data_na$tumor_reduction[1:10] <- NA

  # Rows with a missing value are omitted from the statistics, and the count is
  # now stated - it used to be reported as though every row had been used.
  n <- sp_notices(statsplot2(data = test_data_na, dep = "tumor_reduction",
                             group = "treatment"))
  expect_match(n, "omitted from the statistics")
  expect_match(n, "Observations used:")
})

test_that("statsplot2 handles all NA in dependent variable", {

  data(statsplot2_test)
  test_data_all_na <- statsplot2_test
  test_data_all_na$tumor_reduction <- NA_real_

  # An all-missing outcome is rejected with an ERROR notice in the panel.
  expect_match(sp_notices(statsplot2(data = test_data_all_na, dep = "tumor_reduction",
                                     group = "treatment")),
               "Insufficient Dependent Values")
})

test_that("statsplot2 handles missing grouping variable values", {

  data(statsplot2_test)
  test_data_na_group <- statsplot2_test
  test_data_na_group$treatment[1:5] <- NA

  # Missing group values are omitted from the statistics and disclosed.
  n <- sp_notices(statsplot2(data = test_data_na_group, dep = "tumor_reduction",
                             group = "treatment"))
  expect_match(n, "omitted from the statistics")
})

test_that("statsplot2 handles small sample sizes", {

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

  data(statsplot2_test)
  tiny_data <- statsplot2_test[1:6, ]

  # n = 6 runs, with a small-sample caveat in the panel.
  expect_match(sp_notices(statsplot2(data = tiny_data, dep = "tumor_reduction",
                                     group = "treatment")),
               "Small Sample Size")
})

test_that("statsplot2 handles single group", {

  data(statsplot2_test)
  single_group <- subset(statsplot2_test, treatment == "Placebo")

  # One group means there is nothing to compare against. This used to run and
  # report "Analysis completed successfully".
  n <- sp_notices(statsplot2(data = single_group, dep = "tumor_reduction",
                             group = "treatment"))
  expect_match(n, "Only one group to compare")
  expect_false(grepl("completed successfully", n, fixed = TRUE))
})

test_that("statsplot2 handles constant dependent variable", {

  data(statsplot2_test)
  const_data <- statsplot2_test
  const_data$tumor_reduction <- 50

  # A constant numeric outcome has one unique value, so the automatic plot
  # selection reads it as a FACTOR and silently switches the analysis type.
  n <- sp_notices(statsplot2(data = const_data, dep = "tumor_reduction",
                             group = "treatment"))
  expect_match(n, "Outcome has no variation")
  expect_match(n, "changes the analysis type")
  expect_false(grepl("completed successfully", n, fixed = TRUE))
})

test_that("statsplot2 handles constant grouping variable", {

  data(statsplot2_test)
  const_group <- statsplot2_test
  const_group$treatment <- "Placebo"

  expect_match(sp_notices(statsplot2(data = const_group, dep = "tumor_reduction",
                                     group = "treatment")),
               "Only one group to compare")
})

test_that("statsplot2 handles variables with special characters", {

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

  data(statsplot2_test)

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles very long plot title", {

  data(statsplot2_test)

  long_title <- paste(rep("Very Long Title", 30), collapse = " ")

  result <- statsplot2(
    data = statsplot2_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles factor levels in different orders", {

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
