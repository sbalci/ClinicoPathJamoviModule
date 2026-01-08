# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: linechart
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("linechart handles missing data in x variable", {
  devtools::load_all()

  data(linechart_simple)

  test_data_na <- linechart_simple
  test_data_na$time_point[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    linechart(
      data = test_data_na,
      xvar = "time_point",
      yvar = "value"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("linechart handles missing data in y variable", {
  devtools::load_all()

  data(linechart_simple)

  test_data_na <- linechart_simple
  test_data_na$value[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    linechart(
      data = test_data_na,
      xvar = "time_point",
      yvar = "value"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("linechart handles missing data in group variable", {
  devtools::load_all()

  data(linechart_grouped)

  test_data_na <- linechart_grouped
  test_data_na$treatment[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    linechart(
      data = test_data_na,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("linechart handles two time points", {
  devtools::load_all()

  # Minimal time series
  minimal_data <- data.frame(
    time = c(1, 2),
    value = c(10, 15)
  )

  result <- linechart(
    data = minimal_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles single time point", {
  devtools::load_all()

  # Only one time point
  single_point <- data.frame(
    time = 1,
    value = 10
  )

  # Should error with informative message
  expect_error(
    linechart(
      data = single_point,
      xvar = "time",
      yvar = "value"
    ),
    regexp = "time points|observations|insufficient",
    ignore.case = TRUE
  )
})

test_that("linechart handles constant y values", {
  devtools::load_all()

  # All y values the same
  const_data <- data.frame(
    time = 1:20,
    value = rep(10, 20)
  )

  result <- linechart(
    data = const_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles Inf values in y", {
  devtools::load_all()

  data(linechart_simple)

  inf_data <- linechart_simple
  inf_data$value[1] <- Inf

  # Should either error or warn
  expect_condition(
    linechart(
      data = inf_data,
      xvar = "time_point",
      yvar = "value"
    )
  )
})

test_that("linechart handles -Inf values", {
  devtools::load_all()

  data(linechart_simple)

  inf_data <- linechart_simple
  inf_data$value[1] <- -Inf

  # Should either error or warn
  expect_condition(
    linechart(
      data = inf_data,
      xvar = "time_point",
      yvar = "value"
    )
  )
})

test_that("linechart handles NaN values", {
  devtools::load_all()

  data(linechart_simple)

  nan_data <- linechart_simple
  nan_data$value[1] <- NaN

  # Should either error or warn
  expect_condition(
    linechart(
      data = nan_data,
      xvar = "time_point",
      yvar = "value"
    )
  )
})

test_that("linechart handles very small sample size", {
  devtools::load_all()

  # Only 3 time points
  tiny_data <- data.frame(
    time = 1:3,
    value = c(10, 15, 12)
  )

  result <- linechart(
    data = tiny_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles single group", {
  devtools::load_all()

  data(linechart_grouped)

  single_group_data <- linechart_grouped[linechart_grouped$treatment == "Control", ]

  # Should complete (effectively ungrouped)
  result <- linechart(
    data = single_group_data,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles many groups", {
  devtools::load_all()

  # Create data with 8 groups
  many_groups_data <- data.frame(
    time = rep(1:20, 8),
    value = rnorm(160) + rep(1:8, each = 20),
    group = rep(LETTERS[1:8], each = 20)
  )

  result <- linechart(
    data = many_groups_data,
    xvar = "time",
    yvar = "value",
    groupby = "group"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles variable names with spaces", {
  devtools::load_all()

  data(linechart_simple)

  space_data <- linechart_simple
  names(space_data)[names(space_data) == "time_point"] <- "time point"
  names(space_data)[names(space_data) == "value"] <- "measured value"

  result <- linechart(
    data = space_data,
    xvar = "time point",
    yvar = "measured value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles variable names with special characters", {
  devtools::load_all()

  data(linechart_simple)

  special_data <- linechart_simple
  names(special_data)[names(special_data) == "time_point"] <- "time-point"
  names(special_data)[names(special_data) == "value"] <- "value_1"

  result <- linechart(
    data = special_data,
    xvar = "time-point",
    yvar = "value_1"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles very long group names", {
  devtools::load_all()

  data(linechart_grouped)

  long_name_data <- linechart_grouped
  long_name_data$treatment <- ifelse(
    long_name_data$treatment == "Control",
    "This is a very long treatment name that might cause display issues",
    ifelse(
      long_name_data$treatment == "Treatment A",
      "Another extremely long treatment group name for testing purposes",
      "Yet another very long treatment group name"
    )
  )

  result <- linechart(
    data = long_name_data,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles extreme y values (very large)", {
  devtools::load_all()

  # Very large scale
  large_data <- data.frame(
    time = 1:20,
    value = rnorm(20, mean = 1e6, sd = 1e5)
  )

  result <- linechart(
    data = large_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles extreme y values (very small)", {
  devtools::load_all()

  # Very small scale
  small_data <- data.frame(
    time = 1:20,
    value = rnorm(20, mean = 1e-6, sd = 1e-7)
  )

  result <- linechart(
    data = small_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles negative y values", {
  devtools::load_all()

  # All negative values
  negative_data <- data.frame(
    time = 1:20,
    value = rnorm(20, mean = -50, sd = 10)
  )

  result <- linechart(
    data = negative_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles mixed positive and negative y values", {
  devtools::load_all()

  # Centered around zero
  mixed_data <- data.frame(
    time = 1:20,
    value = rnorm(20, mean = 0, sd = 20)
  )

  result <- linechart(
    data = mixed_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles non-sequential x values", {
  devtools::load_all()

  # X values with gaps
  gap_data <- data.frame(
    time = c(1, 2, 5, 6, 10, 15, 20),
    value = rnorm(7)
  )

  result <- linechart(
    data = gap_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles duplicate x values", {
  devtools::load_all()

  # Multiple measurements at same time point
  duplicate_data <- data.frame(
    time = c(1, 1, 2, 2, 3, 3),
    value = rnorm(6)
  )

  result <- linechart(
    data = duplicate_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles unequal group sizes", {
  devtools::load_all()

  # Very unbalanced groups
  unbalanced_data <- data.frame(
    time = c(rep(1:20, 1), rep(1:20, 1), rep(1:10, 1)),
    value = rnorm(50),
    group = c(rep("A", 20), rep("B", 20), rep("C", 10))
  )

  result <- linechart(
    data = unbalanced_data,
    xvar = "time",
    yvar = "value",
    groupby = "group"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles empty groups after filtering", {
  devtools::load_all()

  data(linechart_grouped)

  # Make one group entirely NA
  empty_group_data <- linechart_grouped
  empty_group_data$lab_value[empty_group_data$treatment == "Control"] <- NA

  # Should either error or warn
  expect_condition(
    linechart(
      data = empty_group_data,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment"
    )
  )
})

test_that("linechart handles tibble vs data.frame", {
  devtools::load_all()

  data(linechart_simple)

  # As tibble
  tibble_data <- tibble::as_tibble(linechart_simple)

  result_tibble <- linechart(
    data = tibble_data,
    xvar = "time_point",
    yvar = "value"
  )
  expect_s3_class(result_tibble, "linechartResults")

  # As data.frame
  df_data <- as.data.frame(linechart_simple)

  result_df <- linechart(
    data = df_data,
    xvar = "time_point",
    yvar = "value"
  )
  expect_s3_class(result_df, "linechartResults")
})

test_that("linechart handles invalid dimension values", {
  devtools::load_all()

  data(linechart_simple)

  # Width too small
  expect_error(
    linechart(
      data = linechart_simple,
      xvar = "time_point",
      yvar = "value",
      width = 200
    ),
    regexp = "width|range|invalid|between",
    ignore.case = TRUE
  )

  # Width too large
  expect_error(
    linechart(
      data = linechart_simple,
      xvar = "time_point",
      yvar = "value",
      width = 1500
    ),
    regexp = "width|range|invalid|between",
    ignore.case = TRUE
  )
})

test_that("linechart handles non-existent variables", {
  devtools::load_all()

  data(linechart_simple)

  # Non-existent x variable
  expect_error(
    linechart(
      data = linechart_simple,
      xvar = "nonexistent_x",
      yvar = "value"
    ),
    regexp = "variable|column|not found|does not exist",
    ignore.case = TRUE
  )

  # Non-existent y variable
  expect_error(
    linechart(
      data = linechart_simple,
      xvar = "time_point",
      yvar = "nonexistent_y"
    ),
    regexp = "variable|column|not found|does not exist",
    ignore.case = TRUE
  )

  # Non-existent group variable
  expect_error(
    linechart(
      data = linechart_simple,
      xvar = "time_point",
      yvar = "value",
      groupby = "nonexistent_group"
    ),
    regexp = "variable|column|not found|does not exist",
    ignore.case = TRUE
  )
})

test_that("linechart handles empty dataset", {
  devtools::load_all()

  empty_data <- data.frame(
    time = numeric(0),
    value = numeric(0)
  )

  # Should error with informative message
  expect_error(
    linechart(
      data = empty_data,
      xvar = "time",
      yvar = "value"
    ),
    regexp = "empty|no data|no observations",
    ignore.case = TRUE
  )
})

test_that("linechart handles all missing data after filtering", {
  devtools::load_all()

  data(linechart_simple)

  all_na_data <- linechart_simple
  all_na_data$value <- NA

  # Should error with informative message
  expect_error(
    linechart(
      data = all_na_data,
      xvar = "time_point",
      yvar = "value"
    ),
    regexp = "missing|NA|no valid|insufficient data",
    ignore.case = TRUE
  )
})

test_that("linechart handles character x variable", {
  devtools::load_all()

  data(linechart_simple)

  char_x_data <- linechart_simple
  char_x_data$time_point <- as.character(char_x_data$time_point)

  # Should convert to numeric or error
  result <- linechart(
    data = char_x_data,
    xvar = "time_point",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles factor x variable", {
  devtools::load_all()

  # Ordered factor x variable
  factor_data <- data.frame(
    time = factor(c("Baseline", "Month 1", "Month 3", "Month 6"),
                 levels = c("Baseline", "Month 1", "Month 3", "Month 6"),
                 ordered = TRUE),
    value = c(100, 95, 85, 80)
  )

  result <- linechart(
    data = factor_data,
    xvar = "time",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles extreme outliers in grouped data", {
  devtools::load_all()

  data(linechart_grouped)

  outlier_data <- linechart_grouped
  outlier_data$lab_value[1] <- 10000

  result <- linechart(
    data = outlier_data,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles zero variance within groups", {
  devtools::load_all()

  # Each group has constant value
  zero_var_data <- data.frame(
    time = rep(1:10, 2),
    value = c(rep(50, 10), rep(100, 10)),
    group = rep(c("A", "B"), each = 10)
  )

  result <- linechart(
    data = zero_var_data,
    xvar = "time",
    yvar = "value",
    groupby = "group",
    confidence = TRUE
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles numeric group variable", {
  devtools::load_all()

  data(linechart_grouped)

  numeric_group_data <- linechart_grouped
  numeric_group_data$treatment <- as.numeric(as.factor(numeric_group_data$treatment))

  # Should convert to factor and complete
  result <- linechart(
    data = numeric_group_data,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles very high variance in y", {
  devtools::load_all()

  # Extreme variance
  high_var_data <- data.frame(
    time = 1:20,
    value = rnorm(20, mean = 100, sd = 100)
  )

  result <- linechart(
    data = high_var_data,
    xvar = "time",
    yvar = "value",
    confidence = TRUE
  )

  expect_s3_class(result, "linechartResults")
})
