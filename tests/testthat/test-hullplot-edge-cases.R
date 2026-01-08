# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: hullplot
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("hullplot handles missing data in x variable", {
  devtools::load_all()

  data(hullplot_test)

  test_data_na <- hullplot_test
  test_data_na$x[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    hullplot(
      data = test_data_na,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("hullplot handles missing data in y variable", {
  devtools::load_all()

  data(hullplot_test)

  test_data_na <- hullplot_test
  test_data_na$y[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    hullplot(
      data = test_data_na,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("hullplot handles missing data in group variable", {
  devtools::load_all()

  data(hullplot_test)

  test_data_na <- hullplot_test
  test_data_na$group[1:5] <- NA

  # Should either error or warn about missing data
  expect_warning(
    hullplot(
      data = test_data_na,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("hullplot handles missing data in size variable", {
  devtools::load_all()

  data(hullplot_test)

  test_data_na <- hullplot_test
  test_data_na$size_var[1:5] <- NA

  # Should complete but may warn
  result <- hullplot(
    data = test_data_na,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    size_var = "size_var"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles single point per group", {
  devtools::load_all()

  # Create minimal data with one point per group
  minimal_data <- data.frame(
    x = c(1, 5, 10),
    y = c(2, 6, 11),
    group = c("A", "B", "C")
  )

  # Should either complete or error with informative message
  expect_condition(
    hullplot(
      data = minimal_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles two points per group", {
  devtools::load_all()

  # Create data with two points per group
  two_point_data <- data.frame(
    x = c(1, 2, 5, 6),
    y = c(1, 2, 5, 6),
    group = c("A", "A", "B", "B")
  )

  # Should complete (minimum for hull)
  result <- hullplot(
    data = two_point_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles all points at same location", {
  devtools::load_all()

  # All points identical
  identical_data <- data.frame(
    x = rep(5, 20),
    y = rep(5, 20),
    group = sample(c("A", "B"), 20, replace = TRUE)
  )

  # Should error with informative message
  expect_error(
    hullplot(
      data = identical_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "variance|constant|identical|variation",
    ignore.case = TRUE
  )
})

test_that("hullplot handles collinear points", {
  devtools::load_all()

  # Points on a line
  collinear_data <- data.frame(
    x = 1:20,
    y = 1:20,
    group = sample(c("A", "B"), 20, replace = TRUE)
  )

  # Should complete (line can have hull)
  result <- hullplot(
    data = collinear_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles extreme outliers", {
  devtools::load_all()

  data(hullplot_test)

  outlier_data <- hullplot_test
  outlier_data$x[1] <- 1000
  outlier_data$y[1] <- 1000

  # Should complete
  result <- hullplot(
    data = outlier_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles Inf values in x", {
  devtools::load_all()

  data(hullplot_test)

  inf_data <- hullplot_test
  inf_data$x[1] <- Inf

  # Should either error or warn
  expect_condition(
    hullplot(
      data = inf_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles Inf values in y", {
  devtools::load_all()

  data(hullplot_test)

  inf_data <- hullplot_test
  inf_data$y[1] <- Inf

  # Should either error or warn
  expect_condition(
    hullplot(
      data = inf_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles -Inf values", {
  devtools::load_all()

  data(hullplot_test)

  inf_data <- hullplot_test
  inf_data$x[1] <- -Inf
  inf_data$y[2] <- -Inf

  # Should either error or warn
  expect_condition(
    hullplot(
      data = inf_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles NaN values", {
  devtools::load_all()

  data(hullplot_test)

  nan_data <- hullplot_test
  nan_data$x[1] <- NaN

  # Should either error or warn
  expect_condition(
    hullplot(
      data = nan_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles very small sample size", {
  devtools::load_all()

  # Only 6 observations total
  tiny_data <- data.frame(
    x = c(1, 2, 5, 6, 10, 11),
    y = c(1, 2, 5, 6, 10, 11),
    group = c("A", "A", "B", "B", "C", "C")
  )

  result <- hullplot(
    data = tiny_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles single group", {
  devtools::load_all()

  data(hullplot_test)

  single_group_data <- hullplot_test[hullplot_test$group == "Group A", ]

  # Should error with informative message
  expect_error(
    hullplot(
      data = single_group_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "one group|single group|multiple groups",
    ignore.case = TRUE
  )
})

test_that("hullplot handles many groups", {
  devtools::load_all()

  # Create data with 10 groups
  many_groups_data <- data.frame(
    x = rnorm(200),
    y = rnorm(200),
    group = sample(LETTERS[1:10], 200, replace = TRUE)
  )

  result <- hullplot(
    data = many_groups_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles variable names with spaces", {
  devtools::load_all()

  data(hullplot_test)

  space_data <- hullplot_test
  names(space_data)[names(space_data) == "x"] <- "x variable"
  names(space_data)[names(space_data) == "y"] <- "y variable"
  names(space_data)[names(space_data) == "group"] <- "group variable"

  result <- hullplot(
    data = space_data,
    x_var = "x variable",
    y_var = "y variable",
    group_var = "group variable"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles variable names with special characters", {
  devtools::load_all()

  data(hullplot_test)

  special_data <- hullplot_test
  names(special_data)[names(special_data) == "x"] <- "x-coordinate"
  names(special_data)[names(special_data) == "y"] <- "y_coordinate"

  result <- hullplot(
    data = special_data,
    x_var = "x-coordinate",
    y_var = "y_coordinate",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles very long group names", {
  devtools::load_all()

  data(hullplot_test)

  long_name_data <- hullplot_test
  long_name_data$group <- ifelse(
    long_name_data$group == "Group A",
    "This is a very long group name that might cause display issues",
    "Another extremely long group name for testing purposes"
  )

  result <- hullplot(
    data = long_name_data,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_labels = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles groups with different sample sizes (extreme imbalance)", {
  devtools::load_all()

  # Very unbalanced: 100, 5, 3
  extreme_unbalanced <- data.frame(
    x = rnorm(108),
    y = rnorm(108),
    group = c(rep("Large", 100), rep("Small", 5), rep("Tiny", 3))
  )

  result <- hullplot(
    data = extreme_unbalanced,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles negative values", {
  devtools::load_all()

  # All negative values
  negative_data <- data.frame(
    x = rnorm(60, mean = -50, sd = 10),
    y = rnorm(60, mean = -30, sd = 10),
    group = sample(c("A", "B"), 60, replace = TRUE)
  )

  result <- hullplot(
    data = negative_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles mixed positive and negative values", {
  devtools::load_all()

  # Centered around zero
  mixed_data <- data.frame(
    x = rnorm(60, mean = 0, sd = 20),
    y = rnorm(60, mean = 0, sd = 20),
    group = sample(c("A", "B"), 60, replace = TRUE)
  )

  result <- hullplot(
    data = mixed_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles very large values", {
  devtools::load_all()

  # Very large scale
  large_data <- data.frame(
    x = rnorm(60, mean = 1e6, sd = 1e5),
    y = rnorm(60, mean = 1e6, sd = 1e5),
    group = sample(c("A", "B"), 60, replace = TRUE)
  )

  result <- hullplot(
    data = large_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles very small values", {
  devtools::load_all()

  # Very small scale
  small_data <- data.frame(
    x = rnorm(60, mean = 1e-6, sd = 1e-7),
    y = rnorm(60, mean = 1e-6, sd = 1e-7),
    group = sample(c("A", "B"), 60, replace = TRUE)
  )

  result <- hullplot(
    data = small_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles extreme aspect ratios", {
  devtools::load_all()

  # Very wide range in x, narrow in y
  aspect_data <- data.frame(
    x = rnorm(60, mean = 50, sd = 50),
    y = rnorm(60, mean = 5, sd = 0.1),
    group = sample(c("A", "B"), 60, replace = TRUE)
  )

  result <- hullplot(
    data = aspect_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles constant x variable", {
  devtools::load_all()

  data(hullplot_test)

  const_x_data <- hullplot_test
  const_x_data$x <- 5

  # Should error with informative message
  expect_error(
    hullplot(
      data = const_x_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "constant|variance|variation",
    ignore.case = TRUE
  )
})

test_that("hullplot handles constant y variable", {
  devtools::load_all()

  data(hullplot_test)

  const_y_data <- hullplot_test
  const_y_data$y <- 5

  # Should error with informative message
  expect_error(
    hullplot(
      data = const_y_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "constant|variance|variation",
    ignore.case = TRUE
  )
})

test_that("hullplot handles constant size variable", {
  devtools::load_all()

  data(hullplot_test)

  const_size_data <- hullplot_test
  const_size_data$size_var <- 10

  # Should complete (constant size is valid)
  result <- hullplot(
    data = const_size_data,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    size_var = "size_var"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles zero variance within groups", {
  devtools::load_all()

  # All points within each group at same location
  zero_var_data <- data.frame(
    x = c(rep(1, 10), rep(5, 10)),
    y = c(rep(1, 10), rep(5, 10)),
    group = c(rep("A", 10), rep("B", 10))
  )

  # Should error or warn
  expect_condition(
    hullplot(
      data = zero_var_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles empty groups after removing missing data", {
  devtools::load_all()

  data(hullplot_test)

  # Make one group entirely NA
  empty_group_data <- hullplot_test
  empty_group_data$x[empty_group_data$group == "Group A"] <- NA

  # Should either error or warn
  expect_condition(
    hullplot(
      data = empty_group_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  )
})

test_that("hullplot handles tibble vs data.frame", {
  devtools::load_all()

  data(hullplot_test)

  # As tibble
  tibble_data <- tibble::as_tibble(hullplot_test)

  result_tibble <- hullplot(
    data = tibble_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )
  expect_s3_class(result_tibble, "hullplotResults")

  # As data.frame
  df_data <- as.data.frame(hullplot_test)

  result_df <- hullplot(
    data = df_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )
  expect_s3_class(result_df, "hullplotResults")
})

test_that("hullplot handles invalid concavity values", {
  devtools::load_all()

  data(hullplot_test)

  # Negative concavity
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_concavity = -1
    ),
    regexp = "concavity|range|invalid|between",
    ignore.case = TRUE
  )

  # Concavity > 2
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_concavity = 3
    ),
    regexp = "concavity|range|invalid|between",
    ignore.case = TRUE
  )
})

test_that("hullplot handles invalid alpha values", {
  devtools::load_all()

  data(hullplot_test)

  # Negative alpha
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_alpha = -0.1
    ),
    regexp = "alpha|transparency|range|invalid|between",
    ignore.case = TRUE
  )

  # Alpha > 1
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_alpha = 1.5
    ),
    regexp = "alpha|transparency|range|invalid|between",
    ignore.case = TRUE
  )
})

test_that("hullplot handles invalid expand values", {
  devtools::load_all()

  data(hullplot_test)

  # Negative expand
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_expand = -0.1
    ),
    regexp = "expand|range|invalid|negative",
    ignore.case = TRUE
  )
})

test_that("hullplot handles non-existent variables", {
  devtools::load_all()

  data(hullplot_test)

  # Non-existent x variable
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "nonexistent_x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "variable|column|not found|does not exist",
    ignore.case = TRUE
  )

  # Non-existent group variable
  expect_error(
    hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "nonexistent_group"
    ),
    regexp = "variable|column|not found|does not exist",
    ignore.case = TRUE
  )
})

test_that("hullplot handles empty dataset", {
  devtools::load_all()

  empty_data <- data.frame(
    x = numeric(0),
    y = numeric(0),
    group = character(0)
  )

  # Should error with informative message
  expect_error(
    hullplot(
      data = empty_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "empty|no data|no observations",
    ignore.case = TRUE
  )
})

test_that("hullplot handles numeric group variable", {
  devtools::load_all()

  data(hullplot_test)

  numeric_group_data <- hullplot_test
  numeric_group_data$group <- ifelse(numeric_group_data$group == "Group A", 1, 2)

  # Should convert to factor/character and complete
  result <- hullplot(
    data = numeric_group_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles all missing data after filtering", {
  devtools::load_all()

  data(hullplot_test)

  all_na_data <- hullplot_test
  all_na_data$x <- NA
  all_na_data$y <- NA

  # Should error with informative message
  expect_error(
    hullplot(
      data = all_na_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    ),
    regexp = "missing|NA|no valid|insufficient data",
    ignore.case = TRUE
  )
})
