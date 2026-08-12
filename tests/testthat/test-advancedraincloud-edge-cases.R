# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: advancedraincloud
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("advancedraincloud waits for the required variables instead of erroring", {

  data(advancedraincloud_test)

  # A half-filled variable picker is the NORMAL state of a jamovi analysis while
  # the user is still setting it up, so y_var/x_var missing must NOT raise - the
  # contract is the "Welcome to Advanced Raincloud Plots!" instruction panel in
  # the todo output and empty results everywhere else.
  res_no_y <- advancedraincloud(
    data = advancedraincloud_test,
    x_var = "treatment"
  )
  expect_match(res_no_y$todo$content, "Required Variables")
  expect_equal(res_no_y$statistics$content, "")

  res_no_x <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score"
  )
  expect_match(res_no_x$todo$content, "Required Variables")

  # Missing data, however, is a caller error and must still raise.
  expect_error(
    advancedraincloud(
      y_var = "pain_score",
      x_var = "treatment"
    )
  )
})

test_that("advancedraincloud handles missing data correctly", {

  data(advancedraincloud_test)

  # The test data already has ~3% missing values
  # Should complete without error
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles data with all NA in y_var", {

  data(advancedraincloud_test)
  test_data_all_na <- advancedraincloud_test
  test_data_all_na$pain_score <- NA_real_

  # Should error or warn with informative message
  expect_condition(
    advancedraincloud(
      data = test_data_all_na,
      y_var = "pain_score",
      x_var = "treatment"
    )
  )
})

test_that("advancedraincloud handles small sample sizes", {

  data(advancedraincloud_test)

  # Very small dataset (n=10)
  small_data <- advancedraincloud_test[1:10, ]

  result <- advancedraincloud(
    data = small_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  # Should complete (may warn about small N)
  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles single group", {

  data(advancedraincloud_test)

  # Single treatment group
  single_group_data <- subset(advancedraincloud_test, treatment == "Placebo")

  result <- advancedraincloud(
    data = single_group_data,
    y_var = "pain_score",
    x_var = "timepoint"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles constant y variable", {

  data(advancedraincloud_test)
  constant_data <- advancedraincloud_test
  constant_data$constant_var <- 50  # All same value

  # Should warn or error about zero variance
  expect_condition(
    advancedraincloud(
      data = constant_data,
      y_var = "constant_var",
      x_var = "treatment"
    )
  )
})

test_that("advancedraincloud handles variables with special characters", {

  data(advancedraincloud_test)
  special_data <- advancedraincloud_test

  # Rename variables with spaces
  names(special_data)[names(special_data) == "pain_score"] <- "pain score"
  names(special_data)[names(special_data) == "treatment"] <- "treatment group"

  result <- advancedraincloud(
    data = special_data,
    y_var = "pain score",
    x_var = "treatment group"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles extreme values", {

  data(advancedraincloud_test)
  extreme_data <- advancedraincloud_test

  # Add extreme outlier
  extreme_data$pain_score[1] <- 999

  result <- advancedraincloud(
    data = extreme_data,
    y_var = "pain_score",
    x_var = "treatment",
    outlier_method = "none"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles negative values", {

  data(advancedraincloud_test)
  negative_data <- advancedraincloud_test

  # Create variable with negative values
  negative_data$change_score <- negative_data$pain_score - 65

  result <- advancedraincloud(
    data = negative_data,
    y_var = "change_score",
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles zero values", {

  data(advancedraincloud_test)
  zero_data <- advancedraincloud_test
  zero_data$pain_score[1:10] <- 0

  result <- advancedraincloud(
    data = zero_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles factor vs character variables", {

  data(advancedraincloud_test)

  # Convert factor to character
  char_data <- advancedraincloud_test
  char_data$treatment <- as.character(char_data$treatment)

  result_char <- advancedraincloud(
    data = char_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  # Both should work
  result_factor <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment"
  )

  expect_s3_class(result_char, "advancedraincloudResults")
  expect_s3_class(result_factor, "advancedraincloudResults")
})

test_that("advancedraincloud handles unbalanced groups", {

  data(advancedraincloud_test)

  # Create unbalanced data (different group sizes)
  unbalanced_data <- rbind(
    subset(advancedraincloud_test, treatment == "Placebo")[1:10, ],
    subset(advancedraincloud_test, treatment == "Low Dose")[1:50, ],
    subset(advancedraincloud_test, treatment == "High Dose")[1:30, ]
  )

  result <- advancedraincloud(
    data = unbalanced_data,
    y_var = "pain_score",
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles numeric x_var", {

  data(advancedraincloud_test)

  # Convert categorical to numeric
  numeric_x_data <- advancedraincloud_test
  numeric_x_data$treatment_numeric <- as.numeric(numeric_x_data$treatment)

  result <- advancedraincloud(
    data = numeric_x_data,
    y_var = "pain_score",
    x_var = "treatment_numeric"
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles longitudinal ID with missing connections", {

  data(advancedraincloud_test)

  # Some patients missing follow-up
  incomplete_longit_data <- advancedraincloud_test[
    !(advancedraincloud_test$patient_id %in% c(1, 5, 10) &
      advancedraincloud_test$timepoint == "Week 12"),
  ]

  result <- advancedraincloud(
    data = incomplete_longit_data,
    y_var = "pain_score",
    x_var = "timepoint",
    id_var = "patient_id",
    show_longitudinal = TRUE
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles log transform with zero values", {

  data(advancedraincloud_test)
  zero_biomarker_data <- advancedraincloud_test
  zero_biomarker_data$crp_level[1:5] <- 0

  # Should handle zeros in log transform (add constant or warn)
  expect_condition(
    advancedraincloud(
      data = zero_biomarker_data,
      y_var = "crp_level",
      x_var = "treatment",
      log_transform = TRUE
    )
  )
})

test_that("advancedraincloud handles invalid clinical cutoff values", {

  data(advancedraincloud_test)

  # Cutoff outside data range
  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment",
    clinical_cutoff = 999
  )

  # Should still plot (cutoff line may be off-plot)
  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud rejects an invalid MCID instead of drawing it", {

  data(advancedraincloud_test)

  # A minimal clinically important DIFFERENCE is a magnitude; a negative value
  # would draw an inverted band around each group mean and imply a threshold
  # that cannot exist. The option validators used to run inside .init(), whose
  # error handler swallowed jmvcore::reject(), so this silently produced a plot;
  # they now run in .run() and the rejection reaches the user.
  expect_error(
    advancedraincloud(
      data = advancedraincloud_test,
      y_var = "pain_score",
      x_var = "treatment",
      show_mcid = TRUE,
      mcid_value = -10
    ),
    "MCID value must be a positive"
  )

  # ... and it is only enforced when the band is actually requested.
  expect_true(inherits(
    advancedraincloud(
      data = advancedraincloud_test,
      y_var = "pain_score",
      x_var = "treatment",
      show_mcid = FALSE,
      mcid_value = -10
    ),
    "advancedraincloudResults"
  ))
})

test_that("advancedraincloud handles baseline_group not in data", {

  data(advancedraincloud_test)

  # Baseline group that doesn't exist
  expect_condition(
    advancedraincloud(
      data = advancedraincloud_test,
      y_var = "pain_score",
      x_var = "timepoint",
      show_change_scores = TRUE,
      baseline_group = "NonExistentGroup"
    )
  )
})

test_that("advancedraincloud handles empty string labels", {

  data(advancedraincloud_test)

  result <- advancedraincloud(
    data = advancedraincloud_test,
    y_var = "pain_score",
    x_var = "treatment",
    plot_title = "",
    x_label = "",
    y_label = ""
  )

  expect_s3_class(result, "advancedraincloudResults")
})

test_that("advancedraincloud handles very long variable names", {

  data(advancedraincloud_test)
  long_name_data <- advancedraincloud_test

  long_var_name <- paste0(rep("very_", 20), collapse = "")
  long_var_name <- paste0(long_var_name, "long_variable_name")

  names(long_name_data)[names(long_name_data) == "pain_score"] <- long_var_name

  # y_var is captured with jmvcore::enquo(), so a BARE symbol resolves to its own
  # name: passing `y_var = long_var_name` asks for a column literally called
  # "long_var_name". Unquote so the string value is what reaches the option.
  result <- advancedraincloud(
    data = long_name_data,
    y_var = !!long_var_name,
    x_var = "treatment"
  )

  expect_s3_class(result, "advancedraincloudResults")
})
