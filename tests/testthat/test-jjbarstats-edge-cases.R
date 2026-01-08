# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjbarstats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjbarstats errors on missing required arguments", {
  devtools::load_all()

  data(jjbarstats_test)

  # Missing dep
  expect_error(
    jjbarstats(
      data = jjbarstats_test,
      group = "treatment"
    )
  )

  # Missing group
  expect_error(
    jjbarstats(
      data = jjbarstats_test,
      dep = "response"
    )
  )

  # Missing data
  expect_error(
    jjbarstats(
      dep = "response",
      group = "treatment"
    )
  )
})

test_that("jjbarstats handles missing data correctly", {
  devtools::load_all()

  data(jjbarstats_test)
  test_data_na <- jjbarstats_test
  test_data_na$response[1:10] <- NA

  # Should handle NA values with excl option
  result <- jjbarstats(
    data = test_data_na,
    dep = "response",
    group = "treatment",
    excl = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles all NA in dependent variable", {
  devtools::load_all()

  data(jjbarstats_test)
  test_data_all_na <- jjbarstats_test
  test_data_all_na$response <- NA

  # Should error with informative message
  expect_error(
    jjbarstats(
      data = test_data_all_na,
      dep = "response",
      group = "treatment"
    )
  )
})

test_that("jjbarstats handles missing grouping variable values", {
  devtools::load_all()

  data(jjbarstats_test)
  test_data_na_group <- jjbarstats_test
  test_data_na_group$treatment[1:5] <- NA

  # Should handle NA groups
  result <- jjbarstats(
    data = test_data_na_group,
    dep = "response",
    group = "treatment",
    excl = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles small sample sizes", {
  devtools::load_all()

  data(jjbarstats_test)
  small_data <- jjbarstats_test[1:30, ]

  result <- jjbarstats(
    data = small_data,
    dep = "response",
    group = "treatment"
  )

  # Should complete, possibly with Fisher's exact test
  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles very small sample sizes", {
  devtools::load_all()

  data(jjbarstats_test)
  tiny_data <- jjbarstats_test[1:10, ]

  # Should complete or warn about small sample
  expect_condition(
    jjbarstats(
      data = tiny_data,
      dep = "response",
      group = "treatment"
    )
  )
})

test_that("jjbarstats handles single level in dependent variable", {
  devtools::load_all()

  data(jjbarstats_test)
  single_dep <- jjbarstats_test
  single_dep$response <- "No Response"

  # Should error as no variation
  expect_error(
    jjbarstats(
      data = single_dep,
      dep = "response",
      group = "treatment"
    )
  )
})

test_that("jjbarstats handles single level in grouping variable", {
  devtools::load_all()

  data(jjbarstats_test)
  single_group <- jjbarstats_test
  single_group$treatment <- "Placebo"

  # Should error as cannot compare groups
  expect_error(
    jjbarstats(
      data = single_group,
      dep = "response",
      group = "treatment"
    )
  )
})

test_that("jjbarstats handles variables with special characters", {
  devtools::load_all()

  data(jjbarstats_test)
  special_data <- jjbarstats_test
  names(special_data)[names(special_data) == "response"] <- "response status (%))"

  result <- jjbarstats(
    data = special_data,
    dep = "response status (%))",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles very long variable names", {
  devtools::load_all()

  data(jjbarstats_test)
  long_name_data <- jjbarstats_test
  names(long_name_data)[names(long_name_data) == "response"] <-
    "VeryLongVariableName_TumorResponseStatus_AssessedByRECIST_v1.1_Criteria"

  result <- jjbarstats(
    data = long_name_data,
    dep = "VeryLongVariableName_TumorResponseStatus_AssessedByRECIST_v1.1_Criteria",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles unbalanced contingency tables", {
  devtools::load_all()

  data(jjbarstats_test)

  # Create highly unbalanced table (90% in one cell)
  unbalanced_data <- jjbarstats_test[1:100, ]
  unbalanced_data$response <- c(rep("No Response", 90), rep("Complete Response", 10))
  unbalanced_data$treatment <- c(rep("Placebo", 95), rep("High Dose", 5))

  result <- jjbarstats(
    data = unbalanced_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles sparse contingency tables", {
  devtools::load_all()

  # Create sparse 3×3 table with many zero cells
  sparse_data <- data.frame(
    response = c(rep("No Response", 25), rep("Partial Response", 5), rep("Complete Response", 5)),
    treatment = c(rep("Placebo", 20), rep("Low Dose", 10), rep("High Dose", 5))
  )

  result <- jjbarstats(
    data = sparse_data,
    dep = "response",
    group = "treatment",
    typestatistics = "nonparametric"  # Fisher's exact for sparse tables
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles factor levels in different orders", {
  devtools::load_all()

  data(jjbarstats_test)

  # Reorder factor levels
  reordered_data <- jjbarstats_test
  reordered_data$response <- factor(
    reordered_data$response,
    levels = c("Complete Response", "Partial Response", "No Response")
  )

  result <- jjbarstats(
    data = reordered_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles unused factor levels", {
  devtools::load_all()

  data(jjbarstats_test)

  # Add unused factor level
  unused_level_data <- jjbarstats_test
  unused_level_data$response <- factor(
    unused_level_data$response,
    levels = c(levels(unused_level_data$response), "Mixed Response")
  )

  result <- jjbarstats(
    data = unused_level_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles missing split-by variable values", {
  devtools::load_all()

  data(jjbarstats_test)
  test_data_na_split <- jjbarstats_test
  test_data_na_split$sex[1:10] <- NA

  # Should handle NA in split variable
  result <- jjbarstats(
    data = test_data_na_split,
    dep = "response",
    group = "treatment",
    grvar = "sex",
    excl = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles single level in split variable", {
  devtools::load_all()

  data(jjbarstats_test)
  single_split <- jjbarstats_test
  single_split$sex <- "Male"

  result <- jjbarstats(
    data = single_split,
    dep = "response",
    group = "treatment",
    grvar = "sex"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles invalid expected proportions", {
  devtools::load_all()

  data(jjbarstats_test)

  # Proportions don't sum to 1
  expect_condition(
    jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      proportiontest = TRUE,
      ratio = "0.5,0.5,0.5"  # Sums to 1.5
    )
  )
})

test_that("jjbarstats handles wrong number of expected proportions", {
  devtools::load_all()

  data(jjbarstats_test)

  # 2 proportions for 3 groups
  expect_condition(
    jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      proportiontest = TRUE,
      ratio = "0.5,0.5"
    )
  )
})

test_that("jjbarstats handles aggregated data with zero counts", {
  devtools::load_all()

  # Create aggregated data with some zero counts
  zero_counts_data <- data.frame(
    response = rep(c("No Response", "Partial Response"), each = 2),
    treatment = rep(c("Placebo", "High Dose"), times = 2),
    count = c(10, 0, 5, 15)  # Zero count for Placebo + Partial Response
  )

  result <- jjbarstats(
    data = zero_counts_data,
    dep = "response",
    group = "treatment",
    counts = "count"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles aggregated data with all zero counts", {
  devtools::load_all()

  zero_all_data <- data.frame(
    response = rep(c("No Response", "Partial Response"), each = 2),
    treatment = rep(c("Placebo", "High Dose"), times = 2),
    count = c(0, 0, 0, 0)
  )

  # Should error as no data
  expect_error(
    jjbarstats(
      data = zero_all_data,
      dep = "response",
      group = "treatment",
      counts = "count"
    )
  )
})

test_that("jjbarstats handles perfect association", {
  devtools::load_all()

  # Create perfect 1:1 association (deterministic relationship)
  perfect_data <- data.frame(
    diagnosis = c(rep("Negative", 50), rep("Positive", 50)),
    test_result = c(rep("Negative", 50), rep("Positive", 50))
  )

  result <- jjbarstats(
    data = perfect_data,
    dep = "diagnosis",
    group = "test_result"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles complete independence", {
  devtools::load_all()

  # Create data with no association (completely independent)
  independent_data <- data.frame(
    var1 = sample(c("A", "B"), 100, replace = TRUE, prob = c(0.5, 0.5)),
    var2 = sample(c("X", "Y"), 100, replace = TRUE, prob = c(0.5, 0.5))
  )

  result <- jjbarstats(
    data = independent_data,
    dep = "var1",
    group = "var2"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles paired data with perfect agreement", {
  devtools::load_all()

  # All subjects have same status at baseline and follow-up
  perfect_agreement <- data.frame(
    baseline = c(rep("Negative", 30), rep("Positive", 20)),
    followup = c(rep("Negative", 30), rep("Positive", 20))
  )

  result <- jjbarstats(
    data = perfect_agreement,
    dep = "baseline",
    group = "followup",
    paired = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles paired data with perfect disagreement", {
  devtools::load_all()

  # All subjects flip status
  perfect_disagreement <- data.frame(
    baseline = c(rep("Negative", 25), rep("Positive", 25)),
    followup = c(rep("Positive", 25), rep("Negative", 25))
  )

  result <- jjbarstats(
    data = perfect_disagreement,
    dep = "baseline",
    group = "followup",
    paired = TRUE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles extreme confidence levels", {
  devtools::load_all()

  data(jjbarstats_test)

  # Very low confidence (not recommended but should work)
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    conflevel = 0.50
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Very high confidence
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    conflevel = 0.999
  )
  expect_s3_class(result2, "jjbarstatsResults")
})

test_that("jjbarstats handles large contingency tables", {
  devtools::load_all()

  # Create 5×5 contingency table
  large_table <- data.frame(
    stage = sample(paste("Stage", 1:5), 200, replace = TRUE),
    grade = sample(paste("Grade", 1:5), 200, replace = TRUE)
  )

  result <- jjbarstats(
    data = large_table,
    dep = "stage",
    group = "grade"
  )

  expect_s3_class(result, "jjbarstatsResults")
})
