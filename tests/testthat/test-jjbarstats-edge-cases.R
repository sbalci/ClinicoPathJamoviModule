# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjbarstats
# ═══════════════════════════════════════════════════════════

library(testthat)

# A jamovi analysis does not throw on bad input - .run() catches the rejection
# and writes it into the `todo` panel. `expect_error()` therefore asserts the
# OPPOSITE of the desired behaviour: it passes only when the analysis crashes.
# Read the panel instead.
jbs_todo <- function(res) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(res$todo$content)))

test_that("jjbarstats prompts for the variables it still needs", {

  data(jjbarstats_test)

  # Missing dep -> the getting-started panel, not a crash
  expect_match(jbs_todo(jjbarstats(data = jjbarstats_test, group = "treatment")),
               "Select your Outcome Variable")

  # Missing group
  expect_match(jbs_todo(jjbarstats(data = jjbarstats_test, dep = "response")),
               "Choose a Group Variable")

  # Missing data
  expect_error(
    jjbarstats(
      dep = "response",
      group = "treatment"
    )
  )
})

test_that("jjbarstats handles missing data correctly", {

  data(jjbarstats_test)
  test_data_na <- jjbarstats_test
  test_data_na$response[1:10] <- NA

  # Rows with NA in a selected variable are dropped and reported
  result <- jjbarstats(
    data = test_data_na,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles all NA in dependent variable", {

  # jmvcore::reject() stops the analysis; through the R wrapper that is an error.

  data(jjbarstats_test)
  test_data_all_na <- jjbarstats_test
  test_data_all_na$response <- NA

  # `$response <- NA` replaces the factor with a logical column, so this lands on
  # the variation guard (0 levels) rather than the complete-cases guard.
  expect_error(jjbarstats(data = test_data_all_na, dep = "response",
                          group = "treatment"),
               "insufficient variation")

  # Keeping the factor - all values missing, levels intact - takes the other path
  keep_levels <- jjbarstats_test
  keep_levels$response <- factor(NA, levels = levels(jjbarstats_test$response))
  expect_error(jjbarstats(data = keep_levels, dep = "response",
                          group = "treatment"),
               "No complete data rows available")
})

test_that("jjbarstats handles missing grouping variable values", {

  data(jjbarstats_test)
  test_data_na_group <- jjbarstats_test
  test_data_na_group$treatment[1:5] <- NA

  # Should handle NA groups
  result <- jjbarstats(
    data = test_data_na_group,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles small sample sizes", {

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

  data(jjbarstats_test)
  tiny_data <- jjbarstats_test[1:10, ]

  # This used to pass by catching an incidental "Chi-squared approximation may be
  # incorrect" console warning from chisq.test(). That warning is now suppressed
  # where the backend computes expected counts, because it fires on exactly the
  # sparse tables those helpers exist to DETECT and report properly. Assert the
  # analysis's own signal, which is more specific and actually reaches the user.
  res <- jjbarstats(data = tiny_data, dep = "response", group = "treatment")
  n <- gsub("[[:space:]]+", " ", paste(as.character(res$notices$content), collapse = " "))
  expect_match(n, "Low Expected Counts")
  expect_match(n, "expected counts below 5")
})

test_that("jjbarstats handles single level in dependent variable", {

  data(jjbarstats_test)
  single_dep <- jjbarstats_test
  single_dep$response <- "No Response"

  expect_error(jjbarstats(data = single_dep, dep = "response",
                          group = "treatment"),
               "insufficient variation")
})

test_that("jjbarstats handles single level in grouping variable", {

  data(jjbarstats_test)
  single_group <- jjbarstats_test
  single_group$treatment <- "Placebo"

  expect_error(jjbarstats(data = single_group, dep = "response",
                          group = "treatment"),
               "at least 2 categories")
})

test_that("jjbarstats handles variables with special characters", {

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

  # Create sparse 3×3 table with many zero cells
  sparse_data <- data.frame(
    response = c(rep("No Response", 25), rep("Partial Response", 5), rep("Complete Response", 5)),
    treatment = c(rep("Placebo", 20), rep("Low Dose", 10), rep("High Dose", 5))
  )

  result <- jjbarstats(
    data = sparse_data,
    dep = "response",
    group = "treatment"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles factor levels in different orders", {

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

  data(jjbarstats_test)
  test_data_na_split <- jjbarstats_test
  test_data_na_split$sex[1:10] <- NA

  # Should handle NA in split variable
  result <- jjbarstats(
    data = test_data_na_split,
    dep = "response",
    group = "treatment",
    grvar = "sex"
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles single level in split variable", {

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

  data(jjbarstats_test)

  # Proportions don't sum to 1 -> rescaled, and the user is told what was used.
  # The rescaling always happened; the notice explaining it was raised inside
  # .createBarPlot, i.e. during .plot(), where notices are discarded.
  res <- jjbarstats(data = jjbarstats_test, dep = "response", group = "treatment",
                    proportiontest = TRUE, ratio = "0.5,0.5,0.5")
  n <- gsub("[[:space:]]+", " ", paste(as.character(res$notices$content), collapse = " "))
  expect_match(n, "rescaled")
  expect_match(n, "0.333, 0.333, 0.333", fixed = TRUE)
})

test_that("jjbarstats handles wrong number of expected proportions", {

  data(jjbarstats_test)

  # 2 proportions for a 3-category outcome. The length was never checked at all,
  # so the test ran against proportions the user did not specify, silently.
  res <- jjbarstats(data = jjbarstats_test, dep = "response", group = "treatment",
                    proportiontest = TRUE, ratio = "0.5,0.5")
  n <- gsub("[[:space:]]+", " ", paste(as.character(res$notices$content), collapse = " "))
  expect_match(n, "Expected proportions ignored")
  expect_match(n, "2 proportions given for 'response', which has 3 categories", fixed = TRUE)
})

test_that("jjbarstats handles aggregated data with zero counts", {

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

  zero_all_data <- data.frame(
    response = rep(c("No Response", "Partial Response"), each = 2),
    treatment = rep(c("Placebo", "High Dose"), times = 2),
    count = c(0, 0, 0, 0)
  )

  # n = 0 is not a small sample, it is no sample. This used to run: the summary
  # panel announced "Sample Size: 0 observations" beside "Statistical Method:
  # Chi-square test of independence" and a chart was drawn.
  expect_error(jjbarstats(data = zero_all_data, dep = "response",
                          group = "treatment", counts = "count"),
               "sums to zero")
})

test_that("jjbarstats handles perfect association", {

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

test_that("jjbarstats rejects paired data with perfect agreement", {

  # All subjects have the same status at baseline and follow-up: no discordant
  # pairs, so McNemar's statistic is 0/0 and the analysis must say so.
  perfect_agreement <- data.frame(
    baseline = c(rep("Negative", 30), rep("Positive", 20)),
    followup = c(rep("Negative", 30), rep("Positive", 20))
  )

  expect_error(jjbarstats(data = perfect_agreement, dep = "baseline",
                          group = "followup", paired = TRUE),
               "No discordant pairs")
})

test_that("jjbarstats handles paired data with perfect disagreement", {

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
