context("Medical Decision Test Comparison")

# Synthetic dataset with known diagnostic characteristics
diagnostic_sample <- local({
  pos_block <- data.frame(
    gold = rep("Positive", 60),
    testA = c(rep("Positive", 54), rep("Negative", 6)),
    testB = c(rep("Positive", 48), rep("Negative", 12)),
    testC = c(rep("Positive", 45), rep("Negative", 15))
  )
  neg_block <- data.frame(
    gold = rep("Negative", 60),
    testA = c(rep("Positive", 9), rep("Negative", 51)),
    testB = c(rep("Positive", 6), rep("Negative", 54)),
    testC = c(rep("Positive", 12), rep("Negative", 48))
  )
  combined <- rbind(pos_block, neg_block)
  combined <- within(combined, {
    gold <- factor(gold, levels = c("Positive", "Negative"))
    testA <- factor(testA, levels = c("Positive", "Negative"))
    testB <- factor(testB, levels = c("Positive", "Negative"))
    testC <- factor(testC, levels = c("Positive", "Negative"))
  })
  combined
})

basic_args <- list(
  gold = "gold",
  goldPositive = "Positive",
  test1 = "testA",
  test1Positive = "Positive",
  test2 = "testB",
  test2Positive = "Positive",
  test3 = NULL,
  test3Positive = NULL
)

test_that("decisioncompare works with basic 2 test comparison", {
  result <- do.call(decisioncompare, c(list(data = diagnostic_sample), basic_args))

  expect_true(!is.null(result))
  expect_true(result$comparisonTable$rowCount > 0)
  expect_true(result$cTable1$rowCount > 0)
  expect_true(result$cTable2$rowCount > 0)
})

test_that("decisioncompare works with confidence intervals", {
  result <- do.call(decisioncompare, c(list(data = diagnostic_sample, ci = TRUE), basic_args))

  expect_true(result$epirTable1$visible)
  expect_true(result$epirTable2$visible)
})

test_that("decisioncompare works with statistical comparison", {
  result <- do.call(decisioncompare, c(list(data = diagnostic_sample, statComp = TRUE), basic_args))

  expect_true(result$mcnemarTable$rowCount > 0)
  expect_true(result$diffTable$rowCount > 0)
})

test_that("decisioncompare works with 3 tests", {
  result <- decisioncompare(
    data = diagnostic_sample,
    gold = "gold",
    goldPositive = "Positive",
    test1 = "testA",
    test1Positive = "Positive",
    test2 = "testB",
    test2Positive = "Positive",
    test3 = "testC",
    test3Positive = "Positive",
    statComp = TRUE
  )

  expect_true(result$cTable1$rowCount > 0)
  expect_true(result$cTable2$rowCount > 0)
  expect_true(result$cTable3$rowCount > 0)
  expect_true(all(c("testA", "testB", "testC") %in% result$comparisonTable$rowKeys))
  expect_true(result$mcnemarTable$rowCount >= 3)
})

test_that("decisioncompare works with footnotes and original data", {
  result <- do.call(decisioncompare, c(list(data = diagnostic_sample, fnote = TRUE, od = TRUE), basic_args))

  expect_true(!is.null(result$text1$content))
  expect_true(!is.null(result$text2$content))
})

test_that("decisioncompare handles missing data gracefully", {
  test_data <- diagnostic_sample
  test_data$testA[seq_len(5)] <- NA

  result <- do.call(decisioncompare, c(list(data = test_data), basic_args))

  expect_true(!is.null(result))
  expect_true(result$comparisonTable$rowCount > 0)
})

test_that("decisioncompare fails gracefully with invalid input", {
  expect_error(decisioncompare(data = diagnostic_sample))
})

test_that("decisioncompare plotting functionality works", {
  result <- do.call(decisioncompare, c(list(data = diagnostic_sample, plot = TRUE), basic_args))

  expect_true(!is.null(result$plot1))
  expect_true(result$plot1$visible)
})

test_that("paired difference estimates use paired variance", {
  synthetic <- data.frame(
    gold = factor(c("Positive", "Positive", "Negative", "Negative", "Positive", "Negative", "Positive", "Negative"),
                  levels = c("Positive", "Negative")),
    testA = factor(c("Positive", "Negative", "Negative", "Negative", "Positive", "Positive", "Positive", "Negative"),
                   levels = c("Positive", "Negative")),
    testB = factor(c("Positive", "Positive", "Negative", "Negative", "Negative", "Negative", "Positive", "Negative"),
                   levels = c("Positive", "Negative"))
  )

  result <- decisioncompare(
    data = synthetic,
    gold = "gold",
    goldPositive = "Positive",
    test1 = "testA",
    test1Positive = "Positive",
    test2 = "testB",
    test2Positive = "Positive",
    test3 = NULL,
    test3Positive = NULL,
    statComp = TRUE
  )

  diff_df <- result$diffTable$asDF

  expect_equal(nrow(diff_df), 3)
  expect_setequal(diff_df$metric, c("Sensitivity", "Specificity", "Accuracy"))

  sens_row <- diff_df[diff_df$metric == "Sensitivity", ]
  expect_equal(sens_row$diff, 0, tolerance = 1e-6)
  expect_true(sens_row$lower <= sens_row$diff && sens_row$upper >= sens_row$diff)

  spec_row <- diff_df[diff_df$metric == "Specificity", ]
  expect_equal(spec_row$diff, -0.25, tolerance = 1e-6)
  expect_true(spec_row$lower <= spec_row$diff && spec_row$upper >= spec_row$diff)

  acc_row <- diff_df[diff_df$metric == "Accuracy", ]
  expect_equal(acc_row$diff, -0.125, tolerance = 1e-6)
  expect_true(acc_row$lower <= acc_row$diff && acc_row$upper >= acc_row$diff)
})
