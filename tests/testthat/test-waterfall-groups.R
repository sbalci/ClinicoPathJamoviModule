# Tests for group-based colouring options in waterfall analysis

library(testthat)
library(ClinicoPath)

set.seed(123)
test_data <- data.frame(
  PatientID = paste0("PT", 1:20),
  Response = seq(-75, 105, length.out = 20),
  TreatmentArm = rep(c("Arm A", "Arm B"), each = 10),
  DiseaseStage = rep(c("Early", "Advanced"), times = 10)
)

test_that("waterfall handles default RECIST colouring", {
  result <- suppressMessages(
    waterfall(
      data = test_data,
      patientID = "PatientID",
      responseVar = "Response",
      inputType = "percentage",
      colorBy = "recist",
      showWaterfallPlot = TRUE
    )
  )

  expect_s3_class(result, "waterfallResults")

  summary_table <- result$summaryTable$asDF
  expect_equal(sum(summary_table$n), nrow(test_data))
})

test_that("group-based colouring populates comparison tables", {
  result <- suppressMessages(
    waterfall(
      data = test_data,
      patientID = "PatientID",
      responseVar = "Response",
      inputType = "percentage",
      groupVar = "TreatmentArm",
      colorBy = "group",
      colorScheme = "colorful",
      showWaterfallPlot = TRUE
    )
  )

  comparison <- result$groupComparisonTable$asDF
  expect_gt(nrow(comparison), 0)
  expect_setequal(comparison$group, unique(test_data$TreatmentArm))
})

test_that("requesting group colours without a group variable falls back gracefully", {
  result <- suppressMessages(
    waterfall(
      data = test_data,
      patientID = "PatientID",
      responseVar = "Response",
      inputType = "percentage",
      colorBy = "group",
      showWaterfallPlot = TRUE
    )
  )

  comparison <- result$groupComparisonTable$asDF
  expect_equal(nrow(comparison), 0)

  # Ensure RECIST summary still produced
  summary_table <- result$summaryTable$asDF
  expect_equal(sum(summary_table$n), nrow(test_data))
})
