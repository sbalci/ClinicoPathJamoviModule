new_swimmerplot_lifecycle <- function(data, ...) {
  options <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end", ...
  )
  ClinicoPath:::swimmerplotClass$new(options = options, data = data)
}

test_that("fatal timeline validation enters jamovi's error state", {
  analysis <- new_swimmerplot_lifecycle(
    data.frame(id = c("A", "B"), start = c(5, 5), end = c(1, 2))
  )
  analysis$run(noThrow = TRUE)

  expect_identical(analysis$status, "error")
  expect_true(analysis$errored)
  expect_false(analysis$complete)
  expect_null(analysis$results$plot$state)
  expect_match(analysis$results$instructions$content, "No valid data rows after validation")
})

test_that("both fatal time-type mismatches enter jamovi's error state", {
  numeric_data <- data.frame(id = c("A", "B"), start = 0, end = c(5, 8))
  numeric_analysis <- new_swimmerplot_lifecycle(numeric_data, timeType = "datetime")
  numeric_analysis$run(noThrow = TRUE)
  expect_identical(numeric_analysis$status, "error")
  expect_true(numeric_analysis$errored)
  expect_null(numeric_analysis$results$plot$state)
  expect_match(numeric_analysis$results$instructions$content, "Raw Values")

  date_data <- data.frame(
    id = c("A", "B"), start = c("2023-01-01", "2023-02-01"),
    end = c("2023-06-01", "2023-07-01")
  )
  date_analysis <- new_swimmerplot_lifecycle(date_data)
  date_analysis$run(noThrow = TRUE)
  expect_identical(date_analysis$status, "error")
  expect_true(date_analysis$errored)
  expect_null(date_analysis$results$plot$state)
  expect_match(date_analysis$results$instructions$content, "Date/Time")
})

test_that("a failed rerun clears the previous timeline and can recover", {
  data <- data.frame(id = c("A", "B"), start = 0, end = c(5, 8))
  analysis <- new_swimmerplot_lifecycle(data, exportTimeline = TRUE)
  analysis$run(noThrow = TRUE)
  expect_identical(analysis$status, "complete")
  expect_false(analysis$errored)
  expect_false(is.null(analysis$results$plot$state))
  expect_equal(analysis$results$timelineData$rowCount, 2)
  expect_gt(nchar(analysis$results$instructions$content), 0)
  expect_identical(trimws(gsub("<[^>]+>", "", analysis$results$instructions$content)), "")

  analysis$.__enclos_env__$private$.data$end <- c(-1, -2)
  analysis$run(noThrow = TRUE)
  expect_identical(analysis$status, "error")
  expect_true(analysis$errored)
  expect_null(analysis$results$plot$state)
  expect_equal(analysis$results$timelineData$rowCount, 0)
  expect_true(all(is.na(analysis$results$summary$asDF$value)))

  analysis$.__enclos_env__$private$.data <- data
  analysis$run(noThrow = TRUE)
  expect_identical(analysis$status, "complete")
  expect_false(analysis$errored)
  expect_false(is.null(analysis$results$plot$state))
  expect_equal(analysis$results$timelineData$rowCount, 2)
  expect_false(grepl("No valid data rows", analysis$results$instructions$content, fixed = TRUE))
  expect_gt(nchar(analysis$results$instructions$content), 0)
  expect_identical(trimws(gsub("<[^>]+>", "", analysis$results$instructions$content)), "")
})

test_that("a downstream exception is propagated after result cleanup", {
  failing_class <- R6::R6Class(
    inherit = ClinicoPath:::swimmerplotClass,
    private = list(
      .generateCopyReadyReport = function(...) stop("Copy-ready {report} failed")
    )
  )
  options <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end",
    showCopyReady = TRUE, exportTimeline = TRUE
  )
  analysis <- failing_class$new(
    options = options, data = data.frame(id = c("A", "B"), start = 0, end = c(5, 8))
  )
  expect_error(analysis$run(), "Copy-ready {report} failed", fixed = TRUE)
  analysis$run(noThrow = TRUE)

  expect_identical(analysis$status, "error")
  expect_true(analysis$errored)
  expect_null(analysis$results$plot$state)
  expect_equal(analysis$results$timelineData$rowCount, 0)
  expect_true(all(is.na(analysis$results$summary$asDF$value)))
  expect_match(analysis$results$instructions$content, "Copy-ready (report) failed", fixed = TRUE)
})

test_that("setup guidance and nonfatal exclusions still complete", {
  options <- ClinicoPath:::swimmerplotOptions$new()
  setup <- ClinicoPath:::swimmerplotClass$new(options = options, data = data.frame())
  setup$run(noThrow = TRUE)
  expect_identical(setup$status, "complete")
  expect_false(setup$errored)
  expect_null(setup$results$plot$state)

  analysis <- new_swimmerplot_lifecycle(
    data.frame(id = c("A", "B", "C"), start = 0, end = c(5, -1, 8))
  )
  analysis$run(noThrow = TRUE)
  expect_identical(analysis$status, "complete")
  expect_false(analysis$errored)
  expect_false(is.null(analysis$results$plot$state))
  expect_equal(analysis$results$summary$asDF$value[1], 2)
})
