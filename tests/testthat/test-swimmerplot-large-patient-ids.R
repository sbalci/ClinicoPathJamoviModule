sw_id_run <- function(data, ...) {
  suppressWarnings(swimmerplot(
    data = data, patientID = "id", startTime = "start", endTime = "end", ...
  ))
}

test_that("patient labels distinguish doubles and stay stable across subsets", {
  ids <- c(1e15 + 0:3, 0.1, 0.1 + .Machine$double.eps)
  labels <- .patientIDLabels(ids)
  expect_length(unique(labels), length(unique(ids)))
  expect_equal(as.numeric(labels), ids, tolerance = .Machine$double.eps)
  expect_identical(.patientIDLabels(ids[2:4]), labels[2:4])
  expect_identical(.patientIDLabels(c(-0, 0)), c("0", "0"))
  expect_true(all(is.na(.patientIDLabels(c(NA_real_, NaN)))))
  expect_identical(.patientIDLabels(c("001", "1e+15", "A/B {x}")),
                   c("001", "1e+15", "A/B {x}"))
})

test_that("four large numeric patients remain four in summaries, plot and export", {
  d <- data.frame(id = 1e15 + 0:3, start = 0, end = c(10, 20, 30, 40))
  result <- sw_id_run(d, exportTimeline = TRUE)
  expect_equal(as.numeric(result$summary$asDF$value), c(4, 4, 25, 25, 100))
  expect_setequal(as.character(result$plot$state$patient_data$patient_id),
                  sprintf("%.0f", d$id))
  expect_setequal(result$timelineData$asDF$patient_id, sprintf("%.0f", d$id))
})

test_that("numeric patient identity is consistent across timeline features", {
  d <- data.frame(
    id = rep(1e15 + 0:1, each = 2),
    start = c(10, 20, 20, 30), end = c(15, 25, 25, 40),
    milestone = c(NA, 22, NA, 32),
    event = c("scan A", NA, "scan B", "scan C"),
    event_time = c(12, NA, 23, 33),
    status = c(NA, 0, NA, 1), sort = c(2, 2, 1, 1)
  )
  args <- list(milestone1Name = "Visit", milestone1Date = "milestone",
               showEventMarkers = TRUE, eventVar = "event", eventTimeVar = "event_time",
               censorVar = "status", sortVariable = "sort", exportTimeline = TRUE)
  numeric_result <- do.call(sw_id_run, c(list(d), args))
  text <- d
  text$id <- sprintf("%.0f", text$id)
  text_result <- do.call(sw_id_run, c(list(text), args))
  expect_equal(numeric_result$summary$asDF, text_result$summary$asDF)
  expect_equal(numeric_result$timelineData$asDF, text_result$timelineData$asDF)
  for (field in c("patient_data", "milestone_data", "event_data", "arrow_data")) {
    expect_equal(numeric_result$plot$state[[field]], text_result$plot$state[[field]])
  }
  expect_equal(as.numeric(numeric_result$summary$asDF$value), c(2, 4, 17.5, 17.5, 25))
})

test_that("the data.table summary path preserves large numeric patient identity", {
  n <- 1001
  d <- data.frame(id = 1e15 + seq_len(n), start = 0, end = seq_len(n) %% 30 + 1)
  result <- sw_id_run(d)
  expect_equal(as.numeric(result$summary$asDF$value[1]), n)
  expect_equal(as.numeric(result$summary$asDF$value[5]), sum(d$end))
  expect_length(unique(result$plot$state$patient_data$patient_id), n)
})
