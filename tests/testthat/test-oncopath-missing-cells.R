# Unavailable values reach jamovi as missing cells, never "NaN" (2026-10-18 check-module findings 1-3).
# jamovi shows a numeric cell that holds a double NA or NaN as the text "NaN" and a NULL cell as "."; an
# unavailable statistic is written as NULL and the reason is given once (note, detail column or notice).

missing_cells <- function(results) {
  tables <- list()
  collect <- function(el) {
    if (inherits(el, "Group") || inherits(el, "Array")) {
      for (child in el$items) collect(child)
    } else if (inherits(el, "Table")) {
      tables[[length(tables) + 1L]] <<- el
    }
  }
  collect(results)
  hits <- character()
  for (tbl in tables) {
    if (!isTRUE(tbl$visible)) next
    for (col in tbl$columns) {
      if (!isTRUE(col$visible)) next
      for (i in seq_len(tbl$rowCount)) {
        v <- tbl$getCell(col$name, rowNo = i)$value
        if (is.double(v) && length(v) == 1L && is.na(v))
          hits <- c(hits, sprintf("%s$%s[%d]", tbl$name, col$name, i))
      }
    }
  }
  hits
}

# The jamovi engine runs init(noThrow = TRUE) and calls run(noThrow = TRUE) only if init succeeded.
engine_run <- function(fn, data, ...) {
  ns <- asNamespace("ClinicoPath")
  options <- get(paste0(fn, "Options"), envir = ns)$new(...)
  analysis <- get(paste0(fn, "Class"), envir = ns)$new(options = options, data = data)
  analysis$init(noThrow = TRUE)
  if (!analysis$errored) analysis$run(noThrow = TRUE)
  analysis
}

notice_text <- function(analysis) paste(analysis$results$notices$content, collapse = " ")

dm_counts <- list(study = "study", true_positives = "true_positives", false_positives = "false_positives",
                  false_negatives = "false_negatives", true_negatives = "true_negatives")

test_that("waterfall leaves an unreached Kaplan-Meier median empty", {
  a <- engine_run("waterfall", waterfall_raw_longitudinal, patientID = "PatientID",
                  responseVar = "Measurement", timeVar = "Time", inputType = "raw",
                  showResponseDuration = TRUE)
  expect_identical(a$status, "complete")
  rdt <- a$results$responseDurationTable
  expect_true(is.finite(rdt$getRow(rowKey = "ttr")$value$value))
  expect_true(is.finite(rdt$getRow(rowKey = "dor_naive")$value$value))
  km <- rdt$getRow(rowKey = "dor_km")
  expect_null(km$value$value)
  expect_match(km$detail$value, "Median not reached")
  expect_identical(missing_cells(a$results), character(0))
})

test_that("waterfall response-duration rows are empty without responders or a time variable", {
  grow <- waterfall_raw_longitudinal
  grow$Measurement <- ifelse(grow$Time == 0, 50, 50 + grow$Time * 3)
  a <- engine_run("waterfall", grow, patientID = "PatientID", responseVar = "Measurement",
                  timeVar = "Time", inputType = "raw", showResponseDuration = TRUE)
  rdt <- a$results$responseDurationTable
  for (key in c("ttr", "dor_naive", "dor_km")) expect_null(rdt$getRow(rowKey = key)$value$value)
  expect_match(rdt$notes[["none"]]$note, "No patient reached a response")
  expect_identical(missing_cells(a$results), character(0))

  b <- engine_run("waterfall", waterfall_percentage_basic, patientID = "PatientID",
                  responseVar = "Response", inputType = "percentage", showResponseDuration = TRUE)
  rdt <- b$results$responseDurationTable
  for (key in c("ttr", "dor_naive", "dor_km")) expect_null(rdt$getRow(rowKey = key)$value$value)
  expect_match(rdt$notes[["needs_time"]]$note, "Time Variable is required")
  expect_identical(missing_cells(b$results), character(0))
})

test_that("waterfall leaves the percentage of non-evaluable patients empty", {
  d <- waterfall_raw_longitudinal[waterfall_raw_longitudinal$Time == 0, ]
  d <- rbind(d, transform(d[1, ], Time = 2, Measurement = 40))
  a <- engine_run("waterfall", d, patientID = "PatientID", responseVar = "Measurement",
                  timeVar = "Time", inputType = "raw")
  expect_identical(a$status, "complete")
  unknown <- a$results$summaryTable$getRow(rowKey = "recist_Unknown")
  expect_gt(unknown$n$value, 0)
  expect_null(unknown$percent$value)
  expect_identical(missing_cells(a$results), character(0))
})

test_that("waterfall with zero rows stays idle and says why", {
  a <- engine_run("waterfall", waterfall_percentage_basic[0, ], patientID = "PatientID",
                  responseVar = "Response", inputType = "percentage")
  expect_identical(a$status, "complete")
  expect_match(notice_text(a), "The dataset contains no rows")
  expect_match(paste(a$results$todo$content, collapse = " "), "Treatment Response: Patient-Level Burden")

  guided <- engine_run("waterfall", waterfall_percentage_basic[0, ], patientID = "PatientID",
                       responseVar = "Response", inputType = "percentage", enableGuidedMode = TRUE)
  expect_identical(guided$status, "complete")
  expect_match(notice_text(guided), "The dataset contains no rows")
  expect_match(paste(guided$results$guidedAnalysis$content, collapse = " "), "[STOPPED]", fixed = TRUE)

  unconfigured <- engine_run("waterfall", data.frame())
  expect_identical(unconfigured$status, "complete")
  expect_false(grepl("contains no rows", notice_text(unconfigured)))
})

test_that("swimmerplot errored runs leave the scaffolded rows empty", {
  a <- engine_run("swimmerplot", data.frame(id = c("A", "B"), start = c(5, 5), end = c(1, 2)),
                  patientID = "id", startTime = "start", endTime = "end")
  expect_true(a$errored)
  summary <- a$results$summary
  expect_equal(summary$rowCount, 5)
  for (i in seq_len(summary$rowCount)) expect_null(summary$getCell("value", rowNo = i)$value)
  advanced <- a$results$advancedMetrics
  expect_gt(advanced$rowCount, 0)
  for (i in seq_len(advanced$rowCount)) expect_null(advanced$getCell("metric_value", rowNo = i)$value)
  expect_identical(missing_cells(a$results), character(0))
})

test_that("swimmerplot leaves undefined rates empty", {
  zero <- data.frame(id = paste0("P", 1:6), start = 3, end = 3, resp = rep(c("CR", "PD"), 3))
  a <- engine_run("swimmerplot", zero, patientID = "id", startTime = "start", endTime = "end",
                  responseVar = "resp")
  expect_identical(a$status, "complete")
  person_time <- a$results$personTimeTable
  expect_gt(person_time$rowCount, 0)
  for (i in seq_len(person_time$rowCount))
    expect_null(person_time$getCell("incidence_rate", rowNo = i)$value)
  expect_null(a$results$advancedMetrics$getRow(rowKey = "followup_density")$metric_value$value)
  expect_identical(missing_cells(a$results), character(0))

  labels <- swimmerplot_sample
  labels$Response <- ifelse(seq_len(nrow(labels)) %% 2 == 0, "Good", "Bad")
  b <- engine_run("swimmerplot", labels, patientID = "PatientID", startTime = "StartTime",
                  endTime = "EndTime", responseVar = "Response")
  expect_identical(b$status, "complete")
  for (key in c("orr", "dcr"))
    expect_null(b$results$advancedMetrics$getRow(rowKey = key)$metric_value$value)
  expect_identical(missing_cells(b$results), character(0))
})

test_that("diagnosticmeta leaves the Deeks row empty when study size cannot vary", {
  d <- diagnosticmeta_test_categorical[1:6, ]
  d$true_positives <- c(40, 42, 38, 45, 41, 39)
  d$false_negatives <- 60 - d$true_positives
  d$true_negatives <- c(80, 82, 79, 85, 81, 78)
  d$false_positives <- 100 - d$true_negatives
  a <- do.call(engine_run, c(list("diagnosticmeta", d), dm_counts,
                             list(publication_bias = TRUE, heterogeneity_analysis = TRUE)))
  expect_identical(a$status, "complete")
  deeks <- a$results$publicationbias$getRow(rowKey = "deeks_test")
  expect_null(deeks$statistic$value)
  expect_null(deeks$p_value$value)
  expect_match(deeks$interpretation$value, "same effective sample size")
  expect_identical(missing_cells(a$results), character(0))
})

test_that("diagnosticmeta with zero rows stays idle and says why", {
  a <- do.call(engine_run, c(list("diagnosticmeta", diagnosticmeta_test_categorical[0, ]), dm_counts))
  expect_identical(a$status, "complete")
  expect_true(a$results$instructions$visible)
  expect_match(notice_text(a), "The dataset contains no rows")

  unconfigured <- engine_run("diagnosticmeta", data.frame())
  expect_identical(unconfigured$status, "complete")
  expect_false(grepl("contains no rows", notice_text(unconfigured)))
})

test_that("bundled examples with every table enabled show no NaN cell", {
  dm <- do.call(engine_run, c(list("diagnosticmeta", diagnosticmeta_test_categorical), dm_counts, list(
    hsroc_analysis = TRUE, meta_regression = TRUE, covariate = "imaging_modality",
    heterogeneity_analysis = TRUE, publication_bias = TRUE, show_individual_studies = TRUE)))
  expect_identical(dm$status, "complete")
  expect_identical(missing_cells(dm$results), character(0))

  sw <- engine_run("swimmerplot", swimmerplot_sample, patientID = "PatientID", startTime = "StartTime",
                   endTime = "EndTime", responseVar = "Response", milestone1Date = "Surgery",
                   milestone4Date = "Progression", showEventMarkers = TRUE, eventVar = "EventType",
                   eventTimeVar = "EventTime", exportTimeline = TRUE, exportSummary = TRUE)
  expect_identical(sw$status, "complete")
  expect_identical(missing_cells(sw$results), character(0))

  wf <- engine_run("waterfall", waterfall_raw_longitudinal, patientID = "PatientID",
                   responseVar = "Measurement", timeVar = "Time", inputType = "raw",
                   showResponseDuration = TRUE, showSpiderPlot = TRUE, showClinicalSignificance = TRUE)
  expect_identical(wf$status, "complete")
  expect_identical(missing_cells(wf$results), character(0))
})
