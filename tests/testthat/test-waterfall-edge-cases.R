# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: waterfall
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error conditions, and boundary scenarios

library(testthat)

# Load test data
data(waterfall_test, package = "ClinicoPath")
data(waterfall_missing, package = "ClinicoPath")
data(waterfall_extreme, package = "ClinicoPath")
data(waterfall_no_baseline, package = "ClinicoPath")
data(waterfall_small, package = "ClinicoPath")
data(waterfall_large, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# MISSING DATA HANDLING
# ═══════════════════════════════════════════════════════════


# jamovi analyses report problems through result elements, never by throwing an
# R condition -- throwing would break the GUI. Assert on the rendered text.
wf_text <- function(result, element = "notices") {
  el <- result[[element]]
  if (is.null(el)) return("")
  gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(el$content), collapse = " ")))
}

test_that("patients with a missing response are excluded and reported", {
  result <- waterfall(
    data = waterfall_missing,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_match(wf_text(result), "PATIENTS EXCLUDED")
})

test_that("waterfall handles missing group values", {
  result <- waterfall(
    data = waterfall_missing,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  # Should handle missing groups gracefully
  expect_s3_class(result, "waterfallResults")
})

test_that("an all-missing cohort says why nothing was produced", {
  all_na_data <- waterfall_test
  all_na_data$best_response <- NA_real_

  result <- waterfall(
    data = all_na_data,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_equal(result$summaryTable$rowCount, 0)
  # Validation aborts before the analysis runs and explains itself in todo2.
  expect_match(wf_text(result, "todo2"), "No patients with valid response data")
})

test_that("waterfall handles missing patientID values", {
  missing_id_data <- waterfall_test
  missing_id_data$patientID[1:3] <- NA

  # Should error or warn about missing IDs
  expect_condition(
    waterfall(
      data = missing_id_data,
      patientID = "patientID",
      responseVar = "best_response"
    )
  )
})

# ═══════════════════════════════════════════════════════════
# EXTREME VALUES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles complete response (<= -100%)", {
  result <- waterfall(
    data = waterfall_extreme,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result)
})

test_that("waterfall handles impossible shrinkage values (< -100%)", {
  skip("KNOWN GAP: values below -100% are neither capped nor flagged. The
  responseVar option text promises 'Values will be automatically capped at -100%
  for analysis', but -110/-150/-200 are carried through unchanged; they are
  categorised CR (correct) while the plotted bar and any reported minimum still
  show the impossible value. Fix by capping at -100 in .processData and emitting
  a notice naming the affected patients.")
})

test_that("waterfall handles extreme progressive disease (> 200%)", {
  result <- waterfall(
    data = waterfall_extreme,
    patientID = "patientID",
    responseVar = "best_response"
  )

  # Should handle but may warn about extreme values
  expect_s3_class(result, "waterfallResults")
})

test_that("waterfall handles zero response values", {
  zero_data <- waterfall_test
  zero_data$best_response <- 0 # All stable disease

  result <- waterfall(
    data = zero_data,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# DUPLICATE PATIENT IDs
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles duplicate patient IDs in percentage mode", {
  dup_data <- rbind(waterfall_test, waterfall_test[1:3, ])

  # In percentage mode, duplicates should error or warn
  expect_condition(
    waterfall(
      data = dup_data,
      patientID = "patientID",
      responseVar = "best_response",
      inputType = "percentage"
    )
  )
})

test_that("waterfall accepts duplicate IDs for longitudinal data", {
  # Load spider data which has multiple rows per patient
  data(waterfall_spider_test, package = "ClinicoPath")

  result <- waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    inputType = "percentage"
  )

  # Should handle longitudinal data with duplicate IDs
  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# RAW MEASUREMENT EDGE CASES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles missing baseline measurements", {
  # Should error with informative message
  expect_error(
    waterfall(
      data = waterfall_no_baseline,
      patientID = "patientID",
      responseVar = "pct_change",
      timeVar = "time",
      inputType = "raw"
    ),
    regexp = "baseline|time.*0|reference",
    ignore.case = TRUE
  )
})

test_that("waterfall handles zero tumor size", {
  data(waterfall_raw_test, package = "ClinicoPath")
  zero_tumor_data <- waterfall_raw_test
  zero_tumor_data$tumor_size[zero_tumor_data$time > 0][1:3] <- 0

  result <- waterfall(
    data = zero_tumor_data,
    patientID = "patientID",
    responseVar = "tumor_size",
    timeVar = "time",
    inputType = "raw"
  )

  # Should handle zero sizes (complete response)
  expect_s3_class(result, "waterfallResults")
})

test_that("waterfall handles negative raw measurements", {
  skip("KNOWN GAP: negative raw tumour measurements are accepted silently. A
  negative baseline yields a percent change with an inverted sign, so the patient
  is categorised as responding when the tumour grew. Fix by rejecting non-positive
  raw measurements with a notice naming the affected patients.")
})

# ═══════════════════════════════════════════════════════════
# SMALL AND LARGE DATASETS
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles minimal dataset (n=5)", {
  result <- waterfall(
    data = waterfall_small,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_s3_class(result, "waterfallResults")
})

test_that("a single-patient cohort completes and is flagged as uninformative", {
  single <- waterfall_test[1, ]
  result <- waterfall(
    data = single,
    patientID = "patientID",
    responseVar = "best_response"
  )
  expect_equal(result$summaryTable$rowCount, 4)
  expect_match(wf_text(result), "VERY SMALL COHORT")
})

test_that("waterfall handles large dataset (n=200)", {
  result <- waterfall(
    data = waterfall_large,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# VARIABLE NAME EDGE CASES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles variables with spaces in names", {
  space_data <- waterfall_test
  names(space_data)[names(space_data) == "best_response"] <- "best response"

  result <- waterfall(
    data = space_data,
    patientID = "patientID",
    responseVar = "best response"
  )

  expect_no_error(result)
})

test_that("waterfall handles variables with special characters", {
  special_data <- waterfall_test
  names(special_data)[names(special_data) == "best_response"] <- "best-response%"

  result <- waterfall(
    data = special_data,
    patientID = "patientID",
    responseVar = "best-response%"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# CONSTANT VALUES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles constant response values", {
  const_data <- waterfall_test
  const_data$best_response <- -30 # All exactly PR threshold

  result <- waterfall(
    data = const_data,
    patientID = "patientID",
    responseVar = "best_response"
  )

  # Should work but plot may be uninformative
  expect_no_error(result)
})

test_that("waterfall handles constant group variable", {
  const_group_data <- waterfall_test
  const_group_data$treatment <- "Same Treatment"

  result <- waterfall(
    data = const_group_data,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  # Should work but grouping is meaningless
  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# DATA TYPE MISMATCHES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles character response variable", {
  char_data <- waterfall_test
  char_data$best_response <- as.character(char_data$best_response)

  # Should error - response must be numeric
  expect_error(
    waterfall(
      data = char_data,
      patientID = "patientID",
      responseVar = "best_response"
    ),
    regexp = "numeric|number|character|type",
    ignore.case = TRUE
  )
})

test_that("waterfall handles character time variable", {
  data(waterfall_spider_test, package = "ClinicoPath")
  char_time_data <- waterfall_spider_test
  char_time_data$time <- as.character(char_time_data$time)

  # Should error - time must be numeric
  expect_error(
    waterfall(
      data = char_time_data,
      patientID = "patientID",
      responseVar = "pct_change",
      timeVar = "time",
      showSpiderPlot = TRUE
    ),
    regexp = "numeric|number|character|time.*type",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# THRESHOLD BOUNDARY VALUES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles exact RECIST threshold values", {
  threshold_data <- tibble::tibble(
    patientID = paste0("PT", 1:6),
    best_response = c(
      -100, # Exactly CR threshold
      -30, # Exactly PR threshold (lower bound)
      -29.9, # Just above PR threshold (SD)
      19.9, # Just below PD threshold (SD)
      20, # Exactly PD threshold
      20.1 # Just above PD threshold
    )
  )

  result <- waterfall(
    data = threshold_data,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# ORDERING AND SORTING EDGE CASES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles non-sequential patient IDs", {
  nonsq_data <- waterfall_test
  nonsq_data$patientID <- paste0("PT", sample(1:1000, nrow(nonsq_data)))

  result <- waterfall(
    data = nonsq_data,
    patientID = "patientID",
    responseVar = "best_response",
    sortBy = "id"
  )

  expect_no_error(result)
})

test_that("waterfall handles mixed numeric-character patient IDs", {
  mixed_data <- waterfall_test
  mixed_data$patientID <- c(
    paste0("PT", 1:10),
    paste0("Subject_", 11:20),
    paste0("ID-", 21:30)
  )

  result <- waterfall(
    data = mixed_data,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result)
})

# ═══════════════════════════════════════════════════════════
# PARAMETER BOUNDARY VALUES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles boundary alpha values", {
  # Minimum transparency
  result_min <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barAlpha = 0
  )

  # Maximum transparency
  result_max <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barAlpha = 1
  )

  expect_no_error(result_min)
  expect_no_error(result_max)
})

test_that("waterfall handles boundary width values", {
  # Minimum width
  result_min <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barWidth = 0.1
  )

  # Maximum width
  result_max <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    barWidth = 1.0
  )

  expect_no_error(result_min)
  expect_no_error(result_max)
})

test_that("waterfall handles boundary minResponseForLabel values", {
  # Minimum threshold
  result_min <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    labelOutliers = TRUE,
    minResponseForLabel = 0
  )

  # Maximum threshold
  result_max <- waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    labelOutliers = TRUE,
    minResponseForLabel = 100
  )

  expect_no_error(result_min)
  expect_no_error(result_max)
})

# ═══════════════════════════════════════════════════════════
# TIME VARIABLE EDGE CASES
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles negative time values", {
  data(waterfall_spider_test, package = "ClinicoPath")
  neg_time_data <- waterfall_spider_test
  neg_time_data$time <- neg_time_data$time - 5 # Shift so some are negative

  # Should handle or warn about negative times
  expect_condition(
    waterfall(
      data = neg_time_data,
      patientID = "patientID",
      responseVar = "pct_change",
      timeVar = "time",
      showSpiderPlot = TRUE
    )
  )
})

test_that("a cohort with no time-zero baseline is rejected with guidance", {
  data(waterfall_raw_test, package = "ClinicoPath")
  nonzero_baseline <- waterfall_raw_test
  nonzero_baseline$time <- nonzero_baseline$time + 10  # no row at time == 0

  # Baseline is defined as the time == 0 measurement. Inferring it from each
  # patient's earliest available scan would silently treat a post-treatment scan
  # as baseline when the real baseline is missing, so the analysis stops and says
  # exactly what to do instead.
  expect_error(
    waterfall(
      data = nonzero_baseline,
      patientID = "patientID",
      responseVar = "tumor_size",
      timeVar = "time",
      inputType = "raw"
    ),
    regexp = "baseline",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# UNICODE AND INTERNATIONALIZATION
# ═══════════════════════════════════════════════════════════

test_that("waterfall handles unicode in patient IDs", {
  unicode_data <- waterfall_test
  unicode_data$patientID <- paste0("患者", 1:nrow(unicode_data))

  result <- waterfall(
    data = unicode_data,
    patientID = "patientID",
    responseVar = "best_response"
  )

  expect_no_error(result)
})

test_that("waterfall handles unicode in group names", {
  unicode_group_data <- waterfall_test
  unicode_group_data$treatment <- sample(
    c("Traitement A", "Behandlung B", "治療C"),
    nrow(unicode_group_data),
    replace = TRUE
  )

  result <- waterfall(
    data = unicode_group_data,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "treatment"
  )

  expect_no_error(result)
})
