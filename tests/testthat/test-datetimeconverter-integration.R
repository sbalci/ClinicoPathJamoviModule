# ═══════════════════════════════════════════════════════════
# Integration Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the datetimeconverter jamovi function

library(testthat)

# Load test data
data(datetimeconverter_test, package = "ClinicoPath")
data(datetimeconverter_clinical, package = "ClinicoPath")
data(datetimeconverter_excel, package = "ClinicoPath")

test_that("datetimeconverter produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE
  )

  result2 <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE
  )

  # Results should be identical (no randomness)
  expect_s3_class(result1, "datetimeconverterResults")
  expect_s3_class(result2, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Excel import -> conversion -> component extraction", {
  # Step 1: Basic Excel conversion
  basic <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial"
  )
  expect_s3_class(basic, "datetimeconverterResults")

  # Step 2: Add component extraction
  with_components <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE
  )
  expect_s3_class(with_components, "datetimeconverterResults")

  # Step 3: Complete analysis
  complete <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_monthname = TRUE,
    extract_day = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE,
    show_quality_metrics = TRUE
  )
  expect_s3_class(complete, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Clinical data processing", {
  # Step 1: Convert surgery dates
  surgery_res <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd"
  )
  expect_s3_class(surgery_res, "datetimeconverterResults")

  # Step 2: Extract temporal features for analysis
  temporal_features <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE
  )
  expect_s3_class(temporal_features, "datetimeconverterResults")

  # Step 3: Process lab timestamp with timezone
  lab_res <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "lab_timestamp",
    datetime_format = "ymdhms",
    timezone = "America/New_York",
    extract_hour = TRUE,
    extract_dayname = TRUE
  )
  expect_s3_class(lab_res, "datetimeconverterResults")
})

test_that("datetimeconverter handles data from CSV import", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(datetimeconverter_test, temp_csv, row.names = FALSE)

  csv_data <- read.csv(temp_csv)

  result <- datetimeconverter(
    data = csv_data,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )

  expect_s3_class(result, "datetimeconverterResults")

  unlink(temp_csv)
})

test_that("datetimeconverter handles data from Excel import", {
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(datetimeconverter_test, temp_xlsx)

  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- datetimeconverter(
    data = as.data.frame(xlsx_data),
    datetime_var = "date_ymd",
    datetime_format = "auto"
  )

  expect_s3_class(result, "datetimeconverterResults")

  unlink(temp_xlsx)
})

test_that("datetimeconverter handles different data structures consistently", {
  library(tibble)
  tibble_data <- as_tibble(datetimeconverter_test)

  result_tibble <- datetimeconverter(
    data = tibble_data,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )

  expect_s3_class(result_tibble, "datetimeconverterResults")

  df_data <- as.data.frame(datetimeconverter_test)

  result_df <- datetimeconverter(
    data = df_data,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )

  expect_s3_class(result_df, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Legacy system migration", {
  excel_migration <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE
  )
  expect_s3_class(excel_migration, "datetimeconverterResults")

  data(datetimeconverter_unix, package = "ClinicoPath")

  unix_migration <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_timestamp",
    datetime_format = "unix_epoch",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE
  )
  expect_s3_class(unix_migration, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Epidemiological week analysis", {
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_weeknum = TRUE,
    extract_month = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Seasonal analysis", {
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd",
    extract_month = TRUE,
    extract_monthname = TRUE,
    extract_quarter = TRUE,
    extract_dayofyear = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter integration: Clinical scheduling", {
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "lab_timestamp",
    datetime_format = "ymdhms",
    extract_dayname = TRUE,
    extract_hour = TRUE
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Data quality assessment", {
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "auto",
    show_quality_metrics = TRUE,
    preview_rows = 20
  )
  expect_s3_class(result, "datetimeconverterResults")
})

test_that("datetimeconverter workflow: Auto-format detection workflow", {
  data(datetimeconverter_mixed, package = "ClinicoPath")

  auto_result <- datetimeconverter(
    data = datetimeconverter_mixed,
    datetime_var = "mixed_datetime",
    datetime_format = "auto",
    show_quality_metrics = TRUE
  )
  expect_s3_class(auto_result, "datetimeconverterResults")

  manual_result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(manual_result, "datetimeconverterResults")
})

test_that("datetimeconverter handles comprehensive publication workflow", {
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "surgery_date",
    datetime_format = "ymd",
    timezone = "UTC",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_monthname = TRUE,
    extract_day = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE,
    extract_dayofyear = TRUE,
    show_quality_metrics = TRUE,
    preview_rows = 10
  )
  expect_s3_class(result, "datetimeconverterResults")
})
