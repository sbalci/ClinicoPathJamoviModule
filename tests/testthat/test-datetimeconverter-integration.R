# ═══════════════════════════════════════════════════════════
# Integration Tests: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the datetimeconverter jamovi function

library(testthat)
library(ClinicoPath)

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
  expect_s3_class(result1, "datetimeconverterClass")
  expect_s3_class(result2, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Excel import → conversion → component extraction", {
  # Step 1: Basic Excel conversion
  basic <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial"
  )
  expect_s3_class(basic, "datetimeconverterClass")

  # Step 2: Add component extraction
  with_components <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE
  )
  expect_s3_class(with_components, "datetimeconverterClass")

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
  expect_s3_class(complete, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Clinical data processing", {
  # Step 1: Convert admission dates
  admission <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
    datetime_format = "ymd"
  )
  expect_s3_class(admission, "datetimeconverterClass")

  # Step 2: Extract temporal features for analysis
  temporal_features <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_dayname = TRUE,
    extract_weeknum = TRUE,
    extract_quarter = TRUE
  )
  expect_s3_class(temporal_features, "datetimeconverterClass")

  # Step 3: Process surgery datetime with timezone
  surgery <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "SurgeryDateTime",
    datetime_format = "ymdhms",
    timezone = "America/New_York",
    extract_hour = TRUE,
    extract_dayname = TRUE
  )
  expect_s3_class(surgery, "datetimeconverterClass")
})

test_that("datetimeconverter handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(datetimeconverter_test, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- datetimeconverter(
    data = csv_data,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )

  expect_s3_class(result, "datetimeconverterClass")

  # Clean up
  unlink(temp_csv)
})

test_that("datetimeconverter handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(datetimeconverter_test, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- datetimeconverter(
    data = as.data.frame(xlsx_data),
    datetime_var = "date_ymd",
    datetime_format = "auto"
  )

  expect_s3_class(result, "datetimeconverterClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("datetimeconverter handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(datetimeconverter_test)

  result_tibble <- datetimeconverter(
    data = tibble_data,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )

  expect_s3_class(result_tibble, "datetimeconverterClass")

  # Test with data.frame
  df_data <- as.data.frame(datetimeconverter_test)

  result_df <- datetimeconverter(
    data = df_data,
    datetime_var = "date_ymd",
    datetime_format = "ymd"
  )

  expect_s3_class(result_df, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Multi-timezone clinical trial", {
  data(datetimeconverter_timezone, package = "ClinicoPath")

  # Convert UTC timestamps to local times
  utc_conversion <- datetimeconverter(
    data = datetimeconverter_timezone,
    datetime_var = "datetime_utc",
    datetime_format = "ymdhms",
    timezone = "UTC"
  )
  expect_s3_class(utc_conversion, "datetimeconverterClass")

  # Convert to New York time
  ny_conversion <- datetimeconverter(
    data = datetimeconverter_timezone,
    datetime_var = "datetime_utc",
    datetime_format = "ymdhms",
    timezone = "America/New_York",
    extract_hour = TRUE
  )
  expect_s3_class(ny_conversion, "datetimeconverterClass")

  # Convert to London time
  london_conversion <- datetimeconverter(
    data = datetimeconverter_timezone,
    datetime_var = "datetime_utc",
    datetime_format = "ymdhms",
    timezone = "Europe/London",
    extract_hour = TRUE
  )
  expect_s3_class(london_conversion, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Legacy system migration", {
  # Migrate from Excel serial dates to standard format
  excel_migration <- datetimeconverter(
    data = datetimeconverter_excel,
    datetime_var = "excel_serial_date",
    datetime_format = "excel_serial",
    output_as_text = TRUE,
    custom_output_format = "%Y-%m-%d"
  )
  expect_s3_class(excel_migration, "datetimeconverterClass")

  # Migrate from Unix timestamps
  data(datetimeconverter_unix, package = "ClinicoPath")

  unix_migration <- datetimeconverter(
    data = datetimeconverter_unix,
    datetime_var = "unix_timestamp",
    datetime_format = "unix_epoch",
    output_as_text = TRUE,
    custom_output_format = "%Y-%m-%d %H:%M:%S"
  )
  expect_s3_class(unix_migration, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Epidemiological week analysis", {
  # Extract week numbers for disease surveillance
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_weeknum = TRUE,
    extract_month = TRUE
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Seasonal analysis", {
  # Extract temporal components for seasonal pattern analysis
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
    datetime_format = "ymd",
    extract_month = TRUE,
    extract_monthname = TRUE,
    extract_quarter = TRUE,
    extract_dayofyear = TRUE
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter integration: Clinical scheduling", {
  # Extract day of week for scheduling analysis
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "SurgeryDateTime",
    datetime_format = "ymdhms",
    extract_dayname = TRUE,
    extract_hour = TRUE
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Data quality assessment", {
  # Use quality metrics to identify conversion issues
  result <- datetimeconverter(
    data = datetimeconverter_test,
    datetime_var = "date_ymd",
    datetime_format = "auto",
    show_quality_metrics = TRUE,
    show_preview = TRUE,
    preview_rows = 20
  )
  expect_s3_class(result, "datetimeconverterClass")
})

test_that("datetimeconverter workflow: Auto-format detection workflow", {
  data(datetimeconverter_mixed, package = "ClinicoPath")

  # Step 1: Try auto-detection
  auto_result <- datetimeconverter(
    data = datetimeconverter_mixed,
    datetime_var = "mixed_dates",
    datetime_format = "auto",
    show_quality_metrics = TRUE
  )
  expect_s3_class(auto_result, "datetimeconverter")

  # Step 2: Manual format specification if auto fails
  manual_result <- datetimeconverter(
    data = datetimeconverter_mixed,
    datetime_var = "dates_ymd",
    datetime_format = "ymd"
  )
  expect_s3_class(manual_result, "datetimeconverterClass")
})

test_that("datetimeconverter handles comprehensive publication workflow", {
  # Complete analysis for research publication
  result <- datetimeconverter(
    data = datetimeconverter_clinical,
    datetime_var = "AdmissionDate",
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
    output_as_text = TRUE,
    custom_output_format = "%Y-%m-%d",
    show_quality_metrics = TRUE,
    show_preview = TRUE,
    preview_rows = 10
  )
  expect_s3_class(result, "datetimeconverterClass")
})
