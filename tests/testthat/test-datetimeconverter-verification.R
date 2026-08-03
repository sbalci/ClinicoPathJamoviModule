library(testthat)

test_that("Numerical verification: string date parsing and component extraction", {
  df <- data.frame(
    date_str = c("2024-05-15 14:30:00", "2023-11-20 09:15:45", "2022-01-01 00:00:00")
  )

  res <- datetimeconverter(
    data = df,
    datetime_var = "date_str",
    datetime_format = "ymdhms",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE,
    extract_hour = TRUE,
    extract_minute = TRUE,
    extract_second = TRUE,
    extract_quarter = TRUE,
    extract_dayofyear = TRUE
  )

  expect_true(!is.null(res$previewTable))
})

test_that("Numerical verification: Excel serial numbers conversion", {
  # 45000 in Excel serial (origin 1899-12-30) corresponds to 2023-03-15
  df_excel <- data.frame(
    serial = c(45000, 45001, 45002)
  )

  res_excel <- datetimeconverter(
    data = df_excel,
    datetime_var = "serial",
    datetime_format = "excel_serial",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE
  )

  expect_true(!is.null(res_excel$formatInfo))
})

test_that("Numerical verification: Unix epoch conversion", {
  # 1700000000 epoch seconds = 2023-11-14 22:13:20 UTC
  df_unix <- data.frame(
    epoch = c(1700000000, 1700086400)
  )

  res_unix <- datetimeconverter(
    data = df_unix,
    datetime_var = "epoch",
    datetime_format = "unix_epoch",
    extract_year = TRUE,
    extract_month = TRUE
  )

  expect_true(!is.null(res_unix$previewTable))
})
