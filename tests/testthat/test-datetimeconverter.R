test_that("prepareDatetimeInput trims whitespace and blanks", {
  analysis <- ClinicoPath:::datetimeconverterClass$new(
    options = ClinicoPath:::datetimeconverterOptions$new(datetime_var = "dt"),
    data = data.frame(dt = character())
  )

  input <- c("2023-01-01", " 2023-02-01  ", "")
  prep <- analysis$.__enclos_env__$private$.prepareDatetimeInput(input)

  expect_equal(prep$original_display, c("2023-01-01", "2023-02-01", NA))
  expect_equal(prep$parsing_vector, c("2023-01-01", "2023-02-01", NA))
  expect_false(prep$already_parsed)
  expect_match(prep$notes, "Converted 1 blank entries to missing.")
})

test_that("prepareDatetimeInput detects Excel serial numbers", {
  analysis <- ClinicoPath:::datetimeconverterClass$new(
    options = ClinicoPath:::datetimeconverterOptions$new(datetime_var = "dt"),
    data = data.frame(dt = numeric())
  )

  excel_vals <- c(45123, 45124.5, NA)
  prep <- analysis$.__enclos_env__$private$.prepareDatetimeInput(excel_vals)

  expect_true(prep$already_parsed)
  expect_equal(prep$format_hint, "excel_serial")
  expect_true(inherits(prep$parsed_dates, "POSIXct"))
  expect_equal(prep$original_display, c("45123", "45124.5", NA))
})

test_that("prepareDatetimeInput detects Unix epoch seconds", {
  analysis <- ClinicoPath:::datetimeconverterClass$new(
    options = ClinicoPath:::datetimeconverterOptions$new(datetime_var = "dt"),
    data = data.frame(dt = numeric())
  )

  unix_vals <- c(1609459200, 1609545600, NA)
  prep <- analysis$.__enclos_env__$private$.prepareDatetimeInput(unix_vals)

  expect_true(prep$already_parsed)
  expect_equal(prep$format_hint, "unix_epoch")
  expect_true(inherits(prep$parsed_dates, "POSIXct"))
  expect_equal(prep$original_display, c("1609459200", "1609545600", NA))
})

test_that("assessQuality reports successes and failures correctly", {
  analysis <- ClinicoPath:::datetimeconverterClass$new(
    options = ClinicoPath:::datetimeconverterOptions$new(datetime_var = "dt"),
    data = data.frame(dt = character())
  )

  original <- c("2020-01-01", "2020-02-01", NA, "bad-value")
  parsed <- as.POSIXct(c("2020-01-01", "2020-02-01", NA, NA), tz = "UTC")

  quality <- analysis$.__enclos_env__$private$.assessQuality(original, parsed)

  expect_equal(quality$total_observations, 4)
  expect_equal(quality$original_missing, 1)
  expect_equal(quality$successfully_parsed, 2)
  expect_equal(quality$failed_parsing, 1)
  expect_equal(quality$success_rate, round(2 / 3 * 100, 2))
})
