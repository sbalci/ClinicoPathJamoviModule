# Extracted from test-timeinterval.R:263

# prequel ----------------------------------------------------------------------
library(testthat)

# test -------------------------------------------------------------------------
skip_if_not(file.exists(testthat::test_path("..", "..", "data", "timeinterval_european_dates.rda")),
              "European dates test data not available")
load(testthat::test_path("..", "..", "data", "timeinterval_european_dates.rda"))
expect_error({
    result <- timeinterval(
      data = timeinterval_european_dates,
      dx_date = "diagnosis_date_dmy", 
      fu_date = "last_visit_dmy",
      time_format = "dmy",
      output_unit = "months"
    )
  }, NA)
expect_s3_class(result, "timeintervalClass")
