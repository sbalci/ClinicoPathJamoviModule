# Extracted from test-timeinterval.R:243

# prequel ----------------------------------------------------------------------
library(testthat)

# test -------------------------------------------------------------------------
skip_if_not(file.exists(testthat::test_path("..", "..", "data", "timeinterval_clinical_trial.rda")), 
              "Clinical trial test data not available")
load(testthat::test_path("..", "..", "data", "timeinterval_clinical_trial.rda"))
expect_error({
    result <- timeinterval(
      data = timeinterval_clinical_trial,
      dx_date = "enrollment_date_ymd",
      fu_date = "followup_date_ymd",
      time_format = "ymd",
      output_unit = "months"
    )
  }, NA)
expect_s3_class(result, "timeintervalClass")
