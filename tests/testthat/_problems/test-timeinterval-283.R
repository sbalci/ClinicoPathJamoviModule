# Extracted from test-timeinterval.R:283

# prequel ----------------------------------------------------------------------
library(testthat)

# test -------------------------------------------------------------------------
skip_if_not(file.exists(testthat::test_path("..", "..", "data", "timeinterval_landmark.rda")),
              "Landmark test data not available")
load(testthat::test_path("..", "..", "data", "timeinterval_landmark.rda"))
expect_error({
    result <- timeinterval(
      data = timeinterval_landmark,
      dx_date = "diagnosis_date",
      fu_date = "last_contact_date",
      time_format = "ymd",
      output_unit = "months",
      use_landmark = TRUE,
      landmark_time = 6
    )
  }, NA)
