library(testthat) 
library(jmvcore) 

# ====================================================================
# SETUP: Create a diverse test dataset 
# ====================================================================

test_data <- data.frame( 
    id = 1:15, 
    ymd_hms_var = c("2023-01-15 14:30:00", "2024-02-20 09:00:00", NA, "", "2023-03-10 12:00:00", rep(NA, 10)), 
    dmy_var = c("15/01/2023", "20/02/2024", "10/03/2023", NA, "", rep(NA, 10)), 
    mdy_var = c("01-15-2023", "02-20-2024", "03-10-2023", NA, "", rep(NA, 10)), 
    excel_var = c(44940.6041666667, 45341.375, NA, "", 45000, rep(NA, 10)), 
    unix_var = c(1673800200, 1708419600, NA, "", 1678459200, rep(NA, 10)), 
    ambiguous_dmy = c("01-02-2023", "02-03-2023", NA, "", "04-05-2023", rep(NA, 10)), 
    posix_var = as.POSIXct(c("2023-01-15 14:30:00", "2024-02-20 09:00:00", NA, NA, "2023-03-10 12:00:00"), tz = "UTC"), 
    date_var = as.Date(c("2023-01-15", "2024-02-20", NA, NA, "2023-03-10")) 
)

# ====================================================================
# TEST SUITE: datetimeconverter 
# ====================================================================

test_that("Auto-detection and basic parsing works correctly", { 
    opts <- list( 
        datetime_var = "ymd_hms_var", 
        datetime_format = "auto", 
        corrected_datetime_numeric = TRUE 
    ) 
    analysis <- datetimeconverterClass$new(options = opts, data = test_data) 
    analysis$.run() # Use the private .run method for direct testing 

    output_df <- analysis$results$corrected_datetime_numeric$asDF 
    expected_posix <- as.POSIXct("2023-01-15 14:30:00", tz = Sys.timezone())
    expect_equal(as.numeric(output_df[1, "corrected_datetime_numeric"]), as.numeric(expected_posix), tolerance = 1) 
    expect_true(is.na(output_df[3, "corrected_datetime_numeric"]))
    expect_true(is.na(output_df[4, "corrected_datetime_numeric"]))
})

test_that("Manual format specification works", { 
    opts <- list( 
        datetime_var = "ambiguous_dmy", 
        datetime_format = "dmy", 
        corrected_datetime_numeric = TRUE 
    ) 
    analysis <- datetimeconverterClass$new(options = opts, data = test_data) 
    analysis$.run() 

    output_df <- analysis$results$corrected_datetime_numeric$asDF 
    expected_posix <- as.POSIXct("2023-02-01 00:00:00", tz = Sys.timezone())
    expect_equal(as.numeric(output_df[1, "corrected_datetime_numeric"]), as.numeric(expected_posix), tolerance = 1)
})

test_that("Numeric (Excel, Unix) and special formats are handled", { 
    opts_excel <- list( 
        datetime_var = "excel_var", 
        datetime_format = "auto", 
        corrected_datetime_numeric = TRUE 
    ) 
    analysis_excel <- datetimeconverterClass$new(options = opts_excel, data = test_data)
    analysis_excel$.run()
    output_df_excel <- analysis_excel$results$corrected_datetime_numeric$asDF 
    expected_posix_excel <- as.POSIXct("2023-01-15 14:30:00", tz = "UTC")
    expect_equal(as.numeric(output_df_excel[1, "corrected_datetime_numeric"]), as.numeric(expected_posix_excel), tolerance = 1)

    opts_unix <- list( 
        datetime_var = "unix_var", 
        datetime_format = "auto", 
        corrected_datetime_numeric = TRUE 
    )
    analysis_unix <- datetimeconverterClass$new(options = opts_unix, data = test_data)
    analysis_unix$.run()
    output_df_unix <- analysis_unix$results$corrected_datetime_numeric$asDF 
    expect_equal(as.numeric(output_df_unix[1, "corrected_datetime_numeric"]), 1673800200, tolerance = 1)
})

test_that("Component extraction is accurate", { 
    opts <- list( 
        datetime_var = "ymd_hms_var", 
        datetime_format = "auto", 
        year_out = TRUE, month_out = TRUE, day_out = TRUE, hour_out = TRUE, 
        minute_out = TRUE, second_out = TRUE, dayname_out = TRUE, 
        weeknum_out = TRUE, quarter_out = TRUE, dayofyear_out = TRUE 
    )
    analysis <- datetimeconverterClass$new(options = opts, data = test_data)
    analysis$.run()

    results <- analysis$results 
    expect_equal(results$year_out$asDF[1, "year_out"], 2023)
    expect_equal(results$month_out$asDF[1, "month_out"], 1)
    expect_equal(results$day_out$asDF[1, "day_out"], 15)
    expect_equal(results$hour_out$asDF[1, "hour_out"], 14)
    expect_equal(results$minute_out$asDF[1, "minute_out"], 30)
    expect_equal(results$second_out$asDF[1, "second_out"], 0)
    expect_equal(results$dayname_out$asDF[1, "dayname_out"], "Sunday")
    expect_equal(results$weeknum_out$asDF[1, "weeknum_out"], 2)
    expect_equal(results$quarter_out$asDF[1, "quarter_out"], 1)
    expect_equal(results$dayofyear_out$asDF[1, "dayofyear_out"], 15)
    expect_true(is.na(results$year_out$asDF[3, "year_out"]))
})

test_that("Dual output (numeric and character) works correctly", { 
    opts <- list( 
        datetime_var = "ymd_hms_var", 
        datetime_format = "auto", 
        corrected_datetime_char = TRUE, 
        corrected_datetime_numeric = TRUE 
    )
    analysis <- datetimeconverterClass$new(options = opts, data = test_data)
    analysis$.run()

    char_df <- analysis$results$corrected_datetime_char$asDF 
    expect_type(char_df[1, "corrected_datetime_char"], "character")
    expect_equal(char_df[1, "corrected_datetime_char"], "2023-01-15 14:30:00")

    num_df <- analysis$results$corrected_datetime_numeric$asDF 
    expected_posix <- as.POSIXct("2023-01-15 14:30:00", tz = Sys.timezone())
    expect_type(num_df[1, "corrected_datetime_numeric"], "double")
    expect_equal(num_df[1, "corrected_datetime_numeric"], as.numeric(expected_posix), tolerance = 1)
})

cat("\nDateTime Converter tests passed successfully!\n")
