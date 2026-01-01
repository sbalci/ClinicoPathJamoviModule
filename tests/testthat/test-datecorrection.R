test_that("datecorrection function works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    testthat::skip_on_cran()
    
    # Test basic function existence
    expect_true(exists("datecorrection"))
    
    # Test with histopathology data containing date variables
    data("histopathology", package = "ClinicoPath")
    
    # Test that function doesn't error with basic inputs
    expect_no_error({
        result <- datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate", "SurgeryDate")
        )
    })
    
    # Test that function returns results object
    result <- datecorrection(
        data = histopathology,
        date_vars = c("LastFollowUpDate", "SurgeryDate")
    )
    
    expect_s3_class(result, "Group")
    expect_true(length(result) > 0)
    
    # Test different correction methods
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            correction_method = "datefixr"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            correction_method = "anytime"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            correction_method = "lubridate"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            correction_method = "consensus"
        )
    })
    
    # Test different date formats
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            date_format = "dmy"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            date_format = "mdy"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            date_format = "ymd"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            date_format = "auto"
        )
    })
    
    # Test imputation parameters
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            day_impute = 15,
            month_impute = 6
        )
    })
    
    # Test display options
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            show_correction_table = TRUE,
            show_quality_assessment = TRUE,
            show_format_analysis = TRUE,
            show_correction_summary = TRUE,
            show_interpretation = TRUE
        )
    })
    
    # Test Excel handling option
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            handle_excel = TRUE
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            handle_excel = FALSE
        )
    })
    
    # Test timezone option
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            timezone = "UTC"
        )
    })
    
    expect_no_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            timezone = "America/New_York"
        )
    })
})

test_that("datecorrection handles edge cases", {
    testthat::skip_on_cran()
    
    # Test with empty data
    empty_data <- data.frame()
    expect_error({
        datecorrection(
            data = empty_data,
            date_vars = c()
        )
    }, class = "error")
    
    # Test with missing date variables
    data("histopathology", package = "ClinicoPath")
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c()
        )
    }, class = "error")
    
    # Test with non-existent variables
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("NonExistentDateVar")
        )
    }, class = "error")
})

test_that("datecorrection validates parameters", {
    testthat::skip_on_cran()
    
    data("histopathology", package = "ClinicoPath")
    
    # Test invalid day_impute values
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            day_impute = 0
        )
    })
    
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            day_impute = 32
        )
    })
    
    # Test invalid month_impute values
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            month_impute = 0
        )
    })
    
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            month_impute = 13
        )
    })
    
    # Test invalid correction_method
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            correction_method = "invalid_method"
        )
    })
    
    # Test invalid date_format
    expect_error({
        datecorrection(
            data = histopathology,
            date_vars = c("LastFollowUpDate"),
            date_format = "invalid_format"
        )
    })
})

test_that("datecorrection output structure is correct", {
    testthat::skip_on_cran()
    
    data("histopathology", package = "ClinicoPath")
    
    result <- datecorrection(
        data = histopathology,
        date_vars = c("LastFollowUpDate", "SurgeryDate")
    )
    
    # Check that result has expected structure
    expect_s3_class(result, "Group")
    
    # Check for expected output components
    expect_true("todo" %in% names(result))
    expect_true("correction_table" %in% names(result))
    expect_true("quality_assessment" %in% names(result))
    expect_true("format_analysis" %in% names(result))
    expect_true("correction_summary" %in% names(result))
    expect_true("interpretation" %in% names(result))
    
    # Check that each component is an HTML object
    expect_s3_class(result$todo, "Html")
    expect_s3_class(result$correction_table, "Html")
    expect_s3_class(result$quality_assessment, "Html")
    expect_s3_class(result$format_analysis, "Html")
    expect_s3_class(result$correction_summary, "Html")
    expect_s3_class(result$interpretation, "Html")
})
