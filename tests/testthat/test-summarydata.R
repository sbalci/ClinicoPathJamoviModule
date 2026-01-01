# Test for summarydata function

# Load testthat
library(testthat)

# Create a sample dataset for testing
set.seed(123)
test_data <- data.frame(
    age = rnorm(100, mean = 50, sd = 10),
    biomarker1 = rlnorm(100, meanlog = 2, sdlog = 0.5),
    biomarker2 = c(rnorm(95, mean = 10, sd = 2), 50, 52, 55, 58, 60),
    all_na = rep(NA, 100),
    single_value = rep(5, 100)
)

test_that("summarydata - Basic functionality", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Run the analysis
    results <- summarydata(
        data = test_data,
        vars = c("age", "biomarker1")
    )

    # Check that the results object is of the correct class
    expect_s3_class(results, "jmvcore::Results")

    # Check that the main table is produced
    expect_true(!is.null(results$text1))

})

test_that("summarydata - Statistical correctness", {
    # Run the analysis
    results <- summarydata(
        data = test_data,
        vars = "age",
        decimal_places = 3
    )

    # Extract the text output
    text_output <- results$text$content
    print(text_output)

    # Check for labels
    expect_true(grepl("Mean of <strong>age</strong> is:", text_output))
    expect_true(grepl("Median:", text_output))
    expect_true(grepl("Min:", text_output))
    expect_true(grepl("Max:", text_output))
})


test_that("summarydata - Distribution diagnostics", {
    # Run the analysis with distribution diagnostics
    results <- summarydata(
        data = test_data,
        vars = "age",
        distr = TRUE
    )

    # Extract the text output
    text_output <- results$text$content

    # Check for Shapiro-Wilk test results
    expect_true(grepl("Shapiro-Wilk p-value", text_output))
    # Check for skewness
    expect_true(grepl("Skewness", text_output))
    # Check for kurtosis
    expect_true(grepl("Kurtosis", text_output))
})

test_that("summarydata - Outlier detection", {
    # Run the analysis with outlier detection
    results <- summarydata(
        data = test_data,
        vars = "biomarker2",
        outliers = TRUE
    )

    # Check the outlier report
    outlier_report <- results$outlierReport$content
    expect_true(grepl("5 outliers detected", outlier_report))
    expect_true(grepl("50, 52, 55, 58, 60", outlier_report))
})

test_that("summarydata - Edge cases", {
    # Test with a variable that is all NAs
    results_na <- summarydata(
        data = test_data,
        vars = "all_na"
    )
    expect_true(grepl("Variable 'all_na' contains only missing values and could not be analyzed.", results_na$todo$content))
    expect_equal(results_na$text$content, NULL)

    # Test with a non-numeric variable
    test_data$non_numeric <- "a"
    results_non_numeric <- summarydata(
        data = test_data,
        vars = "non_numeric"
    )
    expect_true(grepl("Variable 'non_numeric' is not numeric and could not be analyzed.", results_non_numeric$todo$content))
    expect_equal(results_non_numeric$text$content, NULL)


    # Test with a variable that has a single value
    results_single <- summarydata(
        data = test_data,
        vars = "single_value"
    )
    text_output_single <- results_single$text$content
    expect_true(grepl("Mean of <strong>single_value</strong> is: 5", text_output_single))
    expect_true(grepl("sd.*0", text_output_single))
})
