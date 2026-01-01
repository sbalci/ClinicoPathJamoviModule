# Test suite for reportcat function
# Tests the current implementation with Notice API and modern architecture

testthat::test_that("reportcat works with standard categorical data", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Skip if required packages are not available
    testthat::skip_if_not_installed("ClinicoPath")

    # Create test data with categorical variables
    test_data <- data.frame(
        Gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female")),
        Grade = factor(c("I", "II", "III", "I", "II", "III", "I", "II")),
        Stage = factor(c("A", "B", "A", "B", "C", "A", "B", "C"))
    )

    # Test basic functionality
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("Gender", "Grade", "Stage")
        )
    })

    # Run and check structure
    result <- ClinicoPath::reportcat(
        data = test_data,
        vars = c("Gender", "Grade")
    )

    # Verify result structure
    testthat::expect_true("todo" %in% names(result))
    testthat::expect_true("text" %in% names(result))
    testthat::expect_true("text1" %in% names(result))
    testthat::expect_true("clinicalSummary" %in% names(result))
    testthat::expect_true("aboutAnalysis" %in% names(result))
    testthat::expect_true("assumptions" %in% names(result))
    testthat::expect_true("reportSentences" %in% names(result))
})

testthat::test_that("reportcat shows welcome message with no variables", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        Gender = factor(c("Male", "Female", "Male", "Female"))
    )

    # Test with empty variable selection
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = character(0)
        )
    })
})

testthat::test_that("reportcat handles sparse categories", {
    testthat::skip_if_not_installed("ClinicoPath")

    # Create data with many sparse categories (8 categories with n=1 each)
    test_data <- data.frame(
        Sparse = factor(c("A", "A", "B", "C", "D", "E", "F", "G", "H", "I"))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "Sparse"
        )
    })

    # Should generate output despite warnings
    result <- ClinicoPath::reportcat(
        data = test_data,
        vars = "Sparse"
    )
    testthat::expect_false(is.null(result$text))
})

testthat::test_that("reportcat warns for too many categories", {
    testthat::skip_if_not_installed("ClinicoPath")

    # Create data with >20 categories
    many_levels <- paste0("Cat", 1:25)
    test_data <- data.frame(
        ManyCategories = factor(sample(many_levels, 100, replace = TRUE))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "ManyCategories"
        )
    })
})

testthat::test_that("reportcat handles high missing data", {
    testthat::skip_if_not_installed("ClinicoPath")

    # Create data with >20% missing (7 out of 10)
    test_data <- data.frame(
        HighMissing = factor(c("Active", "Inactive", NA, NA, NA, "Active", NA, NA, "Inactive", NA))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "HighMissing"
        )
    })
})

testthat::test_that("reportcat handles special character variable names", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        "Grade (I/II/III)" = factor(c("I", "II", "III", "I", "II", "III", "I", "II")),
        "Status ($)" = factor(c("Active", "Inactive", "Active", "Inactive", "Active", "Inactive", "Active", "Inactive")),
        check.names = FALSE
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("Grade (I/II/III)", "Status ($)")
        )
    })
})

testthat::test_that("reportcat errors on empty dataset", {
    testthat::skip_if_not_installed("ClinicoPath")

    # Empty dataframe should trigger error Notice
    test_data <- data.frame(
        Gender = factor(character(0))
    )

    # Should handle gracefully with Notice (no R error)
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "Gender"
        )
    })
})

testthat::test_that("reportcat handles non-categorical variables", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        NumericVar = c(1, 2, 3, 4, 5),
        Gender = factor(c("Male", "Female", "Male", "Female", "Male"))
    )

    # Should handle gracefully - numeric will be treated as factor or filtered out
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("NumericVar", "Gender")
        )
    })
})

testthat::test_that("reportcat handles variables with all missing values", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        EmptyLevels = factor(c(NA, NA, NA, NA)),
        ValidVar = factor(c("A", "B", "A", "B"))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("EmptyLevels", "ValidVar")
        )
    })

    # Should continue with valid variable only
    result <- ClinicoPath::reportcat(
        data = test_data,
        vars = c("EmptyLevels", "ValidVar")
    )
    testthat::expect_false(is.null(result$text))
})

testthat::test_that("reportcat works with single variable", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        Status = factor(c("Active", "Inactive", "Active", "Inactive", "Active", "Inactive"))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "Status"
        )
    })
})

testthat::test_that("reportcat handles character variables", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        CharVar = c("Type1", "Type2", "Type1", "Type2", "Type1", "Type2"),
        stringsAsFactors = FALSE
    )

    # Character variables should be treated as categorical
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "CharVar"
        )
    })
})

testthat::test_that("reportcat handles large datasets efficiently", {
    testthat::skip_if_not_installed("ClinicoPath")

    # Create dataset with 1000 rows
    test_data <- data.frame(
        Var1 = factor(sample(c("A", "B", "C", "D", "E"), 1000, replace = TRUE)),
        Var2 = factor(sample(c("Low", "Medium", "High"), 1000, replace = TRUE)),
        Var3 = factor(sample(c("Yes", "No"), 1000, replace = TRUE))
    )

    # Measure execution time
    start_time <- Sys.time()
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("Var1", "Var2", "Var3")
        )
    })
    end_time <- Sys.time()

    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Should complete in reasonable time (<5 seconds)
    testthat::expect_true(execution_time < 5)
})

testthat::test_that("reportcat handles unbalanced categories", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        Unbalanced = factor(c(rep("Common", 90), rep("Rare", 10)))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = "Unbalanced"
        )
    })
})

testthat::test_that("reportcat handles missing variable names gracefully", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        Gender = factor(c("Male", "Female", "Male", "Female"))
    )

    # Should handle gracefully with Notice
    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("Gender", "NonExistent")
        )
    })
})

testthat::test_that("reportcat handles mixed valid and invalid variables", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        EmptyVar = factor(c(NA, NA, NA, NA)),
        ValidCat1 = factor(c("A", "B", "A", "B")),
        ValidCat2 = factor(c("X", "Y", "Z", "X"))
    )

    testthat::expect_no_error({
        result <- ClinicoPath::reportcat(
            data = test_data,
            vars = c("EmptyVar", "ValidCat1", "ValidCat2")
        )
    })

    # Should continue with valid variables
    result <- ClinicoPath::reportcat(
        data = test_data,
        vars = c("EmptyVar", "ValidCat1", "ValidCat2")
    )
    testthat::expect_false(is.null(result$text))
})

testthat::test_that("reportcat result structure is complete", {
    testthat::skip_if_not_installed("ClinicoPath")

    test_data <- data.frame(
        Category = factor(c("A", "B", "A", "C", "B", "A"))
    )

    result <- ClinicoPath::reportcat(
        data = test_data,
        vars = "Category"
    )

    # Check all expected output sections exist
    testthat::expect_true("todo" %in% names(result))
    testthat::expect_true("clinicalSummary" %in% names(result))
    testthat::expect_true("aboutAnalysis" %in% names(result))
    testthat::expect_true("assumptions" %in% names(result))
    testthat::expect_true("text" %in% names(result))
    testthat::expect_true("text1" %in% names(result))
    testthat::expect_true("reportSentences" %in% names(result))

    # Results should not be NULL
    testthat::expect_false(is.null(result$text))
    testthat::expect_false(is.null(result$clinicalSummary))
    testthat::expect_false(is.null(result$aboutAnalysis))
    testthat::expect_false(is.null(result$assumptions))
    testthat::expect_false(is.null(result$reportSentences))
})
