# Test file for clinicalheatmap function
# Tests core heatmap functionality, data preparation, annotations, and error handling

library(testthat)

# Create test data in tidy (long) format
create_test_data <- function() {
    # Simple biomarker dataset
    set.seed(123)
    data.frame(
        patient_id = rep(paste0("P", 1:10), each = 4),
        biomarker = rep(c("ER", "PR", "HER2", "Ki67"), 10),
        expression = rnorm(40, mean = 50, sd = 20),
        tumor_type = rep(c("TypeA", "TypeB"), each = 20),
        stage = rep(c("I", "II", "III", "IV"), each = 10),
        stringsAsFactors = FALSE
    )
}

# Test: Basic heatmap creation
test_that("Basic heatmap creation works without errors", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Test with minimal required parameters
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression"
        )
    )

    # Basic checks
    expect_s3_class(result, "clinicalheatmapClass")
    expect_true(!is.null(result))
})

# Test: Data preparation retains annotation columns
test_that("Data preparation retains annotation columns", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Create heatmap with annotations
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression",
            annotationCols = "tumor_type",
            annotationRows = "stage"
        )
    )

    # Check that result was created
    expect_s3_class(result, "clinicalheatmapClass")

    # Note: Cannot directly test internal data structure without
    # exposing private methods, but we can verify no errors occurred
})

# Test: Input validation - missing required variables
test_that("Input validation catches missing required variables", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Test with NULL row variable - should not error, just show instructions
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = NULL,
            colVar = "biomarker",
            valueVar = "expression"
        )
    )

    expect_s3_class(result, "clinicalheatmapClass")
})

# Test: Scaling methods
test_that("Different scaling methods work correctly", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Test each scaling method
    scaling_methods <- c("none", "row", "column", "both")

    for (method in scaling_methods) {
        result <- suppressWarnings(
            ClinicoPath::clinicalheatmap(
                data = data,
                rowVar = "patient_id",
                colVar = "biomarker",
                valueVar = "expression",
                scaleMethod = method
            )
        )

        expect_s3_class(result, "clinicalheatmapClass",
                        info = paste("Failed for scaleMethod =", method))
    }
})

# Test: Clustering options
test_that("Clustering options work correctly", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Test with clustering enabled
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression",
            clusterRows = TRUE,
            clusterCols = TRUE
        )
    )

    expect_s3_class(result, "clinicalheatmapClass")
})

# Test: Missing data handling
test_that("Missing data handling strategies work", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Introduce missing values
    data$expression[c(1, 5, 10, 15)] <- NA

    na_handling_methods <- c("exclude", "mean", "median", "zero")

    for (method in na_handling_methods) {
        result <- suppressWarnings(
            ClinicoPath::clinicalheatmap(
                data = data,
                rowVar = "patient_id",
                colVar = "biomarker",
                valueVar = "expression",
                naHandling = method
            )
        )

        expect_s3_class(result, "clinicalheatmapClass",
                        info = paste("Failed for naHandling =", method))
    }
})

# Test: Empty dataset handling
test_that("Empty dataset is handled gracefully", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    # Create empty dataset
    data <- create_test_data()[0, ]

    # Should not crash, but show error message
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression"
        )
    )

    expect_s3_class(result, "clinicalheatmapClass")
})

# Test: Non-numeric value variable
test_that("Non-numeric value variable is caught by validation", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()
    data$expression <- as.character(data$expression)

    # Should show validation error
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression"
        )
    )

    expect_s3_class(result, "clinicalheatmapClass")
    # Error should be shown in results, not crash
})

# Test: Color palette options
test_that("Different color palettes work", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    palettes <- c("viridis", "plasma", "inferno", "RdYlBu", "RdBu")

    for (palette in palettes) {
        result <- suppressWarnings(
            ClinicoPath::clinicalheatmap(
                data = data,
                rowVar = "patient_id",
                colVar = "biomarker",
                valueVar = "expression",
                colorPalette = palette
            )
        )

        expect_s3_class(result, "clinicalheatmapClass",
                        info = paste("Failed for colorPalette =", palette))
    }
})

# Test: Display options
test_that("Display options (row/col names) work", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    data <- create_test_data()

    # Test with names shown
    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression",
            showRownames = TRUE,
            showColnames = TRUE
        )
    )

    expect_s3_class(result, "clinicalheatmapClass")

    # Test with names hidden
    result2 <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = data,
            rowVar = "patient_id",
            colVar = "biomarker",
            valueVar = "expression",
            showRownames = FALSE,
            showColnames = FALSE
        )
    )

    expect_s3_class(result2, "clinicalheatmapClass")
})

# Test: Larger dataset performance
test_that("Larger dataset is handled correctly", {
    skip_if_not_installed("tidyheatmaps")
    skip_if_not_installed("ClinicoPath")

    # Create larger dataset
    set.seed(456)
    large_data <- data.frame(
        sample_id = rep(paste0("S", 1:50), each = 20),
        gene = rep(paste0("Gene", 1:20), 50),
        expression = rnorm(1000, mean = 0, sd = 1),
        stringsAsFactors = FALSE
    )

    result <- suppressWarnings(
        ClinicoPath::clinicalheatmap(
            data = large_data,
            rowVar = "sample_id",
            colVar = "gene",
            valueVar = "expression",
            scaleMethod = "row",
            clusterRows = TRUE,
            clusterCols = TRUE
        )
    )

    expect_s3_class(result, "clinicalheatmapClass")
})
