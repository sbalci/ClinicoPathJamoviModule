
# Test script for ihcheterogeneity analysis

# Load necessary libraries
library(testthat)
library(jmvcore)
library(devtools)
devtools::load_all('.')

# Create a sample dataset for testing
sample_data <- read.csv("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ihc_heterogeneity.csv")

# Convert spatial_region to factor
sample_data$spatial_region <- as.factor(sample_data$spatial_region)

test_that("ihcheterogeneity runs with default options", {
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        wholesection = "whole_section_ki67",
        biopsy1 = "biopsy1_ki67",
        biopsy2 = "biopsy2_ki67",
        biopsy3 = "biopsy3_ki67",
        biopsy4 = "biopsy4_ki67",
        biopsies = c("additional_biopsy1", "additional_biopsy2"),
        spatial_id = "spatial_region"
    )

    # Check if the main results tables are produced
    expect_true("reproducibilitytable" %in% names(results))
    expect_true("samplingbiastable" %in% names(results))
    expect_true("variancetable" %in% names(results))
    expect_true("spatialanalysistable" %in% names(results))

    # Check if plots are generated
    expect_true(!is.null(results$biopsyplot$state))
    expect_true(!is.null(results$variabilityplot$state))
    expect_true(!is.null(results$spatialplot$state))
})

test_that("ihcheterogeneity handles inter-regional analysis", {
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        biopsy1 = "biopsy1_ki67",
        biopsy2 = "biopsy2_ki67",
        biopsy3 = "biopsy3_ki67"
    )

    # `wholesection` is not provided, so bias table should be empty
    expect_equal(nrow(results$samplingbiastable$asDF()), 0)
})


test_that("ihcheterogeneity handles missing data", {
    # Introduce some missing data for testing
    sample_data_missing <- sample_data
    sample_data_missing$biopsy2_ki67[c(3, 8, 15)] <- NA
    sample_data_missing$whole_section_ki67[5] <- NA

    # The function should run without errors with missing data
    expect_no_error({
        ClinicoPath::ihcheterogeneity(
            data = sample_data_missing,
            wholesection = "whole_section_ki67",
            biopsy1 = "biopsy1_ki67",
            biopsy2 = "biopsy2_ki67"
        )
    })
})

test_that("ihcheterogeneity misuse detection works", {
    # Test with a small sample size
    small_data <- sample_data[1:5, ]
    results <- ClinicoPath::ihcheterogeneity(
        data = small_data,
        wholesection = "whole_section_ki67",
        biopsy1 = "biopsy1_ki67"
    )
    # Expect a warning in the interpretation
    expect_true(stringr::str_detect(results$interpretation$content, "Small sample size"))

    # Test with constant values
    constant_data <- sample_data
    constant_data$biopsy1_ki67 <- 100
    results <- ClinicoPath::ihcheterogeneity(
        data = constant_data,
        wholesection = "whole_section_ki67",
        biopsy1 = "biopsy1_ki67"
    )
    expect_true(stringr::str_detect(results$interpretation$content, "no variability"))
})

test_that("ihcheterogeneity plain language summary works", {
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        wholesection = "whole_section_ki67",
        biopsy1 = "biopsy1_ki67",
        biopsy2 = "biopsy2_ki67",
        showSummary = TRUE
    )
    expect_true(results$summary$visible)
    expect_true(nchar(results$summary$content) > 0)
})

test_that("ihcheterogeneity glossary works", {
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        wholesection = "whole_section_ki67",
        biopsy1 = "biopsy1_ki67",
        biopsy2 = "biopsy2_ki67",
        showGlossary = TRUE
    )
    expect_true(results$glossary$visible)
    expect_true(nchar(results$glossary$content) > 0)
})

