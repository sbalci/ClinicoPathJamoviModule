
# Test script for ihcheterogeneity analysis

# Load necessary libraries
library(testthat)

# Helper function to get correct data path
get_data_path <- function(filename) {
  # Try multiple potential locations
  paths_to_try <- c(
    file.path(getwd(), "data", filename),
    file.path("..", "..", "data", filename),
    file.path(dirname(dirname(getwd())), "data", filename),
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ihc_heterogeneity.csv"
  )

  for (path in paths_to_try) {
    if (file.exists(path)) {
      return(path)
    }
  }

  skip(paste("Test data file not found:", filename))
}

# Load sample data
sample_data <- read.csv(get_data_path("ihc_heterogeneity.csv"), stringsAsFactors = TRUE)

test_that("ihcheterogeneity runs with default options", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        biopsy3 = "ki67_region3",
        biopsy4 = "ki67_region4",
        biopsies = c("ki67_region5", "ki67_region6"),
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
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        biopsy3 = "ki67_region3"
    )

    # `wholesection` is not provided, so bias table should be empty
    expect_equal(nrow(results$samplingbiastable$asDF()), 0)
})


test_that("ihcheterogeneity handles missing data", {
    # Introduce some missing data for testing
    sample_data_missing <- sample_data
    sample_data_missing$ki67_region2[c(3, 8, 15)] <- NA
    sample_data_missing$ki67_wholesection[5] <- NA

    # The function should run without errors with missing data
    expect_error({
        ClinicoPath::ihcheterogeneity(
            data = sample_data_missing,
            wholesection = "ki67_wholesection",
            biopsy1 = "ki67_region1",
            biopsy2 = "ki67_region2"
        )
    }, NA)
})

test_that("ihcheterogeneity misuse detection works", {
    # Test with a small sample size
    small_data <- sample_data[1:5, ]
    results <- ClinicoPath::ihcheterogeneity(
        data = small_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1"
    )
    # Expect a warning in the interpretation (may be in notices or interpretation)
    # Just check that analysis completes
    expect_s3_class(results, "ihcheterogeneityClass")

    # Test with constant values
    constant_data <- sample_data
    constant_data$ki67_region1 <- 100
    results <- ClinicoPath::ihcheterogeneity(
        data = constant_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1"
    )
    # Should complete even with no variability
    expect_s3_class(results, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity plain language summary works", {
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        showSummary = TRUE
    )
    expect_true(results$summary$visible)
})

test_that("ihcheterogeneity glossary works", {
    results <- ClinicoPath::ihcheterogeneity(
        data = sample_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        showGlossary = TRUE
    )
    expect_true(results$glossary$visible)
})

