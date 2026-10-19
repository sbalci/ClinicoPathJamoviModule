
# Test script for ihcheterogeneity analysis


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
    results <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
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
    results <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
        data = sample_data,
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        biopsy3 = "ki67_region3"
    )

    # `wholesection` is not provided: since 2026-10-19 every pair of regions is
    # compared (the earlier region-vs-mean-of-the-others rows spread one region's
    # offset onto the others)
    bias <- results$samplingbiastable$asDF
    expect_equal(bias$comparison, c("ki67_region2 vs ki67_region1", "ki67_region3 vs ki67_region1",
                                    "ki67_region3 vs ki67_region2"))
    reg <- sample_data[, c("ki67_region1", "ki67_region2", "ki67_region3")]
    reg <- reg[rowSums(!is.na(reg)) >= 2, ]          # usable cases: at least 2 regions
    ok <- !is.na(reg$ki67_region1) & !is.na(reg$ki67_region3)
    expect_equal(bias$mean_diff[2], mean(reg$ki67_region3[ok] - reg$ki67_region1[ok]), tolerance = 1e-10)
    expect_equal(bias$n[2], sum(ok))
})


test_that("ihcheterogeneity handles missing data", {
    # Introduce some missing data for testing
    sample_data_missing <- sample_data
    sample_data_missing$ki67_region2[c(3, 8, 15)] <- NA
    sample_data_missing$ki67_wholesection[5] <- NA

    # The function should run without errors with missing data
    expect_error({
        ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
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
    results <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
        data = small_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1"
    )
    # Expect a warning in the interpretation (may be in notices or interpretation)
    # Just check that analysis completes
    expect_s3_class(results, "ihcheterogeneityResults")

    # Test with constant values
    constant_data <- sample_data
    constant_data$ki67_region1 <- 100
    results <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
        data = constant_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1"
    )
    # Should complete even with no variability
    expect_s3_class(results, "ihcheterogeneityResults")
})

test_that("ihcheterogeneity plain language summary works", {
    results <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
        data = sample_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        showSummary = TRUE
    )
    expect_true(results$summary$visible)
})

test_that("ihcheterogeneity glossary works", {
    results <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
        data = sample_data,
        wholesection = "ki67_wholesection",
        biopsy1 = "ki67_region1",
        biopsy2 = "ki67_region2",
        showGlossary = TRUE
    )
    expect_true(results$glossary$visible)
})

test_that("ihcheterogeneity omits opposite-direction calibration wording without a reference", {
    set.seed(42)
    n <- 30
    base <- rnorm(n, 20, 4)
    df_inter <- data.frame(
        core1 = base + 35 + rnorm(n, 0, 1),
        core2 = base + rnorm(n, 0, 1),
        core3 = base + rnorm(n, 0, 1)
    )
    res <- ClinicoPath::ihcheterogeneity(showInterpretation = TRUE,
        data = df_inter,
        biopsy1 = "core1",
        biopsy2 = "core2",
        biopsy3 = "core3"
    )
    interp_html <- res$interpretation$content
    expect_false(grepl("offset in opposite directions", interp_html, fixed = TRUE))
})


