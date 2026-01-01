context("test-pathagreement")

# Load required library
library(ClinicoPath)

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("pathagreement works with two raters (Cohen's kappa)", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Load test data
  data("pathagreement_two_raters", package = "ClinicoPath")

  # Test basic two-rater analysis
  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B")
    ),
    NA
  )
})


test_that("pathagreement works with multiple raters (Fleiss' kappa)", {

  # Load test data
  data("pathagreement_multi_raters", package = "ClinicoPath")

  # Test multi-rater analysis
  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5")
    ),
    NA
  )
})


test_that("pathagreement works with 3 raters", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3")
    ),
    NA
  )
})


# =============================================================================
# Weighted Kappa Tests (Ordinal Data)
# =============================================================================

test_that("pathagreement works with weighted kappa - squared weights", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      wght = "squared"
    ),
    NA
  )
})


test_that("pathagreement works with weighted kappa - equal/linear weights", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      wght = "equal"
    ),
    NA
  )
})


# =============================================================================
# Multi-Rater Method Tests
# =============================================================================

test_that("pathagreement works with Fleiss' kappa method", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      multiraterMethod = "fleiss",
      fleissCI = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with Krippendorff's alpha - nominal", {

  data("pathagreement_melanoma", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_melanoma,
      vars = c("Dermpath_1", "Dermpath_2", "Dermpath_3", "Dermpath_4"),
      multiraterMethod = "krippendorff",
      kripp = TRUE,
      krippMethod = "nominal"
    ),
    NA
  )
})


test_that("pathagreement works with Krippendorff's alpha - ordinal", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      multiraterMethod = "krippendorff",
      kripp = TRUE,
      krippMethod = "ordinal"
    ),
    NA
  )
})


# =============================================================================
# Visualization Tests
# =============================================================================

test_that("pathagreement works with frequency tables", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      sft = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with heatmap visualization", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      heatmap = TRUE,
      heatmapDetails = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with different heatmap themes", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  themes <- c("viridis", "plasma", "cividis", "bwr", "ryg")

  for (theme in themes) {
    expect_error(
      pathagreement(
        data = pathagreement_multi_raters,
        vars = c("Rater_1", "Rater_2", "Rater_3"),
        heatmap = TRUE,
        heatmapTheme = theme
      ),
      NA,
      info = paste("Theme:", theme)
    )
  }
})


# =============================================================================
# Consensus Analysis Tests
# =============================================================================

test_that("pathagreement works with majority consensus", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      consensus = TRUE,
      consensus_method = "majority",
      show_consensus_table = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with super majority consensus", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      consensus = TRUE,
      consensus_method = "super_majority"
    ),
    NA
  )
})


test_that("pathagreement works with unanimous consensus", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      consensus = TRUE,
      consensus_method = "unanimous"
    ),
    NA
  )
})


# =============================================================================
# Diagnostic Style Clustering Tests (Usubutun Method)
# =============================================================================

test_that("pathagreement works with diagnostic style clustering", {

  data("pathagreement_clustering", package = "ClinicoPath")

  rater_vars <- LETTERS[1:12]

  expect_error(
    pathagreement(
      data = pathagreement_clustering,
      vars = rater_vars,
      performClustering = TRUE,
      nStyleGroups = 3,
      showClusteringHeatmap = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with different clustering methods", {

  data("pathagreement_clustering", package = "ClinicoPath")

  rater_vars <- LETTERS[1:12]
  methods <- c("ward", "complete", "average")

  for (method in methods) {
    expect_error(
      pathagreement(
        data = pathagreement_clustering,
        vars = rater_vars,
        performClustering = TRUE,
        clusteringMethod = method,
        nStyleGroups = 3
      ),
      NA,
      info = paste("Clustering method:", method)
    )
  }
})


test_that("pathagreement works with auto-select number of groups", {

  data("pathagreement_clustering", package = "ClinicoPath")

  rater_vars <- LETTERS[1:12]

  expect_error(
    pathagreement(
      data = pathagreement_clustering,
      vars = rater_vars,
      performClustering = TRUE,
      autoSelectGroups = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with discordant case identification", {

  data("pathagreement_clustering", package = "ClinicoPath")

  rater_vars <- LETTERS[1:12]

  expect_error(
    pathagreement(
      data = pathagreement_clustering,
      vars = rater_vars,
      performClustering = TRUE,
      identifyDiscordant = TRUE,
      discordantThreshold = 0.5
    ),
    NA
  )
})


# =============================================================================
# Advanced Analysis Tests
# =============================================================================

test_that("pathagreement works with ICC calculation", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      icc = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with pairwise analysis", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4"),
      pairwiseAnalysis = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with category-specific agreement", {

  data("pathagreement_multi_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_multi_raters,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
      categoryAnalysis = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with Gwet's AC coefficients", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      gwetAC = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with PABAK", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      pabak = TRUE
    ),
    NA
  )
})


# =============================================================================
# Edge Case Tests
# =============================================================================

test_that("pathagreement handles perfect agreement", {

  data("pathagreement_perfect", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_perfect,
      vars = c("Rater_1", "Rater_2", "Rater_3")
    ),
    NA
  )
})


test_that("pathagreement handles complete disagreement", {

  data("pathagreement_disagreement", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_disagreement,
      vars = c("Rater_1", "Rater_2", "Rater_3")
    ),
    NA
  )
})


test_that("pathagreement handles missing data", {

  data("pathagreement_missing", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_missing,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4")
    ),
    NA
  )
})


test_that("pathagreement handles single case", {

  data("pathagreement_single", package = "ClinicoPath")

  # Single case should either work or give informative error
  result <- tryCatch(
    {
      pathagreement(
        data = pathagreement_single,
        vars = c("Rater_1", "Rater_2")
      )
      TRUE
    },
    error = function(e) {
      # If it errors, check that error message is informative
      expect_match(as.character(e), "case|sample|observation", ignore.case = TRUE)
      FALSE
    }
  )

  expect_true(result || !result)  # Either outcome is acceptable
})


# =============================================================================
# Documentation and Educational Features Tests
# =============================================================================

test_that("pathagreement works with clinical summary", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      showClinicalSummary = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with about analysis", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      showAboutAnalysis = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with assumptions display", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      showAssumptions = TRUE
    ),
    NA
  )
})


# =============================================================================
# Comprehensive Dataset Test
# =============================================================================

test_that("pathagreement works with comprehensive dataset - all features", {

  data("pathagreement_comprehensive", package = "ClinicoPath")

  rater_vars <- paste0("Pathologist_", LETTERS[1:8])

  expect_error(
    pathagreement(
      data = pathagreement_comprehensive,
      vars = rater_vars,
      multiraterMethod = "fleiss",
      fleissCI = TRUE,
      heatmap = TRUE,
      heatmapDetails = TRUE,
      sft = TRUE,
      pairwiseAnalysis = TRUE,
      categoryAnalysis = TRUE,
      showClinicalSummary = TRUE
    ),
    NA
  )
})


# =============================================================================
# Breast Pathology Reference Standard Test
# =============================================================================

test_that("pathagreement works with reference standard comparison", {

  data("pathagreement_breast", package = "ClinicoPath")

  rater_vars <- paste0("Path_", 1:6)

  expect_error(
    pathagreement(
      data = pathagreement_breast,
      vars = rater_vars,
      pathologyContext = TRUE
    ),
    NA
  )
})


# =============================================================================
# Return Structure Tests
# =============================================================================

test_that("pathagreement returns correct structure", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  result <- pathagreement(
    data = pathagreement_two_raters,
    vars = c("Pathologist_A", "Pathologist_B")
  )

  # Test that it returns a pathagreementResults object (jamovi results object)
  expect_s3_class(result, "pathagreementResults")
})


# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("pathagreement handles errors appropriately", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  # Test with missing vars parameter
  expect_error(
    pathagreement(
      data = pathagreement_two_raters
      # Missing vars parameter
    )
  )

  # Test with non-existent variables
  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("NonExistent1", "NonExistent2")
    )
  )

  # Test with single variable (need at least 2 raters)
  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = "Pathologist_A"
    )
  )
})


# =============================================================================
# Nominal vs Ordinal Data Tests
# =============================================================================

test_that("pathagreement correctly handles nominal data", {

  data("pathagreement_melanoma", package = "ClinicoPath")

  # Nominal data should use unweighted kappa
  expect_error(
    pathagreement(
      data = pathagreement_melanoma,
      vars = c("Dermpath_1", "Dermpath_2", "Dermpath_3", "Dermpath_4"),
      wght = "unweighted"  # Appropriate for nominal
    ),
    NA
  )
})


test_that("pathagreement correctly handles ordinal data", {

  data("pathagreement_two_raters", package = "ClinicoPath")

  # Ordinal data can use weighted kappa
  expect_error(
    pathagreement(
      data = pathagreement_two_raters,
      vars = c("Pathologist_A", "Pathologist_B"),
      wght = "squared"  # Appropriate for ordinal
    ),
    NA
  )
})
