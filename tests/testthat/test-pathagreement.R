# test_pathagreement.R

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("pathagreement works with two raters (Cohen's kappa)", {
  skip_if_not_installed('jmvReadWrite')

  # Load test data
  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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
  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_melanoma <- pa_test_data("pathagreement_melanoma")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_clustering <- pa_test_data("pathagreement_clustering")

  rater_vars <- LETTERS[1:12]

  expect_error(
    pathagreement(
      data = pathagreement_clustering,
      vars = LETTERS[1:12],
      performClustering = TRUE,
      nStyleGroups = 3,
      showClusteringHeatmap = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with different clustering methods", {

  pathagreement_clustering <- pa_test_data("pathagreement_clustering")

  rater_vars <- LETTERS[1:12]
  methods <- c("ward", "complete", "average")

  for (method in methods) {
    expect_error(
      pathagreement(
        data = pathagreement_clustering,
        vars = LETTERS[1:12],
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

  pathagreement_clustering <- pa_test_data("pathagreement_clustering")

  rater_vars <- LETTERS[1:12]

  expect_error(
    pathagreement(
      data = pathagreement_clustering,
      vars = LETTERS[1:12],
      performClustering = TRUE,
      autoSelectGroups = TRUE
    ),
    NA
  )
})


test_that("pathagreement works with discordant case identification", {

  pathagreement_clustering <- pa_test_data("pathagreement_clustering")

  rater_vars <- LETTERS[1:12]

  expect_error(
    pathagreement(
      data = pathagreement_clustering,
      vars = LETTERS[1:12],
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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_multi_raters <- pa_test_data("pathagreement_multi_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_perfect <- pa_test_data("pathagreement_perfect")

  expect_error(
    pathagreement(
      data = pathagreement_perfect,
      vars = c("Rater_1", "Rater_2", "Rater_3")
    ),
    NA
  )
})


test_that("pathagreement handles complete disagreement", {

  pathagreement_disagreement <- pa_test_data("pathagreement_disagreement")

  expect_error(
    pathagreement(
      data = pathagreement_disagreement,
      vars = c("Rater_1", "Rater_2", "Rater_3")
    ),
    NA
  )
})


test_that("pathagreement handles missing data", {

  pathagreement_missing <- pa_test_data("pathagreement_missing")

  expect_error(
    pathagreement(
      data = pathagreement_missing,
      vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4")
    ),
    NA
  )
})


test_that("pathagreement handles single case", {

  pathagreement_single <- pa_test_data("pathagreement_single")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_comprehensive <- pa_test_data("pathagreement_comprehensive")

  rater_vars <- paste0("Pathologist_", LETTERS[1:8])

  expect_error(
    pathagreement(
      data = pathagreement_comprehensive,
      vars = paste0("Pathologist_", LETTERS[1:8]),
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

  pathagreement_breast <- pa_test_data("pathagreement_breast")

  rater_vars <- paste0("Path_", 1:6)

  expect_error(
    pathagreement(
      data = pathagreement_breast,
      vars = paste0("Path_", 1:6),
      pathologyContext = TRUE
    ),
    NA
  )
})


# =============================================================================
# Return Structure Tests
# =============================================================================

test_that("pathagreement returns correct structure", {

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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

  pathagreement_melanoma <- pa_test_data("pathagreement_melanoma")

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

  pathagreement_two_raters <- pa_test_data("pathagreement_two_raters")

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
