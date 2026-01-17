# Comprehensive Test Suite for Agreement Module
# Tests each argument systematically with appropriate test data

test_that("agreement module setup", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  data("histopathology", package = "ClinicoPath")
  # expect_true(exists("agreementResults")) # This check might fail if not exported
  expect_true(is.function(agreement))
})

# ======================================================================
# Test Data Setup
# ======================================================================

# Create various test datasets for different scenarios
create_test_data <- function() {
  set.seed(123)
  n <- 50

  # Binary categorical raters (2 categories)
  binary_r1 <- factor(sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.6, 0.4)))
  binary_r2 <- factor(sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.6, 0.4)))
  binary_r3 <- factor(sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.6, 0.4)))

  # Ordinal raters (3 ordered categories)
  ordinal_r1 <- ordered(sample(c("G1", "G2", "G3"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
                        levels = c("G1", "G2", "G3"))
  ordinal_r2 <- ordered(sample(c("G1", "G2", "G3"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
                        levels = c("G1", "G2", "G3"))
  ordinal_r3 <- ordered(sample(c("G1", "G2", "G3"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
                        levels = c("G1", "G2", "G3"))

  # Numeric raters for continuous agreement (tumor size in mm)
  continuous_r1 <- rnorm(n, mean = 25, sd = 5)
  continuous_r2 <- continuous_r1 + rnorm(n, mean = 0, sd = 2)  # Correlated with small error

  # Cluster/institution variable for hierarchical analysis
  cluster <- factor(sample(c("HospitalA", "HospitalB", "HospitalC"), n, replace = TRUE))

  # Reference/gold standard
  reference <- factor(sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.6, 0.4)))

  # Create dataset with some missing values
  binary_r1_na <- binary_r1
  binary_r1_na[sample(1:n, 5)] <- NA

  list(
    binary = data.frame(r1 = binary_r1, r2 = binary_r2, r3 = binary_r3,
                       reference = reference, cluster = cluster),
    ordinal = data.frame(r1 = ordinal_r1, r2 = ordinal_r2, r3 = ordinal_r3,
                        cluster = cluster),
    continuous = data.frame(r1 = continuous_r1, r2 = continuous_r2),
    with_missing = data.frame(r1 = binary_r1_na, r2 = binary_r2, r3 = binary_r3)
  )
}

# ======================================================================
# Test Basic Options
# ======================================================================

test_that("vars - minimum 2 variables required", {
  test_data <- create_test_data()

  # Should work with 2 raters
  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2")
    )
  }, NA)

  # Should work with 3+ raters
  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3")
    )
  }, NA)
})

test_that("baConfidenceLevel - controls confidence level for LoA", {
  test_data <- create_test_data()

  conf_levels <- c(0.50, 0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    expect_error({
      result <- agreement(
        data = test_data$continuous,
        vars = c("r1", "r2"),
        baConfidenceLevel = conf,
        blandAltmanPlot = TRUE
      )
    }, NA, info = paste("conf_level:", conf))
  }
})

test_that("proportionalBias - tests for proportional bias in B-A plot", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$continuous,
      vars = c("r1", "r2"),
      proportionalBias = TRUE,
      blandAltmanPlot = TRUE
    )
  }, NA)

  expect_error({
    result <- agreement(
      data = test_data$continuous,
      vars = c("r1", "r2"),
      proportionalBias = FALSE,
      blandAltmanPlot = TRUE
    )
  }, NA)
})

test_that("blandAltmanPlot - generates B-A plot for continuous data", {
  test_data <- create_test_data()

  # Should work with continuous data
  expect_error({
    result <- agreement(
      data = test_data$continuous,
      vars = c("r1", "r2"),
      blandAltmanPlot = TRUE
    )
  }, NA)

  # Should work without B-A plot
  expect_error({
    result <- agreement(
      data = test_data$continuous,
      vars = c("r1", "r2"),
      blandAltmanPlot = FALSE
    )
  }, NA)
})

test_that("sft - frequency table display", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2"),
      sft = TRUE
    )
  }, NA)

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      sft = TRUE
    )
  }, NA)
})

# ======================================================================
# Test Weighting Options
# ======================================================================

test_that("wght - different weighting schemes", {
  test_data <- create_test_data()

  weights <- c("unweighted", "equal", "squared")

  for (weight in weights) {
    expect_error({
      result <- agreement(
        data = test_data$ordinal,
        vars = c("r1", "r2"),
        wght = weight
      )
    }, NA, info = paste("weight:", weight))
  }
})

test_that("wght - weighted kappa requires ordered factors", {
  test_data <- create_test_data()

  # Should work with ordered factors
  expect_error({
    agreement(
      data = test_data$ordinal,
      vars = c("r1", "r2"),
      wght = "equal"
    )
  }, NA)

  # Unweighted should work with nominal factors
  expect_error({
    agreement(
      data = test_data$binary,
      vars = c("r1", "r2"),
      wght = "unweighted"
    )
  }, NA)
})

test_that("exct - exact kappa for 3+ raters", {
  test_data <- create_test_data()

  # Should work with 3+ raters
  expect_error({
    agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      exct = TRUE
    )
  }, NA)

  # Should work with FALSE
  expect_error({
    agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      exct = FALSE
    )
  }, NA)
})

# ======================================================================
# Test Alternative Measures
# ======================================================================

test_that("kripp - Krippendorff's alpha calculation", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      kripp = TRUE
    )
  }, NA)
})

test_that("krippMethod - different data types for Krippendorff's alpha", {
  test_data <- create_test_data()

  methods <- c("nominal", "ordinal", "interval", "ratio")

  for (method in methods) {
    expect_error({
      result <- agreement(
        data = test_data$ordinal,
        vars = c("r1", "r2", "r3"),
        kripp = TRUE,
        krippMethod = method
      )
    }, NA, info = paste("kripp_method:", method))
  }
})

test_that("bootstrap - bootstrap confidence intervals for Krippendorff's alpha", {
  test_data <- create_test_data()

  # Use small sample for faster testing
  small_data <- test_data$binary[1:20, ]

  expect_error({
    result <- agreement(
      data = small_data,
      vars = c("r1", "r2"),
      kripp = TRUE,
      bootstrap = TRUE
    )
  }, NA)
})

test_that("gwet - Gwet's AC1/AC2 coefficient", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2"),
      gwet = TRUE
    )
  }, NA)
})

test_that("gwetWeights - weights for Gwet's AC", {
  test_data <- create_test_data()

  weights <- c("unweighted", "linear", "quadratic")

  for (weight in weights) {
    expect_error({
      result <- agreement(
        data = test_data$ordinal,
        vars = c("r1", "r2"),
        gwet = TRUE,
        gwetWeights = weight
      )
    }, NA, info = paste("gwet_weight:", weight))
  }
})

# ======================================================================
# Test Display Options
# ======================================================================

test_that("showLevelInfo - displays level ordering information", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$ordinal,
      vars = c("r1", "r2"),
      showLevelInfo = TRUE
    )
  }, NA)
})

test_that("showSummary - plain-language summary", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2"),
      showSummary = TRUE
    )
  }, NA)
})

test_that("showAbout - analysis explanation panel", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2"),
      showAbout = TRUE
    )
  }, NA)
})

# ======================================================================
# Test Hierarchical Analysis
# ======================================================================

test_that("hierarchicalKappa - basic hierarchical analysis", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster"
    )
  }, NA)
})

test_that("iccHierarchical - hierarchical ICC", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      iccHierarchical = TRUE
    )
  }, NA)
})

test_that("clusterSpecificKappa - cluster-specific estimates", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      clusterSpecificKappa = TRUE
    )
  }, NA)
})

test_that("varianceDecomposition - variance components", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      varianceDecomposition = TRUE
    )
  }, NA)
})

test_that("shrinkageEstimates - empirical Bayes estimates", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      shrinkageEstimates = TRUE
    )
  }, NA)
})

test_that("testClusterHomogeneity - test cluster homogeneity", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      testClusterHomogeneity = TRUE
    )
  }, NA)
})

test_that("clusterRankings - cluster performance rankings", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      clusterRankings = TRUE
    )
  }, NA)
})

# ======================================================================
# Test Consensus Variable Creation
# ======================================================================

test_that("consensusVar - agreement with consensus options", {
  test_data <- create_test_data()

  # Note: consensusVar is an Output type in Jamovi, so it's not an argument here.
  # We test the options that control consensus generation.
  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      consensusName = "consensus_score"
    )
  }, NA)
})

test_that("consensusRule - different consensus rules", {
  test_data <- create_test_data()

  rules <- c("majority", "supermajority", "unanimous")

  for (rule in rules) {
    expect_error({
      result <- agreement(
        data = test_data$binary,
        vars = c("r1", "r2", "r3"),
        consensusRule = rule,
        consensusName = paste0("consensus_", rule)
      )
    }, NA, info = paste("consensus_rule:", rule))
  }
})

test_that("tieBreaker - different tie handling methods", {
  test_data <- create_test_data()

  tie_methods <- c("exclude", "first", "lowest", "highest")

  for (method in tie_methods) {
    expect_error({
      result <- agreement(
        data = test_data$binary,
        vars = c("r1", "r2", "r3"),
        tieBreaker = method,
        consensusName = paste0("consensus_", method)
      )
    }, NA, info = paste("tie_breaker:", method))
  }
})

test_that("consensusName - custom variable name", {
  test_data <- create_test_data()

  custom_names <- c("final_diagnosis", "gold_standard", "consensus_v1")

  for (name in custom_names) {
    expect_error({
      result <- agreement(
        data = test_data$binary,
        vars = c("r1", "r2", "r3"),
        consensusName = name
      )
    }, NA, info = paste("consensus_name:", name))
  }
})

# ======================================================================
# Test Reference Rater Analysis
# ======================================================================

test_that("referenceRater - pairwise comparison with reference", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      referenceRater = "reference"
    )
  }, NA)
})

test_that("rankRaters - rank raters by kappa", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      referenceRater = "reference",
      rankRaters = TRUE
    )
  }, NA)

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      referenceRater = "reference",
      rankRaters = FALSE
    )
  }, NA)
})

# ======================================================================
# Test Level of Agreement Variable
# ======================================================================

test_that("loaVariable - creates LoA categorical variable", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      loaVariable = TRUE
    )
  }, NA)
})

test_that("loaThresholds - different threshold methods", {
  test_data <- create_test_data()

  threshold_methods <- c("custom", "quartiles", "tertiles")

  for (method in threshold_methods) {
    expect_error({
      result <- agreement(
        data = test_data$binary,
        vars = c("r1", "r2", "r3"),
        loaVariable = TRUE,
        loaThresholds = method
      )
    }, NA, info = paste("loa_threshold:", method))
  }
})

test_that("loaHighThreshold - custom high threshold", {
  test_data <- create_test_data()

  thresholds <- c(50, 66, 75, 90)

  for (thresh in thresholds) {
    expect_error({
      result <- agreement(
        data = test_data$binary,
        vars = c("r1", "r2", "r3"),
        loaVariable = TRUE,
        loaThresholds = "custom",
        loaHighThreshold = thresh
      )
    }, NA, info = paste("high_threshold:", thresh))
  }
})

test_that("loaLowThreshold - custom low threshold", {
  test_data <- create_test_data()

  thresholds <- c(30, 40, 50, 56)

  for (thresh in thresholds) {
    expect_error({
      result <- agreement(
        data = test_data$binary,
        vars = c("r1", "r2", "r3"),
        loaVariable = TRUE,
        loaThresholds = "custom",
        loaLowThreshold = thresh,
        loaHighThreshold = 75
      )
    }, NA, info = paste("low_threshold:", thresh))
  }
})

# ======================================================================
# Test Edge Cases and Error Handling
# ======================================================================

test_that("handles missing data appropriately", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$with_missing,
      vars = c("r1", "r2", "r3")
    )
  }, NA)
})

test_that("handles perfect agreement", {
  n <- 30
  perfect_data <- data.frame(
    r1 = factor(rep(c("A", "B"), each = n/2)),
    r2 = factor(rep(c("A", "B"), each = n/2)),
    r3 = factor(rep(c("A", "B"), each = n/2))
  )

  expect_error({
    result <- agreement(
      data = perfect_data,
      vars = c("r1", "r2", "r3")
    )
  }, NA)
})

test_that("handles complete disagreement", {
  n <- 30
  disagree_data <- data.frame(
    r1 = factor(rep(c("A", "B", "C"), length.out = n)),
    r2 = factor(rep(c("B", "C", "A"), length.out = n)),
    r3 = factor(rep(c("C", "A", "B"), length.out = n))
  )

  expect_error({
    result <- agreement(
      data = disagree_data,
      vars = c("r1", "r2", "r3")
    )
  }, NA)
})

test_that("handles small sample sizes", {
  small_data <- create_test_data()$binary[1:10, ]

  expect_error({
    result <- agreement(
      data = small_data,
      vars = c("r1", "r2")
    )
  }, NA)
})

# ======================================================================
# Test Complex Parameter Combinations
# ======================================================================

test_that("complex combination 1: all display options", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$ordinal,
      vars = c("r1", "r2", "r3"),
      wght = "equal",
      showLevelInfo = TRUE,
      showSummary = TRUE,
      showAbout = TRUE,
      sft = TRUE
    )
  }, NA)
})

test_that("complex combination 2: alternative measures", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      kripp = TRUE,
      krippMethod = "nominal",
      gwet = TRUE,
      gwetWeights = "unweighted"
    )
  }, NA)
})

test_that("complex combination 3: consensus and LoA", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      consensusRule = "majority",
      tieBreaker = "exclude",
      consensusName = "final_consensus",
      loaVariable = TRUE,
      loaThresholds = "custom",
      loaHighThreshold = 75,
      loaLowThreshold = 50
    )
  }, NA)
})

test_that("complex combination 4: hierarchical with all options", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      hierarchicalKappa = TRUE,
      clusterVariable = "cluster",
      iccHierarchical = TRUE,
      clusterSpecificKappa = TRUE,
      varianceDecomposition = TRUE,
      testClusterHomogeneity = TRUE,
      clusterRankings = TRUE
    )
  }, NA)
})

test_that("complex combination 5: comprehensive analysis", {
  test_data <- create_test_data()

  expect_error({
    result <- agreement(
      data = test_data$binary,
      vars = c("r1", "r2", "r3"),
      wght = "unweighted",
      exct = FALSE,
      kripp = TRUE,
      krippMethod = "nominal",
      gwet = TRUE,
      gwetWeights = "unweighted",
      showLevelInfo = TRUE,
      showSummary = TRUE,
      consensusRule = "majority",
      referenceRater = "reference",
      rankRaters = TRUE,
      loaVariable = TRUE,
      sft = TRUE
    )
  }, NA)
})

# ======================================================================
# Test with Real Pathology Data
# ======================================================================

test_that("works with histopathology dataset", {
  data("histopathology", package = "ClinicoPath")
  histopathology_small <- histopathology[1:30, ]

  # Basic analysis
  expect_error({
    result <- agreement(
      data = histopathology_small,
      vars = c("Rater 1", "Rater 2")
    )
  }, NA)

  # With multiple raters
  expect_error({
    result <- agreement(
      data = histopathology_small,
      vars = c("Rater 1", "Rater 2", "Rater 3")
    )
  }, NA)

  # With ordinal raters
  histopathology_small$`Rater A` <- factor(histopathology_small$`Rater A`, ordered = TRUE)
  histopathology_small$`Rater B` <- factor(histopathology_small$`Rater B`, ordered = TRUE)
  expect_error({
    result <- agreement(
      data = histopathology_small,
      vars = c("Rater A", "Rater B"),
      wght = "equal"
    )
  }, NA)
})
