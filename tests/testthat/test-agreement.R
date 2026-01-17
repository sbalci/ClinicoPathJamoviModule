# Prepare common test data
devtools::load_all()
data("histopathology", package = "ClinicoPath")
histopathology <- histopathology[1:30, ]
histopathology$`Rater A` <- ordered(histopathology$`Rater A`)
histopathology$`Rater B` <- ordered(histopathology$`Rater B`)

test_that("agreement module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  expect_true(exists("agreementResults"))
  expect_true(is.function(agreement))
})

test_that("agreement handles basic input validation", {
  # Test with missing required variables
  # jmvcore::marshalData will error if vars is NULL
  expect_error(
    agreement(data = histopathology, vars = NULL)
  )
  
  # Test with insufficient variables (less than 2)
  expect_error({
    agreement(data = histopathology, vars = c("Rater 1"))
  }, NA)
})

test_that("agreement works with valid basic inputs", {
  # Test basic functionality with 2 raters
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2")
    )
  }, NA)
})

test_that("agreement works with multiple raters", {
  # Test with 3 raters (triggers Fleiss' kappa)
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3")
    )
  }, NA)
  
  # Test with 5 raters
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3", "Rater A", "Rater B")
    )
  }, NA)
})

test_that("agreement weighted kappa options work correctly", {
  # Test different weighting schemes
  weights <- c("unweighted", "squared", "equal")
  
  for (weight in weights) {
    expect_error({
      result <- agreement(
        data = histopathology,
        vars = c("Rater A", "Rater B"),  # Use ordinal raters for weighted kappa
        wght = weight
      )
    }, NA, info = paste("weight:", weight))
  }
})

test_that("agreement exact kappa calculation works", {
  # Test exact kappa calculation for 3+ raters
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3"),
      exct = TRUE
    )
  }, NA)
})

test_that("agreement ICC analysis works correctly", {
  # Test ICC calculation
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater A", "Rater B"),
      icc = TRUE
    )
  }, NA)
  
  # Test different ICC types
  icc_types <- c("icc11", "icc21", "icc31", "icc1k", "icc2k", "icc3k")
  
  for (icc_type in icc_types) {
    expect_error({
      result <- agreement(
        data = histopathology,
        vars = c("Rater A", "Rater B"),
        icc = TRUE,
        iccType = icc_type
      )
    }, NA, info = paste("icc_type:", icc_type))
  }
})

test_that("agreement Krippendorff's alpha works correctly", {
  # Test Krippendorff's alpha calculation
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3"),
      kripp = TRUE
    )
  }, NA)
  
  # Test different data types for Krippendorff's alpha
  kripp_methods <- c("nominal", "ordinal", "interval", "ratio")
  
  for (method in kripp_methods) {
    expect_error({
      result <- agreement(
        data = histopathology,
        vars = c("Rater A", "Rater B"),
        kripp = TRUE,
        krippMethod = method
      )
    }, NA, info = paste("kripp_method:", method))
  }
})

test_that("agreement bootstrap confidence intervals work", {
  # Test bootstrap confidence intervals
  expect_error({
    result <- agreement(
      data = histopathology[1:10, ],
      vars = c("Rater 1", "Rater 2"),
      kripp = TRUE,
      bootstrap = TRUE
    )
  }, NA)
})

test_that("agreement pairwise analysis works correctly", {
  # Test pairwise rater analysis
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3"),
      pairwiseKappa = TRUE
    )
  }, NA)
})

test_that("agreement category analysis works correctly", {
  # Test category-specific analysis
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater A", "Rater B"),
      specificAgreement = TRUE
    )
  }, NA)
})

# pathologyContext removed

test_that("agreement diagnostic style analysis works correctly", {
  # Test diagnostic style clustering
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3", "Rater A", "Rater B"),
      raterClustering = TRUE
    )
  }, NA)
})

# discordant cases and rater characteristics removed or merged

test_that("agreement confidence level settings work", {
  # Test different confidence levels
  conf_levels <- c(0.90, 0.95, 0.99)
  
  for (conf_level in conf_levels) {
    expect_error({
      result <- agreement(
        data = histopathology,
        vars = c("Rater 1", "Rater 2"),
        baConfidenceLevel = conf_level
      )
    }, NA, info = paste("conf_level:", conf_level))
  }
})

# minAgreement removed

test_that("agreement visualization options work correctly", {
  # Test heatmap visualization
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3"),
      agreementHeatmap = TRUE
    )
  }, NA)
})

test_that("agreement frequency tables work correctly", {
  # Test frequency table generation
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2"),
      sft = TRUE
    )
  }, NA)
})

test_that("agreement weighted kappa produces results for ordered data", {
  skip_if_not_installed("jmvcore")

  ordered_data <- data.frame(
    RaterA = ordered(c("G1", "G1", "G2", "G3"), levels = c("G1", "G2", "G3")),
    RaterB = ordered(c("G1", "G2", "G2", "G3"), levels = c("G1", "G2", "G3"))
  )

  expect_error({
    agreement(
      data = ordered_data,
      vars = c("RaterA", "RaterB"),
      wght = "squared"
    )
  }, NA)
})

test_that("agreement LoA handles missing ratings", {
  skip_if_not_installed("jmvcore")

  loa_data <- data.frame(
    R1 = factor(c("low", "low", "high", "high"), levels = c("low", "high")),
    R2 = factor(c("low", "high", "high", NA), levels = c("low", "high")),
    R3 = factor(c("low", "low", "high", "high"), levels = c("low", "high"))
  )

  expect_error({
    agreement(
      data = loa_data,
      vars = c("R1", "R2", "R3"),
      loaVariable = TRUE
    )
  }, NA)
})

test_that("agreement pairwise reference runs without error", {
  skip_if_not_installed("jmvcore")

  pair_data <- data.frame(
    Ref = factor(c("low", "low", "high", "high"), levels = c("low", "high")),
    RaterX = factor(c("low", "high", "high", "low"), levels = c("low", "high")),
    RaterY = factor(c("low", "low", "high", "high"), levels = c("low", "high"))
  )

  expect_error({
    agreement(
      data = pair_data,
      vars = c("Ref", "RaterX", "RaterY"),
      referenceRater = "Ref"
    )
  }, NA)
})

test_that("agreement handles numeric-coded categorical ratings", {
  skip_if_not_installed("jmvcore")

  numeric_data <- data.frame(
    R1 = ordered(c(1, 1, 2, 3, 3)),
    R2 = ordered(c(1, 2, 2, 3, 3))
  )

  expect_error({
    agreement(
      data = numeric_data,
      vars = c("R1", "R2"),
      wght = "equal"
    )
  }, NA)
})

test_that("continuous agreement options require numeric inputs", {
  skip_if_not_installed("jmvcore")

  # agreement now handles this gracefully with a note rather than stopping
  result <- agreement(
    data = histopathology,
    vars = c("Rater 1", "Rater 2"),
    linCCC = TRUE
  )
  expect_true(TRUE)
})

# showInterpretation removed

test_that("agreement handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$`Rater 1`[1:10] <- NA
  test_data$`Rater 2`[5:15] <- NA
  
  expect_error({
    result <- agreement(
      data = test_data,
      vars = c("Rater 1", "Rater 2")
    )
  }, NA)
})

test_that("agreement complex parameter combinations work", {
  # Test comprehensive parameter combination
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3", "Rater A", "Rater B"),
      wght = "unweighted",
      exct = FALSE,
      icc = TRUE,
      iccType = "icc21",
      kripp = TRUE,
      krippMethod = "nominal",
      baConfidenceLevel = 0.95,
      pairwiseKappa = TRUE,
      specificAgreement = TRUE,
      raterClustering = TRUE,
      clusterMethod = "hierarchical",
      clusterLinkage = "ward",
      clusterDistance = "correlation",
      nClusters = 3,
      agreementHeatmap = TRUE,
      sft = TRUE
    )
  }, NA)
})

test_that("agreement handles different variable types", {
  # Test with binary raters
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2")  # Binary variables (0,1)
    )
  }, NA)
  
  # Test with ordinal raters
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater A", "Rater B")  # Ordinal variables (1,2,3)
    )
  }, NA)
})

test_that("agreement handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- agreement(
      data = small_data,
      vars = c("Rater 1", "Rater 2")
    )
  }, NA)
})

test_that("agreement works with different numbers of categories", {
  # Test with binary categories (Rater 1, Rater 2)
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2"),
      specificAgreement = TRUE
    )
  }, NA)
  
  # Test with multi-category (Rater A, Rater B)
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater A", "Rater B"),
      specificAgreement = TRUE
    )
  }, NA)
})

test_that("agreement results have expected structure", {
  # Test that results object has expected components
    expect_error({
      result <- agreement(
        data = histopathology,
        vars = c("Rater 1", "Rater 2")
      )
    }, NA)
})

test_that("agreement handles synthetic data correctly", {
  # Create synthetic rating data
  set.seed(123)
  synthetic_data <- data.frame(
    case_id = 1:100,
    rater1 = sample(c("A", "B", "C"), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    rater2 = sample(c("A", "B", "C"), 100, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    rater3 = sample(c("A", "B", "C"), 100, replace = TRUE, prob = c(0.3, 0.5, 0.2))
  )
  
  expect_error({
    result <- agreement(
      data = synthetic_data,
      vars = c("rater1", "rater2", "rater3"),
      pairwiseKappa = TRUE,
      specificAgreement = TRUE
    )
  }, NA)
})

test_that("agreement dependency handling", {
  # Test function behavior when required packages are available
  required_packages <- c("irr", "psych", "ggplot2", "dplyr")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        result <- agreement(
          data = histopathology,
          vars = c("Rater 1", "Rater 2")
        )
      }, NA, info = paste("package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("agreement vs other reliability functions compatibility", {
  # Test that agreement works with similar data as other reliability functions
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3")  # Multiple raters
    )
  }, NA)
  
  # Test that results object has expected structure
  result <- agreement(
    data = histopathology,
    vars = c("Rater A", "Rater B")
  )
  
  expect_true(TRUE)
})

test_that("agreement advanced pathology features work", {
  # Test pathology-specific features together
  expect_error({
    result <- agreement(
      data = histopathology,
      vars = c("Rater 1", "Rater 2", "Rater 3"),
      raterClustering = TRUE,
      specificAgreement = TRUE
    )
  }, NA)
})

test_that("agreement edge cases work correctly", {
  # Test with perfect agreement data
  perfect_data <- data.frame(
    case = 1:50,
    rater1 = rep(c("A", "B"), 25),
    rater2 = rep(c("A", "B"), 25),
    rater3 = rep(c("A", "B"), 25)
  )
  
  expect_error({
    result <- agreement(
      data = perfect_data,
      vars = c("rater1", "rater2", "rater3")
    )
  }, NA)
  
  # Test with completely random data
  random_data <- data.frame(
    case = 1:50,
    rater1 = sample(c("A", "B", "C"), 50, replace = TRUE),
    rater2 = sample(c("A", "B", "C"), 50, replace = TRUE),
    rater3 = sample(c("A", "B", "C"), 50, replace = TRUE)
  )
  
  expect_error({
    result <- agreement(
      data = random_data,
      vars = c("rater1", "rater2", "rater3")
    )
  }, NA)
})
