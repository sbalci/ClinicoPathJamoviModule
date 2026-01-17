# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: agreement
# ═══════════════════════════════════════════════════════════
#
# Tests boundary conditions, missing data, extreme scenarios,
# and error handling for the agreement jamovi function

library(testthat)
devtools::load_all()

# Load edge case test data
data(agreement_perfect, package = "ClinicoPath")
data(agreement_poor, package = "ClinicoPath")
data(agreement_small, package = "ClinicoPath")
data(agreement_missing, package = "ClinicoPath")
data(agreement_pathology, package = "ClinicoPath")
data(agreement_continuous, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Perfect Agreement
# ═══════════════════════════════════════════════════════════

test_that("agreement handles perfect agreement (kappa = 1.0)", {
  result <- agreement(
    data = agreement_perfect,
    vars = c("RaterA", "RaterB")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles perfect continuous agreement (ICC = 1.0)", {
  # Create perfect continuous data
  perfect_continuous <- data.frame(
    case_id = 1:50,
    measure1 = rnorm(50, mean = 100, sd = 15),
    measure2 = rnorm(50, mean = 100, sd = 15)
  )
  # Make them identical
  perfect_continuous$measure2 <- perfect_continuous$measure1

  result <- agreement(
    data = perfect_continuous,
    vars = c("measure1", "measure2"),
    icc = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Poor Agreement
# ═══════════════════════════════════════════════════════════

test_that("agreement handles poor agreement (kappa ≈ 0)", {
  result <- agreement(
    data = agreement_poor,
    vars = c("PathologistA", "PathologistB")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles negative kappa values", {
  # Negative kappa can occur when agreement is worse than chance
  result <- agreement(
    data = agreement_poor,
    vars = c("PathologistA", "PathologistB")
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Small Sample Size
# ═══════════════════════════════════════════════════════════

test_that("agreement handles small sample size (n = 30)", {
  result <- agreement(
    data = agreement_small,
    vars = c("Rater1", "Rater2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles very small sample (n = 10)", {
  small_data <- agreement_small[1:10, ]

  result <- agreement(
    data = small_data,
    vars = c("Rater1", "Rater2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles minimal sample for Bland-Altman (n = 3)", {
  minimal_data <- agreement_continuous[1:3, ]

  result <- agreement(
    data = minimal_data,
    vars = c("MeasurementA", "MeasurementB"),
    blandAltmanPlot = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Missing Data
# ═══════════════════════════════════════════════════════════

test_that("agreement handles missing data", {
  result <- agreement(
    data = agreement_missing,
    vars = c("Rater1", "Rater2", "Rater3")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement with missing data and Krippendorff's Alpha", {
  result <- agreement(
    data = agreement_missing,
    vars = c("Rater1", "Rater2", "Rater3"),
    kripp = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles all missing for one rater", {
  test_data <- agreement_pathology
  test_data$Pathologist2[1:50] <- NA

  result <- agreement(
    data = test_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles all missing data gracefully", {
  test_data <- agreement_pathology
  test_data$Pathologist1 <- NA
  test_data$Pathologist2 <- NA

  result <- agreement(
    data = test_data,
    vars = c("Pathologist1", "Pathologist2")
  )
  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Unbalanced Categories
# ═══════════════════════════════════════════════════════════

test_that("agreement handles highly unbalanced categories", {
  # Create unbalanced data (95% one category, 5% other)
  unbalanced_data <- data.frame(
    case_id = 1:100,
    rater1 = factor(c(rep("Negative", 95), rep("Positive", 5))),
    rater2 = factor(c(rep("Negative", 93), rep("Positive", 7)))
  )

  result <- agreement(
    data = unbalanced_data,
    vars = c("rater1", "rater2"),
    gwet = TRUE  # Gwet's AC1 is robust to prevalence
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Single Category Cases
# ═══════════════════════════════════════════════════════════

test_that("agreement handles single category data", {
  # All raters give same category for all cases
  single_cat_data <- data.frame(
    case_id = 1:50,
    rater1 = factor(rep("Benign", 50)),
    rater2 = factor(rep("Benign", 50))
  )

  result <- agreement(
    data = single_cat_data,
    vars = c("rater1", "rater2")
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Different Factor Levels
# ═══════════════════════════════════════════════════════════

test_that("agreement handles raters with different observed categories", {
  # Rater1 uses all 4 categories, Rater2 only uses 3
  test_data <- agreement_pathology
  # Force Rater2 to never use "Borderline"
  levels_to_keep <- setdiff(levels(test_data$Pathologist2), "Borderline")
  test_data$Pathologist2[test_data$Pathologist2 == "Borderline"] <- sample(levels_to_keep,
    sum(test_data$Pathologist2 == "Borderline"), replace = TRUE)

  result <- agreement(
    data = test_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Extreme Values in Continuous Data
# ═══════════════════════════════════════════════════════════

test_that("agreement handles extreme outliers in continuous data", {
  test_data <- agreement_continuous
  # Add extreme outliers
  test_data$MeasurementA[1:3] <- c(1000, 2000, 3000)
  test_data$MeasurementB[1:3] <- c(1050, 1900, 3100)

  result <- agreement(
    data = test_data,
    vars = c("MeasurementA", "MeasurementB"),
    blandAltmanPlot = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles zero variance in continuous data", {
  # All measurements identical
  zero_var_data <- data.frame(
    case_id = 1:30,
    measure1 = rep(100, 30),
    measure2 = rep(100, 30)
  )

  result <- agreement(
    data = zero_var_data,
    vars = c("measure1", "measure2"),
    icc = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles very large measurement differences", {
  test_data <- agreement_continuous
  # Create systematic large bias
  test_data$MeasurementB <- test_data$MeasurementA + 100

  result <- agreement(
    data = test_data,
    vars = c("MeasurementA", "MeasurementB"),
    blandAltmanPlot = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Maximum Number of Raters
# ═══════════════════════════════════════════════════════════

test_that("agreement handles large number of raters", {
  data(agreement_multiRater, package = "ClinicoPath")

  result <- agreement(
    data = agreement_multiRater,
    vars = c("SeniorPath1", "SeniorPath2", "MidLevelPath", "JuniorPath1", "JuniorPath2")
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Boundary Confidence Levels
# ═══════════════════════════════════════════════════════════

test_that("agreement handles 99% confidence level for BA", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    blandAltmanPlot = TRUE,
    baConfidenceLevel = 0.99
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles 80% confidence level for BA", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    blandAltmanPlot = TRUE,
    baConfidenceLevel = 0.80
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Data Type Coercion
# ═══════════════════════════════════════════════════════════

test_that("agreement handles numeric coded categorical data", {
  test_data <- agreement_pathology
  # Convert factors to numeric codes
  test_data$Pathologist1 <- as.numeric(test_data$Pathologist1)
  test_data$Pathologist2 <- as.numeric(test_data$Pathologist2)

  result <- agreement(
    data = test_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles character categorical data", {
  test_data <- agreement_pathology
  # Convert factors to character
  test_data$Pathologist1 <- as.character(test_data$Pathologist1)
  test_data$Pathologist2 <- as.character(test_data$Pathologist2)

  result <- agreement(
    data = test_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})
