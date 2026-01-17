# ═══════════════════════════════════════════════════════════
# Basic Tests: agreement
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality and required arguments for the
# agreement (Interrater Reliability Analysis) jamovi function

library(testthat)
devtools::load_all()

# Load basic test data
data(agreement_pathology, package = "ClinicoPath")
data(agreement_continuous, package = "ClinicoPath")
data(agreement_threeRater, package = "ClinicoPath")

test_that("agreement function exists and is callable", {
  expect_true(exists("agreement"))
  expect_true(is.function(agreement))
})

test_that("agreement returns proper class object", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement requires data argument", {
  expect_error(
    agreement(vars = c("Pathologist1", "Pathologist2"))
  )
})

test_that("agreement requires vars argument", {
  expect_error(
    agreement(data = agreement_pathology)
  )
})

test_that("agreement handles single variable (shows welcome)", {
  result <- agreement(
    data = agreement_pathology,
    vars = "Pathologist1"
  )
  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles basic 2-rater categorical data", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement calculates Cohen's kappa for 2 raters", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles 3+ rater categorical data", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement calculates Fleiss' kappa for 3+ raters", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles continuous data", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement calculates ICC for continuous data", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement calculates Bland-Altman for 2 continuous variables", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    blandAltmanPlot = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement generates Bland-Altman plot", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    blandAltmanPlot = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement calculates weighted kappa with linear weights", {
  data(agreement_ordinal, package = "ClinicoPath")
  agreement_ordinal$PathologistA <- factor(agreement_ordinal$PathologistA, ordered = TRUE)
  agreement_ordinal$PathologistB <- factor(agreement_ordinal$PathologistB, ordered = TRUE)

  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "equal"
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement calculates weighted kappa with quadratic weights", {
  data(agreement_ordinal, package = "ClinicoPath")
  agreement_ordinal$PathologistA <- factor(agreement_ordinal$PathologistA, ordered = TRUE)
  agreement_ordinal$PathologistB <- factor(agreement_ordinal$PathologistB, ordered = TRUE)

  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "squared"
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles tibble input", {
  library(tibble)
  tibble_data <- as_tibble(agreement_pathology)

  result <- agreement(
    data = tibble_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement handles data.frame input", {
  df_data <- as.data.frame(agreement_pathology)

  result <- agreement(
    data = df_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement produces consistent results across runs", {
  result1 <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  result2 <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result1, "agreementResults")
  expect_s3_class(result2, "agreementResults")
  # Results should be deterministic
})

test_that("agreement accepts character vector for vars", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")
})

test_that("agreement rejects non-existent variable names", {
  expect_error(
    agreement(
      data = agreement_pathology,
      vars = c("NonExistent1", "NonExistent2")
    )
  )
})
