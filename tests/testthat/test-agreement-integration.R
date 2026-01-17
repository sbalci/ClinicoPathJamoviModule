# ═══════════════════════════════════════════════════════════
# Integration Tests: agreement
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the agreement jamovi function

library(testthat)
devtools::load_all()

# Load test data
data(agreement_pathology, package = "ClinicoPath")
data(agreement_threeRater, package = "ClinicoPath")
data(agreement_ordinal, package = "ClinicoPath")
data(agreement_continuous, package = "ClinicoPath")
data(agreement_multiRater, package = "ClinicoPath")
data(agreement_binary, package = "ClinicoPath")
data(agreement_testRetest, package = "ClinicoPath")
data(agreement_hierarchical, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Consistency Tests
# ═══════════════════════════════════════════════════════════

test_that("agreement produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  result2 <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )

  # Results should be identical (deterministic)
  expect_s3_class(result1, "agreementResults")
  expect_s3_class(result2, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Basic Pathology Agreement Study
# ═══════════════════════════════════════════════════════════

test_that("workflow: basic pathology interrater reliability", {
  # Step 1: Basic kappa
  basic <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )
  expect_s3_class(basic, "agreementResults")

  # Step 2: Add multiple measures
  comprehensive <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    gwet = TRUE,
    specificAgreement = TRUE
  )
  expect_s3_class(comprehensive, "agreementResults")

  # Step 3: Add visualizations
  with_viz <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    agreementHeatmap = TRUE
  )
  expect_s3_class(with_viz, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Multi-Rater Panel
# ═══════════════════════════════════════════════════════════

test_that("workflow: multi-rater expert panel analysis", {
  # Complete expert panel workflow
  result <- agreement(
    data = agreement_multiRater,
    vars = c("SeniorPath1", "SeniorPath2", "MidLevelPath", "JuniorPath1", "JuniorPath2"),
    lightKappa = TRUE,
    kripp = TRUE,
    finn = TRUE,
    raterClustering = TRUE,
    showDendrogram = TRUE,
    subgroupForestPlot = TRUE,
    subgroupVariable = "difficulty"
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Ordinal Data with Weighted Kappa
# ═══════════════════════════════════════════════════════════

test_that("workflow: ordinal grade agreement with weighted kappa", {
  # Compare unweighted vs weighted kappa
  data_ordered <- agreement_ordinal
  data_ordered$PathologistA <- factor(data_ordered$PathologistA, ordered = TRUE)
  data_ordered$PathologistB <- factor(data_ordered$PathologistB, ordered = TRUE)
  result <- agreement(
    data = data_ordered,
    vars = c("PathologistA", "PathologistB"),
    wght = "squared",
    agreementHeatmap = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Continuous Measurement Agreement
# ═══════════════════════════════════════════════════════════

test_that("workflow: continuous measurement agreement (Bland-Altman + ICC)", {
  # Comprehensive continuous agreement analysis
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE,
    iccType = "icc21",
    blandAltmanPlot = TRUE,
    linCCC = TRUE,
    meanPearson = TRUE,
    raterProfiles = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Test-Retest Reliability
# ═══════════════════════════════════════════════════════════

test_that("workflow: test-retest reliability study", {
  # Test-retest analysis (inter-rater AND intra-rater)
  result <- agreement(
    data = agreement_testRetest,
    vars = c("Rater1_T1", "Rater1_T2", "Rater2_T1", "Rater2_T2", "Rater3_T1", "Rater3_T2"),
    interIntraRater = TRUE,
    interIntraSeparator = "_",
    icc = TRUE,
    iccType = "icc31"
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Hierarchical Agreement
# ═══════════════════════════════════════════════════════════

test_that("workflow: hierarchical agreement (raters nested in institutions)", {
  # Hierarchical/multilevel agreement analysis
  result <- agreement(
    data = agreement_hierarchical,
    vars = c("HospitalA_Rater1", "HospitalA_Rater2", "HospitalB_Rater1",
             "HospitalB_Rater2", "HospitalC_Rater1", "HospitalC_Rater2"),
    hierarchicalKappa = TRUE,
    clusterVariable = "institution"
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Binary Specific Agreement
# ═══════════════════════════════════════════════════════════

test_that("workflow: binary diagnostic test agreement (PSA/NSA)", {
  # Positive and Negative Specific Agreement
  result <- agreement(
    data = agreement_binary,
    vars = c("PathologistX", "PathologistY"),
    specificAgreement = TRUE,
    agreementHeatmap = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Data Import Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(agreement_pathology, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  # Convert character columns back to factors
  csv_data$Pathologist1 <- factor(csv_data$Pathologist1)
  csv_data$Pathologist2 <- factor(csv_data$Pathologist2)

  result <- agreement(
    data = csv_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")

  # Clean up
  unlink(temp_csv)
})

test_that("workflow: data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(agreement_pathology, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  # Convert to data frame and factors
  xlsx_data <- as.data.frame(xlsx_data)
  xlsx_data$Pathologist1 <- factor(xlsx_data$Pathologist1)
  xlsx_data$Pathologist2 <- factor(xlsx_data$Pathologist2)

  result <- agreement(
    data = xlsx_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result, "agreementResults")

  # Clean up
  unlink(temp_xlsx)
})

test_that("workflow: handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(agreement_pathology)

  result_tibble <- agreement(
    data = tibble_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result_tibble, "agreementResults")

  # Test with data.frame
  df_data <- as.data.frame(agreement_pathology)

  result_df <- agreement(
    data = df_data,
    vars = c("Pathologist1", "Pathologist2")
  )

  expect_s3_class(result_df, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Publication-Ready Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: complete publication-ready categorical analysis", {
  # Comprehensive analysis for publication
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    gwet = TRUE,
    specificAgreement = TRUE,
    agreementHeatmap = TRUE,
    caseClustering = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

test_that("workflow: complete publication-ready continuous analysis", {
  # Comprehensive continuous agreement analysis for publication
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE,
    iccType = "icc21",
    blandAltmanPlot = TRUE,
    linCCC = TRUE,
    tdi = TRUE,
    raterProfiles = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Comparison Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: compare 2-rater vs 3-rater reliability", {
  # Two raters
  two_rater <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2")
  )
  expect_s3_class(two_rater, "agreementResults")

  # Three raters
  three_rater <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    lightKappa = TRUE
  )
  expect_s3_class(three_rater, "agreementResults")
})

test_that("workflow: compare unweighted vs weighted kappa", {
  data_ordered <- agreement_ordinal
  data_ordered$PathologistA <- factor(data_ordered$PathologistA, ordered = TRUE)
  data_ordered$PathologistB <- factor(data_ordered$PathologistB, ordered = TRUE)

  # Unweighted kappa
  unweighted <- agreement(
    data = data_ordered,
    vars = c("PathologistA", "PathologistB"),
    wght = "unweighted"
  )
  expect_s3_class(unweighted, "agreementResults")

  # Linear weighted kappa
  linear <- agreement(
    data = data_ordered,
    vars = c("PathologistA", "PathologistB"),
    wght = "equal"
  )
  expect_s3_class(linear, "agreementResults")

  # Quadratic weighted kappa
  quadratic <- agreement(
    data = data_ordered,
    vars = c("PathologistA", "PathologistB"),
    wght = "squared"
  )
  expect_s3_class(quadratic, "agreementResults")
})

test_that("workflow: compare different ICC models", {
  # ICC(2,1) - Two-way random effects, single measures
  icc21 <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE,
    iccType = "icc21"
  )
  expect_s3_class(icc21, "agreementResults")

  # ICC(3,1) - Two-way mixed effects, single measures
  icc31 <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE,
    iccType = "icc31"
  )
  expect_s3_class(icc31, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Subgroup Analysis Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: agreement by subgroup (stratified analysis)", {
  # Agreement stratified by specimen type
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    agreementBySubgroup = TRUE,
    subgroupVariable = "specimen_type",
    subgroupForestPlot = TRUE
  )

  expect_s3_class(result, "agreementResults")
})

# ═══════════════════════════════════════════════════════════
# Consensus Variable Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: create consensus variable with majority rule", {
  # Create consensus diagnosis from multiple raters
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    consensusRule = "majority"
  )

  expect_s3_class(result, "agreementResults")
})

test_that("workflow: create consensus variable with unanimous rule", {
  # Require unanimous agreement for consensus
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    consensusRule = "unanimous"
  )

  expect_s3_class(result, "agreementResults")
})
