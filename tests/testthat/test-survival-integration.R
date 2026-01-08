# ═══════════════════════════════════════════════════════════
# Integration Tests: survival
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the survival jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(survival_test, package = "ClinicoPath")

test_that("survival produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  result2 <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  # Results should be identical (no randomness)
  expect_s3_class(result1, "survivalClass")
  expect_s3_class(result2, "survivalClass")
})

test_that("survival workflow: basic → plots → diagnostics", {
  # Step 1: Basic analysis
  basic <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(basic, "survivalClass")

  # Step 2: Add plots
  with_plots <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    risktable = TRUE,
    ci95 = TRUE
  )
  expect_s3_class(with_plots, "survivalClass")

  # Step 3: Add diagnostics
  with_diagnostics <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    ph_cox = TRUE,
    residual_diagnostics = TRUE
  )
  expect_s3_class(with_diagnostics, "survivalClass")
})

test_that("survival workflow: date-based → calculated time → analysis", {
  data(survival_dates, package = "ClinicoPath")

  # Calculate time from dates
  result <- survival(
    data = survival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    outcome = "outcome",
    explanatory = "treatment",
    timetypedata = "ymd",
    timetypeoutput = "months"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival workflow: competing risks complete analysis", {
  data(survival_competing, package = "ClinicoPath")

  # Complete competing risks workflow
  result <- survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other",
    awd = "Alive w Disease",
    awod = "Alive w/o Disease",
    analysistype = "compete",
    multievent = TRUE,
    explanatory = "treatment",
    sc = TRUE,
    ce = TRUE,
    risktable = TRUE
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival workflow: landmark analysis", {
  data(survival_landmark, package = "ClinicoPath")

  # Step 1: Overall survival
  overall <- survival(
    data = survival_landmark,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(overall, "survivalClass")

  # Step 2: Landmark analysis from 6 months
  landmark <- survival(
    data = survival_landmark,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    uselandmark = TRUE,
    landmark = 6,
    explanatory = "response_6mo"
  )
  expect_s3_class(landmark, "survivalClass")
})

test_that("survival workflow: stratified Cox regression", {
  data(survival_stratified, package = "ClinicoPath")

  # Step 1: Test proportional hazards
  ph_test <- survival(
    data = survival_stratified,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    ph_cox = TRUE
  )
  expect_s3_class(ph_test, "survivalClass")

  # Step 2: Stratify on non-proportional variable
  stratified <- survival(
    data = survival_stratified,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    stratified_cox = TRUE,
    strata_variable = "sex"
  )
  expect_s3_class(stratified, "survivalClass")
})

test_that("survival workflow: person-time analysis", {
  data(survival_person_time, package = "ClinicoPath")

  # Complete person-time analysis
  result <- survival(
    data = survival_person_time,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "risk_category",
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100,
    sc = TRUE
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival workflow: RMST analysis", {
  data(survival_rmst, package = "ClinicoPath")

  # RMST with specified time horizon
  result <- survival(
    data = survival_rmst,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    rmst_analysis = TRUE,
    rmst_tau = 48,
    sc = TRUE
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(survival_test, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- survival(
    data = csv_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalClass")

  # Clean up
  unlink(temp_csv)
})

test_that("survival handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(survival_test, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- survival(
    data = as.data.frame(xlsx_data),
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("survival handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(survival_test)

  result_tibble <- survival(
    data = tibble_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result_tibble, "survivalClass")

  # Test with data.frame
  df_data <- as.data.frame(survival_test)

  result_df <- survival(
    data = df_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result_df, "survivalClass")
})

test_that("survival workflow: complete publication-ready analysis", {
  # Comprehensive analysis for publication
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    kmunicate = TRUE,
    risktable = TRUE,
    ci95 = TRUE,
    censored = TRUE,
    pplot = TRUE,
    medianline = "hv",
    pw = TRUE,
    padjustmethod = "holm",
    ph_cox = TRUE,
    residual_diagnostics = TRUE,
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rmst_analysis = TRUE,
    rmst_tau = 60,
    showExplanations = TRUE,
    showSummaries = TRUE
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival workflow: subgroup analysis", {
  # Analyze different subgroups
  result_stage1 <- survival(
    data = survival_test[survival_test$stage == "I", ],
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result_stage1, "survivalClass")

  result_stage4 <- survival(
    data = survival_test[survival_test$stage == "IV", ],
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result_stage4, "survivalClass")
})

test_that("survival workflow: multiple outcome comparisons", {
  data(survival_competing, package = "ClinicoPath")

  # Disease-specific death
  result_dod <- survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "cause",
    explanatory = "treatment"
  )
  expect_s3_class(result_dod, "survivalClass")

  # Other causes of death
  result_dooc <- survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Other",
    analysistype = "cause",
    explanatory = "treatment"
  )
  expect_s3_class(result_dooc, "survivalClass")
})

test_that("survival workflow: sensitivity analysis with different cutpoints", {
  # Cutpoints at 1, 3, 5 years
  result1 <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    cutp = "12, 36, 60"
  )
  expect_s3_class(result1, "survivalClass")

  # Different cutpoints
  result2 <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    cutp = "6, 24, 48"
  )
  expect_s3_class(result2, "survivalClass")
})

test_that("survival integrates with small dataset workflow", {
  data(survival_small, package = "ClinicoPath")

  # Quick analysis with small dataset
  result <- survival(
    data = survival_small,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival workflow: comparing analysis types", {
  data(survival_competing, package = "ClinicoPath")

  # Overall survival (ignores cause)
  result_overall <- survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "overall",
    explanatory = "treatment"
  )
  expect_s3_class(result_overall, "survivalClass")

  # Cause-specific (censors other causes)
  result_cause <- survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "cause",
    explanatory = "treatment"
  )
  expect_s3_class(result_cause, "survivalClass")

  # Competing risk (accounts for other causes)
  result_compete <- survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other",
    awd = "Alive w Disease",
    awod = "Alive w/o Disease",
    analysistype = "compete",
    multievent = TRUE,
    explanatory = "treatment"
  )
  expect_s3_class(result_compete, "survivalClass")
})
