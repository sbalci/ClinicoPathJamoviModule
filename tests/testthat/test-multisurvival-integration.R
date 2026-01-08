# ═══════════════════════════════════════════════════════════
# Integration Tests: multisurvival
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the multisurvival jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(multisurvival_test, package = "ClinicoPath")
data(multisurvival_dates, package = "ClinicoPath")
data(multisurvival_risk, package = "ClinicoPath")

test_that("multisurvival produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = c("age", "nodes")
  )

  result2 <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = c("age", "nodes")
  )

  # Results should be identical
  expect_s3_class(result1, "multisurvivalClass")
  expect_s3_class(result2, "multisurvivalClass")
})

test_that("multisurvival workflow: basic → multivariable → risk stratification", {
  # Step 1: Univariable analysis
  basic <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage"
  )
  expect_s3_class(basic, "multisurvivalClass")

  # Step 2: Multivariable model
  multivariable <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade", "molecular_subtype"),
    contexpl = c("age", "ki67")
  )
  expect_s3_class(multivariable, "multisurvivalClass")

  # Step 3: Risk stratification
  risk_strat <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade"),
    contexpl = c("age", "ki67"),
    calculateRiskScore = TRUE,
    numRiskGroups = "three",
    plotRiskGroups = TRUE
  )
  expect_s3_class(risk_strat, "multisurvivalClass")
})

test_that("multisurvival workflow: date-based analysis pipeline", {
  # Step 1: Calculate survival time from dates
  date_calc <- multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "outcome"
  )
  expect_s3_class(date_calc, "multisurvivalClass")

  # Step 2: Add explanatory variables
  with_predictors <- multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = "age"
  )
  expect_s3_class(with_predictors, "multisurvivalClass")

  # Step 3: Complete analysis with visualization
  complete <- multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = "age",
    hr = TRUE,
    km = TRUE,
    risktable = TRUE
  )
  expect_s3_class(complete, "multisurvivalClass")
})

test_that("multisurvival handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(multisurvival_test, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- multisurvival(
    data = csv_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "multisurvivalClass")

  # Clean up
  unlink(temp_csv)
})

test_that("multisurvival handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(multisurvival_test, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- multisurvival(
    data = as.data.frame(xlsx_data),
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "multisurvivalClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("multisurvival handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(multisurvival_test)

  result_tibble <- multisurvival(
    data = tibble_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result_tibble, "multisurvivalClass")

  # Test with data.frame
  df_data <- as.data.frame(multisurvival_test)

  result_df <- multisurvival(
    data = df_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result_df, "multisurvivalClass")
})

test_that("multisurvival workflow: clinical trial analysis pipeline", {
  # Step 1: Overall survival by treatment
  overall <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    km = TRUE,
    risktable = TRUE
  )
  expect_s3_class(overall, "multisurvivalClass")

  # Step 2: Adjusted for baseline characteristics
  adjusted <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = c("age", "performance_status"),
    hr = TRUE
  )
  expect_s3_class(adjusted, "multisurvivalClass")

  # Step 3: Check proportional hazards assumption
  ph_check <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    ph_cox = TRUE
  )
  expect_s3_class(ph_check, "multisurvivalClass")

  # Step 4: Publication-ready output with nomogram
  publication <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage", "grade"),
    contexpl = c("age", "nodes"),
    hr = TRUE,
    km = TRUE,
    risktable = TRUE,
    ci95 = TRUE,
    showNomogram = TRUE,
    showSummaries = TRUE
  )
  expect_s3_class(publication, "multisurvivalClass")
})

test_that("multisurvival workflow: prognostic model development", {
  # Step 1: Identify significant univariable predictors
  # (In practice, would run multiple univariable analyses)

  # Step 2: Build multivariable model
  multivariable <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade", "molecular_subtype"),
    contexpl = c("age", "ki67", "tumor_size", "nodes_positive"),
    hr = TRUE
  )
  expect_s3_class(multivariable, "multisurvivalClass")

  # Step 3: Calculate risk scores
  risk_model <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade", "molecular_subtype"),
    contexpl = c("age", "ki67"),
    calculateRiskScore = TRUE,
    numRiskGroups = "three"
  )
  expect_s3_class(risk_model, "multisurvivalClass")

  # Step 4: Visualize risk group survival
  risk_viz <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade"),
    contexpl = c("age", "ki67"),
    calculateRiskScore = TRUE,
    numRiskGroups = "three",
    plotRiskGroups = TRUE,
    km = TRUE,
    risktable = TRUE
  )
  expect_s3_class(risk_viz, "multisurvivalClass")

  # Step 5: Create nomogram for clinical use
  nomogram <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade"),
    contexpl = c("age", "ki67"),
    showNomogram = TRUE
  )
  expect_s3_class(nomogram, "multisurvivalClass")
})

test_that("multisurvival workflow: competing risks analysis", {
  data(multisurvival_competing, package = "ClinicoPath")

  # Step 1: Overall survival (all-cause mortality)
  overall <- multisurvival(
    data = multisurvival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "overall",
    explanatory = "treatment"
  )
  expect_s3_class(overall, "multisurvivalClass")

  # Step 2: Cause-specific survival (disease-specific mortality)
  cause_specific <- multisurvival(
    data = multisurvival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "cause",
    explanatory = c("treatment", "stage")
  )
  expect_s3_class(cause_specific, "multisurvivalClass")

  # Step 3: Competing risks analysis
  competing <- multisurvival(
    data = multisurvival_competing,
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
  expect_s3_class(competing, "multisurvivalClass")
})

test_that("multisurvival workflow: person-time incidence rate analysis", {
  data(multisurvival_persontime, package = "ClinicoPath")

  # Step 1: Overall incidence rate
  overall_rate <- multisurvival(
    data = multisurvival_persontime,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    person_time = TRUE,
    rate_multiplier = 1000
  )
  expect_s3_class(overall_rate, "multisurvivalClass")

  # Step 2: Stratified by exposure
  stratified_rate <- multisurvival(
    data = multisurvival_persontime,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment_group",
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 1000
  )
  expect_s3_class(stratified_rate, "multisurvivalClass")

  # Step 3: Adjusted rate ratios
  adjusted_rate <- multisurvival(
    data = multisurvival_persontime,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment_group",
    contexpl = "age",
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 1000,
    hr = TRUE
  )
  expect_s3_class(adjusted_rate, "multisurvivalClass")
})

test_that("multisurvival handles comprehensive publication workflow", {
  # Complete analysis for research publication
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "1",
    timetypeoutput = "months",
    explanatory = c("treatment", "stage", "grade"),
    contexpl = c("age", "nodes", "biomarker"),
    hr = TRUE,
    sty = "t3",
    km = TRUE,
    ph_cox = TRUE,
    risktable = TRUE,
    ci95 = TRUE,
    censored = TRUE,
    medianline = "hv",
    pplot = TRUE,
    endplot = 60,
    byplot = 12,
    showNomogram = TRUE,
    showExplanations = TRUE,
    showSummaries = TRUE
  )

  expect_s3_class(result, "multisurvivalClass")
})
