# ═══════════════════════════════════════════════════════════
# Comprehensive Tests: survival function
# ═══════════════════════════════════════════════════════════
#
# Tests all major features of the survival jamovi function
# using the survival_comprehensive dataset (n=200).
#
# Covers: KM, Cox, competing risks, date-based time,
# person-time, RMST, age adjustment, stratification,
# RCS, pairwise, landmark, weighted log-rank, parametric,
# calibration, bootstrap validation, and edge cases.
#
# Generated: 2026-03-16
# ═══════════════════════════════════════════════════════════

library(testthat)

# ───────────────────────────────────────────────────────────
# Load test data
# ───────────────────────────────────────────────────────────

data(survival_comprehensive, package = "ClinicoPath")

# ───────────────────────────────────────────────────────────
# 1. Basic KM analysis
# ───────────────────────────────────────────────────────────

test_that("Basic KM analysis with time + outcome + explanatory works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage"
  )

  expect_s3_class(result, "survivalClass")
  expect_true("results" %in% names(result))
})


test_that("KM analysis without explanatory variable (overall survival)", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("KM analysis with two-level explanatory variable", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "Sex"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("KM analysis with TumorGrade as explanatory", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorGrade"
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 2. Cox regression with age adjustment
# ───────────────────────────────────────────────────────────

test_that("Cox regression with age adjustment works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    age_adjustment = TRUE,
    age_variable = "Age"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Age interaction test works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    age_adjustment = TRUE,
    age_variable = "Age",
    age_interaction = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Age-stratified Cox model works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    age_adjustment = TRUE,
    age_variable = "Age",
    age_stratified_cox = TRUE,
    age_group_cutpoints = "50, 65, 75"
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 3. Competing risks analysis
# ───────────────────────────────────────────────────────────

test_that("Competing risks analysis works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    analysistype = "compete",
    multievent = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Cause-specific survival analysis works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    analysistype = "cause",
    multievent = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 4. Date-based time calculation
# ───────────────────────────────────────────────────────────

test_that("Date-based survival time calculation works", {
  result <- survival(
    data = survival_comprehensive,
    tint = TRUE,
    dxdate = "DiagnosisDate",
    fudate = "LastFollowUpDate",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Date-based time with days output works", {
  result <- survival(
    data = survival_comprehensive,
    tint = TRUE,
    dxdate = "DiagnosisDate",
    fudate = "LastFollowUpDate",
    timetypedata = "ymd",
    timetypeoutput = "days",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "Sex"
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 5. Person-time analysis
# ───────────────────────────────────────────────────────────

test_that("Person-time metrics calculation works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Person-time with custom intervals and multiplier", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorGrade",
    person_time = TRUE,
    time_intervals = "6, 12, 24, 48",
    rate_multiplier = 1000
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 6. RMST analysis
# ───────────────────────────────────────────────────────────

test_that("RMST analysis with default tau works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    rmst_analysis = TRUE,
    rmst_tau = 0
  )

  expect_s3_class(result, "survivalClass")
})


test_that("RMST analysis with custom tau works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "Sex",
    rmst_analysis = TRUE,
    rmst_tau = 60
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 7. Pairwise comparisons
# ───────────────────────────────────────────────────────────

test_that("Pairwise comparisons with 4-level explanatory work", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    pw = TRUE,
    padjustmethod = "holm"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Pairwise with BH correction works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorGrade",
    pw = TRUE,
    padjustmethod = "BH"
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 8. Stratified Cox regression
# ───────────────────────────────────────────────────────────

test_that("Stratified Cox regression works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    stratified_cox = TRUE,
    strata_variable = "Sex"
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 9. Landmark analysis
# ───────────────────────────────────────────────────────────

test_that("Landmark analysis works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    uselandmark = TRUE,
    landmark = 12
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 10. Weighted log-rank tests
# ───────────────────────────────────────────────────────────

test_that("Weighted log-rank tests work (all types)", {
  for (test_type in c("logrank", "gehan_breslow", "tarone_ware",
                       "peto_peto", "fleming_harrington")) {
    result <- survival(
      data = survival_comprehensive,
      elapsedtime = "FollowUpMonths",
      outcome = "Status",
      outcomeLevel = "Dead of Disease",
      dod = "Dead of Disease",
      dooc = "Dead of Other Causes",
      awd = "Alive with Disease",
      awod = "Alive without Disease",
      explanatory = "TumorStage",
      weightedLogRank = TRUE,
      survivalTestType = test_type
    )

    expect_s3_class(result, "survivalClass",
                    info = paste("Failed for test type:", test_type))
  }
})


# ───────────────────────────────────────────────────────────
# 11. Proportional hazards assumption testing
# ───────────────────────────────────────────────────────────

test_that("PH assumption test works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    ph_cox = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 12. Residual diagnostics
# ───────────────────────────────────────────────────────────

test_that("Residual diagnostics work", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    residual_diagnostics = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 13. Plot generation
# ───────────────────────────────────────────────────────────

test_that("Survival curve plot generation works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    sc = TRUE,
    endplot = 80,
    byplot = 12,
    ci95 = TRUE,
    risktable = TRUE,
    censored = TRUE,
    pplot = TRUE,
    medianline = "hv"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Cumulative events and hazard plots work", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    ce = TRUE,
    ch = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Log-log plot works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    loglog = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


test_that("KMunicate-style plot works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    kmunicate = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 14. RCS (Restricted Cubic Splines) non-linearity assessment
# ───────────────────────────────────────────────────────────

test_that("RCS non-linearity assessment with Age works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    rcs_analysis = TRUE,
    rcs_variable = "Age",
    rcs_knots = 4
  )

  expect_s3_class(result, "survivalClass")
})


test_that("RCS with Ki67 (contains NAs) works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    rcs_analysis = TRUE,
    rcs_variable = "Ki67",
    rcs_knots = 3
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 15. Calibration curves
# ───────────────────────────────────────────────────────────

test_that("Calibration curves with default timepoint work", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    calibration_curves = TRUE,
    calibration_timepoint = 0,
    calibration_ngroups = 5
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Calibration curves with custom timepoint work", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    calibration_curves = TRUE,
    calibration_timepoint = 36,
    calibration_ngroups = 4
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 16. Bootstrap internal validation
# ───────────────────────────────────────────────────────────

test_that("Bootstrap internal validation works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    bootstrapValidation = TRUE,
    bootstrapValN = 50   # small N for speed in testing
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 17. Parametric survival models
# ───────────────────────────────────────────────────────────

test_that("Parametric Weibull model works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    use_parametric = TRUE,
    parametric_distribution = "weibull"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Parametric distribution comparison works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    use_parametric = TRUE,
    parametric_distribution = "weibull",
    compare_distributions = TRUE,
    parametric_diagnostics = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 18. Age standardization
# ───────────────────────────────────────────────────────────

test_that("Age standardization (indirect) works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    age_adjustment = TRUE,
    age_variable = "Age",
    age_standardization = TRUE,
    age_standardization_method = "indirect"
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 19. Comprehensive multi-feature analysis
# ───────────────────────────────────────────────────────────

test_that("Comprehensive analysis with many features enabled works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    cutp = "12, 36, 60",
    pw = TRUE,
    padjustmethod = "BH",
    ph_cox = TRUE,
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100,
    rmst_analysis = TRUE,
    rmst_tau = 60,
    sc = TRUE,
    endplot = 80,
    byplot = 12
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 20. Edge cases
# ───────────────────────────────────────────────────────────

test_that("Single-group analysis (no explanatory) works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Binary explanatory variable works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "LymphovascularInvasion"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Three-level explanatory variable works (MarginStatus)", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "MarginStatus"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Small subset analysis works", {
  # Take first 30 observations as a small dataset
  small_data <- survival_comprehensive[1:30, ]

  result <- survival(
    data = small_data,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "Sex"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Analysis with HER2Status (3 levels including Equivocal) works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "HER2Status"
  )

  expect_s3_class(result, "survivalClass")
})


test_that("REMARK checklist generation works", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    remark_checklist = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


test_that("Show explanations and summaries options work", {
  result <- survival(
    data = survival_comprehensive,
    elapsedtime = "FollowUpMonths",
    outcome = "Status",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other Causes",
    awd = "Alive with Disease",
    awod = "Alive without Disease",
    explanatory = "TumorStage",
    showExplanations = TRUE,
    showSummaries = TRUE
  )

  expect_s3_class(result, "survivalClass")
})


# ───────────────────────────────────────────────────────────
# 21. Data integrity checks
# ───────────────────────────────────────────────────────────

test_that("Dataset has expected dimensions", {
  expect_equal(nrow(survival_comprehensive), 200)
  expect_equal(ncol(survival_comprehensive), 15)
})


test_that("Dataset has expected variable names", {
  expected_vars <- c(
    "PatientID", "FollowUpMonths", "Status", "TumorStage", "Age", "Sex",
    "TumorGrade", "Ki67", "TumorSize", "LymphovascularInvasion",
    "MarginStatus", "DiagnosisDate", "LastFollowUpDate",
    "HER2Status", "ERStatus"
  )
  expect_true(all(expected_vars %in% names(survival_comprehensive)))
})


test_that("Dataset has correct factor levels", {
  expect_equal(
    levels(survival_comprehensive$Status),
    c("Alive without Disease", "Alive with Disease",
      "Dead of Disease", "Dead of Other Causes")
  )

  expect_equal(
    levels(survival_comprehensive$TumorStage),
    c("Stage I", "Stage II", "Stage III", "Stage IV")
  )

  expect_equal(
    levels(survival_comprehensive$TumorGrade),
    c("Grade 1", "Grade 2", "Grade 3")
  )
})


test_that("Dataset has realistic value ranges", {
  expect_true(all(survival_comprehensive$FollowUpMonths >= 0.5))
  expect_true(all(survival_comprehensive$Age >= 25 & survival_comprehensive$Age <= 90))
  expect_true(all(survival_comprehensive$Ki67[!is.na(survival_comprehensive$Ki67)] >= 0))
  expect_true(all(survival_comprehensive$Ki67[!is.na(survival_comprehensive$Ki67)] <= 100))
  expect_true(all(survival_comprehensive$TumorSize[!is.na(survival_comprehensive$TumorSize)] >= 0.5))
  expect_true(all(survival_comprehensive$TumorSize[!is.na(survival_comprehensive$TumorSize)] <= 15))
})


test_that("Dataset has expected missingness (~5%)", {
  ki67_missing_pct <- sum(is.na(survival_comprehensive$Ki67)) / 200
  size_missing_pct <- sum(is.na(survival_comprehensive$TumorSize)) / 200
  expect_true(ki67_missing_pct > 0.02 & ki67_missing_pct < 0.10)
  expect_true(size_missing_pct > 0.02 & size_missing_pct < 0.10)
})
