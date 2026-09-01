# Bundled fixtures exercise real output contents and explicit invalid-data behavior.
lassocox_integration_data <- function(name) {
  path <- file.path("../../data", paste0(name, ".rda"))
  if (!file.exists(path)) path <- system.file("data", paste0(name, ".rda"), package = "ClinicoPath")
  if (!nzchar(path) || !file.exists(path)) skip(paste(name, "fixture unavailable"))
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  object <- if (name %in% loaded) name else if (length(loaded) == 1) loaded else
    stop("Ambiguous fixture object names")
  data <- env[[object]]
  stopifnot(is.data.frame(data))
  data
}

lassocox_integration_run <- function(data, time, status, event, predictors, ...) {
  skip_if_not_installed("glmnet")
  args <- modifyList(list(elapsedtime = time, outcome = status, outcomeLevel = event,
    censorLevel = NULL, explanatory = predictors, cv_plot = FALSE, coef_plot = FALSE,
    survival_plot = FALSE), list(...))
  opts <- do.call(getFromNamespace("lassocoxOptions", "ClinicoPath")$new, args)
  a <- getFromNamespace("lassocoxClass", "ClinicoPath")$new(options = opts, data = data)
  a$run()
  a$results
}

lassocox_integration_valid <- function(r) {
  expect_gt(r$modelSummary$rowCount, 0)
  expect_false(grepl("Analysis Error", r$todo$content, fixed = TRUE))
  cindex <- as.numeric(as.data.frame(r$performance)$value)
  expect_true(length(cindex) == 1 && is.finite(cindex) && cindex >= 0 && cindex <= 1)
  tab <- as.data.frame(r$coefficients)
  selected <- !is.na(tab$coefficient)
  expect_true(all(is.finite(tab$coefficient[selected])))
  if (any(selected)) expect_equal(tab$hazardRatio[selected], exp(tab$coefficient[selected]))
  else expect_equal(tab$variable, "No variables selected")
}

test_that("lassocox integrates breast-cancer coding and exploratory Cox refits", {
  d <- lassocox_integration_data("lassocox_breast_cancer")
  r <- lassocox_integration_run(d, "survival_months", "death", "Dead",
    c("age", "tumor_size_cm", "grade", "stage", "lymph_nodes_positive",
      "lymph_nodes_examined", "er_status", "pr_status", "her2_status", "ki67_percent",
      "histology", "lvi", "margin_status", "surgery_type", "chemotherapy", "radiation",
      "albumin", "hemoglobin"), lambda = "lambda.min", showVariableImportance = TRUE,
    showModelComparison = TRUE, showEncoding = TRUE)
  lassocox_integration_valid(r)
  expect_equal(r$modelComparison$rowCount, 2)
  expect_true(all(is.finite(as.data.frame(r$modelComparison)$cindex)))
  expect_gt(r$encoding$rowCount, 18)
})

test_that("lassocox reports inadequate censoring and fits an independent-censoring trial", {
  d <- lassocox_integration_data("lassocox_lung_cancer")
  predictors <- c("age", "gender", "smoking_status", "histology", "stage", "tumor_size_cm",
    "ecog_performance_status", "hemoglobin_g_dl", "wbc_count_k_ul", "platelet_count_k_ul",
    "creatinine_mg_dl", "treatment_type")
  rejected <- lassocox_integration_run(d, "follow_up_months", "progression", "Yes", predictors)
  expect_equal(rejected$modelSummary$rowCount, 0)
  expect_match(rejected$todo$content, "zero values")
  positive <- d
  positive$follow_up_months <- seq_len(nrow(d))
  rejected <- lassocox_integration_run(positive, "follow_up_months", "progression", "Yes", predictors)
  expect_match(rejected$todo$content, "at least 3 events and 3 censored")
  # Preserve the bundled stress fixture; simulate a usable endpoint in memory only.
  withr::local_seed(455)
  event <- rexp(nrow(d), exp(.03 * (d$age - mean(d$age))))
  censor <- rexp(nrow(d), .5)
  d$follow_up_months <- pmin(event, censor)
  d$progression <- factor(ifelse(event <= censor, "Yes", "No"))
  r <- lassocox_integration_run(d, "follow_up_months", "progression", "Yes", predictors)
  lassocox_integration_valid(r)
  expect_match(r$todo$content, "Excluded [0-9]+ row")
})

test_that("lassocox integrates cardiovascular predictors and missingness reporting", {
  d <- lassocox_integration_data("lassocox_cardiovascular")
  r <- lassocox_integration_run(d, "time_to_event_months", "cv_event", "Event",
    c("age_years", "gender", "race_ethnicity", "bmi_kg_m2", "systolic_bp_mmhg",
      "diastolic_bp_mmhg", "total_cholesterol_mg_dl", "hdl_cholesterol_mg_dl",
      "ldl_cholesterol_mg_dl", "diabetes_mellitus", "hypertension", "smoking_status",
      "family_history_cvd", "ace_inhibitor_use", "statin_use", "aspirin_use"))
  lassocox_integration_valid(r)
  expect_match(r$suitabilityReport$content, "Data Quality")
})

test_that("lassocox integrates a small cohort with reproducible reduced folds", {
  d <- lassocox_integration_data("lassocox_small_cohort")
  # Rounded zero times in the bundled stress fixture must be rejected.
  original <- lassocox_integration_run(d, "time_months", "event_occurred", "Yes",
    c("age", "biomarker_a"))
  expect_equal(original$modelSummary$rowCount, 0)
  expect_match(original$todo$content, "zero values")
  withr::local_seed(612)
  d$time_months <- rexp(nrow(d))  # independent synthetic times, in memory only
  r <- lassocox_integration_run(d, "time_months", "event_occurred", "Yes",
    c("age", "gender", "biomarker_a", "biomarker_b", "biomarker_c", "treatment_group",
      "severity_score"), nfolds = 5, showReproducibility = TRUE)
  lassocox_integration_valid(r)
  expect_match(paste(as.data.frame(r$reproducibility)$item, collapse = " "), "Fold 1")
})

test_that("lassocox integrates a dynamically specified genomic predictor vector", {
  d <- lassocox_integration_data("lassocox_genomic")
  predictors <- c("age", "sex", "tumor_stage", grep("^gene_", names(d), value = TRUE))
  original <- lassocox_integration_run(d, "os_months", "vital_status", "Dead", predictors)
  expect_equal(original$modelSummary$rowCount, 0)
  expect_match(original$todo$content, "at least 3 events and 3 censored")
  withr::local_seed(714)
  event <- rexp(nrow(d), exp(.8 * as.numeric(scale(d[[predictors[4]]]))))
  censor <- rexp(nrow(d), .3)
  d$os_months <- pmin(event, censor)
  d$vital_status <- factor(ifelse(event <= censor, "Dead", "Alive"))
  r <- lassocox_integration_run(d, "os_months", "vital_status", "Dead", predictors,
    nfolds = 5, showVariableImportance = TRUE, showEncoding = TRUE)
  lassocox_integration_valid(r)
  expect_setequal(unique(as.data.frame(r$encoding)$variable), predictors)
  tab <- as.data.frame(r$modelSummary)
  expect_lte(as.numeric(tab$value[tab$statistic == "Selected Predictor Columns"]), r$encoding$rowCount)
})

test_that("lassocox integrates highly correlated biomarkers without a selection guarantee", {
  d <- lassocox_integration_data("lassocox_multicollinear")
  r <- lassocox_integration_run(d, "survival_months", "death", "Dead",
    c("age", "sex", "ecog_ps", "comorbidity_index", "crp_mg_l", "esr_mm_hr", "il6_pg_ml",
      "ferritin_ng_ml", "albumin_g_dl", "prealbumin_mg_dl", "bmi", "weight_loss_pct"),
    showModelComparison = TRUE)
  lassocox_integration_valid(r)
  expect_equal(r$modelComparison$rowCount, 2)
  expect_match(r$suitabilityReport$content, "collinearity", ignore.case = TRUE)
})

test_that("lassocox fixture outputs reproduce exactly at the same seed", {
  d <- lassocox_integration_data("lassocox_breast_cancer")
  args <- list(data = d, time = "survival_months", status = "death", event = "Dead",
    predictors = c("age", "tumor_size_cm", "grade", "lymph_nodes_positive", "ki67_percent"),
    random_seed = 42, showReproducibility = TRUE)
  a <- do.call(lassocox_integration_run, args)
  b <- do.call(lassocox_integration_run, args)
  lassocox_integration_valid(a)
  expect_equal(as.data.frame(a$coefficients), as.data.frame(b$coefficients), tolerance = 0)
  expect_equal(as.data.frame(a$performance), as.data.frame(b$performance), tolerance = 0)
  expect_equal(as.data.frame(a$reproducibility), as.data.frame(b$reproducibility), tolerance = 0)
})

test_that("lassocox fills all enabled result panels in a complete workflow", {
  d <- lassocox_integration_data("lassocox_breast_cancer")
  r <- lassocox_integration_run(d, "survival_months", "death", "Dead",
    c("age", "tumor_size_cm", "grade", "stage", "lymph_nodes_positive", "er_status",
      "her2_status", "ki67_percent"), lambda = "lambda.min", cv_plot = TRUE, coef_plot = TRUE,
    survival_plot = TRUE, path_plot = TRUE, showSummary = TRUE, showExplanations = TRUE,
    showMethodologyNotes = TRUE, includeClinicalGuidance = TRUE, showVariableImportance = TRUE,
    showModelComparison = TRUE, showEncoding = TRUE, showReproducibility = TRUE, showRCode = TRUE)
  lassocox_integration_valid(r)
  for (name in c("cv_plot", "coef_plot", "survival_plot", "path_plot")) {
    expect_false(is.null(r[[name]]$state), info = name)
  }
  for (name in c("summaryText", "lassoExplanation", "methodologyNotes", "clinicalGuidance",
    "regularizationPathExplanation", "crossValidationExplanation", "riskScoreExplanation", "rCode")) {
    expect_gt(nchar(r[[name]]$content), 20, label = name)
  }
  expect_gt(r$encoding$rowCount, 0)
  expect_gt(r$reproducibility$rowCount, 0)
})
