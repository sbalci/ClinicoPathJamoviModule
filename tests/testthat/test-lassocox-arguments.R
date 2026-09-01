# ===============================================================
# Argument Combination Tests: lassocox (Lasso-Cox Regression)
# ===============================================================
#
# Tests all option combinations: lambda selection, nfolds, standardize,
# suitability checks, plot toggles, explanatory output toggles, and
# variable importance / model comparison.
#
# Uses lassocox_lung_cancer (n=200, mixed variable types).

library(testthat)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
skip_lassocox_deps <- function() {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
}

load_lung_cancer <- function() {
  data_path <- file.path("../../data", "lassocox_lung_cancer.rda")
  if (!file.exists(data_path)) {
    data_path <- system.file("data", "lassocox_lung_cancer.rda",
                             package = "ClinicoPath")
  }
  if (data_path == "" || !file.exists(data_path)) {
    skip("lassocox_lung_cancer.rda not found")
  }
  env <- new.env()
  load(data_path, envir = env)
  d <- env$lassocox_lung_cancer
  # The bundled stress fixture has only two censored rows. Simulate a separate
  # independently censored endpoint for option-combination tests of valid fits.
  withr::local_seed(713)
  event_time <- rexp(nrow(d), exp(0.03 * (d$age - 60)) / 10)
  censor_time <- rexp(nrow(d), 0.06)
  d$follow_up_months <- pmin(event_time, censor_time)
  d$progression <- factor(ifelse(event_time <= censor_time, "Yes", "No"))
  d
}

# Common argument set for lung cancer data
lung_base_args <- function(data) {
  list(
    data = data,
    elapsedtime = "follow_up_months",
    outcome = "progression",
    outcomeLevel = "Yes",
    censorLevel = "No",
    explanatory = c("age", "gender", "smoking_status", "histology",
                     "stage", "tumor_size_cm", "ecog_performance_status",
                     "hemoglobin_g_dl", "wbc_count_k_ul",
                     "platelet_count_k_ul", "creatinine_mg_dl",
                     "treatment_type"),
    cv_plot = FALSE,
    coef_plot = FALSE,
    survival_plot = FALSE
  )
}

# ---------------------------------------------------------------
# Tests
# ---------------------------------------------------------------

test_that("lassocox works with all CV fold values (3, 5, 10)", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  for (nf in c(3, 5, 10)) {
    args <- lung_base_args(data)
    args$nfolds <- nf
    expect_no_error(do.call(lassocox, args))
  }
})

test_that("lassocox standardize on/off both work", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  for (std in c(TRUE, FALSE)) {
    args <- lung_base_args(data)
    args$standardize <- std
    expect_no_error(do.call(lassocox, args))
  }
})

test_that("lassocox suitabilityCheck toggle works", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  for (sc in c(TRUE, FALSE)) {
    args <- lung_base_args(data)
    args$suitabilityCheck <- sc
    expect_no_error(do.call(lassocox, args))
  }
})

test_that("lassocox plot toggles work independently", {
  skip_lassocox_deps()
  data <- load_lung_cancer()
  args <- lung_base_args(data)

  # All plots off (baseline)
  expect_no_error(do.call(lassocox, args))

  # Each plot on individually
  for (plot_opt in c("cv_plot", "coef_plot", "survival_plot", "path_plot")) {
    test_args <- args
    test_args[[plot_opt]] <- TRUE
    expect_no_error(do.call(lassocox, test_args))
  }

  # All plots on
  args$cv_plot <- TRUE
  args$coef_plot <- TRUE
  args$survival_plot <- TRUE
  expect_no_error(do.call(lassocox, args))
})

test_that("lassocox explanatory output toggles work", {
  skip_lassocox_deps()
  data <- load_lung_cancer()
  args <- lung_base_args(data)

  # Test each explanatory output option
  for (opt in c("showExplanations", "showMethodologyNotes",
                "includeClinicalGuidance", "showEncoding", "showReproducibility", "showRCode")) {
    test_args <- args
    test_args[[opt]] <- TRUE
    expect_no_error(do.call(lassocox, test_args))
  }
})

test_that("lassocox showSummary works", {
  skip_lassocox_deps()
  data <- load_lung_cancer()
  args <- lung_base_args(data)
  args$showSummary <- TRUE

  result <- do.call(lassocox, args)
  expect_true(!is.null(result$summaryText))
})

test_that("lassocox showVariableImportance works", {
  skip_lassocox_deps()
  data <- load_lung_cancer()
  args <- lung_base_args(data)
  args$showVariableImportance <- TRUE

  result <- do.call(lassocox, args)
  expect_true(!is.null(result$variableImportance))
})

test_that("lassocox showModelComparison works", {
  skip_lassocox_deps()
  data <- load_lung_cancer()
  args <- lung_base_args(data)
  args$showModelComparison <- TRUE

  result <- do.call(lassocox, args)
  expect_true(!is.null(result$modelComparison))
})

test_that("lassocox handles factor vs character explanatory variables", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  # Ensure factor
  data_factor <- data
  data_factor$gender <- as.factor(data_factor$gender)
  data_factor$stage <- as.factor(data_factor$stage)

  args <- lung_base_args(data_factor)
  expect_no_error(do.call(lassocox, args))

  # Convert to character
  data_char <- data
  data_char$gender <- as.character(data_char$gender)
  data_char$stage <- as.character(data_char$stage)

  args <- lung_base_args(data_char)
  expect_no_error(do.call(lassocox, args))
})

test_that("lassocox handles continuous-only explanatory variables", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  result <- lassocox(
    data = data,
    elapsedtime = "follow_up_months",
    outcome = "progression",
    outcomeLevel = "Yes",
    explanatory = c("age", "tumor_size_cm", "hemoglobin_g_dl",
                     "wbc_count_k_ul", "platelet_count_k_ul",
                     "creatinine_mg_dl"),
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE,
    censorLevel = "No"
  )

  expect_true(result$modelSummary$rowCount > 0)
})

test_that("lassocox handles factor-only explanatory variables", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  result <- lassocox(
    data = data,
    elapsedtime = "follow_up_months",
    outcome = "progression",
    outcomeLevel = "Yes",
    explanatory = c("gender", "smoking_status", "histology", "stage",
                     "ecog_performance_status", "treatment_type"),
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE,
    censorLevel = "No"
  )

  expect_true(result$modelSummary$rowCount > 0)
})

test_that("lassocox random_seed produces reproducible results", {
  skip_lassocox_deps()
  data <- load_lung_cancer()

  common_args <- lung_base_args(data)
  common_args$random_seed <- 42

  result1 <- do.call(lassocox, common_args)
  result2 <- do.call(lassocox, common_args)

  # Results should be identical with same seed
  # (compare coefficient table contents)
  expect_equal(
    as.data.frame(result1$coefficients),
    as.data.frame(result2$coefficients)
  )
})
