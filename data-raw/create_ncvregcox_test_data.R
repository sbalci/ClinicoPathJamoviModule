# Test data generation for ncvregcox function
# Creates realistic survival datasets for testing SCAD/MCP/Lasso Cox regression
# with all ncvregcox options: penalty types, gamma, alpha, lambda selection,
# standardization, plots, and variable importance.

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(survival)

# Set seed for reproducibility
set.seed(2024)

# =============================================================================
# Dataset 1: ncvregcox_clinical (n=200, 15 covariates)
# =============================================================================
# Simulates a clinicopathological study with mixed variable types.
# Designed to test:
#   - SCAD, MCP, and Lasso penalty comparison
#   - Gamma parameter sensitivity
#   - Alpha elastic-net mixing
#   - Lambda selection (min vs 1se)
#   - Standardization effects (variables on different scales)
#   - All plots and variable importance output
#
# True effects: age, tumor_diameter, ldh_level, cea_level,
#               t_stage (T3/T4), n_stage (N2) --> 5-6 active predictors
# Noise variables: bmi, crp, albumin, wbc_count, neutrophil_ratio,
#                  platelet_count, gender, histology

create_ncvregcox_clinical <- function() {
  n <- 200

  # --- Continuous covariates ---
  age <- round(rnorm(n, mean = 62, sd = 11))
  age <- pmax(25, pmin(90, age))

  bmi <- round(rnorm(n, mean = 26.5, sd = 4.8), 1)
  bmi <- pmax(16, pmin(45, bmi))

  tumor_diameter <- round(rlnorm(n, meanlog = log(3.5), sdlog = 0.55), 1)
  tumor_diameter <- pmax(0.3, pmin(20, tumor_diameter))

  ldh_level <- round(rnorm(n, mean = 220, sd = 65))
  ldh_level <- pmax(80, pmin(600, ldh_level))

  crp <- round(rlnorm(n, meanlog = log(5), sdlog = 0.9), 1)
  crp <- pmax(0.1, pmin(150, crp))

  albumin <- round(rnorm(n, mean = 3.8, sd = 0.5), 1)
  albumin <- pmax(1.5, pmin(5.5, albumin))

  cea_level <- round(rlnorm(n, meanlog = log(4), sdlog = 1.1), 1)
  cea_level <- pmax(0.1, pmin(500, cea_level))

  wbc_count <- round(rnorm(n, mean = 7.5, sd = 2.8), 1)
  wbc_count <- pmax(2.0, pmin(25.0, wbc_count))

  neutrophil_ratio <- round(rnorm(n, mean = 60, sd = 10), 1)
  neutrophil_ratio <- pmax(20, pmin(95, neutrophil_ratio))

  platelet_count <- round(rnorm(n, mean = 250, sd = 75))
  platelet_count <- pmax(50, pmin(700, platelet_count))

  # --- Categorical covariates ---
  gender <- factor(sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)))

  t_stage <- factor(
    sample(c("T1", "T2", "T3", "T4"), n, replace = TRUE,
           prob = c(0.20, 0.30, 0.30, 0.20)),
    levels = c("T1", "T2", "T3", "T4")
  )

  n_stage <- factor(
    sample(c("N0", "N1", "N2"), n, replace = TRUE,
           prob = c(0.40, 0.35, 0.25)),
    levels = c("N0", "N1", "N2")
  )

  histology <- factor(
    sample(c("Adenocarcinoma", "Squamous", "Other"), n, replace = TRUE,
           prob = c(0.55, 0.30, 0.15)),
    levels = c("Adenocarcinoma", "Squamous", "Other")
  )

  # --- Introduce moderate correlations ---
  # Make ldh correlated with tumor_diameter (r ~ 0.35)
  ldh_level <- round(ldh_level + 8 * (tumor_diameter - mean(tumor_diameter)))
  ldh_level <- pmax(80, pmin(600, ldh_level))

  # Make crp correlated with wbc_count (r ~ 0.30)
  crp <- round(crp + 0.5 * (wbc_count - mean(wbc_count)), 1)
  crp <- pmax(0.1, pmin(150, crp))

  # --- True linear predictor (log-hazard) ---
  # 5-6 variables have real effects; rest are noise
  lp <- (
    0.025 * (age - 62) +                            # age: moderate effect
    0.12  * (tumor_diameter - 3.5) +                 # tumor_diameter: strong
    0.004 * (ldh_level - 220) +                      # ldh_level: moderate
    0.015 * (cea_level - 4) +                        # cea_level: moderate-weak
    0.45  * (t_stage == "T3") +                      # T3: increased risk
    0.85  * (t_stage == "T4") +                      # T4: strong increase
    0.55  * (n_stage == "N2")                        # N2: strong increase
  )
  # bmi, crp, albumin, wbc_count, neutrophil_ratio, platelet_count,
  # gender, histology --> no true effect (noise)

  # --- Generate survival times (Weibull baseline) ---
  shape_param <- 1.3
  scale_param <- 36
  survival_times <- rweibull(n, shape = shape_param,
                              scale = scale_param * exp(-lp / shape_param))
  survival_times <- pmax(0.5, survival_times)

  # --- Censoring: administrative at 60 months + random dropout ---
  admin_censor <- 60
  dropout_times <- rexp(n, rate = 0.012)
  censoring_times <- pmin(admin_censor, dropout_times)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  event_numeric <- as.integer(survival_times <= censoring_times)

  # Target ~55% event rate; adjust scale if needed
  # (With the parameters above, event rate is approximately 50-60%)

  # --- Assemble data frame ---
  ncvregcox_clinical <- data.frame(
    time          = observed_time,
    event         = factor(event_numeric, levels = c(0, 1)),
    age           = age,
    bmi           = bmi,
    tumor_diameter = tumor_diameter,
    ldh_level     = ldh_level,
    crp           = crp,
    albumin       = albumin,
    cea_level     = cea_level,
    wbc_count     = wbc_count,
    neutrophil_ratio = neutrophil_ratio,
    platelet_count = platelet_count,
    gender        = gender,
    t_stage       = t_stage,
    n_stage       = n_stage,
    histology     = histology,
    stringsAsFactors = FALSE
  )

  # --- Add a handful of missing values for realism ---
  set.seed(2024 + 1)
  miss_idx <- sample(n, 6)
  ncvregcox_clinical$crp[miss_idx[1]] <- NA
  ncvregcox_clinical$albumin[miss_idx[2]] <- NA
  ncvregcox_clinical$cea_level[miss_idx[3]] <- NA
  ncvregcox_clinical$bmi[miss_idx[4]] <- NA
  ncvregcox_clinical$ldh_level[miss_idx[5]] <- NA
  ncvregcox_clinical$platelet_count[miss_idx[6]] <- NA

  return(ncvregcox_clinical)
}


# =============================================================================
# Dataset 2: ncvregcox_sparse (n=100, 30 covariates)
# =============================================================================
# High-dimensional scenario designed to test the advantage of non-convex
# penalties (SCAD/MCP) over LASSO for variable selection.
#
# True effects: x1, x5, x12 (strong), x20 (moderate) --> 3-4 active
# The remaining 26-27 variables are pure noise.
# Some noise variables are correlated with each other to create a
# challenging variable-selection problem.

create_ncvregcox_sparse <- function() {
  n <- 100
  p <- 30

  # --- Generate correlated noise matrix ---
  # Create a block-correlated structure:
  #   Block 1 (x1-x5):   rho = 0.4
  #   Block 2 (x6-x10):  rho = 0.3
  #   Block 3 (x11-x15): rho = 0.2
  #   Block 4-6 (x16-x30): independent

  generate_block <- function(n, p_block, rho) {
    Sigma <- matrix(rho, nrow = p_block, ncol = p_block)
    diag(Sigma) <- 1
    L <- chol(Sigma)
    Z <- matrix(rnorm(n * p_block), nrow = n, ncol = p_block)
    X_block <- Z %*% L
    return(X_block)
  }

  set.seed(2024 + 42)
  block1 <- generate_block(n, 5, 0.4)
  block2 <- generate_block(n, 5, 0.3)
  block3 <- generate_block(n, 5, 0.2)
  block4 <- matrix(rnorm(n * 15), nrow = n, ncol = 15)

  X <- cbind(block1, block2, block3, block4)
  colnames(X) <- paste0("x", 1:p)

  # --- True coefficient vector (sparse) ---
  beta_true <- rep(0, p)
  beta_true[1]  <-  0.8    # x1: strong positive (in correlated block)
  beta_true[5]  <- -0.6    # x5: strong negative (in correlated block)
  beta_true[12] <-  0.7    # x12: strong positive (moderate-correlation block)
  beta_true[20] <- -0.4    # x20: moderate negative (independent)

  # --- Generate survival times ---
  lp <- X %*% beta_true
  shape_param <- 1.2
  scale_param <- 24
  survival_times <- rweibull(n, shape = shape_param,
                              scale = scale_param * exp(-lp / shape_param))
  survival_times <- pmax(0.3, survival_times)

  # --- Censoring ---
  admin_censor <- 48
  dropout_times <- rexp(n, rate = 0.015)
  censoring_times <- pmin(admin_censor, dropout_times)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  event_numeric <- as.integer(survival_times <= censoring_times)

  # --- Assemble data frame ---
  ncvregcox_sparse <- data.frame(
    time  = observed_time,
    event = factor(event_numeric, levels = c(0, 1)),
    as.data.frame(round(X, 4)),
    stringsAsFactors = FALSE
  )

  return(ncvregcox_sparse)
}


# =============================================================================
# Dataset 3: ncvregcox_small (n=25, 5 covariates)
# =============================================================================
# Small-sample edge case designed to test:
#   - Low EPV warnings
#   - Suitability assessment traffic-light system
#   - Stability with very few events
#   - CV folds > n/3 scenario

create_ncvregcox_small <- function() {
  set.seed(2024 + 99)
  n <- 25

  age <- round(rnorm(n, mean = 60, sd = 10))
  marker1 <- round(rnorm(n, mean = 50, sd = 15), 1)
  marker2 <- round(rnorm(n, mean = 100, sd = 30), 1)
  marker3 <- round(rnorm(n, mean = 5, sd = 2), 2)
  grade <- factor(sample(c("Low", "High"), n, replace = TRUE, prob = c(0.6, 0.4)),
                  levels = c("Low", "High"))

  lp <- 0.03 * (age - 60) + 0.02 * (marker1 - 50) + 0.5 * (grade == "High")
  survival_times <- rweibull(n, shape = 1.1, scale = 30 * exp(-lp / 1.1))
  survival_times <- pmax(0.5, survival_times)

  censor_times <- rexp(n, rate = 0.02)
  observed_time <- round(pmin(survival_times, censor_times), 1)
  event_numeric <- as.integer(survival_times <= censor_times)

  ncvregcox_small <- data.frame(
    time    = observed_time,
    event   = factor(event_numeric, levels = c(0, 1)),
    age     = age,
    marker1 = marker1,
    marker2 = marker2,
    marker3 = marker3,
    grade   = grade,
    stringsAsFactors = FALSE
  )

  return(ncvregcox_small)
}


# =============================================================================
# Dataset 4: ncvregcox_collinear (n=150, 10 covariates, high multicollinearity)
# =============================================================================
# Tests SCAD/MCP behavior under extreme collinearity (r > 0.9).
# Designed to trigger:
#   - Multicollinearity suitability check (red)
#   - Unstable variable selection
#   - Comparison of SCAD vs MCP stability

create_ncvregcox_collinear <- function() {
  set.seed(2024 + 77)
  n <- 150

  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)   # r ~ 0.995 with x1
  x3 <- x1 + rnorm(n, sd = 0.3)   # r ~ 0.96 with x1
  x4 <- rnorm(n)
  x5 <- x4 + rnorm(n, sd = 0.15)  # r ~ 0.99 with x4
  x6 <- rnorm(n)
  x7 <- rnorm(n)
  x8 <- rnorm(n)
  x9 <- rnorm(n)
  x10 <- rnorm(n)

  # True model: only x1 and x6 matter
  lp <- 0.7 * x1 + 0.5 * x6
  survival_times <- rweibull(n, shape = 1.2, scale = 24 * exp(-lp / 1.2))
  survival_times <- pmax(0.3, survival_times)

  censor_times <- rexp(n, rate = 0.015)
  observed_time <- round(pmin(survival_times, censor_times), 1)
  event_numeric <- as.integer(survival_times <= censor_times)

  ncvregcox_collinear <- data.frame(
    time  = observed_time,
    event = factor(event_numeric, levels = c(0, 1)),
    x1 = round(x1, 4), x2 = round(x2, 4), x3 = round(x3, 4),
    x4 = round(x4, 4), x5 = round(x5, 4), x6 = round(x6, 4),
    x7 = round(x7, 4), x8 = round(x8, 4), x9 = round(x9, 4),
    x10 = round(x10, 4),
    stringsAsFactors = FALSE
  )

  return(ncvregcox_collinear)
}


# =============================================================================
# Generate all datasets
# =============================================================================
cat("Generating test datasets for ncvregcox function...\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

ncvregcox_clinical   <- create_ncvregcox_clinical()
ncvregcox_sparse     <- create_ncvregcox_sparse()
ncvregcox_small      <- create_ncvregcox_small()
ncvregcox_collinear  <- create_ncvregcox_collinear()

# --- Summary statistics ---
cat("\nDataset 1: ncvregcox_clinical\n")
cat("  Dimensions: ", nrow(ncvregcox_clinical), " x ", ncol(ncvregcox_clinical), "\n")
cat("  Events:     ", sum(ncvregcox_clinical$event == "1", na.rm = TRUE), " / ",
    nrow(ncvregcox_clinical), " (",
    round(100 * mean(ncvregcox_clinical$event == "1", na.rm = TRUE), 1), "%)\n")
cat("  Median time:", round(median(ncvregcox_clinical$time), 1), "months\n")
cat("  Missing:    ", sum(is.na(ncvregcox_clinical)), "values\n")
cat("  Covariates: 10 continuous + 4 categorical = 14 total\n")
cat("  True effects: age, tumor_diameter, ldh_level, cea_level, t_stage (T3/T4), n_stage (N2)\n")

cat("\nDataset 2: ncvregcox_sparse\n")
cat("  Dimensions: ", nrow(ncvregcox_sparse), " x ", ncol(ncvregcox_sparse), "\n")
cat("  Events:     ", sum(ncvregcox_sparse$event == "1", na.rm = TRUE), " / ",
    nrow(ncvregcox_sparse), " (",
    round(100 * mean(ncvregcox_sparse$event == "1", na.rm = TRUE), 1), "%)\n")
cat("  Median time:", round(median(ncvregcox_sparse$time), 1), "months\n")
cat("  Covariates: 30 continuous (all standardized)\n")
cat("  True effects: x1 (+0.8), x5 (-0.6), x12 (+0.7), x20 (-0.4)\n")
cat("  Correlation blocks: x1-x5 (rho=0.4), x6-x10 (rho=0.3), x11-x15 (rho=0.2)\n")

cat("\nDataset 3: ncvregcox_small\n")
cat("  Dimensions: ", nrow(ncvregcox_small), " x ", ncol(ncvregcox_small), "\n")
cat("  Events:     ", sum(ncvregcox_small$event == "1", na.rm = TRUE), " / ",
    nrow(ncvregcox_small), "\n")
cat("  Covariates: 4 continuous + 1 categorical = 5 total\n")
cat("  Purpose: Small-sample edge case for EPV warnings and suitability checks\n")

cat("\nDataset 4: ncvregcox_collinear\n")
cat("  Dimensions: ", nrow(ncvregcox_collinear), " x ", ncol(ncvregcox_collinear), "\n")
cat("  Events:     ", sum(ncvregcox_collinear$event == "1", na.rm = TRUE), " / ",
    nrow(ncvregcox_collinear), "\n")
cat("  Covariates: 10 continuous with high multicollinearity (r > 0.9)\n")
cat("  Purpose: Extreme collinearity edge case for suitability red flags\n")


# =============================================================================
# Save datasets
# =============================================================================
cat("\n", paste(rep("-", 60), collapse = ""), "\n")
cat("Saving datasets...\n")

save_data_multi_format(ncvregcox_clinical,   "ncvregcox_clinical",   save_csv = TRUE)
save_data_multi_format(ncvregcox_sparse,     "ncvregcox_sparse",     save_csv = TRUE)
save_data_multi_format(ncvregcox_small,      "ncvregcox_small",      save_csv = TRUE)
save_data_multi_format(ncvregcox_collinear,  "ncvregcox_collinear",  save_csv = TRUE)


# =============================================================================
# Clinical interpretation guide
# =============================================================================
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TESTING GUIDE FOR ncvregcox DATASETS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n1. ncvregcox_clinical (Clinical Scenario, n=200)\n")
cat("   Purpose: Realistic clinicopathological study with mixed types\n")
cat("   Variables with true effects:\n")
cat("     - age (continuous, weak)\n")
cat("     - tumor_diameter (continuous, strong)\n")
cat("     - ldh_level (continuous, moderate, correlated with tumor_diameter)\n")
cat("     - cea_level (continuous, weak-moderate)\n")
cat("     - t_stage T3/T4 (categorical, strong)\n")
cat("     - n_stage N2 (categorical, strong)\n")
cat("   Noise variables: bmi, crp, albumin, wbc_count, neutrophil_ratio,\n")
cat("                    platelet_count, gender, histology\n")
cat("   Test scenarios:\n")
cat("     - SCAD should select true effects and drop noise\n")
cat("     - Gamma variations (3.7 vs lower) affect how aggressively noise is dropped\n")
cat("     - Standardization matters because scales differ (age~62, ldh~220, crp~5)\n")
cat("     - Lambda 1se should give sparser model than lambda min\n")

cat("\n2. ncvregcox_sparse (High-Dimensional Scenario, n=100, p=30)\n")
cat("   Purpose: Demonstrate SCAD/MCP advantage over Lasso\n")
cat("   True signal: 4 variables out of 30 (87% sparsity)\n")
cat("   Correlated blocks test variable selection under collinearity\n")
cat("   Test scenarios:\n")
cat("     - SCAD/MCP should recover x1, x5, x12, x20 more accurately than Lasso\n")
cat("     - Lasso may over-select correlated noise variables\n")
cat("     - Alpha < 1 (elastic net) may help with correlated blocks\n")

cat("\n3. ncvregcox_small (Small Sample Edge Case, n=25)\n")
cat("   Purpose: Test low-EPV warnings and suitability checks\n")
cat("   Test scenarios:\n")
cat("     - Should trigger STRONG_WARNING or WARNING for low events\n")
cat("     - Suitability check should show yellow/red for sample size\n")
cat("     - CV folds may need to be reduced (e.g., 3-5)\n")

cat("\n4. ncvregcox_collinear (Extreme Collinearity, n=150, p=10)\n")
cat("   Purpose: Test behavior under r > 0.9 collinearity\n")
cat("   Test scenarios:\n")
cat("     - Suitability check should flag red for multicollinearity\n")
cat("     - SCAD/MCP selection may be unstable (x1 vs x2 vs x3 swap)\n")
cat("     - Compare alpha=1 vs alpha=0.5 for stability\n")

cat("\nTest datasets created successfully.\n")
