# Test data generation for plscox function
# Creates realistic survival datasets for PLS-based Cox Regression analysis
# Designed to exercise ALL plscox options: PLS algorithms, component selection,
# cross-validation, scaling, bootstrap validation, permutation tests, plots,
# risk stratification, and feature importance.

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(MASS)  # for mvrnorm (correlated metabolites)

# Set seed for reproducibility
set.seed(42)

# ==============================================================================
# Dataset 1: Metabolomics Survival Study (n=120, p=80)
# ==============================================================================
#
# Simulates a metabolomics profiling study in cancer patients.
# True survival signal comes from 2-3 latent biological components that
# each influence a cluster of 10-15 metabolites. This mirrors the structure
# PLS is designed to recover: correlated blocks of predictors driven by
# a small number of underlying latent factors.
#
# Design:
#   - 120 patients, ~50% event rate
#   - 80 metabolite measurements (METAB_001 to METAB_080)
#   - 3 clinical variables: age, gender, bmi
#   - Moderate inter-metabolite correlation (natural for metabolomics data)
#   - True signal from 3 latent components affecting different metabolite groups

create_plscox_metabolomics <- function() {
  n <- 120
  p <- 80

  # --- Clinical variables ---
  age <- round(rnorm(n, mean = 62, sd = 11))
  age[age < 30] <- 30
  age[age > 85] <- 85

  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))

  bmi <- round(rnorm(n, mean = 27.5, sd = 5.2), 1)
  bmi[bmi < 16] <- 16
  bmi[bmi > 45] <- 45

  # --- Define correlation structure for metabolites ---
  # Build a block-diagonal-ish correlation matrix with 3 correlated blocks
  # Block 1: Metabolites 1-15  (amino acid pathway)
  # Block 2: Metabolites 25-40 (lipid metabolism)
  # Block 3: Metabolites 55-70 (energy metabolism)
  # Other metabolites have weaker background correlations

  sigma <- diag(p)

  # Helper: set block correlations
  set_block_cor <- function(sigma, indices, rho) {
    for (i in indices) {
      for (j in indices) {
        if (i != j) {
          sigma[i, j] <- rho
        }
      }
    }
    return(sigma)
  }

  block1_idx <- 1:15    # amino acid pathway
  block2_idx <- 25:40   # lipid metabolism
  block3_idx <- 55:70   # energy metabolism

  sigma <- set_block_cor(sigma, block1_idx, 0.45)
  sigma <- set_block_cor(sigma, block2_idx, 0.40)
  sigma <- set_block_cor(sigma, block3_idx, 0.35)

  # Add weak background correlations between some non-block metabolites
  for (i in 1:p) {
    for (j in 1:p) {
      if (i != j && sigma[i, j] == 0) {
        if (abs(i - j) <= 3) {
          sigma[i, j] <- 0.10  # neighboring metabolites weakly correlated
        }
      }
    }
  }

  # Ensure positive definiteness
  eigen_vals <- eigen(sigma)$values
  if (any(eigen_vals <= 0)) {
    sigma <- sigma + diag(p) * (abs(min(eigen_vals)) + 0.01)
    diag(sigma) <- 1
  }

  # Generate correlated metabolite data
  metab_data <- mvrnorm(n = n, mu = rep(0, p), Sigma = sigma)

  # Add realistic metabolite means and scales (log-concentrations)
  metab_means <- runif(p, min = 2, max = 8)
  metab_sds <- runif(p, min = 0.5, max = 2.0)
  for (j in 1:p) {
    metab_data[, j] <- metab_data[, j] * metab_sds[j] + metab_means[j]
  }
  metab_data <- round(metab_data, 3)
  colnames(metab_data) <- paste0("METAB_", sprintf("%03d", 1:p))

  # --- Define true latent components affecting survival ---
  # Latent component 1: driven by amino acid pathway metabolites (block 1)
  #   Higher amino acid levels -> worse prognosis
  latent_1 <- rowMeans(metab_data[, block1_idx[1:12]])

  # Latent component 2: driven by lipid metabolism (block 2)
  #   Specific lipids protective
  latent_2 <- rowMeans(metab_data[, block2_idx[1:10]])

  # Latent component 3: driven by energy metabolism (block 3)
  #   Energy metabolite imbalance -> worse prognosis
  latent_3 <- rowMeans(metab_data[, block3_idx[1:8]])

  # Clinical effects
  clinical_effect <- 0.015 * (age - 62) +
                     0.2 * (as.numeric(gender) == 1) +
                     0.01 * (bmi - 27.5)

  # Linear predictor based on latent components + clinical
  linear_pred <- 0.08 * scale(latent_1) +
                 -0.06 * scale(latent_2) +
                 0.05 * scale(latent_3) +
                 clinical_effect

  # Generate survival times (Weibull distribution for realistic shape)
  shape_param <- 1.3
  scale_param <- exp(-as.numeric(linear_pred) * 0.4) * 24
  survival_times <- rweibull(n, shape = shape_param, scale = scale_param)

  # Censoring: administrative at 36 months + random loss to follow-up
  admin_censor <- 36
  loss_followup <- rexp(n, rate = 0.03)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1  # prevent zero times
  event <- as.numeric(survival_times <= censoring_times)

  # Create final dataset
  plscox_metabolomics <- data.frame(
    patient_id = paste0("MET_", sprintf("%03d", 1:n)),
    survival_months = observed_time,
    death = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    gender = gender,
    bmi = bmi,
    metab_data
  )

  return(plscox_metabolomics)
}

# ==============================================================================
# Dataset 2: Small PLS Dataset (n=50, p=25)
# ==============================================================================
#
# Smaller dataset for testing:
#   - LOO cross-validation (feasible at n=50)
#   - Edge cases with limited sample size
#   - Higher event rate (~60%)
#   - Fewer predictors for faster computation
#   - Simple correlation structure

create_plscox_small <- function() {
  n <- 50
  p <- 25

  # --- Build moderate correlation structure ---
  sigma <- diag(p)

  # Block 1: variables 1-8 (correlated biomarker panel)
  block1 <- 1:8
  for (i in block1) {
    for (j in block1) {
      if (i != j) sigma[i, j] <- 0.50
    }
  }

  # Block 2: variables 12-18 (second panel)
  block2 <- 12:18
  for (i in block2) {
    for (j in block2) {
      if (i != j) sigma[i, j] <- 0.40
    }
  }

  # Ensure positive definiteness
  eigen_vals <- eigen(sigma)$values
  if (any(eigen_vals <= 0)) {
    sigma <- sigma + diag(p) * (abs(min(eigen_vals)) + 0.01)
    diag(sigma) <- 1
  }

  # Generate data
  predictor_data <- mvrnorm(n = n, mu = rep(0, p), Sigma = sigma)

  # Scale to realistic biomarker ranges
  for (j in 1:p) {
    predictor_data[, j] <- round(predictor_data[, j] * runif(1, 0.8, 2.5) +
                                   runif(1, 3, 10), 2)
  }
  colnames(predictor_data) <- paste0("MARKER_", sprintf("%02d", 1:p))

  # True signal from first block
  latent_signal <- 0.12 * scale(rowMeans(predictor_data[, block1[1:5]])) +
                   -0.08 * scale(rowMeans(predictor_data[, block2[1:4]]))

  # Generate survival with higher event rate (~60%)
  survival_times <- rweibull(n, shape = 1.1,
                             scale = exp(-as.numeric(latent_signal) * 0.5) * 18)

  # Shorter follow-up to push event rate higher
  admin_censor <- 24
  censoring_times <- runif(n, min = 12, max = admin_censor)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1
  event <- as.numeric(survival_times <= censoring_times)

  plscox_small <- data.frame(
    id = paste0("SM_", sprintf("%02d", 1:n)),
    time_months = observed_time,
    status = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    predictor_data
  )

  return(plscox_small)
}


# ==============================================================================
# Dataset 3: Genomic p >> n Dataset (n=60, p=200)
# ==============================================================================
#
# True p >> n scenario typical of gene expression studies.
# Tests the core PLS use case: more variables than observations.
# Also includes some edge-case features:
#   - Near-zero-variance genes (sparse expression)
#   - A few negative/zero-like time values (tests validation guard)
#   - Numeric (0/1) status instead of factor (tests both encodings)
#   - Some missing values in predictors
#
# Design:
#   - 60 patients, ~55% event rate
#   - 200 gene expression features (GENE_001 to GENE_200)
#   - No clinical covariates (pure genomic)
#   - Signal from 2 gene modules (20 genes each)

create_plscox_genomic <- function() {
  n <- 60
  p <- 200

  # --- Build sparse block correlation ---
  sigma <- diag(p)

  # Module 1: genes 1-20 (proliferation signature)
  mod1 <- 1:20
  for (i in mod1) {
    for (j in mod1) {
      if (i != j) sigma[i, j] <- 0.55
    }
  }

  # Module 2: genes 80-100 (immune response)
  mod2 <- 80:100
  for (i in mod2) {
    for (j in mod2) {
      if (i != j) sigma[i, j] <- 0.45
    }
  }

  # Ensure positive definiteness
  eigen_vals <- eigen(sigma)$values
  if (any(eigen_vals <= 0)) {
    sigma <- sigma + diag(p) * (abs(min(eigen_vals)) + 0.01)
    diag(sigma) <- 1
  }

  # Generate gene expression (log2 scale, centered around 6-10)
  gene_data <- mvrnorm(n = n, mu = rep(0, p), Sigma = sigma)
  gene_means <- runif(p, min = 5, max = 11)
  gene_sds <- runif(p, min = 0.3, max = 1.8)
  for (j in 1:p) {
    gene_data[, j] <- gene_data[, j] * gene_sds[j] + gene_means[j]
  }

  # Make ~10 genes near-zero-variance (sparse expression)
  sparse_genes <- sample(setdiff(1:p, c(mod1, mod2)), 10)
  for (sg in sparse_genes) {
    gene_data[, sg] <- gene_means[sg] + rnorm(n, sd = 0.01)
  }

  gene_data <- round(gene_data, 4)
  colnames(gene_data) <- paste0("GENE_", sprintf("%03d", 1:p))

  # True signal from modules
  latent_prolif <- rowMeans(gene_data[, mod1[1:15]])
  latent_immune <- rowMeans(gene_data[, mod2[1:12]])

  linear_pred <- 0.10 * scale(latent_prolif) - 0.07 * scale(latent_immune)

  # Generate survival
  survival_times <- rweibull(n, shape = 1.2,
                             scale = exp(-as.numeric(linear_pred) * 0.5) * 30)

  admin_censor <- 48
  loss_followup <- rexp(n, rate = 0.025)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1
  event <- as.integer(survival_times <= censoring_times)

  # Add ~3% missing values in gene expression
  n_missing <- round(n * p * 0.03)
  miss_rows <- sample(n, n_missing, replace = TRUE)
  miss_cols <- sample(p, n_missing, replace = TRUE)
  for (k in seq_len(n_missing)) {
    gene_data[miss_rows[k], miss_cols[k]] <- NA
  }

  plscox_genomic <- data.frame(
    patient_id = paste0("GEN_", sprintf("%03d", 1:n)),
    os_time = observed_time,
    os_event = event,  # numeric 0/1 encoding
    gene_data
  )

  return(plscox_genomic)
}


# ==============================================================================
# Generate and save datasets
# ==============================================================================

print("Generating test datasets for plscox function...")

plscox_metabolomics <- create_plscox_metabolomics()
plscox_small <- create_plscox_small()
plscox_genomic <- create_plscox_genomic()

# Display summary information
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("PLS-COX TEST DATA SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nDataset 1: Metabolomics Survival Study (plscox_metabolomics)\n")
cat("Dimensions:", nrow(plscox_metabolomics), "x", ncol(plscox_metabolomics), "\n")
cat("Events:", sum(plscox_metabolomics$death == "Dead"), "/",
    nrow(plscox_metabolomics),
    "(", round(100 * sum(plscox_metabolomics$death == "Dead") /
                 nrow(plscox_metabolomics), 1), "%)\n")
cat("Median follow-up:", round(median(plscox_metabolomics$survival_months), 1),
    "months\n")
cat("Predictor columns:", sum(grepl("^METAB_", names(plscox_metabolomics))),
    "metabolites + 3 clinical\n")

cat("\nDataset 2: Small PLS Dataset (plscox_small)\n")
cat("Dimensions:", nrow(plscox_small), "x", ncol(plscox_small), "\n")
cat("Events:", sum(plscox_small$status == "Dead"), "/",
    nrow(plscox_small),
    "(", round(100 * sum(plscox_small$status == "Dead") /
                 nrow(plscox_small), 1), "%)\n")
cat("Median follow-up:", round(median(plscox_small$time_months), 1), "months\n")
cat("Predictor columns:", sum(grepl("^MARKER_", names(plscox_small))),
    "markers\n")


# --- Dataset 3 summary ---
cat("\nDataset 3: Genomic p>>n Dataset (plscox_genomic)\n")
cat("Dimensions:", nrow(plscox_genomic), "x", ncol(plscox_genomic), "\n")
cat("Events:", sum(plscox_genomic$os_event == 1), "/",
    nrow(plscox_genomic),
    "(", round(100 * sum(plscox_genomic$os_event == 1) /
                 nrow(plscox_genomic), 1), "%)\n")
cat("Median follow-up:", round(median(plscox_genomic$os_time), 1), "months\n")
cat("Predictor columns:", sum(grepl("^GENE_", names(plscox_genomic))),
    "genes (p >> n)\n")
cat("Missing values:", sum(is.na(plscox_genomic)), "\n")

# --- Save datasets ---

# Save using helper functions
save_data_multi_format(plscox_metabolomics, "plscox_metabolomics")
save_data_multi_format(plscox_small, "plscox_small")
save_data_multi_format(plscox_genomic, "plscox_genomic")

# Also save CSV to data-raw for easy inspection
write.csv(plscox_metabolomics,
          "data-raw/plscox_metabolomics.csv", row.names = FALSE)
write.csv(plscox_small,
          "data-raw/plscox_small.csv", row.names = FALSE)
write.csv(plscox_genomic,
          "data-raw/plscox_genomic.csv", row.names = FALSE)


# ==============================================================================
# Clinical interpretation guide
# ==============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("CLINICAL INTERPRETATION GUIDE FOR PLSCOX TEST DATA\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n1. METABOLOMICS SURVIVAL STUDY (n=120, p=80)\n")
cat("   Purpose: Metabolomic profiling for cancer prognosis\n")
cat("   Design: 80 metabolites in 3 correlated blocks + 3 clinical variables\n")
cat("   True signal: 3 latent components from amino acid, lipid,\n")
cat("                and energy metabolism pathways\n")
cat("   Challenge: Moderate dimensionality with block correlation structure\n")
cat("   Expected PLS behavior:\n")
cat("     - 2-3 optimal components should capture most prognostic signal\n")
cat("     - Variable loadings should cluster by metabolic pathway\n")
cat("     - Block 1 (METAB_001-015): amino acid pathway, risk-increasing\n")
cat("     - Block 2 (METAB_025-040): lipid metabolism, protective\n")
cat("     - Block 3 (METAB_055-070): energy metabolism, risk-increasing\n")
cat("   Good for testing: All PLS algorithms, component selection methods,\n")
cat("                     all scaling methods, feature importance, risk groups\n")

cat("\n2. SMALL PLS DATASET (n=50, p=25)\n")
cat("   Purpose: Edge case testing with limited sample\n")
cat("   Design: 25 biomarkers in 2 correlated blocks\n")
cat("   True signal: 2 latent components from marker blocks\n")
cat("   Challenge: Small n, higher event rate, limited power\n")
cat("   Expected PLS behavior:\n")
cat("     - LOO CV feasible and recommended\n")
cat("     - 1-2 components should suffice\n")
cat("     - Results more variable across bootstrap replicates\n")
cat("   Good for testing: LOO CV, small-sample performance,\n")
cat("                     bootstrap stability, algorithm convergence\n")

cat("\n3. GENOMIC p>>n DATASET (n=60, p=200)\n")
cat("   Purpose: True high-dimensional gene expression scenario\n")
cat("   Design: 200 genes, 2 co-expressed modules + noise genes\n")
cat("   True signal: Proliferation (GENE_001-020) and immune (GENE_080-100)\n")
cat("   Challenge: p >> n, near-zero-variance genes, missing values\n")
cat("   Uses numeric 0/1 status (tests non-factor encoding path)\n")
cat("   Expected PLS behavior:\n")
cat("     - 2 optimal components should suffice\n")
cat("     - Sparse PLS should identify signal genes\n")
cat("     - Near-zero-variance genes should be filtered/downweighted\n")
cat("   Good for testing: p>>n, sparse PLS, missing data handling,\n")
cat("                     numeric status encoding, component selection\n")

cat("\nThese datasets provide comprehensive scenarios for testing plscox:\n")
cat("  - Block-correlated predictors (natural for omics data)\n")
cat("  - Known latent component structure (validates PLS recovery)\n")
cat("  - Realistic event rates and follow-up patterns\n")
cat("  - Multiple dimensionality levels (p=25, p=80, p=200)\n")
cat("  - Mixed clinical + high-dimensional predictors\n")
cat("  - True p>>n scenario (genomic dataset)\n")
cat("  - Missing data and near-zero-variance features\n")
cat("  - Both factor and numeric status encodings\n")
cat("  - Suitable for all cross-validation strategies\n")

cat("\nTest datasets created successfully!\n")
cat("Saved as .rda/.omv in data/ and .csv in data-raw/\n")
