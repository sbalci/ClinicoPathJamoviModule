# Test data generation for highdimcox function
# Creates realistic high-dimensional survival datasets for testing
# High-Dimensional Cox Regression with LASSO/Ridge/Elastic Net/Adaptive LASSO
#
# Datasets:
#   1. highdimcox_genomic   (n=150, p=100 genes + 5 clinical) - genomic survival study
#   2. highdimcox_proteomic (n=80, p=50 proteins + 3 clinical) - proteomic study
#
# Usage:
#   source("data-raw/create_highdimcox_test_data.R")
#   # Creates data/highdimcox_genomic.rda and data/highdimcox_proteomic.rda

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(survival)

# Set seed for reproducibility
set.seed(2024)

# =============================================================================
# Dataset 1: highdimcox_genomic (n=150, p=100 genes + 5 clinical vars)
# Simulates a genomic survival study with realistic gene expression profiles
# =============================================================================

create_highdimcox_genomic <- function() {
  n <- 150
  p_genes <- 100

  # ---- Clinical variables ----
  age <- round(rnorm(n, mean = 61, sd = 11))
  age[age < 30] <- 30
  age[age > 85] <- 85

  gender <- factor(
    sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))
  )

  stage <- factor(
    sample(1:4, n, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
    labels = c("Stage I", "Stage II", "Stage III", "Stage IV")
  )

  grade <- factor(
    sample(1:3, n, replace = TRUE, prob = c(0.20, 0.50, 0.30)),
    labels = c("Grade 1", "Grade 2", "Grade 3")
  )

  treatment <- factor(
    sample(c("Surgery", "Chemo", "Combined"), n, replace = TRUE,
           prob = c(0.35, 0.30, 0.35))
  )

  # ---- Gene expression variables (standardised) ----
  # Create a block-correlated structure: genes in clusters share partial signal
  gene_data <- matrix(rnorm(n * p_genes), nrow = n, ncol = p_genes)
  colnames(gene_data) <- paste0("GENE_", sprintf("%03d", seq_len(p_genes)))

  # Introduce correlation blocks (realistic for pathway-based expression)
  # Genes 1-10 share a latent factor, 11-20 share another, etc.
  for (block_start in seq(1, 50, by = 10)) {
    block_end <- block_start + 9
    latent <- rnorm(n)
    for (j in block_start:block_end) {
      gene_data[, j] <- 0.6 * latent + 0.8 * gene_data[, j]
    }
  }

  # ---- True effects: 8 genes out of 100 have non-zero coefficients ----
  true_beta <- rep(0, p_genes)
  prognostic_indices <- c(3, 8, 15, 22, 47, 63, 78, 91)
  true_beta[prognostic_indices] <- c(0.7, -0.55, 0.45, -0.4, 0.65, -0.5, 0.35, -0.7)

  # Clinical effects on hazard
  clinical_lp <- (
    0.02 * (age - 61) +
    0.25 * (as.numeric(stage) - 1) +
    0.20 * (as.numeric(grade) - 1) +
    -0.15 * (treatment == "Combined")
  )

  # Linear predictor = gene effects + clinical effects
  lp <- as.numeric(gene_data %*% true_beta) + clinical_lp

  # ---- Generate survival times (Weibull baseline) ----
  # Scale so that median survival ~ 24 months for average-risk patient
  baseline_rate <- 0.03
  survival_times <- rweibull(n, shape = 1.1, scale = exp(-lp * 0.4) / baseline_rate)

  # Administrative censoring at 60 months + random loss to follow-up
  admin_censor <- 60
  loss_to_followup <- rexp(n, rate = 0.012)
  censoring_times <- pmin(admin_censor, loss_to_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1 # floor at 0.1 months
  event <- as.integer(survival_times <= censoring_times)

  # Target ~50% event rate; adjust censoring if needed
  # (With current parameters this lands near 45-55%)

  # Build the outcome as a factor (Alive / Dead)
  outcome <- factor(event, levels = c(0, 1), labels = c("Alive", "Dead"))

  # ---- Assemble data frame ----
  highdimcox_genomic <- data.frame(
    patient_id       = paste0("GEN_", sprintf("%03d", seq_len(n))),
    survival_months  = observed_time,
    vital_status     = outcome,
    age              = age,
    gender           = gender,
    stage            = stage,
    grade            = grade,
    treatment        = treatment,
    gene_data,
    stringsAsFactors = FALSE
  )

  # Add a handful of realistic missing values (< 3%)
  set.seed(9999)
  na_rows <- sample(n, 4)
  highdimcox_genomic$age[na_rows[1]] <- NA
  highdimcox_genomic$GENE_042[na_rows[2]] <- NA
  highdimcox_genomic$GENE_071[na_rows[3]] <- NA
  highdimcox_genomic$GENE_095[na_rows[4]] <- NA

  return(highdimcox_genomic)
}


# =============================================================================
# Dataset 2: highdimcox_proteomic (n=80, p=50 proteins + 3 clinical vars)
# Smaller proteomic study for testing different regularization methods
# Higher event rate (~65%) to ensure convergence even with small n
# =============================================================================

create_highdimcox_proteomic <- function() {
  n <- 80
  p_proteins <- 50

  # ---- Clinical variables ----
  age <- round(rnorm(n, mean = 56, sd = 13))
  age[age < 28] <- 28
  age[age > 82] <- 82

  sex <- factor(
    sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.50, 0.50))
  )

  tumor_size_cm <- round(rnorm(n, mean = 3.8, sd = 1.6), 1)
  tumor_size_cm[tumor_size_cm < 0.5] <- 0.5
  tumor_size_cm[tumor_size_cm > 12] <- 12

  # ---- Protein abundance variables (log2 intensities) ----
  protein_data <- matrix(rnorm(n * p_proteins, mean = 0, sd = 1),
                         nrow = n, ncol = p_proteins)
  colnames(protein_data) <- paste0("PROT_", sprintf("%02d", seq_len(p_proteins)))

  # Introduce mild correlation between some proteins (same pathway)
  latent_pathway1 <- rnorm(n)
  latent_pathway2 <- rnorm(n)
  for (j in 1:8) {
    protein_data[, j] <- 0.5 * latent_pathway1 + 0.87 * protein_data[, j]
  }
  for (j in 20:25) {
    protein_data[, j] <- 0.4 * latent_pathway2 + 0.92 * protein_data[, j]
  }

  # ---- True effects: 5 proteins are prognostic ----
  true_beta <- rep(0, p_proteins)
  prognostic_idx <- c(2, 11, 23, 37, 45)
  true_beta[prognostic_idx] <- c(0.9, -0.7, 0.55, -0.6, 0.8)

  clinical_lp <- (
    0.015 * (age - 56) +
    0.10 * (tumor_size_cm - 3.8)
  )

  lp <- as.numeric(protein_data %*% true_beta) + clinical_lp

  # ---- Survival times targeting ~65% event rate ----
  baseline_rate <- 0.06
  survival_times <- rweibull(n, shape = 1.0, scale = exp(-lp * 0.35) / baseline_rate)

  # Lighter censoring to achieve higher event rate
  admin_censor <- 48
  loss_to_followup <- rexp(n, rate = 0.008)
  censoring_times <- pmin(admin_censor, loss_to_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1
  event <- as.integer(survival_times <= censoring_times)

  outcome <- factor(event, levels = c(0, 1), labels = c("Alive", "Dead"))

  # ---- Assemble data frame ----
  highdimcox_proteomic <- data.frame(
    sample_id     = paste0("PROT_", sprintf("%02d", seq_len(n))),
    follow_up_months = observed_time,
    event_status  = outcome,
    age           = age,
    sex           = sex,
    tumor_size_cm = tumor_size_cm,
    protein_data,
    stringsAsFactors = FALSE
  )

  # Minimal missing data (2 values)
  set.seed(7777)
  na_rows <- sample(n, 2)
  highdimcox_proteomic$PROT_18[na_rows[1]] <- NA
  highdimcox_proteomic$PROT_39[na_rows[2]] <- NA

  return(highdimcox_proteomic)
}


# =============================================================================
# Generate and save both datasets
# =============================================================================

cat("Generating highdimcox test datasets...\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

highdimcox_genomic <- create_highdimcox_genomic()
highdimcox_proteomic <- create_highdimcox_proteomic()

# ---- Summary statistics ----
cat("\nDataset 1: highdimcox_genomic\n")
cat("  Dimensions: ", nrow(highdimcox_genomic), " x ", ncol(highdimcox_genomic), "\n")
cat("  Events (Dead): ", sum(highdimcox_genomic$vital_status == "Dead"), " / ",
    nrow(highdimcox_genomic), " (",
    round(100 * mean(highdimcox_genomic$vital_status == "Dead"), 1), "%)\n")
cat("  Median follow-up: ", round(median(highdimcox_genomic$survival_months), 1),
    " months\n")
cat("  Predictors: 100 gene expression + 5 clinical variables\n")
cat("  True prognostic genes: GENE_003, GENE_008, GENE_015, GENE_022,",
    "GENE_047, GENE_063, GENE_078, GENE_091\n")
cat("  outcomeLevel: \"Dead\"\n")

cat("\nDataset 2: highdimcox_proteomic\n")
cat("  Dimensions: ", nrow(highdimcox_proteomic), " x ",
    ncol(highdimcox_proteomic), "\n")
cat("  Events (Dead): ", sum(highdimcox_proteomic$event_status == "Dead"), " / ",
    nrow(highdimcox_proteomic), " (",
    round(100 * mean(highdimcox_proteomic$event_status == "Dead"), 1), "%)\n")
cat("  Median follow-up: ", round(median(highdimcox_proteomic$follow_up_months), 1),
    " months\n")
cat("  Predictors: 50 protein markers + 3 clinical variables\n")
cat("  True prognostic proteins: PROT_02, PROT_11, PROT_23, PROT_37, PROT_45\n")
cat("  outcomeLevel: \"Dead\"\n")

# ---- Save using multi-format helper ----
cat("\n", paste(rep("-", 60), collapse = ""), "\n")
cat("Saving datasets...\n\n")

save_multiple_datasets(
  highdimcox_genomic   = highdimcox_genomic,
  highdimcox_proteomic = highdimcox_proteomic,
  save_csv  = TRUE,
  data_dir  = "data",
  verbose   = TRUE
)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("highdimcox test datasets created successfully.\n")
cat("Files saved:\n")
cat("  data/highdimcox_genomic.rda   (+ .omv, .csv)\n")
cat("  data/highdimcox_proteomic.rda (+ .omv, .csv)\n")

# =============================================================================
# Usage guide for testing highdimcox
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("USAGE GUIDE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n1. GENOMIC DATASET (highdimcox_genomic)\n")
cat("   Use case: Classic p >> n genomic survival study\n")
cat("   Challenge: 100 gene predictors, only 8 truly prognostic\n")
cat("   Recommended test:\n")
cat('     highdimcox(\n')
cat('       data = highdimcox_genomic,\n')
cat('       elapsedtime = "survival_months",\n')
cat('       outcome = "vital_status",\n')
cat('       outcomeLevel = "Dead",\n')
cat('       predictors = paste0("GENE_", sprintf("%03d", 1:100)),\n')
cat('       regularization_method = "elastic_net",\n')
cat('       alpha_value = 0.5,\n')
cat('       cv_folds = 10\n')
cat('     )\n')

cat("\n2. PROTEOMIC DATASET (highdimcox_proteomic)\n")
cat("   Use case: Smaller proteomic study, higher event rate\n")
cat("   Challenge: Test all four regularization methods\n")
cat("   Recommended test:\n")
cat('     highdimcox(\n')
cat('       data = highdimcox_proteomic,\n')
cat('       elapsedtime = "follow_up_months",\n')
cat('       outcome = "event_status",\n')
cat('       outcomeLevel = "Dead",\n')
cat('       predictors = paste0("PROT_", sprintf("%02d", 1:50)),\n')
cat('       regularization_method = "lasso",\n')
cat('       stability_selection = TRUE,\n')
cat('       bootstrap_iterations = 200\n')
cat('     )\n')
