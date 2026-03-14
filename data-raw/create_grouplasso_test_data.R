# ===============================================================
# Test Data Generation: grouplasso (Group LASSO Cox Regression)
# ===============================================================
#
# This script generates realistic test data for the grouplasso jamovi function.
# It creates 3 clinically realistic datasets covering diverse scenarios:
#
#   1. Breast cancer biomarker panel study (n=200, 15 predictors in 5 groups)
#   2. Genomic pathway analysis (n=120, 30 gene features in 6 pathways)
#   3. Small clinical cohort (n=60, 8 predictors in 3 groups)
#
# Generated: 2026-03-14
# Seed: 42
# Formats: .rda (data/), .omv + .csv (data-raw/non-rda/)
# ===============================================================

source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)

set.seed(42)

# -------------------------------------------------------------------
# Dataset 1: Breast Cancer Biomarker Panel Study
# Purpose: Grouped predictors from clinical domains
#   Group 1 (Demographics): age, bmi
#   Group 2 (Tumor features): tumor_size, grade, lvi
#   Group 3 (Biomarkers): er, pr, her2, ki67
#   Group 4 (Lab values): albumin, ldh, crp
#   Group 5 (Treatment): chemo, radiation, hormonal
# -------------------------------------------------------------------
create_grouplasso_biomarker <- function() {
  n <- 200

  # Group 1: Demographics
  age <- round(rnorm(n, mean = 58, sd = 11))
  age <- pmax(30, pmin(age, 85))
  bmi <- round(rnorm(n, mean = 26, sd = 5), 1)
  bmi <- pmax(16, pmin(bmi, 45))

  # Group 2: Tumor features
  tumor_size <- round(rgamma(n, shape = 2.5, rate = 1), 1)
  tumor_size <- pmax(0.5, pmin(tumor_size, 12))
  grade <- factor(sample(1:3, n, replace = TRUE, prob = c(0.25, 0.45, 0.30)),
                  labels = c("Grade1", "Grade2", "Grade3"))
  lvi <- factor(sample(c("Absent", "Present"), n, replace = TRUE, prob = c(0.55, 0.45)))

  # Group 3: Biomarkers
  er <- round(rnorm(n, mean = 60, sd = 30))
  er <- pmax(0, pmin(er, 100))
  pr <- round(pmax(0, er * 0.7 + rnorm(n, 0, 15)))
  pr <- pmin(pr, 100)
  her2 <- factor(sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.80, 0.20)))
  ki67 <- round(ifelse(grade == "Grade1", rnorm(n, 8, 4),
                ifelse(grade == "Grade2", rnorm(n, 20, 8),
                       rnorm(n, 40, 12))), 1)
  ki67 <- pmax(1, pmin(ki67, 95))

  # Group 4: Lab values
  albumin <- round(rnorm(n, mean = 3.8, sd = 0.5), 1)
  albumin <- pmax(2.0, pmin(albumin, 5.5))
  ldh <- round(rnorm(n, mean = 200, sd = 60))
  ldh <- pmax(80, pmin(ldh, 500))
  crp <- round(rgamma(n, shape = 1.5, rate = 0.3), 1)
  crp <- pmax(0.1, pmin(crp, 30))

  # Group 5: Treatment
  chemo <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.35, 0.65)))
  radiation <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.40, 0.60)))
  hormonal <- factor(ifelse(er > 10,
    sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.20, 0.80)),
    sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.90, 0.10))))

  # Generate survival times (influenced by tumor features and biomarkers)
  lp <- 0.03 * (age - 58) +
        0.15 * (tumor_size - 2.5) +
        0.4 * (grade == "Grade3") +
        0.25 * (lvi == "Present") -
        0.01 * er +
        0.35 * (her2 == "Positive") +
        0.008 * ki67 -
        0.3 * (albumin - 3.8) +
        0.002 * (ldh - 200)

  base_time <- rexp(n, rate = 0.015)
  surv_time <- base_time * exp(-lp)
  surv_time <- round(pmax(0.5, pmin(surv_time, 120)), 1)

  # Censoring
  censor_time <- runif(n, min = 12, max = 96)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Dead", "Alive")),
    age = age, bmi = bmi,
    tumor_size = tumor_size, grade = grade, lvi = lvi,
    er = er, pr = pr, her2 = her2, ki67 = ki67,
    albumin = albumin, ldh = ldh, crp = crp,
    chemo = chemo, radiation = radiation, hormonal = hormonal,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------
# Dataset 2: Genomic Pathway Analysis
# Purpose: High-dimensional with natural pathway grouping
#   Pathway 1 (Cell cycle): CCND1, CCNE1, CDK4, CDK6, RB1
#   Pathway 2 (PI3K/AKT): PIK3CA, AKT1, PTEN, MTOR, TSC1
#   Pathway 3 (p53): TP53, MDM2, ATM, CHEK2, CDKN2A
#   Pathway 4 (RAS/MAPK): KRAS, BRAF, MAP2K1, ERK1, ERK2
#   Pathway 5 (Apoptosis): BCL2, BAX, BIRC5, CASP3, CASP8
#   Pathway 6 (Angiogenesis): VEGFA, FLT1, KDR, ANGPT1, ANGPT2
# -------------------------------------------------------------------
create_grouplasso_genomic <- function() {
  n <- 120
  set.seed(123)

  # Helper: generate correlated gene expression within a pathway
  gen_pathway <- function(n, n_genes, mean_expr, sd_expr, cor_within = 0.4) {
    # Generate with within-pathway correlation
    sigma <- matrix(cor_within, n_genes, n_genes)
    diag(sigma) <- 1
    L <- chol(sigma)
    z <- matrix(rnorm(n * n_genes), n, n_genes)
    x <- z %*% L
    x <- sweep(x, 2, mean_expr, "+")
    x <- sweep(x, 2, sd_expr, "*")
    round(x, 2)
  }

  # Generate pathway expression data
  p1 <- gen_pathway(n, 5, c(8, 6, 7, 5, 9), c(2, 1.5, 1.8, 1.2, 2.5))
  colnames(p1) <- c("CCND1", "CCNE1", "CDK4", "CDK6", "RB1")

  p2 <- gen_pathway(n, 5, c(7, 6, 8, 5, 4), c(1.5, 1.8, 2, 1.5, 1))
  colnames(p2) <- c("PIK3CA", "AKT1", "PTEN", "MTOR", "TSC1")

  p3 <- gen_pathway(n, 5, c(6, 5, 7, 4, 3), c(2, 1.5, 1.8, 1, 1.2))
  colnames(p3) <- c("TP53", "MDM2", "ATM", "CHEK2", "CDKN2A")

  p4 <- gen_pathway(n, 5, c(5, 4, 6, 7, 6), c(1.5, 1, 1.5, 2, 2))
  colnames(p4) <- c("KRAS", "BRAF", "MAP2K1", "ERK1", "ERK2")

  p5 <- gen_pathway(n, 5, c(7, 5, 6, 4, 3), c(2, 1.5, 1.8, 1, 1))
  colnames(p5) <- c("BCL2", "BAX", "BIRC5", "CASP3", "CASP8")

  p6 <- gen_pathway(n, 5, c(8, 4, 5, 6, 5), c(2.5, 1, 1.5, 1.5, 1.2))
  colnames(p6) <- c("VEGFA", "FLT1", "KDR", "ANGPT1", "ANGPT2")

  # Survival influenced by pathways 1, 3, and 6 (true signal)
  lp <- 0.15 * p1[, 1] - 0.10 * p1[, 5] +  # Cell cycle
        0.20 * p3[, 1] - 0.12 * p3[, 3] +   # p53
        0.08 * p6[, 1]                        # Angiogenesis

  base_time <- rexp(n, rate = 0.02)
  surv_time <- base_time * exp(-lp + mean(lp))
  surv_time <- round(pmax(0.5, pmin(surv_time, 96)), 1)

  censor_time <- runif(n, min = 6, max = 72)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  df <- data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Progressed", "Stable")),
    p1, p2, p3, p4, p5, p6,
    stringsAsFactors = FALSE
  )

  df
}

# -------------------------------------------------------------------
# Dataset 3: Small Clinical Cohort
# Purpose: Edge case testing with small n, few groups
#   Group 1 (Clinical): age, ecog
#   Group 2 (Pathology): tumor_size, grade
#   Group 3 (Lab): hemoglobin, wbc, platelets, ldh
# -------------------------------------------------------------------
create_grouplasso_small <- function() {
  n <- 60
  set.seed(99)

  age <- round(rnorm(n, mean = 62, sd = 10))
  age <- pmax(35, pmin(age, 80))

  ecog <- factor(sample(0:3, n, replace = TRUE, prob = c(0.25, 0.40, 0.25, 0.10)))

  tumor_size <- round(rnorm(n, mean = 4, sd = 2), 1)
  tumor_size <- pmax(0.5, pmin(tumor_size, 15))

  grade <- factor(sample(c("Low", "Intermediate", "High"), n, replace = TRUE,
                         prob = c(0.20, 0.50, 0.30)))

  hemoglobin <- round(rnorm(n, mean = 12.5, sd = 2), 1)
  wbc <- round(rnorm(n, mean = 7.5, sd = 3), 1)
  wbc <- pmax(1, wbc)
  platelets <- round(rnorm(n, mean = 250, sd = 80))
  platelets <- pmax(50, platelets)
  ldh <- round(rnorm(n, mean = 220, sd = 70))
  ldh <- pmax(80, ldh)

  # Simple survival
  lp <- 0.02 * age + 0.3 * (ecog == "2") + 0.6 * (ecog == "3") +
        0.1 * tumor_size + 0.4 * (grade == "High") - 0.1 * hemoglobin

  surv_time <- rexp(n, rate = 0.03 * exp(lp - mean(lp)))
  surv_time <- round(pmax(0.5, pmin(surv_time, 60)), 1)
  censor_time <- runif(n, min = 6, max = 48)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Dead", "Alive")),
    age = age, ecog = ecog,
    tumor_size = tumor_size, grade = grade,
    hemoglobin = hemoglobin, wbc = wbc, platelets = platelets, ldh = ldh,
    stringsAsFactors = FALSE
  )
}

# ===================================================================
# Generate and save all datasets
# ===================================================================

cat("Generating grouplasso test datasets...\n")

grouplasso_biomarker <- create_grouplasso_biomarker()
cat("  biomarker: n=", nrow(grouplasso_biomarker), ", p=", ncol(grouplasso_biomarker) - 2,
    ", events=", sum(grouplasso_biomarker$status == "Dead"), "\n")
save_data_multi_format(grouplasso_biomarker, "grouplasso_biomarker")

grouplasso_genomic <- create_grouplasso_genomic()
cat("  genomic: n=", nrow(grouplasso_genomic), ", p=", ncol(grouplasso_genomic) - 2,
    ", events=", sum(grouplasso_genomic$status == "Progressed"), "\n")
save_data_multi_format(grouplasso_genomic, "grouplasso_genomic")

grouplasso_small <- create_grouplasso_small()
cat("  small: n=", nrow(grouplasso_small), ", p=", ncol(grouplasso_small) - 2,
    ", events=", sum(grouplasso_small$status == "Dead"), "\n")
save_data_multi_format(grouplasso_small, "grouplasso_small")

cat("\nDone. Datasets saved to data/ and data-raw/non-rda/\n")
