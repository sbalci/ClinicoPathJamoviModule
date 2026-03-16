# ===============================================================
# Test Data Generation: sparsegrouplasso (Sparse Group LASSO Cox)
# ===============================================================
#
# Creates 3 clinically realistic datasets:
#   1. Lung cancer with mixed clinical/genomic groups (n=180)
#   2. High-dimensional gene panel (n=100, 40 genes in 8 pathways)
#   3. Small cohort edge case (n=50, 6 predictors)
#
# Generated: 2026-03-14
# Seed: 42
# Formats: .rda (data/), .omv + .csv (data-raw/non-rda/)
# ===============================================================

library(dplyr)
library(survival)

set.seed(42)

# -------------------------------------------------------------------
# Dataset 1: Lung Cancer Mixed Clinical/Genomic
# Groups: Clinical (4), Pathology (3), Inflammation (4), Treatment (3)
# -------------------------------------------------------------------
create_sgl_lung <- function() {
  n <- 180

  # Clinical
  age <- round(rnorm(n, 65, 10))
  age <- pmax(40, pmin(age, 85))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))
  smoking_py <- round(pmax(0, rnorm(n, 30, 15)))
  ecog <- factor(sample(0:3, n, replace = TRUE, prob = c(0.20, 0.40, 0.30, 0.10)))

  # Pathology
  histology <- factor(sample(c("Adenocarcinoma", "Squamous", "Large_Cell"), n,
                             replace = TRUE, prob = c(0.50, 0.35, 0.15)))
  tumor_size <- round(rgamma(n, 3, 0.8), 1)
  tumor_size <- pmax(0.5, pmin(tumor_size, 15))
  pdl1 <- round(pmax(0, pmin(100, rnorm(n, 40, 25))))

  # Inflammation
  crp <- round(rgamma(n, 2, 0.4), 1)
  crp <- pmax(0.1, pmin(crp, 30))
  nlr <- round(rgamma(n, 3, 1), 1)
  nlr <- pmax(0.5, pmin(nlr, 20))
  albumin <- round(rnorm(n, 3.6, 0.6), 1)
  albumin <- pmax(2.0, pmin(albumin, 5.0))
  ldh <- round(rnorm(n, 220, 70))
  ldh <- pmax(80, pmin(ldh, 500))

  # Treatment
  chemo <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.30, 0.70)))
  immuno <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.40, 0.60)))
  radiation <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.50, 0.50)))

  # Generate survival
  lp <- 0.02 * (age - 65) +
        0.3 * (ecog == "2") + 0.7 * (ecog == "3") +
        0.12 * (tumor_size - 3) +
        -0.008 * pdl1 +
        0.05 * crp +
        0.08 * nlr -
        0.4 * (albumin - 3.6) +
        0.002 * (ldh - 220) -
        0.2 * (immuno == "Yes")

  base_time <- rexp(n, rate = 0.02)
  surv_time <- base_time * exp(-lp)
  surv_time <- round(pmax(0.5, pmin(surv_time, 60)), 1)
  censor_time <- runif(n, min = 6, max = 48)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Dead", "Alive")),
    age, sex, smoking_py, ecog,
    histology, tumor_size, pdl1,
    crp, nlr, albumin, ldh,
    chemo, immuno, radiation,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------
# Dataset 2: High-Dimensional Gene Panel (8 pathways, 5 genes each)
# -------------------------------------------------------------------
create_sgl_genepanel <- function() {
  n <- 100
  set.seed(123)

  gen_pathway <- function(n, n_genes, base_means, base_sds, cor_within = 0.35) {
    sigma <- matrix(cor_within, n_genes, n_genes)
    diag(sigma) <- 1
    L <- chol(sigma)
    z <- matrix(rnorm(n * n_genes), n, n_genes)
    x <- z %*% L
    x <- sweep(x, 2, base_means, "+")
    x <- sweep(x, 2, base_sds, "*")
    round(x, 2)
  }

  # 8 pathways x 5 genes = 40 predictors
  p1 <- gen_pathway(n, 5, c(8,7,6,5,9), c(2,1.5,1.8,1,2.5))
  colnames(p1) <- paste0("EGFR_p_", c("EGFR","ERBB2","ERBB3","SHC1","GRB2"))

  p2 <- gen_pathway(n, 5, c(6,7,5,4,8), c(1.5,2,1.5,1,1.8))
  colnames(p2) <- paste0("PI3K_p_", c("PIK3CA","AKT1","PTEN","MTOR","TSC2"))

  p3 <- gen_pathway(n, 5, c(7,5,6,4,3), c(2,1.5,1.5,1.2,1))
  colnames(p3) <- paste0("P53_p_", c("TP53","MDM2","CDKN1A","BAX","BBC3"))

  p4 <- gen_pathway(n, 5, c(5,4,6,7,5), c(1.5,1,1.5,2,1.5))
  colnames(p4) <- paste0("RAS_p_", c("KRAS","NRAS","BRAF","MEK1","ERK2"))

  p5 <- gen_pathway(n, 5, c(6,5,7,4,3), c(1.8,1.5,2,1,1))
  colnames(p5) <- paste0("WNT_p_", c("CTNNB1","APC","GSK3B","AXIN1","TCF4"))

  p6 <- gen_pathway(n, 5, c(7,6,5,8,4), c(2,1.5,1.5,2.5,1))
  colnames(p6) <- paste0("VEGF_p_", c("VEGFA","KDR","FLT1","HIF1A","ANGPT2"))

  p7 <- gen_pathway(n, 5, c(5,6,4,7,5), c(1.5,1.8,1,2,1.5))
  colnames(p7) <- paste0("JAK_p_", c("JAK2","STAT3","SOCS1","IL6","IL6R"))

  p8 <- gen_pathway(n, 5, c(4,5,6,3,7), c(1,1.5,1.8,1,2))
  colnames(p8) <- paste0("NOTCH_p_", c("NOTCH1","DLL1","HES1","MAML1","RBPJ"))

  # True signal in EGFR (p1), P53 (p3), VEGF (p6) pathways
  lp <- 0.15 * p1[,1] - 0.10 * p1[,4] +
        0.20 * p3[,1] - 0.08 * p3[,4] +
        0.12 * p6[,1] - 0.06 * p6[,5]

  base_time <- rexp(n, rate = 0.015)
  surv_time <- base_time * exp(-lp + mean(lp))
  surv_time <- round(pmax(0.5, pmin(surv_time, 72)), 1)
  censor_time <- runif(n, 6, 60)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  df <- data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Progressed", "Stable")),
    p1, p2, p3, p4, p5, p6, p7, p8,
    stringsAsFactors = FALSE
  )
  df
}

# -------------------------------------------------------------------
# Dataset 3: Small Clinical Cohort
# -------------------------------------------------------------------
create_sgl_small <- function() {
  n <- 50
  set.seed(99)

  age <- round(rnorm(n, 60, 12))
  age <- pmax(35, pmin(age, 80))
  tumor_size <- round(rgamma(n, 2.5, 1), 1)
  tumor_size <- pmax(0.5, pmin(tumor_size, 10))
  grade <- factor(sample(c("Low", "High"), n, replace = TRUE, prob = c(0.45, 0.55)))
  stage <- factor(sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.25, 0.45, 0.30)))
  marker1 <- round(rnorm(n, 50, 20))
  marker1 <- pmax(0, pmin(marker1, 100))
  marker2 <- round(rnorm(n, 30, 15))
  marker2 <- pmax(0, pmin(marker2, 100))

  lp <- 0.02 * age + 0.15 * tumor_size + 0.5 * (grade == "High") +
        0.3 * (stage == "III") - 0.01 * marker1

  surv_time <- rexp(n, rate = 0.03 * exp(lp - mean(lp)))
  surv_time <- round(pmax(0.5, pmin(surv_time, 48)), 1)
  censor_time <- runif(n, 6, 36)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Dead", "Alive")),
    age, tumor_size, grade, stage, marker1, marker2,
    stringsAsFactors = FALSE
  )
}

# ===================================================================
# Generate and save
# ===================================================================
cat("Generating sparsegrouplasso test datasets...\n")

sparsegrouplasso_lung <- create_sgl_lung()
cat("  lung: n=", nrow(sparsegrouplasso_lung), ", p=", ncol(sparsegrouplasso_lung) - 2,
    ", events=", sum(sparsegrouplasso_lung$status == "Dead"), "\n")

sparsegrouplasso_genepanel <- create_sgl_genepanel()
cat("  genepanel: n=", nrow(sparsegrouplasso_genepanel), ", p=", ncol(sparsegrouplasso_genepanel) - 2,
    ", events=", sum(sparsegrouplasso_genepanel$status == "Progressed"), "\n")

sparsegrouplasso_small <- create_sgl_small()
cat("  small: n=", nrow(sparsegrouplasso_small), ", p=", ncol(sparsegrouplasso_small) - 2,
    ", events=", sum(sparsegrouplasso_small$status == "Dead"), "\n")

# Save .rda to data/
save(sparsegrouplasso_lung,      file = "data/sparsegrouplasso_lung.rda")
save(sparsegrouplasso_genepanel, file = "data/sparsegrouplasso_genepanel.rda")
save(sparsegrouplasso_small,     file = "data/sparsegrouplasso_small.rda")

# Save .csv and .omv to data-raw/non-rda/
dir.create("data-raw/non-rda", showWarnings = FALSE, recursive = TRUE)

write.csv(sparsegrouplasso_lung,      "data-raw/non-rda/sparsegrouplasso_lung.csv",      row.names = FALSE)
write.csv(sparsegrouplasso_genepanel, "data-raw/non-rda/sparsegrouplasso_genepanel.csv", row.names = FALSE)
write.csv(sparsegrouplasso_small,     "data-raw/non-rda/sparsegrouplasso_small.csv",     row.names = FALSE)

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(sparsegrouplasso_lung,      "data-raw/non-rda/sparsegrouplasso_lung.omv")
  jmvReadWrite::write_omv(sparsegrouplasso_genepanel, "data-raw/non-rda/sparsegrouplasso_genepanel.omv")
  jmvReadWrite::write_omv(sparsegrouplasso_small,     "data-raw/non-rda/sparsegrouplasso_small.omv")
}

cat("\nDone. RDA saved to data/, CSV/OMV saved to data-raw/non-rda/\n")
