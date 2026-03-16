# ===============================================================
# Test Data Generation: pcacox (PCA Cox Regression)
# ===============================================================
#
# Creates 2 clinically realistic datasets:
#   1. Genomic expression panel (n=150, 30 gene features)
#   2. Small clinical cohort (n=60, 10 clinical variables)
#
# Generated: 2026-03-15
# Seed: 42
# ===============================================================

set.seed(42)

# -------------------------------------------------------------------
# Dataset 1: Genomic Expression Panel
# 30 gene features with correlated blocks simulating pathways
# -------------------------------------------------------------------
create_pcacox_genomic <- function() {
  n <- 150
  set.seed(42)

  gen_block <- function(n, k, base_means, base_sds, cor_within = 0.4) {
    sigma <- matrix(cor_within, k, k)
    diag(sigma) <- 1
    L <- chol(sigma)
    z <- matrix(rnorm(n * k), n, k)
    x <- z %*% L
    x <- sweep(x, 2, base_means, "+")
    x <- sweep(x, 2, base_sds, "*")
    round(x, 2)
  }

  # 6 blocks of 5 genes
  b1 <- gen_block(n, 5, c(8,7,6,5,9), c(2,1.5,1.8,1,2.5))
  colnames(b1) <- paste0("gene_", 1:5)
  b2 <- gen_block(n, 5, c(6,7,5,4,8), c(1.5,2,1.5,1,1.8))
  colnames(b2) <- paste0("gene_", 6:10)
  b3 <- gen_block(n, 5, c(7,5,6,4,3), c(2,1.5,1.5,1.2,1))
  colnames(b3) <- paste0("gene_", 11:15)
  b4 <- gen_block(n, 5, c(5,4,6,7,5), c(1.5,1,1.5,2,1.5))
  colnames(b4) <- paste0("gene_", 16:20)
  b5 <- gen_block(n, 5, c(6,5,7,4,3), c(1.8,1.5,2,1,1))
  colnames(b5) <- paste0("gene_", 21:25)
  b6 <- gen_block(n, 5, c(7,6,5,8,4), c(2,1.5,1.5,2.5,1))
  colnames(b6) <- paste0("gene_", 26:30)

  # True signal from blocks 1, 3, 6
  lp <- 0.12 * b1[,1] - 0.08 * b1[,5] +
        0.15 * b3[,1] - 0.10 * b3[,3] +
        0.09 * b6[,1]

  base_time <- rexp(n, rate = 0.015)
  surv_time <- base_time * exp(-lp + mean(lp))
  surv_time <- round(pmax(0.5, pmin(surv_time, 96)), 1)
  censor_time <- runif(n, 6, 72)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Dead", "Alive")),
    b1, b2, b3, b4, b5, b6,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------
# Dataset 2: Small Clinical Cohort
# -------------------------------------------------------------------
create_pcacox_clinical <- function() {
  n <- 60
  set.seed(99)

  age <- round(rnorm(n, 62, 10))
  age <- pmax(35, pmin(age, 85))
  bmi <- round(rnorm(n, 26, 4), 1)
  bmi <- pmax(16, pmin(bmi, 45))
  albumin <- round(rnorm(n, 3.7, 0.5), 1)
  albumin <- pmax(2.0, pmin(albumin, 5.0))
  crp <- round(rgamma(n, 2, 0.5), 1)
  crp <- pmax(0.1, pmin(crp, 25))
  ldh <- round(rnorm(n, 200, 60))
  ldh <- pmax(80, pmin(ldh, 500))
  hemoglobin <- round(rnorm(n, 12.5, 2), 1)
  wbc <- round(rnorm(n, 7.5, 3), 1)
  wbc <- pmax(1, wbc)
  platelets <- round(rnorm(n, 250, 80))
  platelets <- pmax(50, platelets)
  tumor_size <- round(rgamma(n, 2.5, 1), 1)
  tumor_size <- pmax(0.5, pmin(tumor_size, 12))
  ki67 <- round(pmax(1, pmin(95, rnorm(n, 25, 15))))

  lp <- 0.02 * age + 0.1 * tumor_size - 0.3 * albumin + 0.005 * ldh + 0.01 * ki67
  surv_time <- rexp(n, rate = 0.025 * exp(lp - mean(lp)))
  surv_time <- round(pmax(0.5, pmin(surv_time, 60)), 1)
  censor_time <- runif(n, 6, 48)
  event_time <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)

  data.frame(
    time = event_time,
    status = factor(ifelse(event == 1, "Dead", "Alive")),
    age, bmi, albumin, crp, ldh, hemoglobin, wbc, platelets, tumor_size, ki67,
    stringsAsFactors = FALSE
  )
}

# ===================================================================
# Generate and save
# ===================================================================
cat("Generating pcacox test datasets...\n")

pcacox_genomic <- create_pcacox_genomic()
cat("  genomic: n=", nrow(pcacox_genomic), ", p=", ncol(pcacox_genomic) - 2,
    ", events=", sum(pcacox_genomic$status == "Dead"), "\n")

pcacox_clinical <- create_pcacox_clinical()
cat("  clinical: n=", nrow(pcacox_clinical), ", p=", ncol(pcacox_clinical) - 2,
    ", events=", sum(pcacox_clinical$status == "Dead"), "\n")

# Save .rda to data/
save(pcacox_genomic,  file = "data/pcacox_genomic.rda")
save(pcacox_clinical, file = "data/pcacox_clinical.rda")

# Save .csv and .omv to data-raw/non-rda/
dir.create("data-raw/non-rda", showWarnings = FALSE, recursive = TRUE)

write.csv(pcacox_genomic,  "data-raw/non-rda/pcacox_genomic.csv",  row.names = FALSE)
write.csv(pcacox_clinical, "data-raw/non-rda/pcacox_clinical.csv", row.names = FALSE)

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(pcacox_genomic,  "data-raw/non-rda/pcacox_genomic.omv")
  jmvReadWrite::write_omv(pcacox_clinical, "data-raw/non-rda/pcacox_clinical.omv")
}

cat("\nDone. RDA saved to data/, CSV/OMV saved to data-raw/non-rda/\n")
