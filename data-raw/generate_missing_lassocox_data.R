# ===============================================================
# Generate Missing Lassocox Test Datasets
# ===============================================================
#
# This script generates the 2 datasets that were defined in
# create_lassocox_test_data.R but not yet saved as .rda files:
#
#   5. lassocox_genomic        (n=80, p=56 cols, high-dim)
#   6. lassocox_multicollinear (n=180, 15 cols, correlated predictors)
#
# Run from package root: Rscript data-raw/generate_missing_lassocox_data.R
# ===============================================================

source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)
set.seed(42)

# -------------------------------------------------------------------
# Dataset 5: High-Dimensional Genomic Data (p > n)
# Purpose: Test LASSO variable selection when predictors >> samples
# -------------------------------------------------------------------
create_genomic_highdim <- function() {
  n <- 80
  p <- 50  # 50 gene expression features

  # Base gene expression matrix (correlated blocks)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("gene_", sprintf("%02d", 1:p))

  # Add correlation structure: genes 1-5 are pathway A, 6-10 pathway B
  for (i in 2:5) X[, i] <- X[, 1] * 0.6 + rnorm(n) * 0.8
  for (i in 7:10) X[, i] <- X[, 6] * 0.5 + rnorm(n) * 0.85

  # Sparse true effects: only 6 genes truly prognostic
  true_coef <- rep(0, p)
  true_coef[c(1, 6, 15, 22, 30, 45)] <- c(0.7, -0.5, 0.4, -0.6, 0.3, -0.35)

  linear_pred <- X %*% true_coef

  # Demographics
  age <- round(rnorm(n, mean = 60, sd = 10))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  tumor_stage <- factor(sample(c("Early", "Advanced"), n, replace = TRUE,
                                prob = c(0.55, 0.45)))

  # Survival driven by genomic risk + clinical factors
  combined_risk <- linear_pred + 0.3 * (as.numeric(tumor_stage) - 1) +
                   0.1 * (age - 60) / 10
  survival_times <- rexp(n, rate = exp(combined_risk * 0.35))

  admin_censor <- 36
  loss_followup <- rexp(n, rate = 0.025)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- pmin(survival_times, censoring_times)
  event <- as.numeric(survival_times <= censoring_times)

  # Build data frame
  clinical <- data.frame(
    sample_id = paste0("GEN_", sprintf("%03d", 1:n)),
    os_months = round(observed_time, 2),
    vital_status = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    sex = sex,
    tumor_stage = tumor_stage
  )

  gene_df <- as.data.frame(round(X, 4))
  data <- cbind(clinical, gene_df)
  return(data)
}


# -------------------------------------------------------------------
# Dataset 6: Multicollinearity Scenario
# Purpose: Test LASSO with highly correlated predictors
# -------------------------------------------------------------------
create_multicollinearity_study <- function() {
  n <- 180

  # Create a latent "inflammation" factor and derive correlated biomarkers
  inflammation <- rnorm(n)
  crp <- round(exp(1.5 + 0.8 * inflammation + rnorm(n, 0, 0.3)), 1)
  esr <- round(20 + 15 * inflammation + rnorm(n, 0, 5))
  il6 <- round(5 + 4 * inflammation + rnorm(n, 0, 1.5), 1)
  ferritin <- round(150 + 80 * inflammation + rnorm(n, 0, 30))

  # Create a latent "nutritional status" factor
  nutrition <- rnorm(n)
  albumin <- round(4.0 + 0.3 * nutrition + rnorm(n, 0, 0.15), 1)
  prealbumin <- round(25 + 5 * nutrition + rnorm(n, 0, 2), 1)
  bmi <- round(25 + 4 * nutrition + rnorm(n, 0, 2), 1)
  weight_loss_pct <- round(pmax(0, 5 - 3 * nutrition + rnorm(n, 0, 2)), 1)

  # Clinical
  age <- round(rnorm(n, 65, 11))
  age <- pmax(30, pmin(age, 90))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48)))
  ecog <- factor(sample(0:2, n, replace = TRUE, prob = c(0.45, 0.40, 0.15)))
  comorbidity_index <- rpois(n, lambda = 2)

  # Survival driven by BOTH latent factors + age
  risk_score <- 0.5 * inflammation +
               -0.4 * nutrition +
                0.3 * (age - 65) / 11 +
                0.2 * as.numeric(ecog)

  survival_times <- rweibull(n, shape = 1.15,
                             scale = exp(-risk_score * 0.35) * 30)

  admin_censor <- 48
  loss_followup <- rexp(n, rate = 0.018)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time <- pmax(observed_time, 0.1)
  event <- as.numeric(survival_times <= censoring_times)

  data <- data.frame(
    patient_id = paste0("MC_", sprintf("%03d", 1:n)),
    survival_months = observed_time,
    death = factor(event, levels = c(0, 1), labels = c("Alive", "Dead")),
    age = age,
    sex = sex,
    ecog_ps = ecog,
    comorbidity_index = comorbidity_index,
    crp_mg_l = crp,
    esr_mm_hr = esr,
    il6_pg_ml = il6,
    ferritin_ng_ml = ferritin,
    albumin_g_dl = albumin,
    prealbumin_mg_dl = prealbumin,
    bmi = bmi,
    weight_loss_pct = weight_loss_pct
  )

  # 4% missing
  set.seed(77)
  data$il6_pg_ml[sample(n, 5)] <- NA
  data$prealbumin_mg_dl[sample(n, 3)] <- NA

  return(data)
}


# ===============================================================
# Generate and save
# ===============================================================
message("Generating missing lassocox test datasets...")
message(paste(rep("=", 60), collapse = ""))

lassocox_genomic        <- create_genomic_highdim()
lassocox_multicollinear <- create_multicollinearity_study()

cat("Genomic dataset:", nrow(lassocox_genomic), "x", ncol(lassocox_genomic), "\n")
cat("  Events:", sum(lassocox_genomic$vital_status == "Dead"), "/",
    nrow(lassocox_genomic), "\n")
cat("Multicollinearity dataset:", nrow(lassocox_multicollinear), "x",
    ncol(lassocox_multicollinear), "\n")
cat("  Events:", sum(lassocox_multicollinear$death == "Dead"), "/",
    nrow(lassocox_multicollinear), "\n")

# .rda files into data/
save_data_multi_format(lassocox_genomic,        "lassocox_genomic",        save_csv = FALSE)
save_data_multi_format(lassocox_multicollinear, "lassocox_multicollinear", save_csv = FALSE)

# .csv and .omv into data-raw/non-rda/
dir.create("data-raw/non-rda", showWarnings = FALSE, recursive = TRUE)
write.csv(lassocox_genomic,
          "data-raw/non-rda/lassocox_genomic.csv", row.names = FALSE)
write.csv(lassocox_multicollinear,
          "data-raw/non-rda/lassocox_multicollinear.csv", row.names = FALSE)

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(lassocox_genomic,
                           "data-raw/non-rda/lassocox_genomic.omv")
  jmvReadWrite::write_omv(lassocox_multicollinear,
                           "data-raw/non-rda/lassocox_multicollinear.omv")
  message("Created .omv files in data-raw/non-rda/")
}

message("\nMissing lassocox test datasets generated successfully!")
message("Run from package root: Rscript data-raw/generate_missing_lassocox_data.R")
