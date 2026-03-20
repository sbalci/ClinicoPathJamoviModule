# ===============================================================
# Comprehensive Test Data Generation: timeroc
# ===============================================================
#
# Generates a realistic biomarker evaluation dataset (n=200)
# for both time-dependent and binary ROC analysis.
#
# Clinical scenario: Breast cancer cohort with two candidate
# biomarkers (Ki67 score and gene expression risk score) evaluated
# for their ability to predict disease recurrence.
#
# Features:
# - Weibull survival times correlated with biomarker values
# - Two markers with different discriminative abilities
# - A noise marker (AUC ~0.5) for comparison tests
# - Factor and numeric outcome coding
# - Realistic censoring (~35%)
# - ~5% missing data
#
# Generated: 2026-03-20
# Seed: 2026
# ===============================================================

library(here)

set.seed(2026)

n <- 200

# ===============================================================
# 1. Baseline covariates
# ===============================================================

PatientID <- sprintf("TR-%04d", 1:n)

Age <- round(pmin(85, pmax(30, rnorm(n, mean = 58, sd = 12))))

Stage <- factor(
  sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  levels = c("I", "II", "III")
)

# ===============================================================
# 2. Biomarkers (correlated with outcome)
# ===============================================================

# Latent risk factor
latent_risk <- 0.3 * (as.numeric(Stage) - 1) + 0.01 * (Age - 58) + rnorm(n, 0, 0.5)

# Ki67 score: good discriminator (AUC ~0.75-0.80)
Ki67 <- round(pmax(1, pmin(100,
  25 + 20 * latent_risk + rnorm(n, 0, 10)
)), 1)

# Gene expression risk score: excellent discriminator (AUC ~0.80-0.85)
GeneScore <- round(pmax(0, pmin(10,
  4 + 2.5 * latent_risk + rnorm(n, 0, 1)
)), 2)

# Random noise marker (should have AUC ~0.50)
NoiseMarker <- round(rnorm(n, mean = 50, sd = 15), 1)

# ===============================================================
# 3. Survival times (Weibull, correlated with biomarkers)
# ===============================================================

base_shape <- 1.3
base_scale <- 60  # months

hazard_mult <- exp(0.03 * (Ki67 - 25) + 0.15 * (GeneScore - 4))
hazard_mult <- hazard_mult / median(hazard_mult)

U <- runif(n)
true_time <- (base_scale / hazard_mult) * (-log(U))^(1 / base_shape)
true_time <- round(pmax(0.5, true_time), 1)

admin_censor <- runif(n, min = 24, max = 120)
lost <- rbinom(n, 1, prob = 0.10)
loss_time <- ifelse(lost == 1, runif(n, 1, 90), Inf)

censor_time <- pmin(admin_censor, loss_time)
FollowUpMonths <- round(pmin(true_time, censor_time), 1)
Recurrence <- as.integer(true_time <= censor_time)

RecurrenceFactor <- factor(Recurrence, levels = c(0, 1),
                           labels = c("No Recurrence", "Recurrence"))

# ===============================================================
# 4. Sprinkle ~5% missing data
# ===============================================================

missing_ki67 <- sample(n, round(0.05 * n))
missing_gene <- sample(n, round(0.03 * n))
Ki67[missing_ki67] <- NA
GeneScore[missing_gene] <- NA

# ===============================================================
# 5. Assemble dataset
# ===============================================================

timeroc_test <- data.frame(
  PatientID       = PatientID,
  FollowUpMonths  = FollowUpMonths,
  Recurrence      = Recurrence,
  RecurrenceFactor = RecurrenceFactor,
  Age             = Age,
  Stage           = Stage,
  Ki67            = Ki67,
  GeneScore       = GeneScore,
  NoiseMarker     = NoiseMarker,
  stringsAsFactors = FALSE
)

# ===============================================================
# 6. Summary
# ===============================================================

cat("=== timeroc_test dataset ===\n")
cat("N:", nrow(timeroc_test), "\n")
cat("Events:", sum(Recurrence, na.rm = TRUE), "(", round(100 * mean(Recurrence), 1), "%)\n")
cat("Median follow-up:", round(median(FollowUpMonths), 1), "months\n")
cat("Follow-up range:", round(min(FollowUpMonths), 1), "-",
    round(max(FollowUpMonths), 1), "months\n")
cat("Ki67 missing:", sum(is.na(timeroc_test$Ki67)), "\n")
cat("GeneScore missing:", sum(is.na(timeroc_test$GeneScore)), "\n")

# ===============================================================
# 7. Save
# ===============================================================

save(timeroc_test,
     file = here("data", "timeroc_test.rda"),
     compress = "xz")
cat("\nSaved: data/timeroc_test.rda\n")

write.csv(timeroc_test,
          file = here("data-raw", "non-rda", "timeroc_test.csv"),
          row.names = FALSE)
cat("Saved: data-raw/non-rda/timeroc_test.csv\n")

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(
    timeroc_test,
    here("data-raw", "non-rda", "timeroc_test.omv")
  )
  cat("Saved: data-raw/non-rda/timeroc_test.omv\n")
}

cat("\nDone.\n")
