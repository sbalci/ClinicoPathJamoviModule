# ===============================================================
# Comprehensive Test Data Generation: curemodels
# ===============================================================
#
# Generates a realistic cancer survival dataset (n=250) with a
# built-in cure fraction (~30%) for testing all cure model types:
# mixture (smcure), non-mixture (flexsurvcure), cuRe, npcure.
#
# Clinical scenario: Early-stage colorectal cancer cohort with
# curative-intent treatment. ~30% of patients are cured (never
# recur), while uncured patients have Weibull-distributed
# recurrence times.
#
# Features:
# - Explicit cure/susceptible groups with realistic correlations
# - Factor and numeric outcome coding
# - Continuous covariate for npcure (tumor_size)
# - Simulated background hazard for cuRe models
# - Multiple predictors with prognostic signal
# - ~3% missing data
#
# Generated: 2026-03-20
# Seed: 2026
# ===============================================================

library(here)

set.seed(2026)

n <- 250

# ===============================================================
# 1. Baseline covariates
# ===============================================================

PatientID <- sprintf("CM-%04d", 1:n)

Age <- round(pmin(85, pmax(35, rnorm(n, mean = 62, sd = 11))))

Sex <- factor(
  sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))
)

Treatment <- factor(
  sample(c("Surgery Only", "Surgery+Adjuvant"), n, replace = TRUE, prob = c(0.40, 0.60)),
  levels = c("Surgery Only", "Surgery+Adjuvant")
)

Stage <- factor(
  sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  levels = c("I", "II", "III")
)

# Continuous covariate for npcure model
TumorSize <- round(pmax(0.5, rnorm(n, mean = 3.5, sd = 1.5)), 1)

# Performance status (0=good, 1=moderate, 2=poor)
PerformanceStatus <- sample(0:2, n, replace = TRUE, prob = c(0.5, 0.35, 0.15))

# ===============================================================
# 2. Cure probability (depends on covariates)
# ===============================================================

# Logistic model for cure probability
logit_cure <- 0.5 +                       # base ~62% cure for Stage I
  -0.8 * (as.numeric(Stage) - 1) +        # lower stage → more cure
  0.4 * (Treatment == "Surgery+Adjuvant") + # adjuvant helps
  -0.02 * (Age - 62) +                    # older → less cure
  -0.15 * TumorSize +                     # larger tumor → less cure
  -0.3 * PerformanceStatus                # worse PS → less cure

cure_prob <- plogis(logit_cure)
is_cured <- rbinom(n, 1, cure_prob)

# ===============================================================
# 3. Survival times
# ===============================================================

# Cured patients: very long censoring times (administrative)
# Uncured patients: Weibull recurrence times

base_shape <- 1.4
base_scale <- 36  # months

# Uncured hazard modifiers
hazard_mult <- exp(
  0.3 * (as.numeric(Stage) - 1) +
  -0.2 * (Treatment == "Surgery+Adjuvant") +
  0.01 * (Age - 62) +
  0.1 * TumorSize
)

U <- runif(n)
uncured_time <- (base_scale / hazard_mult) * (-log(U))^(1 / base_shape)
uncured_time <- round(pmax(0.5, uncured_time), 1)

# Cured patients get long admin-censored times
cured_time <- round(runif(n, min = 60, max = 120), 1)

# True event time
true_time <- ifelse(is_cured == 1, cured_time, uncured_time)

# Administrative censoring at 120 months
admin_censor <- runif(n, min = 36, max = 120)

# Random loss to follow-up (~8%)
lost <- rbinom(n, 1, prob = 0.08)
loss_time <- ifelse(lost == 1, runif(n, 1, 100), Inf)

censor_time <- pmin(admin_censor, loss_time)
FollowUpMonths <- round(pmin(true_time, censor_time), 1)

# Event indicator: 1 = recurrence, 0 = censored
# Cured patients: always censored (their "event" time is beyond follow-up)
Recurrence <- as.integer(!is_cured & uncured_time <= censor_time)

RecurrenceFactor <- factor(Recurrence, levels = c(0, 1),
                           labels = c("Censored", "Recurrence"))

# ===============================================================
# 4. Background hazard (for cuRe model)
# ===============================================================

# Simulate age-sex-specific background mortality rate (per month)
# Based on approximate US life table rates
BackgroundHazard <- round(
  exp(-10 + 0.08 * Age + 0.1 * (Sex == "Male")) / 12,
  6
)

# ===============================================================
# 5. Sprinkle ~3% missing data
# ===============================================================

missing_age <- sample(n, round(0.03 * n))
missing_tumor <- sample(n, round(0.02 * n))
Age[missing_age] <- NA
TumorSize[missing_tumor] <- NA

# ===============================================================
# 6. Assemble dataset
# ===============================================================

curemodels_test <- data.frame(
  PatientID         = PatientID,
  FollowUpMonths    = FollowUpMonths,
  Recurrence        = Recurrence,
  RecurrenceFactor  = RecurrenceFactor,
  Age               = Age,
  Sex               = Sex,
  Treatment         = Treatment,
  Stage             = Stage,
  TumorSize         = TumorSize,
  PerformanceStatus = PerformanceStatus,
  BackgroundHazard  = BackgroundHazard,
  stringsAsFactors  = FALSE
)

# ===============================================================
# 7. Summary
# ===============================================================

cat("=== curemodels_test dataset ===\n")
cat("N:", nrow(curemodels_test), "\n")
cat("Events:", sum(Recurrence, na.rm = TRUE), "(", round(100 * mean(Recurrence), 1), "%)\n")
cat("True cure rate:", round(100 * mean(is_cured), 1), "%\n")
cat("Median follow-up:", round(median(FollowUpMonths), 1), "months\n")
cat("Follow-up range:", round(min(FollowUpMonths), 1), "-",
    round(max(FollowUpMonths), 1), "months\n")
cat("\nStage distribution:\n")
print(table(Stage))
cat("\nTreatment distribution:\n")
print(table(Treatment))
cat("\nEvents by stage:\n")
print(table(Stage, Recurrence))

# ===============================================================
# 8. Save
# ===============================================================

save(curemodels_test,
     file = here("data", "curemodels_test.rda"),
     compress = "xz")
cat("\nSaved: data/curemodels_test.rda\n")

write.csv(curemodels_test,
          file = here("data-raw", "non-rda", "curemodels_test.csv"),
          row.names = FALSE)
cat("Saved: data-raw/non-rda/curemodels_test.csv\n")

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(
    curemodels_test,
    here("data-raw", "non-rda", "curemodels_test.omv")
  )
  cat("Saved: data-raw/non-rda/curemodels_test.omv\n")
}

cat("\nDone.\n")
