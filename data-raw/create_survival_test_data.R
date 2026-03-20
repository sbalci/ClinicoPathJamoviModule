# ═══════════════════════════════════════════════════════════
# Comprehensive Test Data Generation: survival
# ═══════════════════════════════════════════════════════════
#
# Generates a REALISTIC clinicopathological dataset (n=200)
# designed to exercise every major feature of the survival
# jamovi function: KM, Cox, competing risks, date-based time,
# person-time, RMST, age adjustment, stratification, RCS,
# calibration, bootstrap validation, parametric models, etc.
#
# Survival times are Weibull-distributed with realistic
# hazard correlations by stage, grade, age, Ki67, and margins.
#
# Generated: 2026-03-16
# Seed: 2026
# ═══════════════════════════════════════════════════════════

library(here)

# ───────────────────────────────────────────────────────────
# Helper: simulate correlated Weibull survival times
# ───────────────────────────────────────────────────────────
# We use the inverse-CDF method:
#   T = scale * (-log(U))^(1/shape)
# where U ~ Uniform(0,1).
#
# Then multiply by a hazard-ratio multiplier per patient to
# create realistic prognostic factor correlations.
# A multiplier < 1 means shorter survival (higher hazard).
# ───────────────────────────────────────────────────────────

set.seed(2026)

n <- 200

# ═══════════════════════════════════════════════════════════
# 1. Baseline covariates (independent of survival)
# ═══════════════════════════════════════════════════════════

PatientID <- 1:n

# Age: normal, mean 62, sd 13, clipped to 25-90
Age <- round(pmin(90, pmax(25, rnorm(n, mean = 62, sd = 13))))

Sex <- factor(
  sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))
)

# Tumor stage: weighted to give reasonable stage distribution
TumorStage <- factor(

  sample(
    c("Stage I", "Stage II", "Stage III", "Stage IV"),
    n, replace = TRUE,
    prob = c(0.20, 0.30, 0.30, 0.20)
  ),
  levels = c("Stage I", "Stage II", "Stage III", "Stage IV")
)

# Tumor grade: correlated with stage (higher stage -> higher grade tendency)
TumorGrade <- factor(
  sapply(TumorStage, function(s) {
    probs <- switch(as.character(s),
      "Stage I"   = c(0.50, 0.35, 0.15),
      "Stage II"  = c(0.30, 0.45, 0.25),
      "Stage III" = c(0.15, 0.40, 0.45),
      "Stage IV"  = c(0.10, 0.30, 0.60)
    )
    sample(c("Grade 1", "Grade 2", "Grade 3"), 1, prob = probs)
  }),
  levels = c("Grade 1", "Grade 2", "Grade 3")
)

# Ki67: right-skewed (beta distribution scaled to 0-100)
# Higher in higher grade tumors
Ki67 <- round(sapply(TumorGrade, function(g) {
  base <- switch(as.character(g),
    "Grade 1" = rbeta(1, 2, 8),   # median ~20%
    "Grade 2" = rbeta(1, 3, 5),   # median ~37%
    "Grade 3" = rbeta(1, 5, 3)    # median ~62%
  )
  base * 100
}), 1)

# Tumor size: log-normal, 0.5-15 cm, correlated with stage
TumorSize <- round(sapply(TumorStage, function(s) {
  mu <- switch(as.character(s),
    "Stage I"   = 1.5,
    "Stage II"  = 3.0,
    "Stage III" = 5.0,
    "Stage IV"  = 7.0
  )
  pmin(15, pmax(0.5, rlnorm(1, log(mu), 0.5)))
}), 1)

LymphovascularInvasion <- factor(
  sapply(TumorStage, function(s) {
    p <- switch(as.character(s),
      "Stage I" = 0.10, "Stage II" = 0.25,
      "Stage III" = 0.55, "Stage IV" = 0.75
    )
    sample(c("Present", "Absent"), 1, prob = c(p, 1 - p))
  }),
  levels = c("Absent", "Present")
)

MarginStatus <- factor(
  sample(
    c("Negative", "Close", "Positive"),
    n, replace = TRUE,
    prob = c(0.65, 0.20, 0.15)
  ),
  levels = c("Negative", "Close", "Positive")
)

HER2Status <- factor(
  sample(
    c("Positive", "Negative", "Equivocal"),
    n, replace = TRUE,
    prob = c(0.20, 0.65, 0.15)
  ),
  levels = c("Negative", "Equivocal", "Positive")
)

ERStatus <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.70, 0.30)),
  levels = c("Negative", "Positive")
)


# ═══════════════════════════════════════════════════════════
# 2. Simulate survival times (Weibull with covariate effects)
# ═══════════════════════════════════════════════════════════

# Weibull parameters for baseline (Stage I, Grade 1, etc.)
weibull_shape <- 1.2     # slight increasing hazard (aging effect)
weibull_scale <- 72      # median ~60 months for baseline group

# Hazard ratio multipliers (applied as time multiplier = 1/HR)
# Higher HR -> shorter survival -> smaller time multiplier
hr_stage <- ifelse(TumorStage == "Stage I", 1.0,
            ifelse(TumorStage == "Stage II", 1.4,
            ifelse(TumorStage == "Stage III", 2.0, 3.0)))  # Stage IV: HR~3

hr_grade <- ifelse(TumorGrade == "Grade 1", 1.0,
            ifelse(TumorGrade == "Grade 2", 1.3, 2.0))     # Grade 3: HR~2

hr_age <- ifelse(Age > 75, 1.5,
          ifelse(Age > 65, 1.2, 1.0))

hr_margin <- ifelse(MarginStatus == "Negative", 1.0,
             ifelse(MarginStatus == "Close", 1.3, 1.8))

hr_ki67 <- ifelse(Ki67 > 30, 1.4, 1.0)

hr_lvi <- ifelse(LymphovascularInvasion == "Present", 1.5, 1.0)

# Combined hazard ratio (multiplicative model)
combined_hr <- hr_stage * hr_grade * hr_age * hr_margin * hr_ki67 * hr_lvi

# Time multiplier is inverse of HR (higher HR = shorter time)
time_mult <- 1 / (combined_hr^(1 / weibull_shape))

# Generate Weibull survival times
U <- runif(n)
raw_time <- weibull_scale * (-log(U))^(1 / weibull_shape) * time_mult

# Enforce minimum 0.5 months, round to 1 decimal
FollowUpMonths <- round(pmax(0.5, raw_time), 1)


# ═══════════════════════════════════════════════════════════
# 3. Assign event status with competing risks
# ═══════════════════════════════════════════════════════════
# Target distribution:
#   ~55% Dead of Disease
#   ~10% Dead of Other Causes
#   ~15% Alive with Disease
#   ~20% Alive without Disease

# Patients who survived a long time relative to their risk profile
# are more likely to be alive/censored
# Median time for this cohort is roughly 36 months

survival_percentile <- rank(FollowUpMonths) / n

# Base probabilities, modified by survival percentile
p_dod  <- 0.55 * (1 - 0.5 * survival_percentile)   # less likely if survived long
p_dooc <- 0.10 + 0.05 * (Age > 70)                  # more likely if old
p_awd  <- 0.15
p_awod <- 1 - p_dod - p_dooc - p_awd

# Normalize and sample
status_probs <- cbind(p_dod, p_dooc, p_awd, p_awod)
status_probs <- status_probs / rowSums(status_probs)

Status <- factor(
  sapply(1:n, function(i) {
    sample(
      c("Dead of Disease", "Dead of Other Causes",
        "Alive with Disease", "Alive without Disease"),
      1, prob = status_probs[i, ]
    )
  }),
  levels = c("Alive without Disease", "Alive with Disease",
             "Dead of Disease", "Dead of Other Causes")
)


# ═══════════════════════════════════════════════════════════
# 4. Generate date variables
# ═══════════════════════════════════════════════════════════

# Diagnosis dates: spread over 2015-2023
DiagnosisDate <- as.Date("2015-01-01") + sample(0:2921, n, replace = TRUE)
# 2921 = days between 2015-01-01 and 2022-12-31

# Last follow-up date: diagnosis + FollowUpMonths (convert months to days)
LastFollowUpDate <- DiagnosisDate + round(FollowUpMonths * 30.44)


# ═══════════════════════════════════════════════════════════
# 5. Introduce ~5% missing data in Ki67 and TumorSize
# ═══════════════════════════════════════════════════════════

n_missing_ki67 <- round(n * 0.05)
n_missing_size <- round(n * 0.05)
Ki67[sample(n, n_missing_ki67)] <- NA
TumorSize[sample(n, n_missing_size)] <- NA


# ═══════════════════════════════════════════════════════════
# 6. Assemble the data frame
# ═══════════════════════════════════════════════════════════

survival_comprehensive <- data.frame(
  PatientID              = PatientID,
  FollowUpMonths         = FollowUpMonths,
  Status                 = Status,
  TumorStage             = TumorStage,
  Age                    = Age,
  Sex                    = Sex,
  TumorGrade             = TumorGrade,
  Ki67                   = Ki67,
  TumorSize              = TumorSize,
  LymphovascularInvasion = LymphovascularInvasion,
  MarginStatus           = MarginStatus,
  DiagnosisDate          = as.character(DiagnosisDate),
  LastFollowUpDate       = as.character(LastFollowUpDate),
  HER2Status             = HER2Status,
  ERStatus               = ERStatus,
  stringsAsFactors       = FALSE
)


# ═══════════════════════════════════════════════════════════
# 7. Summary diagnostics
# ═══════════════════════════════════════════════════════════

cat("═══════════════════════════════════════════════════════════\n")
cat("survival_comprehensive: Dataset Summary\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat("Dimensions:", nrow(survival_comprehensive), "x", ncol(survival_comprehensive), "\n\n")

cat("Status distribution:\n")
print(table(survival_comprehensive$Status))
cat("\n")

cat("Stage distribution:\n")
print(table(survival_comprehensive$TumorStage))
cat("\n")

cat("Grade distribution:\n")
print(table(survival_comprehensive$TumorGrade))
cat("\n")

cat("Median FollowUpMonths:", median(survival_comprehensive$FollowUpMonths), "\n")
cat("Mean Age:", round(mean(survival_comprehensive$Age), 1), "\n")
cat("Ki67 missing:", sum(is.na(survival_comprehensive$Ki67)), "\n")
cat("TumorSize missing:", sum(is.na(survival_comprehensive$TumorSize)), "\n\n")


# ═══════════════════════════════════════════════════════════
# 8. Save in multiple formats
# ═══════════════════════════════════════════════════════════

# --- RDA (compressed) ---
save(survival_comprehensive,
     file = here::here("data", "survival_comprehensive.rda"),
     compress = "xz")
cat("Saved: data/survival_comprehensive.rda\n")

# --- CSV ---
csv_dir <- here::here("data-raw", "non-rda")
if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
write.csv(survival_comprehensive,
          file = file.path(csv_dir, "survival_comprehensive.csv"),
          row.names = FALSE)
cat("Saved: data-raw/non-rda/survival_comprehensive.csv\n")

# --- OMV (jamovi format, if available) ---
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(
    survival_comprehensive,
    file.path(csv_dir, "survival_comprehensive.omv")
  )
  cat("Saved: data-raw/non-rda/survival_comprehensive.omv\n")
} else {
  cat("Skipped: OMV format (jmvReadWrite not installed)\n")
}

cat("\nDone.\n")
