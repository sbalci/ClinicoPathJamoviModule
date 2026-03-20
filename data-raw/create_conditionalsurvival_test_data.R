# ═══════════════════════════════════════════════════════════
# Comprehensive Test Data Generation: conditionalsurvival
# ═══════════════════════════════════════════════════════════
#
# Generates a REALISTIC clinicopathological dataset (n=150)
# designed to exercise conditional survival estimation:
# - KM weights, landmark, IPW, and presmoothed KM methods
# - Unstratified and stratified (by treatment, stage) analyses
# - Custom and default time points
# - Various conditioning times
# - Factor and numeric outcome coding
#
# Clinical scenario: Colorectal cancer cohort with follow-up
# in months. Survival times are Weibull-distributed with
# stage-dependent hazard. Treatment effect modeled as HR.
#
# Generated: 2026-03-20
# Seed: 2026
# ═══════════════════════════════════════════════════════════

library(here)

set.seed(2026)

n <- 150

# ═══════════════════════════════════════════════════════════
# 1. Baseline covariates
# ═══════════════════════════════════════════════════════════

PatientID <- sprintf("CS-%03d", 1:n)

Age <- round(pmin(85, pmax(30, rnorm(n, mean = 63, sd = 12))))

Sex <- factor(
  sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))
)

# Treatment: Surgery only vs Surgery + Chemo
Treatment <- factor(
  sample(c("Surgery", "Surgery+Chemo"), n, replace = TRUE, prob = c(0.45, 0.55)),
  levels = c("Surgery", "Surgery+Chemo")
)

# Stage: I-IV distribution typical of colorectal cancer
Stage <- factor(
  sample(c("I", "II", "III", "IV"), n, replace = TRUE,
         prob = c(0.15, 0.30, 0.35, 0.20)),
  levels = c("I", "II", "III", "IV")
)

# Grade: correlated with stage
grade_probs <- function(stage) {
  switch(stage,
    "I"   = c(0.60, 0.30, 0.10),
    "II"  = c(0.40, 0.40, 0.20),
    "III" = c(0.20, 0.40, 0.40),
    "IV"  = c(0.10, 0.30, 0.60)
  )
}
Grade <- factor(
  sapply(as.character(Stage), function(s) {
    sample(c("Well", "Moderate", "Poor"), 1, prob = grade_probs(s))
  }),
  levels = c("Well", "Moderate", "Poor")
)

# CEA level (continuous biomarker, correlated with stage)
CEA <- round(pmax(0.5, rnorm(n, mean = 5 + 3 * as.numeric(Stage), sd = 4)), 1)

# ═══════════════════════════════════════════════════════════
# 2. Survival times (Weibull with prognostic correlations)
# ═══════════════════════════════════════════════════════════

# Baseline Weibull parameters (months)
base_shape <- 1.2
base_scale <- 60  # median ~50 months

# Hazard ratio multipliers
hr_stage <- c("I" = 1.0, "II" = 1.3, "III" = 2.0, "IV" = 3.5)
hr_treatment <- c("Surgery" = 1.0, "Surgery+Chemo" = 0.7)
hr_grade <- c("Well" = 1.0, "Moderate" = 1.2, "Poor" = 1.6)

# Per-patient hazard multiplier
hazard_mult <- hr_stage[as.character(Stage)] *
               hr_treatment[as.character(Treatment)] *
               hr_grade[as.character(Grade)] *
               exp(0.01 * (Age - 63))  # age effect

# Weibull survival times: T = scale * (-log(U))^(1/shape) / hazard_mult
U <- runif(n)
true_time <- (base_scale / hazard_mult) * (-log(U))^(1 / base_shape)
true_time <- round(pmax(0.5, true_time), 1)

# Administrative censoring at 120 months (10 years)
max_follow <- 120
admin_censor <- runif(n, min = 24, max = max_follow)

# Random loss to follow-up (~15%)
lost <- rbinom(n, 1, prob = 0.15)
loss_time <- ifelse(lost == 1, runif(n, min = 1, max = max_follow * 0.8), Inf)

# Observed time = min(event time, admin censoring, loss to follow-up)
censor_time <- pmin(admin_censor, loss_time)
OverallTime <- round(pmin(true_time, censor_time), 1)
Event <- as.integer(true_time <= censor_time)

# Factor-coded version of outcome
EventFactor <- factor(Event, levels = c(0, 1), labels = c("Alive", "Dead"))

# ═══════════════════════════════════════════════════════════
# 3. Assemble dataset
# ═══════════════════════════════════════════════════════════

conditionalsurvival_test <- data.frame(
  PatientID    = PatientID,
  OverallTime  = OverallTime,
  Event        = Event,
  EventFactor  = EventFactor,
  Age          = Age,
  Sex          = Sex,
  Treatment    = Treatment,
  Stage        = Stage,
  Grade        = Grade,
  CEA          = CEA,
  stringsAsFactors = FALSE
)

# ═══════════════════════════════════════════════════════════
# 4. Summary statistics
# ═══════════════════════════════════════════════════════════

cat("=== conditionalsurvival_test dataset ===\n")
cat("N:", nrow(conditionalsurvival_test), "\n")
cat("Events:", sum(Event), "(", round(100 * mean(Event), 1), "%)\n")
cat("Median follow-up:", round(median(OverallTime), 1), "months\n")
cat("Time range:", round(min(OverallTime), 1), "-", round(max(OverallTime), 1), "months\n")
cat("\nStage distribution:\n")
print(table(Stage))
cat("\nTreatment distribution:\n")
print(table(Treatment))
cat("\nEvents by treatment:\n")
print(table(Treatment, Event))
cat("\nEvents by stage:\n")
print(table(Stage, Event))

# ═══════════════════════════════════════════════════════════
# 5. Save in multiple formats
# ═══════════════════════════════════════════════════════════

# RDA
save(conditionalsurvival_test,
     file = here("data", "conditionalsurvival_test.rda"),
     compress = "xz")
cat("\nSaved: data/conditionalsurvival_test.rda\n")

# CSV
write.csv(conditionalsurvival_test,
          file = here("data-raw", "non-rda", "conditionalsurvival_test.csv"),
          row.names = FALSE)
cat("Saved: data-raw/non-rda/conditionalsurvival_test.csv\n")

# OMV (jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(
    conditionalsurvival_test,
    here("data-raw", "non-rda", "conditionalsurvival_test.omv")
  )
  cat("Saved: data-raw/non-rda/conditionalsurvival_test.omv\n")
} else {
  cat("jmvReadWrite not available, skipping OMV export\n")
}

cat("\nDone.\n")
