# ===============================================================
# Test Data Generation: firthregression (Firth Penalized Likelihood)
# ===============================================================
#
# Creates 3 datasets covering both logistic and Cox modes:
#   1. Standard clinical (n=120, balanced, no separation)
#   2. Rare events with separation (n=80, ~10% event rate, separation)
#   3. Small Cox cohort (n=50, survival with few events)
#
# Generated: 2026-03-16
# Seed: 42
# ===============================================================

set.seed(42)

# -------------------------------------------------------------------
# Dataset 1: Standard Clinical (Logistic + Cox)
# Balanced binary outcome, no separation, mixed predictor types
# -------------------------------------------------------------------
create_firth_standard <- function() {
  n <- 120

  age <- round(rnorm(n, 62, 11))
  age <- pmax(30, pmin(age, 85))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))
  grade <- factor(sample(c("Low", "Intermediate", "High"), n, replace = TRUE,
                         prob = c(0.25, 0.45, 0.30)))
  tumor_size <- round(rgamma(n, 2.5, 0.8), 1)
  tumor_size <- pmax(0.5, pmin(tumor_size, 12))
  lvi <- factor(sample(c("Absent", "Present"), n, replace = TRUE, prob = c(0.55, 0.45)))
  marker <- round(rnorm(n, 50, 20))
  marker <- pmax(0, pmin(marker, 100))

  # Binary outcome (logistic mode)
  lp <- 0.03 * (age - 62) + 0.4 * (grade == "High") + 0.12 * (tumor_size - 3) +
        0.3 * (lvi == "Present") - 0.01 * marker
  prob_event <- 1 / (1 + exp(-lp))
  mortality <- factor(ifelse(rbinom(n, 1, prob_event) == 1, "Dead", "Alive"))

  # Survival time (Cox mode)
  base_time <- rexp(n, rate = 0.02)
  surv_time <- base_time * exp(-lp)
  surv_time <- round(pmax(0.5, pmin(surv_time, 60)), 1)
  censor_time <- runif(n, 6, 48)
  follow_up <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)
  status <- factor(ifelse(event == 1, "Dead", "Alive"))

  data.frame(
    follow_up_time = follow_up,
    status = status,
    mortality = mortality,
    age = age, sex = sex, grade = grade,
    tumor_size = tumor_size, lvi = lvi, marker = marker,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------
# Dataset 2: Rare Events with Separation
# Low event rate (~10%), includes a variable with complete separation
# -------------------------------------------------------------------
create_firth_separation <- function() {
  n <- 80
  set.seed(123)

  age <- round(rnorm(n, 65, 10))
  age <- pmax(40, pmin(age, 85))
  bmi <- round(rnorm(n, 26, 4), 1)
  bmi <- pmax(16, pmin(bmi, 45))

  # Rare binary outcome (~10% event rate)
  base_prob <- 0.10
  lp <- 0.02 * (age - 65) + 0.03 * (bmi - 26)
  prob_event <- pmin(0.3, pmax(0.02, base_prob * exp(lp)))
  rare_event <- rbinom(n, 1, prob_event)

  # Create a variable with complete separation:
  # "margin_positive" is TRUE only when rare_event == 1 (plus some noise)
  margin_positive <- factor(
    ifelse(rare_event == 1, "Positive",
           ifelse(runif(n) < 0.02, "Positive", "Negative")),
    levels = c("Negative", "Positive")
  )

  # Grade (quasi-separation: almost all High -> event)
  grade <- factor(
    ifelse(rare_event == 1,
           sample(c("Low", "High"), n, replace = TRUE, prob = c(0.1, 0.9)),
           sample(c("Low", "High"), n, replace = TRUE, prob = c(0.7, 0.3))),
    levels = c("Low", "High")
  )

  outcome <- factor(ifelse(rare_event == 1, "Recurrence", "NoRecurrence"))

  data.frame(
    outcome = outcome,
    age = age, bmi = bmi, grade = grade,
    margin_positive = margin_positive,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------
# Dataset 3: Small Cox Cohort
# Very few events for Firth Cox regression
# -------------------------------------------------------------------
create_firth_smallcox <- function() {
  n <- 50
  set.seed(99)

  age <- round(rnorm(n, 58, 12))
  age <- pmax(30, pmin(age, 80))
  treatment <- factor(sample(c("Standard", "Experimental"), n, replace = TRUE))
  biomarker <- round(rnorm(n, 100, 30))
  biomarker <- pmax(10, biomarker)

  lp <- 0.02 * age + 0.3 * (treatment == "Standard") - 0.005 * biomarker
  surv_time <- rexp(n, rate = 0.04 * exp(lp - mean(lp)))
  surv_time <- round(pmax(0.5, pmin(surv_time, 36)), 1)

  # Heavy censoring -> few events
  censor_time <- runif(n, 3, 30)
  follow_up <- pmin(surv_time, censor_time)
  event <- as.integer(surv_time <= censor_time)
  status <- factor(ifelse(event == 1, "Dead", "Alive"))

  data.frame(
    time = follow_up,
    status = status,
    age = age, treatment = treatment, biomarker = biomarker,
    stringsAsFactors = FALSE
  )
}

# ===================================================================
# Generate and save
# ===================================================================
cat("Generating firthregression test datasets...\n")

firth_standard <- create_firth_standard()
cat("  standard: n=", nrow(firth_standard),
    ", mortality events=", sum(firth_standard$mortality == "Dead"),
    ", survival events=", sum(firth_standard$status == "Dead"), "\n")

firth_separation <- create_firth_separation()
cat("  separation: n=", nrow(firth_separation),
    ", events=", sum(firth_separation$outcome == "Recurrence"), "\n")

firth_smallcox <- create_firth_smallcox()
cat("  smallcox: n=", nrow(firth_smallcox),
    ", events=", sum(firth_smallcox$status == "Dead"), "\n")

# Save .rda to data/
save(firth_standard,   file = "data/firth_standard.rda")
save(firth_separation, file = "data/firth_separation.rda")
save(firth_smallcox,   file = "data/firth_smallcox.rda")

# Save .csv and .omv to data-raw/non-rda/
dir.create("data-raw/non-rda", showWarnings = FALSE, recursive = TRUE)

write.csv(firth_standard,   "data-raw/non-rda/firth_standard.csv",   row.names = FALSE)
write.csv(firth_separation, "data-raw/non-rda/firth_separation.csv", row.names = FALSE)
write.csv(firth_smallcox,   "data-raw/non-rda/firth_smallcox.csv",   row.names = FALSE)

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(firth_standard,   "data-raw/non-rda/firth_standard.omv")
  jmvReadWrite::write_omv(firth_separation, "data-raw/non-rda/firth_separation.omv")
  jmvReadWrite::write_omv(firth_smallcox,   "data-raw/non-rda/firth_smallcox.omv")
}

cat("\nDone. RDA saved to data/, CSV/OMV saved to data-raw/non-rda/\n")
