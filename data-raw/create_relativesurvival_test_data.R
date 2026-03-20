# ═══════════════════════════════════════════════════════════════════════
# Test Data Generation: relativesurvival
# ═══════════════════════════════════════════════════════════════════════
#
# Generates a realistic cancer registry dataset suitable for relative
# survival analysis with the relsurv package and survexp.us rate table.
#
# Generated: 2026-03-18
# Seed: 42
# Observations: 200
# ═══════════════════════════════════════════════════════════════════════

set.seed(42)
n <- 200

# --- Demographics (must match survexp.us ratetable dimensions) --------

# Age at diagnosis: realistic cancer patient distribution (40-85)
age_at_diagnosis <- round(pmin(90, pmax(30, rnorm(n, mean = 65, sd = 12))))

# Sex: roughly balanced
sex <- factor(sample(c("male", "female"), n, replace = TRUE, prob = c(0.52, 0.48)))

# Calendar year of diagnosis: 2000-2015 (well within survexp.us range)
diagnosis_year <- sample(2000:2015, n, replace = TRUE)

# --- Disease characteristics ------------------------------------------

# Cancer site
cancer_site <- factor(sample(
  c("Colon", "Breast", "Lung", "Prostate"),
  n, replace = TRUE,
  prob = c(0.30, 0.25, 0.25, 0.20)
))

# Stage at diagnosis (I-IV)
stage <- factor(sample(
  c("I", "II", "III", "IV"),
  n, replace = TRUE,
  prob = c(0.20, 0.30, 0.30, 0.20)
), levels = c("I", "II", "III", "IV"), ordered = TRUE)

# Grade
grade <- factor(sample(
  c("Well differentiated", "Moderately differentiated", "Poorly differentiated"),
  n, replace = TRUE,
  prob = c(0.25, 0.50, 0.25)
))

# --- Survival outcomes ------------------------------------------------

# Base hazard rate per year (modified by stage and age)
stage_hr <- c(I = 1.0, II = 1.5, III = 2.5, IV = 5.0)
age_hr <- exp(0.03 * (age_at_diagnosis - 65))  # 3% increase per year above 65
site_hr <- c(Colon = 1.0, Breast = 0.7, Lung = 2.0, Prostate = 0.5)

base_rate <- 0.08  # ~8% annual hazard for reference group
individual_rate <- base_rate *
  stage_hr[as.character(stage)] *
  age_hr *
  site_hr[as.character(cancer_site)]

# Generate survival times (exponential model)
followup_years <- rexp(n, rate = individual_rate)

# Administrative censoring at 10 years
max_followup <- 10
censored_at_admin <- followup_years > max_followup
followup_years <- pmin(followup_years, max_followup)

# Additional random censoring (~15% lost to follow-up)
random_censor <- rbinom(n, 1, 0.15) == 1
vital_status <- as.integer(!censored_at_admin & !random_censor)
followup_years[random_censor] <- runif(sum(random_censor), 0.1, followup_years[random_censor])

# Ensure minimum follow-up of 1 month
followup_years <- pmax(followup_years, 1/12)

# --- Covariates for regression ----------------------------------------

# Charlson comorbidity index (0-3)
comorbidity <- sample(0:3, n, replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10))

# Tumor size (cm, continuous)
tumor_size <- round(pmax(0.5, rnorm(n, mean = 3.5, sd = 1.8)), 1)

# --- Assemble data frame ----------------------------------------------

relativesurvival_test <- data.frame(
  patient_id = seq_len(n),
  followup_years = round(followup_years, 2),
  vital_status = vital_status,
  age_at_diagnosis = age_at_diagnosis,
  sex = sex,
  diagnosis_year = diagnosis_year,
  cancer_site = cancer_site,
  stage = stage,
  grade = grade,
  comorbidity = comorbidity,
  tumor_size = tumor_size,
  stringsAsFactors = FALSE
)

# --- Summary statistics -----------------------------------------------
cat("Dataset: relativesurvival_test\n")
cat("Observations:", nrow(relativesurvival_test), "\n")
cat("Variables:", ncol(relativesurvival_test), "\n")
cat("Events (deaths):", sum(relativesurvival_test$vital_status), "\n")
cat("Censored:", sum(1 - relativesurvival_test$vital_status), "\n")
cat("Event rate:", round(mean(relativesurvival_test$vital_status) * 100, 1), "%\n")
cat("Median follow-up:", round(median(relativesurvival_test$followup_years), 1), "years\n")
cat("Age range:", range(relativesurvival_test$age_at_diagnosis), "\n")
cat("Year range:", range(relativesurvival_test$diagnosis_year), "\n")
cat("Sex distribution:", table(relativesurvival_test$sex), "\n\n")

# --- Save in multiple formats -----------------------------------------

# 1. RDA
save(relativesurvival_test, file = "data/relativesurvival_test.rda", compress = "xz")

# 2. CSV
write.csv(relativesurvival_test,
          file = "data-raw/non-rda/relativesurvival_test.csv",
          row.names = FALSE)

# 3. OMV (jamovi)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(
    relativesurvival_test,
    "data-raw/non-rda/relativesurvival_test.omv"
  )
}

cat("Files written:\n")
cat("  data/relativesurvival_test.rda\n")
cat("  data-raw/non-rda/relativesurvival_test.csv\n")
cat("  data-raw/non-rda/relativesurvival_test.omv\n")
