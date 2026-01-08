# ═══════════════════════════════════════════════════════════
# Test Data Generation: singlearm
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Single Arm Survival Analysis

library(tibble)
library(dplyr)
library(here)
library(lubridate)
set.seed(42)

# ═══ 1. OVERALL SURVIVAL (BASIC) ═══
n_basic <- 150
singlearm_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_basic)),
  time_months = pmax(0.5, round(rexp(n_basic, rate = 1/36), 1)),
  outcome = sample(c("Alive", "Dead"), n_basic, replace = TRUE, prob = c(0.6, 0.4)),
  age = round(rnorm(n_basic, 65, 12)),
  stage = sample(c("I", "II", "III", "IV"), n_basic, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  sex = sample(c("Male", "Female"), n_basic, replace = TRUE)
)

# ═══ 2. DATE-BASED SURVIVAL (YMD FORMAT) ═══
n_dates <- 120
diagnosis_dates <- seq(as.Date("2018-01-01"), as.Date("2020-12-31"), length.out = n_dates)
followup_time <- round(rexp(n_dates, rate = 1/30), 1)
singlearm_dates <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_dates)),
  diagnosis_date = as.character(diagnosis_dates),
  followup_date = as.character(as.Date(diagnosis_dates) + round(followup_time * 30)),
  outcome = sample(c("Alive", "Dead"), n_dates, replace = TRUE, prob = c(0.65, 0.35)),
  treatment = sample(c("Surgery", "Chemo", "Radio", "Combo"), n_dates, replace = TRUE)
)

# ═══ 3. COMPETING RISKS DATA ═══
n_compete <- 100
singlearm_compete <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_compete)),
  time_months = pmax(0.5, round(rexp(n_compete, rate = 1/40), 1)),
  outcome = sample(
    c("Alive_NED", "Alive_Disease", "Dead_Disease", "Dead_Other"),
    n_compete, replace = TRUE, prob = c(0.4, 0.2, 0.25, 0.15)
  ),
  biomarker = sample(c("Positive", "Negative"), n_compete, replace = TRUE),
  grade = sample(c("Low", "Intermediate", "High"), n_compete, replace = TRUE)
)

# ═══ 4. CAUSE-SPECIFIC SURVIVAL ═══
n_cause <- 110
singlearm_causespecific <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_cause)),
  time_months = pmax(0.5, round(rexp(n_cause, rate = 1/45), 1)),
  outcome = sample(
    c("Alive", "Dead_Cancer", "Dead_Cardiac", "Dead_Other"),
    n_cause, replace = TRUE, prob = c(0.5, 0.3, 0.12, 0.08)
  ),
  risk_score = round(rnorm(n_cause, 50, 15)),
  comorbidity = sample(c("None", "Diabetes", "Cardiac", "Both"), n_cause, replace = TRUE)
)

# ═══ 5. LANDMARK ANALYSIS DATA ═══
n_landmark <- 130
singlearm_landmark <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_landmark)),
  time_months = pmax(0.5, round(rexp(n_landmark, rate = 1/42), 1)),
  outcome = sample(c("Alive", "Dead"), n_landmark, replace = TRUE, prob = c(0.62, 0.38)),
  treatment_response = sample(c("CR", "PR", "SD", "PD"), n_landmark, replace = TRUE),
  early_toxicity = sample(c("Yes", "No"), n_landmark, replace = TRUE, prob = c(0.2, 0.8))
)

# ═══ 6. DATE FORMATS - DMY ═══
n_dmy <- 90
diagnosis_dates_dmy <- seq(as.Date("2019-06-01"), as.Date("2021-12-31"), length.out = n_dmy)
singlearm_dmy <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_dmy)),
  diagnosis_date = format(diagnosis_dates_dmy, "%d-%m-%Y"),
  followup_date = format(as.Date(diagnosis_dates_dmy) + round(rexp(n_dmy, 1/25) * 30), "%d-%m-%Y"),
  outcome = sample(c("Event", "Censored"), n_dmy, replace = TRUE, prob = c(0.4, 0.6))
)

# ═══ 7. DATE FORMATS - MDY ═══
n_mdy <- 85
diagnosis_dates_mdy <- seq(as.Date("2019-01-01"), as.Date("2022-06-30"), length.out = n_mdy)
singlearm_mdy <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_mdy)),
  diagnosis_date = format(diagnosis_dates_mdy, "%m/%d/%Y"),
  followup_date = format(as.Date(diagnosis_dates_mdy) + round(rexp(n_mdy, 1/28) * 30), "%m/%d/%Y"),
  outcome = sample(c("Alive", "Dead"), n_mdy, replace = TRUE, prob = c(0.68, 0.32))
)

# ═══ 8. DATETIME WITH HMS ═══
n_hms <- 75
diagnosis_datetime <- seq(as.POSIXct("2020-01-01 08:00:00"),
                          as.POSIXct("2022-12-31 17:00:00"), length.out = n_hms)
singlearm_datetime <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_hms)),
  diagnosis_datetime = as.character(diagnosis_datetime),
  followup_datetime = as.character(as.POSIXct(diagnosis_datetime) + round(rexp(n_hms, 1/32) * 30) * 86400),
  outcome = sample(c("Alive", "Dead"), n_hms, replace = TRUE, prob = c(0.7, 0.3)),
  emergency_admission = sample(c("Yes", "No"), n_hms, replace = TRUE, prob = c(0.15, 0.85))
)

# ═══ 9. LONG FOLLOW-UP DATA ═══
n_long <- 95
singlearm_longfu <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_long)),
  time_months = pmax(1, round(rexp(n_long, rate = 1/80), 1)),
  outcome = sample(c("Alive", "Dead"), n_long, replace = TRUE, prob = c(0.55, 0.45)),
  enrollment_year = sample(2010:2015, n_long, replace = TRUE),
  clinical_trial = sample(c("Trial_A", "Trial_B", "Registry"), n_long, replace = TRUE)
)

# ═══ 10. SHORT FOLLOW-UP DATA ═══
n_short <- 80
singlearm_shortfu <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_short)),
  time_months = pmax(0.1, round(rexp(n_short, rate = 1/12), 1)),
  outcome = sample(c("Alive", "Dead"), n_short, replace = TRUE, prob = c(0.5, 0.5)),
  disease = sample(c("Aggressive", "Indolent"), n_short, replace = TRUE, prob = c(0.7, 0.3))
)

# ═══ 11. PERSON-TIME ANALYSIS DATA ═══
n_pt <- 140
singlearm_persontime <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_pt)),
  time_months = pmax(0.5, round(rexp(n_pt, rate = 1/50), 1)),
  outcome = sample(c("Alive", "Dead"), n_pt, replace = TRUE, prob = c(0.58, 0.42)),
  age_group = sample(c("<50", "50-65", "65-75", ">75"), n_pt, replace = TRUE),
  exposure = sample(c("High", "Medium", "Low"), n_pt, replace = TRUE)
)

# ═══ 12. EDGE CASES - SMALL DATASET ═══
singlearm_small <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:15)),
  time_months = round(rexp(15, rate = 1/24), 1),
  outcome = sample(c("Alive", "Dead"), 15, replace = TRUE, prob = c(0.6, 0.4))
)

# ═══ 13. EDGE CASES - ALL CENSORED ═══
singlearm_censored <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:30)),
  time_months = round(runif(30, 1, 60), 1),
  outcome = rep("Alive", 30)
)

# ═══ 14. EDGE CASES - ALL EVENTS ═══
singlearm_allevents <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:25)),
  time_months = round(rexp(25, rate = 1/20), 1),
  outcome = rep("Dead", 25)
)

# ═══ 15. EDGE CASES - VERY EARLY EVENTS ═══
singlearm_early <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:40)),
  time_months = pmax(0.1, round(rexp(40, rate = 1/3), 1)),
  outcome = sample(c("Alive", "Dead"), 40, replace = TRUE, prob = c(0.3, 0.7)),
  rapid_progression = sample(c("Yes", "No"), 40, replace = TRUE, prob = c(0.8, 0.2))
)

# ═══ 16. EDGE CASES - MISSING DATA ═══
n_missing <- 60
singlearm_missing <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_missing)),
  time_months = round(rexp(n_missing, rate = 1/30), 1),
  outcome = sample(c("Alive", "Dead"), n_missing, replace = TRUE, prob = c(0.6, 0.4)),
  covariate1 = rnorm(n_missing, 50, 10),
  covariate2 = sample(c("A", "B", "C"), n_missing, replace = TRUE)
)
# Add missing values
singlearm_missing$time_months[sample(n_missing, 3)] <- NA
singlearm_missing$outcome[sample(n_missing, 2)] <- NA
singlearm_missing$covariate1[sample(n_missing, 8)] <- NA

# ═══ 17. EDGE CASES - ZERO TIME ═══
singlearm_zerotime <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:35)),
  time_months = c(rep(0, 5), round(rexp(30, rate = 1/24), 1)),
  outcome = sample(c("Alive", "Dead"), 35, replace = TRUE, prob = c(0.5, 0.5))
)

# ═══ 18. LARGE DATASET FOR PERFORMANCE TESTING ═══
n_large <- 500
singlearm_large <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_large)),
  time_months = pmax(0.5, round(rexp(n_large, rate = 1/40), 1)),
  outcome = sample(c("Alive", "Dead"), n_large, replace = TRUE, prob = c(0.6, 0.4)),
  age = round(rnorm(n_large, 65, 12)),
  stage = sample(c("I", "II", "III", "IV"), n_large, replace = TRUE),
  grade = sample(1:3, n_large, replace = TRUE),
  sex = sample(c("Male", "Female"), n_large, replace = TRUE),
  biomarker = rnorm(n_large, 100, 20),
  treatment = sample(c("A", "B", "C", "D"), n_large, replace = TRUE),
  site = sample(paste0("Site_", 1:10), n_large, replace = TRUE)
)

# ═══ SAVE ALL DATASETS ═══
datasets <- list(
  singlearm_test = singlearm_test,
  singlearm_dates = singlearm_dates,
  singlearm_compete = singlearm_compete,
  singlearm_causespecific = singlearm_causespecific,
  singlearm_landmark = singlearm_landmark,
  singlearm_dmy = singlearm_dmy,
  singlearm_mdy = singlearm_mdy,
  singlearm_datetime = singlearm_datetime,
  singlearm_longfu = singlearm_longfu,
  singlearm_shortfu = singlearm_shortfu,
  singlearm_persontime = singlearm_persontime,
  singlearm_small = singlearm_small,
  singlearm_censored = singlearm_censored,
  singlearm_allevents = singlearm_allevents,
  singlearm_early = singlearm_early,
  singlearm_missing = singlearm_missing,
  singlearm_zerotime = singlearm_zerotime,
  singlearm_large = singlearm_large
)

for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  save_name <- dataset_name
  assign(save_name, data)
  save(list = save_name, file = here::here("data", paste0(dataset_name, ".rda")), compress = "xz")
  write.csv(data, file = here::here("data", paste0(dataset_name, ".csv")), row.names = FALSE)
  writexl::write_xlsx(data, path = here::here("data", paste0(dataset_name, ".xlsx")))
  jmvReadWrite::write_omv(dtaFrm = data, fleOut = here::here("data", paste0(dataset_name, ".omv")), frcWrt = TRUE)
}

cat("✅ Single Arm test data: 18 datasets × 4 formats = 72 files\n")
