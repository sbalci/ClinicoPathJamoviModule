# ═══════════════════════════════════════════════════════════
# Test Data Generation: survivalcont
# ═══════════════════════════════════════════════════════════
# Generated: 2026-01-06 | Seed: 42
# Function: Survival Analysis for Continuous Variable with Cut-off Detection

library(tibble)
library(dplyr)
library(here)
library(lubridate)
set.seed(42)

# ═══ 1. BASIC CONTINUOUS EXPLANATORY VARIABLE ═══
n_basic <- 150
survivalcont_test <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_basic)),
  time_months = pmax(0.5, round(rexp(n_basic, rate = 1/36), 1)),
  outcome = sample(c("Alive", "Dead"), n_basic, replace = TRUE, prob = c(0.6, 0.4)),
  biomarker = round(rnorm(n_basic, mean = 100, sd = 25), 1),
  age = round(rnorm(n_basic, 65, 12)),
  sex = sample(c("Male", "Female"), n_basic, replace = TRUE)
)
# Add realistic correlation: higher biomarker → worse survival
survivalcont_test <- survivalcont_test %>%
  mutate(
    time_months = case_when(
      biomarker > 120 ~ time_months * 0.6,
      biomarker > 100 ~ time_months * 0.8,
      TRUE ~ time_months
    ),
    outcome = case_when(
      biomarker > 120 & time_months < 24 ~ "Dead",
      TRUE ~ outcome
    )
  )

# ═══ 2. KI67 BIOMARKER DATA ═══
n_ki67 <- 140
survivalcont_ki67 <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_ki67)),
  time_months = pmax(0.5, round(rexp(n_ki67, rate = 1/40), 1)),
  outcome = sample(c("Alive", "Dead"), n_ki67, replace = TRUE, prob = c(0.62, 0.38)),
  ki67_percent = round(runif(n_ki67, 0, 100), 1),
  tumor_grade = sample(c("Grade 1", "Grade 2", "Grade 3"), n_ki67, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), n_ki67, replace = TRUE)
)
# Ki67 correlation with survival
survivalcont_ki67 <- survivalcont_ki67 %>%
  mutate(
    time_months = time_months * (1 - ki67_percent/200),
    outcome = if_else(ki67_percent > 50 & time_months < 20, "Dead", outcome)
  )

# ═══ 3. PSA LEVEL DATA ═══
n_psa <- 130
survivalcont_psa <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_psa)),
  time_months = pmax(0.5, round(rexp(n_psa, rate = 1/38), 1)),
  outcome = sample(c("Alive", "Dead"), n_psa, replace = TRUE, prob = c(0.65, 0.35)),
  psa_level = round(exp(rnorm(n_psa, log(10), 1)), 2),
  gleason_score = sample(6:10, n_psa, replace = TRUE, prob = c(0.15, 0.25, 0.3, 0.2, 0.1)),
  treatment = sample(c("Surgery", "Radiation", "ADT"), n_psa, replace = TRUE)
)
# PSA correlation
survivalcont_psa <- survivalcont_psa %>%
  mutate(
    time_months = time_months / (1 + log10(psa_level)/2),
    outcome = if_else(psa_level > 20 & time_months < 15, "Dead", outcome)
  )

# ═══ 4. HEMOGLOBIN LEVEL DATA ═══
n_hgb <- 120
survivalcont_hemoglobin <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_hgb)),
  time_months = pmax(0.5, round(rexp(n_hgb, rate = 1/35), 1)),
  outcome = sample(c("Alive", "Dead"), n_hgb, replace = TRUE, prob = c(0.58, 0.42)),
  hemoglobin_gL = round(rnorm(n_hgb, 120, 20), 1),
  performance_status = sample(0:3, n_hgb, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
)
# Lower hemoglobin → worse survival
survivalcont_hemoglobin <- survivalcont_hemoglobin %>%
  mutate(
    time_months = time_months * (hemoglobin_gL / 120),
    outcome = if_else(hemoglobin_gL < 90 & time_months < 18, "Dead", outcome)
  )

# ═══ 5. TUMOR SIZE DATA ═══
n_size <- 135
survivalcont_tumorsize <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_size)),
  time_months = pmax(0.5, round(rexp(n_size, rate = 1/42), 1)),
  outcome = sample(c("Alive", "Dead"), n_size, replace = TRUE, prob = c(0.6, 0.4)),
  tumor_size_cm = round(abs(rnorm(n_size, 3.5, 1.5)), 1),
  lymph_nodes = sample(c("Negative", "Positive"), n_size, replace = TRUE, prob = c(0.6, 0.4)),
  histology = sample(c("Adenocarcinoma", "Squamous", "Other"), n_size, replace = TRUE)
)
# Larger tumor → worse survival
survivalcont_tumorsize <- survivalcont_tumorsize %>%
  mutate(
    time_months = time_months / (1 + tumor_size_cm/10),
    outcome = if_else(tumor_size_cm > 5 & time_months < 20, "Dead", outcome)
  )

# ═══ 6. AGE AS CONTINUOUS VARIABLE ═══
n_age <- 145
survivalcont_age <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_age)),
  time_months = pmax(0.5, round(rexp(n_age, rate = 1/40), 1)),
  outcome = sample(c("Alive", "Dead"), n_age, replace = TRUE, prob = c(0.61, 0.39)),
  age_years = round(rnorm(n_age, 68, 14)),
  comorbidity_index = sample(0:5, n_age, replace = TRUE, prob = c(0.25, 0.3, 0.2, 0.15, 0.07, 0.03))
)
# Older age → worse survival
survivalcont_age <- survivalcont_age %>%
  mutate(
    time_months = time_months * (90 - age_years) / 40,
    time_months = pmax(0.5, time_months),
    outcome = if_else(age_years > 80 & time_months < 12, "Dead", outcome)
  )

# ═══ 7. COMPETING RISKS DATA ═══
n_compete <- 110
survivalcont_compete <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_compete)),
  time_months = pmax(0.5, round(rexp(n_compete, rate = 1/38), 1)),
  outcome = sample(
    c("Alive_NED", "Alive_Disease", "Dead_Disease", "Dead_Other"),
    n_compete, replace = TRUE, prob = c(0.4, 0.2, 0.25, 0.15)
  ),
  biomarker_score = round(rnorm(n_compete, 50, 15), 1),
  risk_category = sample(c("Low", "Intermediate", "High"), n_compete, replace = TRUE)
)

# ═══ 8. DATE-BASED WITH CONTINUOUS VARIABLE ═══
n_dates <- 100
diagnosis_dates <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), length.out = n_dates)
survivalcont_dates <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_dates)),
  diagnosis_date = as.character(diagnosis_dates),
  followup_date = as.character(as.Date(diagnosis_dates) + round(rexp(n_dates, 1/30) * 30)),
  outcome = sample(c("Alive", "Dead"), n_dates, replace = TRUE, prob = c(0.63, 0.37)),
  continuous_marker = round(rnorm(n_dates, 75, 20), 1)
)

# ═══ 9. LANDMARK ANALYSIS DATA ═══
n_landmark <- 125
survivalcont_landmark <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_landmark)),
  time_months = pmax(0.5, round(rexp(n_landmark, rate = 1/45), 1)),
  outcome = sample(c("Alive", "Dead"), n_landmark, replace = TRUE, prob = c(0.64, 0.36)),
  early_response_score = round(rnorm(n_landmark, 60, 18), 1),
  baseline_score = round(rnorm(n_landmark, 55, 17), 1)
)

# ═══ 10. MULTIPLE CUTOFF OPTIMIZATION DATA ═══
n_multicut <- 150
survivalcont_multicut <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_multicut)),
  time_months = pmax(0.5, round(rexp(n_multicut, rate = 1/36), 1)),
  outcome = sample(c("Alive", "Dead"), n_multicut, replace = TRUE, prob = c(0.6, 0.4)),
  risk_score = round(rnorm(n_multicut, 100, 30), 1)
)
# Create clear risk strata
survivalcont_multicut <- survivalcont_multicut %>%
  mutate(
    time_months = case_when(
      risk_score < 70 ~ time_months * 1.5,   # Low risk - better survival
      risk_score < 100 ~ time_months * 1.0,  # Medium risk
      risk_score < 130 ~ time_months * 0.7,  # High risk
      TRUE ~ time_months * 0.4               # Very high risk
    )
  )

# ═══ 11. GENE EXPRESSION DATA ═══
n_expr <- 115
survivalcont_expression <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_expr)),
  time_months = pmax(0.5, round(rexp(n_expr, rate = 1/40), 1)),
  outcome = sample(c("Alive", "Dead"), n_expr, replace = TRUE, prob = c(0.62, 0.38)),
  gene_expression = round(2^rnorm(n_expr, 5, 2), 2),
  mutation_status = sample(c("Wild-type", "Mutant"), n_expr, replace = TRUE)
)
# Higher expression → worse survival (oncogene)
survivalcont_expression <- survivalcont_expression %>%
  mutate(
    time_months = time_months / (1 + log2(gene_expression)/10),
    outcome = if_else(gene_expression > 50 & time_months < 18, "Dead", outcome)
  )

# ═══ 12. EDGE CASES - SMALL DATASET ═══
survivalcont_small <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:20)),
  time_months = round(rexp(20, rate = 1/30), 1),
  outcome = sample(c("Alive", "Dead"), 20, replace = TRUE),
  continuous_var = round(rnorm(20, 50, 15), 1)
)

# ═══ 13. EDGE CASES - NO CLEAR CUTPOINT ═══
n_nocutoff <- 80
survivalcont_nocutoff <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_nocutoff)),
  time_months = round(rexp(n_nocutoff, rate = 1/35), 1),
  outcome = sample(c("Alive", "Dead"), n_nocutoff, replace = TRUE, prob = c(0.6, 0.4)),
  # Random variable with no correlation to survival
  random_marker = rnorm(n_nocutoff, 100, 20)
)

# ═══ 14. EDGE CASES - EXTREME VALUES ═══
n_extreme <- 70
survivalcont_extreme <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_extreme)),
  time_months = round(rexp(n_extreme, rate = 1/30), 1),
  outcome = sample(c("Alive", "Dead"), n_extreme, replace = TRUE),
  extreme_values = c(
    rnorm(60, 100, 15),  # Normal range
    runif(5, 0, 10),     # Very low outliers
    runif(5, 300, 500)   # Very high outliers
  )
)

# ═══ 15. EDGE CASES - MISSING DATA ═══
n_missing <- 90
survivalcont_missing <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_missing)),
  time_months = round(rexp(n_missing, rate = 1/32), 1),
  outcome = sample(c("Alive", "Dead"), n_missing, replace = TRUE),
  biomarker = rnorm(n_missing, 100, 20),
  covariate = sample(c("A", "B", "C"), n_missing, replace = TRUE)
)
# Add missing values
survivalcont_missing$time_months[sample(n_missing, 4)] <- NA
survivalcont_missing$outcome[sample(n_missing, 3)] <- NA
survivalcont_missing$biomarker[sample(n_missing, 12)] <- NA

# ═══ 16. EDGE CASES - CONSTANT VARIABLE ═══
survivalcont_constant <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:50)),
  time_months = round(rexp(50, rate = 1/28), 1),
  outcome = sample(c("Alive", "Dead"), 50, replace = TRUE),
  constant_marker = rep(100, 50)  # No variation
)

# ═══ 17. LARGE DATASET ═══
n_large <- 500
survivalcont_large <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_large)),
  time_months = pmax(0.5, round(rexp(n_large, rate = 1/38), 1)),
  outcome = sample(c("Alive", "Dead"), n_large, replace = TRUE, prob = c(0.6, 0.4)),
  biomarker = round(rnorm(n_large, 100, 25), 1),
  age = round(rnorm(n_large, 65, 12)),
  stage = sample(c("I", "II", "III", "IV"), n_large, replace = TRUE),
  grade = sample(1:3, n_large, replace = TRUE),
  sex = sample(c("Male", "Female"), n_large, replace = TRUE),
  site = sample(paste0("Site_", 1:10), n_large, replace = TRUE)
)

# ═══ SAVE ALL DATASETS ═══
datasets <- list(
  survivalcont_test = survivalcont_test,
  survivalcont_ki67 = survivalcont_ki67,
  survivalcont_psa = survivalcont_psa,
  survivalcont_hemoglobin = survivalcont_hemoglobin,
  survivalcont_tumorsize = survivalcont_tumorsize,
  survivalcont_age = survivalcont_age,
  survivalcont_compete = survivalcont_compete,
  survivalcont_dates = survivalcont_dates,
  survivalcont_landmark = survivalcont_landmark,
  survivalcont_multicut = survivalcont_multicut,
  survivalcont_expression = survivalcont_expression,
  survivalcont_small = survivalcont_small,
  survivalcont_nocutoff = survivalcont_nocutoff,
  survivalcont_extreme = survivalcont_extreme,
  survivalcont_missing = survivalcont_missing,
  survivalcont_constant = survivalcont_constant,
  survivalcont_large = survivalcont_large
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

cat("✅ Survival Cont test data: 17 datasets × 4 formats = 68 files\n")
