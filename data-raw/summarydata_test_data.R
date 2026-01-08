# ═══════════════════════════════════════════════════════════
# Test Data Generation: summarydata
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the summarydata jamovi function
# (Summary of Continuous Variables with Distribution Diagnostics)
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 200

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# Sample size
n <- 200

# ═══════════════════════════════════════════════════════════
# 1. NORMAL DISTRIBUTION VARIABLES (Pass Shapiro-Wilk)
# ═══════════════════════════════════════════════════════════

# Perfect normal distribution
age_normal <- rnorm(n, mean = 65, sd = 12)
age_normal <- round(pmax(18, pmin(age_normal, 100)))  # Age 18-100

# Normal with small variance
temperature_normal <- rnorm(n, mean = 37.0, sd = 0.5)
temperature_normal <- round(temperature_normal, 1)  # °C

# Normal with moderate variance
weight_normal <- rnorm(n, mean = 75, sd = 15)
weight_normal <- round(pmax(weight_normal, 40), 1)  # kg

# Normal with large variance
cholesterol_normal <- rnorm(n, mean = 200, sd = 40)
cholesterol_normal <- round(pmax(cholesterol_normal, 100))  # mg/dL

# ═══════════════════════════════════════════════════════════
# 2. RIGHT-SKEWED DISTRIBUTIONS (Positive Skewness)
# ═══════════════════════════════════════════════════════════

# Mild right skew (log-normal, skew ~1)
psa_mild_skew <- rlnorm(n, meanlog = log(5), sdlog = 0.6)
psa_mild_skew <- round(pmin(psa_mild_skew, 100), 1)  # PSA ng/mL

# Moderate right skew (log-normal, skew ~2)
crp_moderate_skew <- rlnorm(n, meanlog = log(3), sdlog = 1.0)
crp_moderate_skew <- round(pmin(crp_moderate_skew, 50), 1)  # CRP mg/L

# Strong right skew (log-normal, skew ~3)
income_strong_skew <- rlnorm(n, meanlog = log(50000), sdlog = 1.2)
income_strong_skew <- round(pmin(income_strong_skew, 500000), 0)  # Annual income

# Gamma distribution (count-like, right skew)
hospital_stay <- rgamma(n, shape = 3, scale = 2)
hospital_stay <- round(pmax(hospital_stay, 1))  # Days

# ═══════════════════════════════════════════════════════════
# 3. LEFT-SKEWED DISTRIBUTIONS (Negative Skewness)
# ═══════════════════════════════════════════════════════════

# Mild left skew (reflected log-normal)
test_score_mild_left <- 100 - rlnorm(n, meanlog = log(10), sdlog = 0.5)
test_score_mild_left <- round(pmax(test_score_mild_left, 0))  # 0-100 scale

# Moderate left skew (ceiling effect)
age_at_diagnosis_left <- 90 - rexp(n, rate = 0.05)
age_at_diagnosis_left <- round(pmax(age_at_diagnosis_left, 30))  # Years

# ═══════════════════════════════════════════════════════════
# 4. KURTOSIS VARIATIONS
# ═══════════════════════════════════════════════════════════

# Platykurtic (light tails, kurtosis < 3) - Uniform distribution
uniform_var <- runif(n, min = 50, max = 150)
uniform_var <- round(uniform_var, 1)

# Mesokurtic (normal tails, kurtosis ~3) - Normal distribution
mesokurtic_var <- rnorm(n, mean = 100, sd = 15)
mesokurtic_var <- round(mesokurtic_var, 1)

# Leptokurtic (heavy tails, kurtosis > 3) - t-distribution
leptokurtic_var <- rt(n, df = 4) * 15 + 100  # df=4 gives kurtosis ~6
leptokurtic_var <- round(leptokurtic_var, 1)

# Extreme leptokurtic (very heavy tails) - Cauchy distribution
cauchy_var <- rcauchy(n, location = 100, scale = 10)
cauchy_var <- round(pmax(pmin(cauchy_var, 200), 0), 1)  # Bounded

# ═══════════════════════════════════════════════════════════
# 5. BIMODAL DISTRIBUTIONS
# ═══════════════════════════════════════════════════════════

# Bimodal (two populations)
n1 <- round(n * 0.6)
n2 <- n - n1
bimodal_biomarker <- c(
  rnorm(n1, mean = 50, sd = 8),   # Population 1
  rnorm(n2, mean = 90, sd = 8)    # Population 2
)
bimodal_biomarker <- round(bimodal_biomarker, 1)

# Trimodal (responders/non-responders/super-responders)
n_resp1 <- round(n * 0.4)
n_resp2 <- round(n * 0.4)
n_resp3 <- n - n_resp1 - n_resp2
trimodal_response <- c(
  rnorm(n_resp1, mean = 30, sd = 5),   # Non-responders
  rnorm(n_resp2, mean = 60, sd = 5),   # Responders
  rnorm(n_resp3, mean = 90, sd = 5)    # Super-responders
)
trimodal_response <- round(trimodal_response, 1)

# ═══════════════════════════════════════════════════════════
# 6. VARIABLES WITH OUTLIERS (IQR Method)
# ═══════════════════════════════════════════════════════════

# Few mild outliers (~5%)
hemoglobin_few_outliers <- rnorm(n, mean = 13.5, sd = 1.5)
n_outliers <- round(n * 0.05)
outlier_indices <- sample(n, n_outliers)
hemoglobin_few_outliers[outlier_indices] <- rnorm(n_outliers, mean = 18.5, sd = 0.5)  # High outliers
hemoglobin_few_outliers <- round(pmax(hemoglobin_few_outliers, 5), 1)

# Many outliers (~15%)
glucose_many_outliers <- rnorm(n, mean = 100, sd = 15)
n_outliers_many <- round(n * 0.15)
outlier_indices_many <- sample(n, n_outliers_many)
glucose_many_outliers[outlier_indices_many] <- rnorm(n_outliers_many, mean = 250, sd = 30)
glucose_many_outliers <- round(pmax(glucose_many_outliers, 50))

# Extreme outliers
creatinine_extreme <- rnorm(n, mean = 1.0, sd = 0.2)
n_extreme <- round(n * 0.03)
extreme_indices <- sample(n, n_extreme)
creatinine_extreme[extreme_indices] <- rnorm(n_extreme, mean = 10, sd = 2)  # Renal failure
creatinine_extreme <- round(pmax(creatinine_extreme, 0.3), 1)

# No outliers (clean data)
albumin_no_outliers <- rnorm(n, mean = 4.0, sd = 0.3)
albumin_no_outliers <- round(pmax(albumin_no_outliers, 2.0), 1)  # g/dL

# ═══════════════════════════════════════════════════════════
# 7. MISSING DATA PATTERNS
# ═══════════════════════════════════════════════════════════

# No missing data
bmi_complete <- rnorm(n, mean = 25, sd = 5)
bmi_complete <- round(pmax(bmi_complete, 15), 1)

# Low missing (3%)
systolic_bp_low_missing <- rnorm(n, mean = 125, sd = 15)
systolic_bp_low_missing[sample(n, round(n * 0.03))] <- NA
systolic_bp_low_missing <- round(pmax(systolic_bp_low_missing, 70))

# Moderate missing (15%)
ldl_moderate_missing <- rnorm(n, mean = 130, sd = 30)
ldl_moderate_missing[sample(n, round(n * 0.15))] <- NA
ldl_moderate_missing <- round(pmax(ldl_moderate_missing, 50))

# High missing (35%)
vitamin_d_high_missing <- rnorm(n, mean = 25, sd = 10)
vitamin_d_high_missing[sample(n, round(n * 0.35))] <- NA
vitamin_d_high_missing <- round(pmax(vitamin_d_high_missing, 5), 1)

# Very high missing (60%)
genetic_score_very_high_missing <- rnorm(n, mean = 50, sd = 20)
genetic_score_very_high_missing[sample(n, round(n * 0.60))] <- NA
genetic_score_very_high_missing <- round(genetic_score_very_high_missing, 1)

# ═══════════════════════════════════════════════════════════
# 8. CLINICAL LAB VALUES (Different Scales and Ranges)
# ═══════════════════════════════════════════════════════════

# High precision (3 decimals) - Troponin
troponin <- rlnorm(n, meanlog = log(0.01), sdlog = 1.0)
troponin <- round(pmin(troponin, 5), 3)  # ng/mL

# Medium precision (1 decimal) - Hemoglobin
hemoglobin_lab <- rnorm(n, mean = 13.5, sd = 1.5)
hemoglobin_lab <- round(pmax(hemoglobin_lab, 5), 1)  # g/dL

# Integer values - White Blood Cell count
wbc_count <- rnorm(n, mean = 7.5, sd = 2.0)
wbc_count <- round(pmax(wbc_count, 2))  # x10³/μL

# Large values - Platelet count
platelet_count <- rnorm(n, mean = 250, sd = 50)
platelet_count <- round(pmax(platelet_count, 50))  # x10³/μL

# Small decimal values - Creatinine
creatinine_lab <- rnorm(n, mean = 1.0, sd = 0.3)
creatinine_lab <- round(pmax(creatinine_lab, 0.3), 2)  # mg/dL

# ═══════════════════════════════════════════════════════════
# 9. EDGE CASES
# ═══════════════════════════════════════════════════════════

# Constant variable (zero variance)
constant_var <- rep(100, n)

# Nearly constant (minimal variance)
nearly_constant <- rnorm(n, mean = 100, sd = 0.01)
nearly_constant <- round(nearly_constant, 2)

# Extreme range (0 to millions)
extreme_range <- rlnorm(n, meanlog = log(1000), sdlog = 3)
extreme_range <- round(extreme_range, 0)

# All missing
all_missing <- rep(NA_real_, n)

# Tiny variance (precision issues)
tiny_variance <- rnorm(n, mean = 1000000, sd = 0.1)
tiny_variance <- round(tiny_variance, 2)

# Negative values allowed
profit_loss <- rnorm(n, mean = 0, sd = 50000)
profit_loss <- round(profit_loss, 0)

# ═══════════════════════════════════════════════════════════
# 10. SAMPLE SIZE EDGE CASES FOR SHAPIRO-WILK
# ═══════════════════════════════════════════════════════════

# Very small sample (n=10)
small_sample_var <- rnorm(n, mean = 100, sd = 15)
small_sample_var[11:n] <- NA  # Only 10 valid observations
small_sample_var <- round(small_sample_var, 1)

# Exactly at lower limit (n=3 valid)
tiny_sample_var <- rnorm(n, mean = 50, sd = 10)
tiny_sample_var[4:n] <- NA  # Only 3 valid observations
tiny_sample_var <- round(tiny_sample_var, 1)

# Below lower limit (n=2 valid - Shapiro-Wilk not applicable)
insufficient_sample_var <- rnorm(n, mean = 75, sd = 10)
insufficient_sample_var[3:n] <- NA  # Only 2 valid observations
insufficient_sample_var <- round(insufficient_sample_var, 1)

# ═══════════════════════════════════════════════════════════
# 11. ANTHROPOMETRIC AND VITAL SIGNS
# ═══════════════════════════════════════════════════════════

# Height (cm)
height_cm <- rnorm(n, mean = 170, sd = 10)
height_cm <- round(pmax(pmin(height_cm, 210), 140), 1)

# Weight (kg)
weight_kg <- rnorm(n, mean = 75, sd = 15)
weight_kg <- round(pmax(pmin(weight_kg, 150), 40), 1)

# BMI (calculated)
bmi_calculated <- weight_kg / (height_cm / 100)^2
bmi_calculated <- round(bmi_calculated, 1)

# Diastolic BP
diastolic_bp <- rnorm(n, mean = 80, sd = 10)
diastolic_bp <- round(pmax(pmin(diastolic_bp, 120), 50))

# Heart rate
heart_rate <- rnorm(n, mean = 72, sd = 12)
heart_rate <- round(pmax(pmin(heart_rate, 150), 40))

# ═══════════════════════════════════════════════════════════
# 12. BIOMARKERS WITH DIFFERENT CHARACTERISTICS
# ═══════════════════════════════════════════════════════════

# Tumor marker CA-19-9 (highly variable, log-normal)
ca19_9 <- rlnorm(n, meanlog = log(20), sdlog = 1.5)
ca19_9 <- round(pmin(ca19_9, 1000), 1)

# HbA1c (diabetic control)
hba1c <- rnorm(n, mean = 6.5, sd = 1.5)
hba1c <- round(pmax(hba1c, 4.0), 1)

# Ferritin (iron stores, highly variable)
ferritin <- rlnorm(n, meanlog = log(100), sdlog = 1.0)
ferritin <- round(pmin(ferritin, 500), 0)

# TSH (thyroid function, log-normal)
tsh <- rlnorm(n, meanlog = log(2.0), sdlog = 0.8)
tsh <- round(pmin(tsh, 20), 2)

# ═══════════════════════════════════════════════════════════
# PATIENT IDENTIFIERS
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PAT%05d", 1:n)

# ═══════════════════════════════════════════════════════════
# CREATE TIBBLE WITH ALL VARIABLES
# ═══════════════════════════════════════════════════════════

summarydata_test <- tibble(
  # Identifier
  patient_id = patient_id,

  # 1. Normal distributions
  age_normal = age_normal,
  temperature_normal = temperature_normal,
  weight_normal = weight_normal,
  cholesterol_normal = cholesterol_normal,

  # 2. Right-skewed distributions
  psa_mild_skew = psa_mild_skew,
  crp_moderate_skew = crp_moderate_skew,
  income_strong_skew = income_strong_skew,
  hospital_stay = hospital_stay,

  # 3. Left-skewed distributions
  test_score_mild_left = test_score_mild_left,
  age_at_diagnosis_left = age_at_diagnosis_left,

  # 4. Kurtosis variations
  uniform_var = uniform_var,
  mesokurtic_var = mesokurtic_var,
  leptokurtic_var = leptokurtic_var,
  cauchy_var = cauchy_var,

  # 5. Bimodal distributions
  bimodal_biomarker = bimodal_biomarker,
  trimodal_response = trimodal_response,

  # 6. Variables with outliers
  hemoglobin_few_outliers = hemoglobin_few_outliers,
  glucose_many_outliers = glucose_many_outliers,
  creatinine_extreme = creatinine_extreme,
  albumin_no_outliers = albumin_no_outliers,

  # 7. Missing data patterns
  bmi_complete = bmi_complete,
  systolic_bp_low_missing = systolic_bp_low_missing,
  ldl_moderate_missing = ldl_moderate_missing,
  vitamin_d_high_missing = vitamin_d_high_missing,
  genetic_score_very_high_missing = genetic_score_very_high_missing,

  # 8. Clinical lab values
  troponin = troponin,
  hemoglobin_lab = hemoglobin_lab,
  wbc_count = wbc_count,
  platelet_count = platelet_count,
  creatinine_lab = creatinine_lab,

  # 9. Edge cases
  constant_var = constant_var,
  nearly_constant = nearly_constant,
  extreme_range = extreme_range,
  all_missing = all_missing,
  tiny_variance = tiny_variance,
  profit_loss = profit_loss,

  # 10. Sample size edge cases
  small_sample_var = small_sample_var,
  tiny_sample_var = tiny_sample_var,
  insufficient_sample_var = insufficient_sample_var,

  # 11. Anthropometric and vitals
  height_cm = height_cm,
  weight_kg = weight_kg,
  bmi_calculated = bmi_calculated,
  diastolic_bp = diastolic_bp,
  heart_rate = heart_rate,

  # 12. Biomarkers
  ca19_9 = ca19_9,
  hba1c = hba1c,
  ferritin = ferritin,
  tsh = tsh
)

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(summarydata_test, file = here::here("data", "summarydata_test.rda"))

# 2. CSV format
write.csv(summarydata_test,
          file = here::here("data", "summarydata_test.csv"),
          row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(summarydata_test,
                    path = here::here("data", "summarydata_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(summarydata_test, here::here("data", "summarydata_test.omv"))

cat("Dataset summarydata_test created successfully!\n")
