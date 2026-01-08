# ═══════════════════════════════════════════════════════════
# Test Data Generation: outlierdetection
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the outlierdetection jamovi function
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 250
#
# Outlier Detection Scenarios Tested:
# 1. Clean data (no outliers) - control variables
# 2. Mild univariate outliers (just beyond thresholds)
# 3. Extreme univariate outliers (far beyond thresholds)
# 4. Symmetric outliers (both tails)
# 5. Asymmetric outliers (one tail only)
# 6. Multivariate outliers (normal univariately, outliers multivariately)
# 7. Different distributions (normal, skewed, heavy-tailed, bimodal)
# 8. Clinical lab values with realistic outlier patterns
# 9. Anthropometric data with measurement errors
# 10. Biomarkers with biological and technical outliers

library(tibble)
library(dplyr)
library(MASS)  # For mvrnorm (multivariate normal)
library(here)
set.seed(42)

# Sample size
n <- 250

# ═══════════════════════════════════════════════════════════
# 1. CLEAN DATA (No Outliers) - Control Variables
# ═══════════════════════════════════════════════════════════

# Perfect normal distribution, no outliers
age_clean <- round(rnorm(n, mean = 55, sd = 12))
age_clean <- pmin(pmax(age_clean, 20), 90)  # Constrain to 20-90 range

# Clean bimodal distribution (two patient populations)
bmi_clean <- c(
  rnorm(n/2, mean = 24, sd = 3),    # Normal weight
  rnorm(n/2, mean = 32, sd = 3)     # Overweight
)
bmi_clean <- pmin(pmax(bmi_clean, 16), 45)  # Constrain to realistic range

# Clean skewed distribution (natural biological skew, no outliers)
glucose_clean <- rgamma(n, shape = 20, scale = 5)
glucose_clean <- pmin(glucose_clean, 200)  # Cap at reasonable maximum

# ═══════════════════════════════════════════════════════════
# 2. MILD UNIVARIATE OUTLIERS (Just Beyond Thresholds)
# ═══════════════════════════════════════════════════════════

# Hemoglobin with mild outliers (5% slightly low/high)
n_hgb_normal <- n - 2 * round(n * 0.025)  # Ensure exact n
hemoglobin_mild <- c(
  rnorm(n_hgb_normal, mean = 13.5, sd = 1.5),     # 95% normal range
  rnorm(round(n * 0.025), mean = 9.0, sd = 0.5),  # 2.5% mildly low (anemia)
  rnorm(round(n * 0.025), mean = 18.0, sd = 0.5)  # 2.5% mildly high (polycythemia)
)
hemoglobin_mild <- pmax(hemoglobin_mild, 5)  # Physiological minimum

# Systolic BP with mild outliers
n_sbp_normal <- n - round(n * 0.04) - round(n * 0.03)  # Ensure exact n
systolic_bp_mild <- c(
  rnorm(n_sbp_normal, mean = 125, sd = 12),       # 93% normal
  rnorm(round(n * 0.04), mean = 95, sd = 5),      # 4% mildly low (hypotension)
  rnorm(round(n * 0.03), mean = 165, sd = 8)      # 3% mildly high (hypertension)
)
systolic_bp_mild <- round(pmin(pmax(systolic_bp_mild, 70), 200))

# ═══════════════════════════════════════════════════════════
# 3. EXTREME UNIVARIATE OUTLIERS (Far Beyond Thresholds)
# ═══════════════════════════════════════════════════════════

# Creatinine with extreme outliers (3% severe renal impairment)
n_creat_normal <- n - round(n * 0.03)  # Ensure exact n
creatinine_extreme <- c(
  rnorm(n_creat_normal, mean = 1.0, sd = 0.2),    # 97% normal
  rnorm(round(n * 0.03), mean = 8.5, sd = 2.0)    # 3% severe renal failure
)
creatinine_extreme <- pmax(creatinine_extreme, 0.3)

# WBC count with extreme outliers (2% severe infection/leukemia)
n_wbc_normal <- n - round(n * 0.02)  # Ensure exact n
wbc_extreme <- c(
  rnorm(n_wbc_normal, mean = 7.5, sd = 2.0),      # 98% normal
  rnorm(round(n * 0.02), mean = 35, sd = 10)      # 2% extreme elevation
)
wbc_extreme <- pmax(wbc_extreme, 2)

# Troponin with extreme outliers (cardiac biomarker, 1% acute MI)
n_trop_normal <- n - round(n * 0.01)  # Ensure exact n
troponin_extreme <- c(
  rnorm(n_trop_normal, mean = 0.01, sd = 0.005),  # 99% normal (<0.04)
  rnorm(round(n * 0.01), mean = 5.5, sd = 2.0)    # 1% acute MI (>100x normal)
)
troponin_extreme <- pmax(troponin_extreme, 0)

# ═══════════════════════════════════════════════════════════
# 4. SYMMETRIC OUTLIERS (Both Tails)
# ═══════════════════════════════════════════════════════════

# Temperature with symmetric outliers (fever and hypothermia)
n_temp_normal <- n - 2 * round(n * 0.03)  # Ensure exact n
temperature_symmetric <- c(
  rnorm(n_temp_normal, mean = 37.0, sd = 0.3),     # 94% normal
  rnorm(round(n * 0.03), mean = 35.5, sd = 0.2),   # 3% hypothermia
  rnorm(round(n * 0.03), mean = 39.5, sd = 0.3)    # 3% fever
)
temperature_symmetric <- round(temperature_symmetric, 1)

# Sodium with symmetric outliers (hypo/hypernatremia)
n_sodium_normal <- n - 2 * round(n * 0.04)  # Ensure exact n
sodium_symmetric <- c(
  rnorm(n_sodium_normal, mean = 140, sd = 2),      # 92% normal
  rnorm(round(n * 0.04), mean = 128, sd = 2),      # 4% hyponatremia
  rnorm(round(n * 0.04), mean = 152, sd = 2)       # 4% hypernatremia
)
sodium_symmetric <- round(sodium_symmetric, 1)

# ═══════════════════════════════════════════════════════════
# 5. ASYMMETRIC OUTLIERS (One Tail Only)
# ═══════════════════════════════════════════════════════════

# ALT (liver enzyme) - only high outliers (liver damage)
n_alt_normal <- n - round(n * 0.05)  # Ensure exact n
alt_asymmetric <- c(
  rlnorm(n_alt_normal, meanlog = log(25), sdlog = 0.4),    # 95% normal
  rlnorm(round(n * 0.05), meanlog = log(200), sdlog = 0.6) # 5% elevated (hepatitis)
)
alt_asymmetric <- round(pmin(alt_asymmetric, 1000))

# D-dimer - only high outliers (thrombosis marker)
n_ddimer_normal <- n - round(n * 0.08)  # Ensure exact n
ddimer_asymmetric <- c(
  rlnorm(n_ddimer_normal, meanlog = log(0.3), sdlog = 0.3), # 92% normal
  rlnorm(round(n * 0.08), meanlog = log(3.5), sdlog = 0.7)  # 8% elevated (DVT/PE)
)
ddimer_asymmetric <- round(ddimer_asymmetric, 2)

# ═══════════════════════════════════════════════════════════
# 6. MULTIVARIATE OUTLIERS
# ═══════════════════════════════════════════════════════════
# Create correlated variables where some observations are normal
# univariately but outliers multivariately (high Mahalanobis distance)

# Generate correlated biomarker panel (3 biomarkers)
# 95% follow correlation structure, 5% break correlation (multivariate outliers)

# Correlation matrix for biomarker panel
cor_matrix <- matrix(c(
  1.0,  0.7,  0.6,
  0.7,  1.0,  0.65,
  0.6,  0.65, 1.0
), nrow = 3, byrow = TRUE)

# Covariance matrix (scale correlation by SDs)
sigma <- cor_matrix
diag(sigma) <- c(15^2, 12^2, 10^2)  # SDs: 15, 12, 10

# Normal multivariate data
n_normal_mv <- round(n * 0.95)
mv_normal <- mvrnorm(n_normal_mv, mu = c(50, 45, 40), Sigma = sigma)

# Multivariate outliers - normal margins but break correlation
n_outlier_mv <- n - n_normal_mv
biomarker1_mv_out <- rnorm(n_outlier_mv, mean = 50, sd = 15)  # Normal marginal
biomarker2_mv_out <- rnorm(n_outlier_mv, mean = 45, sd = 12)  # Normal marginal
biomarker3_mv_out <- rnorm(n_outlier_mv, mean = 40, sd = 10)  # Normal marginal
# But they don't follow the correlation structure!

mv_outliers <- cbind(biomarker1_mv_out, biomarker2_mv_out, biomarker3_mv_out)

# Combine and shuffle
mv_combined <- rbind(mv_normal, mv_outliers)
shuffle_idx <- sample(n)
mv_combined <- mv_combined[shuffle_idx, ]

biomarker1_multivar <- round(mv_combined[, 1], 1)
biomarker2_multivar <- round(mv_combined[, 2], 1)
biomarker3_multivar <- round(mv_combined[, 3], 1)

# ═══════════════════════════════════════════════════════════
# 7. DIFFERENT DISTRIBUTIONS
# ═══════════════════════════════════════════════════════════

# Heavy-tailed distribution (t-distribution, df=3)
# More extreme outliers than normal
psa_heavy_tail <- rt(n, df = 3) * 2 + 6
psa_heavy_tail <- pmax(psa_heavy_tail, 0.1)
psa_heavy_tail <- round(psa_heavy_tail, 2)

# Extreme skewed distribution (log-normal)
crp_skewed <- rlnorm(n, meanlog = log(3), sdlog = 1.2)
crp_skewed <- pmin(crp_skewed, 100)
crp_skewed <- round(crp_skewed, 1)

# Bimodal distribution with outliers
# Two populations (responders vs non-responders) + measurement errors
n_chol_pop1 <- round(n * 0.45)
n_chol_outliers <- round(n * 0.10)
n_chol_pop2 <- n - n_chol_pop1 - n_chol_outliers  # Ensure exact n
cholesterol_bimodal <- c(
  rnorm(n_chol_pop1, mean = 180, sd = 20),        # Population 1
  rnorm(n_chol_pop2, mean = 240, sd = 25),        # Population 2
  rnorm(n_chol_outliers, mean = 350, sd = 30)     # Outliers (familial hypercholesterolemia)
)
cholesterol_bimodal <- round(pmax(cholesterol_bimodal, 100))

# ═══════════════════════════════════════════════════════════
# 8. CLINICAL LAB VALUES WITH REALISTIC PATTERNS
# ═══════════════════════════════════════════════════════════

# Platelet count with outliers (thrombocytopenia/thrombocytosis)
n_plt_normal <- n - 2 * round(n * 0.05)  # Ensure exact n
platelets_clinical <- c(
  rnorm(n_plt_normal, mean = 250, sd = 50),        # 90% normal
  rnorm(round(n * 0.05), mean = 80, sd = 20),      # 5% thrombocytopenia
  rnorm(round(n * 0.05), mean = 650, sd = 100)     # 5% thrombocytosis
)
platelets_clinical <- round(pmax(platelets_clinical, 10))

# Potassium with narrow normal range, critical outliers
n_k_normal <- n - round(n * 0.04) - round(n * 0.03)  # Ensure exact n
potassium_clinical <- c(
  rnorm(n_k_normal, mean = 4.2, sd = 0.4),         # 93% normal (3.5-5.0)
  rnorm(round(n * 0.04), mean = 2.8, sd = 0.3),    # 4% hypokalemia (critical)
  rnorm(round(n * 0.03), mean = 6.2, sd = 0.4)     # 3% hyperkalemia (critical)
)
potassium_clinical <- round(pmax(potassium_clinical, 1.5), 1)

# ═══════════════════════════════════════════════════════════
# 9. ANTHROPOMETRIC DATA WITH MEASUREMENT ERRORS
# ═══════════════════════════════════════════════════════════

# Height with data entry errors (cm mis-entered as inches, etc.)
n_ht_normal <- n - 2 * round(n * 0.02)  # Ensure exact n
height_errors <- c(
  rnorm(n_ht_normal, mean = 170, sd = 10),         # 96% correct (cm)
  rnorm(round(n * 0.02), mean = 67, sd = 3),       # 2% entered in inches (outliers)
  rnorm(round(n * 0.02), mean = 5.7, sd = 0.3) * 30 # 2% entered in feet (extreme outliers)
)
height_errors <- round(pmax(height_errors, 50))

# Weight with implausible values (data entry errors)
n_wt_normal <- n - round(n * 0.03) - round(n * 0.02)  # Ensure exact n
weight_errors <- c(
  rnorm(n_wt_normal, mean = 75, sd = 15),          # 95% realistic
  rnorm(round(n * 0.03), mean = 750, sd = 50),     # 3% decimal point error (750 instead of 75.0)
  rnorm(round(n * 0.02), mean = 165, sd = 10) / 2.2 # 2% lb entered instead of kg
)
weight_errors <- round(pmax(weight_errors, 30), 1)

# ═══════════════════════════════════════════════════════════
# 10. BIOMARKERS WITH BIOLOGICAL AND TECHNICAL OUTLIERS
# ═══════════════════════════════════════════════════════════

# CA-125 tumor marker (highly variable, biological and technical outliers)
n_ca125_normal <- round(n * 0.85)
n_ca125_elevated <- round(n * 0.10)
n_ca125_high <- n - n_ca125_normal - n_ca125_elevated  # Ensure exact n
ca125_biomarker <- c(
  rlnorm(n_ca125_normal, meanlog = log(15), sdlog = 0.6),   # 85% normal/benign
  rlnorm(n_ca125_elevated, meanlog = log(250), sdlog = 0.8), # 10% elevated (cancer)
  rlnorm(n_ca125_high, meanlog = log(2000), sdlog = 0.9)    # 5% very high (advanced cancer)
)
ca125_biomarker <- round(pmin(ca125_biomarker, 5000), 1)

# HbA1c with diabetic patients and measurement errors
n_hba1c_normal <- round(n * 0.70)
n_hba1c_diabetic <- round(n * 0.25)
n_hba1c_poor <- n - n_hba1c_normal - n_hba1c_diabetic  # Ensure exact n
hba1c_biomarker <- c(
  rnorm(n_hba1c_normal, mean = 5.3, sd = 0.4),     # 70% non-diabetic
  rnorm(n_hba1c_diabetic, mean = 7.5, sd = 1.2),   # 25% diabetic
  rnorm(n_hba1c_poor, mean = 12.5, sd = 1.5)       # 5% poorly controlled (biological outliers)
)
hba1c_biomarker <- round(pmax(hba1c_biomarker, 4.0), 1)

# ═══════════════════════════════════════════════════════════
# PATIENT CHARACTERISTICS (for grouping/interpretation)
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PAT%04d", 1:n)

sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))

diagnosis <- sample(
  c("Healthy", "Diabetes", "Hypertension", "Cancer", "Renal Disease", "Liver Disease"),
  n, replace = TRUE,
  prob = c(0.30, 0.20, 0.18, 0.12, 0.10, 0.10)
)

# Study site (for batch effects)
site <- sample(c("Site A", "Site B", "Site C"), n, replace = TRUE)

# ═══════════════════════════════════════════════════════════
# ADD SOME MISSING DATA (5% in select variables)
# ═══════════════════════════════════════════════════════════

n_missing <- round(n * 0.05)
biomarker1_multivar[sample(n, n_missing)] <- NA
ca125_biomarker[sample(n, n_missing)] <- NA
ddimer_asymmetric[sample(n, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# COMBINE INTO DATASET
# ═══════════════════════════════════════════════════════════

outlierdetection_test <- tibble(
  # Patient info
  patient_id = patient_id,
  sex = sex,
  diagnosis = diagnosis,
  site = site,

  # 1. Clean data (no outliers)
  age_clean = age_clean,
  bmi_clean = bmi_clean,
  glucose_clean = glucose_clean,

  # 2. Mild outliers
  hemoglobin_mild = round(hemoglobin_mild, 1),
  systolic_bp_mild = systolic_bp_mild,

  # 3. Extreme outliers
  creatinine_extreme = round(creatinine_extreme, 2),
  wbc_extreme = round(wbc_extreme, 1),
  troponin_extreme = round(troponin_extreme, 3),

  # 4. Symmetric outliers
  temperature_symmetric = temperature_symmetric,
  sodium_symmetric = sodium_symmetric,

  # 5. Asymmetric outliers
  alt_asymmetric = alt_asymmetric,
  ddimer_asymmetric = ddimer_asymmetric,

  # 6. Multivariate outliers
  biomarker1_multivar = biomarker1_multivar,
  biomarker2_multivar = biomarker2_multivar,
  biomarker3_multivar = biomarker3_multivar,

  # 7. Different distributions
  psa_heavy_tail = psa_heavy_tail,
  crp_skewed = crp_skewed,
  cholesterol_bimodal = cholesterol_bimodal,

  # 8. Clinical lab values
  platelets_clinical = platelets_clinical,
  potassium_clinical = potassium_clinical,

  # 9. Anthropometric errors
  height_errors = height_errors,
  weight_errors = weight_errors,

  # 10. Biomarkers
  ca125_biomarker = ca125_biomarker,
  hba1c_biomarker = hba1c_biomarker
)

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(outlierdetection_test, file = here::here("data", "outlierdetection_test.rda"))

# 2. CSV format
write.csv(outlierdetection_test, file = here::here("data", "outlierdetection_test.csv"), row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(outlierdetection_test, path = here::here("data", "outlierdetection_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(outlierdetection_test, here::here("data", "outlierdetection_test.omv"))

# ═══════════════════════════════════════════════════════════
# DATASET DOCUMENTATION
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
OUTLIER DETECTION TEST DATASET
═══════════════════════════════════════════════════════════

Dataset: outlierdetection_test
Total Observations: ", n, "
Variables: ", ncol(outlierdetection_test), "
Generated: ", Sys.Date(), "
Seed: 42

═══════════════════════════════════════════════════════════
VARIABLE DESCRIPTIONS BY OUTLIER TYPE
═══════════════════════════════════════════════════════════

PATIENT CHARACTERISTICS:
  1. patient_id              - Unique patient identifier
  2. sex                     - Patient sex (Male/Female)
  3. diagnosis               - Primary diagnosis (6 categories)
  4. site                    - Study site (A/B/C for batch effects)

1. CLEAN DATA (No Outliers):
  5. age_clean               - Age in years, normal distribution, no outliers
  6. bmi_clean               - BMI, bimodal distribution (normal + overweight), no outliers
  7. glucose_clean           - Fasting glucose, gamma distribution, no outliers

2. MILD UNIVARIATE OUTLIERS (5%):
  8. hemoglobin_mild         - Hemoglobin (g/dL), 5% mild anemia/polycythemia
  9. systolic_bp_mild        - Systolic BP (mmHg), 7% mild hypo/hypertension

3. EXTREME UNIVARIATE OUTLIERS (1-3%):
 10. creatinine_extreme      - Creatinine (mg/dL), 3% severe renal failure
 11. wbc_extreme             - WBC count (×10⁹/L), 2% severe infection/leukemia
 12. troponin_extreme        - Troponin (ng/mL), 1% acute MI (>100× normal)

4. SYMMETRIC OUTLIERS (Both Tails, 6%):
 13. temperature_symmetric   - Body temperature (°C), 3% hypothermia + 3% fever
 14. sodium_symmetric        - Sodium (mEq/L), 4% hypo + 4% hypernatremia

5. ASYMMETRIC OUTLIERS (One Tail, 5-8%):
 15. alt_asymmetric          - ALT (U/L), 5% elevated (hepatitis), log-normal
 16. ddimer_asymmetric       - D-dimer (mg/L), 8% elevated (DVT/PE), log-normal

6. MULTIVARIATE OUTLIERS (5%):
 17. biomarker1_multivar     - Biomarker panel member 1 (correlated)
 18. biomarker2_multivar     - Biomarker panel member 2 (correlated)
 19. biomarker3_multivar     - Biomarker panel member 3 (correlated)
    NOTE: 5% are normal univariately but outliers multivariately
          (break correlation structure, high Mahalanobis distance)

7. DIFFERENT DISTRIBUTIONS:
 20. psa_heavy_tail          - PSA (ng/mL), t-distribution (heavy tails, more extremes)
 21. crp_skewed              - CRP (mg/L), extreme right skew, log-normal
 22. cholesterol_bimodal     - Total cholesterol (mg/dL), bimodal + 10% outliers

8. CLINICAL LAB VALUES:
 23. platelets_clinical      - Platelet count (×10⁹/L), 5% thrombocytopenia + 5% thrombocytosis
 24. potassium_clinical      - Potassium (mEq/L), 7% critical hypo/hyperkalemia

9. ANTHROPOMETRIC DATA WITH MEASUREMENT ERRORS:
 25. height_errors           - Height, 4% data entry errors (inches/feet instead of cm)
 26. weight_errors           - Weight, 5% data entry errors (decimal point, lb instead of kg)

10. BIOMARKERS (Biological + Technical Outliers):
 27. ca125_biomarker         - CA-125 tumor marker, 15% elevated/very high
 28. hba1c_biomarker         - HbA1c (%), 5% poorly controlled diabetes (biological outliers)

Missing Data: ~5% in biomarker1_multivar, ca125_biomarker, ddimer_asymmetric

═══════════════════════════════════════════════════════════
OUTLIER PREVALENCE BY VARIABLE
═══════════════════════════════════════════════════════════

No Outliers (0%):          age_clean, bmi_clean, glucose_clean
Mild (1-3%):               troponin_extreme (1%)
Low (3-5%):                hemoglobin_mild (5%), creatinine_extreme (3%)
Moderate (5-8%):           systolic_bp_mild (7%), alt_asymmetric (5%),
                           ddimer_asymmetric (8%), multivariate (5%)
High (>10%):               cholesterol_bimodal (10%), ca125_biomarker (15%)

═══════════════════════════════════════════════════════════
DETECTION METHOD TESTING
═══════════════════════════════════════════════════════════

UNIVARIATE METHODS:
✓ Z-score (standard)       - Good for normal distributions (age_clean, hemoglobin_mild)
✓ Z-score (robust/MAD)     - Better for skewed data (alt_asymmetric, crp_skewed)
✓ IQR                      - Robust to distribution (cholesterol_bimodal)
✓ ETI (interval-based)     - Symmetric confidence intervals
✓ HDI (interval-based)     - Bayesian credible intervals

MULTIVARIATE METHODS:
✓ Mahalanobis distance     - Detect multivariate outliers (biomarker panel)
✓ Robust Mahalanobis       - Resistant to outlier contamination
✓ MCD (Min Cov Det)        - Robust covariance estimation
✓ OPTICS clustering        - Density-based detection
✓ LOF (Local Outlier)      - Local density deviation

COMPOSITE METHOD:
✓ Multiple methods         - Robust consensus across algorithms
✓ Adjustable threshold     - Tune sensitivity (default 0.5 = 50% agreement)

═══════════════════════════════════════════════════════════
CLINICAL SCENARIOS REPRESENTED
═══════════════════════════════════════════════════════════

1. Hematology: hemoglobin_mild, wbc_extreme, platelets_clinical
2. Chemistry: creatinine_extreme, sodium_symmetric, potassium_clinical
3. Liver function: alt_asymmetric
4. Cardiac biomarkers: troponin_extreme
5. Coagulation: ddimer_asymmetric
6. Tumor markers: ca125_biomarker, psa_heavy_tail
7. Metabolic: glucose_clean, hba1c_biomarker, cholesterol_bimodal
8. Inflammatory: crp_skewed
9. Vital signs: temperature_symmetric, systolic_bp_mild
10. Anthropometric: height_errors, weight_errors, bmi_clean

═══════════════════════════════════════════════════════════
USAGE EXAMPLES
═══════════════════════════════════════════════════════════

# Load data
data(outlierdetection_test)

# Example 1: Univariate detection with robust z-score
outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'creatinine_extreme', 'wbc_extreme'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',
  zscore_threshold = 3.29,
  show_outlier_table = TRUE
)

# Example 2: Multivariate detection on biomarker panel
outlierdetection(
  data = outlierdetection_test,
  vars = c('biomarker1_multivar', 'biomarker2_multivar', 'biomarker3_multivar'),
  method_category = 'multivariate',
  multivariate_methods = 'mahalanobis',
  show_method_comparison = TRUE
)

# Example 3: Composite method for robust detection
outlierdetection(
  data = outlierdetection_test,
  vars = c('alt_asymmetric', 'crp_skewed', 'ca125_biomarker'),
  method_category = 'composite',
  composite_threshold = 0.6,
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# Example 4: Comprehensive analysis (all methods)
outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'creatinine_extreme', 'sodium_symmetric'),
  method_category = 'all',
  show_method_comparison = TRUE,
  show_exclusion_summary = TRUE
)

═══════════════════════════════════════════════════════════
FILES GENERATED
═══════════════════════════════════════════════════════════

✓ data/outlierdetection_test.rda    - R data format
✓ data/outlierdetection_test.csv    - CSV format
✓ data/outlierdetection_test.xlsx   - Excel format
✓ data/outlierdetection_test.omv    - Jamovi format

═══════════════════════════════════════════════════════════
")
