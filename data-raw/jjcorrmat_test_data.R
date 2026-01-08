# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjcorrmat
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjcorrmat jamovi function
# which creates correlation matrix visualizations with significance testing.
#
# Generated: 2026-01-05
# Seed: 42
# Observations: Various (100-200 per dataset)

library(tibble)
library(dplyr)
library(MASS)  # For mvrnorm (multivariate normal)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: General Clinical Metrics (jjcorrmat_test)
# ═══════════════════════════════════════════════════════════
# Mixed correlation structure for comprehensive testing

n_test <- 150

# Define correlation matrix for realistic clinical relationships
cor_matrix_test <- matrix(c(
  1.00,  0.65,  0.70,  0.55, -0.10,  0.05,  # tumor_size
  0.65,  1.00,  0.85,  0.60, -0.15,  0.00,  # ki67_index (proliferation)
  0.70,  0.85,  1.00,  0.50, -0.12,  0.03,  # mitotic_count
  0.55,  0.60,  0.50,  1.00, -0.20,  0.10,  # necrosis_percent
 -0.10, -0.15, -0.12, -0.20,  1.00, -0.05,  # age
  0.05,  0.00,  0.03,  0.10, -0.05,  1.00   # bmi
), nrow = 6, byrow = TRUE)

# Generate correlated data
means_test <- c(45, 35, 15, 25, 62, 26)
sds_test <- c(15, 20, 8, 15, 12, 4)

# Generate multivariate normal data
raw_data_test <- mvrnorm(n = n_test, mu = means_test,
                          Sigma = cor_matrix_test * outer(sds_test, sds_test))

jjcorrmat_test <- tibble(
  patient_id = 1:n_test,
  tumor_size = pmax(5, raw_data_test[, 1]),  # Tumor size (mm)
  ki67_index = pmin(100, pmax(0, raw_data_test[, 2])),  # Ki-67 proliferation index (0-100%)
  mitotic_count = pmax(0, round(raw_data_test[, 3])),  # Mitotic count per HPF
  necrosis_percent = pmin(100, pmax(0, raw_data_test[, 4])),  # Necrosis percentage
  age = round(pmax(18, pmin(90, raw_data_test[, 5]))),  # Age in years
  bmi = pmax(15, pmin(45, raw_data_test[, 6])),  # BMI
  tumor_stage = sample(c("I", "II", "III", "IV"), n_test, replace = TRUE,
                       prob = c(0.2, 0.3, 0.3, 0.2))
)

# Add ~3% missing data
n_missing_test <- round(n_test * 0.03)
jjcorrmat_test$ki67_index[sample(n_test, n_missing_test)] <- NA
jjcorrmat_test$necrosis_percent[sample(n_test, n_missing_test)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 2: Biomarker Panel (jjcorrmat_biomarker)
# ═══════════════════════════════════════════════════════════
# Multiple tumor markers with realistic correlations

n_bio <- 120

# Correlation matrix for biomarkers
cor_matrix_bio <- matrix(c(
  1.00,  0.45,  0.20,  0.35,  0.50, -0.40,  # CEA
  0.45,  1.00,  0.25,  0.30,  0.40, -0.35,  # CA19-9
  0.20,  0.25,  1.00,  0.15,  0.25, -0.20,  # AFP
  0.35,  0.30,  0.15,  1.00,  0.60, -0.30,  # LDH
  0.50,  0.40,  0.25,  0.60,  1.00, -0.55,  # CRP (inflammation)
 -0.40, -0.35, -0.20, -0.30, -0.55,  1.00   # Albumin (inverse with inflammation)
), nrow = 6, byrow = TRUE)

# Log-normal distributions for biomarkers
means_bio <- c(log(15), log(25), log(8), log(250), log(10), log(4.0))
sds_bio <- c(0.8, 1.0, 1.2, 0.6, 1.0, 0.3)

raw_data_bio <- mvrnorm(n = n_bio, mu = means_bio,
                         Sigma = cor_matrix_bio * outer(sds_bio, sds_bio))

jjcorrmat_biomarker <- tibble(
  patient_id = 1:n_bio,
  cea = exp(raw_data_bio[, 1]),  # CEA (ng/mL)
  ca199 = exp(raw_data_bio[, 2]),  # CA19-9 (U/mL)
  afp = exp(raw_data_bio[, 3]),  # AFP (ng/mL)
  ldh = exp(raw_data_bio[, 4]),  # LDH (U/L)
  crp = exp(raw_data_bio[, 5]),  # CRP (mg/L)
  albumin = exp(raw_data_bio[, 6]),  # Albumin (g/dL)
  cancer_type = sample(c("Colorectal", "Pancreatic", "Hepatocellular"),
                       n_bio, replace = TRUE)
)

# Add missing data
n_missing_bio <- round(n_bio * 0.05)
jjcorrmat_biomarker$afp[sample(n_bio, n_missing_bio)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 3: Laboratory Values (jjcorrmat_labvalues)
# ═══════════════════════════════════════════════════════════
# Clinical chemistry panel with realistic relationships

n_lab <- 200

# Correlation matrix for lab values
cor_matrix_lab <- matrix(c(
  1.00,  0.55,  0.60,  0.15,  0.50,  0.10,  0.25,  0.30,  # glucose
  0.55,  1.00,  0.70, -0.40,  0.85,  0.20,  0.15,  0.20,  # cholesterol
  0.60,  0.70,  1.00, -0.50,  0.80,  0.25,  0.10,  0.15,  # triglycerides
  0.15, -0.40, -0.50,  1.00, -0.60,  0.05,  0.00,  0.00,  # HDL
  0.50,  0.85,  0.80, -0.60,  1.00,  0.15,  0.18,  0.22,  # LDL
  0.10,  0.20,  0.25,  0.05,  0.15,  1.00,  0.35,  0.40,  # creatinine
  0.25,  0.15,  0.10,  0.00,  0.18,  0.35,  1.00,  0.75,  # ALT (liver enzymes)
  0.30,  0.20,  0.15,  0.00,  0.22,  0.40,  0.75,  1.00   # AST
), nrow = 8, byrow = TRUE)

means_lab <- c(95, 195, 140, 55, 115, 0.95, 30, 28)
sds_lab <- c(20, 40, 60, 15, 35, 0.25, 15, 12)

raw_data_lab <- mvrnorm(n = n_lab, mu = means_lab,
                         Sigma = cor_matrix_lab * outer(sds_lab, sds_lab))

jjcorrmat_labvalues <- tibble(
  patient_id = 1:n_lab,
  glucose = pmax(60, pmin(300, raw_data_lab[, 1])),  # mg/dL
  cholesterol = pmax(100, pmin(350, raw_data_lab[, 2])),  # mg/dL
  triglycerides = pmax(30, pmin(400, raw_data_lab[, 3])),  # mg/dL
  hdl = pmax(20, pmin(100, raw_data_lab[, 4])),  # mg/dL
  ldl = pmax(50, pmin(250, raw_data_lab[, 5])),  # mg/dL
  creatinine = pmax(0.5, pmin(2.5, raw_data_lab[, 6])),  # mg/dL
  alt = pmax(5, pmin(150, raw_data_lab[, 7])),  # U/L
  ast = pmax(5, pmin(150, raw_data_lab[, 8])),  # U/L
  risk_group = sample(c("Low", "Medium", "High"), n_lab, replace = TRUE)
)

# Add missing data
n_missing_lab <- round(n_lab * 0.02)
jjcorrmat_labvalues$triglycerides[sample(n_lab, n_missing_lab)] <- NA
jjcorrmat_labvalues$hdl[sample(n_lab, n_missing_lab)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 4: Imaging Metrics (jjcorrmat_imaging)
# ═══════════════════════════════════════════════════════════
# Radiological measurements with strong size correlations

n_img <- 100

# Strong correlations for size measurements
cor_matrix_img <- matrix(c(
  1.00,  0.92,  0.88,  0.65,  0.70, -0.55,  # tumor_volume
  0.92,  1.00,  0.85,  0.60,  0.65, -0.50,  # longest_diameter
  0.88,  0.85,  1.00,  0.58,  0.63, -0.48,  # shortest_diameter
  0.65,  0.60,  0.58,  1.00,  0.90, -0.45,  # SUV_max
  0.70,  0.65,  0.63,  0.90,  1.00, -0.50,  # SUV_mean
 -0.55, -0.50, -0.48, -0.45, -0.50,  1.00   # ADC (inverse with cellularity)
), nrow = 6, byrow = TRUE)

means_img <- c(log(25), log(40), log(30), log(8), log(5), log(1.2))
sds_img <- c(0.6, 0.5, 0.5, 0.4, 0.4, 0.3)

raw_data_img <- mvrnorm(n = n_img, mu = means_img,
                         Sigma = cor_matrix_img * outer(sds_img, sds_img))

jjcorrmat_imaging <- tibble(
  patient_id = 1:n_img,
  tumor_volume = exp(raw_data_img[, 1]),  # cm³
  tumor_longest_diameter = exp(raw_data_img[, 2]),  # mm
  tumor_shortest_diameter = exp(raw_data_img[, 3]),  # mm
  suv_max = exp(raw_data_img[, 4]),  # SUV (PET)
  suv_mean = exp(raw_data_img[, 5]),  # SUV (PET)
  adc = exp(raw_data_img[, 6]),  # ADC (×10⁻³ mm²/s)
  imaging_modality = sample(c("CT", "MRI", "PET-CT"), n_img, replace = TRUE)
)

# Add missing data
n_missing_img <- round(n_img * 0.03)
jjcorrmat_imaging$suv_max[sample(n_img, n_missing_img)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 5: Vital Signs (jjcorrmat_vitals)
# ═══════════════════════════════════════════════════════════
# Physiological measurements with realistic correlations

n_vital <- 180

cor_matrix_vital <- matrix(c(
  1.00,  0.75,  0.35,  0.20, -0.10,  0.15,  # systolic_bp
  0.75,  1.00,  0.30,  0.15, -0.08,  0.12,  # diastolic_bp
  0.35,  0.30,  1.00,  0.40, -0.25,  0.20,  # heart_rate
  0.20,  0.15,  0.40,  1.00, -0.30,  0.10,  # respiratory_rate
 -0.10, -0.08, -0.25, -0.30,  1.00, -0.35,  # temperature
  0.15,  0.12,  0.20,  0.10, -0.35,  1.00   # oxygen_saturation
), nrow = 6, byrow = TRUE)

means_vital <- c(125, 78, 75, 16, 37.0, 97)
sds_vital <- c(15, 10, 12, 3, 0.6, 2.5)

raw_data_vital <- mvrnorm(n = n_vital, mu = means_vital,
                           Sigma = cor_matrix_vital * outer(sds_vital, sds_vital))

jjcorrmat_vitals <- tibble(
  patient_id = 1:n_vital,
  systolic_bp = pmax(90, pmin(180, raw_data_vital[, 1])),  # mmHg
  diastolic_bp = pmax(50, pmin(110, raw_data_vital[, 2])),  # mmHg
  heart_rate = pmax(45, pmin(140, round(raw_data_vital[, 3]))),  # bpm
  respiratory_rate = pmax(10, pmin(30, round(raw_data_vital[, 4]))),  # breaths/min
  temperature = pmax(35.5, pmin(39.5, raw_data_vital[, 5])),  # °C
  oxygen_saturation = pmax(88, pmin(100, round(raw_data_vital[, 6]))),  # %
  patient_status = sample(c("Stable", "Monitoring", "Critical"),
                          n_vital, replace = TRUE, prob = c(0.6, 0.3, 0.1))
)

# Add missing data
n_missing_vital <- round(n_vital * 0.02)
jjcorrmat_vitals$oxygen_saturation[sample(n_vital, n_missing_vital)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 6: Mixed Correlations (jjcorrmat_mixed)
# ═══════════════════════════════════════════════════════════
# Variables with varying correlation strengths for testing

n_mixed <- 150

# Designed correlation matrix with varying strengths
cor_matrix_mixed <- matrix(c(
  1.00,  0.90,  0.70,  0.40,  0.10, -0.30, -0.60,  # var_a (very strong positive)
  0.90,  1.00,  0.65,  0.35,  0.08, -0.25, -0.55,  # var_b (strong positive)
  0.70,  0.65,  1.00,  0.50,  0.05, -0.20, -0.45,  # var_c (moderate positive)
  0.40,  0.35,  0.50,  1.00,  0.02, -0.15, -0.30,  # var_d (weak positive)
  0.10,  0.08,  0.05,  0.02,  1.00, -0.05, -0.08,  # var_e (near zero)
 -0.30, -0.25, -0.20, -0.15, -0.05,  1.00,  0.80,  # var_f (moderate negative)
 -0.60, -0.55, -0.45, -0.30, -0.08,  0.80,  1.00   # var_g (strong negative)
), nrow = 7, byrow = TRUE)

means_mixed <- rep(50, 7)
sds_mixed <- rep(15, 7)

raw_data_mixed <- mvrnorm(n = n_mixed, mu = means_mixed,
                           Sigma = cor_matrix_mixed * outer(sds_mixed, sds_mixed))

jjcorrmat_mixed <- tibble(
  subject_id = 1:n_mixed,
  var_a = raw_data_mixed[, 1],  # Very strong positive correlation with b
  var_b = raw_data_mixed[, 2],  # Strong positive
  var_c = raw_data_mixed[, 3],  # Moderate positive
  var_d = raw_data_mixed[, 4],  # Weak positive
  var_e = raw_data_mixed[, 5],  # Near zero correlation
  var_f = raw_data_mixed[, 6],  # Moderate negative
  var_g = raw_data_mixed[, 7],  # Strong negative with a
  group = sample(c("Group1", "Group2", "Group3"), n_mixed, replace = TRUE)
)

# Add missing data
n_missing_mixed <- round(n_mixed * 0.04)
jjcorrmat_mixed$var_d[sample(n_mixed, n_missing_mixed)] <- NA
jjcorrmat_mixed$var_e[sample(n_mixed, n_missing_mixed)] <- NA


# ═══════════════════════════════════════════════════════════
# Save all datasets in multiple formats
# ═══════════════════════════════════════════════════════════

# Dataset 1: General clinical metrics
save(jjcorrmat_test, file = here("data", "jjcorrmat_test.rda"))
write.csv(jjcorrmat_test, file = here("data", "jjcorrmat_test.csv"), row.names = FALSE)
write_xlsx(jjcorrmat_test, path = here("data", "jjcorrmat_test.xlsx"))
write_omv(jjcorrmat_test, here("data", "jjcorrmat_test.omv"))

# Dataset 2: Biomarker panel
save(jjcorrmat_biomarker, file = here("data", "jjcorrmat_biomarker.rda"))
write.csv(jjcorrmat_biomarker, file = here("data", "jjcorrmat_biomarker.csv"), row.names = FALSE)
write_xlsx(jjcorrmat_biomarker, path = here("data", "jjcorrmat_biomarker.xlsx"))
write_omv(jjcorrmat_biomarker, here("data", "jjcorrmat_biomarker.omv"))

# Dataset 3: Laboratory values
save(jjcorrmat_labvalues, file = here("data", "jjcorrmat_labvalues.rda"))
write.csv(jjcorrmat_labvalues, file = here("data", "jjcorrmat_labvalues.csv"), row.names = FALSE)
write_xlsx(jjcorrmat_labvalues, path = here("data", "jjcorrmat_labvalues.xlsx"))
write_omv(jjcorrmat_labvalues, here("data", "jjcorrmat_labvalues.omv"))

# Dataset 4: Imaging metrics
save(jjcorrmat_imaging, file = here("data", "jjcorrmat_imaging.rda"))
write.csv(jjcorrmat_imaging, file = here("data", "jjcorrmat_imaging.csv"), row.names = FALSE)
write_xlsx(jjcorrmat_imaging, path = here("data", "jjcorrmat_imaging.xlsx"))
write_omv(jjcorrmat_imaging, here("data", "jjcorrmat_imaging.omv"))

# Dataset 5: Vital signs
save(jjcorrmat_vitals, file = here("data", "jjcorrmat_vitals.rda"))
write.csv(jjcorrmat_vitals, file = here("data", "jjcorrmat_vitals.csv"), row.names = FALSE)
write_xlsx(jjcorrmat_vitals, path = here("data", "jjcorrmat_vitals.xlsx"))
write_omv(jjcorrmat_vitals, here("data", "jjcorrmat_vitals.omv"))

# Dataset 6: Mixed correlations
save(jjcorrmat_mixed, file = here("data", "jjcorrmat_mixed.rda"))
write.csv(jjcorrmat_mixed, file = here("data", "jjcorrmat_mixed.csv"), row.names = FALSE)
write_xlsx(jjcorrmat_mixed, path = here("data", "jjcorrmat_mixed.xlsx"))
write_omv(jjcorrmat_mixed, here("data", "jjcorrmat_mixed.omv"))


# ═══════════════════════════════════════════════════════════
# Generate summary documentation
# ═══════════════════════════════════════════════════════════

summary_text <- "═══════════════════════════════════════════════════════════
JJCORRMAT TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: jjcorrmat (Correlation Matrix)
Generated: 2026-01-05
Seed: 42

DIMENSIONS
----------
Main Dataset (jjcorrmat_test):
  Observations: 150
  Variables: 6 continuous + 1 grouping
  Missing data: ~3% in ki67_index and necrosis_percent

Biomarker Panel (jjcorrmat_biomarker):
  Observations: 120
  Variables: 6 continuous + 1 grouping
  Distribution: Log-normal (right-skewed)
  Missing data: ~5% in afp

Laboratory Values (jjcorrmat_labvalues):
  Observations: 200
  Variables: 8 continuous + 1 grouping
  Missing data: ~2% in triglycerides and hdl

Imaging Metrics (jjcorrmat_imaging):
  Observations: 100
  Variables: 6 continuous + 1 grouping
  Missing data: ~3% in suv_max

Vital Signs (jjcorrmat_vitals):
  Observations: 180
  Variables: 6 continuous + 1 grouping
  Missing data: ~2% in oxygen_saturation

Mixed Correlations (jjcorrmat_mixed):
  Observations: 150
  Variables: 7 continuous + 1 grouping
  Missing data: ~4% in var_d and var_e

VARIABLE DESCRIPTIONS
---------------------

Main Dataset (Clinical Metrics):
  • tumor_size [numeric, ≥5]: Tumor size in millimeters
  • ki67_index [numeric, 0-100]: Ki-67 proliferation index (percentage)
  • mitotic_count [numeric, ≥0]: Mitotic count per high-power field
  • necrosis_percent [numeric, 0-100]: Necrosis percentage
  • age [numeric, 18-90]: Patient age in years
  • bmi [numeric, 15-45]: Body mass index
  • tumor_stage [factor]: Tumor stage (I, II, III, IV)

Biomarker Dataset:
  • cea [numeric, >0]: Carcinoembryonic antigen (ng/mL)
  • ca199 [numeric, >0]: Cancer antigen 19-9 (U/mL)
  • afp [numeric, >0]: Alpha-fetoprotein (ng/mL)
  • ldh [numeric, >0]: Lactate dehydrogenase (U/L)
  • crp [numeric, >0]: C-reactive protein (mg/L)
  • albumin [numeric, >0]: Serum albumin (g/dL)
  • cancer_type [factor]: Cancer type (Colorectal, Pancreatic, Hepatocellular)

Laboratory Values Dataset:
  • glucose [numeric, 60-300]: Fasting glucose (mg/dL)
  • cholesterol [numeric, 100-350]: Total cholesterol (mg/dL)
  • triglycerides [numeric, 30-400]: Triglycerides (mg/dL)
  • hdl [numeric, 20-100]: HDL cholesterol (mg/dL)
  • ldl [numeric, 50-250]: LDL cholesterol (mg/dL)
  • creatinine [numeric, 0.5-2.5]: Serum creatinine (mg/dL)
  • alt [numeric, 5-150]: Alanine aminotransferase (U/L)
  • ast [numeric, 5-150]: Aspartate aminotransferase (U/L)
  • risk_group [factor]: Risk group (Low, Medium, High)

Imaging Metrics Dataset:
  • tumor_volume [numeric, >0]: Tumor volume (cm³)
  • tumor_longest_diameter [numeric, >0]: Longest diameter (mm)
  • tumor_shortest_diameter [numeric, >0]: Shortest diameter (mm)
  • suv_max [numeric, >0]: Maximum standardized uptake value
  • suv_mean [numeric, >0]: Mean standardized uptake value
  • adc [numeric, >0]: Apparent diffusion coefficient (×10⁻³ mm²/s)
  • imaging_modality [factor]: Imaging type (CT, MRI, PET-CT)

Vital Signs Dataset:
  • systolic_bp [numeric, 90-180]: Systolic blood pressure (mmHg)
  • diastolic_bp [numeric, 50-110]: Diastolic blood pressure (mmHg)
  • heart_rate [numeric, 45-140]: Heart rate (beats per minute)
  • respiratory_rate [numeric, 10-30]: Respiratory rate (breaths per minute)
  • temperature [numeric, 35.5-39.5]: Body temperature (°C)
  • oxygen_saturation [numeric, 88-100]: Oxygen saturation (%)
  • patient_status [factor]: Patient status (Stable, Monitoring, Critical)

Mixed Correlations Dataset:
  • var_a [numeric]: Variable with very strong positive correlations
  • var_b [numeric]: Variable with strong positive correlations
  • var_c [numeric]: Variable with moderate positive correlations
  • var_d [numeric]: Variable with weak positive correlations
  • var_e [numeric]: Variable with near-zero correlations
  • var_f [numeric]: Variable with moderate negative correlations
  • var_g [numeric]: Variable with strong negative correlations
  • group [factor]: Grouping variable (Group1, Group2, Group3)

CORRELATION STRUCTURES
----------------------

Main Dataset:
  • Strong positive: ki67_index ↔ mitotic_count (r ≈ 0.85)
  • Strong positive: tumor_size ↔ ki67_index (r ≈ 0.65)
  • Moderate positive: tumor_size ↔ necrosis_percent (r ≈ 0.55)
  • Near zero: age ↔ bmi (r ≈ -0.05)

Biomarker Dataset:
  • Strong positive: CRP ↔ albumin (r ≈ -0.55, inverse inflammatory response)
  • Moderate positive: CEA ↔ CA19-9 (r ≈ 0.45)
  • Moderate positive: LDH ↔ CRP (r ≈ 0.60)

Laboratory Values:
  • Very strong: Cholesterol ↔ LDL (r ≈ 0.85)
  • Strong positive: Triglycerides ↔ LDL (r ≈ 0.80)
  • Strong negative: HDL ↔ LDL (r ≈ -0.60)
  • Strong positive: ALT ↔ AST (r ≈ 0.75, liver enzymes)

Imaging Metrics:
  • Very strong: Tumor volume ↔ longest diameter (r ≈ 0.92)
  • Very strong: SUV_max ↔ SUV_mean (r ≈ 0.90)
  • Strong negative: Tumor volume ↔ ADC (r ≈ -0.55)

Vital Signs:
  • Strong positive: Systolic ↔ diastolic BP (r ≈ 0.75)
  • Moderate positive: Heart rate ↔ respiratory rate (r ≈ 0.40)
  • Moderate negative: Temperature ↔ oxygen saturation (r ≈ -0.35)

Mixed Correlations:
  • Very strong positive: var_a ↔ var_b (r = 0.90)
  • Strong positive: var_a ↔ var_c (r = 0.70)
  • Strong negative: var_a ↔ var_g (r = -0.60)
  • Near zero: var_e ↔ others (r ≈ 0.05 to 0.10)

STATISTICAL APPROACHES COVERED
-------------------------------

Parametric (typestatistics = 'parametric'):
  • Pearson product-moment correlation
  • Assumes bivariate normality
  • Most powerful when assumptions met
  • Example: Lab values, vital signs

Nonparametric (typestatistics = 'nonparametric'):
  • Spearman's rank correlation
  • Distribution-free
  • Robust to outliers and non-normality
  • Example: Biomarker data (log-normal)

Robust (typestatistics = 'robust'):
  • Percentage bend correlation
  • Resistant to outliers
  • Compromise between power and robustness
  • Example: Clinical metrics with outliers

Bayesian (typestatistics = 'bayes'):
  • Bayes Factor for Pearson's r
  • Evidence-based inference
  • No p-value thresholds
  • Example: Any continuous correlations

MATRIX TYPES
------------

Upper Triangle (matrixtype = 'upper'):
  • Shows only upper diagonal
  • Reduces visual clutter
  • Standard presentation

Lower Triangle (matrixtype = 'lower'):
  • Shows only lower diagonal
  • Alternative presentation
  • Useful for comparison with adjusted p-values

Full Matrix (matrixtype = 'full'):
  • Shows complete symmetric matrix
  • Redundant but comprehensive
  • Useful for showing adjusted vs unadjusted p-values

SPECIAL FEATURES TESTED
-----------------------

Partial Correlations (partial = TRUE):
  • Controls for all other variables
  • Shows unique associations
  • Removes confounding
  • Example: Use with clinical metrics to control for age

Missing Data Handling:
  • Listwise deletion (naHandling = 'listwise'): Complete cases only
  • Pairwise deletion (naHandling = 'pairwise'): Use all available pairs
  • Different results with sparse missing data

Multiple Testing Correction:
  • None: No adjustment (default in many contexts)
  • Holm: Step-down procedure (recommended)
  • Bonferroni: Conservative family-wise error control
  • BH (FDR): False discovery rate control
  • Example: With 6 variables → 15 pairwise correlations

Grouped Analysis (grvar):
  • Separate matrices for each group
  • Tests correlation heterogeneity
  • Example: Tumor stage groups, cancer types, risk groups

EXAMPLE R CODE
--------------

# Load test data
data(jjcorrmat_test, package = 'ClinicoPath')

# 1. Basic correlation matrix (Pearson)
jjcorrmat(
  data = jjcorrmat_test,
  dep = c('tumor_size', 'ki67_index', 'mitotic_count',
          'necrosis_percent', 'age', 'bmi'),
  typestatistics = 'parametric'
)

# 2. With multiple testing correction
jjcorrmat(
  data = jjcorrmat_test,
  dep = c('tumor_size', 'ki67_index', 'mitotic_count', 'necrosis_percent'),
  typestatistics = 'parametric',
  padjustmethod = 'bonferroni',
  matrixtype = 'upper',
  matrixmethod = 'circle'
)

# 3. Grouped by tumor stage
jjcorrmat(
  data = jjcorrmat_test,
  dep = c('tumor_size', 'ki67_index', 'mitotic_count'),
  grvar = 'tumor_stage',
  typestatistics = 'parametric'
)

# 4. Biomarker correlations (nonparametric for skewed data)
data(jjcorrmat_biomarker)
jjcorrmat(
  data = jjcorrmat_biomarker,
  dep = c('cea', 'ca199', 'afp', 'ldh', 'crp', 'albumin'),
  typestatistics = 'nonparametric',
  matrixtype = 'lower',
  padjustmethod = 'holm'
)

# 5. Laboratory values with custom colors
data(jjcorrmat_labvalues)
jjcorrmat(
  data = jjcorrmat_labvalues,
  dep = c('glucose', 'cholesterol', 'triglycerides', 'hdl', 'ldl'),
  typestatistics = 'parametric',
  lowcolor = 'blue',
  midcolor = 'white',
  highcolor = 'red',
  title = 'Metabolic Panel Correlations'
)

# 6. Partial correlations (controlling for other variables)
jjcorrmat(
  data = jjcorrmat_test,
  dep = c('tumor_size', 'ki67_index', 'mitotic_count', 'age'),
  partial = TRUE,
  typestatistics = 'parametric'
)

# 7. Imaging metrics with strong correlations
data(jjcorrmat_imaging)
jjcorrmat(
  data = jjcorrmat_imaging,
  dep = c('tumor_volume', 'tumor_longest_diameter',
          'tumor_shortest_diameter', 'suv_max', 'suv_mean'),
  typestatistics = 'parametric',
  matrixmethod = 'circle',
  k = 3
)

# 8. Vital signs with robust correlation
data(jjcorrmat_vitals)
jjcorrmat(
  data = jjcorrmat_vitals,
  dep = c('systolic_bp', 'diastolic_bp', 'heart_rate',
          'respiratory_rate', 'temperature', 'oxygen_saturation'),
  typestatistics = 'robust',
  siglevel = 0.01
)

# 9. Bayesian correlation matrix
data(jjcorrmat_mixed)
jjcorrmat(
  data = jjcorrmat_mixed,
  dep = c('var_a', 'var_b', 'var_c', 'var_d', 'var_e'),
  typestatistics = 'bayes',
  matrixtype = 'full'
)

# 10. Pairwise deletion for missing data
jjcorrmat(
  data = jjcorrmat_test,
  dep = c('tumor_size', 'ki67_index', 'necrosis_percent', 'age'),
  naHandling = 'pairwise',
  typestatistics = 'parametric'
)

FILES GENERATED
---------------
  ✓ data/jjcorrmat_test.rda           (Clinical metrics)
  ✓ data/jjcorrmat_biomarker.rda      (Tumor markers, log-normal)
  ✓ data/jjcorrmat_labvalues.rda      (Clinical chemistry panel)
  ✓ data/jjcorrmat_imaging.rda        (Radiological measurements)
  ✓ data/jjcorrmat_vitals.rda         (Vital signs)
  ✓ data/jjcorrmat_mixed.rda          (Mixed correlation strengths)
  ✓ CSV, XLSX, OMV formats for all datasets

═══════════════════════════════════════════════════════════

✓ All test data files generated successfully!
✓ Summary saved to: JJCORRMAT_TEST_DATA_SUMMARY.md

✓ jjcorrmat test data generation complete!
"

cat(summary_text)
writeLines(summary_text, here("JJCORRMAT_TEST_DATA_SUMMARY.md"))

cat("\n✓ jjcorrmat test data generation complete!\n")
