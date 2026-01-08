# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjhistostats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjhistostats jamovi function
# which creates histograms with statistical annotations for continuous variables.
#
# Generated: 2026-01-05
# Seed: 42
# Test data: jjhistostats_test, jjhistostats_labvalues, jjhistostats_skewed,
#            jjhistostats_bimodal, jjhistostats_pathology, jjhistostats_grouped,
#            jjhistostats_small, jjhistostats_uniform

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: General Clinical Data (jjhistostats_test)
# ═══════════════════════════════════════════════════════════
# Multiple continuous variables with realistic clinical distributions

n_test <- 150

jjhistostats_test <- tibble(
  patient_id = 1:n_test,

  # Age (years) - approximately normal
  age_years = pmax(18, pmin(90, rnorm(n_test, mean = 62, sd = 12))),

  # Tumor size (mm) - slightly right-skewed
  tumor_size_mm = pmax(5, rnorm(n_test, mean = 35, sd = 18) * 1.2),

  # BMI (kg/m²) - approximately normal
  bmi = pmax(15, pmin(45, rnorm(n_test, mean = 26.5, sd = 4.5))),

  # PSA level (ng/mL) - log-normal (right-skewed)
  psa_level = exp(rnorm(n_test, mean = log(8), sd = 0.8)),

  # Hemoglobin (g/dL) - normal distribution
  hemoglobin = rnorm(n_test, mean = 13.5, sd = 1.8),

  # Treatment group
  treatment = factor(sample(c("Control", "Treatment A", "Treatment B"),
                            n_test, replace = TRUE)),

  # Disease stage
  disease_stage = factor(sample(c("Early", "Intermediate", "Advanced"),
                                n_test, replace = TRUE,
                                prob = c(0.3, 0.5, 0.2)),
                         levels = c("Early", "Intermediate", "Advanced")),

  # Hospital site
  hospital = factor(sample(c("Site A", "Site B", "Site C"),
                           n_test, replace = TRUE))
)

# Ensure reasonable ranges
jjhistostats_test$tumor_size_mm <- pmin(120, pmax(5, jjhistostats_test$tumor_size_mm))
jjhistostats_test$psa_level <- pmin(100, pmax(0.1, jjhistostats_test$psa_level))

# Add ~3% missing data
n_missing <- round(n_test * 0.03)
jjhistostats_test$psa_level[sample(n_test, n_missing)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 2: Lab Values (jjhistostats_labvalues)
# ═══════════════════════════════════════════════════════════
# Normally distributed lab measurements

n_lab <- 120

jjhistostats_labvalues <- tibble(
  patient_id = 1:n_lab,

  # Glucose (mg/dL) - normal distribution
  glucose = rnorm(n_lab, mean = 95, sd = 12),

  # Cholesterol (mg/dL) - normal distribution
  cholesterol = rnorm(n_lab, mean = 200, sd = 35),

  # Triglycerides (mg/dL) - slightly right-skewed
  triglycerides = pmax(40, rnorm(n_lab, mean = 150, sd = 60)),

  # Creatinine (mg/dL) - normal distribution
  creatinine = pmax(0.5, rnorm(n_lab, mean = 1.0, sd = 0.25)),

  # ALT (U/L) - slightly right-skewed
  alt = pmax(10, rnorm(n_lab, mean = 30, sd = 15)),

  # AST (U/L) - slightly right-skewed
  ast = pmax(10, rnorm(n_lab, mean = 28, sd = 12)),

  # Patient group
  patient_group = factor(sample(c("Healthy", "At Risk", "Disease"),
                                n_lab, replace = TRUE,
                                prob = c(0.4, 0.4, 0.2)),
                        levels = c("Healthy", "At Risk", "Disease"))
)


# ═══════════════════════════════════════════════════════════
# Dataset 3: Skewed Biomarker Data (jjhistostats_skewed)
# ═══════════════════════════════════════════════════════════
# Right-skewed distributions (log-normal)

n_skew <- 100

jjhistostats_skewed <- tibble(
  patient_id = 1:n_skew,

  # CEA (ng/mL) - strongly right-skewed
  cea = exp(rnorm(n_skew, mean = log(5), sd = 1.2)),

  # CA 19-9 (U/mL) - strongly right-skewed
  ca199 = exp(rnorm(n_skew, mean = log(20), sd = 1.5)),

  # AFP (ng/mL) - strongly right-skewed
  afp = exp(rnorm(n_skew, mean = log(8), sd = 1.0)),

  # CRP (mg/L) - right-skewed
  crp = exp(rnorm(n_skew, mean = log(3), sd = 0.8)),

  # Cancer type
  cancer_type = factor(sample(c("Type A", "Type B", "Type C"),
                              n_skew, replace = TRUE))
)

# Cap extreme values
jjhistostats_skewed$cea <- pmin(200, jjhistostats_skewed$cea)
jjhistostats_skewed$ca199 <- pmin(500, jjhistostats_skewed$ca199)
jjhistostats_skewed$afp <- pmin(150, jjhistostats_skewed$afp)
jjhistostats_skewed$crp <- pmin(50, jjhistostats_skewed$crp)


# ═══════════════════════════════════════════════════════════
# Dataset 4: Bimodal Distribution (jjhistostats_bimodal)
# ═══════════════════════════════════════════════════════════
# Two distinct populations creating bimodal distributions

n_bimodal <- 140

# Create two populations
pop1_size <- round(n_bimodal * 0.45)  # 45%
pop2_size <- n_bimodal - pop1_size     # 55%

jjhistostats_bimodal <- tibble(
  patient_id = 1:n_bimodal,

  # Age - bimodal (younger and older patients)
  age_bimodal = c(
    rnorm(pop1_size, mean = 45, sd = 8),
    rnorm(pop2_size, mean = 70, sd = 8)
  ),

  # Tumor burden - two distinct groups
  tumor_burden = c(
    rnorm(pop1_size, mean = 20, sd = 5),
    rnorm(pop2_size, mean = 60, sd = 10)
  ),

  # Response score - responders vs non-responders
  response_score = c(
    rnorm(pop1_size, mean = 30, sd = 10),
    rnorm(pop2_size, mean = 70, sd = 12)
  ),

  # Population indicator
  population = factor(c(
    rep("Population A", pop1_size),
    rep("Population B", pop2_size)
  )),

  # Gender
  gender = factor(sample(c("Male", "Female"), n_bimodal, replace = TRUE))
)

# Ensure reasonable ranges
jjhistostats_bimodal$age_bimodal <- pmin(90, pmax(18, jjhistostats_bimodal$age_bimodal))
jjhistostats_bimodal$tumor_burden <- pmin(100, pmax(0, jjhistostats_bimodal$tumor_burden))
jjhistostats_bimodal$response_score <- pmin(100, pmax(0, jjhistostats_bimodal$response_score))


# ═══════════════════════════════════════════════════════════
# Dataset 5: Pathology Scores (jjhistostats_pathology)
# ═══════════════════════════════════════════════════════════
# Pathology measurements and scores

n_path <- 130

jjhistostats_pathology <- tibble(
  patient_id = 1:n_path,

  # Ki-67 index (%) - right-skewed
  ki67_index = pmin(95, pmax(1, abs(rnorm(n_path, mean = 25, sd = 18)))),

  # Mitotic count (per 10 HPF) - right-skewed, discrete
  mitotic_count = pmax(0, round(abs(rnorm(n_path, mean = 8, sd = 6)))),

  # Tumor cellularity (%) - approximately normal
  tumor_cellularity = pmin(95, pmax(10, rnorm(n_path, mean = 60, sd = 20))),

  # Necrosis percentage (%) - right-skewed
  necrosis_percent = pmin(80, pmax(0, abs(rnorm(n_path, mean = 15, sd = 15)))),

  # Nuclear grade (1-3 but treated as continuous for histogram)
  nuclear_grade_continuous = sample(c(1, 1.5, 2, 2.5, 3), n_path, replace = TRUE,
                                    prob = c(0.3, 0.2, 0.3, 0.1, 0.1)),

  # Tumor grade
  tumor_grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"),
                              n_path, replace = TRUE,
                              prob = c(0.3, 0.5, 0.2)),
                      levels = c("Grade 1", "Grade 2", "Grade 3")),

  # Organ
  organ = factor(sample(c("Lung", "Breast", "Colon"), n_path, replace = TRUE))
)


# ═══════════════════════════════════════════════════════════
# Dataset 6: Grouped Analysis (jjhistostats_grouped)
# ═══════════════════════════════════════════════════════════
# Data optimized for grouped/stratified histogram analysis

n_grouped <- 180

jjhistostats_grouped <- tibble(
  patient_id = 1:n_grouped,

  # Age varies by treatment
  age_years = c(
    rnorm(60, mean = 58, sd = 10),  # Control
    rnorm(60, mean = 62, sd = 11),  # Treatment A
    rnorm(60, mean = 65, sd = 9)    # Treatment B
  ),

  # Biomarker level varies by disease stage
  biomarker_level = c(
    rnorm(54, mean = 20, sd = 8),   # Early
    rnorm(72, mean = 45, sd = 12),  # Intermediate
    rnorm(54, mean = 75, sd = 15)   # Advanced
  ),

  # Response varies by gender
  response_value = c(
    rnorm(90, mean = 55, sd = 18),  # Male
    rnorm(90, mean = 48, sd = 16)   # Female
  ),

  # Treatment group
  treatment = factor(rep(c("Control", "Treatment A", "Treatment B"), each = 60),
                    levels = c("Control", "Treatment A", "Treatment B")),

  # Disease stage
  disease_stage = factor(rep(c("Early", "Intermediate", "Advanced"),
                            times = c(54, 72, 54)),
                        levels = c("Early", "Intermediate", "Advanced")),

  # Gender
  gender = factor(rep(c("Male", "Female"), each = 90))
)

# Ensure reasonable ranges
jjhistostats_grouped$age_years <- pmin(85, pmax(25, jjhistostats_grouped$age_years))
jjhistostats_grouped$biomarker_level <- pmin(120, pmax(5, jjhistostats_grouped$biomarker_level))
jjhistostats_grouped$response_value <- pmin(100, pmax(0, jjhistostats_grouped$response_value))


# ═══════════════════════════════════════════════════════════
# Dataset 7: Small Sample Size (jjhistostats_small)
# ═══════════════════════════════════════════════════════════
# Small dataset for testing edge cases

n_small <- 25

jjhistostats_small <- tibble(
  patient_id = 1:n_small,

  # Age
  age = round(rnorm(n_small, mean = 60, sd = 15)),

  # Measurement
  measurement = rnorm(n_small, mean = 50, sd = 12),

  # Score
  score = pmax(0, pmin(100, rnorm(n_small, mean = 55, sd = 20))),

  # Group
  group = factor(sample(c("Group A", "Group B"), n_small, replace = TRUE))
)

# Ensure reasonable age range
jjhistostats_small$age <- pmin(90, pmax(20, jjhistostats_small$age))


# ═══════════════════════════════════════════════════════════
# Dataset 8: Nearly Uniform Distribution (jjhistostats_uniform)
# ═══════════════════════════════════════════════════════════
# Testing with uniform-like distributions

n_uniform <- 100

jjhistostats_uniform <- tibble(
  patient_id = 1:n_uniform,

  # Uniform distribution (0-100)
  uniform_score = runif(n_uniform, min = 0, max = 100),

  # Slightly noisy uniform
  noisy_uniform = runif(n_uniform, min = 20, max = 80) + rnorm(n_uniform, 0, 5),

  # Discrete uniform (ordinal scale 1-10)
  ordinal_scale = sample(1:10, n_uniform, replace = TRUE),

  # Category
  category = factor(sample(c("Category 1", "Category 2", "Category 3"),
                          n_uniform, replace = TRUE))
)

# Ensure bounds
jjhistostats_uniform$noisy_uniform <- pmin(100, pmax(0, jjhistostats_uniform$noisy_uniform))


# ═══════════════════════════════════════════════════════════
# Save All Datasets in Multiple Formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  jjhistostats_test = jjhistostats_test,
  jjhistostats_labvalues = jjhistostats_labvalues,
  jjhistostats_skewed = jjhistostats_skewed,
  jjhistostats_bimodal = jjhistostats_bimodal,
  jjhistostats_pathology = jjhistostats_pathology,
  jjhistostats_grouped = jjhistostats_grouped,
  jjhistostats_small = jjhistostats_small,
  jjhistostats_uniform = jjhistostats_uniform
)

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]

  # 1. RDA format
  assign(dataset_name, dataset)
  save(list = dataset_name,
       file = here::here("data", paste0(dataset_name, ".rda")))

  # 2. CSV format
  write.csv(dataset,
            file = here::here("data", paste0(dataset_name, ".csv")),
            row.names = FALSE)

  # 3. Excel format
  write_xlsx(dataset,
             path = here::here("data", paste0(dataset_name, ".xlsx")))

  # 4. Jamovi format (OMV)
  write_omv(dataset,
            here::here("data", paste0(dataset_name, ".omv")))

  cat("✓ Generated", dataset_name, "in all formats\n")
}


# ═══════════════════════════════════════════════════════════
# Generate Summary Documentation
# ═══════════════════════════════════════════════════════════

summary_doc <- "
# Test Data Summary: jjhistostats

Generated: 2026-01-05
Seed: 42
Total datasets: 8

## Dataset 1: jjhistostats_test (n=150)
General clinical data with multiple continuous variables

Variables:
- age_years: Age in years (approximately normal, mean=62, sd=12)
- tumor_size_mm: Tumor size in mm (slightly right-skewed, mean=35, sd=18)
- bmi: Body mass index (normal, mean=26.5, sd=4.5)
- psa_level: PSA level in ng/mL (log-normal, right-skewed)
- hemoglobin: Hemoglobin in g/dL (normal, mean=13.5, sd=1.8)
- treatment: Treatment group (Control, Treatment A, Treatment B)
- disease_stage: Disease stage (Early, Intermediate, Advanced)
- hospital: Hospital site (Site A, Site B, Site C)

Missing data: ~3% in psa_level

Use for: General histogram exploration, normality testing, multiple variables


## Dataset 2: jjhistostats_labvalues (n=120)
Normally distributed laboratory measurements

Variables:
- glucose: Glucose in mg/dL (normal, mean=95, sd=12)
- cholesterol: Cholesterol in mg/dL (normal, mean=200, sd=35)
- triglycerides: Triglycerides in mg/dL (slightly right-skewed)
- creatinine: Creatinine in mg/dL (normal, mean=1.0, sd=0.25)
- alt: ALT in U/L (slightly right-skewed)
- ast: AST in U/L (slightly right-skewed)
- patient_group: Patient group (Healthy, At Risk, Disease)

Use for: Parametric tests, lab values preset, reference value testing


## Dataset 3: jjhistostats_skewed (n=100)
Strongly right-skewed biomarker data (log-normal distributions)

Variables:
- cea: CEA in ng/mL (log-normal, strongly right-skewed)
- ca199: CA 19-9 in U/mL (log-normal, strongly right-skewed)
- afp: AFP in ng/mL (log-normal, right-skewed)
- crp: CRP in mg/L (log-normal, right-skewed)
- cancer_type: Cancer type (Type A, Type B, Type C)

Use for: Nonparametric tests, testing skewness detection, biomarker preset


## Dataset 4: jjhistostats_bimodal (n=140)
Bimodal distributions from two distinct populations

Variables:
- age_bimodal: Age with two peaks (45 and 70 years)
- tumor_burden: Tumor burden with two modes (20 and 60)
- response_score: Response with bimodal pattern (30 and 70)
- population: Population indicator (A or B)
- gender: Gender (Male, Female)

Use for: Testing distribution shape detection, bimodality


## Dataset 5: jjhistostats_pathology (n=130)
Pathology measurements and scores

Variables:
- ki67_index: Ki-67 proliferation index (%, right-skewed)
- mitotic_count: Mitotic count per 10 HPF (discrete, right-skewed)
- tumor_cellularity: Tumor cellularity (%, approximately normal)
- necrosis_percent: Necrosis percentage (%, right-skewed)
- nuclear_grade_continuous: Nuclear grade as continuous (1-3)
- tumor_grade: Tumor grade (Grade 1, Grade 2, Grade 3)
- organ: Organ (Lung, Breast, Colon)

Use for: Pathology scores preset, discrete distributions


## Dataset 6: jjhistostats_grouped (n=180)
Data optimized for grouped/stratified histogram analysis

Variables:
- age_years: Age varying by treatment (means: 58, 62, 65)
- biomarker_level: Biomarker varying by stage (means: 20, 45, 75)
- response_value: Response varying by gender (means: 55, 48)
- treatment: Treatment group (Control, Treatment A, Treatment B)
- disease_stage: Disease stage (Early, Intermediate, Advanced)
- gender: Gender (Male, Female)

Use for: Grouped histogram analysis, stratification testing


## Dataset 7: jjhistostats_small (n=25)
Small sample size for edge case testing

Variables:
- age: Age in years
- measurement: Generic measurement
- score: Score (0-100)
- group: Group (Group A, Group B)

Use for: Small sample size testing, edge cases


## Dataset 8: jjhistostats_uniform (n=100)
Nearly uniform distributions

Variables:
- uniform_score: Pure uniform (0-100)
- noisy_uniform: Uniform with noise (20-80)
- ordinal_scale: Discrete uniform (1-10)
- category: Category (Category 1, Category 2, Category 3)

Use for: Testing non-normal distributions, uniform detection


## File Formats

Each dataset is available in 4 formats:
- .rda: Native R format (fastest loading)
- .csv: Universal text format
- .xlsx: Excel format
- .omv: Jamovi native format

## Usage Example

```r
# Load package
library(ClinicoPath)

# Load test data
data(jjhistostats_test)

# Basic histogram
jjhistostats(
  data = jjhistostats_test,
  dep = \"age_years\",
  typestatistics = \"parametric\",
  centralityline = TRUE,
  resultssubtitle = TRUE
)

# Multiple variables
jjhistostats(
  data = jjhistostats_test,
  dep = c(\"age_years\", \"tumor_size_mm\", \"bmi\"),
  centralityline = TRUE
)

# Grouped analysis
jjhistostats(
  data = jjhistostats_grouped,
  dep = \"biomarker_level\",
  grvar = \"disease_stage\",
  typestatistics = \"nonparametric\"
)

# Skewed biomarker data
jjhistostats(
  data = jjhistostats_skewed,
  dep = \"cea\",
  typestatistics = \"nonparametric\",
  centralityline = TRUE,
  resultssubtitle = TRUE
)
```

## Statistical Approaches by Dataset

- **jjhistostats_test**: Parametric for age_years, bmi, hemoglobin; Nonparametric for psa_level, tumor_size_mm
- **jjhistostats_labvalues**: Parametric (all normally distributed)
- **jjhistostats_skewed**: Nonparametric or Robust (all strongly skewed)
- **jjhistostats_bimodal**: Special case (bimodal - both parametric and nonparametric tests may be inappropriate)
- **jjhistostats_pathology**: Mixed (tumor_cellularity parametric; ki67, mitotic_count nonparametric)
- **jjhistostats_grouped**: Depends on variable (use grouped analysis)
- **jjhistostats_small**: Any (for edge case testing)
- **jjhistostats_uniform**: Nonparametric (uniform distribution)
"

cat(summary_doc)
writeLines(summary_doc, here::here("JJHISTOSTATS_TEST_DATA_SUMMARY.md"))

cat("\n✓ All datasets generated successfully!\n")
cat("✓ Summary documentation created: JJHISTOSTATS_TEST_DATA_SUMMARY.md\n")
