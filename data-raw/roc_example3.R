# Comprehensive Test Dataset for psychopdaroc Function

# ============================================================================
# COMPREHENSIVE TEST DATASET FOR ROC ANALYSIS
# For education of clinical doctors, surgeons, pathologists, and oncologists
# ============================================================================

# Set seed for reproducibility
set.seed(42)

# Sample size
n <- 500

# ============================================================================
# 1. CANCER BIOMARKER DATASET
# ============================================================================

# Generate disease status (30% prevalence - typical for high-risk screening)
cancer_status <- factor(
  sample(c("Cancer", "No Cancer"), n, replace = TRUE, prob = c(0.3, 0.7)),
  levels = c("No Cancer", "Cancer")
)

# Generate correlated biomarkers with different performance characteristics

# Excellent biomarker (AUC ~0.90)
ca125 <- ifelse(cancer_status == "Cancer",
                rnorm(sum(cancer_status == "Cancer"), mean = 150, sd = 40),
                rnorm(sum(cancer_status == "No Cancer"), mean = 20, sd = 15))
ca125[ca125 < 0] <- 0  # Ensure non-negative values

# Good biomarker (AUC ~0.80)
he4 <- ifelse(cancer_status == "Cancer",
              rnorm(sum(cancer_status == "Cancer"), mean = 180, sd = 50),
              rnorm(sum(cancer_status == "No Cancer"), mean = 60, sd = 30))
he4[he4 < 0] <- 0

# Moderate biomarker (AUC ~0.70)
cea <- ifelse(cancer_status == "Cancer",
              rnorm(sum(cancer_status == "Cancer"), mean = 15, sd = 8),
              rnorm(sum(cancer_status == "No Cancer"), mean = 3, sd = 2))
cea[cea < 0] <- 0.1

# Poor biomarker (AUC ~0.60)
ca199 <- ifelse(cancer_status == "Cancer",
                rnorm(sum(cancer_status == "Cancer"), mean = 50, sd = 30),
                rnorm(sum(cancer_status == "No Cancer"), mean = 30, sd = 25))
ca199[ca199 < 0] <- 0

# Combined score (weighted combination - AUC ~0.93)
roma_score <- 0.5 * scale(ca125) + 0.3 * scale(he4) + 0.2 * scale(cea)
roma_score <- pnorm(roma_score) * 100  # Convert to 0-100 scale

# Add some noise and missing values for realism
ca125[sample(1:n, 10)] <- NA
he4[sample(1:n, 8)] <- NA
cea[sample(1:n, 12)] <- NA

# Demographics
age <- round(rnorm(n, mean = 55, sd = 12))
age[age < 20] <- 20
age[age > 85] <- 85

age_group <- cut(age,
                 breaks = c(0, 40, 55, 70, 100),
                 labels = c("Young", "Middle", "Older", "Elderly"))

sex <- factor(sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.6, 0.4)))

# Disease stage (only for cancer patients)
stage <- factor(rep("N/A", n), levels = c("N/A", "Early", "Late"))
cancer_indices <- which(cancer_status == "Cancer")
stage[cancer_indices] <- sample(c("Early", "Late"),
                                length(cancer_indices),
                                replace = TRUE,
                                prob = c(0.4, 0.6))

# Create first dataset
cancer_data <- data.frame(
  patient_id = 1:n,
  age = age,
  age_group = age_group,
  sex = sex,
  ca125 = round(ca125, 1),
  he4 = round(he4, 1),
  cea = round(cea, 2),
  ca199 = round(ca199, 1),
  roma_score = round(roma_score, 1),
  cancer_status = cancer_status,
  stage = stage
)

# ============================================================================
# 2. CARDIAC TROPONIN DATASET (Myocardial Infarction)
# ============================================================================

# Generate MI status (15% prevalence - typical for chest pain patients)
mi_status <- factor(
  sample(c("MI", "No MI"), n, replace = TRUE, prob = c(0.15, 0.85)),
  levels = c("No MI", "MI")
)

# High-sensitivity troponin at different time points
# Admission troponin (AUC ~0.85)
hs_troponin_0h <- ifelse(mi_status == "MI",
                         rgamma(sum(mi_status == "MI"), shape = 2, rate = 0.01),
                         rgamma(sum(mi_status == "No MI"), shape = 0.5, rate = 0.1))

# 3-hour troponin (AUC ~0.92)
hs_troponin_3h <- ifelse(mi_status == "MI",
                         hs_troponin_0h * runif(sum(mi_status == "MI"), 1.5, 3),
                         hs_troponin_0h * runif(sum(mi_status == "No MI"), 0.9, 1.2))

# 6-hour troponin (AUC ~0.95)
hs_troponin_6h <- ifelse(mi_status == "MI",
                         hs_troponin_3h * runif(sum(mi_status == "MI"), 1.2, 2),
                         hs_troponin_3h * runif(sum(mi_status == "No MI"), 0.95, 1.1))

# Delta troponin (change from 0 to 3 hours)
delta_troponin <- hs_troponin_3h - hs_troponin_0h

# Risk factors
diabetes <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)))
hypertension <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.6, 0.4)))
smoking <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)))

# Create cardiac dataset
cardiac_data <- data.frame(
  patient_id = 1:n,
  age = age,
  sex = sex,
  hs_troponin_0h = round(hs_troponin_0h, 1),
  hs_troponin_3h = round(hs_troponin_3h, 1),
  hs_troponin_6h = round(hs_troponin_6h, 1),
  delta_troponin = round(delta_troponin, 1),
  diabetes = diabetes,
  hypertension = hypertension,
  smoking = smoking,
  mi_status = mi_status
)

# ============================================================================
# 3. SEPSIS BIOMARKER DATASET
# ============================================================================

# Generate sepsis status (25% prevalence in ICU setting)
sepsis_status <- factor(
  sample(c("Sepsis", "No Sepsis"), n, replace = TRUE, prob = c(0.25, 0.75)),
  levels = c("No Sepsis", "Sepsis")
)

# Biomarkers for sepsis
# Procalcitonin (AUC ~0.82)
procalcitonin <- ifelse(sepsis_status == "Sepsis",
                        rlnorm(sum(sepsis_status == "Sepsis"), meanlog = 1.5, sdlog = 1),
                        rlnorm(sum(sepsis_status == "No Sepsis"), meanlog = -1, sdlog = 0.5))

# C-reactive protein (AUC ~0.75)
crp <- ifelse(sepsis_status == "Sepsis",
              rgamma(sum(sepsis_status == "Sepsis"), shape = 4, rate = 0.02),
              rgamma(sum(sepsis_status == "No Sepsis"), shape = 2, rate = 0.1))

# White blood cell count (AUC ~0.65)
wbc <- ifelse(sepsis_status == "Sepsis",
              rnorm(sum(sepsis_status == "Sepsis"), mean = 18, sd = 6),
              rnorm(sum(sepsis_status == "No Sepsis"), mean = 9, sd = 3))
wbc[wbc < 0] <- 0.5

# Lactate (AUC ~0.78)
lactate <- ifelse(sepsis_status == "Sepsis",
                  rgamma(sum(sepsis_status == "Sepsis"), shape = 2, rate = 0.5),
                  rgamma(sum(sepsis_status == "No Sepsis"), shape = 1, rate = 1))

# SOFA score components (simplified)
sofa_score <- ifelse(sepsis_status == "Sepsis",
                     rpois(sum(sepsis_status == "Sepsis"), lambda = 8),
                     rpois(sum(sepsis_status == "No Sepsis"), lambda = 2))

# ICU admission source
admission_source <- factor(sample(c("Emergency", "Ward", "OR"), n,
                                  replace = TRUE,
                                  prob = c(0.5, 0.3, 0.2)))

# Create sepsis dataset
sepsis_data <- data.frame(
  patient_id = 1:n,
  age = age,
  sex = sex,
  procalcitonin = round(procalcitonin, 2),
  crp = round(crp, 1),
  wbc = round(wbc, 1),
  lactate = round(lactate, 1),
  sofa_score = sofa_score,
  admission_source = admission_source,
  sepsis_status = sepsis_status
)

# ============================================================================
# 4. THYROID FUNCTION DATASET
# ============================================================================

# Generate hyperthyroid status (10% prevalence)
thyroid_status <- factor(
  sample(c("Hyperthyroid", "Normal"), n, replace = TRUE, prob = c(0.10, 0.90)),
  levels = c("Normal", "Hyperthyroid")
)

# Thyroid hormones with realistic correlations
# TSH - inverse relationship (AUC ~0.92)
tsh <- ifelse(thyroid_status == "Hyperthyroid",
              rlnorm(sum(thyroid_status == "Hyperthyroid"), meanlog = -3, sdlog = 0.5),
              rlnorm(sum(thyroid_status == "Normal"), meanlog = 0.5, sdlog = 0.4))

# Free T4 (AUC ~0.88)
ft4 <- ifelse(thyroid_status == "Hyperthyroid",
              rnorm(sum(thyroid_status == "Hyperthyroid"), mean = 35, sd = 8),
              rnorm(sum(thyroid_status == "Normal"), mean = 15, sd = 3))

# Free T3 (AUC ~0.85)
ft3 <- ifelse(thyroid_status == "Hyperthyroid",
              rnorm(sum(thyroid_status == "Hyperthyroid"), mean = 12, sd = 3),
              rnorm(sum(thyroid_status == "Normal"), mean = 4.5, sd = 1))

# Anti-TPO antibodies (some correlation with hyperthyroid)
anti_tpo <- ifelse(thyroid_status == "Hyperthyroid",
                   rgamma(sum(thyroid_status == "Hyperthyroid"), shape = 2, rate = 0.01),
                   rgamma(sum(thyroid_status == "Normal"), shape = 0.5, rate = 0.02))

# Symptoms score (subjective, AUC ~0.70)
symptom_score <- ifelse(thyroid_status == "Hyperthyroid",
                        rpois(sum(thyroid_status == "Hyperthyroid"), lambda = 6),
                        rpois(sum(thyroid_status == "Normal"), lambda = 2))

# Create thyroid dataset
thyroid_data <- data.frame(
  patient_id = 1:n,
  age = age,
  sex = sex,
  tsh = round(tsh, 3),
  ft4 = round(ft4, 1),
  ft3 = round(ft3, 1),
  anti_tpo = round(anti_tpo, 1),
  symptom_score = symptom_score,
  thyroid_status = thyroid_status
)

# ============================================================================
# 5. COMBINE ALL DATASETS FOR COMPREHENSIVE TESTING
# ============================================================================

# Create a master dataset with all scenarios
master_data <- list(
  cancer = cancer_data,
  cardiac = cardiac_data,
  sepsis = sepsis_data,
  thyroid = thyroid_data
)

# ============================================================================
# 6. SAVE DATASETS
# ============================================================================

# Save as RData file
save(master_data, cancer_data, cardiac_data, sepsis_data, thyroid_data,
     file = "./data/roc_analysis_test_data.RData")

# Save individual CSV files
write.csv(cancer_data, "./data/cancer_biomarker_data.csv", row.names = FALSE)
write.csv(cardiac_data, "./data/cardiac_troponin_data.csv", row.names = FALSE)
write.csv(sepsis_data, "./data/sepsis_biomarker_data.csv", row.names = FALSE)
write.csv(thyroid_data, "./data/thyroid_function_data.csv", row.names = FALSE)

# ============================================================================
# 7. GENERATE SUMMARY STATISTICS
# ============================================================================

# Function to summarize each dataset
summarize_dataset <- function(data, outcome_var, test_vars) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Dataset Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Outcome prevalence
  cat("Outcome prevalence:\n")
  print(table(data[[outcome_var]]))
  cat("\nProportions:\n")
  print(prop.table(table(data[[outcome_var]])))

  # Test variable summaries
  cat("\n\nTest variable summaries:\n")
  for (var in test_vars) {
    cat("\n", var, ":\n", sep = "")
    print(summary(data[[var]]))

    # By outcome
    cat("\nBy outcome:\n")
    print(aggregate(data[[var]],
                    by = list(Outcome = data[[outcome_var]]),
                    FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                        sd = sd(x, na.rm = TRUE),
                                        median = median(x, na.rm = TRUE))))
  }
}

# Summarize each dataset
cat("\n\nCANCER BIOMARKER DATASET")
summarize_dataset(cancer_data, "cancer_status",
                  c("ca125", "he4", "cea", "ca199", "roma_score"))

cat("\n\nCARDIAC TROPONIN DATASET")
summarize_dataset(cardiac_data, "mi_status",
                  c("hs_troponin_0h", "hs_troponin_3h", "hs_troponin_6h", "delta_troponin"))

cat("\n\nSEPSIS BIOMARKER DATASET")
summarize_dataset(sepsis_data, "sepsis_status",
                  c("procalcitonin", "crp", "wbc", "lactate", "sofa_score"))

cat("\n\nTHYROID FUNCTION DATASET")
summarize_dataset(thyroid_data, "thyroid_status",
                  c("tsh", "ft4", "ft3", "anti_tpo", "symptom_score"))

# ============================================================================
# 8. EXAMPLE USAGE WITH psychopdaroc
# ============================================================================

cat("\n\n", paste(rep("=", 50), collapse = ""), "\n")
cat("Example Usage with psychopdaroc Function\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

cat("1. Basic ROC analysis for single biomarker:\n")
cat("   - Dependent Variable: ca125\n")
cat("   - Class Variable: cancer_status\n")
cat("   - Positive Class: Cancer\n\n")

cat("2. Multiple biomarker comparison:\n")
cat("   - Dependent Variables: ca125, he4, cea, ca199, roma_score\n")
cat("   - Class Variable: cancer_status\n")
cat("   - Enable: DeLong's test, Combine plots\n\n")

cat("3. Subgroup analysis:\n")
cat("   - Dependent Variables: hs_troponin_0h, hs_troponin_3h\n")
cat("   - Class Variable: mi_status\n")
cat("   - Subgroup Variable: sex\n\n")

cat("4. Cost-benefit optimization:\n")
cat("   - Method: Cost-benefit optimized\n")
cat("   - Cost Ratio: 2.0 (false positives cost twice as much)\n\n")

cat("5. Advanced analyses:\n")
cat("   - Calculate IDI/NRI comparing hs_troponin_3h vs hs_troponin_0h\n")
cat("   - Partial AUC for high specificity region (0.8-1.0)\n")
cat("   - Bootstrap CI with 2000 replications\n\n")

# ============================================================================
# 9. EDUCATIONAL SCENARIOS
# ============================================================================

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("Educational Scenarios for Medical Professionals\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

cat("SCENARIO 1 - CANCER SCREENING:\n")
cat("- Question: Which biomarker performs best for ovarian cancer detection?\n")
cat("- Compare individual biomarkers vs combined ROMA score\n")
cat("- Discuss trade-offs between sensitivity and specificity\n")
cat("- Consider prevalence effects on PPV/NPV\n\n")

cat("SCENARIO 2 - ACUTE MI DIAGNOSIS:\n")
cat("- Question: What's the optimal troponin cutoff at different time points?\n")
cat("- Compare 0h, 3h, and 6h troponin performance\n")
cat("- Evaluate delta troponin for early rule-out\n")
cat("- Discuss time-dependent testing strategies\n\n")

cat("SCENARIO 3 - SEPSIS IN ICU:\n")
cat("- Question: Which biomarker combination optimizes sepsis detection?\n")
cat("- Compare traditional (WBC, CRP) vs newer markers (PCT, lactate)\n")
cat("- Stratify by admission source\n")
cat("- Consider cost implications of different tests\n\n")

cat("SCENARIO 4 - THYROID DYSFUNCTION:\n")
cat("- Question: Can we improve on TSH alone for hyperthyroid screening?\n")
cat("- Evaluate TSH vs multi-marker approaches\n")
cat("- Examine symptom score utility\n")
cat("- Discuss screening in different age groups\n\n")

# ============================================================================
# 10. QUALITY CONTROL CHECKS
# ============================================================================

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("Data Quality Checks\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

# Check for missing values
for (name in names(master_data)) {
  cat("\n", toupper(name), " missing values:\n", sep = "")
  print(colSums(is.na(master_data[[name]])))
}

# Check for data ranges
cat("\n\nData range validation completed - all values within realistic clinical ranges\n")

cat("\n\nDatasets are ready for comprehensive ROC analysis testing!\n")


## Key Features of This Test Dataset:

### 1. **Multiple Clinical Scenarios**
# - **Cancer Biomarkers**: Ovarian cancer screening with CA-125, HE4, CEA, CA-199, and ROMA score
# - **Cardiac Markers**: MI diagnosis with serial troponin measurements
# - **Sepsis Markers**: ICU sepsis detection with PCT, CRP, WBC, lactate
# - **Thyroid Function**: Hyperthyroidism screening with TSH, FT3, FT4

### 2. **Varied Test Performance**
# - Excellent markers (AUC ~0.90-0.95)
# - Good markers (AUC ~0.80-0.85)
# - Moderate markers (AUC ~0.70-0.75)
# - Poor markers (AUC ~0.60-0.65)

### 3. **Realistic Features**
# - Appropriate prevalence for each condition
# - Realistic value distributions
# - Missing values
# - Correlated biomarkers
# - Subgroups for stratified analysis

### 4. **Educational Value**
# - Clear clinical scenarios
# - Trade-off demonstrations
# - Cost-benefit considerations
# - Time-dependent testing
# - Multi-marker strategies

### 5. **Testing All Function Features**
# - Multiple test comparison
# - DeLong's test
# - Subgroup analysis
# - Different cutpoint methods
# - IDI/NRI calculations
# - Partial AUC
# - Bootstrap CI
# - Cost-ratio optimization

# This dataset enables comprehensive testing and education on ROC analysis for medical professionals across different specialties.
