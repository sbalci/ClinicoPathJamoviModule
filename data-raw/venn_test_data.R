# ═══════════════════════════════════════════════════════════
# Test Data Generation: venn
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the venn jamovi function
# (Venn Diagram and UpSet Plot for Set Overlap Visualization)
#
# Generated: 2026-01-04
# Seed: 42
# Observations: 500

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# Sample size
n <- 500

# ═══════════════════════════════════════════════════════════
# BINARY VARIABLES (Yes/No, Positive/Negative, Present/Absent)
# ═══════════════════════════════════════════════════════════

# Biomarker A - Moderate prevalence (40%)
biomarker_a <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.40, 0.60)),
  levels = c("Negative", "Positive")
)

# Biomarker B - Lower prevalence (25%)
biomarker_b <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.25, 0.75)),
  levels = c("Negative", "Positive")
)

# Biomarker C - Higher prevalence (60%)
biomarker_c <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.60, 0.40)),
  levels = c("Negative", "Positive")
)

# Diagnostic Test 1 - Moderate sensitivity (50%)
test_1 <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.50, 0.50)),
  levels = c("Negative", "Positive")
)

# Diagnostic Test 2 - Lower sensitivity (30%)
test_2 <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.30, 0.70)),
  levels = c("Negative", "Positive")
)

# Symptom 1 (Pain) - Common (55%)
symptom_pain <- factor(
  sample(c("Present", "Absent"), n, replace = TRUE, prob = c(0.55, 0.45)),
  levels = c("Absent", "Present")
)

# Symptom 2 (Mass) - Less common (35%)
symptom_mass <- factor(
  sample(c("Present", "Absent"), n, replace = TRUE, prob = c(0.35, 0.65)),
  levels = c("Absent", "Present")
)

# ═══════════════════════════════════════════════════════════
# MULTI-LEVEL CATEGORICAL VARIABLES (Test true level selection)
# ═══════════════════════════════════════════════════════════

# Mutation Status (3 levels) - Select "Mutation" as true
mutation_status <- factor(
  sample(c("Wild Type", "Mutation", "Unknown"), n, replace = TRUE, 
         prob = c(0.60, 0.30, 0.10)),
  levels = c("Wild Type", "Mutation", "Unknown")
)

# Tumor Grade (3 levels, ordered) - Select "High" as true
tumor_grade <- factor(
  sample(c("Low", "Intermediate", "High"), n, replace = TRUE,
         prob = c(0.30, 0.45, 0.25)),
  levels = c("Low", "Intermediate", "High"),
  ordered = TRUE
)

# Receptor Status (4 levels) - Select specific positive status as true
receptor_status <- factor(
  sample(c("Triple Negative", "ER+", "PR+", "HER2+"), n, replace = TRUE,
         prob = c(0.25, 0.35, 0.25, 0.15)),
  levels = c("Triple Negative", "ER+", "PR+", "HER2+")
)

# Stage (4 levels, ordered) - Select "Advanced" (III or IV) as true
# Create binary version for easier testing
stage_binary <- factor(
  sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.65, 0.35)),
  levels = c("Early", "Advanced")
)

# ═══════════════════════════════════════════════════════════
# REALISTIC CORRELATIONS AND OVERLAP PATTERNS
# ═══════════════════════════════════════════════════════════

# Correlation 1: Biomarker A positive → Higher likelihood of Test 1 positive
# Correlation strength: 0.7
biomarker_a_positive <- biomarker_a == "Positive"
test_1_prob <- ifelse(biomarker_a_positive, 0.75, 0.30)
test_1 <- factor(
  ifelse(runif(n) < test_1_prob, "Positive", "Negative"),
  levels = c("Negative", "Positive")
)

# Correlation 2: Biomarker B and Biomarker C often co-occur
# If B is positive, 60% chance C is also positive
biomarker_b_positive <- biomarker_b == "Positive"
biomarker_c_prob <- ifelse(biomarker_b_positive, 0.80, 0.50)
biomarker_c <- factor(
  ifelse(runif(n) < biomarker_c_prob, "Positive", "Negative"),
  levels = c("Negative", "Positive")
)

# Correlation 3: Symptoms often co-occur
# If pain present, 70% chance mass also present
pain_present <- symptom_pain == "Present"
mass_prob <- ifelse(pain_present, 0.70, 0.15)
symptom_mass <- factor(
  ifelse(runif(n) < mass_prob, "Present", "Absent"),
  levels = c("Absent", "Present")
)

# Correlation 4: Test 2 is more sensitive when both biomarkers positive
both_biomarkers <- (biomarker_a == "Positive") & (biomarker_b == "Positive")
test_2_prob <- case_when(
  both_biomarkers ~ 0.70,
  biomarker_a_positive ~ 0.45,
  biomarker_b_positive ~ 0.40,
  TRUE ~ 0.15
)
test_2 <- factor(
  ifelse(runif(n) < test_2_prob, "Positive", "Negative"),
  levels = c("Negative", "Positive")
)

# Correlation 5: Mutation status affects biomarker expression
mutation_positive <- mutation_status == "Mutation"
biomarker_a_prob <- ifelse(mutation_positive, 0.65, 0.30)
biomarker_a <- factor(
  ifelse(runif(n) < biomarker_a_prob, "Positive", "Negative"),
  levels = c("Negative", "Positive")
)

# Correlation 6: Advanced stage → More symptoms
advanced_stage <- stage_binary == "Advanced"
pain_prob <- ifelse(advanced_stage, 0.75, 0.45)
symptom_pain <- factor(
  ifelse(runif(n) < pain_prob, "Present", "Absent"),
  levels = c("Absent", "Present")
)

# ═══════════════════════════════════════════════════════════
# EDGE CASE VARIABLES
# ═══════════════════════════════════════════════════════════

# Rare condition (5% prevalence) - Tests minimum intersection sizes
rare_marker <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.05, 0.95)),
  levels = c("Negative", "Positive")
)

# Very common condition (95% prevalence) - Tests large overlaps
common_marker <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.95, 0.05)),
  levels = c("Negative", "Positive")
)

# Perfectly balanced (50/50) - Tests equal distribution
balanced_marker <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.50, 0.50)),
  levels = c("Negative", "Positive")
)

# Nearly mutually exclusive with biomarker_a
# If biomarker_a positive, only 10% chance this is positive
exclusive_marker <- factor(
  ifelse(biomarker_a == "Positive",
         sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.10, 0.90)),
         sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.50, 0.50))),
  levels = c("Negative", "Positive")
)

# Highly correlated with test_1 (90% agreement)
correlated_test <- factor(
  ifelse(runif(n) < 0.90, as.character(test_1),
         ifelse(test_1 == "Positive", "Negative", "Positive")),
  levels = c("Negative", "Positive")
)

# ═══════════════════════════════════════════════════════════
# CLINICAL COMORBIDITY VARIABLES
# ═══════════════════════════════════════════════════════════

# Diabetes (30% prevalence)
diabetes <- factor(
  sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.30, 0.70)),
  levels = c("No", "Yes")
)

# Hypertension (40% prevalence, correlated with diabetes)
hypertension_prob <- ifelse(diabetes == "Yes", 0.65, 0.30)
hypertension <- factor(
  ifelse(runif(n) < hypertension_prob, "Yes", "No"),
  levels = c("No", "Yes")
)

# Obesity (25% prevalence, correlated with both)
obesity_prob <- case_when(
  diabetes == "Yes" & hypertension == "Yes" ~ 0.60,
  diabetes == "Yes" | hypertension == "Yes" ~ 0.40,
  TRUE ~ 0.15
)
obesity <- factor(
  ifelse(runif(n) < obesity_prob, "Yes", "No"),
  levels = c("No", "Yes")
)

# ═══════════════════════════════════════════════════════════
# TREATMENT RESPONSE VARIABLES
# ═══════════════════════════════════════════════════════════

# Treatment A response (45% positive)
treatment_a_response <- factor(
  sample(c("Responder", "Non-responder"), n, replace = TRUE, 
         prob = c(0.45, 0.55)),
  levels = c("Non-responder", "Responder")
)

# Treatment B response (35% positive, some overlap with A)
treatment_b_prob <- ifelse(treatment_a_response == "Responder", 0.50, 0.25)
treatment_b_response <- factor(
  ifelse(runif(n) < treatment_b_prob, "Responder", "Non-responder"),
  levels = c("Non-responder", "Responder")
)

# ═══════════════════════════════════════════════════════════
# PATIENT CHARACTERISTICS
# ═══════════════════════════════════════════════════════════

# Sex
sex <- factor(
  sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.45, 0.55)),
  levels = c("Male", "Female")
)

# Smoking status
smoking <- factor(
  sample(c("Never", "Former", "Current"), n, replace = TRUE,
         prob = c(0.40, 0.35, 0.25)),
  levels = c("Never", "Former", "Current")
)

# Family history (30% positive)
family_history <- factor(
  sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.30, 0.70)),
  levels = c("No", "Yes")
)

# ═══════════════════════════════════════════════════════════
# VARIABLES WITH MISSING DATA
# ═══════════════════════════════════════════════════════════

# Biomarker D - 10% missing
biomarker_d <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.35, 0.65)),
  levels = c("Negative", "Positive")
)
biomarker_d[sample(n, round(n * 0.10))] <- NA

# Test 3 - 5% missing
test_3 <- factor(
  sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.40, 0.60)),
  levels = c("Negative", "Positive")
)
test_3[sample(n, round(n * 0.05))] <- NA

# ═══════════════════════════════════════════════════════════
# IDENTIFIERS AND METADATA
# ═══════════════════════════════════════════════════════════

patient_id <- sprintf("PAT%05d", 1:n)

# Continuous variables for context (not used in Venn, but realistic dataset)
age <- round(rnorm(n, mean = 62, sd = 14))
age <- pmax(pmin(age, 95), 18)

tumor_size_mm <- rnorm(n, mean = 28, sd = 12)
tumor_size_mm <- round(pmax(tumor_size_mm, 5), 1)

# ═══════════════════════════════════════════════════════════
# CREATE TIBBLE
# ═══════════════════════════════════════════════════════════

venn_test <- tibble(
  # Identifier
  patient_id = patient_id,
  
  # Primary binary variables for 2-3 set Venn diagrams
  biomarker_a = biomarker_a,
  biomarker_b = biomarker_b,
  biomarker_c = biomarker_c,
  test_1 = test_1,
  test_2 = test_2,
  symptom_pain = symptom_pain,
  symptom_mass = symptom_mass,
  
  # Multi-level categorical (for testing true level selection)
  mutation_status = mutation_status,
  tumor_grade = tumor_grade,
  receptor_status = receptor_status,
  stage_binary = stage_binary,
  
  # Edge cases
  rare_marker = rare_marker,
  common_marker = common_marker,
  balanced_marker = balanced_marker,
  exclusive_marker = exclusive_marker,
  correlated_test = correlated_test,
  
  # Comorbidities (clinical scenario)
  diabetes = diabetes,
  hypertension = hypertension,
  obesity = obesity,
  
  # Treatment responses
  treatment_a_response = treatment_a_response,
  treatment_b_response = treatment_b_response,
  
  # Patient characteristics
  sex = sex,
  smoking = smoking,
  family_history = family_history,
  
  # Variables with missing data
  biomarker_d = biomarker_d,
  test_3 = test_3,
  
  # Contextual continuous variables
  age = age,
  tumor_size_mm = tumor_size_mm
)

# ═══════════════════════════════════════════════════════════
# SAVE IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(venn_test, file = here::here("data", "venn_test.rda"))

# 2. CSV format
write.csv(venn_test,
          file = here::here("data", "venn_test.csv"),
          row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(venn_test,
                    path = here::here("data", "venn_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(venn_test, here::here("data", "venn_test.omv"))

cat("Dataset venn_test created successfully!\n")
