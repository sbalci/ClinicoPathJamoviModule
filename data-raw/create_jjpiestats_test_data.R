# Create test data for jjpiestats function
set.seed(42)

# Create pie chart test data suitable for clinical research
# Simulating categorical outcome distributions

# Generate categorical variables typical in clinical research
n_total <- 250

jjpiestats_test_data <- data.frame(
  patient_id = 1:n_total,
  
  # Primary outcome: Treatment response (typical clinical trial endpoint)
  treatment_response = factor(
    sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
           n_total, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
    levels = c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
    ordered = FALSE
  ),
  
  # Disease severity: Ordered categorical (common in clinical assessment)
  disease_severity = factor(
    sample(c("Mild", "Moderate", "Severe"), 
           n_total, replace = TRUE, prob = c(0.4, 0.45, 0.15)),
    levels = c("Mild", "Moderate", "Severe"),
    ordered = TRUE
  ),
  
  # Tumor grade: Pathological classification
  tumor_grade = factor(
    sample(c("Grade I", "Grade II", "Grade III", "Grade IV"), 
           n_total, replace = TRUE, prob = c(0.3, 0.4, 0.25, 0.05)),
    levels = c("Grade I", "Grade II", "Grade III", "Grade IV"),
    ordered = TRUE
  ),
  
  # Treatment arm: Clinical trial groups
  treatment_arm = factor(
    sample(c("Control", "Treatment A", "Treatment B"), 
           n_total, replace = TRUE, prob = c(0.33, 0.34, 0.33))
  ),
  
  # Hospital site: Multi-center study
  hospital_site = factor(
    sample(paste("Site", LETTERS[1:5]), 
           n_total, replace = TRUE)
  ),
  
  # Gender: Binary demographic
  gender = factor(
    sample(c("Male", "Female"), n_total, replace = TRUE, prob = c(0.58, 0.42))
  ),
  
  # Age group: Categorized continuous variable
  age_group = factor(
    sample(c("18-40", "41-60", "61-75", "76+"), 
           n_total, replace = TRUE, prob = c(0.15, 0.35, 0.40, 0.10)),
    levels = c("18-40", "41-60", "61-75", "76+"),
    ordered = TRUE
  ),
  
  # Performance status: ECOG scale (common in oncology)
  performance_status = factor(
    sample(c("ECOG 0", "ECOG 1", "ECOG 2", "ECOG 3"), 
           n_total, replace = TRUE, prob = c(0.35, 0.45, 0.15, 0.05)),
    levels = c("ECOG 0", "ECOG 1", "ECOG 2", "ECOG 3"),
    ordered = TRUE
  ),
  
  # Biomarker status: Binary biomarker (e.g., mutation status)
  biomarker_status = factor(
    sample(c("Positive", "Negative"), 
           n_total, replace = TRUE, prob = c(0.35, 0.65))
  ),
  
  # Prior therapy: Treatment history
  prior_therapy = factor(
    sample(c("Naive", "One Prior", "Multiple Prior"), 
           n_total, replace = TRUE, prob = c(0.40, 0.35, 0.25)),
    levels = c("Naive", "One Prior", "Multiple Prior"),
    ordered = TRUE
  ),
  
  # Histological type: Pathological classification
  histology = factor(
    sample(c("Adenocarcinoma", "Squamous Cell", "Large Cell", "Small Cell", "Other"), 
           n_total, replace = TRUE, prob = c(0.45, 0.25, 0.10, 0.15, 0.05))
  ),
  
  # Insurance type: Healthcare access factor
  insurance_type = factor(
    sample(c("Private", "Medicare", "Medicaid", "Uninsured"), 
           n_total, replace = TRUE, prob = c(0.55, 0.25, 0.15, 0.05))
  ),
  
  # Smoking status: Risk factor
  smoking_status = factor(
    sample(c("Never", "Former", "Current"), 
           n_total, replace = TRUE, prob = c(0.35, 0.45, 0.20))
  ),
  
  # Adverse event grade: Safety endpoint
  adverse_event_grade = factor(
    sample(c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4"), 
           n_total, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.15, 0.05)),
    levels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4"),
    ordered = TRUE
  ),
  
  # Geographic region: Multi-regional study
  region = factor(
    sample(c("North America", "Europe", "Asia-Pacific", "Latin America"), 
           n_total, replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10))
  )
)

# Add some realistic missing values
missing_indices <- sample(nrow(jjpiestats_test_data), size = 12)
jjpiestats_test_data$biomarker_status[missing_indices[1:4]] <- NA
jjpiestats_test_data$prior_therapy[missing_indices[5:8]] <- NA
jjpiestats_test_data$adverse_event_grade[missing_indices[9:12]] <- NA

# Create some correlations between variables to make data more realistic
# Treatment response should correlate with performance status
for (i in 1:nrow(jjpiestats_test_data)) {
  if (jjpiestats_test_data$performance_status[i] == "ECOG 3") {
    # Poor performance status -> worse response
    jjpiestats_test_data$treatment_response[i] <- sample(
      c("Stable Disease", "Progressive Disease"), 1, prob = c(0.3, 0.7)
    )
  } else if (jjpiestats_test_data$performance_status[i] == "ECOG 0") {
    # Good performance status -> better response
    jjpiestats_test_data$treatment_response[i] <- sample(
      c("Complete Response", "Partial Response", "Stable Disease"), 1, prob = c(0.4, 0.5, 0.1)
    )
  }
}

# Tumor grade should correlate with disease severity
for (i in 1:nrow(jjpiestats_test_data)) {
  if (jjpiestats_test_data$tumor_grade[i] %in% c("Grade III", "Grade IV")) {
    jjpiestats_test_data$disease_severity[i] <- sample(
      c("Moderate", "Severe"), 1, prob = c(0.3, 0.7)
    )
  } else if (jjpiestats_test_data$tumor_grade[i] == "Grade I") {
    jjpiestats_test_data$disease_severity[i] <- sample(
      c("Mild", "Moderate"), 1, prob = c(0.7, 0.3)
    )
  }
}

# Save the dataset
usethis::use_data(jjpiestats_test_data, overwrite = TRUE)

# Preview the data
print("jjpiestats_test_data structure:")
str(jjpiestats_test_data)
print("First few rows:")
head(jjpiestats_test_data)
print("Summary of categorical variables:")
summary(jjpiestats_test_data)

# Check distributions of key variables
print("Distribution characteristics:")
categorical_vars <- c("treatment_response", "disease_severity", "tumor_grade", "treatment_arm", "biomarker_status")

for (var in categorical_vars) {
  cat("\n", var, ":\n")
  print(table(jjpiestats_test_data[[var]], useNA = "ifany"))
  cat("  Proportions:\n")
  print(round(prop.table(table(jjpiestats_test_data[[var]], useNA = "ifany")), 3))
}

# Check cross-tabulations for grouped analyses
print("\nCross-tabulations for grouped analysis:")
print("Treatment Response by Treatment Arm:")
print(table(jjpiestats_test_data$treatment_response, jjpiestats_test_data$treatment_arm))

print("\nDisease Severity by Hospital Site:")
print(table(jjpiestats_test_data$disease_severity, jjpiestats_test_data$hospital_site))

print("\nTumor Grade by Gender:")
print(table(jjpiestats_test_data$tumor_grade, jjpiestats_test_data$gender))

# Check for suitable variables for different pie chart scenarios
print("\nVariables suitable for different pie chart types:")
print("Single pie chart: treatment_response, disease_severity, tumor_grade")
print("Grouped pie chart (x vs y): treatment_response vs treatment_arm")
print("Split pie chart (x vs y by grouping.var): treatment_response vs treatment_arm by hospital_site")

# Data quality checks
print("\nData quality summary:")
cat("Total observations:", nrow(jjpiestats_test_data), "\n")
cat("Complete cases:", sum(complete.cases(jjpiestats_test_data)), "\n")
cat("Missing values by variable:\n")
sapply(jjpiestats_test_data, function(x) sum(is.na(x)))