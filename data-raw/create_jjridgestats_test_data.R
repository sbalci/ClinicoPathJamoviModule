# Create test data for jjridgestats function
set.seed(42)

# Create ridgeline plot test data suitable for clinical research
# Simulating continuous distributions across different categorical groups

# Generate realistic clinical scenarios where ridgeline plots are useful
n_total <- 400

jjridgestats_test_data <- data.frame(
  patient_id = 1:n_total,
  
  # Biomarker levels by disease stage (typical ridgeline plot use case)
  # Different stages have different distribution shapes
  disease_stage = factor(
    sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 
           n_total, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
    levels = c("Stage I", "Stage II", "Stage III", "Stage IV"),
    ordered = TRUE
  ),
  
  # Treatment groups for clinical trials
  treatment_group = factor(
    sample(c("Control", "Treatment A", "Treatment B", "Treatment C"), 
           n_total, replace = TRUE)
  ),
  
  # Hospital centers for multi-center studies
  hospital_center = factor(
    sample(paste("Center", LETTERS[1:6]), 
           n_total, replace = TRUE)
  ),
  
  # Time periods for longitudinal studies
  time_period = factor(
    sample(c("Baseline", "Month 3", "Month 6", "Month 12"), 
           n_total, replace = TRUE, prob = c(0.4, 0.25, 0.25, 0.1)),
    levels = c("Baseline", "Month 3", "Month 6", "Month 12"),
    ordered = TRUE
  ),
  
  # Age groups for demographic analysis
  age_group = factor(
    sample(c("18-30", "31-50", "51-65", "66-80", "80+"), 
           n_total, replace = TRUE, prob = c(0.1, 0.25, 0.35, 0.25, 0.05)),
    levels = c("18-30", "31-50", "51-65", "66-80", "80+"),
    ordered = TRUE
  ),
  
  # Gender for demographic comparisons
  gender = factor(
    sample(c("Male", "Female"), n_total, replace = TRUE, prob = c(0.55, 0.45))
  ),
  
  # Geographic regions
  region = factor(
    sample(c("North America", "Europe", "Asia", "Latin America"), 
           n_total, replace = TRUE, prob = c(0.35, 0.30, 0.25, 0.10))
  )
)

# Create realistic continuous variables with different distributions by groups

# 1. Biomarker expression (log-normal distribution, varies by disease stage)
jjridgestats_test_data$biomarker_expression <- numeric(n_total)
for (i in 1:n_total) {
  stage <- jjridgestats_test_data$disease_stage[i]
  if (stage == "Stage I") {
    # Early stage: lower, tighter distribution
    jjridgestats_test_data$biomarker_expression[i] <- rlnorm(1, meanlog = 1.5, sdlog = 0.4)
  } else if (stage == "Stage II") {
    # Moderate stage: slightly higher
    jjridgestats_test_data$biomarker_expression[i] <- rlnorm(1, meanlog = 2.0, sdlog = 0.5)
  } else if (stage == "Stage III") {
    # Advanced stage: higher, more variable
    jjridgestats_test_data$biomarker_expression[i] <- rlnorm(1, meanlog = 2.5, sdlog = 0.6)
  } else {
    # Stage IV: highest, very variable
    jjridgestats_test_data$biomarker_expression[i] <- rlnorm(1, meanlog = 3.0, sdlog = 0.8)
  }
}

# 2. Treatment response (varies by treatment group)
jjridgestats_test_data$response_score <- numeric(n_total)
for (i in 1:n_total) {
  treatment <- jjridgestats_test_data$treatment_group[i]
  if (treatment == "Control") {
    # Control: lower response, normal distribution
    jjridgestats_test_data$response_score[i] <- rnorm(1, mean = 50, sd = 15)
  } else if (treatment == "Treatment A") {
    # Treatment A: moderate improvement
    jjridgestats_test_data$response_score[i] <- rnorm(1, mean = 65, sd = 18)
  } else if (treatment == "Treatment B") {
    # Treatment B: good improvement
    jjridgestats_test_data$response_score[i] <- rnorm(1, mean = 75, sd = 20)
  } else {
    # Treatment C: best improvement, bimodal (responders vs non-responders)
    if (runif(1) < 0.7) {
      # Responders (70%)
      jjridgestats_test_data$response_score[i] <- rnorm(1, mean = 85, sd = 12)
    } else {
      # Non-responders (30%)
      jjridgestats_test_data$response_score[i] <- rnorm(1, mean = 45, sd = 10)
    }
  }
}

# 3. Laboratory values (varies by hospital center - showing center effects)
jjridgestats_test_data$lab_value <- numeric(n_total)
for (i in 1:n_total) {
  center <- jjridgestats_test_data$hospital_center[i]
  base_mean <- 100  # Target lab value
  
  # Each center has slightly different mean (calibration differences)
  center_effect <- switch(as.character(center),
                         "Center A" = 0,     # Reference center
                         "Center B" = 5,     # Slightly higher
                         "Center C" = -3,    # Slightly lower
                         "Center D" = 8,     # Higher
                         "Center E" = -5,    # Lower
                         "Center F" = 2,     # Slightly higher
                         0)
  
  jjridgestats_test_data$lab_value[i] <- rnorm(1, mean = base_mean + center_effect, sd = 20)
}

# 4. Age (years) - varies by age group (for validation)
jjridgestats_test_data$age_years <- numeric(n_total)
for (i in 1:n_total) {
  age_grp <- jjridgestats_test_data$age_group[i]
  if (age_grp == "18-30") {
    jjridgestats_test_data$age_years[i] <- runif(1, 18, 30)
  } else if (age_grp == "31-50") {
    jjridgestats_test_data$age_years[i] <- runif(1, 31, 50)
  } else if (age_grp == "51-65") {
    jjridgestats_test_data$age_years[i] <- runif(1, 51, 65)
  } else if (age_grp == "66-80") {
    jjridgestats_test_data$age_years[i] <- runif(1, 66, 80)
  } else {
    jjridgestats_test_data$age_years[i] <- runif(1, 80, 95)
  }
}

# 5. Symptom severity score (varies by time period - longitudinal effect)
jjridgestats_test_data$symptom_severity <- numeric(n_total)
for (i in 1:n_total) {
  time <- jjridgestats_test_data$time_period[i]
  if (time == "Baseline") {
    # Baseline: higher severity
    jjridgestats_test_data$symptom_severity[i] <- rnorm(1, mean = 70, sd = 20)
  } else if (time == "Month 3") {
    # Improvement at 3 months
    jjridgestats_test_data$symptom_severity[i] <- rnorm(1, mean = 55, sd = 18)
  } else if (time == "Month 6") {
    # Further improvement at 6 months
    jjridgestats_test_data$symptom_severity[i] <- rnorm(1, mean = 45, sd = 16)
  } else {
    # Sustained improvement at 12 months
    jjridgestats_test_data$symptom_severity[i] <- rnorm(1, mean = 40, sd = 15)
  }
}

# 6. Quality of life score (varies by gender - demographic differences)
jjridgestats_test_data$quality_of_life <- numeric(n_total)
for (i in 1:n_total) {
  gender <- jjridgestats_test_data$gender[i]
  if (gender == "Male") {
    # Male baseline
    jjridgestats_test_data$quality_of_life[i] <- rnorm(1, mean = 65, sd = 20)
  } else {
    # Female: slightly different distribution
    jjridgestats_test_data$quality_of_life[i] <- rnorm(1, mean = 62, sd = 22)
  }
}

# 7. Inflammatory marker (skewed distribution, varies by region)
jjridgestats_test_data$inflammatory_marker <- numeric(n_total)
for (i in 1:n_total) {
  region <- jjridgestats_test_data$region[i]
  
  # Different populations have different inflammatory marker patterns
  if (region == "North America") {
    # Higher baseline inflammation
    jjridgestats_test_data$inflammatory_marker[i] <- rgamma(1, shape = 2, scale = 15)
  } else if (region == "Europe") {
    # Moderate baseline
    jjridgestats_test_data$inflammatory_marker[i] <- rgamma(1, shape = 2.5, scale = 12)
  } else if (region == "Asia") {
    # Lower baseline inflammation
    jjridgestats_test_data$inflammatory_marker[i] <- rgamma(1, shape = 3, scale = 8)
  } else {
    # Latin America: intermediate
    jjridgestats_test_data$inflammatory_marker[i] <- rgamma(1, shape = 2.2, scale = 13)
  }
}

# 8. Tumor size (for oncology studies, log-normal distribution)
jjridgestats_test_data$tumor_size_mm <- rlnorm(n_total, meanlog = 2.5, sdlog = 0.8)

# 9. Blood pressure (normal distribution with age effect)
jjridgestats_test_data$systolic_bp <- 100 + 0.5 * jjridgestats_test_data$age_years + rnorm(n_total, 0, 15)

# 10. Genetic risk score (beta distribution scaled)
jjridgestats_test_data$genetic_risk_score <- rbeta(n_total, 2, 5) * 100

# Apply realistic constraints
jjridgestats_test_data$biomarker_expression <- pmax(0.1, pmin(50, jjridgestats_test_data$biomarker_expression))
jjridgestats_test_data$response_score <- pmax(0, pmin(100, jjridgestats_test_data$response_score))
jjridgestats_test_data$lab_value <- pmax(20, pmin(200, jjridgestats_test_data$lab_value))
jjridgestats_test_data$symptom_severity <- pmax(0, pmin(100, jjridgestats_test_data$symptom_severity))
jjridgestats_test_data$quality_of_life <- pmax(0, pmin(100, jjridgestats_test_data$quality_of_life))
jjridgestats_test_data$inflammatory_marker <- pmax(0.5, pmin(100, jjridgestats_test_data$inflammatory_marker))
jjridgestats_test_data$tumor_size_mm <- pmax(5, pmin(150, jjridgestats_test_data$tumor_size_mm))
jjridgestats_test_data$systolic_bp <- pmax(80, pmin(200, jjridgestats_test_data$systolic_bp))

# Add some realistic missing values
missing_indices <- sample(nrow(jjridgestats_test_data), size = 20)
jjridgestats_test_data$biomarker_expression[missing_indices[1:5]] <- NA
jjridgestats_test_data$response_score[missing_indices[6:10]] <- NA
jjridgestats_test_data$lab_value[missing_indices[11:15]] <- NA
jjridgestats_test_data$inflammatory_marker[missing_indices[16:20]] <- NA

# Round numeric variables to appropriate precision
jjridgestats_test_data$biomarker_expression <- round(jjridgestats_test_data$biomarker_expression, 2)
jjridgestats_test_data$response_score <- round(jjridgestats_test_data$response_score, 1)
jjridgestats_test_data$lab_value <- round(jjridgestats_test_data$lab_value, 1)
jjridgestats_test_data$age_years <- round(jjridgestats_test_data$age_years, 0)
jjridgestats_test_data$symptom_severity <- round(jjridgestats_test_data$symptom_severity, 1)
jjridgestats_test_data$quality_of_life <- round(jjridgestats_test_data$quality_of_life, 1)
jjridgestats_test_data$inflammatory_marker <- round(jjridgestats_test_data$inflammatory_marker, 2)
jjridgestats_test_data$tumor_size_mm <- round(jjridgestats_test_data$tumor_size_mm, 1)
jjridgestats_test_data$systolic_bp <- round(jjridgestats_test_data$systolic_bp, 0)
jjridgestats_test_data$genetic_risk_score <- round(jjridgestats_test_data$genetic_risk_score, 1)

# Save the dataset
usethis::use_data(jjridgestats_test_data, overwrite = TRUE)

# Preview the data
print("jjridgestats_test_data structure:")
str(jjridgestats_test_data)
print("First few rows:")
head(jjridgestats_test_data)
print("Summary statistics:")
summary(jjridgestats_test_data)

# Check distributions of key variables by groups
print("Distribution characteristics for ridgeline plots:")
continuous_vars <- c("biomarker_expression", "response_score", "lab_value", "symptom_severity", "inflammatory_marker")

for (var in continuous_vars) {
  cat("\n", var, ":\n")
  cat("  Overall mean:", round(mean(jjridgestats_test_data[[var]], na.rm = TRUE), 2), "\n")
  cat("  Overall SD:", round(sd(jjridgestats_test_data[[var]], na.rm = TRUE), 2), "\n")
  cat("  Range:", round(range(jjridgestats_test_data[[var]], na.rm = TRUE), 2), "\n")
}

# Check group distributions for categorical variables
print("\nGroup distributions:")
categorical_vars <- c("disease_stage", "treatment_group", "hospital_center", "time_period", "age_group")

for (var in categorical_vars) {
  cat("\n", var, ":\n")
  print(table(jjridgestats_test_data[[var]]))
}

# Show examples of good ridgeline plot combinations
print("\nRecommended ridgeline plot examples:")
print("1. Biomarker expression by disease stage:")
cat("   Shows progression of biomarker levels across disease stages\n")

print("2. Response score by treatment group:")
cat("   Compares treatment efficacy distributions\n")

print("3. Lab value by hospital center:")
cat("   Reveals center-specific calibration differences\n")

print("4. Symptom severity by time period:")
cat("   Shows treatment response over time\n")

print("5. Inflammatory marker by region:")
cat("   Demonstrates population-specific distributions\n")

# Data quality summary
print("\nData quality summary:")
cat("Total observations:", nrow(jjridgestats_test_data), "\n")
cat("Complete cases:", sum(complete.cases(jjridgestats_test_data)), "\n")
cat("Missing values by variable:\n")
missing_summary <- sapply(jjridgestats_test_data, function(x) sum(is.na(x)))
print(missing_summary[missing_summary > 0])

# Statistical summaries by key grouping variables
print("\nBiomarker expression by disease stage (demonstrates ridgeline utility):")
biomarker_by_stage <- aggregate(biomarker_expression ~ disease_stage, 
                               data = jjridgestats_test_data, 
                               FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                  sd = sd(x, na.rm = TRUE)))
print(biomarker_by_stage)

print("\nResponse score by treatment group:")
response_by_treatment <- aggregate(response_score ~ treatment_group, 
                                 data = jjridgestats_test_data, 
                                 FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                    sd = sd(x, na.rm = TRUE)))
print(response_by_treatment)