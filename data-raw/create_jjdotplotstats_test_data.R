# Create test data for jjdotplotstats function
set.seed(42)

# Create dot plot test data suitable for clinical research
# Simulating biomarker expression levels across different disease groups

# Sample sizes for different groups
n_healthy <- 60
n_mild <- 80
n_moderate <- 70
n_severe <- 50

# Generate biomarker expression data with realistic clinical patterns
jjdotplotstats_test_data <- data.frame(
  patient_id = 1:(n_healthy + n_mild + n_moderate + n_severe),
  
  # Primary biomarker: CRP levels (mg/L) - shows clear group differences
  crp_level = c(
    rnorm(n_healthy, mean = 2.5, sd = 1.0),      # Healthy: low CRP
    rnorm(n_mild, mean = 8.0, sd = 2.5),         # Mild: slightly elevated
    rnorm(n_moderate, mean = 15.0, sd = 4.0),    # Moderate: elevated
    rnorm(n_severe, mean = 25.0, sd = 6.0)       # Severe: high
  ),
  
  # Secondary biomarker: ESR levels (mm/hr) - correlated with disease severity
  esr_level = c(
    rnorm(n_healthy, mean = 12, sd = 3),         # Healthy: low ESR
    rnorm(n_mild, mean = 25, sd = 5),           # Mild
    rnorm(n_moderate, mean = 45, sd = 8),       # Moderate
    rnorm(n_severe, mean = 70, sd = 12)         # Severe: high ESR
  ),
  
  # Tertiary biomarker: Platelet count (×10³/μL)
  platelet_count = c(
    rnorm(n_healthy, mean = 280, sd = 40),       # Healthy: normal range
    rnorm(n_mild, mean = 350, sd = 50),         # Mild: slightly elevated
    rnorm(n_moderate, mean = 420, sd = 60),     # Moderate: elevated
    rnorm(n_severe, mean = 500, sd = 80)        # Severe: high
  ),
  
  # Disease severity groups
  disease_severity = factor(
    rep(c("Healthy", "Mild", "Moderate", "Severe"), 
        times = c(n_healthy, n_mild, n_moderate, n_severe)),
    levels = c("Healthy", "Mild", "Moderate", "Severe"),
    ordered = TRUE
  ),
  
  # Treatment center (for grouping analysis)
  treatment_center = factor(
    sample(paste("Center", LETTERS[1:4]), 
           size = n_healthy + n_mild + n_moderate + n_severe, 
           replace = TRUE)
  ),
  
  # Patient demographics
  age_years = c(
    round(rnorm(n_healthy, mean = 45, sd = 12)),     # Healthy: younger
    round(rnorm(n_mild, mean = 55, sd = 15)),        # Mild
    round(rnorm(n_moderate, mean = 62, sd = 18)),    # Moderate: older
    round(rnorm(n_severe, mean = 68, sd = 20))       # Severe: oldest
  ),
  
  gender = factor(
    sample(c("Male", "Female"), 
           size = n_healthy + n_mild + n_moderate + n_severe, 
           replace = TRUE, prob = c(0.45, 0.55))
  ),
  
  # Comorbidity status
  comorbidity_status = factor(
    c(
      sample(c("None", "Single", "Multiple"), n_healthy, 
             replace = TRUE, prob = c(0.7, 0.25, 0.05)),
      sample(c("None", "Single", "Multiple"), n_mild, 
             replace = TRUE, prob = c(0.4, 0.45, 0.15)),
      sample(c("None", "Single", "Multiple"), n_moderate, 
             replace = TRUE, prob = c(0.2, 0.5, 0.3)),
      sample(c("None", "Single", "Multiple"), n_severe, 
             replace = TRUE, prob = c(0.1, 0.4, 0.5))
    )
  ),
  
  # Treatment response (for additional analysis)
  treatment_response = factor(
    c(
      rep("N/A", n_healthy),  # Healthy don't need treatment
      sample(c("Good", "Moderate", "Poor"), n_mild, 
             replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      sample(c("Good", "Moderate", "Poor"), n_moderate, 
             replace = TRUE, prob = c(0.4, 0.4, 0.2)),
      sample(c("Good", "Moderate", "Poor"), n_severe, 
             replace = TRUE, prob = c(0.2, 0.3, 0.5))
    )
  )
)

# Ensure realistic ranges and round values appropriately
jjdotplotstats_test_data$crp_level <- pmax(0.1, round(jjdotplotstats_test_data$crp_level, 1))
jjdotplotstats_test_data$esr_level <- pmax(1, round(jjdotplotstats_test_data$esr_level))
jjdotplotstats_test_data$platelet_count <- pmax(100, round(jjdotplotstats_test_data$platelet_count))
jjdotplotstats_test_data$age_years <- pmax(18, pmin(95, jjdotplotstats_test_data$age_years))

# Add some realistic missing values (common in clinical data)
missing_indices <- sample(nrow(jjdotplotstats_test_data), size = 8)
jjdotplotstats_test_data$esr_level[missing_indices[1:3]] <- NA
jjdotplotstats_test_data$platelet_count[missing_indices[4:6]] <- NA
jjdotplotstats_test_data$age_years[missing_indices[7:8]] <- NA

# Create additional continuous variables for comprehensive testing
jjdotplotstats_test_data$hemoglobin_level <- ifelse(
  jjdotplotstats_test_data$gender == "Male",
  rnorm(nrow(jjdotplotstats_test_data), mean = 14.5, sd = 1.5),
  rnorm(nrow(jjdotplotstats_test_data), mean = 12.5, sd = 1.2)
)
jjdotplotstats_test_data$hemoglobin_level <- round(jjdotplotstats_test_data$hemoglobin_level, 1)

# Save the dataset
usethis::use_data(jjdotplotstats_test_data, overwrite = TRUE)

# Preview the data
print("jjdotplotstats_test_data structure:")
str(jjdotplotstats_test_data)
print("First few rows:")
head(jjdotplotstats_test_data)
print("Summary statistics:")
summary(jjdotplotstats_test_data)

# Test the group differences
print("CRP levels by disease severity:")
aggregate(crp_level ~ disease_severity, data = jjdotplotstats_test_data, 
          FUN = function(x) c(mean = round(mean(x, na.rm = TRUE), 2),
                              sd = round(sd(x, na.rm = TRUE), 2)))

print("ESR levels by disease severity:")
aggregate(esr_level ~ disease_severity, data = jjdotplotstats_test_data, 
          FUN = function(x) c(mean = round(mean(x, na.rm = TRUE), 2),
                              sd = round(sd(x, na.rm = TRUE), 2)))

# Group counts
print("Group sizes:")
table(jjdotplotstats_test_data$disease_severity)

print("Treatment center distribution:")
table(jjdotplotstats_test_data$treatment_center)