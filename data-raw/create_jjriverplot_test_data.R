# Create test data for jjriverplot function
set.seed(42)

# Create river plot test data suitable for clinical research
# Simulating patient pathways, treatment flows, and outcome transitions

# Generate realistic clinical scenarios where river plots are useful
n_patients <- 600

# Create patient treatment pathway data (longitudinal format)
jjriverplot_test_data_long <- data.frame(
  patient_id = rep(paste0("P", sprintf("%04d", 1:n_patients)), each = 4),
  
  # Time points in a clinical study
  timepoint = factor(
    rep(c("Baseline", "Month_3", "Month_6", "Month_12"), times = n_patients),
    levels = c("Baseline", "Month_3", "Month_6", "Month_12"),
    ordered = TRUE
  ),
  
  # Patient demographics (constant per patient)
  age_group = factor(
    rep(sample(c("18-40", "41-60", "61-75", "75+"), 
               n_patients, replace = TRUE, prob = c(0.2, 0.35, 0.35, 0.1)),
         each = 4),
    levels = c("18-40", "41-60", "61-75", "75+"),
    ordered = TRUE
  ),
  
  gender = factor(
    rep(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.55, 0.45)),
         each = 4)
  ),
  
  # Initial diagnosis (constant per patient)
  initial_diagnosis = factor(
    rep(sample(c("Stage_I", "Stage_II", "Stage_III", "Stage_IV"), 
               n_patients, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
         each = 4),
    levels = c("Stage_I", "Stage_II", "Stage_III", "Stage_IV"),
    ordered = TRUE
  ),
  
  # Treatment assignment (constant per patient within study)
  treatment_arm = factor(
    rep(sample(c("Control", "Treatment_A", "Treatment_B", "Combination"), 
               n_patients, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
         each = 4)
  ),
  
  # Hospital center (constant per patient)
  hospital_center = factor(
    rep(sample(paste0("Center_", LETTERS[1:8]), 
               n_patients, replace = TRUE),
         each = 4)
  )
)

# Create realistic treatment response progression over time
jjriverplot_test_data_long$treatment_response <- factor("Unknown", 
  levels = c("Complete_Response", "Partial_Response", "Stable_Disease", 
             "Progressive_Disease", "Discontinued", "Unknown"))

# Function to generate realistic treatment progression
generate_treatment_progression <- function(initial_stage, treatment) {
  progression <- c("Unknown", "Unknown", "Unknown", "Unknown")  # Default
  
  # Baseline response probabilities based on initial stage and treatment
  if (initial_stage == "Stage_I") {
    if (treatment == "Control") {
      base_probs <- c(0.15, 0.35, 0.35, 0.15)  # CR, PR, SD, PD
    } else {
      base_probs <- c(0.35, 0.40, 0.20, 0.05)  # Better response with treatment
    }
  } else if (initial_stage == "Stage_II") {
    if (treatment == "Control") {
      base_probs <- c(0.10, 0.30, 0.40, 0.20)
    } else {
      base_probs <- c(0.25, 0.35, 0.30, 0.10)
    }
  } else if (initial_stage == "Stage_III") {
    if (treatment == "Control") {
      base_probs <- c(0.05, 0.20, 0.45, 0.30)
    } else {
      base_probs <- c(0.15, 0.30, 0.35, 0.20)
    }
  } else { # Stage_IV
    if (treatment == "Control") {
      base_probs <- c(0.02, 0.15, 0.35, 0.48)
    } else {
      base_probs <- c(0.08, 0.22, 0.40, 0.30)
    }
  }
  
  # Month 3 response
  month3_response <- sample(c("Complete_Response", "Partial_Response", 
                             "Stable_Disease", "Progressive_Disease"), 
                           1, prob = base_probs)
  progression[2] <- month3_response
  
  # Month 6 response (depends on Month 3)
  if (month3_response == "Complete_Response") {
    month6_response <- sample(c("Complete_Response", "Partial_Response", "Stable_Disease"), 
                             1, prob = c(0.8, 0.15, 0.05))
  } else if (month3_response == "Partial_Response") {
    month6_response <- sample(c("Complete_Response", "Partial_Response", "Stable_Disease", "Progressive_Disease"), 
                             1, prob = c(0.25, 0.45, 0.25, 0.05))
  } else if (month3_response == "Stable_Disease") {
    month6_response <- sample(c("Partial_Response", "Stable_Disease", "Progressive_Disease", "Discontinued"), 
                             1, prob = c(0.20, 0.50, 0.25, 0.05))
  } else { # Progressive_Disease
    month6_response <- sample(c("Stable_Disease", "Progressive_Disease", "Discontinued"), 
                             1, prob = c(0.15, 0.60, 0.25))
  }
  progression[3] <- month6_response
  
  # Month 12 response (depends on Month 6)
  if (month6_response == "Complete_Response") {
    month12_response <- sample(c("Complete_Response", "Partial_Response"), 
                              1, prob = c(0.9, 0.1))
  } else if (month6_response == "Partial_Response") {
    month12_response <- sample(c("Complete_Response", "Partial_Response", "Stable_Disease"), 
                              1, prob = c(0.30, 0.50, 0.20))
  } else if (month6_response == "Stable_Disease") {
    month12_response <- sample(c("Partial_Response", "Stable_Disease", "Progressive_Disease"), 
                              1, prob = c(0.25, 0.55, 0.20))
  } else if (month6_response == "Progressive_Disease") {
    month12_response <- sample(c("Stable_Disease", "Progressive_Disease", "Discontinued"), 
                              1, prob = c(0.10, 0.70, 0.20))
  } else { # Discontinued
    month12_response <- "Discontinued"
  }
  progression[4] <- month12_response
  
  return(progression)
}

# Generate treatment progressions for all patients
for (i in seq(1, nrow(jjriverplot_test_data_long), by = 4)) {
  patient_idx <- ceiling(i/4)
  initial_stage <- jjriverplot_test_data_long$initial_diagnosis[i]
  treatment <- jjriverplot_test_data_long$treatment_arm[i]
  
  progression <- generate_treatment_progression(initial_stage, treatment)
  
  # Assign progression to the 4 timepoints for this patient
  jjriverplot_test_data_long$treatment_response[i:(i+3)] <- factor(progression, 
    levels = levels(jjriverplot_test_data_long$treatment_response))
}

# Add treatment costs (realistic healthcare costs)
jjriverplot_test_data_long$treatment_cost <- numeric(nrow(jjriverplot_test_data_long))

for (i in 1:nrow(jjriverplot_test_data_long)) {
  # Base cost depends on timepoint
  timepoint <- jjriverplot_test_data_long$timepoint[i]
  base_cost <- switch(as.character(timepoint),
                     "Baseline" = 2000,      # Initial workup
                     "Month_3" = 1500,       # Follow-up visit
                     "Month_6" = 1500,       # Follow-up visit  
                     "Month_12" = 2000,      # Extended follow-up
                     1500)
  
  # Treatment modifier
  treatment <- jjriverplot_test_data_long$treatment_arm[i]
  treatment_multiplier <- switch(as.character(treatment),
                                "Control" = 0.5,
                                "Treatment_A" = 1.2,
                                "Treatment_B" = 1.5,
                                "Combination" = 2.0,
                                1.0)
  
  # Response modifier (higher costs for poor response)
  response <- jjriverplot_test_data_long$treatment_response[i]
  response_multiplier <- switch(as.character(response),
                               "Complete_Response" = 0.8,
                               "Partial_Response" = 1.0,
                               "Stable_Disease" = 1.2,
                               "Progressive_Disease" = 1.8,
                               "Discontinued" = 0.3,
                               "Unknown" = 1.0,
                               1.0)
  
  # Stage modifier (higher stages cost more)
  stage <- jjriverplot_test_data_long$initial_diagnosis[i]
  stage_multiplier <- switch(as.character(stage),
                            "Stage_I" = 1.0,
                            "Stage_II" = 1.3,
                            "Stage_III" = 1.7,
                            "Stage_IV" = 2.2,
                            1.0)
  
  jjriverplot_test_data_long$treatment_cost[i] <- base_cost * treatment_multiplier * 
                                                  response_multiplier * stage_multiplier * 
                                                  (1 + runif(1, -0.1, 0.1)) # Add noise
}

# Round costs
jjriverplot_test_data_long$treatment_cost <- round(jjriverplot_test_data_long$treatment_cost, 0)

# Create additional categorical outcomes for more river plot examples
# Quality of life scores (categorized)
jjriverplot_test_data_long$quality_of_life <- factor("Unknown", 
  levels = c("Excellent", "Good", "Fair", "Poor", "Unknown"))

for (i in seq(1, nrow(jjriverplot_test_data_long), by = 4)) {
  # QoL progression based on treatment response
  patient_responses <- jjriverplot_test_data_long$treatment_response[i:(i+3)]
  qol_progression <- c("Unknown", "Unknown", "Unknown", "Unknown")
  
  # Month 3 QoL
  if (patient_responses[2] %in% c("Complete_Response", "Partial_Response")) {
    qol_progression[2] <- sample(c("Excellent", "Good", "Fair"), 1, prob = c(0.4, 0.4, 0.2))
  } else if (patient_responses[2] == "Stable_Disease") {
    qol_progression[2] <- sample(c("Good", "Fair", "Poor"), 1, prob = c(0.3, 0.5, 0.2))
  } else {
    qol_progression[2] <- sample(c("Fair", "Poor"), 1, prob = c(0.3, 0.7))
  }
  
  # Month 6 QoL (tends to improve or maintain)
  prev_qol <- qol_progression[2]
  if (prev_qol == "Excellent") {
    qol_progression[3] <- sample(c("Excellent", "Good"), 1, prob = c(0.8, 0.2))
  } else if (prev_qol == "Good") {
    qol_progression[3] <- sample(c("Excellent", "Good", "Fair"), 1, prob = c(0.3, 0.5, 0.2))
  } else if (prev_qol == "Fair") {
    qol_progression[3] <- sample(c("Good", "Fair", "Poor"), 1, prob = c(0.3, 0.5, 0.2))
  } else {
    qol_progression[3] <- sample(c("Fair", "Poor"), 1, prob = c(0.4, 0.6))
  }
  
  # Month 12 QoL
  prev_qol <- qol_progression[3]
  if (prev_qol == "Excellent") {
    qol_progression[4] <- sample(c("Excellent", "Good"), 1, prob = c(0.9, 0.1))
  } else if (prev_qol == "Good") {
    qol_progression[4] <- sample(c("Excellent", "Good", "Fair"), 1, prob = c(0.2, 0.6, 0.2))
  } else if (prev_qol == "Fair") {
    qol_progression[4] <- sample(c("Good", "Fair", "Poor"), 1, prob = c(0.3, 0.5, 0.2))
  } else {
    qol_progression[4] <- sample(c("Fair", "Poor"), 1, prob = c(0.3, 0.7))
  }
  
  jjriverplot_test_data_long$quality_of_life[i:(i+3)] <- factor(qol_progression, 
    levels = levels(jjriverplot_test_data_long$quality_of_life))
}

# Create wide format dataset for different river plot scenarios
# Extract unique patients
unique_patients <- jjriverplot_test_data_long[jjriverplot_test_data_long$timepoint == "Baseline", ]

jjriverplot_test_data_wide <- data.frame(
  patient_id = unique_patients$patient_id,
  age_group = unique_patients$age_group,
  gender = unique_patients$gender,
  initial_diagnosis = unique_patients$initial_diagnosis,
  treatment_arm = unique_patients$treatment_arm,
  hospital_center = unique_patients$hospital_center,
  
  # Treatment response at each timepoint
  baseline_response = factor("Unknown", levels = levels(jjriverplot_test_data_long$treatment_response)),
  month3_response = factor("Unknown", levels = levels(jjriverplot_test_data_long$treatment_response)),
  month6_response = factor("Unknown", levels = levels(jjriverplot_test_data_long$treatment_response)),
  month12_response = factor("Unknown", levels = levels(jjriverplot_test_data_long$treatment_response)),
  
  # QoL at each timepoint
  baseline_qol = factor("Unknown", levels = levels(jjriverplot_test_data_long$quality_of_life)),
  month3_qol = factor("Unknown", levels = levels(jjriverplot_test_data_long$quality_of_life)),
  month6_qol = factor("Unknown", levels = levels(jjriverplot_test_data_long$quality_of_life)),
  month12_qol = factor("Unknown", levels = levels(jjriverplot_test_data_long$quality_of_life)),
  
  # Total treatment cost
  total_cost = numeric(n_patients)
)

# Fill in the wide format data
for (i in 1:n_patients) {
  patient_data <- jjriverplot_test_data_long[jjriverplot_test_data_long$patient_id == unique_patients$patient_id[i], ]
  
  # Treatment responses
  jjriverplot_test_data_wide$baseline_response[i] <- patient_data$treatment_response[1]
  jjriverplot_test_data_wide$month3_response[i] <- patient_data$treatment_response[2]
  jjriverplot_test_data_wide$month6_response[i] <- patient_data$treatment_response[3]
  jjriverplot_test_data_wide$month12_response[i] <- patient_data$treatment_response[4]
  
  # Quality of life
  jjriverplot_test_data_wide$baseline_qol[i] <- patient_data$quality_of_life[1]
  jjriverplot_test_data_wide$month3_qol[i] <- patient_data$quality_of_life[2]
  jjriverplot_test_data_wide$month6_qol[i] <- patient_data$quality_of_life[3]
  jjriverplot_test_data_wide$month12_qol[i] <- patient_data$quality_of_life[4]
  
  # Total cost
  jjriverplot_test_data_wide$total_cost[i] <- sum(patient_data$treatment_cost)
}

# Create additional datasets for different river plot scenarios

# 1. Educational pathways dataset
n_students <- 400
jjriverplot_education_data <- data.frame(
  student_id = paste0("S", sprintf("%04d", 1:n_students)),
  
  # Demographics
  entrance_score = sample(c("High", "Medium", "Low"), n_students, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  socioeconomic_status = sample(c("High", "Middle", "Low"), n_students, replace = TRUE, prob = c(0.25, 0.50, 0.25)),
  
  # Academic progression
  year1_performance = sample(c("Excellent", "Good", "Satisfactory", "Poor"), n_students, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  year2_performance = sample(c("Excellent", "Good", "Satisfactory", "Poor", "Dropped"), n_students, replace = TRUE, prob = c(0.18, 0.32, 0.30, 0.15, 0.05)),
  year3_performance = sample(c("Excellent", "Good", "Satisfactory", "Poor", "Dropped"), n_students, replace = TRUE, prob = c(0.20, 0.35, 0.28, 0.12, 0.05)),
  final_outcome = sample(c("Graduated_Honors", "Graduated", "Probation", "Dropped"), n_students, replace = TRUE, prob = c(0.25, 0.55, 0.10, 0.10))
)

# 2. Marketing funnel dataset  
n_customers <- 800
jjriverplot_marketing_data <- data.frame(
  customer_id = paste0("C", sprintf("%04d", 1:n_customers)),
  
  # Customer characteristics
  source = sample(c("Social_Media", "Email", "Referral", "Direct", "Ads"), n_customers, replace = TRUE, prob = c(0.3, 0.2, 0.15, 0.2, 0.15)),
  device_type = sample(c("Mobile", "Desktop", "Tablet"), n_customers, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  
  # Funnel stages
  awareness = factor("Aware", levels = c("Aware", "Unaware")),  # All start aware
  interest = sample(c("Interested", "Not_Interested"), n_customers, replace = TRUE, prob = c(0.4, 0.6)),
  consideration = sample(c("Considering", "Not_Considering", "Lost"), n_customers, replace = TRUE, prob = c(0.25, 0.60, 0.15)),
  purchase = sample(c("Purchased", "Abandoned_Cart", "Browsing", "Lost"), n_customers, replace = TRUE, prob = c(0.15, 0.10, 0.50, 0.25)),
  loyalty = sample(c("Repeat_Customer", "One_Time", "Churned"), n_customers, replace = TRUE, prob = c(0.30, 0.50, 0.20)),
  
  # Purchase value
  purchase_value = ifelse(runif(n_customers) < 0.15, runif(n_customers, 50, 500), 0)  # Only purchasers have value
)

# Save all datasets
usethis::use_data(jjriverplot_test_data_long, overwrite = TRUE)
usethis::use_data(jjriverplot_test_data_wide, overwrite = TRUE) 
usethis::use_data(jjriverplot_education_data, overwrite = TRUE)
usethis::use_data(jjriverplot_marketing_data, overwrite = TRUE)

# Preview the data
print("jjriverplot_test_data_long structure:")
str(jjriverplot_test_data_long)
print("First few rows:")
head(jjriverplot_test_data_long, 12)

print("\njjriverplot_test_data_wide structure:")
str(jjriverplot_test_data_wide)
print("First few rows:")
head(jjriverplot_test_data_wide)

print("\njjriverplot_education_data structure:")
str(jjriverplot_education_data)
print("First few rows:")
head(jjriverplot_education_data)

print("\njjriverplot_marketing_data structure:")
str(jjriverplot_marketing_data)
print("First few rows:")
head(jjriverplot_marketing_data)

# Summary statistics
print("\nTreatment response distributions over time:")
response_by_time <- table(jjriverplot_test_data_long$timepoint, 
                         jjriverplot_test_data_long$treatment_response)
print(response_by_time)

print("\nTreatment response by treatment arm (Month 12 only):")
month12_data <- jjriverplot_test_data_long[jjriverplot_test_data_long$timepoint == "Month_12", ]
response_by_treatment <- table(month12_data$treatment_arm, month12_data$treatment_response)
print(response_by_treatment)

print("\nQuality of life by treatment response (Month 12):")
qol_by_response <- table(month12_data$treatment_response, month12_data$quality_of_life)
print(qol_by_response)

# Cost analysis
print("\nAverage treatment costs by timepoint:")
cost_by_time <- aggregate(treatment_cost ~ timepoint, 
                         data = jjriverplot_test_data_long, 
                         FUN = function(x) c(mean = mean(x), median = median(x)))
print(cost_by_time)

print("\nAverage total costs by final response:")
cost_by_final_response <- aggregate(total_cost ~ month12_response, 
                                   data = jjriverplot_test_data_wide, 
                                   FUN = function(x) c(mean = mean(x), median = median(x)))
print(cost_by_final_response)

# Data quality checks
print("\nData quality summary:")
cat("Long format - Total observations:", nrow(jjriverplot_test_data_long), "\n")
cat("Long format - Unique patients:", length(unique(jjriverplot_test_data_long$patient_id)), "\n")
cat("Wide format - Total patients:", nrow(jjriverplot_test_data_wide), "\n")
cat("Education data - Total students:", nrow(jjriverplot_education_data), "\n")
cat("Marketing data - Total customers:", nrow(jjriverplot_marketing_data), "\n")

# Check for missing values
cat("\nMissing values:\n")
cat("Long format:", sum(is.na(jjriverplot_test_data_long)), "\n")
cat("Wide format:", sum(is.na(jjriverplot_test_data_wide)), "\n")
cat("Education data:", sum(is.na(jjriverplot_education_data)), "\n")
cat("Marketing data:", sum(is.na(jjriverplot_marketing_data)), "\n")

print("\nRecommended river plot examples:")
print("1. Treatment response over time (long format):")
cat("   time = 'timepoint', strata = 'treatment_response', weight = 'treatment_cost'\n")

print("2. Multi-stage treatment pathway (wide format):")
cat("   strata = c('month3_response', 'month6_response', 'month12_response')\n")

print("3. Educational progression:")
cat("   strata = c('year1_performance', 'year2_performance', 'year3_performance', 'final_outcome')\n")

print("4. Marketing funnel:")
cat("   strata = c('awareness', 'interest', 'consideration', 'purchase', 'loyalty')\n")