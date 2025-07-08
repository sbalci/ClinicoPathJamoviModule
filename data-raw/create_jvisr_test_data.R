# Create test data for jvisr function
# Clinical Research Visualization with visR package

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# 1. Basic clinical trial survival data
jvisr_basic <- data.frame(
    patient_id = paste0("PT_", sprintf("%03d", 1:300)),
    
    # Standard survival analysis variables
    time_to_event = rexp(300, rate = 0.1),  # Exponential survival times
    event_indicator = rbinom(300, 1, 0.65),  # 65% event rate
    time_to_progression = rweibull(300, shape = 1.2, scale = 12),  # Weibull distribution
    progression_indicator = rbinom(300, 1, 0.7),  # 70% progression rate
    
    # CDISC ADaM ADTTE standard variables
    AVAL = rexp(300, rate = 0.08),  # Analysis value (time to event)
    CNSR = rbinom(300, 1, 0.35),   # Censor indicator (1=censored, 0=event)
    AVALC = character(300),         # Character version of AVAL
    PARAM = factor(rep("Overall Survival", 300)),  # Parameter
    PARAMCD = factor(rep("OS", 300)),  # Parameter code
    
    # Treatment and study design variables
    treatment_arm = factor(sample(c("Placebo", "Experimental"), 300, replace = TRUE, prob = c(0.5, 0.5))),
    dose_group = factor(sample(c("Placebo", "Low_Dose", "High_Dose"), 300, replace = TRUE, prob = c(0.3, 0.35, 0.35))),
    study_phase = factor(sample(c("Phase_II", "Phase_III"), 300, replace = TRUE, prob = c(0.4, 0.6))),
    study_site = factor(sample(c("Site_001", "Site_002", "Site_003", "Site_004"), 300, replace = TRUE)),
    
    # Patient demographics
    age = round(rnorm(300, 58, 12)),
    gender = factor(sample(c("Male", "Female"), 300, replace = TRUE, prob = c(0.52, 0.48))),
    race = factor(sample(c("White", "Black", "Asian", "Hispanic", "Other"), 
                        300, replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.08, 0.02))),
    ethnicity = factor(sample(c("Hispanic", "Non-Hispanic"), 300, replace = TRUE, prob = c(0.12, 0.88))),
    
    # Clinical characteristics
    baseline_ecog = factor(sample(0:2, 300, replace = TRUE, prob = c(0.4, 0.45, 0.15)), ordered = TRUE),
    disease_stage = factor(sample(c("I", "II", "III", "IV"), 300, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))),
    histology = factor(sample(c("Adenocarcinoma", "Squamous", "Large_Cell", "Other"), 
                             300, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05))),
    smoking_status = factor(sample(c("Never", "Former", "Current"), 300, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    
    # Biomarkers and laboratory values
    biomarker_positive = factor(sample(c("Positive", "Negative"), 300, replace = TRUE, prob = c(0.4, 0.6))),
    pdl1_expression = rnorm(300, 25, 15),  # PD-L1 expression percentage
    tumor_size = exp(rnorm(300, 3, 0.6)),  # Log-normal tumor size
    lymph_nodes = rpois(300, 2),  # Number of positive lymph nodes
    
    # Quality of life and patient reported outcomes
    baseline_qol = round(rnorm(300, 70, 15)),
    pain_score = round(rnorm(300, 4, 2.5)),
    fatigue_score = round(rnorm(300, 5, 2)),
    
    # Response and safety outcomes
    best_response = factor(sample(c("CR", "PR", "SD", "PD"), 300, replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.2))),
    adverse_event_grade = factor(sample(1:4, 300, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)), ordered = TRUE),
    discontinuation = factor(sample(c("No", "Yes"), 300, replace = TRUE, prob = c(0.75, 0.25)))
) %>%
    mutate(
        # Create realistic relationships between variables
        time_to_event = case_when(
            treatment_arm == "Placebo" ~ rexp(300, 0.12),
            treatment_arm == "Experimental" ~ rexp(300, 0.08)
        ) + rnorm(300, 0, 2),
        
        # Ensure positive survival times
        time_to_event = pmax(0.1, time_to_event),
        
        # Create dose-response relationships
        time_to_progression = case_when(
            dose_group == "Placebo" ~ rweibull(300, 1.0, 8),
            dose_group == "Low_Dose" ~ rweibull(300, 1.2, 12),
            dose_group == "High_Dose" ~ rweibull(300, 1.4, 15)
        ) + rnorm(300, 0, 1.5),
        
        time_to_progression = pmax(0.1, time_to_progression),
        
        # CDISC AVAL should match time_to_event for OS endpoint
        AVAL = time_to_event,
        AVALC = paste(round(AVAL, 1), "months"),
        
        # Event indicators should be consistent
        event_indicator = case_when(
            CNSR == 1 ~ 0,  # If censored, no event
            CNSR == 0 ~ 1   # If not censored, event occurred
        ),
        
        # Age effects
        age = pmax(18, pmin(85, age)),
        
        # Biomarker relationships
        pdl1_expression = pmax(0, pmin(100, pdl1_expression)),
        baseline_qol = pmax(0, pmin(100, baseline_qol)),
        pain_score = pmax(0, pmin(10, pain_score)),
        fatigue_score = pmax(0, pmin(10, fatigue_score))
    )

# 2. Multi-endpoint clinical trial data
jvisr_multiendpoint <- data.frame(
    patient_id = rep(paste0("PT_", sprintf("%03d", 1:200)), each = 3),
    
    # Multiple time-to-event endpoints
    endpoint_type = factor(rep(c("Overall_Survival", "Progression_Free_Survival", "Time_to_Response"), 200)),
    
    # Time and event variables for each endpoint
    time_value = c(
        rexp(200, 0.08),  # OS times
        rweibull(200, 1.5, 10),  # PFS times  
        rweibull(200, 2.0, 6)   # TTR times
    ),
    
    event_occurred = c(
        rbinom(200, 1, 0.6),  # OS events
        rbinom(200, 1, 0.75), # PFS events
        rbinom(200, 1, 0.4)   # Response events
    ),
    
    # Treatment information
    treatment = factor(rep(sample(c("Control", "Treatment_A", "Treatment_B"), 200, replace = TRUE), each = 3)),
    randomization_strata = factor(rep(sample(c("Strata_1", "Strata_2"), 200, replace = TRUE), each = 3)),
    
    # Patient characteristics (repeated for each endpoint)
    age_group = factor(rep(sample(c("18-65", "65-75", ">75"), 200, replace = TRUE, prob = c(0.6, 0.3, 0.1)), each = 3)),
    gender = factor(rep(sample(c("Male", "Female"), 200, replace = TRUE), each = 3)),
    region = factor(rep(sample(c("North_America", "Europe", "Asia_Pacific"), 200, replace = TRUE, prob = c(0.4, 0.4, 0.2)), each = 3))
) %>%
    mutate(
        # Create endpoint-specific realistic times
        time_value = case_when(
            endpoint_type == "Overall_Survival" & treatment == "Control" ~ rexp(600, 0.1)[1:600],
            endpoint_type == "Overall_Survival" & treatment == "Treatment_A" ~ rexp(600, 0.08)[1:600],
            endpoint_type == "Overall_Survival" & treatment == "Treatment_B" ~ rexp(600, 0.06)[1:600],
            endpoint_type == "Progression_Free_Survival" & treatment == "Control" ~ rweibull(600, 1.2, 6)[1:600],
            endpoint_type == "Progression_Free_Survival" & treatment == "Treatment_A" ~ rweibull(600, 1.4, 9)[1:600],
            endpoint_type == "Progression_Free_Survival" & treatment == "Treatment_B" ~ rweibull(600, 1.6, 12)[1:600],
            endpoint_type == "Time_to_Response" & treatment == "Control" ~ rep(NA, 600)[1:600],  # No response expected in control
            endpoint_type == "Time_to_Response" & treatment == "Treatment_A" ~ rweibull(600, 2.0, 4)[1:600],
            endpoint_type == "Time_to_Response" & treatment == "Treatment_B" ~ rweibull(600, 2.5, 3)[1:600],
            TRUE ~ time_value
        ),
        
        time_value = pmax(0.1, time_value, na.rm = TRUE),
        
        # Adjust event rates by endpoint and treatment
        event_occurred = case_when(
            endpoint_type == "Time_to_Response" & treatment == "Control" ~ 0,  # No response in control
            TRUE ~ event_occurred
        )
    )

# 3. Real-world evidence / observational study data
jvisr_rwe <- data.frame(
    patient_id = paste0("RWE_", sprintf("%04d", 1:500)),
    
    # Complex survival times with heterogeneity
    time_to_death = rweibull(500, shape = 1.1, scale = 24),
    death_observed = rbinom(500, 1, 0.45),  # Lower event rate in RWE
    time_to_hospitalization = rweibull(500, shape = 1.8, scale = 8),
    hospitalization_occurred = rbinom(500, 1, 0.8),  # High hospitalization rate
    
    # Treatment patterns (more complex than RCT)
    line_of_therapy = factor(sample(1:4, 500, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))),
    treatment_class = factor(sample(c("Chemotherapy", "Immunotherapy", "Targeted", "Combination"), 
                                  500, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2))),
    treatment_setting = factor(sample(c("Adjuvant", "Neoadjuvant", "Metastatic"), 500, replace = TRUE, prob = c(0.2, 0.1, 0.7))),
    
    # Healthcare system variables
    hospital_type = factor(sample(c("Academic", "Community", "Cancer_Center"), 500, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    insurance_type = factor(sample(c("Private", "Medicare", "Medicaid", "Uninsured"), 
                                 500, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05))),
    geographic_region = factor(sample(c("Northeast", "Southeast", "Midwest", "West"), 500, replace = TRUE)),
    
    # More heterogeneous patient population
    age = round(rnorm(500, 65, 18)),  # Older population
    comorbidity_count = rpois(500, 3),  # More comorbidities
    charlson_score = sample(0:10, 500, replace = TRUE, prob = c(0.2, 0.25, 0.2, 0.15, 0.1, 0.05, 0.03, 0.01, 0.01, 0, 0)),
    
    # Socioeconomic factors
    rural_urban = factor(sample(c("Urban", "Suburban", "Rural"), 500, replace = TRUE, prob = c(0.6, 0.25, 0.15))),
    education_level = factor(sample(c("Less_than_HS", "HS_Graduate", "Some_College", "College_Graduate"), 
                                   500, replace = TRUE, prob = c(0.15, 0.3, 0.3, 0.25))),
    median_income = round(rnorm(500, 55000, 20000)),
    
    # Clinical complexity
    metastatic_sites = sample(0:5, 500, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.25, 0.1, 0.05)),
    prior_treatments = sample(0:8, 500, replace = TRUE, prob = c(0.05, 0.15, 0.2, 0.2, 0.15, 0.1, 0.08, 0.05, 0.02)),
    
    # Missing data patterns (realistic for RWE)
    missing_biomarker = factor(sample(c("Available", "Missing"), 500, replace = TRUE, prob = c(0.3, 0.7))),
    incomplete_staging = factor(sample(c("Complete", "Incomplete"), 500, replace = TRUE, prob = c(0.7, 0.3)))
) %>%
    mutate(
        # Realistic relationships in RWE setting
        time_to_death = case_when(
            line_of_therapy == 1 ~ rweibull(500, 1.5, 30),
            line_of_therapy == 2 ~ rweibull(500, 1.3, 20),
            line_of_therapy == 3 ~ rweibull(500, 1.1, 12),
            line_of_therapy == 4 ~ rweibull(500, 0.9, 8)
        ) + 
        case_when(
            charlson_score <= 2 ~ rnorm(500, 5, 3),
            charlson_score <= 5 ~ rnorm(500, 0, 3),
            TRUE ~ rnorm(500, -5, 3)
        ),
        
        time_to_death = pmax(0.5, time_to_death),
        
        # Age constraints
        age = pmax(18, pmin(95, age)),
        median_income = pmax(15000, median_income)
    )

# 4. Competing risks data
jvisr_competing_risks <- data.frame(
    patient_id = paste0("CR_", sprintf("%03d", 1:250)),
    
    # Time variables
    time_to_event = rexp(250, 0.15),
    
    # Competing events
    event_type = factor(sample(c("Primary_Event", "Competing_Event", "Censored"), 
                              250, replace = TRUE, prob = c(0.4, 0.25, 0.35))),
    
    # Cause-specific indicators
    primary_event = as.numeric(NA),
    competing_event = as.numeric(NA),
    any_event = as.numeric(NA),
    
    # Risk factors
    risk_group = factor(sample(c("Low", "Intermediate", "High"), 250, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    age_category = factor(sample(c("Young", "Middle", "Elderly"), 250, replace = TRUE, prob = c(0.25, 0.5, 0.25))),
    gender = factor(sample(c("Male", "Female"), 250, replace = TRUE)),
    
    # Additional variables for Fine-Gray model
    baseline_risk_score = rnorm(250, 0, 1),
    treatment_received = factor(sample(c("Standard", "Experimental"), 250, replace = TRUE))
) %>%
    mutate(
        # Create competing events indicators
        primary_event = ifelse(event_type == "Primary_Event", 1, 0),
        competing_event = ifelse(event_type == "Competing_Event", 1, 0),
        any_event = ifelse(event_type == "Censored", 0, 1),
        
        # Adjust times based on risk factors
        time_to_event = case_when(
            risk_group == "Low" ~ rexp(250, 0.08),
            risk_group == "Intermediate" ~ rexp(250, 0.15),
            risk_group == "High" ~ rexp(250, 0.25)
        ) * case_when(
            age_category == "Young" ~ 1.5,
            age_category == "Middle" ~ 1.0,
            age_category == "Elderly" ~ 0.7
        ),
        
        time_to_event = pmax(0.1, time_to_event)
    )

# 5. Biomarker-driven precision medicine data  
jvisr_precision <- data.frame(
    patient_id = paste0("PM_", sprintf("%03d", 1:300)),
    
    # Genomic and molecular characteristics
    mutation_status = factor(sample(c("Wild_Type", "Mutated"), 300, replace = TRUE, prob = c(0.7, 0.3))),
    pd_l1_status = factor(sample(c("Negative", "Low", "High"), 300, replace = TRUE, prob = c(0.4, 0.35, 0.25))),
    microsatellite_status = factor(sample(c("MSS", "MSI-H"), 300, replace = TRUE, prob = c(0.85, 0.15))),
    tumor_mutation_burden = rpois(300, 8),  # TMB score
    
    # Targeted therapy assignments
    therapy_type = factor(sample(c("Standard_Chemo", "Targeted_Therapy", "Immunotherapy", "Combination"), 
                                300, replace = TRUE, prob = c(0.25, 0.3, 0.25, 0.2))),
    
    # Biomarker-stratified outcomes
    time_to_progression = rweibull(300, 1.2, 10),
    progression_occurred = rbinom(300, 1, 0.75),
    time_to_response = rweibull(300, 2.0, 4),
    response_occurred = rbinom(300, 1, 0.45),
    
    # Continuous biomarkers
    ctdna_level = exp(rnorm(300, 2, 1.2)),  # Circulating tumor DNA
    tumor_size_baseline = exp(rnorm(300, 3.2, 0.6)),
    immune_signature_score = rnorm(300, 0, 1),
    
    # Patient stratification
    biomarker_group = factor(sample(c("Biomarker_Positive", "Biomarker_Negative", "Unknown"), 
                                   300, replace = TRUE, prob = c(0.4, 0.45, 0.15))),
    treatment_line = factor(sample(c("First_Line", "Second_Line", "Later_Line"), 
                                  300, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
) %>%
    mutate(
        # Create biomarker-treatment interactions
        time_to_progression = case_when(
            mutation_status == "Mutated" & therapy_type == "Targeted_Therapy" ~ rweibull(300, 1.8, 18),
            mutation_status == "Wild_Type" & therapy_type == "Targeted_Therapy" ~ rweibull(300, 1.0, 8),
            pd_l1_status == "High" & therapy_type == "Immunotherapy" ~ rweibull(300, 1.6, 16),
            pd_l1_status == "Negative" & therapy_type == "Immunotherapy" ~ rweibull(300, 1.0, 6),
            TRUE ~ time_to_progression
        ),
        
        time_to_progression = pmax(0.5, time_to_progression),
        
        # Response rates based on biomarkers
        response_occurred = case_when(
            mutation_status == "Mutated" & therapy_type == "Targeted_Therapy" ~ rbinom(300, 1, 0.7),
            pd_l1_status == "High" & therapy_type == "Immunotherapy" ~ rbinom(300, 1, 0.6),
            TRUE ~ response_occurred
        ),
        
        # Ensure positive values
        ctdna_level = pmax(0.1, ctdna_level),
        tumor_size_baseline = pmax(0.5, tumor_size_baseline),
        tumor_mutation_burden = pmax(0, tumor_mutation_burden)
    )

# 6. Missing data scenarios for robustness testing
jvisr_missing <- jvisr_basic
set.seed(123)

# Introduce different missing data patterns
# Missing completely at random (MCAR)
jvisr_missing$time_to_event[sample(1:300, 25)] <- NA
jvisr_missing$event_indicator[sample(1:300, 20)] <- NA

# Missing at random (MAR) - related to treatment arm
placebo_indices <- which(jvisr_missing$treatment_arm == "Placebo")
jvisr_missing$biomarker_positive[sample(placebo_indices, length(placebo_indices) * 0.3)] <- NA

# Missing not at random (MNAR) - related to disease severity
severe_indices <- which(jvisr_missing$disease_stage == "IV")
jvisr_missing$baseline_qol[sample(severe_indices, length(severe_indices) * 0.4)] <- NA

# Stratification variable with missing values
jvisr_missing$treatment_arm[sample(1:300, 15)] <- NA

# CDISC variables with missing patterns
jvisr_missing$AVAL[sample(1:300, 18)] <- NA
jvisr_missing$CNSR[sample(1:300, 12)] <- NA

# 7. Small sample size data for edge case testing
jvisr_small <- jvisr_basic[1:25, ]

# 8. Large dataset for performance testing
jvisr_large <- do.call(rbind, replicate(8, jvisr_basic, simplify = FALSE))
jvisr_large$patient_id <- paste0("LG_", sprintf("%04d", 1:nrow(jvisr_large)))

# 9. Edge cases and extreme scenarios
jvisr_edge_cases <- data.frame(
    patient_id = paste0("EDGE_", sprintf("%02d", 1:80)),
    
    # Single treatment group
    single_treatment = factor(rep("OnlyTreatment", 80)),
    
    # Very unbalanced groups
    unbalanced_treatment = factor(sample(c("Large_Group", "Small_Group"), 80, replace = TRUE, prob = c(0.95, 0.05))),
    
    # Extreme survival times
    very_short_times = rexp(80, 5),  # Very short survival
    very_long_times = rexp(80, 0.01),  # Very long survival
    extreme_outlier_times = c(rexp(75, 0.1), rep(1000, 5)),  # With extreme outliers
    
    # All events or all censored
    all_events = rep(1, 80),
    all_censored = rep(0, 80),
    no_variation_time = rep(12, 80),  # No time variation
    
    # Very small event rates
    rare_events = rbinom(80, 1, 0.05),  # 5% event rate
    
    # Groups with very different sample sizes
    heterogeneous_groups = factor(c(rep("Big", 60), rep("Medium", 15), rep("Tiny", 5))),
    
    # Integer survival times that might cause issues
    integer_times = sample(1:24, 80, replace = TRUE),
    
    # Near-zero survival times
    near_zero_times = runif(80, 0.001, 0.1)
)

# 10. Longitudinal survival data with landmark analysis
jvisr_landmark <- do.call(rbind, lapply(1:100, function(patient) {
    landmark_times <- c(6, 12, 18, 24)  # 6, 12, 18, 24 months
    treatment <- sample(c("Control", "Treatment"), 1)
    baseline_risk <- rnorm(1, 0, 1)
    
    data.frame(
        patient_id = rep(paste0("LM_", sprintf("%03d", patient)), 4),
        landmark_time = landmark_times,
        treatment = rep(treatment, 4),
        baseline_risk_score = rep(baseline_risk, 4),
        
        # Time from landmark to event
        time_from_landmark = sapply(landmark_times, function(lm) {
            if (treatment == "Control") {
                rexp(1, 0.15 + 0.02 * lm)  # Increasing hazard over time
            } else {
                rexp(1, 0.10 + 0.01 * lm)  # Lower hazard for treatment
            }
        }),
        
        event_from_landmark = rbinom(4, 1, 0.6),
        
        # Total survival time
        total_survival_time = landmark_times + sapply(landmark_times, function(lm) {
            if (treatment == "Control") {
                rexp(1, 0.15 + 0.02 * lm)
            } else {
                rexp(1, 0.10 + 0.01 * lm) 
            }
        })
    )
}))

# Create comprehensive test descriptions
jvisr_test_descriptions <- data.frame(
    dataset_name = c(
        "jvisr_basic",
        "jvisr_multiendpoint", 
        "jvisr_rwe",
        "jvisr_competing_risks",
        "jvisr_precision",
        "jvisr_missing",
        "jvisr_small",
        "jvisr_large",
        "jvisr_edge_cases",
        "jvisr_landmark"
    ),
    description = c(
        "Basic clinical trial survival data with CDISC support - 300 patients",
        "Multi-endpoint clinical trial with OS, PFS, TTR - 200 patients × 3 endpoints",
        "Real-world evidence data with treatment complexity - 500 patients",
        "Competing risks analysis data with multiple event types - 250 patients", 
        "Precision medicine with biomarker-driven therapy - 300 patients",
        "Basic data with various missing data patterns (MCAR, MAR, MNAR) - 300 patients with NAs",
        "Small sample size data for edge case testing - 25 patients",
        "Large dataset for performance testing - 2400 patients",
        "Edge cases and extreme scenarios for robustness - 80 patients",
        "Landmark analysis data with time-varying effects - 400 observations (100 patients × 4 landmarks)"
    ),
    primary_use = c(
        "Standard survival analysis testing, CDISC format validation",
        "Multiple endpoint analysis, competing events",
        "Real-world evidence studies, treatment heterogeneity",
        "Competing risks modeling, cause-specific hazards",
        "Biomarker-stratified analysis, precision medicine",
        "Missing data handling and robustness testing",
        "Small sample behavior and statistical power",
        "Performance testing and scalability",
        "Robustness testing with extreme values and edge cases",
        "Landmark analysis and time-varying treatment effects"
    ),
    jvisr_features = c(
        "All analysis types, Kaplan-Meier, CDISC format, stratification",
        "Multiple endpoints, cumulative incidence, time-varying hazards",
        "Complex treatment patterns, heterogeneous populations",
        "Competing risks, cause-specific analysis, Fine-Gray models",
        "Biomarker stratification, precision medicine workflows",
        "Missing data patterns, incomplete observations",
        "Small sample handling, confidence interval behavior",
        "Performance optimization, caching effectiveness", 
        "Outlier handling, extreme survival times, edge cases",
        "Landmark analysis, time-dependent covariates"
    ),
    patients = c(300, 200, 500, 250, 300, 300, 25, 2400, 80, 100),
    observations = c(300, 600, 500, 250, 300, 300, 25, 2400, 80, 400),
    variables = c(25, 12, 20, 12, 15, 25, 25, 25, 12, 7)
)

# Print summary
cat("Created", nrow(jvisr_test_descriptions), "test datasets for jvisr function:\\n\\n")
print(jvisr_test_descriptions)

cat("\\n\\nDataset memory usage summary:\\n")
for (dataset_name in jvisr_test_descriptions$dataset_name) {
    if (exists(dataset_name)) {
        obj_size <- format(object.size(get(dataset_name)), units = "KB")
        cat(sprintf("%-25s: %s\\n", dataset_name, obj_size))
    }
}

cat("\\n\\nExample usage for different clinical visualization types:\\n\\n")

cat("# Basic Kaplan-Meier Analysis\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_basic,\\n")
cat("    analysis_type = 'kaplan_meier',\\n")
cat("    time_var = 'time_to_event',\\n")
cat("    event_var = 'event_indicator',\\n")
cat("    strata_var = 'treatment_arm'\\n")
cat(")\\n\\n")

cat("# CDISC ADaM ADTTE Analysis\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_basic,\\n")
cat("    analysis_type = 'kaplan_meier',\\n")
cat("    cdisc_format = TRUE,\\n")
cat("    aval_var = 'AVAL',\\n")
cat("    cnsr_var = 'CNSR',\\n")
cat("    strata_var = 'dose_group'\\n")
cat(")\\n\\n")

cat("# Cumulative Incidence Analysis\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_competing_risks,\\n")
cat("    analysis_type = 'cuminc',\\n")
cat("    time_var = 'time_to_event',\\n")
cat("    event_var = 'primary_event',\\n")
cat("    strata_var = 'risk_group'\\n")
cat(")\\n\\n")

cat("# Precision Medicine Biomarker Analysis\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_precision,\\n")
cat("    analysis_type = 'kaplan_meier',\\n")
cat("    time_var = 'time_to_progression',\\n")
cat("    event_var = 'progression_occurred',\\n")
cat("    strata_var = 'biomarker_group',\\n")
cat("    title = 'Biomarker-Stratified Survival Analysis'\\n")
cat(")\\n\\n")

cat("# Real-World Evidence Analysis\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_rwe,\\n")
cat("    analysis_type = 'kaplan_meier',\\n")
cat("    time_var = 'time_to_death',\\n")
cat("    event_var = 'death_observed',\\n")
cat("    strata_var = 'treatment_class',\\n")
cat("    confidence_interval = TRUE,\\n")
cat("    risk_table = TRUE\\n")
cat(")\\n\\n")

cat("# Table One Clinical Summary\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_basic,\\n")
cat("    analysis_type = 'tableone'\\n")
cat(")\\n\\n")

cat("# Risk Table Analysis\\n")
cat("result <- jvisr(\\n")
cat("    data = jvisr_multiendpoint[jvisr_multiendpoint$endpoint_type == 'Overall_Survival', ],\\n")
cat("    analysis_type = 'risktable',\\n")
cat("    time_var = 'time_value',\\n")
cat("    event_var = 'event_occurred',\\n")
cat("    strata_var = 'treatment'\\n")
cat(")\\n\\n")

# Save datasets if requested
if (exists("save_datasets") && save_datasets) {
    save_dir <- "data"
    if (!dir.exists(save_dir)) dir.create(save_dir)
    
    datasets_to_save <- list(
        jvisr_basic = jvisr_basic,
        jvisr_multiendpoint = jvisr_multiendpoint,
        jvisr_rwe = jvisr_rwe,
        jvisr_competing_risks = jvisr_competing_risks,
        jvisr_precision = jvisr_precision,
        jvisr_missing = jvisr_missing,
        jvisr_small = jvisr_small,
        jvisr_large = jvisr_large,
        jvisr_edge_cases = jvisr_edge_cases,
        jvisr_landmark = jvisr_landmark,
        jvisr_test_descriptions = jvisr_test_descriptions
    )
    
    save(list = names(datasets_to_save), 
         file = file.path(save_dir, "jvisr_test_data.RData"))
    
    cat("Test datasets saved to:", file.path(save_dir, "jvisr_test_data.RData"), "\\n")
}

# Additional utility functions for testing
create_survival_test_data <- function(n = 200, treatments = 2, distribution = "exponential") {
    # Helper function to create survival data on demand
    treatment_names <- LETTERS[1:treatments]
    
    if (distribution == "exponential") {
        times <- rexp(n, 0.1)
    } else if (distribution == "weibull") {
        times <- rweibull(n, shape = 1.2, scale = 10)
    } else if (distribution == "lognormal") {
        times <- rlnorm(n, meanlog = 2, sdlog = 0.5)
    }
    
    data.frame(
        id = 1:n,
        time = times,
        event = rbinom(n, 1, 0.6),
        treatment = factor(sample(treatment_names, n, replace = TRUE))
    )
}

create_cdisc_test_data <- function(n = 150) {
    # Helper function for CDISC-compliant survival data
    data.frame(
        USUBJID = paste0("STUDY001-", sprintf("%03d", 1:n)),
        AVAL = rexp(n, 0.08),  # Analysis value
        CNSR = rbinom(n, 1, 0.3),  # Censor indicator
        PARAM = factor(rep("Overall Survival", n)),
        PARAMCD = factor(rep("OS", n)),
        TRT01P = factor(sample(c("Placebo", "Active"), n, replace = TRUE)),
        TRT01PN = ifelse(sample(c("Placebo", "Active"), n, replace = TRUE) == "Placebo", 0, 1),
        SITEID = factor(sample(paste0("SITE", sprintf("%02d", 1:5)), n, replace = TRUE)),
        AGE = round(rnorm(n, 60, 12)),
        SEX = factor(sample(c("M", "F"), n, replace = TRUE))
    )
}

cat("Utility functions created:\\n")
cat("- create_survival_test_data(n, treatments, distribution): Generate survival data for testing\\n")
cat("- create_cdisc_test_data(n): Generate CDISC-compliant survival data\\n")

# Clean up workspace
rm(list = setdiff(ls(), c(jvisr_test_descriptions$dataset_name, "jvisr_test_descriptions", 
                          "create_survival_test_data", "create_cdisc_test_data")))
gc()