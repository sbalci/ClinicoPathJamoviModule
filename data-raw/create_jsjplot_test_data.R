# Create test data for jsjplot function
# Social Science Statistical Visualization using sjPlot package

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# 1. Basic regression data with multiple model types
jsjplot_regression <- data.frame(
    # Continuous outcomes
    outcome_continuous = rnorm(500, mean = 50, sd = 15),
    outcome_income = exp(rnorm(500, mean = 10, sd = 0.5)), # Log-normal distribution
    
    # Binary outcomes
    outcome_binary = rbinom(500, 1, 0.3),
    outcome_success = rbinom(500, 1, 0.4),
    
    # Count outcomes
    outcome_count = rpois(500, lambda = 3),
    outcome_events = rnbinom(500, size = 5, prob = 0.3),
    
    # Continuous predictors
    age = round(rnorm(500, mean = 45, sd = 15)),
    income = exp(rnorm(500, mean = 10.5, sd = 0.8)),
    education_years = round(rnorm(500, mean = 14, sd = 3)),
    experience = round(runif(500, 0, 30)),
    
    # Categorical predictors
    gender = factor(sample(c("Male", "Female"), 500, replace = TRUE, prob = c(0.48, 0.52))),
    education_level = factor(sample(c("High School", "Bachelor", "Master", "PhD"), 
                                  500, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))),
    employment = factor(sample(c("Full-time", "Part-time", "Unemployed", "Student"), 
                              500, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1))),
    region = factor(sample(c("North", "South", "East", "West"), 500, replace = TRUE)),
    
    # Interaction variables
    treatment = factor(sample(c("Control", "Treatment_A", "Treatment_B"), 
                             500, replace = TRUE, prob = c(0.4, 0.3, 0.3))),
    time_point = factor(sample(c("Baseline", "Month_3", "Month_6", "Month_12"), 
                              500, replace = TRUE))
) %>%
    mutate(
        # Create realistic relationships
        outcome_continuous = outcome_continuous + 
            0.3 * age + 
            0.0001 * income + 
            2 * education_years + 
            ifelse(gender == "Female", 5, 0) + 
            rnorm(500, sd = 8),
        
        # Binary outcome with logistic relationship
        outcome_binary = rbinom(500, 1, plogis(-2 + 
            0.02 * age + 
            0.3 * education_years + 
            ifelse(gender == "Female", 0.5, 0) + 
            ifelse(treatment == "Treatment_A", 0.8, 0) + 
            ifelse(treatment == "Treatment_B", 1.2, 0))),
        
        # Count outcome with Poisson relationship
        outcome_count = rpois(500, exp(1 + 
            0.01 * age + 
            0.05 * education_years + 
            ifelse(employment == "Full-time", 0.3, 0)))
    )

# 2. Longitudinal/Panel data for mixed effects models
jsjplot_longitudinal <- do.call(rbind, lapply(1:100, function(id) {
    baseline_ability <- rnorm(1, 0, 1)
    data.frame(
        participant_id = id,
        time = 1:5,
        age_baseline = round(rnorm(1, 25, 5)),
        treatment = factor(sample(c("Control", "Intervention"), 1)),
        gender = factor(sample(c("Male", "Female"), 1)),
        
        # Outcome that changes over time
        performance = baseline_ability + 
            0.2 * (1:5) + # Time trend
            ifelse(rep(sample(c("Control", "Intervention"), 1) == "Intervention", 5), 
                   0.5 * (1:5), 0) + # Treatment effect
            rnorm(5, 0, 0.5), # Random error
        
        cognitive_score = 100 + baseline_ability * 15 + 
            0.3 * (1:5) + 
            rnorm(5, 0, 8)
    )
}))

# 3. Survey/Psychology data with multiple scales
jsjplot_psychology <- data.frame(
    participant_id = 1:300,
    
    # Demographics
    age = round(rnorm(300, 35, 12)),
    gender = factor(sample(c("Male", "Female", "Non-binary"), 300, replace = TRUE, prob = c(0.45, 0.5, 0.05))),
    education = factor(sample(c("High School", "Some College", "Bachelor", "Graduate"), 
                             300, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))),
    marital_status = factor(sample(c("Single", "Married", "Divorced", "Widowed"), 
                                  300, replace = TRUE, prob = c(0.4, 0.45, 0.1, 0.05))),
    
    # Psychological measures (realistic correlations)
    stress_baseline = rnorm(300, 50, 15),
    anxiety_baseline = rnorm(300, 45, 12),
    depression_baseline = rnorm(300, 40, 18)
) %>%
    mutate(
        # Correlated measures
        stress_score = pmax(0, pmin(100, stress_baseline + 
            0.3 * anxiety_baseline + 
            0.2 * depression_baseline + 
            rnorm(300, 0, 8))),
        
        anxiety_score = pmax(0, pmin(100, anxiety_baseline + 
            0.4 * stress_baseline + 
            0.25 * depression_baseline + 
            rnorm(300, 0, 7))),
        
        depression_score = pmax(0, pmin(100, depression_baseline + 
            0.3 * stress_baseline + 
            0.35 * anxiety_baseline + 
            rnorm(300, 0, 10))),
        
        # Wellbeing as outcome
        wellbeing_score = pmax(0, pmin(100, 80 - 
            0.4 * stress_score - 
            0.3 * anxiety_score - 
            0.5 * depression_score + 
            ifelse(marital_status == "Married", 5, 0) + 
            rnorm(300, 0, 12))),
        
        # Binary clinical indicator
        clinical_depression = ifelse(depression_score > 60, 1, 0),
        needs_intervention = ifelse(stress_score > 70 | anxiety_score > 65, 1, 0)
    )

# 4. Economic/Business data for various model types
jsjplot_economics <- data.frame(
    company_id = 1:200,
    
    # Company characteristics
    company_size = factor(sample(c("Small", "Medium", "Large"), 200, replace = TRUE, prob = c(0.5, 0.3, 0.2))),
    industry = factor(sample(c("Tech", "Manufacturing", "Services", "Healthcare", "Finance"), 
                            200, replace = TRUE)),
    years_operation = round(runif(200, 1, 50)),
    location = factor(sample(c("Urban", "Suburban", "Rural"), 200, replace = TRUE, prob = c(0.6, 0.3, 0.1))),
    
    # Financial variables
    initial_investment = exp(rnorm(200, mean = 12, sd = 1.5)), # Log-normal
    annual_revenue = exp(rnorm(200, mean = 14, sd = 1.2)),
    employee_count = round(exp(rnorm(200, mean = 4, sd = 1.3))),
    
    # Performance metrics
    customer_satisfaction = round(rnorm(200, 7.5, 1.2), 1),
    innovation_score = round(rnorm(200, 6.8, 1.5), 1),
    
    # Binary outcomes
    profitable = rbinom(200, 1, 0.7),
    expanded_last_year = rbinom(200, 1, 0.3),
    
    # Count outcomes
    new_products = rpois(200, 2),
    partnerships = rpois(200, 1.5)
) %>%
    mutate(
        # Create realistic relationships
        annual_revenue = annual_revenue * 
            ifelse(company_size == "Small", 0.3, 
                   ifelse(company_size == "Medium", 0.7, 1.2)) *
            (1 + 0.02 * years_operation),
        
        employee_count = employee_count * 
            ifelse(company_size == "Small", 0.2, 
                   ifelse(company_size == "Medium", 0.6, 1.5)),
        
        # Profitability depends on efficiency
        profitable = rbinom(200, 1, plogis(-1 + 
            0.0000001 * annual_revenue + 
            0.3 * customer_satisfaction + 
            0.2 * innovation_score - 
            0.00001 * employee_count))
    )

# 5. Medical/Clinical trial data
jsjplot_clinical <- data.frame(
    patient_id = 1:400,
    
    # Patient demographics
    age = round(rnorm(400, 55, 15)),
    gender = factor(sample(c("Male", "Female"), 400, replace = TRUE)),
    bmi = rnorm(400, 26, 4),
    smoking_status = factor(sample(c("Never", "Former", "Current"), 
                                  400, replace = TRUE, prob = c(0.5, 0.3, 0.2))),
    
    # Baseline measures
    systolic_bp = round(rnorm(400, 130, 20)),
    diastolic_bp = round(rnorm(400, 85, 10)),
    cholesterol = rnorm(400, 200, 30),
    glucose = rnorm(400, 100, 25),
    
    # Treatment assignment
    treatment_arm = factor(sample(c("Placebo", "Low_Dose", "High_Dose"), 
                                 400, replace = TRUE, prob = c(0.33, 0.33, 0.34))),
    study_site = factor(sample(c("Site_A", "Site_B", "Site_C"), 400, replace = TRUE)),
    
    # Comorbidities
    diabetes = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.8, 0.2))),
    heart_disease = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.85, 0.15))),
    
    # Continuous outcomes
    quality_of_life = rnorm(400, 70, 15),
    pain_score = round(rnorm(400, 4, 2.5)),
    
    # Binary outcomes
    treatment_response = rbinom(400, 1, 0.4),
    adverse_event = rbinom(400, 1, 0.15),
    
    # Count outcomes
    hospitalizations = rpois(400, 0.8),
    medication_changes = rpois(400, 1.2)
) %>%
    mutate(
        # Create realistic medical relationships
        systolic_bp = systolic_bp + 0.5 * age + 2 * bmi + 
            ifelse(smoking_status == "Current", 10, 0),
        
        cholesterol = cholesterol + 0.3 * age + 1.5 * bmi,
        
        glucose = glucose + 0.2 * age + 0.8 * bmi + 
            ifelse(diabetes == "Yes", 40, 0),
        
        # Treatment response depends on multiple factors
        treatment_response = rbinom(400, 1, plogis(-1.5 + 
            ifelse(treatment_arm == "Low_Dose", 0.8, 0) + 
            ifelse(treatment_arm == "High_Dose", 1.5, 0) - 
            0.01 * age + 
            ifelse(diabetes == "Yes", -0.5, 0))),
        
        # Quality of life outcome
        quality_of_life = pmax(0, pmin(100, quality_of_life + 
            ifelse(treatment_response == 1, 15, 0) - 
            0.2 * age - 
            pain_score * 3 + 
            ifelse(adverse_event == 1, -10, 0)))
    )

# 6. High-dimensional correlation/PCA data
jsjplot_highdim <- data.frame(
    sample_id = 1:150,
    replicate(15, rnorm(150))
) %>%
    setNames(c("sample_id", paste0("measure_", 1:15))) %>%
    mutate(
        # Create factor structure
        factor1 = measure_1 + measure_2 + measure_3 + rnorm(150, 0, 0.5),
        factor2 = measure_4 + measure_5 + measure_6 + rnorm(150, 0, 0.5),
        factor3 = measure_7 + measure_8 + measure_9 + rnorm(150, 0, 0.5),
        
        # Group variable for coloring
        cluster = factor(kmeans(.[paste0("measure_", 1:15)], centers = 3)$cluster),
        
        # Overall composite score
        total_score = rowSums(.[paste0("measure_", 1:15)]) / 15
    )

# 7. Interaction effects data
jsjplot_interactions <- data.frame(
    subject_id = 1:250,
    
    # Main effect variables
    dosage = rnorm(250, 5, 1.5),
    baseline_severity = rnorm(250, 6, 2),
    
    # Categorical moderators
    genotype = factor(sample(c("Type_A", "Type_B"), 250, replace = TRUE)),
    age_group = factor(sample(c("Young", "Middle", "Older"), 250, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
    
    # Additional variables
    gender = factor(sample(c("Male", "Female"), 250, replace = TRUE)),
    comorbidity = factor(sample(c("None", "Mild", "Severe"), 250, replace = TRUE, prob = c(0.6, 0.3, 0.1)))
) %>%
    mutate(
        # Outcome with interaction effects
        treatment_effect = 10 + 
            2 * dosage + 
            1.5 * baseline_severity + 
            # Interaction: dosage effect depends on genotype
            ifelse(genotype == "Type_B", 1.5 * dosage, 0) + 
            # Interaction: baseline severity effect depends on age
            ifelse(age_group == "Older", 0.8 * baseline_severity, 0) + 
            rnorm(250, 0, 3),
        
        # Binary outcome with interactions
        response = rbinom(250, 1, plogis(-2 + 
            0.3 * dosage + 
            0.2 * baseline_severity + 
            ifelse(genotype == "Type_B" & dosage > 5, 1.2, 0)))
    )

# 8. Frequency/Categorical analysis data
jsjplot_categorical <- data.frame(
    respondent_id = 1:600,
    
    # Demographics
    age_category = factor(sample(c("18-25", "26-35", "36-50", "51-65", "65+"), 
                                600, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.2, 0.15))),
    education = factor(sample(c("Less than HS", "High School", "Some College", "Bachelor", "Graduate"), 
                             600, replace = TRUE, prob = c(0.1, 0.25, 0.25, 0.25, 0.15))),
    income_bracket = factor(sample(c("Under 25k", "25k-50k", "50k-75k", "75k-100k", "Over 100k"), 
                                  600, replace = TRUE, prob = c(0.2, 0.25, 0.25, 0.15, 0.15))),
    
    # Attitudes and preferences
    political_affiliation = factor(sample(c("Conservative", "Moderate", "Liberal", "Other"), 
                                         600, replace = TRUE, prob = c(0.3, 0.35, 0.3, 0.05))),
    satisfaction_level = factor(sample(c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied"), 
                                      600, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15))),
    
    # Behavioral variables
    usage_frequency = factor(sample(c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                                   600, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.3, 0.2))),
    brand_preference = factor(sample(c("Brand_A", "Brand_B", "Brand_C", "No_Preference"), 
                                    600, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))),
    
    # Binary choices
    will_recommend = factor(sample(c("No", "Yes"), 600, replace = TRUE, prob = c(0.3, 0.7))),
    owns_product = factor(sample(c("No", "Yes"), 600, replace = TRUE, prob = c(0.4, 0.6))),
    
    # Ordinal scales
    importance_rating = factor(sample(1:7, 600, replace = TRUE), levels = 1:7, ordered = TRUE),
    likelihood_purchase = factor(sample(1:5, 600, replace = TRUE), levels = 1:5, ordered = TRUE)
)

# 9. Missing data scenarios
jsjplot_missing <- jsjplot_regression
# Introduce different missing data patterns
jsjplot_missing$outcome_continuous[sample(1:500, 50)] <- NA  # Missing completely at random
jsjplot_missing$age[sample(1:500, 30)] <- NA  # Random missing
# Missing not at random: higher income people less likely to report income
high_income_indices <- which(jsjplot_missing$income > quantile(jsjplot_missing$income, 0.8, na.rm = TRUE))
jsjplot_missing$income[sample(high_income_indices, length(high_income_indices) * 0.3)] <- NA

# 10. Edge cases and extreme scenarios
jsjplot_edge_cases <- data.frame(
    id = 1:100,
    
    # Perfect correlations
    x1 = rnorm(100),
    x2 = NA, # Will be set to x1 (perfect correlation)
    
    # Constant variables
    constant_var = rep(5, 100),
    
    # Highly skewed variables
    skewed_positive = exp(rnorm(100, 0, 2)),
    skewed_negative = -exp(rnorm(100, 0, 2)),
    
    # Binary with extreme distributions
    rare_event = rbinom(100, 1, 0.05), # Very rare
    common_event = rbinom(100, 1, 0.95), # Very common
    
    # Outlier-prone variables
    with_outliers = c(rnorm(95), rnorm(5, 100, 5)), # 5 extreme outliers
    
    # Categorical with unbalanced levels
    unbalanced_factor = factor(sample(c("Common", "Rare1", "Rare2"), 100, 
                                     replace = TRUE, prob = c(0.9, 0.05, 0.05))),
    
    # Outcome for testing
    outcome = rnorm(100, 10, 3)
)
jsjplot_edge_cases$x2 <- jsjplot_edge_cases$x1  # Perfect correlation

# Create comprehensive test descriptions
test_descriptions <- data.frame(
    dataset_name = c(
        "jsjplot_regression",
        "jsjplot_longitudinal", 
        "jsjplot_psychology",
        "jsjplot_economics",
        "jsjplot_clinical",
        "jsjplot_highdim",
        "jsjplot_interactions",
        "jsjplot_categorical",
        "jsjplot_missing",
        "jsjplot_edge_cases"
    ),
    description = c(
        "Basic regression data with multiple outcome types - 500 observations",
        "Longitudinal panel data for mixed effects models - 500 observations (100 subjects × 5 time points)",
        "Psychology survey data with correlated scales - 300 participants",
        "Economic/business data for various model types - 200 companies",
        "Medical/clinical trial data with realistic relationships - 400 patients",
        "High-dimensional data for correlation and PCA analysis - 150 samples with 15+ measures",
        "Interaction effects data for moderation analysis - 250 subjects",
        "Categorical/frequency analysis data with multiple factors - 600 respondents",
        "Regression data with various missing data patterns - 500 observations with NAs",
        "Edge cases and extreme scenarios for robustness testing - 100 observations"
    ),
    use_case = c(
        "Linear, logistic, and Poisson regression testing",
        "Mixed effects and longitudinal modeling",
        "Psychology research with correlated measures",
        "Business analytics and economic modeling",
        "Clinical research and medical data analysis",
        "Multivariate analysis, PCA, correlation matrices",
        "Interaction and moderation effect testing",
        "Frequency tables and categorical data analysis",
        "Missing data handling and robustness",
        "Edge case testing and error handling"
    ),
    analysis_types = c(
        "regression_table, coefficient_plot, marginal_effects",
        "regression_table, coefficient_plot (lmer when implemented)",
        "correlation_matrix, pca_plot, regression_table",
        "coefficient_plot, regression_table, correlation_matrix",
        "coefficient_plot, regression_table, interaction_plot",
        "correlation_matrix, pca_plot",
        "interaction_plot, marginal_effects, coefficient_plot",
        "frequency_table",
        "All analysis types with missing data",
        "Error handling and robustness testing"
    ),
    observations = c(500, 500, 300, 200, 400, 150, 250, 600, 500, 100),
    variables = c(16, 8, 12, 13, 18, 20, 8, 12, 16, 11)
)

# Print summary
cat("Created", nrow(test_descriptions), "test datasets for jsjplot function:\n\n")
print(test_descriptions)

cat("\n\nDataset memory usage summary:\n")
for (dataset_name in test_descriptions$dataset_name) {
    if (exists(dataset_name)) {
        obj_size <- format(object.size(get(dataset_name)), units = "KB")
        cat(sprintf("%-25s: %s\n", dataset_name, obj_size))
    }
}

cat("\n\nExample usage for different analysis types:\n\n")

cat("# Coefficient Plot\n")
cat("result <- jsjplot(\n")
cat("    data = jsjplot_regression,\n")
cat("    analysis_type = 'coefficient_plot',\n")
cat("    dependent_var = 'outcome_continuous',\n")
cat("    independent_vars = c('age', 'education_years', 'gender')\n")
cat(")\n\n")

cat("# Interaction Plot\n")
cat("result <- jsjplot(\n")
cat("    data = jsjplot_interactions,\n")
cat("    analysis_type = 'interaction_plot',\n")
cat("    dependent_var = 'treatment_effect',\n")
cat("    interaction_vars = c('dosage', 'genotype')\n")
cat(")\n\n")

cat("# Frequency Table\n")
cat("result <- jsjplot(\n")
cat("    data = jsjplot_categorical,\n")
cat("    analysis_type = 'frequency_table'\n")
cat(")\n\n")

cat("# Correlation Matrix\n")
cat("result <- jsjplot(\n")
cat("    data = jsjplot_highdim,\n")
cat("    analysis_type = 'correlation_matrix'\n")
cat(")\n\n")

cat("# PCA Plot\n")
cat("result <- jsjplot(\n")
cat("    data = jsjplot_highdim,\n")
cat("    analysis_type = 'pca_plot'\n")
cat(")\n\n")

# Save datasets if requested
if (exists("save_datasets") && save_datasets) {
    save_dir <- "data"
    if (!dir.exists(save_dir)) dir.create(save_dir)
    
    datasets_to_save <- list(
        jsjplot_regression = jsjplot_regression,
        jsjplot_longitudinal = jsjplot_longitudinal,
        jsjplot_psychology = jsjplot_psychology,
        jsjplot_economics = jsjplot_economics,
        jsjplot_clinical = jsjplot_clinical,
        jsjplot_highdim = jsjplot_highdim,
        jsjplot_interactions = jsjplot_interactions,
        jsjplot_categorical = jsjplot_categorical,
        jsjplot_missing = jsjplot_missing,
        jsjplot_edge_cases = jsjplot_edge_cases,
        jsjplot_test_descriptions = test_descriptions
    )
    
    save(list = names(datasets_to_save), 
         file = file.path(save_dir, "jsjplot_test_data.RData"))
    
    cat("\nTest datasets saved to:", file.path(save_dir, "jsjplot_test_data.RData"), "\n")
}

# Additional utility functions for testing
create_interaction_data <- function(n = 200) {
    # Helper function to create interaction data on demand
    data.frame(
        x1 = rnorm(n),
        x2 = rnorm(n),
        group = factor(sample(c("A", "B"), n, replace = TRUE)),
        y = rnorm(n, 0, 1)
    ) %>%
        mutate(y = y + x1 + x2 + ifelse(group == "B", x1 * 2, 0))
}

create_clinical_trial_data <- function(n = 300) {
    # Helper function for clinical trial simulation
    data.frame(
        treatment = factor(sample(c("Placebo", "Active"), n, replace = TRUE)),
        age = round(rnorm(n, 65, 10)),
        baseline_score = rnorm(n, 50, 15),
        outcome_score = NA
    ) %>%
        mutate(
            outcome_score = baseline_score + 
                ifelse(treatment == "Active", 8, 2) + 
                0.1 * age + 
                rnorm(n, 0, 10)
        )
}

cat("\nUtility functions created:\n")
cat("- create_interaction_data(n): Generate interaction data\n")
cat("- create_clinical_trial_data(n): Generate clinical trial data\n")

# Clean up workspace
rm(list = setdiff(ls(), c(test_descriptions$dataset_name, "test_descriptions", 
                          "create_interaction_data", "create_clinical_trial_data")))
gc()