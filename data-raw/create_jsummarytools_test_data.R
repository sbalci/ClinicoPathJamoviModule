# Create test data for jsummarytools function
# Comprehensive descriptive statistics using summarytools package

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(123)

# 1. Basic descriptive data with mixed variable types
jsummarytools_basic <- data.frame(
    participant_id = 1:300,
    
    # Continuous variables with different distributions
    age = round(rnorm(300, 45, 15)),
    income = exp(rnorm(300, 10.5, 0.8)), # Log-normal distribution
    bmi = rnorm(300, 25, 4),
    blood_pressure = rnorm(300, 120, 20),
    cholesterol = rnorm(300, 200, 40),
    
    # Discrete numeric variables
    years_education = round(rnorm(300, 14, 3)),
    children_count = rpois(300, 1.8),
    hospital_visits = rpois(300, 2),
    
    # Categorical variables
    gender = factor(sample(c("Male", "Female"), 300, replace = TRUE, prob = c(0.48, 0.52))),
    marital_status = factor(sample(c("Single", "Married", "Divorced", "Widowed"), 
                                  300, replace = TRUE, prob = c(0.35, 0.5, 0.1, 0.05))),
    education_level = factor(sample(c("High School", "Some College", "Bachelor", "Master", "PhD"), 
                                   300, replace = TRUE, prob = c(0.2, 0.25, 0.3, 0.2, 0.05))),
    employment_status = factor(sample(c("Employed", "Unemployed", "Retired", "Student"), 
                                     300, replace = TRUE, prob = c(0.6, 0.15, 0.2, 0.05))),
    
    # Binary variables  
    smoker = factor(sample(c("No", "Yes"), 300, replace = TRUE, prob = c(0.75, 0.25))),
    diabetes = factor(sample(c("No", "Yes"), 300, replace = TRUE, prob = c(0.85, 0.15))),
    
    # Ordinal variables
    health_rating = factor(sample(1:5, 300, replace = TRUE, prob = c(0.05, 0.15, 0.3, 0.35, 0.15)), 
                          levels = 1:5, labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"), ordered = TRUE),
    satisfaction = factor(sample(1:7, 300, replace = TRUE), levels = 1:7, ordered = TRUE),
    
    # Text/character variables
    city = sample(c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia"), 
                  300, replace = TRUE),
    
    # Logical variables
    insurance = sample(c(TRUE, FALSE), 300, replace = TRUE, prob = c(0.8, 0.2))
) %>%
    mutate(
        # Create realistic relationships
        bmi = pmax(15, pmin(50, bmi + ifelse(age > 50, 2, 0) + ifelse(gender == "Male", 1, 0))),
        blood_pressure = pmax(80, blood_pressure + 0.5 * age + 0.3 * bmi + ifelse(smoker == "Yes", 10, 0)),
        cholesterol = pmax(120, cholesterol + 0.3 * age + 0.5 * bmi),
        income = income * ifelse(education_level %in% c("Master", "PhD"), 1.5, 1) * 
                 ifelse(employment_status == "Employed", 1, 0.3)
    )

# 2. Survey data with Likert scales and multiple choice
jsummarytools_survey <- data.frame(
    respondent_id = 1:500,
    
    # Demographics
    age_group = factor(sample(c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"), 
                             500, replace = TRUE, prob = c(0.15, 0.2, 0.2, 0.2, 0.15, 0.1))),
    gender = factor(sample(c("Male", "Female", "Non-binary", "Prefer not to say"), 
                          500, replace = TRUE, prob = c(0.45, 0.48, 0.04, 0.03))),
    education = factor(sample(c("Less than HS", "High School", "Some College", "Bachelor", "Graduate"), 
                             500, replace = TRUE, prob = c(0.08, 0.25, 0.27, 0.25, 0.15))),
    income_bracket = factor(sample(c("Under $25k", "$25k-$50k", "$50k-$75k", "$75k-$100k", "Over $100k"), 
                                  500, replace = TRUE, prob = c(0.2, 0.25, 0.25, 0.15, 0.15))),
    
    # Likert scale responses (1-5)
    satisfaction_service = sample(1:5, 500, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.45, 0.2)),
    satisfaction_price = sample(1:5, 500, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15)),
    satisfaction_quality = sample(1:5, 500, replace = TRUE, prob = c(0.03, 0.07, 0.2, 0.5, 0.2)),
    
    # Agreement scales (1-7)
    agree_recommend = sample(1:7, 500, replace = TRUE, prob = c(0.05, 0.05, 0.1, 0.15, 0.2, 0.25, 0.2)),
    agree_value = sample(1:7, 500, replace = TRUE, prob = c(0.08, 0.07, 0.12, 0.18, 0.25, 0.2, 0.1)),
    
    # Frequency responses
    usage_frequency = factor(sample(c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                                   500, replace = TRUE, prob = c(0.05, 0.15, 0.3, 0.35, 0.15))),
    
    # Multiple choice preferences
    preferred_contact = factor(sample(c("Email", "Phone", "Text", "Mail", "In-person"), 
                                     500, replace = TRUE, prob = c(0.4, 0.25, 0.2, 0.05, 0.1))),
    product_category = factor(sample(c("Electronics", "Clothing", "Books", "Home", "Sports", "Other"), 
                                    500, replace = TRUE)),
    
    # Binary choices
    will_recommend = factor(sample(c("No", "Yes"), 500, replace = TRUE, prob = c(0.25, 0.75))),
    return_customer = factor(sample(c("No", "Yes"), 500, replace = TRUE, prob = c(0.3, 0.7))),
    
    # Numeric ratings
    overall_rating = round(rnorm(500, 4.2, 0.8), 1),
    price_rating = round(rnorm(500, 3.8, 0.9), 1),
    
    # Open numeric
    years_customer = round(runif(500, 0, 10), 1),
    monthly_spending = round(exp(rnorm(500, 4.5, 0.8)), 2)
) %>%
    mutate(
        # Constrain ratings to realistic ranges
        overall_rating = pmax(1, pmin(5, overall_rating)),
        price_rating = pmax(1, pmin(5, price_rating))
    )

# 3. Clinical/Medical data with detailed measurements
jsummarytools_clinical <- data.frame(
    patient_id = paste0("PT_", sprintf("%03d", 1:400)),
    
    # Demographics
    age = round(rnorm(400, 58, 16)),
    gender = factor(sample(c("Male", "Female"), 400, replace = TRUE)),
    race_ethnicity = factor(sample(c("White", "Black", "Hispanic", "Asian", "Other"), 
                                  400, replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.08, 0.02))),
    
    # Vital signs
    height_cm = round(rnorm(400, 170, 12)),
    weight_kg = round(rnorm(400, 75, 15)),
    systolic_bp = round(rnorm(400, 130, 20)),
    diastolic_bp = round(rnorm(400, 80, 12)),
    heart_rate = round(rnorm(400, 72, 12)),
    temperature_c = round(rnorm(400, 36.8, 0.5), 1),
    
    # Lab values
    glucose_mg_dl = round(rnorm(400, 95, 25)),
    cholesterol_mg_dl = round(rnorm(400, 190, 40)),
    hdl_mg_dl = round(rnorm(400, 55, 15)),
    ldl_mg_dl = round(rnorm(400, 115, 35)),
    triglycerides_mg_dl = round(rnorm(400, 140, 50)),
    creatinine_mg_dl = round(rnorm(400, 1.0, 0.3), 2),
    
    # Clinical categories
    smoking_status = factor(sample(c("Never", "Former", "Current"), 
                                  400, replace = TRUE, prob = c(0.5, 0.35, 0.15))),
    alcohol_use = factor(sample(c("None", "Light", "Moderate", "Heavy"), 
                               400, replace = TRUE, prob = c(0.3, 0.4, 0.25, 0.05))),
    
    # Medical history (binary)
    diabetes = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.8, 0.2))),
    hypertension = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.65, 0.35))),
    heart_disease = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.85, 0.15))),
    stroke = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.95, 0.05))),
    
    # Severity scores
    pain_score = sample(0:10, 400, replace = TRUE, prob = c(0.15, 0.1, 0.1, 0.12, 0.12, 0.1, 0.1, 0.08, 0.05, 0.03, 0.05)),
    mobility_score = sample(1:5, 400, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.05, 0.05)),
    
    # Counts
    medications_count = rpois(400, 3),
    hospitalizations_count = rpois(400, 0.5),
    
    # Treatment response
    treatment_group = factor(sample(c("Control", "Treatment_A", "Treatment_B"), 
                                   400, replace = TRUE, prob = c(0.33, 0.33, 0.34)))
) %>%
    mutate(
        # Calculate BMI
        bmi = round(weight_kg / (height_cm/100)^2, 1),
        
        # Create realistic medical relationships
        systolic_bp = pmax(90, systolic_bp + 0.3 * age + 0.5 * bmi + 
                          ifelse(diabetes == "Yes", 15, 0) + 
                          ifelse(smoking_status == "Current", 8, 0)),
        
        diastolic_bp = pmax(60, diastolic_bp + 0.15 * age + 0.3 * bmi),
        
        glucose_mg_dl = pmax(70, glucose_mg_dl + 
                            ifelse(diabetes == "Yes", 60, 0) + 
                            0.1 * age + 0.2 * bmi),
        
        cholesterol_mg_dl = pmax(120, cholesterol_mg_dl + 0.5 * age + 0.8 * bmi),
        
        # Blood pressure categories
        bp_category = factor(case_when(
            systolic_bp < 120 & diastolic_bp < 80 ~ "Normal",
            systolic_bp < 130 & diastolic_bp < 80 ~ "Elevated",
            systolic_bp < 140 | diastolic_bp < 90 ~ "Stage 1",
            TRUE ~ "Stage 2"
        ), levels = c("Normal", "Elevated", "Stage 1", "Stage 2")),
        
        # BMI categories
        bmi_category = factor(case_when(
            bmi < 18.5 ~ "Underweight",
            bmi < 25 ~ "Normal",
            bmi < 30 ~ "Overweight",
            TRUE ~ "Obese"
        ), levels = c("Underweight", "Normal", "Overweight", "Obese"))
    )

# 4. Educational data with test scores and grades
jsummarytools_education <- data.frame(
    student_id = paste0("STU_", sprintf("%03d", 1:250)),
    
    # Demographics
    age = round(rnorm(250, 20, 2.5)),
    gender = factor(sample(c("Male", "Female", "Non-binary"), 250, replace = TRUE, prob = c(0.45, 0.52, 0.03))),
    year_level = factor(sample(c("Freshman", "Sophomore", "Junior", "Senior"), 
                              250, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2))),
    major = factor(sample(c("Biology", "Chemistry", "Physics", "Mathematics", "Psychology", "English", "History"), 
                         250, replace = TRUE)),
    
    # Academic background
    high_school_gpa = round(rnorm(250, 3.3, 0.5), 2),
    sat_total = round(rnorm(250, 1200, 150)),
    act_score = round(rnorm(250, 25, 4)),
    
    # Current grades (0-100 scale)
    math_grade = round(rnorm(250, 78, 12)),
    science_grade = round(rnorm(250, 81, 11)),
    english_grade = round(rnorm(250, 83, 10)),
    history_grade = round(rnorm(250, 80, 13)),
    
    # Categorical grades
    math_letter = factor(sample(c("A", "B", "C", "D", "F"), 250, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.1, 0.05))),
    science_letter = factor(sample(c("A", "B", "C", "D", "F"), 250, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.1, 0.05))),
    
    # Study habits
    study_hours_week = round(rnorm(250, 15, 8)),
    classes_missed = rpois(250, 2),
    
    # Financial
    tuition_paid = factor(sample(c("Full", "Partial", "None"), 250, replace = TRUE, prob = c(0.4, 0.45, 0.15))),
    financial_aid = factor(sample(c("No", "Yes"), 250, replace = TRUE, prob = c(0.35, 0.65))),
    work_hours_week = round(rnorm(250, 10, 8)),
    
    # Extracurricular
    clubs_count = rpois(250, 1.5),
    sports_participation = factor(sample(c("No", "Yes"), 250, replace = TRUE, prob = c(0.7, 0.3))),
    
    # Academic status
    academic_probation = factor(sample(c("No", "Yes"), 250, replace = TRUE, prob = c(0.85, 0.15))),
    dean_list = factor(sample(c("No", "Yes"), 250, replace = TRUE, prob = c(0.75, 0.25)))
) %>%
    mutate(
        # Constrain values to realistic ranges
        high_school_gpa = pmax(0, pmin(4.0, high_school_gpa)),
        sat_total = pmax(400, pmin(1600, sat_total)),
        act_score = pmax(1, pmin(36, act_score)),
        math_grade = pmax(0, pmin(100, math_grade)),
        science_grade = pmax(0, pmin(100, science_grade)),
        english_grade = pmax(0, pmin(100, english_grade)),
        history_grade = pmax(0, pmin(100, history_grade)),
        study_hours_week = pmax(0, study_hours_week),
        work_hours_week = pmax(0, work_hours_week),
        
        # Calculate current GPA from grades
        current_gpa = round((math_grade + science_grade + english_grade + history_grade) / 100 * 4, 2),
        current_gpa = pmax(0, pmin(4.0, current_gpa))
    )

# 5. Missing data scenarios
jsummarytools_missing <- jsummarytools_basic
# Introduce various missing data patterns
set.seed(456)

# Missing completely at random (MCAR)
jsummarytools_missing$age[sample(1:300, 25)] <- NA
jsummarytools_missing$bmi[sample(1:300, 20)] <- NA

# Missing at random (MAR) - older people less likely to report income
older_indices <- which(jsummarytools_missing$age > 60)
jsummarytools_missing$income[sample(older_indices, length(older_indices) * 0.4)] <- NA

# Missing not at random (MNAR) - people with health issues less likely to report health rating
health_issues <- which(jsummarytools_missing$diabetes == "Yes" | jsummarytools_missing$smoker == "Yes")
jsummarytools_missing$health_rating[sample(health_issues, length(health_issues) * 0.3)] <- NA

# Systematic missingness - some variables missing for entire groups
unemployed_indices <- which(jsummarytools_missing$employment_status == "Unemployed")
jsummarytools_missing$children_count[sample(unemployed_indices, length(unemployed_indices) * 0.6)] <- NA

# 6. Small sample size data
jsummarytools_small <- jsummarytools_basic[1:25, ]

# 7. Cross-tabulation specific data
jsummarytools_crosstab <- data.frame(
    id = 1:200,
    
    # Two main categorical variables for cross-tabulation
    treatment = factor(sample(c("Placebo", "Drug_A", "Drug_B"), 200, replace = TRUE, prob = c(0.4, 0.3, 0.3))),
    outcome = factor(sample(c("Poor", "Fair", "Good", "Excellent"), 200, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25))),
    
    # Additional categorical variables
    gender = factor(sample(c("Male", "Female"), 200, replace = TRUE)),
    age_group = factor(sample(c("Young", "Middle", "Older"), 200, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
    severity = factor(sample(c("Mild", "Moderate", "Severe"), 200, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    
    # Weights for weighted analysis
    sample_weight = runif(200, 0.5, 2.0),
    
    # Additional variables
    days_treatment = round(rnorm(200, 30, 10)),
    side_effects = factor(sample(c("None", "Mild", "Moderate", "Severe"), 200, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05)))
) %>%
    mutate(
        # Create realistic relationships for cross-tabulation
        outcome = factor(case_when(
            treatment == "Placebo" ~ sample(c("Poor", "Fair", "Good", "Excellent"), 
                                          sum(treatment == "Placebo"), replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
            treatment == "Drug_A" ~ sample(c("Poor", "Fair", "Good", "Excellent"), 
                                         sum(treatment == "Drug_A"), replace = TRUE, prob = c(0.15, 0.25, 0.4, 0.2)),
            treatment == "Drug_B" ~ sample(c("Poor", "Fair", "Good", "Excellent"), 
                                         sum(treatment == "Drug_B"), replace = TRUE, prob = c(0.05, 0.15, 0.4, 0.4))
        ), levels = c("Poor", "Fair", "Good", "Excellent"))
    )

# Create comprehensive test descriptions
test_descriptions <- data.frame(
    dataset_name = c(
        "jsummarytools_basic",
        "jsummarytools_survey", 
        "jsummarytools_clinical",
        "jsummarytools_education",
        "jsummarytools_missing",
        "jsummarytools_small",
        "jsummarytools_crosstab"
    ),
    description = c(
        "Basic mixed-type data with continuous, categorical, and binary variables - 300 participants",
        "Survey data with Likert scales and multiple choice responses - 500 respondents", 
        "Clinical/medical data with detailed measurements and lab values - 400 patients",
        "Educational data with test scores, grades, and academic information - 250 students",
        "Basic data with various missing data patterns (MCAR, MAR, MNAR) - 300 participants",
        "Small sample size data for testing edge cases - 25 participants",
        "Cross-tabulation specific data with categorical relationships - 200 subjects"
    ),
    primary_use = c(
        "dfSummary, descr, freq testing with realistic mixed data",
        "freq analysis with Likert scales and survey responses",
        "Medical research descriptive statistics and clinical summaries",
        "Educational assessment and academic performance analysis",
        "Missing data handling and robustness testing",
        "Small sample size behavior and edge case testing", 
        "ctable cross-tabulation analysis with treatment outcomes"
    ),
    analysis_types = c(
        "dfsummary, descr, freq",
        "freq, dfsummary", 
        "dfsummary, descr, freq",
        "descr, dfsummary, freq",
        "All types with missing data",
        "All types with small n",
        "ctable, freq, dfsummary"
    ),
    observations = c(300, 500, 400, 250, 300, 25, 200),
    variables = c(20, 18, 28, 21, 20, 20, 8)
)

# Print summary
cat("Created", nrow(test_descriptions), "test datasets for jsummarytools function:\n\n")
print(test_descriptions)

cat("\n\nDataset memory usage summary:\n")
for (dataset_name in test_descriptions$dataset_name) {
    if (exists(dataset_name)) {
        obj_size <- format(object.size(get(dataset_name)), units = "KB")
        cat(sprintf("%-25s: %s\n", dataset_name, obj_size))
    }
}

cat("\n\nExample usage for different analysis types:\n\n")

cat("# Data Frame Summary\n")
cat("result <- jsummarytools(\n")
cat("    data = jsummarytools_basic,\n")
cat("    analysis_type = 'dfsummary',\n")
cat("    show_graphs = TRUE,\n")
cat("    show_labels = TRUE\n")
cat(")\n\n")

cat("# Frequency Tables\n")
cat("result <- jsummarytools(\n")
cat("    data = jsummarytools_survey,\n")
cat("    analysis_type = 'freq',\n")
cat("    vars = c('satisfaction_service', 'usage_frequency'),\n")
cat("    include_cumulative = TRUE\n")
cat(")\n\n")

cat("# Descriptive Statistics\n")
cat("result <- jsummarytools(\n")
cat("    data = jsummarytools_clinical,\n")
cat("    analysis_type = 'descr',\n")
cat("    vars = c('age', 'bmi', 'systolic_bp', 'cholesterol_mg_dl'),\n")
cat("    stats_to_include = 'all'\n")
cat(")\n\n")

cat("# Cross-tabulation\n")
cat("result <- jsummarytools(\n")
cat("    data = jsummarytools_crosstab,\n")
cat("    analysis_type = 'ctable',\n")
cat("    cross_var1 = 'treatment',\n")
cat("    cross_var2 = 'outcome',\n")
cat("    cross_proportions = 'row'\n")
cat(")\n\n")

cat("# Grouped Analysis\n")
cat("result <- jsummarytools(\n")
cat("    data = jsummarytools_clinical,\n")
cat("    analysis_type = 'descr',\n")
cat("    vars = c('systolic_bp', 'diastolic_bp'),\n")
cat("    group_var = 'gender'\n")
cat(")\n\n")

# Save datasets if requested
if (exists("save_datasets") && save_datasets) {
    save_dir <- "data"
    if (!dir.exists(save_dir)) dir.create(save_dir)
    
    datasets_to_save <- list(
        jsummarytools_basic = jsummarytools_basic,
        jsummarytools_survey = jsummarytools_survey,
        jsummarytools_clinical = jsummarytools_clinical,
        jsummarytools_education = jsummarytools_education,
        jsummarytools_missing = jsummarytools_missing,
        jsummarytools_small = jsummarytools_small,
        jsummarytools_crosstab = jsummarytools_crosstab,
        jsummarytools_test_descriptions = test_descriptions
    )
    
    save(list = names(datasets_to_save), 
         file = file.path(save_dir, "jsummarytools_test_data.RData"))
    
    cat("Test datasets saved to:", file.path(save_dir, "jsummarytools_test_data.RData"), "\n")
}

# Additional utility functions for testing
create_likert_data <- function(n = 100, scales = 5) {
    # Helper function to create Likert scale data
    data.frame(
        id = 1:n,
        replicate(scales, sample(1:5, n, replace = TRUE))
    ) %>%
        setNames(c("id", paste0("scale_", 1:scales)))
}

create_medical_data <- function(n = 200) {
    # Helper function for medical research data
    data.frame(
        patient_id = 1:n,
        age = round(rnorm(n, 60, 15)),
        gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
        treatment = factor(sample(c("Control", "Treatment"), n, replace = TRUE)),
        outcome_score = rnorm(n, 50, 15),
        adverse_events = rpois(n, 0.5)
    ) %>%
        mutate(
            outcome_score = outcome_score + ifelse(treatment == "Treatment", 10, 0) + rnorm(n, 0, 8)
        )
}

cat("Utility functions created:\n")
cat("- create_likert_data(n, scales): Generate Likert scale survey data\n")
cat("- create_medical_data(n): Generate medical research data\n")

# Clean up workspace
rm(list = setdiff(ls(), c(test_descriptions$dataset_name, "test_descriptions", 
                          "create_likert_data", "create_medical_data")))
gc()
