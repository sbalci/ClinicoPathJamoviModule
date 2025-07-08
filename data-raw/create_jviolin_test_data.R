# Create test data for jviolin function
# Professional violin plots for distribution visualization

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# 1. Basic violin plot data with different distributions
jviolin_basic <- data.frame(
    participant_id = 1:300,
    
    # Different distribution types for testing
    normal_dist = rnorm(300, 50, 15),
    skewed_right = exp(rnorm(300, 3, 0.5)),
    skewed_left = 100 - exp(rnorm(300, 3, 0.5)),
    bimodal_dist = c(rnorm(150, 30, 8), rnorm(150, 70, 8)),
    uniform_dist = runif(300, 20, 80),
    exponential_dist = rexp(300, 0.1),
    
    # Different group structures
    group_2 = factor(sample(c("Control", "Treatment"), 300, replace = TRUE)),
    group_3 = factor(sample(c("Low", "Medium", "High"), 300, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
    group_4 = factor(sample(c("A", "B", "C", "D"), 300, replace = TRUE)),
    group_5 = factor(sample(c("Group1", "Group2", "Group3", "Group4", "Group5"), 
                           300, replace = TRUE, prob = c(0.15, 0.25, 0.3, 0.2, 0.1))),
    
    # Additional categorical variables for fill/color mapping
    gender = factor(sample(c("Male", "Female"), 300, replace = TRUE)),
    age_category = factor(sample(c("Young", "Middle", "Older"), 300, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    treatment_type = factor(sample(c("Drug_A", "Drug_B", "Placebo"), 300, replace = TRUE)),
    
    # Ordinal variables
    severity = factor(sample(1:5, 300, replace = TRUE), levels = 1:5, ordered = TRUE),
    satisfaction = factor(sample(1:7, 300, replace = TRUE), levels = 1:7, ordered = TRUE)
) %>%
    mutate(
        # Create realistic relationships between variables
        normal_dist = normal_dist + 
            ifelse(group_3 == "Low", -10, ifelse(group_3 == "High", 10, 0)) +
            ifelse(gender == "Male", 5, -5) +
            rnorm(300, 0, 5),
        
        # Make bimodal distribution group-dependent
        bimodal_dist = case_when(
            group_2 == "Control" ~ rnorm(300, 35, 10),
            group_2 == "Treatment" ~ c(rnorm(150, 30, 8), rnorm(150, 65, 8))
        )
    )

# 2. Clinical trial data with realistic medical measurements
jviolin_clinical <- data.frame(
    patient_id = paste0("PT_", sprintf("%03d", 1:250)),
    
    # Treatment groups
    treatment_arm = factor(sample(c("Placebo", "Low_Dose", "High_Dose"), 
                                 250, replace = TRUE, prob = c(0.35, 0.35, 0.3))),
    study_site = factor(sample(c("Site_A", "Site_B", "Site_C"), 250, replace = TRUE)),
    
    # Patient characteristics
    age = round(rnorm(250, 55, 15)),
    gender = factor(sample(c("Male", "Female"), 250, replace = TRUE)),
    baseline_severity = factor(sample(c("Mild", "Moderate", "Severe"), 
                                     250, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    
    # Baseline measurements
    baseline_score = rnorm(250, 45, 12),
    blood_pressure = rnorm(250, 130, 20),
    heart_rate = rnorm(250, 75, 12),
    
    # Laboratory values
    cholesterol = rnorm(250, 200, 40),
    glucose = rnorm(250, 100, 25),
    creatinine = rnorm(250, 1.0, 0.3),
    
    # Quality of life scores
    physical_function = rnorm(250, 60, 20),
    mental_health = rnorm(250, 65, 18),
    pain_score = round(rnorm(250, 4, 2.5)),
    
    # Binary outcomes
    response_binary = factor(sample(c("Responder", "Non-responder"), 250, replace = TRUE, prob = c(0.4, 0.6))),
    adverse_event = factor(sample(c("No", "Yes"), 250, replace = TRUE, prob = c(0.7, 0.3)))
) %>%
    mutate(
        # Create realistic dose-response relationships
        change_from_baseline = case_when(
            treatment_arm == "Placebo" ~ rnorm(250, 2, 8),
            treatment_arm == "Low_Dose" ~ rnorm(250, 8, 10),
            treatment_arm == "High_Dose" ~ rnorm(250, 15, 12)
        ) + 
            # Add baseline severity effect
            case_when(
                baseline_severity == "Mild" ~ rnorm(250, 5, 5),
                baseline_severity == "Moderate" ~ rnorm(250, 0, 5),
                baseline_severity == "Severe" ~ rnorm(250, -5, 5)
            ) +
            # Add age effect
            -0.1 * age + rnorm(250, 0, 6),
        
        # Quality of life improvement
        qol_improvement = case_when(
            treatment_arm == "Placebo" ~ rnorm(250, 5, 15),
            treatment_arm == "Low_Dose" ~ rnorm(250, 12, 18),
            treatment_arm == "High_Dose" ~ rnorm(250, 20, 20)
        ) + 
            ifelse(gender == "Female", 3, 0) +
            rnorm(250, 0, 10),
        
        # Side effect score (higher with higher dose)
        side_effect_score = case_when(
            treatment_arm == "Placebo" ~ rpois(250, 0.5),
            treatment_arm == "Low_Dose" ~ rpois(250, 1.2),
            treatment_arm == "High_Dose" ~ rpois(250, 2.1)
        )
    )

# 3. Educational data with test scores and performance measures
jviolin_education <- data.frame(
    student_id = paste0("STU_", sprintf("%03d", 1:400)),
    
    # School characteristics
    school_type = factor(sample(c("Public", "Private", "Charter"), 400, replace = TRUE, prob = c(0.6, 0.25, 0.15))),
    grade_level = factor(sample(c("9th", "10th", "11th", "12th"), 400, replace = TRUE)),
    class_size = factor(sample(c("Small", "Medium", "Large"), 400, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    
    # Student demographics
    gender = factor(sample(c("Male", "Female", "Non-binary"), 400, replace = TRUE, prob = c(0.48, 0.5, 0.02))),
    ethnicity = factor(sample(c("White", "Hispanic", "Black", "Asian", "Other"), 
                             400, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.08, 0.02))),
    socioeconomic = factor(sample(c("Low", "Middle", "High"), 400, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    
    # Intervention groups
    teaching_method = factor(sample(c("Traditional", "Technology", "Hybrid"), 400, replace = TRUE)),
    tutoring = factor(sample(c("No", "Yes"), 400, replace = TRUE, prob = c(0.7, 0.3))),
    
    # Test scores (normally distributed with realistic means)
    math_score = rnorm(400, 75, 15),
    reading_score = rnorm(400, 78, 13),
    science_score = rnorm(400, 72, 14),
    writing_score = rnorm(400, 74, 12),
    
    # Standardized test scores
    sat_math = rnorm(400, 520, 80),
    sat_verbal = rnorm(400, 510, 75),
    
    # Performance measures
    gpa = rnorm(400, 3.2, 0.6),
    attendance_rate = rnorm(400, 92, 8),
    
    # Engagement measures
    study_hours_week = round(rnorm(400, 8, 4)),
    extracurricular = factor(sample(c("None", "Some", "Many"), 400, replace = TRUE, prob = c(0.3, 0.5, 0.2)))
) %>%
    mutate(
        # Create realistic educational relationships
        math_score = pmax(0, pmin(100, math_score + 
            case_when(
                teaching_method == "Traditional" ~ rnorm(400, 0, 5),
                teaching_method == "Technology" ~ rnorm(400, 5, 5),
                teaching_method == "Hybrid" ~ rnorm(400, 8, 5)
            ) +
            ifelse(tutoring == "Yes", 10, 0) +
            case_when(
                socioeconomic == "Low" ~ rnorm(400, -8, 3),
                socioeconomic == "Middle" ~ rnorm(400, 0, 3),
                socioeconomic == "High" ~ rnorm(400, 8, 3)
            ))),
        
        # GPA related to scores
        gpa = pmax(0, pmin(4.0, gpa + 
            0.01 * (math_score + reading_score + science_score) / 3 - 0.75 +
            rnorm(400, 0, 0.3))),
        
        # Constrain other scores
        reading_score = pmax(0, pmin(100, reading_score)),
        science_score = pmax(0, pmin(100, science_score)),
        writing_score = pmax(0, pmin(100, writing_score)),
        attendance_rate = pmax(60, pmin(100, attendance_rate)),
        study_hours_week = pmax(0, study_hours_week)
    )

# 4. Survey data with Likert scales and satisfaction measures
jviolin_survey <- data.frame(
    respondent_id = 1:350,
    
    # Demographics
    age_group = factor(sample(c("18-25", "26-35", "36-50", "51-65", "65+"), 
                             350, replace = TRUE, prob = c(0.2, 0.25, 0.25, 0.2, 0.1))),
    gender = factor(sample(c("Male", "Female", "Prefer not to say"), 
                          350, replace = TRUE, prob = c(0.45, 0.52, 0.03))),
    education = factor(sample(c("High School", "Some College", "Bachelor", "Graduate"), 
                             350, replace = TRUE, prob = c(0.25, 0.3, 0.3, 0.15))),
    income = factor(sample(c("Under 30k", "30k-60k", "60k-100k", "Over 100k"), 
                          350, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))),
    
    # Product/Service groups
    product_type = factor(sample(c("Basic", "Premium", "Enterprise"), 350, replace = TRUE)),
    usage_duration = factor(sample(c("New", "6months", "1year", "2years+"), 
                                  350, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))),
    customer_segment = factor(sample(c("Individual", "Small_Business", "Large_Enterprise"), 
                                    350, replace = TRUE, prob = c(0.6, 0.3, 0.1))),
    
    # Satisfaction scores (1-5 scale)
    overall_satisfaction = sample(1:5, 350, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.45, 0.2)),
    ease_of_use = sample(1:5, 350, replace = TRUE, prob = c(0.03, 0.07, 0.15, 0.5, 0.25)),
    value_for_money = sample(1:5, 350, replace = TRUE, prob = c(0.08, 0.12, 0.25, 0.35, 0.2)),
    customer_support = sample(1:5, 350, replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.35, 0.2)),
    
    # NPS-style scores (0-10)
    likelihood_recommend = sample(0:10, 350, replace = TRUE, 
                                 prob = c(0.02, 0.02, 0.03, 0.05, 0.05, 0.08, 0.1, 0.15, 0.2, 0.15, 0.15)),
    
    # Continuous satisfaction measures
    satisfaction_continuous = rnorm(350, 3.8, 0.9),
    perceived_quality = rnorm(350, 4.1, 0.8),
    purchase_intention = rnorm(350, 3.5, 1.1),
    
    # Usage frequency
    usage_frequency = factor(sample(c("Daily", "Weekly", "Monthly", "Rarely"), 
                                   350, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))),
    
    # Binary outcomes
    will_renew = factor(sample(c("No", "Yes"), 350, replace = TRUE, prob = c(0.25, 0.75))),
    referred_others = factor(sample(c("No", "Yes"), 350, replace = TRUE, prob = c(0.6, 0.4)))
) %>%
    mutate(
        # Create realistic survey relationships
        satisfaction_continuous = pmax(1, pmin(5, satisfaction_continuous + 
            case_when(
                product_type == "Basic" ~ rnorm(350, -0.2, 0.3),
                product_type == "Premium" ~ rnorm(350, 0.1, 0.3),
                product_type == "Enterprise" ~ rnorm(350, 0.3, 0.3)
            ) +
            case_when(
                usage_duration == "New" ~ rnorm(350, -0.1, 0.2),
                usage_duration == "6months" ~ rnorm(350, 0, 0.2),
                usage_duration == "1year" ~ rnorm(350, 0.2, 0.2),
                usage_duration == "2years+" ~ rnorm(350, 0.4, 0.2)
            ))),
        
        perceived_quality = pmax(1, pmin(5, perceived_quality + 
            0.3 * (satisfaction_continuous - 3.8) + rnorm(350, 0, 0.4)))
    )

# 5. Biological/Medical research data with biomarkers
jviolin_biomarker <- data.frame(
    sample_id = paste0("BIO_", sprintf("%04d", 1:200)),
    
    # Experimental conditions
    treatment_group = factor(sample(c("Control", "Treatment_1", "Treatment_2", "Combination"), 
                                   200, replace = TRUE)),
    time_point = factor(sample(c("Baseline", "Week_4", "Week_8", "Week_12"), 
                              200, replace = TRUE)),
    dose_level = factor(sample(c("Low", "Medium", "High"), 200, replace = TRUE)),
    
    # Subject characteristics
    gender = factor(sample(c("Male", "Female"), 200, replace = TRUE)),
    age_group = factor(sample(c("Young", "Adult", "Senior"), 200, replace = TRUE, prob = c(0.3, 0.5, 0.2))),
    bmi_category = factor(sample(c("Normal", "Overweight", "Obese"), 200, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    
    # Biomarker measurements (log-normal distributions typical in biology)
    protein_a = exp(rnorm(200, 2, 0.8)),
    protein_b = exp(rnorm(200, 3, 0.6)),
    enzyme_activity = exp(rnorm(200, 1.5, 1.0)),
    cytokine_level = exp(rnorm(200, 2.5, 0.9)),
    
    # Gene expression (often log-normal)
    gene_expr_1 = exp(rnorm(200, 4, 1.2)),
    gene_expr_2 = exp(rnorm(200, 3.5, 1.0)),
    gene_expr_3 = exp(rnorm(200, 4.2, 0.8)),
    
    # Cell counts (often right-skewed)
    white_cells = exp(rnorm(200, 8.5, 0.5)) * 1000,
    red_cells = rnorm(200, 4.5, 0.5) * 1000000,
    platelets = rnorm(200, 250, 50) * 1000,
    
    # Clinical scores
    disease_severity = round(rnorm(200, 15, 5)),
    quality_score = round(rnorm(200, 75, 15)),
    
    # Response indicators
    response_category = factor(sample(c("Non-responder", "Partial", "Complete"), 
                                     200, replace = TRUE, prob = c(0.4, 0.4, 0.2)))
) %>%
    mutate(
        # Create realistic biological relationships
        protein_a = protein_a * case_when(
            treatment_group == "Control" ~ 1,
            treatment_group == "Treatment_1" ~ 1.3,
            treatment_group == "Treatment_2" ~ 1.6,
            treatment_group == "Combination" ~ 2.1
        ) * exp(rnorm(200, 0, 0.3)),
        
        cytokine_level = cytokine_level * case_when(
            treatment_group == "Control" ~ 1,
            treatment_group == "Treatment_1" ~ 0.8,
            treatment_group == "Treatment_2" ~ 0.6,
            treatment_group == "Combination" ~ 0.4
        ) * exp(rnorm(200, 0, 0.4)),
        
        # Ensure positive values for biological measurements
        protein_a = pmax(0.1, protein_a),
        protein_b = pmax(0.1, protein_b),
        enzyme_activity = pmax(0.1, enzyme_activity),
        cytokine_level = pmax(0.1, cytokine_level),
        gene_expr_1 = pmax(1, gene_expr_1),
        gene_expr_2 = pmax(1, gene_expr_2),
        gene_expr_3 = pmax(1, gene_expr_3),
        
        # Constrain clinical scores
        disease_severity = pmax(0, pmin(30, disease_severity)),
        quality_score = pmax(0, pmin(100, quality_score))
    )

# 6. Missing data scenarios for robustness testing
jviolin_missing <- jviolin_basic
set.seed(456)

# Introduce different missing data patterns
# Missing completely at random (MCAR)
jviolin_missing$normal_dist[sample(1:300, 30)] <- NA
jviolin_missing$uniform_dist[sample(1:300, 25)] <- NA

# Missing at random (MAR) - related to group
high_group_indices <- which(jviolin_missing$group_3 == "High")
jviolin_missing$skewed_right[sample(high_group_indices, length(high_group_indices) * 0.4)] <- NA

# Missing not at random (MNAR) - related to values themselves
extreme_values <- which(jviolin_missing$bimodal_dist > quantile(jviolin_missing$bimodal_dist, 0.9, na.rm = TRUE))
jviolin_missing$bimodal_dist[sample(extreme_values, length(extreme_values) * 0.6)] <- NA

# Group variable with missing values
jviolin_missing$group_3[sample(1:300, 20)] <- NA

# 7. Small sample size data for edge case testing
jviolin_small <- jviolin_basic[1:20, ]

# 8. Large dataset for performance testing
jviolin_large <- do.call(rbind, replicate(10, jviolin_basic, simplify = FALSE))
jviolin_large$participant_id <- 1:nrow(jviolin_large)

# 9. Edge cases and extreme scenarios
jviolin_edge_cases <- data.frame(
    id = 1:100,
    
    # Single group
    single_group = factor(rep("OnlyGroup", 100)),
    
    # Very unbalanced groups
    unbalanced_group = factor(sample(c("Large", "Small"), 100, replace = TRUE, prob = c(0.95, 0.05))),
    
    # Extreme outliers
    with_outliers = c(rnorm(95, 50, 10), c(1000, -1000, 500, -500, 2000)),
    
    # Constant values (no variation)
    constant_var = rep(42, 100),
    
    # Nearly constant (minimal variation)
    near_constant = rep(50, 100) + rnorm(100, 0, 0.01),
    
    # Perfect correlations
    perfect_corr_1 = rnorm(100, 25, 5),
    perfect_corr_2 = NA,  # Will be set to perfect_corr_1
    
    # Highly skewed data
    extreme_skew = exp(rnorm(100, 0, 3)),
    
    # Integer data that might cause issues
    count_data = rpois(100, 3),
    
    # Groups with very different sample sizes
    different_n_group = factor(c(rep("Big", 80), rep("Medium", 15), rep("Tiny", 5))),
    
    # Normal group for comparison
    normal_group = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
)
jviolin_edge_cases$perfect_corr_2 <- jviolin_edge_cases$perfect_corr_1

# 10. Time series / longitudinal violin data
jviolin_longitudinal <- do.call(rbind, lapply(1:50, function(participant) {
    baseline_value <- rnorm(1, 50, 10)
    time_effect <- seq(0, 10, length.out = 6)  # 6 time points
    treatment_effect <- ifelse(sample(c("Control", "Treatment"), 1) == "Treatment", 8, 0)
    
    data.frame(
        participant_id = participant,
        time_point = factor(paste0("Week_", c(0, 2, 4, 8, 12, 24))),
        treatment = factor(rep(sample(c("Control", "Treatment"), 1), 6)),
        outcome = baseline_value + 
            time_effect + 
            treatment_effect * (time_effect / max(time_effect)) + 
            rnorm(6, 0, 5)
    )
}))

# Create comprehensive test descriptions
test_descriptions <- data.frame(
    dataset_name = c(
        "jviolin_basic",
        "jviolin_clinical",
        "jviolin_education", 
        "jviolin_survey",
        "jviolin_biomarker",
        "jviolin_missing",
        "jviolin_small",
        "jviolin_large",
        "jviolin_edge_cases",
        "jviolin_longitudinal"
    ),
    description = c(
        "Basic violin plot data with multiple distribution types - 300 observations",
        "Clinical trial data with realistic dose-response relationships - 250 patients",
        "Educational data with test scores and teaching methods - 400 students", 
        "Survey data with Likert scales and satisfaction measures - 350 respondents",
        "Biological research data with biomarkers and treatment effects - 200 samples",
        "Basic data with various missing data patterns (MCAR, MAR, MNAR) - 300 observations with NAs",
        "Small sample size data for edge case testing - 20 observations",
        "Large dataset for performance testing - 3000 observations",
        "Edge cases and extreme scenarios for robustness testing - 100 observations",
        "Longitudinal data with repeated measures - 300 observations (50 subjects × 6 time points)"
    ),
    primary_use = c(
        "Testing different distribution shapes and group structures",
        "Medical research with dose-response relationships",
        "Educational assessment and intervention effectiveness", 
        "Customer satisfaction and market research analysis",
        "Biological research with treatment comparisons",
        "Missing data handling and robustness testing",
        "Small sample behavior and statistical power",
        "Performance testing and scalability",
        "Robustness testing with extreme values and edge cases",
        "Repeated measures and time series visualization"
    ),
    violin_features = c(
        "All basic violin plot options, multiple distributions",
        "Medical dose-response, boxplot overlays, quantiles",
        "Group comparisons, point overlays, color mapping",
        "Likert scales, satisfaction measures, mean indicators", 
        "Biomarker distributions, log-scale data, treatment effects",
        "Missing data patterns, incomplete groups",
        "Single group handling, minimal sample sizes",
        "Performance optimization, caching effectiveness",
        "Outlier handling, constant variables, extreme skewness",
        "Time series, repeated measures, individual trajectories"
    ),
    observations = c(300, 250, 400, 350, 200, 300, 20, 3000, 100, 300),
    variables = c(15, 18, 18, 16, 20, 15, 15, 15, 12, 4)
)

# Print summary
cat("Created", nrow(test_descriptions), "test datasets for jviolin function:\n\n")
print(test_descriptions)

cat("\n\nDataset memory usage summary:\n")
for (dataset_name in test_descriptions$dataset_name) {
    if (exists(dataset_name)) {
        obj_size <- format(object.size(get(dataset_name)), units = "KB")
        cat(sprintf("%-25s: %s\n", dataset_name, obj_size))
    }
}

cat("\n\nExample usage for different violin plot types:\n\n")

cat("# Basic Violin Plot\n")
cat("result <- jviolin(\n")
cat("    data = jviolin_basic,\n")
cat("    dep = 'normal_dist',\n")
cat("    group = 'group_3'\n")
cat(")\n\n")

cat("# Violin with Boxplot Overlay\n")
cat("result <- jviolin(\n")
cat("    data = jviolin_clinical,\n")
cat("    dep = 'change_from_baseline',\n")
cat("    group = 'treatment_arm',\n")
cat("    add_boxplot = TRUE,\n")
cat("    add_mean = TRUE\n")
cat(")\n\n")

cat("# Violin with Points and Quantiles\n")
cat("result <- jviolin(\n")
cat("    data = jviolin_education,\n")
cat("    dep = 'math_score',\n")
cat("    group = 'teaching_method',\n")
cat("    add_points = TRUE,\n")
cat("    draw_quantiles = TRUE,\n")
cat("    quantile_lines = '0.25,0.5,0.75'\n")
cat(")\n\n")

cat("# Grouped Violin with Color Mapping\n")
cat("result <- jviolin(\n")
cat("    data = jviolin_survey,\n")
cat("    dep = 'satisfaction_continuous',\n")
cat("    group = 'product_type',\n")
cat("    fill = 'usage_duration',\n")
cat("    color_palette = 'viridis'\n")
cat(")\n\n")

cat("# Biomarker Data (Log Scale)\n")
cat("result <- jviolin(\n")
cat("    data = jviolin_biomarker,\n")
cat("    dep = 'protein_a',\n")
cat("    group = 'treatment_group',\n")
cat("    add_boxplot = TRUE,\n")
cat("    scale_violin = 'width'\n")
cat(")\n\n")

cat("# Longitudinal Data\n")
cat("result <- jviolin(\n")
cat("    data = jviolin_longitudinal,\n")
cat("    dep = 'outcome',\n")
cat("    group = 'time_point',\n")
cat("    fill = 'treatment',\n")
cat("    add_mean = TRUE\n")
cat(")\n\n")

# Save datasets if requested
if (exists("save_datasets") && save_datasets) {
    save_dir <- "data"
    if (!dir.exists(save_dir)) dir.create(save_dir)
    
    datasets_to_save <- list(
        jviolin_basic = jviolin_basic,
        jviolin_clinical = jviolin_clinical,
        jviolin_education = jviolin_education,
        jviolin_survey = jviolin_survey,
        jviolin_biomarker = jviolin_biomarker,
        jviolin_missing = jviolin_missing,
        jviolin_small = jviolin_small,
        jviolin_large = jviolin_large,
        jviolin_edge_cases = jviolin_edge_cases,
        jviolin_longitudinal = jviolin_longitudinal,
        jviolin_test_descriptions = test_descriptions
    )
    
    save(list = names(datasets_to_save), 
         file = file.path(save_dir, "jviolin_test_data.RData"))
    
    cat("Test datasets saved to:", file.path(save_dir, "jviolin_test_data.RData"), "\n")
}

# Additional utility functions for testing
create_violin_test_data <- function(n = 200, groups = 3, distribution = "normal") {
    # Helper function to create violin plot test data on demand
    group_names <- LETTERS[1:groups]
    
    if (distribution == "normal") {
        values <- rnorm(n, 50, 15)
    } else if (distribution == "skewed") {
        values <- exp(rnorm(n, 3, 0.5))
    } else if (distribution == "bimodal") {
        values <- c(rnorm(n/2, 30, 8), rnorm(n/2, 70, 8))
    } else if (distribution == "uniform") {
        values <- runif(n, 20, 80)
    }
    
    data.frame(
        id = 1:n,
        value = values,
        group = factor(sample(group_names, n, replace = TRUE))
    )
}

create_dose_response_data <- function(n = 150) {
    # Helper function for dose-response violin plots
    doses <- c("Placebo", "Low", "Medium", "High")
    dose_effects <- c(0, 5, 12, 20)
    
    data.frame(
        subject_id = 1:n,
        dose = factor(sample(doses, n, replace = TRUE)),
        response = sapply(1:n, function(i) {
            dose_level <- which(doses == as.character(data.frame(dose = factor(sample(doses, n, replace = TRUE)))$dose[i]))
            rnorm(1, 50 + dose_effects[dose_level], 12)
        })
    )
}

cat("Utility functions created:\n")
cat("- create_violin_test_data(n, groups, distribution): Generate test data for violin plots\n")
cat("- create_dose_response_data(n): Generate dose-response data\n")

# Clean up workspace
rm(list = setdiff(ls(), c(test_descriptions$dataset_name, "test_descriptions", 
                          "create_violin_test_data", "create_dose_response_data")))
gc()