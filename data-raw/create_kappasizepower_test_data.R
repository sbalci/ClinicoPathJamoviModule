# Test Data Generation Script for kappasizepower Function
# Creates realistic datasets for comprehensive testing of kappa power analysis for sample size determination
# Each dataset represents different research scenarios requiring sample size calculations based on power analysis

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(2024)

# Helper function to validate proportions
validate_proportions <- function(props) {
  all(props > 0) && all(props < 1) && abs(sum(props) - 1) < 0.01
}

# Helper function to create realistic kappa scenarios
create_power_scenario <- function(kappa0, kappa1, alpha = 0.05, power = 0.80) {
  # Validate kappa relationship
  if (kappa1 <= kappa0) {
    stop("kappa1 must be greater than kappa0 for power analysis")
  }
  
  # Calculate effect size
  effect_size <- kappa1 - kappa0
  
  return(list(
    kappa0 = kappa0,
    kappa1 = kappa1,
    alpha = alpha,
    power = power,
    effect_size = effect_size
  ))
}

# 1. Medical Diagnosis Studies - Power Analysis Scenarios
create_medical_diagnosis_power_data <- function() {
  scenarios <- list(
    # Emergency department studies seeking improved agreement
    ed_pneumonia_agreement = list(
      study_type = "Emergency Department Pneumonia Agreement",
      description = "Sample size needed to detect improvement in chest X-ray agreement",
      outcome = "2",
      raters = "2",
      power_scenario = create_power_scenario(0.50, 0.75),  # Fair to good agreement
      props = c(0.25, 0.75),  # 25% pneumonia prevalence
      clinical_context = "Two emergency physicians want to establish reliable pneumonia detection protocol",
      research_question = "How many chest X-rays needed to detect improvement from fair (κ=0.50) to good (κ=0.75) agreement?"
    ),
    
    mammography_screening_improvement = list(
      study_type = "Mammography Screening Agreement Improvement",
      description = "Sample size for detecting enhanced radiologist agreement after training",
      outcome = "2",
      raters = "2",
      power_scenario = create_power_scenario(0.60, 0.80),  # Good to excellent agreement
      props = c(0.12, 0.88),  # ~12% positive findings (BI-RADS 4-5)
      clinical_context = "Two radiologists post-training want to validate improved agreement",
      research_question = "Sample size needed to demonstrate training improved agreement from κ=0.60 to κ=0.80?"
    ),
    
    skin_lesion_expert_vs_trainee = list(
      study_type = "Skin Lesion Expert vs Trainee Agreement",
      description = "Sample size to detect difference between expert and trainee agreement levels",
      outcome = "2", 
      raters = "3",
      power_scenario = create_power_scenario(0.45, 0.70),  # Moderate to good agreement
      props = c(0.18, 0.82),  # 18% requiring biopsy
      clinical_context = "Comparing dermatology expert vs trainee biopsy recommendations",
      research_question = "Sample size to detect improvement from trainee (κ=0.45) to expert (κ=0.70) levels?"
    ),
    
    # ICU decision making protocol validation
    icu_discharge_protocol = list(
      study_type = "ICU Discharge Protocol Validation",
      description = "Sample size for validating standardized discharge criteria",
      outcome = "2",
      raters = "3", 
      power_scenario = create_power_scenario(0.55, 0.80, power = 0.85),  # Higher power for safety
      props = c(0.60, 0.40),  # 60% ready for discharge
      clinical_context = "Three ICU physicians implementing standardized discharge criteria",
      research_question = "Sample size to validate protocol improves agreement from κ=0.55 to κ=0.80?"
    ),
    
    # Pathology consultation standardization
    pathology_standardization = list(
      study_type = "Pathology Diagnosis Standardization",
      description = "Sample size for demonstrating improved diagnostic agreement",
      outcome = "2",
      raters = "4",
      power_scenario = create_power_scenario(0.70, 0.90, alpha = 0.01, power = 0.90),  # Strict criteria
      props = c(0.35, 0.65),  # 35% malignant
      clinical_context = "Four pathologists implementing standardized diagnostic criteria",
      research_question = "Sample size for rigorous validation of improved agreement (κ=0.70 to κ=0.90)?"
    )
  )
  
  return(scenarios)
}

# 2. Multi-Category Medical Assessments - Power Analysis (3 categories)
create_medical_severity_power_data <- function() {
  scenarios <- list(
    # Hospital-based studies for protocol validation
    heart_failure_staging = list(
      study_type = "Heart Failure Staging Agreement",
      description = "Sample size for validating improved staging agreement",
      outcome = "3",
      raters = "2",
      power_scenario = create_power_scenario(0.55, 0.75),  # Moderate to good agreement
      props = c(0.20, 0.50, 0.30),  # Mild, Moderate, Severe
      clinical_context = "Two cardiologists validating new echocardiographic staging criteria",
      research_question = "Sample size to detect staging agreement improvement from κ=0.55 to κ=0.75?"
    ),
    
    burn_severity_protocol = list(
      study_type = "Burn Severity Grading Protocol",
      description = "Sample size for standardized burn assessment validation",
      outcome = "3",
      raters = "3",
      power_scenario = create_power_scenario(0.60, 0.85),  # Good to excellent
      props = c(0.40, 0.35, 0.25),  # First, Second, Third degree
      clinical_context = "Three emergency physicians implementing standardized burn grading",
      research_question = "Sample size needed to validate protocol improves grading from κ=0.60 to κ=0.85?"
    ),
    
    # Outpatient clinic studies
    osteoarthritis_imaging = list(
      study_type = "Osteoarthritis Imaging Standardization",
      description = "Sample size for improved radiological grading agreement",
      outcome = "3",
      raters = "2",
      power_scenario = create_power_scenario(0.65, 0.80),  # Good to very good
      props = c(0.25, 0.45, 0.30),  # Mild, Moderate, Severe OA
      clinical_context = "Two musculoskeletal radiologists using standardized Kellgren-Lawrence criteria",
      research_question = "Sample size to demonstrate improved grading consistency (κ=0.65 to κ=0.80)?"
    ),
    
    # Psychiatric assessment standardization
    depression_severity_scale = list(
      study_type = "Depression Severity Scale Validation",
      description = "Sample size for validating new depression rating agreement",
      outcome = "3",
      raters = "2",
      power_scenario = create_power_scenario(0.50, 0.70),  # Moderate to good
      props = c(0.30, 0.45, 0.25),  # Mild, Moderate, Severe depression
      clinical_context = "Two psychiatrists validating structured depression assessment tool",
      research_question = "Sample size to validate improved rating consistency (κ=0.50 to κ=0.70)?"
    ),
    
    # Pediatric assessment protocol
    asthma_control_validation = list(
      study_type = "Pediatric Asthma Control Validation",
      description = "Sample size for standardized asthma control assessment",
      outcome = "3",
      raters = "3",
      power_scenario = create_power_scenario(0.55, 0.75, power = 0.85),
      props = c(0.35, 0.40, 0.25),  # Well controlled, Partly controlled, Uncontrolled
      clinical_context = "Three pediatricians implementing standardized asthma control criteria",
      research_question = "Sample size for validating control assessment improvement (κ=0.55 to κ=0.75)?"
    )
  )
  
  return(scenarios)
}

# 3. Four-Category Assessments - Power Analysis
create_four_category_power_data <- function() {
  scenarios <- list(
    # Oncology studies for improved grading
    tumor_grading_standardization = list(
      study_type = "Tumor Grading Standardization",
      description = "Sample size for validating improved tumor grading agreement",
      outcome = "4",
      raters = "3",
      power_scenario = create_power_scenario(0.60, 0.80),  # Good to very good
      props = c(0.15, 0.25, 0.35, 0.25),  # Grade 1, 2, 3, 4
      clinical_context = "Three pathologists implementing standardized grading criteria",
      research_question = "Sample size to validate grading standardization (κ=0.60 to κ=0.80)?"
    ),
    
    # Cognitive assessment validation
    cognitive_assessment_tool = list(
      study_type = "Cognitive Assessment Tool Validation",
      description = "Sample size for new cognitive assessment protocol",
      outcome = "4",
      raters = "2",
      power_scenario = create_power_scenario(0.55, 0.75),  # Moderate to good
      props = c(0.20, 0.30, 0.30, 0.20),  # Normal, MCI, Mild dementia, Moderate+ dementia
      clinical_context = "Two neuropsychologists validating structured cognitive assessment",
      research_question = "Sample size for demonstrating assessment improvement (κ=0.55 to κ=0.75)?"
    ),
    
    # Surgical outcome standardization
    surgical_complication_grading = list(
      study_type = "Surgical Complication Grading Validation",
      description = "Sample size for standardized complication assessment",
      outcome = "4",
      raters = "3",
      power_scenario = create_power_scenario(0.65, 0.85, alpha = 0.01),  # High standards for surgery
      props = c(0.40, 0.30, 0.20, 0.10),  # None, Grade I, Grade II, Grade III+
      clinical_context = "Three surgeons implementing standardized Clavien-Dindo classification",
      research_question = "Sample size for validating complication grading improvement (κ=0.65 to κ=0.85)?"
    ),
    
    # Quality of life assessment
    performance_status_validation = list(
      study_type = "Performance Status Validation",
      description = "Sample size for improved performance status agreement",
      outcome = "4",
      raters = "2",
      power_scenario = create_power_scenario(0.60, 0.80),
      props = c(0.25, 0.35, 0.25, 0.15),  # ECOG 0, 1, 2, 3-4
      clinical_context = "Two oncologists validating structured performance assessment",
      research_question = "Sample size to validate performance rating improvement (κ=0.60 to κ=0.80)?"
    ),
    
    # Pain assessment in clinical trial
    pain_interference_scale = list(
      study_type = "Pain Interference Scale Validation",
      description = "Sample size for standardized pain interference assessment",
      outcome = "4",
      raters = "2",
      power_scenario = create_power_scenario(0.50, 0.70),
      props = c(0.20, 0.30, 0.30, 0.20),  # None, Mild, Moderate, Severe interference
      clinical_context = "Two clinicians validating pain interference assessment tool",
      research_question = "Sample size for demonstrating interference rating improvement (κ=0.50 to κ=0.70)?"
    )
  )
  
  return(scenarios)
}

# 4. Five-Category Assessments - Power Analysis for Complex Scales
create_five_category_power_data <- function() {
  scenarios <- list(
    # Behavioral health validation
    anxiety_severity_tool = list(
      study_type = "Anxiety Severity Tool Validation",
      description = "Sample size for standardized anxiety assessment",
      outcome = "5",
      raters = "2",
      power_scenario = create_power_scenario(0.45, 0.65),  # Moderate improvement
      props = c(0.10, 0.20, 0.35, 0.25, 0.10),  # None, Mild, Moderate, Severe, Extreme
      clinical_context = "Two psychologists validating structured anxiety assessment protocol",
      research_question = "Sample size for anxiety assessment validation (κ=0.45 to κ=0.65)?"
    ),
    
    # Functional assessment standardization
    functional_independence_validation = list(
      study_type = "Functional Independence Validation",
      description = "Sample size for improved functional assessment agreement",
      outcome = "5",
      raters = "3",
      power_scenario = create_power_scenario(0.55, 0.75, power = 0.85),
      props = c(0.15, 0.20, 0.30, 0.25, 0.10),  # Complete dependence to complete independence
      clinical_context = "Three rehabilitation specialists implementing standardized functional assessment",
      research_question = "Sample size for functional assessment standardization (κ=0.55 to κ=0.75)?"
    ),
    
    # Pain intensity validation
    pain_intensity_non_verbal = list(
      study_type = "Non-verbal Pain Assessment Validation",
      description = "Sample size for validating non-verbal pain assessment",
      outcome = "5",
      raters = "2",
      power_scenario = create_power_scenario(0.40, 0.60),  # Improvement in challenging area
      props = c(0.20, 0.25, 0.25, 0.20, 0.10),  # No pain to severe pain
      clinical_context = "Two nurses validating behavioral pain indicators in ICU patients",
      research_question = "Sample size for non-verbal pain assessment validation (κ=0.40 to κ=0.60)?"
    ),
    
    # Academic performance assessment
    clinical_competency_validation = list(
      study_type = "Clinical Competency Assessment Validation",
      description = "Sample size for standardized medical student evaluation",
      outcome = "5",
      raters = "4",
      power_scenario = create_power_scenario(0.60, 0.80, alpha = 0.01),  # High standards for education
      props = c(0.05, 0.15, 0.35, 0.35, 0.10),  # Below expectations to exceeds expectations
      clinical_context = "Four attending physicians implementing standardized competency assessment",
      research_question = "Sample size for competency assessment standardization (κ=0.60 to κ=0.80)?"
    ),
    
    # Quality of life in chronic disease
    quality_of_life_validation = list(
      study_type = "Quality of Life Assessment Validation",
      description = "Sample size for improved quality of life rating agreement",
      outcome = "5",
      raters = "2",
      power_scenario = create_power_scenario(0.50, 0.70),
      props = c(0.10, 0.20, 0.30, 0.25, 0.15),  # Very poor to excellent QoL
      clinical_context = "Two clinicians validating quality of life assessment in chronic disease",
      research_question = "Sample size for quality of life assessment improvement (κ=0.50 to κ=0.70)?"
    )
  )
  
  return(scenarios)
}

# 5. Research Methodology and Quality Assessment - Power Analysis
create_research_quality_power_data <- function() {
  scenarios <- list(
    # Systematic review methodology validation
    study_quality_assessment_tool = list(
      study_type = "Study Quality Assessment Tool Validation",
      description = "Sample size for improved systematic review quality assessment",
      outcome = "3",
      raters = "2",
      power_scenario = create_power_scenario(0.55, 0.75),
      props = c(0.25, 0.45, 0.30),  # Low, Moderate, High quality
      clinical_context = "Two researchers validating systematic review quality assessment criteria",
      research_question = "Sample size for quality assessment tool validation (κ=0.55 to κ=0.75)?"
    ),
    
    # Grant review process standardization
    grant_scoring_standardization = list(
      study_type = "Grant Scoring Standardization",
      description = "Sample size for improved grant evaluation agreement",
      outcome = "5",
      raters = "3",
      power_scenario = create_power_scenario(0.45, 0.70, power = 0.85),
      props = c(0.10, 0.20, 0.30, 0.25, 0.15),  # Score ranges 1-5
      clinical_context = "Three expert reviewers implementing standardized grant evaluation criteria",
      research_question = "Sample size for grant scoring standardization (κ=0.45 to κ=0.70)?"
    ),
    
    # Clinical trial data monitoring
    adverse_event_severity_protocol = list(
      study_type = "Adverse Event Severity Protocol Validation",
      description = "Sample size for standardized adverse event assessment",
      outcome = "4",
      raters = "2",
      power_scenario = create_power_scenario(0.65, 0.85, alpha = 0.01),  # High standards for safety
      props = c(0.35, 0.30, 0.25, 0.10),  # Mild, Moderate, Severe, Life-threatening
      clinical_context = "Two investigators implementing standardized adverse event classification",
      research_question = "Sample size for adverse event classification validation (κ=0.65 to κ=0.85)?"
    ),
    
    # Medical image quality assessment
    image_quality_standardization = list(
      study_type = "Medical Image Quality Standardization",
      description = "Sample size for improved image quality assessment",
      outcome = "4",
      raters = "3",
      power_scenario = create_power_scenario(0.60, 0.80),
      props = c(0.10, 0.20, 0.45, 0.25),  # Poor, Fair, Good, Excellent
      clinical_context = "Three radiologists implementing standardized image quality criteria",
      research_question = "Sample size for image quality assessment standardization (κ=0.60 to κ=0.80)?"
    ),
    
    # Biomarker assessment standardization
    biomarker_expression_validation = list(
      study_type = "Biomarker Expression Assessment Validation",
      description = "Sample size for standardized biomarker scoring",
      outcome = "3",
      raters = "2",
      power_scenario = create_power_scenario(0.70, 0.90, alpha = 0.01, power = 0.90),  # High precision needed
      props = c(0.30, 0.45, 0.25),  # Low, Moderate, High expression
      clinical_context = "Two pathologists implementing standardized immunohistochemical scoring",
      research_question = "Sample size for biomarker scoring standardization (κ=0.70 to κ=0.90)?"
    )
  )
  
  return(scenarios)
}

# Function to consolidate all scenarios into structured datasets
consolidate_all_power_scenarios <- function() {
  
  all_scenarios <- list(
    medical_diagnosis_power = create_medical_diagnosis_power_data(),
    medical_severity_power = create_medical_severity_power_data(),
    four_category_power = create_four_category_power_data(),
    five_category_power = create_five_category_power_data(),
    research_quality_power = create_research_quality_power_data()
  )
  
  # Convert to structured data frames
  consolidated_data <- list()
  
  for (domain in names(all_scenarios)) {
    domain_scenarios <- all_scenarios[[domain]]
    
    for (scenario_name in names(domain_scenarios)) {
      scenario <- domain_scenarios[[scenario_name]]
      
      # Create data frame row
      scenario_df <- tibble(
        domain = domain,
        scenario_id = scenario_name,
        study_type = scenario$study_type,
        description = scenario$description,
        outcome_categories = as.numeric(scenario$outcome),
        raters = as.numeric(scenario$raters),
        kappa0 = scenario$power_scenario$kappa0,
        kappa1 = scenario$power_scenario$kappa1,
        effect_size = scenario$power_scenario$effect_size,
        proportions = paste(scenario$props, collapse = ", "),
        alpha = scenario$power_scenario$alpha,
        power = scenario$power_scenario$power,
        clinical_context = scenario$clinical_context,
        research_question = scenario$research_question
      )
      
      consolidated_data[[paste(domain, scenario_name, sep = "_")]] <- scenario_df
    }
  }
  
  # Combine all scenarios into single data frame
  final_dataset <- do.call(rbind, consolidated_data)
  
  return(final_dataset)
}

# Function to create parameter validation test cases for power analysis
create_power_validation_test_cases <- function() {
  
  validation_cases <- tibble(
    test_case = c(
      "valid_binary", "valid_three_cat", "valid_four_cat", "valid_five_cat",
      "invalid_kappa0_high", "invalid_kappa0_low", "invalid_kappa1_high", "invalid_kappa1_low",
      "invalid_kappa_relationship", "invalid_props_count", "invalid_props_sum",
      "invalid_alpha_high", "invalid_alpha_low", "invalid_power_high", "invalid_power_low",
      "invalid_outcome", "invalid_raters", "edge_case_minimal_effect", 
      "edge_case_large_effect", "high_power_scenario"
    ),
    
    outcome = c(
      "2", "3", "4", "5",
      "2", "2", "2", "2",
      "2", "3", "2",
      "2", "2", "2", "2",
      "6", "2", "2",
      "2", "2"
    ),
    
    kappa0 = c(
      0.40, 0.35, 0.40, 0.35,
      1.5, -0.1, 0.60, 0.60,
      0.70, 0.35, 0.40,
      0.40, 0.40, 0.40, 0.40,
      0.40, 0.40, 0.49,
      0.20, 0.30
    ),
    
    kappa1 = c(
      0.60, 0.55, 0.65, 0.60,
      0.60, 0.60, 1.5, -0.1,
      0.60, 0.55, 0.60,  # kappa1 < kappa0 (invalid)
      0.60, 0.60, 0.60, 0.60,
      0.60, 0.60, 0.50,  # Minimal effect
      0.80, 0.85   # Large effects
    ),
    
    props = c(
      "0.30, 0.70", "0.25, 0.35, 0.40", "0.20, 0.25, 0.30, 0.25", "0.15, 0.20, 0.25, 0.25, 0.15",
      "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70",
      "0.30, 0.70", "0.25, 0.75", "0.30, 0.80",  # Invalid sum
      "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70",
      "0.30, 0.70", "0.30, 0.70", "0.50, 0.50",
      "0.20, 0.80", "0.25, 0.75"
    ),
    
    raters = c(
      "2", "3", "3", "4",
      "2", "2", "2", "2",
      "2", "3", "2",
      "2", "2", "2", "2",
      "2", "7", "2",  # Invalid rater count
      "3", "2"
    ),
    
    alpha = c(
      0.05, 0.05, 0.05, 0.05,
      0.05, 0.05, 0.05, 0.05,
      0.05, 0.05, 0.05,
      1.5, -0.01, 0.05, 0.05,  # Invalid alpha values
      0.05, 0.05, 0.05,
      0.01, 0.05
    ),
    
    power = c(
      0.80, 0.80, 0.80, 0.80,
      0.80, 0.80, 0.80, 0.80,
      0.80, 0.80, 0.80,
      0.80, 0.80, 1.5, 0.3,  # Invalid power values
      0.80, 0.80, 0.80,
      0.85, 0.95
    ),
    
    expected_result = c(
      "valid", "valid", "valid", "valid",
      "error", "error", "error", "error",
      "error", "error", "error",
      "error", "error", "error", "error",
      "error", "error", "valid",
      "valid", "valid"
    ),
    
    error_type = c(
      NA, NA, NA, NA,
      "kappa0_out_of_range", "kappa0_out_of_range", "kappa1_out_of_range", "kappa1_out_of_range",
      "invalid_kappa_relationship", "props_count_mismatch", "props_sum_invalid",
      "alpha_out_of_range", "alpha_out_of_range", "power_out_of_range", "power_too_low",
      "invalid_outcome_categories", "invalid_rater_count", NA,
      NA, NA
    )
  )
  
  return(validation_cases)
}

# Function to create power relationship test cases
create_power_relationship_cases <- function() {
  
  relationship_cases <- tibble(
    test_name = c(
      "small_effect_large_n", "large_effect_small_n",
      "high_power_large_n", "low_power_small_n",
      "strict_alpha_large_n", "lenient_alpha_small_n",
      "many_raters_complex", "few_raters_simple",
      "balanced_proportions", "unbalanced_proportions"
    ),
    
    description = c(
      "Small effect size should require larger sample size",
      "Large effect size should require smaller sample size",
      "High power should require larger sample size",
      "Lower power should require smaller sample size",
      "Strict alpha should require larger sample size",
      "Lenient alpha should require smaller sample size",
      "More raters with more categories increases complexity",
      "Fewer raters with fewer categories is simpler",
      "Balanced proportions baseline scenario",
      "Unbalanced proportions may affect sample size"
    ),
    
    outcome = c("2", "2", "2", "2", "2", "2", "5", "2", "3", "3"),
    kappa0 = c(0.50, 0.30, 0.40, 0.40, 0.40, 0.40, 0.35, 0.40, 0.40, 0.40),
    kappa1 = c(0.55, 0.70, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60),
    props = c(
      "0.50, 0.50", "0.50, 0.50", "0.50, 0.50", "0.50, 0.50",
      "0.50, 0.50", "0.50, 0.50", "0.20, 0.20, 0.20, 0.20, 0.20", "0.50, 0.50",
      "0.33, 0.33, 0.34", "0.10, 0.20, 0.70"
    ),
    raters = c("2", "2", "2", "2", "2", "2", "4", "2", "3", "3"),
    alpha = c(0.05, 0.05, 0.05, 0.05, 0.01, 0.10, 0.05, 0.05, 0.05, 0.05),
    power = c(0.80, 0.80, 0.95, 0.60, 0.80, 0.80, 0.80, 0.80, 0.80, 0.80),
    expected_relationship = c(
      "larger_sample_size", "smaller_sample_size",
      "larger_sample_size", "smaller_sample_size", 
      "larger_sample_size", "smaller_sample_size",
      "complex_calculation", "simple_calculation",
      "baseline_comparison", "different_from_balanced"
    )
  )
  
  return(relationship_cases)
}

# Generate all datasets
message("Generating kappasizepower test datasets...")

# Create comprehensive scenario dataset
kappa_power_scenarios_comprehensive <- consolidate_all_power_scenarios()

# Create validation test cases
kappa_power_validation_cases <- create_power_validation_test_cases()

# Create power relationship test cases
kappa_power_relationship_cases <- create_power_relationship_cases()

# Save datasets to package data directory (if it exists)
if (dir.exists("data")) {
  save(kappa_power_scenarios_comprehensive, file = "data/kappasizepower_scenarios_comprehensive.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_power_scenarios_comprehensive, "data/kappasizepower_scenarios_comprehensive.omv")
  message("✓ Created kappasizepower_scenarios_comprehensive.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_power_scenarios_comprehensive, "data/kappasizepower_scenarios_comprehensive.omv")
  message("✓ Created kappasizepower_scenarios_comprehensive.omv")
}
  save(kappa_power_validation_cases, file = "data/kappasizepower_validation_cases.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_power_validation_cases, "data/kappasizepower_validation_cases.omv")
  message("✓ Created kappasizepower_validation_cases.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_power_validation_cases, "data/kappasizepower_validation_cases.omv")
  message("✓ Created kappasizepower_validation_cases.omv")
}
  save(kappa_power_relationship_cases, file = "data/kappasizepower_relationship_cases.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_power_relationship_cases, "data/kappasizepower_relationship_cases.omv")
  message("✓ Created kappasizepower_relationship_cases.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_power_relationship_cases, "data/kappasizepower_relationship_cases.omv")
  message("✓ Created kappasizepower_relationship_cases.omv")
}
  
  message("✓ All kappa power analysis test datasets saved to data/ directory")
} else {
  message("Note: data/ directory not found. Datasets created in memory only.")
}

# Print dataset summaries
message("\n=== Dataset Summaries ===")

message("\n1. Comprehensive Power Scenarios Dataset:")
message(sprintf("   - Total scenarios: %d", nrow(kappa_power_scenarios_comprehensive)))
message(sprintf("   - Research domains: %d", length(unique(kappa_power_scenarios_comprehensive$domain))))
message(sprintf("   - Outcome categories covered: %s", paste(sort(unique(kappa_power_scenarios_comprehensive$outcome_categories)), collapse = ", ")))
message(sprintf("   - Rater combinations: %s", paste(sort(unique(kappa_power_scenarios_comprehensive$raters)), collapse = ", ")))

# Domain breakdown
domain_summary <- kappa_power_scenarios_comprehensive %>%
  group_by(domain) %>%
  summarise(
    scenarios = n(),
    avg_kappa0 = round(mean(kappa0), 3),
    avg_kappa1 = round(mean(kappa1), 3),
    avg_effect_size = round(mean(effect_size), 3),
    avg_power = round(mean(power), 3),
    .groups = "drop"
  )

message("\nDomain breakdown:")
for (i in 1:nrow(domain_summary)) {
  message(sprintf("   %s: %d scenarios (κ₀=%.3f, κ₁=%.3f, effect=%.3f, power=%.3f)", 
                 domain_summary$domain[i], 
                 domain_summary$scenarios[i],
                 domain_summary$avg_kappa0[i], 
                 domain_summary$avg_kappa1[i],
                 domain_summary$avg_effect_size[i],
                 domain_summary$avg_power[i]))
}

message("\n2. Validation Test Cases:")
message(sprintf("   - Total test cases: %d", nrow(kappa_power_validation_cases)))
valid_cases <- sum(kappa_power_validation_cases$expected_result == "valid")
error_cases <- sum(kappa_power_validation_cases$expected_result == "error")
message(sprintf("   - Valid parameter combinations: %d", valid_cases))
message(sprintf("   - Invalid parameter combinations: %d", error_cases))

message("\n3. Power Relationship Cases:")
message(sprintf("   - Power relationship test scenarios: %d", nrow(kappa_power_relationship_cases)))
message(sprintf("   - Effect size range: %.3f - %.3f", 
               min(kappa_power_relationship_cases$kappa1 - kappa_power_relationship_cases$kappa0), 
               max(kappa_power_relationship_cases$kappa1 - kappa_power_relationship_cases$kappa0)))

message("\n=== Usage Examples ===")
message("# Basic power analysis for sample size")
message("kappaSizePower(outcome='2', kappa0=0.40, kappa1=0.60, props='0.30, 0.70', raters='2', alpha=0.05, power=0.80)")
message("")
message("# Medical diagnosis study with improved agreement")
message("kappaSizePower(outcome='2', kappa0=0.50, kappa1=0.75, props='0.25, 0.75', raters='2', alpha=0.05, power=0.80)")
message("")
message("# Multi-category assessment with standardization")
message("kappaSizePower(outcome='4', kappa0=0.60, kappa1=0.80, props='0.25, 0.30, 0.25, 0.20', raters='3', alpha=0.05, power=0.85)")
message("")
message("# High-precision research study")
message("kappaSizePower(outcome='3', kappa0=0.55, kappa1=0.75, props='0.33, 0.33, 0.34', raters='2', alpha=0.01, power=0.90)")

message("\n✓ kappasizepower test data generation completed successfully!")
