# Test Data Generation Script for kappasizefixedn Function
# Creates realistic datasets for comprehensive testing of kappa fixed-n power analysis
# Each dataset represents different research scenarios requiring power analysis for available sample sizes

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

# Helper function to create realistic kappa values and sample sizes
create_kappa_scenario <- function(base_kappa, available_n, precision_expectation = "moderate") {
  # Adjust kappa based on realistic expectations
  kappa0 <- base_kappa
  
  # Sample sizes should reflect realistic study constraints
  n <- available_n
  
  return(list(kappa0 = kappa0, n = n))
}

# 1. Medical Diagnosis Studies - Fixed Sample Size Scenarios
create_medical_diagnosis_data <- function() {
  scenarios <- list(
    # Emergency department studies with limited patient availability
    ed_pneumonia_detection = list(
      study_type = "Emergency Department Pneumonia Detection",
      description = "Available emergency patients for chest X-ray agreement study",
      outcome = "2",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.65, 80),
      props = c(0.25, 0.75),  # 25% pneumonia prevalence
      alpha = 0.05,
      clinical_context = "Two emergency physicians review chest X-rays, limited by patient flow",
      expected_power = "Analysis determines if 80 patients provide adequate power for detecting κ=0.65"
    ),
    
    mammography_screening = list(
      study_type = "Mammography Screening Agreement",
      description = "Radiologist agreement on mammography interpretation with fixed cohort",
      outcome = "2",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.75, 150),
      props = c(0.12, 0.88),  # ~12% positive findings (BI-RADS 4-5)
      alpha = 0.05,
      clinical_context = "Two radiologists independently review screening mammograms from existing cohort",
      expected_power = "Determines lower bound for κ=0.75 with 150 available mammograms"
    ),
    
    skin_lesion_biopsy = list(
      study_type = "Skin Lesion Biopsy Decision",
      description = "Dermatologists agreeing on biopsy recommendations",
      outcome = "2", 
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.70, 120),
      props = c(0.18, 0.82),  # 18% requiring biopsy
      alpha = 0.05,
      clinical_context = "Three dermatologists evaluate skin lesions from clinic database",
      expected_power = "Assesses power for detecting κ=0.70 with 120 lesion photos"
    ),
    
    # ICU decision making with limited cases
    icu_discharge_readiness = list(
      study_type = "ICU Discharge Readiness",
      description = "ICU physicians agreeing on discharge readiness",
      outcome = "2",
      raters = "3", 
      kappa_scenario = create_kappa_scenario(0.80, 90),
      props = c(0.60, 0.40),  # 60% ready for discharge
      alpha = 0.05,
      clinical_context = "Three ICU physicians assess discharge readiness from patient records",
      expected_power = "Determines if 90 patient cases provide sufficient power for κ=0.80"
    ),
    
    # Pathology consultation with specimen limitations
    pathology_diagnosis = list(
      study_type = "Pathology Diagnosis Agreement",
      description = "Pathologists agreeing on primary diagnosis",
      outcome = "2",
      raters = "4",
      kappa_scenario = create_kappa_scenario(0.85, 200),
      props = c(0.35, 0.65),  # 35% malignant
      alpha = 0.01,
      clinical_context = "Four pathologists review histological specimens from tumor bank",
      expected_power = "Evaluates power for detecting κ=0.85 with 200 available specimens"
    )
  )
  
  return(scenarios)
}

# 2. Multi-Category Medical Assessments with Fixed Samples (3 categories)
create_medical_severity_data <- function() {
  scenarios <- list(
    # Hospital-based studies with patient availability constraints
    heart_failure_severity = list(
      study_type = "Heart Failure Severity Assessment",
      description = "Cardiologists grading heart failure severity",
      outcome = "3",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.70, 100),
      props = c(0.20, 0.50, 0.30),  # Mild, Moderate, Severe
      alpha = 0.05,
      clinical_context = "Two cardiologists assess heart failure severity from echocardiograms",
      expected_power = "Determines lower bound for κ=0.70 with 100 echo studies"
    ),
    
    burn_severity_grading = list(
      study_type = "Burn Severity Grading",
      description = "Emergency physicians grading burn severity",
      outcome = "3",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.75, 75),
      props = c(0.40, 0.35, 0.25),  # First, Second, Third degree
      alpha = 0.05,
      clinical_context = "Three emergency physicians grade burn severity from clinical photos",
      expected_power = "Assesses if 75 burn cases provide adequate power for κ=0.75"
    ),
    
    # Outpatient clinic studies
    osteoarthritis_grading = list(
      study_type = "Osteoarthritis Grading",
      description = "Radiologists grading knee osteoarthritis",
      outcome = "3",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.80, 130),
      props = c(0.25, 0.45, 0.30),  # Mild, Moderate, Severe OA
      alpha = 0.05,
      clinical_context = "Two musculoskeletal radiologists grade knee X-rays using Kellgren-Lawrence",
      expected_power = "Evaluates power for κ=0.80 with 130 knee X-rays from clinic"
    ),
    
    # Psychiatric assessment with limited patient availability
    depression_severity = list(
      study_type = "Depression Severity Rating",
      description = "Psychiatrists rating depression severity",
      outcome = "3",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.65, 85),
      props = c(0.30, 0.45, 0.25),  # Mild, Moderate, Severe depression
      alpha = 0.05,
      clinical_context = "Two psychiatrists rate depression severity using clinical interview data",
      expected_power = "Determines if 85 patient interviews provide sufficient power for κ=0.65"
    ),
    
    # Pediatric assessment
    asthma_control_level = list(
      study_type = "Pediatric Asthma Control Assessment",
      description = "Pediatricians assessing asthma control levels",
      outcome = "3",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.70, 110),
      props = c(0.35, 0.40, 0.25),  # Well controlled, Partly controlled, Uncontrolled
      alpha = 0.05,
      clinical_context = "Three pediatricians assess asthma control from clinic records",
      expected_power = "Assesses power for detecting κ=0.70 with 110 pediatric cases"
    )
  )
  
  return(scenarios)
}

# 3. Four-Category Assessments with Sample Constraints
create_four_category_data <- function() {
  scenarios <- list(
    # Oncology studies with limited tumor samples
    tumor_grading = list(
      study_type = "Tumor Grading Agreement",
      description = "Pathologists grading tumor differentiation",
      outcome = "4",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.75, 160),
      props = c(0.15, 0.25, 0.35, 0.25),  # Grade 1, 2, 3, 4
      alpha = 0.05,
      clinical_context = "Three pathologists grade tumor specimens from tissue bank",
      expected_power = "Determines lower bound for κ=0.75 with 160 tumor samples"
    ),
    
    # Cognitive assessment with patient limitations
    cognitive_impairment = list(
      study_type = "Cognitive Impairment Assessment",
      description = "Neuropsychologists rating cognitive impairment",
      outcome = "4",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.70, 90),
      props = c(0.20, 0.30, 0.30, 0.20),  # Normal, MCI, Mild dementia, Moderate+ dementia
      alpha = 0.05,
      clinical_context = "Two neuropsychologists assess cognitive status from test batteries",
      expected_power = "Evaluates if 90 cognitive assessments provide adequate power for κ=0.70"
    ),
    
    # Surgical outcome assessment
    surgical_complication = list(
      study_type = "Surgical Complication Grading",
      description = "Surgeons grading post-operative complications",
      outcome = "4",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.80, 140),
      props = c(0.40, 0.30, 0.20, 0.10),  # None, Grade I, Grade II, Grade III+
      alpha = 0.05,
      clinical_context = "Three surgeons grade complications using Clavien-Dindo classification",
      expected_power = "Determines power for κ=0.80 with 140 surgical cases"
    ),
    
    # Quality of life assessment
    performance_status = list(
      study_type = "Performance Status Rating",
      description = "Oncologists rating patient performance status",
      outcome = "4",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.75, 120),
      props = c(0.25, 0.35, 0.25, 0.15),  # ECOG 0, 1, 2, 3-4
      alpha = 0.05,
      clinical_context = "Two oncologists rate performance status from clinic visits",
      expected_power = "Assesses if 120 patient evaluations provide sufficient power for κ=0.75"
    ),
    
    # Pain assessment in clinical trial
    pain_interference = list(
      study_type = "Pain Interference Rating",
      description = "Clinicians rating pain interference with daily activities",
      outcome = "4",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.65, 100),
      props = c(0.20, 0.30, 0.30, 0.20),  # None, Mild, Moderate, Severe interference
      alpha = 0.05,
      clinical_context = "Two clinicians rate pain interference from patient questionnaires",
      expected_power = "Determines lower bound for κ=0.65 with 100 patient assessments"
    )
  )
  
  return(scenarios)
}

# 4. Five-Category Assessments for Complex Scales
create_five_category_data <- function() {
  scenarios <- list(
    # Behavioral health with limited observations
    anxiety_severity = list(
      study_type = "Anxiety Severity Rating",
      description = "Psychologists rating anxiety severity using clinical scales",
      outcome = "5",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.60, 80),
      props = c(0.10, 0.20, 0.35, 0.25, 0.10),  # None, Mild, Moderate, Severe, Extreme
      alpha = 0.05,
      clinical_context = "Two psychologists rate anxiety using standardized interview data",
      expected_power = "Evaluates if 80 clinical interviews provide adequate power for κ=0.60"
    ),
    
    # Functional assessment
    functional_independence = list(
      study_type = "Functional Independence Assessment",
      description = "Rehabilitation specialists rating functional independence",
      outcome = "5",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.70, 110),
      props = c(0.15, 0.20, 0.30, 0.25, 0.10),  # Complete dependence to complete independence
      alpha = 0.05,
      clinical_context = "Three rehabilitation specialists assess functional status from therapy notes",
      expected_power = "Determines power for κ=0.70 with 110 patient assessments"
    ),
    
    # Pain intensity with limited patient pool
    pain_intensity = list(
      study_type = "Pain Intensity Assessment",
      description = "Nurses rating pain intensity in non-verbal patients",
      outcome = "5",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.55, 70),
      props = c(0.20, 0.25, 0.25, 0.20, 0.10),  # No pain to severe pain
      alpha = 0.05,
      clinical_context = "Two nurses assess pain using behavioral indicators in ICU patients",
      expected_power = "Assesses if 70 patient observations provide sufficient power for κ=0.55"
    ),
    
    # Academic performance assessment
    clinical_competency = list(
      study_type = "Clinical Competency Rating",
      description = "Medical educators rating student clinical competency",
      outcome = "5",
      raters = "4",
      kappa_scenario = create_kappa_scenario(0.75, 95),
      props = c(0.05, 0.15, 0.35, 0.35, 0.10),  # Below expectations to exceeds expectations
      alpha = 0.05,
      clinical_context = "Four attending physicians rate medical student performance",
      expected_power = "Determines lower bound for κ=0.75 with 95 student evaluations"
    ),
    
    # Quality of life in chronic disease
    quality_of_life = list(
      study_type = "Quality of Life Assessment",
      description = "Clinicians rating patient quality of life",
      outcome = "5",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.65, 125),
      props = c(0.10, 0.20, 0.30, 0.25, 0.15),  # Very poor to excellent QoL
      alpha = 0.05,
      clinical_context = "Two clinicians assess quality of life in chronic disease patients",
      expected_power = "Evaluates power for κ=0.65 with 125 patient questionnaires"
    )
  )
  
  return(scenarios)
}

# 5. Research Methodology and Quality Assessment Data
create_research_quality_data <- function() {
  scenarios <- list(
    # Systematic review with limited papers
    study_quality_assessment = list(
      study_type = "Study Quality Assessment",
      description = "Researchers assessing study quality for systematic review",
      outcome = "3",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.70, 60),
      props = c(0.25, 0.45, 0.30),  # Low, Moderate, High quality
      alpha = 0.05,
      clinical_context = "Two researchers assess study quality using standardized criteria",
      expected_power = "Determines if 60 studies provide adequate power for κ=0.70"
    ),
    
    # Grant review process
    grant_scoring = list(
      study_type = "Grant Application Scoring",
      description = "Reviewers scoring research grant applications",
      outcome = "5",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.60, 45),
      props = c(0.10, 0.20, 0.30, 0.25, 0.15),  # Score ranges 1-5
      alpha = 0.05,
      clinical_context = "Three expert reviewers score research proposals using standardized rubrics",
      expected_power = "Assesses if 45 grant applications provide sufficient power for κ=0.60"
    ),
    
    # Clinical trial data monitoring
    adverse_event_severity = list(
      study_type = "Adverse Event Severity Assessment",
      description = "Clinical investigators grading adverse event severity",
      outcome = "4",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.80, 85),
      props = c(0.35, 0.30, 0.25, 0.10),  # Mild, Moderate, Severe, Life-threatening
      alpha = 0.05,
      clinical_context = "Two investigators assess adverse events in clinical trial",
      expected_power = "Determines power for κ=0.80 with 85 adverse event reports"
    ),
    
    # Medical image quality assessment
    image_quality_rating = list(
      study_type = "Medical Image Quality Rating",
      description = "Radiologists rating image quality for research study",
      outcome = "4",
      raters = "3",
      kappa_scenario = create_kappa_scenario(0.75, 150),
      props = c(0.10, 0.20, 0.45, 0.25),  # Poor, Fair, Good, Excellent
      alpha = 0.05,
      clinical_context = "Three radiologists rate image quality for multi-center study",
      expected_power = "Evaluates if 150 images provide adequate power for κ=0.75"
    ),
    
    # Biomarker assessment
    biomarker_expression = list(
      study_type = "Biomarker Expression Assessment",
      description = "Pathologists scoring biomarker expression levels",
      outcome = "3",
      raters = "2",
      kappa_scenario = create_kappa_scenario(0.85, 180),
      props = c(0.30, 0.45, 0.25),  # Low, Moderate, High expression
      alpha = 0.01,
      clinical_context = "Two pathologists score immunohistochemical staining intensity",
      expected_power = "Determines lower bound for κ=0.85 with 180 tissue samples"
    )
  )
  
  return(scenarios)
}

# Function to consolidate all scenarios into structured datasets
consolidate_all_scenarios <- function() {
  
  all_scenarios <- list(
    medical_diagnosis = create_medical_diagnosis_data(),
    medical_severity = create_medical_severity_data(),
    four_category_assessment = create_four_category_data(),
    five_category_assessment = create_five_category_data(),
    research_quality = create_research_quality_data()
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
        kappa0 = scenario$kappa_scenario$kappa0,
        sample_size = scenario$kappa_scenario$n,
        proportions = paste(scenario$props, collapse = ", "),
        alpha = scenario$alpha,
        clinical_context = scenario$clinical_context,
        expected_power = scenario$expected_power
      )
      
      consolidated_data[[paste(domain, scenario_name, sep = "_")]] <- scenario_df
    }
  }
  
  # Combine all scenarios into single data frame
  final_dataset <- do.call(rbind, consolidated_data)
  
  return(final_dataset)
}

# Function to create parameter validation test cases
create_validation_test_cases <- function() {
  
  validation_cases <- tibble(
    test_case = c(
      "valid_binary", "valid_three_cat", "valid_four_cat", "valid_five_cat",
      "invalid_kappa0_high", "invalid_kappa0_low", "invalid_n_zero", 
      "invalid_n_negative", "invalid_n_decimal", "invalid_n_small",
      "invalid_props_count", "invalid_props_sum", "invalid_alpha_high", 
      "invalid_alpha_low", "invalid_outcome", "invalid_raters", 
      "edge_case_minimal", "edge_case_maximal", "small_sample", "large_sample"
    ),
    
    outcome = c(
      "2", "3", "4", "5",
      "2", "2", "2", "2", "2", "2",
      "3", "2", "2", "2", "6", "2",
      "2", "2", "2", "2"
    ),
    
    kappa0 = c(
      0.60, 0.50, 0.65, 0.55,
      1.5, -0.1, 0.60, 0.60, 0.60, 0.60,
      0.50, 0.60, 0.60, 0.60, 0.60, 0.60,
      0.01, 0.99, 0.60, 0.60
    ),
    
    props = c(
      "0.30, 0.70", "0.25, 0.35, 0.40", "0.20, 0.25, 0.30, 0.25", "0.15, 0.20, 0.25, 0.25, 0.15",
      "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70",
      "0.25, 0.75", "0.30, 0.80", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70", "0.30, 0.70",
      "0.50, 0.50", "0.05, 0.95", "0.50, 0.50", "0.25, 0.75"
    ),
    
    raters = c(
      "2", "3", "3", "4",
      "2", "2", "2", "2", "2", "2",
      "3", "2", "2", "2", "2", "7",
      "2", "5", "2", "3"
    ),
    
    alpha = c(
      0.05, 0.05, 0.05, 0.05,
      0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
      0.05, 0.05, 1.5, -0.01, 0.05, 0.05,
      0.05, 0.01, 0.05, 0.05
    ),
    
    n = c(
      100, 150, 200, 250,
      100, 100, 0, -10, 50.5, 5,
      150, 100, 100, 100, 100, 100,
      25, 500, 15, 1000
    ),
    
    expected_result = c(
      "valid", "valid", "valid", "valid",
      "error", "error", "error", "error", "error", "error",
      "error", "error", "error", "error", "error", "error",
      "valid", "valid", "warning", "valid"
    ),
    
    error_type = c(
      NA, NA, NA, NA,
      "kappa0_out_of_range", "kappa0_out_of_range", "invalid_sample_size", 
      "invalid_sample_size", "invalid_sample_size", "sample_size_too_small",
      "props_count_mismatch", "props_sum_invalid", "alpha_out_of_range", 
      "alpha_out_of_range", "invalid_outcome_categories", "invalid_rater_count",
      NA, NA, NA, NA
    )
  )
  
  return(validation_cases)
}

# Function to create power analysis test cases
create_power_analysis_cases <- function() {
  
  power_cases <- tibble(
    test_name = c(
      "small_sample_low_power", "large_sample_high_power",
      "many_raters_better_power", "few_raters_lower_power",
      "balanced_proportions", "unbalanced_proportions",
      "high_kappa_tight_bounds", "low_kappa_wide_bounds",
      "strict_alpha", "lenient_alpha"
    ),
    
    description = c(
      "Small sample size should provide limited power",
      "Large sample size should provide better power",
      "More raters should improve power/precision",
      "Fewer raters should reduce power/precision", 
      "Balanced proportions baseline scenario",
      "Unbalanced proportions may affect power",
      "High kappa values with existing sample",
      "Low kappa values with existing sample",
      "Strict significance level affects bounds",
      "Lenient significance level affects bounds"
    ),
    
    outcome = c("2", "2", "2", "2", "3", "3", "2", "2", "2", "2"),
    kappa0 = c(0.70, 0.70, 0.60, 0.60, 0.60, 0.60, 0.85, 0.25, 0.60, 0.60),
    props = c(
      "0.50, 0.50", "0.50, 0.50", "0.50, 0.50", "0.50, 0.50",
      "0.33, 0.33, 0.34", "0.10, 0.20, 0.70", "0.20, 0.80", "0.50, 0.50",
      "0.50, 0.50", "0.50, 0.50"
    ),
    raters = c("2", "2", "5", "2", "3", "3", "2", "2", "2", "2"),
    alpha = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.01, 0.20),
    n = c(30, 300, 100, 100, 150, 150, 200, 200, 100, 100),
    expected_relationship = c(
      "lower_bound_less_precise", "lower_bound_more_precise",
      "better_precision_than_few_raters", "worse_precision_than_many_raters",
      "baseline_comparison", "different_from_balanced",
      "high_expected_lower_bound", "low_expected_lower_bound",
      "more_conservative_bound", "less_conservative_bound"
    )
  )
  
  return(power_cases)
}

# Generate all datasets
message("Generating kappasizefixedn test datasets...")

# Create comprehensive scenario dataset
kappa_fixedn_scenarios_comprehensive <- consolidate_all_scenarios()

# Create validation test cases
kappa_fixedn_validation_cases <- create_validation_test_cases()

# Create power analysis test cases
kappa_fixedn_power_cases <- create_power_analysis_cases()

# Save datasets to package data directory (if it exists)
if (dir.exists("data")) {
  save(kappa_fixedn_scenarios_comprehensive, file = "data/kappasizefixedn_scenarios_comprehensive.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_fixedn_scenarios_comprehensive, "data/kappasizefixedn_scenarios_comprehensive.omv")
  message("✓ Created kappasizefixedn_scenarios_comprehensive.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_fixedn_scenarios_comprehensive, "data/kappasizefixedn_scenarios_comprehensive.omv")
  message("✓ Created kappasizefixedn_scenarios_comprehensive.omv")
}
  save(kappa_fixedn_validation_cases, file = "data/kappasizefixedn_validation_cases.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_fixedn_validation_cases, "data/kappasizefixedn_validation_cases.omv")
  message("✓ Created kappasizefixedn_validation_cases.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_fixedn_validation_cases, "data/kappasizefixedn_validation_cases.omv")
  message("✓ Created kappasizefixedn_validation_cases.omv")
}
  save(kappa_fixedn_power_cases, file = "data/kappasizefixedn_power_cases.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_fixedn_power_cases, "data/kappasizefixedn_power_cases.omv")
  message("✓ Created kappasizefixedn_power_cases.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(kappa_fixedn_power_cases, "data/kappasizefixedn_power_cases.omv")
  message("✓ Created kappasizefixedn_power_cases.omv")
}
  
  message("✓ All kappa fixed-n test datasets saved to data/ directory")
} else {
  message("Note: data/ directory not found. Datasets created in memory only.")
}

# Print dataset summaries
message("\n=== Dataset Summaries ===")

message("\n1. Comprehensive Scenarios Dataset:")
message(sprintf("   - Total scenarios: %d", nrow(kappa_fixedn_scenarios_comprehensive)))
message(sprintf("   - Research domains: %d", length(unique(kappa_fixedn_scenarios_comprehensive$domain))))
message(sprintf("   - Outcome categories covered: %s", paste(sort(unique(kappa_fixedn_scenarios_comprehensive$outcome_categories)), collapse = ", ")))
message(sprintf("   - Rater combinations: %s", paste(sort(unique(kappa_fixedn_scenarios_comprehensive$raters)), collapse = ", ")))

# Domain breakdown
domain_summary <- kappa_fixedn_scenarios_comprehensive %>%
  group_by(domain) %>%
  summarise(
    scenarios = n(),
    avg_kappa0 = round(mean(kappa0), 3),
    avg_sample_size = round(mean(sample_size), 1),
    .groups = "drop"
  )

message("\nDomain breakdown:")
for (i in 1:nrow(domain_summary)) {
  message(sprintf("   %s: %d scenarios (avg κ₀=%.3f, avg n=%.0f)", 
                 domain_summary$domain[i], 
                 domain_summary$scenarios[i],
                 domain_summary$avg_kappa0[i], 
                 domain_summary$avg_sample_size[i]))
}

message("\n2. Validation Test Cases:")
message(sprintf("   - Total test cases: %d", nrow(kappa_fixedn_validation_cases)))
valid_cases <- sum(kappa_fixedn_validation_cases$expected_result == "valid")
error_cases <- sum(kappa_fixedn_validation_cases$expected_result == "error")
warning_cases <- sum(kappa_fixedn_validation_cases$expected_result == "warning")
message(sprintf("   - Valid parameter combinations: %d", valid_cases))
message(sprintf("   - Invalid parameter combinations: %d", error_cases))
message(sprintf("   - Warning parameter combinations: %d", warning_cases))

message("\n3. Power Analysis Cases:")
message(sprintf("   - Power relationship test scenarios: %d", nrow(kappa_fixedn_power_cases)))
message(sprintf("   - Sample size range: %d - %d", min(kappa_fixedn_power_cases$n), max(kappa_fixedn_power_cases$n)))

message("\n=== Usage Examples ===")
message("# Basic kappa lower bound calculation")
message("kappaSizeFixedN(outcome='2', kappa0=0.60, props='0.30, 0.70', raters='2', alpha=0.05, n=100)")
message("")
message("# Medical diagnosis study with fixed sample")
message("kappaSizeFixedN(outcome='2', kappa0=0.75, props='0.15, 0.85', raters='2', alpha=0.05, n=150)")
message("")
message("# Multi-category assessment with available sample")
message("kappaSizeFixedN(outcome='4', kappa0=0.70, props='0.25, 0.30, 0.25, 0.20', raters='3', alpha=0.05, n=200)")
message("")
message("# Research study with limited cases")
message("kappaSizeFixedN(outcome='3', kappa0=0.65, props='0.33, 0.33, 0.34', raters='2', alpha=0.01, n=80)")

message("\n✓ kappasizefixedn test data generation completed successfully!")
