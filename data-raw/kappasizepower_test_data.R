# ═══════════════════════════════════════════════════════════════════════════
# Test Data Generation: kappaSizePower
# ═══════════════════════════════════════════════════════════════════════════
#
# This script generates realistic test data for the kappaSizePower jamovi function
#
# Purpose: Power analysis for interobserver agreement studies
# Function: Calculates required sample size to detect difference between two kappa values
#
# Parameters:
#   - outcome: Number of outcome categories (2, 3, 4, 5)
#   - kappa0: Null hypothesis kappa value (0.01-0.99)
#   - kappa1: Alternative hypothesis kappa value (0.01-0.99, > kappa0)
#   - proportions: Expected proportions for each category (comma-separated)
#   - raters: Number of raters (2, 3, 4, 5)
#   - alpha: Significance level (0.01-0.99, typically 0.01 or 0.05)
#   - power: Desired statistical power (0.01-0.99, typically 0.80, 0.85, 0.90)
#
# Generated: 2026-01-07
# ═══════════════════════════════════════════════════════════════════════════

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

# ═══════════════════════════════════════════════════════════════════════════
# DATASET 1: Comprehensive Clinical Scenarios
# ═══════════════════════════════════════════════════════════════════════════

# This dataset contains 25 realistic clinical scenarios covering major
# applications of kappa agreement studies across different medical specialties

kappa_power_scenarios_comprehensive <- tribble(
  ~domain, ~scenario_id, ~study_type, ~description, ~outcome_categories, ~raters, ~kappa0, ~kappa1, ~effect_size, ~proportions, ~alpha, ~power, ~clinical_context, ~research_question,

  # Medical Diagnosis Power (Binary outcomes)
  "medical_diagnosis_power", "ed_pneumonia_agreement", "Emergency Department Pneumonia Agreement",
  "Sample size needed to detect improvement in chest X-ray agreement", 2, 2, 0.50, 0.75, 0.25, "0.25, 0.75", 0.05, 0.80,
  "Two emergency physicians want to establish reliable pneumonia detection protocol",
  "How many chest X-rays needed to detect improvement from fair (κ=0.50) to good (κ=0.75) agreement?",

  "medical_diagnosis_power", "mammography_screening_improvement", "Mammography Screening Agreement Improvement",
  "Sample size for detecting enhanced radiologist agreement after training", 2, 2, 0.60, 0.80, 0.20, "0.12, 0.88", 0.05, 0.80,
  "Two radiologists post-training want to validate improved agreement",
  "Sample size needed to demonstrate training improved agreement from κ=0.60 to κ=0.80?",

  "medical_diagnosis_power", "skin_lesion_expert_vs_trainee", "Skin Lesion Expert vs Trainee Agreement",
  "Sample size to detect difference between expert and trainee agreement levels", 2, 3, 0.45, 0.70, 0.25, "0.18, 0.82", 0.05, 0.80,
  "Comparing dermatology expert vs trainee biopsy recommendations",
  "Sample size to detect improvement from trainee (κ=0.45) to expert (κ=0.70) levels?",

  "medical_diagnosis_power", "icu_discharge_protocol", "ICU Discharge Protocol Validation",
  "Sample size for validating standardized discharge criteria", 2, 3, 0.55, 0.80, 0.25, "0.6, 0.4", 0.05, 0.85,
  "Three ICU physicians implementing standardized discharge criteria",
  "Sample size to validate protocol improves agreement from κ=0.55 to κ=0.80?",

  "medical_diagnosis_power", "pathology_standardization", "Pathology Diagnosis Standardization",
  "Sample size for demonstrating improved diagnostic agreement", 2, 4, 0.70, 0.90, 0.20, "0.35, 0.65", 0.01, 0.90,
  "Four pathologists implementing standardized diagnostic criteria",
  "Sample size for rigorous validation of improved agreement (κ=0.70 to κ=0.90)?",

  # Medical Severity Power (Three categories)
  "medical_severity_power", "heart_failure_staging", "Heart Failure Staging Agreement",
  "Sample size for validating improved staging agreement", 3, 2, 0.55, 0.75, 0.20, "0.2, 0.5, 0.3", 0.05, 0.80,
  "Two cardiologists validating new echocardiographic staging criteria",
  "Sample size to detect staging agreement improvement from κ=0.55 to κ=0.75?",

  "medical_severity_power", "burn_severity_protocol", "Burn Severity Grading Protocol",
  "Sample size for standardized burn assessment validation", 3, 3, 0.60, 0.85, 0.25, "0.4, 0.35, 0.25", 0.05, 0.80,
  "Three emergency physicians implementing standardized burn grading",
  "Sample size needed to validate protocol improves grading from κ=0.60 to κ=0.85?",

  "medical_severity_power", "osteoarthritis_imaging", "Osteoarthritis Imaging Standardization",
  "Sample size for improved radiological grading agreement", 3, 2, 0.65, 0.80, 0.15, "0.25, 0.45, 0.3", 0.05, 0.80,
  "Two musculoskeletal radiologists using standardized Kellgren-Lawrence criteria",
  "Sample size to demonstrate improved grading consistency (κ=0.65 to κ=0.80)?",

  "medical_severity_power", "depression_severity_scale", "Depression Severity Scale Validation",
  "Sample size for validating new depression rating agreement", 3, 2, 0.50, 0.70, 0.20, "0.3, 0.45, 0.25", 0.05, 0.80,
  "Two psychiatrists validating structured depression assessment tool",
  "Sample size to validate improved rating consistency (κ=0.50 to κ=0.70)?",

  "medical_severity_power", "asthma_control_validation", "Pediatric Asthma Control Validation",
  "Sample size for standardized asthma control assessment", 3, 3, 0.55, 0.75, 0.20, "0.35, 0.4, 0.25", 0.05, 0.85,
  "Three pediatricians implementing standardized asthma control criteria",
  "Sample size for validating control assessment improvement (κ=0.55 to κ=0.75)?",

  # Four Category Power
  "four_category_power", "tumor_grading_standardization", "Tumor Grading Standardization",
  "Sample size for validating improved tumor grading agreement", 4, 3, 0.60, 0.80, 0.20, "0.15, 0.25, 0.35, 0.25", 0.05, 0.80,
  "Three pathologists implementing standardized grading criteria",
  "Sample size to validate grading standardization (κ=0.60 to κ=0.80)?",

  "four_category_power", "cognitive_assessment_tool", "Cognitive Assessment Tool Validation",
  "Sample size for new cognitive assessment protocol", 4, 2, 0.55, 0.75, 0.20, "0.2, 0.3, 0.3, 0.2", 0.05, 0.80,
  "Two neuropsychologists validating structured cognitive assessment",
  "Sample size for demonstrating assessment improvement (κ=0.55 to κ=0.75)?",

  "four_category_power", "surgical_complication_grading", "Surgical Complication Grading Validation",
  "Sample size for standardized complication assessment", 4, 3, 0.65, 0.85, 0.20, "0.4, 0.3, 0.2, 0.1", 0.01, 0.80,
  "Three surgeons implementing standardized Clavien-Dindo classification",
  "Sample size for validating complication grading improvement (κ=0.65 to κ=0.85)?",

  "four_category_power", "performance_status_validation", "Performance Status Validation",
  "Sample size for improved performance status agreement", 4, 2, 0.60, 0.80, 0.20, "0.25, 0.35, 0.25, 0.15", 0.05, 0.80,
  "Two oncologists validating structured performance assessment",
  "Sample size to validate performance rating improvement (κ=0.60 to κ=0.80)?",

  "four_category_power", "pain_interference_scale", "Pain Interference Scale Validation",
  "Sample size for standardized pain interference assessment", 4, 2, 0.50, 0.70, 0.20, "0.2, 0.3, 0.3, 0.2", 0.05, 0.80,
  "Two clinicians validating pain interference assessment tool",
  "Sample size for demonstrating interference rating improvement (κ=0.50 to κ=0.70)?",

  # Five Category Power
  "five_category_power", "anxiety_severity_tool", "Anxiety Severity Tool Validation",
  "Sample size for standardized anxiety assessment", 5, 2, 0.45, 0.65, 0.20, "0.1, 0.2, 0.35, 0.25, 0.1", 0.05, 0.80,
  "Two psychologists validating structured anxiety assessment protocol",
  "Sample size for anxiety assessment validation (κ=0.45 to κ=0.65)?",

  "five_category_power", "functional_independence_validation", "Functional Independence Validation",
  "Sample size for improved functional assessment agreement", 5, 3, 0.55, 0.75, 0.20, "0.15, 0.2, 0.3, 0.25, 0.1", 0.05, 0.85,
  "Three rehabilitation specialists implementing standardized functional assessment",
  "Sample size for functional assessment standardization (κ=0.55 to κ=0.75)?",

  "five_category_power", "pain_intensity_non_verbal", "Non-verbal Pain Assessment Validation",
  "Sample size for validating non-verbal pain assessment", 5, 2, 0.40, 0.60, 0.20, "0.2, 0.25, 0.25, 0.2, 0.1", 0.05, 0.80,
  "Two nurses validating behavioral pain indicators in ICU patients",
  "Sample size for non-verbal pain assessment validation (κ=0.40 to κ=0.60)?",

  "five_category_power", "clinical_competency_validation", "Clinical Competency Assessment Validation",
  "Sample size for standardized medical student evaluation", 5, 4, 0.60, 0.80, 0.20, "0.05, 0.15, 0.35, 0.35, 0.1", 0.01, 0.80,
  "Four attending physicians implementing standardized competency assessment",
  "Sample size for competency assessment standardization (κ=0.60 to κ=0.80)?",

  "five_category_power", "quality_of_life_validation", "Quality of Life Assessment Validation",
  "Sample size for improved quality of life rating agreement", 5, 2, 0.50, 0.70, 0.20, "0.1, 0.2, 0.3, 0.25, 0.15", 0.05, 0.80,
  "Two clinicians validating quality of life assessment in chronic disease",
  "Sample size for quality of life assessment improvement (κ=0.50 to κ=0.70)?",

  # Research Quality Power
  "research_quality_power", "study_quality_assessment_tool", "Study Quality Assessment Tool Validation",
  "Sample size for improved systematic review quality assessment", 3, 2, 0.55, 0.75, 0.20, "0.25, 0.45, 0.3", 0.05, 0.80,
  "Two researchers validating systematic review quality assessment criteria",
  "Sample size for quality assessment tool validation (κ=0.55 to κ=0.75)?",

  "research_quality_power", "grant_scoring_standardization", "Grant Scoring Standardization",
  "Sample size for improved grant evaluation agreement", 5, 3, 0.45, 0.70, 0.25, "0.1, 0.2, 0.3, 0.25, 0.15", 0.05, 0.85,
  "Three expert reviewers implementing standardized grant evaluation criteria",
  "Sample size for grant scoring standardization (κ=0.45 to κ=0.70)?",

  "research_quality_power", "adverse_event_severity_protocol", "Adverse Event Severity Protocol Validation",
  "Sample size for standardized adverse event assessment", 4, 2, 0.65, 0.85, 0.20, "0.35, 0.3, 0.25, 0.1", 0.01, 0.80,
  "Two investigators implementing standardized adverse event classification",
  "Sample size for adverse event classification validation (κ=0.65 to κ=0.85)?",

  "research_quality_power", "image_quality_standardization", "Medical Image Quality Standardization",
  "Sample size for improved image quality assessment", 4, 3, 0.60, 0.80, 0.20, "0.1, 0.2, 0.45, 0.25", 0.05, 0.80,
  "Three radiologists implementing standardized image quality criteria",
  "Sample size for image quality assessment standardization (κ=0.60 to κ=0.80)?",

  "research_quality_power", "biomarker_expression_validation", "Biomarker Expression Assessment Validation",
  "Sample size for standardized biomarker scoring", 3, 2, 0.70, 0.90, 0.20, "0.3, 0.45, 0.25", 0.01, 0.90,
  "Two pathologists implementing standardized immunohistochemical scoring",
  "Sample size for biomarker scoring standardization (κ=0.70 to κ=0.90)?"
)

# Save Dataset 1 in all formats
save(kappa_power_scenarios_comprehensive, file = here("data", "kappasizepower_scenarios_comprehensive.rda"))
write.csv(kappa_power_scenarios_comprehensive, file = here("data", "kappasizepower_scenarios_comprehensive.csv"), row.names = FALSE)
write_xlsx(kappa_power_scenarios_comprehensive, path = here("data", "kappasizepower_scenarios_comprehensive.xlsx"))
write_omv(kappa_power_scenarios_comprehensive, here("data", "kappasizepower_scenarios_comprehensive.omv"), frcWrt = TRUE)

cat("\n✓ Dataset 1 created: kappasizepower_scenarios_comprehensive\n")
cat("  Scenarios:", nrow(kappa_power_scenarios_comprehensive), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# DATASET 2: Parameter Relationship Cases
# ═══════════════════════════════════════════════════════════════════════════

# This dataset demonstrates critical relationships between parameters
# Effect size, alpha, power interactions for teaching and validation

kappa_power_relationship_cases <- tribble(
  ~case_name, ~relationship_type, ~description, ~outcome, ~raters, ~kappa0, ~kappa1, ~effect_size, ~proportions, ~alpha, ~power, ~expected_pattern, ~teaching_point,

  # Effect Size Relationships
  "minimal_effect", "effect_size", "Minimal detectable effect size", "2", 2, 0.50, 0.51, 0.01, "0.50, 0.50", 0.05, 0.80,
  "Very large sample size required", "Small effect sizes require much larger samples",

  "small_effect", "effect_size", "Small but clinically relevant effect", "2", 2, 0.50, 0.55, 0.05, "0.50, 0.50", 0.05, 0.80,
  "Large sample size required", "Detecting small effects is resource-intensive",

  "medium_effect", "effect_size", "Medium effect size (Cohen's guideline)", "2", 2, 0.40, 0.60, 0.20, "0.30, 0.70", 0.05, 0.80,
  "Moderate sample size", "Medium effects balance feasibility and clinical importance",

  "large_effect", "effect_size", "Large effect size", "2", 2, 0.30, 0.70, 0.40, "0.20, 0.80", 0.05, 0.80,
  "Small sample size sufficient", "Large effects detectable with modest samples",

  "very_large_effect", "effect_size", "Very large effect (nearly perfect vs poor)", "2", 2, 0.20, 0.80, 0.60, "0.15, 0.85", 0.05, 0.80,
  "Minimal sample size needed", "Dramatic improvements easy to demonstrate",

  # Power Relationships
  "low_power_60", "power", "Low power (60%) - underpowered", "3", 2, 0.50, 0.70, 0.20, "0.33, 0.33, 0.34", 0.05, 0.60,
  "Smallest sample but risky", "Underpowered studies waste resources",

  "conventional_power_80", "power", "Conventional power (80%)", "3", 2, 0.50, 0.70, 0.20, "0.33, 0.33, 0.34", 0.05, 0.80,
  "Standard sample size", "80% power is research standard",

  "high_power_90", "power", "High power (90%)", "3", 2, 0.50, 0.70, 0.20, "0.33, 0.33, 0.34", 0.05, 0.90,
  "Larger sample for safety", "High power reduces false negative risk",

  "very_high_power_95", "power", "Very high power (95%)", "3", 2, 0.50, 0.70, 0.20, "0.33, 0.33, 0.34", 0.05, 0.95,
  "Much larger sample", "Diminishing returns beyond 90% power",

  # Alpha Relationships
  "liberal_alpha_10", "alpha", "Liberal alpha (0.10) - less stringent", "2", 3, 0.60, 0.80, 0.20, "0.25, 0.75", 0.10, 0.80,
  "Smaller sample size", "Less stringent testing allows smaller studies",

  "conventional_alpha_05", "alpha", "Conventional alpha (0.05)", "2", 3, 0.60, 0.80, 0.20, "0.25, 0.75", 0.05, 0.80,
  "Standard sample size", "0.05 is conventional significance level",

  "strict_alpha_01", "alpha", "Strict alpha (0.01) - Bonferroni level", "2", 3, 0.60, 0.80, 0.20, "0.25, 0.75", 0.01, 0.80,
  "Larger sample required", "Strict testing protects against false positives",

  "very_strict_alpha_001", "alpha", "Very strict alpha (0.001)", "2", 3, 0.60, 0.80, 0.20, "0.25, 0.75", 0.001, 0.80,
  "Much larger sample needed", "Extreme stringency for critical decisions only",

  # Rater Relationships
  "two_raters", "raters", "Two raters (minimum for agreement)", "4", 2, 0.55, 0.75, 0.20, "0.25, 0.25, 0.25, 0.25", 0.05, 0.80,
  "Baseline sample size", "Simplest agreement design",

  "three_raters", "raters", "Three raters (enhanced reliability)", "4", 3, 0.55, 0.75, 0.20, "0.25, 0.25, 0.25, 0.25", 0.05, 0.80,
  "Adjusted sample size", "Multiple raters improve generalizability",

  "four_raters", "raters", "Four raters (comprehensive)", "4", 4, 0.55, 0.75, 0.20, "0.25, 0.25, 0.25, 0.25", 0.05, 0.80,
  "Further adjusted sample", "More raters = more robust estimates",

  "five_raters", "raters", "Five raters (maximum)", "4", 5, 0.55, 0.75, 0.20, "0.25, 0.25, 0.25, 0.25", 0.05, 0.80,
  "Maximum adjustment", "Diminishing returns beyond 4-5 raters",

  # Proportion Effects
  "balanced_props", "proportions", "Balanced category proportions", "3", 2, 0.55, 0.75, 0.20, "0.33, 0.33, 0.34", 0.05, 0.80,
  "Standard sample size", "Balanced designs most efficient",

  "moderately_unbalanced", "proportions", "Moderately unbalanced categories", "3", 2, 0.55, 0.75, 0.20, "0.20, 0.30, 0.50", 0.05, 0.80,
  "Slightly larger sample", "Imbalance reduces efficiency",

  "highly_unbalanced", "proportions", "Highly unbalanced categories", "3", 2, 0.55, 0.75, 0.20, "0.10, 0.20, 0.70", 0.05, 0.80,
  "Noticeably larger sample", "Severe imbalance problematic",

  "extreme_imbalance", "proportions", "Extreme category imbalance", "3", 2, 0.55, 0.75, 0.20, "0.05, 0.15, 0.80", 0.05, 0.80,
  "Much larger sample needed", "Rare categories need oversampling",

  # Outcome Category Effects
  "binary_standard", "outcome_categories", "Binary outcome (simplest)", "2", 2, 0.50, 0.70, 0.20, "0.30, 0.70", 0.05, 0.80,
  "Smallest sample for given effect", "Binary outcomes most efficient",

  "three_category_standard", "outcome_categories", "Three categories", "3", 2, 0.50, 0.70, 0.20, "0.25, 0.40, 0.35", 0.05, 0.80,
  "Larger sample than binary", "More categories = more complex agreement",

  "four_category_standard", "outcome_categories", "Four categories", "4", 2, 0.50, 0.70, 0.20, "0.20, 0.25, 0.30, 0.25", 0.05, 0.80,
  "Larger still", "Fine-grained scales need more subjects",

  "five_category_standard", "outcome_categories", "Five categories (maximum)", "5", 2, 0.50, 0.70, 0.20, "0.15, 0.20, 0.30, 0.25, 0.10", 0.05, 0.80,
  "Largest sample needed", "Maximum complexity requires largest sample"
)

# Save Dataset 2 in all formats
save(kappa_power_relationship_cases, file = here("data", "kappasizepower_relationship_cases.rda"))
write.csv(kappa_power_relationship_cases, file = here("data", "kappasizepower_relationship_cases.csv"), row.names = FALSE)
write_xlsx(kappa_power_relationship_cases, path = here("data", "kappasizepower_relationship_cases.xlsx"))
write_omv(kappa_power_relationship_cases, here("data", "kappasizepower_relationship_cases.omv"), frcWrt = TRUE)

cat("\n✓ Dataset 2 created: kappasizepower_relationship_cases\n")
cat("  Cases:", nrow(kappa_power_relationship_cases), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# DATASET 3: Validation Test Cases
# ═══════════════════════════════════════════════════════════════════════════

# This dataset contains edge cases, boundary conditions, and validation scenarios
# for comprehensive function testing

kappa_power_validation_cases <- tribble(
  ~test_case, ~test_type, ~description, ~outcome, ~raters, ~kappa0, ~kappa1, ~proportions, ~alpha, ~power, ~expected_outcome, ~validation_purpose,

  # Valid Boundary Cases
  "min_kappa_values", "boundary", "Minimum kappa values", "2", 2, 0.01, 0.02, "0.50, 0.50", 0.05, 0.80,
  "Should succeed with valid parameters", "Test lower boundary of kappa range",

  "max_kappa_values", "boundary", "Near-maximum kappa values", "2", 2, 0.98, 0.99, "0.50, 0.50", 0.05, 0.80,
  "Should succeed", "Test upper boundary of kappa range",

  "min_alpha", "boundary", "Minimum alpha (0.01)", "3", 2, 0.50, 0.70, "0.33, 0.33, 0.34", 0.01, 0.80,
  "Should succeed - larger sample", "Test very strict significance level",

  "max_alpha", "boundary", "Maximum alpha approaching 1", "3", 2, 0.50, 0.70, "0.33, 0.33, 0.34", 0.20, 0.80,
  "Should succeed - smaller sample", "Test liberal significance level",

  "min_power", "boundary", "Minimum reasonable power (50%)", "2", 2, 0.40, 0.60, "0.30, 0.70", 0.05, 0.50,
  "Should warn - underpowered", "Test low power scenario",

  "max_power", "boundary", "Very high power (99%)", "2", 2, 0.40, 0.60, "0.30, 0.70", 0.05, 0.99,
  "Should succeed - very large sample", "Test extreme power requirement",

  # Special Proportion Cases
  "equal_proportions_binary", "special", "Exactly equal binary proportions", "2", 2, 0.50, 0.70, "0.50, 0.50", 0.05, 0.80,
  "Should succeed - optimal balance", "Test perfectly balanced design",

  "equal_proportions_three", "special", "Exactly equal three-category proportions", "3", 3, 0.45, 0.65, "0.333, 0.333, 0.334", 0.05, 0.80,
  "Should succeed", "Test balanced three-category design",

  "rare_category_small", "special", "One rare category (5%)", "3", 2, 0.55, 0.75, "0.05, 0.45, 0.50", 0.05, 0.80,
  "Larger sample needed", "Test impact of rare category",

  "rare_category_very_small", "special", "Very rare category (1%)", "4", 2, 0.55, 0.75, "0.01, 0.24, 0.35, 0.40", 0.05, 0.80,
  "Much larger sample needed", "Test extreme category imbalance",

  "dominant_category", "special", "One dominant category (90%)", "2", 2, 0.50, 0.70, "0.10, 0.90", 0.05, 0.80,
  "Larger sample due to imbalance", "Test skewed distribution",

  # Multiple Rater Scenarios
  "two_raters_minimal", "raters", "Minimum two raters", "2", 2, 0.45, 0.65, "0.25, 0.75", 0.05, 0.80,
  "Should succeed - baseline", "Test minimum rater configuration",

  "three_raters_comparison", "raters", "Three rater agreement", "3", 3, 0.50, 0.70, "0.30, 0.40, 0.30", 0.05, 0.85,
  "Should succeed", "Test three-rater design",

  "four_raters_comprehensive", "raters", "Four rater agreement (large team)", "4", 4, 0.60, 0.80, "0.25, 0.25, 0.25, 0.25", 0.05, 0.90,
  "Should succeed with adjustment", "Test comprehensive rater team",

  "five_raters_maximum", "raters", "Maximum five raters", "5", 5, 0.55, 0.75, "0.15, 0.20, 0.30, 0.25, 0.10", 0.01, 0.90,
  "Should succeed - largest configuration", "Test maximum rater scenario",

  # Category Number Variations
  "binary_simple", "categories", "Simple binary classification", "2", 2, 0.40, 0.70, "0.20, 0.80", 0.05, 0.80,
  "Should succeed - most efficient", "Test binary outcome efficiency",

  "three_category_ordinal", "categories", "Three-level ordinal scale", "3", 2, 0.45, 0.70, "0.25, 0.50, 0.25", 0.05, 0.80,
  "Should succeed", "Test ordinal three-category scale",

  "four_category_severity", "categories", "Four-level severity grading", "4", 3, 0.50, 0.75, "0.30, 0.30, 0.25, 0.15", 0.05, 0.85,
  "Should succeed", "Test four-category grading system",

  "five_category_likert", "categories", "Five-point Likert scale", "5", 2, 0.40, 0.65, "0.10, 0.20, 0.40, 0.20, 0.10", 0.05, 0.80,
  "Should succeed - maximum complexity", "Test maximum category scale",

  # Effect Size Variations
  "minimal_clinically_important", "effect", "Minimal clinically important difference", "2", 2, 0.50, 0.55, "0.30, 0.70", 0.05, 0.80,
  "Very large sample required", "Test minimal effect detection",

  "small_effect_cohen", "effect", "Small effect (Cohen 1988)", "2", 3, 0.50, 0.60, "0.25, 0.75", 0.05, 0.80,
  "Large sample required", "Test small effect size",

  "medium_effect_cohen", "effect", "Medium effect (Cohen 1988)", "3", 2, 0.40, 0.60, "0.30, 0.40, 0.30", 0.05, 0.80,
  "Moderate sample size", "Test medium effect size",

  "large_effect_cohen", "effect", "Large effect (Cohen 1988)", "2", 2, 0.30, 0.70, "0.20, 0.80", 0.05, 0.80,
  "Small sample size", "Test large effect size",

  "dramatic_improvement", "effect", "Dramatic improvement scenario", "2", 2, 0.20, 0.85, "0.15, 0.85", 0.05, 0.90,
  "Minimal sample needed", "Test very large effect",

  # Combined Challenging Scenarios
  "small_effect_high_power", "combined", "Small effect with high power demand", "2", 2, 0.50, 0.55, "0.50, 0.50", 0.05, 0.95,
  "Extremely large sample", "Test worst-case sample size scenario",

  "large_effect_low_power", "combined", "Large effect but low power (poor design)", "2", 2, 0.30, 0.70, "0.20, 0.80", 0.05, 0.60,
  "Small sample but risky", "Test underpowered large effect",

  "strict_testing_high_power", "combined", "Very strict testing with high power", "3", 4, 0.55, 0.75, "0.30, 0.40, 0.30", 0.001, 0.95,
  "Very large sample required", "Test most stringent requirements",

  "liberal_testing_low_power", "combined", "Liberal testing with moderate power", "2", 2, 0.50, 0.70, "0.30, 0.70", 0.10, 0.70,
  "Relatively small sample", "Test relaxed requirements",

  # Real-world Complex Scenarios
  "pilot_study_design", "application", "Pilot study with reasonable expectations", "2", 2, 0.40, 0.60, "0.25, 0.75", 0.10, 0.70,
  "Feasible small sample", "Test pilot study sizing",

  "definitive_trial_design", "application", "Definitive validation study", "3", 3, 0.60, 0.80, "0.25, 0.45, 0.30", 0.01, 0.90,
  "Large robust sample", "Test definitive study design",

  "quality_improvement", "application", "QI project validation", "4", 2, 0.55, 0.75, "0.40, 0.30, 0.20, 0.10", 0.05, 0.80,
  "Moderate sample for QI", "Test quality improvement application",

  "regulatory_submission", "application", "Regulatory submission study", "2", 4, 0.70, 0.90, "0.20, 0.80", 0.001, 0.95,
  "Very large rigorous sample", "Test regulatory requirements"
)

# Save Dataset 3 in all formats
save(kappa_power_validation_cases, file = here("data", "kappasizepower_validation_cases.rda"))
write.csv(kappa_power_validation_cases, file = here("data", "kappasizepower_validation_cases.csv"), row.names = FALSE)
write_xlsx(kappa_power_validation_cases, path = here("data", "kappasizepower_validation_cases.xlsx"))
write_omv(kappa_power_validation_cases, here("data", "kappasizepower_validation_cases.omv"), frcWrt = TRUE)

cat("\n✓ Dataset 3 created: kappasizepower_validation_cases\n")
cat("  Cases:", nrow(kappa_power_validation_cases), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════

cat("\n" )
cat("═══════════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("Generated datasets:\n")
cat("  1. kappasizepower_scenarios_comprehensive (", nrow(kappa_power_scenarios_comprehensive), " scenarios)\n")
cat("     - Medical diagnosis power (5)\n")
cat("     - Medical severity power (5)\n")
cat("     - Four-category power (5)\n")
cat("     - Five-category power (5)\n")
cat("     - Research quality power (5)\n\n")

cat("  2. kappasizepower_relationship_cases (", nrow(kappa_power_relationship_cases), " cases)\n")
cat("     - Effect size relationships (5)\n")
cat("     - Power relationships (4)\n")
cat("     - Alpha relationships (4)\n")
cat("     - Rater relationships (4)\n")
cat("     - Proportion effects (4)\n")
cat("     - Outcome category effects (4)\n\n")

cat("  3. kappasizepower_validation_cases (", nrow(kappa_power_validation_cases), " cases)\n")
cat("     - Boundary conditions (6)\n")
cat("     - Special proportion cases (5)\n")
cat("     - Multiple rater scenarios (4)\n")
cat("     - Category variations (4)\n")
cat("     - Effect size variations (5)\n")
cat("     - Combined scenarios (4)\n")
cat("     - Real-world applications (4)\n\n")

cat("Total scenarios:",
    nrow(kappa_power_scenarios_comprehensive) +
    nrow(kappa_power_relationship_cases) +
    nrow(kappa_power_validation_cases), "\n\n")

cat("Formats generated per dataset:\n")
cat("  ✓ RDA (R data format)\n")
cat("  ✓ CSV (comma-separated values)\n")
cat("  ✓ XLSX (Excel format)\n")
cat("  ✓ OMV (jamovi format)\n\n")

cat("Total files:", 3 * 4, "\n\n")

cat("Usage:\n")
cat("  # Load in R\n")
cat("  data(kappasizepower_scenarios_comprehensive)\n")
cat("  data(kappasizepower_relationship_cases)\n")
cat("  data(kappasizepower_validation_cases)\n\n")

cat("  # Use with function\n")
cat("  library(ClinicoPath)\n")
cat("  scenario <- kappa_power_scenarios_comprehensive[1, ]\n")
cat("  kappaSizePower(\n")
cat("    outcome = scenario$outcome_categories,\n")
cat("    kappa0 = scenario$kappa0,\n")
cat("    kappa1 = scenario$kappa1,\n")
cat("    props = scenario$proportions,\n")
cat("    raters = as.character(scenario$raters),\n")
cat("    alpha = scenario$alpha,\n")
cat("    power = scenario$power\n")
cat("  )\n\n")

cat("═══════════════════════════════════════════════════════════════\n\n")
