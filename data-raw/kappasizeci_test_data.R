# Generate Test Data for kappaSizeCI Function
# This script creates comprehensive test datasets for the kappaSizeCI jamovi analysis
# Purpose: CI-based (precision) sample size calculation for interobserver agreement studies
# Date: 2025-01-07

# Load required packages
library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

# ==============================================================================
# Dataset 1: Clinical CI Estimation Scenarios
# ==============================================================================
# Comprehensive clinical scenarios demonstrating precision-based sample size
# Focus: Different CI widths and clinical contexts

kappa_ci_scenarios_comprehensive <- tribble(
  ~domain, ~scenario_id, ~study_type, ~description, ~outcome_categories, ~raters, ~kappa0, ~kappaL, ~kappaU, ~ci_width, ~proportions, ~alpha, ~clinical_context, ~research_question,

  # Medical Diagnosis CI Estimation (Binary outcomes)
  "medical_diagnosis_ci", "ed_stroke_precision", "Emergency Department Stroke Diagnosis",
  "Sample size for precise stroke CT interpretation agreement", 2, 2, 0.70, 0.60, 0.80, 0.20, "0.15, 0.85", 0.05,
  "Two emergency radiologists establishing stroke protocol with target moderate agreement",
  "How many CT scans needed to estimate agreement (κ=0.70) with 95% CI width of 0.20?",

  "medical_diagnosis_ci", "mammo_narrow_precision", "Mammography BIRADS Classification",
  "High precision mammography agreement estimation", 2, 2, 0.80, 0.75, 0.85, 0.10, "0.20, 0.80", 0.05,
  "Two breast radiologists require narrow CI for clinical guideline recommendation",
  "How many mammograms needed to estimate good agreement (κ=0.80) with narrow CI (±0.05)?",

  "medical_diagnosis_ci", "dermato_melanoma_wide", "Dermatology Melanoma Diagnosis",
  "Wide precision acceptable for initial pilot", 2, 2, 0.60, 0.45, 0.75, 0.30, "0.10, 0.90", 0.05,
  "Two dermatologists conducting pilot reliability study with moderate precision",
  "How many cases needed for moderate agreement estimate with wide CI (±0.15)?",

  "medical_diagnosis_ci", "pathology_her2_tight", "HER2 IHC Scoring Agreement",
  "Very tight precision for regulatory validation", 2, 2, 0.85, 0.80, 0.90, 0.10, "0.25, 0.75", 0.01,
  "Two pathologists validating HER2 scoring for FDA submission requiring stringent precision",
  "How many HER2 cases needed for very narrow CI (±0.05) at α=0.01?",

  "medical_diagnosis_ci", "radiology_covid_moderate", "COVID-19 Chest CT Patterns",
  "Moderate precision for practice improvement", 2, 2, 0.65, 0.55, 0.75, 0.20, "0.30, 0.70", 0.05,
  "Two radiologists assessing COVID-19 CT patterns for quality improvement",
  "How many chest CTs needed for moderate agreement estimate with standard precision?",

  # Medical Severity CI Estimation (Ordinal outcomes - 3 categories)
  "severity_ci", "burn_severity_narrow", "Burn Severity Assessment",
  "Narrow precision for burn classification validation", 3, 2, 0.70, 0.63, 0.77, 0.14, "0.20, 0.50, 0.30", 0.05,
  "Two burn surgeons validating 3-level severity classification (1st/2nd/3rd degree)",
  "How many burn assessments needed for precise agreement estimate on severity?",

  "severity_ci", "pain_scale_moderate", "Pediatric Pain Scale (FLACC)",
  "Moderate precision for clinical implementation", 3, 3, 0.65, 0.55, 0.75, 0.20, "0.30, 0.50, 0.20", 0.05,
  "Three pediatric nurses validating pain scale with moderate precision acceptable",
  "How many pediatric pain assessments needed for 3-rater moderate precision estimate?",

  "severity_ci", "pressure_ulcer_wide", "Pressure Ulcer Staging",
  "Wide precision acceptable for training study", 3, 2, 0.60, 0.45, 0.75, 0.30, "0.40, 0.40, 0.20", 0.05,
  "Two wound care nurses in pilot training reliability study",
  "How many pressure ulcer assessments needed with wide CI for training evaluation?",

  "severity_ci", "copd_exacerbation_tight", "COPD Exacerbation Severity",
  "Tight precision for clinical guideline", 3, 2, 0.75, 0.68, 0.82, 0.14, "0.25, 0.50, 0.25", 0.05,
  "Two pulmonologists validating severity classification for treatment protocol",
  "How many COPD assessments needed for tight precision estimate?",

  "severity_ci", "aki_staging_precision", "AKI Staging Agreement",
  "Standard precision for nephrology protocol", 3, 2, 0.68, 0.58, 0.78, 0.20, "0.30, 0.45, 0.25", 0.05,
  "Two nephrologists validating AKI (Acute Kidney Injury) staging agreement",
  "How many AKI cases needed for moderate precision CI on 3-stage classification?",

  # Four-Category CI Estimation
  "four_category_ci", "tumor_grade_narrow", "Tumor Histologic Grade",
  "Narrow precision for pathology quality program", 4, 2, 0.72, 0.65, 0.79, 0.14, "0.25, 0.35, 0.25, 0.15", 0.05,
  "Two pathologists validating 4-level tumor grading (well/moderate/poor/undifferentiated)",
  "How many tumor grades needed for narrow CI on 4-category classification?",

  "four_category_ci", "gcs_assessment_moderate", "Glasgow Coma Scale",
  "Moderate precision for emergency protocol", 4, 3, 0.68, 0.58, 0.78, 0.20, "0.15, 0.30, 0.35, 0.20", 0.05,
  "Three ER nurses validating GCS assessment with multiple raters",
  "How many GCS assessments needed for 3-rater moderate precision estimate?",

  "four_category_ci", "depression_severity_wide", "Depression Severity Scale",
  "Wide precision for pilot psychiatric study", 4, 2, 0.60, 0.45, 0.75, 0.30, "0.20, 0.30, 0.30, 0.20", 0.05,
  "Two psychiatrists pilot testing depression severity classification",
  "How many depression assessments needed with wide CI for initial validation?",

  "four_category_ci", "diabetic_retino_tight", "Diabetic Retinopathy Grading",
  "Tight precision for screening program validation", 4, 2, 0.78, 0.71, 0.85, 0.14, "0.40, 0.30, 0.20, 0.10", 0.01,
  "Two ophthalmologists validating retinopathy grading for national screening program",
  "How many retinal exams needed for tight precision at stringent alpha (α=0.01)?",

  "four_category_ci", "asthma_control_standard", "Asthma Control Assessment",
  "Standard precision for pediatric protocol", 4, 2, 0.70, 0.60, 0.80, 0.20, "0.25, 0.30, 0.30, 0.15", 0.05,
  "Two pediatric pulmonologists validating asthma control classification",
  "How many asthma assessments needed for standard precision CI?",

  # Five-Category CI Estimation
  "five_category_ci", "cardiac_murmur_narrow", "Cardiac Murmur Grading",
  "Narrow precision for cardiology training", 5, 2, 0.68, 0.61, 0.75, 0.14, "0.15, 0.20, 0.30, 0.25, 0.10", 0.05,
  "Two cardiologists validating 5-level murmur grading for teaching program",
  "How many murmur assessments needed for narrow CI on 5-category scale?",

  "five_category_ci", "cancer_stage_moderate", "Cancer TNM Staging",
  "Moderate precision for oncology QA", 5, 2, 0.72, 0.62, 0.82, 0.20, "0.20, 0.25, 0.25, 0.20, 0.10", 0.05,
  "Two oncologists validating TNM staging agreement for quality assurance",
  "How many cancer staging assessments needed for moderate precision estimate?",

  "five_category_ci", "frailty_index_wide", "Geriatric Frailty Index",
  "Wide precision acceptable for geriatric pilot", 5, 3, 0.62, 0.47, 0.77, 0.30, "0.20, 0.20, 0.25, 0.20, 0.15", 0.05,
  "Three geriatricians pilot testing frailty classification with 3 raters",
  "How many frailty assessments needed for 3-rater wide precision estimate?",

  "five_category_ci", "liver_fibrosis_tight", "Liver Fibrosis Staging",
  "Tight precision for hepatology guideline", 5, 2, 0.75, 0.68, 0.82, 0.14, "0.25, 0.20, 0.20, 0.20, 0.15", 0.05,
  "Two hepatopathologists validating fibrosis staging (F0-F4) for clinical practice",
  "How many liver biopsies needed for tight precision on 5-stage classification?",

  "five_category_ci", "dysplasia_grade_standard", "Cervical Dysplasia Grading",
  "Standard precision for cytology screening", 5, 2, 0.70, 0.60, 0.80, 0.20, "0.30, 0.25, 0.20, 0.15, 0.10", 0.05,
  "Two cytopathologists validating dysplasia grading for screening program",
  "How many cytology cases needed for standard precision CI?",

  # Research Quality CI Estimation
  "research_quality_ci", "psychometric_validation_narrow", "Psychometric Scale Validation",
  "Narrow precision for scale validation study", 3, 2, 0.80, 0.73, 0.87, 0.14, "0.30, 0.50, 0.20", 0.05,
  "Two psychologists validating new assessment instrument with high precision requirement",
  "How many assessments needed for narrow CI on new psychometric scale?",

  "research_quality_ci", "biomarker_cutpoint_tight", "Biomarker Cutpoint Agreement",
  "Tight precision for diagnostic cutpoint validation", 2, 3, 0.78, 0.71, 0.85, 0.14, "0.35, 0.65", 0.01,
  "Three pathologists validating biomarker cutpoint with 3 raters at stringent alpha",
  "How many biomarker cases needed for tight precision with multiple raters?",

  "research_quality_ci", "imaging_ai_validation_moderate", "AI Algorithm Validation",
  "Moderate precision for AI vs radiologist comparison", 2, 2, 0.72, 0.62, 0.82, 0.20, "0.25, 0.75", 0.05,
  "Two radiologists validating AI algorithm agreement for regulatory approval",
  "How many images needed for moderate precision AI validation CI?",

  "research_quality_ci", "surgical_complication_wide", "Surgical Complication Classification",
  "Wide precision for quality improvement pilot", 4, 2, 0.65, 0.50, 0.80, 0.30, "0.50, 0.30, 0.15, 0.05", 0.05,
  "Two surgeons pilot testing complication classification system",
  "How many surgical cases needed for wide CI in pilot quality study?",

  "research_quality_ci", "telemedicine_agreement_standard", "Telemedicine Diagnosis Agreement",
  "Standard precision for telehealth validation", 3, 2, 0.68, 0.58, 0.78, 0.20, "0.40, 0.40, 0.20", 0.05,
  "Two physicians comparing telemedicine vs in-person diagnosis agreement",
  "How many telemedicine consultations needed for standard precision estimate?"
)

# Save Dataset 1 in multiple formats
save(kappa_ci_scenarios_comprehensive, file = here("data", "kappasizeci_scenarios_comprehensive.rda"))
write.csv(kappa_ci_scenarios_comprehensive, file = here("data", "kappasizeci_scenarios_comprehensive.csv"), row.names = FALSE)
write_xlsx(kappa_ci_scenarios_comprehensive, path = here("data", "kappasizeci_scenarios_comprehensive.xlsx"))
write_omv(kappa_ci_scenarios_comprehensive, here("data", "kappasizeci_scenarios_comprehensive.omv"), frcWrt = TRUE)

cat("\n=== Dataset 1 Complete ===\n")
cat("kappa_ci_scenarios_comprehensive: ", nrow(kappa_ci_scenarios_comprehensive), " scenarios\n")

# ==============================================================================
# Dataset 2: Parameter Relationship Cases
# ==============================================================================
# Demonstrates how different parameters affect required sample size

kappa_ci_relationship_cases <- tribble(
  ~case_name, ~relationship_type, ~description, ~outcome, ~raters, ~kappa0, ~kappaL, ~kappaU, ~ci_width, ~proportions, ~alpha, ~expected_pattern, ~teaching_point,

  # CI Width Relationships (holding kappa0 constant)
  "very_narrow_ci", "ci_width", "Very narrow CI requirement (high precision)", "2", 2, 0.70, 0.67, 0.73, 0.06, "0.50, 0.50", 0.05,
  "Very large sample size required", "Narrow CIs require much larger samples for precise estimation",

  "narrow_ci", "ci_width", "Narrow CI requirement", "2", 2, 0.70, 0.63, 0.77, 0.14, "0.50, 0.50", 0.05,
  "Large sample size required", "Narrower CIs need more subjects than wider CIs",

  "moderate_ci", "ci_width", "Moderate CI width (standard precision)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Moderate sample size required", "Standard CI width (±0.10) provides reasonable precision with moderate n",

  "wide_ci", "ci_width", "Wide CI acceptable (lower precision)", "2", 2, 0.70, 0.55, 0.85, 0.30, "0.50, 0.50", 0.05,
  "Smaller sample size required", "Wider CIs acceptable for pilot studies require fewer subjects",

  "very_wide_ci", "ci_width", "Very wide CI (pilot study)", "2", 2, 0.70, 0.50, 0.90, 0.40, "0.50, 0.50", 0.05,
  "Very small sample size", "Very wide CIs for initial feasibility studies need minimal samples",

  # Kappa0 Value Relationships (holding CI width constant)
  "low_kappa_agreement", "kappa0_value", "Low expected agreement", "2", 2, 0.30, 0.20, 0.40, 0.20, "0.50, 0.50", 0.05,
  "Larger sample needed for low kappa", "Lower kappa values have higher variance, requiring larger samples",

  "moderate_kappa_agreement", "kappa0_value", "Moderate expected agreement", "2", 2, 0.50, 0.40, 0.60, 0.20, "0.50, 0.50", 0.05,
  "Moderate sample size", "Moderate kappa has moderate variance",

  "good_kappa_agreement", "kappa0_value", "Good expected agreement", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Smaller sample needed for high kappa", "Higher kappa values have lower variance, requiring fewer subjects",

  "excellent_kappa_agreement", "kappa0_value", "Excellent expected agreement", "2", 2, 0.85, 0.75, 0.95, 0.20, "0.50, 0.50", 0.05,
  "Small sample size", "Very high kappa has low variance, minimal sample needed",

  # Alpha Level Relationships (holding CI width constant)
  "alpha_10", "alpha_level", "Liberal alpha (90% CI)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.10,
  "Smallest sample size", "90% CI requires fewer subjects than 95% CI",

  "alpha_05", "alpha_level", "Standard alpha (95% CI)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Moderate sample size", "95% CI is standard for most clinical studies",

  "alpha_01", "alpha_level", "Stringent alpha (99% CI)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.01,
  "Larger sample size", "99% CI requires more subjects for same precision",

  "alpha_001", "alpha_level", "Very stringent alpha (99.9% CI)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.001,
  "Very large sample size", "Higher confidence levels need substantially more subjects",

  # Rater Number Relationships (holding CI width constant)
  "two_raters", "rater_number", "Standard two-rater design", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Baseline sample size", "Two raters is most common and efficient design",

  "three_raters", "rater_number", "Three-rater design", "2", 3, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Potentially different sample size", "Three raters provide more information per subject",

  "four_raters", "rater_number", "Four-rater design", "2", 4, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Potentially smaller sample size", "Multiple raters can reduce required subjects but increase cost",

  "five_raters", "rater_number", "Five-rater design", "2", 5, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "More raters may reduce n", "Many raters provide rich data but practical constraints apply",

  # Proportion Balance Effects (holding CI width constant)
  "balanced_props", "proportion_balance", "Perfectly balanced categories", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Optimal sample efficiency", "Balanced proportions maximize statistical efficiency",

  "moderate_imbalance", "proportion_balance", "Moderately imbalanced", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.30, 0.70", 0.05,
  "Slightly larger sample needed", "Moderate imbalance increases sample size modestly",

  "severe_imbalance", "proportion_balance", "Severely imbalanced (rare outcome)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.10, 0.90", 0.05,
  "Much larger sample required", "Rare outcomes require substantially more subjects for same precision",

  "extreme_imbalance", "proportion_balance", "Extreme imbalance (very rare)", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.05, 0.95", 0.05,
  "Very large sample needed", "Extremely rare outcomes dramatically increase required sample size",

  # Outcome Category Number Effects (holding CI width and kappa0 constant)
  "two_categories", "category_number", "Binary classification", "2", 2, 0.70, 0.60, 0.80, 0.20, "0.50, 0.50", 0.05,
  "Baseline sample size", "Binary outcomes have simplest structure",

  "three_categories", "category_number", "Three-level ordinal", "3", 2, 0.70, 0.60, 0.80, 0.20, "0.33, 0.34, 0.33", 0.05,
  "Larger sample than binary", "More categories increase complexity and sample requirements",

  "four_categories", "category_number", "Four-level ordinal", "4", 2, 0.70, 0.60, 0.80, 0.20, "0.25, 0.25, 0.25, 0.25", 0.05,
  "Even larger sample needed", "Additional categories further increase sample size",

  "five_categories", "category_number", "Five-level ordinal", "5", 2, 0.70, 0.60, 0.80, 0.20, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05,
  "Largest sample required", "Many categories require most subjects for same precision"
)

# Save Dataset 2 in multiple formats
save(kappa_ci_relationship_cases, file = here("data", "kappasizeci_relationship_cases.rda"))
write.csv(kappa_ci_relationship_cases, file = here("data", "kappasizeci_relationship_cases.csv"), row.names = FALSE)
write_xlsx(kappa_ci_relationship_cases, path = here("data", "kappasizeci_relationship_cases.xlsx"))
write_omv(kappa_ci_relationship_cases, here("data", "kappasizeci_relationship_cases.omv"), frcWrt = TRUE)

cat("\n=== Dataset 2 Complete ===\n")
cat("kappa_ci_relationship_cases: ", nrow(kappa_ci_relationship_cases), " cases\n")

# ==============================================================================
# Dataset 3: Validation and Edge Cases
# ==============================================================================
# Test boundary conditions, edge cases, and special scenarios

kappa_ci_validation_cases <- tribble(
  ~test_case, ~test_type, ~description, ~outcome, ~raters, ~kappa0, ~kappaL, ~kappaU, ~proportions, ~alpha, ~expected_outcome, ~validation_purpose,

  # Valid Boundary Cases
  "min_kappa_values", "boundary", "Minimum kappa values", "2", 2, 0.10, 0.05, 0.15, "0.50, 0.50", 0.05,
  "Should succeed with valid parameters", "Test lower boundary of kappa range",

  "max_kappa_values", "boundary", "Maximum kappa values", "2", 2, 0.95, 0.90, 0.99, "0.50, 0.50", 0.05,
  "Should succeed with valid parameters", "Test upper boundary of kappa range",

  "min_alpha", "boundary", "Minimum alpha value", "2", 2, 0.70, 0.60, 0.80, "0.50, 0.50", 0.01,
  "Should succeed with very stringent CI", "Test stringent confidence level (99% CI)",

  "max_alpha", "boundary", "Maximum reasonable alpha", "2", 2, 0.70, 0.60, 0.80, "0.50, 0.50", 0.10,
  "Should succeed with liberal CI", "Test liberal confidence level (90% CI)",

  "narrow_valid_ci", "boundary", "Narrowest practical CI width", "2", 2, 0.70, 0.68, 0.72, "0.50, 0.50", 0.05,
  "Should succeed but large sample", "Test very narrow but valid CI width",

  "wide_valid_ci", "boundary", "Widest practical CI width", "2", 2, 0.70, 0.40, 0.99, "0.50, 0.50", 0.05,
  "Should succeed with small sample", "Test very wide but valid CI width",

  # Special Proportion Cases
  "perfect_balance_2cat", "proportion", "Perfect 50-50 split (binary)", "2", 2, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - optimal efficiency", "Test perfectly balanced binary proportions",

  "perfect_balance_3cat", "proportion", "Perfect balance (3 categories)", "3", 2, 0.70, 0.60, 0.80, "0.333, 0.334, 0.333", 0.05,
  "Should succeed - optimal efficiency", "Test perfectly balanced 3-category proportions",

  "perfect_balance_4cat", "proportion", "Perfect balance (4 categories)", "4", 2, 0.70, 0.60, 0.80, "0.25, 0.25, 0.25, 0.25", 0.05,
  "Should succeed - optimal efficiency", "Test perfectly balanced 4-category proportions",

  "perfect_balance_5cat", "proportion", "Perfect balance (5 categories)", "5", 2, 0.70, 0.60, 0.80, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05,
  "Should succeed - optimal efficiency", "Test perfectly balanced 5-category proportions",

  "rare_event_5pct", "proportion", "Rare event (5% prevalence)", "2", 2, 0.70, 0.60, 0.80, "0.05, 0.95", 0.05,
  "Should succeed but large sample", "Test rare event impact on sample size",

  # Multiple Rater Scenarios
  "min_raters", "rater_count", "Minimum 2 raters", "2", 2, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - standard design", "Test minimum rater count",

  "three_raters", "rater_count", "Three raters", "2", 3, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - common multi-rater", "Test 3-rater design",

  "four_raters", "rater_count", "Four raters", "2", 4, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - multiple raters", "Test 4-rater design",

  "max_raters", "rater_count", "Maximum 5 raters", "2", 5, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - many raters", "Test maximum rater count",

  # Category Variations with Consistent CI Width
  "two_cat_ci20", "category_count", "Binary with 0.20 CI width", "2", 2, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - simplest case", "Test binary classification",

  "three_cat_ci20", "category_count", "Three categories with 0.20 CI width", "3", 2, 0.70, 0.60, 0.80, "0.33, 0.34, 0.33", 0.05,
  "Should succeed - ordinal scale", "Test 3-level ordinal scale",

  "four_cat_ci20", "category_count", "Four categories with 0.20 CI width", "4", 2, 0.70, 0.60, 0.80, "0.25, 0.25, 0.25, 0.25", 0.05,
  "Should succeed - multi-level", "Test 4-level classification",

  "five_cat_ci20", "category_count", "Five categories with 0.20 CI width", "5", 2, 0.70, 0.60, 0.80, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05,
  "Should succeed - complex scale", "Test 5-level classification",

  # Kappa0 Value Variations with Consistent CI Width
  "low_kappa_ci20", "kappa0_variation", "Low agreement (κ=0.30)", "2", 2, 0.30, 0.20, 0.40, "0.50, 0.50", 0.05,
  "Should succeed - poor agreement", "Test low baseline kappa",

  "fair_kappa_ci20", "kappa0_variation", "Fair agreement (κ=0.50)", "2", 2, 0.50, 0.40, 0.60, "0.50, 0.50", 0.05,
  "Should succeed - moderate agreement", "Test fair baseline kappa",

  "good_kappa_ci20", "kappa0_variation", "Good agreement (κ=0.70)", "2", 2, 0.70, 0.60, 0.80, "0.50, 0.50", 0.05,
  "Should succeed - good agreement", "Test good baseline kappa",

  "excellent_kappa_ci20", "kappa0_variation", "Excellent agreement (κ=0.85)", "2", 2, 0.85, 0.75, 0.95, "0.50, 0.50", 0.05,
  "Should succeed - high agreement", "Test excellent baseline kappa",

  "very_high_kappa_ci14", "kappa0_variation", "Very high agreement (κ=0.90) narrow CI", "2", 2, 0.90, 0.83, 0.97, "0.50, 0.50", 0.05,
  "Should succeed - very high kappa", "Test very high kappa with narrow CI",

  # Combined Challenging Scenarios
  "rare_narrow_ci", "combined", "Rare event + narrow CI", "2", 2, 0.70, 0.65, 0.75, "0.10, 0.90", 0.05,
  "Should succeed but very large sample", "Test combination of challenging conditions",

  "low_kappa_narrow_ci", "combined", "Low kappa + narrow CI", "2", 2, 0.35, 0.30, 0.40, "0.50, 0.50", 0.05,
  "Should succeed but large sample", "Test low agreement with high precision need",

  "five_cat_rare_narrow", "combined", "5 categories + rare + narrow CI", "5", 2, 0.70, 0.65, 0.75, "0.10, 0.20, 0.30, 0.30, 0.10", 0.05,
  "Should succeed but very large sample", "Test multiple challenging factors",

  "stringent_narrow_rare", "combined", "99% CI + narrow width + rare", "2", 2, 0.70, 0.67, 0.73, "0.10, 0.90", 0.01,
  "Should succeed but extremely large sample", "Test extreme precision requirements",

  # Real-world Clinical Applications
  "diagnostic_validation", "real_world", "Diagnostic test validation study", "2", 2, 0.75, 0.68, 0.82, "0.30, 0.70", 0.05,
  "Should succeed - realistic diagnostic scenario", "Common diagnostic accuracy study design",

  "pathology_qc", "real_world", "Pathology quality control", "3", 3, 0.70, 0.62, 0.78, "0.40, 0.40, 0.20", 0.05,
  "Should succeed - QC program design", "Multi-rater pathology quality assessment",

  "radiology_training", "real_world", "Radiology training evaluation", "4", 2, 0.65, 0.55, 0.75, "0.30, 0.30, 0.25, 0.15", 0.05,
  "Should succeed - training assessment", "Educational program evaluation",

  "clinical_guideline", "real_world", "Clinical guideline implementation", "2", 2, 0.80, 0.75, 0.85, "0.25, 0.75", 0.01,
  "Should succeed - guideline validation", "Stringent requirements for practice guidelines"
)

# Save Dataset 3 in multiple formats
save(kappa_ci_validation_cases, file = here("data", "kappasizeci_validation_cases.rda"))
write.csv(kappa_ci_validation_cases, file = here("data", "kappasizeci_validation_cases.csv"), row.names = FALSE)
write_xlsx(kappa_ci_validation_cases, path = here("data", "kappasizeci_validation_cases.xlsx"))
write_omv(kappa_ci_validation_cases, here("data", "kappasizeci_validation_cases.omv"), frcWrt = TRUE)

cat("\n=== Dataset 3 Complete ===\n")
cat("kappa_ci_validation_cases: ", nrow(kappa_ci_validation_cases), " cases\n")

# ==============================================================================
# Summary Statistics
# ==============================================================================
cat("\n========================================\n")
cat("KAPPASIZECI TEST DATA GENERATION COMPLETE\n")
cat("========================================\n\n")

cat("Dataset 1 - Clinical CI Scenarios:\n")
cat("  Files: kappasizeci_scenarios_comprehensive.[rda|csv|xlsx|omv]\n")
cat("  Scenarios: ", nrow(kappa_ci_scenarios_comprehensive), "\n")
cat("  Focus: Comprehensive clinical contexts with different CI widths\n\n")

cat("Dataset 2 - Parameter Relationships:\n")
cat("  Files: kappasizeci_relationship_cases.[rda|csv|xlsx|omv]\n")
cat("  Cases: ", nrow(kappa_ci_relationship_cases), "\n")
cat("  Focus: How CI width, kappa0, alpha, raters, and proportions affect sample size\n\n")

cat("Dataset 3 - Validation Cases:\n")
cat("  Files: kappasizeci_validation_cases.[rda|csv|xlsx|omv]\n")
cat("  Cases: ", nrow(kappa_ci_validation_cases), "\n")
cat("  Focus: Boundary conditions, edge cases, and real-world applications\n\n")

cat("Total test scenarios: ",
    nrow(kappa_ci_scenarios_comprehensive) +
    nrow(kappa_ci_relationship_cases) +
    nrow(kappa_ci_validation_cases), "\n")

cat("\n========================================\n")
cat("All files saved to data/ directory\n")
cat("========================================\n")
