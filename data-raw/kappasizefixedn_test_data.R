# Generate Test Data for kappaSizeFixedN Function
# This script creates comprehensive test datasets for the kappaSizeFixedN jamovi analysis
# Purpose: Fixed sample size - determine lowest detectable kappa value
# Date: 2025-01-07

# Load required packages
library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

# ==============================================================================
# Dataset 1: Clinical Fixed-N Scenarios
# ==============================================================================
# Comprehensive clinical scenarios where sample size is FIXED by constraints
# Focus: Resource-limited settings, predetermined budgets, pilot studies

kappa_fixedn_scenarios_comprehensive <- tribble(
  ~domain, ~scenario_id, ~study_type, ~description, ~outcome_categories, ~raters, ~kappa0, ~proportions, ~alpha, ~n, ~constraint_type, ~clinical_context, ~research_question,

  # Small Sample Fixed-N (n=30-50) - Resource Constrained
  "small_sample_fixed", "pilot_dermato_n30", "Dermatology Pilot Study",
  "Small pilot: What agreement detectable with n=30?", 2, 2, 0.40, "0.10, 0.90", 0.05, 30, "pilot_budget",
  "Dermatology resident training evaluation with limited budget for 30 cases",
  "With only 30 melanoma cases available, what minimum agreement can we detect?",

  "small_sample_fixed", "emergency_protocol_n40", "Emergency Protocol Feasibility",
  "Emergency department feasibility: n=40 stroke CTs", 2, 2, 0.50, "0.15, 0.85", 0.05, 40, "time_constraint",
  "ED can review 40 stroke CT cases in 2 months, need to know detectable agreement",
  "Given time constraints (2 months), what minimum kappa is detectable with n=40?",

  "small_sample_fixed", "rare_pathology_n35", "Rare Pathology Limited Cases",
  "Rare tumor: only 35 cases available annually", 3, 2, 0.40, "0.30, 0.40, 0.30", 0.05, 35, "case_availability",
  "Rare tumor type with only 35 cases per year in tertiary center",
  "What minimum 3-level grading agreement can we assess with 35 rare cases?",

  "small_sample_fixed", "training_validation_n50", "Training Program Validation",
  "Post-training assessment with n=50 budget", 2, 3, 0.50, "0.25, 0.75", 0.05, 50, "educational_budget",
  "Three pathology trainees, budget allows 50 cases for competency assessment",
  "With 50-case training budget and 3 raters, what agreement level is detectable?",

  "small_sample_fixed", "telemedicine_pilot_n45", "Telemedicine Pilot Study",
  "Telemedicine diagnostic agreement pilot", 2, 2, 0.50, "0.30, 0.70", 0.05, 45, "pilot_phase",
  "Pilot telemedicine program comparing remote vs in-person diagnosis",
  "Pilot phase limited to 45 consultations: what minimum agreement detectable?",

  # Moderate Sample Fixed-N (n=75-150) - Standard Constraints
  "moderate_sample_fixed", "radiology_qa_n100", "Radiology QA Program",
  "Standard QA sample: annual 100 mammograms", 2, 2, 0.50, "0.20, 0.80", 0.05, 100, "qa_protocol",
  "Annual quality assurance reviewing 100 mammograms per radiologist pair",
  "With standard QA protocol (n=100), what minimum BIRADS agreement detectable?",

  "moderate_sample_fixed", "pathology_accred_n120", "Pathology Accreditation",
  "Accreditation requirement: minimum 120 cases", 4, 2, 0.50, "0.25, 0.30, 0.30, 0.15", 0.05, 120, "regulatory_minimum",
  "Accreditation body requires minimum 120 tumor grades for validation",
  "Meeting minimum accreditation n=120, what 4-level grading agreement detectable?",

  "moderate_sample_fixed", "clinical_trial_n150", "Clinical Trial Agreement",
  "Biomarker agreement in n=150 trial", 2, 2, 0.60, "0.35, 0.65", 0.05, 150, "trial_enrollment",
  "Clinical trial enrolled 150 patients, need central pathology review agreement",
  "Trial n=150 fixed by enrollment, what biomarker scoring agreement detectable?",

  "moderate_sample_fixed", "multicenter_qa_n90", "Multicenter QA Study",
  "Multicenter study: 30 cases × 3 sites", 3, 2, 0.50, "0.30, 0.45, 0.25", 0.05, 90, "site_contribution",
  "Three centers each contributing 30 cases for standardization study",
  "With n=90 from 3 centers, what 3-level severity agreement detectable?",

  "moderate_sample_fixed", "insurance_audit_n80", "Insurance Audit Sample",
  "Audit sample size predetermined: n=80", 2, 2, 0.40, "0.40, 0.60", 0.05, 80, "audit_protocol",
  "Insurance company audits 80 randomly selected diagnosis codes annually",
  "Fixed audit n=80 per protocol, what diagnosis agreement is detectable?",

  # Large Sample Fixed-N (n=200-500) - Well-Resourced Studies
  "large_sample_fixed", "registry_validation_n250", "Cancer Registry Validation",
  "Registry validation: 250 cases per year", 5, 2, 0.60, "0.20, 0.25, 0.25, 0.20, 0.10", 0.05, 250, "registry_capacity",
  "Cancer registry has capacity to validate 250 staging cases annually",
  "With registry capacity n=250, what 5-stage TNM agreement detectable?",

  "large_sample_fixed", "screening_program_n300", "Screening Program QA",
  "National screening: 300 case QA sample", 2, 2, 0.70, "0.25, 0.75", 0.01, 300, "national_program",
  "National mammography screening program with 300-case annual QA",
  "Stringent α=0.01, n=300 QA sample: what minimum screening agreement detectable?",

  "large_sample_fixed", "ai_validation_n400", "AI Algorithm Validation",
  "AI validation study: 400 images budgeted", 2, 2, 0.60, "0.30, 0.70", 0.05, 400, "validation_protocol",
  "AI diagnostic algorithm validation with 400 expert-labeled images",
  "With 400-image validation set, what AI-human agreement is detectable?",

  "large_sample_fixed", "biobank_study_n500", "Biobank Retrospective Study",
  "Biobank cohort: 500 cases with tissue", 4, 2, 0.65, "0.30, 0.30, 0.25, 0.15", 0.05, 500, "biobank_availability",
  "Biobank has 500 cases with adequate tissue for central review",
  "Using all 500 biobank cases, what 4-grade agreement is detectable?",

  "large_sample_fixed", "pharma_trial_n200", "Pharmaceutical Trial Endpoint",
  "Phase III trial: 200 patients enrolled", 3, 3, 0.60, "0.35, 0.40, 0.25", 0.05, 200, "trial_completion",
  "Completed phase III trial with 200 patients, 3-rater endpoint assessment",
  "Trial complete with n=200 and 3 raters, what endpoint agreement detectable?",

  # Very Large Sample Fixed-N (n=750-1000+) - Major Studies
  "very_large_fixed", "national_survey_n1000", "National Health Survey",
  "National survey: 1000 participant target", 2, 2, 0.70, "0.50, 0.50", 0.05, 1000, "survey_design",
  "National health survey with predetermined 1000-participant sample",
  "Large national survey n=1000, what diagnostic agreement is detectable?",

  "very_large_fixed", "consortium_study_n800", "International Consortium",
  "Consortium study: 100 cases × 8 centers", 3, 2, 0.65, "0.30, 0.45, 0.25", 0.01, 800, "center_contribution",
  "International consortium, each of 8 centers contributes 100 cases",
  "With n=800 consortium cases at α=0.01, what 3-level agreement detectable?",

  "very_large_fixed", "ehr_validation_n750", "EHR Data Validation",
  "Electronic health record: 750 chart reviews", 2, 2, 0.60, "0.40, 0.60", 0.05, 750, "chart_review_capacity",
  "EHR validation study with resources for 750 manual chart reviews",
  "Chart review capacity n=750, what diagnosis coding agreement detectable?",

  "very_large_fixed", "longitudinal_cohort_n600", "Longitudinal Cohort Follow-up",
  "Cohort study: 600 patients reached at follow-up", 4, 2, 0.60, "0.25, 0.30, 0.30, 0.15", 0.05, 600, "cohort_retention",
  "Longitudinal cohort retained 600 of 800 original patients for imaging review",
  "With n=600 retained patients, what 4-level imaging agreement detectable?",

  "very_large_fixed", "database_linkage_n900", "Database Linkage Study",
  "Linked databases: 900 matched patients", 2, 2, 0.65, "0.30, 0.70", 0.05, 900, "record_linkage",
  "Successful linkage of 900 patients between two clinical databases",
  "Database linkage yielded n=900 matches, what diagnosis agreement detectable?",

  # Multiple Rater Fixed-N Scenarios
  "multi_rater_fixed", "training_3raters_n60", "Three-Rater Training Study",
  "Training evaluation: 3 raters, 60 cases", 2, 3, 0.50, "0.30, 0.70", 0.05, 60, "training_program",
  "Pathology training program with 3 trainees reviewing 60 cases",
  "With 3 raters and n=60 training cases, what minimum agreement detectable?",

  "multi_rater_fixed", "consensus_4raters_n80", "Four-Rater Consensus Study",
  "Consensus panel: 4 experts, 80 cases", 3, 4, 0.60, "0.30, 0.40, 0.30", 0.05, 80, "expert_panel",
  "Four experts forming consensus panel for guideline development",
  "Expert panel (4 raters) with n=80, what 3-level consensus detectable?",

  "multi_rater_fixed", "multicenter_5raters_n100", "Five-Center Comparison",
  "Five centers: 1 rater each, 100 cases", 2, 5, 0.55, "0.25, 0.75", 0.05, 100, "center_comparison",
  "Comparing diagnostic practices across 5 centers (1 rater per center)",
  "Five-center study with n=100 shared cases, what agreement detectable?",

  # Stringent Alpha Fixed-N Scenarios
  "stringent_alpha_fixed", "regulatory_n200_alpha01", "Regulatory Submission",
  "Regulatory requirement: n=200, α=0.01", 2, 2, 0.70, "0.30, 0.70", 0.01, 200, "regulatory_standard",
  "FDA submission requires 200 cases with 99% confidence for diagnostic agreement",
  "Meeting regulatory n=200 at α=0.01, what minimum agreement detectable?"
)

# Save Dataset 1 in multiple formats
save(kappa_fixedn_scenarios_comprehensive, file = here("data", "kappasizefixedn_scenarios_comprehensive.rda"))
write.csv(kappa_fixedn_scenarios_comprehensive, file = here("data", "kappasizefixedn_scenarios_comprehensive.csv"), row.names = FALSE)
write_xlsx(kappa_fixedn_scenarios_comprehensive, path = here("data", "kappasizefixedn_scenarios_comprehensive.xlsx"))
write_omv(kappa_fixedn_scenarios_comprehensive, here("data", "kappasizefixedn_scenarios_comprehensive.omv"), frcWrt = TRUE)

cat("\n=== Dataset 1 Complete ===\n")
cat("kappa_fixedn_scenarios_comprehensive: ", nrow(kappa_fixedn_scenarios_comprehensive), " scenarios\n")

# ==============================================================================
# Dataset 2: Sample Size Impact Cases
# ==============================================================================
# Demonstrates how different fixed sample sizes affect detectable kappa

kappa_fixedn_power_cases <- tribble(
  ~case_name, ~relationship_type, ~description, ~outcome, ~raters, ~kappa0, ~proportions, ~alpha, ~n, ~expected_pattern, ~teaching_point,

  # Sample Size Progression (holding other parameters constant)
  "n_20_minimum", "sample_size", "Very small pilot: n=20", "2", 2, 0.50, "0.50, 0.50", 0.05, 20,
  "Limited precision, wide detectable range", "Very small samples can only detect large effects",

  "n_30_small_pilot", "sample_size", "Small pilot study: n=30", "2", 2, 0.50, "0.50, 0.50", 0.05, 30,
  "Moderate precision improvement", "Small pilots require substantial true kappa differences",

  "n_50_feasibility", "sample_size", "Feasibility study: n=50", "2", 2, 0.50, "0.50, 0.50", 0.05, 50,
  "Better precision than n=30", "Feasibility studies have moderate detection capability",

  "n_75_small_standard", "sample_size", "Small standard study: n=75", "2", 2, 0.50, "0.50, 0.50", 0.05, 75,
  "Reasonable precision emerging", "Approaching useful precision for clinical decisions",

  "n_100_standard", "sample_size", "Standard study: n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Good precision for most purposes", "Standard sample size provides reliable detection",

  "n_150_moderate", "sample_size", "Moderate-large study: n=150", "2", 2, 0.50, "0.50, 0.50", 0.05, 150,
  "Very good precision", "Larger samples detect smaller kappa differences",

  "n_200_large", "sample_size", "Large study: n=200", "2", 2, 0.50, "0.50, 0.50", 0.05, 200,
  "High precision achieved", "Large samples provide narrow detectable range",

  "n_300_very_large", "sample_size", "Very large study: n=300", "2", 2, 0.50, "0.50, 0.50", 0.05, 300,
  "Very high precision", "Very large samples approach asymptotic precision",

  "n_500_major", "sample_size", "Major study: n=500", "2", 2, 0.50, "0.50, 0.50", 0.05, 500,
  "Near-maximum precision", "Diminishing returns beyond this sample size",

  # Kappa0 Value Impact (holding n constant)
  "kappa0_20_n100", "kappa0_impact", "Low null hypothesis κ0=0.20, n=100", "2", 2, 0.20, "0.50, 0.50", 0.05, 100,
  "Wider detectable range from low base", "Starting from low kappa allows detecting larger range",

  "kappa0_40_n100", "kappa0_impact", "Moderate null hypothesis κ0=0.40, n=100", "2", 2, 0.40, "0.50, 0.50", 0.05, 100,
  "Moderate detectable range", "Mid-range kappa0 provides moderate detection window",

  "kappa0_60_n100", "kappa0_impact", "Good null hypothesis κ0=0.60, n=100", "2", 2, 0.60, "0.50, 0.50", 0.05, 100,
  "Narrower detectable range", "Higher kappa0 limits upper detection range",

  "kappa0_80_n100", "kappa0_impact", "High null hypothesis κ0=0.80, n=100", "2", 2, 0.80, "0.50, 0.50", 0.05, 100,
  "Very narrow upper range", "Very high kappa0 leaves little room for improvement",

  # Alpha Level Impact (holding n=100 constant)
  "alpha_10_n100", "alpha_impact", "Liberal alpha α=0.10 (90% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.10, 100,
  "Widest detectable range", "Liberal alpha allows detecting smaller differences",

  "alpha_05_n100", "alpha_impact", "Standard alpha α=0.05 (95% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Standard detectable range", "Standard alpha balances precision and confidence",

  "alpha_01_n100", "alpha_impact", "Stringent alpha α=0.01 (99% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.01, 100,
  "Narrower detectable range", "Stringent alpha requires larger differences for detection",

  "alpha_001_n100", "alpha_impact", "Very stringent α=0.001 (99.9% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.001, 100,
  "Very narrow detectable range", "Extreme confidence reduces detection sensitivity",

  # Rater Number Impact (holding n=100 constant)
  "raters_2_n100", "rater_impact", "Two raters, n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Baseline detection capability", "Standard two-rater design",

  "raters_3_n100", "rater_impact", "Three raters, n=100", "2", 3, 0.50, "0.50, 0.50", 0.05, 100,
  "Improved detection vs 2 raters", "More raters provide more information per subject",

  "raters_4_n100", "rater_impact", "Four raters, n=100", "2", 4, 0.50, "0.50, 0.50", 0.05, 100,
  "Better detection than 3 raters", "Multiple raters enhance detection capability",

  "raters_5_n100", "rater_impact", "Five raters, n=100", "2", 5, 0.50, "0.50, 0.50", 0.05, 100,
  "Maximum rater benefit", "Diminishing returns with many raters",

  # Category Number Impact (holding n=100 constant)
  "cat_2_n100", "category_impact", "Binary (2 categories), n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Best detection for given n", "Binary simplest, most efficient",

  "cat_3_n100", "category_impact", "Three categories, n=100", "3", 2, 0.50, "0.33, 0.34, 0.33", 0.05, 100,
  "Reduced detection vs binary", "More categories reduce detection capability",

  "cat_4_n100", "category_impact", "Four categories, n=100", "4", 2, 0.50, "0.25, 0.25, 0.25, 0.25", 0.05, 100,
  "Further reduced detection", "Many categories require larger n for same detection",

  "cat_5_n100", "category_impact", "Five categories, n=100", "5", 2, 0.50, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05, 100,
  "Lowest detection capability", "Maximum categories limit detectable range",

  # Proportion Balance Impact (holding n=100 constant)
  "balanced_n100", "proportion_impact", "Perfectly balanced 50-50, n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Optimal detection efficiency", "Balanced proportions maximize detection power",

  "moderate_imbal_n100", "proportion_impact", "Moderate imbalance 30-70, n=100", "2", 2, 0.50, "0.30, 0.70", 0.05, 100,
  "Slightly reduced detection", "Moderate imbalance has modest impact",

  "severe_imbal_n100", "proportion_impact", "Severe imbalance 10-90, n=100", "2", 2, 0.50, "0.10, 0.90", 0.05, 100,
  "Substantially reduced detection", "Rare events greatly limit detectable range",

  "extreme_imbal_n100", "proportion_impact", "Extreme imbalance 5-95, n=100", "2", 2, 0.50, "0.05, 0.95", 0.05, 100,
  "Severely limited detection", "Extremely rare events can barely detect differences"
)

# Save Dataset 2 in multiple formats
save(kappa_fixedn_power_cases, file = here("data", "kappasizefixedn_power_cases.rda"))
write.csv(kappa_fixedn_power_cases, file = here("data", "kappasizefixedn_power_cases.csv"), row.names = FALSE)
write_xlsx(kappa_fixedn_power_cases, path = here("data", "kappasizefixedn_power_cases.xlsx"))
write_omv(kappa_fixedn_power_cases, here("data", "kappasizefixedn_power_cases.omv"), frcWrt = TRUE)

cat("\n=== Dataset 2 Complete ===\n")
cat("kappa_fixedn_power_cases: ", nrow(kappa_fixedn_power_cases), " cases\n")

# ==============================================================================
# Dataset 3: Validation and Edge Cases
# ==============================================================================
# Test boundary conditions, edge cases, and special scenarios

kappa_fixedn_validation_cases <- tribble(
  ~test_case, ~test_type, ~description, ~outcome, ~raters, ~kappa0, ~proportions, ~alpha, ~n, ~expected_outcome, ~validation_purpose,

  # Minimum Valid Sample Sizes
  "min_n_10", "boundary", "Minimum practical n=10", "2", 2, 0.50, "0.50, 0.50", 0.05, 10,
  "Should succeed but limited precision", "Test very small sample boundary",

  "min_n_15", "boundary", "Very small n=15", "2", 2, 0.50, "0.50, 0.50", 0.05, 15,
  "Should succeed with caution", "Test small sample handling",

  "min_n_20", "boundary", "Small pilot n=20", "2", 2, 0.50, "0.50, 0.50", 0.05, 20,
  "Should succeed - common pilot size", "Test typical pilot sample",

  # Large Sample Sizes
  "large_n_500", "boundary", "Large study n=500", "2", 2, 0.50, "0.50, 0.50", 0.05, 500,
  "Should succeed with high precision", "Test large sample behavior",

  "large_n_1000", "boundary", "Very large study n=1000", "2", 2, 0.50, "0.50, 0.50", 0.05, 1000,
  "Should succeed with very high precision", "Test very large sample",

  "large_n_2000", "boundary", "Major study n=2000", "2", 2, 0.50, "0.50, 0.50", 0.05, 2000,
  "Should succeed - maximum precision", "Test asymptotic behavior",

  # Kappa0 Boundary Values
  "kappa0_min", "boundary", "Minimum kappa0=0.01", "2", 2, 0.01, "0.50, 0.50", 0.05, 100,
  "Should succeed at lower boundary", "Test minimum kappa0 value",

  "kappa0_low", "boundary", "Low kappa0=0.10", "2", 2, 0.10, "0.50, 0.50", 0.05, 100,
  "Should succeed - poor agreement baseline", "Test low kappa0",

  "kappa0_high", "boundary", "High kappa0=0.90", "2", 2, 0.90, "0.50, 0.50", 0.05, 100,
  "Should succeed - excellent baseline", "Test high kappa0",

  "kappa0_max", "boundary", "Maximum kappa0=0.99", "2", 2, 0.99, "0.50, 0.50", 0.05, 100,
  "Should succeed at upper boundary", "Test maximum kappa0 value",

  # Alpha Boundary Values
  "alpha_min_001", "boundary", "Minimum alpha=0.01", "2", 2, 0.50, "0.50, 0.50", 0.01, 100,
  "Should succeed - stringent confidence", "Test stringent alpha",

  "alpha_max_10", "boundary", "Maximum alpha=0.10", "2", 2, 0.50, "0.50, 0.50", 0.10, 100,
  "Should succeed - liberal confidence", "Test liberal alpha",

  # Multiple Categories with Fixed N
  "cat3_n50", "category_test", "Three categories, small n=50", "3", 2, 0.50, "0.33, 0.34, 0.33", 0.05, 50,
  "Should succeed - moderate complexity", "Test 3-category with small n",

  "cat3_n100", "category_test", "Three categories, n=100", "3", 2, 0.50, "0.33, 0.34, 0.33", 0.05, 100,
  "Should succeed - standard", "Test 3-category standard n",

  "cat4_n100", "category_test", "Four categories, n=100", "4", 2, 0.50, "0.25, 0.25, 0.25, 0.25", 0.05, 100,
  "Should succeed - higher complexity", "Test 4-category",

  "cat5_n100", "category_test", "Five categories, n=100", "5", 2, 0.50, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05, 100,
  "Should succeed - maximum complexity", "Test 5-category",

  "cat5_n200", "category_test", "Five categories, large n=200", "5", 2, 0.50, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05, 200,
  "Should succeed with better precision", "Test 5-category with larger n",

  # Multiple Raters with Fixed N
  "raters3_n60", "rater_test", "Three raters, n=60", "2", 3, 0.50, "0.50, 0.50", 0.05, 60,
  "Should succeed - multi-rater", "Test 3-rater design",

  "raters4_n80", "rater_test", "Four raters, n=80", "2", 4, 0.50, "0.50, 0.50", 0.05, 80,
  "Should succeed - many raters", "Test 4-rater design",

  "raters5_n100", "rater_test", "Five raters, n=100", "2", 5, 0.50, "0.50, 0.50", 0.05, 100,
  "Should succeed - maximum raters", "Test 5-rater design",

  # Proportion Imbalance Tests
  "balanced_n50", "proportion_test", "Perfect balance, n=50", "2", 2, 0.50, "0.50, 0.50", 0.05, 50,
  "Should succeed - optimal", "Test balanced proportions",

  "moderate_imbal_n50", "proportion_test", "Moderate imbalance 30-70, n=50", "2", 2, 0.50, "0.30, 0.70", 0.05, 50,
  "Should succeed with reduced precision", "Test moderate imbalance",

  "severe_imbal_n50", "proportion_test", "Severe imbalance 10-90, n=50", "2", 2, 0.50, "0.10, 0.90", 0.05, 50,
  "Should succeed but limited detection", "Test severe imbalance",

  "rare_event_n100", "proportion_test", "Rare event 5%, n=100", "2", 2, 0.50, "0.05, 0.95", 0.05, 100,
  "Should succeed with very limited range", "Test rare event impact",

  # Combined Challenging Scenarios
  "small_n_rare", "combined", "Small n=30 + rare event 10%", "2", 2, 0.50, "0.10, 0.90", 0.05, 30,
  "Should succeed but severely limited", "Test small sample + rare event",

  "small_n_5cat", "combined", "Small n=50 + five categories", "5", 2, 0.50, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05, 50,
  "Should succeed with very limited range", "Test small n + many categories",

  "stringent_small", "combined", "Stringent α=0.01 + small n=40", "2", 2, 0.50, "0.50, 0.50", 0.01, 40,
  "Should succeed with reduced detection", "Test stringent alpha + small n",

  "many_raters_small", "combined", "Five raters + small n=60", "2", 5, 0.50, "0.50, 0.50", 0.05, 60,
  "Should succeed - compensatory effects", "Test many raters help small n",

  # Real-world Application Scenarios
  "pilot_study", "application", "Typical pilot: n=30, binary", "2", 2, 0.40, "0.30, 0.70", 0.05, 30,
  "Should succeed - common pilot design", "Test realistic pilot scenario",

  "qa_program", "application", "QA program: n=100, moderate imbalance", "2", 2, 0.50, "0.25, 0.75", 0.05, 100,
  "Should succeed - typical QA", "Test quality assurance scenario",

  "training_eval", "application", "Training: n=50, 3 raters", "3", 3, 0.50, "0.30, 0.45, 0.25", 0.05, 50,
  "Should succeed - educational setting", "Test training evaluation",

  "clinical_trial", "application", "Clinical trial: n=150, 4 categories", "4", 2, 0.60, "0.25, 0.30, 0.30, 0.15", 0.05, 150,
  "Should succeed - trial endpoint", "Test trial agreement endpoint",

  "registry_study", "application", "Registry: n=250, stringent", "2", 2, 0.70, "0.30, 0.70", 0.01, 250,
  "Should succeed - registry validation", "Test registry validation scenario"
)

# Save Dataset 3 in multiple formats
save(kappa_fixedn_validation_cases, file = here("data", "kappasizefixedn_validation_cases.rda"))
write.csv(kappa_fixedn_validation_cases, file = here("data", "kappasizefixedn_validation_cases.csv"), row.names = FALSE)
write_xlsx(kappa_fixedn_validation_cases, path = here("data", "kappasizefixedn_validation_cases.xlsx"))
write_omv(kappa_fixedn_validation_cases, here("data", "kappasizefixedn_validation_cases.omv"), frcWrt = TRUE)

cat("\n=== Dataset 3 Complete ===\n")
cat("kappa_fixedn_validation_cases: ", nrow(kappa_fixedn_validation_cases), " cases\n")

# ==============================================================================
# Summary Statistics
# ==============================================================================
cat("\n========================================\n")
cat("KAPPASIZEFIXEDN TEST DATA GENERATION COMPLETE\n")
cat("========================================\n\n")

cat("Dataset 1 - Clinical Fixed-N Scenarios:\n")
cat("  Files: kappasizefixedn_scenarios_comprehensive.[rda|csv|xlsx|omv]\n")
cat("  Scenarios: ", nrow(kappa_fixedn_scenarios_comprehensive), "\n")
cat("  Focus: Resource-constrained studies with fixed sample sizes\n\n")

cat("Dataset 2 - Sample Size Impact Cases:\n")
cat("  Files: kappasizefixedn_power_cases.[rda|csv|xlsx|omv]\n")
cat("  Cases: ", nrow(kappa_fixedn_power_cases), "\n")
cat("  Focus: How fixed n affects detectable kappa across parameters\n\n")

cat("Dataset 3 - Validation Cases:\n")
cat("  Files: kappasizefixedn_validation_cases.[rda|csv|xlsx|omv]\n")
cat("  Cases: ", nrow(kappa_fixedn_validation_cases), "\n")
cat("  Focus: Boundary conditions, edge cases, and real-world applications\n\n")

cat("Total test scenarios: ",
    nrow(kappa_fixedn_scenarios_comprehensive) +
    nrow(kappa_fixedn_power_cases) +
    nrow(kappa_fixedn_validation_cases), "\n")

cat("\n========================================\n")
cat("All files saved to data/ directory\n")
cat("========================================\n")
