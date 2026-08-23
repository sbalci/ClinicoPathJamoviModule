# Generate Test Data for kappaSizeFixedN Function
# This script creates comprehensive test datasets for the kappaSizeFixedN jamovi analysis
# Purpose: Fixed sample size - report the expected lower confidence bound for kappa
#          (kappaL is always BELOW the anticipated kappa0; nothing is 'detected' here)
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
  "Small pilot: what lower bound for kappa can we expect with n=30?", 2, 2, 0.40, "0.10, 0.90", 0.05, 30, "pilot_budget",
  "Dermatology resident training evaluation with limited budget for 30 cases",
  "With only 30 melanoma cases available, what lower bound for kappa can we expect?",

  "small_sample_fixed", "emergency_protocol_n40", "Emergency Protocol Feasibility",
  "Emergency department feasibility: n=40 stroke CTs", 2, 2, 0.50, "0.15, 0.85", 0.05, 40, "time_constraint",
  "ED can review 40 stroke CT cases in 2 months, need to know the expected lower bound for kappa",
  "Given time constraints (2 months), what lower bound for kappa can we expect with n=40?",

  "small_sample_fixed", "rare_pathology_n35", "Rare Pathology Limited Cases",
  "Rare tumor: only 35 cases available annually", 3, 2, 0.40, "0.30, 0.40, 0.30", 0.05, 35, "case_availability",
  "Rare tumor type with only 35 cases per year in tertiary center",
  "With 35 rare cases a year, what lower bound for 3-level grading agreement can we expect?",

  "small_sample_fixed", "training_validation_n50", "Training Program Validation",
  "Post-training assessment with n=50 budget", 2, 3, 0.50, "0.25, 0.75", 0.05, 50, "educational_budget",
  "Three pathology trainees, budget allows 50 cases for competency assessment",
  "With 50-case training budget and 3 raters, what lower bound for kappa can we expect?",

  "small_sample_fixed", "telemedicine_pilot_n45", "Telemedicine Pilot Study",
  "Telemedicine diagnostic agreement pilot", 2, 2, 0.50, "0.30, 0.70", 0.05, 45, "pilot_phase",
  "Pilot telemedicine program comparing remote vs in-person diagnosis",
  "Pilot phase limited to 45 consultations: what lower bound for kappa can we expect?",

  # Moderate Sample Fixed-N (n=75-150) - Standard Constraints
  "moderate_sample_fixed", "radiology_qa_n100", "Radiology QA Program",
  "Standard QA sample: annual 100 mammograms", 2, 2, 0.50, "0.20, 0.80", 0.05, 100, "qa_protocol",
  "Annual quality assurance reviewing 100 mammograms per radiologist pair",
  "With standard QA protocol (n=100), what lower bound for BIRADS agreement can we expect?",

  "moderate_sample_fixed", "pathology_accred_n120", "Pathology Accreditation",
  "Accreditation requirement: minimum 120 cases", 4, 2, 0.50, "0.25, 0.30, 0.30, 0.15", 0.05, 120, "regulatory_minimum",
  "Accreditation body requires minimum 120 tumor grades for validation",
  "Meeting minimum accreditation n=120, what lower bound for 4-level grading agreement can we expect?",

  "moderate_sample_fixed", "clinical_trial_n150", "Clinical Trial Agreement",
  "Biomarker agreement in n=150 trial", 2, 2, 0.60, "0.35, 0.65", 0.05, 150, "trial_enrollment",
  "Clinical trial enrolled 150 patients, need central pathology review agreement",
  "Trial n=150 fixed by enrollment, what lower bound for biomarker scoring agreement can we expect?",

  "moderate_sample_fixed", "multicenter_qa_n90", "Multicenter QA Study",
  "Multicenter study: 30 cases x 3 sites", 3, 2, 0.50, "0.30, 0.45, 0.25", 0.05, 90, "site_contribution",
  "Three centers each contributing 30 cases for standardization study",
  "With n=90 from 3 centers, what lower bound for 3-level severity agreement can we expect?",

  "moderate_sample_fixed", "insurance_audit_n80", "Insurance Audit Sample",
  "Audit sample size predetermined: n=80", 2, 2, 0.40, "0.40, 0.60", 0.05, 80, "audit_protocol",
  "Insurance company audits 80 randomly selected diagnosis codes annually",
  "Fixed audit n=80 per protocol, what lower bound for diagnosis agreement can we expect?",

  # Large Sample Fixed-N (n=200-500) - Well-Resourced Studies
  "large_sample_fixed", "registry_validation_n250", "Cancer Registry Validation",
  "Registry validation: 250 cases per year", 5, 2, 0.60, "0.20, 0.25, 0.25, 0.20, 0.10", 0.05, 250, "registry_capacity",
  "Cancer registry has capacity to validate 250 staging cases annually",
  "With registry capacity n=250, what lower bound for 5-stage TNM agreement can we expect?",

  "large_sample_fixed", "screening_program_n300", "Screening Program QA",
  "National screening: 300 case QA sample", 2, 2, 0.70, "0.25, 0.75", 0.01, 300, "national_program",
  "National mammography screening program with 300-case annual QA",
  "Stringent alpha=0.01, n=300 QA sample: what lower bound for screening agreement can we expect?",

  "large_sample_fixed", "ai_validation_n400", "AI Algorithm Validation",
  "AI validation study: 400 images budgeted", 2, 2, 0.60, "0.30, 0.70", 0.05, 400, "validation_protocol",
  "AI diagnostic algorithm validation with 400 expert-labeled images",
  "With 400-image validation set, what lower bound for AI-human agreement can we expect?",

  "large_sample_fixed", "biobank_study_n500", "Biobank Retrospective Study",
  "Biobank cohort: 500 cases with tissue", 4, 2, 0.65, "0.30, 0.30, 0.25, 0.15", 0.05, 500, "biobank_availability",
  "Biobank has 500 cases with adequate tissue for central review",
  "Using all 500 biobank cases, what lower bound for 4-grade agreement can we expect?",

  "large_sample_fixed", "pharma_trial_n200", "Pharmaceutical Trial Endpoint",
  "Phase III trial: 200 patients enrolled", 3, 3, 0.60, "0.35, 0.40, 0.25", 0.05, 200, "trial_completion",
  "Completed phase III trial with 200 patients, 3-rater endpoint assessment",
  "Trial complete with n=200 and 3 raters, what lower bound for endpoint agreement can we expect?",

  # Very Large Sample Fixed-N (n=750-1000+) - Major Studies
  "very_large_fixed", "national_survey_n1000", "National Health Survey",
  "National survey: 1000 participant target", 2, 2, 0.70, "0.50, 0.50", 0.05, 1000, "survey_design",
  "National health survey with predetermined 1000-participant sample",
  "Large national survey n=1000, what lower bound for diagnostic agreement can we expect?",

  "very_large_fixed", "consortium_study_n800", "International Consortium",
  "Consortium study: 100 cases x 8 centers", 3, 2, 0.65, "0.30, 0.45, 0.25", 0.01, 800, "center_contribution",
  "International consortium, each of 8 centers contributes 100 cases",
  "With n=800 consortium cases at alpha=0.01, what lower bound for 3-level agreement can we expect?",

  "very_large_fixed", "ehr_validation_n750", "EHR Data Validation",
  "Electronic health record: 750 chart reviews", 2, 2, 0.60, "0.40, 0.60", 0.05, 750, "chart_review_capacity",
  "EHR validation study with resources for 750 manual chart reviews",
  "Chart review capacity n=750, what lower bound for diagnosis coding agreement can we expect?",

  "very_large_fixed", "longitudinal_cohort_n600", "Longitudinal Cohort Follow-up",
  "Cohort study: 600 patients reached at follow-up", 4, 2, 0.60, "0.25, 0.30, 0.30, 0.15", 0.05, 600, "cohort_retention",
  "Longitudinal cohort retained 600 of 800 original patients for imaging review",
  "With n=600 retained patients, what lower bound for 4-level imaging agreement can we expect?",

  "very_large_fixed", "database_linkage_n900", "Database Linkage Study",
  "Linked databases: 900 matched patients", 2, 2, 0.65, "0.30, 0.70", 0.05, 900, "record_linkage",
  "Successful linkage of 900 patients between two clinical databases",
  "Database linkage yielded n=900 matches, what lower bound for diagnosis agreement can we expect?",

  # Multiple Rater Fixed-N Scenarios
  "multi_rater_fixed", "training_3raters_n60", "Three-Rater Training Study",
  "Training evaluation: 3 raters, 60 cases", 2, 3, 0.50, "0.30, 0.70", 0.05, 60, "training_program",
  "Pathology training program with 3 trainees reviewing 60 cases",
  "With 3 raters and n=60 training cases, what lower bound for kappa can we expect?",

  "multi_rater_fixed", "consensus_4raters_n80", "Four-Rater Consensus Study",
  "Consensus panel: 4 experts, 80 cases", 3, 4, 0.60, "0.30, 0.40, 0.30", 0.05, 80, "expert_panel",
  "Four experts forming consensus panel for guideline development",
  "Expert panel (4 raters) with n=80, what lower bound for 3-level consensus can we expect?",

  "multi_rater_fixed", "multicenter_5raters_n100", "Five-Center Comparison",
  "Five centers: 1 rater each, 100 cases", 2, 5, 0.55, "0.25, 0.75", 0.05, 100, "center_comparison",
  "Comparing diagnostic practices across 5 centers (1 rater per center)",
  "Five-center study with n=100 shared cases, what lower bound for kappa can we expect?",

  # Stringent Alpha Fixed-N Scenarios
  "stringent_alpha_fixed", "regulatory_n200_alpha01", "Regulatory Submission",
  "Regulatory requirement: n=200, alpha=0.01", 2, 2, 0.70, "0.30, 0.70", 0.01, 200, "regulatory_standard",
  "FDA submission requires 200 cases with 99% confidence for diagnostic agreement",
  "Meeting regulatory n=200 at alpha=0.01, what lower bound for kappa can we expect?"
)

# Save Dataset 1 in multiple formats
kappasizefixedn_scenarios_comprehensive <- kappa_fixedn_scenarios_comprehensive  # object name must match the file name
save(kappasizefixedn_scenarios_comprehensive, file = here("data", "kappasizefixedn_scenarios_comprehensive.rda"))
write.csv(kappa_fixedn_scenarios_comprehensive, file = here("data", "kappasizefixedn_scenarios_comprehensive.csv"), row.names = FALSE)
write_xlsx(kappa_fixedn_scenarios_comprehensive, path = here("data", "kappasizefixedn_scenarios_comprehensive.xlsx"))
write_omv(kappa_fixedn_scenarios_comprehensive, here("data", "kappasizefixedn_scenarios_comprehensive.omv"), frcWrt = TRUE)

cat("\n=== Dataset 1 Complete ===\n")
cat("kappa_fixedn_scenarios_comprehensive: ", nrow(kappa_fixedn_scenarios_comprehensive), " scenarios\n")

# ==============================================================================
# Dataset 2: Sample Size Impact Cases
# ==============================================================================
# Demonstrates how different fixed sample sizes affect the expected lower bound for kappa

kappa_fixedn_power_cases <- tribble(
  ~case_name, ~relationship_type, ~description, ~outcome, ~raters, ~kappa0, ~proportions, ~alpha, ~n, ~expected_pattern, ~teaching_point,

  # Sample Size Progression (holding other parameters constant)
  "n_20_minimum", "sample_size", "Very small pilot: n=20", "2", 2, 0.50, "0.50, 0.50", 0.05, 20,
  "Lower bound 0.135; 0.365 below kappa0", "n=20 leaves a 0.365 gap: the study can defend almost nothing",

  "n_30_small_pilot", "sample_size", "Small pilot study: n=30", "2", 2, 0.50, "0.50, 0.50", 0.05, 30,
  "Lower bound 0.206; 0.294 below kappa0", "n=30 still leaves a 0.294 gap below the anticipated kappa",

  "n_50_feasibility", "sample_size", "Feasibility study: n=50", "2", 2, 0.50, "0.50, 0.50", 0.05, 50,
  "Lower bound 0.276; 0.224 below kappa0", "n=50 halves the pilot gap to 0.224; short of a moderate-agreement claim",

  "n_75_small_standard", "sample_size", "Small standard study: n=75", "2", 2, 0.50, "0.50, 0.50", 0.05, 75,
  "Lower bound 0.320; 0.180 below kappa0", "n=75 brings the gap to 0.180, approaching usable precision",

  "n_100_standard", "sample_size", "Standard study: n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.345; 0.155 below kappa0", "n=100 gives a 0.155 gap - the usual working precision",

  "n_150_moderate", "sample_size", "Moderate-large study: n=150", "2", 2, 0.50, "0.50, 0.50", 0.05, 150,
  "Lower bound 0.375; 0.125 below kappa0", "n=150 narrows the gap to 0.125; returns are already diminishing",

  "n_200_large", "sample_size", "Large study: n=200", "2", 2, 0.50, "0.50, 0.50", 0.05, 200,
  "Lower bound 0.393; 0.107 below kappa0", "n=200 narrows the gap to 0.107",

  "n_300_very_large", "sample_size", "Very large study: n=300", "2", 2, 0.50, "0.50, 0.50", 0.05, 300,
  "Lower bound 0.413; 0.087 below kappa0", "n=300 reaches a 0.087 gap",

  "n_500_major", "sample_size", "Major study: n=500", "2", 2, 0.50, "0.50, 0.50", 0.05, 500,
  "Lower bound 0.433; 0.067 below kappa0", "n=500 reaches 0.067; doubling n again buys little",

  # Kappa0 Value Impact (holding n constant)
  "kappa0_20_n100", "kappa0_impact", "Low anticipated agreement kappa0=0.20, n=100", "2", 2, 0.20, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.035; 0.165 below kappa0", "A low kappa0 leaves the bound near chance agreement",

  "kappa0_40_n100", "kappa0_impact", "Moderate anticipated agreement kappa0=0.40, n=100", "2", 2, 0.40, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.240; 0.160 below kappa0", "A mid-range kappa0 leaves a 0.160 gap",

  "kappa0_60_n100", "kappa0_impact", "Good anticipated agreement kappa0=0.60, n=100", "2", 2, 0.60, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.453; 0.147 below kappa0", "A higher kappa0 narrows the gap - agreement is easier to bound away from chance",

  "kappa0_80_n100", "kappa0_impact", "High anticipated agreement kappa0=0.80, n=100", "2", 2, 0.80, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.679; 0.121 below kappa0", "A very high kappa0 gives the narrowest gap of this set (0.121)",

  # Alpha Level Impact (holding n=100 constant)
  "alpha_10_n100", "alpha_impact", "Liberal alpha=0.10 (90% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.10, 100,
  "Lower bound 0.381; 0.119 below kappa0", "A liberal alpha raises the bound, at the cost of confidence",

  "alpha_05_n100", "alpha_impact", "Standard alpha=0.05 (95% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.345; 0.155 below kappa0", "alpha=0.05 is the conventional balance of precision and confidence",

  "alpha_01_n100", "alpha_impact", "Stringent alpha=0.01 (99% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.01, 100,
  "Lower bound 0.276; 0.224 below kappa0", "A stringent alpha lowers the bound: rigour costs precision",

  "alpha_001_n100", "alpha_impact", "Very stringent alpha=0.001 (99.9% CI), n=100", "2", 2, 0.50, "0.50, 0.50", 0.001, 100,
  "Lower bound 0.197; 0.303 below kappa0", "alpha=0.001 costs 0.148 of bound relative to alpha=0.05",

  # Rater Number Impact (holding n=100 constant)
  "raters_2_n100", "rater_impact", "Two raters, n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.345; 0.155 below kappa0", "Two raters is the reference design",

  "raters_3_n100", "rater_impact", "Three raters, n=100", "2", 3, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.390; 0.110 below kappa0", "A third rater raises the bound by 0.045 - more information per subject",

  "raters_4_n100", "rater_impact", "Four raters, n=100", "2", 4, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.406; 0.094 below kappa0", "A fourth rater adds 0.016; the gain per rater falls off quickly",

  "raters_5_n100", "rater_impact", "Five raters, n=100", "2", 5, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.412; 0.088 below kappa0", "A fifth rater adds 0.006 - effectively the ceiling",

  # Category Number Impact (holding n=100 constant)
  "cat_2_n100", "category_impact", "Binary (2 categories), n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.345; 0.155 below kappa0", "Binary gives the LOWEST bound here; balanced categories are not a handicap",

  "cat_3_n100", "category_impact", "Three categories, n=100", "3", 2, 0.50, "0.33, 0.34, 0.33", 0.05, 100,
  "Lower bound 0.378; 0.122 below kappa0", "Three balanced categories beat binary at the same n (0.378 vs 0.345)",

  "cat_4_n100", "category_impact", "Four categories, n=100", "4", 2, 0.50, "0.25, 0.25, 0.25, 0.25", 0.05, 100,
  "Lower bound 0.390; 0.110 below kappa0", "Four balanced categories give a tighter bound still (0.390)",

  "cat_5_n100", "category_impact", "Five categories, n=100", "5", 2, 0.50, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05, 100,
  "Lower bound 0.397; 0.103 below kappa0", "Five balanced categories give the tightest bound of this set (0.397)",

  # Proportion Balance Impact (holding n=100 constant)
  "balanced_n100", "proportion_impact", "Perfectly balanced 50-50, n=100", "2", 2, 0.50, "0.50, 0.50", 0.05, 100,
  "Lower bound 0.345; 0.155 below kappa0", "A 50/50 split is the best case for a binary outcome",

  "moderate_imbal_n100", "proportion_impact", "Moderate imbalance 30-70, n=100", "2", 2, 0.50, "0.30, 0.70", 0.05, 100,
  "Lower bound 0.334; 0.166 below kappa0", "A 30/70 split costs only 0.011 of bound",

  "severe_imbal_n100", "proportion_impact", "Severe imbalance 10-90, n=100", "2", 2, 0.50, "0.10, 0.90", 0.05, 100,
  "Lower bound 0.265; 0.235 below kappa0", "A 10% prevalence drops the bound from 0.345 to 0.265",

  "extreme_imbal_n100", "proportion_impact", "Extreme imbalance 5-95, n=100", "2", 2, 0.50, "0.05, 0.95", 0.05, 100,
  "Lower bound 0.203; 0.297 below kappa0", "A 5% prevalence drops the bound to 0.203 and triggers the sparse-cell notice"
)

# Save Dataset 2 in multiple formats
kappasizefixedn_power_cases <- kappa_fixedn_power_cases  # object name must match the file name
save(kappasizefixedn_power_cases, file = here("data", "kappasizefixedn_power_cases.rda"))
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
  "Should succeed; wide gap below kappa0", "Test severe imbalance",

  "rare_event_n100", "proportion_test", "Rare event 5%, n=100", "2", 2, 0.50, "0.05, 0.95", 0.05, 100,
  "Should succeed with very limited range", "Test rare event impact",

  # Combined Challenging Scenarios
  "small_n_rare", "combined", "Small n=30 + rare event 10%", "2", 2, 0.50, "0.10, 0.90", 0.05, 30,
  "Should succeed but severely limited", "Test small sample + rare event",

  "small_n_5cat", "combined", "Small n=50 + five categories", "5", 2, 0.50, "0.20, 0.20, 0.20, 0.20, 0.20", 0.05, 50,
  "Should succeed with very limited range", "Test small n + many categories",

  "stringent_small", "combined", "Stringent alpha=0.01 + small n=40", "2", 2, 0.50, "0.50, 0.50", 0.01, 40,
  "Should succeed; stringent alpha widens the gap", "Test stringent alpha + small n",

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
kappasizefixedn_validation_cases <- kappa_fixedn_validation_cases  # object name must match the file name
save(kappasizefixedn_validation_cases, file = here("data", "kappasizefixedn_validation_cases.rda"))
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
cat("  Focus: How fixed n affects the expected lower bound for kappa\n\n")

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
