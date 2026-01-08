# ═══════════════════════════════════════════════════════════════════════════
# Kappa Power Analysis - Comprehensive Examples
# ═══════════════════════════════════════════════════════════════════════════
#
# This file contains detailed examples for using the kappaSizePower function
# to calculate required sample sizes for interobserver agreement studies
# using Cohen's kappa statistic.
#
# Each example includes:
#   - Clinical context and research question
#   - Complete function call with parameters
#   - Interpretation of results
#   - Sample size planning implications
#
# Function: kappaSizePower
# Purpose: Calculate sample size needed to detect difference between two kappa values
# Parameters:
#   - outcome: Number of outcome categories (2, 3, 4, 5)
#   - kappa0: Null hypothesis kappa (baseline agreement, 0.01-0.99)
#   - kappa1: Alternative hypothesis kappa (target agreement, 0.01-0.99)
#   - props: Category proportions (comma-separated string)
#   - raters: Number of raters (2, 3, 4, 5)
#   - alpha: Significance level (typ. 0.05 or 0.01)
#   - power: Desired statistical power (typ. 0.80, 0.85, 0.90)
#
# ═══════════════════════════════════════════════════════════════════════════

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 1: Emergency Department Pneumonia Diagnosis
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Two emergency physicians want to establish a reliable protocol for detecting
# pneumonia on chest X-rays. Currently their agreement is fair (κ=0.50).
# After implementing standardized criteria, they expect to achieve good
# agreement (κ=0.75).
#
# Research Question:
# How many chest X-rays do we need to review to demonstrate this improvement
# with 80% power at α=0.05?

kappaSizePower(
  outcome = "2",              # Binary: Pneumonia Present/Absent
  kappa0 = 0.40,              # Current fair agreement
  kappa1 = 0.70,              # Target good agreement
  props = "0.25, 0.75",       # 25% pneumonia prevalence in ED
  raters = "2",               # Two emergency physicians
  alpha = 0.05,               # Standard significance level
  power = 0.80                # Standard power
)

# Clinical Interpretation:
# Sample Size Calculation Results:
#   - Number of subjects needed: ~[calculated value]
#   - Effect size: 0.25 (moderate-large improvement)
#   - This represents detecting improvement from "fair" to "good" agreement
#
# Implementation Planning:
#   - Collect consecutive eligible chest X-rays
#   - Both physicians rate independently (blinded)
#   - Document disagreements for discussion
#   - Re-training if agreement doesn't improve as expected
#
# Key Consideration:
#   - Prevalence (25%) affects sample composition
#   - Need sufficient cases in both categories
#   - Consider stratified sampling if prevalence varies


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 2: Mammography Screening Quality Improvement
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Two radiologists participate in mammography quality improvement training.
# Pre-training agreement was substantial (κ=0.60). Post-training, they expect
# near-perfect agreement (κ=0.80) on recommendations for biopsy vs callback.
#
# Research Question:
# Sample size needed to validate training effectiveness?

kappaSizePower(
  outcome = "2",
  kappa0 = 0.60,              # Pre-training substantial agreement
  kappa1 = 0.80,              # Post-training expected improvement
  props = "0.12, 0.88",       # 12% biopsy recommendation rate
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Clinical Interpretation:
# Validation Study Design:
#   - Required sample: ~[calculated value] mammograms
#   - Effect size: 0.20 (moderate improvement)
#   - Low biopsy rate (12%) means larger sample needed
#
# Quality Improvement Implications:
#   - Document pre-training baseline
#   - Standardize training curriculum
#   - Post-training assessment after stabilization period
#   - Consider interim assessments at 50%, 75% enrollment
#
# Accreditation Context:
#   - MQSA compliance requirements
#   - Meets requirements for quality assurance validation
#   - Can demonstrate effectiveness of intervention


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 3: Dermatology Training Validation (Multiple Raters)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Dermatology residency program wants to validate trainee competency
# in biopsy decision-making. Trainees currently achieve moderate agreement
# (κ=0.45) with expert consensus. After dedicated training module, expect
# to reach substantial agreement (κ=0.70).
#
# Research Question:
# With three independent expert raters, how many cases needed to demonstrate
# trainee improvement to expert-level performance?

kappaSizePower(
  outcome = "2",
  kappa0 = 0.45,              # Trainee baseline
  kappa1 = 0.70,              # Target: Expert-level agreement
  props = "0.18, 0.82",       # 18% require biopsy
  raters = "3",               # Three expert dermatologists
  alpha = 0.05,
  power = 0.80
)

# Clinical Interpretation:
# Training Validation Study:
#   - Sample size accounts for three-rater design
#   - Multiple raters improve reliability of assessment
#   - Effect size: 0.25 (substantial improvement)
#
# Educational Assessment:
#   - Baseline assessment before training
#   - Standardized case set (with photographic documentation)
#   - Post-training assessment after clinical exposure
#   - Can serve as competency milestone
#
# Program Evaluation:
#   - Demonstrates curriculum effectiveness
#   - Supports accreditation documentation
#   - Can compare across training years


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 4: Heart Failure Staging (Three Categories)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Two cardiologists implementing new echocardiographic criteria for
# classifying heart failure severity: Mild / Moderate / Severe.
# Current agreement is moderate (κ=0.55), aiming for substantial (κ=0.75).
#
# Research Question:
# Sample size for validating new staging protocol?

kappaSizePower(
  outcome = "3",              # Three severity categories
  kappa0 = 0.55,              # Current moderate agreement
  kappa1 = 0.75,              # Target substantial agreement
  props = "0.20, 0.50, 0.30", # Distribution: Mild, Moderate, Severe
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Clinical Interpretation:
# Protocol Validation:
#   - Three-category outcome more complex than binary
#   - Larger sample needed than binary outcome
#   - Distribution reflects typical clinical population
#
# Implementation Strategy:
#   - Consecutive eligible echocardiograms
#   - Independent blinded reviews
#   - Standardized measurement protocols
#   - Consensus conference for discrepant cases
#
# Clinical Utility:
#   - Improved staging guides treatment intensity
#   - Better prognostic stratification
#   - Enhanced clinical trial eligibility determination


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 5: Tumor Grading Standardization (Four Categories)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Three pathologists implementing standardized grading criteria for tumor
# specimens: Grade I / II / III / IV. Current agreement is substantial (κ=0.60),
# target is near-perfect (κ=0.80) after implementing WHO guidelines.
#
# Research Question:
# How many tumor specimens needed to validate standardization?

kappaSizePower(
  outcome = "4",                      # Four tumor grades
  kappa0 = 0.60,                      # Current substantial agreement
  kappa1 = 0.80,                      # Target near-perfect
  props = "0.15, 0.25, 0.35, 0.25",   # Grade distribution
  raters = "3",                       # Three pathologists
  alpha = 0.05,
  power = 0.80
)

# Clinical Interpretation:
# Standardization Study:
#   - Four categories increase complexity
#   - Three raters enhance generalizability
#   - Effect size: 0.20 (moderate but important)
#
# Pathology Quality Assurance:
#   - Supports CAP/CLIA quality standards
#   - Demonstrates effectiveness of standardization
#   - Can be used for inter-laboratory comparison
#
# Patient Care Impact:
#   - Grading determines treatment approach
#   - Prognostic information for patients
#   - Clinical trial eligibility
#   - Second opinion reliability


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 6: Anxiety Severity Assessment (Five Categories)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Two psychologists validating structured anxiety severity tool with
# five levels: None / Mild / Moderate / Severe / Very Severe.
# Pilot data shows moderate agreement (κ=0.45), targeting substantial (κ=0.65).
#
# Research Question:
# Sample size for psychometric validation study?

kappaSizePower(
  outcome = "5",                              # Five-point severity scale
  kappa0 = 0.45,                              # Pilot agreement
  kappa1 = 0.65,                              # Target agreement
  props = "0.10, 0.20, 0.35, 0.25, 0.10",     # Expected distribution
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Clinical Interpretation:
# Psychometric Validation:
#   - Five categories capture nuanced severity
#   - Distribution peaks at "Moderate" (most common)
#   - Effect size: 0.20 (clinically meaningful)
#
# Assessment Tool Development:
#   - Structured interview guide
#   - Anchor examples for each severity level
#   - Training standardization for raters
#   - Inter-rater reliability as validity evidence
#
# Clinical Application:
#   - Treatment intensity matching
#   - Progress monitoring
#   - Outcomes assessment
#   - Research eligibility screening


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 7: High-Power Requirement Study
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Quality improvement project with strict requirements for demonstrating
# improvement. Want 90% power to detect improvement in burn severity grading
# from moderate (κ=0.60) to near-perfect (κ=0.85) agreement.
#
# Research Question:
# Sample size for high-confidence validation?

kappaSizePower(
  outcome = "3",
  kappa0 = 0.60,
  kappa1 = 0.85,
  props = "0.40, 0.35, 0.25",    # Severity distribution
  raters = "3",
  alpha = 0.05,
  power = 0.90                   # High power requirement
)

# Clinical Interpretation:
# Rigorous Quality Improvement:
#   - 90% power reduces false negative risk
#   - Large effect size (0.25) is ambitious but achievable
#   - Higher power = larger sample size
#
# When to Use High Power:
#   - Regulatory submissions
#   - High-stakes clinical decisions
#   - Expensive interventions to validate
#   - Publication in high-impact journals
#
# Trade-offs:
#   - Increased cost and time
#   - May be difficult to achieve large sample
#   - Consider pilot study with conventional power first


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 8: Strict Significance Level (Alpha = 0.01)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Regulatory submission for diagnostic device requiring stringent statistical
# criteria. ICU discharge protocol validation with strict significance level.
# Three physicians, binary outcome, expecting substantial improvement.
#
# Research Question:
# Sample size with α=0.01 for regulatory requirements?

kappaSizePower(
  outcome = "2",
  kappa0 = 0.55,
  kappa1 = 0.80,
  props = "0.60, 0.40",          # 60% ready for discharge
  raters = "3",
  alpha = 0.01,                  # Strict significance level
  power = 0.85                   # Higher than standard
)

# Clinical Interpretation:
# Regulatory Study Design:
#   - α=0.01 reduces false positive risk
#   - Combined with 85% power for robust evidence
#   - Larger sample needed than standard α=0.05
#
# Regulatory Context:
#   - FDA/EMA submissions often require α≤0.01
#   - Multiple comparisons may require Bonferroni correction
#   - Pre-specified in protocol (no post-hoc changes)
#
# Implementation:
#   - Prospective patient enrollment
#   - Strict protocol adherence
#   - Independent Data Monitoring Committee
#   - Pre-registration (e.g., ClinicalTrials.gov)


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 9: Pathology Diagnostic Standardization (Maximum Rigor)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Four pathologists implementing standardized diagnostic criteria for
# complex tumor type. This will guide treatment decisions. Want very high
# confidence (α=0.01, power=0.90) in demonstrating improvement from
# substantial (κ=0.70) to near-perfect (κ=0.90) agreement.
#
# Research Question:
# Sample size for maximally rigorous validation?

kappaSizePower(
  outcome = "2",
  kappa0 = 0.70,                 # Already substantial baseline
  kappa1 = 0.90,                 # Target near-perfect
  props = "0.35, 0.65",
  raters = "4",                  # Four pathologists
  alpha = 0.01,                  # Strict testing
  power = 0.90                   # High power
)

# Clinical Interpretation:
# Maximum Rigor Study:
#   - Largest sample size (strict α + high power + 4 raters)
#   - Appropriate for high-stakes clinical decisions
#   - Effect size: 0.20 (from good to excellent)
#
# When Justified:
#   - Diagnostic decisions that determine chemotherapy
#   - Prognostic classifications affecting survival estimates
#   - Inter-institutional standardization
#   - Legal/medicolegal considerations
#
# Practical Considerations:
#   - May require multi-center collaboration
#   - Extended timeline for case accrual
#   - Quality control throughout study period
#   - Consider interim analyses


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 10: Effect Size Comparison - Small vs Large Effects
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Planning inter-rater reliability study. Compare sample size requirements
# for detecting small effect (κ: 0.50→0.55) vs large effect (κ: 0.30→0.70).
#
# Small Effect (Minimal Clinically Important Difference):

kappaSizePower(
  outcome = "2",
  kappa0 = 0.50,
  kappa1 = 0.55,                 # Small effect: 0.05
  props = "0.50, 0.50",
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Large Effect (Dramatic Improvement):

kappaSizePower(
  outcome = "2",
  kappa0 = 0.30,
  kappa1 = 0.70,                 # Large effect: 0.40
  props = "0.20, 0.80",
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Sample Size Comparison:
# Small effect: ~[very large N] subjects required
# Large effect: ~[modest N] subjects required
#
# Planning Implications:
#   - Small effects may not be feasible to detect
#   - Large improvements easier to demonstrate
#   - Balance clinical importance vs detectability
#   - Consider whether small effect is worth detecting
#
# Design Decisions:
#   - For exploratory studies: powered for large effects
#   - For definitive validation: may need large sample for small effects
#   - Pilot studies can estimate realistic effect sizes


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 11: Unequal Category Proportions Impact
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Three-category disease severity with highly unequal distribution.
# Compare balanced vs imbalanced proportions impact on sample size.
#
# Balanced Proportions:

kappaSizePower(
  outcome = "3",
  kappa0 = 0.55,
  kappa1 = 0.75,
  props = "0.33, 0.33, 0.34",    # Perfectly balanced
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Imbalanced Proportions (Rare Category):

kappaSizePower(
  outcome = "3",
  kappa0 = 0.55,
  kappa1 = 0.75,
  props = "0.10, 0.20, 0.70",    # Highly imbalanced
  raters = "2",
  alpha = 0.05,
  power = 0.80
)

# Proportion Effect:
#   - Imbalanced categories increase required sample size
#   - Rare categories need sufficient representation
#   - May need stratified sampling strategy
#
# Design Strategies:
#   - Oversample rare categories if feasible
#   - Consider collapsing categories if clinically appropriate
#   - Ensure minimum cell sizes in all categories
#   - Document selection strategy in protocol


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 12: Pilot Study Design
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Planning pilot study to estimate effect size before definitive study.
# Want preliminary agreement data with relaxed criteria (α=0.10, power=0.70)
# to inform larger study planning.
#
# Research Question:
# Feasible sample size for pilot study?

kappaSizePower(
  outcome = "2",
  kappa0 = 0.40,
  kappa1 = 0.60,                 # Realistic improvement
  props = "0.25, 0.75",
  raters = "2",
  alpha = 0.10,                  # Relaxed significance
  power = 0.70                   # Lower power acceptable
)

# Pilot Study Rationale:
#   - Smaller sample than definitive study
#   - Estimates effect size for power analysis
#   - Tests feasibility of recruitment
#   - Refines measurement protocols
#
# Use Pilot Results To:
#   1. Estimate realistic kappa0 and kappa1
#   2. Determine actual category proportions
#   3. Assess rater training needs
#   4. Calculate definitive study sample size
#   5. Identify protocol problems early
#
# Important:
#   - Pilot is NOT powered for hypothesis testing
#   - Don't use pilot p-values for inference
#   - Focus on estimates and confidence intervals
#   - Pilot + definitive sample ≠ combined analysis


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 13: Quality Improvement Project
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Hospital quality improvement for surgical complication grading using
# Clavien-Dindo classification (4 categories). Three surgeons implementing
# standardized criteria. Baseline κ=0.65, target κ=0.85.
#
# Research Question:
# Sample size for QI validation with standard power (80%)?

kappaSizePower(
  outcome = "4",
  kappa0 = 0.65,
  kappa1 = 0.85,
  props = "0.40, 0.30, 0.20, 0.10",  # Most complications are minor
  raters = "3",
  alpha = 0.05,
  power = 0.80
)

# Quality Improvement Context:
#   - Demonstrates effectiveness of standardization
#   - Supports Morbidity & Mortality reporting
#   - Enables quality benchmarking
#   - Facilitates surgical outcomes research
#
# Implementation:
#   - Consecutive surgical cases
#   - Independent complication assessment
#   - Regular consensus conferences
#   - PDSA cycles based on disagreements
#
# Reporting:
#   - Pre/post intervention comparison
#   - Control charts for ongoing monitoring
#   - Kappa as quality metric
#   - Proportion agreement by category


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 14: Multi-Center Study Planning
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Three-center study for biomarker scoring standardization.
# Two pathologists per center (6 total), but planning sample size
# for two-rater comparison (conservative). Five-point expression scale.
#
# Research Question:
# Sample size for demonstrating improved inter-center reliability?

kappaSizePower(
  outcome = "5",
  kappa0 = 0.60,                              # Inter-center baseline
  kappa1 = 0.80,                              # Target standardization
  props = "0.05, 0.15, 0.35, 0.35, 0.10",     # Expression distribution
  raters = "2",                               # Conservative: 2 raters
  alpha = 0.01,                               # Strict for multi-center
  power = 0.85
)

# Multi-Center Considerations:
#   - Sample divided across centers
#   - Need sufficient sample per center
#   - Account for between-center variability
#   - May need to increase calculated sample by 20-30%
#
# Practical Distribution:
#   - If N=180 total, allocate 60 per center
#   - Each center's pathologists review same 60
#   - Central review of discrepant cases
#
# Additional Analyses:
#   - Within-center agreement
#   - Between-center agreement
#   - Overall agreement (primary outcome)
#   - Subgroup analyses by biomarker intensity


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 15: Using Test Data for Parameter Exploration
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Use generated test datasets to explore parameter relationships
# and plan your own study.

# Load comprehensive scenarios
data(kappasizepower_scenarios_comprehensive)

# View all available clinical scenarios
View(kappasizepower_scenarios_comprehensive)

# Example: Use mammography screening scenario
mammography <- kappasizepower_scenarios_comprehensive[
  kappasizepower_scenarios_comprehensive$scenario_id == "mammography_screening_improvement", ]

kappaSizePower(
  outcome = as.character(mammography$outcome_categories),
  kappa0 = mammography$kappa0,
  kappa1 = mammography$kappa1,
  props = mammography$proportions,
  raters = as.character(mammography$raters),
  alpha = mammography$alpha,
  power = mammography$power
)

# Load relationship cases for parameter exploration
data(kappasizepower_relationship_cases)

# Example: Compare effect sizes
effect_size_cases <- kappasizepower_relationship_cases[
  kappasizepower_relationship_cases$relationship_type == "effect_size", ]

# Run each case
for(i in 1:nrow(effect_size_cases)) {
  cat("\n", effect_size_cases$case_name[i], "\n")
  cat(effect_size_cases$teaching_point[i], "\n\n")

  kappaSizePower(
    outcome = effect_size_cases$outcome[i],
    kappa0 = effect_size_cases$kappa0[i],
    kappa1 = effect_size_cases$kappa1[i],
    props = effect_size_cases$proportions[i],
    raters = as.character(effect_size_cases$raters[i]),
    alpha = effect_size_cases$alpha[i],
    power = effect_size_cases$power[i]
  )
}

# Load validation cases for edge case testing
data(kappasizepower_validation_cases)

# Example: Test boundary conditions
boundary_cases <- kappasizepower_validation_cases[
  kappasizepower_validation_cases$test_type == "boundary", ]

# ═══════════════════════════════════════════════════════════════════════════
# KEY PRINCIPLES AND BEST PRACTICES
# ═══════════════════════════════════════════════════════════════════════════

# 1. Effect Size Considerations:
#    - Small effects (Δκ < 0.10): Very large samples needed, may not be feasible
#    - Medium effects (Δκ = 0.15-0.25): Moderate samples, most common scenario
#    - Large effects (Δκ > 0.30): Smaller samples, dramatic improvements
#
# 2. Power Selection:
#    - 80% is conventional standard
#    - 85-90% for important clinical decisions
#    - 95% for regulatory submissions
#    - Higher power = larger sample, diminishing returns beyond 90%
#
# 3. Alpha Selection:
#    - 0.05 is conventional
#    - 0.01 for regulatory/high-stakes decisions
#    - Consider Bonferroni correction for multiple comparisons
#    - Pre-specify alpha, never adjust post-hoc
#
# 4. Rater Selection:
#    - 2 raters: Minimum, most efficient
#    - 3+ raters: Better generalizability, larger sample
#    - Match rater number to clinical application
#    - Diminishing returns beyond 4-5 raters
#
# 5. Category Considerations:
#    - Fewer categories = smaller sample needed
#    - Balanced proportions = most efficient
#    - Rare categories need special planning
#    - Consider collapsing categories if appropriate
#
# 6. Study Design Tips:
#    - Always conduct pilot study when possible
#    - Blinded independent assessments essential
#    - Standardize rater training
#    - Document disagreements for analysis
#    - Plan consensus process
#
# 7. Reporting Standards:
#    - Report all kappa parameters used
#    - Include confidence intervals
#    - Document category prevalence
#    - Describe rater training
#    - Follow GRRAS guidelines (Guidelines for Reporting Reliability and Agreement Studies)

# ═══════════════════════════════════════════════════════════════════════════
# REFERENCES
# ═══════════════════════════════════════════════════════════════════════════

# Key Publications:
# 1. Cohen J. (1960). A coefficient of agreement for nominal scales. Educ Psychol Meas.
# 2. Fleiss JL, et al. (2003). Statistical methods for rates and proportions (3rd ed).
# 3. Donner A, Eliasziw M. (1992). A goodness-of-fit approach to inference procedures for
#    the kappa statistic. Stat Med.
# 4. Landis JR, Koch GG. (1977). The measurement of observer agreement for categorical data.
# 5. Rotondi MA, Donner A. (2012). Sample size estimation in studies of interobserver
#    agreement. J Clin Epidemiol.
# 6. Kottner J, et al. (2011). Guidelines for Reporting Reliability and Agreement Studies
#    (GRRAS). J Clin Epidemiol.

# Online Calculators and Software:
# - kappaSize R package (basis for this function)
# - Sample Size & Power Calculator (various online tools)
# - G*Power (general power analysis software)

# Interpretation Guidelines:
# Landis & Koch (1977) kappa interpretation:
#   < 0.00: Poor agreement
#   0.00-0.20: Slight agreement
#   0.21-0.40: Fair agreement
#   0.41-0.60: Moderate agreement
#   0.61-0.80: Substantial agreement
#   0.81-1.00: Almost perfect agreement

# Note: These are guidelines, not absolute thresholds.
# Clinical context determines what level of agreement is acceptable.

# ═══════════════════════════════════════════════════════════════════════════
# END OF EXAMPLES
# ═══════════════════════════════════════════════════════════════════════════
