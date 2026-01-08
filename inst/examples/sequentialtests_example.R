# ═══════════════════════════════════════════════════════════════════════════
# Sequential Testing Analysis - Comprehensive Examples
# ═══════════════════════════════════════════════════════════════════════════
#
# This file contains detailed examples for using the sequentialtests function
# to analyze and compare different sequential testing strategies:
#   - Serial Positive (Confirmation): Test 2 only if Test 1 is positive
#   - Serial Negative (Exclusion): Test 2 only if Test 1 is negative
#   - Parallel Testing: Both tests for everyone
#
# Each example includes:
#   - Clinical context and research question
#   - Complete function call with parameters
#   - Interpretation of results
#   - Clinical implications
#
# ═══════════════════════════════════════════════════════════════════════════

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 1: COVID-19 Screening with PCR Confirmation (Serial Positive)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Community screening program for COVID-19 using rapid antigen tests
# followed by RT-PCR confirmation for positive results.
#
# Research Question:
# How does serial positive testing strategy perform for COVID-19 diagnosis?
# What is the cost-effectiveness compared to testing everyone with PCR?

sequentialtests(
  test1_name = "Rapid Antigen Test",
  test1_sens = 0.85,
  test1_spec = 0.95,
  test1_cost = 5,
  test2_name = "RT-PCR",
  test2_sens = 0.95,
  test2_spec = 0.99,
  test2_cost = 100,
  strategy = "serial_positive",
  prevalence = 0.05,
  population_size = 10000,
  show_explanation = TRUE,
  show_cost_analysis = TRUE,
  show_nomogram = TRUE
)

# Clinical Interpretation:
# - Serial positive strategy: Test positives with PCR to confirm
# - High specificity (99.5%) with good sensitivity (80.8%)
# - Cost savings: ~95% reduction vs testing all with PCR
# - False positives minimized by confirmation step
# - Some false negatives from rapid test sensitivity (15%)
#
# When to use:
# - Screening scenarios with low-moderate prevalence
# - When confirmatory test is expensive/limited
# - When false positives have serious consequences


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 2: Using Clinical Presets (COVID-19)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Quick analysis using evidence-based preset parameters
#
# Available presets:
#   - covid_screening_confirmation
#   - breast_cancer_screening
#   - mi_emergency_parallel
#   - tb_screening_confirmation
#   - prostate_screening_exclusion
#   - hiv_screening_confirmation
#   - stroke_emergency_parallel

sequentialtests(
  preset = "covid_screening_confirmation",
  population_size = 5000,
  show_cost_analysis = TRUE
)

# Clinical Interpretation:
# Presets load evidence-based test characteristics from literature
# Automatically selects optimal strategy for the clinical scenario
# Useful for teaching, planning, or quick feasibility assessments


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 3: Breast Cancer Screening (Serial Positive)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Population screening program for breast cancer using mammography
# followed by tissue biopsy for positive mammograms.
#
# Research Question:
# Optimize screening protocol balancing sensitivity, specificity, and cost
# in a population with low disease prevalence (0.6%).

sequentialtests(
  test1_name = "Mammography",
  test1_sens = 0.87,
  test1_spec = 0.88,
  test1_cost = 100,
  test2_name = "Tissue Biopsy",
  test2_sens = 0.95,
  test2_spec = 0.98,
  test2_cost = 500,
  strategy = "serial_positive",
  prevalence = 0.006,
  population_size = 100000,
  show_cost_analysis = TRUE,
  show_formulas = TRUE
)

# Clinical Interpretation:
# - Very low prevalence (0.6%) → PPV of mammography alone is poor
# - Serial positive strategy dramatically improves specificity
# - Biopsy confirmation reduces false positives
# - Cost-effective: Only ~13% of population needs biopsy
# - Final sensitivity: 82.7%, specificity: 99.8%
#
# Key insight:
# Even with "imperfect" tests, sequential strategy achieves
# excellent combined performance characteristics


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 4: Myocardial Infarction Rule-out (Parallel Testing)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Emergency department chest pain evaluation
# Need to rule out MI with maximum sensitivity (can't miss cases)
#
# Research Question:
# Compare parallel testing (Troponin + ECG both done) vs sequential approach
# in emergency setting where missing MI has severe consequences.

sequentialtests(
  test1_name = "High-Sensitivity Troponin",
  test1_sens = 0.85,
  test1_spec = 0.80,
  test1_cost = 50,
  test2_name = "ECG",
  test2_sens = 0.75,
  test2_spec = 0.85,
  test2_cost = 30,
  strategy = "parallel",
  prevalence = 0.20,
  population_size = 1000,
  show_explanation = TRUE,
  show_cost_analysis = TRUE
)

# Clinical Interpretation:
# - Parallel testing: Positive if EITHER test positive
# - Maximizes sensitivity: 96.3% (vs 85% or 75% alone)
# - Trade-off: Lower specificity (68%)
# - Cost: Both tests for everyone ($80/patient)
#
# When to use parallel testing:
# - Emergency/critical decisions
# - Missing cases has severe consequences
# - Both tests are affordable and fast
# - Willing to accept more false positives for safety


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 5: Prostate Cancer Screening (Serial Negative / Exclusion)
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# PSA screening for prostate cancer with MRI follow-up
# Strategy: Only do expensive MRI if PSA is negative (to rule out)
#
# Research Question:
# Can serial negative strategy safely exclude prostate cancer
# while reducing unnecessary expensive MRI scans?

sequentialtests(
  test1_name = "PSA Screening",
  test1_sens = 0.80,
  test1_spec = 0.75,
  test1_cost = 50,
  test2_name = "Multiparametric MRI",
  test2_sens = 0.90,
  test2_spec = 0.85,
  test2_cost = 1000,
  strategy = "serial_negative",
  prevalence = 0.15,
  population_size = 5000,
  show_cost_analysis = TRUE,
  show_nomogram = TRUE
)

# Clinical Interpretation:
# - Serial negative: MRI only if PSA negative
# - High combined sensitivity: 98% (excellent for exclusion)
# - Lower specificity: 63.8% (more false positives)
# - Cost consideration: MRI only for ~25% of patients
#
# When to use serial negative:
# - Need to confidently rule out disease
# - Confirmatory test is very expensive
# - False negatives more problematic than false positives
# - Willing to do more confirmatory tests to catch all cases


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 6: Strategy Comparison - Same Tests, Different Strategies
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Compare all three strategies using the same two tests
# to understand which strategy is optimal for your clinical scenario
#
# Research Question:
# Which sequential testing strategy provides the best balance
# for intermediate prevalence disease?

# Strategy 1: Serial Positive (Confirmation)
serial_pos <- sequentialtests(
  test1_name = "Test A",
  test1_sens = 0.88,
  test1_spec = 0.92,
  test1_cost = 75,
  test2_name = "Test B",
  test2_sens = 0.92,
  test2_spec = 0.88,
  test2_cost = 150,
  strategy = "serial_positive",
  prevalence = 0.15,
  population_size = 5000,
  show_cost_analysis = TRUE
)

# Strategy 2: Serial Negative (Exclusion)
serial_neg <- sequentialtests(
  test1_name = "Test A",
  test1_sens = 0.88,
  test1_spec = 0.92,
  test1_cost = 75,
  test2_name = "Test B",
  test2_sens = 0.92,
  test2_spec = 0.88,
  test2_cost = 150,
  strategy = "serial_negative",
  prevalence = 0.15,
  population_size = 5000,
  show_cost_analysis = TRUE
)

# Strategy 3: Parallel Testing
parallel <- sequentialtests(
  test1_name = "Test A",
  test1_sens = 0.88,
  test1_spec = 0.92,
  test1_cost = 75,
  test2_name = "Test B",
  test2_sens = 0.92,
  test2_spec = 0.88,
  test2_cost = 150,
  strategy = "parallel",
  prevalence = 0.15,
  population_size = 5000,
  show_cost_analysis = TRUE
)

# Comparison Summary:
# Serial Positive: High specificity (99.0%), lower sensitivity (81.0%)
#                  → Best for confirmation, minimize false positives
#
# Serial Negative: High sensitivity (98.9%), lower specificity (80.7%)
#                  → Best for exclusion, minimize false negatives
#
# Parallel:        High sensitivity (99.0%), lower specificity (81.0%)
#                  → Best for emergencies, can't miss cases
#
# Cost considerations vary by strategy and prevalence


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 7: TB Screening in High-Risk Population
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Tuberculosis screening program using chest X-ray followed by
# sputum culture confirmation for positive X-rays
#
# Research Question:
# Design cost-effective TB screening protocol for refugee population
# with moderate-high disease prevalence

sequentialtests(
  test1_name = "Chest X-ray",
  test1_sens = 0.80,
  test1_spec = 0.85,
  test1_cost = 40,
  test2_name = "Sputum Culture",
  test2_sens = 0.90,
  test2_spec = 0.95,
  test2_cost = 200,
  strategy = "serial_positive",
  prevalence = 0.25,
  population_size = 2000,
  show_cost_analysis = TRUE,
  show_explanation = TRUE
)

# Clinical Interpretation:
# - Moderate-high prevalence (25%) in high-risk population
# - Serial positive confirms X-ray positives with culture
# - Combined sensitivity: 72%, specificity: 99.2%
# - Cost per person: $66 (vs $240 if everyone cultured)
# - Culture only needed for ~40% of population
#
# Public health consideration:
# Balance case detection vs resource constraints
# Serial strategy achieves good performance at sustainable cost


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 8: HIV Screening - ELISA to Western Blot
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Standard HIV screening protocol
# High-sensitivity ELISA followed by Western blot confirmation
#
# Research Question:
# Evaluate standard two-tier HIV testing algorithm performance

sequentialtests(
  test1_name = "ELISA (4th generation)",
  test1_sens = 0.99,
  test1_spec = 0.95,
  test1_cost = 25,
  test2_name = "Western Blot",
  test2_sens = 0.98,
  test2_spec = 0.99,
  test2_cost = 150,
  strategy = "serial_positive",
  prevalence = 0.002,
  population_size = 50000,
  show_cost_analysis = TRUE,
  show_nomogram = TRUE
)

# Clinical Interpretation:
# - Very low prevalence (0.2%) requires high specificity
# - Serial positive strategy: Confirm ELISA positives
# - Excellent combined specificity: 99.95%
# - Combined sensitivity: 97%
# - Dramatically improves PPV compared to ELISA alone
#
# Key teaching point:
# When prevalence is very low, even high specificity tests
# have poor PPV. Confirmation step is essential.


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 9: Cost-Effectiveness Analysis
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Budget planning for screening program
# Compare costs of different testing strategies
#
# Research Question:
# What is the incremental cost per true positive detected
# for serial vs parallel strategies?

# Low-cost screening, expensive confirmation
cost_effective <- sequentialtests(
  test1_name = "Point-of-Care Rapid Test",
  test1_sens = 0.90,
  test1_spec = 0.85,
  test1_cost = 5,
  test2_name = "Laboratory Gold Standard",
  test2_sens = 0.98,
  test2_spec = 0.99,
  test2_cost = 500,
  strategy = "serial_positive",
  prevalence = 0.08,
  population_size = 10000,
  show_cost_analysis = TRUE
)

# Inappropriate: Expensive screening
cost_ineffective <- sequentialtests(
  test1_name = "Expensive Screening (NOT RECOMMENDED)",
  test1_sens = 0.95,
  test1_spec = 0.90,
  test1_cost = 400,
  test2_name = "Cheaper Confirmation",
  test2_sens = 0.90,
  test2_spec = 0.95,
  test2_cost = 100,
  strategy = "serial_positive",
  prevalence = 0.08,
  population_size = 10000,
  show_cost_analysis = TRUE
)

# Economic Interpretation:
# Cost-effective approach:
#   - Cheap screening ($5) for everyone
#   - Expensive confirmation ($500) only for positives
#   - Total cost: ~$100,000 for 10,000 people
#
# Cost-ineffective approach:
#   - Expensive screening ($400) for everyone
#   - Total cost: ~$4,100,000 for same population
#   - Minimal gain in accuracy doesn't justify 41× cost increase
#
# Lesson: Screening test should be inexpensive and sensitive


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 10: Rare Disease Screening
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Newborn screening for rare genetic disorder (prevalence 0.01%)
# Need very high specificity to avoid unnecessary confirmatory testing
#
# Research Question:
# Design two-tier screening strategy that minimizes false positives
# while maintaining acceptable sensitivity

sequentialtests(
  test1_name = "Biochemical Screen",
  test1_sens = 0.95,
  test1_spec = 0.99,
  test1_cost = 20,
  test2_name = "Genetic Confirmation",
  test2_sens = 0.99,
  test2_spec = 0.999,
  test2_cost = 2000,
  strategy = "serial_positive",
  prevalence = 0.0001,
  population_size = 100000,
  show_cost_analysis = TRUE,
  show_nomogram = TRUE
)

# Clinical Interpretation:
# - Extremely rare disease (1 in 10,000)
# - Even with 99% specificity, many false positives without confirmation
# - Serial positive strategy essential
# - Combined specificity: 99.999% (only ~10 false positives)
# - Genetic testing only for ~1,010 infants (1% of population)
# - Cost: ~$2.2 million for 100,000 births
#
# Public health impact:
# Without confirmation, 1,000 false positives → unnecessary anxiety
# With confirmation, only ~10 false positives
# Finds ~9 of 10 true cases


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 11: Emergency Stroke Assessment
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Emergency stroke protocol - clinical assessment + CT scan
# Time-critical decision with severe consequences if missed
#
# Research Question:
# Should we use parallel testing (both assessments) or sequential?

sequentialtests(
  test1_name = "FAST Clinical Assessment",
  test1_sens = 0.85,
  test1_spec = 0.75,
  test1_cost = 0,
  test2_name = "CT/MRI Imaging",
  test2_sens = 0.90,
  test2_spec = 0.95,
  test2_cost = 800,
  strategy = "parallel",
  prevalence = 0.35,
  population_size = 500,
  show_explanation = TRUE,
  show_cost_analysis = TRUE
)

# Clinical Interpretation:
# - Emergency setting requires maximum sensitivity
# - Parallel testing: Positive if EITHER test positive
# - Combined sensitivity: 98.5% (can't afford to miss strokes)
# - Combined specificity: 71.3% (acceptable false positive rate)
# - All patients get both assessments
# - Time to treatment: Minimized (tests run simultaneously)
#
# Emergency medicine principle:
# Sensitivity > Specificity when:
#   1. Disease is life-threatening
#   2. Treatment window is narrow
#   3. Cost of false negative >> cost of false positive


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 12: Prevalence Sensitivity Analysis
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Understand how disease prevalence affects optimal strategy
# Same tests, different prevalence scenarios
#
# Research Question:
# How does prevalence affect PPV, NPV, and optimal strategy choice?

# Low prevalence (0.1%) - Screening population
low_prev <- sequentialtests(
  test1_sens = 0.90,
  test1_spec = 0.95,
  test2_sens = 0.95,
  test2_spec = 0.98,
  strategy = "serial_positive",
  prevalence = 0.001,
  population_size = 100000,
  show_nomogram = TRUE
)

# Moderate prevalence (10%) - Symptomatic patients
mod_prev <- sequentialtests(
  test1_sens = 0.90,
  test1_spec = 0.95,
  test2_sens = 0.95,
  test2_spec = 0.98,
  strategy = "serial_positive",
  prevalence = 0.10,
  population_size = 10000,
  show_nomogram = TRUE
)

# High prevalence (50%) - High-risk cohort
high_prev <- sequentialtests(
  test1_sens = 0.90,
  test1_spec = 0.95,
  test2_sens = 0.95,
  test2_spec = 0.98,
  strategy = "serial_positive",
  prevalence = 0.50,
  population_size = 1000,
  show_nomogram = TRUE
)

# Prevalence Effect Summary:
#
# Low prevalence (0.1%):
#   - PPV: Very low without confirmation (~2%)
#   - NPV: Excellent (>99.9%)
#   - Confirmation ESSENTIAL to reduce false positives
#
# Moderate prevalence (10%):
#   - PPV: Moderate with confirmation (~95%)
#   - NPV: Excellent (~99%)
#   - Confirmation improves clinical utility
#
# High prevalence (50%):
#   - PPV: Excellent even without confirmation (~95%)
#   - NPV: Good but lower (~95%)
#   - Confirmation may be less critical
#
# Key insight: Prevalence dramatically affects PPV but not NPV


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 13: Teaching Example - Perfect vs Poor Tests
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Educational demonstration of how test quality affects outcomes
#
# Scenario A: Two near-perfect tests
perfect_tests <- sequentialtests(
  test1_name = "Perfect Screening",
  test1_sens = 0.99,
  test1_spec = 0.99,
  test2_name = "Perfect Confirmation",
  test2_sens = 0.99,
  test2_spec = 0.99,
  strategy = "serial_positive",
  prevalence = 0.10,
  population_size = 1000,
  show_explanation = TRUE,
  show_formulas = TRUE
)

# Scenario B: Two poor-quality tests
poor_tests <- sequentialtests(
  test1_name = "Poor Screening",
  test1_sens = 0.65,
  test1_spec = 0.70,
  test2_name = "Poor Confirmation",
  test2_sens = 0.70,
  test2_spec = 0.75,
  strategy = "serial_positive",
  prevalence = 0.10,
  population_size = 1000,
  show_explanation = TRUE,
  show_formulas = TRUE
)

# Teaching Points:
#
# Perfect tests:
#   - Combined sensitivity: 98%, specificity: 99.99%
#   - Excellent clinical performance
#   - Sequential testing adds little value (already near-perfect)
#
# Poor tests:
#   - Combined sensitivity: 45.5%, specificity: 92.5%
#   - Unacceptable for clinical use
#   - Sequential testing cannot rescue poor test quality
#
# Lesson: Test quality matters!
# Sequential strategies optimize performance but cannot fix bad tests


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 14: Multi-Step Triage Protocol
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Three-tier diagnostic protocol for resource-limited setting
# Step 1: Clinical triage (free)
# Step 2: Rapid test (cheap)
# Step 3: Laboratory confirmation (expensive)
#
# Research Question:
# Model multi-step protocol as sequential pairwise analyses

# Tier 1→2: Clinical triage to rapid test
triage_to_rapid <- sequentialtests(
  test1_name = "Clinical Symptoms",
  test1_sens = 0.95,
  test1_spec = 0.60,
  test1_cost = 0,
  test2_name = "Rapid Antigen",
  test2_sens = 0.85,
  test2_spec = 0.90,
  test2_cost = 10,
  strategy = "serial_positive",
  prevalence = 0.15,
  population_size = 10000,
  show_cost_analysis = TRUE
)

# Tier 2→3: Rapid test to laboratory confirmation
rapid_to_lab <- sequentialtests(
  test1_name = "Rapid Antigen",
  test1_sens = 0.85,
  test1_spec = 0.90,
  test1_cost = 10,
  test2_name = "Laboratory PCR",
  test2_sens = 0.98,
  test2_spec = 0.99,
  test2_cost = 150,
  strategy = "serial_positive",
  prevalence = 0.40,  # Enriched after triage
  population_size = 6700,  # Positive from tier 1
  show_cost_analysis = TRUE
)

# Multi-step Interpretation:
# Tier 1: Screens 10,000 → ~6,700 positive (mostly true + false positives)
# Tier 2: Tests 6,700 → ~1,900 positive (further refinement)
# Tier 3: Confirms 1,900 → Final diagnosis
#
# Cost cascade:
#   - Tier 1: $0 (clinical only)
#   - Tier 2: $67,000 (6,700 × $10)
#   - Tier 3: $285,000 (1,900 × $150)
#   - Total: $352,000 for 10,000 people
#   - vs $1,500,000 if everyone got PCR
#
# Lesson: Multi-tier protocols maximize efficiency


# ═══════════════════════════════════════════════════════════════════════════
# EXAMPLE 15: Publication-Ready Comprehensive Analysis
# ═══════════════════════════════════════════════════════════════════════════

# Clinical Context:
# Complete analysis for publication with all reporting options
# Includes explanations, formulas, cost analysis, and Fagan nomogram
#
# Research Question:
# Comprehensive evaluation of novel diagnostic algorithm
# for regulatory submission or publication

sequentialtests(
  test1_name = "Novel Point-of-Care Biomarker",
  test1_sens = 0.88,
  test1_spec = 0.92,
  test1_cost = 45,
  test2_name = "Reference Laboratory Test",
  test2_sens = 0.94,
  test2_spec = 0.96,
  test2_cost = 280,
  strategy = "serial_positive",
  prevalence = 0.12,
  population_size = 5000,
  show_explanation = TRUE,
  show_formulas = TRUE,
  show_cost_analysis = TRUE,
  show_nomogram = TRUE
)

# Publication Reporting:
# Include in methods:
#   - Test characteristics (sens/spec) with confidence intervals
#   - Sequential testing strategy and rationale
#   - Population prevalence estimate
#   - Cost data sources
#
# Include in results:
#   - Combined performance metrics
#   - Population flow diagram (from analysis output)
#   - Cost per diagnosis
#   - Cost-effectiveness ratio
#
# Include in discussion:
#   - Comparison with alternative strategies
#   - Clinical implications of false positive/negative rates
#   - Generalizability to different prevalence settings
#   - Economic considerations for implementation


# ═══════════════════════════════════════════════════════════════════════════
# ADDITIONAL NOTES AND BEST PRACTICES
# ═══════════════════════════════════════════════════════════════════════════

# 1. Choosing the Right Strategy:
#
#    Serial Positive (Confirmation):
#      ✓ Need high specificity
#      ✓ Confirmatory test is expensive
#      ✓ False positives have serious consequences
#      ✓ Common in: Screening programs, diagnostic confirmation
#
#    Serial Negative (Exclusion):
#      ✓ Need high sensitivity
#      ✓ Must not miss cases
#      ✓ False negatives have serious consequences
#      ✓ Common in: Disease exclusion, ruling out serious conditions
#
#    Parallel Testing:
#      ✓ Emergency situations
#      ✓ Both tests are fast and affordable
#      ✓ Maximum sensitivity required
#      ✓ Common in: Emergency medicine, critical care

# 2. Understanding Performance Metrics:
#
#    Sensitivity: Ability to detect disease when present
#      - Serial positive: MULTIPLIES (reduces sensitivity)
#      - Serial negative: ADDS (increases sensitivity)
#      - Parallel: ADDS (increases sensitivity)
#
#    Specificity: Ability to identify health when disease absent
#      - Serial positive: ADDS (increases specificity)
#      - Serial negative: MULTIPLIES (reduces specificity)
#      - Parallel: MULTIPLIES (reduces specificity)

# 3. Cost Considerations:
#
#    - Screening test should be inexpensive and sensitive
#    - Confirmatory test can be expensive but must be specific
#    - Consider total program cost, not just per-test cost
#    - Account for costs of false positives (anxiety, follow-up)
#    - Account for costs of false negatives (missed disease, progression)

# 4. Prevalence Effects:
#
#    - PPV increases with prevalence
#    - NPV decreases with prevalence
#    - Sensitivity and specificity independent of prevalence
#    - Always consider your population's actual prevalence
#    - Screening programs (low prevalence) need different strategies
#      than diagnostic testing (higher pre-test probability)

# 5. Clinical Presets:
#
#    - Based on published test characteristics
#    - Useful for planning, teaching, feasibility assessment
#    - May need adjustment for local population characteristics
#    - Should be validated against your own data when possible

# 6. Population Flow Analysis:
#
#    - Shows how many people proceed through each testing stage
#    - Critical for resource planning and logistics
#    - Helps identify bottlenecks in testing pathway
#    - Useful for explaining protocol to administrators/funders

# 7. Fagan Nomogram:
#
#    - Visual representation of Bayesian probability revision
#    - Shows how test results change probability of disease
#    - Excellent teaching tool
#    - Helps clinicians interpret test results in context

# ═══════════════════════════════════════════════════════════════════════════
# END OF EXAMPLES
# ═══════════════════════════════════════════════════════════════════════════
