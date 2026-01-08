################################################################################
# KAPPASIZECI FUNCTION - COMPREHENSIVE USAGE EXAMPLES
################################################################################
#
# Function: kappaSizeCI
# Purpose: Confidence Interval (CI) based sample size calculation for
#          interobserver agreement studies
# Approach: Precision-based (CI width) rather than power-based
#
# Key Concept: This function calculates the required sample size to estimate
# kappa with a specified confidence interval width, ensuring precise estimation
# rather than hypothesis testing power.
#
################################################################################

# Load the ClinicoPath package
library(ClinicoPath)

################################################################################
# EXAMPLE 1: Emergency Department Stroke CT - Moderate Precision
################################################################################
# Clinical Context: Two emergency radiologists establishing stroke protocol
# Research Question: How many CT scans needed to estimate agreement (κ=0.70)
#                   with 95% CI width of ±0.10?
# Justification: Standard precision acceptable for clinical quality improvement

kappaSizeCI(
  outcome = "2",           # Binary: Stroke present vs absent
  kappa0 = 0.70,          # Expected kappa (good agreement)
  kappaL = 0.60,          # Lower CI limit (κ - 0.10)
  kappaU = 0.80,          # Upper CI limit (κ + 0.10)
  props = "0.15, 0.85",   # 15% stroke prevalence in ED
  raters = "2",           # Two emergency radiologists
  alpha = 0.05            # 95% confidence interval
)

# Clinical Interpretation:
# - CI Width: 0.20 (±0.10) provides moderate precision
# - Sample Size: [calculated value] CT scans needed
# - Implementation: Review consecutive eligible emergency CT scans
# - Timeline: Based on ED volume, estimate completion time
# - Quality: Standard precision adequate for protocol validation

################################################################################
# EXAMPLE 2: Mammography BIRADS - Narrow Precision (High Precision Need)
################################################################################
# Clinical Context: Two breast radiologists for clinical guideline
# Research Question: How many mammograms needed for high precision (narrow CI)?
# Justification: Clinical guidelines require stringent precision estimates

kappaSizeCI(
  outcome = "2",           # Binary: Suspicious vs benign
  kappa0 = 0.80,          # Expected kappa (good-excellent agreement)
  kappaL = 0.75,          # Lower CI limit (κ - 0.05)
  kappaU = 0.85,          # Upper CI limit (κ + 0.05)
  props = "0.20, 0.80",   # 20% suspicious findings
  raters = "2",           # Two breast radiologists
  alpha = 0.05            # 95% confidence interval
)

# Clinical Interpretation:
# - CI Width: 0.10 (±0.05) provides narrow, high precision
# - Narrower CI requires larger sample than moderate precision
# - Use Case: Guideline recommendations, regulatory submissions
# - Quality Standards: High precision justified by clinical impact

################################################################################
# EXAMPLE 3: Dermatology Melanoma - Wide Precision (Pilot Study)
################################################################################
# Clinical Context: Initial pilot reliability study
# Research Question: How many cases needed for preliminary agreement estimate?
# Justification: Wide CI acceptable for feasibility/pilot phase

kappaSizeCI(
  outcome = "2",           # Binary: Melanoma vs benign
  kappa0 = 0.60,          # Expected kappa (moderate agreement)
  kappaL = 0.45,          # Lower CI limit (κ - 0.15)
  kappaU = 0.75,          # Upper CI limit (κ + 0.15)
  props = "0.10, 0.90",   # 10% melanoma (rare event)
  raters = "2",           # Two dermatologists
  alpha = 0.05            # 95% confidence interval
)

# Clinical Interpretation:
# - CI Width: 0.30 (±0.15) wide precision acceptable for pilot
# - Smaller sample size feasible for initial study
# - Next Steps: If promising, conduct larger study with narrower CI
# - Resource Efficiency: Pilot with wide CI minimizes initial resource use

################################################################################
# EXAMPLE 4: HER2 IHC Scoring - Very Tight Precision (Regulatory)
################################################################################
# Clinical Context: Pathology validation for FDA regulatory submission
# Research Question: Very narrow CI with stringent confidence level
# Justification: Regulatory requirements demand highest precision

kappaSizeCI(
  outcome = "2",           # Binary: HER2 positive vs negative
  kappa0 = 0.85,          # Expected kappa (excellent agreement)
  kappaL = 0.80,          # Lower CI limit (κ - 0.05)
  kappaU = 0.90,          # Upper CI limit (κ + 0.05)
  props = "0.25, 0.75",   # 25% HER2 positive
  raters = "2",           # Two expert breast pathologists
  alpha = 0.01            # 99% confidence interval (stringent)
)

# Clinical Interpretation:
# - CI Width: 0.10 (±0.05) at 99% confidence
# - Very large sample needed for regulatory standards
# - Context: FDA submission, diagnostic test validation
# - Quality: Highest precision and confidence required

################################################################################
# EXAMPLE 5: Burn Severity - Three Categories
################################################################################
# Clinical Context: Burn center severity classification validation
# Research Question: Sample size for 3-level burn severity agreement
# Ordinal Scale: 1st degree, 2nd degree, 3rd degree

kappaSizeCI(
  outcome = "3",                     # Three severity levels
  kappa0 = 0.70,                    # Expected kappa
  kappaL = 0.63,                    # Lower CI limit
  kappaU = 0.77,                    # Upper CI limit
  props = "0.20, 0.50, 0.30",       # Distribution across severity
  raters = "2",                     # Two burn surgeons
  alpha = 0.05                      # 95% confidence interval
)

# Clinical Interpretation:
# - Ordinal Scale: More categories increase complexity
# - Sample Size: Larger than binary for same CI width
# - Proportion Balance: Affects required sample size
# - Clinical Application: Burn center quality assessment

################################################################################
# EXAMPLE 6: Pediatric Pain Scale - Three Categories, Three Raters
################################################################################
# Clinical Context: FLACC pain scale validation with multiple raters
# Research Question: How does 3 raters affect sample size?
# Design: Multiple rater design for pediatric pain assessment

kappaSizeCI(
  outcome = "3",                     # Three pain levels: mild/moderate/severe
  kappa0 = 0.65,                    # Expected moderate agreement
  kappaL = 0.55,                    # Lower CI limit
  kappaU = 0.75,                    # Upper CI limit
  props = "0.30, 0.50, 0.20",       # Pain distribution
  raters = "3",                     # Three pediatric nurses
  alpha = 0.05                      # 95% confidence interval
)

# Clinical Interpretation:
# - Multiple Raters: Three raters provide more information per subject
# - May reduce required subjects compared to 2 raters
# - Practical Considerations: Coordination complexity, cost
# - Use Case: Clinical implementation, nursing training validation

################################################################################
# EXAMPLE 7: Tumor Histologic Grade - Four Categories
################################################################################
# Clinical Context: Pathology quality program for tumor grading
# Research Question: Four-level tumor grade agreement precision
# Classification: Well/Moderately/Poorly differentiated/Undifferentiated

kappaSizeCI(
  outcome = "4",                          # Four grade levels
  kappa0 = 0.72,                         # Expected agreement
  kappaL = 0.65,                         # Lower CI limit
  kappaU = 0.79,                         # Upper CI limit
  props = "0.25, 0.35, 0.25, 0.15",      # Grade distribution
  raters = "2",                          # Two pathologists
  alpha = 0.05                           # 95% confidence interval
)

# Clinical Interpretation:
# - Four Categories: Substantially more complex than binary
# - Sample Size: Larger due to increased complexity
# - Clinical Impact: Tumor grade affects treatment decisions
# - Quality Program: Adequate precision for QA monitoring

################################################################################
# EXAMPLE 8: Glasgow Coma Scale - Four Categories, Three Raters
################################################################################
# Clinical Context: Emergency nursing protocol validation
# Research Question: GCS assessment agreement with multiple raters
# Design: Multi-rater emergency assessment

kappaSizeCI(
  outcome = "4",                          # Four GCS categories
  kappa0 = 0.68,                         # Expected agreement
  kappaL = 0.58,                         # Lower CI limit
  kappaU = 0.78,                         # Upper CI limit
  props = "0.15, 0.30, 0.35, 0.20",      # GCS distribution
  raters = "3",                          # Three ER nurses
  alpha = 0.05                           # 95% confidence interval
)

# Clinical Interpretation:
# - Complex Assessment: 4 categories + 3 raters
# - Emergency Setting: Rapid assessment protocol
# - Training: Results inform nursing education needs
# - Practical Implementation: Feasible sample size for ER setting

################################################################################
# EXAMPLE 9: Diabetic Retinopathy - Four Categories, Stringent Alpha
################################################################################
# Clinical Context: National screening program validation
# Research Question: Retinopathy grading for population screening
# High Stakes: Requires stringent confidence and narrow precision

kappaSizeCI(
  outcome = "4",                          # Four DR grades
  kappa0 = 0.78,                         # Expected good agreement
  kappaL = 0.71,                         # Lower CI limit (narrow)
  kappaU = 0.85,                         # Upper CI limit (narrow)
  props = "0.40, 0.30, 0.20, 0.10",      # DR distribution
  raters = "2",                          # Two ophthalmologists
  alpha = 0.01                           # 99% confidence interval
)

# Clinical Interpretation:
# - Population Screening: High-stakes public health application
# - Stringent Requirements: 99% CI + narrow width
# - Very Large Sample: Justified by screening program scope
# - Quality Standards: National program demands highest precision

################################################################################
# EXAMPLE 10: Liver Fibrosis Staging - Five Categories
################################################################################
# Clinical Context: Hepatopathology fibrosis staging (F0-F4)
# Research Question: Five-level staging agreement estimation
# Classification: F0 (no fibrosis) through F4 (cirrhosis)

kappaSizeCI(
  outcome = "5",                                # Five fibrosis stages
  kappa0 = 0.75,                               # Expected agreement
  kappaL = 0.68,                               # Lower CI limit
  kappaU = 0.82,                               # Upper CI limit
  props = "0.25, 0.20, 0.20, 0.20, 0.15",      # Stage distribution
  raters = "2",                                # Two hepatopathologists
  alpha = 0.05                                 # 95% confidence interval
)

# Clinical Interpretation:
# - Maximum Complexity: Five categories most complex
# - Largest Sample: More categories require more subjects
# - Clinical Importance: Fibrosis stage guides treatment
# - Precision Need: Adequate CI width for clinical guideline

################################################################################
# EXAMPLE 11: Cancer TNM Staging - Five Categories, Moderate Precision
################################################################################
# Clinical Context: Oncology quality assurance program
# Research Question: TNM staging agreement for QA monitoring
# Stages: Stage 0, I, II, III, IV

kappaSizeCI(
  outcome = "5",                                # Five TNM stages
  kappa0 = 0.72,                               # Expected agreement
  kappaL = 0.62,                               # Lower CI limit
  kappaU = 0.82,                               # Upper CI limit
  props = "0.20, 0.25, 0.25, 0.20, 0.10",      # Stage distribution
  raters = "2",                                # Two oncologists
  alpha = 0.05                                 # 95% confidence interval
)

# Clinical Interpretation:
# - Moderate Precision: CI width = 0.20 (±0.10)
# - QA Application: Standard precision for quality monitoring
# - Feasible Sample: Achievable within typical QA timeframe
# - Ongoing Monitoring: Can repeat periodically

################################################################################
# EXAMPLE 12: Geriatric Frailty Index - Five Categories, Three Raters
################################################################################
# Clinical Context: Geriatric assessment tool validation
# Research Question: Multi-rater frailty classification
# Design: Three geriatricians, pilot study with wide CI

kappaSizeCI(
  outcome = "5",                                # Five frailty levels
  kappa0 = 0.62,                               # Expected moderate agreement
  kappaL = 0.47,                               # Lower CI limit (wide)
  kappaU = 0.77,                               # Upper CI limit (wide)
  props = "0.20, 0.20, 0.25, 0.20, 0.15",      # Frailty distribution
  raters = "3",                                # Three geriatricians
  alpha = 0.05                                 # 95% confidence interval
)

# Clinical Interpretation:
# - Pilot Study: Wide CI (0.30) acceptable for initial work
# - Three Raters: Multiple perspectives on frailty
# - Smaller Sample: Wide CI reduces sample requirement
# - Next Phase: If promising, conduct larger study with narrow CI

################################################################################
# EXAMPLE 13: Rare Event - Extremely Imbalanced Proportions
################################################################################
# Clinical Context: Rare pathology diagnosis agreement
# Research Question: Sample size when outcome is rare (5% prevalence)
# Challenge: Rare events require larger samples

kappaSizeCI(
  outcome = "2",           # Binary: Rare condition present vs absent
  kappa0 = 0.70,          # Expected kappa
  kappaL = 0.60,          # Lower CI limit
  kappaU = 0.80,          # Upper CI limit
  props = "0.05, 0.95",   # Only 5% positive cases
  raters = "2",           # Two pathologists
  alpha = 0.05            # 95% confidence interval
)

# Clinical Interpretation:
# - Rare Events: Extreme imbalance increases sample size dramatically
# - Need Many Subjects: To observe sufficient positive cases
# - Alternative Strategies: Enriched sampling, longer study period
# - Practical Considerations: Feasibility assessment critical

################################################################################
# EXAMPLE 14: Biomarker Cutpoint - Three Raters, Stringent
################################################################################
# Clinical Context: Biomarker cutpoint validation for diagnostic use
# Research Question: Multi-rater validation with stringent requirements
# High Stakes: Diagnostic cutpoint affects patient management

kappaSizeCI(
  outcome = "2",           # Binary: Above vs below cutpoint
  kappa0 = 0.78,          # Expected good agreement
  kappaL = 0.71,          # Lower CI limit (narrow)
  kappaU = 0.85,          # Upper CI limit (narrow)
  props = "0.35, 0.65",   # 35% above cutpoint
  raters = "3",           # Three pathologists
  alpha = 0.01            # 99% confidence interval
)

# Clinical Interpretation:
# - Multiple Raters + Stringent: High quality validation
# - Narrow CI + 99% confidence: Maximum precision and confidence
# - Large Sample Needed: Justified by diagnostic importance
# - Regulatory Context: Appropriate for test validation

################################################################################
# EXAMPLE 15: AI Algorithm Validation - Moderate Precision
################################################################################
# Clinical Context: AI vs radiologist agreement estimation
# Research Question: Sample size to estimate AI-human agreement
# Application: Regulatory approval for AI diagnostic tool

kappaSizeCI(
  outcome = "2",           # Binary: AI vs radiologist classification
  kappa0 = 0.72,          # Expected good agreement
  kappaL = 0.62,          # Lower CI limit
  kappaU = 0.82,          # Upper CI limit
  props = "0.25, 0.75",   # 25% positive findings
  raters = "2",           # AI algorithm vs radiologist
  alpha = 0.05            # 95% confidence interval
)

# Clinical Interpretation:
# - AI Validation: Estimate AI-human agreement precision
# - Standard Precision: CI width = 0.20 adequate for validation
# - Regulatory Path: Results support FDA 510(k) submission
# - Implementation: If agreement adequate, consider deployment

################################################################################
# BEST PRACTICES: CHOOSING CONFIDENCE INTERVAL WIDTH
################################################################################

# 1. CI Width Selection Guidelines:
#
#    Very Narrow (±0.03 to ±0.05, width 0.06-0.10):
#    - Regulatory submissions (FDA, CE mark)
#    - Clinical practice guidelines
#    - Diagnostic test validation
#    - Large sample requirements
#
#    Narrow (±0.05 to ±0.07, width 0.10-0.14):
#    - Quality assurance programs
#    - Published validation studies
#    - Multi-center trials
#    - Moderate-large sample requirements
#
#    Moderate (±0.10, width 0.20):
#    - Standard clinical research
#    - Quality improvement projects
#    - Educational validation
#    - Reasonable sample requirements
#
#    Wide (±0.15, width 0.30):
#    - Pilot/feasibility studies
#    - Initial reliability assessment
#    - Exploratory research
#    - Small sample acceptable

# 2. Kappa0 Estimation:
#    - Use pilot data if available
#    - Literature review for similar assessments
#    - Conservative estimates preferable
#    - Lower kappa requires larger samples

# 3. Alpha Level Selection:
#    - 0.05 (95% CI): Standard for most studies
#    - 0.01 (99% CI): Regulatory, high-stakes applications
#    - 0.10 (90% CI): Exploratory, pilot studies
#    - More stringent alpha increases sample size

# 4. Proportion Balance Considerations:
#    - Balanced proportions (50-50): Most efficient
#    - Moderate imbalance (30-70): Modest sample increase
#    - Severe imbalance (10-90): Substantial sample increase
#    - Extreme imbalance (<5%): Very large samples needed
#    - Consider enriched sampling for rare events

# 5. Number of Raters:
#    - Two raters: Most common, cost-effective
#    - Three raters: Provides redundancy, may reduce n
#    - Four-five raters: Rich data but coordination complexity
#    - Cost-benefit analysis important

# 6. Number of Categories:
#    - Binary (2): Simplest, smallest samples
#    - Three categories: Moderate increase
#    - Four categories: Substantial increase
#    - Five categories: Largest samples needed
#    - Consider collapsing categories if justified

# 7. CI-Based vs Power-Based Approach:
#
#    Use CI-Based (kappaSizeCI) when:
#    - Estimating current agreement (descriptive)
#    - Validating a scale or instrument
#    - Quality assurance monitoring
#    - No specific hypothesis to test
#    - Focus on precision of estimate
#
#    Use Power-Based (kappaSizePower) when:
#    - Testing if agreement improved (before-after)
#    - Comparing two methods/raters
#    - Hypothesis: κ > threshold value
#    - Intervention study design
#    - Focus on detecting differences

# 8. Sample Size Interpretation:
#    - Result is NUMBER OF SUBJECTS rated by all raters
#    - Each subject assessed by all specified raters
#    - Feasibility assessment critical
#    - Consider attrition, data quality issues
#    - Add 10-20% for potential exclusions

# 9. Reporting Standards:
#    - Report expected kappa and CI width
#    - Justify CI width choice
#    - Report all input parameters
#    - Discuss feasibility
#    - Follow GRRAS guidelines (Kottner et al., 2011)

# 10. Study Design Implications:
#     - Prospective data collection preferred
#     - Blinded, independent assessments
#     - Representative sample of cases
#     - Standardized assessment conditions
#     - Rater training and calibration

################################################################################
# INTERPRETATION OF RESULTS
################################################################################

# The kappaSizeCI function returns the number of SUBJECTS (cases) needed.
#
# Each subject is rated by ALL specified raters.
#
# For example:
# - Result: 150 subjects needed
# - Raters: 2
# - Total assessments: 150 subjects × 2 raters = 300 assessments
#
# Planning considerations:
# - Recruitment timeline based on case availability
# - Rater availability and scheduling
# - Data quality monitoring during collection
# - Budget: Consider time, materials, compensation
# - IRB requirements and informed consent

################################################################################
# REFERENCES
################################################################################

# 1. Cantor AB (1996). Sample-size calculations for Cohen's kappa.
#    Psychological Methods, 1(2), 150-153.
#
# 2. Donner A, Eliasziw M (1992). A goodness-of-fit approach to inference
#    procedures for the kappa statistic: confidence interval construction,
#    significance-testing and sample size estimation. Statistics in Medicine,
#    11(11), 1511-1519.
#
# 3. Flack VF, Afifi AA, Lachenbruch PA, Schouten HJA (1988). Sample size
#    determinations for the two rater kappa statistic. Psychometrika, 53(3),
#    321-325.
#
# 4. Kottner J, et al. (2011). Guidelines for Reporting Reliability and
#    Agreement Studies (GRRAS) were proposed. Journal of Clinical Epidemiology,
#    64(1), 96-106.
#
# 5. Landis JR, Koch GG (1977). The measurement of observer agreement for
#    categorical data. Biometrics, 33(1), 159-174.
#
# 6. Rotondi MA, Donner A (2012). A confidence interval approach to sample
#    size estimation for interobserver agreement studies with multiple raters
#    and outcomes. Journal of Clinical Epidemiology, 65(7), 778-784.

################################################################################
# END OF EXAMPLES
################################################################################
