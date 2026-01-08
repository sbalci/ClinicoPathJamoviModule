# ═══════════════════════════════════════════════════════════════════════════════
# decisioncalculator() - Medical Decision Calculator
# ═══════════════════════════════════════════════════════════════════════════════
#
# Comprehensive examples for the decisioncalculator jamovi function
# Evaluates diagnostic test performance from 2×2 confusion matrix counts
#
# Load package
library(ClinicoPath)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 1: Basic Diagnostic Test Evaluation
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Rapid strep test evaluation in pediatric clinic
# Question: What is the test's sensitivity and specificity?
# Data: 200 patients, culture as gold standard

decisioncalculator(
  TP = 85,   # True Positives
  TN = 90,   # True Negatives
  FP = 10,   # False Positives
  FN = 15    # False Negatives
)

# Clinical Interpretation:
# - Sensitivity = TP/(TP+FN) = 85/100 = 0.85 (85%)
# - Specificity = TN/(TN+FP) = 90/100 = 0.90 (90%)
# - PPV = TP/(TP+FP) = 85/95 = 0.89 (89%)
# - NPV = TN/(TN+FN) = 90/105 = 0.86 (86%)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 2: Add Confidence Intervals
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Same test, but report uncertainty estimates
# Question: How precise are our estimates?

decisioncalculator(
  TP = 85,
  TN = 90,
  FP = 10,
  FN = 15,
  ci = TRUE      # Calculate 95% confidence intervals
)

# Clinical Interpretation:
# - CI width reflects sample size and observed proportion
# - Larger samples → narrower CIs → more precise estimates
# - Report CIs in publications to show estimate precision

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 3: Population Prevalence and Fagan Nomogram
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Mammography screening
# Question: How does test result change probability of breast cancer?
# Note: Study prevalence (enriched) ≠ population prevalence (true)

decisioncalculator(
  TP = 45,
  TN = 808,
  FP = 142,
  FN = 5,
  pp = TRUE,          # Use known population prevalence
  pprob = 0.005,      # 0.5% prevalence in screening population
  fagan = TRUE        # Generate Fagan nomogram
)

# Clinical Interpretation:
# - Study prevalence = 5%, but population prevalence = 0.5%
# - PPV based on study prevalence overestimates real-world PPV
# - Fagan nomogram shows: Pre-test 0.5% → Post-test positive ~3%
# - Even positive result → probability still low due to rare disease

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 4: High-Sensitivity Screening Test
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: HIV screening test
# Question: Design test to catch all cases (minimize false negatives)
# Strategy: Accept more false positives to avoid missing cases

decisioncalculator(
  TP = 95,    # High sensitivity (95%)
  TN = 150,   # Moderate specificity (75%)
  FP = 50,
  FN = 5,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.01,
  showSummary = TRUE
)

# Clinical Interpretation:
# - High sensitivity (95%) good for screening: few missed cases
# - Lower specificity (75%) acceptable: confirm positives with second test
# - Rule-out test: Negative result confidently excludes disease (high NPV)
# - Positive results need confirmation due to many false positives

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 5: High-Specificity Confirmatory Test
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Biopsy after positive imaging
# Question: Confirm suspected malignancy with high confidence
# Strategy: Minimize false positives, accept some false negatives

decisioncalculator(
  TP = 85,    # Moderate sensitivity (85%)
  TN = 195,   # Very high specificity (97.5%)
  FP = 5,
  FN = 15,
  ci = TRUE,
  showSummary = TRUE
)

# Clinical Interpretation:
# - Very high specificity (97.5%): positive result → disease present
# - Rule-in test: Positive result confirms diagnosis (high PPV)
# - Lower sensitivity (85%) acceptable if used after screening
# - Sequential testing: Screen (high sens) → Confirm (high spec)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 6: Comparing Multiple Cut-off Values
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: PSA testing for prostate cancer
# Question: Should we use PSA ≥4 (standard) or PSA ≥2.5 (aggressive)?
# Analysis: Compare sensitivity/specificity trade-offs

decisioncalculator(
  # Current cut-off (PSA ≥4)
  TP = 85,
  TN = 180,
  FP = 20,
  FN = 15,

  multiplecuts = TRUE,

  # Alternative cut-off 1: Conservative (PSA ≥10)
  cutoff1 = "Conservative (≥10)",
  tp1 = 65,
  tn1 = 195,
  fp1 = 5,
  fn1 = 35,

  # Alternative cut-off 2: Aggressive (PSA ≥2.5)
  cutoff2 = "Aggressive (≥2.5)",
  tp2 = 95,
  tn2 = 160,
  fp2 = 40,
  fn2 = 5,

  ci = TRUE
)

# Clinical Interpretation:
# - Conservative (≥10): High specificity, low sensitivity (few biopsies, miss cases)
# - Standard (≥4): Balanced (current guideline)
# - Aggressive (≥2.5): High sensitivity, low specificity (more biopsies, fewer missed)
# - Choice depends on: Patient preferences, biopsy risks, disease severity

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 7: Excellent Biomarker Test
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Troponin for myocardial infarction
# Question: Validate new troponin assay performance

data(decisioncalculator_biomarker)

decisioncalculator(
  TP = decisioncalculator_biomarker$TP,
  TN = decisioncalculator_biomarker$TN,
  FP = decisioncalculator_biomarker$FP,
  FN = decisioncalculator_biomarker$FN,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.15,
  fagan = TRUE,
  showSummary = TRUE
)

# Clinical Interpretation:
# - Excellent test: Sensitivity ~95%, Specificity ~92%
# - Both sens and spec > 90% → clinically useful
# - Good PPV and NPV in moderate prevalence setting
# - Can be used for both rule-in and rule-out

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 8: Rare Disease Screening Challenge
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Newborn screening for PKU
# Question: Why is PPV low despite excellent test?
# Lesson: Prevalence profoundly affects PPV

data(decisioncalculator_raredisease)

decisioncalculator(
  TP = decisioncalculator_raredisease$TP,
  TN = decisioncalculator_raredisease$TN,
  FP = decisioncalculator_raredisease$FP,
  FN = decisioncalculator_raredisease$FN,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.001,   # 1:1000 prevalence
  fagan = TRUE,
  showSummary = TRUE,
  showGlossary = TRUE
)

# Clinical Interpretation:
# - Test performance: Sensitivity 90%, Specificity 99% (excellent!)
# - But PPV only ~8% due to low prevalence
# - Most positive results are false positives
# - Confirms with second test before treatment
# - NPV remains excellent (99.9%) - negative = truly negative

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 9: Prevalence Effect on PPV/NPV
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Same rapid strep test in two settings
# Question: How does prevalence affect predictive values?
# Comparison: Pediatric clinic (high prev) vs adult clinic (low prev)

# High prevalence setting (Pediatric clinic, 40% strep)
decisioncalculator(
  TP = 85,
  TN = 133,
  FP = 7,
  FN = 15,
  pp = TRUE,
  pprob = 0.40,
  fagan = TRUE
)

# Low prevalence setting (Adult clinic, 5% strep)
decisioncalculator(
  TP = 43,
  TN = 903,
  FP = 47,
  FN = 7,
  pp = TRUE,
  pprob = 0.05,
  fagan = TRUE
)

# Clinical Interpretation:
# - Same test characteristics (sens, spec) in both settings
# - High prevalence → High PPV, Good NPV
# - Low prevalence → Low PPV, Excellent NPV
# - Takeaway: Know your population's disease prevalence!

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 10: Point-of-Care Rapid Test
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: COVID-19 rapid antigen test
# Question: Is this rapid test good enough for clinical decisions?

data(decisioncalculator_pointofcare)

decisioncalculator(
  TP = decisioncalculator_pointofcare$TP,
  TN = decisioncalculator_pointofcare$TN,
  FP = decisioncalculator_pointofcare$FP,
  FN = decisioncalculator_pointofcare$FN,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.10,
  fagan = TRUE,
  fnote = TRUE,
  showSummary = TRUE
)

# Clinical Interpretation:
# - Sensitivity ~92%, Specificity ~93%
# - Trade-off: Fast results (15 min) vs slightly lower accuracy than PCR
# - Use when: Rapid decision needed, moderate pre-test probability
# - Limitations: Lower sensitivity in asymptomatic, early infection
# - Consider confirmatory PCR if negative but high suspicion

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 11: Imaging Study Evaluation
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: CT scan for appendicitis
# Question: Does CT justify radiation exposure and cost?

data(decisioncalculator_imaging)

decisioncalculator(
  TP = decisioncalculator_imaging$TP,
  TN = decisioncalculator_imaging$TN,
  FP = decisioncalculator_imaging$FP,
  FN = decisioncalculator_imaging$FN,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.25,
  fagan = TRUE,
  showSummary = TRUE
)

# Clinical Interpretation:
# - Sensitivity ~88%, Specificity ~90%
# - Excellent rule-out: Negative CT → very low post-test probability
# - Consider: Radiation dose, cost, availability
# - Alternatives: Ultrasound (no radiation), clinical scoring
# - Best use: Moderate pre-test probability (20-60%)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 12: Perfect Test (Theoretical)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Theoretical gold standard
# Question: What does perfect test performance look like?

data(decisioncalculator_perfect)

decisioncalculator(
  TP = decisioncalculator_perfect$TP,
  TN = decisioncalculator_perfect$TN,
  FP = decisioncalculator_perfect$FP,
  FN = decisioncalculator_perfect$FN,
  ci = TRUE
)

# Clinical Interpretation:
# - Perfect test: Sensitivity = Specificity = PPV = NPV = 100%
# - Extremely rare in practice
# - If found in real data: Check for circular reasoning
# - Example: Using same test as reference standard (invalid!)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 13: Small Pilot Study
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Preliminary validation of new test
# Question: Is sample size adequate for reliable estimates?

data(decisioncalculator_small)

decisioncalculator(
  TP = decisioncalculator_small$TP,
  TN = decisioncalculator_small$TN,
  FP = decisioncalculator_small$FP,
  FN = decisioncalculator_small$FN,
  ci = TRUE
)

# Clinical Interpretation:
# - Small sample (n=50) → Wide confidence intervals
# - Point estimates unreliable
# - Use for: Pilot study, feasibility assessment
# - Next step: Calculate required sample size for definitive study
# - Aim for: CI width < 10% for clinical utility

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 14: Publication-Ready Comprehensive Analysis
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Final analysis for manuscript
# Question: Complete diagnostic accuracy study reporting
# Requirements: All statistics, CIs, visualizations, interpretations

decisioncalculator(
  TP = 90,
  TN = 180,
  FP = 20,
  FN = 10,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.15,
  fagan = TRUE,
  fnote = TRUE,
  showSummary = TRUE,
  showAbout = TRUE,
  showGlossary = TRUE
)

# Clinical Interpretation:
# - Report: Sensitivity, Specificity, PPV, NPV with 95% CIs
# - Include: Likelihood ratios (LR+, LR-)
# - Visualize: Fagan nomogram for clinical communication
# - Discuss: Impact of prevalence on predictive values
# - Follow: STARD guidelines for diagnostic accuracy studies

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 15: Sequential Testing Strategy
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Two-stage screening program
# Question: Evaluate overall diagnostic pathway
# Strategy: Screen all → Confirm positives

# Stage 1: Screening test (high sensitivity)
screening_result <- decisioncalculator(
  TP = 95,
  TN = 8500,
  FP = 1500,
  FN = 5,
  pp = TRUE,
  pprob = 0.01,
  showSummary = TRUE
)

# Stage 2: Confirmatory test on screen-positives (high specificity)
# Updated prevalence = PPV from screening
confirmatory_result <- decisioncalculator(
  TP = 90,
  TN = 1480,
  FP = 20,
  FN = 5,
  pp = TRUE,
  pprob = 0.06,  # Enriched population after screening
  fagan = TRUE,
  showSummary = TRUE
)

# Clinical Interpretation:
# - Two-stage testing improves overall performance
# - Stage 1: Catch all cases (minimize FN)
# - Stage 2: Confirm cases (minimize FP)
# - Final PPV much higher than single-test approach
# - Cost-effective: Expensive test only on screen-positives

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 16: Educational Example with All Features
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Teaching diagnostic test evaluation
# Question: Demonstrate all calculator features
# Audience: Medical students, residents

decisioncalculator(
  TP = 85,
  TN = 90,
  FP = 10,
  FN = 15,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.20,
  fagan = TRUE,
  fnote = TRUE,
  showWelcome = TRUE,
  showSummary = TRUE,
  showAbout = TRUE,
  showGlossary = TRUE
)

# Teaching Points:
# 1. Sensitivity vs Specificity (test characteristics)
# 2. PPV vs NPV (clinical utility, depends on prevalence)
# 3. Likelihood ratios (how much test changes probability)
# 4. Fagan nomogram (visual probability revision)
# 5. Impact of prevalence on predictive values
# 6. When to use high-sens vs high-spec tests

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 17: Quality Improvement Monitoring
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Monitor test performance over time
# Question: Has accuracy improved after training intervention?

# Baseline (Q1)
q1_result <- decisioncalculator(
  TP = 80,
  TN = 170,
  FP = 30,
  FN = 20,
  ci = TRUE
)

# After training (Q2)
q2_result <- decisioncalculator(
  TP = 88,
  TN = 180,
  FP = 20,
  FN = 12,
  ci = TRUE
)

# Follow-up (Q3)
q3_result <- decisioncalculator(
  TP = 92,
  TN = 185,
  FP = 15,
  FN = 8,
  ci = TRUE
)

# Clinical Interpretation:
# - Track sensitivity and specificity over time
# - Check if CIs overlap (significant improvement?)
# - Quality metrics: Accuracy, LR+, LR-
# - Sustainability: Has improvement persisted?

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 18: Meta-Analysis Data Entry
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Systematic review of diagnostic accuracy
# Question: Extract 2×2 data from multiple studies for meta-analysis

# Study 1: Smith et al. 2020
study1 <- decisioncalculator(
  TP = 85, TN = 180, FP = 20, FN = 15,
  ci = TRUE
)

# Study 2: Jones et al. 2021
study2 <- decisioncalculator(
  TP = 92, TN = 170, FP = 30, FN = 8,
  ci = TRUE
)

# Study 3: Lee et al. 2022
study3 <- decisioncalculator(
  TP = 78, TN = 195, FP = 5, FN = 22,
  ci = TRUE
)

# Clinical Interpretation:
# - Extract sens, spec, CIs from each study
# - Check heterogeneity: Do estimates vary widely?
# - Consider meta-analysis if studies similar
# - Use bivariate model for correlated sens/spec

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 19: Cost-Effectiveness Context
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Economic evaluation of screening program
# Question: Balance test costs with clinical outcomes

# Expensive high-accuracy test ($500)
expensive_test <- decisioncalculator(
  TP = 95,
  TN = 195,
  FP = 5,
  FN = 5,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.10,
  showSummary = TRUE
)

# Cheaper moderate-accuracy test ($50)
cheap_test <- decisioncalculator(
  TP = 85,
  TN = 180,
  FP = 20,
  FN = 15,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.10,
  showSummary = TRUE
)

# Clinical Interpretation:
# - Expensive test: Better accuracy, higher cost
# - Cheap test: Lower accuracy, lower cost
# - Consider: Disease severity, treatment availability
# - Calculate: False positive costs, false negative costs
# - Decision: Incremental cost-effectiveness ratio (ICER)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 20: Likelihood Ratio Application
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Bayesian diagnostic reasoning
# Question: How much does test result change probability?

decisioncalculator(
  TP = 85,
  TN = 180,
  FP = 20,
  FN = 15,
  ci = TRUE,
  pp = TRUE,
  pprob = 0.20,  # Pre-test probability
  fagan = TRUE,
  showSummary = TRUE,
  showGlossary = TRUE
)

# Clinical Interpretation:
# - LR+ = Sensitivity / (1-Specificity)
# - LR- = (1-Sensitivity) / Specificity
# - LR+ > 10 or LR- < 0.1 = Strong evidence
# - LR+ 5-10 or LR- 0.1-0.2 = Moderate evidence
# - LR+ 2-5 or LR- 0.2-0.5 = Weak evidence
# - LR+ = 1 or LR- = 1 = No diagnostic value
# - Use LRs to revise probability given test result

# ═══════════════════════════════════════════════════════════════════════════════
# END OF EXAMPLES
# ═══════════════════════════════════════════════════════════════════════════════
#
# Key References:
# - Altman DG, Bland JM. (1994). Diagnostic tests 1-3. BMJ series.
# - Fagan TJ. (1975). Nomogram for Bayes' theorem. NEJM.
# - Deeks JJ, Altman DG. (2004). Diagnostic tests 4: likelihood ratios. BMJ.
# - Bossuyt PM, et al. (2015). STARD 2015: updated reporting guidelines. BMJ.
# - Leeflang MM, et al. (2013). Systematic reviews of diagnostic accuracy. Ann Intern Med.
#
# For more information: ?decisioncalculator
