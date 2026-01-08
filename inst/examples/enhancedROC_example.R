# ═══════════════════════════════════════════════════════════
# Example Usage: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive clinical applications of the
# enhancedROC function for ROC curve analysis and diagnostic test evaluation.
#
# The enhancedROC function provides:
# - Single and comparative ROC analysis
# - Youden Index optimization for optimal cutoffs
# - Sensitivity/specificity threshold-based cutoffs
# - Bootstrap confidence intervals
# - Partial AUC analysis
# - Clinical metrics (PPV, NPV, LR+, LR-)
# - Calibration assessment
# - Multi-class ROC analysis
# - Class imbalance detection

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Single Biomarker ROC Analysis
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Evaluate a novel biomarker for disease diagnosis

data(enhancedroc_biomarker, package = "ClinicoPath")

basic_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1"
)

# Interpretation:
# - AUC represents overall discriminative ability (0.5 = no discrimination, 1.0 = perfect)
# - AUC >0.7 = acceptable, >0.8 = excellent, >0.9 = outstanding
# - 95% CI for AUC indicates precision of estimate

# ═══════════════════════════════════════════════════════════
# Example 2: ROC Analysis with Youden Index Optimization
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Find optimal cutoff that balances sensitivity and specificity

youden_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1",
  youdenOptimization = TRUE,
  optimalCutoffs = TRUE,
  rocCurve = TRUE,
  showCutoffPoints = TRUE,
  diagnosticMetrics = TRUE
)

# Interpretation:
# - Youden Index = Sensitivity + Specificity - 1 (maximizes sum)
# - Optimal cutoff provides best balance for general use
# - May not be optimal for specific clinical contexts (screening vs confirmation)

# ═══════════════════================================================================

# Example 3: Comparative ROC Analysis of Multiple Biomarkers
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare established vs novel biomarkers

data(enhancedroc_comparative, package = "ClinicoPath")

comparative_roc <- enhancedROC(
  data = enhancedroc_comparative,
  outcome = "cancer_status",
  positiveClass = "Cancer",
  predictors = c("established_marker", "novel_marker1", "novel_marker2", "genetic_risk_score"),
  analysisType = "comparative",
  pairwiseComparisons = TRUE,
  comparisonMethod = "delong",
  rocCurve = TRUE,
  aucTable = TRUE,
  diagnosticMetrics = TRUE,
  plotTheme = "clinical"
)

# Interpretation:
# - Pairwise comparisons test if AUC differences are statistically significant
# - DeLong test is standard for comparing correlated ROC curves (same patients)
# - Consider clinical relevance: is AUC difference meaningful in practice?
# - Novel biomarkers should show significant improvement to justify adoption

# ═══════════════════════════════════════════════════════════
# Example 4: Screening Context - High Sensitivity Priority
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Population screening for early disease detection
# Goal: Minimize false negatives (don't miss disease)

data(enhancedroc_screening, package = "ClinicoPath")

screening_roc <- enhancedROC(
  data = enhancedroc_screening,
  outcome = "screening_indication",
  positiveClass = "Screen Positive",
  predictors = c("sensitive_marker", "imaging_marker", "panel_score"),
  analysisType = "comparative",
  clinicalContext = "screening",
  sensitivityThreshold = 0.95,  # Require ≥95% sensitivity
  youdenOptimization = TRUE,
  rocCurve = TRUE,
  cutoffTable = TRUE,
  diagnosticMetrics = TRUE,
  clinicalMetrics = TRUE,
  useObservedPrevalence = TRUE
)

# Interpretation:
# - Screening requires high sensitivity (few false negatives)
# - Lower specificity acceptable (false positives can be ruled out later)
# - Sensitivity threshold finds cutoff meeting minimum sensitivity requirement
# - Consider costs of false positives (unnecessary follow-up)

# ═══════════════════════════════════════════════════════════
# Example 5: Confirmatory Testing - High Specificity Priority
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Confirmatory diagnosis after positive screening
# Goal: Minimize false positives (avoid overdiagnosis)

data(enhancedroc_confirmatory, package = "ClinicoPath")

confirmatory_roc <- enhancedROC(
  data = enhancedroc_confirmatory,
  outcome = "confirmed_diagnosis",
  positiveClass = "Confirmed",
  predictors = c("specific_marker", "pathology_score", "molecular_signature"),
  analysisType = "comparative",
  clinicalContext = "diagnosis",
  specificityThreshold = 0.95,  # Require ≥95% specificity
  youdenOptimization = TRUE,
  rocCurve = TRUE,
  cutoffTable = TRUE,
  diagnosticMetrics = TRUE,
  clinicalMetrics = TRUE,
  prevalence = 0.45  # Enriched population (post-screening)
)

# Interpretation:
# - Confirmatory tests require high specificity (few false positives)
# - Lower sensitivity acceptable (true positives already identified by screening)
# - Specificity threshold finds cutoff meeting minimum specificity requirement
# - Prevalence affects PPV/NPV - higher in enriched (post-screening) populations

# ═══════════════════════════════════════════════════════════
# Example 6: Custom Cutoff Evaluation
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Evaluate performance at clinically relevant cutoffs
# (e.g., lab reference ranges, published guidelines)

custom_cutoff_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1",
  customCutoffs = "10, 15, 18, 20, 25, 30",  # Clinical cutoffs of interest
  cutoffTable = TRUE,
  diagnosticMetrics = TRUE,
  clinicalMetrics = TRUE,
  prevalence = 0.15,  # Expected disease prevalence
  rocCurve = TRUE,
  showCutoffPoints = TRUE
)

# Interpretation:
# - Custom cutoffs allow evaluation of established thresholds
# - Compare with Youden-optimal to see if clinical cutoffs are appropriate
# - Sensitivity and specificity vary inversely - cannot optimize both
# - PPV and NPV depend on prevalence (change custom prevalence to see effect)

# ═══════════════════════════════════════════════════════════
# Example 7: ROC Analysis with Bootstrap Confidence Intervals
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Robust inference with bootstrap methods

bootstrap_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = c("biomarker1", "biomarker2"),
  useBootstrap = TRUE,
  bootstrapSamples = 1000,
  bootstrapMethod = "bca",  # Bias-corrected and accelerated (most accurate)
  bootstrapCutoffCI = TRUE,
  rocCurve = TRUE,
  showConfidenceBands = TRUE,
  aucTable = TRUE
)

# Interpretation:
# - Bootstrap provides robust confidence intervals without assumptions
# - BCa method adjusts for bias and skewness (preferred when sample size allows)
# - Confidence bands show uncertainty around ROC curve
# - Wider bands indicate less precision (smaller sample size, lower AUC)

# ═══════════════════════════════════════════════════════════
# Example 8: Class Imbalance Detection and Handling
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Rare disease with very low prevalence (5%)

data(enhancedroc_imbalanced, package = "ClinicoPath")

imbalanced_roc <- enhancedROC(
  data = enhancedroc_imbalanced,
  outcome = "rare_disease",
  positiveClass = "Positive",
  predictors = c("screening_marker", "confirmatory_marker", "combined_risk"),
  detectImbalance = TRUE,
  imbalanceThreshold = 3.0,  # Flag if ratio >3:1 or <1:3
  showImbalanceWarning = TRUE,
  stratifiedBootstrap = TRUE,  # Maintain class proportions in bootstrap
  useBootstrap = TRUE,
  bootstrapSamples = 1000,
  rocCurve = TRUE,
  clinicalMetrics = TRUE,
  useObservedPrevalence = TRUE  # Use actual 5% prevalence for PPV/NPV
)

# Interpretation:
# - Imbalanced data (rare disease) affects interpretation
# - ROC/AUC less sensitive to imbalance than other metrics
# - PPV will be low even with high specificity (many false positives)
# - Stratified bootstrap maintains class proportions for valid inference
# - Consider Precision-Recall curves for severely imbalanced data

# ═══════════════════════════════════════════════════════════
# Example 9: Partial AUC for Specific Sensitivity/Specificity Ranges
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Focus on high-specificity region for confirmatory test

partial_auc_roc <- enhancedROC(
  data = enhancedroc_confirmatory,
  outcome = "confirmed_diagnosis",
  positiveClass = "Confirmed",
  predictors = c("specific_marker", "pathology_score"),
  analysisType = "comparative",
  partialAuc = TRUE,
  partialAucType = "specificity",
  partialRange = "0.90,1.00",  # Focus on high specificity (90-100%)
  bootstrapPartialAUC = TRUE,
  useBootstrap = TRUE,
  bootstrapSamples = 500,
  rocCurve = TRUE
)

# Interpretation:
# - Partial AUC evaluates performance in clinically relevant range
# - High-specificity pAUC: relevant for confirmatory testing
# - High-sensitivity pAUC: relevant for screening applications
# - pAUC values are not directly comparable to full AUC
# - Standardized pAUC (rescaled to 0-1) allows comparison

# ═══════════════════════════════════════════════════════════
# Example 10: Clinical Impact Analysis
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Assess real-world clinical utility

clinical_impact_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1",
  youdenOptimization = TRUE,
  clinicalImpact = TRUE,
  nntCalculation = TRUE,
  clinicalUtilityCurve = TRUE,
  decisionImpactTable = TRUE,
  clinicalMetrics = TRUE,
  prevalence = 0.20,
  rocCurve = TRUE
)

# Interpretation:
# - Number Needed to Test (NNT): patients to test to find one true positive
# - Number Needed to Diagnose (NND): positive tests needed for one true positive
# - Clinical utility curve: net benefit of testing at different thresholds
# - Decision impact table: clinical consequences at various cutoffs
# - Consider test costs, treatment benefits, and harm of misclassification

# ═══════════════════════════════════════════════════════════
# Example 11: Calibration Assessment
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Validate clinical prediction model calibration

data(enhancedroc_calibration, package = "ClinicoPath")

calibration_roc <- enhancedROC(
  data = enhancedroc_calibration,
  outcome = "outcome",
  positiveClass = "Event",
  predictors = c("predictor1", "predictor2", "risk_score"),
  rocCurve = TRUE,
  aucTable = TRUE,
  calibrationAnalysis = TRUE,
  calibrationPlot = TRUE,
  hosmerLemeshow = TRUE,
  hlGroups = 10,
  brierScore = TRUE,
  calibrationMetrics = TRUE
)

# Interpretation:
# - Calibration: agreement between predicted and observed probabilities
# - Hosmer-Lemeshow test: p<0.05 suggests miscalibration
# - Brier score: 0 = perfect, 0.25 = uninformative (for 50% prevalence)
# - Calibration slope: should be ~1.0 (>1 = overfitting, <1 = underfitting)
# - Calibration-in-the-large: average predicted vs observed (should be ~0)
# - Good discrimination (high AUC) doesn't guarantee good calibration

# ═══════════════════════════════════════════════════════════
# Example 12: Multi-Class ROC Analysis
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Disease severity classification (4 levels)

data(enhancedroc_multiclass, package = "ClinicoPath")

multiclass_roc <- enhancedROC(
  data = enhancedroc_multiclass,
  outcome = "disease_severity",
  predictors = c("biomarker_A", "biomarker_B", "imaging_severity_score"),
  multiClassROC = TRUE,
  multiClassStrategy = "ovr",  # One-vs-Rest
  multiClassAveraging = "macro",  # Unweighted average
  rocCurve = TRUE,
  aucTable = TRUE
)

# Interpretation:
# - One-vs-Rest (OVR): each class vs all others (4 ROC curves for 4 classes)
# - One-vs-One (OVO): all pairwise comparisons (6 ROC curves for 4 classes)
# - Macro-average AUC: unweighted mean across classes
# - Weighted-average AUC: weighted by class prevalence
# - Multi-class AUC not directly comparable to binary AUC

# ═══════════════════════════════════════════════════════════
# Example 13: Internal Validation with Bootstrap
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Assess optimism in model performance

internal_validation_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = c("biomarker1", "biomarker2", "clinical_risk_score"),
  internalValidation = TRUE,
  validationMethod = "bootstrap",
  optimismCorrection = TRUE,
  useBootstrap = TRUE,
  bootstrapSamples = 500,
  rocCurve = TRUE,
  aucTable = TRUE
)

# Interpretation:
# - Apparent performance: optimistic (overfits to this sample)
# - Optimism-corrected performance: more realistic estimate
# - Bootstrap validation: samples with replacement, tests on excluded cases
# - Optimism = Apparent - Validated (should be subtracted from apparent AUC)
# - Important for prediction models to avoid overly optimistic claims

# ═══════════════════════════════════════════════════════════
# Example 14: Complete Publication-Ready Analysis
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Comprehensive biomarker validation for publication

publication_roc <- enhancedROC(
  data = enhancedroc_comparative,
  outcome = "cancer_status",
  positiveClass = "Cancer",
  predictors = c("established_marker", "novel_marker1", "genetic_risk_score"),
  analysisType = "comprehensive",

  # Optimal cutoff determination
  youdenOptimization = TRUE,
  customCutoffs = "50, 100, 150, 200",
  sensitivityThreshold = 0.80,
  specificityThreshold = 0.80,

  # Statistical inference
  confidenceLevel = 95,
  useBootstrap = TRUE,
  bootstrapSamples = 2000,
  bootstrapMethod = "bca",
  bootstrapCutoffCI = TRUE,

  # Comparative analysis
  pairwiseComparisons = TRUE,
  comparisonMethod = "delong",
  showMetricsDiff = TRUE,
  statisticalComparison = TRUE,

  # Outputs
  rocCurve = TRUE,
  aucTable = TRUE,
  cutoffTable = TRUE,
  optimalCutoffs = TRUE,
  diagnosticMetrics = TRUE,
  clinicalMetrics = TRUE,

  # Clinical context
  useObservedPrevalence = TRUE,
  clinicalContext = "diagnosis",

  # Visualization
  plotTheme = "clinical",
  showCutoffPoints = TRUE,
  showConfidenceBands = TRUE,
  plotWidth = 800,
  plotHeight = 800,

  # Comprehensive reporting
  comprehensive_output = TRUE,
  clinical_interpretation = TRUE
)

# Reporting guidelines for publication:
# 1. Report AUC with 95% CI
# 2. Show ROC curve with confidence bands
# 3. Report sensitivity, specificity, PPV, NPV at optimal cutoff
# 4. State prevalence used for PPV/NPV calculation
# 5. For comparative studies: report pairwise comparisons with p-values
# 6. Describe cutoff selection method (Youden, clinical threshold, etc.)
# 7. Discuss clinical implications of false positives and false negatives
# 8. Consider STARD guidelines for diagnostic accuracy studies

# ═══════════════════════════════════════════════════════════
# Example 15: Sensitivity Analysis - Varying Prevalence
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Show how PPV/NPV change with disease prevalence

# Low prevalence (screening population: 5%)
low_prev_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1",
  youdenOptimization = TRUE,
  clinicalMetrics = TRUE,
  prevalence = 0.05
)

# Moderate prevalence (primary care: 20%)
mod_prev_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1",
  youdenOptimization = TRUE,
  clinicalMetrics = TRUE,
  prevalence = 0.20
)

# High prevalence (specialty clinic: 50%)
high_prev_roc <- enhancedROC(
  data = enhancedroc_biomarker,
  outcome = "disease_status",
  positiveClass = "Disease",
  predictors = "biomarker1",
  youdenOptimization = TRUE,
  clinicalMetrics = TRUE,
  prevalence = 0.50
)

# Interpretation:
# - Sensitivity and specificity are INVARIANT to prevalence (test properties)
# - PPV and NPV vary with prevalence (depend on pre-test probability)
# - Low prevalence → low PPV (many false positives relative to true positives)
# - High prevalence → high PPV (fewer false positives relative to true positives)
# - Always report or consider prevalence when interpreting PPV/NPV
# - Use observed prevalence for validation, population prevalence for deployment

# ═══════════════════════════════════════════════════════════
# Additional Notes and Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Sample Size Considerations:
#    - Minimum 10 events per predictor variable (rule of thumb)
#    - For 95% CI width of ±0.05 around AUC: need ~200 events
#    - Rare outcomes require larger samples for stable estimates

# 2. Direction Specification:
#    - Auto-detection usually works well
#    - Specify direction if known: "higher" or "lower"
#    - Check that direction makes clinical sense

# 3. Cutoff Selection Strategy:
#    - Youden Index: balanced sensitivity/specificity (general use)
#    - Sensitivity threshold: screening applications
#    - Specificity threshold: confirmatory applications
#    - Custom cutoffs: evaluate established thresholds
#    - Clinical: consider costs of false positives vs false negatives

# 4. Bootstrap Recommendations:
#    - Use ≥1000 samples for publication (2000 for BCa method)
#    - BCa method preferred when sample size allows (more accurate)
#    - Stratified bootstrap for imbalanced data
#    - Bootstrap for cutoff CI when validating optimal thresholds

# 5. Comparative ROC Analysis:
#    - DeLong test: standard for paired data (same subjects)
#    - Bootstrap: alternative when assumptions violated
#    - Consider whether AUC difference is clinically meaningful
#    - Report both statistical and clinical significance

# 6. Partial AUC:
#    - Use when only part of ROC curve is clinically relevant
#    - Specify range based on clinical requirements
#    - Report both full and partial AUC for completeness
#    - Standardized pAUC allows comparison across studies

# 7. Clinical Interpretation:
#    - AUC alone doesn't determine clinical utility
#    - Consider prevalence, costs, and consequences
#    - High AUC doesn't mean good calibration
#    - Validate in external populations when possible

# 8. Reporting Standards:
#    - STARD guidelines for diagnostic accuracy studies
#    - TRIPOD for prediction model studies
#    - Report methodology clearly (cutoff selection, CI method)
#    - Include patient flow diagram and baseline characteristics
