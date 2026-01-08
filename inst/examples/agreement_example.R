# ═══════════════════════════════════════════════════════════════════════════════
# agreement() - Interrater Reliability Analysis
# ═══════════════════════════════════════════════════════════════════════════════
#
# Comprehensive examples for the agreement jamovi function
# Covers categorical and continuous agreement measures
#
# Load package
library(ClinicoPath)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 1: Basic 2-Rater Pathology Agreement (Cohen's Kappa)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Two pathologists independently diagnose 200 surgical specimens
# Question: What is the inter-rater reliability?
# Expected: Cohen's kappa ~0.70 (substantial agreement)

data(agreement_pathology)

agreement(
  data = agreement_pathology,
  vars = c("Pathologist1", "Pathologist2"),
  cohensKappa = TRUE,
  ci = TRUE,
  heatmap = TRUE
)

# Clinical Interpretation:
# - Kappa 0.61-0.80 = Substantial agreement
# - Kappa 0.81-1.00 = Almost perfect agreement
# - Kappa < 0.40 = Poor agreement, consider training

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 2: Three-Rater Panel Agreement (Fleiss' Kappa)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Three pathologists independently review 150 biopsies
# Question: What is the overall panel agreement?
# Expected: Fleiss' kappa ~0.75

data(agreement_threeRater)

agreement(
  data = agreement_threeRater,
  vars = c("Rater1", "Rater2", "Rater3"),
  fleissKappa = TRUE,
  lightKappa = TRUE,  # Average pairwise kappa
  finn = TRUE,        # Alternative multi-rater measure
  ci = TRUE,
  heatmap = TRUE
)

# Clinical Interpretation:
# - Fleiss' kappa: Overall panel agreement
# - Light's kappa: Average of all pairwise agreements
# - Compare both to assess consistency across rater pairs

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 3: Ordinal Data with Weighted Kappa
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Two pathologists grade dysplasia (Grade 1-4)
# Question: Does weighted kappa better reflect disagreement severity?
# Expected: Weighted kappa higher than unweighted (adjacent disagreements common)

data(agreement_ordinal)

agreement(
  data = agreement_ordinal,
  vars = c("PathologistA", "PathologistB"),
  cohensKappa = TRUE,      # Unweighted
  wght = "squared",        # Quadratic weighted kappa
  meanSpearman = TRUE,     # Ordinal correlation
  ci = TRUE,
  heatmap = TRUE
)

# Clinical Interpretation:
# - Use weighted kappa for ordinal data (grades, stages)
# - Squared weights penalize larger disagreements more
# - Linear weights give equal penalty for each category difference
# - Unweighted kappa treats all disagreements equally

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 4: Continuous Measurement Agreement (Bland-Altman + ICC)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Two pathologists measure tumor size (mm) independently
# Question: Are measurements interchangeable? Is there systematic bias?
# Expected: ICC ~0.85, small systematic bias (+2mm)

data(agreement_continuous)

agreement(
  data = agreement_continuous,
  vars = c("PathologistA", "PathologistB"),
  icc = TRUE,
  iccType = "icc21",       # Two-way random effects, single measures
  blandAltman = TRUE,
  blandAltmanPlot = TRUE,
  linCCC = TRUE,           # Lin's Concordance Correlation Coefficient
  meanPearson = TRUE,
  profilePlot = TRUE,
  ci = TRUE
)

# Clinical Interpretation:
# - ICC > 0.90 = Excellent reliability
# - ICC 0.75-0.90 = Good reliability
# - ICC < 0.50 = Poor reliability
# - Bland-Altman: Check for systematic bias and limits of agreement
# - If bias exists, consider calibration between raters

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 5: Multi-Rater Panel with Varying Expertise
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: 5 raters (3 experts, 2 residents) review 100 cases
# Question: Which raters cluster together? Are experts more consistent?
# Expected: Experts cluster separately from residents

data(agreement_multiRater)

agreement(
  data = agreement_multiRater,
  vars = c("Expert1", "Expert2", "Expert3", "Resident1", "Resident2"),
  fleissKappa = TRUE,
  lightKappa = TRUE,
  kripp = TRUE,            # Robust to missing data
  raterClustering = TRUE,  # Identify rater groups
  raterDendrogram = TRUE,  # Visual clustering
  forestPlot = TRUE,       # Individual rater performance
  ci = TRUE
)

# Clinical Interpretation:
# - Dendrogram reveals natural rater groupings
# - Forest plot shows individual rater agreement vs reference
# - Use to identify raters needing additional training
# - Consider weighted consensus based on expertise

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 6: Binary Diagnostic Test Agreement (PSA/NSA)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Two pathologists classify samples as Positive/Negative
# Question: Do they agree on positives and negatives equally?
# Expected: PSA ~0.80, NSA ~0.85 (better agreement on negatives)

data(agreement_binary)

agreement(
  data = agreement_binary,
  vars = c("Pathologist1", "Pathologist2"),
  cohensKappa = TRUE,
  specificAgreement = TRUE,  # PSA and NSA
  gwet = TRUE,               # Robust to prevalence
  ci = TRUE,
  heatmap = TRUE
)

# Clinical Interpretation:
# - PSA (Positive Specific Agreement): Agreement among positive cases
# - NSA (Negative Specific Agreement): Agreement among negative cases
# - Unequal PSA/NSA suggests category-specific reliability issues
# - Gwet's AC1 more stable than kappa with unbalanced prevalence

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 7: Test-Retest Reliability Study
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: 3 raters review same cases twice (1 week apart)
# Question: Are raters consistent with themselves (intra-rater) and others (inter-rater)?
# Expected: High intra-rater, moderate inter-rater reliability

data(agreement_testRetest)

agreement(
  data = agreement_testRetest,
  vars = c("Rater1_T1", "Rater1_T2", "Rater2_T1", "Rater2_T2", "Rater3_T1", "Rater3_T2"),
  interIntraRater = TRUE,
  interIntraSeparator = "_",  # Split variable names at underscore
  icc = TRUE,
  iccType = "icc31",          # Appropriate for test-retest
  ci = TRUE
)

# Clinical Interpretation:
# - Intra-rater reliability should exceed inter-rater reliability
# - Low intra-rater suggests inconsistent criteria application
# - Use for quality control and training evaluation
# - Test-retest interval affects reliability (shorter = higher)

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 8: Hierarchical Agreement (Nested Raters)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: 6 pathologists from 3 hospitals review same cases
# Question: Does hospital affiliation affect agreement?
# Expected: Within-hospital agreement higher than between-hospital

data(agreement_hierarchical)

agreement(
  data = agreement_hierarchical,
  vars = c("HospitalA_Rater1", "HospitalA_Rater2", "HospitalB_Rater1",
           "HospitalB_Rater2", "HospitalC_Rater1", "HospitalC_Rater2"),
  hierarchicalKappa = TRUE,
  hierarchicalVariable = "institution",
  fleissKappa = TRUE,
  raterClustering = TRUE,
  raterDendrogram = TRUE
)

# Clinical Interpretation:
# - Hierarchical kappa accounts for nested structure
# - Dendrogram may show hospital-based clusters
# - Significant hospital effect suggests local training/protocol differences
# - Consider inter-institutional calibration sessions

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 9: Agreement with Missing Data (Krippendorff's Alpha)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: 3 raters, some unavailable for certain cases
# Question: How to handle missing ratings without deleting cases?
# Expected: Krippendorff's Alpha robust to ~10% missing

data(agreement_missing)

agreement(
  data = agreement_missing,
  vars = c("Rater1", "Rater2", "Rater3"),
  kripp = TRUE,         # Handles missing data
  fleissKappa = TRUE,   # For comparison (listwise deletion)
  ci = TRUE
)

# Clinical Interpretation:
# - Krippendorff's Alpha uses all available pairwise comparisons
# - Preferred when missing completely at random (MCAR)
# - Compare with Fleiss' kappa to assess impact of missingness
# - Report percentage of missing data in methods

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 10: Subgroup Analysis (Stratified Agreement)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Does agreement differ between biopsy and resection specimens?
# Question: Is reliability consistent across specimen types?
# Expected: May differ due to tissue quality/quantity

data(agreement_pathology)

agreement(
  data = agreement_pathology,
  vars = c("Pathologist1", "Pathologist2"),
  cohensKappa = TRUE,
  agreementBySubgroup = TRUE,
  subgroupVariable = "specimen_type",
  forestPlot = TRUE,
  ci = TRUE
)

# Clinical Interpretation:
# - Forest plot shows kappa with CI for each subgroup
# - Non-overlapping CIs suggest real differences
# - Lower agreement in biopsies expected (smaller samples)
# - Consider subgroup-specific training if large differences

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 11: Consensus Variable Creation (Majority Rule)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Create consensus diagnosis from 3 independent pathologists
# Question: What is the "gold standard" diagnosis for validation studies?
# Expected: Majority rule consensus for 2+ agreeing raters

data(agreement_threeRater)

agreement(
  data = agreement_threeRater,
  vars = c("Rater1", "Rater2", "Rater3"),
  fleissKappa = TRUE,
  consensusVar = TRUE,
  consensusRule = "majority",  # 2/3 agreement required
  loaVariable = TRUE           # Flag low agreement cases
)

# Clinical Interpretation:
# - Majority rule: Use when 2+ raters agree
# - Unanimous rule: More conservative, only complete agreement
# - Supermajority: Custom threshold (e.g., 75%)
# - Flag discordant cases for expert review or exclusion

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 12: Case Clustering (Identify Difficult Cases)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Which cases cause the most disagreement?
# Question: Are there clusters of difficult-to-diagnose cases?
# Expected: Borderline cases cluster together

data(agreement_pathology)

agreement(
  data = agreement_pathology,
  vars = c("Pathologist1", "Pathologist2"),
  cohensKappa = TRUE,
  caseClustering = TRUE,
  caseDendrogram = TRUE,
  loaVariable = TRUE  # Create categorical agreement variable
)

# Clinical Interpretation:
# - Case dendrogram reveals agreement patterns
# - Low agreement cases may need expert review
# - Use to identify challenging diagnostic categories
# - Inform educational case selection for training

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 13: Perfect Agreement Edge Case
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Training dataset with known diagnoses
# Question: Can function handle perfect agreement?
# Expected: Kappa = 1.0, no disagreements

data(agreement_perfect)

agreement(
  data = agreement_perfect,
  vars = c("Rater1", "Rater2"),
  cohensKappa = TRUE,
  ci = TRUE,
  heatmap = TRUE
)

# Clinical Interpretation:
# - Perfect agreement (kappa = 1.0) rare in practice
# - Common in artificial datasets or training sets
# - Check for rater collusion if unexpected in real data
# - May indicate overly simple/obvious cases

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 14: Poor Agreement (Quality Control)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Quality audit reveals unexpectedly low agreement
# Question: How poor is the agreement, and what's the next step?
# Expected: Kappa ~0.2 (poor), triggers intervention

data(agreement_poor)

agreement(
  data = agreement_poor,
  vars = c("Rater1", "Rater2"),
  cohensKappa = TRUE,
  gwet = TRUE,
  ci = TRUE,
  heatmap = TRUE
)

# Clinical Interpretation:
# - Kappa < 0.40 = Poor, warrants investigation
# - Check diagnostic criteria clarity and training adequacy
# - Consider: Different interpretations, outdated criteria, skill gaps
# - Intervention: Training, consensus meetings, written guidelines

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 15: Highly Unbalanced Categories (Prevalence Effect)
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Rare disease screening (5% prevalence)
# Question: Does kappa underestimate agreement due to low prevalence?
# Expected: Gwet's AC1 higher than Cohen's kappa

# Create unbalanced data
unbalanced_data <- data.frame(
  case_id = 1:200,
  Screener1 = factor(c(rep("Negative", 190), rep("Positive", 10))),
  Screener2 = factor(c(rep("Negative", 188), rep("Positive", 12)))
)

agreement(
  data = unbalanced_data,
  vars = c("Screener1", "Screener2"),
  cohensKappa = TRUE,
  gwet = TRUE,           # More stable with unbalanced data
  specificAgreement = TRUE,
  ci = TRUE
)

# Clinical Interpretation:
# - Kappa paradox: Low kappa despite high observed agreement
# - Gwet's AC1/AC2 less affected by prevalence
# - Also report observed agreement percentage
# - Consider PSA/NSA for category-specific reliability

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 16: ICC Model Selection for Continuous Data
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Comparing different ICC models for measurement reliability
# Question: Which ICC model is most appropriate for our design?
# Expected: Model choice depends on whether raters are fixed or random

data(agreement_continuous)

# ICC(2,1): Two-way random effects, single measures
# Use when: Raters are random sample, generalizing to other raters
agreement(
  data = agreement_continuous,
  vars = c("PathologistA", "PathologistB"),
  icc = TRUE,
  iccType = "icc21",
  ci = TRUE
)

# ICC(3,1): Two-way mixed effects, single measures
# Use when: Same raters will always be used (fixed effects)
agreement(
  data = agreement_continuous,
  vars = c("PathologistA", "PathologistB"),
  icc = TRUE,
  iccType = "icc31",
  ci = TRUE
)

# Clinical Interpretation:
# - ICC(1,1): One-way random - each subject rated by different raters
# - ICC(2,1): Two-way random - generalizing to population of raters
# - ICC(3,1): Two-way mixed - specific raters of interest
# - ICC(k) models: Average measures across k raters
# - Choose based on study design and inference goals

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 17: Bland-Altman Analysis for Method Comparison
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Compare digital vs manual tumor measurement methods
# Question: Are methods interchangeable? Is there systematic bias?
# Expected: Small bias, narrow limits of agreement

data(agreement_continuous)

agreement(
  data = agreement_continuous,
  vars = c("PathologistA", "PathologistB"),
  blandAltman = TRUE,
  blandAltmanPlot = TRUE,
  linCCC = TRUE,
  tdi = TRUE,
  ci = TRUE
)

# Clinical Interpretation:
# - Bland-Altman plot: Difference vs. mean
# - Systematic bias: Mean difference ≠ 0
# - Limits of agreement: Mean ± 1.96*SD (95% of differences)
# - Proportional bias: Difference increases with magnitude
# - TDI: Total deviation index (agreement boundary)
# - CCC: Combines precision and accuracy

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 18: Complete Publication-Ready Analysis
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Final analysis for manuscript reporting
# Question: Comprehensive inter-rater reliability for publication
# Expected: All relevant measures, visualizations, and confidence intervals

data(agreement_pathology)

agreement(
  data = agreement_pathology,
  vars = c("Pathologist1", "Pathologist2"),
  cohensKappa = TRUE,
  gwet = TRUE,
  specificAgreement = TRUE,
  ci = TRUE,
  ciWidth = 95,
  bootstrap = TRUE,
  nboot = 1000,
  heatmap = TRUE,
  caseClustering = TRUE,
  agreementBySubgroup = TRUE,
  subgroupVariable = "specimen_type",
  forestPlot = TRUE
)

# Clinical Interpretation:
# - Report kappa with 95% CI
# - Include Gwet's AC1 for robustness
# - PSA/NSA for category-specific agreement
# - Bootstrap CI for small samples
# - Subgroup analysis to explore heterogeneity
# - Visualizations for clear communication

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 19: Comparison of Weighting Schemes
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Ordinal tumor grade (1-4), adjacent disagreements common
# Question: Does weighting scheme affect kappa substantially?
# Expected: Weighted kappa > unweighted (adjacent disagreements less penalized)

data(agreement_ordinal)

# Unweighted (treats all disagreements equally)
agreement(
  data = agreement_ordinal,
  vars = c("PathologistA", "PathologistB"),
  cohensKappa = TRUE,
  wght = "unweighted",
  ci = TRUE
)

# Linear weights (each category difference penalized equally)
agreement(
  data = agreement_ordinal,
  vars = c("PathologistA", "PathologistB"),
  cohensKappa = TRUE,
  wght = "linear",
  ci = TRUE
)

# Quadratic weights (larger disagreements penalized more)
agreement(
  data = agreement_ordinal,
  vars = c("PathologistA", "PathologistB"),
  cohensKappa = TRUE,
  wght = "squared",
  ci = TRUE
)

# Clinical Interpretation:
# - Unweighted: All disagreements treated equally
# - Linear: Penalty proportional to distance (1→2 = 2→3)
# - Quadratic: Penalty increases with distance (1→4 > 1→2)
# - Report weighted kappa for ordinal data (preferred)
# - Specify weighting scheme in methods section

# ═══════════════════════════════════════════════════════════════════════════════
# EXAMPLE 20: Small Sample Size Handling
# ═══════════════════════════════════════════════════════════════════════════════
#
# Clinical Context: Pilot study with limited cases
# Question: Is reliability adequate despite small sample?
# Expected: Wide confidence intervals, use bootstrap

data(agreement_small)

agreement(
  data = agreement_small,
  vars = c("Rater1", "Rater2"),
  cohensKappa = TRUE,
  ci = TRUE,
  bootstrap = TRUE,
  nboot = 5000,  # More bootstraps for small samples
  ciWidth = 95
)

# Clinical Interpretation:
# - Small samples (n < 50): Wide confidence intervals
# - Bootstrap CI more reliable than asymptotic CI
# - Report CI width to show precision
# - Consider larger sample for definitive conclusions
# - Pilot data useful for power analysis of main study

# ═══════════════════════════════════════════════════════════════════════════════
# END OF EXAMPLES
# ═══════════════════════════════════════════════════════════════════════════════
#
# Key References:
# - Cohen J. (1960). A coefficient of agreement for nominal scales.
# - Fleiss JL. (1971). Measuring nominal scale agreement among many raters.
# - Landis JR, Koch GG. (1977). The measurement of observer agreement.
# - Bland JM, Altman DG. (1986). Statistical methods for assessing agreement.
# - Gwet KL. (2008). Computing inter-rater reliability and its variance.
# - Krippendorff K. (2011). Computing Krippendorff's Alpha-Reliability.
#
# For more information: ?agreement
