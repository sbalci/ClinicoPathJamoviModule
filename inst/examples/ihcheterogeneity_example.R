# ═══════════════════════════════════════════════════════════
# Example Usage: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for IHC biomarker heterogeneity analysis
# in digital pathology using the ihcheterogeneity jamovi function

library(ClinicoPath)

# ───────────────────────────────────────────────────────────
# Example 1: Basic IHC Heterogeneity Analysis
# ───────────────────────────────────────────────────────────

# Load test data
data(ihcheterogeneity_test)

# Basic analysis with reference and two biopsies
basic_result <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2"
)

# ───────────────────────────────────────────────────────────
# Example 2: Ki67 Proliferation Index Heterogeneity
# ───────────────────────────────────────────────────────────

# Load Ki67 data
data(ihcheterogeneity_ki67)

# Ki67 heterogeneity assessment with typical threshold (CV ~25%)
ki67_result <- ihcheterogeneity(
  data = ihcheterogeneity_ki67,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  biopsy4 = "biopsy4",
  spatial_id = "spatial_id",
  cv_threshold = 25.0,
  correlation_threshold = 0.75,
  show_variability_plots = TRUE,
  generate_recommendations = TRUE,
  showSummary = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 3: ER H-Score Heterogeneity (0-300 scale)
# ───────────────────────────────────────────────────────────

# Load ER H-score data
data(ihcheterogeneity_er_hscore)

# ER heterogeneity with stringent threshold (CV ~18%)
er_result <- ihcheterogeneity(
  data = ihcheterogeneity_er_hscore,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  cv_threshold = 18.0,
  correlation_threshold = 0.85,
  analysis_type = "reproducibility",
  variance_components = TRUE,
  showGlossary = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 4: Spatial Compartment Comparison
# ───────────────────────────────────────────────────────────

# Load compartment data
data(ihcheterogeneity_compartments)

# Compare heterogeneity across spatial compartments
compartment_result <- ihcheterogeneity(
  data = ihcheterogeneity_compartments,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  biopsy4 = "biopsy4",
  spatial_id = "spatial_id",
  compareCompartments = TRUE,
  compartmentTests = TRUE,
  show_variability_plots = TRUE,
  showSummary = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 5: Reproducibility-Focused Analysis
# ───────────────────────────────────────────────────────────

# Focus on measurement reproducibility
reproducibility_result <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  analysis_type = "reproducibility",
  correlation_threshold = 0.90,
  show_variability_plots = TRUE,
  generate_recommendations = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 6: Sampling Bias Analysis
# ───────────────────────────────────────────────────────────

# Assess systematic bias in regional measurements
bias_result <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  biopsy4 = "biopsy4",
  analysis_type = "bias",
  sampling_strategy = "systematic",
  show_variability_plots = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 7: Variance Component Analysis
# ───────────────────────────────────────────────────────────

# Decompose variance into between- and within-region components
variance_result <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  analysis_type = "variability",
  variance_components = TRUE,
  cv_threshold = 20.0,
  power_analysis = TRUE,
  generate_recommendations = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 8: Inter-Regional Comparison (No Reference)
# ───────────────────────────────────────────────────────────

# Load dataset without reference measurement
data(ihcheterogeneity_no_reference)

# Compare regions without reference whole-section measurement
inter_regional_result <- ihcheterogeneity(
  data = ihcheterogeneity_no_reference,
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  biopsy4 = "biopsy4",
  spatial_id = "spatial_id",
  analysis_type = "comprehensive",
  variance_components = TRUE,
  show_variability_plots = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 9: High Heterogeneity Assessment
# ───────────────────────────────────────────────────────────

# Load high heterogeneity data
data(ihcheterogeneity_high_hetero)

# Assess highly heterogeneous biomarker (e.g., PD-L1, HER2)
high_hetero_result <- ihcheterogeneity(
  data = ihcheterogeneity_high_hetero,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  cv_threshold = 30.0,
  correlation_threshold = 0.70,
  variance_components = TRUE,
  power_analysis = TRUE,
  generate_recommendations = TRUE,
  showSummary = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 10: Comprehensive Quality Assessment
# ───────────────────────────────────────────────────────────

# Complete heterogeneity analysis with all features
comprehensive_result <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  biopsy4 = "biopsy4",
  spatial_id = "spatial_id",
  # Comparison options
  compareCompartments = TRUE,
  compartmentTests = TRUE,
  # Analysis options
  analysis_type = "comprehensive",
  sampling_strategy = "stratified",
  # Thresholds
  cv_threshold = 20.0,
  correlation_threshold = 0.80,
  # Display options
  show_variability_plots = TRUE,
  variance_components = TRUE,
  power_analysis = TRUE,
  generate_recommendations = TRUE,
  showSummary = TRUE,
  showGlossary = TRUE
)

# ───────────────────────────────────────────────────────────
# Clinical Interpretation Guide
# ───────────────────────────────────────────────────────────

# Coefficient of Variation (CV) Guidelines:
# - CV < 15%: Low heterogeneity (excellent consistency)
# - CV 15-25%: Moderate heterogeneity (acceptable for most IHC)
# - CV 25-35%: High heterogeneity (may affect clinical interpretation)
# - CV > 35%: Very high heterogeneity (additional sampling recommended)

# Correlation Guidelines:
# - r ≥ 0.90: Excellent agreement between biopsy and whole section
# - r 0.80-0.89: Good agreement
# - r 0.70-0.79: Moderate agreement
# - r 0.60-0.69: Fair agreement
# - r < 0.60: Poor agreement (biopsy may not represent whole section)

# ICC (Intraclass Correlation) Guidelines:
# - ICC > 0.90: Excellent reproducibility
# - ICC 0.75-0.90: Good reproducibility
# - ICC 0.50-0.75: Moderate reproducibility
# - ICC < 0.50: Poor reproducibility

# ───────────────────────────────────────────────────────────
# Tips for Pathologists and Digital Pathology
# ───────────────────────────────────────────────────────────

# 1. Ki67 Proliferation Index:
#    - Expected CV: 20-30% (due to biological heterogeneity)
#    - Recommendation: Sample multiple regions (≥3)
#    - Consider hot-spot vs. whole-section approaches
#    - Use cv_threshold = 25.0

# 2. Hormone Receptors (ER, PR):
#    - Expected CV: 15-20% (more homogeneous)
#    - Recommendation: 2-3 representative samples usually sufficient
#    - Use cv_threshold = 18.0
#    - H-score scale (0-300) may show different heterogeneity than %

# 3. HER2:
#    - Expected CV: 25-40% (highly heterogeneous)
#    - Recommendation: Sample multiple regions, include invasive front
#    - Use cv_threshold = 30.0
#    - Consider dual-probe FISH for equivocal cases

# 4. PD-L1:
#    - Expected CV: 30-50% (extremely heterogeneous)
#    - Recommendation: Multiple sampling essential
#    - Consider tumor vs. immune cell expression separately
#    - Use cv_threshold = 35.0

# 5. Digital Pathology Applications:
#    - Whole slide imaging: Reference should be whole-section mean
#    - AI/ML validation: Assess heterogeneity across different regions
#    - Tissue microarray: High CV expected (small samples)
#    - Virtual biopsy simulation: Use sampling_strategy = "random"

# ───────────────────────────────────────────────────────────
# Example Workflows by Use Case
# ───────────────────────────────────────────────────────────

# Workflow A: Biopsy Representativeness Study
# -------------------------------------------
# Goal: Determine if core biopsies adequately represent whole tumor

# Step 1: Measure reference (whole section)
# Step 2: Simulate multiple biopsies (3-5 regions)
# Step 3: Assess correlation and bias

workflow_a <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  analysis_type = "bias",
  correlation_threshold = 0.80,
  generate_recommendations = TRUE
)

# Workflow B: Multi-Region Sampling Protocol Optimization
# -------------------------------------------------------
# Goal: Determine optimal number of samples needed

# Step 1: Collect multiple regional measurements
# Step 2: Perform variance component analysis
# Step 3: Power analysis for sample size recommendation

workflow_b <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  biopsy4 = "biopsy4",
  analysis_type = "variability",
  variance_components = TRUE,
  power_analysis = TRUE,
  generate_recommendations = TRUE
)

# Workflow C: Spatial Heterogeneity Characterization
# --------------------------------------------------
# Goal: Compare heterogeneity patterns across tumor compartments

# Step 1: Identify spatial compartments (central, invasive, etc.)
# Step 2: Measure biomarker in each compartment
# Step 3: Statistical comparison of compartments

workflow_c <- ihcheterogeneity(
  data = ihcheterogeneity_compartments,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  spatial_id = "spatial_id",
  compareCompartments = TRUE,
  compartmentTests = TRUE,
  show_variability_plots = TRUE,
  showSummary = TRUE
)

# Workflow D: Quality Control for IHC Reproducibility
# ---------------------------------------------------
# Goal: Validate consistency of IHC staining protocol

# Step 1: Multiple measurements from same or similar regions
# Step 2: Assess reproducibility (ICC, CV)
# Step 3: Generate QC recommendations

workflow_d <- ihcheterogeneity(
  data = ihcheterogeneity_test,
  wholesection = "wholesection",
  biopsy1 = "biopsy1",
  biopsy2 = "biopsy2",
  biopsy3 = "biopsy3",
  analysis_type = "reproducibility",
  cv_threshold = 15.0,
  correlation_threshold = 0.90,
  variance_components = TRUE,
  generate_recommendations = TRUE
)

# ───────────────────────────────────────────────────────────
# Real-World Applications
# ───────────────────────────────────────────────────────────

# Application 1: Core Needle Biopsy Validation
# Application 2: Digital Pathology Algorithm Assessment
# Application 3: Tissue Microarray Design
# Application 4: Multi-Region Sampling Strategy
# Application 5: IHC Quality Control
# Application 6: AI/ML Training Data Heterogeneity
# Application 7: Clinical Trial Biomarker Assessment
# Application 8: Virtual Biopsy Simulation

# ───────────────────────────────────────────────────────────
# Statistical Interpretation
# ───────────────────────────────────────────────────────────

# ICC (Intraclass Correlation Coefficient):
#   - Measures agreement between regional measurements
#   - Values close to 1 indicate high consistency
#   - Values close to 0 indicate high variability

# CV (Coefficient of Variation):
#   - Standardized measure of dispersion (SD/mean * 100%)
#   - Lower values indicate more homogeneous measurements
#   - Allows comparison across different biomarkers/scales

# Spearman Correlation:
#   - Assesses monotonic relationship between biopsy and reference
#   - Robust to outliers
#   - Values ≥ 0.80 indicate good representativeness

# Bias:
#   - Systematic difference between regional and reference measurements
#   - Positive bias: regions consistently overestimate
#   - Negative bias: regions consistently underestimate

# ───────────────────────────────────────────────────────────
# Reporting Guidelines
# ───────────────────────────────────────────────────────────

# When reporting IHC heterogeneity results, include:
# 1. Number of cases and regions analyzed
# 2. Biomarker(s) assessed
# 3. Measurement scale (%, H-score, intensity)
# 4. ICC with 95% CI
# 5. Mean CV with range
# 6. Spearman correlation (if reference available)
# 7. Number/percentage of cases exceeding CV threshold
# 8. Clinical interpretation of heterogeneity impact
# 9. Recommendations for sampling strategy

# ───────────────────────────────────────────────────────────
# References and Guidelines
# ───────────────────────────────────────────────────────────

# Statistical Methods:
# - Shrout PE & Fleiss JL. Intraclass correlations: uses in assessing
#   rater reliability. Psychol Bull. 1979.
# - Landis JR & Koch GG. The measurement of observer agreement for
#   categorical data. Biometrics. 1977.

# IHC Guidelines:
# - CAP/ASCP/AMP Guidelines for IHC testing
# - ASCO/CAP Guidelines for ER, PR, HER2 testing
# - International Ki67 Working Group recommendations

# Digital Pathology:
# - FDA guidance on digital pathology validation
# - CAP Digital Pathology Checklist
