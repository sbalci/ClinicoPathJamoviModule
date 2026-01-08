# ═══════════════════════════════════════════════════════════
# Waterfall Plot Examples
# ═══════════════════════════════════════════════════════════
#
# Example usage of the waterfall function for tumor response analysis
# following RECIST v1.1 criteria

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 1: Basic Waterfall Plot
# ═══════════════════════════════════════════════════════════

# Load test data
data(waterfall_test)

# Simple waterfall plot with percentage changes
waterfall(
  data = waterfall_test,
  patientID = "patientID",
  responseVar = "best_response",
  inputType = "percentage"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 2: Waterfall with Grouping
# ═══════════════════════════════════════════════════════════

# Color bars by treatment group instead of RECIST categories
waterfall(
  data = waterfall_test,
  patientID = "patientID",
  responseVar = "best_response",
  groupVar = "treatment",
  colorBy = "group",
  colorScheme = "colorful"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 3: Spider Plot (Longitudinal Data)
# ═══════════════════════════════════════════════════════════

# Load longitudinal data
data(waterfall_spider_test)

# Show response trajectories over time
waterfall(
  data = waterfall_spider_test,
  patientID = "patientID",
  responseVar = "pct_change",
  timeVar = "time",
  showWaterfallPlot = TRUE,
  showSpiderPlot = TRUE,
  timeUnitLabel = "months"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 4: Both Plots with Treatment Comparison
# ═══════════════════════════════════════════════════════════

# Combined waterfall and spider plot colored by treatment
waterfall(
  data = waterfall_spider_test,
  patientID = "patientID",
  responseVar = "pct_change",
  timeVar = "time",
  groupVar = "treatment",
  showWaterfallPlot = TRUE,
  showSpiderPlot = TRUE,
  spiderColorBy = "group",
  colorBy = "group",
  colorScheme = "colorful"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 5: Raw Tumor Measurements
# ═══════════════════════════════════════════════════════════

# Load raw measurement data
data(waterfall_raw_test)

# Process raw tumor sizes (function calculates percentage changes)
waterfall(
  data = waterfall_raw_test,
  patientID = "patientID",
  responseVar = "tumor_size",
  timeVar = "time",
  inputType = "raw",
  groupVar = "treatment"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 6: Phase II Trial Analysis
# ═══════════════════════════════════════════════════════════

# Load Phase II trial data
data(waterfall_phase2)

# Complete clinical trial report
waterfall(
  data = waterfall_phase2,
  patientID = "patientID",
  responseVar = "best_response",
  groupVar = "cohort",
  sortBy = "response",
  showThresholds = TRUE,
  colorBy = "recist",
  colorScheme = "recist",
  generateCopyReadyReport = TRUE,
  showClinicalSignificance = TRUE,
  showConfidenceIntervals = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 7: Publication-Ready Plots
# ═══════════════════════════════════════════════════════════

# Clean plot for manuscript with colorblind-safe palette
waterfall(
  data = waterfall_test,
  patientID = "patientID",
  responseVar = "best_response",
  groupVar = "treatment",
  sortBy = "response",
  showThresholds = TRUE,
  colorScheme = "colorblind",
  barAlpha = 0.9,
  barWidth = 0.8,
  showMedian = TRUE,
  showCI = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 8: Biomarker Correlation Study
# ═══════════════════════════════════════════════════════════

# Analyze response by PD-L1 status
waterfall(
  data = waterfall_phase2,
  patientID = "patientID",
  responseVar = "best_response",
  groupVar = "pdl1_status",
  colorBy = "group",
  colorScheme = "colorful",
  sortBy = "response",
  generateCopyReadyReport = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 9: Custom Visual Options
# ═══════════════════════════════════════════════════════════

# Customize plot appearance
waterfall(
  data = waterfall_test,
  patientID = "patientID",
  responseVar = "best_response",
  sortBy = "response",
  showThresholds = TRUE,
  labelOutliers = TRUE,
  minResponseForLabel = 40,  # Label responses > 40%
  showMedian = TRUE,
  showCI = FALSE,
  barAlpha = 0.85,
  barWidth = 0.75,
  colorScheme = "simple"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 10: Guided Analysis Mode
# ═══════════════════════════════════════════════════════════

# Enable step-by-step guidance for new users
waterfall(
  data = waterfall_test,
  patientID = "patientID",
  responseVar = "best_response",
  enableGuidedMode = TRUE,
  showExplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 11: Responders vs Non-Responders
# ═══════════════════════════════════════════════════════════

# Spider plot colored by responder status
waterfall(
  data = waterfall_spider_test,
  patientID = "patientID",
  responseVar = "pct_change",
  timeVar = "time",
  showSpiderPlot = TRUE,
  spiderColorBy = "response",
  spiderColorScheme = "classic",
  timeUnitLabel = "months"
)

# ═══════════════════════════════════════════════════════════
# UNDERSTANDING THE OUTPUT
# ═══════════════════════════════════════════════════════════

# RECIST v1.1 Response Categories:
# - CR (Complete Response):  ≤ -100% (complete disappearance)
# - PR (Partial Response):   -99% to -30% (significant shrinkage)
# - SD (Stable Disease):     -29% to +19% (minimal change)
# - PD (Progressive Disease): ≥ +20% (tumor growth)

# Objective Response Rate (ORR) = (CR + PR) / Total
# Disease Control Rate (DCR) = (CR + PR + SD) / Total

# ═══════════════════════════════════════════════════════════
# DATA FORMAT REQUIREMENTS
# ═══════════════════════════════════════════════════════════

# For PERCENTAGE CHANGES format:
# - One row per patient with best response
# - Negative values = tumor shrinkage (good)
# - Positive values = tumor growth (poor)

# For RAW MEASUREMENTS format:
# - Multiple rows per patient (one per timepoint)
# - Must include time variable with baseline at time = 0
# - Function calculates percentage changes automatically

# ═══════════════════════════════════════════════════════════
# ADDITIONAL TIPS
# ═══════════════════════════════════════════════════════════

# 1. Use colorBy = "recist" for clinical interpretation
# 2. Use colorBy = "group" for treatment comparisons
# 3. Enable showConfidenceIntervals for statistical rigor
# 4. Use generateCopyReadyReport for manuscript preparation
# 5. Spider plots require time variable and longitudinal data
# 6. Sort by "response" for visual impact, by "id" for tracking
