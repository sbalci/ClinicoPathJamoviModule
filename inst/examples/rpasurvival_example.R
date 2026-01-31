# ═══════════════════════════════════════════════════════════
# Example Usage: rpasurvival
# ═══════════════════════════════════════════════════════════
#
# Recursive Partitioning Analysis for Survival Data
# Develops risk stratification systems using CART methodology

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic RPA Analysis
# ═══════════════════════════════════════════════════════════

# Load test data
data(rpasurvival_test)

# Basic analysis with default settings
result1 <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("age", "stage", "grade", "LVI"),
  time_unit = "months",
  minbucket = 20,
  cp = 0.01,
  maxdepth = 3,
  prunetree = TRUE,
  treeplot = TRUE,
  kmplot = TRUE,
  riskgrouptable = TRUE,
  variableimportance = TRUE,
  riskgrouplabels = "auto",
  showSummary = TRUE,
  showReport = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 2: Create and Save Risk Groups
# ═══════════════════════════════════════════════════════════

# Develop risk stratification and save as new variable
result2 <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "LVI", "grade"),
  createnewvar = TRUE,
  newvarname = "rpa_risk_group",
  riskgrouplabels = "risk"  # Low, Intermediate, High
)

# The new variable "rpa_risk_group" can now be used in subsequent analyses

# ═══════════════════════════════════════════════════════════
# Example 3: Conservative Settings for External Validation
# ═══════════════════════════════════════════════════════════

# More conservative settings to avoid overfitting
result3 <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("age", "stage", "grade"),
  minbucket = 30,      # Larger minimum node size
  cp = 0.02,           # More conservative pruning
  maxdepth = 2,        # Simpler tree
  nfolds = 10,         # 10-fold cross-validation
  prunetree = TRUE,
  cptable = TRUE       # Show complexity parameter table
)

# ═══════════════════════════════════════════════════════════
# Example 4: Comprehensive Analysis with All Features
# ═══════════════════════════════════════════════════════════

# Full analysis with all available predictors
result4 <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("age", "stage", "grade", "LVI", "tumor_size",
                 "ki67", "performance_status", "treatment"),
  time_unit = "months",
  minbucket = 25,
  cp = 0.015,
  maxdepth = 4,
  nfolds = 10,
  prunetree = TRUE,
  riskgrouplabels = "auto",
  # All outputs
  treeplot = TRUE,
  kmplot = TRUE,
  kmci = TRUE,
  risktable = TRUE,
  pval = TRUE,
  riskgrouptable = TRUE,
  cptable = TRUE,
  variableimportance = TRUE,
  # Guidance
  showSummary = TRUE,
  showInterpretation = TRUE,
  showReport = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 5: Different Time Units
# ═══════════════════════════════════════════════════════════

# Time in days
data(rpasurvival_edge_days)
result_days <- rpasurvival(
  data = rpasurvival_edge_days,
  time = "time_days",
  event = "event",
  predictors = c("stage", "grade"),
  time_unit = "days"  # Correctly specify time unit
)

# Time in years
data(rpasurvival_edge_years)
result_years <- rpasurvival(
  data = rpasurvival_edge_years,
  time = "time_years",
  event = "event",
  predictors = c("stage", "grade"),
  time_unit = "years"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Different Risk Group Labeling
# ═══════════════════════════════════════════════════════════

# Stage-based labels (Stage I, II, III...)
result_stage <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "grade", "LVI"),
  riskgrouplabels = "auto"
)

# Risk-based labels (Low, Intermediate, High)
result_risk <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "grade", "LVI"),
  riskgrouplabels = "risk"
)

# Numeric labels (Group 1, 2, 3...)
result_numeric <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "grade", "LVI"),
  riskgrouplabels = "numeric"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Minimal vs Maximal Output
# ═══════════════════════════════════════════════════════════

# Minimal output (expert mode)
result_minimal <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "grade"),
  treeplot = TRUE,
  kmplot = FALSE,
  riskgrouptable = TRUE,
  variableimportance = FALSE,
  showSummary = FALSE,
  showInterpretation = FALSE,
  showReport = FALSE
)

# Maximal output (beginner mode)
result_maximal <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "grade", "LVI", "age"),
  treeplot = TRUE,
  kmplot = TRUE,
  kmci = TRUE,
  risktable = TRUE,
  pval = TRUE,
  riskgrouptable = TRUE,
  cptable = TRUE,
  variableimportance = TRUE,
  showSummary = TRUE,
  showInterpretation = TRUE,
  showReport = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 8: Using with Large Dataset
# ═══════════════════════════════════════════════════════════

# Large sample allows more complex trees
data(rpasurvival_large)

result_large <- rpasurvival(
  data = rpasurvival_large,
  time = "time",
  event = "event",
  predictors = c("age", "stage", "grade", "LVI", "PNI",
                 "tumor_size", "nodes_positive", "biomarker1"),
  minbucket = 30,
  cp = 0.01,
  maxdepth = 5,      # Deeper tree possible with large sample
  nfolds = 10,
  prunetree = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 9: Clinical Workflow - Development to Validation
# ═══════════════════════════════════════════════════════════

# Step 1: Develop risk stratification on training data
training_result <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("stage", "grade", "LVI", "age"),
  createnewvar = TRUE,
  newvarname = "rpa_stage",
  riskgrouplabels = "auto"
)

# Step 2: Apply to validation data (would use external dataset)
# validation_result <- rpasurvival(...)

# Step 3: Compare performance using groomecompare or stagemigration
# comparison <- groomecompare(...)

# ═══════════════════════════════════════════════════════════
# Tips and Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Start with conservative settings (large minbucket, higher cp)
# 2. Use cross-validation (nfolds >= 5) for reliable results
# 3. Check events-per-predictor ratio (EPV >= 10 recommended)
# 4. Always specify time_unit correctly
# 5. Use prunetree=TRUE to avoid overfitting
# 6. Validate risk groups on external dataset
# 7. Consider clinical interpretability when setting maxdepth
# 8. Use showInterpretation=TRUE for guidance if new to RPA

# ═══════════════════════════════════════════════════════════
# Interpreting Results
# ═══════════════════════════════════════════════════════════

# The function produces:
# 1. Decision tree showing splits
# 2. Risk group summary table with median OS and 5-year survival
# 3. Kaplan-Meier curves comparing risk groups
# 4. Cox regression hazard ratios
# 5. Log-rank test for group differences
# 6. Variable importance (if requested)
# 7. Plain-language summary (if showSummary=TRUE)
# 8. Copy-ready report sentence (if showReport=TRUE)

# Risk groups are ordered by prognosis:
# - Stage I / Low Risk / Group 1 = BEST survival
# - Stage N / High Risk / Group N = WORST survival

# ═══════════════════════════════════════════════════════════
# Integration with Other Functions
# ═══════════════════════════════════════════════════════════

# Use rpasurvival results with:
# - groomecompare: Compare RPA groups with standard staging
# - stagemigration: Analyze stage migration effects
# - survival: Further Cox regression on risk groups
# - tableone: Describe characteristics by risk group
