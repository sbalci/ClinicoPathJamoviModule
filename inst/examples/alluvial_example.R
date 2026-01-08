# ═══════════════════════════════════════════════════════════
# Example Usage: alluvial
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating the alluvial function
# for visualizing categorical data flows and patient pathways
#
# Generated: 2026-01-04

library(ClinicoPath)

# Load test data
data(alluvial_test, package = "ClinicoPath")

# View the structure of the test data
str(alluvial_test)
summary(alluvial_test)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Alluvial Diagram - Disease Progression
# ═══════════════════════════════════════════════════════════

# Visualize tumor grade → stage → treatment → response pathway
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment", "response")
)

# ═══════════════════════════════════════════════════════════
# Example 2: Diagnostic Pathway
# ═══════════════════════════════════════════════════════════

# Show symptom → test → result flow
alluvial(
  data = alluvial_test,
  vars = c("presenting_symptom", "diagnostic_test", "test_result")
)

# ═══════════════════════════════════════════════════════════
# Example 3: With Marginal Histograms
# ═══════════════════════════════════════════════════════════

# Add marginal plots to see individual variable distributions
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  marg = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 4: Custom Fill Strategies
# ═══════════════════════════════════════════════════════════

# Fill by first variable (default)
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment"),
  fill = "first_variable"
)

# Fill by last variable
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment"),
  fill = "last_variable"
)

# Fill by all flows (unique color per flow)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  fill = "all_flows"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Color Palettes
# ═══════════════════════════════════════════════════════════

# Viridis palette (good for colorblind accessibility)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  colorPalette = "viridis"
)

# Plasma palette
alluvial(
  data = alluvial_test,
  vars = c("presenting_symptom", "diagnostic_test", "test_result"),
  colorPalette = "plasma"
)

# Pastel colors (softer appearance)
alluvial(
  data = alluvial_test,
  vars = c("age_group", "sex", "tumor_site"),
  colorPalette = "pastel1"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Show Node Counts
# ═══════════════════════════════════════════════════════════

# Display count values on nodes
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  showCounts = TRUE,
  colorPalette = "viridis"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Plot Orientation
# ═══════════════════════════════════════════════════════════

# Vertical (default - flows top to bottom)
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "treatment", "response"),
  orient = "vert"
)

# Horizontal (flows left to right)
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "treatment", "response"),
  orient = "horr"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Custom Titles and Subtitles
# ═══════════════════════════════════════════════════════════

# Custom title
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  usetitle = TRUE,
  mytitle = "Treatment Response Pathway in Oncology Patients",
  colorPalette = "viridis"
)

# Title with subtitle
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  usetitle = TRUE,
  mytitle = "Treatment Response Analysis",
  plotSubtitle = "N=200 patients, Follow-up 2020-2024",
  colorPalette = "plasma"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Theme Styles
# ═══════════════════════════════════════════════════════════

# Minimal theme (clean, simple)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  themeStyle = "minimal",
  colorPalette = "viridis"
)

# Black & white theme (publication-ready)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  themeStyle = "bw"
)

# Classic theme
alluvial(
  data = alluvial_test,
  vars = c("presenting_symptom", "diagnostic_test", "test_result"),
  themeStyle = "classic"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Maximum Variables Control
# ═══════════════════════════════════════════════════════════

# Full progression pathway (5 variables)
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment", "response", "followup_status"),
  maxvars = 5,  # Show all 5 variables
  colorPalette = "viridis"
)

# Limit to first 3 variables even if more selected
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment", "response", "followup_status"),
  maxvars = 3,  # Only show first 3
  colorPalette = "plasma"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Weighted Alluvial Diagrams (ggalluvial engine)
# ═══════════════════════════════════════════════════════════

# Use frequency weights to show aggregated data
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  weight = "frequency",
  engine = "ggalluvial",
  fillGgalluvial = "initial_stage"
)

# Cost-weighted diagram (thicker flows = higher costs)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  weight = "treatment_cost",
  engine = "ggalluvial",
  fillGgalluvial = "treatment"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Sankey Diagram Style
# ═══════════════════════════════════════════════════════════

# Sankey-style diagram (narrow nodes, S-curves)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  sankeyStyle = TRUE,
  fillGgalluvial = "initial_stage",
  colorPalette = "viridis"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Different Curve Types (ggalluvial engine)
# ═══════════════════════════════════════════════════════════

# Cubic curves (smooth, default)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  curveType = "cubic",
  fillGgalluvial = "initial_stage"
)

# Sigmoid curves (S-shaped)
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  curveType = "sigmoid",
  fillGgalluvial = "response"
)

# Linear flows (straight lines)
alluvial(
  data = alluvial_test,
  vars = c("presenting_symptom", "diagnostic_test", "test_result"),
  engine = "ggalluvial",
  curveType = "linear",
  fillGgalluvial = "test_result"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Node Labels (ggalluvial engine)
# ═══════════════════════════════════════════════════════════

# Show category labels on nodes
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  labelNodes = TRUE,
  fillGgalluvial = "initial_stage"
)

# Labels + counts
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  labelNodes = TRUE,
  showCounts = TRUE,
  fillGgalluvial = "treatment"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Enhanced Gradients
# ═══════════════════════════════════════════════════════════

# Sophisticated color gradients for flows
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment", "response"),
  enhancedGradients = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 16: Condensation Plots
# ═══════════════════════════════════════════════════════════

# Focus on treatment variable relationships
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response", "followup_status"),
  condensationvar = "treatment"
)

# Focus on response variable
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response", "followup_status"),
  condensationvar = "response"
)

# ═══════════════════════════════════════════════════════════
# Example 17: Clinical Use Cases
# ═══════════════════════════════════════════════════════════

cat("\n=== Clinical Use Case 1: Complete Treatment Pathway ===\n")
# From diagnosis through follow-up
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment", "response", "followup_status"),
  colorPalette = "viridis",
  usetitle = TRUE,
  mytitle = "Complete Treatment Pathway",
  plotSubtitle = "Grade → Stage → Treatment → Response → Follow-up"
)

cat("\n=== Clinical Use Case 2: Diagnostic Workflow ===\n")
# Patient journey from symptom to diagnosis
alluvial(
  data = alluvial_test,
  vars = c("presenting_symptom", "diagnostic_test", "test_result"),
  colorPalette = "plasma",
  marg = TRUE
)

cat("\n=== Clinical Use Case 3: Risk Stratification ===\n")
# Comorbidity → Performance → Risk → Treatment pathway
alluvial(
  data = alluvial_test,
  vars = c("comorbidity", "performance_status", "risk_group", "treatment"),
  colorPalette = "set3",
  usetitle = TRUE,
  mytitle = "Risk-Stratified Treatment Allocation"
)

cat("\n=== Clinical Use Case 4: Healthcare Utilization ===\n")
# Patient flow through healthcare system
alluvial(
  data = alluvial_test,
  vars = c("encounter_type", "care_setting"),
  colorPalette = "dark2"
)

cat("\n=== Clinical Use Case 5: Demographics to Outcomes ===\n")
# Age/sex → tumor site → treatment → response
alluvial(
  data = alluvial_test,
  vars = c("age_group", "sex", "tumor_site", "treatment", "response"),
  maxvars = 5,
  colorPalette = "viridis"
)

# ═══════════════════════════════════════════════════════════
# Example 18: Comparing Engines
# ═══════════════════════════════════════════════════════════

cat("\n=== Comparison: Easy Alluvial vs GG Alluvial ===\n")

# Easy Alluvial (automatic, simpler)
cat("\nEasy Alluvial Engine (automatic):\n")
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "easyalluvial",
  fill = "first_variable",
  colorPalette = "viridis"
)

# GG Alluvial (manual control, more options)
cat("\nGG Alluvial Engine (manual control):\n")
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  fillGgalluvial = "initial_stage",
  curveType = "sigmoid",
  labelNodes = TRUE,
  showCounts = TRUE,
  colorPalette = "viridis"
)

# ═══════════════════════════════════════════════════════════
# Example 19: Handling Missing Data
# ═══════════════════════════════════════════════════════════

# Default: Missing data included (shown as NA category)
cat("\nMissing data handling (default - NAs included):\n")
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "response", "followup_status"),  # followup_status has ~8% missing
  excl = FALSE
)

# Exclude missing data
cat("\nMissing data excluded:\n")
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "response", "followup_status"),
  excl = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 20: Publication-Ready Figures
# ═══════════════════════════════════════════════════════════

# High-quality figure for manuscript
alluvial(
  data = alluvial_test,
  vars = c("initial_stage", "treatment", "response"),
  usetitle = TRUE,
  mytitle = "Treatment Response by Initial Disease Stage",
  plotSubtitle = "Oncology cohort (N=200)",
  themeStyle = "bw",
  colorPalette = "set3",
  orient = "vert"
)

# Sankey diagram for presentations
alluvial(
  data = alluvial_test,
  vars = c("initial_grade", "initial_stage", "treatment", "response"),
  engine = "ggalluvial",
  sankeyStyle = TRUE,
  fillGgalluvial = "initial_stage",
  labelNodes = FALSE,
  colorPalette = "viridis",
  usetitle = TRUE,
  mytitle = "Disease Progression and Treatment Response",
  themeStyle = "minimal"
)

# ═══════════════════════════════════════════════════════════
# Example 21: Working with Subsets
# ═══════════════════════════════════════════════════════════

# Subset: Only breast cancer patients
breast_patients <- alluvial_test[alluvial_test$tumor_site == "Breast", ]
alluvial(
  data = breast_patients,
  vars = c("initial_stage", "treatment", "response"),
  usetitle = TRUE,
  mytitle = "Breast Cancer Treatment Pathway",
  colorPalette = "plasma"
)

# Subset: Only high-risk patients
high_risk <- alluvial_test[alluvial_test$risk_group == "High Risk", ]
alluvial(
  data = high_risk,
  vars = c("comorbidity", "performance_status", "treatment", "response"),
  usetitle = TRUE,
  mytitle = "High-Risk Patient Outcomes",
  colorPalette = "dark2"
)

# Subset: Early-stage disease only
early_stage <- alluvial_test[alluvial_test$initial_stage %in% c("Stage I", "Stage II"), ]
alluvial(
  data = early_stage,
  vars = c("initial_grade", "treatment", "response", "followup_status"),
  usetitle = TRUE,
  mytitle = "Early-Stage Disease Outcomes",
  colorPalette = "viridis"
)

# ═══════════════════════════════════════════════════════════
# Interpretation Guide
# ═══════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════\n")
cat("Understanding Alluvial Diagrams\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("What is an Alluvial Diagram?\n")
cat("  - Visualizes flows of categorical data across multiple dimensions\n")
cat("  - Shows how observations transition between categories\n")
cat("  - Flow thickness represents count or weight\n\n")

cat("Key Components:\n")
cat("  - Nodes (vertical bars): Categorical variables\n")
cat("  - Flows (ribbons): Transitions between categories\n")
cat("  - Colors: Group identification (by first/last variable or custom)\n")
cat("  - Width: Number of observations or weights\n\n")

cat("Best Practices:\n")
cat("  1. Use 2-5 variables (optimal: 3-4)\n")
cat("  2. Each variable should have 3-7 categories\n")
cat("  3. Arrange variables in logical order (temporal or hierarchical)\n")
cat("  4. Use color palettes appropriate for your audience\n")
cat("  5. Add marginal plots for distribution context\n\n")

cat("When to Use:\n")
cat("  ✓ Patient flow through treatment stages\n")
cat("  ✓ Disease progression pathways\n")
cat("  ✓ Diagnostic journeys\n")
cat("  ✓ Risk stratification analysis\n")
cat("  ✓ Healthcare utilization patterns\n")
cat("  ✓ Demographic flow analysis\n\n")

cat("When NOT to Use:\n")
cat("  ✗ Continuous variables (use categorize function first)\n")
cat("  ✗ Too many variables (>8) - becomes unreadable\n")
cat("  ✗ Variables with too many categories (>10) - spaghetti plot\n")
cat("  ✗ Small sample sizes (<20) - unreliable patterns\n\n")

cat("Engine Selection:\n")
cat("  • Easy Alluvial: Automatic, quick, good defaults\n")
cat("  • GG Alluvial: Manual control, more customization,\n")
cat("                 supports weights, curve types, Sankey style\n\n")

cat("Color Strategies:\n")
cat("  • first_variable: Color by starting point (shows origins)\n")
cat("  • last_variable: Color by endpoint (shows outcomes)\n")
cat("  • all_flows: Unique color per flow path (complex)\n")
cat("  • values: Color by specific variable values\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("For more information:\n")
cat("  ?alluvial\n")
cat("  help(package='ClinicoPath')\n")
cat("  https://www.serdarbalci.com/ClinicoPath/\n")
cat("═══════════════════════════════════════════════════════════\n")
