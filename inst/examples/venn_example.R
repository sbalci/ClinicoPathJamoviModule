# ═══════════════════════════════════════════════════════════
# VENN - COMPREHENSIVE USAGE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the venn function
# (Venn Diagram and UpSet Plot for Set Overlap Visualization)
# using the venn_test dataset.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(venn_test)

# ═══════════════════════════════════════════════════════════
# BASIC USAGE: 2-SET VENN DIAGRAMS
# ═══════════════════════════════════════════════════════════

# Example 1: Basic 2-variable Venn diagram (Biomarkers)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive'
)

# Example 2: Two diagnostic tests overlap
venn(
  data = venn_test,
  var1 = 'test_1',
  var1true = 'Positive',
  var2 = 'test_2',
  var2true = 'Positive'
)

# Example 3: Symptom co-occurrence
venn(
  data = venn_test,
  var1 = 'symptom_pain',
  var1true = 'Present',
  var2 = 'symptom_mass',
  var2true = 'Present'
)

# Example 4: Comorbidity patterns
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes'
)

# Example 5: Treatment response overlap
venn(
  data = venn_test,
  var1 = 'treatment_a_response',
  var1true = 'Responder',
  var2 = 'treatment_b_response',
  var2true = 'Responder'
)

# ═══════════════════════════════════════════════════════════
# 3-SET VENN DIAGRAMS
# ═══════════════════════════════════════════════════════════

# Example 6: Three biomarkers
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive'
)

# Example 7: Three comorbidities (metabolic syndrome components)
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes'
)

# Example 8: Two tests plus one biomarker
venn(
  data = venn_test,
  var1 = 'test_1',
  var1true = 'Positive',
  var2 = 'test_2',
  var2true = 'Positive',
  var3 = 'biomarker_a',
  var3true = 'Positive'
)

# Example 9: Two symptoms plus stage
venn(
  data = venn_test,
  var1 = 'symptom_pain',
  var1true = 'Present',
  var2 = 'symptom_mass',
  var2true = 'Present',
  var3 = 'stage_binary',
  var3true = 'Advanced'
)

# ═══════════════════════════════════════════════════════════
# 4-SET VENN DIAGRAMS
# ═══════════════════════════════════════════════════════════

# Example 10: Four biomarkers
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'biomarker_d',
  var4true = 'Positive'
)

# Example 11: Clinical risk factors
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  var4 = 'family_history',
  var4true = 'Yes'
)

# Example 12: Two symptoms + two tests
venn(
  data = venn_test,
  var1 = 'symptom_pain',
  var1true = 'Present',
  var2 = 'symptom_mass',
  var2true = 'Present',
  var3 = 'test_1',
  var3true = 'Positive',
  var4 = 'test_2',
  var4true = 'Positive'
)

# ═══════════════════════════════════════════════════════════
# 5-SET AND LARGER VENN DIAGRAMS
# ═══════════════════════════════════════════════════════════

# Example 13: Five biomarkers (automatically uses advanced engine)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'biomarker_d',
  var4true = 'Positive',
  var5 = 'rare_marker',
  var5true = 'Positive'
)

# Example 14: Six clinical variables
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  var4 = 'family_history',
  var4true = 'Yes',
  var5 = 'symptom_pain',
  var5true = 'Present',
  var6 = 'symptom_mass',
  var6true = 'Present'
)

# Example 15: Seven variables (maximum)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'test_1',
  var4true = 'Positive',
  var5 = 'test_2',
  var5true = 'Positive',
  var6 = 'symptom_pain',
  var6true = 'Present',
  var7 = 'symptom_mass',
  var7true = 'Present'
)

# ═══════════════════════════════════════════════════════════
# MULTI-LEVEL CATEGORICAL VARIABLES (Testing true level selection)
# ═══════════════════════════════════════════════════════════

# Example 16: Mutation status (3 levels) - Select "Mutation" as true
venn(
  data = venn_test,
  var1 = 'mutation_status',
  var1true = 'Mutation',
  var2 = 'biomarker_a',
  var2true = 'Positive'
)

# Example 17: Tumor grade (3 levels, ordered) - Select "High" as true
venn(
  data = venn_test,
  var1 = 'tumor_grade',
  var1true = 'High',
  var2 = 'stage_binary',
  var2true = 'Advanced'
)

# Example 18: Receptor status (4 levels) - Select "ER+" as true
venn(
  data = venn_test,
  var1 = 'receptor_status',
  var1true = 'ER+',
  var2 = 'receptor_status',
  var2true = 'PR+'
)

# Example 19: Multiple multi-level variables
venn(
  data = venn_test,
  var1 = 'mutation_status',
  var1true = 'Mutation',
  var2 = 'tumor_grade',
  var2true = 'High',
  var3 = 'receptor_status',
  var3true = 'HER2+'
)

# ═══════════════════════════════════════════════════════════
# VISUALIZATION ENGINES
# ═══════════════════════════════════════════════════════════

# Example 20: ggvenn (default, simple and clean)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggvenn = TRUE
)

# Example 21: ggVennDiagram (advanced, customizable)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE
)

# Example 22: UpSetR (for many sets or complex overlaps)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'test_1',
  var4true = 'Positive',
  show_upsetR = TRUE
)

# Example 23: ComplexUpset (advanced UpSet with annotations)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'test_1',
  var4true = 'Positive',
  show_complexUpset = TRUE
)

# Example 24: Show multiple visualizations simultaneously
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggvenn = TRUE,
  show_ggVennDiagram = TRUE,
  show_upsetR = TRUE
)

# ═══════════════════════════════════════════════════════════
# UPSET PLOT CUSTOMIZATION
# ═══════════════════════════════════════════════════════════

# Example 25: Sort by frequency (most common intersections first)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_complexUpset = TRUE,
  sortBy = 'freq'
)

# Example 26: Sort by degree (number of sets in intersection)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_complexUpset = TRUE,
  sortBy = 'degree'
)

# Example 27: Filter small intersections (minSize)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'rare_marker',
  var4true = 'Positive',
  show_complexUpset = TRUE,
  minSize = 10
)

# Example 28: Show percentage annotations
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_complexUpset = TRUE,
  showAnnotations = TRUE
)

# ═══════════════════════════════════════════════════════════
# GGVENNDIAGRAM CUSTOMIZATION
# ═══════════════════════════════════════════════════════════

# Example 29: Circle shape
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  shapeType = 'circle'
)

# Example 30: Ellipse shape
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  shapeType = 'ellipse'
)

# Example 31: Show percentages instead of counts
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggVennDiagram = TRUE,
  regionLabels = 'percent'
)

# Example 32: Show both count and percent
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggVennDiagram = TRUE,
  regionLabels = 'both'
)

# Example 33: Custom color palette (Set1)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  colorPalette = 'Set1'
)

# Example 34: Viridis color palette
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  colorPalette = 'viridis'
)

# Example 35: Custom sizes and styles
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggVennDiagram = TRUE,
  setNameSize = 6,
  labelSize = 5,
  edgeSize = 2,
  edgeColor = 'darkblue'
)

# Example 36: Transparency adjustments
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  fillAlpha = 0.3,
  edgeAlpha = 1
)

# Example 37: Dashed edge lines
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  show_ggVennDiagram = TRUE,
  edgeLineType = 'dashed'
)

# ═══════════════════════════════════════════════════════════
# SET OPERATIONS AND CALCULATIONS
# ═══════════════════════════════════════════════════════════

# Example 38: Calculate overlaps
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  calculateOverlap = TRUE
)

# Example 39: Calculate unique members (discern)
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  calculateDiscern = TRUE
)

# Example 40: Calculate union of all sets
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  calculateUnite = TRUE
)

# Example 41: Show all set calculations
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  showSetCalculations = TRUE,
  calculateOverlap = TRUE,
  calculateDiscern = TRUE,
  calculateUnite = TRUE
)

# Example 42: Show membership table
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  showMembershipTable = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL INTERPRETATION OUTPUTS
# ═══════════════════════════════════════════════════════════

# Example 43: Show explanatory output
venn(
  data = venn_test,
  var1 = 'test_1',
  var1true = 'Positive',
  var2 = 'test_2',
  var2true = 'Positive',
  explanatory = TRUE
)

# Example 44: Show about analysis
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  aboutAnalysis = TRUE
)

# Example 45: Show clinical summary
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  clinicalSummary = TRUE
)

# Example 46: Show report sentences
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  reportSentences = TRUE
)

# Example 47: Show assumptions
venn(
  data = venn_test,
  var1 = 'test_1',
  var1true = 'Positive',
  var2 = 'test_2',
  var2true = 'Positive',
  assumptions = TRUE
)

# Example 48: Show glossary
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  showGlossary = TRUE
)

# ═══════════════════════════════════════════════════════════
# EDGE CASES AND SPECIAL SCENARIOS
# ═══════════════════════════════════════════════════════════

# Example 49: Rare marker (5% prevalence) - Small intersection sizes
venn(
  data = venn_test,
  var1 = 'rare_marker',
  var1true = 'Positive',
  var2 = 'biomarker_a',
  var2true = 'Positive',
  var3 = 'biomarker_b',
  var3true = 'Positive'
)

# Example 50: Common marker (95% prevalence) - Large overlaps
venn(
  data = venn_test,
  var1 = 'common_marker',
  var1true = 'Positive',
  var2 = 'biomarker_a',
  var2true = 'Positive',
  var3 = 'biomarker_b',
  var3true = 'Positive'
)

# Example 51: Balanced marker (50/50 distribution)
venn(
  data = venn_test,
  var1 = 'balanced_marker',
  var1true = 'Positive',
  var2 = 'biomarker_a',
  var2true = 'Positive',
  var3 = 'biomarker_b',
  var3true = 'Positive'
)

# Example 52: Nearly mutually exclusive sets
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'exclusive_marker',
  var2true = 'Positive'
)

# Example 53: Highly correlated sets (90% agreement)
venn(
  data = venn_test,
  var1 = 'test_1',
  var1true = 'Positive',
  var2 = 'correlated_test',
  var2true = 'Positive'
)

# Example 54: Variables with missing data
venn(
  data = venn_test,
  var1 = 'biomarker_d',
  var1true = 'Positive',
  var2 = 'test_3',
  var2true = 'Positive'
)

# ═══════════════════════════════════════════════════════════
# COMPREHENSIVE FEATURE COMBINATIONS
# ═══════════════════════════════════════════════════════════

# Example 55: Full ggVennDiagram customization
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggVennDiagram = TRUE,
  shapeType = 'ellipse',
  regionLabels = 'both',
  colorPalette = 'Set1',
  setNameSize = 6,
  labelSize = 4.5,
  edgeSize = 1.5,
  edgeColor = 'darkblue',
  edgeLineType = 'solid',
  fillAlpha = 0.4,
  clinicalSummary = TRUE,
  reportSentences = TRUE
)

# Example 56: Full ComplexUpset customization
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'test_1',
  var4true = 'Positive',
  var5 = 'test_2',
  var5true = 'Positive',
  show_complexUpset = TRUE,
  sortBy = 'freq',
  minSize = 5,
  showAnnotations = TRUE,
  showSetCalculations = TRUE,
  calculateOverlap = TRUE,
  calculateDiscern = TRUE,
  calculateUnite = TRUE,
  clinicalSummary = TRUE
)

# Example 57: All visualization types with all outputs
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggvenn = TRUE,
  show_ggVennDiagram = TRUE,
  show_upsetR = TRUE,
  show_complexUpset = TRUE,
  explanatory = TRUE,
  clinicalSummary = TRUE,
  reportSentences = TRUE,
  showSetCalculations = TRUE,
  showMembershipTable = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# Scenario 1: Biomarker Co-Expression Analysis
# Examine which biomarkers are co-expressed in tumors
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  regionLabels = 'both',
  colorPalette = 'Set1',
  clinicalSummary = TRUE,
  reportSentences = TRUE,
  showSetCalculations = TRUE,
  calculateOverlap = TRUE,
  calculateDiscern = TRUE
)

# Scenario 2: Diagnostic Test Concordance
# Assess agreement between different diagnostic tests
venn(
  data = venn_test,
  var1 = 'test_1',
  var1true = 'Positive',
  var2 = 'test_2',
  var2true = 'Positive',
  var3 = 'test_3',
  var3true = 'Positive',
  show_ggVennDiagram = TRUE,
  regionLabels = 'both',
  clinicalSummary = TRUE,
  reportSentences = TRUE,
  explanatory = TRUE,
  assumptions = TRUE
)

# Scenario 3: Metabolic Syndrome Components
# Visualize overlap of diabetes, hypertension, and obesity
venn(
  data = venn_test,
  var1 = 'diabetes',
  var1true = 'Yes',
  var2 = 'hypertension',
  var2true = 'Yes',
  var3 = 'obesity',
  var3true = 'Yes',
  show_ggVennDiagram = TRUE,
  shapeType = 'circle',
  regionLabels = 'both',
  colorPalette = 'Pastel1',
  setNameSize = 5,
  clinicalSummary = TRUE,
  reportSentences = TRUE
)

# Scenario 4: Treatment Response Patterns
# Identify patients responding to multiple treatments
venn(
  data = venn_test,
  var1 = 'treatment_a_response',
  var1true = 'Responder',
  var2 = 'treatment_b_response',
  var2true = 'Responder',
  show_ggVennDiagram = TRUE,
  regionLabels = 'both',
  clinicalSummary = TRUE,
  reportSentences = TRUE,
  showMembershipTable = TRUE
)

# Scenario 5: Symptom Clusters
# Examine co-occurrence of clinical symptoms
venn(
  data = venn_test,
  var1 = 'symptom_pain',
  var1true = 'Present',
  var2 = 'symptom_mass',
  var2true = 'Present',
  var3 = 'stage_binary',
  var3true = 'Advanced',
  show_ggVennDiagram = TRUE,
  regionLabels = 'both',
  clinicalSummary = TRUE,
  calculateOverlap = TRUE
)

# Scenario 6: Risk Factor Stratification
# Assess cumulative risk from multiple factors
venn(
  data = venn_test,
  var1 = 'family_history',
  var1true = 'Yes',
  var2 = 'diabetes',
  var2true = 'Yes',
  var3 = 'hypertension',
  var3true = 'Yes',
  var4 = 'obesity',
  var4true = 'Yes',
  show_complexUpset = TRUE,
  sortBy = 'freq',
  showAnnotations = TRUE,
  clinicalSummary = TRUE,
  reportSentences = TRUE
)

# Scenario 7: Molecular Subtyping
# Define molecular subtypes based on biomarker profiles
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'biomarker_b',
  var2true = 'Positive',
  var3 = 'biomarker_c',
  var3true = 'Positive',
  var4 = 'mutation_status',
  var4true = 'Mutation',
  show_complexUpset = TRUE,
  sortBy = 'degree',
  minSize = 5,
  showAnnotations = TRUE,
  clinicalSummary = TRUE,
  showMembershipTable = TRUE
)

# Scenario 8: Multi-Test Diagnostic Algorithm
# Develop diagnostic algorithm using multiple tests
venn(
  data = venn_test,
  var1 = 'biomarker_a',
  var1true = 'Positive',
  var2 = 'test_1',
  var2true = 'Positive',
  var3 = 'test_2',
  var3true = 'Positive',
  var4 = 'symptom_pain',
  var4true = 'Present',
  var5 = 'symptom_mass',
  var5true = 'Present',
  show_complexUpset = TRUE,
  sortBy = 'freq',
  minSize = 10,
  showAnnotations = TRUE,
  clinicalSummary = TRUE,
  reportSentences = TRUE,
  showSetCalculations = TRUE
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# OUTPUT INTERPRETATION:
#
# The venn function produces:
# 1. **Venn Diagram**: Visual representation of set overlaps
#    - Each circle/ellipse represents a set (variable = true level)
#    - Overlapping regions show intersections
#    - Numbers show count (or percentage) in each region
#
# 2. **UpSet Plot**: Alternative visualization for complex overlaps
#    - Horizontal bars show set sizes
#    - Vertical bars show intersection sizes
#    - Matrix below shows which sets are in each intersection
#    - Better than Venn for 5+ sets or many intersections
#
# 3. **Set Calculations**:
#    - **Overlap**: Intersection of all selected sets (AND operation)
#    - **Discern**: Unique members in each set (exclusive to that set)
#    - **Unite**: Union of all sets (OR operation)
#
# 4. **Membership Table**: Shows which cases belong to which set combinations
#
# READING VENN DIAGRAMS:
# - **Non-overlapping regions**: Items in only that set
# - **2-way overlaps**: Items in exactly those 2 sets
# - **3-way overlap (center)**: Items in all 3 sets
# - **Outside all circles**: Items in none of the sets
#
# READING UPSET PLOTS:
# - **Top bars**: Size of each intersection
# - **Dots connected by lines**: Which sets are in that intersection
# - **Single dots**: Items in only that set
# - **Multiple connected dots**: Items in multiple sets
#
# SET OPERATION INTERPRETATION:
# - **Overlap size = 0**: No items in all sets simultaneously
# - **Discern size = 0**: No unique items (all shared)
# - **Unite size**: Total unique items across all sets
#
# ═══════════════════════════════════════════════════════════
# BEST PRACTICES
# ═══════════════════════════════════════════════════════════

# 1. **Choosing Number of Sets**:
#    - 2-3 sets: Use traditional Venn diagram (ggvenn, ggVennDiagram)
#    - 4 sets: Venn still works but gets complex
#    - 5+ sets: Prefer UpSet plots (clearer visualization)

# 2. **Selecting True Levels**:
#    - For binary variables: Select the "positive" outcome
#    - For multi-level: Select clinically meaningful level
#    - Be consistent across related analyses

# 3. **Visualization Choice**:
#    - **ggvenn**: Simple, clean, good for presentations
#    - **ggVennDiagram**: Customizable, publication-ready
#    - **upsetR**: Standard UpSet plot, 4+ sets
#    - **complexUpset**: Advanced UpSet, annotations, theming

# 4. **Clinical Interpretation**:
#    - Large overlaps → Sets are related/correlated
#    - Small overlaps → Sets are independent
#    - No overlap → Mutually exclusive conditions
#    - Use clinical summary for interpretation guidance

# 5. **Handling Missing Data**:
#    - Missing values are excluded from set membership
#    - Document missing data percentage
#    - Consider imputation if > 10% missing

# 6. **Sample Size Considerations**:
#    - Minimum 30-50 observations recommended
#    - Small intersections (<5) may be unstable
#    - Use minSize parameter to filter small intersections

# ═══════════════════════════════════════════════════════════
# TROUBLESHOOTING
# ═══════════════════════════════════════════════════════════

# Error: "Variable must be categorical"
# Solution: Ensure variables are factors or characters
# Example fix:
# venn_test$continuous_var <- as.numeric(venn_test$biomarker_a)
# venn(data = venn_test, var1 = 'continuous_var')  # Error!
# venn(data = venn_test, var1 = 'biomarker_a')     # Works!

# Error: "True level not found"
# Solution: Check spelling and case of true level
# Example:
# levels(venn_test$biomarker_a)  # Check available levels
# venn(var1true = 'Positive')    # Correct
# venn(var1true = 'positive')    # Error! Case sensitive

# Issue: Empty intersections
# Solution: Normal if sets don't overlap
# - Verify data is correct
# - Check if this is clinically expected
# - Use minSize to hide empty intersections

# Issue: Too many intersections in UpSet plot
# Solution:
# - Use minSize parameter to filter small intersections
# - Reduce number of sets
# - Use sortBy = 'freq' to show largest first

# Issue: Diagram is too cluttered
# Solution:
# - Reduce number of sets (3-4 optimal for Venn)
# - Switch to UpSet plot for 5+ sets
# - Adjust label sizes and transparency
# - Use regionLabels = 'none' for cleaner look

# ═══════════════════════════════════════════════════════════
