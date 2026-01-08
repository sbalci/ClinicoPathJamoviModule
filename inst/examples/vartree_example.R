# ═══════════════════════════════════════════════════════════
# VARTREE - COMPREHENSIVE USAGE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the vartree function
# (Variable Tree - Hierarchical Visualization of Categorical Variables)
# using the vartree_test dataset.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(vartree_test)

# ═══════════════════════════════════════════════════════════
# BASIC USAGE
# ═══════════════════════════════════════════════════════════

# Example 1: Single Variable Tree
vartree(
  data = vartree_test,
  vars = 'treatment'
)

# Example 2: Two Variable Tree (Treatment → Response)
vartree(
  data = vartree_test,
  vars = c('treatment', 'response')
)

# Example 3: Three Variable Tree (Treatment → Response → Stage)
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage')
)

# Example 4: Four Variable Tree (Clinical Pathway)
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage', 'metastasis')
)

# Example 5: Five Variable Tree (Extended Pathway)
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage', 'grade', 'vital_status')
)

# ═══════════════════════════════════════════════════════════
# CONTINUOUS VARIABLE SUMMARIES
# ═══════════════════════════════════════════════════════════

# Example 6: Tree with Age Summary at All Nodes
vartree(
  data = vartree_test,
  vars = c('treatment', 'response'),
  summaryvar = 'age',
  summarylocation = 'allnodes'
)

# Example 7: Tree with Survival Time Summary at Leaf Nodes Only
vartree(
  data = vartree_test,
  vars = c('stage', 'grade', 'vital_status'),
  summaryvar = 'survival_months',
  summarylocation = 'leafonly'
)

# Example 8: Tree with Tumor Size Summary
vartree(
  data = vartree_test,
  vars = c('stage', 'histology'),
  summaryvar = 'tumor_size',
  summarylocation = 'allnodes'
)

# Example 9: Tree with BMI Summary
vartree(
  data = vartree_test,
  vars = c('sex', 'age_group'),
  summaryvar = 'bmi',
  summarylocation = 'leafonly'
)

# Example 10: Tree with Biomarker Summary
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage'),
  summaryvar = 'biomarker',
  summarylocation = 'allnodes'
)

# Example 11: Tree with Quality of Life Score Summary
vartree(
  data = vartree_test,
  vars = c('performance_status', 'comorbidity'),
  summaryvar = 'qol_score',
  summarylocation = 'leafonly'
)

# ═══════════════════════════════════════════════════════════
# PERCENTAGE CALCULATIONS
# ═══════════════════════════════════════════════════════════

# Example 12: Tree with Percentages Based on Event Variable
vartree(
  data = vartree_test,
  vars = c('treatment', 'stage'),
  percvar = 'vital_status'
)

# Example 13: Tree with Percentages Based on Response
vartree(
  data = vartree_test,
  vars = c('stage', 'grade'),
  percvar = 'response'
)

# Example 14: Tree with Percentages Based on Metastasis
vartree(
  data = vartree_test,
  vars = c('histology', 'stage'),
  percvar = 'metastasis'
)

# ═══════════════════════════════════════════════════════════
# PRUNING SMALL NODES
# ═══════════════════════════════════════════════════════════

# Example 15: Prune Nodes with < 5 Observations
vartree(
  data = vartree_test,
  vars = c('mutation_status', 'treatment', 'response'),
  useprunesmaller = TRUE,
  prunesmaller = 5
)

# Example 16: Prune Nodes with < 10 Observations
vartree(
  data = vartree_test,
  vars = c('subtype', 'stage', 'grade'),
  useprunesmaller = TRUE,
  prunesmaller = 10
)

# Example 17: Prune Nodes with < 15 Observations (Strict)
vartree(
  data = vartree_test,
  vars = c('ethnicity', 'marital_status', 'insurance'),
  useprunesmaller = TRUE,
  prunesmaller = 15
)

# Example 18: Pruning with Rare Categories
vartree(
  data = vartree_test,
  vars = c('mutation_status', 'stage', 'vital_status'),
  useprunesmaller = TRUE,
  prunesmaller = 3
)

# ═══════════════════════════════════════════════════════════
# CONDITIONAL PRUNING (prunebelow)
# ═══════════════════════════════════════════════════════════

# Example 19: Show Only Early Stage Patients
vartree(
  data = vartree_test,
  vars = c('stage', 'treatment', 'response'),
  prunebelow = 'stage="I"'
)

# Example 20: Show Only High Grade Tumors
vartree(
  data = vartree_test,
  vars = c('grade', 'histology', 'metastasis'),
  prunebelow = 'grade="High"'
)

# Example 21: Show Only Surgery Patients
vartree(
  data = vartree_test,
  vars = c('treatment', 'stage', 'vital_status'),
  prunebelow = 'treatment="Surgery"'
)

# ═══════════════════════════════════════════════════════════
# CONDITIONAL FOLLOWING (follow)
# ═══════════════════════════════════════════════════════════

# Example 22: Follow Complete Response Branch
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'vital_status'),
  follow = 'response="Complete Response"'
)

# Example 23: Follow Metastasis Branch
vartree(
  data = vartree_test,
  vars = c('stage', 'grade', 'metastasis', 'vital_status'),
  follow = 'metastasis="Yes"'
)

# Example 24: Follow Specific Treatment Branch
vartree(
  data = vartree_test,
  vars = c('treatment', 'stage', 'response'),
  follow = 'treatment="Chemotherapy"'
)

# ═══════════════════════════════════════════════════════════
# PATTERN TREES
# ═══════════════════════════════════════════════════════════

# Example 25: Pattern Tree - Treatment Response Patterns
vartree(
  data = vartree_test,
  vars = c('treatment', 'response_pattern', 'vital_status'),
  pattern = TRUE
)

# Example 26: Pattern Tree - Stage and Grade Combinations
vartree(
  data = vartree_test,
  vars = c('stage', 'grade', 'metastasis'),
  pattern = TRUE
)

# Example 27: Pattern Tree with Summary Variable
vartree(
  data = vartree_test,
  vars = c('histology', 'stage', 'grade'),
  pattern = TRUE,
  summaryvar = 'survival_months'
)

# ═══════════════════════════════════════════════════════════
# SEQUENCE TREES
# ═══════════════════════════════════════════════════════════

# Example 28: Sequence Tree - Treatment Sequences
vartree(
  data = vartree_test,
  vars = c('treatment_sequence', 'response', 'vital_status'),
  sequence = TRUE
)

# Example 29: Sequence Tree - Disease Progression
vartree(
  data = vartree_test,
  vars = c('stage', 'treatment_sequence', 'response_pattern'),
  sequence = TRUE
)

# Example 30: Sequence Tree with Pruning
vartree(
  data = vartree_test,
  vars = c('treatment_sequence', 'response_pattern', 'vital_status'),
  sequence = TRUE,
  useprunesmaller = TRUE,
  prunesmaller = 5
)

# ═══════════════════════════════════════════════════════════
# VISUAL STYLES
# ═══════════════════════════════════════════════════════════

# Example 31: Default Style
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage'),
  style = 'default'
)

# Example 32: Clean Style
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage'),
  style = 'clean'
)

# Example 33: Minimal Style
vartree(
  data = vartree_test,
  vars = c('treatment', 'response', 'stage'),
  style = 'minimal'
)

# ═══════════════════════════════════════════════════════════
# DEMOGRAPHIC TREES
# ═══════════════════════════════════════════════════════════

# Example 34: Demographic Breakdown by Age and Sex
vartree(
  data = vartree_test,
  vars = c('sex', 'ethnicity', 'marital_status')
)

# Example 35: Ethnicity Distribution
vartree(
  data = vartree_test,
  vars = c('ethnicity', 'age_group', 'insurance')
)

# Example 36: Marital Status and Insurance
vartree(
  data = vartree_test,
  vars = c('marital_status', 'insurance', 'comorbidity')
)

# ═══════════════════════════════════════════════════════════
# CLINICAL PATHWAY TREES
# ═══════════════════════════════════════════════════════════

# Example 37: Treatment Decision Pathway
vartree(
  data = vartree_test,
  vars = c('stage', 'performance_status', 'treatment', 'response')
)

# Example 38: Diagnostic Pathway
vartree(
  data = vartree_test,
  vars = c('histology', 'stage', 'grade', 'metastasis')
)

# Example 39: Prognostic Pathway
vartree(
  data = vartree_test,
  vars = c('stage', 'grade', 'treatment', 'vital_status'),
  summaryvar = 'survival_months'
)

# Example 40: Risk Stratification Pathway
vartree(
  data = vartree_test,
  vars = c('smoking_status', 'performance_status', 'comorbidity', 'vital_status')
)

# ═══════════════════════════════════════════════════════════
# EDGE CASES AND SPECIAL SCENARIOS
# ═══════════════════════════════════════════════════════════

# Example 41: Tree with Constant Variable (All One Category)
# Expected: Single node with 100% in one category
vartree(
  data = vartree_test,
  vars = c('constant_category', 'stage')
)

# Example 42: Tree with Nearly Constant Variable
# Expected: Highly imbalanced tree
vartree(
  data = vartree_test,
  vars = c('nearly_constant', 'treatment')
)

# Example 43: Tree with Many Categories (12 levels)
# Expected: Wide tree with many branches
vartree(
  data = vartree_test,
  vars = c('many_categories', 'vital_status')
)

# Example 44: Tree with Missing Data Variable
# Expected: Includes NA category
vartree(
  data = vartree_test,
  vars = c('grade_missing', 'stage', 'vital_status')
)

# Example 45: Tree Pruning with Many Rare Categories
vartree(
  data = vartree_test,
  vars = c('many_categories', 'subtype', 'mutation_status'),
  useprunesmaller = TRUE,
  prunesmaller = 10
)

# ═══════════════════════════════════════════════════════════
# COMBINED FEATURES
# ═══════════════════════════════════════════════════════════

# Example 46: Tree with Summary, Percentages, and Pruning
vartree(
  data = vartree_test,
  vars = c('treatment', 'stage', 'response'),
  summaryvar = 'survival_months',
  summarylocation = 'allnodes',
  percvar = 'vital_status',
  useprunesmaller = TRUE,
  prunesmaller = 5
)

# Example 47: Pattern Tree with Summary and Style
vartree(
  data = vartree_test,
  vars = c('treatment', 'response_pattern', 'vital_status'),
  pattern = TRUE,
  summaryvar = 'qol_score',
  style = 'clean'
)

# Example 48: Sequence Tree with Following and Summary
vartree(
  data = vartree_test,
  vars = c('treatment_sequence', 'response_pattern', 'vital_status'),
  sequence = TRUE,
  follow = 'response_pattern="Early Response"',
  summaryvar = 'survival_months'
)

# Example 49: Complex Tree with All Features
vartree(
  data = vartree_test,
  vars = c('stage', 'grade', 'treatment', 'response', 'vital_status'),
  summaryvar = 'survival_months',
  summarylocation = 'leafonly',
  percvar = 'metastasis',
  useprunesmaller = TRUE,
  prunesmaller = 8,
  style = 'clean'
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# Scenario 1: Treatment Response Analysis
# Visualize how treatment outcomes vary by stage and grade
vartree(
  data = vartree_test,
  vars = c('treatment', 'stage', 'grade', 'response'),
  summaryvar = 'survival_months',
  summarylocation = 'leafonly',
  percvar = 'vital_status'
)

# Scenario 2: Biomarker Stratification
# Examine biomarker levels across different patient subgroups
vartree(
  data = vartree_test,
  vars = c('mutation_status', 'stage', 'treatment'),
  summaryvar = 'biomarker',
  summarylocation = 'allnodes',
  useprunesmaller = TRUE,
  prunesmaller = 5
)

# Scenario 3: Quality of Life Assessment
# Analyze QOL scores across performance status and treatment groups
vartree(
  data = vartree_test,
  vars = c('performance_status', 'treatment', 'comorbidity'),
  summaryvar = 'qol_score',
  summarylocation = 'allnodes'
)

# Scenario 4: Demographic Distribution in Clinical Trial
# Check balance of demographic variables across treatment arms
vartree(
  data = vartree_test,
  vars = c('treatment', 'sex', 'ethnicity', 'age_group'),
  style = 'clean'
)

# Scenario 5: Metastasis Risk Stratification
# Identify patient subgroups at high risk for metastasis
vartree(
  data = vartree_test,
  vars = c('stage', 'histology', 'grade', 'metastasis'),
  percvar = 'metastasis',
  summaryvar = 'tumor_size'
)

# Scenario 6: Survival Analysis by Treatment Pathway
# Compare survival across different treatment sequences
vartree(
  data = vartree_test,
  vars = c('treatment_sequence', 'treatment', 'response_pattern'),
  summaryvar = 'survival_months',
  summarylocation = 'leafonly',
  percvar = 'vital_status',
  sequence = TRUE
)

# Scenario 7: Smoking Impact on Treatment Outcomes
# Examine how smoking status affects performance and outcomes
vartree(
  data = vartree_test,
  vars = c('smoking_status', 'performance_status', 'treatment', 'response'),
  summaryvar = 'survival_months',
  percvar = 'vital_status'
)

# Scenario 8: Insurance and Treatment Access
# Analyze treatment patterns across insurance types
vartree(
  data = vartree_test,
  vars = c('insurance', 'treatment', 'stage'),
  percvar = 'response',
  useprunesmaller = TRUE,
  prunesmaller = 10
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# OUTPUT INTERPRETATION:
#
# The vartree function produces hierarchical tree visualizations where:
# 1. **Nodes**: Represent subgroups defined by category combinations
# 2. **Branches**: Show splits by categorical variable levels
# 3. **Node size**: Proportional to number of observations
# 4. **Percentages**: Show distribution within each node
# 5. **Summaries**: Display mean ± SD for continuous variables
#
# READING THE TREE:
# - **Root node** (top): Entire dataset
# - **Child nodes**: Subgroups created by first variable
# - **Leaf nodes** (bottom): Final subgroups after all splits
# - **Numbers**: Count (n) and percentage (%) of observations
#
# SUMMARY VARIABLE INTERPRETATION:
# - **All nodes**: Shows summary at every split level
# - **Leaf only**: Shows summary only at terminal nodes
# - Format: mean ± SD (range: min-max)
#
# PERCENTAGE VARIABLE (percvar):
# - Shows percentage of specified category within each node
# - Useful for outcome rates (e.g., % deceased, % with metastasis)
# - Helps identify high-risk subgroups
#
# PRUNING INTERPRETATION:
# - **Prunebelow**: Shows only branches matching condition
# - **Follow**: Highlights specific pathway through tree
# - **Prunesmaller**: Removes nodes with insufficient sample size
# - Helps focus on clinically relevant subgroups
#
# PATTERN VS SEQUENCE:
# - **Pattern**: All combinations of variables (unordered)
# - **Sequence**: Ordered progression through states
# - Use sequence for temporal progressions (treatment lines)
# - Use pattern for concurrent characteristics
#

# ═══════════════════════════════════════════════════════════
# BEST PRACTICES
# ═══════════════════════════════════════════════════════════

# 1. **Variable Order**:
#    - Place most important stratifying variable first
#    - Use clinically logical ordering (e.g., stage → grade → treatment)
#    - Limit to 3-5 variables to maintain readability

# 2. **Pruning for Sample Size**:
#    - Use prunesmaller to remove unreliable small subgroups
#    - Minimum n=5-10 per node for chi-squared assumptions
#    - Larger n required for regression analyses

# 3. **Summary Variables**:
#    - Use 'allnodes' to see how means change across splits
#    - Use 'leafonly' for cleaner presentation of final subgroups
#    - Choose clinically relevant continuous variables

# 4. **Percentage Variables**:
#    - Select binary outcomes of interest (event, response, survival)
#    - Helps identify high-risk or high-response subgroups
#    - Complements raw counts with outcome rates

# 5. **Clinical Reporting**:
#    - Use for subgroup analyses in clinical trials
#    - Visualize treatment pathways and decision points
#    - Identify patient populations with specific characteristics
#    - Export trees for inclusion in manuscripts/presentations

# 6. **Handling Missing Data**:
#    - vtree includes NA as a separate category
#    - Consider creating "Unknown" category for reporting
#    - Document missing data patterns in results

# ═══════════════════════════════════════════════════════════
# TROUBLESHOOTING
# ═══════════════════════════════════════════════════════════

# Error: "Variable must be categorical"
# Solution: Ensure all vars are factors or characters
# Example fix:
# vartree_test$age_numeric <- as.numeric(vartree_test$age)
# vartree(data = vartree_test, vars = 'age_numeric')  # Error!
# vartree(data = vartree_test, vars = 'age_group')    # Works!

# Error: "Summary variable must be numeric"
# Solution: Check that summaryvar is continuous
# Example fix:
# str(vartree_test$stage)  # Factor - won't work
# str(vartree_test$age)    # Numeric - works!

# Issue: Tree is too complex/unreadable
# Solution: 
# - Reduce number of variables (use 3-4 instead of 5+)
# - Use pruning to focus on specific branches
# - Try different visual styles (minimal, clean)

# Issue: Too many rare categories
# Solution:
# - Use useprunesmaller = TRUE with appropriate threshold
# - Consider combining rare categories before analysis
# - Use pattern = TRUE to simplify visualization

# Issue: Summary statistics seem incorrect
# Solution:
# - Check for outliers in summary variable
# - Verify missing data handling
# - Ensure variable is properly coded

# ═══════════════════════════════════════════════════════════
