# ═══════════════════════════════════════════════════════════
# Example Usage: crosstable
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating cross table generation
# with multiple table styles and automatic test selection
#
# Generated: 2026-01-04

library(ClinicoPath)

# Load test data
data(crosstable_test, package = "ClinicoPath")

# View data structure
str(crosstable_test)
summary(crosstable_test)

# ═══════════════════════════════════════════════════════════
# WHAT ARE CROSS TABLES?
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
CROSS TABLES (CONTINGENCY TABLES) - Background
═══════════════════════════════════════════════════════════

Cross tables summarize relationships between variables across groups,
displaying descriptive statistics and hypothesis test results.

STRUCTURE:
  • Rows: Dependent variables (outcomes, characteristics)
  • Columns: Grouping variable (treatment, stage, etc.)
  • Cells: Frequencies, proportions, means, medians, etc.

AUTOMATIC TEST SELECTION:

For Categorical Variables:
  • Chi-square test: Default for adequate cell frequencies (≥5)
  • Fisher's exact test: Small expected frequencies (<5)
  • Reports: n (%), p-value

For Continuous Variables:
  • t-test: 2 groups comparison
  • ANOVA: 3+ groups comparison
  • Reports: mean (SD) or median (IQR), p-value

TABLE STYLES AVAILABLE:

1. ARSENAL
   - Comprehensive clinical tables
   - Similar to SAS PROC TABULATE
   - Detailed summary statistics

2. FINALFIT
   - Clean, publication-ready format
   - Emphasis on clarity and simplicity
   - Good for manuscripts

3. GTSUMMARY
   - Modern gt-based tables
   - Supports p-value adjustment for multiple testing
   - Highly customizable

4. NEJM (New England Journal of Medicine)
   - Journal-specific formatting
   - Professional medical journal style
   - Conservative presentation

5. LANCET (The Lancet)
   - Journal-specific formatting
   - Alternative medical journal style
   - International format preferences

6. HMISC (Harrell's Hmisc)
   - Statistical analysis focus
   - Comprehensive test results
   - Academic research oriented

WHEN TO USE CROSS TABLES:

✓ Comparing baseline characteristics across groups
✓ Treatment arm comparisons in clinical trials
✓ Subgroup analyses by stage, grade, etc.
✓ Demographic summaries stratified by outcome
✓ Quality assurance and data validation
✓ Manuscript Table 1 (baseline characteristics)

CONTINUOUS VARIABLES:
  • Mean (SD): Normally distributed data
  • Median (IQR): Skewed data or outliers
  • Choose based on data distribution

MULTIPLE TESTING:
  • When testing many variables simultaneously
  • Consider p-value adjustment (Bonferroni, Holm, FDR)
  • Available with gtsummary style only

═══════════════════════════════════════════════════════════
\n")

# ═══════════════════════════════════════════════════════════
# EXAMPLE 1: BASIC CROSS TABLE (NEJM Style)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 1: Treatment Groups × Response (NEJM Style) ===\n")
cat("Purpose: Compare response rates across treatment groups\n")
cat("Table style: NEJM (New England Journal of Medicine)\n")
cat("Test: Chi-square (categorical)\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(response, grade, sex),
  group = "treatment",
  sty = "nejm"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 2: FINALFIT STYLE (Publication-Ready)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 2: Clean Publication Format (Finalfit) ===\n")
cat("Purpose: Manuscript Table 1 - baseline characteristics\n")
cat("Table style: finalfit (publication-ready)\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, sex, smoking, ecog, grade),
  group = "treatment",
  sty = "finalfit"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 3: GTSUMMARY STYLE (Modern Tables)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 3: Modern gt-Based Tables (gtsummary) ===\n")
cat("Purpose: Customizable modern table format\n")
cat("Table style: gtsummary\n")
cat("Feature: Supports p-value adjustment\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(response, complications, mutation),
  group = "treatment",
  sty = "gtsummary"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 4: LANCET STYLE
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 4: The Lancet Journal Style ===\n")
cat("Purpose: Lancet journal formatting\n")
cat("Table style: lancet\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(histology, grade, mutation),
  group = "stage",
  sty = "lancet"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 5: ARSENAL STYLE (Comprehensive)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 5: Comprehensive Clinical Tables (arsenal) ===\n")
cat("Purpose: Detailed summary statistics\n")
cat("Table style: arsenal\n")
cat("Similar to: SAS PROC TABULATE\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, tumor_size, ki67, response),
  group = "treatment",
  sty = "arsenal"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 6: HMISC STYLE (Statistical Focus)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 6: Statistical Analysis Focus (hmisc) ===\n")
cat("Purpose: Academic research tables\n")
cat("Table style: hmisc (Harrell's approach)\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, bmi, hemoglobin, creatinine),
  group = "stage",
  sty = "hmisc"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 7: CONTINUOUS VARIABLES - MEAN (Default)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 7: Continuous Variables with Means ===\n")
cat("Purpose: Compare means across groups (ANOVA)\n")
cat("Summary: mean (SD)\n")
cat("Test: ANOVA for 3+ groups\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, tumor_size, ki67),
  group = "treatment",
  sty = "nejm",
  cont = "mean"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 8: CONTINUOUS VARIABLES - MEDIAN
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 8: Continuous Variables with Medians ===\n")
cat("Purpose: Skewed data - use median instead of mean\n")
cat("Summary: median (IQR)\n")
cat("Variables: PSA (log-normal), survival (right-skewed)\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(psa, survival_months, wbc),
  group = "treatment",
  sty = "finalfit",
  cont = "median"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 9: MIXED CATEGORICAL AND CONTINUOUS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 9: Mixed Variable Types ===\n")
cat("Purpose: Combine categorical and continuous in one table\n")
cat("Categorical: Chi-square test\n")
cat("Continuous: ANOVA\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, sex, response, tumor_size, complications),
  group = "stage",
  sty = "gtsummary",
  cont = "mean"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 10: BINARY GROUPING (t-test)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 10: Binary Grouping Variable (2 groups) ===\n")
cat("Purpose: Compare outcomes between Yes/No recurrence\n")
cat("Test: t-test (continuous), chi-square (categorical)\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, tumor_size, survival_months, grade, response),
  group = "recurrence",
  sty = "nejm",
  cont = "mean"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 11: FISHER'S EXACT TEST (Small Frequencies)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 11: Fisher's Exact Test (Small Expected Frequencies) ===\n")
cat("Purpose: Force Fisher's exact test\n")
cat("Use: rare_event has only 5% in 'Yes' category\n")
cat("When: Expected cell frequencies < 5\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(rare_event, mutation),
  group = "recurrence",
  sty = "finalfit",
  pcat = "fisher"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 12: MULTIPLE TESTING CORRECTION (gtsummary only)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 12: Multiple Testing Correction ===\n")
cat("Purpose: Adjust p-values for multiple comparisons\n")
cat("Method: Benjamini-Hochberg FDR\n")
cat("Note: Only available with gtsummary style\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(response, grade, mutation, complications, ecog),
  group = "treatment",
  sty = "gtsummary",
  p_adjust = "BH"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 13: BONFERRONI CORRECTION
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 13: Bonferroni Correction (Conservative) ===\n")
cat("Purpose: Most stringent multiple testing correction\n")
cat("Method: Bonferroni\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, bmi, tumor_size, ki67, hemoglobin),
  group = "stage",
  sty = "gtsummary",
  p_adjust = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 14: HOLM CORRECTION
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 14: Holm Correction (Less Conservative) ===\n")
cat("Purpose: Step-down Bonferroni procedure\n")
cat("Method: Holm\n")
cat("More powerful than Bonferroni\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(response, grade, histology, mutation),
  group = "treatment",
  sty = "gtsummary",
  p_adjust = "holm"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 15: VARIABLES WITH SPECIAL CHARACTERS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 15: Variable Names with Spaces/Special Characters ===\n")
cat("Purpose: Test variable name handling\n")
cat("Variables: `Treatment Response`, `Age (years)`\n")
cat("Note: Function automatically handles special characters\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(`Treatment Response`, `Age (years)`),
  group = "treatment",
  sty = "lancet"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 16: FOUR-LEVEL GROUPING (Stage)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 16: Four-Level Grouping Variable ===\n")
cat("Purpose: Compare across tumor stages (I-IV)\n")
cat("Test: ANOVA for continuous, chi-square for categorical\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(tumor_size, survival_months, grade, response),
  group = "stage",
  sty = "nejm",
  cont = "mean"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("CLINICAL RESEARCH SCENARIOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Scenario 1: Manuscript Table 1 - Baseline Characteristics
cat("Scenario 1: Manuscript Table 1 - Baseline Characteristics\n")
cat("──────────────────────────────────────────────────\n")
cat("Purpose: Compare baseline patient characteristics across treatment arms\n")
cat("Use: Clinical trial manuscript\n")
cat("Style: Finalfit (publication-ready)\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, sex, smoking, ecog, stage, grade, tumor_size, ki67),
  group = "treatment",
  sty = "finalfit",
  cont = "mean"
)

cat("\n\n")

# Scenario 2: Treatment Efficacy Analysis
cat("Scenario 2: Treatment Efficacy Analysis\n")
cat("──────────────────────────────────────────────────\n")
cat("Purpose: Compare outcomes across treatment groups\n")
cat("Variables: Response, survival, QOL, complications\n")
cat("Style: NEJM\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(response, survival_months, qol_score, complications),
  group = "treatment",
  sty = "nejm",
  cont = "median"
)

cat("\n\n")

# Scenario 3: Prognostic Factor Analysis
cat("Scenario 3: Prognostic Factor Analysis by Stage\n")
cat("──────────────────────────────────────────────────\n")
cat("Purpose: Examine tumor characteristics by stage\n")
cat("Variables: Size, grade, Ki-67, histology\n")
cat("Style: gtsummary with FDR correction\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(tumor_size, grade, ki67, histology, mutation),
  group = "stage",
  sty = "gtsummary",
  cont = "median",
  p_adjust = "BH"
)

cat("\n\n")

# Scenario 4: Subgroup Analysis by Mutation Status
cat("Scenario 4: Subgroup Analysis by Mutation Status\n")
cat("──────────────────────────────────────────────────\n")
cat("Purpose: Compare wild-type vs mutated patients\n")
cat("Variables: Demographics, treatment, outcomes\n")
cat("Style: Lancet\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(age, sex, treatment, response, survival_months),
  group = "mutation",
  sty = "lancet",
  cont = "median"
)

cat("\n\n")

# Scenario 5: Safety Analysis
cat("Scenario 5: Safety Analysis - Complications by Treatment\n")
cat("──────────────────────────────────────────────────\n")
cat("Purpose: Safety profile comparison\n")
cat("Variables: Complications, adverse events, lab values\n")
cat("Style: arsenal\n\n")

crosstable(
  data = crosstable_test,
  vars = vars(complications, rare_event, hemoglobin, creatinine, wbc),
  group = "treatment",
  sty = "arsenal",
  cont = "mean"
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("INTERPRETATION GUIDE\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("1. TABLE STYLE SELECTION:\n\n")

cat("   NEJM / Lancet:\n")
cat("     ✓ Journal submission to these specific journals\n")
cat("     ✓ Medical manuscript Table 1\n")
cat("     ✓ Professional clinical presentation\n\n")

cat("   Finalfit:\n")
cat("     ✓ Clean, simple presentation\n")
cat("     ✓ Emphasis on clarity over detail\n")
cat("     ✓ General manuscript submission\n")
cat("     ✓ Good default choice\n\n")

cat("   gtsummary:\n")
cat("     ✓ Modern, highly customizable\n")
cat("     ✓ ONLY option with p-value adjustment\n")
cat("     ✓ Many variables tested simultaneously\n")
cat("     ✓ Exploratory analysis\n\n")

cat("   arsenal:\n")
cat("     ✓ Comprehensive clinical tables\n")
cat("     ✓ Detailed summary statistics\n")
cat("     ✓ Similar to SAS output\n")
cat("     ✓ Internal clinical reports\n\n")

cat("   hmisc:\n")
cat("     ✓ Statistical analysis focus\n")
cat("     ✓ Academic research orientation\n")
cat("     ✓ Comprehensive test results\n\n")

cat("2. CONTINUOUS VARIABLE SUMMARIES:\n\n")

cat("   Mean (SD) - Use when:\n")
cat("     ✓ Data is normally distributed\n")
cat("     ✓ Symmetric distribution\n")
cat("     ✓ No extreme outliers\n")
cat("     ✓ Examples: age, BMI, hemoglobin\n\n")

cat("   Median (IQR) - Use when:\n")
cat("     ✓ Data is skewed (right or left)\n")
cat("     ✓ Presence of outliers\n")
cat("     ✓ Non-normal distribution\n")
cat("     ✓ Examples: PSA, survival time, hospital charges\n\n")

cat("3. STATISTICAL TESTS:\n\n")

cat("   Automatic selection for CATEGORICAL variables:\n")
cat("     • Chi-square test: Expected frequencies ≥ 5 in all cells\n")
cat("     • Fisher's exact test: Any expected frequency < 5\n")
cat("     • Manual override: Use pcat = 'fisher' to force exact test\n\n")

cat("   Automatic selection for CONTINUOUS variables:\n")
cat("     • t-test: 2 groups (e.g., recurrence Yes/No)\n")
cat("     • ANOVA: 3+ groups (e.g., treatment, stage)\n")
cat("     • Assumes: Independent observations, roughly normal distribution\n\n")

cat("4. P-VALUE INTERPRETATION:\n\n")

cat("   Without adjustment:\n")
cat("     • p < 0.05: Significant difference between groups\n")
cat("     • p ≥ 0.05: No evidence of difference\n")
cat("     • Interpret each test independently\n\n")

cat("   With adjustment (gtsummary only):\n")
cat("     • Adjusted p-value accounts for multiple testing\n")
cat("     • Bonferroni: Most conservative (divide α by number of tests)\n")
cat("     • Holm: Less conservative, more power\n")
cat("     • BH/FDR: Least conservative, controls false discovery rate\n")
cat("     • BY: Very conservative FDR control\n\n")

cat("5. MULTIPLE TESTING CONSIDERATIONS:\n\n")

cat("   When to adjust p-values:\n")
cat("     ✓ Testing many variables (>5) simultaneously\n")
cat("     ✓ Exploratory analysis with multiple outcomes\n")
cat("     ✓ Subgroup analyses\n")
cat("     ✓ Want to control family-wise error rate\n\n")

cat("   When NOT to adjust:\n")
cat("     • Pre-specified primary endpoint\n")
cat("     • Confirmatory analysis (single hypothesis)\n")
cat("     • Descriptive Table 1 (baseline characteristics)\n")
cat("     • Each test answers independent question\n\n")

cat("6. MISSING DATA:\n\n")

cat("   Default behavior (excl = FALSE):\n")
cat("     • Missing values included in table\n")
cat("     • Shown as separate category or noted\n")
cat("     • Tests exclude missing by default\n\n")

cat("   With excl = TRUE:\n")
cat("     • Rows with missing values completely excluded\n")
cat("     • May reduce sample size substantially\n")
cat("     • Use when complete case analysis required\n\n")

cat("7. REPORTING RESULTS:\n\n")

cat("   Essential elements for manuscripts:\n")
cat("     ✓ Table title describing comparison\n")
cat("     ✓ Column headers (group names)\n")
cat("     ✓ Row labels (variable names)\n")
cat("     ✓ Summary statistics (n (%), mean (SD), median (IQR))\n")
cat("     ✓ P-values (specify test used)\n")
cat("     ✓ Sample sizes per group\n")
cat("     ✓ Footnote: statistical tests used\n")
cat("     ✓ Footnote: missing data handling\n\n")

cat("   Example footnote:\n")
cat('     \"Continuous variables presented as mean (SD) or median (IQR).\n')
cat('     Categorical variables presented as n (%). P-values from\n')
cat('     ANOVA/t-test for continuous, chi-square/Fisher exact for\n')
cat('     categorical variables.\"\n\n')

cat("8. COMMON PITFALLS:\n\n")

cat("   ❌ AVOID:\n")
cat("     • Using mean for highly skewed data (use median)\n")
cat("     • Not adjusting for multiple testing in exploratory analysis\n")
cat("     • Over-interpreting borderline p-values (p ~ 0.05)\n")
cat("     • Ignoring clinical significance vs statistical significance\n")
cat("     • Testing baseline characteristics in RCTs (not hypothesis tests!)\n")
cat("     • Using chi-square with small expected frequencies (<5)\n\n")

cat("   ✓ BEST PRACTICES:\n")
cat("     • Choose table style based on submission target\n")
cat("     • Use median for skewed data (PSA, survival, costs)\n")
cat("     • Report both n and % for categorical variables\n")
cat("     • Include total sample sizes\n")
cat("     • Document missing data clearly\n")
cat("     • Use Fisher's exact for small cell counts\n")
cat("     • Consider clinical importance, not just p-values\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("STATISTICAL NOTES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Baseline Characteristics Tables (Table 1):\n")
cat("  • Purpose: Show groups are comparable at baseline\n")
cat("  • In RCTs: P-values often NOT reported (randomization ensures balance)\n")
cat("  • In observational studies: P-values show baseline differences\n")
cat("  • Do NOT interpret significant p-values as treatment effects\n\n")

cat("Multiple Comparisons:\n")
cat("  • Family-wise error rate (FWER): Probability of ≥1 false positive\n")
cat("  • False discovery rate (FDR): Expected proportion of false positives\n")
cat("  • 5 tests at α=0.05 without adjustment: FWER ≈ 23%\n")
cat("  • Bonferroni: Controls FWER (stringent)\n")
cat("  • BH/FDR: Controls FDR (less stringent, more power)\n\n")

cat("Effect Size vs Significance:\n")
cat("  • Statistical significance (p-value): Probability result due to chance\n")
cat("  • Clinical significance: Magnitude of difference that matters\n")
cat("  • Large sample: Small differences can be statistically significant\n")
cat("  • Small sample: Large differences may not be statistically significant\n")
cat("  • Always consider both statistical AND clinical significance\n\n")

cat("Assumptions:\n")
cat("  • Independence: Observations are independent (no repeated measures)\n")
cat("  • t-test/ANOVA: Approximately normal distributions, equal variances\n")
cat("  • Chi-square: Expected frequencies ≥ 5 in each cell\n")
cat("  • Fisher's exact: No assumptions about sample size (always valid)\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("REFERENCES & FURTHER READING\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Table Creation Packages:\n")
cat("• arsenal: https://cran.r-project.org/package=arsenal\n")
cat("• finalfit: https://finalfit.org/\n")
cat("• gtsummary: https://www.danieldsjoberg.com/gtsummary/\n\n")

cat("Statistical Methods:\n")
cat("• Agresti A. (2013). Categorical Data Analysis, 3rd ed. Wiley.\n")
cat("• Harrell FE. (2015). Regression Modeling Strategies, 2nd ed. Springer.\n\n")

cat("Multiple Testing:\n")
cat("• Benjamini Y, Hochberg Y. (1995). Controlling the false discovery\n")
cat("  rate. J R Stat Soc Series B 57(1):289-300.\n")
cat("• Holm S. (1979). A simple sequentially rejective multiple test\n")
cat("  procedure. Scand J Stat 6(2):65-70.\n\n")

cat("Reporting Guidelines:\n")
cat("• CONSORT Statement (Clinical Trials): http://www.consort-statement.org/\n")
cat("• STROBE Statement (Observational Studies): https://www.strobe-statement.org/\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("For more information:\n")
cat("  ?crosstable\n")
cat("  help(package='ClinicoPath')\n")
cat("  https://www.serdarbalci.com/ClinicoPath/\n")
cat("═══════════════════════════════════════════════════════════\n")
