# ═══════════════════════════════════════════════════════════
# Example Usage: chisqposttest
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating chi-square post-hoc tests
# with multiple testing correction methods for clinical research
#
# Generated: 2026-01-04

library(ClinicoPath)

# Load test data
data(chisqposttest_test, package = "ClinicoPath")
data(chisqposttest_aggregated, package = "ClinicoPath")

# View data structure
str(chisqposttest_test)
summary(chisqposttest_test)

# ═══════════════════════════════════════════════════════════
# WHAT IS CHI-SQUARE POST-HOC TESTING?
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
CHI-SQUARE POST-HOC TESTS - Background
═══════════════════════════════════════════════════════════

The chi-square test of independence examines whether two categorical
variables are associated (dependent) or independent.

OVERALL CHI-SQUARE TEST:
  H₀: The two variables are independent
  H₁: The two variables are associated

  If χ² test is significant (p < α):
    → Variables ARE associated
    → But which specific groups differ?
    → POST-HOC TESTS answer this question

POST-HOC PAIRWISE COMPARISONS:
  - ONLY performed when overall χ² is significant
  - Compares each pair of groups individually
  - Uses 2×2 chi-square or Fisher's exact test
  - Applies multiple testing correction

MULTIPLE TESTING PROBLEM:
  - More comparisons → Higher chance of Type I error
  - Example: 3 groups = 3 pairwise comparisons
  - Without correction: α = 0.05 per test
  - Family-wise error rate: 1 - (1-0.05)³ = 14.3%

CORRECTION METHODS AVAILABLE:

1. BONFERRONI (Most Conservative)
   - Adjusted α = α / number of comparisons
   - Example: 0.05 / 3 = 0.0167 per test
   - Controls family-wise error rate (FWER)
   - Use when: Type I errors are very costly

2. HOLM (Less Conservative)
   - Sequential Bonferroni method
   - More powerful than Bonferroni
   - Still controls FWER
   - Use when: Balance between power and control

3. FDR - False Discovery Rate
   - Benjamini-Hochberg procedure
   - Controls proportion of false positives
   - More powerful than Bonferroni/Holm
   - Use when: Exploratory analysis, many tests

4. NONE
   - DISABLES all post-hoc testing
   - Not the same as \"no adjustment\"
   - Use when: Only overall test matters

ASSUMPTIONS:
  ✓ Categorical (nominal or ordinal) variables
  ✓ Independent observations
  ✓ Expected frequencies ≥ 5 in each cell (for χ²)
  ✓ If expected < 5: Use Fisher's exact test

RESIDUALS ANALYSIS:
  - Identifies which cells contribute to significance
  - Standardized residuals (adjusted for marginals)
  - |residual| > 2: Cell differs from expected
  - Complements pairwise comparisons

WHEN TO USE POST-HOC TESTS:
  ✓ Overall chi-square is significant (p < α)
  ✓ Variables have >2 levels (for meaningful pairwise tests)
  ✓ Research question asks \"which groups differ?\"

  ✗ DON'T use if overall χ² is non-significant
  ✗ DON'T use for 2×2 tables (already pairwise!)

═══════════════════════════════════════════════════════════
\n")

# ═══════════════════════════════════════════════════════════
# EXAMPLE 1: STRONG ASSOCIATION - Post-hoc Tests Run
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 1: Treatment Response (3×4 Table, Strong Association) ===\n")
cat("Purpose: Compare treatment groups on response outcomes\n")
cat("Groups: 3 treatments × 4 response categories\n")
cat("Expected: Significant overall χ² → post-hoc tests run\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "treatment",
  cols = "response",
  posthoc = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 2: USING AGGREGATED DATA WITH COUNTS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 2: Same Analysis with Pre-aggregated Data ===\n")
cat("Purpose: Demonstrate analysis with frequency counts\n")
cat("Data format: Aggregated (one row per cell combination)\n\n")

chisqposttest(
  data = chisqposttest_aggregated,
  rows = "treatment",
  cols = "response",
  counts = "count",
  posthoc = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 3: COMPARING ADJUSTMENT METHODS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 3: Bonferroni Correction (Most Conservative) ===\n")
chisqposttest(
  data = chisqposttest_test,
  rows = "grade",
  cols = "stage",
  posthoc = "bonferroni"
)

cat("\n=== Example 3b: Holm Correction (Less Conservative) ===\n")
chisqposttest(
  data = chisqposttest_test,
  rows = "grade",
  cols = "stage",
  posthoc = "holm"
)

cat("\n=== Example 3c: FDR Correction (False Discovery Rate) ===\n")
chisqposttest(
  data = chisqposttest_test,
  rows = "grade",
  cols = "stage",
  posthoc = "fdr"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 4: 2×2 TABLE (Post-hoc Not Applicable)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 4: Mutation Status × Outcome (2×2 Table) ===\n")
cat("Purpose: Binary comparison (post-hoc not meaningful)\n")
cat("Note: Overall χ² already tests the only possible pairwise comparison\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "mutation",
  cols = "outcome",
  posthoc = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 5: SMALL EXPECTED FREQUENCIES (Fisher's Exact)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 5: Rare Mutation × Adverse Events (Small Cells) ===\n")
cat("Purpose: Test with small expected frequencies\n")
cat("Expected: Auto-selection of Fisher's exact test\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "rare_mutation",
  cols = "adverse_event",
  testSelection = "auto"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 6: NO ASSOCIATION (Post-hoc Should NOT Run)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 6: Sex × Tumor Site (Independent Variables) ===\n")
cat("Purpose: Variables with no association\n")
cat("Expected: Non-significant overall χ² → NO post-hoc tests\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "sex",
  cols = "tumor_site",
  posthoc = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 7: WITH EXPECTED VALUES DISPLAY
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 7: Performance Status × Survival (Show Expected) ===\n")
cat("Purpose: Display expected cell frequencies\n")
cat("Use: Verify chi-square assumptions\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "ecog",
  cols = "survival",
  posthoc = "bonferroni",
  exp = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 8: WITH RESIDUALS ANALYSIS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 8: Risk Group × Complications (With Residuals) ===\n")
cat("Purpose: Identify which cells contribute to significance\n")
cat("Interpretation: |residual| > 2 indicates significant deviation\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "risk_group",
  cols = "complications",
  posthoc = "bonferroni",
  showResiduals = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 9: WITH RESIDUALS PLOT
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 9: Histology × Biomarker (Residuals Plot) ===\n")
cat("Purpose: Visual representation of standardized residuals\n")
cat("Colors: Blue = under-represented, Red = over-represented\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "histology",
  cols = "biomarker_expression",
  posthoc = "bonferroni",
  plot = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 10: MODERATE ASSOCIATION
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 10: Age Group × Disease Severity (Moderate) ===\n")
cat("Purpose: Moderate association - some pairwise differences\n")
cat("Expected: Overall significant, adjustment method affects which pairs\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "age_group",
  cols = "severity",
  posthoc = "holm"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 11: WEAK ASSOCIATION
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 11: Smoking × Cancer Type (Weak Association) ===\n")
cat("Purpose: Weak association - borderline significance\n")
cat("Expected: May or may not be significant depending on sample\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "smoking",
  cols = "cancer_type",
  posthoc = "fdr"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 12: DIFFERENT SIGNIFICANCE LEVELS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 12: Custom Significance Level (α = 0.01) ===\n")
cat("Purpose: More stringent significance threshold\n")
cat("Use: Reduce Type I error in confirmatory research\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "treatment",
  cols = "response",
  posthoc = "bonferroni",
  sig = 0.01
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 13: WITH EDUCATIONAL PANELS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 13: With Educational Guidance ===\n")
cat("Purpose: Display interpretation help for non-statisticians\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "mutation",
  cols = "outcome",
  posthoc = "bonferroni",
  showEducational = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 14: EXCLUDING MISSING DATA
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 14: Biomarker Analysis (Exclude Missing) ===\n")
cat("Purpose: Demonstrate missing data handling\n")
cat("Note: Missing data automatically excluded by default\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "histology",
  cols = "biomarker_expression",
  posthoc = "bonferroni",
  excl = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 15: DISABLE POST-HOC TESTS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 15: Overall Test Only (No Post-hoc) ===\n")
cat("Purpose: Only interested in overall association\n")
cat("Method: Set posthoc = 'none'\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "treatment",
  cols = "response",
  posthoc = "none"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 16: FORCE FISHER'S EXACT TEST
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 16: Force Fisher's Exact Test ===\n")
cat("Purpose: Use exact test regardless of sample size\n")
cat("Use: Small samples or categorical data without assumptions\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "mutation",
  cols = "outcome",
  testSelection = "fisher",
  posthoc = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 17: ALWAYS USE CHI-SQUARE
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 17: Force Chi-Square Test ===\n")
cat("Purpose: Always use χ² even with small expected frequencies\n")
cat("Warning: May violate assumptions if cells < 5\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "rare_mutation",
  cols = "adverse_event",
  testSelection = "chisquare"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("CLINICAL RESEARCH SCENARIOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Scenario 1: Clinical Trial Efficacy
cat("Scenario 1: Treatment Efficacy Comparison\n")
cat("──────────────────────────────────────────────────\n")
cat("Research Question: Which treatment produces best response?\n")
cat("Design: Randomized trial with 3 arms, 4 response categories\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "treatment",
  cols = "response",
  posthoc = "bonferroni",
  showResiduals = TRUE
)

cat("\n\n")

# Scenario 2: Prognostic Factor Analysis
cat("Scenario 2: Tumor Grade as Prognostic Factor\n")
cat("──────────────────────────────────────────────────\n")
cat("Research Question: Does grade predict stage at diagnosis?\n")
cat("Design: Cross-sectional analysis of pathology records\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "grade",
  cols = "stage",
  posthoc = "holm",
  showResiduals = TRUE
)

cat("\n\n")

# Scenario 3: Safety Analysis
cat("Scenario 3: Complication Risk by Patient Group\n")
cat("──────────────────────────────────────────────────\n")
cat("Research Question: Which risk groups have higher complication rates?\n")
cat("Design: Retrospective cohort study\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "risk_group",
  cols = "complications",
  posthoc = "bonferroni",
  plot = TRUE
)

cat("\n\n")

# Scenario 4: Biomarker Association
cat("Scenario 4: Mutation Status and Treatment Outcome\n")
cat("──────────────────────────────────────────────────\n")
cat("Research Question: Do mutated patients respond better?\n")
cat("Design: Biomarker-directed therapy study\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "mutation",
  cols = "outcome",
  posthoc = "bonferroni",
  exp = TRUE
)

cat("\n\n")

# Scenario 5: Survival Analysis
cat("Scenario 5: Performance Status Predicts Survival\n")
cat("──────────────────────────────────────────────────\n")
cat("Research Question: Does baseline performance predict mortality?\n")
cat("Design: Prospective cohort with survival endpoint\n\n")

chisqposttest(
  data = chisqposttest_test,
  rows = "ecog",
  cols = "survival",
  posthoc = "fdr",
  showResiduals = TRUE
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("INTERPRETATION GUIDE\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("1. OVERALL CHI-SQUARE TEST:\n\n")

cat("   χ² statistic:\n")
cat("     • Measures overall association strength\n")
cat("     • Larger χ² = stronger association\n")
cat("     • Depends on sample size (larger n → larger χ²)\n\n")

cat("   Degrees of freedom (df):\n")
cat("     • df = (rows - 1) × (columns - 1)\n")
cat("     • Example: 3×4 table → df = (3-1)×(4-1) = 6\n\n")

cat("   p-value interpretation:\n")
cat("     • p < 0.05: Variables ARE associated (reject H₀)\n")
cat("     • p ≥ 0.05: No evidence of association (fail to reject H₀)\n")
cat("     • Post-hoc tests ONLY run if p < α\n\n")

cat("2. POST-HOC PAIRWISE COMPARISONS:\n\n")

cat("   Number of comparisons:\n")
cat("     • k groups → k(k-1)/2 pairwise tests\n")
cat("     • 3 groups → 3 comparisons\n")
cat("     • 4 groups → 6 comparisons\n")
cat("     • 5 groups → 10 comparisons\n\n")

cat("   Adjusted p-values:\n")
cat("     • Bonferroni: p_adj = p_raw × n_comparisons\n")
cat("     • Holm: Sequential Bonferroni (less conservative)\n")
cat("     • FDR: Benjamini-Hochberg (least conservative)\n\n")

cat("   Significant pairwise difference:\n")
cat("     • p_adjusted < α (usually 0.05)\n")
cat("     • Indicates those two groups differ significantly\n")
cat("     • Check which direction (see contingency table)\n\n")

cat("3. EFFECT SIZES:\n\n")

cat("   Cramér's V:\n")
cat("     • V = 0: No association\n")
cat("     • V = 0.1: Small effect\n")
cat("     • V = 0.3: Medium effect\n")
cat("     • V = 0.5: Large effect\n")
cat("     • Range: 0 to 1\n\n")

cat("   Phi coefficient (2×2 tables only):\n")
cat("     • φ = 0: No association\n")
cat("     • |φ| = 0.1: Small\n")
cat("     • |φ| = 0.3: Medium\n")
cat("     • |φ| = 0.5: Large\n")
cat("     • Range: -1 to +1\n\n")

cat("4. STANDARDIZED RESIDUALS:\n\n")

cat("   Interpretation:\n")
cat("     • Residual = (Observed - Expected) / SE\n")
cat("     • |residual| > 2: Cell significantly different from expected\n")
cat("     • |residual| > 3: Very strong deviation\n")
cat("     • Positive: Over-represented (more than expected)\n")
cat("     • Negative: Under-represented (fewer than expected)\n\n")

cat("   Use residuals to:\n")
cat("     • Identify which cells drive overall significance\n")
cat("     • Understand pattern of association\n")
cat("     • Complement pairwise comparisons\n\n")

cat("5. ASSUMPTIONS AND VALIDITY:\n\n")

cat("   Chi-square assumptions:\n")
cat("     ✓ Independent observations (no repeated measures)\n")
cat("     ✓ Expected frequency ≥ 5 in each cell (80% rule)\n")
cat("     ✓ Categorical (nominal or ordinal) variables\n")
cat("     ✓ Mutually exclusive categories\n\n")

cat("   When expected < 5:\n")
cat("     ⚠ Chi-square may be invalid\n")
cat("     ✓ Use Fisher's exact test instead\n")
cat("     ✓ Or collapse sparse categories\n\n")

cat("   Sample size guidelines:\n")
cat("     • Minimum: n > 5 × (rows × cols)\n")
cat("     • Adequate: n > 10 × (rows × cols)\n")
cat("     • Ideal: Expected ≥ 10 in all cells\n\n")

cat("6. CHOOSING CORRECTION METHOD:\n\n")

cat("   BONFERRONI - Use when:\n")
cat("     • Confirmatory research\n")
cat("     • Type I errors are very costly\n")
cat("     • Few comparisons (<5)\n")
cat("     • Need strong control of FWER\n\n")

cat("   HOLM - Use when:\n")
cat("     • Balance power and control needed\n")
cat("     • More powerful than Bonferroni\n")
cat("     • Still controls FWER\n")
cat("     • Recommended default\n\n")

cat("   FDR - Use when:\n")
cat("     • Exploratory research\n")
cat("     • Many comparisons (>10)\n")
cat("     • Acceptable to have some false positives\n")
cat("     • Genomics/high-throughput screening\n\n")

cat("   NONE - Use when:\n")
cat("     • Only overall association matters\n")
cat("     • Not interested in pairwise differences\n")
cat("     • 2×2 table (post-hoc not meaningful)\n\n")

cat("7. REPORTING RESULTS:\n\n")

cat("   Essential elements:\n")
cat("     ✓ Sample size (n)\n")
cat("     ✓ Contingency table (frequencies or %)\n")
cat("     ✓ Overall χ² statistic, df, p-value\n")
cat("     ✓ Effect size (Cramér's V or φ)\n")
cat("     ✓ Post-hoc correction method used\n")
cat("     ✓ Which pairwise comparisons were significant\n\n")

cat("   Example reporting:\n")
cat('     \"A chi-square test revealed a significant association between\n')
cat('     treatment group and response (χ²(6) = 45.2, p < 0.001, V = 0.38).\n')
cat('     Post-hoc pairwise comparisons with Bonferroni correction showed\n')
cat('     Drug A produced significantly better response than Control\n')
cat('     (p_adj = 0.003) and Drug B (p_adj = 0.012).\"\n\n')

cat("8. COMMON PITFALLS:\n\n")

cat("   ❌ AVOID:\n")
cat("     • Running post-hoc when overall χ² is non-significant\n")
cat("     • Using unadjusted p-values for multiple comparisons\n")
cat("     • Ignoring small expected frequency warnings\n")
cat("     • Interpreting χ² p-value as effect size\n")
cat("     • Using chi-square for continuous outcomes\n")
cat("     • Ignoring which direction differences go\n\n")

cat("   ✓ BEST PRACTICES:\n")
cat("     • Check assumptions before interpreting\n")
cat("     • Report both overall and pairwise results\n")
cat("     • Include effect sizes (not just p-values)\n")
cat("     • Use residuals to understand patterns\n")
cat("     • Document correction method choice\n")
cat("     • Consider clinical significance, not just statistical\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("STATISTICAL NOTES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Post-hoc Workflow (Proper Statistical Practice):\n\n")

cat("Step 1: Run overall chi-square test\n")
cat("  → If p ≥ α: STOP. No pairwise testing justified.\n")
cat("  → If p < α: Proceed to Step 2.\n\n")

cat("Step 2: Check expected frequencies\n")
cat("  → If any expected < 5: Consider Fisher's exact\n")
cat("  → If all expected ≥ 5: Chi-square valid\n\n")

cat("Step 3: Run pairwise comparisons\n")
cat("  → For each pair: 2×2 chi-square or Fisher's exact\n")
cat("  → Obtain raw p-values\n\n")

cat("Step 4: Apply multiple testing correction\n")
cat("  → Bonferroni: Multiply by number of tests\n")
cat("  → Holm: Sequential Bonferroni\n")
cat("  → FDR: Benjamini-Hochberg procedure\n\n")

cat("Step 5: Interpret adjusted p-values\n")
cat("  → p_adj < α: Pairwise difference is significant\n")
cat("  → p_adj ≥ α: No evidence of pairwise difference\n\n")

cat("Type I vs Type II Error Trade-off:\n")
cat("  • More conservative correction → fewer Type I errors\n")
cat("  • More conservative correction → more Type II errors (less power)\n")
cat("  • Bonferroni: Lowest Type I, highest Type II\n")
cat("  • FDR: Higher Type I, lower Type II\n\n")

cat("When 2×2 Tables Don't Need Post-hoc:\n")
cat("  • Only one pairwise comparison possible\n")
cat("  • Overall χ² already tests this comparison\n")
cat("  • Post-hoc testing is redundant\n")
cat("  • Example: Mutation (Yes/No) × Outcome (Responder/Non-responder)\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("REFERENCES & FURTHER READING\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Methodology:\n")
cat("• Agresti A. (2013). Categorical Data Analysis, 3rd ed. Wiley.\n\n")

cat("• Sharpe D. (2015). Chi-square test is statistically significant:\n")
cat("  Now what? Practical Assessment, Research & Evaluation 20(8).\n\n")

cat("• García-Pérez MA, Núñez-Antón V. (2003). Cellwise residual\n")
cat("  analysis in two-way contingency tables. Educational and\n")
cat("  Psychological Measurement 63(5):825-839.\n\n")

cat("Multiple Testing Correction:\n")
cat("• Holm S. (1979). A simple sequentially rejective multiple\n")
cat("  test procedure. Scandinavian Journal of Statistics 6(2):65-70.\n\n")

cat("• Benjamini Y, Hochberg Y. (1995). Controlling the false\n")
cat("  discovery rate. Journal of the Royal Statistical Society\n")
cat("  Series B 57(1):289-300.\n\n")

cat("Effect Sizes:\n")
cat("• Cohen J. (1988). Statistical Power Analysis for the\n")
cat("  Behavioral Sciences, 2nd ed. Routledge.\n\n")

cat("• Cramér H. (1946). Mathematical Methods of Statistics.\n")
cat("  Princeton University Press.\n\n")

cat("R Packages:\n")
cat("• stats - Base R chi-square and Fisher's exact tests\n")
cat("• vcd - Visualizing categorical data\n")
cat("• DescTools - PostHocTest function\n\n")

cat("Online Resources:\n")
cat("• Jamovi forum post-hoc discussions:\n")
cat("  https://forum.jamovi.org/\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("For more information:\n")
cat("  ?chisqposttest\n")
cat("  help(package='ClinicoPath')\n")
cat("  https://www.serdarbalci.com/ClinicoPath/\n")
cat("═══════════════════════════════════════════════════════════\n")
