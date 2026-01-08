# ═══════════════════════════════════════════════════════════
# Example Usage: categorize
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating continuous variable categorization
# using various binning methods for clinical research
#
# Generated: 2026-01-04

library(ClinicoPath)

# Load test data
data(categorize_test, package = "ClinicoPath")

# View data structure
str(categorize_test)
summary(categorize_test)

# ═══════════════════════════════════════════════════════════
# WHAT IS VARIABLE CATEGORIZATION?
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
VARIABLE CATEGORIZATION - Background
═══════════════════════════════════════════════════════════

Categorization (binning) converts continuous numeric variables into
categorical variables with defined groups or bins.

WHY CATEGORIZE?
  ✓ Create clinically meaningful groups (Low/Normal/High)
  ✓ Simplify interpretation for non-statisticians
  ✓ Enable stratified analysis
  ✓ Meet analysis requirements (e.g., chi-square tests)
  ✓ Reduce impact of outliers
  ✓ Create risk categories

CAUTION - Information Loss:
  ⚠ Categorization discards information
  ⚠ Reduces statistical power
  ⚠ Can introduce bias if done arbitrarily
  ⚠ Use continuous variables in analysis when possible

When categorization is justified:
  • Clinical guidelines define categories (BMI, BP)
  • Creating risk stratification groups
  • Presenting results to non-technical audiences
  • Meeting specific analytical requirements
  • Reducing complexity for decision-making

BINNING METHODS AVAILABLE:

1. QUANTILE (default):
   - Divides data into equal-frequency bins
   - Each bin contains same number of observations
   - Good for: Tertiles, quartiles, quintiles
   - Example: Age quartiles for stratified analysis

2. EQUAL INTERVALS:
   - Divides range into equal-width bins
   - Bins may have different frequencies
   - Good for: Uniformly distributed data
   - Example: Temperature ranges (36-37°C, 37-38°C, etc.)

3. MANUAL BREAKS:
   - User-defined breakpoints
   - Based on clinical/scientific criteria
   - Good for: Established clinical categories
   - Example: BMI (18.5, 25, 30), PSA (4, 10, 20)

4. MEAN ± SD:
   - Categories based on standard deviations from mean
   - Good for: Normal distributions
   - Example: Low (<M-SD), Normal (M±SD), High (>M+SD)

5. MEDIAN SPLIT:
   - Divides at median (50th percentile)
   - Creates two equal-frequency groups
   - Good for: Simple binary categorization
   - Example: Young/Old, Small/Large tumors

6. JENKS NATURAL BREAKS:
   - Minimizes within-group variance
   - Maximizes between-group variance
   - Good for: Data with natural clusters
   - Example: Bimodal or irregular distributions

LABEL OPTIONS:

• Auto (default): Numeric ranges with brackets
  Example: \"[20,40)\", \"[40,60)\", \"[60,90]\"

• Semantic: Low, Medium-Low, Medium-High, High
  (adapts to number of bins)

• Numbered: 1, 2, 3, 4...

• Lettered: A, B, C, D...

• Custom: User-defined labels

INTERVAL NOTATION:

• (a, b] → Right-closed: a < x ≤ b (default)
• [a, b) → Left-closed: a ≤ x < b
• include.lowest controls first interval

═══════════════════════════════════════════════════════════
\n")

# ═══════════════════════════════════════════════════════════
# EXAMPLE 1: QUANTILE METHOD (Default)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 1: Age into Quartiles (Quantile Method) ===\n")
cat("Purpose: Divide patients into 4 equal-sized age groups\n")
cat("Method: quantile (default)\n")
cat("Use case: Stratified analysis by age group\n\n")

categorize(
  data = categorize_test,
  var = "age",
  method = "quantile",
  nbins = 4,
  labels = "semantic"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 2: EQUAL INTERVALS METHOD
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 2: Temperature into Equal Intervals ===\n")
cat("Purpose: Create equal-width temperature ranges\n")
cat("Method: equal\n")
cat("Use case: Fever classification\n\n")

categorize(
  data = categorize_test,
  var = "temperature",
  method = "equal",
  nbins = 4,
  labels = "auto"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 3: MANUAL BREAKS - BMI (WHO Classification)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 3: BMI with WHO Categories (Manual Breaks) ===\n")
cat("Purpose: Apply internationally recognized BMI categories\n")
cat("Method: manual\n")
cat("Breaks: 18.5 (underweight), 25 (normal), 30 (overweight)\n")
cat("Use case: Obesity classification\n\n")

categorize(
  data = categorize_test,
  var = "bmi",
  method = "manual",
  breaks = "15, 18.5, 25, 30, 45",
  labels = "custom",
  customlabels = "Underweight, Normal, Overweight, Obese"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 4: MANUAL BREAKS - PSA (Clinical Guidelines)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 4: PSA Risk Categories (Manual Breaks) ===\n")
cat("Purpose: Clinical PSA interpretation categories\n")
cat("Method: manual\n")
cat("Breaks: 4 ng/mL (normal upper limit), 10, 20\n")
cat("Use case: Prostate cancer risk stratification\n\n")

categorize(
  data = categorize_test,
  var = "psa_level",
  method = "manual",
  breaks = "0, 4, 10, 20, 100",
  labels = "custom",
  customlabels = "Normal, Borderline, Elevated, Very High"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 5: MEAN ± SD METHOD
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 5: Blood Pressure with Mean±SD ===\n")
cat("Purpose: Define normal range as within 1 SD from mean\n")
cat("Method: meansd\n")
cat("SD Multiplier: 1 (default)\n")
cat("Categories: Low (<M-SD), Normal (M±SD), High (>M+SD)\n\n")

categorize(
  data = categorize_test,
  var = "systolic_bp",
  method = "meansd",
  sdmult = 1,
  labels = "custom",
  customlabels = "Low, Normal, High"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 6: MEAN ± SD with Different Multiplier
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 6: Hemoglobin with Mean±1.5SD ===\n")
cat("Purpose: Wider normal range (±1.5 SD)\n")
cat("Method: meansd\n")
cat("SD Multiplier: 1.5\n\n")

categorize(
  data = categorize_test,
  var = "hemoglobin",
  method = "meansd",
  sdmult = 1.5,
  labels = "custom",
  customlabels = "Low, Normal, High"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 7: MEDIAN SPLIT
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 7: Tumor Size Median Split ===\n")
cat("Purpose: Binary categorization at median\n")
cat("Method: median\n")
cat("Use case: Small vs Large tumors for outcome comparison\n\n")

categorize(
  data = categorize_test,
  var = "tumor_size",
  method = "median",
  labels = "custom",
  customlabels = "Small, Large"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 8: JENKS NATURAL BREAKS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 8: Bimodal Distribution with Jenks Method ===\n")
cat("Purpose: Find natural groupings in data\n")
cat("Method: jenks (natural breaks optimization)\n")
cat("Use case: Data with clusters or irregular distributions\n\n")

categorize(
  data = categorize_test,
  var = "bimodal_dist",
  method = "jenks",
  nbins = 2,
  labels = "semantic"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 9: TERTILES (3 Groups)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 9: CA-125 Tertiles ===\n")
cat("Purpose: Divide biomarker into 3 equal-frequency groups\n")
cat("Method: quantile\n")
cat("Number of bins: 3\n\n")

categorize(
  data = categorize_test,
  var = "ca125",
  method = "quantile",
  nbins = 3,
  labels = "custom",
  customlabels = "Low, Medium, High"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 10: QUINTILES (5 Groups)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 10: Framingham Score Quintiles ===\n")
cat("Purpose: Fine-grained risk stratification\n")
cat("Method: quantile\n")
cat("Number of bins: 5\n\n")

categorize(
  data = categorize_test,
  var = "framingham_score",
  method = "quantile",
  nbins = 5,
  labels = "semantic"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 11: NUMBERED LABELS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 11: Hospital Charges with Numbered Labels ===\n")
cat("Purpose: Simple numeric categories\n")
cat("Method: quantile\n")
cat("Label style: numbered (1, 2, 3, 4)\n\n")

categorize(
  data = categorize_test,
  var = "hospital_charges",
  method = "quantile",
  nbins = 4,
  labels = "numbered"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 12: LETTERED LABELS
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 12: WBC Count with Lettered Labels ===\n")
cat("Purpose: Alphabetic categories\n")
cat("Method: quantile\n")
cat("Label style: lettered (A, B, C, D)\n\n")

categorize(
  data = categorize_test,
  var = "wbc_count",
  method = "quantile",
  nbins = 4,
  labels = "lettered"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 13: AUTO LABELS (Numeric Ranges)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 13: CRP with Auto Labels ===\n")
cat("Purpose: Show exact numeric ranges\n")
cat("Method: equal\n")
cat("Label style: auto (shows intervals)\n\n")

categorize(
  data = categorize_test,
  var = "crp",
  method = "equal",
  nbins = 4,
  labels = "auto"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 14: BINARY CATEGORIZATION (2 Groups)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 14: Ki-67 High vs Low (Binary) ===\n")
cat("Purpose: Dichotomization for prognostic grouping\n")
cat("Method: median\n")
cat("Groups: 2\n\n")

categorize(
  data = categorize_test,
  var = "ki67_index",
  method = "median",
  labels = "custom",
  customlabels = "Low Proliferation, High Proliferation"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 15: DECILES (10 Groups)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 15: Age Deciles ===\n")
cat("Purpose: Fine-grained age stratification\n")
cat("Method: quantile\n")
cat("Number of bins: 10\n\n")

categorize(
  data = categorize_test,
  var = "age",
  method = "quantile",
  nbins = 10,
  labels = "numbered"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 16: HANDLING SKEWED DATA
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 16: Skewed Data (CRP) with Quantiles ===\n")
cat("Purpose: Equal-frequency bins for right-skewed biomarker\n")
cat("Method: quantile (handles skewness well)\n")
cat("Note: Equal intervals would create unbalanced groups\n\n")

categorize(
  data = categorize_test,
  var = "crp",
  method = "quantile",
  nbins = 4,
  labels = "semantic"
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 17: COMPARING METHODS - Same Variable
# ═══════════════════════════════════════════════════════════

cat("\n=== COMPARISON: Different Methods on Same Variable ===\n\n")

cat("Method 1: QUANTILE (equal frequencies)\n")
categorize(data = categorize_test, var = "age", method = "quantile", nbins = 3)

cat("\n\nMethod 2: EQUAL INTERVALS (equal widths)\n")
categorize(data = categorize_test, var = "age", method = "equal", nbins = 3)

cat("\n\nMethod 3: MEAN±SD (statistical)\n")
categorize(data = categorize_test, var = "age", method = "meansd", sdmult = 1)

cat("\n\nMethod 4: MANUAL (clinically defined)\n")
categorize(data = categorize_test, var = "age", method = "manual",
          breaks = "20, 40, 65, 90",
          labels = "custom",
          customlabels = "Young Adult, Middle Aged, Senior")

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("CLINICAL RESEARCH SCENARIOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Scenario 1: Biomarker Quartiles for Survival Analysis
cat("Scenario 1: Creating PSA Quartiles for Survival Analysis\n")
cat("──────────────────────────────────────────────────\n")
cat("Goal: Compare survival across PSA quartile groups\n")
cat("Method: Quantile (ensures equal sample sizes)\n\n")

categorize(
  data = categorize_test,
  var = "psa_level",
  method = "quantile",
  nbins = 4,
  labels = "semantic"
)

cat("\n\n")

# Scenario 2: Risk Stratification
cat("Scenario 2: Cardiovascular Risk Stratification\n")
cat("──────────────────────────────────────────────────\n")
cat("Goal: Low/Moderate/High risk groups\n")
cat("Method: Quantile tertiles or manual breaks\n\n")

categorize(
  data = categorize_test,
  var = "framingham_score",
  method = "quantile",
  nbins = 3,
  labels = "custom",
  customlabels = "Low Risk, Moderate Risk, High Risk"
)

cat("\n\n")

# Scenario 3: Laboratory Reference Ranges
cat("Scenario 3: Creatinine - Normal vs Abnormal\n")
cat("──────────────────────────────────────────────────\n")
cat("Goal: Flag abnormal kidney function\n")
cat("Method: Manual break at clinical threshold (1.2 mg/dL)\n\n")

categorize(
  data = categorize_test,
  var = "creatinine",
  method = "manual",
  breaks = "0.5, 1.2, 8",
  labels = "custom",
  customlabels = "Normal, Elevated"
)

cat("\n\n")

# Scenario 4: Age-Adjusted Analysis
cat("Scenario 4: Age Groups for Age-Adjusted Analysis\n")
cat("──────────────────────────────────────────────────\n")
cat("Goal: Standard age categories for comparison\n")
cat("Method: Manual breaks at clinically meaningful ages\n\n")

categorize(
  data = categorize_test,
  var = "age",
  method = "manual",
  breaks = "20, 40, 60, 80, 90",
  labels = "custom",
  customlabels = "20-39, 40-59, 60-79, 80+"
)

cat("\n\n")

# Scenario 5: Outlier-Resistant Categorization
cat("Scenario 5: Tumor Size with Outliers\n")
cat("──────────────────────────────────────────────────\n")
cat("Goal: Create groups resistant to outliers\n")
cat("Method: Quantile (outliers don't affect bin boundaries)\n\n")

categorize(
  data = categorize_test,
  var = "tumor_size",
  method = "quantile",
  nbins = 3,
  labels = "custom",
  customlabels = "Small, Medium, Large"
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("INTERPRETATION GUIDE\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("1. METHOD SELECTION:\n\n")

cat("   QUANTILE - Use when:\n")
cat("     ✓ You need equal sample sizes per group\n")
cat("     ✓ Data is skewed (doesn't matter for quantiles)\n")
cat("     ✓ Creating tertiles/quartiles/quintiles\n")
cat("     ✓ Outliers are present (robust method)\n")
cat("     ✓ Stratified analysis requires balanced groups\n\n")

cat("   EQUAL INTERVALS - Use when:\n")
cat("     ✓ Data is uniformly distributed\n")
cat("     ✓ You want equal-width bins\n")
cat("     ✓ Range interpretation is important\n")
cat("     ✓ Example: Temperature ranges\n\n")

cat("   MANUAL BREAKS - Use when:\n")
cat("     ✓ Clinical guidelines define cutoffs\n")
cat("     ✓ Established reference ranges exist\n")
cat("     ✓ Regulatory/policy thresholds apply\n")
cat("     ✓ Examples: BMI (18.5, 25, 30), PSA (4, 10)\n\n")

cat("   MEAN±SD - Use when:\n")
cat("     ✓ Data is normally distributed\n")
cat("     ✓ Statistical definition of normal range needed\n")
cat("     ✓ Identifying outliers (>2 or >3 SD from mean)\n")
cat("     ✓ Example: Quality control in lab testing\n\n")

cat("   MEDIAN SPLIT - Use when:\n")
cat("     ✓ Simple binary categorization needed\n")
cat("     ✓ High vs Low dichotomy makes sense\n")
cat("     ✓ Equal group sizes are important\n")
cat("     ⚠ WARNING: Arbitrary cutoff, use with caution\n\n")

cat("   JENKS NATURAL BREAKS - Use when:\n")
cat("     ✓ Data has natural clusters\n")
cat("     ✓ Bimodal or multimodal distributions\n")
cat("     ✓ You want to minimize within-group variance\n")
cat("     ✓ Example: Geographic data, irregular distributions\n\n")

cat("2. NUMBER OF BINS:\n\n")

cat("   2 bins: Binary categorization (High/Low, Positive/Negative)\n")
cat("   3 bins: Tertiles or Low/Medium/High (common in risk stratification)\n")
cat("   4 bins: Quartiles (balanced, widely used)\n")
cat("   5 bins: Quintiles (finer stratification)\n")
cat("   10 bins: Deciles (detailed stratification)\n")
cat("   >10 bins: Usually not recommended (too many categories)\n\n")

cat("   General rule: Fewer bins = easier interpretation\n")
cat("                 More bins = more detail but less power per group\n\n")

cat("3. LABEL SELECTION:\n\n")

cat("   AUTO: Best for technical reports (shows exact ranges)\n")
cat("   SEMANTIC: Best for general audience (Low/Medium/High)\n")
cat("   NUMBERED: Best for ordinal analysis (preserves order)\n")
cat("   LETTERED: Best for anonymous categories (A/B/C)\n")
cat("   CUSTOM: Best for domain-specific terms (Underweight/Normal/Obese)\n\n")

cat("4. COMMON PITFALLS:\n\n")

cat("   ❌ AVOID:\n")
cat("     • Arbitrary cutoffs without justification\n")
cat("     • Too many categories (reduces power)\n")
cat("     • Ignoring clinical guidelines when available\n")
cat("     • Using equal intervals on skewed data\n")
cat("     • Categorizing when continuous analysis is better\n\n")

cat("   ✓ BEST PRACTICES:\n")
cat("     • Document rationale for binning method\n")
cat("     • Use established clinical cutoffs when available\n")
cat("     • Check bin frequencies (avoid very small groups)\n")
cat("     • Consider keeping continuous variable for sensitivity analysis\n")
cat("     • Report both categorized and continuous results\n\n")

cat("5. INTERPRETING RESULTS:\n\n")

cat("   Frequency Table:\n")
cat("     - Shows distribution across categories\n")
cat("     - Check for balanced/imbalanced groups\n")
cat("     - Identify if any category has too few observations (<10)\n\n")

cat("   Distribution Plot:\n")
cat("     - Visualizes original data with cut points\n")
cat("     - Helps assess appropriateness of binning\n")
cat("     - Shows if categories capture meaningful variation\n\n")

cat("   R Code:\n")
cat("     - Use to reproduce categorization\n")
cat("     - Can be applied to new datasets\n")
cat("     - Documents exact methodology\n\n")

cat("6. CLINICAL EXAMPLES:\n\n")

cat("   BMI Categories (Manual):\n")
cat("     Breaks: 18.5, 25, 30\n")
cat("     Labels: Underweight, Normal, Overweight, Obese\n")
cat("     Reference: WHO classification\n\n")

cat("   Blood Pressure (Manual):\n")
cat("     Systolic Breaks: 120, 130, 140, 180\n")
cat("     Labels: Normal, Elevated, Stage 1 HTN, Stage 2 HTN, Crisis\n")
cat("     Reference: ACC/AHA 2017 guidelines\n\n")

cat("   PSA Interpretation (Manual):\n")
cat("     Breaks: 4, 10, 20\n")
cat("     Labels: Normal, Borderline, Elevated, Very High\n")
cat("     Reference: Clinical practice\n\n")

cat("   Age Groups for Cancer Studies (Manual):\n")
cat("     Breaks: 40, 60, 75\n")
cat("     Labels: <40, 40-59, 60-74, 75+\n")
cat("     Reference: SEER age categories\n\n")

cat("   eGFR Stages (Manual):\n")
cat("     Breaks: 15, 30, 60, 90\n")
cat("     Labels: G5 (Kidney Failure), G4 (Severe), G3 (Moderate),\n")
cat("             G2 (Mild), G1 (Normal)\n")
cat("     Reference: KDIGO 2012\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("STATISTICAL NOTES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Loss of Information:\n")
cat("  - Categorization always loses information\n")
cat("  - Reduces statistical power (larger sample needed)\n")
cat("  - Can introduce bias if cutoffs are chosen arbitrarily\n")
cat("  - Use continuous variables in primary analysis when possible\n\n")

cat("Sample Size Considerations:\n")
cat("  - Minimum 10 observations per category recommended\n")
cat("  - More categories = smaller groups = less power\n")
cat("  - Consider collapsing sparse categories\n\n")

cat("Ordered vs Unordered Factors:\n")
cat("  - Use ordered=TRUE (default) to preserve ordering\n")
cat("  - Enables ordinal regression, trend tests\n")
cat("  - Set ordered=FALSE only for nominal categories\n\n")

cat("Missing Data:\n")
cat("  - Excluded by default (excl=TRUE)\n")
cat("  - Missing values shown in frequency table\n")
cat("  - Can be retained by setting excl=FALSE\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("REFERENCES & FURTHER READING\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Methodology:\n")
cat("• Altman DG, Royston P. (2006). The cost of dichotomising\n")
cat("  continuous variables. BMJ 332(7549):1080.\n\n")

cat("• Royston P, Altman DG, Sauerbrei W. (2006). Dichotomizing\n")
cat("  continuous predictors in multiple regression: a bad idea.\n")
cat("  Stat Med 25(1):127-141.\n\n")

cat("• Cohen J. (1983). The cost of dichotomization.\n")
cat("  Applied Psychological Measurement 7(3):249-253.\n\n")

cat("Clinical Guidelines:\n")
cat("• WHO BMI Classification\n")
cat("• ACC/AHA Blood Pressure Guidelines (2017)\n")
cat("• KDIGO CKD Classification (2012)\n\n")

cat("R Packages:\n")
cat("• questionr::icut - Interactive cutting\n")
cat("• classInt - Natural breaks (Jenks)\n")
cat("• santoku - Comprehensive binning toolkit\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("For more information:\n")
cat("  ?categorize\n")
cat("  help(package='ClinicoPath')\n")
cat("  https://www.serdarbalci.com/ClinicoPath/\n")
cat("═══════════════════════════════════════════════════════════\n")
