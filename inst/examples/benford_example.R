# ═══════════════════════════════════════════════════════════
# Example Usage: benford
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating Benford's Law analysis
# for fraud detection and data quality assessment
#
# Generated: 2026-01-04

library(ClinicoPath)

# Load test data
data(benford_test, package = "ClinicoPath")

# View data structure
str(benford_test)
summary(benford_test)

# ═══════════════════════════════════════════════════════════
# WHAT IS BENFORD'S LAW?
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
BENFORD'S LAW - Background
═══════════════════════════════════════════════════════════

Benford's Law states that in many naturally occurring datasets,
the leading digits are NOT equally distributed.

Expected distribution of FIRST digits:
  1: 30.1%    6: 6.7%
  2: 17.6%    7: 5.8%
  3: 12.5%    8: 5.1%
  4: 9.7%     9: 4.6%
  5: 7.9%

This applies to:
  ✓ Population sizes
  ✓ Financial data
  ✓ Physical constants
  ✓ Scientific measurements spanning orders of magnitude

Does NOT apply to:
  ✗ Uniform distributions
  ✗ Narrow-range data (e.g., human heights 150-190cm)
  ✗ Data with artificial limits
  ✗ Assigned numbers (IDs, phone numbers)

Use cases in clinical research:
  • Detect fabricated clinical trial data
  • Identify data entry errors
  • Audit healthcare billing/charges
  • Quality control for genomic datasets
  • Validate epidemiological statistics

MAD (Mean Absolute Deviation) - Conformity measure:
  MAD < 0.006: Close conformity
  MAD 0.006-0.012: Acceptable conformity
  MAD 0.012-0.015: Marginally acceptable
  MAD > 0.015: Non-conformity (investigate!)

═══════════════════════════════════════════════════════════
\n")

# ═══════════════════════════════════════════════════════════
# Example 1: Natural Data (SHOULD Conform)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 1: Population Sizes (Natural Data) ===\n")
cat("Expected: CONFORMS to Benford's Law (MAD < 0.006)\n\n")

benford(
  data = benford_test,
  var = "population_size",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 2: Fabricated Data (Should NOT Conform)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 2: Uniform Random Numbers (Fabricated Data) ===\n")
cat("Expected: VIOLATES Benford's Law (MAD > 0.015)\n")
cat("Interpretation: All digits appear with ~equal frequency (~11%)\n\n")

benford(
  data = benford_test,
  var = "uniform_random",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 3: Clinical Biomarker (Generally Conforms)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 3: CA-125 Tumor Marker Levels ===\n")
cat("Expected: CONFORMS (MAD < 0.012)\n")
cat("Use case: Validate lab result database integrity\n\n")

benford(
  data = benford_test,
  var = "ca125_level",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 4: PSA Levels (Wide Range Clinical Data)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 4: PSA Levels (Prostate Cancer Screening) ===\n")
cat("Range: 0.1-1000 ng/mL (multiple orders of magnitude)\n")
cat("Expected: Good Benford conformity\n\n")

benford(
  data = benford_test,
  var = "psa_level",
  digits = 1
)

# ═══════════════════════════════════════════════════════════
# Example 5: Hospital Charges (Healthcare Economics)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 5: Hospital Billing Charges ===\n")
cat("Use case: Audit healthcare billing for fraudulent patterns\n")
cat("Expected: Should conform (natural economic data)\n\n")

benford(
  data = benford_test,
  var = "hospital_charges",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 6: Rounded Data (Data Quality Issue)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 6: Rounded Values (Data Entry Artifact) ===\n")
cat("Common issue: People round to nice numbers (10, 20, 50, 100)\n")
cat("Expected: VIOLATES Benford's Law\n")
cat("Interpretation: Digit 1 appears too often due to rounding to 10, 100, 1000\n\n")

benford(
  data = benford_test,
  var = "rounded_values",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 7: Gene Expression Data (Genomics)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 7: RNA-Seq Gene Expression Counts ===\n")
cat("Range: 1-100,000 transcript counts\n")
cat("Use case: Quality control for genomic datasets\n\n")

benford(
  data = benford_test,
  var = "gene_expression",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 8: White Blood Cell Counts
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 8: WBC Counts (Clinical Hematology) ===\n")
cat("Range: 1,000-50,000 cells/μL\n")
cat("Use case: Validate lab database integrity\n\n")

benford(
  data = benford_test,
  var = "wbc_count",
  digits = 1
)

# ═══════════════════════════════════════════════════════════
# Example 9: First Digit Only Analysis
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 9: First Digit Analysis (digits=1) ===\n")
cat("Simpler analysis - easier interpretation\n")
cat("Good for initial screening\n\n")

benford(
  data = benford_test,
  var = "transaction_amount",
  digits = 1
)

# ═══════════════════════════════════════════════════════════
# Example 10: First Two Digits Analysis (More Sensitive)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 10: First Two Digits (digits=2) ===\n")
cat("More sensitive to subtle deviations\n")
cat("Better for fraud detection\n\n")

benford(
  data = benford_test,
  var = "medicare_payment",
  digits = 2
)

# ═══════════════════════════════════════════════════════════
# Example 11: Comparing Conforming vs Non-Conforming
# ═══════════════════════════════════════════════════════════

cat("\n=== COMPARISON: Natural vs Fabricated Data ===\n\n")

cat("Natural data (city populations):\n")
benford(data = benford_test, var = "city_population", digits = 2)

cat("\n\nFabricated data (truncated range 200-299):\n")
benford(data = benford_test, var = "truncated_data", digits = 2)

# ═══════════════════════════════════════════════════════════
# Example 12: Edge Cases - Narrow Range (Triggers Warning)
# ═══════════════════════════════════════════════════════════

cat("\n=== Example 12: Narrow Range Data (45-55) ===\n")
cat("Expected: Warning - data spans less than 1 order of magnitude\n")
cat("Benford's Law does not apply to narrow ranges\n\n")

benford(
  data = benford_test,
  var = "narrow_range",
  digits = 1
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════\n")
cat("CLINICAL RESEARCH SCENARIOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Scenario 1: Clinical Trial Data Validation
cat("Scenario 1: Validating Clinical Trial Endpoints\n")
cat("──────────────────────────────────────────────\n")
cat("Problem: Suspected fabrication in tumor size measurements\n")
cat("Solution: Apply Benford's Law to detect unusual patterns\n\n")

cat("Example - Analyzing tumor measurements:\n")
# In real scenario, you'd have actual tumor size data
# Here we use gene_expression as a proxy
benford(data = benford_test, var = "gene_expression", digits = 2)

cat("\n\n")

# Scenario 2: Lab Result Database Integrity
cat("Scenario 2: Laboratory Database Quality Control\n")
cat("──────────────────────────────────────────────\n")
cat("Problem: Ensure lab results haven't been manually altered\n")
cat("Solution: Benford analysis on biomarker levels\n\n")

cat("Example - CA-125 tumor marker database:\n")
benford(data = benford_test, var = "ca125_level", digits = 2)

cat("\n\n")

# Scenario 3: Healthcare Billing Audit
cat("Scenario 3: Medicare/Insurance Billing Audit\n")
cat("──────────────────────────────────────────────\n")
cat("Problem: Detect fraudulent billing patterns\n")
cat("Solution: Benford analysis on charge amounts\n\n")

cat("Example - Hospital billing charges:\n")
benford(data = benford_test, var = "hospital_charges", digits = 2)

cat("\n\n")

# Scenario 4: Genomic Data Quality
cat("Scenario 4: RNA-Seq Quality Control\n")
cat("──────────────────────────────────────────────\n")
cat("Problem: Verify integrity of gene expression counts\n")
cat("Solution: Benford analysis on read counts\n\n")

cat("Example - Gene expression counts:\n")
benford(data = benford_test, var = "gene_expression", digits = 1)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("INTERPRETATION GUIDE\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("1. CONFORMITY LEVELS (based on MAD):\n")
cat("   • MAD < 0.006: EXCELLENT - Data follows Benford's Law closely\n")
cat("   • MAD 0.006-0.012: GOOD - Acceptable conformity\n")
cat("   • MAD 0.012-0.015: MARGINAL - Borderline conformity\n")
cat("   • MAD > 0.015: POOR - Likely non-conformity\n\n")

cat("2. WHAT NON-CONFORMITY MEANS:\n")
cat("   ⚠ Potential data quality issues:\n")
cat("     - Fabricated/made-up data\n")
cat("     - Data entry errors\n")
cat("     - Systematic rounding\n")
cat("     - Copy-paste errors\n")
cat("     - Truncation/censoring\n")
cat("   ℹ Natural causes:\n")
cat("     - Data doesn't span orders of magnitude\n")
cat("     - Artificial limits (e.g., lab detection limits)\n")
cat("     - Small sample size (<100)\n\n")

cat("3. WHEN TO INVESTIGATE FURTHER:\n")
cat("   🔴 HIGH PRIORITY (MAD > 0.020):\n")
cat("      - Review data collection procedures\n")
cat("      - Check for systematic errors\n")
cat("      - Audit data entry logs\n")
cat("      - Consider data exclusion\n")
cat("   🟡 MEDIUM PRIORITY (MAD 0.015-0.020):\n")
cat("      - Examine suspect observations\n")
cat("      - Verify unusual patterns\n")
cat("      - Document deviations\n")
cat("   🟢 LOW PRIORITY (MAD < 0.015):\n")
cat("      - Data appears reliable\n")
cat("      - Proceed with analysis\n\n")

cat("4. DIGITS PARAMETER:\n")
cat("   • digits = 1: First digit only (simpler, less sensitive)\n")
cat("   • digits = 2: First two digits (MORE sensitive, recommended)\n")
cat("   • digits = 3-4: Very sensitive, requires large samples (>1000)\n\n")

cat("5. SAMPLE SIZE RECOMMENDATIONS:\n")
cat("   • Minimum: 30 observations (will trigger warning if less)\n")
cat("   • Adequate: 100-500 observations\n")
cat("   • Ideal: 1000+ observations\n\n")

cat("6. DATA REQUIREMENTS:\n")
cat("   ✓ REQUIRED:\n")
cat("     - Positive numbers only (no zeros or negatives)\n")
cat("     - Spans multiple orders of magnitude\n")
cat("     - Natural/non-assigned data\n")
cat("   ✗ NOT SUITABLE:\n")
cat("     - IDs, phone numbers, zip codes\n")
cat("     - Narrow ranges (e.g., ages 20-30)\n")
cat("     - Categorical data\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("REFERENCES & FURTHER READING\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Key Papers:\n")
cat("• Benford, F. (1938). The law of anomalous numbers.\n")
cat("  Proceedings of the American Philosophical Society, 78(4), 551-572.\n\n")

cat("• Nigrini, M. J. (2012). Benford's Law: Applications for Forensic\n")
cat("  Accounting, Auditing, and Fraud Detection. Wiley.\n\n")

cat("• Diekmann, A. (2007). Not the first digit! Using Benford's law to\n")
cat("  detect fraudulent scientific data. Journal of Applied Statistics,\n")
cat("  34(3), 321-329.\n\n")

cat("R Package Documentation:\n")
cat("• benford.analysis package by Carlos Cinelli\n")
cat("  https://github.com/carloscinelli/benford.analysis\n\n")

cat("Clinical Applications:\n")
cat("• Al Hajjar, S., Nejad, M. M. (2020). Applying Benford's Law to\n")
cat("  COVID-19 data: The case of the second wave in Iran. medRxiv.\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("For more information:\n")
cat("  ?benford\n")
cat("  help(package='ClinicoPath')\n")
cat("  https://www.serdarbalci.com/ClinicoPath/\n")
cat("═══════════════════════════════════════════════════════════\n")
