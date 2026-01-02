# ═══════════════════════════════════════════════════════════
# Example Usage: tableone
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates how to use the tableone function from
# the ClinicoPath package for creating descriptive summary tables
# commonly used in clinicopathological research manuscripts.
#
# Author: ClinicoPath Development Team
# Date: 2026-01-02
# Package: ClinicoPath

# ═══════════════════════════════════════════════════════════
# Setup
# ═══════════════════════════════════════════════════════════

# Load required packages
library(ClinicoPath)

# Load test dataset
data(tableone_test, package = "ClinicoPath")

# View dataset structure
str(tableone_test)
# 150 obs. of 19 variables
# Mix of continuous, categorical, and ordinal variables

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Table One (Demographics Only)
# ═══════════════════════════════════════════════════════════

# Create a simple Table 1 with patient demographics
example1 <- tableone(
  data = tableone_test,
  vars = c("Age", "Sex")
)

# This produces a basic descriptive table showing:
# - Age: Mean ± SD, Median [IQR]
# - Sex: N (%) for each category

print(example1)

# ═══════════════════════════════════════════════════════════
# Example 2: Clinical Characteristics Table
# ═══════════════════════════════════════════════════════════

# Create a comprehensive clinical characteristics table
example2 <- tableone(
  data = tableone_test,
  vars = c(
    "Age",
    "Sex",
    "TumorSize",
    "TumorStage",
    "Grade",
    "LymphNodes"
  ),
  sty = "t1"  # Standard tableone format
)

print(example2)

# ═══════════════════════════════════════════════════════════
# Example 3: Publication-Ready Table (gtsummary style)
# ═══════════════════════════════════════════════════════════

# Use gtsummary style for publication-ready formatting
example3 <- tableone(
  data = tableone_test,
  vars = c(
    "Age",
    "Sex",
    "TumorSize",
    "TumorStage",
    "Grade",
    "LVI",
    "PNI",
    "LymphNodes"
  ),
  sty = "t2",              # gtsummary style
  excl = TRUE,             # Exclude missing values
  showReportSentence = TRUE  # Generate copy-ready text
)

print(example3)

# The gtsummary style provides:
# - Clean, publication-ready formatting
# - Proper statistical notation
# - Professional appearance

# ═══════════════════════════════════════════════════════════
# Example 4: Comprehensive Descriptive Table (arsenal style)
# ═══════════════════════════════════════════════════════════

# Use arsenal style for comprehensive descriptive statistics
example4 <- tableone(
  data = tableone_test,
  vars = c(
    "Age",
    "Sex",
    "TumorSize",
    "TumorStage",
    "Grade",
    "Hemoglobin",
    "WBC",
    "Ki67"
  ),
  sty = "t3",        # arsenal style
  excl = FALSE,      # Keep missing values to see patterns
  showSummary = TRUE  # Show data quality metrics
)

print(example4)

# Arsenal style provides:
# - Detailed descriptive statistics
# - Missing data information
# - Comprehensive variable summaries

# ═══════════════════════════════════════════════════════════
# Example 5: Frequency Tables (janitor style)
# ═══════════════════════════════════════════════════════════

# Use janitor style for simple frequency tables (categorical only)
example5 <- tableone(
  data = tableone_test,
  vars = c(
    "Sex",
    "TumorStage",
    "Grade",
    "LymphNodes",
    "LVI",
    "PNI",
    "TumorType"
  ),
  sty = "t4"  # janitor style (categorical focus)
)

print(example5)

# Janitor style provides:
# - Simple frequency counts
# - Percentage formatting
# - Totals

# ═══════════════════════════════════════════════════════════
# Example 6: Complete Pathology Workup Table
# ═══════════════════════════════════════════════════════════

# Comprehensive table for pathology reporting
example6 <- tableone(
  data = tableone_test,
  vars = c(
    # Demographics
    "Age",
    "Sex",

    # Tumor characteristics
    "TumorSize",
    "TumorStage",
    "Grade",
    "TumorType",

    # Histopathological features
    "LVI",
    "PNI",
    "PreinvasiveComponent",
    "LymphNodes",

    # Biomarkers
    "Ki67",
    "Hemoglobin"
  ),
  sty = "t2",              # Publication-ready gtsummary
  excl = TRUE,             # Exclude missing for clean table
  showSummary = TRUE,      # Show sample size info
  showReportSentence = TRUE  # Generate methods text
)

print(example6)

# ═══════════════════════════════════════════════════════════
# Example 7: Handling Missing Data
# ═══════════════════════════════════════════════════════════

# Table showing how missing data is handled

# Option A: Keep missing values (default)
example7a <- tableone(
  data = tableone_test,
  vars = c("Age", "Hemoglobin", "Ki67", "CA199"),
  excl = FALSE  # Keep rows with missing
)

print(example7a)
# Will show: N, N-miss, and statistics on available data

# Option B: Exclude missing values
example7b <- tableone(
  data = tableone_test,
  vars = c("Age", "Hemoglobin", "Ki67", "CA199"),
  excl = TRUE  # Exclude incomplete rows
)

print(example7b)
# Will show: Only complete cases, smaller N

# ═══════════════════════════════════════════════════════════
# Example 8: Treatment and Outcomes Table
# ═══════════════════════════════════════════════════════════

# Summarize treatment and outcome variables
example8 <- tableone(
  data = tableone_test,
  vars = c(
    "Treatment",
    "Response",
    "FollowUpMonths",
    "VitalStatus"
  ),
  sty = "t2",
  showSummary = TRUE
)

print(example8)

# ═══════════════════════════════════════════════════════════
# Example 9: Laboratory Values Table
# ═══════════════════════════════════════════════════════════

# Focus on laboratory and biomarker results
example9 <- tableone(
  data = tableone_test,
  vars = c(
    "Hemoglobin",
    "WBC",
    "Ki67",
    "CA199"
  ),
  sty = "t1",
  excl = FALSE,
  showSummary = TRUE  # Show missing data patterns
)

print(example9)

# ═══════════════════════════════════════════════════════════
# Example 10: Minimal Table for Supplementary Material
# ═══════════════════════════════════════════════════════════

# Compact table for supplementary files
example10 <- tableone(
  data = tableone_test,
  vars = c(
    "Age",
    "Sex",
    "TumorStage",
    "Grade"
  ),
  sty = "t1"
)

print(example10)

# ═══════════════════════════════════════════════════════════
# Example 11: Using Different Styles - Comparison
# ═══════════════════════════════════════════════════════════

# Same variables, different styles for comparison
vars_to_compare <- c("Age", "Sex", "TumorStage", "Grade", "LVI")

# Style t1: tableone (medical standard)
style_t1 <- tableone(
  data = tableone_test,
  vars = vars_to_compare,
  sty = "t1"
)

# Style t2: gtsummary (publication-ready)
style_t2 <- tableone(
  data = tableone_test,
  vars = vars_to_compare,
  sty = "t2"
)

# Style t3: arsenal (comprehensive)
style_t3 <- tableone(
  data = tableone_test,
  vars = vars_to_compare,
  sty = "t3"
)

# Compare outputs
cat("\n=== Style t1 (tableone) ===\n")
print(style_t1)

cat("\n=== Style t2 (gtsummary) ===\n")
print(style_t2)

cat("\n=== Style t3 (arsenal) ===\n")
print(style_t3)

# ═══════════════════════════════════════════════════════════
# Example 12: Manuscript-Ready Table with All Features
# ═══════════════════════════════════════════════════════════

# Complete example for manuscript Table 1
manuscript_table1 <- tableone(
  data = tableone_test,
  vars = c(
    # Patient Characteristics
    "Age",
    "Sex",

    # Tumor Characteristics
    "TumorSize",
    "TumorStage",
    "Grade",
    "TumorType",

    # Pathological Features
    "LVI",
    "PNI",
    "LymphNodes",

    # Laboratory Values
    "Hemoglobin",
    "Ki67"
  ),
  sty = "t2",                   # Publication-ready gtsummary
  excl = TRUE,                  # Clean data (exclude missing)
  showSummary = TRUE,           # Document sample size
  showAbout = FALSE,            # Hide educational content
  showReportSentence = TRUE     # Generate methods text
)

print(manuscript_table1)

# ═══════════════════════════════════════════════════════════
# Tips for Using tableone
# ═══════════════════════════════════════════════════════════

# 1. Choose the right style:
#    - t1 (tableone): Standard medical format, good for general use
#    - t2 (gtsummary): Best for publication, clean formatting
#    - t3 (arsenal): Most comprehensive, good for exploratory analysis
#    - t4 (janitor): Simple frequencies, categorical variables only

# 2. Handle missing data appropriately:
#    - excl = FALSE: Keep missing, show patterns (default)
#    - excl = TRUE: Exclude missing, cleaner table

# 3. Use optional displays wisely:
#    - showSummary: Document sample size and completeness
#    - showReportSentence: Generate methods text for manuscript
#    - showAbout: Educational info (good for learning)

# 4. Variable selection:
#    - Mix continuous, categorical, and ordinal variables
#    - Group logically (demographics, clinical, lab values)
#    - Consider table length for publication

# 5. For manuscripts:
#    - Use sty = "t2" (gtsummary) for polished formatting
#    - Set excl = TRUE for clean presentation
#    - Enable showReportSentence for methods section text
#    - Enable showSummary to document sample characteristics

# ═══════════════════════════════════════════════════════════
# Working with Your Own Data
# ═══════════════════════════════════════════════════════════

# Template for using tableone with your data:
#
# my_table <- tableone(
#   data = your_data,
#   vars = c("var1", "var2", "var3"),
#   sty = "t2",              # Choose: t1, t2, t3, or t4
#   excl = TRUE,             # TRUE = exclude missing
#   showSummary = TRUE,      # Show sample info
#   showReportSentence = TRUE  # Generate methods text
# )
#
# print(my_table)

# ═══════════════════════════════════════════════════════════
# End of Examples
# ═══════════════════════════════════════════════════════════
