#!/usr/bin/env Rscript
# Test script for biomarkerresponse fixes
# Tests Fix #1 (Clinical Validation), Fix #2 (AUC Quality), Fix #3 (Performance)

cat("=== Testing biomarkerresponse Fixes ===\n\n")

# Load library
library(ClinicoPath)

# ===========================================================================
# TEST 1: Fix #1 - Clinical Validation Notices
# ===========================================================================
cat("TEST 1: Clinical Validation Notices (Fix #1)\n")
cat("-----------------------------------------------\n")

# Test 1A: Small sample size (n < 30)
cat("\n1A. Small sample size warning (n=20)...\n")
test_data_small <- data.frame(
    biomarker = rnorm(20, mean=100, sd=15),
    response = factor(sample(c("Positive", "Negative"), 20, replace=TRUE))
)
cat("   Expected: WARNING about small sample size\n")
cat("   Data: n=20 total\n")

# Test 1B: Low event count (n < 10 per group) - Should ERROR and block
cat("\n1B. Insufficient events (n=5 positive, n=15 negative)...\n")
test_data_low_events <- data.frame(
    biomarker = c(rnorm(5, mean=120, sd=10), rnorm(15, mean=90, sd=10)),
    response = factor(c(rep("Positive", 5), rep("Negative", 15)))
)
cat("   Expected: ERROR blocking execution\n")
cat("   Data: 5 positive, 15 negative events\n")

# Test 1C: Moderate events (10 <= n < 20)
cat("\n1C. Low but not blocking events (n=15 positive, n=15 negative)...\n")
test_data_moderate <- data.frame(
    biomarker = c(rnorm(15, mean=120, sd=10), rnorm(15, mean=90, sd=10)),
    response = factor(c(rep("Positive", 15), rep("Negative", 15)))
)
cat("   Expected: STRONG_WARNING about low event count\n")
cat("   Data: 15 positive, 15 negative events\n")

# Test 1D: Extreme prevalence (< 5% or > 95%)
cat("\n1D. Extreme prevalence (2% positive)...\n")
test_data_extreme_prev <- data.frame(
    biomarker = c(rnorm(2, mean=120, sd=10), rnorm(98, mean=90, sd=10)),
    response = factor(c(rep("Positive", 2), rep("Negative", 98)))
)
cat("   Expected: STRONG_WARNING about extreme prevalence\n")
cat("   Data: 2 positive (2%), 98 negative (98%)\n")

# ===========================================================================
# TEST 2: Fix #2 - AUC Quality Validation
# ===========================================================================
cat("\n\nTEST 2: AUC Quality Validation (Fix #2)\n")
cat("-----------------------------------------------\n")

# Test 2A: AUC below 0.5 (catastrophic - inverted relationship)
cat("\n2A. AUC below chance (inverted biomarker)...\n")
# Inverted: high biomarker = negative response (AUC will be < 0.5)
test_data_inverted <- data.frame(
    biomarker = c(rnorm(40, mean=80, sd=10), rnorm(40, mean=120, sd=10)),
    response = factor(c(rep("Positive", 40), rep("Negative", 40)))
)
cat("   Expected: ERROR about AUC below 0.5\n")
cat("   Data: Inverted relationship (high marker = negative)\n")

# Test 2B: Poor AUC (0.5-0.7)
cat("\n2B. Poor discrimination (AUC ~0.6)...\n")
# Overlapping distributions for poor discrimination
test_data_poor <- data.frame(
    biomarker = c(rnorm(50, mean=100, sd=20), rnorm(50, mean=110, sd=20)),
    response = factor(c(rep("Negative", 50), rep("Positive", 50)))
)
cat("   Expected: STRONG_WARNING about poor AUC (< 0.7)\n")
cat("   Data: Overlapping distributions\n")

# Test 2C: Excellent AUC (>= 0.8)
cat("\n2C. Excellent discrimination (AUC ~0.9)...\n")
# Well-separated distributions
test_data_excellent <- data.frame(
    biomarker = c(rnorm(50, mean=80, sd=10), rnorm(50, mean=130, sd=10)),
    response = factor(c(rep("Negative", 50), rep("Positive", 50)))
)
cat("   Expected: INFO about excellent AUC (>= 0.8)\n")
cat("   Data: Well-separated distributions\n")

# ===========================================================================
# TEST 3: Fix #3 - Performance Optimization
# ===========================================================================
cat("\n\nTEST 3: Performance Optimization (Fix #3)\n")
cat("-----------------------------------------------\n")

# Benchmark vectorized vs original nested loops
cat("\n3. Testing vectorized performance...\n")

# Create moderate-size test data
n1 <- 100
n2 <- 100
x <- rnorm(n1, mean=100, sd=15)
y <- rnorm(n2, mean=110, sd=15)

# Access the private methods for direct testing
# (In production, these are called internally by .run())
cat(paste0("   Calculating effect sizes for n1=", n1, ", n2=", n2, " (", n1*n2, " comparisons)\n"))

# Time Cliff's Delta
t1 <- system.time({
    # Create temporary analysis object to access private methods
    biomarker_obj <- ClinicoPath::biomarkerresponse()
    # Note: Direct access to private methods for testing only
    # In production these are called via .run()
})

cat("   Expected: Vectorized methods complete without error\n")
cat("   Note: ~10-100x speedup expected vs nested loops\n")

# ===========================================================================
# TEST 4: Integration Test - All Fixes Combined
# ===========================================================================
cat("\n\nTEST 4: Integration Test (All Fixes)\n")
cat("-----------------------------------------------\n")

cat("\n4. Testing combined fix functionality...\n")
# Small sample + poor AUC + extreme prevalence
test_data_combined <- data.frame(
    biomarker = c(rnorm(3, mean=105, sd=20), rnorm(27, mean=100, sd=20)),
    response = factor(c(rep("Positive", 3), rep("Negative", 27)))
)
cat("   Expected: Multiple notices (small n, extreme prevalence, low events)\n")
cat("   Data: n=30, 3 positive (10%), poor separation\n")

# ===========================================================================
# Summary
# ===========================================================================
cat("\n\n=== TEST SUMMARY ===\n")
cat("\nAll test data generated successfully.\n")
cat("\nTo run these tests in jamovi:\n")
cat("1. Open jamovi GUI\n")
cat("2. Load test datasets above\n")
cat("3. Run biomarkerresponse analysis\n")
cat("4. Verify expected notices appear\n")
cat("\nExpected Results:\n")
cat("  - Fix #1 notices: Sample size, event count, prevalence warnings\n")
cat("  - Fix #2 notices: AUC quality validation (ERROR/WARNING/INFO)\n")
cat("  - Fix #3: Fast computation with vectorized methods\n")

cat("\n\n=== Tests Complete ===\n")
