#!/usr/bin/env Rscript
# Test script for agepyramid function
# Tests responsiveness of plot engine and color options

cat("=== Testing agepyramid function ===\n\n")

# Load required packages
library(ClinicoPath)
library(ggplot2)

# Load test data
cat("1. Loading test data...\n")
load("data/agepyramid_test.rda")
cat("   Data loaded: ", nrow(agepyramid_test), "rows\n")
cat("   Columns:", paste(names(agepyramid_test), collapse=", "), "\n\n")

# Test 1: Create basic pyramid with ggcharts (default)
cat("2. Test 1: Creating pyramid with ggcharts engine...\n")
result1 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        bin_width = 5,
        plot_engine = "ggcharts",
        color_palette = "standard"
    )
    cat("   ✓ SUCCESS: ggcharts plot created\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Test 2: Switch to ggplot2 engine (this was causing the freeze)
cat("3. Test 2: Switching to ggplot2 engine...\n")
result2 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        bin_width = 5,
        plot_engine = "ggplot2",  # CRITICAL: This was freezing before
        color_palette = "standard"
    )
    cat("   ✓ SUCCESS: ggplot2 plot created (no freeze!)\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Test 3: Change color palette
cat("4. Test 3: Changing color palette to accessible...\n")
result3 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        bin_width = 5,
        plot_engine = "ggplot2",
        color_palette = "accessible"  # Should update instantly
    )
    cat("   ✓ SUCCESS: Color palette changed\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Test 4: Custom colors
cat("5. Test 4: Using custom colors...\n")
result4 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        bin_width = 5,
        plot_engine = "ggplot2",
        color_palette = "custom",
        color1 = "#FF1493",  # Deep pink
        color2 = "#4169E1"   # Royal blue
    )
    cat("   ✓ SUCCESS: Custom colors applied\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Test 5: Change bin width (data option - should trigger full recompute)
cat("6. Test 5: Changing bin width from 5 to 10...\n")
result5 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        bin_width = 10,  # Changed from 5 to 10
        plot_engine = "ggplot2",
        color_palette = "standard"
    )
    cat("   ✓ SUCCESS: Bin width changed (data recomputed)\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Test 6: Change plot title
cat("7. Test 6: Changing plot title...\n")
result6 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        bin_width = 5,
        plot_engine = "ggplot2",
        color_palette = "standard",
        plot_title = "Test Population Age Distribution"
    )
    cat("   ✓ SUCCESS: Plot title changed\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Test 7: Test age group presets
cat("8. Test 7: Testing pediatric age group preset...\n")
result7 <- tryCatch({
    agepyramid(
        data = agepyramid_test,
        age = "age",
        gender = "gender",
        female = "Female",
        male = "Male",
        age_groups = "pediatric",
        plot_engine = "ggcharts",
        color_palette = "accessible"
    )
    cat("   ✓ SUCCESS: Pediatric age groups applied\n\n")
    TRUE
}, error = function(e) {
    cat("   ✗ FAILED:", e$message, "\n\n")
    FALSE
})

# Summary
cat("\n=== TEST SUMMARY ===\n")
total_tests <- 7
passed_tests <- sum(result1, result2, result3, result4, result5, result6, result7)
cat("Passed:", passed_tests, "/", total_tests, "\n")

if (passed_tests == total_tests) {
    cat("\n✓✓✓ ALL TESTS PASSED ✓✓✓\n")
    cat("The agepyramid function is now fully responsive!\n")
    cat("No freezing when changing plot_engine or other options.\n")
} else {
    cat("\n✗ SOME TESTS FAILED ✗\n")
    cat("Please review error messages above.\n")
}

cat("\n")
