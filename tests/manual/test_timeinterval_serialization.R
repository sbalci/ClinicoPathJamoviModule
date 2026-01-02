#!/usr/bin/env Rscript
# Manual test script for timeinterval function
# Tests serialization fix for Notice objects

cat("=== Testing timeinterval function serialization ===\n\n")

# Load the package
devtools::load_all()

# Load test data
test_data_path <- "data/timeinterval_test.csv"
if (!file.exists(test_data_path)) {
    stop("Test data not found. Please run from package root directory.")
}

test_data <- read.csv(test_data_path, stringsAsFactors = FALSE)
cat("✓ Loaded test data:", nrow(test_data), "rows\n\n")

# Test 1: Basic functionality with valid data
cat("TEST 1: Basic time interval calculation\n")
cat(strrep("-", 50), "\n")
tryCatch({
    result1 <- timeinterval(
        data = test_data,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = "ymd",
        output_unit = "months"
    )
    cat("✓ TEST 1 PASSED: Basic calculation successful\n")
    cat("  Result class:", class(result1)[1], "\n\n")
}, error = function(e) {
    cat("✗ TEST 1 FAILED:", e$message, "\n\n")
})

# Test 2: With negative interval removal
cat("TEST 2: Negative interval removal\n")
cat(strrep("-", 50), "\n")
tryCatch({
    result2 <- timeinterval(
        data = test_data,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = "ymd",
        output_unit = "months",
        remove_negative = TRUE
    )
    cat("✓ TEST 2 PASSED: Negative removal successful\n\n")
}, error = function(e) {
    cat("✗ TEST 2 FAILED:", e$message, "\n\n")
})

# Test 3: With quality metrics (triggers multiple notices)
cat("TEST 3: Quality metrics with notices\n")
cat(strrep("-", 50), "\n")
tryCatch({
    result3 <- timeinterval(
        data = test_data,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = "ymd",
        output_unit = "months",
        include_quality_metrics = TRUE,
        remove_negative = TRUE
    )
    cat("✓ TEST 3 PASSED: Quality metrics successful\n\n")
}, error = function(e) {
    cat("✗ TEST 3 FAILED:", e$message, "\n\n")
})

# Test 4: Landmark analysis
cat("TEST 4: Landmark analysis\n")
cat(strrep("-", 50), "\n")
tryCatch({
    result4 <- timeinterval(
        data = test_data,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = "ymd",
        output_unit = "months",
        use_landmark = TRUE,
        landmark_time = 12,
        remove_negative = TRUE
    )
    cat("✓ TEST 4 PASSED: Landmark analysis successful\n\n")
}, error = function(e) {
    cat("✗ TEST 4 FAILED:", e$message, "\n\n")
})

# Test 5: Serialization test (the critical test for the bug fix)
cat("TEST 5: Serialization test (Notice objects)\n")
cat(strrep("-", 50), "\n")
tryCatch({
    result5 <- timeinterval(
        data = test_data,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = "ymd",
        output_unit = "months",
        include_quality_metrics = TRUE,
        remove_negative = TRUE
    )

    # Try to serialize the results (this was causing the error)
    serialized <- try(serialize(result5, NULL), silent = TRUE)

    if (inherits(serialized, "try-error")) {
        cat("✗ TEST 5 FAILED: Serialization error\n")
        cat("  Error:", attr(serialized, "condition")$message, "\n\n")
    } else {
        cat("✓ TEST 5 PASSED: Serialization successful\n")
        cat("  Serialized size:", length(serialized), "bytes\n\n")
    }
}, error = function(e) {
    cat("✗ TEST 5 FAILED:", e$message, "\n\n")
})

# Test 6: Add calculated times to dataset
cat("TEST 6: Add calculated times to dataset\n")
cat(strrep("-", 50), "\n")
tryCatch({
    result6 <- timeinterval(
        data = test_data,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = "ymd",
        output_unit = "years",
        add_times = TRUE,
        remove_negative = TRUE
    )
    cat("✓ TEST 6 PASSED: Add times successful\n\n")
}, error = function(e) {
    cat("✗ TEST 6 FAILED:", e$message, "\n\n")
})

# Test 7: Different time units
cat("TEST 7: Different time units\n")
cat(strrep("-", 50), "\n")
units <- c("days", "weeks", "months", "years")
for (unit in units) {
    tryCatch({
        result <- timeinterval(
            data = test_data,
            dx_date = "diagnosis_date",
            fu_date = "followup_date",
            time_format = "ymd",
            output_unit = unit,
            remove_negative = TRUE
        )
        cat("  ✓", unit, "- OK\n")
    }, error = function(e) {
        cat("  ✗", unit, "- FAILED:", e$message, "\n")
    })
}
cat("\n")

# Test 8: Calendar vs standardized time basis
cat("TEST 8: Time basis options\n")
cat(strrep("-", 50), "\n")
bases <- c("standardized", "calendar")
for (basis in bases) {
    tryCatch({
        result <- timeinterval(
            data = test_data,
            dx_date = "diagnosis_date",
            fu_date = "followup_date",
            time_format = "ymd",
            output_unit = "months",
            time_basis = basis,
            remove_negative = TRUE
        )
        cat("  ✓", basis, "- OK\n")
    }, error = function(e) {
        cat("  ✗", basis, "- FAILED:", e$message, "\n")
    })
}
cat("\n")

# Summary
cat(strrep("=", 50), "\n")
cat("TESTING COMPLETE\n")
cat(strrep("=", 50), "\n")
cat("\nKey test: Serialization (TEST 5) - This tests the Notice fix\n")
cat("If TEST 5 passed, the serialization bug is fixed!\n\n")
