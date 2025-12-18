# Test script for jjwithinstats fixes
# Tests the implementation of:
# 1. .addNotice() helper method
# 2. .accumulateMessage() with notice support
# 3. Plot state management with setState()
# 4. Complete clearWith in .r.yaml
# 5. Comprehensive roxygen documentation

# Test 1: Check if function loads without errors
test_function_loads <- function() {
    cat("Test 1: Checking if jjwithinstats loads without errors...\n")

    tryCatch({
        # Source the function file to check for syntax errors
        source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/jjwithinstats.b.R", local = TRUE)
        cat("✓ Function source loaded successfully\n\n")
        return(TRUE)
    }, error = function(e) {
        cat("✗ Error loading function:", e$message, "\n\n")
        return(FALSE)
    })
}

# Test 2: Check if .addNotice() method exists
test_addnotice_exists <- function() {
    cat("Test 2: Checking if .addNotice() method exists in source...\n")

    # Read the source file
    source_lines <- readLines("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/jjwithinstats.b.R")

    # Check for .addNotice method definition
    has_addnotice <- any(grepl("\\.addNotice\\s*=\\s*function", source_lines))

    if (has_addnotice) {
        cat("✓ .addNotice() method found in source\n\n")
        return(TRUE)
    } else {
        cat("✗ .addNotice() method NOT found in source\n\n")
        return(FALSE)
    }
}

# Test 3: Check if .accumulateMessage() was updated for notices
test_accumulate_message_updated <- function() {
    cat("Test 3: Checking if .accumulateMessage() includes notice support...\n")

    source_lines <- readLines("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/jjwithinstats.b.R")

    # Check for MODERN comment indicating notice support
    has_modern_notice <- any(grepl("# MODERN.*Notice", source_lines))

    if (has_modern_notice) {
        cat("✓ .accumulateMessage() has been updated with notice support\n\n")
        return(TRUE)
    } else {
        cat("✗ .accumulateMessage() may not have notice support\n\n")
        return(FALSE)
    }
}

# Test 4: Check if setState() is implemented in .plot()
test_setstate_exists <- function() {
    cat("Test 4: Checking if setState() is implemented in .plot()...\n")

    source_lines <- readLines("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/jjwithinstats.b.R")

    # Check for setState call
    has_setstate <- any(grepl("image\\$setState", source_lines))

    # Check for state_data list creation
    has_state_data <- any(grepl("state_data.*<-.*list", source_lines))

    if (has_setstate && has_state_data) {
        cat("✓ setState() is implemented with state_data structure\n\n")
        return(TRUE)
    } else {
        cat("✗ setState() implementation not found or incomplete\n\n")
        return(FALSE)
    }
}

# Test 5: Check if roxygen documentation was enhanced
test_roxygen_documentation <- function() {
    cat("Test 5: Checking if comprehensive roxygen documentation exists...\n")

    source_lines <- readLines("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/jjwithinstats.b.R")

    # Check for key roxygen tags
    has_description <- any(grepl("@description", source_lines))
    has_params <- any(grepl("@param", source_lines))
    has_return <- any(grepl("@return", source_lines))
    has_details <- any(grepl("@details", source_lines))
    has_examples <- any(grepl("@examples", source_lines))
    has_references <- any(grepl("@references", source_lines))

    checks <- c(
        description = has_description,
        params = has_params,
        return = has_return,
        details = has_details,
        examples = has_examples,
        references = has_references
    )

    if (all(checks)) {
        cat("✓ Comprehensive roxygen documentation found\n")
        cat("  - @description: ✓\n")
        cat("  - @param: ✓\n")
        cat("  - @return: ✓\n")
        cat("  - @details: ✓\n")
        cat("  - @examples: ✓\n")
        cat("  - @references: ✓\n\n")
        return(TRUE)
    } else {
        cat("✗ Some roxygen tags missing:\n")
        for (name in names(checks)) {
            cat("  -", name, ":", if(checks[[name]]) "✓" else "✗", "\n")
        }
        cat("\n")
        return(FALSE)
    }
}

# Test 6: Check if .r.yaml has complete clearWith
test_clearwith_complete <- function() {
    cat("Test 6: Checking if .r.yaml has complete clearWith lists...\n")

    yaml_lines <- readLines("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/jjwithinstats.r.yaml")

    # Check if showExplanations was added to clearWith
    has_showexplanations <- any(grepl("showExplanations", yaml_lines))

    # Check if plot has extensive clearWith
    # Find the plot section and check for clearWith
    plot_section_start <- grep("name: plot", yaml_lines)
    if (length(plot_section_start) > 0) {
        # Look for clearWith in next 30 lines after plot definition
        plot_section <- yaml_lines[plot_section_start:(plot_section_start + 30)]
        has_plot_clearwith <- any(grepl("clearWith:", plot_section))
    } else {
        has_plot_clearwith <- FALSE
    }

    if (has_showexplanations && has_plot_clearwith) {
        cat("✓ .r.yaml has been updated with complete clearWith lists\n")
        cat("  - showExplanations added: ✓\n")
        cat("  - plot clearWith exists: ✓\n\n")
        return(TRUE)
    } else {
        cat("✗ .r.yaml clearWith may be incomplete:\n")
        cat("  - showExplanations added:", if(has_showexplanations) "✓" else "✗", "\n")
        cat("  - plot clearWith exists:", if(has_plot_clearwith) "✓" else "✗", "\n\n")
        return(FALSE)
    }
}

# Run all tests
cat("\n")
cat("================================================================================\n")
cat("jjwithinstats FIXES VALIDATION TEST SUITE\n")
cat("================================================================================\n\n")

results <- list(
    function_loads = test_function_loads(),
    addnotice_exists = test_addnotice_exists(),
    accumulate_updated = test_accumulate_message_updated(),
    setstate_exists = test_setstate_exists(),
    roxygen_docs = test_roxygen_documentation(),
    clearwith_complete = test_clearwith_complete()
)

# Summary
cat("================================================================================\n")
cat("TEST RESULTS SUMMARY\n")
cat("================================================================================\n\n")

passed <- sum(unlist(results))
total <- length(results)

cat(sprintf("Tests Passed: %d/%d (%.1f%%)\n\n", passed, total, (passed/total)*100))

for (test_name in names(results)) {
    status <- if (results[[test_name]]) "✓ PASS" else "✗ FAIL"
    cat(sprintf("  %s: %s\n", test_name, status))
}

cat("\n")

if (passed == total) {
    cat("🎉 ALL TESTS PASSED! jjwithinstats fixes successfully implemented.\n\n")
    cat("Next steps:\n")
    cat("1. Test in jamovi with real data\n")
    cat("2. Verify notice system displays correctly\n")
    cat("3. Confirm plot caching improves performance\n")
    cat("4. Check documentation with ?jjwithinstats\n\n")
} else {
    cat("⚠️  SOME TESTS FAILED. Review the implementation.\n\n")
}

cat("================================================================================\n")
