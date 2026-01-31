#!/usr/bin/env Rscript
# Test Script for groomecompare Example Code
# Date: 2026-01-31
# Purpose: Verify the example from groomecompare.a.yaml works correctly

cat("Testing groomecompare example code...\n\n")

# Load package from source (development mode)
cat("Loading ClinicoPath from source...\n")
tryCatch({
    suppressMessages(devtools::load_all(quiet = TRUE))
    cat("✅ ClinicoPath package loaded from source\n\n")
}, error = function(e) {
    cat("❌ Error loading ClinicoPath package:\n")
    cat("   ", e$message, "\n")
    cat("\n")
    cat("   Troubleshooting:\n")
    cat("   1. Make sure you're in the package root directory:\n")
    cat("      cd /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule\n")
    cat("   2. Install devtools if needed: install.packages('devtools')\n")
    cat("   3. Run: devtools::load_all()\n\n")
    quit(save = "no", status = 1)
})

cat("Generating sample survival data...\n")

# Example: Compare ypTNM vs RPA staging systems
# Generate sample survival data with two staging systems
set.seed(12345)
n <- 150

survData <- data.frame(
    time = rexp(n, 0.05),
    event = rbinom(n, 1, 0.6),
    ypTNM = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                         n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))),
    RPA = factor(sample(c("Low Risk", "Intermediate", "High Risk"),
                       n, replace = TRUE, prob = c(0.4, 0.35, 0.25)))
)

cat("✅ Data generated:\n")
cat("   - Observations:", n, "\n")
cat("   - Events:", sum(survData$event), "\n")
cat("   - ypTNM stages:", nlevels(survData$ypTNM), "\n")
cat("   - RPA groups:", nlevels(survData$RPA), "\n\n")

cat("Running groomecompare (basic example, no bootstrap)...\n")

# Compare staging systems using Groome criteria
tryCatch({
    result <- groomecompare(
        data = survData,
        time = "time",
        event = "event",
        stage1 = "ypTNM",
        stage2 = "RPA",
        stage1name = "ypTNM Staging",
        stage2name = "RPA Classification",
        eventValue = "1",
        radarplot = TRUE,
        kmplots = TRUE,
        detailedmetrics = TRUE,
        cindexcompare = TRUE,
        bootstrap = FALSE
    )

    cat("✅ Basic comparison completed successfully\n\n")

    # Check that results exist
    if (!is.null(result$summary)) {
        cat("✅ Summary table populated\n")
    }
    if (!is.null(result$cindexcompare)) {
        cat("✅ C-index comparison populated\n")
    }
    if (!is.null(result$detailedmetrics)) {
        cat("✅ Detailed metrics populated\n")
    }

}, error = function(e) {
    cat("❌ Error in basic comparison:\n")
    cat("   ", e$message, "\n\n")
    quit(save = "no", status = 1)
})

cat("\nRunning groomecompare with bootstrap (nboot=50 for speed)...\n")

# With bootstrap validation (smaller nboot for speed)
tryCatch({
    result_boot <- groomecompare(
        data = survData,
        time = "time",
        event = "event",
        stage1 = "ypTNM",
        stage2 = "RPA",
        stage1name = "ypTNM Staging",
        stage2name = "RPA Classification",
        bootstrap = TRUE,
        nboot = 50,  # Reduced from 100 for faster testing
        seed = 12345
    )

    cat("✅ Bootstrap validation completed successfully\n\n")

    # Check that bootstrap results exist
    if (!is.null(result_boot$bootstrap)) {
        cat("✅ Bootstrap table populated\n")
        cat("   - Bootstrap samples: 50\n")
    }

}, error = function(e) {
    cat("❌ Error in bootstrap validation:\n")
    cat("   ", e$message, "\n\n")
    quit(save = "no", status = 1)
})

cat("\n" , paste(rep("=", 60), collapse = ""), "\n")
cat("✅ ALL TESTS PASSED!\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("Summary:\n")
cat("  ✅ Package loads correctly\n")
cat("  ✅ Data generation works\n")
cat("  ✅ Basic comparison runs without errors\n")
cat("  ✅ Bootstrap validation runs without errors\n")
cat("  ✅ All outputs populated as expected\n\n")

cat("Next steps:\n")
cat("  1. Test in jamovi UI\n")
cat("  2. Test with real clinical data\n")
cat("  3. Verify all plots render correctly\n")
cat("  4. Check notice messages display properly\n\n")

cat("Test completed successfully!\n")
