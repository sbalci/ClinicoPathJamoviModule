#!/usr/bin/env Rscript
# Create Test Data for groomecompare in jamovi
# Date: 2026-01-31

cat("Creating test data for groomecompare...\n\n")

# Generate test data
set.seed(12345)
n <- 150

groomecompare_test <- data.frame(
    time = rexp(n, 0.05),
    event = rbinom(n, 1, 0.6),
    ypTNM = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                         n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15))),
    RPA = factor(sample(c("Low Risk", "Intermediate", "High Risk"),
                       n, replace = TRUE, prob = c(0.4, 0.35, 0.25)))
)

cat("Test data created:\n")
cat("  - Observations:", nrow(groomecompare_test), "\n")
cat("  - Events:", sum(groomecompare_test$event), "\n")
cat("  - ypTNM stages:", nlevels(groomecompare_test$ypTNM), "\n")
cat("  - RPA groups:", nlevels(groomecompare_test$RPA), "\n\n")

# Save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
    output_file <- "groomecompare_test_data.omv"
    jmvReadWrite::write_omv(groomecompare_test, output_file)
    cat("✅ Saved jamovi file:", output_file, "\n\n")

    cat("To use in jamovi:\n")
    cat("  1. Open jamovi\n")
    cat("  2. File → Open →", output_file, "\n")
    cat("  3. Analyses → ClinicoPath → Survival → Groome Staging System Comparison\n")
    cat("  4. Set variables:\n")
    cat("     - Survival Time: time\n")
    cat("     - Event Status: event\n")
    cat("     - Staging System 1: ypTNM\n")
    cat("     - Staging System 2: RPA\n")
    cat("  5. Options:\n")
    cat("     - System 1 Name: ypTNM Staging\n")
    cat("     - System 2 Name: RPA Classification\n")
    cat("     - Event Value: 1\n")
    cat("     - Enable: Radar Chart, KM Curves, Detailed Metrics, C-Index\n\n")
} else {
    cat("⚠️  jmvReadWrite package not installed\n")
    cat("   Install with: install.packages('jmvReadWrite')\n\n")

    # Save as CSV as fallback
    output_file <- "groomecompare_test_data.csv"
    write.csv(groomecompare_test, output_file, row.names = FALSE)
    cat("✅ Saved CSV file:", output_file, "\n")
    cat("   Import this into jamovi using File → Open\n\n")
}

cat("Done!\n")
