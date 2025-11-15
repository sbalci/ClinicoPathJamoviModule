#!/usr/bin/env Rscript
# Generate test data for tidyplots with challenging variable names
# This tests the .escapeVar() function's ability to handle:
# - Spaces in variable names
# - Special characters (dashes, parentheses, etc.)
# - Unicode characters
# - Mixed case with punctuation

library(dplyr)
library(tibble)

set.seed(123)

# Create comprehensive test dataset with challenging variable names
tidyplots_testdata <- tibble(
    # Variable with spaces
    `Group Name` = rep(c("Control", "Treatment A", "Treatment B"), each = 30),

    # Variable with dashes and parentheses
    `Response-Value (mg/dL)` = c(
        rnorm(30, 10, 2),  # Control
        rnorm(30, 15, 3),  # Treatment A
        rnorm(30, 12, 2.5) # Treatment B
    ),

    # Variable with time notation
    `Time-Point (hours)` = rep(1:30, 3),

    # Variable with special chars
    `Subject#ID` = rep(1:30, each = 3),

    # Variable with mixed notation
    `Category_Type-I` = sample(c("Type-I", "Type-II", "Type-III"), 90, replace = TRUE),

    # Variable with percentage
    `Efficacy%` = runif(90, 0, 100),

    # Variable with Greek letter (Unicode)
    `α-Level` = sample(c("Low", "Medium", "High"), 90, replace = TRUE),

    # Variable with forward slash
    `Ratio (A/B)` = rnorm(90, 1.5, 0.3),

    # Variable with multiple spaces
    `Treatment   Response` = rnorm(90, 50, 10),

    # Variable starting with number (edge case)
    `2nd-Measurement` = rnorm(90, 20, 5),

    # Variable with dot notation
    `Value.Score` = sample(1:10, 90, replace = TRUE),

    # Variable with ampersand
    `Risk & Benefit` = sample(c("Low Risk", "High Risk", "Uncertain"), 90, replace = TRUE),

    # Variable with plus sign
    `Change+Baseline` = rnorm(90, 5, 2)
)

# Save to data directory
data_dir <- file.path(dirname(getwd()), "data")
if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
}

csv_path <- file.path(data_dir, "tidyplots_testdata.csv")
write.csv(tidyplots_testdata, csv_path, row.names = FALSE)

cat("✓ Test data generated successfully:\n")
cat("  - File:", csv_path, "\n")
cat("  - Rows:", nrow(tidyplots_testdata), "\n")
cat("  - Columns:", ncol(tidyplots_testdata), "\n")
cat("\nVariable names with special characters:\n")
for (i in seq_along(names(tidyplots_testdata))) {
    cat(sprintf("  %2d. %s\n", i, names(tidyplots_testdata)[i]))
}

# Also create a simpler version for basic testing
tidyplots_simple <- tibble(
    group = rep(c("A", "B", "C"), each = 20),
    value = c(rnorm(20, 10, 2), rnorm(20, 15, 3), rnorm(20, 12, 2)),
    time = rep(1:20, 3),
    category = sample(c("X", "Y", "Z"), 60, replace = TRUE)
)

simple_path <- file.path(data_dir, "tidyplots_simple.csv")
write.csv(tidyplots_simple, simple_path, row.names = FALSE)

cat("\n✓ Simple test data also generated:\n")
cat("  - File:", simple_path, "\n")
cat("  - Rows:", nrow(tidyplots_simple), "\n")
cat("  - Columns:", ncol(tidyplots_simple), "\n")
