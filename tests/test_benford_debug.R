#!/usr/bin/env Rscript

# Load required packages
library(jmvcore)
library(benford.analysis)
library(glue)

# Source the benford.b.R file
source('R/benford.b.R')

# Create a mock results object
create_mock_results <- function() {
    rows <- list()
    list(
        summary = list(
            addRow = function(rowKey, values) {
                rows[[rowKey]] <<- values
                cat(sprintf("Row %d: %s | %s | %s\n",
                           rowKey,
                           values$statistic,
                           values$value,
                           values$interpretation))
            },
            getRows = function() rows
        )
    )
}

# TEST 1: Compliant data (exponential distribution - should follow Benford)
cat("=== TEST 1: Compliant Data (Exponential) ===\n")
set.seed(123)
compliant_values <- round(rexp(500, rate = 0.1), 2)
compliant_values <- compliant_values[compliant_values > 0]

cat("Sample values:", head(compliant_values, 10), "\n")
cat("Total observations:", length(compliant_values), "\n\n")

# Run benford analysis
benford_obj1 <- benford.analysis::benford(compliant_values, number.of.digits = 1)
cat("MAD:", benford_obj1$MAD, "\n")
cat("MAD Conformity:", as.character(benford_obj1$MAD.conformity), "\n")
cat("Chi-square p-value:", benford_obj1$stats$chisq$p.value, "\n")
cat("Mantissa Arc Test p-value:", benford_obj1$stats$mantissa.arc.test$p.value, "\n\n")

# Get suspects - needs data frame
compliant_df <- data.frame(values = compliant_values)
suspects_obj1 <- benford.analysis::getSuspects(benford_obj1, compliant_df, how.many = 2)
cat("Suspects:", if(is.null(suspects_obj1) || nrow(suspects_obj1) == 0) "None" else nrow(suspects_obj1), "\n\n")

# Create benford instance to test interpretation
results1 <- create_mock_results()
benford_instance <- benfordClass$new(
    options = list(vars = 'values', number_of_digits = 1),
    data = compliant_df,
    datasetId = '',
    analysisId = '',
    revision = 0,
    create = function(...) { results1 }
)

# Test .interpretResults directly
interpret_func <- benford_instance$.__enclos_env__$private$.interpretResults
result1 <- interpret_func(benford_obj1, suspects_obj1, compliant_values)

cat("\n=== Interpretation Result ===\n")
cat("Total observations:", result1$total_observations, "\n")
cat("MAD value:", result1$mad_value, "\n")
cat("MAD conformity:", result1$mad_conformity, "\n")
cat("Chi-square p-value:", result1$chisq_pvalue, "\n")
cat("Concern level:", result1$concern_level, "\n")
cat("Clinical interpretation:", result1$clinical_interpretation, "\n\n")

cat("\n\n=== TEST 2: Manipulated Data (Uniform) ===\n")
set.seed(456)
manipulated_values <- runif(500, min = 100, max = 999)

cat("Sample values:", head(manipulated_values, 10), "\n")
cat("Total observations:", length(manipulated_values), "\n\n")

# Run benford analysis
benford_obj2 <- benford.analysis::benford(manipulated_values, number.of.digits = 1)
cat("MAD:", benford_obj2$MAD, "\n")
cat("MAD Conformity:", as.character(benford_obj2$MAD.conformity), "\n")
cat("Chi-square p-value:", benford_obj2$stats$chisq$p.value, "\n")
cat("Mantissa Arc Test p-value:", benford_obj2$stats$mantissa.arc.test$p.value, "\n\n")

# Get suspects
manipulated_df <- data.frame(values = manipulated_values)
suspects_obj2 <- benford.analysis::getSuspects(benford_obj2, manipulated_df, how.many = 2)
cat("Suspects:", if(is.null(suspects_obj2) || nrow(suspects_obj2) == 0) "None" else nrow(suspects_obj2), "\n\n")

# Test interpretation
result2 <- interpret_func(benford_obj2, suspects_obj2, manipulated_values)

cat("\n=== Interpretation Result ===\n")
cat("Total observations:", result2$total_observations, "\n")
cat("MAD value:", result2$mad_value, "\n")
cat("MAD conformity:", result2$mad_conformity, "\n")
cat("Chi-square p-value:", result2$chisq_pvalue, "\n")
cat("Concern level:", result2$concern_level, "\n")
cat("Clinical interpretation:", result2$clinical_interpretation, "\n")
