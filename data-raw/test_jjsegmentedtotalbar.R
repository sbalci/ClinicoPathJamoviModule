# Test script for jjsegmentedtotalbar function
# Make sure to compile the module first with jmvtools::prepare()

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(ClinicoPath)

# Test 1: Simple data
cat("=== Test 1: Simple segmented bar chart ===\n")
simple_data <- data.frame(
    Category = rep(c("Group_A", "Group_B", "Group_C"), each = 3),
    Segment = rep(c("Positive", "Negative", "Uncertain"), 3),
    Value = c(30, 20, 10, 25, 35, 15, 40, 15, 5)
)

print("Simple test data:")
print(simple_data)

# Test 2: Clinical data
cat("\n=== Test 2: Clinical treatment response data ===\n")
clinical_data <- data.frame(
    Treatment = rep(c("Chemotherapy", "Immunotherapy", "Surgery", "Combination"), each = 4),
    Response = rep(c("Complete_Response", "Partial_Response", "Stable_Disease", "Progressive_Disease"), 4),
    Count = c(25, 35, 30, 10,  # Chemo
             40, 30, 20, 10,   # Immuno
             60, 25, 10, 5,    # Surgery
             35, 40, 15, 10)   # Combination
)

print("Clinical test data:")
print(clinical_data)

# Test 3: With faceting variable
cat("\n=== Test 3: Data with faceting variable ===\n")
faceted_data <- data.frame(
    Patient_Group = rep(c("Early_Stage", "Advanced_Stage"), each = 8),
    Treatment_Response = rep(rep(c("Complete_Response", "Partial_Response", "No_Response", "Progressive_Disease"), 2), 2),
    Hospital = rep(c("Hospital_A", "Hospital_B"), each = 4, times = 2),
    Count = c(45, 30, 15, 10,   # Early + Hospital_A
             50, 25, 20, 5,     # Early + Hospital_B  
             20, 35, 30, 15,    # Advanced + Hospital_A
             15, 40, 35, 10)    # Advanced + Hospital_B
)

print("Faceted test data:")
print(faceted_data)

cat("\n=== Data saved to CSV files ===\n")
cat("1. data/simple_test_data.csv\n")
cat("2. data/test_segmented_data.csv\n") 
cat("3. data/test_clinical_data.csv\n")
cat("\nTo test in jamovi:\n")
cat("1. Load any of these CSV files from the data/ folder\n")
cat("2. Go to JJStatsPlotT > ClinicoPath Advanced Plots > Segmented Total Bar Charts\n")
cat("3. Set variables:\n")
cat("   - Category Variable (X-axis): Treatment/Category/Patient_Group\n")
cat("   - Value Variable (Y-axis): Count/Value\n")
cat("   - Segment Variable (Fill): Response/Segment/Treatment_Response\n")
cat("   - Panel Variable (Optional): Hospital (for faceted data)\n")
