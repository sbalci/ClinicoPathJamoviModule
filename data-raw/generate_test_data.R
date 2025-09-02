# Generate test data for jjsegmentedtotalbar function
# This script creates test datasets and saves them to data/ folder

library(here)

# Create data directory if it doesn't exist
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
}

# Generate Dataset 1: Simple test data
cat("Generating simple_test_data.csv...\n")
simple_data <- data.frame(
  Category = rep(c("Group_A", "Group_B", "Group_C"), each = 3),
  Segment = rep(c("Positive", "Negative", "Uncertain"), 3),
  Value = c(30, 20, 10, 25, 35, 15, 40, 15, 5)
)

write.csv(simple_data, here("data", "simple_test_data.csv"), row.names = FALSE)
print(simple_data)

# Generate Dataset 2: Clinical treatment response data
cat("\nGenerating test_segmented_data.csv...\n")
clinical_data <- data.frame(
  Treatment = rep(c("Chemotherapy", "Immunotherapy", "Surgery", "Combination"), each = 4),
  Response = rep(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 4),
  Count = c(25, 35, 30, 10,  # Chemotherapy
           40, 30, 20, 10,   # Immunotherapy
           60, 25, 10, 5,    # Surgery
           35, 40, 15, 10)   # Combination
)

write.csv(clinical_data, here("data", "test_segmented_data.csv"), row.names = FALSE)
print(clinical_data)

# Generate Dataset 3: Complex data with faceting
cat("\nGenerating test_clinical_data.csv...\n")
faceted_data <- data.frame(
  Patient_Group = rep(rep(c("Early_Stage", "Advanced_Stage"), each = 4), 3),
  Treatment_Response = rep(c("Complete_Response", "Partial_Response", "No_Response", "Progressive_Disease"), 6),
  Hospital = rep(c("Hospital_A", "Hospital_B", "Hospital_C"), each = 8),
  Count = c(
    # Hospital A
    45, 30, 15, 10,   # Early_Stage
    20, 35, 30, 15,   # Advanced_Stage
    # Hospital B  
    50, 25, 20, 5,    # Early_Stage
    15, 40, 35, 10,   # Advanced_Stage
    # Hospital C
    40, 35, 20, 5,    # Early_Stage
    10, 30, 40, 20    # Advanced_Stage
  )
)

write.csv(faceted_data, here("data", "test_clinical_data.csv"), row.names = FALSE)
print(faceted_data)

cat("\n=== Test datasets generated successfully! ===\n")
cat("Files saved to data/ folder:\n")
cat("1. data/simple_test_data.csv - Basic 3x3 test data\n")
cat("2. data/test_segmented_data.csv - Clinical treatment responses\n")
cat("3. data/test_clinical_data.csv - Multi-hospital data with faceting\n")

cat("\n=== Usage Instructions ===\n")
cat("To test jjsegmentedtotalbar in jamovi:\n")
cat("1. Open jamovi and load any CSV file from data/ folder\n")
cat("2. Navigate to: JJStatsPlotT > ClinicoPath Advanced Plots > Segmented Total Bar Charts\n")
cat("3. Variable assignments:\n")
cat("   For simple_test_data.csv:\n")
cat("     - Category Variable (X): Category\n")
cat("     - Value Variable (Y): Value\n") 
cat("     - Segment Variable (Fill): Segment\n")
cat("   For test_segmented_data.csv:\n")
cat("     - Category Variable (X): Treatment\n")
cat("     - Value Variable (Y): Count\n")
cat("     - Segment Variable (Fill): Response\n")
cat("   For test_clinical_data.csv:\n")
cat("     - Category Variable (X): Patient_Group\n")
cat("     - Value Variable (Y): Count\n")
cat("     - Segment Variable (Fill): Treatment_Response\n")
cat("     - Panel Variable (Optional): Hospital\n")