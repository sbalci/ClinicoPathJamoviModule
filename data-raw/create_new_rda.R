# Load the data from .rds file
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

oncology_data <- readRDS("data/oncology_response_data.rds")

# Rename the object
treatmentResponse <- oncology_data

# Save the new object to .rda file
save(treatmentResponse, file = "data/treatmentResponse.rda", version = 2)

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(treatmentResponse, "data/treatmentResponse.omv")
  message("✓ Created treatmentResponse.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(treatmentResponse, "data/treatmentResponse.omv")
  message("✓ Created treatmentResponse.omv")
}

# Print a success message
print("Successfully created data/treatmentResponse.rda with the treatmentResponse object.")
