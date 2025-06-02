# Load the data from .rds file
oncology_data <- readRDS("data/oncology_response_data.rds")

# Rename the object
treatmentResponse <- oncology_data

# Save the new object to .rda file
save(treatmentResponse, file = "data/treatmentResponse.rda", version = 2)

# Print a success message
print("Successfully created data/treatmentResponse.rda with the treatmentResponse object.")
