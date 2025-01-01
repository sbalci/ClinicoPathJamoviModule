# Set seed for reproducibility
set.seed(42)

# Generate 250 patient IDs
n <- 250
patient_ids <- sprintf("PT%04d", 1:n)

# Create realistic response distribution
# - Complete Response (CR): ~5-10%
# - Partial Response (PR): ~20-30%
# - Stable Disease (SD): ~40-50%
# - Progressive Disease (PD): ~20-25%

# Function to generate responses within RECIST categories
generate_response <- function(category) {
  switch(category,
         "CR" = -100,  # Complete response is always -100%
         "PR" = runif(1, min = -95, max = -30),  # Partial response: -30% to -95%
         "SD" = runif(1, min = -29.9, max = 19.9),  # Stable disease: -30% to +20%
         "PD" = runif(1, min = 20, max = 150)  # Progressive disease: >20% (some can be large)
  )
}

# Generate response categories with realistic distribution
response_categories <- sample(
  c("CR", "PR", "SD", "PD"),
  size = n,
  replace = TRUE,
  prob = c(0.08, 0.25, 0.45, 0.22)  # Typical distribution in trials
)

# Generate responses based on categories
responses <- sapply(response_categories, generate_response)

# Create the dataframe
oncology_data <- data.frame(
  PatientID = patient_ids,
  ResponseValue = round(responses, 1)
)

# Add some random missing data (about 5%)
missing_indices <- sample(1:n, size = round(0.05 * n))
oncology_data$ResponseValue[missing_indices] <- NA

# Print summary statistics
cat("Summary of Response Data:\n")
cat("------------------------\n")
cat("Total Patients:", n, "\n")
cat("Missing Values:", sum(is.na(oncology_data$ResponseValue)), "\n\n")

# Calculate and print response categories
responses_clean <- oncology_data$ResponseValue[!is.na(oncology_data$ResponseValue)]
response_summary <- table(cut(responses_clean,
                              breaks = c(-Inf, -100, -30, 20, Inf),
                              labels = c("CR", "PR", "SD", "PD"),
                              right = TRUE
))
response_pct <- round(prop.table(response_summary) * 100, 1)

cat("Response Distribution:\n")
for(i in 1:length(response_summary)) {
  cat(sprintf("%s: n=%d (%.1f%%)\n",
              names(response_summary)[i],
              response_summary[i],
              response_pct[i]))
}

# Preview first few rows
cat("\nFirst few rows of the dataset:\n")
head(oncology_data)

saveRDS(oncology_data, "./data/oncology_response_data.rds")

save.image("oncology_response_data.RData")

# Save data
write.csv(oncology_data, "./data/oncology_response_data.csv", row.names = FALSE)
