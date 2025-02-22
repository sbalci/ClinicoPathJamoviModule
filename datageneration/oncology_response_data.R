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



# Create example data
set.seed(123) # For reproducibility

# Generate 10 patients with measurements at 5 time points
patient_ids <- paste0("PT", 1:10)
times <- c(0, 2, 4, 6, 8) # months

# Create empty data frame
example_data <- data.frame(
  PatientID = rep(patient_ids, each = length(times)),
  Time = rep(times, times = length(patient_ids))
)

# Generate response patterns
set.seed(123)
responses <- list(
  # Complete response
  PT1 = c(0, -45, -80, -95, -100),
  # Partial response
  PT2 = c(0, -20, -35, -40, -38),
  # Stable disease with slight improvement
  PT3 = c(0, -10, -15, -18, -15),
  # Stable disease
  PT4 = c(0, 5, 8, 12, 15),
  # Progressive disease
  PT5 = c(0, 15, 25, 35, 45),
  # Late response
  PT6 = c(0, 5, -25, -40, -42),
  # Initial response then progression
  PT7 = c(0, -30, -35, -20, 15),
  # Rapid progression
  PT8 = c(0, 30, 50, 70, 85),
  # Delayed progression
  PT9 = c(0, -5, 0, 22, 35),
  # Mixed response
  PT10 = c(0, -20, -15, -10, -5)
)

# Add responses to data frame
example_data$Response <- unlist(responses)

# View first few rows
head(example_data, 10)

write.csv(example_data, "./data/tumor_response_data.csv")




# Scenario 1: Raw measurements with time
raw_with_time <- data.frame(
  PatientID = rep(paste0("PT", 1:5), each = 4),  # 5 patients, 4 timepoints each
  Time = rep(c(0, 2, 4, 6), times = 5),          # Baseline and 3 follow-ups
  Measurement = c(
    # PT1: Good responder
    50, 30, 20, 10,    # Starting at 50mm, shrinking to 10mm
    # PT2: Partial responder
    40, 30, 25, 28,    # Starting at 40mm, initial response then slight growth
    # PT3: Stable disease
    30, 28, 32, 33,    # Starting at 30mm, minor fluctuations
    # PT4: Progressive disease
    45, 50, 60, 70,    # Starting at 45mm, steady growth
    # PT5: Complete response
    25, 10, 5, 0       # Starting at 25mm, disappearing completely
  )
)

# Scenario 2: Raw measurements without time (single timepoint)
raw_no_time <- data.frame(
  PatientID = paste0("PT", 1:10),
  Measurement = c(
    50, 40, 30, 45, 25,     # Baseline measurements
    45, 35, 28, 55, 20      # Current measurements
  )
)

# Scenario 3: Pre-calculated percentages with time
percent_with_time <- data.frame(
  PatientID = rep(paste0("PT", 1:5), each = 4),
  Time = rep(c(0, 2, 4, 6), times = 5),
  Response = c(
    # PT1: Good responder
    0, -40, -60, -80,    # Baseline 0%, progressing to 80% reduction
    # PT2: Partial responder
    0, -25, -35, -30,    # Baseline 0%, ~30% reduction
    # PT3: Stable disease
    0, -10, 5, 15,       # Baseline 0%, minor changes
    # PT4: Progressive disease
    0, 10, 25, 35,       # Baseline 0%, progressive growth
    # PT5: Complete response
    0, -60, -90, -100    # Baseline 0%, complete disappearance
  )
)

# Scenario 4: Pre-calculated percentages without time
percent_no_time <- data.frame(
  PatientID = paste0("PT", 1:10),
  Response = c(
    -80, -30, 15, 35, -100,    # Various responses
    -45, -25, 10, 25, -65      # Mix of responses
  )
)

# Save each dataset as CSV
write.csv(raw_with_time, "./data/raw_with_time.csv", row.names = FALSE)
write.csv(raw_no_time, "./data/raw_no_time.csv", row.names = FALSE)
write.csv(percent_with_time, "./data/percent_with_time.csv", row.names = FALSE)
write.csv(percent_no_time, "./data/percent_no_time.csv", row.names = FALSE)

# Save as .rda files
save(raw_with_time, file = "./data/raw_with_time.rda")
save(raw_no_time, file = "./data/raw_no_time.rda")
save(percent_with_time, file = "./data/percent_with_time.rda")
save(percent_no_time, file = "./data/percent_no_time.rda")

# Alternative: save all datasets in a single .rda file
tumor_response_examples <- list(
  raw_with_time = raw_with_time,
  raw_no_time = raw_no_time,
  percent_with_time = percent_with_time,
  percent_no_time = percent_no_time
)
save(tumor_response_examples, file = "./data/tumor_response_examples.rda")
