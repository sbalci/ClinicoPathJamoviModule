# =============================================================================
# Oncology Response Data Generation
# =============================================================================
# 
# Description: Generates comprehensive tumor response datasets for testing
#              waterfall plots and oncology response analysis functions
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# Data includes:
# - Single response values per patient (waterfall plot data)
# - Longitudinal response data (time series)
# - Various measurement scenarios (raw vs percentage)
# - RECIST criteria compliant response categories
# 
# =============================================================================

# Load required libraries
if (!require(here, quietly = TRUE)) {
  stop("Package 'here' is required but not installed. Please install it with: install.packages('here')")
}

# Set seed for reproducibility
set.seed(42)

# Create output directory if it doesn't exist
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# =============================================================================
# Dataset 1: Main Oncology Response Data (for waterfall plots)
# =============================================================================

# Parameters
n <- 250
patient_ids <- sprintf("PT%04d", 1:n)

# Function to generate responses within RECIST categories
generate_response <- function(category) {
  switch(category,
         "CR" = -100,  # Complete response is always -100%
         "PR" = runif(1, min = -95, max = -30),  # Partial response: -30% to -95%
         "SD" = runif(1, min = -29.9, max = 19.9),  # Stable disease: -30% to +20%
         "PD" = runif(1, min = 20, max = 150)  # Progressive disease: >20%
  )
}

# Generate response categories with realistic distribution
# CR: ~8%, PR: ~25%, SD: ~45%, PD: ~22%
response_categories <- sample(
  c("CR", "PR", "SD", "PD"),
  size = n,
  replace = TRUE,
  prob = c(0.08, 0.25, 0.45, 0.22)
)

# Generate responses based on categories
responses <- sapply(response_categories, generate_response)

# Create main oncology dataset
oncology_response_data <- data.frame(
  PatientID = patient_ids,
  ResponseValue = round(responses, 1)
)

# Add some random missing data (about 5%)
missing_indices <- sample(1:n, size = round(0.05 * n))
oncology_response_data$ResponseValue[missing_indices] <- NA

# =============================================================================
# Dataset 2: Longitudinal Response Data (time series)
# =============================================================================

set.seed(123)

# Generate 10 patients with measurements at 5 time points
longitudinal_patient_ids <- paste0("PT", sprintf("%03d", 1:10))
times <- c(0, 2, 4, 6, 8) # months

# Create longitudinal data frame
tumor_response_longitudinal <- data.frame(
  PatientID = rep(longitudinal_patient_ids, each = length(times)),
  Time = rep(times, times = length(longitudinal_patient_ids))
)

# Define response patterns for different patient types
responses <- list(
  # Complete response
  PT001 = c(0, -45, -80, -95, -100),
  # Partial response
  PT002 = c(0, -20, -35, -40, -38),
  # Stable disease with slight improvement
  PT003 = c(0, -10, -15, -18, -15),
  # Stable disease
  PT004 = c(0, 5, 8, 12, 15),
  # Progressive disease
  PT005 = c(0, 15, 25, 35, 45),
  # Late response
  PT006 = c(0, 5, -25, -40, -42),
  # Initial response then progression
  PT007 = c(0, -30, -35, -20, 15),
  # Rapid progression
  PT008 = c(0, 30, 50, 70, 85),
  # Delayed progression
  PT009 = c(0, -5, 0, 22, 35),
  # Mixed response
  PT010 = c(0, -20, -15, -10, -5)
)

# Add responses to data frame
tumor_response_longitudinal$Response <- unlist(responses)

# =============================================================================
# Dataset 3: Multiple Response Measurement Scenarios
# =============================================================================

# Scenario 1: Raw measurements with time
raw_with_time <- data.frame(
  PatientID = rep(paste0("PT", sprintf("%03d", 1:5)), each = 4),
  Time = rep(c(0, 2, 4, 6), times = 5),
  Measurement = c(
    50, 30, 20, 10,    # PT001: Good responder (50mm → 10mm)
    40, 30, 25, 28,    # PT002: Partial responder with rebound
    30, 28, 32, 33,    # PT003: Stable disease
    45, 50, 60, 70,    # PT004: Progressive disease
    25, 10, 5, 0       # PT005: Complete response
  )
)

# Scenario 2: Pre-calculated percentages with time
percent_with_time <- data.frame(
  PatientID = rep(paste0("PT", sprintf("%03d", 1:5)), each = 4),
  Time = rep(c(0, 2, 4, 6), times = 5),
  Response = c(
    0, -40, -60, -80,    # PT001: Progressive reduction
    0, -25, -35, -30,    # PT002: Partial response with rebound
    0, -10, 5, 15,       # PT003: Stable disease
    0, 10, 25, 35,       # PT004: Progressive disease
    0, -60, -90, -100    # PT005: Complete response
  )
)

# =============================================================================
# Save All Datasets
# =============================================================================

# Save main oncology response data
output_csv <- file.path(data_dir, "oncology_response_data.csv")
output_rda <- file.path(data_dir, "oncology_response_data.rda")

write.csv(oncology_response_data, output_csv, row.names = FALSE)
save(oncology_response_data, file = output_rda)

# Save longitudinal data
write.csv(tumor_response_longitudinal, file.path(data_dir, "tumor_response_longitudinal.csv"), row.names = FALSE)
save(tumor_response_longitudinal, file = file.path(data_dir, "tumor_response_longitudinal.rda"))

# Save measurement scenarios
write.csv(raw_with_time, file.path(data_dir, "raw_with_time.csv"), row.names = FALSE)
write.csv(percent_with_time, file.path(data_dir, "percent_with_time.csv"), row.names = FALSE)

save(raw_with_time, file = file.path(data_dir, "raw_with_time.rda"))
save(percent_with_time, file = file.path(data_dir, "percent_with_time.rda"))

# Create combined examples dataset
tumor_response_examples <- list(
  main_data = oncology_response_data,
  longitudinal = tumor_response_longitudinal,
  raw_with_time = raw_with_time,
  percent_with_time = percent_with_time
)
save(tumor_response_examples, file = file.path(data_dir, "tumor_response_examples.rda"))

# =============================================================================
# Print Summary Statistics
# =============================================================================

cat("\n=== Oncology Response Data Generation Summary ===\n")
cat("Total patients in main dataset:", n, "\n")
cat("Missing values:", sum(is.na(oncology_response_data$ResponseValue)), "\n")

# Calculate response distribution
responses_clean <- oncology_response_data$ResponseValue[!is.na(oncology_response_data$ResponseValue)]
response_summary <- table(cut(responses_clean,
                              breaks = c(-Inf, -100, -30, 20, Inf),
                              labels = c("CR", "PR", "SD", "PD"),
                              right = TRUE))
response_pct <- round(prop.table(response_summary) * 100, 1)

cat("\nRECIST Response Distribution:\n")
for(i in 1:length(response_summary)) {
  cat(sprintf("  %s: n=%d (%.1f%%)\n",
              names(response_summary)[i],
              response_summary[i],
              response_pct[i]))
}

cat("\nDatasets generated:\n")
cat("  - oncology_response_data: Main waterfall plot data\n")
cat("  - tumor_response_longitudinal: Time series data\n")
cat("  - raw_with_time: Raw measurements over time\n")
cat("  - percent_with_time: Pre-calculated percentages over time\n")
cat("  - tumor_response_examples: Combined dataset list\n")

cat("\n=== Data generation completed successfully ===\n")