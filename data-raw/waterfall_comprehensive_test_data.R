# Comprehensive Test Data Generation for Waterfall Function
# This script generates various test datasets to validate the waterfall function

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
set.seed(42)  # For reproducible results

# ================================
# Dataset 1: Basic Percentage Data
# ================================
# Simple percentage changes - good for basic functionality testing
waterfall_percentage_basic <- data.frame(
  PatientID = paste0("PT", sprintf("%02d", 1:20)),
  Response = c(
    # Complete Response (CR): <= -100%
    -100,
    # Partial Response (PR): <= -30%
    -85, -60, -45, -35, -32, -30,
    # Stable Disease (SD): > -30% to <= 20%
    -25, -20, -15, -10, -5, 0, 5, 10, 15, 20,
    # Progressive Disease (PD): > 20%
    25, 35, 50
  ),
  Treatment = rep(c("Drug A", "Drug B"), each = 10),
  stringsAsFactors = FALSE
)

# ================================
# Dataset 2: Raw Measurements Data
# ================================
# Longitudinal raw measurements for spider plot testing
waterfall_raw_longitudinal <- data.frame(
  PatientID = rep(paste0("PT", sprintf("%02d", 1:15)), each = 4),
  Time = rep(c(0, 2, 4, 6), 15),  # Months
  Measurement = c(
    # PT01: Good responder
    50, 30, 25, 20,
    # PT02: Partial responder
    60, 45, 40, 42,
    # PT03: Stable disease
    55, 50, 52, 48,
    # PT04: Progressive disease
    45, 50, 55, 65,
    # PT05: Complete response
    40, 20, 0, 0,
    # PT06-PT15: Mixed responses with realistic variability
    70, 55, 50, 45,
    35, 25, 20, 18,
    80, 60, 55, 50,
    65, 45, 40, 38,
    75, 70, 72, 75,
    42, 30, 25, 22,
    58, 40, 35, 32,
    90, 75, 65, 60,
    48, 35, 30, 28,
    52, 48, 45, 42
  ),
  stringsAsFactors = FALSE
)

# ================================
# Dataset 3: Edge Cases
# ================================
# Test edge cases and validation
waterfall_edge_cases <- data.frame(
  PatientID = paste0("PT", sprintf("%02d", 1:10)),
  Response = c(
    # Extreme values
    -100,    # Complete response
    -150,    # Invalid shrinkage (will be capped)
    500,     # Very large growth
    # Missing values
    NA,      # Missing response
    # Boundary values
    -30.0,   # Exact PR threshold
    -29.9,   # Just above PR threshold
    20.0,    # Exact SD threshold
    20.1,    # Just above SD threshold
    # Normal values
    -45, 
    15
  ),
  stringsAsFactors = FALSE
)

# ================================
# Dataset 4: Single Patient
# ================================
# Test minimum data scenario
waterfall_single_patient <- data.frame(
  PatientID = "PT001",
  Response = -45,
  stringsAsFactors = FALSE
)

# ================================
# Dataset 5: Raw Measurements with Missing Baseline
# ================================
# Test validation for missing baseline
waterfall_missing_baseline <- data.frame(
  PatientID = rep(paste0("PT", sprintf("%02d", 1:3)), each = 3),
  Time = rep(c(2, 4, 6), 3),  # No baseline (time = 0)
  Measurement = c(
    30, 25, 20,
    45, 40, 42,
    50, 52, 48
  ),
  stringsAsFactors = FALSE
)

# ================================
# Dataset 6: Oncology Clinical Trial
# ================================
# Realistic oncology trial data
waterfall_oncology_trial <- data.frame(
  PatientID = paste0("PT", sprintf("%03d", 1:50)),
  Response = c(
    # Simulate realistic distribution
    # ~10% complete response
    rep(-100, 5),
    # ~30% partial response  
    sample(seq(-85, -30, by = 5), 15, replace = TRUE),
    # ~40% stable disease
    sample(seq(-29, 20, by = 3), 20, replace = TRUE),
    # ~20% progressive disease
    sample(seq(21, 100, by = 5), 10, replace = TRUE)
  ),
  Age = sample(40:80, 50, replace = TRUE),
  Gender = sample(c("Male", "Female"), 50, replace = TRUE),
  Stage = sample(c("I", "II", "III", "IV"), 50, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)),
  Treatment = sample(c("Experimental", "Control"), 50, replace = TRUE),
  stringsAsFactors = FALSE
)

# ================================
# Dataset 7: Time-to-Event Data
# ================================
# For testing person-time analysis
waterfall_time_to_event <- data.frame(
  PatientID = rep(paste0("PT", sprintf("%02d", 1:12)), each = 5),
  Time = rep(c(0, 1, 2, 3, 4), 12),
  Measurement = c(
    # Generate realistic tumor measurement trajectories
    c(50, 45, 40, 35, 30),  # Steady decline
    c(60, 55, 50, 45, 40),  # Steady decline
    c(70, 60, 45, 35, 30),  # Rapid initial response
    c(55, 50, 45, 50, 55),  # Initial response then progression
    c(45, 40, 35, 30, 25),  # Continued response
    c(80, 75, 70, 75, 80),  # Minimal response
    c(65, 60, 55, 50, 45),  # Gradual response
    c(40, 35, 30, 25, 20),  # Strong response
    c(75, 70, 65, 70, 75),  # Stable
    c(85, 80, 75, 70, 65),  # Slow response
    c(35, 30, 25, 20, 15),  # Excellent response
    c(95, 90, 85, 90, 95)   # Stable
  ),
  stringsAsFactors = FALSE
)

# ================================
# Save all datasets
# ================================
# Save to data directory
datasets <- list(
  waterfall_percentage_basic = waterfall_percentage_basic,
  waterfall_raw_longitudinal = waterfall_raw_longitudinal,
  waterfall_edge_cases = waterfall_edge_cases,
  waterfall_single_patient = waterfall_single_patient,
  waterfall_missing_baseline = waterfall_missing_baseline,
  waterfall_oncology_trial = waterfall_oncology_trial,
  waterfall_time_to_event = waterfall_time_to_event
)

# Save as RDA files
for (name in names(datasets)) {
  assign(name, datasets[[name]])
  save(list = name, file = paste0("data/", name, ".rda"))
}

# Save as CSV files for easy inspection
for (name in names(datasets)) {
  write.csv(datasets[[name]], paste0("data/", name, ".csv"), row.names = FALSE)
}

# ================================
# Dataset Summary Information
# ================================
cat("=== Waterfall Function Test Datasets Created ===\n\n")

for (name in names(datasets)) {
  df <- datasets[[name]]
  cat(sprintf("Dataset: %s\n", name))
  cat(sprintf("  - Rows: %d\n", nrow(df)))
  cat(sprintf("  - Columns: %s\n", paste(names(df), collapse = ", ")))
  
  if ("Response" %in% names(df)) {
    cat(sprintf("  - Response range: %.1f to %.1f\n", 
                min(df$Response, na.rm = TRUE), 
                max(df$Response, na.rm = TRUE)))
  }
  
  if ("PatientID" %in% names(df)) {
    cat(sprintf("  - Unique patients: %d\n", length(unique(df$PatientID))))
  }
  
  if ("Time" %in% names(df)) {
    cat(sprintf("  - Time points: %s\n", paste(unique(df$Time), collapse = ", ")))
  }
  
  cat("\n")
}

# ================================
# Usage Examples
# ================================
cat("=== Usage Examples ===\n\n")

cat("# Basic percentage data:\n")
cat("waterfall(\n")
cat("  data = waterfall_percentage_basic,\n")
cat("  patientID = \"PatientID\",\n")
cat("  responseVar = \"Response\",\n")
cat("  inputType = \"percentage\"\n")
cat(")\n\n")

cat("# Raw measurements with time:\n")
cat("waterfall(\n")
cat("  data = waterfall_raw_longitudinal,\n")
cat("  patientID = \"PatientID\",\n")
cat("  responseVar = \"Measurement\",\n")
cat("  timeVar = \"Time\",\n")
cat("  inputType = \"raw\"\n")
cat(")\n\n")

cat("# Clinical trial data:\n")
cat("waterfall(\n")
cat("  data = waterfall_oncology_trial,\n")
cat("  patientID = \"PatientID\",\n")
cat("  responseVar = \"Response\",\n")
cat("  inputType = \"percentage\",\n")
cat("  showThresholds = TRUE,\n")
cat("  labelOutliers = TRUE\n")
cat(")\n")
