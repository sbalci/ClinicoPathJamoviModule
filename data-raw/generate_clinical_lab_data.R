# Clinical Lab Data Generator for Lollipop Chart Testing
# Generates realistic clinical laboratory values for testing lollipop charts

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(42)

n_patients <- 60

# Generate clinical lab data
clinical_lab_data <- data.frame(
  patient_id = paste0("PT", sprintf("%03d", 1:n_patients)),

  # Treatment groups
  treatment_group = sample(c("Control", "Treatment A", "Treatment B"),
                          n_patients, replace = TRUE),

  # Disease severity
  disease_severity = sample(c("Mild", "Moderate", "Severe"),
                           n_patients, replace = TRUE, prob = c(0.4, 0.4, 0.2)),

  # Age groups
  age_group = sample(c("18-40", "41-60", "61-80", ">80"),
                    n_patients, replace = TRUE),

  # Hospital
  hospital = sample(c("Hospital A", "Hospital B", "Hospital C"),
                   n_patients, replace = TRUE),

  # Lab values (with realistic ranges and distributions)
  hemoglobin = round(rnorm(n_patients, mean = 13, sd = 2), 1),  # g/dL
  albumin = round(rnorm(n_patients, mean = 3.8, sd = 0.5), 1),  # g/dL
  creatinine = round(abs(rnorm(n_patients, mean = 1.0, sd = 0.3)), 2),  # mg/dL
  platelet_count = round(rnorm(n_patients, mean = 200, sd = 50), 0),  # K/uL
  white_blood_cells = round(abs(rnorm(n_patients, mean = 8, sd = 2)), 1)  # K/uL
)

# Introduce some severity-dependent relationships
clinical_lab_data$hemoglobin[clinical_lab_data$disease_severity == "Severe"] <-
  clinical_lab_data$hemoglobin[clinical_lab_data$disease_severity == "Severe"] - 2

clinical_lab_data$albumin[clinical_lab_data$disease_severity == "Severe"] <-
  clinical_lab_data$albumin[clinical_lab_data$disease_severity == "Severe"] - 0.5

# Ensure realistic bounds
clinical_lab_data$hemoglobin <- pmax(7, pmin(18, clinical_lab_data$hemoglobin))
clinical_lab_data$albumin <- pmax(2.0, pmin(5.5, clinical_lab_data$albumin))
clinical_lab_data$creatinine <- pmax(0.5, pmin(3.0, clinical_lab_data$creatinine))
clinical_lab_data$platelet_count <- pmax(50, pmin(450, clinical_lab_data$platelet_count))
clinical_lab_data$white_blood_cells <- pmax(2, pmin(20, clinical_lab_data$white_blood_cells))

# Save to data/ as RDA (for package use)
if (requireNamespace("usethis", quietly = TRUE)) {
  use_data_multi_format(clinical_lab_data, overwrite = TRUE, save_csv = TRUE)
  cat("✓ Generated clinical_lab_data with", nrow(clinical_lab_data), "rows\n")
  cat("✓ Saved to data/clinical_lab_data.rda\n")
} else {
  # Fallback if usethis not available
  save(clinical_lab_data, file = "data/clinical_lab_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_lab_data, "data/clinical_lab_data.omv")
  message("✓ Created clinical_lab_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_lab_data, "data/clinical_lab_data.omv")
  message("✓ Created clinical_lab_data.omv")
}
  cat("✓ Generated clinical_lab_data with", nrow(clinical_lab_data), "rows\n")
  cat("✓ Saved to data/clinical_lab_data.rda (usethis not available)\n")
}

# Also save as CSV for manual testing and jamovi
write.csv(clinical_lab_data, "data/clinical_lab_data.csv", row.names = FALSE)
cat("✓ Saved to data/clinical_lab_data.csv\n")

# Print summary
cat("\nData Summary:\n")
cat("Variables:", paste(names(clinical_lab_data), collapse = ", "), "\n")
cat("Categorical variables for grouping:", paste(c("treatment_group", "disease_severity", "age_group", "hospital"), collapse = ", "), "\n")
cat("Continuous variables (lab values):", paste(c("hemoglobin", "albumin", "creatinine", "platelet_count", "white_blood_cells"), collapse = ", "), "\n")
