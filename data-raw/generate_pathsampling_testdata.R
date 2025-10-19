# Pathology Sampling Test Data Generator
# Creates realistic test datasets with edge cases for validation

library(dplyr)

# Function to generate realistic pathology sampling data
generate_pathsampling_data <- function(n_cases = 60, scenario = "omentum") {

  set.seed(42)

  if (scenario == "omentum") {
    # Based on real omentum data patterns:
    # 55% detected in cassette 1
    # 77% by cassette 2
    # 85% by cassette 3
    # 95% by cassette 4
    # 100% by cassette 5

    first_detection_dist <- sample(1:5, n_cases, replace = TRUE,
                                    prob = c(0.55, 0.22, 0.08, 0.10, 0.05))

    # Total cassettes: mostly 5, but some variation
    total_cassettes <- pmax(first_detection_dist,
                            sample(c(4, 5, 5, 5, 6, 7), n_cases, replace = TRUE))

    # Clinical variables
    tumor_types <- c("Serous", "Endometrioid", "Clear cell", "Mucinous", "Carcinosarcoma")
    tumor_type <- sample(tumor_types, n_cases, replace = TRUE,
                         prob = c(0.60, 0.20, 0.08, 0.07, 0.05))

    macroscopic_tumor <- sample(c("Yes", "No"), n_cases, replace = TRUE,
                                 prob = c(0.75, 0.25))

    stage <- sample(c("IA", "IB", "IC", "IIA", "IIB", "IIIA", "IIIB", "IIIC", "IV"),
                    n_cases, replace = TRUE)

  } else if (scenario == "lymph_nodes") {
    # Lymph node dissection scenario
    # More variable detection patterns

    first_detection_dist <- sample(1:20, n_cases, replace = TRUE,
                                    prob = c(rep(0.08, 5), rep(0.05, 5), rep(0.02, 10)))

    total_cassettes <- pmax(first_detection_dist,
                            sample(5:25, n_cases, replace = TRUE))

    tumor_type <- sample(c("Colon", "Rectal", "Gastric", "Breast"), n_cases, replace = TRUE)
    macroscopic_tumor <- sample(c("T1", "T2", "T3", "T4"), n_cases, replace = TRUE)
    stage <- sample(c("N0", "N1", "N2"), n_cases, replace = TRUE)

  } else {  # serial_sections
    # Serial sections scenario - tight clustering

    first_detection_dist <- sample(1:10, n_cases, replace = TRUE,
                                    prob = c(0.40, 0.25, 0.15, 0.10, 0.05, 0.03, 0.01, 0.005, 0.003, 0.002))

    total_cassettes <- pmax(first_detection_dist,
                            sample(3:15, n_cases, replace = TRUE))

    tumor_type <- sample(c("Melanoma", "Breast", "Lung"), n_cases, replace = TRUE)
    macroscopic_tumor <- sample(c("Macro", "Micro", "ITC"), n_cases, replace = TRUE)
    stage <- sample(c("Sentinel", "Axillary", "Mediastinal"), n_cases, replace = TRUE)
  }

  # Patient demographics
  age <- round(rnorm(n_cases, mean = 60, sd = 12))
  age <- pmax(30, pmin(90, age))

  # Create data frame
  data.frame(
    case_id = sprintf("%s_%04d", toupper(scenario), 1:n_cases),
    patient_age = age,
    tumor_type = tumor_type,
    tumor_characteristic = macroscopic_tumor,
    disease_stage = stage,
    total_samples = total_cassettes,
    first_detection = first_detection_dist,
    stringsAsFactors = FALSE
  )
}

# Generate main test datasets
cat("Generating test datasets...\n\n")

# 1. Omentum sampling (primary dataset)
omentum_data <- generate_pathsampling_data(60, "omentum")
cat("✓ Generated omentum dataset: n =", nrow(omentum_data), "\n")

# 2. Lymph node dissection
lymph_node_data <- generate_pathsampling_data(80, "lymph_nodes")
cat("✓ Generated lymph node dataset: n =", nrow(lymph_node_data), "\n")

# 3. Serial sections
serial_section_data <- generate_pathsampling_data(50, "serial_sections")
cat("✓ Generated serial section dataset: n =", nrow(serial_section_data), "\n")

# 4. EDGE CASES AND VALIDATION TESTS
cat("\nGenerating edge case scenarios...\n")

# Edge case 1: Very small sample (n=5)
small_sample <- data.frame(
  case_id = sprintf("SMALL_%02d", 1:5),
  patient_age = c(45, 55, 60, 65, 70),
  tumor_type = rep("Serous", 5),
  tumor_characteristic = rep("Yes", 5),
  disease_stage = rep("IIIA", 5),
  total_samples = c(5, 5, 5, 5, 5),
  first_detection = c(1, 1, 2, 3, 4),
  stringsAsFactors = FALSE
)
cat("✓ Small sample (n=5) for low-power warning test\n")

# Edge case 2: Variables with special characters (tests escaping)
special_chars_data <- omentum_data[1:20, ]
names(special_chars_data)[6:7] <- c("Total Samples (n)", "First Detection #")
cat("✓ Special character variables for escaping test\n")

# Edge case 3: Missing values
missing_data <- omentum_data[1:30, ]
missing_data$total_samples[c(5, 10, 15)] <- NA
missing_data$first_detection[c(7, 14, 21)] <- NA
cat("✓ Missing values for validation test\n")

# Edge case 4: Invalid data (first detection > total)
invalid_data <- omentum_data[1:25, ]
invalid_data$first_detection[c(3, 8, 12)] <- invalid_data$total_samples[c(3, 8, 12)] + 1
cat("✓ Invalid data for error handling test\n")

# Edge case 5: Perfect detection (all found in first sample)
perfect_data <- data.frame(
  case_id = sprintf("PERFECT_%02d", 1:15),
  patient_age = sample(40:80, 15, replace = TRUE),
  tumor_type = sample(c("Serous", "Endometrioid"), 15, replace = TRUE),
  tumor_characteristic = rep("Yes", 15),
  disease_stage = rep("IIIB", 15),
  total_samples = rep(5, 15),
  first_detection = rep(1, 15),  # All detected in first sample
  stringsAsFactors = FALSE
)
cat("✓ Perfect detection scenario (100% in first sample)\n")

# Edge case 6: Late detection (testing high sample numbers)
late_detection_data <- data.frame(
  case_id = sprintf("LATE_%02d", 1:20),
  patient_age = sample(45:75, 20, replace = TRUE),
  tumor_type = rep("Carcinosarcoma", 20),
  tumor_characteristic = rep("No", 20),
  disease_stage = rep("IIIC", 20),
  total_samples = sample(10:15, 20, replace = TRUE),
  first_detection = sample(8:15, 20, replace = TRUE),
  stringsAsFactors = FALSE
)
cat("✓ Late detection scenario (sparse lesions)\n")

# Save datasets
cat("\nSaving datasets to data/ directory...\n")

# Main datasets
saveRDS(omentum_data, "data/pathsampling_omentum.rds")
write.csv(omentum_data, "data/pathsampling_omentum.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_omentum.{rds,csv}\n")

saveRDS(lymph_node_data, "data/pathsampling_lymphnodes.rds")
write.csv(lymph_node_data, "data/pathsampling_lymphnodes.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_lymphnodes.{rds,csv}\n")

saveRDS(serial_section_data, "data/pathsampling_sections.rds")
write.csv(serial_section_data, "data/pathsampling_sections.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_sections.{rds,csv}\n")

# Edge case datasets
saveRDS(small_sample, "data/pathsampling_small.rds")
write.csv(small_sample, "data/pathsampling_small.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_small.{rds,csv}\n")

saveRDS(special_chars_data, "data/pathsampling_specialchars.rds")
write.csv(special_chars_data, "data/pathsampling_specialchars.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_specialchars.{rds,csv}\n")

saveRDS(missing_data, "data/pathsampling_missing.rds")
write.csv(missing_data, "data/pathsampling_missing.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_missing.{rds,csv}\n")

saveRDS(invalid_data, "data/pathsampling_invalid.rds")
write.csv(invalid_data, "data/pathsampling_invalid.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_invalid.{rds,csv}\n")

saveRDS(perfect_data, "data/pathsampling_perfect.rds")
write.csv(perfect_data, "data/pathsampling_perfect.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_perfect.{rds,csv}\n")

saveRDS(late_detection_data, "data/pathsampling_late.rds")
write.csv(late_detection_data, "data/pathsampling_late.csv", row.names = FALSE)
cat("✓ Saved: pathsampling_late.{rds,csv}\n")

# Summary
cat("\n" , paste(rep("=", 60), collapse=""), "\n")
cat("TEST DATA GENERATION COMPLETE\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")
cat("Main Datasets:\n")
cat("  • Omentum (n=60) - Based on real research patterns\n")
cat("  • Lymph Nodes (n=80) - Variable detection\n")
cat("  • Serial Sections (n=50) - Tight clustering\n\n")
cat("Edge Case Datasets:\n")
cat("  • Small Sample (n=5) - Tests low power warning\n")
cat("  • Special Chars (n=20) - Tests variable escaping\n")
cat("  • Missing Values (n=30) - Tests validation\n")
cat("  • Invalid Data (n=25) - Tests error handling\n")
cat("  • Perfect Detection (n=15) - All in first sample\n")
cat("  • Late Detection (n=20) - Sparse lesions\n\n")
cat("All files saved to data/ directory\n")
cat("Ready for testing in jamovi!\n\n")
