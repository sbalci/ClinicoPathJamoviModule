# Example Scenarios for Diagnostic Sample Size Planning
# Based on Bujang MA (2023) Diagnostics 13(8):1390

# Scenario 1: High-Risk Population Colorectal Cancer Screening
# A blood-based biomarker test for detecting colorectal cancer in high-risk patients
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

colorectal_screening <- data.frame(
    scenario = "Colorectal Cancer Blood Test",
    population = "High-risk patients (age >50, family history)",
    prevalence = 0.10,  # 10% prevalence in high-risk population
    target_sensitivity = 0.95,
    target_specificity = 0.95,
    ci_width = 0.10,
    study_purpose = "diagnostic",
    expected_n_sens = 940,  # From Bujang Table 2
    expected_n_spec = 105,
    final_n = 940,
    notes = "Need excellent Sens AND Spec for diagnostic replacement"
)

# Scenario 2: Population-Based COVID-19 Antigen Test
# Rapid antigen test for screening asymptomatic individuals
covid_screening <- data.frame(
    scenario = "COVID-19 Rapid Antigen Test",
    population = "General population (asymptomatic screening)",
    prevalence = 0.05,  # 5% prevalence during outbreak
    target_sensitivity = 0.95,
    target_specificity = 0.50,  # Can sacrifice specificity for screening
    ci_width = 0.10,
    study_purpose = "screening_sens",
    expected_n_sens = 1880,  # From Bujang Table 3
    expected_n_spec = 424,
    final_n = 1880,
    notes = "Emphasize sensitivity to avoid missing cases"
)

# Scenario 3: Diabetic Retinopathy Screening
# Fundus photography + AI for diabetic retinopathy screening
diabetic_retinopathy <- data.frame(
    scenario = "AI-Based Diabetic Retinopathy Detection",
    population = "Diabetic patients",
    prevalence = 0.30,  # 30% prevalence of DR in diabetic patients
    target_sensitivity = 0.90,
    target_specificity = 0.90,
    ci_width = 0.20,  # Wider CI acceptable for screening
    study_purpose = "diagnostic",
    expected_n_sens = 147,  # Calculated from Bujang method
    expected_n_spec = 116,
    final_n = 147,
    notes = "Moderate precision sufficient for screening tool"
)

# Scenario 4: Rare Disease Biomarker Discovery
# New biomarker for rare genetic disorder
rare_disease_biomarker <- data.frame(
    scenario = "Rare Disease Biomarker (e.g., Fabry Disease)",
    population = "Suspected patients referred to genetics clinic",
    prevalence = 0.02,  # 2% prevalence in referred population
    target_sensitivity = 0.90,
    target_specificity = 0.90,
    ci_width = 0.10,
    study_purpose = "diagnostic",
    expected_n_sens = 7900,  # Very large N due to low prevalence
    expected_n_spec = 176,
    final_n = 7900,
    notes = "Low prevalence drives large sample size for sensitivity"
)

# Scenario 5: Lung Cancer Specificity-Focused Screening
# Low-dose CT for lung cancer with high specificity to reduce false positives
lung_cancer_screening <- data.frame(
    scenario = "Lung Cancer LDCT Screening",
    population = "Heavy smokers (>30 pack-years)",
    prevalence = 0.15,  # 15% prevalence in heavy smokers
    target_sensitivity = 0.50,  # Can sacrifice sensitivity
    target_specificity = 0.95,  # Need high specificity to reduce false positives
    ci_width = 0.10,
    study_purpose = "screening_spec",
    expected_n_sens = 4020,
    expected_n_spec = 176,
    final_n = 4020,
    notes = "Emphasize specificity to avoid unnecessary follow-up procedures"
)

# Scenario 6: Breast Cancer Mammography
# Mammography screening with moderate prevalence
breast_cancer_screening <- data.frame(
    scenario = "Digital Mammography Screening",
    population = "Women age 50-70",
    prevalence = 0.50,  # 50% enriched population (recalled for further testing)
    target_sensitivity = 0.90,
    target_specificity = 0.90,
    ci_width = 0.20,
    study_purpose = "diagnostic",
    expected_n_sens = 88,
    expected_n_spec = 88,
    final_n = 88,
    notes = "Moderate prevalence balanced sample size requirements"
)

# Combine all scenarios
diagnostic_sample_size_examples <- rbind(
    colorectal_screening,
    covid_screening,
    diabetic_retinopathy,
    rare_disease_biomarker,
    lung_cancer_screening,
    breast_cancer_screening
)

# Add non-response adjustment example
diagnostic_sample_size_examples$nonresponse_rate <- c(20, 15, 10, 25, 20, 15)
diagnostic_sample_size_examples$final_n_adjusted <- ceiling(
    diagnostic_sample_size_examples$final_n / (1 - diagnostic_sample_size_examples$nonresponse_rate/100)
)

# Validation data: Bujang 2023 Table 2 excerpts
# Used to validate Clopper-Pearson implementation
bujang_table2_validation <- data.frame(
    prevalence = c(0.05, 0.05, 0.10, 0.10, 0.20, 0.50, 0.50, 0.90),
    sensitivity = c(0.95, 0.70, 0.95, 0.90, 0.90, 0.90, 0.80, 0.95),
    specificity = c(0.95, 0.70, 0.95, 0.90, 0.90, 0.90, 0.80, 0.95),
    ci_width = c(0.10, 0.10, 0.10, 0.20, 0.20, 0.20, 0.20, 0.20),
    n_sens_expected = c(940, 3410, 940, 440, 220, 88, 140, 49),
    n_spec_expected = c(105, 379, 105, 49, 49, 88, 140, 941),
    n_total_expected = c(940, 3410, 940, 440, 220, 88, 140, 941)
)

# Save data
save(diagnostic_sample_size_examples,
     file = "data/diagnostic_sample_size_examples.rda",
     compress = "xz")

save(bujang_table2_validation,
     file = "data/bujang_table2_validation.rda",
     compress = "xz")

# Also save as CSV for easy viewing
write.csv(diagnostic_sample_size_examples,
          "data/diagnostic_sample_size_examples.csv",
          row.names = FALSE)

write.csv(bujang_table2_validation,
          "data/bujang_table2_validation.csv",
          row.names = FALSE)

message("✓ Created diagnostic_sample_size_examples.rda and .csv")
message("✓ Created bujang_table2_validation.rda and .csv")
message("✓ 6 clinical scenarios + 8 validation test cases")
