# Create comprehensive test data for ggprism function
# This script generates various datasets for testing the ggprism function
# ggprism creates GraphPad Prism style plots with various plot types and features

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(789)

# 1. Basic Clinical Data (for violin plots and box plots)
clinical_prism_data <- data.frame(
  treatment_group = rep(c("Control", "Treatment A", "Treatment B"), each = 30),
  biomarker_level = c(
    rnorm(30, mean = 15, sd = 3),    # Control
    rnorm(30, mean = 20, sd = 4),    # Treatment A  
    rnorm(30, mean = 25, sd = 3.5)   # Treatment B
  ),
  patient_type = rep(c("Type I", "Type II"), times = 45),
  study_site = sample(c("Site A", "Site B", "Site C"), 90, replace = TRUE),
  stringsAsFactors = FALSE
)

# 2. Dose-Response Data (for scatter plots and line plots)
dose_response_prism_data <- data.frame(
  dose_concentration = rep(c(0, 0.1, 0.5, 1, 5, 10, 50, 100), each = 12),
  response_percentage = c(
    rnorm(12, mean = 5, sd = 2),     # 0 dose
    rnorm(12, mean = 10, sd = 3),    # 0.1
    rnorm(12, mean = 25, sd = 4),    # 0.5
    rnorm(12, mean = 40, sd = 5),    # 1
    rnorm(12, mean = 65, sd = 6),    # 5
    rnorm(12, mean = 80, sd = 4),    # 10
    rnorm(12, mean = 90, sd = 3),    # 50
    rnorm(12, mean = 95, sd = 2)     # 100
  ),
  compound_type = rep(c("Compound A", "Compound B"), times = 48),
  assay_batch = sample(c("Batch 1", "Batch 2", "Batch 3"), 96, replace = TRUE),
  stringsAsFactors = FALSE
)

# 3. Time Course Data (for line plots)
time_course_prism_data <- data.frame(
  time_point = rep(c(0, 2, 4, 6, 8, 12, 24, 48), each = 15),
  expression_level = c(
    rnorm(15, mean = 1.0, sd = 0.1),   # 0h baseline
    rnorm(15, mean = 1.2, sd = 0.15),  # 2h
    rnorm(15, mean = 1.8, sd = 0.2),   # 4h
    rnorm(15, mean = 2.5, sd = 0.25),  # 6h
    rnorm(15, mean = 3.2, sd = 0.3),   # 8h
    rnorm(15, mean = 4.0, sd = 0.35),  # 12h
    rnorm(15, mean = 2.8, sd = 0.3),   # 24h
    rnorm(15, mean = 1.5, sd = 0.2)    # 48h
  ),
  treatment_condition = rep(c("Vehicle", "Drug X", "Drug Y"), times = 40),
  cell_line = sample(c("Cell Line 1", "Cell Line 2"), 120, replace = TRUE),
  stringsAsFactors = FALSE
)

# 4. Multi-Group Statistical Comparison Data
statistical_comparison_data <- data.frame(
  disease_stage = rep(c("Stage I", "Stage II", "Stage III", "Stage IV"), each = 25),
  survival_months = c(
    rnorm(25, mean = 85, sd = 15),  # Stage I
    rnorm(25, mean = 65, sd = 18),  # Stage II
    rnorm(25, mean = 45, sd = 20),  # Stage III
    rnorm(25, mean = 25, sd = 12)   # Stage IV
  ),
  tumor_grade = rep(c("Low", "High"), times = 50),
  age_group = sample(c("Young", "Middle", "Elderly"), 100, replace = TRUE),
  stringsAsFactors = FALSE
)

# 5. Biomarker Expression Data (for publication quality plots)
biomarker_expression_data <- data.frame(
  gene_target = rep(c("Gene A", "Gene B", "Gene C", "Gene D", "Gene E"), each = 20),
  fold_change = c(
    rnorm(20, mean = 2.5, sd = 0.5),   # Gene A
    rnorm(20, mean = 4.2, sd = 0.8),   # Gene B
    rnorm(20, mean = 1.8, sd = 0.3),   # Gene C
    rnorm(20, mean = 6.1, sd = 1.2),   # Gene D
    rnorm(20, mean = 3.7, sd = 0.7)    # Gene E
  ),
  experiment_type = rep(c("qPCR", "RNA-seq"), times = 50),
  sample_condition = sample(c("Normal", "Disease"), 100, replace = TRUE),
  stringsAsFactors = FALSE
)

# 6. Pharmacokinetics Data (for scatter and line plots)
pharmacokinetics_data <- data.frame(
  time_hours = rep(c(0.5, 1, 2, 4, 8, 12, 24), each = 18),
  concentration_ng_ml = c(
    rnorm(18, mean = 45, sd = 8),    # 0.5h
    rnorm(18, mean = 85, sd = 12),   # 1h
    rnorm(18, mean = 65, sd = 10),   # 2h
    rnorm(18, mean = 40, sd = 8),    # 4h
    rnorm(18, mean = 20, sd = 5),    # 8h
    rnorm(18, mean = 8, sd = 3),     # 12h
    rnorm(18, mean = 2, sd = 1)      # 24h
  ),
  formulation = rep(c("Immediate Release", "Extended Release", "Delayed Release"), times = 42),
  subject_id = rep(paste0("Subject_", 1:21), each = 6),
  stringsAsFactors = FALSE
)

# 7. Colorblind Safe Test Data (for accessibility testing)
colorblind_safe_data <- data.frame(
  category = rep(c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5", "Cat6"), each = 15),
  measurement_value = c(
    rnorm(15, mean = 10, sd = 2),   # Cat1
    rnorm(15, mean = 15, sd = 3),   # Cat2
    rnorm(15, mean = 20, sd = 2.5), # Cat3
    rnorm(15, mean = 25, sd = 4),   # Cat4
    rnorm(15, mean = 18, sd = 3.5), # Cat5
    rnorm(15, mean = 12, sd = 2)    # Cat6
  ),
  accessibility_group = rep(c("Standard", "Colorblind Friendly"), times = 45),
  stringsAsFactors = FALSE
)

# 8. Minimal Test Data (for edge case testing)
minimal_prism_data <- data.frame(
  simple_x = c("A", "B"),
  simple_y = c(10, 15),
  simple_group = c("Group1", "Group1"),
  stringsAsFactors = FALSE
)

# 9. Large Dataset (for performance testing)
large_prism_dataset <- data.frame(
  treatment = sample(c("Placebo", "Low Dose", "Medium Dose", "High Dose"), 500, replace = TRUE),
  outcome_score = rnorm(500, mean = 50, sd = 15),
  patient_subgroup = sample(c("Subgroup A", "Subgroup B", "Subgroup C"), 500, replace = TRUE),
  study_center = sample(paste0("Center_", 1:10), 500, replace = TRUE),
  stringsAsFactors = FALSE
)

# 10. Publication Example Data (comprehensive showcase)
publication_prism_data <- data.frame(
  experimental_group = rep(c("Vehicle Control", "Compound A 1μM", "Compound A 10μM", 
                            "Compound B 1μM", "Compound B 10μM"), each = 24),
  cell_viability_percent = c(
    rnorm(24, mean = 98, sd = 3),    # Vehicle Control
    rnorm(24, mean = 85, sd = 5),    # Compound A 1μM
    rnorm(24, mean = 65, sd = 8),    # Compound A 10μM
    rnorm(24, mean = 75, sd = 6),    # Compound B 1μM
    rnorm(24, mean = 45, sd = 10)    # Compound B 10μM
  ),
  experiment_replicate = rep(c("Replicate 1", "Replicate 2", "Replicate 3"), times = 40),
  culture_conditions = sample(c("Standard", "Hypoxic"), 120, replace = TRUE),
  stringsAsFactors = FALSE
)

# 11. Comprehensive Multi-Variable Dataset
comprehensive_prism_data <- rbind(
  data.frame(
    dataset_type = "Clinical Trial",
    x_variable = clinical_prism_data$treatment_group,
    y_variable = clinical_prism_data$biomarker_level,
    group_variable = clinical_prism_data$patient_type,
    facet_variable = clinical_prism_data$study_site,
    stringsAsFactors = FALSE
  ),
  data.frame(
    dataset_type = "Dose Response",
    x_variable = as.character(dose_response_prism_data$dose_concentration),
    y_variable = dose_response_prism_data$response_percentage,
    group_variable = dose_response_prism_data$compound_type,
    facet_variable = dose_response_prism_data$assay_batch,
    stringsAsFactors = FALSE
  ),
  data.frame(
    dataset_type = "Expression Analysis",
    x_variable = biomarker_expression_data$gene_target,
    y_variable = biomarker_expression_data$fold_change,
    group_variable = biomarker_expression_data$experiment_type,
    facet_variable = biomarker_expression_data$sample_condition,
    stringsAsFactors = FALSE
  )
)

# Save all datasets
save(clinical_prism_data, file = "data/clinical_prism_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_prism_data, "data/clinical_prism_data.omv")
  message("✓ Created clinical_prism_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_prism_data, "data/clinical_prism_data.omv")
  message("✓ Created clinical_prism_data.omv")
}
save(dose_response_prism_data, file = "data/dose_response_prism_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(dose_response_prism_data, "data/dose_response_prism_data.omv")
  message("✓ Created dose_response_prism_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(dose_response_prism_data, "data/dose_response_prism_data.omv")
  message("✓ Created dose_response_prism_data.omv")
}
save(time_course_prism_data, file = "data/time_course_prism_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(time_course_prism_data, "data/time_course_prism_data.omv")
  message("✓ Created time_course_prism_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(time_course_prism_data, "data/time_course_prism_data.omv")
  message("✓ Created time_course_prism_data.omv")
}
save(statistical_comparison_data, file = "data/statistical_comparison_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(statistical_comparison_data, "data/statistical_comparison_data.omv")
  message("✓ Created statistical_comparison_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(statistical_comparison_data, "data/statistical_comparison_data.omv")
  message("✓ Created statistical_comparison_data.omv")
}
save(biomarker_expression_data, file = "data/biomarker_expression_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(biomarker_expression_data, "data/biomarker_expression_data.omv")
  message("✓ Created biomarker_expression_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(biomarker_expression_data, "data/biomarker_expression_data.omv")
  message("✓ Created biomarker_expression_data.omv")
}
save(pharmacokinetics_data, file = "data/pharmacokinetics_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(pharmacokinetics_data, "data/pharmacokinetics_data.omv")
  message("✓ Created pharmacokinetics_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(pharmacokinetics_data, "data/pharmacokinetics_data.omv")
  message("✓ Created pharmacokinetics_data.omv")
}
save(colorblind_safe_data, file = "data/colorblind_safe_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(colorblind_safe_data, "data/colorblind_safe_data.omv")
  message("✓ Created colorblind_safe_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(colorblind_safe_data, "data/colorblind_safe_data.omv")
  message("✓ Created colorblind_safe_data.omv")
}
save(minimal_prism_data, file = "data/minimal_prism_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(minimal_prism_data, "data/minimal_prism_data.omv")
  message("✓ Created minimal_prism_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(minimal_prism_data, "data/minimal_prism_data.omv")
  message("✓ Created minimal_prism_data.omv")
}
save(large_prism_dataset, file = "data/large_prism_dataset.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(large_prism_dataset, "data/large_prism_dataset.omv")
  message("✓ Created large_prism_dataset.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(large_prism_dataset, "data/large_prism_dataset.omv")
  message("✓ Created large_prism_dataset.omv")
}
save(publication_prism_data, file = "data/publication_prism_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(publication_prism_data, "data/publication_prism_data.omv")
  message("✓ Created publication_prism_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(publication_prism_data, "data/publication_prism_data.omv")
  message("✓ Created publication_prism_data.omv")
}
save(comprehensive_prism_data, file = "data/comprehensive_prism_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(comprehensive_prism_data, "data/comprehensive_prism_data.omv")
  message("✓ Created comprehensive_prism_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(comprehensive_prism_data, "data/comprehensive_prism_data.omv")
  message("✓ Created comprehensive_prism_data.omv")
}

# Display summary
cat("=== GGPRISM TEST DATA CREATED ===\n\n")

cat("1. Clinical Prism Data:\n")
print(head(clinical_prism_data))
cat("\n")

cat("2. Dose Response Prism Data:\n")
print(head(dose_response_prism_data))
cat("\n")

cat("3. Time Course Prism Data:\n")
print(head(time_course_prism_data))
cat("\n")

cat("4. Statistical Comparison Data:\n")
print(head(statistical_comparison_data))
cat("\n")

cat("5. Biomarker Expression Data:\n")
print(head(biomarker_expression_data))
cat("\n")

cat("6. Pharmacokinetics Data:\n")
print(head(pharmacokinetics_data))
cat("\n")

cat("7. Colorblind Safe Data:\n")
print(head(colorblind_safe_data))
cat("\n")

cat("8. Minimal Prism Data:\n")
print(minimal_prism_data)
cat("\n")

cat("9. Large Prism Dataset (sample):\n")
print(head(large_prism_dataset))
cat("\n")

cat("10. Publication Prism Data:\n")
print(head(publication_prism_data))
cat("\n")

cat("11. Comprehensive Dataset Summary:\n")
print(table(comprehensive_prism_data$dataset_type))

cat("\n=== DATASETS SAVED SUCCESSFULLY ===\n")
cat("Use these datasets to test the ggprism function with various configurations:\n")
cat("- clinical_prism_data: Basic clinical trial data for violin/box plots\n")
cat("- dose_response_prism_data: Dose-response curves for scatter/line plots\n")
cat("- time_course_prism_data: Time series data for line plots\n")
cat("- statistical_comparison_data: Multi-group comparisons with statistics\n")
cat("- biomarker_expression_data: Gene expression analysis data\n")
cat("- pharmacokinetics_data: PK/PD data for concentration-time plots\n")
cat("- colorblind_safe_data: Data for accessibility testing\n")
cat("- minimal_prism_data: Minimal dataset for edge case testing\n")
cat("- large_prism_dataset: Large dataset for performance testing\n")
cat("- publication_prism_data: Publication-quality example data\n")
cat("- comprehensive_prism_data: Combined dataset for multiple plot types\n")
