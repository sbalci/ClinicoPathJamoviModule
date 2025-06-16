# =============================================================================
# Interrater Reliability Test Data Generation
# =============================================================================
# 
# Description: Generates comprehensive interrater reliability test data for
#              pathology applications with realistic agreement patterns
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# Data includes:
# - 5 pathology subspecialty datasets with varying rater counts
# - Realistic disagreement patterns based on literature
# - Adjacent category errors more common than random errors
# - Gold standard diagnoses for accuracy assessment
# 
# =============================================================================

# Load required libraries with error checking
required_packages <- c("here", "dplyr", "tibble")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed. Please install it with: install.packages('", pkg, "')", sep = ""))
  }
}

set.seed(42)  # For reproducible results

# Create output directory if it doesn't exist
data_dir <- here::here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# =============================================================================
# PATHOLOGY INTERRATER RELIABILITY DATASETS
# =============================================================================

# Dataset 1: Breast Cancer Histologic Grade Assessment (3 pathologists)
# Based on Nottingham grading system
n_cases_breast <- 150

# Simulate realistic agreement patterns for breast cancer grading
# True grades with realistic distribution
true_grade <- sample(c("Grade_1", "Grade_2", "Grade_3"), n_cases_breast, 
                    replace = TRUE, prob = c(0.2, 0.5, 0.3))

# Pathologist 1 (experienced, high agreement with gold standard)
generate_rater_diagnosis <- function(true_diagnosis, agreement_rate = 0.85, adjacent_error_rate = 0.12) {
  n <- length(true_diagnosis)
  rater_diagnosis <- character(n)
  
  for (i in 1:n) {
    if (runif(1) < agreement_rate) {
      # Agree with true diagnosis
      rater_diagnosis[i] <- true_diagnosis[i]
    } else {
      # Disagree - more likely to be adjacent grade
      if (runif(1) < adjacent_error_rate / (1 - agreement_rate)) {
        # Adjacent grade error
        if (true_diagnosis[i] == "Grade_1") {
          rater_diagnosis[i] <- "Grade_2"
        } else if (true_diagnosis[i] == "Grade_2") {
          rater_diagnosis[i] <- sample(c("Grade_1", "Grade_3"), 1)
        } else {
          rater_diagnosis[i] <- "Grade_2"
        }
      } else {
        # Random error
        rater_diagnosis[i] <- sample(c("Grade_1", "Grade_2", "Grade_3"), 1)
      }
    }
  }
  return(rater_diagnosis)
}

breast_pathologist_1 <- generate_rater_diagnosis(true_grade, 0.87, 0.10)
breast_pathologist_2 <- generate_rater_diagnosis(true_grade, 0.82, 0.13)
breast_pathologist_3 <- generate_rater_diagnosis(true_grade, 0.79, 0.15)

breast_agreement_data <- tibble(
  Case_ID = paste0("BC", sprintf("%03d", 1:n_cases_breast)),
  Age = round(rnorm(n_cases_breast, 58, 12)),
  Tumor_Size_mm = round(rnorm(n_cases_breast, 22, 8)),
  True_Grade = as.factor(true_grade),
  Pathologist_1 = as.factor(breast_pathologist_1),
  Pathologist_2 = as.factor(breast_pathologist_2),
  Pathologist_3 = as.factor(breast_pathologist_3),
  Institution = sample(c("Academic", "Community", "Reference"), n_cases_breast, 
                      replace = TRUE, prob = c(0.4, 0.4, 0.2))
)

# Dataset 2: Prostate Cancer Gleason Scoring (4 urological pathologists)
n_cases_prostate <- 120

# Gleason score combinations (primary + secondary)
gleason_scores <- c("3+3=6", "3+4=7", "4+3=7", "4+4=8", "4+5=9", "5+4=9", "5+5=10")
true_gleason <- sample(gleason_scores, n_cases_prostate, replace = TRUE,
                      prob = c(0.25, 0.30, 0.20, 0.12, 0.08, 0.03, 0.02))

# Generate Gleason scores with realistic disagreement patterns
generate_gleason_rating <- function(true_scores, agreement_rate = 0.80) {
  n <- length(true_scores)
  rater_scores <- character(n)
  
  for (i in 1:n) {
    if (runif(1) < agreement_rate) {
      rater_scores[i] <- true_scores[i]
    } else {
      # Systematic disagreement patterns in Gleason scoring
      current_score <- true_scores[i]
      if (current_score == "3+3=6") {
        rater_scores[i] <- sample(c("3+4=7", "4+3=7"), 1, prob = c(0.7, 0.3))
      } else if (current_score == "3+4=7") {
        rater_scores[i] <- sample(c("3+3=6", "4+3=7", "4+4=8"), 1, prob = c(0.3, 0.5, 0.2))
      } else if (current_score == "4+3=7") {
        rater_scores[i] <- sample(c("3+4=7", "4+4=8"), 1, prob = c(0.6, 0.4))
      } else {
        # Random adjacent score
        all_scores <- gleason_scores
        current_idx <- which(all_scores == current_score)
        adjacent_idx <- pmax(1, pmin(length(all_scores), current_idx + sample(c(-1, 1), 1)))
        rater_scores[i] <- all_scores[adjacent_idx]
      }
    }
  }
  return(rater_scores)
}

prostate_agreement_data <- tibble(
  Case_ID = paste0("PC", sprintf("%03d", 1:n_cases_prostate)),
  Age = round(rnorm(n_cases_prostate, 65, 8)),
  PSA = round(rnorm(n_cases_prostate, 8.5, 4.2), 1),
  True_Gleason = as.factor(true_gleason),
  Urologist_A = as.factor(generate_gleason_rating(true_gleason, 0.85)),
  Urologist_B = as.factor(generate_gleason_rating(true_gleason, 0.82)),
  Urologist_C = as.factor(generate_gleason_rating(true_gleason, 0.78)),
  Urologist_D = as.factor(generate_gleason_rating(true_gleason, 0.80)),
  Biopsy_Cores = sample(6:12, n_cases_prostate, replace = TRUE)
)

# Dataset 3: Melanoma Breslow Thickness Assessment (2 dermatopathologists)
n_cases_melanoma <- 100

# Breslow thickness categories
breslow_categories <- c("≤1.0mm", "1.01-2.0mm", "2.01-4.0mm", ">4.0mm")
true_breslow <- sample(breslow_categories, n_cases_melanoma, replace = TRUE,
                      prob = c(0.45, 0.30, 0.15, 0.10))

generate_breslow_rating <- function(true_thickness, agreement_rate = 0.88) {
  n <- length(true_thickness)
  rater_thickness <- character(n)
  
  for (i in 1:n) {
    if (runif(1) < agreement_rate) {
      rater_thickness[i] <- true_thickness[i]
    } else {
      # Adjacent category errors more common
      current_idx <- which(breslow_categories == true_thickness[i])
      if (current_idx == 1) {
        rater_thickness[i] <- breslow_categories[2]
      } else if (current_idx == length(breslow_categories)) {
        rater_thickness[i] <- breslow_categories[current_idx - 1]
      } else {
        rater_thickness[i] <- sample(breslow_categories[c(current_idx - 1, current_idx + 1)], 1)
      }
    }
  }
  return(rater_thickness)
}

melanoma_agreement_data <- tibble(
  Case_ID = paste0("ML", sprintf("%03d", 1:n_cases_melanoma)),
  Age = round(rnorm(n_cases_melanoma, 52, 15)),
  Location = sample(c("Head/Neck", "Trunk", "Extremities"), n_cases_melanoma, 
                   replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  True_Breslow = as.factor(true_breslow),
  Dermatopathologist_1 = as.factor(generate_breslow_rating(true_breslow, 0.90)),
  Dermatopathologist_2 = as.factor(generate_breslow_rating(true_breslow, 0.87)),
  Ulceration = sample(c("Present", "Absent"), n_cases_melanoma, 
                     replace = TRUE, prob = c(0.3, 0.7))
)

# Dataset 4: Thyroid FNA Bethesda Classification (5 cytopathologists)
n_cases_thyroid <- 200

bethesda_categories <- c("I_Nondiagnostic", "II_Benign", "III_AUS", "IV_FN", "V_Suspicious", "VI_Malignant")
true_bethesda <- sample(bethesda_categories, n_cases_thyroid, replace = TRUE,
                       prob = c(0.05, 0.60, 0.15, 0.10, 0.05, 0.05))

generate_bethesda_rating <- function(true_category, agreement_rate = 0.75) {
  n <- length(true_category)
  rater_category <- character(n)
  
  for (i in 1:n) {
    if (runif(1) < agreement_rate) {
      rater_category[i] <- true_category[i]
    } else {
      # Bethesda disagreement patterns
      current <- true_category[i]
      if (current == "II_Benign") {
        rater_category[i] <- sample(c("I_Nondiagnostic", "III_AUS"), 1, prob = c(0.3, 0.7))
      } else if (current == "III_AUS") {
        rater_category[i] <- sample(c("II_Benign", "IV_FN"), 1, prob = c(0.6, 0.4))
      } else if (current == "IV_FN") {
        rater_category[i] <- sample(c("III_AUS", "V_Suspicious"), 1, prob = c(0.5, 0.5))
      } else {
        # Random adjacent category
        all_cats <- bethesda_categories
        current_idx <- which(all_cats == current)
        adjacent_options <- all_cats[pmax(1, pmin(length(all_cats), current_idx + c(-1, 1)))]
        rater_category[i] <- sample(adjacent_options, 1)
      }
    }
  }
  return(rater_category)
}

thyroid_agreement_data <- tibble(
  Case_ID = paste0("TH", sprintf("%03d", 1:n_cases_thyroid)),
  Age = round(rnorm(n_cases_thyroid, 48, 16)),
  Gender = sample(c("Male", "Female"), n_cases_thyroid, replace = TRUE, prob = c(0.2, 0.8)),
  Nodule_Size_cm = round(rnorm(n_cases_thyroid, 2.1, 0.8), 1),
  True_Bethesda = as.factor(true_bethesda),
  Cytopathologist_A = as.factor(generate_bethesda_rating(true_bethesda, 0.78)),
  Cytopathologist_B = as.factor(generate_bethesda_rating(true_bethesda, 0.75)),
  Cytopathologist_C = as.factor(generate_bethesda_rating(true_bethesda, 0.73)),
  Cytopathologist_D = as.factor(generate_bethesda_rating(true_bethesda, 0.76)),
  Cytopathologist_E = as.factor(generate_bethesda_rating(true_bethesda, 0.74)),
  Institution_Type = sample(c("Academic", "Community", "Commercial"), n_cases_thyroid,
                           replace = TRUE, prob = c(0.4, 0.4, 0.2))
)

# Dataset 5: Lung Cancer Subtyping (3 pulmonary pathologists)
n_cases_lung <- 80

lung_subtypes <- c("Adenocarcinoma", "Squamous_Cell", "Large_Cell", "Small_Cell", "NSCLC_NOS")
true_lung <- sample(lung_subtypes, n_cases_lung, replace = TRUE,
                   prob = c(0.50, 0.25, 0.08, 0.12, 0.05))

generate_lung_rating <- function(true_subtype, agreement_rate = 0.83) {
  n <- length(true_subtype)
  rater_subtype <- character(n)
  
  for (i in 1:n) {
    if (runif(1) < agreement_rate) {
      rater_subtype[i] <- true_subtype[i]
    } else {
      # Lung cancer subtyping disagreement patterns
      current <- true_subtype[i]
      if (current == "Adenocarcinoma") {
        rater_subtype[i] <- sample(c("NSCLC_NOS", "Large_Cell"), 1, prob = c(0.7, 0.3))
      } else if (current == "Squamous_Cell") {
        rater_subtype[i] <- sample(c("NSCLC_NOS", "Large_Cell"), 1, prob = c(0.6, 0.4))
      } else if (current == "Small_Cell") {
        rater_subtype[i] <- sample(c("Large_Cell", "NSCLC_NOS"), 1, prob = c(0.4, 0.6))
      } else {
        rater_subtype[i] <- sample(lung_subtypes[lung_subtypes != current], 1)
      }
    }
  }
  return(rater_subtype)
}

lung_agreement_data <- tibble(
  Case_ID = paste0("LC", sprintf("%03d", 1:n_cases_lung)),
  Age = round(rnorm(n_cases_lung, 67, 10)),
  Smoking_History = sample(c("Never", "Former", "Current"), n_cases_lung,
                          replace = TRUE, prob = c(0.1, 0.6, 0.3)),
  Stage = sample(c("I", "II", "III", "IV"), n_cases_lung,
                replace = TRUE, prob = c(0.3, 0.2, 0.3, 0.2)),
  True_Subtype = as.factor(true_lung),
  Pulmonary_Path_1 = as.factor(generate_lung_rating(true_lung, 0.85)),
  Pulmonary_Path_2 = as.factor(generate_lung_rating(true_lung, 0.83)),
  Pulmonary_Path_3 = as.factor(generate_lung_rating(true_lung, 0.81)),
  Biopsy_Type = sample(c("Transbronchial", "CT_Guided", "Surgical"), n_cases_lung,
                      replace = TRUE, prob = c(0.4, 0.35, 0.25))
)

# =============================================================================
# SAVE DATASETS
# =============================================================================

# Save as R data files
save(breast_agreement_data, file = file.path(data_dir, "breast_agreement_data.rda"))
save(prostate_agreement_data, file = file.path(data_dir, "prostate_agreement_data.rda"))
save(melanoma_agreement_data, file = file.path(data_dir, "melanoma_agreement_data.rda"))
save(thyroid_agreement_data, file = file.path(data_dir, "thyroid_agreement_data.rda"))
save(lung_agreement_data, file = file.path(data_dir, "lung_agreement_data.rda"))

# Save as CSV files for jamovi
write.csv(breast_agreement_data, file.path(data_dir, "breast_agreement_data.csv"), row.names = FALSE)
write.csv(prostate_agreement_data, file.path(data_dir, "prostate_agreement_data.csv"), row.names = FALSE)
write.csv(melanoma_agreement_data, file.path(data_dir, "melanoma_agreement_data.csv"), row.names = FALSE)
write.csv(thyroid_agreement_data, file.path(data_dir, "thyroid_agreement_data.csv"), row.names = FALSE)
write.csv(lung_agreement_data, file.path(data_dir, "lung_agreement_data.csv"), row.names = FALSE)

# Create a comprehensive combined dataset for testing all features
comprehensive_agreement_data <- tibble(
  Case_ID = paste0("AGR", sprintf("%03d", 1:100)),
  Specialty = sample(c("General_Path", "Dermatopathology", "GI_Path", "Breast_Path"), 100,
                    replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.2)),
  Difficulty = sample(c("Easy", "Moderate", "Difficult"), 100,
                     replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  True_Diagnosis = sample(c("Benign", "Borderline", "Malignant"), 100,
                         replace = TRUE, prob = c(0.5, 0.2, 0.3)),
  Rater_1 = sample(c("Benign", "Borderline", "Malignant"), 100, replace = TRUE),
  Rater_2 = sample(c("Benign", "Borderline", "Malignant"), 100, replace = TRUE),
  Rater_3 = sample(c("Benign", "Borderline", "Malignant"), 100, replace = TRUE),
  Rater_4 = sample(c("Benign", "Borderline", "Malignant"), 100, replace = TRUE),
  Experience_Years = sample(c("Resident", "Fellow", "Attending_<5yrs", "Attending_5-15yrs", "Attending_>15yrs"), 
                           100, replace = TRUE),
  Institution = sample(c("Academic", "Community", "Reference"), 100, replace = TRUE)
)

# Adjust agreement based on difficulty and experience (more realistic patterns)
for (i in 1:nrow(comprehensive_agreement_data)) {
  true_dx <- comprehensive_agreement_data$True_Diagnosis[i]
  difficulty <- comprehensive_agreement_data$Difficulty[i]
  
  # Base agreement rate by difficulty
  base_agreement <- switch(difficulty,
    "Easy" = 0.90,
    "Moderate" = 0.75,
    "Difficult" = 0.60
  )
  
  # Adjust each rater
  for (rater_col in c("Rater_1", "Rater_2", "Rater_3", "Rater_4")) {
    if (runif(1) > base_agreement) {
      # Disagreement - more likely to be adjacent category
      if (true_dx == "Benign") {
        comprehensive_agreement_data[[rater_col]][i] <- sample(c("Borderline", "Malignant"), 1, prob = c(0.8, 0.2))
      } else if (true_dx == "Borderline") {
        comprehensive_agreement_data[[rater_col]][i] <- sample(c("Benign", "Malignant"), 1, prob = c(0.6, 0.4))
      } else {
        comprehensive_agreement_data[[rater_col]][i] <- sample(c("Borderline", "Benign"), 1, prob = c(0.7, 0.3))
      }
    } else {
      comprehensive_agreement_data[[rater_col]][i] <- true_dx
    }
  }
}

# Convert to factors
comprehensive_agreement_data$True_Diagnosis <- as.factor(comprehensive_agreement_data$True_Diagnosis)
comprehensive_agreement_data$Rater_1 <- as.factor(comprehensive_agreement_data$Rater_1)
comprehensive_agreement_data$Rater_2 <- as.factor(comprehensive_agreement_data$Rater_2)
comprehensive_agreement_data$Rater_3 <- as.factor(comprehensive_agreement_data$Rater_3)
comprehensive_agreement_data$Rater_4 <- as.factor(comprehensive_agreement_data$Rater_4)
comprehensive_agreement_data$Specialty <- as.factor(comprehensive_agreement_data$Specialty)
comprehensive_agreement_data$Difficulty <- as.factor(comprehensive_agreement_data$Difficulty)
comprehensive_agreement_data$Experience_Years <- as.factor(comprehensive_agreement_data$Experience_Years)
comprehensive_agreement_data$Institution <- as.factor(comprehensive_agreement_data$Institution)

save(comprehensive_agreement_data, file = file.path(data_dir, "comprehensive_agreement_data.rda"))
write.csv(comprehensive_agreement_data, file.path(data_dir, "comprehensive_agreement_data.csv"), row.names = FALSE)

# =============================================================================
# SUMMARY AND VALIDATION
# =============================================================================

cat("=== Interrater Reliability Datasets Generated Successfully ===\n")
cat("\nDatasets created:\n")
cat("1. Breast Cancer Grading (3 pathologists):", nrow(breast_agreement_data), "cases\n")
cat("2. Prostate Gleason Scoring (4 urologists):", nrow(prostate_agreement_data), "cases\n")
cat("3. Melanoma Breslow Thickness (2 dermatopathologists):", nrow(melanoma_agreement_data), "cases\n")
cat("4. Thyroid Bethesda Classification (5 cytopathologists):", nrow(thyroid_agreement_data), "cases\n")
cat("5. Lung Cancer Subtyping (3 pulmonary pathologists):", nrow(lung_agreement_data), "cases\n")
cat("6. Comprehensive Agreement Dataset (4 raters):", nrow(comprehensive_agreement_data), "cases\n")

cat("\nKey features:\n")
cat("- Realistic agreement patterns based on pathology literature\n")
cat("- Adjacent category errors more common than random errors\n")
cat("- Varying difficulty levels and rater experience\n")
cat("- Gold standard diagnoses for accuracy assessment\n")
cat("- Multiple pathology subspecialties represented\n")

cat("\nExample agreement rates by dataset:\n")
# Calculate example agreement rates
for (dataset_name in c("breast", "prostate", "melanoma", "thyroid", "lung")) {
  dataset <- get(paste0(dataset_name, "_agreement_data"))
  if (dataset_name == "breast") {
    rater_cols <- c("Pathologist_1", "Pathologist_2", "Pathologist_3")
  } else if (dataset_name == "prostate") {
    rater_cols <- c("Urologist_A", "Urologist_B", "Urologist_C", "Urologist_D")
  } else if (dataset_name == "melanoma") {
    rater_cols <- c("Dermatopathologist_1", "Dermatopathologist_2")
  } else if (dataset_name == "thyroid") {
    rater_cols <- c("Cytopathologist_A", "Cytopathologist_B", "Cytopathologist_C", "Cytopathologist_D", "Cytopathologist_E")
  } else {
    rater_cols <- c("Pulmonary_Path_1", "Pulmonary_Path_2", "Pulmonary_Path_3")
  }
  
  # Calculate overall agreement (all raters agree)
  agreement_count <- 0
  for (i in 1:nrow(dataset)) {
    case_ratings <- unlist(dataset[i, rater_cols])
    if (length(unique(case_ratings)) == 1) {
      agreement_count <- agreement_count + 1
    }
  }
  agreement_rate <- round((agreement_count / nrow(dataset)) * 100, 1)
  dataset_name_title <- paste0(toupper(substring(dataset_name, 1, 1)), substring(dataset_name, 2))
  cat(paste0("- ", dataset_name_title, ": ", agreement_rate, "% complete agreement\n"))
}

cat("\n=== Data generation complete! ===\n")