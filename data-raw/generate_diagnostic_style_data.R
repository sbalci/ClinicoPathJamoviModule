# Generate Comprehensive Diagnostic Style Clustering Test Data
# Based on Usubutun et al. 2012 methodology for identifying pathologist diagnostic "schools"

library(dplyr)
library(tibble)
library(tidyr)
set.seed(42)

# =============================================================================
# SCENARIO 1: Endometrial Pathology (Based on Original Usubutun Study)
# =============================================================================

generate_endometrial_diagnostic_data <- function() {
  n_cases <- 80
  n_pathologists <- 15
  
  # Create pathologist characteristics representing different "schools"
  pathologist_info <- tibble(
    pathologist_id = paste0("Path_", sprintf("%02d", 1:n_pathologists)),
    experience_years = c(5, 8, 12, 25, 30, 35, 3, 6, 15, 20, 28, 32, 7, 18, 22),
    experience_level = case_when(
      experience_years <= 10 ~ "Junior",
      experience_years <= 20 ~ "Mid-level", 
      TRUE ~ "Senior"
    ),
    training_institution = c(
      "Harvard", "Johns Hopkins", "Mayo", "Harvard", "Johns Hopkins", "Mayo",
      "Stanford", "UCSF", "Columbia", "Stanford", "UCSF", "Columbia",
      "European_School", "European_School", "Asian_School"
    ),
    current_institution = c(
      "Academic_A", "Academic_A", "Academic_B", "Academic_A", "Academic_A", "Academic_B",
      "Community_West", "Community_West", "Community_East", "Community_West", 
      "Community_West", "Community_East", "International", "International", "International"
    ),
    specialty_focus = c(
      "Gynecologic", "Gynecologic", "Gynecologic", "Gynecologic", "Gynecologic", "Gynecologic",
      "General", "General", "General", "General", "General", "General",
      "Gynecologic", "Gynecologic", "General"
    ),
    conservative_tendency = c(0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.6, 0.7, 0.4, 0.6, 0.7, 0.4, 0.3, 0.2, 0.5),
    specialist_training = c(rep(TRUE, 6), rep(FALSE, 6), rep(TRUE, 3))
  )
  
  # Generate cases with known difficulty levels and true diagnoses
  case_info <- tibble(
    case_id = paste0("Case_", sprintf("%03d", 1:n_cases)),
    true_diagnosis = sample(c("Benign", "EIN", "Carcinoma"), n_cases, 
                           prob = c(0.4, 0.35, 0.25), replace = TRUE),
    difficulty_level = sample(c("Easy", "Moderate", "Difficult"), n_cases,
                             prob = c(0.3, 0.5, 0.2), replace = TRUE),
    has_polyp = sample(c(TRUE, FALSE), n_cases, prob = c(0.3, 0.7), replace = TRUE),
    hormonal_effects = sample(c(TRUE, FALSE), n_cases, prob = c(0.25, 0.75), replace = TRUE),
    poor_quality = sample(c(TRUE, FALSE), n_cases, prob = c(0.15, 0.85), replace = TRUE)
  )
  
  # Generate diagnoses based on pathologist characteristics and case features
  diagnosis_matrix <- matrix(NA, nrow = n_cases, ncol = n_pathologists)
  colnames(diagnosis_matrix) <- pathologist_info$pathologist_id
  
  for (i in 1:n_cases) {
    true_dx <- case_info$true_diagnosis[i]
    difficulty <- case_info$difficulty_level[i]
    
    for (j in 1:n_pathologists) {
      path_info <- pathologist_info[j, ]
      
      # Base accuracy based on experience and specialty
      base_accuracy <- case_when(
        path_info$experience_level == "Junior" ~ 0.65,
        path_info$experience_level == "Mid-level" ~ 0.75,
        path_info$experience_level == "Senior" ~ 0.85
      )
      
      # Adjust for specialty
      if (path_info$specialty_focus == "Gynecologic") base_accuracy <- base_accuracy + 0.1
      
      # Adjust for case difficulty
      if (difficulty == "Moderate") base_accuracy <- base_accuracy - 0.1
      if (difficulty == "Difficult") base_accuracy <- base_accuracy - 0.2
      
      # Diagnostic style biases
      conservative_bias <- path_info$conservative_tendency
      
      # Generate diagnosis with style-specific patterns
      if (runif(1) < base_accuracy) {
        # Correct diagnosis
        diagnosis_matrix[i, j] <- true_dx
      } else {
        # Error based on style
        if (true_dx == "Benign") {
          # Conservative pathologists more likely to overcall
          if (runif(1) < conservative_bias) {
            diagnosis_matrix[i, j] <- "EIN"
          } else {
            diagnosis_matrix[i, j] <- "Benign"
          }
        } else if (true_dx == "EIN") {
          # Style differences in EIN vs Benign vs Carcinoma
          if (path_info$training_institution %in% c("Harvard", "Johns Hopkins", "Mayo")) {
            # "Academic style" - more likely to call EIN
            diagnosis_matrix[i, j] <- sample(c("Benign", "EIN", "Carcinoma"), 1, prob = c(0.3, 0.5, 0.2))
          } else if (path_info$training_institution %in% c("Stanford", "UCSF", "Columbia")) {
            # "West Coast style" - more conservative
            diagnosis_matrix[i, j] <- sample(c("Benign", "EIN", "Carcinoma"), 1, prob = c(0.5, 0.3, 0.2))
          } else {
            # "International style" - different pattern
            diagnosis_matrix[i, j] <- sample(c("Benign", "EIN", "Carcinoma"), 1, prob = c(0.2, 0.4, 0.4))
          }
        } else {
          # Carcinoma cases
          diagnosis_matrix[i, j] <- sample(c("Benign", "EIN", "Carcinoma"), 1, prob = c(0.1, 0.3, 0.6))
        }
      }
      
      # Additional confounding factors
      if (case_info$has_polyp[i] && runif(1) < 0.3) {
        # Polyp cases create diagnostic confusion
        diagnosis_matrix[i, j] <- sample(c("Benign", "EIN"), 1, prob = c(0.6, 0.4))
      }
      
      if (case_info$poor_quality[i] && runif(1) < 0.4) {
        # Poor quality cases increase disagreement
        diagnosis_matrix[i, j] <- sample(c("Benign", "EIN", "Carcinoma"), 1, prob = c(0.4, 0.4, 0.2))
      }
    }
  }
  
  # Combine into final dataset
  final_data <- cbind(case_info, as.data.frame(diagnosis_matrix))
  
  return(list(
    diagnosis_data = final_data,
    pathologist_info = pathologist_info,
    case_info = case_info
  ))
}

# =============================================================================
# SCENARIO 2: Breast Pathology Diagnostic Styles
# =============================================================================

generate_breast_diagnostic_data <- function() {
  n_cases <- 60
  n_pathologists <- 12
  
  pathologist_info <- tibble(
    pathologist_id = paste0("BreastPath_", sprintf("%02d", 1:n_pathologists)),
    experience_years = c(4, 8, 15, 25, 30, 35, 6, 12, 18, 22, 28, 32),
    experience_level = case_when(
      experience_years <= 10 ~ "Junior",
      experience_years <= 20 ~ "Mid-level",
      TRUE ~ "Senior"
    ),
    training_institution = c(
      "Memorial_Sloan", "MD_Anderson", "Dana_Farber", "Memorial_Sloan", 
      "MD_Anderson", "Dana_Farber", "European_Center", "European_Center",
      "Academic_General", "Academic_General", "Community_Training", "Community_Training"
    ),
    current_institution = c(
      "Cancer_Center_A", "Cancer_Center_A", "Cancer_Center_B", "Cancer_Center_A",
      "Cancer_Center_B", "Cancer_Center_B", "Community_A", "Community_A",
      "Community_B", "Community_B", "Community_C", "Community_C"
    ),
    specialty_focus = c(rep("Breast_Specialist", 6), rep("General_Pathologist", 6)),
    diagnostic_philosophy = c(
      "Conservative", "Moderate", "Aggressive", "Conservative", "Moderate", "Aggressive",
      "Conservative", "Conservative", "Moderate", "Moderate", "Aggressive", "Aggressive"
    )
  )
  
  case_info <- tibble(
    case_id = paste0("Breast_", sprintf("%03d", 1:n_cases)),
    true_diagnosis = sample(c("Benign", "Atypical", "DCIS", "Invasive"), n_cases,
                           prob = c(0.3, 0.25, 0.25, 0.2), replace = TRUE),
    lesion_size = sample(c("Small", "Medium", "Large"), n_cases, prob = c(0.4, 0.4, 0.2), replace = TRUE),
    nuclear_grade = sample(c("Low", "Intermediate", "High"), n_cases, prob = c(0.4, 0.4, 0.2), replace = TRUE),
    calcifications = sample(c(TRUE, FALSE), n_cases, prob = c(0.6, 0.4), replace = TRUE)
  )
  
  # Generate diagnoses with specialty-specific patterns
  diagnosis_matrix <- matrix(NA, nrow = n_cases, ncol = n_pathologists)
  colnames(diagnosis_matrix) <- pathologist_info$pathologist_id
  
  for (i in 1:n_cases) {
    true_dx <- case_info$true_diagnosis[i]
    
    for (j in 1:n_pathologists) {
      path_info <- pathologist_info[j, ]
      
      # Base accuracy
      base_accuracy <- case_when(
        path_info$specialty_focus == "Breast_Specialist" ~ 0.85,
        TRUE ~ 0.70
      )
      
      # Experience adjustment
      if (path_info$experience_level == "Senior") base_accuracy <- base_accuracy + 0.1
      if (path_info$experience_level == "Junior") base_accuracy <- base_accuracy - 0.1
      
      if (runif(1) < base_accuracy) {
        diagnosis_matrix[i, j] <- true_dx
      } else {
        # Style-based errors
        philosophy <- path_info$diagnostic_philosophy
        
        if (philosophy == "Conservative") {
          # Tend to undercall
          if (true_dx == "Invasive") diagnosis_matrix[i, j] <- sample(c("DCIS", "Invasive"), 1, prob = c(0.6, 0.4))
          else if (true_dx == "DCIS") diagnosis_matrix[i, j] <- sample(c("Atypical", "DCIS"), 1, prob = c(0.6, 0.4))
          else if (true_dx == "Atypical") diagnosis_matrix[i, j] <- sample(c("Benign", "Atypical"), 1, prob = c(0.6, 0.4))
          else diagnosis_matrix[i, j] <- "Benign"
        } else if (philosophy == "Aggressive") {
          # Tend to overcall
          if (true_dx == "Benign") diagnosis_matrix[i, j] <- sample(c("Benign", "Atypical"), 1, prob = c(0.4, 0.6))
          else if (true_dx == "Atypical") diagnosis_matrix[i, j] <- sample(c("Atypical", "DCIS"), 1, prob = c(0.4, 0.6))
          else if (true_dx == "DCIS") diagnosis_matrix[i, j] <- sample(c("DCIS", "Invasive"), 1, prob = c(0.4, 0.6))
          else diagnosis_matrix[i, j] <- "Invasive"
        } else {
          # Moderate - random error
          diagnosis_matrix[i, j] <- sample(c("Benign", "Atypical", "DCIS", "Invasive"), 1)
        }
      }
    }
  }
  
  final_data <- cbind(case_info, as.data.frame(diagnosis_matrix))
  
  return(list(
    diagnosis_data = final_data,
    pathologist_info = pathologist_info,
    case_info = case_info
  ))
}

# =============================================================================
# SCENARIO 3: Lymphoma Classification Styles
# =============================================================================

generate_lymphoma_diagnostic_data <- function() {
  n_cases <- 45
  n_pathologists <- 10
  
  pathologist_info <- tibble(
    pathologist_id = paste0("HemaPath_", sprintf("%02d", 1:n_pathologists)),
    experience_years = c(6, 10, 16, 24, 28, 32, 8, 14, 20, 26),
    experience_level = case_when(
      experience_years <= 12 ~ "Junior",
      experience_years <= 22 ~ "Mid-level",
      TRUE ~ "Senior"
    ),
    training_institution = c(
      "WHO_Classification", "WHO_Classification", "Traditional_US", "Traditional_US",
      "European_School", "European_School", "Asian_School", "Asian_School",
      "Modern_Molecular", "Modern_Molecular"
    ),
    current_institution = c(
      "Academic_Hemepath", "Academic_Hemepath", "Cancer_Center", "Cancer_Center",
      "International_A", "International_A", "International_B", "International_B",
      "Research_Institute", "Research_Institute"
    ),
    specialty_focus = rep("Hematopathology", 10),
    classification_approach = c(
      "WHO_Strict", "WHO_Moderate", "Morphology_First", "Morphology_First",
      "Molecular_Heavy", "Molecular_Heavy", "Clinical_Context", "Clinical_Context",
      "Integrated_Modern", "Integrated_Modern"
    )
  )
  
  case_info <- tibble(
    case_id = paste0("Lymphoma_", sprintf("%03d", 1:n_cases)),
    true_diagnosis = sample(
      c("Reactive", "DLBCL", "Follicular", "Marginal_Zone", "Mantle_Cell"), 
      n_cases, prob = c(0.25, 0.25, 0.2, 0.15, 0.15), replace = TRUE
    ),
    morphology_clarity = sample(c("Clear", "Ambiguous", "Difficult"), n_cases, 
                               prob = c(0.4, 0.4, 0.2), replace = TRUE),
    immunophenotype = sample(c("Typical", "Atypical", "Incomplete"), n_cases,
                           prob = c(0.5, 0.3, 0.2), replace = TRUE),
    molecular_available = sample(c(TRUE, FALSE), n_cases, prob = c(0.7, 0.3), replace = TRUE)
  )
  
  diagnosis_matrix <- matrix(NA, nrow = n_cases, ncol = n_pathologists)
  colnames(diagnosis_matrix) <- pathologist_info$pathologist_id
  
  for (i in 1:n_cases) {
    true_dx <- case_info$true_diagnosis[i]
    
    for (j in 1:n_pathologists) {
      path_info <- pathologist_info[j, ]
      
      # Base accuracy varies by approach
      base_accuracy <- case_when(
        path_info$classification_approach %in% c("WHO_Strict", "Integrated_Modern") ~ 0.80,
        path_info$classification_approach %in% c("WHO_Moderate", "Molecular_Heavy") ~ 0.75,
        TRUE ~ 0.70
      )
      
      # Adjust for case features
      if (case_info$morphology_clarity[i] == "Difficult") base_accuracy <- base_accuracy - 0.2
      if (case_info$immunophenotype[i] == "Atypical") base_accuracy <- base_accuracy - 0.1
      
      if (runif(1) < base_accuracy) {
        diagnosis_matrix[i, j] <- true_dx
      } else {
        # Style-specific error patterns
        approach <- path_info$classification_approach
        
        if (approach == "WHO_Strict") {
          # Conservative, prefer established categories
          diagnosis_matrix[i, j] <- sample(c("Reactive", "DLBCL", "Follicular"), 1, prob = c(0.4, 0.4, 0.2))
        } else if (approach == "Molecular_Heavy") {
          # Rely heavily on molecular data
          if (case_info$molecular_available[i]) {
            diagnosis_matrix[i, j] <- true_dx  # More accurate with molecular data
          } else {
            diagnosis_matrix[i, j] <- "Reactive"  # Default without molecular
          }
        } else if (approach == "Morphology_First") {
          # Traditional morphology-based
          if (case_info$morphology_clarity[i] == "Clear") {
            diagnosis_matrix[i, j] <- true_dx
          } else {
            diagnosis_matrix[i, j] <- sample(c("DLBCL", "Follicular", "Reactive"), 1)
          }
        } else {
          # Random error for other approaches
          diagnosis_matrix[i, j] <- sample(c("Reactive", "DLBCL", "Follicular", "Marginal_Zone", "Mantle_Cell"), 1)
        }
      }
    }
  }
  
  final_data <- cbind(case_info, as.data.frame(diagnosis_matrix))
  
  return(list(
    diagnosis_data = final_data,
    pathologist_info = pathologist_info,
    case_info = case_info
  ))
}

# =============================================================================
# Generate All Datasets
# =============================================================================

# Generate datasets
endometrial_data <- generate_endometrial_diagnostic_data()
breast_data <- generate_breast_diagnostic_data()
lymphoma_data <- generate_lymphoma_diagnostic_data()

# Save datasets
save(endometrial_data, file = "data/endometrial_diagnostic_styles.rda")
save(breast_data, file = "data/breast_diagnostic_styles.rda")  
save(lymphoma_data, file = "data/lymphoma_diagnostic_styles.rda")

# Export CSV versions for jamovi
write.csv(endometrial_data$diagnosis_data, "data/endometrial_diagnostic_styles.csv", row.names = FALSE)
write.csv(breast_data$diagnosis_data, "data/breast_diagnostic_styles.csv", row.names = FALSE)
write.csv(lymphoma_data$diagnosis_data, "data/lymphoma_diagnostic_styles.csv", row.names = FALSE)

# Create pathologist information files
write.csv(endometrial_data$pathologist_info, "data/endometrial_pathologist_info.csv", row.names = FALSE)
write.csv(breast_data$pathologist_info, "data/breast_pathologist_info.csv", row.names = FALSE)
write.csv(lymphoma_data$pathologist_info, "data/lymphoma_pathologist_info.csv", row.names = FALSE)

cat("✓ Generated comprehensive diagnostic style test datasets\n")
cat("✓ Endometrial dataset: 80 cases, 15 pathologists, 3 diagnostic categories\n")
cat("✓ Breast dataset: 60 cases, 12 pathologists, 4 diagnostic categories\n") 
cat("✓ Lymphoma dataset: 45 cases, 10 pathologists, 5 diagnostic categories\n")
cat("✓ All datasets include pathologist characteristics and case features\n")
cat("✓ Designed to demonstrate all diagnostic style clustering features\n")