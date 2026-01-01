# Create comprehensive test dataset for chisqposttest function
# This script generates a dataset with known associations and various configurations

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(42)  # For reproducibility

# Sample size
n <- 300

# Create dataset with different categorical variables
chisqposttest_test_data <- data.frame(
  
  # Patient ID
  PatientID = 1:n,
  
  # 2x2 table variables (binary)
  # Strong association between Treatment and Response
  Treatment = factor(sample(c("Standard", "Experimental"), n, replace = TRUE, prob = c(0.4, 0.6)),
                    levels = c("Standard", "Experimental")),
  
  # Response variable associated with Treatment
  Response = factor(rep(NA, n), levels = c("No Response", "Response")),
  
  # Sex variable (balanced)
  Sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.5, 0.5)),
               levels = c("Male", "Female")),
  
  # 3x3 table variables
  # Tumor Grade (3 levels)
  TumorGrade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE, 
                            prob = c(0.3, 0.4, 0.3)),
                     levels = c("Grade 1", "Grade 2", "Grade 3")),
  
  # Tumor Stage (3 levels) - associated with Grade
  TumorStage = factor(rep(NA, n), levels = c("Stage I", "Stage II", "Stage III")),
  
  # 4x2 table variables  
  # Institution (4 levels)
  Institution = factor(sample(c("Hospital A", "Hospital B", "Hospital C", "Hospital D"), 
                             n, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15)),
                      levels = c("Hospital A", "Hospital B", "Hospital C", "Hospital D")),
  
  # Quality Score (2 levels) - associated with Institution
  QualityScore = factor(rep(NA, n), levels = c("High", "Low")),
  
  # Variables with no association (null cases)
  RandomVar1 = factor(sample(c("Group A", "Group B", "Group C"), n, replace = TRUE),
                     levels = c("Group A", "Group B", "Group C")),
  
  RandomVar2 = factor(sample(c("Type X", "Type Y"), n, replace = TRUE),
                     levels = c("Type X", "Type Y")),
  
  # Variables with edge cases
  # Rare category variable (unbalanced)
  RareCategory = factor(sample(c("Common", "Uncommon", "Rare"), n, replace = TRUE, 
                              prob = c(0.7, 0.25, 0.05)),
                       levels = c("Common", "Uncommon", "Rare")),
  
  # Binary variable associated with RareCategory
  BinaryOutcome = factor(rep(NA, n), levels = c("Negative", "Positive")),
  
  # Age groups for analysis
  AgeGroup = factor(sample(c("Young", "Middle", "Elderly"), n, replace = TRUE,
                          prob = c(0.25, 0.5, 0.25)),
                   levels = c("Young", "Middle", "Elderly")),
  
  # Biomarker status
  BiomarkerStatus = factor(rep(NA, n), levels = c("Negative", "Positive"))
)

# Create known associations

# 1. Treatment -> Response association (strong)
treatment_effect <- ifelse(chisqposttest_test_data$Treatment == "Experimental", 0.7, 0.3)
chisqposttest_test_data$Response <- factor(
  ifelse(runif(n) < treatment_effect, "Response", "No Response"),
  levels = c("No Response", "Response")
)

# 2. TumorGrade -> TumorStage association (moderate)
grade_to_stage <- function(grade) {
  if (grade == "Grade 1") {
    sample(c("Stage I", "Stage II", "Stage III"), 1, prob = c(0.6, 0.3, 0.1))
  } else if (grade == "Grade 2") {
    sample(c("Stage I", "Stage II", "Stage III"), 1, prob = c(0.2, 0.6, 0.2))
  } else {  # Grade 3
    sample(c("Stage I", "Stage II", "Stage III"), 1, prob = c(0.1, 0.3, 0.6))
  }
}

for (i in 1:n) {
  chisqposttest_test_data$TumorStage[i] <- grade_to_stage(chisqposttest_test_data$TumorGrade[i])
}
chisqposttest_test_data$TumorStage <- factor(chisqposttest_test_data$TumorStage,
                                           levels = c("Stage I", "Stage II", "Stage III"))

# 3. Institution -> QualityScore association (weak but detectable)
institution_to_quality <- function(institution) {
  probs <- switch(as.character(institution),
                  "Hospital A" = c(0.8, 0.2),   # High quality
                  "Hospital B" = c(0.6, 0.4),   # Good quality
                  "Hospital C" = c(0.5, 0.5),   # Average quality
                  "Hospital D" = c(0.3, 0.7))   # Lower quality
  sample(c("High", "Low"), 1, prob = probs)
}

for (i in 1:n) {
  chisqposttest_test_data$QualityScore[i] <- institution_to_quality(chisqposttest_test_data$Institution[i])
}
chisqposttest_test_data$QualityScore <- factor(chisqposttest_test_data$QualityScore,
                                             levels = c("High", "Low"))

# 4. RareCategory -> BinaryOutcome association (with rare events)
rare_to_outcome <- function(category) {
  probs <- switch(as.character(category),
                  "Common" = c(0.8, 0.2),
                  "Uncommon" = c(0.6, 0.4),
                  "Rare" = c(0.2, 0.8))  # Rare category has high positive rate
  sample(c("Negative", "Positive"), 1, prob = probs)
}

for (i in 1:n) {
  chisqposttest_test_data$BinaryOutcome[i] <- rare_to_outcome(chisqposttest_test_data$RareCategory[i])
}
chisqposttest_test_data$BinaryOutcome <- factor(chisqposttest_test_data$BinaryOutcome,
                                              levels = c("Negative", "Positive"))

# 5. AgeGroup -> BiomarkerStatus association (moderate)
age_to_biomarker <- function(age) {
  probs <- switch(as.character(age),
                  "Young" = c(0.7, 0.3),
                  "Middle" = c(0.5, 0.5),
                  "Elderly" = c(0.3, 0.7))
  sample(c("Negative", "Positive"), 1, prob = probs)
}

for (i in 1:n) {
  chisqposttest_test_data$BiomarkerStatus[i] <- age_to_biomarker(chisqposttest_test_data$AgeGroup[i])
}
chisqposttest_test_data$BiomarkerStatus <- factor(chisqposttest_test_data$BiomarkerStatus,
                                                levels = c("Negative", "Positive"))

# Add some missing data for testing exclusion options
missing_indices <- sample(1:n, floor(n * 0.05))  # 5% missing data
chisqposttest_test_data$Treatment[missing_indices[1:5]] <- NA
chisqposttest_test_data$Sex[missing_indices[6:10]] <- NA
chisqposttest_test_data$TumorGrade[missing_indices[11:15]] <- NA

# Add metadata attributes
attr(chisqposttest_test_data, "description") <- "Test dataset for chisqposttest function with known associations"
attr(chisqposttest_test_data, "associations") <- list(
  strong = c("Treatment -> Response"),
  moderate = c("TumorGrade -> TumorStage", "AgeGroup -> BiomarkerStatus"),
  weak = c("Institution -> QualityScore", "RareCategory -> BinaryOutcome"),
  none = c("RandomVar1 ⊥ RandomVar2")
)
attr(chisqposttest_test_data, "created") <- Sys.Date()
attr(chisqposttest_test_data, "sample_size") <- n

# Display summary
cat("Created chisqposttest_test_data with:\n")
cat("- Sample size:", n, "\n")
cat("- Variables:", ncol(chisqposttest_test_data), "\n") 
cat("- Missing data: ~5% in selected variables\n")
cat("- Known associations: Strong, moderate, weak, and null\n")

# Display crosstab examples
cat("\nExample associations:\n")
cat("1. Treatment x Response (Strong association):\n")
print(table(chisqposttest_test_data$Treatment, chisqposttest_test_data$Response, useNA = "no"))

cat("\n2. TumorGrade x TumorStage (Moderate association):\n")
print(table(chisqposttest_test_data$TumorGrade, chisqposttest_test_data$TumorStage, useNA = "no"))

cat("\n3. RandomVar1 x RandomVar2 (No association):\n")
print(table(chisqposttest_test_data$RandomVar1, chisqposttest_test_data$RandomVar2, useNA = "no"))

# Save the dataset
chisqposttest_test_data
