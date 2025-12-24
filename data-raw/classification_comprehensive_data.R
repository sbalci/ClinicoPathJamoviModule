## Comprehensive Test Data for Clinical Classification Analysis
## This script generates multiple datasets testing all classification features

library(dplyr)
library(tibble)

set.seed(42)  # For reproducibility

# ==============================================================================
# Dataset 1: Binary Classification - Balanced (Primary Test Dataset)
# ==============================================================================
# Clinical scenario: Predicting post-operative complications
# Features: Mix of continuous biomarkers and categorical clinical factors
# n=150, balanced classes (75/75)

n1 <- 150
classification_binary_balanced <- tibble(
  PatientID = 1:n1,

  # Outcome variable (binary)
  Complication = factor(
    sample(c("No", "Yes"), n1, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("No", "Yes")
  ),

  # Continuous predictors
  Age = round(rnorm(n1, mean = 65, sd = 12)),
  BMI = round(rnorm(n1, mean = 27, sd = 4.5), 1),
  Biomarker = round(rnorm(n1, mean = 50, sd = 15), 1),
  BloodPressure = round(rnorm(n1, mean = 130, sd = 15)),

  # Categorical predictors
  TumorGrade = factor(
    sample(c("Grade1", "Grade2", "Grade3"), n1, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    levels = c("Grade1", "Grade2", "Grade3")
  ),
  LymphNodeStatus = factor(
    sample(c("Negative", "Positive"), n1, replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("Negative", "Positive")
  ),
  Gender = factor(
    sample(c("Male", "Female"), n1, replace = TRUE, prob = c(0.48, 0.52)),
    levels = c("Male", "Female")
  ),
  Smoking = factor(
    sample(c("Never", "Former", "Current"), n1, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    levels = c("Never", "Former", "Current")
  )
)

# Add some correlations to make predictions meaningful
classification_binary_balanced <- classification_binary_balanced %>%
  mutate(
    # Make Biomarker correlated with outcome
    Biomarker = if_else(Complication == "Yes",
                       Biomarker + rnorm(n1, 10, 5),
                       Biomarker),
    # Make Grade3 more likely with complications
    TumorGrade = if_else(Complication == "Yes" & runif(n1) > 0.7,
                         factor("Grade3", levels = levels(TumorGrade)),
                         TumorGrade)
  )


# ==============================================================================
# Dataset 2: Binary Classification - Imbalanced (Tests Balancing Methods)
# ==============================================================================
# Clinical scenario: Rare adverse events (10% prevalence)
# n=200, imbalanced (180 No / 20 Yes)

n2 <- 200
classification_binary_imbalanced <- tibble(
  PatientID = 1:n2,

  RareEvent = factor(
    sample(c("No", "Yes"), n2, replace = TRUE, prob = c(0.9, 0.1)),
    levels = c("No", "Yes")
  ),

  RiskScore = round(rnorm(n2, mean = 5, sd = 2), 1),
  Age = round(rnorm(n2, mean = 60, sd = 15)),
  ExposureLevel = round(rnorm(n2, mean = 100, sd = 25)),

  RiskCategory = factor(
    sample(c("Low", "Medium", "High"), n2, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    levels = c("Low", "Medium", "High")
  ),
  Comorbidity = factor(
    sample(c("None", "One", "Multiple"), n2, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    levels = c("None", "One", "Multiple")
  )
)

# Make rare events more associated with high risk
classification_binary_imbalanced <- classification_binary_imbalanced %>%
  mutate(
    RiskScore = if_else(RareEvent == "Yes",
                        RiskScore + rnorm(n2, 3, 1),
                        RiskScore),
    ExposureLevel = if_else(RareEvent == "Yes",
                            ExposureLevel + rnorm(n2, 30, 10),
                            ExposureLevel)
  )


# ==============================================================================
# Dataset 3: Multiclass Classification (3 classes)
# ==============================================================================
# Clinical scenario: Cancer staging (Stage I, II, III)
# n=180, relatively balanced (60/60/60)

n3 <- 180
classification_multiclass <- tibble(
  PatientID = 1:n3,

  CancerStage = factor(
    sample(c("StageI", "StageII", "StageIII"), n3, replace = TRUE, prob = c(0.33, 0.34, 0.33)),
    levels = c("StageI", "StageII", "StageIII")
  ),

  TumorSize = round(rnorm(n3, mean = 3.5, sd = 1.5), 1),
  NodeCount = round(abs(rnorm(n3, mean = 2, sd = 3))),
  KI67 = round(rnorm(n3, mean = 20, sd = 10), 1),

  Differentiation = factor(
    sample(c("Well", "Moderate", "Poor"), n3, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    levels = c("Well", "Moderate", "Poor")
  ),
  VascularInvasion = factor(
    sample(c("Absent", "Present"), n3, replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("Absent", "Present")
  ),
  Receptor = factor(
    sample(c("Positive", "Negative"), n3, replace = TRUE, prob = c(0.7, 0.3)),
    levels = c("Positive", "Negative")
  )
)

# Add correlations with stage
classification_multiclass <- classification_multiclass %>%
  mutate(
    TumorSize = case_when(
      CancerStage == "StageI" ~ TumorSize - rnorm(n3, 1, 0.3),
      CancerStage == "StageII" ~ TumorSize,
      CancerStage == "StageIII" ~ TumorSize + rnorm(n3, 1.5, 0.5)
    ),
    NodeCount = case_when(
      CancerStage == "StageI" ~ round(abs(rnorm(n3, 0, 1))),
      CancerStage == "StageII" ~ round(abs(rnorm(n3, 2, 2))),
      CancerStage == "StageIII" ~ round(abs(rnorm(n3, 5, 3)))
    ),
    KI67 = case_when(
      CancerStage == "StageI" ~ KI67 - rnorm(n3, 5, 2),
      CancerStage == "StageII" ~ KI67,
      CancerStage == "StageIII" ~ KI67 + rnorm(n3, 10, 3)
    )
  )


# ==============================================================================
# Dataset 4: Small Sample Size (Tests Warnings)
# ==============================================================================
# n=25 total (below recommended minimum of 30)

n4 <- 25
classification_small_sample <- tibble(
  PatientID = 1:n4,

  Outcome = factor(
    sample(c("Poor", "Good"), n4, replace = TRUE, prob = c(0.4, 0.6)),
    levels = c("Poor", "Good")
  ),

  Score1 = round(rnorm(n4, 50, 10), 1),
  Score2 = round(rnorm(n4, 25, 5), 1),

  Category = factor(
    sample(c("A", "B"), n4, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("A", "B")
  )
)


# ==============================================================================
# Dataset 5: Perfect Separation (Tests Edge Cases)
# ==============================================================================
# Biomarker perfectly predicts outcome (tests model warnings)

n5 <- 100
classification_perfect <- tibble(
  PatientID = 1:n5,

  Outcome = factor(
    sample(c("Negative", "Positive"), n5, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("Negative", "Positive")
  ),

  PerfectPredictor = 0,
  NoisyPredictor = round(rnorm(n5, 50, 10), 1),

  Group = factor(
    sample(c("Control", "Treatment"), n5, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("Control", "Treatment")
  )
)

# Create perfect separation
classification_perfect <- classification_perfect %>%
  mutate(
    PerfectPredictor = if_else(Outcome == "Positive", 100, 0)
  )


# ==============================================================================
# Dataset 6: High Dimensionality (Tests Overfitting Warnings)
# ==============================================================================
# Many features (15) with small sample (n=50) - ratio < 10

n6 <- 50
classification_high_dim <- tibble(
  PatientID = 1:n6,

  Outcome = factor(
    sample(c("No", "Yes"), n6, replace = TRUE, prob = c(0.6, 0.4)),
    levels = c("No", "Yes")
  ),

  # 15 predictors
  Var1 = rnorm(n6, 50, 10),
  Var2 = rnorm(n6, 30, 8),
  Var3 = rnorm(n6, 70, 12),
  Var4 = rnorm(n6, 40, 7),
  Var5 = rnorm(n6, 60, 15),
  Var6 = rnorm(n6, 25, 5),
  Var7 = rnorm(n6, 80, 20),
  Var8 = rnorm(n6, 35, 9),
  Var9 = rnorm(n6, 55, 11),
  Var10 = rnorm(n6, 45, 6),

  Cat1 = factor(sample(c("A", "B"), n6, replace = TRUE)),
  Cat2 = factor(sample(c("X", "Y", "Z"), n6, replace = TRUE)),
  Cat3 = factor(sample(c("Low", "High"), n6, replace = TRUE)),
  Cat4 = factor(sample(c("Type1", "Type2"), n6, replace = TRUE)),
  Cat5 = factor(sample(c("Group1", "Group2", "Group3"), n6, replace = TRUE))
)


# ==============================================================================
# Dataset 7: Missing Data Patterns (Tests Complete Case Handling)
# ==============================================================================

n7 <- 120
classification_with_missing <- tibble(
  PatientID = 1:n7,

  Outcome = factor(
    sample(c("Control", "Case"), n7, replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("Control", "Case")
  ),

  Predictor1 = rnorm(n7, 50, 10),
  Predictor2 = rnorm(n7, 75, 15),
  Predictor3 = rnorm(n7, 30, 8),

  Category1 = factor(sample(c("A", "B", "C"), n7, replace = TRUE)),
  Category2 = factor(sample(c("Low", "High"), n7, replace = TRUE))
)

# Introduce missing values (MCAR pattern)
missing_indices <- sample(1:n7, size = 20)
classification_with_missing$Predictor2[missing_indices[1:10]] <- NA
classification_with_missing$Category1[missing_indices[11:20]] <- NA


# ==============================================================================
# Save all datasets
# ==============================================================================

usethis::use_data(classification_binary_balanced, overwrite = TRUE)
usethis::use_data(classification_binary_imbalanced, overwrite = TRUE)
usethis::use_data(classification_multiclass, overwrite = TRUE)
usethis::use_data(classification_small_sample, overwrite = TRUE)
usethis::use_data(classification_perfect, overwrite = TRUE)
usethis::use_data(classification_high_dim, overwrite = TRUE)
usethis::use_data(classification_with_missing, overwrite = TRUE)


# Also save as CSV for jamovi testing
write.csv(classification_binary_balanced,
          "data/classification_binary_balanced.csv",
          row.names = FALSE)
write.csv(classification_binary_imbalanced,
          "data/classification_binary_imbalanced.csv",
          row.names = FALSE)
write.csv(classification_multiclass,
          "data/classification_multiclass.csv",
          row.names = FALSE)
write.csv(classification_small_sample,
          "data/classification_small_sample.csv",
          row.names = FALSE)
write.csv(classification_perfect,
          "data/classification_perfect.csv",
          row.names = FALSE)
write.csv(classification_high_dim,
          "data/classification_high_dim.csv",
          row.names = FALSE)
write.csv(classification_with_missing,
          "data/classification_with_missing.csv",
          row.names = FALSE)


# ==============================================================================
# Print summary statistics
# ==============================================================================

cat("\n=== DATASET SUMMARIES ===\n\n")

cat("1. Binary Balanced (n=150):\n")
cat("   Outcome distribution:", table(classification_binary_balanced$Complication), "\n")
cat("   Predictors: 4 continuous, 4 categorical\n\n")

cat("2. Binary Imbalanced (n=200):\n")
cat("   Outcome distribution:", table(classification_binary_imbalanced$RareEvent), "\n")
cat("   Imbalance ratio: 9:1 (tests balancing methods)\n\n")

cat("3. Multiclass (n=180):\n")
cat("   Outcome distribution:", table(classification_multiclass$CancerStage), "\n")
cat("   Classes: 3 (StageI, StageII, StageIII)\n\n")

cat("4. Small Sample (n=25):\n")
cat("   Outcome distribution:", table(classification_small_sample$Outcome), "\n")
cat("   Purpose: Tests small sample warnings\n\n")

cat("5. Perfect Separation (n=100):\n")
cat("   Outcome distribution:", table(classification_perfect$Outcome), "\n")
cat("   Purpose: Tests edge case with perfect predictor\n\n")

cat("6. High Dimensionality (n=50):\n")
cat("   Outcome distribution:", table(classification_high_dim$Outcome), "\n")
cat("   Features: 15, Sample-to-feature ratio: 3.3\n\n")

cat("7. With Missing Data (n=120):\n")
cat("   Outcome distribution:", table(classification_with_missing$Outcome), "\n")
cat("   Missing values: 20 observations (10 in Predictor2, 10 in Category1)\n\n")

cat("All datasets saved to data/ directory as both .rda and .csv\n")
