# Generate Realistic Haralick Texture Test Data for Digital Pathology
# This script creates comprehensive test datasets for the haralicktexture function

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)

set.seed(42)  # For reproducible results

# Function to generate correlated texture features based on real pathology patterns
generate_texture_features <- function(n_samples, base_entropy = 2.5, heterogeneity_level = "moderate") {
  
  # Adjust parameters based on heterogeneity level
  entropy_params <- switch(heterogeneity_level,
    "low" = list(mean = 1.8, sd = 0.3),
    "moderate" = list(mean = 2.5, sd = 0.6),
    "high" = list(mean = 3.2, sd = 0.8)
  )
  
  # Generate primary entropy values (most important clinical feature)
  entropy <- pmax(0, rnorm(n_samples, entropy_params$mean, entropy_params$sd))
  
  # Generate correlated features based on entropy (realistic GLCM relationships)
  contrast <- pmax(0, entropy * 2.3 + rnorm(n_samples, 0, 0.5))
  correlation <- pmax(-1, pmin(1, 0.6 - entropy * 0.15 + rnorm(n_samples, 0, 0.1)))
  energy <- pmax(0, pmin(1, 0.8 - entropy * 0.2 + rnorm(n_samples, 0, 0.05)))
  homogeneity <- pmax(0, pmin(1, 0.9 - entropy * 0.18 + rnorm(n_samples, 0, 0.04)))
  variance <- pmax(0, entropy * 1.5 + contrast * 0.3 + rnorm(n_samples, 0, 0.4))
  dissimilarity <- pmax(0, entropy * 0.8 + contrast * 0.4 + rnorm(n_samples, 0, 0.2))
  asm <- energy^2  # Angular Second Moment
  idm <- homogeneity  # Inverse Difference Moment (alternative name)
  
  # Additional texture features for comprehensive testing
  sum_average <- entropy * 1.2 + rnorm(n_samples, 0, 0.3)
  sum_entropy <- entropy * 1.1 + rnorm(n_samples, 0, 0.2)
  difference_variance <- variance * 0.7 + rnorm(n_samples, 0, 0.3)
  difference_entropy <- entropy * 0.9 + rnorm(n_samples, 0, 0.2)
  
  return(data.frame(
    entropy = entropy,
    contrast = contrast,
    correlation = correlation,
    energy = energy,
    homogeneity = homogeneity,
    variance = variance,
    dissimilarity = dissimilarity,
    asm = asm,
    idm = idm,
    sum_average = sum_average,
    sum_entropy = sum_entropy,
    difference_variance = difference_variance,
    difference_entropy = difference_entropy
  ))
}

# Generate spatial coordinates with realistic patterns
generate_spatial_coords <- function(n_samples, pattern = "random") {
  switch(pattern,
    "random" = data.frame(
      x_coord = runif(n_samples, 0, 1000),
      y_coord = runif(n_samples, 0, 1000)
    ),
    "clustered" = {
      # Create 3-4 clusters
      n_clusters <- sample(3:4, 1)
      cluster_centers_x <- runif(n_clusters, 100, 900)
      cluster_centers_y <- runif(n_clusters, 100, 900)
      
      cluster_assignment <- sample(1:n_clusters, n_samples, replace = TRUE)
      
      x_coords <- numeric(n_samples)
      y_coords <- numeric(n_samples)
      
      for(i in 1:n_samples) {
        cluster <- cluster_assignment[i]
        x_coords[i] <- rnorm(1, cluster_centers_x[cluster], 50)
        y_coords[i] <- rnorm(1, cluster_centers_y[cluster], 50)
      }
      
      data.frame(x_coord = x_coords, y_coord = y_coords)
    },
    "gradient" = data.frame(
      x_coord = runif(n_samples, 0, 1000),
      y_coord = runif(n_samples, 0, 1000)
    )
  )
}

# Create comprehensive test datasets

# Dataset 1: Ki67 Proliferation Study (High Heterogeneity)
message("Generating Ki67 proliferation dataset...")
ki67_data <- generate_texture_features(85, heterogeneity_level = "high") %>%
  bind_cols(generate_spatial_coords(85, "clustered")) %>%
  mutate(
    case_id = paste0("KI67_", sprintf("%03d", 1:n())),
    tumor_grade = sample(c("Grade I", "Grade II", "Grade III"), n(), replace = TRUE, prob = c(0.2, 0.5, 0.3)),
    ki67_percentage = pmax(0, pmin(100, entropy * 8 + rnorm(n(), 0, 5))),
    survival_months = pmax(6, 60 - entropy * 8 + rnorm(n(), 0, 10)),
    recurrence = ifelse(entropy > 3.0, 
                       sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.7, 0.3)),
                       sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.3, 0.7))),
    tissue_type = "Breast Carcinoma",
    staining_method = "Ki67 IHC"
  ) %>%
  select(case_id, everything())

# Dataset 2: HER2 Expression Analysis (Moderate Heterogeneity)
message("Generating HER2 expression dataset...")
her2_data <- generate_texture_features(62, heterogeneity_level = "moderate") %>%
  bind_cols(generate_spatial_coords(62, "random")) %>%
  mutate(
    case_id = paste0("HER2_", sprintf("%03d", 1:n())),
    her2_score = case_when(
      entropy < 2.0 ~ sample(c("0", "1+"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      entropy < 2.8 ~ sample(c("1+", "2+"), n(), replace = TRUE, prob = c(0.4, 0.6)),
      TRUE ~ sample(c("2+", "3+"), n(), replace = TRUE, prob = c(0.3, 0.7))
    ),
    treatment_response = ifelse(her2_score %in% c("2+", "3+"),
                               sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                                     n(), replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
                               sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                                     n(), replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))),
    tissue_type = "Breast Carcinoma",
    staining_method = "HER2 IHC/FISH"
  ) %>%
  select(case_id, everything())

# Dataset 3: PD-L1 Immunotherapy Study (Variable Heterogeneity)
message("Generating PD-L1 expression dataset...")
pdl1_data <- generate_texture_features(48, heterogeneity_level = "moderate") %>%
  bind_cols(generate_spatial_coords(48, "gradient")) %>%
  mutate(
    case_id = paste0("PDL1_", sprintf("%03d", 1:n())),
    pdl1_tps = pmax(0, pmin(100, entropy * 12 + rnorm(n(), 0, 8))),
    pdl1_cps = pmax(0, pmin(100, entropy * 15 + rnorm(n(), 0, 10))),
    tumor_stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n(), replace = TRUE),
    immunotherapy_response = case_when(
      pdl1_tps >= 50 ~ sample(c("Response", "No Response"), n(), replace = TRUE, prob = c(0.6, 0.4)),
      pdl1_tps >= 1 ~ sample(c("Response", "No Response"), n(), replace = TRUE, prob = c(0.4, 0.6)),
      TRUE ~ sample(c("Response", "No Response"), n(), replace = TRUE, prob = c(0.2, 0.8))
    ),
    tissue_type = "Lung Adenocarcinoma",
    staining_method = "PD-L1 IHC (22C3)"
  ) %>%
  select(case_id, everything())

# Dataset 4: CD8+ T-cell Infiltration (Low-Moderate Heterogeneity)
message("Generating CD8+ T-cell dataset...")
cd8_data <- generate_texture_features(38, heterogeneity_level = "low") %>%
  bind_cols(generate_spatial_coords(38, "clustered")) %>%
  mutate(
    case_id = paste0("CD8_", sprintf("%03d", 1:n())),
    cd8_density = pmax(0, entropy * 50 + rnorm(n(), 0, 15)),
    immune_score = case_when(
      cd8_density > 100 ~ "Hot",
      cd8_density > 50 ~ "Warm", 
      TRUE ~ "Cold"
    ),
    tumor_mutation_burden = pmax(0, entropy * 3 + rnorm(n(), 0, 1)),
    tissue_type = "Colorectal Carcinoma",
    staining_method = "CD8 IHC"
  ) %>%
  select(case_id, everything())

# Dataset 5: Small Sample Size (for testing warnings)
message("Generating small sample dataset...")
small_data <- generate_texture_features(4, heterogeneity_level = "high") %>%
  mutate(
    case_id = paste0("SMALL_", sprintf("%03d", 1:n())),
    diagnosis = sample(c("Malignant", "Benign"), n(), replace = TRUE),
    tissue_type = "Mixed",
    staining_method = "H&E"
  ) %>%
  select(case_id, everything())

# Dataset 6: Missing Data Patterns (for testing robustness)
message("Generating dataset with missing values...")
missing_data <- generate_texture_features(75, heterogeneity_level = "moderate") %>%
  bind_cols(generate_spatial_coords(75, "random")) %>%
  mutate(
    case_id = paste0("MISS_", sprintf("%03d", 1:n())),
    tissue_type = "Various",
    staining_method = "Multiple"
  ) %>%
  # Introduce realistic missing patterns
  mutate(
    # MCAR: 5% random missingness
    entropy = ifelse(runif(n()) < 0.05, NA, entropy),
    contrast = ifelse(runif(n()) < 0.05, NA, contrast),
    # MAR: Missing correlates with other variables
    correlation = ifelse(entropy > 3.5 & runif(n()) < 0.15, NA, correlation),
    energy = ifelse(contrast > 5 & runif(n()) < 0.12, NA, energy),
    # MNAR: Missing related to the value itself
    variance = ifelse(variance < 0.5 & runif(n()) < 0.20, NA, variance),
    # Complete case loss
    homogeneity = ifelse(case_id %in% sample(case_id, 8), NA, homogeneity),
    dissimilarity = ifelse(case_id %in% sample(case_id, 8), NA, dissimilarity)
  ) %>%
  select(case_id, everything())

# Dataset 7: High-Dimensional (more features than samples)
message("Generating high-dimensional dataset...")
high_dim_data <- generate_texture_features(15, heterogeneity_level = "moderate") %>%
  mutate(
    case_id = paste0("HDIM_", sprintf("%03d", 1:n())),
    # Add many additional texture features
    glcm_mean = entropy + rnorm(n(), 0, 0.1),
    glcm_std = variance^0.5,
    cluster_shade = entropy * contrast + rnorm(n(), 0, 0.3),
    cluster_prominence = entropy^2 + rnorm(n(), 0, 0.2),
    max_correlation = correlation + 0.1,
    sum_variance = variance * 1.1,
    difference_mean = dissimilarity,
    information_correlation_1 = entropy * 0.8,
    information_correlation_2 = entropy * 0.9,
    inverse_difference = homogeneity,
    normalized_inverse_difference = homogeneity * 0.9,
    inverse_difference_moment = idm,
    tissue_type = "Test",
    staining_method = "Multiple"
  ) %>%
  select(case_id, everything())

# Dataset 8: Comprehensive Multi-Biomarker Study
message("Generating comprehensive multi-biomarker dataset...")
comprehensive_data <- generate_texture_features(120, heterogeneity_level = "moderate") %>%
  bind_cols(generate_spatial_coords(120, "clustered")) %>%
  mutate(
    case_id = paste0("COMP_", sprintf("%03d", 1:n())),
    biomarker_type = sample(c("Ki67", "HER2", "PD-L1", "CD8", "p53", "EGFR"), n(), replace = TRUE),
    tumor_type = sample(c("Breast", "Lung", "Colorectal", "Prostate", "Ovarian"), n(), replace = TRUE),
    tumor_grade = sample(c("Grade I", "Grade II", "Grade III"), n(), replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    tumor_stage = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n(), replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    age = round(rnorm(n(), 65, 12)),
    gender = sample(c("Male", "Female"), n(), replace = TRUE),
    survival_months = pmax(1, 48 - entropy * 6 + rnorm(n(), 0, 8)),
    vital_status = ifelse(survival_months < 24 & entropy > 2.8,
                         sample(c("Dead", "Alive"), n(), replace = TRUE, prob = c(0.6, 0.4)),
                         sample(c("Dead", "Alive"), n(), replace = TRUE, prob = c(0.3, 0.7))),
    treatment_response = sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                               n(), replace = TRUE),
    tissue_type = "Mixed Cohort",
    staining_method = "Multiplex IHC"
  ) %>%
  select(case_id, everything())

# Save all datasets
datasets <- list(
  ki67_proliferation = ki67_data,
  her2_expression = her2_data,
  pdl1_immunotherapy = pdl1_data,
  cd8_infiltration = cd8_data,
  small_sample = small_data,
  missing_data = missing_data,
  high_dimensional = high_dim_data,
  comprehensive_study = comprehensive_data
)

# Create individual CSV files
message("Saving datasets to CSV files...")
for(name in names(datasets)) {
  write.csv(datasets[[name]], 
           file = paste0("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/haralick_", name, ".csv"), 
           row.names = FALSE)
}

# Create combined RData file
save(datasets, file = "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/haralick_test_datasets.RData")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(datasets, "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/haralick_test_datasets.RData")
  message("✓ Created haralick_test_datasets.RData")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(datasets, "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/haralick_test_datasets.RData")
  message("✓ Created haralick_test_datasets.RData")
}

# Generate summary report
message("Dataset Summary:")
for(name in names(datasets)) {
  data <- datasets[[name]]
  message(sprintf("  %s: %d cases, %d texture features", 
                 name, 
                 nrow(data),
                 sum(grepl("entropy|contrast|correlation|energy|homogeneity|variance|dissimilarity|asm|idm", names(data)))))
}

message("Test datasets created successfully!")
message("Files saved to: /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/")
