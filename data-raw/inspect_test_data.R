# Quick Data Inspection Script for Haralick Texture Test Datasets
# Run this script to explore the generated test data

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(ggplot2)
library(corrplot)

# Load all datasets
datasets <- list()
dataset_files <- list.files(path = "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data", 
                           pattern = "haralick_.*\\.csv$", 
                           full.names = TRUE)

for(file in dataset_files) {
  name <- gsub(".*haralick_(.*)\\.csv", "\\1", basename(file))
  datasets[[name]] <- read.csv(file, stringsAsFactors = FALSE)
  cat(sprintf("Loaded %s: %d cases, %d variables\n", name, nrow(datasets[[name]]), ncol(datasets[[name]])))
}

# Function to identify texture features in a dataset
get_texture_features <- function(data) {
  texture_names <- c("entropy", "contrast", "correlation", "energy", "homogeneity", 
                    "variance", "dissimilarity", "asm", "idm", "sum_average", 
                    "sum_entropy", "difference_variance", "difference_entropy")
  intersect(names(data), texture_names)
}

# Function to summarize texture features
summarize_textures <- function(data, dataset_name) {
  texture_features <- get_texture_features(data)
  
  cat(sprintf("\n=== %s ===\n", toupper(dataset_name)))
  cat(sprintf("Cases: %d\n", nrow(data)))
  cat(sprintf("Texture features: %s\n", paste(texture_features, collapse = ", ")))
  
  if(length(texture_features) > 0) {
    texture_data <- data[, texture_features, drop = FALSE]
    
    # Calculate heterogeneity metrics
    entropy_mean <- if("entropy" %in% texture_features) mean(data$entropy, na.rm = TRUE) else NA
    entropy_cv <- if("entropy" %in% texture_features) sd(data$entropy, na.rm = TRUE) / mean(data$entropy, na.rm = TRUE) else NA
    
    cat(sprintf("Average entropy: %.3f (CV: %.3f)\n", entropy_mean, entropy_cv))
    cat(sprintf("Heterogeneity level: %s\n", 
               if(is.na(entropy_cv)) "Unknown" else
               if(entropy_cv > 0.5) "High" else if(entropy_cv > 0.25) "Moderate" else "Low"))
    
    # Missing data summary
    missing_rates <- colSums(is.na(texture_data)) / nrow(texture_data) * 100
    max_missing <- max(missing_rates)
    cat(sprintf("Max missing data: %.1f%%\n", max_missing))
    
    # Check for spatial coordinates
    has_spatial <- all(c("x_coord", "y_coord") %in% names(data))
    cat(sprintf("Spatial coordinates: %s\n", if(has_spatial) "Available" else "Not available"))
    
    # Clinical variables
    clinical_vars <- setdiff(names(data), c(texture_features, "case_id", "x_coord", "y_coord"))
    if(length(clinical_vars) > 0) {
      cat(sprintf("Clinical variables: %s\n", paste(head(clinical_vars, 5), collapse = ", ")))
    }
  }
}

# Inspect each dataset
for(name in names(datasets)) {
  summarize_textures(datasets[[name]], name)
}

# Create visualization for entropy distributions across datasets
cat("\n=== Creating entropy distribution comparison ===\n")

entropy_data <- data.frame()
for(name in names(datasets)) {
  if("entropy" %in% names(datasets[[name]])) {
    temp_data <- data.frame(
      dataset = name,
      entropy = datasets[[name]]$entropy
    )
    entropy_data <- rbind(entropy_data, temp_data)
  }
}

if(nrow(entropy_data) > 0) {
  p1 <- ggplot(entropy_data, aes(x = dataset, y = entropy, fill = dataset)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Entropy Distribution Across Test Datasets",
         x = "Dataset", y = "Entropy Value") +
    guides(fill = FALSE)
  
  ggsave("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/entropy_distributions.png", 
         plot = p1, width = 10, height = 6)
  cat("Saved entropy distribution plot\n")
}

# Create correlation matrix for comprehensive dataset
if("comprehensive_study" %in% names(datasets)) {
  cat("\n=== Creating correlation matrix for comprehensive dataset ===\n")
  comprehensive_data <- datasets[["comprehensive_study"]]
  texture_features <- get_texture_features(comprehensive_data)
  
  if(length(texture_features) > 5) {
    cor_matrix <- cor(comprehensive_data[, texture_features], use = "complete.obs")
    
    png("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/test_data/texture_correlations.png", 
        width = 800, height = 800)
    corrplot(cor_matrix, method = "color", type = "upper", 
             title = "Texture Feature Correlations - Comprehensive Dataset",
             mar = c(0,0,2,0))
    dev.off()
    cat("Saved correlation matrix plot\n")
  }
}

# Test specific scenarios
cat("\n=== Testing Scenario Recommendations ===\n")

test_scenarios <- list(
  "Ki67 Analysis" = list(
    dataset = "ki67_proliferation",
    features = c("entropy", "contrast", "correlation"),
    biomarker = "Ki67",
    grouping = "tumor_grade",
    outcome = "survival_months"
  ),
  "HER2 Assessment" = list(
    dataset = "her2_expression", 
    features = c("entropy", "contrast", "energy", "homogeneity"),
    biomarker = "HER2",
    grouping = "her2_score",
    outcome = "treatment_response"
  ),
  "Spatial Analysis" = list(
    dataset = "cd8_infiltration",
    features = c("entropy", "variance", "dissimilarity"),
    biomarker = "CD8+",
    spatial = TRUE,
    grouping = "immune_score"
  ),
  "Warning Testing" = list(
    dataset = "small_sample",
    features = c("entropy", "contrast", "correlation", "energy"),
    expect_warnings = TRUE
  )
)

for(scenario_name in names(test_scenarios)) {
  scenario <- test_scenarios[[scenario_name]]
  dataset_name <- scenario$dataset
  
  if(dataset_name %in% names(datasets)) {
    data <- datasets[[dataset_name]]
    available_features <- intersect(scenario$features, names(data))
    
    cat(sprintf("\n%s:\n", scenario_name))
    cat(sprintf("  Dataset: %s (%d cases)\n", dataset_name, nrow(data)))
    cat(sprintf("  Recommended features: %s\n", paste(available_features, collapse = ", ")))
    
    if(!is.null(scenario$grouping) && scenario$grouping %in% names(data)) {
      groups <- unique(data[[scenario$grouping]])
      cat(sprintf("  Grouping by: %s (%s)\n", scenario$grouping, paste(groups, collapse = ", ")))
    }
    
    if(!is.null(scenario$outcome) && scenario$outcome %in% names(data)) {
      cat(sprintf("  Outcome variable: %s\n", scenario$outcome))
    }
    
    if(!is.null(scenario$spatial) && scenario$spatial && all(c("x_coord", "y_coord") %in% names(data))) {
      cat("  Spatial coordinates: Available for spatial analysis\n")
    }
    
    if(!is.null(scenario$expect_warnings) && scenario$expect_warnings) {
      cat("  Expected: Sample size warnings\n")
    }
  }
}

cat("\n=== Data Inspection Complete ===\n")
cat("Files created:\n")
cat("- entropy_distributions.png\n") 
cat("- texture_correlations.png\n")
cat("\nReady for haralicktexture testing!\n")
