# Generate CSV files for jjoncoplot testing
# This script creates comprehensive test data and saves it to the data folder

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Generate realistic cancer genomics test data
n_samples <- 100
n_genes <- 15

# Sample IDs
sample_ids <- paste0("TCGA-", sprintf("%02d", 1:n_samples))

# Realistic cancer gene names
gene_names <- c("TP53", "KRAS", "PIK3CA", "PTEN", "EGFR", "BRAF", "APC", 
                "BRCA1", "BRCA2", "ATM", "CDH1", "CTNNB1", "IDH1", "MYC", "RB1")

# Generate mutation data with realistic patterns
mutation_rates <- c(0.5, 0.3, 0.25, 0.2, 0.18, 0.15, 0.12, 0.1, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02)

# Create mutation matrix with some co-occurrence patterns
mutation_data <- data.frame(SampleID = sample_ids)

# Add mutations with realistic patterns
for (i in 1:n_genes) {
  gene <- gene_names[i]
  prob <- mutation_rates[i]
  mutation_data[[gene]] <- rbinom(n_samples, 1, prob)
}

# Add mutation type information for testing
mutation_types <- c("SNV", "CNV", "Fusion", "Indel")
mutation_data$MutationType <- sample(mutation_types, n_samples, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1))

# Create co-occurrence between BRCA1 and BRCA2 (mutually exclusive)
brca1_mutated <- which(mutation_data$BRCA1 == 1)
brca2_mutated <- which(mutation_data$BRCA2 == 1)
overlap <- intersect(brca1_mutated, brca2_mutated)
if (length(overlap) > 0) {
  for (idx in overlap) {
    if (runif(1) > 0.5) {
      mutation_data$BRCA1[idx] <- 0
    } else {
      mutation_data$BRCA2[idx] <- 0
    }
  }
}

# Create co-occurrence between TP53 and PIK3CA (often co-occur)
tp53_mutated <- which(mutation_data$TP53 == 1)
additional_pik3ca <- sample(tp53_mutated, size = round(0.4 * length(tp53_mutated)))
mutation_data$PIK3CA[additional_pik3ca] <- 1

# Add clinical variables
clinical_data <- data.frame(
  SampleID = sample_ids,
  
  # Age (realistic distribution)
  Age = round(rnorm(n_samples, mean = 62, sd = 12)),
  
  # Gender
  Gender = sample(c("Male", "Female"), n_samples, replace = TRUE, prob = c(0.4, 0.6)),
  
  # Cancer Stage
  Stage = sample(c("I", "II", "III", "IV"), n_samples, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  
  # Treatment Response
  Response = sample(c("Complete", "Partial", "Stable", "Progressive"), n_samples, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
  
  # Tumor Grade
  Grade = sample(c("Low", "Intermediate", "High"), n_samples, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  
  # Microsatellite Instability Status
  MSI_Status = sample(c("MSI-H", "MSI-L", "MSS"), n_samples, replace = TRUE, prob = c(0.15, 0.15, 0.7)),
  
  # ECOG Performance Status
  ECOG_PS = sample(c("0", "1", "2", "3"), n_samples, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05)),
  
  # Overall Survival (months) - some missing values
  OS_months = ifelse(runif(n_samples) > 0.1, 
                    round(rexp(n_samples, rate = 1/24)), 
                    NA),
  
  # Progression Free Survival (months)
  PFS_months = ifelse(runif(n_samples) > 0.05, 
                     round(rexp(n_samples, rate = 1/12)), 
                     NA)
)

# Merge mutation and clinical data
test_data <- merge(mutation_data, clinical_data, by = "SampleID")

# Create some missing values in mutations (realistic)
missing_indices <- sample(1:nrow(test_data), size = round(0.02 * nrow(test_data) * n_genes))
gene_cols <- gene_names
for (idx in missing_indices) {
  gene_col <- sample(gene_cols, 1)
  row_idx <- ((idx - 1) %% nrow(test_data)) + 1
  test_data[row_idx, gene_col] <- NA
}

# Create smaller test datasets
small_test_data <- test_data[1:10, ]

# Create edge case datasets
test_data_with_duplicates <- test_data
test_data_with_duplicates[1, "SampleID"] <- test_data_with_duplicates[2, "SampleID"]

test_data_multivalue <- test_data
test_data_multivalue$TP53[1:10] <- sample(c(0, 1, 2, 3), 10, replace = TRUE)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save datasets to CSV files
write.csv(test_data, "data/jjoncoplot_test_data.csv", row.names = FALSE)
write.csv(small_test_data, "data/jjoncoplot_small_test_data.csv", row.names = FALSE)
write.csv(test_data_with_duplicates, "data/jjoncoplot_test_data_duplicates.csv", row.names = FALSE)
write.csv(test_data_multivalue, "data/jjoncoplot_test_data_multivalue.csv", row.names = FALSE)

# Generate summary information
cat("✅ CSV files generated successfully!\n\n")

cat("📁 Files created in data/ folder:\n")
cat("  • jjoncoplot_test_data.csv (", nrow(test_data), " samples, ", ncol(test_data), " variables)\n")
cat("  • jjoncoplot_small_test_data.csv (", nrow(small_test_data), " samples)\n")
cat("  • jjoncoplot_test_data_duplicates.csv (contains duplicate sample IDs)\n")
cat("  • jjoncoplot_test_data_multivalue.csv (contains non-binary mutation values)\n\n")

cat("🧬 Gene Variables (", length(gene_names), " genes):\n")
cat("  ", paste(gene_names, collapse = ", "), "\n\n")

cat("🏥 Clinical Variables:\n")
clinical_vars <- setdiff(names(clinical_data), "SampleID")
cat("  ", paste(clinical_vars, collapse = ", "), "\n\n")

cat("📊 Mutation Type Variable:\n")
cat("  MutationType: ", paste(unique(test_data$MutationType), collapse = ", "), "\n\n")

cat("🎯 Usage in jamovi:\n")
cat("1. Load jjoncoplot_test_data.csv\n")
cat("2. Set SampleID as Sample ID Variable\n")
cat("3. Set gene names as Gene Variables\n") 
cat("4. Set MutationType as Mutation Type Variable (optional)\n")
cat("5. Set clinical variables as Clinical Variables (optional)\n")
cat("6. Choose sorting method (Hierarchical recommended)\n\n")

# Print mutation frequencies for reference
cat("📈 Mutation Frequencies:\n")
mutation_freq <- test_data %>% 
  select(all_of(gene_names)) %>% 
  summarise_all(function(x) mean(x, na.rm = TRUE)) %>%
  t() %>%
  round(3)

print(mutation_freq)

cat("\n✨ Ready for jjoncoplot testing!\n")
