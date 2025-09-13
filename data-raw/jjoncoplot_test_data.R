# Comprehensive test data for jjoncoplot function
# Tests all functionality including co-occurrence, clinical variables, and different plot types

# Load required libraries
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
# TP53 is highly mutated (50%)
# KRAS has moderate mutation rate (30%)
# Some genes are rare (5-10%)
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
# Make them mutually exclusive
overlap <- intersect(brca1_mutated, brca2_mutated)
if (length(overlap) > 0) {
  # Randomly assign overlap to one gene
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
# Add PIK3CA mutations to 40% of TP53-mutated samples
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

# Add some edge cases for testing
# Duplicate sample IDs (should be handled)
test_data_with_duplicates <- test_data
test_data_with_duplicates[1, "SampleID"] <- test_data_with_duplicates[2, "SampleID"]

# Data with non-binary mutations (should be converted to 0/1)
test_data_multivalue <- test_data
test_data_multivalue$TP53[1:10] <- sample(c(0, 1, 2, 3), 10, replace = TRUE)

# Create small dataset for testing edge cases
small_test_data <- test_data[1:10, ]

# Create large dataset for performance testing
large_test_data <- do.call(rbind, replicate(10, test_data, simplify = FALSE))
large_test_data$SampleID <- paste0(large_test_data$SampleID, "_", rep(1:10, each = nrow(test_data)))

cat("Comprehensive jjoncoplot test data generated!\n")
cat("Main dataset:", nrow(test_data), "samples,", n_genes, "genes\n")
cat("Genes:", paste(gene_names, collapse = ", "), "\n")
cat("Clinical variables:", paste(names(clinical_data)[-1], collapse = ", "), "\n\n")

cat("Mutation frequencies:\n")
mutation_frequencies <- test_data %>% 
  select(all_of(gene_names)) %>% 
  summarise_all(mean, na.rm = TRUE) %>%
  t()
print(round(mutation_frequencies, 3))

cat("\nClinical variable summaries:\n")
print(summary(clinical_data[, -1]))

cat("\nTest datasets created:\n")
cat("- test_data: Standard test dataset (", nrow(test_data), " samples)\n")
cat("- small_test_data: Small dataset for quick testing (", nrow(small_test_data), " samples)\n") 
cat("- large_test_data: Large dataset for performance testing (", nrow(large_test_data), " samples)\n")
cat("- test_data_with_duplicates: Dataset with duplicate sample IDs\n")
cat("- test_data_multivalue: Dataset with non-binary mutation values\n\n")

# Test function calls for different scenarios
cat("Example function calls:\n\n")

cat("1. Basic oncoplot:\n")
cat("jjoncoplot(\n")
cat("  data = test_data,\n")
cat("  sampleVar = 'SampleID',\n")
cat("  geneVars = c('TP53', 'KRAS', 'PIK3CA', 'PTEN', 'EGFR'),\n")
cat("  plotType = 'oncoplot'\n")
cat(")\n\n")

cat("2. Gene frequency plot:\n")
cat("jjoncoplot(\n")
cat("  data = test_data,\n")
cat("  sampleVar = 'SampleID',\n")
cat("  geneVars = gene_names,\n")
cat("  plotType = 'frequency',\n")
cat("  maxGenes = 10\n")
cat(")\n\n")

cat("3. Co-occurrence analysis:\n")
cat("jjoncoplot(\n")
cat("  data = test_data,\n")
cat("  sampleVar = 'SampleID',\n")
cat("  geneVars = gene_names,\n")
cat("  plotType = 'cooccurrence',\n")
cat("  maxGenes = 8\n")
cat(")\n\n")

cat("4. ggoncoplot-style hierarchical sorting:\n")
cat("jjoncoplot(\n")
cat("  data = test_data,\n")
cat("  sampleVar = 'SampleID',\n")
cat("  geneVars = gene_names,\n")
cat("  mutationTypeVar = 'MutationType',\n")
cat("  sortBy = 'hierarchical',\n")
cat("  topn = 8,\n")
cat("  showGeneFreq = TRUE,\n")
cat("  showTMB = TRUE\n")
cat(")\n\n")

cat("5. Specific genes with exclusions:\n")
cat("jjoncoplot(\n")
cat("  data = test_data,\n")
cat("  sampleVar = 'SampleID',\n")
cat("  geneVars = gene_names,\n")
cat("  genesToInclude = 'TP53,KRAS,PIK3CA,BRCA1,BRCA2',\n")
cat("  genesToIgnore = 'RB1,MYC',\n")
cat("  clinicalVars = c('Age', 'Stage', 'Response'),\n")
cat("  showClinicalAnnotation = TRUE\n")
cat(")\n\n")

cat("6. Custom colors:\n")
cat("jjoncoplot(\n")
cat("  data = test_data,\n")
cat("  sampleVar = 'SampleID',\n")
cat("  geneVars = gene_names[1:6],\n")
cat("  colorScheme = 'custom',\n")
cat("  customColors = '#ffffff,#ff6b6b'\n")
cat(")\n\n")

cat("Data saved to environment. Use test_data for standard testing.\n")