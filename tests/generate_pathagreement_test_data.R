# Generate Pathagreement Test Data
# This script creates comprehensive test datasets for the pathagreement function
# Use this to regenerate test data or create new test scenarios

library(dplyr)

# Set seed for reproducibility
set.seed(2025)

cat("\n📊 PATHOLOGY INTERRATER RELIABILITY TEST DATA GENERATION\n")
cat(strrep("=", 70), "\n\n")

# =============================================================================
# Helper Functions
# =============================================================================

#' Generate categorical ratings with controlled agreement
#' @param n_cases Number of cases
#' @param n_raters Number of raters
#' @param categories Vector of category names
#' @param agreement_level Target agreement level (0-1)
#' @param reference_index Index for reference rater (default=1)
#' @return Matrix of ratings (cases x raters)
generate_ratings <- function(n_cases,
                            n_raters,
                            categories = c("Benign", "Borderline", "Malignant"),
                            agreement_level = 0.7,
                            reference_index = 1) {

  # Generate reference ratings
  reference <- sample(categories, n_cases, replace = TRUE)

  # Create rating matrix
  ratings <- matrix("", nrow = n_cases, ncol = n_raters)
  ratings[, reference_index] <- reference

  # Generate other raters with controlled agreement
  for (i in setdiff(1:n_raters, reference_index)) {
    for (j in 1:n_cases) {
      if (runif(1) < agreement_level) {
        # Agree with reference
        ratings[j, i] <- reference[j]
      } else {
        # Disagree - choose different category
        other_categories <- setdiff(categories, reference[j])
        ratings[j, i] <- sample(other_categories, 1)
      }
    }
  }

  return(ratings)
}


#' Generate rater characteristics data
#' @param n_raters Number of raters
#' @param rater_names Vector of rater names
#' @return Data frame with rater characteristics
generate_rater_characteristics <- function(n_raters, rater_names) {

  data.frame(
    Rater = rater_names,
    Experience_Years = round(runif(n_raters, 1, 25)),
    Specialty = sample(
      c("General Pathology", "Subspecialist", "Fellow", "Community Practice"),
      n_raters,
      replace = TRUE
    ),
    Institution = sample(
      c("Academic Medical Center", "Community Hospital", "Reference Lab"),
      n_raters,
      replace = TRUE
    ),
    Monthly_Volume = round(runif(n_raters, 20, 500)),
    Training_Location = sample(
      c("University A", "University B", "University C", "University D"),
      n_raters,
      replace = TRUE
    )
  )
}


# =============================================================================
# Dataset 1: Basic Two-Rater Agreement (Cohen's Kappa)
# =============================================================================

cat("Creating Dataset 1: Basic Two-Rater Agreement...\n")

n_cases_basic <- 50
ratings_basic <- generate_ratings(n_cases_basic, 2, agreement_level = 0.75)

pathagreement_two_raters <- data.frame(
  Case_ID = paste0("CASE_", sprintf("%03d", 1:n_cases_basic)),
  Pathologist_A = ratings_basic[, 1],
  Pathologist_B = ratings_basic[, 2]
)

# Convert to factors with proper ordering
pathagreement_two_raters$Pathologist_A <- factor(
  pathagreement_two_raters$Pathologist_A,
  levels = c("Benign", "Borderline", "Malignant"),
  ordered = TRUE
)
pathagreement_two_raters$Pathologist_B <- factor(
  pathagreement_two_raters$Pathologist_B,
  levels = c("Benign", "Borderline", "Malignant"),
  ordered = TRUE
)

# Save
write.csv(pathagreement_two_raters,
          "data/pathagreement_two_raters.csv",
          row.names = FALSE)
save(pathagreement_two_raters,
     file = "data/pathagreement_two_raters.rda")

# Calculate actual agreement
agreement_pct <- mean(pathagreement_two_raters$Pathologist_A ==
                     pathagreement_two_raters$Pathologist_B) * 100

cat(sprintf("   ✅ Created: pathagreement_two_raters (n=%d)\n", n_cases_basic))
cat(sprintf("   - Observed agreement: %.1f%%\n", agreement_pct))
cat(sprintf("   - 2 raters, 3 categories (ordinal)\n\n"))


# =============================================================================
# Dataset 2: Multi-Rater Agreement (Fleiss' Kappa)
# =============================================================================

cat("Creating Dataset 2: Multi-Rater Agreement (5 raters)...\n")

n_cases_multi <- 60
n_raters <- 5
ratings_multi <- generate_ratings(n_cases_multi, n_raters, agreement_level = 0.65)

pathagreement_multi_raters <- data.frame(
  Case_ID = paste0("CASE_", sprintf("%03d", 1:n_cases_multi)),
  Rater_1 = ratings_multi[, 1],
  Rater_2 = ratings_multi[, 2],
  Rater_3 = ratings_multi[, 3],
  Rater_4 = ratings_multi[, 4],
  Rater_5 = ratings_multi[, 5]
)

# Convert to ordered factors
for (i in 2:6) {
  pathagreement_multi_raters[[i]] <- factor(
    pathagreement_multi_raters[[i]],
    levels = c("Benign", "Borderline", "Malignant"),
    ordered = TRUE
  )
}

# Save
write.csv(pathagreement_multi_raters,
          "data/pathagreement_multi_raters.csv",
          row.names = FALSE)
save(pathagreement_multi_raters,
     file = "data/pathagreement_multi_raters.rda")

cat(sprintf("   ✅ Created: pathagreement_multi_raters (n=%d)\n", n_cases_multi))
cat(sprintf("   - 5 raters, 3 categories (ordinal)\n"))
cat(sprintf("   - Tests Fleiss' kappa, Krippendorff's alpha\n\n"))


# =============================================================================
# Dataset 3: Breast Pathology Agreement Study with Reference Standard
# =============================================================================

cat("Creating Dataset 3: Breast Pathology with Reference Standard...\n")

n_cases_breast <- 100
categories_breast <- c("Benign", "Atypical", "DCIS", "Invasive")

# Generate reference standard (expert consensus)
reference_std <- sample(categories_breast, n_cases_breast, replace = TRUE,
                       prob = c(0.4, 0.2, 0.2, 0.2))

# Generate 6 pathologist ratings with varying agreement to reference
ratings_breast <- matrix("", nrow = n_cases_breast, ncol = 6)
agreement_levels <- c(0.85, 0.80, 0.75, 0.70, 0.65, 0.60)  # Varying expertise

for (i in 1:6) {
  for (j in 1:n_cases_breast) {
    if (runif(1) < agreement_levels[i]) {
      ratings_breast[j, i] <- reference_std[j]
    } else {
      ratings_breast[j, i] <- sample(setdiff(categories_breast, reference_std[j]), 1)
    }
  }
}

pathagreement_breast <- data.frame(
  Case_ID = paste0("BREAST_", sprintf("%04d", 1:n_cases_breast)),
  Reference_Standard = reference_std,
  Path_1 = ratings_breast[, 1],
  Path_2 = ratings_breast[, 2],
  Path_3 = ratings_breast[, 3],
  Path_4 = ratings_breast[, 4],
  Path_5 = ratings_breast[, 5],
  Path_6 = ratings_breast[, 6],
  Case_Difficulty = sample(c("Easy", "Moderate", "Difficult"),
                          n_cases_breast, replace = TRUE,
                          prob = c(0.3, 0.5, 0.2))
)

# Convert to ordered factors
factor_cols <- c("Reference_Standard", paste0("Path_", 1:6))
for (col in factor_cols) {
  pathagreement_breast[[col]] <- factor(
    pathagreement_breast[[col]],
    levels = categories_breast,
    ordered = TRUE
  )
}
pathagreement_breast$Case_Difficulty <- factor(
  pathagreement_breast$Case_Difficulty,
  levels = c("Easy", "Moderate", "Difficult"),
  ordered = TRUE
)

# Save
write.csv(pathagreement_breast,
          "data/pathagreement_breast.csv",
          row.names = FALSE)
save(pathagreement_breast,
     file = "data/pathagreement_breast.rda")

cat(sprintf("   ✅ Created: pathagreement_breast (n=%d)\n", n_cases_breast))
cat(sprintf("   - 6 pathologists + reference standard\n"))
cat(sprintf("   - 4 diagnostic categories (ordinal)\n"))
cat(sprintf("   - Includes case difficulty ratings\n\n"))


# =============================================================================
# Dataset 4: Diagnostic Style Clustering (Usubutun Method)
# =============================================================================

cat("Creating Dataset 4: Diagnostic Style Clustering Dataset...\n")

n_cases_cluster <- 80
n_raters_cluster <- 12

# Create three diagnostic style groups:
# Group 1 (Conservative): Tends toward lower-grade diagnoses
# Group 2 (Moderate): Balanced approach
# Group 3 (Aggressive): Tends toward higher-grade diagnoses

categories_ord <- c("Benign", "EIN", "Adenocarcinoma")

# Generate true diagnoses
true_diag <- sample(categories_ord, n_cases_cluster, replace = TRUE,
                   prob = c(0.5, 0.3, 0.2))

ratings_cluster <- matrix("", nrow = n_cases_cluster, ncol = n_raters_cluster)

# Generate ratings for each style group
for (i in 1:n_raters_cluster) {
  style_group <- ceiling(i / 4)  # 3 groups of 4 raters each

  for (j in 1:n_cases_cluster) {
    base_diag <- true_diag[j]

    if (style_group == 1) {
      # Conservative: Bias toward lower grade
      if (base_diag == "Adenocarcinoma" && runif(1) < 0.4) {
        ratings_cluster[j, i] <- "EIN"
      } else if (base_diag == "EIN" && runif(1) < 0.3) {
        ratings_cluster[j, i] <- "Benign"
      } else {
        ratings_cluster[j, i] <- base_diag
      }

    } else if (style_group == 2) {
      # Moderate: Accurate with some variation
      if (runif(1) < 0.8) {
        ratings_cluster[j, i] <- base_diag
      } else {
        ratings_cluster[j, i] <- sample(categories_ord, 1)
      }

    } else {
      # Aggressive: Bias toward higher grade
      if (base_diag == "Benign" && runif(1) < 0.3) {
        ratings_cluster[j, i] <- "EIN"
      } else if (base_diag == "EIN" && runif(1) < 0.4) {
        ratings_cluster[j, i] <- "Adenocarcinoma"
      } else {
        ratings_cluster[j, i] <- base_diag
      }
    }
  }
}

# Create dataset
rater_names <- LETTERS[1:n_raters_cluster]
pathagreement_clustering <- data.frame(
  Case_ID = paste0("EIN_", sprintf("%03d", 1:n_cases_cluster)),
  Reference = true_diag
)

# Add rater columns
for (i in 1:n_raters_cluster) {
  pathagreement_clustering[[rater_names[i]]] <- factor(
    ratings_cluster[, i],
    levels = categories_ord,
    ordered = TRUE
  )
}
pathagreement_clustering$Reference <- factor(
  pathagreement_clustering$Reference,
  levels = categories_ord,
  ordered = TRUE
)

# Generate rater characteristics
rater_chars <- generate_rater_characteristics(n_raters_cluster, rater_names)
rater_chars$True_Style_Group <- rep(1:3, each = 4)  # Hidden true grouping

# Save main dataset
write.csv(pathagreement_clustering,
          "data/pathagreement_clustering.csv",
          row.names = FALSE)
save(pathagreement_clustering,
     file = "data/pathagreement_clustering.rda")

# Save rater characteristics separately
write.csv(rater_chars,
          "data/pathagreement_clustering_rater_info.csv",
          row.names = FALSE)
save(rater_chars,
     file = "data/pathagreement_clustering_rater_info.rda")

cat(sprintf("   ✅ Created: pathagreement_clustering (n=%d)\n", n_cases_cluster))
cat(sprintf("   - 12 raters in 3 diagnostic style groups\n"))
cat(sprintf("   - Tests Usubutun clustering method\n"))
cat(sprintf("   - Rater characteristics included\n\n"))


# =============================================================================
# Dataset 5: Melanoma Grading (Nominal Categories)
# =============================================================================

cat("Creating Dataset 5: Melanoma Classification (Nominal)...\n")

n_cases_melanoma <- 70
categories_melanoma <- c("Benign Nevus", "Dysplastic Nevus", "Melanoma in situ",
                        "Invasive Melanoma", "Spitzoid Lesion")

# Generate ratings for 4 dermatopathologists
ratings_melanoma <- generate_ratings(
  n_cases_melanoma,
  4,
  categories_melanoma,
  agreement_level = 0.60  # Lower agreement for difficult diagnoses
)

pathagreement_melanoma <- data.frame(
  Case_ID = paste0("MEL_", sprintf("%03d", 1:n_cases_melanoma)),
  Dermpath_1 = ratings_melanoma[, 1],
  Dermpath_2 = ratings_melanoma[, 2],
  Dermpath_3 = ratings_melanoma[, 3],
  Dermpath_4 = ratings_melanoma[, 4]
)

# These are NOMINAL categories (no inherent order)
for (i in 2:5) {
  pathagreement_melanoma[[i]] <- factor(
    pathagreement_melanoma[[i]],
    levels = categories_melanoma,
    ordered = FALSE  # Nominal, not ordinal
  )
}

# Save
write.csv(pathagreement_melanoma,
          "data/pathagreement_melanoma.csv",
          row.names = FALSE)
save(pathagreement_melanoma,
     file = "data/pathagreement_melanoma.rda")

cat(sprintf("   ✅ Created: pathagreement_melanoma (n=%d)\n", n_cases_melanoma))
cat(sprintf("   - 4 dermatopathologists\n"))
cat(sprintf("   - 5 nominal categories\n"))
cat(sprintf("   - Tests unweighted kappa (nominal data)\n\n"))


# =============================================================================
# Dataset 6: Perfect Agreement (Edge Case)
# =============================================================================

cat("Creating Dataset 6: Perfect Agreement (Edge Case)...\n")

n_cases_perfect <- 20
diagnosis_perfect <- sample(c("Benign", "Malignant"), n_cases_perfect, replace = TRUE)

pathagreement_perfect <- data.frame(
  Case_ID = paste0("PERF_", sprintf("%02d", 1:n_cases_perfect)),
  Rater_1 = diagnosis_perfect,
  Rater_2 = diagnosis_perfect,
  Rater_3 = diagnosis_perfect
)

for (i in 2:4) {
  pathagreement_perfect[[i]] <- factor(pathagreement_perfect[[i]])
}

# Save
write.csv(pathagreement_perfect,
          "data/pathagreement_perfect.csv",
          row.names = FALSE)
save(pathagreement_perfect,
     file = "data/pathagreement_perfect.rda")

cat(sprintf("   ✅ Created: pathagreement_perfect (n=%d)\n", n_cases_perfect))
cat(sprintf("   - Perfect agreement (kappa = 1.0)\n"))
cat(sprintf("   - Tests edge case\n\n"))


# =============================================================================
# Dataset 7: Complete Disagreement (Edge Case)
# =============================================================================

cat("Creating Dataset 7: Complete Disagreement (Edge Case)...\n")

n_cases_disagree <- 15
categories_3 <- c("Category_1", "Category_2", "Category_3")

# Force complete disagreement where possible
pathagreement_disagreement <- data.frame(
  Case_ID = paste0("DIS_", sprintf("%02d", 1:n_cases_disagree)),
  Rater_1 = rep(categories_3[1], n_cases_disagree),
  Rater_2 = rep(categories_3[2], n_cases_disagree),
  Rater_3 = rep(categories_3[3], n_cases_disagree)
)

for (i in 2:4) {
  pathagreement_disagreement[[i]] <- factor(
    pathagreement_disagreement[[i]],
    levels = categories_3
  )
}

# Save
write.csv(pathagreement_disagreement,
          "data/pathagreement_disagreement.csv",
          row.names = FALSE)
save(pathagreement_disagreement,
     file = "data/pathagreement_disagreement.rda")

cat(sprintf("   ✅ Created: pathagreement_disagreement (n=%d)\n", n_cases_disagree))
cat(sprintf("   - Maximal disagreement\n"))
cat(sprintf("   - Tests edge case (kappa near 0 or negative)\n\n"))


# =============================================================================
# Dataset 8: Missing Data Patterns
# =============================================================================

cat("Creating Dataset 8: Missing Data Patterns...\n")

n_cases_missing <- 40
ratings_missing <- generate_ratings(n_cases_missing, 4, agreement_level = 0.70)

pathagreement_missing <- data.frame(
  Case_ID = paste0("MISS_", sprintf("%03d", 1:n_cases_missing)),
  Rater_1 = ratings_missing[, 1],
  Rater_2 = ratings_missing[, 2],
  Rater_3 = ratings_missing[, 3],
  Rater_4 = ratings_missing[, 4]
)

# Introduce missing values (10% random)
for (i in 2:5) {
  missing_indices <- sample(1:n_cases_missing, size = round(n_cases_missing * 0.1))
  pathagreement_missing[[i]][missing_indices] <- NA
  pathagreement_missing[[i]] <- factor(
    pathagreement_missing[[i]],
    levels = c("Benign", "Borderline", "Malignant"),
    ordered = TRUE
  )
}

# Save
write.csv(pathagreement_missing,
          "data/pathagreement_missing.csv",
          row.names = FALSE, na = "")
save(pathagreement_missing,
     file = "data/pathagreement_missing.rda")

cat(sprintf("   ✅ Created: pathagreement_missing (n=%d)\n", n_cases_missing))
cat(sprintf("   - Contains ~10%% missing values\n"))
cat(sprintf("   - Tests missing data handling\n\n"))


# =============================================================================
# Dataset 9: Single Case (Minimum Data)
# =============================================================================

cat("Creating Dataset 9: Single Case (Minimum Data)...\n")

pathagreement_single <- data.frame(
  Case_ID = "SINGLE_001",
  Rater_1 = factor("Malignant", levels = c("Benign", "Malignant")),
  Rater_2 = factor("Benign", levels = c("Benign", "Malignant"))
)

# Save
write.csv(pathagreement_single,
          "data/pathagreement_single.csv",
          row.names = FALSE)
save(pathagreement_single,
     file = "data/pathagreement_single.rda")

cat(sprintf("   ✅ Created: pathagreement_single (n=1)\n"))
cat(sprintf("   - Minimum viable data\n"))
cat(sprintf("   - Tests edge case handling\n\n"))


# =============================================================================
# Dataset 10: Comprehensive Test with All Features
# =============================================================================

cat("Creating Dataset 10: Comprehensive Test Dataset...\n")

n_cases_comp <- 120
n_raters_comp <- 8

# Generate complex data
ratings_comp <- generate_ratings(n_cases_comp, n_raters_comp, agreement_level = 0.68)

# Create reference standard
reference_comp <- apply(ratings_comp, 1, function(x) {
  tbl <- table(x)
  names(tbl)[which.max(tbl)][1]  # Majority vote
})

pathagreement_comprehensive <- data.frame(
  Case_ID = paste0("COMP_", sprintf("%04d", 1:n_cases_comp)),
  Reference_Diagnosis = reference_comp,
  Subspecialty = sample(
    c("Breast", "GI", "GU", "Derm", "Lung", "Heme"),
    n_cases_comp,
    replace = TRUE
  ),
  Case_Difficulty = sample(
    c("Easy", "Moderate", "Difficult"),
    n_cases_comp,
    replace = TRUE,
    prob = c(0.3, 0.5, 0.2)
  ),
  Image_Quality = sample(
    c("Excellent", "Good", "Fair", "Poor"),
    n_cases_comp,
    replace = TRUE,
    prob = c(0.4, 0.3, 0.2, 0.1)
  )
)

# Add rater columns
for (i in 1:n_raters_comp) {
  pathagreement_comprehensive[[paste0("Pathologist_", LETTERS[i])]] <- factor(
    ratings_comp[, i],
    levels = c("Benign", "Borderline", "Malignant"),
    ordered = TRUE
  )
}
pathagreement_comprehensive$Reference_Diagnosis <- factor(
  pathagreement_comprehensive$Reference_Diagnosis,
  levels = c("Benign", "Borderline", "Malignant"),
  ordered = TRUE
)

# Add case metadata
pathagreement_comprehensive$Discordant <- apply(
  ratings_comp,
  1,
  function(x) length(unique(x)) > 1
)

# Save
write.csv(pathagreement_comprehensive,
          "data/pathagreement_comprehensive.csv",
          row.names = FALSE)
save(pathagreement_comprehensive,
     file = "data/pathagreement_comprehensive.rda")

cat(sprintf("   ✅ Created: pathagreement_comprehensive (n=%d)\n", n_cases_comp))
cat(sprintf("   - 8 pathologists + reference\n"))
cat(sprintf("   - Multiple metadata variables\n"))
cat(sprintf("   - Tests all analysis features\n\n"))


# =============================================================================
# Create OMV files (jamovi format)
# =============================================================================

cat("\n📊 Creating OMV (jamovi) files...\n")

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {

  datasets <- list(
    "pathagreement_two_raters" = pathagreement_two_raters,
    "pathagreement_multi_raters" = pathagreement_multi_raters,
    "pathagreement_breast" = pathagreement_breast,
    "pathagreement_clustering" = pathagreement_clustering,
    "pathagreement_melanoma" = pathagreement_melanoma,
    "pathagreement_perfect" = pathagreement_perfect,
    "pathagreement_disagreement" = pathagreement_disagreement,
    "pathagreement_missing" = pathagreement_missing,
    "pathagreement_single" = pathagreement_single,
    "pathagreement_comprehensive" = pathagreement_comprehensive
  )

  for (name in names(datasets)) {
    omv_path <- paste0("data/", name, ".omv")
    jmvReadWrite::write_omv(datasets[[name]], omv_path)
    cat(sprintf("   ✅ %s.omv\n", name))
  }

} else {
  cat("   ⚠️  Package 'jmvReadWrite' not available\n")
  cat("   Install with: install.packages('jmvReadWrite')\n")
}


# =============================================================================
# Summary Report
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("📋 PATHAGREEMENT TEST DATA GENERATION COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Generated 10 test datasets:\n\n")

cat("1. pathagreement_two_raters       - Basic Cohen's kappa (n=50, 2 raters)\n")
cat("2. pathagreement_multi_raters     - Fleiss' kappa (n=60, 5 raters)\n")
cat("3. pathagreement_breast           - Reference standard (n=100, 6 raters)\n")
cat("4. pathagreement_clustering       - Diagnostic styles (n=80, 12 raters)\n")
cat("5. pathagreement_melanoma         - Nominal categories (n=70, 4 raters)\n")
cat("6. pathagreement_perfect          - Perfect agreement (n=20, 3 raters)\n")
cat("7. pathagreement_disagreement     - Complete disagreement (n=15, 3 raters)\n")
cat("8. pathagreement_missing          - Missing data (n=40, 4 raters)\n")
cat("9. pathagreement_single           - Single case (n=1, 2 raters)\n")
cat("10. pathagreement_comprehensive   - All features (n=120, 8 raters)\n")

cat("\nFeature Coverage:\n")
cat("  ✅ Cohen's kappa (2 raters)\n")
cat("  ✅ Fleiss' kappa (3+ raters)\n")
cat("  ✅ Krippendorff's alpha\n")
cat("  ✅ Weighted kappa (ordinal data)\n")
cat("  ✅ Reference standard comparisons\n")
cat("  ✅ Diagnostic style clustering (Usubutun)\n")
cat("  ✅ Nominal vs ordinal categories\n")
cat("  ✅ Missing data handling\n")
cat("  ✅ Edge cases (perfect/no agreement)\n")

cat("\nFormats created:\n")
cat("  ✅ CSV (human-readable, for manual testing)\n")
cat("  ✅ RDA (R binary, for automated tests)\n")
cat("  ✅ OMV (jamovi native, for manual testing)\n")

cat("\nNext steps:\n")
cat("  1. Review generated data:\n")
cat("     ls -lh data/pathagreement_*.csv\n")
cat("  2. Run automated tests:\n")
cat("     testthat::test_file('tests/testthat/test-pathagreement.R')\n")
cat("  3. Manual testing in jamovi:\n")
cat("     Open data/pathagreement_*.omv files\n")
cat("  4. View test guide:\n")
cat("     cat tests/PATHAGREEMENT_TEST_GUIDE.md\n")

cat("\n", strrep("=", 70), "\n\n")
