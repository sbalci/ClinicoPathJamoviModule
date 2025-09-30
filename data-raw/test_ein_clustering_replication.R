# Test Script: Replicate Usubutun et al. (2012) Figure 1
# Verify clustering implementation produces expected analysis

library(dplyr)
library(pheatmap)
library(cluster)
library(RColorBrewer)

# Load data
ein_wide <- read.csv("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_agreement_wide.csv")
pathologist_info <- read.csv("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_pathologist_info.csv")

cat("=============================================================\n")
cat("Replication Test: Usubutun et al. (2012) Clustering Analysis\n")
cat("=============================================================\n\n")

# Extract diagnosis matrix (cases × pathologists)
pathologist_cols <- c("T", "S", "R", "N", "H", "J", "K", "B", "C", "I",
                      "D", "M", "L", "Q", "P", "O", "A", "G", "F", "E")
diagnosis_matrix <- as.matrix(ein_wide[, pathologist_cols])
rownames(diagnosis_matrix) <- ein_wide$case_id

cat("Data Structure:\n")
cat("  Cases:", nrow(diagnosis_matrix), "\n")
cat("  Pathologists:", ncol(diagnosis_matrix), "\n")
cat("  Diagnoses:", paste(unique(as.vector(diagnosis_matrix)), collapse = ", "), "\n\n")

# Calculate pairwise agreement matrix (pathologists × pathologists)
n_pathologists <- ncol(diagnosis_matrix)
agreement_matrix <- matrix(0, nrow = n_pathologists, ncol = n_pathologists)
rownames(agreement_matrix) <- colnames(agreement_matrix) <- pathologist_cols

for (i in 1:n_pathologists) {
  for (j in 1:n_pathologists) {
    if (i == j) {
      agreement_matrix[i, j] <- 1.0
    } else {
      agreement_matrix[i, j] <- mean(diagnosis_matrix[, i] == diagnosis_matrix[, j], na.rm = TRUE)
    }
  }
}

cat("Agreement Matrix Statistics:\n")
cat("  Mean pairwise agreement:", round(mean(agreement_matrix[upper.tri(agreement_matrix)]) * 100, 1), "%\n")
cat("  Range:", round(min(agreement_matrix[upper.tri(agreement_matrix)]) * 100, 1), "-",
    round(max(agreement_matrix[upper.tri(agreement_matrix)]) * 100, 1), "%\n\n")

# Convert to distance matrix (1 - agreement)
distance_matrix <- as.dist(1 - agreement_matrix)

# Hierarchical clustering using Ward's linkage
hc_pathologists <- hclust(distance_matrix, method = "ward.D2")

cat("Hierarchical Clustering:\n")
cat("  Method: Ward's linkage\n")
cat("  Distance metric: 1 - percentage agreement\n\n")

# Cut tree to get 3 style groups
k <- 3
style_assignments <- cutree(hc_pathologists, k = k)

cat("Style Group Assignments (k = 3):\n")
for (group in 1:k) {
  members <- names(style_assignments[style_assignments == group])
  cat("  Group", group, "(n =", length(members), "):", paste(members, collapse = ", "), "\n")
}
cat("\n")

# Calculate silhouette scores
sil <- silhouette(style_assignments, distance_matrix)
avg_sil <- mean(sil[, "sil_width"])

cat("Cluster Quality:\n")
cat("  Average silhouette score:", round(avg_sil, 3), "\n")
cat("  Interpretation:",
    ifelse(avg_sil > 0.7, "Strong separation",
    ifelse(avg_sil > 0.5, "Moderate separation", "Weak separation")), "\n\n")

# Within-group vs between-group agreement
cat("Within-Group vs Between-Group Agreement:\n")
for (group in 1:k) {
  group_members <- which(style_assignments == group)

  if (length(group_members) > 1) {
    within_agreements <- agreement_matrix[group_members, group_members]
    within_agreement <- mean(within_agreements[upper.tri(within_agreements)])

    other_members <- which(style_assignments != group)
    if (length(other_members) > 0) {
      between_agreements <- agreement_matrix[group_members, other_members, drop = FALSE]
      between_agreement <- mean(between_agreements)
    } else {
      between_agreement <- NA
    }

    cat("  Group", group, ":\n")
    cat("    Within-group:  ", round(within_agreement * 100, 1), "%\n")
    cat("    Between-group: ", round(between_agreement * 100, 1), "%\n")
  }
}
cat("\n")

# Identify discordant cases
cat("Discordant Cases (high disagreement between groups):\n")
discordant_threshold <- 0.5
n_discordant <- 0

for (i in 1:nrow(diagnosis_matrix)) {
  case_diagnoses <- diagnosis_matrix[i, ]

  # Calculate between-group disagreement
  max_disagreement <- 0
  for (g1 in 1:(k-1)) {
    for (g2 in (g1+1):k) {
      group1_members <- which(style_assignments == g1)
      group2_members <- which(style_assignments == g2)

      g1_diagnoses <- case_diagnoses[group1_members]
      g2_diagnoses <- case_diagnoses[group2_members]

      disagreement <- 1 - mean(outer(g1_diagnoses, g2_diagnoses, "=="))
      if (disagreement > max_disagreement) {
        max_disagreement <- disagreement
      }
    }
  }

  if (max_disagreement >= discordant_threshold) {
    n_discordant <- n_discordant + 1
    cat("  Case", i, ": disagreement =", round(max_disagreement, 3), "\n")
  }
}
cat("  Total discordant cases:", n_discordant, "\n\n")

# Create heatmap visualization
cat("Generating heatmap...\n")

# Convert diagnoses to numeric for coloring
diagnosis_numeric <- matrix(
  as.numeric(factor(diagnosis_matrix,
                   levels = c("Benign", "EIN", "Adenocarcinoma"))),
  nrow = nrow(diagnosis_matrix),
  ncol = ncol(diagnosis_matrix)
)
rownames(diagnosis_numeric) <- rownames(diagnosis_matrix)
colnames(diagnosis_numeric) <- colnames(diagnosis_matrix)

# Define colors
colors <- colorRampPalette(c("#1976D2", "#4CAF50", "#FFC107"))(100)

# Style group annotation
style_group_names <- c("Conservative", "Balanced", "Sensitive")[style_assignments]
annotation_col <- data.frame(
  Style = factor(style_group_names, levels = c("Conservative", "Balanced", "Sensitive"))
)
rownames(annotation_col) <- colnames(diagnosis_matrix)

annotation_colors <- list(
  Style = c(
    "Conservative" = "#66BB6A",   # Green
    "Balanced" = "#FFEB3B",        # Yellow
    "Sensitive" = "#EF5350"        # Red
  )
)

# Create heatmap
png("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_clustering_heatmap_test.png",
    width = 1000, height = 1200, res = 150)

pheatmap(
  diagnosis_numeric,
  cluster_rows = TRUE,
  cluster_cols = hc_pathologists,
  clustering_distance_cols = distance_matrix,
  clustering_method = "ward.D2",
  color = colors,
  breaks = seq(0.5, 3.5, length.out = 101),
  cutree_cols = 3,
  annotation_col = annotation_col,
  annotation_colors = annotation_colors,
  main = "Diagnostic Style Groups of 20 Pathologists\n(Replication of Usubutun et al. 2012)",
  fontsize = 10,
  fontsize_row = 6,
  fontsize_col = 10,
  angle_col = 0,
  legend_breaks = c(1, 2, 3),
  legend_labels = c("Benign", "EIN", "Cancer"),
  border_color = "white"
)

dev.off()

cat("Heatmap saved to: data/ein_clustering_heatmap_test.png\n\n")

# Compare with expected results
cat("=============================================================\n")
cat("Comparison with Original Study (Usubutun et al. 2012)\n")
cat("=============================================================\n\n")

cat("Expected Results:\n")
cat("  Style groups: 3 (Green n=4, Yellow n=11, Red n=5)\n")
cat("  Overall kappa: 0.58\n")
cat("  Average agreement with reference: 79%\n")
cat("  No association with experience/institution\n\n")

cat("Our Results:\n")
cat("  Style groups: 3 (n =", table(style_assignments), ")\n")
cat("  Average pairwise agreement:", round(mean(agreement_matrix[upper.tri(agreement_matrix)]) * 100, 1), "%\n")
cat("  Average silhouette:", round(avg_sil, 3), "\n")
cat("  Discordant cases:", n_discordant, "\n\n")

cat("✓ Replication test complete!\n")
cat("  Check heatmap for visual comparison with Figure 1\n")
cat("  Expected pattern: horizontal banding by case difficulty\n")
cat("                    vertical grouping by style\n")
cat("                    3 distinct pathologist clusters\n\n")

# Test with pathologist characteristics
cat("Testing Association with Characteristics:\n")

# Chi-square test: style group vs specialty
if (length(unique(pathologist_info$specialty)) > 1) {
  # Reorder pathologist_info to match style_assignments
  pathologist_info_ordered <- pathologist_info[match(names(style_assignments), pathologist_info$pathologist), ]

  contingency <- table(pathologist_info_ordered$specialty, style_assignments)
  if (min(contingency) >= 5) {
    chi_test <- chisq.test(contingency)
    cat("  Style vs Specialty: χ² =", round(chi_test$statistic, 3),
        ", p =", round(chi_test$p.value, 3), "\n")
  } else {
    fisher_test <- fisher.test(contingency, simulate.p.value = TRUE)
    cat("  Style vs Specialty: Fisher's exact p =", round(fisher_test$p.value, 3), "\n")
  }
}

# Kruskal-Wallis test: style group vs experience
kruskal_test <- kruskal.test(pathologist_info_ordered$years_experience ~ style_assignments)
cat("  Style vs Experience: H =", round(kruskal_test$statistic, 3),
    ", p =", round(kruskal_test$p.value, 3), "\n")

cat("\nExpected: p > 0.05 (no significant association)\n")
cat("This replicates the finding that diagnostic style is independent\n")
cat("of training, experience, and practice setting.\n")
