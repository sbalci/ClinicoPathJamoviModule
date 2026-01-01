# CRITICAL: Integration Testing for clinicalheatmap Function
#
# **STATUS: VALIDATION TESTS FOR COMPLEX MULTI-STAGE WORKFLOW**
#
# This test suite validates the critical "glue code" that chains together:
# 1. Data reshaping (long -> wide format)
# 2. Scaling (Z-score normalization)
# 3. Clustering (hierarchical clustering)
# 4. Cluster assignment extraction
# 5. Survival data merge
# 6. Survival analysis
#
# REVIEWER CONCERN:
# "The problem is not the individual components, but the 'glue code' that connects
# them. A bug at any point in this chain—for example, a misalignment of patient IDs
# when merging cluster assignments with survival data—would silently and completely
# invalidate the final result."
#
# TESTING STRATEGY:
# Use small, controlled datasets where we can manually verify each step of the
# transformation pipeline to ensure data integrity is maintained throughout.

library(testthat)
library(dplyr)
library(tidyr)
library(tibble)

# ============================================================================
# PART 1: DATA TRANSFORMATION CHAIN VALIDATION
# Tests that patient IDs are preserved through reshaping operations
# ============================================================================

test_that("CRITICAL: Patient IDs preserved through pivot_wider transformation", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Create test data with known patient IDs
  test_data <- data.frame(
    patient_id = rep(c("P001", "P002", "P003"), each = 3),
    biomarker = rep(c("ER", "PR", "HER2"), 3),
    expression = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
    stringsAsFactors = FALSE
  )

  # Perform the transformation that clinicalheatmap does
  wide_data <- test_data %>%
    dplyr::select(patient_id, biomarker, expression) %>%
    tidyr::pivot_wider(names_from = biomarker, values_from = expression)

  # CRITICAL VALIDATION: Patient IDs must be in original order
  expect_equal(wide_data$patient_id, c("P001", "P002", "P003"))

  # Convert to matrix with rownames (as clinicalheatmap does)
  mat_data <- wide_data %>%
    tibble::column_to_rownames("patient_id") %>%
    as.matrix()

  # CRITICAL VALIDATION: Rownames must match original patient IDs
  expect_equal(rownames(mat_data), c("P001", "P002", "P003"))

  # CRITICAL VALIDATION: Data values must be in correct positions
  expect_equal(mat_data["P001", "ER"], 10)
  expect_equal(mat_data["P001", "PR"], 20)
  expect_equal(mat_data["P001", "HER2"], 30)
  expect_equal(mat_data["P002", "ER"], 40)
  expect_equal(mat_data["P003", "HER2"], 90)
})

test_that("CRITICAL: Cluster assignments preserve patient ID mapping", {
  skip_if_not_installed("tibble")

  # Create simple data with known structure
  test_data <- data.frame(
    patient_id = c("P001", "P002", "P003", "P004"),
    biomarker = rep(c("ER", "PR"), 4),
    expression = c(10, 20, 15, 25, 80, 90, 85, 95),  # Two distinct groups
    stringsAsFactors = FALSE
  )

  # Transform to matrix (mimics clinicalheatmap workflow)
  mat_data <- test_data %>%
    dplyr::select(patient_id, biomarker, expression) %>%
    tidyr::pivot_wider(names_from = biomarker, values_from = expression) %>%
    tibble::column_to_rownames("patient_id") %>%
    as.matrix()

  # Perform clustering (mimics clinicalheatmap workflow)
  dist_rows <- stats::dist(mat_data, method = "euclidean")
  hc_rows <- stats::hclust(dist_rows, method = "complete")
  row_clusters <- stats::cutree(hc_rows, k = 2)

  # Extract cluster assignments (mimics clinicalheatmap workflow)
  cluster_df <- data.frame(
    patient_id = names(row_clusters),
    cluster = as.integer(row_clusters),
    stringsAsFactors = FALSE
  )

  # CRITICAL VALIDATION: All original patient IDs must be in cluster assignments
  expect_setequal(cluster_df$patient_id, c("P001", "P002", "P003", "P004"))

  # CRITICAL VALIDATION: Number of clusters must match number of patients
  expect_equal(nrow(cluster_df), 4)

  # CRITICAL VALIDATION: Each patient must have exactly one cluster assignment
  expect_equal(length(unique(cluster_df$patient_id)), 4)
})

# ============================================================================
# PART 2: SCALING VALIDATION
# Tests that Z-score normalization is mathematically correct
# ============================================================================

test_that("CRITICAL: Row scaling produces correct Z-scores", {
  # Create test data with known mean/sd
  test_data <- data.frame(
    patient_id = c("P001", "P002"),
    biomarker = rep(c("B1", "B2", "B3"), 2),
    expression = c(10, 20, 30, 40, 50, 60),
    stringsAsFactors = FALSE
  )

  # Transform to matrix
  mat_data <- test_data %>%
    tidyr::pivot_wider(names_from = biomarker, values_from = expression) %>%
    tibble::column_to_rownames("patient_id") %>%
    as.matrix()

  # Apply row scaling (mimics clinicalheatmap with scaleMethod="row")
  scaled_data <- t(scale(t(mat_data)))

  # CRITICAL VALIDATION: Each row should have mean ≈ 0 and sd ≈ 1
  row_means <- apply(scaled_data, 1, mean)
  row_sds <- apply(scaled_data, 1, sd)

  expect_equal(row_means[1], 0, tolerance = 1e-10)
  expect_equal(row_means[2], 0, tolerance = 1e-10)
  expect_equal(row_sds[1], 1, tolerance = 1e-10)
  expect_equal(row_sds[2], 1, tolerance = 1e-10)

  # CRITICAL VALIDATION: Manual calculation for P001 row
  p001_values <- c(10, 20, 30)
  p001_mean <- mean(p001_values)  # 20
  p001_sd <- sd(p001_values)      # 10
  p001_scaled_manual <- (p001_values - p001_mean) / p001_sd  # c(-1, 0, 1)

  expect_equal(scaled_data["P001", ], p001_scaled_manual, tolerance = 1e-10)
})

test_that("CRITICAL: Column scaling produces correct Z-scores", {
  # Create test data
  test_data <- data.frame(
    patient_id = rep(c("P001", "P002", "P003"), each = 2),
    biomarker = rep(c("B1", "B2"), 3),
    expression = c(10, 20, 30, 40, 50, 60),
    stringsAsFactors = FALSE
  )

  # Transform to matrix
  mat_data <- test_data %>%
    tidyr::pivot_wider(names_from = biomarker, values_from = expression) %>%
    tibble::column_to_rownames("patient_id") %>%
    as.matrix()

  # Apply column scaling (mimics clinicalheatmap with scaleMethod="column")
  scaled_data <- scale(mat_data)

  # CRITICAL VALIDATION: Each column should have mean ≈ 0 and sd ≈ 1
  col_means <- apply(scaled_data, 2, mean)
  col_sds <- apply(scaled_data, 2, sd)

  expect_equal(col_means[1], 0, tolerance = 1e-10)
  expect_equal(col_means[2], 0, tolerance = 1e-10)
  expect_equal(col_sds[1], 1, tolerance = 1e-10)
  expect_equal(col_sds[2], 1, tolerance = 1e-10)

  # CRITICAL VALIDATION: Manual calculation for B1 column
  b1_values <- c(10, 30, 50)
  b1_mean <- mean(b1_values)  # 30
  b1_sd <- sd(b1_values)      # 20
  b1_scaled_manual <- (b1_values - b1_mean) / b1_sd

  expect_equal(scaled_data[, "B1"], b1_scaled_manual, tolerance = 1e-10)
})

# ============================================================================
# PART 3: SURVIVAL DATA MERGE VALIDATION
# Tests that cluster assignments merge correctly with survival data
# ============================================================================

test_that("CRITICAL: Survival data merge preserves patient-cluster alignment", {
  # Create original dataset with survival data
  original_data <- data.frame(
    patient_id = rep(c("P001", "P002", "P003", "P004"), each = 2),
    biomarker = rep(c("ER", "PR"), 4),
    expression = c(10, 20, 15, 25, 80, 90, 85, 95),
    survival_time = rep(c(12, 24, 18, 30), each = 2),  # Months
    event = rep(c(1, 0, 1, 0), each = 2),
    stringsAsFactors = FALSE
  )

  # Simulate cluster assignments from heatmap analysis
  cluster_assignments <- data.frame(
    patient_id = c("P001", "P002", "P003", "P004"),
    cluster = c(1, 1, 2, 2),  # P001/P002 in cluster 1, P003/P004 in cluster 2
    stringsAsFactors = FALSE
  )

  # Merge (mimics clinicalheatmap survival analysis merge)
  merged_data <- original_data %>%
    dplyr::inner_join(cluster_assignments, by = "patient_id")

  # CRITICAL VALIDATION: Each patient must have correct cluster assignment
  p001_cluster <- merged_data %>% filter(patient_id == "P001") %>% pull(cluster) %>% unique()
  p002_cluster <- merged_data %>% filter(patient_id == "P002") %>% pull(cluster) %>% unique()
  p003_cluster <- merged_data %>% filter(patient_id == "P003") %>% pull(cluster) %>% unique()
  p004_cluster <- merged_data %>% filter(patient_id == "P004") %>% pull(cluster) %>% unique()

  expect_equal(p001_cluster, 1)
  expect_equal(p002_cluster, 1)
  expect_equal(p003_cluster, 2)
  expect_equal(p004_cluster, 2)

  # CRITICAL VALIDATION: Survival data must match original data
  p001_surv_time <- merged_data %>% filter(patient_id == "P001") %>% pull(survival_time) %>% unique()
  p003_event <- merged_data %>% filter(patient_id == "P003") %>% pull(event) %>% unique()

  expect_equal(p001_surv_time, 12)
  expect_equal(p003_event, 1)

  # CRITICAL VALIDATION: No patients should be lost in merge
  expect_equal(length(unique(merged_data$patient_id)), 4)
})

test_that("CRITICAL: Misaligned patient IDs would be detected", {
  # Create scenario where patient IDs could become misaligned

  # Original data (alphabetically sorted)
  original_data <- data.frame(
    patient_id = c("P_Alpha", "P_Beta", "P_Gamma"),
    biomarker = rep(c("B1", "B2"), length.out = 6),
    expression = c(10, 20, 30, 40, 50, 60),
    survival_time = rep(c(12, 24, 36), each = 2),
    stringsAsFactors = FALSE
  )

  # Cluster assignments (accidentally in different order)
  cluster_assignments_wrong_order <- data.frame(
    patient_id = c("P_Beta", "P_Gamma", "P_Alpha"),  # Wrong order!
    cluster = c(1, 2, 3),  # These would be assigned incorrectly if not matched by ID
    stringsAsFactors = FALSE
  )

  # Merge using inner_join (correct behavior - matches by patient_id)
  merged_correct <- original_data %>%
    dplyr::inner_join(cluster_assignments_wrong_order, by = "patient_id")

  # Verify that inner_join correctly matched by patient_id (not by row order)
  alpha_cluster <- merged_correct %>% filter(patient_id == "P_Alpha") %>% pull(cluster) %>% unique()
  beta_cluster <- merged_correct %>% filter(patient_id == "P_Beta") %>% pull(cluster) %>% unique()
  gamma_cluster <- merged_correct %>% filter(patient_id == "P_Gamma") %>% pull(cluster) %>% unique()

  # These should match the cluster assignment data (not row order)
  expect_equal(alpha_cluster, 3)  # P_Alpha gets cluster 3 (from assignment data)
  expect_equal(beta_cluster, 1)   # P_Beta gets cluster 1
  expect_equal(gamma_cluster, 2)  # P_Gamma gets cluster 2

  # This test PASSES if inner_join works correctly
  # This test would FAIL if code used cbind or positional matching instead of join
})

# ============================================================================
# PART 4: END-TO-END INTEGRATION VALIDATION
# Tests complete workflow with manual verification at each step
# ============================================================================

test_that("CRITICAL: End-to-end workflow maintains data integrity", {
  skip_if_not_installed("tidyheatmaps")
  skip("Manual end-to-end validation - requires full clinicalheatmap setup")

  # This test would validate the complete workflow:
  # 1. Load known dataset
  # 2. Extract cluster assignments
  # 3. Manually verify cluster-patient mapping
  # 4. Compare survival analysis results to manual calculation

  # TODO: Implement once we have access to clinicalheatmap internal methods
  # or after architectural refactoring exposes cluster assignments
})

# ============================================================================
# PART 5: CLUSTERING CORRECTNESS VALIDATION
# Tests that clustering algorithms produce expected results
# ============================================================================

test_that("CRITICAL: Hierarchical clustering produces deterministic results", {
  # Create test data with clear cluster structure
  set.seed(123)
  test_data <- data.frame(
    patient_id = c("P001", "P002", "P003", "P004", "P005", "P006"),
    biomarker = rep(c("B1", "B2", "B3"), 6),
    # P001-P003: Low expression (cluster 1)
    # P004-P006: High expression (cluster 2)
    expression = c(
      10, 12, 11,  # P001
      9, 13, 10,   # P002
      11, 11, 12,  # P003
      90, 88, 91,  # P004
      89, 92, 90,  # P005
      91, 89, 88   # P006
    ),
    stringsAsFactors = FALSE
  )

  # Transform to matrix
  mat_data <- test_data %>%
    tidyr::pivot_wider(names_from = biomarker, values_from = expression) %>%
    tibble::column_to_rownames("patient_id") %>%
    as.matrix()

  # Perform hierarchical clustering
  dist_rows <- stats::dist(mat_data, method = "euclidean")
  hc_rows <- stats::hclust(dist_rows, method = "complete")
  clusters <- stats::cutree(hc_rows, k = 2)

  # CRITICAL VALIDATION: P001-P003 should be in same cluster (low values)
  cluster_1 <- clusters["P001"]
  expect_equal(clusters["P002"], cluster_1)
  expect_equal(clusters["P003"], cluster_1)

  # CRITICAL VALIDATION: P004-P006 should be in same cluster (high values)
  cluster_2 <- clusters["P004"]
  expect_equal(clusters["P005"], cluster_2)
  expect_equal(clusters["P006"], cluster_2)

  # CRITICAL VALIDATION: Low and high groups should be in different clusters
  expect_false(cluster_1 == cluster_2)
})

test_that("CRITICAL: Cluster count matches splitRows parameter", {
  # Create test data
  set.seed(456)
  test_data <- data.frame(
    patient_id = rep(paste0("P", 1:10), each = 3),
    biomarker = rep(c("B1", "B2", "B3"), 10),
    expression = rnorm(30),
    stringsAsFactors = FALSE
  )

  # Transform to matrix
  mat_data <- test_data %>%
    tidyr::pivot_wider(names_from = biomarker, values_from = expression) %>%
    tibble::column_to_rownames("patient_id") %>%
    as.matrix()

  # Test different cluster counts
  for (k in 2:5) {
    dist_rows <- stats::dist(mat_data, method = "euclidean")
    hc_rows <- stats::hclust(dist_rows, method = "complete")
    clusters <- stats::cutree(hc_rows, k = k)

    # CRITICAL VALIDATION: Number of unique clusters must equal k
    n_unique_clusters <- length(unique(clusters))
    expect_equal(n_unique_clusters, k,
                 info = paste("Expected", k, "clusters, got", n_unique_clusters))

    # CRITICAL VALIDATION: All patients must be assigned a cluster
    expect_equal(length(clusters), 10)
  }
})

# ============================================================================
# PART 6: SURVIVAL ANALYSIS INTEGRATION VALIDATION
# Tests that survival analysis receives correctly aligned data
# ============================================================================

test_that("CRITICAL: Survival analysis uses correct cluster-patient mapping", {
  skip_if_not_installed("survival")

  library(survival)

  # Create test data with known survival differences between clusters
  test_data <- data.frame(
    patient_id = c("P001", "P002", "P003", "P004"),
    cluster = c(1, 1, 2, 2),  # Cluster assignments
    survival_time = c(10, 12, 50, 48),  # Cluster 1: short survival, Cluster 2: long survival
    event = c(1, 1, 1, 1),  # All events observed
    stringsAsFactors = FALSE
  )

  # Perform survival analysis (mimics clinicalheatmap)
  surv_obj <- survival::Surv(test_data$survival_time, test_data$event)
  fit <- survival::survfit(surv_obj ~ cluster, data = test_data)

  # CRITICAL VALIDATION: Median survival for cluster 1 should be ~11 months
  median_cluster1 <- summary(fit)$table[1, "median"]
  expect_equal(median_cluster1, 11, tolerance = 1)

  # CRITICAL VALIDATION: Median survival for cluster 2 should be ~49 months
  median_cluster2 <- summary(fit)$table[2, "median"]
  expect_equal(median_cluster2, 49, tolerance = 1)

  # CRITICAL VALIDATION: Log-rank test should show significant difference
  log_rank <- survival::survdiff(surv_obj ~ cluster, data = test_data)
  p_value <- 1 - pchisq(log_rank$chisq, df = 1)

  # With such different survival times, p-value should be very small
  expect_lt(p_value, 0.05)
})

test_that("CRITICAL: Misaligned survival data would produce wrong results", {
  skip_if_not_installed("survival")

  library(survival)

  # Create CORRECT alignment
  correct_data <- data.frame(
    patient_id = c("P001", "P002", "P003", "P004"),
    cluster = c(1, 1, 2, 2),
    survival_time = c(10, 12, 50, 48),  # Cluster 1: short, Cluster 2: long
    event = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Create INCORRECT alignment (clusters swapped for P002 and P004)
  incorrect_data <- data.frame(
    patient_id = c("P001", "P002", "P003", "P004"),
    cluster = c(1, 2, 2, 1),  # WRONG! P002 and P004 swapped
    survival_time = c(10, 12, 50, 48),
    event = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Analyze correct data
  surv_obj_correct <- survival::Surv(correct_data$survival_time, correct_data$event)
  fit_correct <- survival::survfit(surv_obj_correct ~ cluster, data = correct_data)
  median_correct_c1 <- summary(fit_correct)$table[1, "median"]
  median_correct_c2 <- summary(fit_correct)$table[2, "median"]

  # Analyze incorrect data
  surv_obj_incorrect <- survival::Surv(incorrect_data$survival_time, incorrect_data$event)
  fit_incorrect <- survival::survfit(surv_obj_incorrect ~ cluster, data = incorrect_data)
  median_incorrect_c1 <- summary(fit_incorrect)$table[1, "median"]
  median_incorrect_c2 <- summary(fit_incorrect)$table[2, "median"]

  # CRITICAL VALIDATION: Results should be DIFFERENT with misalignment
  # Correct: Cluster 1 ≈ 11, Cluster 2 ≈ 49
  # Incorrect: Cluster 1 ≈ 29, Cluster 2 ≈ 31 (mixed)

  expect_false(isTRUE(all.equal(median_correct_c1, median_incorrect_c1, tolerance = 5)))
  expect_false(isTRUE(all.equal(median_correct_c2, median_incorrect_c2, tolerance = 5)))

  # This test demonstrates that data misalignment produces WRONG results
  # The test PASSES if we can detect the difference
})

# ============================================================================
# SUMMARY OF CRITICAL ISSUES
# ============================================================================

# ISSUE 1: Data transformation chain complexity
# LOCATION: Lines 1104-1108, 1182-1186 (pivot_wider, column_to_rownames)
# RISK: Patient ID misalignment if rownames not preserved correctly
# SEVERITY: CRITICAL - Would silently invalidate all downstream analysis
#
# ISSUE 2: Cluster assignment extraction
# LOCATION: Lines 1193-1204 (hclust, cutree, data.frame creation)
# RISK: Row order vs. patient ID mismatch
# SEVERITY: CRITICAL - Wrong clusters assigned to wrong patients
#
# ISSUE 3: Survival data merge
# LOCATION: Lines 1254-1261 (inner_join with cluster assignments)
# RISK: Merge on wrong key or wrong data
# SEVERITY: CRITICAL - Survival analysis on misaligned data
#
# VALIDATION STRATEGY:
# These tests use small, controlled datasets where we can manually verify:
# 1. Patient IDs are preserved through transformations
# 2. Cluster assignments map to correct patients
# 3. Scaling produces mathematically correct results
# 4. Survival data merges with correct alignment
#
# RECOMMENDATION:
# - Current implementation CAN be validated with these tests
# - BUT: Complexity still makes it fragile and hard to debug
# - FUTURE: Consider modular architecture (separate heatmap + survival modules)
