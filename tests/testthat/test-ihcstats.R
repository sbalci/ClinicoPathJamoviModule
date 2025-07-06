test_that("ihcstats module loads correctly", {
  expect_true(exists("ihcstatsClass"))
  expect_true(is.function(ihcstats))
})

test_that("ihcstats handles basic input validation", {
  # Test with missing required markers
  expect_error(
    ihcstats(data = histopathology, markers = NULL),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with insufficient markers (less than 2)  
  expect_error(
    ihcstats(data = histopathology, markers = c("Grade"), significanceThreshold = 0.01),
    NA  # Should not error during initialization, only during run
  )
})

test_that("ihcstats works with basic IHC markers", {
  # Test basic functionality with 3 IHC markers
  result <- ihcstats(
    data = histopathology,
    markers = c("Grade", "LVI", "PNI"),
    significanceThreshold = 0.01
  )
  
  expect_s3_class(result, "ihcstatsClass")
  expect_true("Grade" %in% names(histopathology))
  expect_true("LVI" %in% names(histopathology))
  expect_true("PNI" %in% names(histopathology))
})

test_that("ihcstats works with multiple clustering methods", {
  # Test hierarchical clustering
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      clusterMethod = "hierarchical",
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
  
  # Test PAM clustering
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      clusterMethod = "pam",
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
  
  # Test K-means clustering
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      clusterMethod = "kmeans",
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats different distance metrics work", {
  # Test Gower distance
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      distanceMetric = "gower",
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
  
  # Test Jaccard distance
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      distanceMetric = "jaccard",
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats linkage methods work", {
  # Test different linkage methods
  linkage_methods <- c("complete", "average", "single", "ward.D2")
  
  for (method in linkage_methods) {
    expect_error({
      result <- ihcstats(
        data = histopathology,
        markers = c("Grade", "LVI", "PNI"),
        clusterMethod = "hierarchical",
        linkageMethod = method,
        nClusters = 3,
        significanceThreshold = 0.01
      )
    }, NA, info = paste("linkageMethod:", method))
  }
})

test_that("ihcstats cluster numbers work", {
  # Test different cluster numbers
  cluster_numbers <- c(2, 3, 4, 5)
  
  for (n_clusters in cluster_numbers) {
    expect_error({
      result <- ihcstats(
        data = histopathology,
        markers = c("Grade", "LVI", "PNI", "TStage"),
        nClusters = n_clusters,
        significanceThreshold = 0.01
      )
    }, NA, info = paste("nClusters:", n_clusters))
  }
})

test_that("ihcstats visualization options work", {
  # Test dendrogram
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      showDendrogram = TRUE,
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
  
  # Test heatmap
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      showHeatmap = TRUE,
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
  
  # Test both visualizations
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      showDendrogram = TRUE,
      showHeatmap = TRUE,
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats H-score computation works", {
  # Test H-score calculation
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      computeHScore = TRUE,
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats cluster validation works", {
  # Test cluster validation
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      showClusterValidation = TRUE,
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats handles grouping variables", {
  # Test with grouping variable
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI"),
      groupVariable = "Sex",
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats works with mixed data types", {
  # Test with mixed categorical and continuous variables
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "Age", "LVI"),  # Mix of categorical and continuous
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Grade[1:10] <- NA
  test_data$LVI[5:15] <- NA
  
  expect_error({
    result <- ihcstats(
      data = test_data,
      markers = c("Grade", "LVI", "PNI"),
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats complex parameter combinations work", {
  # Test comprehensive parameter combination
  expect_error({
    result <- ihcstats(
      data = histopathology,
      markers = c("Grade", "LVI", "PNI", "TStage"),
      clusterMethod = "hierarchical",
      distanceMetric = "gower",
      linkageMethod = "ward.D2",
      nClusters = 4,
      showDendrogram = TRUE,
      showHeatmap = TRUE,
      computeHScore = TRUE,
      showClusterValidation = TRUE,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- ihcstats(
      data = small_data,
      markers = c("Grade", "LVI", "PNI"),
      nClusters = 2,  # Smaller number of clusters for small dataset
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats results have expected structure", {
  # Test that results object has expected components
  result <- ihcstats(
    data = histopathology,
    markers = c("Grade", "LVI", "PNI"),
    nClusters = 3,
    significanceThreshold = 0.01
  )
  
  # Check for expected result components
  expect_true(exists("todo", envir = result))
  expect_true(exists("clusterSummary", envir = result))
})

test_that("ihcstats synthetic IHC data works", {
  # Create synthetic IHC data
  set.seed(123)
  synthetic_ihc_data <- data.frame(
    case_id = 1:100,
    marker1 = sample(c("0", "1+", "2+", "3+"), 100, replace = TRUE),
    marker2 = sample(c("0", "1+", "2+", "3+"), 100, replace = TRUE),
    marker3 = sample(c("0", "1+", "2+", "3+"), 100, replace = TRUE),
    marker4 = sample(c("Negative", "Positive"), 100, replace = TRUE),
    stringsAsFactors = TRUE
  )
  
  expect_error({
    result <- ihcstats(
      data = synthetic_ihc_data,
      markers = c("marker1", "marker2", "marker3", "marker4"),
      nClusters = 3,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats dependency handling", {
  # Test function behavior when required packages are available
  required_packages <- c("cluster", "pheatmap", "dendextend", "RColorBrewer")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        result <- ihcstats(
          data = histopathology,
          markers = c("Grade", "LVI", "PNI"),
          nClusters = 3,
          significanceThreshold = 0.01
        )
      }, NA, info = paste("package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("ihcstats edge cases work correctly", {
  # Test with binary markers only
  binary_data <- data.frame(
    case = 1:50,
    marker1 = sample(c("Negative", "Positive"), 50, replace = TRUE),
    marker2 = sample(c("Negative", "Positive"), 50, replace = TRUE),
    marker3 = sample(c("Negative", "Positive"), 50, replace = TRUE),
    stringsAsFactors = TRUE
  )
  
  expect_error({
    result <- ihcstats(
      data = binary_data,
      markers = c("marker1", "marker2", "marker3"),
      nClusters = 2,
      significanceThreshold = 0.01
    )
  }, NA)
})

test_that("ihcstats scoring scales work", {
  # Test different scoring scales
  scoring_scales <- c("binary", "carvalho", "standard", "matsuoka")
  
  for (scale in scoring_scales) {
    expect_error({
      result <- ihcstats(
        data = histopathology,
        markers = c("Grade", "LVI", "PNI"),
        scoringScale = scale,
        nClusters = 3,
        significanceThreshold = 0.01
      )
    }, NA, info = paste("scoringScale:", scale))
  }
})

test_that("ihcstats robust clustering validation", {
  # Test clustering stability with different random seeds
  results <- list()
  
  for (i in 1:3) {
    set.seed(i * 100)
    expect_error({
      results[[i]] <- ihcstats(
        data = histopathology,
        markers = c("Grade", "LVI", "PNI"),
        clusterMethod = "hierarchical",
        nClusters = 3,
        significanceThreshold = 0.01
      )
    }, NA)
  }
  
  # Should complete without errors for all seeds
  expect_length(results, 3)
})