context("test-dendrogram")

# Load required library
library(ClinicoPath)

test_that("dendrogram function exists and can be called", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic functionality with histopathology data
  expect_error(
    dendrogram(
      data = histopathology,
      vars = c("Age", "OverallTime", "MeasurementA"),
      clusterMethod = "ward.D2",
      distanceMethod = "euclidean",
      showLabels = TRUE,
      colorGroups = FALSE
    ),
    NA
  )
  
})

test_that("dendrogram works with different cluster methods", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test different clustering methods
  cluster_methods <- c("complete", "single", "average", "ward.D2", "ward.D", "mcquitty", "median", "centroid")
  
  for (method in cluster_methods) {
    test_that(paste("dendrogram works with", method, "cluster method"), {
      expect_error(
        dendrogram(
          data = histopathology,
          vars = c("Age", "OverallTime"),
          clusterMethod = method,
          distanceMethod = "euclidean",
          showLabels = FALSE,
          colorGroups = FALSE
        ),
        NA
      )
    })
  }
  
})

test_that("dendrogram works with different distance methods", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test different distance methods
  distance_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  
  for (method in distance_methods) {
    test_that(paste("dendrogram works with", method, "distance method"), {
      expect_error(
        dendrogram(
          data = histopathology,
          vars = c("Age", "OverallTime"),
          clusterMethod = "ward.D2",
          distanceMethod = method,
          showLabels = FALSE,
          colorGroups = FALSE
        ),
        NA
      )
    })
  }
  
})

test_that("dendrogram works with different variable combinations", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test with different variable combinations
  var_combinations <- list(
    c("Age"),
    c("Age", "OverallTime"),
    c("Age", "OverallTime", "MeasurementA"),
    c("Age", "OverallTime", "MeasurementA", "MeasurementB")
  )
  
  for (vars in var_combinations) {
    test_that(paste("dendrogram works with variables:", paste(vars, collapse = ", ")), {
      expect_error(
        dendrogram(
          data = histopathology,
          vars = vars,
          clusterMethod = "ward.D2",
          distanceMethod = "euclidean",
          showLabels = FALSE,
          colorGroups = FALSE
        ),
        NA
      )
    })
  }
  
})

test_that("dendrogram works with grouping variables", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test with different grouping variables
  group_vars <- c("Group", "Sex", "Grade_Level")
  
  for (grp in group_vars) {
    if (grp %in% names(histopathology)) {
      test_that(paste("dendrogram works with", grp, "as grouping variable"), {
        expect_error(
          dendrogram(
            data = histopathology,
            vars = c("Age", "OverallTime"),
            clusterMethod = "ward.D2",
            distanceMethod = "euclidean",
            showLabels = FALSE,
            colorGroups = TRUE,
            group = grp
          ),
          NA
        )
      })
    }
  }
  
})

test_that("dendrogram handles error conditions appropriately", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test with no variables selected
  expect_error(
    dendrogram(
      data = histopathology,
      vars = character(0),
      clusterMethod = "ward.D2",
      distanceMethod = "euclidean",
      showLabels = FALSE,
      colorGroups = FALSE
    ),
    NA  # This should not error but show a message about no variables
  )
  
  # Test with non-existent variables
  expect_error(
    dendrogram(
      data = histopathology,
      vars = c("NonExistentVar1", "NonExistentVar2"),
      clusterMethod = "ward.D2",
      distanceMethod = "euclidean",
      showLabels = FALSE,
      colorGroups = FALSE
    )
  )
  
  # Test with empty data
  expect_error(
    dendrogram(
      data = data.frame(),
      vars = c("Age"),
      clusterMethod = "ward.D2",
      distanceMethod = "euclidean",
      showLabels = FALSE,
      colorGroups = FALSE
    )
  )
  
})

test_that("dendrogram return structure", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test that it returns a dendrogramResults object (jamovi results object)
  result <- dendrogram(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    clusterMethod = "ward.D2",
    distanceMethod = "euclidean",
    showLabels = TRUE,
    colorGroups = FALSE
  )
  
  expect_s3_class(result, "dendrogramResults")
  expect_true("plot" %in% names(result))
  expect_true("clusterInfo" %in% names(result))
  expect_true("summary" %in% names(result))
  
})

test_that("dendrogram with advanced visualization options", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test linear dendrogram with cluster highlighting
  expect_error(
    dendrogram(
      data = histopathology,
      vars = c("Age", "OverallTime", "MeasurementA"),
      clusterMethod = "ward.D2",
      distanceMethod = "euclidean",
      showLabels = TRUE,
      colorGroups = FALSE,
      plotType = "linear",
      edgeType = "diagonal",
      colorScheme = "viridis",
      highlightClusters = TRUE,
      nClusters = 3,
      plotHeight = 600,
      plotWidth = 800
    ),
    NA
  )
  
  # Test circular dendrogram
  expect_error(
    dendrogram(
      data = histopathology,
      vars = c("MeasurementA", "MeasurementB"),
      clusterMethod = "complete",
      distanceMethod = "manhattan",
      showLabels = FALSE,
      colorGroups = FALSE,
      plotType = "circular",
      edgeType = "link",
      colorScheme = "Set1",
      highlightClusters = TRUE,
      nClusters = 4,
      plotHeight = 600,
      plotWidth = 600
    ),
    NA
  )
  
  # Test base R dendrogram with dendextend features
  expect_error(
    dendrogram(
      data = histopathology,
      vars = c("Age", "OverallTime"),
      clusterMethod = "average",
      distanceMethod = "canberra",
      showLabels = TRUE,
      colorGroups = FALSE,
      plotType = "base",
      colorScheme = "Dark2",
      highlightClusters = TRUE,
      nClusters = 2,
      plotHeight = 500,
      plotWidth = 700
    ),
    NA
  )
  
})

test_that("dendrogram plot types and edge types", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test all plot types
  plot_types <- c("linear", "circular", "base")
  
  for (plot_type in plot_types) {
    test_that(paste("dendrogram works with", plot_type, "plot type"), {
      expect_error(
        dendrogram(
          data = histopathology,
          vars = c("Age", "OverallTime"),
          clusterMethod = "ward.D2",
          distanceMethod = "euclidean",
          showLabels = FALSE,
          colorGroups = FALSE,
          plotType = plot_type,
          edgeType = "diagonal",
          colorScheme = "default",
          highlightClusters = FALSE
        ),
        NA
      )
    })
  }
  
  # Test edge types (for ggraph plots)
  edge_types <- c("diagonal", "link", "elbow")
  
  for (edge_type in edge_types) {
    test_that(paste("dendrogram works with", edge_type, "edge type"), {
      expect_error(
        dendrogram(
          data = histopathology,
          vars = c("Age", "OverallTime"),
          clusterMethod = "ward.D2",
          distanceMethod = "euclidean",
          showLabels = FALSE,
          colorGroups = FALSE,
          plotType = "linear",
          edgeType = edge_type,
          colorScheme = "default",
          highlightClusters = FALSE
        ),
        NA
      )
    })
  }
  
})

test_that("dendrogram color schemes", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test all color schemes
  color_schemes <- c("default", "viridis", "RdYlBu", "Set1", "Dark2")
  
  for (color_scheme in color_schemes) {
    test_that(paste("dendrogram works with", color_scheme, "color scheme"), {
      expect_error(
        dendrogram(
          data = histopathology,
          vars = c("Age", "OverallTime"),
          clusterMethod = "ward.D2",
          distanceMethod = "euclidean",
          showLabels = FALSE,
          colorGroups = FALSE,
          plotType = "linear",
          edgeType = "diagonal",
          colorScheme = color_scheme,
          highlightClusters = TRUE,
          nClusters = 3
        ),
        NA
      )
    })
  }
  
})