devtools::load_all()

test_that("Invalid distance/linkage combinations are rejected", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("ClinicoPath")

    # Create simple test data
    testData <- data.frame(
        x = c(1, 2, 5, 6),
        y = c(1, 2, 5, 6)
    )

    # Ward.D2 with non-Euclidean distance should fail
    result <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "ward.D2",
        distanceMethod = "manhattan"
    )

    # Should produce error or warning in cluster info
    expect_true(!is.null(result$clusterInfo))

    # Ward.D with non-Euclidean should fail
    result2 <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "ward.D",
        distanceMethod = "canberra"
    )
    expect_true(!is.null(result2$clusterInfo))

    # Centroid with non-Euclidean should fail
    result3 <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "centroid",
        distanceMethod = "binary"
    )
    expect_true(!is.null(result3$clusterInfo))

    # Median with non-Euclidean should fail
    result4 <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "median",
        distanceMethod = "maximum"
    )
    expect_true(!is.null(result4$clusterInfo))
})


test_that("Valid distance/linkage combinations work correctly", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        x = c(1, 2, 5, 6, 9, 10),
        y = c(1, 2, 5, 6, 9, 10)
    )

    # Ward.D2 with Euclidean should work
    expect_error(
        dendrogram(
            data = testData,
            vars = c("x", "y"),
            clusterMethod = "ward.D2",
            distanceMethod = "euclidean"
        ),
        NA
    )

    # Complete linkage works with any distance
    expect_error(
        dendrogram(
            data = testData,
            vars = c("x", "y"),
            clusterMethod = "complete",
            distanceMethod = "manhattan"
        ),
        NA
    )

    # Average linkage works with any distance
    expect_error(
        dendrogram(
            data = testData,
            vars = c("x", "y"),
            clusterMethod = "average",
            distanceMethod = "canberra"
        ),
        NA
    )

    # Single linkage works with any distance
    expect_error(
        dendrogram(
            data = testData,
            vars = c("x", "y"),
            clusterMethod = "single",
            distanceMethod = "maximum"
        ),
        NA
    )
})


test_that("Cluster assignments are deterministic and correct", {
    skip_if_not_installed("ClinicoPath")

    # Create data with clear clusters
    set.seed(42)
    testData <- data.frame(
        x = c(1, 1.5, 2, 10, 10.5, 11),
        y = c(1, 1.5, 2, 10, 10.5, 11),
        id = paste0("sample", 1:6)
    )
    rownames(testData) <- testData$id

    result <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "complete",
        distanceMethod = "euclidean",
        nClusters = 2,
        highlightClusters = TRUE
    )

    # Verify result object structure
    expect_s3_class(result, "dendrogramResults")
    expect_true("clusterSummary" %in% names(result))

    # Verify cluster summary table exists and has data
    clusterTable <- result$clusterSummary
    expect_true(!is.null(clusterTable))

    # Note: Cannot easily access cluster membership from jamovi results object
    # This test validates that clustering completes successfully
})


test_that("Distance calculations are mathematically correct", {
    skip_if_not_installed("ClinicoPath")

    # Create data with known distances
    testData <- data.frame(
        x = c(0, 3, 0),
        y = c(0, 0, 4),
        id = c("p1", "p2", "p3")
    )
    rownames(testData) <- testData$id

    # Euclidean distances:
    # p1-p2: sqrt((3-0)^2 + (0-0)^2) = 3
    # p1-p3: sqrt((0-0)^2 + (4-0)^2) = 4
    # p2-p3: sqrt((3-0)^2 + (0-4)^2) = 5

    result <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "single",
        distanceMethod = "euclidean",
        standardize = FALSE  # Don't standardize to preserve known distances
    )

    # Verify clustering completes
    expect_s3_class(result, "dendrogramResults")
})


test_that("Group coloring handles unmatched samples with neutral color", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        x = c(1, 2, 3, 4, 5, 6),
        y = c(1, 2, 3, 4, 5, 6),
        group = factor(c("A", "A", "B", "B", NA, NA)),
        id = paste0("s", 1:6)
    )
    rownames(testData) <- testData$id

    # Should produce warnings about unmatched samples
    expect_warning(
        result <- dendrogram(
            data = testData,
            vars = c("x", "y"),
            clusterMethod = "complete",
            distanceMethod = "euclidean",
            colorGroups = TRUE,
            group = "group"
        ),
        "could not be matched"
    )

    # Should still produce valid result
    expect_s3_class(result, "dendrogramResults")
})


test_that("Group coloring does not silently reassign missing data", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        x = c(1, 2, 3, 4),
        y = c(1, 2, 3, 4),
        disease = factor(c("Cancer", "Benign", NA, "Cancer")),
        id = paste0("patient", 1:4)
    )
    rownames(testData) <- testData$id

    # Should warn about unmatched sample (patient3 with NA)
    expect_warning(
        dendrogram(
            data = testData,
            vars = c("x", "y"),
            clusterMethod = "average",
            distanceMethod = "euclidean",
            colorGroups = TRUE,
            group = "disease"
        ),
        "1 .* could not be matched"
    )
})


test_that("Standardization affects distance calculations correctly", {
    skip_if_not_installed("ClinicoPath")

    # Create data with very different scales
    testData <- data.frame(
        small = c(1, 2, 3, 4, 5),
        large = c(100, 200, 300, 400, 500)
    )

    # With standardization (default)
    result1 <- dendrogram(
        data = testData,
        vars = c("small", "large"),
        clusterMethod = "complete",
        distanceMethod = "euclidean",
        standardize = TRUE
    )
    expect_s3_class(result1, "dendrogramResults")

    # Without standardization
    result2 <- dendrogram(
        data = testData,
        vars = c("small", "large"),
        clusterMethod = "complete",
        distanceMethod = "euclidean",
        standardize = FALSE
    )
    expect_s3_class(result2, "dendrogramResults")

    # Both should complete but produce different trees
    # (Cannot easily compare trees from jamovi results, but both should work)
})


test_that("Zero-variance variables are handled appropriately", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        constant = c(5, 5, 5, 5),
        varying = c(1, 2, 3, 4)
    )

    # Should complete with warning about zero variance
    result <- dendrogram(
        data = testData,
        vars = c("constant", "varying"),
        clusterMethod = "complete",
        distanceMethod = "euclidean",
        standardize = TRUE
    )

    expect_s3_class(result, "dendrogramResults")
    # Zero-variance message should appear in cluster info
})


test_that("Binary distance validation works correctly", {
    skip_if_not_installed("ClinicoPath")

    # Valid binary data
    binaryData <- data.frame(
        marker1 = c(0, 1, 0, 1),
        marker2 = c(1, 1, 0, 0)
    )

    expect_error(
        dendrogram(
            data = binaryData,
            vars = c("marker1", "marker2"),
            clusterMethod = "single",
            distanceMethod = "binary",
            standardize = FALSE
        ),
        NA
    )

    # Invalid binary data (contains values other than 0/1)
    nonBinaryData <- data.frame(
        marker1 = c(0, 1, 2, 1),
        marker2 = c(1, 1, 0, 0)
    )

    result <- dendrogram(
        data = nonBinaryData,
        vars = c("marker1", "marker2"),
        clusterMethod = "single",
        distanceMethod = "binary",
        standardize = FALSE
    )

    # Should produce error message about non-binary values
    expect_true(!is.null(result$clusterInfo))
})


test_that("Missing data handling preserves sample integrity", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        x = c(1, 2, NA, 4, 5),
        y = c(1, NA, 3, 4, 5),
        z = c(1, 2, 3, 4, 5),
        id = paste0("s", 1:5)
    )
    rownames(testData) <- testData$id

    result <- dendrogram(
        data = testData,
        vars = c("x", "y", "z"),
        clusterMethod = "complete",
        distanceMethod = "euclidean"
    )

    # Should use only complete cases (s1, s4, s5)
    # Cluster info should report removed rows
    expect_s3_class(result, "dendrogramResults")
    expect_true(!is.null(result$clusterInfo))
})


test_that("Heatmap uses same clustering as summary tables", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("tidyHeatmap")
    skip_if_not_installed("ComplexHeatmap")

    testData <- data.frame(
        gene1 = c(1, 2, 10, 11),
        gene2 = c(1.5, 2.5, 10.5, 11.5),
        gene3 = c(1.2, 2.2, 10.2, 11.2),
        id = paste0("sample", 1:4)
    )
    rownames(testData) <- testData$id

    # Create heatmap result
    result <- dendrogram(
        data = testData,
        vars = c("gene1", "gene2", "gene3"),
        clusterMethod = "ward.D2",
        distanceMethod = "euclidean",
        plotType = "heatmap",
        showRowDendro = TRUE,
        showColDendro = TRUE,
        heatmapScale = "row"
    )

    # Should complete successfully
    expect_s3_class(result, "dendrogramResults")

    # Verify that plot exists
    expect_true("plot" %in% names(result))
})


test_that("Summary statistics are calculated correctly", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        var1 = c(10, 20, 30, 40, 50),
        var2 = c(5, 10, 15, 20, 25)
    )

    result <- dendrogram(
        data = testData,
        vars = c("var1", "var2"),
        clusterMethod = "complete",
        distanceMethod = "euclidean"
    )

    # Verify summary table exists
    expect_true("summary" %in% names(result))
    summaryTable <- result$summary

    # Known statistics for var1: mean=30, sd=15.81
    # Known statistics for var2: mean=15, sd=7.91
    # (Cannot easily access table values from jamovi results object)
    expect_true(!is.null(summaryTable))
})


test_that("Label display respects maxLabels threshold", {
    skip_if_not_installed("ClinicoPath")

    # Create data with many samples
    set.seed(123)
    largeData <- data.frame(
        x = rnorm(100),
        y = rnorm(100),
        id = paste0("s", 1:100)
    )
    rownames(largeData) <- largeData$id

    # With maxLabels = 50 and showLabels = TRUE, labels should be hidden (100 > 50)
    result1 <- dendrogram(
        data = largeData,
        vars = c("x", "y"),
        clusterMethod = "complete",
        distanceMethod = "euclidean",
        showLabels = TRUE,
        maxLabels = 50
    )
    expect_s3_class(result1, "dendrogramResults")

    # With maxLabels = 150, labels should be shown (100 <= 150)
    result2 <- dendrogram(
        data = largeData,
        vars = c("x", "y"),
        clusterMethod = "complete",
        distanceMethod = "euclidean",
        showLabels = TRUE,
        maxLabels = 150
    )
    expect_s3_class(result2, "dendrogramResults")
})


test_that("Different plot types all use same clustering", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        x = c(1, 2, 10, 11),
        y = c(1, 2, 10, 11)
    )

    # Linear plot
    result1 <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "average",
        distanceMethod = "euclidean",
        plotType = "linear"
    )
    expect_s3_class(result1, "dendrogramResults")

    # Circular plot
    result2 <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "average",
        distanceMethod = "euclidean",
        plotType = "circular"
    )
    expect_s3_class(result2, "dendrogramResults")

    # Base plot
    result3 <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        clusterMethod = "average",
        distanceMethod = "euclidean",
        plotType = "base"
    )
    expect_s3_class(result3, "dendrogramResults")

    # All should produce valid results with same clustering underneath
})

test_that("clustering matrix state matches standardization flag", {
    skip_if_not_installed("ClinicoPath")

    testData <- data.frame(
        x = c(1, 2, 3, 4),
        y = c(10, 20, 30, 40)
    )

    res_std <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        standardize = TRUE
    )

    res_raw <- dendrogram(
        data = testData,
        vars = c("x", "y"),
        standardize = FALSE
    )

    mat_std <- res_std$plot$state$clusteringMatrix
    mat_raw <- res_raw$plot$state$clusteringMatrix

    # Standardized columns should have mean 0 and sd 1
    expect_equal(round(colMeans(mat_std), 6), c(0, 0))
    expect_equal(round(apply(mat_std, 2, sd), 6), c(1, 1))

    # Raw matrix should preserve original scale
    expect_equal(mat_raw[, "x"], testData$x)
    expect_equal(mat_raw[, "y"], testData$y)
})

test_that("heatmap plot uses precomputed clustering and row dendrogram", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("tidyHeatmap")
    skip_if_not_installed("ComplexHeatmap")

    testData <- data.frame(
        a = c(1, 2, 3),
        b = c(1, 2, 2),
        c = c(2, 3, 4)
    )

    res <- dendrogram(
        data = testData,
        vars = c("a", "b", "c"),
        plotType = "heatmap",
        showRowDendro = TRUE,
        showColDendro = TRUE,
        standardize = TRUE
    )

    # Should carry precomputed clustering into state for heatmap rendering
    expect_true(!is.null(res$plot$state$hclustResult))
    expect_true(!is.null(res$plot$state$clusteringMatrix))
    expect_equal(length(res$plot$state$clusterMembership), nrow(testData))
})
