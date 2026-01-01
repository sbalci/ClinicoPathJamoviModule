
# Source the function files
source("../../R/dendrogram.h.R")
source("../../R/dendrogram.b.R")

test_that("dendrogram works with basic linear plot", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    set.seed(123)
    data <- data.frame(
        Gene1 = rnorm(50),
        Gene2 = rnorm(50),
        Gene3 = rnorm(50)
    )

    options <- dendrogramOptions$new(
        vars = c("Gene1", "Gene2", "Gene3"),
        clusterMethod = "ward.D2",
        distanceMethod = "euclidean",
        plotType = "linear",
        highlightClusters = TRUE,
        nClusters = 3
    )

    analysis <- dendrogramClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check that plot state is populated
    expect_false(is.null(analysis$results$plot$state))
    expect_equal(analysis$results$plot$state$plotType, "linear")
})

test_that("dendrogram handles grouping and circular plot", {
    set.seed(123)
    data <- data.frame(
        Gene1 = rnorm(50),
        Gene2 = rnorm(50),
        Group = factor(sample(c("A", "B"), 50, replace = TRUE))
    )

    options <- dendrogramOptions$new(
        vars = c("Gene1", "Gene2"),
        clusterMethod = "complete",
        distanceMethod = "manhattan",
        plotType = "circular",
        colorGroups = TRUE,
        group = "Group"
    )

    analysis <- dendrogramClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    expect_equal(analysis$results$plot$state$plotType, "circular")
    expect_true(analysis$results$plot$state$colorGroups)
})

test_that("dendrogram validates binary distance", {
    data <- data.frame(
        Gene1 = c(1, 2, 3),
        Gene2 = c(4, 5, 6)
    )

    options <- dendrogramOptions$new(
        vars = c("Gene1", "Gene2"),
        distanceMethod = "binary"
    )

    analysis <- dendrogramClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Should have an error message in clusterInfo
    expect_match(analysis$results$clusterInfo$content, "Binary distance requires variables coded as 0/1 only")
})

test_that("dendrogram handles heatmap plot", {
    set.seed(123)
    data <- data.frame(
        Gene1 = rnorm(20),
        Gene2 = rnorm(20),
        Gene3 = rnorm(20)
    )

    options <- dendrogramOptions$new(
        vars = c("Gene1", "Gene2", "Gene3"),
        plotType = "heatmap",
        heatmapScale = "row"
    )

    analysis <- dendrogramClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    expect_equal(analysis$results$plot$state$plotType, "heatmap")
})

test_that("dendrogram handles missing data", {
    set.seed(123)
    data <- data.frame(
        Gene1 = c(rnorm(45), rep(NA, 5)),
        Gene2 = rnorm(50)
    )

    options <- dendrogramOptions$new(
        vars = c("Gene1", "Gene2"),
        clusterMethod = "ward.D2"
    )

    analysis <- dendrogramClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check that rows were removed
    expect_match(analysis$results$clusterInfo$content, "Rows removed due to missing values")
})
