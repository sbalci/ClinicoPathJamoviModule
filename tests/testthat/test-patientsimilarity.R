
test_that("patientsimilarity works for PCA with K-means", {
    set.seed(123)
    n <- 100
    data <- data.frame(
        age = rnorm(n, 60, 10),
        tumor_size = rnorm(n, 30, 10),
        markers = rnorm(n, 5, 2),
        grade = sample(1:3, n, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- patientsimilarityClass$new(
        options = patientsimilarityOptions$new(
            vars = c("age", "tumor_size", "markers", "grade"),
            method = "pca",
            dimensions = "2",
            performClustering = TRUE,
            clusterMethod = "kmeans",
            nClusters = 3,
            showClusterStats = TRUE
        ),
        data = data
    )
    results$run()
    
    var_table <- results$results$varianceTable$asDF
    expect_equal(nrow(var_table), 2)
    expect_true(var_table$cumulative[2] > 0)
    
    cluster_summary <- results$results$clusterSummary$asDF
    expect_equal(nrow(cluster_summary), 3)
})

test_that("patientsimilarity works for MDS with loadings", {
    set.seed(123)
    n <- 50
    data <- data.frame(
        age = rnorm(n, 60, 10),
        tumor_size = rnorm(n, 30, 10),
        markers = rnorm(n, 5, 2),
        grade = sample(1:3, n, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- patientsimilarityClass$new(
        options = patientsimilarityOptions$new(
            vars = c("age", "tumor_size", "markers", "grade"),
            method = "mds",
            dimensions = "3",
            showLoadings = TRUE
        ),
        data = data
    )
    results$run()
    
    loadings <- results$results$loadingsTable$asDF
    expect_equal(nrow(loadings), 4) # 4 variables
    expect_true("dim3" %in% names(loadings))
})

test_that("patientsimilarity works for t-SNE", {
    skip_if_not_installed("Rtsne")
    
    set.seed(123)
    n <- 100
    data <- data.frame(
        age = rnorm(n, 60, 10),
        tumor_size = rnorm(n, 30, 10),
        markers = rnorm(n, 5, 2),
        grade = sample(1:3, n, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- patientsimilarityClass$new(
        options = patientsimilarityOptions$new(
            vars = c("age", "tumor_size", "markers", "grade"),
            method = "tsne",
            dimensions = "2",
            perplexity = 10,
            iterations = 300
        ),
        data = data
    )
    results$run()
    
    # No tables for t-SNE, just check it runs without error
    expect_true(TRUE)
})

test_that("patientsimilarity works for UMAP with Survival", {
    skip_if_not_installed("umap")
    skip_if_not_installed("survival")
    
    set.seed(123)
    n <- 100
    data <- data.frame(
        age = rnorm(n, 60, 10),
        tumor_size = rnorm(n, 30, 10),
        markers = rnorm(n, 5, 2),
        grade = sample(1:3, n, replace = TRUE),
        survival_time = rexp(n, 1/24),
        status = sample(0:1, n, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- patientsimilarityClass$new(
        options = patientsimilarityOptions$new(
            vars = c("age", "tumor_size", "markers", "grade"),
            method = "umap",
            dimensions = "2",
            performClustering = TRUE,
            clusterMethod = "kmeans",
            survivalAnalysis = TRUE,
            survivalTime = "survival_time",
            survivalEvent = "status",
            survivalEventLevel = "1"
        ),
        data = data
    )
    results$run()
    
    surv_comp <- results$results$survivalComparison$asDF
    expect_equal(nrow(surv_comp), 1)
})

test_that("patientsimilarity handles colorBy and cluster outcomes", {
    set.seed(123)
    n <- 100
    data <- data.frame(
        age = rnorm(n, 60, 10),
        tumor_size = rnorm(n, 30, 10),
        markers = rnorm(n, 5, 2),
        grade = sample(1:3, n, replace = TRUE),
        group = sample(c("A", "B"), n, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    results <- patientsimilarityClass$new(
        options = patientsimilarityOptions$new(
            vars = c("age", "tumor_size", "markers", "grade"),
            method = "pca",
            dimensions = "2",
            colorBy = "group",
            performClustering = TRUE,
            clusterMethod = "kmeans",
            nClusters = 2
        ),
        data = data
    )
    results$run()
    
    outcomes <- results$results$clusterOutcomes$asDF
    expect_equal(nrow(outcomes), 2)
    expect_true(any(grepl("A:", outcomes$outcome_summary)))
})
