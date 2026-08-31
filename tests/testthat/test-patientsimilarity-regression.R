# Regression tests for defects found by /check-function patientsimilarity.
# Each block names the bug it guards; a failure here means that bug is back.

library(ClinicoPath)

make_ps_data <- function(n = 80, seed = 11) {
    set.seed(seed)
    data.frame(
        age     = stats::rnorm(n, 60, 10),
        size    = stats::rlnorm(n, 1.5, 0.5),
        ki67    = stats::runif(n, 1, 90),
        grade   = sample(1:3, n, TRUE),
        outcome = factor(sample(c("Alive", "Dead"), n, TRUE)),
        months  = stats::rexp(n, 0.04),
        death   = factor(sample(c("0", "1"), n, TRUE))
    )
}

PS_VARS <- c("age", "size", "ki67", "grade")

render_ok <- function(image) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    isTRUE(image$.render())
}

test_that("cluster assignments reach the renderers (clusters were lost on copy)", {
    res <- ClinicoPath::patientsimilarity(
        data = make_ps_data(), vars = !!PS_VARS, method = "pca",
        scaleVars = TRUE, performClustering = TRUE, nClusters = 3,
        survivalEventLevel = NULL)

    expect_equal(res$clusterSummary$rowCount, 3)
    # The cached projection must carry $clusters, otherwise the coloured scatter and
    # the KM plot both silently fall back to "no clusters".
    expect_true(render_ok(res$projectionPlot))
})

test_that("a 3D projection still draws the main 2D plot and the pairwise panel", {
    res <- ClinicoPath::patientsimilarity(
        data = make_ps_data(), vars = !!PS_VARS, method = "pca",
        dimensions = "3", show3DPlot = TRUE, colorBy = "outcome",
        survivalEventLevel = NULL)

    expect_true(render_ok(res$projectionPlot))   # returned NULL before the fix
    expect_true(render_ok(res$projection3D))     # plotly could not paint a jamovi Image
})

test_that("survival by cluster reports KM medians, a log-rank row and a KM plot", {
    res <- ClinicoPath::patientsimilarity(
        data = make_ps_data(), vars = !!PS_VARS, method = "pca", scaleVars = TRUE,
        performClustering = TRUE, nClusters = 2, survivalAnalysis = TRUE,
        survivalTime = "months", survivalEvent = "death", survivalEventLevel = "1")

    surv <- res$survivalTable$asDF
    expect_equal(nrow(surv), 2)
    expect_true(all(is.finite(surv$median_survival)))

    # rows: 1 pre-creates rowKey "1"; addRow(rowKey = 1) used to append a second, blank row.
    lr <- res$survivalComparison$asDF
    expect_equal(nrow(lr), 1)
    expect_true(is.finite(lr$chisq) && is.finite(lr$pvalue))

    expect_true(render_ok(res$survivalPlot))
})

test_that("a factor event variable with no level selected is rejected, not crashed", {
    res <- ClinicoPath::patientsimilarity(
        data = make_ps_data(), vars = !!PS_VARS, method = "pca",
        performClustering = TRUE, nClusters = 2, survivalAnalysis = TRUE,
        survivalTime = "months", survivalEvent = "death", survivalEventLevel = NULL)

    expect_match(res$notices$content, "Event Level Required")
    expect_equal(res$survivalTable$rowCount, 0)
})

test_that("outlier removal keeps clusters, tables and the export aligned to source rows", {
    dat <- make_ps_data()
    dat$size[1:5] <- 1e4                       # forced outliers

    opts <- ClinicoPath:::patientsimilarityOptions$new(
        vars = PS_VARS, method = "pca", scaleVars = TRUE, removeOutliers = TRUE,
        performClustering = TRUE, nClusters = 2, survivalEventLevel = NULL)
    for (nm in c("exportClusters", "exportCoordinates")) {
        op <- opts$option(nm); op$value <- list(value = TRUE)
    }
    analysis <- ClinicoPath:::patientsimilarityClass$new(options = opts, data = dat)
    analysis$run()
    res <- analysis$results

    values <- res$exportClusters$.__enclos_env__$private$.values
    assigned <- if (is.list(values)) values[[1]] else values

    expect_length(assigned, nrow(dat))
    expect_true(all(is.na(assigned[1:5])))                      # outliers stay unassigned
    expect_equal(sum(!is.na(assigned)), sum(res$clusterSummary$asDF$n))
    expect_match(res$notices$content, "Outliers Removed")
})

test_that("too few variables for the requested dimensions is caught up front", {
    res <- ClinicoPath::patientsimilarity(
        data = make_ps_data(), vars = !!c("age", "size"), dimensions = "3",
        method = "pca", survivalEventLevel = NULL)

    expect_match(res$notices$content, "Too Few Variables")
    expect_equal(res$varianceTable$rowCount, 0)
})

test_that("a constant variable is dropped with a notice instead of producing NaNs", {
    dat <- make_ps_data()
    dat$flat <- 1

    res <- ClinicoPath::patientsimilarity(
        data = dat, vars = !!c("age", "size", "flat"), method = "pca",
        scaleVars = TRUE, survivalEventLevel = NULL)

    expect_match(res$notices$content, "Constant Variables Dropped")
    expect_gt(res$varianceTable$rowCount, 0)
})

test_that("DBSCAN noise is labelled as noise, not as a cluster", {
    res <- ClinicoPath::patientsimilarity(
        data = make_ps_data(), vars = !!PS_VARS, method = "pca", scaleVars = TRUE,
        performClustering = TRUE, clusterMethod = "dbscan",
        dbscan_eps = 0.3, dbscan_minpts = 5, survivalEventLevel = NULL)

    labels <- res$clusterSummary$asDF$cluster
    expect_true(any(grepl("Noise", labels, fixed = TRUE)))
})

test_that("re-running the analysis does not accumulate table rows or notices", {
    opts <- ClinicoPath:::patientsimilarityOptions$new(
        vars = PS_VARS, method = "pca", scaleVars = TRUE, performClustering = TRUE,
        nClusters = 3, showClusterStats = TRUE, showLoadings = TRUE,
        survivalEventLevel = NULL)
    analysis <- ClinicoPath:::patientsimilarityClass$new(
        options = opts, data = make_ps_data())

    counts <- function() vapply(
        c("varianceTable", "loadingsTable", "clusterSummary",
          "clusterCharacteristics", "clusterQuality"),
        function(nm) analysis$results[[nm]]$rowCount, numeric(1))

    analysis$run(); first  <- counts()
    analysis$run(); second <- counts()

    expect_identical(first, second)
})

test_that("variables with spaces and punctuation are handled", {
    dat <- make_ps_data()[, c("age", "size", "ki67")]
    names(dat) <- c("Age at Dx", "Tumour size (mm)", "Ki-67 %")

    res <- ClinicoPath::patientsimilarity(
        data = dat, vars = !!names(dat), method = "pca",
        performClustering = TRUE, nClusters = 2, showClusterStats = TRUE,
        survivalEventLevel = NULL)

    expect_equal(res$clusterSummary$rowCount, 2)
    expect_setequal(res$clusterCharacteristics$asDF$variable, names(dat))
    expect_true(render_ok(res$projectionPlot))
})

test_that("every dimensionality reduction method runs in 2D and 3D", {
    dat <- make_ps_data()
    for (m in c("pca", "tsne", "umap", "mds")) {
        for (d in c("2", "3")) {
            res <- ClinicoPath::patientsimilarity(
                data = dat, vars = !!PS_VARS, method = m, dimensions = d,
                colorBy = "outcome", scaleVars = TRUE, performClustering = TRUE,
                nClusters = 3, show3DPlot = (d == "3"), survivalEventLevel = NULL)

            expect_equal(res$clusterSummary$rowCount, 3,
                         info = paste(m, d))
            expect_true(render_ok(res$projectionPlot), info = paste(m, d))
            if (d == "3")
                expect_true(render_ok(res$projection3D), info = paste(m, d))
        }
    }
})
