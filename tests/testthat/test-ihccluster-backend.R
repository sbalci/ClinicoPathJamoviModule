# Tests for ihccluster backend preprocessing and clustering logic

library(testthat)

skip_if_not_installed("cluster")

create_analysis <- function(data, opts) {
    ClinicoPath:::ihcclusterClass$new(options = opts, data = data)
}

test_that("pairwise missing values remain unmapped and are documented", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    data <- data.frame(
        ER = factor(c("pos", "neg", NA, "neg", "pos")),
        PR = factor(c("pos", "pos", "neg", "neg", "pos")),
        Ki67 = c(10, NA, 30, 25, NA),
        PDL1 = c(5, 10, 20, 30, 40)
    )

    opts <- ClinicoPath:::ihcclusterOptions$new(
        catVars = c("ER", "PR"),
        contVars = c("Ki67", "PDL1"),
        method = "pam",
        nClusters = 2,
        autoSelectK = FALSE,
        handleMissing = "pairwise",
        showHeatmap = FALSE,
        showSilhouette = FALSE,
        showPCAPlot = FALSE,
        showBoxplots = FALSE,
        showDendrogram = FALSE,
        markerSummary = FALSE,
        clusterProfiles = FALSE,
        associationTests = FALSE
    )

    analysis <- create_analysis(data, opts)
    prep <- analysis$.__enclos_env__$private$.prepareData(data, c("ER", "PR"), c("Ki67", "PDL1"), opts)

    expect_true(any(is.na(prep$df$ER)))
    expect_true(any(is.na(prep$df$Ki67)))
    expect_true(any(grepl("pairwise Gower", prep$notes)))
})

test_that("dimension reduction imputes missing values before k-means", {
    skip_if_not_installed("FactoMineR")

    data <- data.frame(
        ER = factor(c("pos", "neg", NA, "neg", "pos", "pos")),
        PR = factor(c("pos", "pos", "neg", "neg", "pos", "neg")),
        Ki67 = c(10, NA, 30, 25, 15, 45),
        PDL1 = c(5, 10, NA, 30, 40, 50)
    )

    opts <- ClinicoPath:::ihcclusterOptions$new(
        catVars = c("ER", "PR"),
        contVars = c("Ki67", "PDL1"),
        method = "dimreduce",
        nClusters = 2,
        autoSelectK = FALSE,
        handleMissing = "pairwise",
        showHeatmap = FALSE,
        showSilhouette = FALSE,
        showPCAPlot = FALSE,
        showBoxplots = FALSE,
        showDendrogram = FALSE,
        markerSummary = FALSE,
        clusterProfiles = FALSE,
        associationTests = FALSE
    )

    analysis <- create_analysis(data, opts)
    prep <- analysis$.__enclos_env__$private$.prepareData(data, c("ER", "PR"), c("Ki67", "PDL1"), opts)

    result <- analysis$.__enclos_env__$private$.clusterData(
        df = prep$df,
        method = "dimreduce",
        opts = opts,
        k = 2,
        autoSelect = FALSE,
        catVars = prep$catVars,
        contVars = prep$contVars,
        weights = NULL,
        silhouetteTable = NULL
    )

    expect_equal(length(result$clusters), nrow(prep$df))
    expect_true(any(grepl("imputation", result$notes)))
})

test_that("categorical association tests fall back to Fisher's exact for sparse 2x2 tables", {
    data <- data.frame(
        ER = factor(c("pos", "pos", "pos", "neg", "neg", "neg")),
        PR = factor(c("pos", "pos", "neg", "neg", "neg", "neg")),
        Ki67 = c(10, 12, 15, 40, 45, 48),
        PDL1 = c(5, 8, 12, 60, 55, 58)
    )

    opts <- ClinicoPath:::ihcclusterOptions$new(
        catVars = c("ER", "PR"),
        contVars = c("Ki67", "PDL1"),
        method = "pam",
        nClusters = 2,
        autoSelectK = FALSE,
        handleMissing = "pairwise",
        associationTests = TRUE,
        showHeatmap = FALSE,
        showSilhouette = FALSE,
        showPCAPlot = FALSE,
        showBoxplots = FALSE,
        showDendrogram = FALSE,
        markerSummary = FALSE,
        clusterProfiles = FALSE
    )

    analysis <- create_analysis(data, opts)
    analysis$run()

    assoc <- analysis$results$associationTests$asDF
    fisher_rows <- assoc$marker[assoc$test == "Fisher's exact"]
    expect_true("ER" %in% fisher_rows)
})
