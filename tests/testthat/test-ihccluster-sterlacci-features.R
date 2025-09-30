# Test for Sterlacci 2019 Phase 1 Features
# Tests: Jaccard distance, complete linkage, Bonferroni correction

test_that("Jaccard distance option works with binary data", {
    skip_if_not_installed("proxy")

    # Create simple binary test data
    set.seed(42)
    test_data <- data.frame(
        CaseID = paste0("Case_", 1:30),
        Marker1 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE)),
        Marker2 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE)),
        Marker3 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE)),
        Marker4 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE)),
        Marker5 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE))
    )

    # Test that Jaccard distance produces valid results
    result <- ihccluster(
        data = test_data,
        catVars = c("Marker1", "Marker2", "Marker3", "Marker4", "Marker5"),
        method = "hierarchical",
        distanceMethod = "jaccard",
        nClusters = 3,
        autoSelectK = FALSE
    )

    expect_true(!is.null(result))
    expect_true("clusters" %in% names(result))
    expect_equal(length(result$clusters), 30)
    expect_true(any(grepl("Jaccard", result$notes)))
})

test_that("Complete linkage produces different results than Ward", {
    # Create test data
    set.seed(42)
    test_data <- data.frame(
        Marker1 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        Marker2 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        Marker3 = rnorm(50, mean = 20, sd = 5)
    )

    # Ward linkage
    result_ward <- ihccluster(
        data = test_data,
        catVars = c("Marker1", "Marker2"),
        contVars = "Marker3",
        method = "hierarchical",
        linkageMethod = "ward",
        nClusters = 3,
        autoSelectK = FALSE
    )

    # Complete linkage
    result_complete <- ihccluster(
        data = test_data,
        catVars = c("Marker1", "Marker2"),
        contVars = "Marker3",
        method = "hierarchical",
        linkageMethod = "complete",
        nClusters = 3,
        autoSelectK = FALSE
    )

    expect_true(!is.null(result_ward))
    expect_true(!is.null(result_complete))

    # Linkage methods should produce different clusters
    expect_false(all(result_ward$clusters == result_complete$clusters))

    # Check notes mention linkage method
    expect_true(any(grepl("ward", result_ward$notes, ignore.case = TRUE)))
    expect_true(any(grepl("complete", result_complete$notes, ignore.case = TRUE)))
})

test_that("Bonferroni correction adjusts p-values correctly", {
    # Create test data with known cluster structure
    set.seed(42)
    n <- 60
    clusters <- rep(1:3, each = 20)

    test_data <- data.frame(
        # Markers strongly associated with clusters
        Marker1 = factor(ifelse(clusters == 1, "Positive", "Negative")),
        Marker2 = factor(ifelse(clusters == 2, "Positive", "Negative")),
        Marker3 = factor(ifelse(clusters == 3, "Positive", "Negative")),
        # Markers not associated
        Marker4 = factor(sample(c("Positive", "Negative"), n, replace = TRUE)),
        Marker5 = factor(sample(c("Positive", "Negative"), n, replace = TRUE)),
        Marker6 = rnorm(n),
        Marker7 = rnorm(n),
        Marker8 = rnorm(n)
    )

    # Run with Bonferroni correction
    result_bonf <- ihccluster(
        data = test_data,
        catVars = paste0("Marker", 1:5),
        contVars = paste0("Marker", 6:8),
        method = "pam",
        nClusters = 3,
        autoSelectK = FALSE,
        associationTests = TRUE,
        multipleTestingCorrection = "bonferroni"
    )

    # Run without correction
    result_none <- ihccluster(
        data = test_data,
        catVars = paste0("Marker", 1:5),
        contVars = paste0("Marker", 6:8),
        method = "pam",
        nClusters = 3,
        autoSelectK = FALSE,
        associationTests = TRUE,
        multipleTestingCorrection = "none"
    )

    # Adjusted p-values should be >= raw p-values
    expect_true(all(result_bonf$association_p_adjusted >= result_bonf$association_p_raw,
                    na.rm = TRUE))

    # With no correction, adjusted = raw
    expect_true(all(result_none$association_p_adjusted == result_none$association_p_raw,
                    na.rm = TRUE))
})

test_that("All linkage methods work without errors", {
    test_data <- data.frame(
        M1 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE)),
        M2 = factor(sample(c("Positive", "Negative"), 30, replace = TRUE)),
        M3 = rnorm(30)
    )

    linkage_methods <- c("ward", "complete", "average", "single")

    for (linkage in linkage_methods) {
        result <- ihccluster(
            data = test_data,
            catVars = c("M1", "M2"),
            contVars = "M3",
            method = "hierarchical",
            linkageMethod = linkage,
            nClusters = 3,
            autoSelectK = FALSE
        )

        expect_true(!is.null(result),
                   info = sprintf("Linkage method %s failed", linkage))
        expect_equal(length(result$clusters), 30,
                    info = sprintf("Linkage method %s wrong cluster count", linkage))
    }
})

test_that("Multiple testing correction methods all work", {
    test_data <- data.frame(
        M1 = factor(sample(c("Positive", "Negative"), 40, replace = TRUE)),
        M2 = factor(sample(c("Positive", "Negative"), 40, replace = TRUE)),
        M3 = rnorm(40),
        M4 = rnorm(40)
    )

    correction_methods <- c("none", "bonferroni", "fdr", "holm")

    for (method in correction_methods) {
        result <- ihccluster(
            data = test_data,
            catVars = c("M1", "M2"),
            contVars = c("M3", "M4"),
            method = "pam",
            nClusters = 2,
            autoSelectK = FALSE,
            associationTests = TRUE,
            multipleTestingCorrection = method
        )

        expect_true(!is.null(result),
                   info = sprintf("Correction method %s failed", method))
    }
})

test_that("Jaccard with continuous markers uses median split", {
    test_data <- data.frame(
        M1 = rnorm(30, mean = 50, sd = 10),
        M2 = rnorm(30, mean = 20, sd = 5),
        M3 = rnorm(30, mean = 80, sd = 15)
    )

    # Should work - continuous markers get binarized
    result <- ihccluster(
        data = test_data,
        contVars = c("M1", "M2", "M3"),
        method = "hierarchical",
        distanceMethod = "jaccard",
        nClusters = 2,
        autoSelectK = FALSE
    )

    expect_true(!is.null(result))
    expect_true(any(grepl("Jaccard", result$notes)))
    expect_true(any(grepl("binary", result$notes, ignore.case = TRUE)))
})
