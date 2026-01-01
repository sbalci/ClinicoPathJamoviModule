

# Source the function files
source("../../R/ihccluster.h.R")
source("../../R/ihccluster.b.R")

test_that("ihccluster works with basic PAM clustering", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    set.seed(123)
    data <- data.frame(
        CaseID = paste0("Case", 1:50),
        CD3 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        CD20 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        Ki67 = runif(50, 0, 100),
        PDL1 = runif(50, 0, 300),
        Diagnosis = factor(sample(c("A", "B"), 50, replace = TRUE))
    )

    options <- ihcclusterOptions$new(
        catVars = c("CD3", "CD20"),
        contVars = c("Ki67", "PDL1"),
        method = "pam",
        nClusters = 3,
        autoSelectK = FALSE
    )

    analysis <- ihcclusterClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check results
    expect_true(analysis$results$clusterSizes$visible)
    expect_equal(analysis$results$clusterSizes$rowCount, 3)
})

test_that("ihccluster handles Jaccard distance and binary conversion", {
    set.seed(123)
    data <- data.frame(
        CD3 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        Ki67 = runif(50, 0, 100)
    )

    options <- ihcclusterOptions$new(
        catVars = c("CD3"),
        contVars = c("Ki67"),
        method = "hierarchical",
        distanceMethod = "jaccard",
        nClusters = 2,
        autoSelectK = FALSE
    )

    analysis <- ihcclusterClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check binary conversion note
    expect_true(analysis$results$binaryConversionNote$visible)
})

test_that("ihccluster performs reproducibility testing", {
    set.seed(123)
    data <- data.frame(
        CD3 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        Ki67 = runif(50, 0, 100)
    )

    options <- ihcclusterOptions$new(
        catVars = c("CD3"),
        contVars = c("Ki67"),
        reproducibilityTest = TRUE,
        nSplits = 2,
        nClusters = 2,
        autoSelectK = FALSE
    )

    analysis <- ihcclusterClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check reproducibility stats
    expect_true(analysis$results$reproducibilityStats$visible)
    expect_gt(analysis$results$reproducibilityStats$rowCount, 0)
})

test_that("ihccluster calculates marker ratios", {
    set.seed(123)
    data <- data.frame(
        Ki67 = runif(50, 10, 100),
        PDL1 = runif(50, 10, 300)
    )

    options <- ihcclusterOptions$new(
        contVars = c("Ki67", "PDL1"),
        calculateRatios = TRUE,
        ratioNumerator = "Ki67",
        ratioDenominator = "PDL1",
        ratioClassification = TRUE
    )

    analysis <- ihcclusterClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check ratio summary
    expect_true(analysis$results$ratioSummary$visible)
})

test_that("ihccluster handles supervised clustering", {
    set.seed(123)
    data <- data.frame(
        CD3 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
        Ki67 = runif(50, 0, 100),
        Diagnosis = factor(sample(c("A", "B"), 50, replace = TRUE))
    )

    options <- ihcclusterOptions$new(
        catVars = c("CD3"),
        contVars = c("Ki67"),
        supervisedClustering = TRUE,
        supervisedVariable = "Diagnosis",
        nClusters = 2,
        autoSelectK = FALSE
    )

    analysis <- ihcclusterClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Check supervised summary
    expect_true(analysis$results$supervisedSummary$visible)
})

test_that("ihccluster handles missing data correctly", {
    set.seed(123)
    data <- data.frame(
        CD3 = factor(sample(c("Positive", "Negative", NA), 50, replace = TRUE)),
        Ki67 = c(runif(45, 0, 100), rep(NA, 5))
    )

    # Pairwise deletion (default)
    options <- ihcclusterOptions$new(
        catVars = c("CD3"),
        contVars = c("Ki67"),
        handleMissing = "pairwise"
    )
    analysis <- ihcclusterClass$new(options = options, data = data)
    expect_error(analysis$run(), NA)

    # Complete cases
    options2 <- ihcclusterOptions$new(
        catVars = c("CD3"),
        contVars = c("Ki67"),
        handleMissing = "complete"
    )
    analysis2 <- ihcclusterClass$new(options = options2, data = data)
    expect_error(analysis2$run(), NA)
})
