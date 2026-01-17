
test_that("pathologyagreement works for basic agreement", {
    skip_if_not_installed("psych")
    skip_if_not_installed("epiR")
    
    set.seed(123)
    n <- 50
    data <- data.frame(
        method1 = rnorm(n, 50, 10),
        method2 = rnorm(n, 50, 10) + rnorm(n, 0, 2),
        stringsAsFactors = FALSE
    )
    
    results <- pathologyagreementClass$new(
        options = pathologyagreementOptions$new(
            dep1 = "method1",
            dep2 = "method2",
            icc_type = "consistency",
            correlation_method = "both",
            show_plots = FALSE
        ),
        data = data
    )
    results$run()
    
    agreement_table <- results$results$agreementtable$asDF
    expect_equal(nrow(agreement_table), 5)
    expect_true("ICC(3,1) - Consistency" %in% agreement_table$metric)
    
    correlation_table <- results$results$correlationtable$asDF
    expect_equal(nrow(correlation_table), 2)
})

test_that("pathologyagreement works for absolute agreement", {
    skip_if_not_installed("psych")
    
    set.seed(123)
    n <- 50
    data <- data.frame(
        method1 = rnorm(n, 50, 10),
        method2 = rnorm(n, 50, 10) + rnorm(n, 0, 2),
        stringsAsFactors = FALSE
    )
    
    results <- pathologyagreementClass$new(
        options = pathologyagreementOptions$new(
            dep1 = "method1",
            dep2 = "method2",
            icc_type = "absolute",
            show_plots = FALSE
        ),
        data = data
    )
    results$run()
    
    agreement_table <- results$results$agreementtable$asDF
    expect_true("ICC(2,1) - Absolute Agreement" %in% agreement_table$metric)
})

test_that("pathologyagreement works for multi-method analysis", {
    skip_if_not_installed("psych")
    
    set.seed(123)
    n <- 50
    data <- data.frame(
        method1 = rnorm(n, 50, 10),
        method2 = rnorm(n, 50, 10) + rnorm(n, 0, 2),
        method3 = rnorm(n, 50, 10) + rnorm(n, 0, 5),
        stringsAsFactors = FALSE
    )
    
    results <- pathologyagreementClass$new(
        options = pathologyagreementOptions$new(
            dep1 = "method1",
            dep2 = "method2",
            additional_methods = "method3",
            clinical_preset = "multisite_validation",
            show_plots = FALSE
        ),
        data = data
    )
    results$run()
    
    corr_matrix <- results$results$correlationmatrix$asDF
    expect_equal(nrow(corr_matrix), 3)
    
    overall_icc <- results$results$overall_icc_table$asDF
    expect_equal(nrow(overall_icc), 3)
})

test_that("pathologyagreement displays warnings for small samples", {
    set.seed(123)
    n <- 10
    data <- data.frame(
        method1 = rnorm(n, 50, 10),
        method2 = rnorm(n, 50, 10) + rnorm(n, 0, 2),
        stringsAsFactors = FALSE
    )
    
    results <- pathologyagreementClass$new(
        options = pathologyagreementOptions$new(
            dep1 = "method1",
            dep2 = "method2",
            clinical_preset = "biomarker_platforms",
            show_plots = FALSE
        ),
        data = data
    )
    results$run()
    
    interpretation <- results$results$interpretation$content
    expect_true(grepl("Sample size", interpretation))
})
