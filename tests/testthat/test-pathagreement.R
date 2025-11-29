
test_that("pathagreement works for basic agreement (Cohen's Kappa)", {
    skip_if_not_installed("irr")
    
    set.seed(123)
    data <- data.frame(
        rater1 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        rater2 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        stringsAsFactors = TRUE
    )
    
    # Introduce some agreement
    data$rater2[1:10] <- data$rater1[1:10]
    
    results <- pathagreementClass$new(
        options = pathagreementOptions$new(
            vars = c("rater1", "rater2"),
            multiraterMethod = "cohen",
            sft = FALSE
        ),
        data = data
    )
    results$run()
    
    kappa_table <- results$results$kappaTable$asDF
    expect_equal(nrow(kappa_table), 1)
    expect_equal(kappa_table$method[1], "Cohen's Kappa")
    expect_true(!is.na(kappa_table$kappa[1]))
})

test_that("pathagreement works for multi-rater agreement (Fleiss' Kappa)", {
    skip_if_not_installed("irr")
    
    set.seed(123)
    data <- data.frame(
        rater1 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        rater2 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        rater3 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        stringsAsFactors = TRUE
    )
    
    results <- pathagreementClass$new(
        options = pathagreementOptions$new(
            vars = c("rater1", "rater2", "rater3"),
            multiraterMethod = "fleiss",
            fleissCI = TRUE
        ),
        data = data
    )
    results$run()
    
    kappa_table <- results$results$kappaTable$asDF
    expect_equal(nrow(kappa_table), 1)
    expect_true(grepl("Fleiss", kappa_table$method[1]))
})

test_that("pathagreement works for Krippendorff's Alpha", {
    skip_if_not_installed("irr")
    
    set.seed(123)
    data <- data.frame(
        rater1 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        rater2 = factor(sample(c("A", "B"), 20, replace = TRUE)),
        stringsAsFactors = TRUE
    )
    
    results <- pathagreementClass$new(
        options = pathagreementOptions$new(
            vars = c("rater1", "rater2"),
            kripp = TRUE,
            krippMethod = "nominal"
        ),
        data = data
    )
    results$run()
    
    kripp_table <- results$results$krippTable$asDF
    expect_equal(nrow(kripp_table), 1)
    expect_true(grepl("Krippendorff", kripp_table$method[1]))
})

test_that("pathagreement works for Consensus Analysis", {
    set.seed(123)
    data <- data.frame(
        rater1 = factor(c("A", "A", "B", "B", "A")),
        rater2 = factor(c("A", "B", "B", "A", "A")),
        rater3 = factor(c("A", "A", "B", "B", "B")),
        stringsAsFactors = TRUE
    )
    
    results <- pathagreementClass$new(
        options = pathagreementOptions$new(
            vars = c("rater1", "rater2", "rater3"),
            consensus = TRUE,
            consensus_method = "majority",
            show_consensus_table = TRUE
        ),
        data = data
    )
    results$run()
    
    summary_table <- results$results$consensusSummary$asDF
    expect_true(nrow(summary_table) > 0)
    
    consensus_table <- results$results$consensusTable$asDF
    expect_equal(nrow(consensus_table), 5)
})
