
context("nogoldstandard refinements")

test_that("nogoldstandard enforces constraints and handles warnings", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Simulate structured data for better LCA convergence
    set.seed(123)
    n <- 100 # Increased N for stability
    true_class <- rbinom(n, 1, 0.4)
    
    test1_prob <- ifelse(true_class == 1, 0.9, 0.1)
    test2_prob <- ifelse(true_class == 1, 0.85, 0.15)
    test3_prob <- ifelse(true_class == 1, 0.8, 0.2)
    
    data <- data.frame(
        test1 = factor(rbinom(n, 1, test1_prob)),
        test2 = factor(rbinom(n, 1, test2_prob)),
        test3 = factor(rbinom(n, 1, test3_prob))
    )
    
    # Test 1: LCA with 2 tests should fail (Error)
    # --------------------------------------------
    expect_error({
        nogoldstandard(
            data = data,
            test1 = "test1",
            test1Positive = "1",
            test2 = "test2",
            test2Positive = "1",
            test3Positive = NULL,
            test4Positive = NULL,
            test5Positive = NULL,
            method = "latent_class"
        )
    }, "Latent Class Analysis requires at least 3 tests")
    
    # Test 2: Warnings Panel for Small N (Bayesian)
    # ---------------------------------------------
    # Create small subset for warning trigger
    data_tiny <- data[1:20, ]
    results_tiny <- nogoldstandard(
        data = data_tiny,
        test1 = "test1",
        test1Positive = "1",
        test2 = "test2",
        test2Positive = "1",
        test3Positive = NULL,
        test4Positive = NULL,
        test5Positive = NULL,
        method = "bayesian"
    )
    
    expect_true(results_tiny$warnings$visible)
    expect_match(results_tiny$warnings$content, "Bayesian analysis may be unstable with N < 50")
    
    # Test 3: Bootstrap Execution and CIs
    # -----------------------------------
    # LCA with 3 tests (should work) + Bootstrap
    results_boot <- nogoldstandard(
        data = data,
        test1 = "test1",
        test1Positive = "1",
        test2 = "test2",
        test2Positive = "1",
        test3 = "test3",
        test3Positive = "1",
        test4Positive = NULL,
        test5Positive = NULL,
        method = "latent_class",
        bootstrap = TRUE,
        nboot = 100 # Minimum allowed nboot
    )
    
    # Check that prev table has CI
    prev_table <- results_boot$prevalence$asDF
    expect_false(is.na(prev_table$ci_lower[1]))
    expect_false(is.na(prev_table$ci_upper[1]))
    
    # Test 4: Agreement Statistics
    # ----------------------------
    # Kappa should be calculated for all pairs
    agree_table <- results_boot$agreement_stats$asDF
    expect_equal(nrow(agree_table), 3) # 3 pairs for 3 tests: 1-2, 1-3, 2-3
    expect_false(any(is.na(agree_table$kappa)))
    
})
