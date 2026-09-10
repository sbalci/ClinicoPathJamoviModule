library(testthat)

# ==============================================================================
# METHODOLOGY VALIDATION TESTS FOR PATHSAMPLING
# ==============================================================================
#
# These tests validate the STATISTICAL METHODOLOGY of pathsampling, not just
# the mathematical correctness (which is covered in test-pathsampling-statistical-validation.R)
#
# Key Areas:
# 1. Beta-binomial parameter estimation (N-weighted vs unweighted)
# 2. Selection bias warnings in bootstrap
# 3. Model choice enforcement (binomial disabled when invalid)
# ==============================================================================

# ==============================================================================
# 1. BETA-BINOMIAL: N-WEIGHTED ESTIMATION
# ==============================================================================

test_that("Beta-binomial estimation differs from unweighted when N varies", {
  skip_if_not_installed('jmvReadWrite')
    skip_if_not_installed("VGAM")

    # Create test data with VARYING sample sizes
    # This is critical - unweighted will give wrong answer
    testData <- data.frame(
        case_id = c("Small", "Medium", "Large"),
        total_pop = c(10, 50, 100),        # Varying N
        success = c(2, 25, 50)              # Proportions: 0.20, 0.50, 0.50
    )

    # Unweighted mean would be: (0.20 + 0.50 + 0.50)/3 = 0.40
    # Weighted mean should be: (2+25+50)/(10+50+100) = 77/160 = 0.48125

    # Test VGAM estimation directly
    betaData <- data.frame(
        success = testData$success,
        fail = testData$total_pop - testData$success
    )

    fit <- VGAM::vglm(
        cbind(success, fail) ~ 1,
        family = VGAM::betabinomial,
        data = betaData,
        trace = FALSE
    )

    coefs <- VGAM::Coef(fit)
    mu_fit <- unname(coefs[["mu"]])   # Coef() is already on the natural scale

    # Verify mu is closer to weighted mean (0.48) than unweighted (0.40)
    expect_true(abs(mu_fit - 0.48125) < 0.05,
                info = sprintf("VGAM mu=%.4f should be near weighted 0.48, not unweighted 0.40", mu_fit))
    expect_true(abs(mu_fit - 0.40) > 0.05,
                info = "Should NOT match unweighted mean")
})

test_that("Beta-binomial with equal N gives similar weighted/unweighted", {
    skip_if_not_installed("VGAM")

    # When all N are equal, weighted = unweighted
    testData <- data.frame(
        case_id = paste0("Case", 1:5),
        total_pop = rep(100, 5),        # All equal N
        success = c(20, 40, 50, 60, 80)  # Mean = 50/100 = 0.50
    )

    betaData <- data.frame(
        success = testData$success,
        fail = testData$total_pop - testData$success
    )

    fit <- VGAM::vglm(
        cbind(success, fail) ~ 1,
        family = VGAM::betabinomial,
        data = betaData,
        trace = FALSE
    )

    coefs <- VGAM::Coef(fit)
    mu_fit <- unname(coefs[["mu"]])   # Coef() is already on the natural scale

    # With equal N, should match simple mean
    simple_mean <- mean(testData$success / testData$total_pop)

    expect_equal(mu_fit, simple_mean, tolerance = 0.01,
                 info = "With equal N, weighted MLE should match simple mean")
})

test_that("Beta-binomial extreme case: one large N dominates", {
    skip_if_not_installed("VGAM")

    # One very large study should dominate the estimate
    testData <- data.frame(
        case_id = c("Tiny1", "Tiny2", "Huge"),
        total_pop = c(5, 5, 1000),       # One dominant study
        success = c(1, 1, 500)             # p = 0.20, 0.20, 0.50
    )

    # Unweighted mean: (0.20 + 0.20 + 0.50)/3 = 0.30
    # Weighted mean: (1+1+500)/(5+5+1000) = 502/1010 = 0.497

    betaData <- data.frame(
        success = testData$success,
        fail = testData$total_pop - testData$success
    )

    fit <- VGAM::vglm(
        cbind(success, fail) ~ 1,
        family = VGAM::betabinomial,
        data = betaData,
        trace = FALSE
    )

    coefs <- VGAM::Coef(fit)
    mu_fit <- unname(coefs[["mu"]])   # Coef() is already on the natural scale

    # Should be near 0.50 (large study), NOT 0.30 (unweighted)
    expect_true(mu_fit > 0.45,
                info = sprintf("Large study should dominate: mu=%.3f should be >0.45", mu_fit))
    expect_true(abs(mu_fit - 0.30) > 0.10,
                info = "Should NOT match unweighted mean of 0.30")
})

# ==============================================================================
# 2. SELECTION BIAS WARNINGS
# ==============================================================================

test_that("Bootstrap analysis generates selection bias warning", {
    # Create simple test data
    testData <- data.frame(
        case_id = paste0("Case", 1:10),
        first_detection = c(1, 2, 3, 1, 2, NA, NA, 1, 2, 3),  # 3 undetected (NA)
        total_samples = rep(10, 10),
        positive_count = c(3, 2, 1, 3, 2, NA, NA, 3, 2, 1)
    )

    # Note: This test assumes pathsampling returns notices in a testable way
    # May need to be adjusted based on actual implementation

    # The function should generate a STRONG_WARNING about selection bias
    # when bootstrap is enabled

    # For now, we'll test that the warning logic exists in the code
    # by checking the function can run without error
    expect_silent({
        # Would need actual pathsampling call here
        # result <- pathsampling(data = testData, showBootstrap = TRUE, ...)
    })

    # TODO: Once we can access notices from pathsampling results,
    # add explicit check for selection bias warning
})

# ==============================================================================
# 3. MODEL CHOICE ENFORCEMENT
# ==============================================================================

test_that("Binomial model disabled when CV > 0.5 (high heterogeneity)", {
    # Create data with high heterogeneity
    testData <- data.frame(
        case_id = paste0("Case", 1:10),
        total_samples = rep(20, 10),
        positive_count = c(1, 1, 1, 18, 19, 19, 2, 2, 18, 18),  # Bimodal -> high CV
        first_detection = c(10, 9, 8, 1, 1, 1, 9, 10, 1, 1)
    )

    # Calculate CV to verify it's > 0.5
    proportions <- testData$positive_count / testData$total_samples
    cv <- sd(proportions) / mean(proportions)

    expect_true(cv > 0.5,
                info = sprintf("Test data should have CV > 0.5, got %.2f", cv))

    # TODO: Call pathsampling and verify:
    # 1. Binomial table is empty
    # 2. Error message is displayed
    # 3. Function doesn't crash

    # For now, verify CV calculation is correct
    expect_gt(cv, 0.5)
})

test_that("Binomial model ALLOWED when CV < 0.5 (low heterogeneity)", {
    # Create data with low heterogeneity
    testData <- data.frame(
        case_id = paste0("Case", 1:10),
        total_samples = rep(20, 10),
        positive_count = rep(10, 10),  # All identical -> CV = 0
        first_detection = rep(5, 10)
    )

    # Calculate CV to verify it's < 0.5
    proportions <- testData$positive_count / testData$total_samples
    cv <- sd(proportions) / mean(proportions)

    expect_true(cv < 0.3,
                info = sprintf("Test data should have CV near 0, got %.2f", cv))

    # TODO: Call pathsampling and verify binomial table is populated

    # For now, verify CV calculation is correct
    expect_lt(cv, 0.3)
})

test_that("Binomial model warned when 0.3 < CV < 0.5 (moderate heterogeneity)", {
    # This previously only generated random data and asserted its own CV landed in a
    # band -- it never called pathsampling(), and the runif() fixture actually gave
    # CV = 0.19. Deterministic fixtures now drive the analysis and check the warning.
    total <- rep(20L, 20)
    first <- rep(c(1L, 2L, 3L, 4L), 5)

    moderate <- data.frame(
        totalSamples   = total,
        firstDetection = first,
        positiveCount  = c(3, 4, 4, 5, 5, 6, 6, 7, 7, 8,
                           12, 13, 13, 14, 14, 15, 15, 16, 16, 17)
    )
    cv_mod <- sd(moderate$positiveCount / total) / mean(moderate$positiveCount / total)
    expect_gt(cv_mod, 0.3)
    expect_lt(cv_mod, 0.5)

    res <- pathsampling(
        data              = moderate,
        totalSamples      = 'totalSamples',
        firstDetection    = 'firstDetection',
        positiveCount     = 'positiveCount',
        estimationMethod  = 'empirical',
        showBinomialModel = TRUE,
        showBootstrap     = FALSE
    )
    expect_match(res$binomialText$content, 'MODERATE HETEROGENEITY')
    # Moderate heterogeneity warns but must NOT disable the model.
    expect_no_match(res$binomialText$content, 'Binomial Model Not Applicable')

    # Above CV 0.5 the model is disabled outright.
    high <- data.frame(
        totalSamples   = total,
        firstDetection = first,
        positiveCount  = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4,
                           16, 17, 17, 18, 18, 19, 19, 19, 20, 20)
    )
    expect_gt(sd(high$positiveCount / total) / mean(high$positiveCount / total), 0.5)

    res2 <- pathsampling(
        data              = high,
        totalSamples      = 'totalSamples',
        firstDetection    = 'firstDetection',
        positiveCount     = 'positiveCount',
        estimationMethod  = 'empirical',
        showBinomialModel = TRUE,
        showBootstrap     = FALSE
    )
    expect_match(res2$binomialText$content, 'Binomial Model Not Applicable')
})

# ==============================================================================
# 4. INTEGRATION TESTS
# ==============================================================================

test_that("All fixes work together: VGAM + warnings + enforcement", {
    skip_if_not_installed("VGAM")

    # Create comprehensive test scenario
    testData <- data.frame(
        case_id = paste0("Case", 1:15),
        # Varying sample sizes (tests VGAM weighting)
        total_pop = c(rep(10, 5), rep(50, 5), rep(100, 5)),
        # High heterogeneity (tests enforcement)
        success = c(1, 1, 1, 2, 2,      # Low detection in small samples
                   5, 5, 6, 25, 24,     # Moderate in medium samples
                   80, 85, 90, 15, 20), # Bimodal in large samples
        total_samples = c(rep(10, 5), rep(50, 5), rep(100, 5)),
        positive_count = c(1, 1, 1, 2, 2,
                          5, 5, 6, 25, 24,
                          80, 85, 90, 15, 20),
        first_detection = c(5, 6, 7, 3, 4,
                           4, 5, 3, 1, 1,
                           1, 1, 1, 6, 5)
    )

    # Verify high heterogeneity
    proportions <- testData$positive_count / testData$total_samples
    cv <- sd(proportions) / mean(proportions)
    expect_gt(cv, 0.5)

    # Verify VGAM can fit this data
    betaData <- data.frame(
        success = testData$success,
        fail = testData$total_pop - testData$success
    )

    expect_silent({
        fit <- VGAM::vglm(
            cbind(success, fail) ~ 1,
            family = VGAM::betabinomial,
            data = betaData,
            trace = FALSE
        )
    })

    # Extract parameters
    coefs <- VGAM::Coef(fit)
    mu_fit <- unname(coefs[["mu"]])   # Coef() is already on the natural scale
    rho_fit <- unname(coefs[["rho"]]) # Coef() is already on the natural scale

    expect_true(is.finite(mu_fit) && mu_fit > 0 && mu_fit < 1)
    expect_true(is.finite(rho_fit) && rho_fit > 0 && rho_fit < 1)
})

# ==============================================================================
# 5. EDGE CASES
# ==============================================================================

test_that("Handles missing VGAM package gracefully", {
    # Test that code provides helpful error when VGAM not available
    # This is tested by the requireNamespace check in the code

    expect_true(TRUE)  # Placeholder - actual test requires mocking
})

test_that("Handles VGAM convergence failure gracefully", {
    # Create problematic data that might not converge
    testData <- data.frame(
        case_id = c("A", "B"),
        total_pop = c(1, 1),
        success = c(0, 1)
    )

    betaData <- data.frame(
        success = testData$success,
        fail = testData$total_pop - testData$success
    )

    # VGAM may fail to converge with only 2 points and extreme values
    # Code should catch this error and set modelRejected = TRUE

    expect_true(TRUE)  # Placeholder - actual test requires calling pathsampling
})

test_that("Zero variance detected correctly", {
    skip_if_not_installed("VGAM")

    # All identical proportions
    testData <- data.frame(
        case_id = paste0("Case", 1:5),
        total_pop = c(10, 20, 30, 40, 50),
        success = c(5, 10, 15, 20, 25)  # All p = 0.50
    )

    betaData <- data.frame(
        success = testData$success,
        fail = testData$total_pop - testData$success
    )

    fit <- VGAM::vglm(
        cbind(success, fail) ~ 1,
        family = VGAM::betabinomial,
        data = betaData,
        trace = FALSE
    )

    coefs <- VGAM::Coef(fit)
    rho_fit <- unname(coefs[["rho"]]) # Coef() is already on the natural scale

    # With no variance, rho should be very small
    expect_lt(rho_fit, 0.01)   # NB: expect_lt() takes no `info` argument
})
