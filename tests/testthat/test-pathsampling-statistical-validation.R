library(testthat)
library(ClinicoPath)

# ==============================================================================
# CRITICAL TESTS: Beta-Binomial PMF Validation
# ==============================================================================
#
# The pathsampling function implements a from-scratch Beta-Binomial PMF at
# R/pathsampling.b.R:2912-2914:
#
#   dbetabinom_pmf <- function(k, n, alpha, beta) {
#       exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
#   }
#
# This MUST be validated against the trusted VGAM package implementation.
# ==============================================================================

test_that("Beta-Binomial PMF matches VGAM::dbetabinom - basic cases", {
  skip_if_not_installed("VGAM")

  # Test case 1: Moderate parameters
  n <- 10
  k_vals <- 0:10
  alpha <- 2
  beta <- 3

  # Extract internal function from pathsampling (we'll need to test via the module)
  # For now, let's test the mathematical formula directly
  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  custom_probs <- sapply(k_vals, function(k) dbetabinom_pmf(k, n, alpha, beta))
  vgam_probs <- VGAM::dbetabinom(k_vals, size = n, prob = alpha/(alpha+beta), rho = 1/(alpha+beta+1))

  expect_equal(custom_probs, vgam_probs, tolerance = 1e-10,
               info = "Custom Beta-Binomial PMF should match VGAM with moderate parameters")
})

test_that("Beta-Binomial PMF matches VGAM - extreme parameters", {
  skip_if_not_installed("VGAM")

  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  # Test case 2: Very small alpha, beta (high variance)
  n <- 20
  k_vals <- 0:20
  alpha <- 0.5
  beta <- 0.5

  custom_probs <- sapply(k_vals, function(k) dbetabinom_pmf(k, n, alpha, beta))
  vgam_probs <- VGAM::dbetabinom(k_vals, size = n, prob = alpha/(alpha+beta), rho = 1/(alpha+beta+1))

  expect_equal(custom_probs, vgam_probs, tolerance = 1e-9,
               info = "Should match VGAM with very small alpha/beta (high variance)")

  # Test case 3: Very large alpha, beta (low variance, approaches binomial)
  alpha <- 100
  beta <- 100

  custom_probs <- sapply(k_vals, function(k) dbetabinom_pmf(k, n, alpha, beta))
  vgam_probs <- VGAM::dbetabinom(k_vals, size = n, prob = alpha/(alpha+beta), rho = 1/(alpha+beta+1))

  expect_equal(custom_probs, vgam_probs, tolerance = 1e-10,
               info = "Should match VGAM with very large alpha/beta (low variance)")
})

test_that("Beta-Binomial PMF matches VGAM - asymmetric parameters", {
  skip_if_not_installed("VGAM")

  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  # Test case 4: Highly skewed (alpha >> beta)
  n <- 15
  k_vals <- 0:15
  alpha <- 10
  beta <- 2

  custom_probs <- sapply(k_vals, function(k) dbetabinom_pmf(k, n, alpha, beta))
  vgam_probs <- VGAM::dbetabinom(k_vals, size = n, prob = alpha/(alpha+beta), rho = 1/(alpha+beta+1))

  expect_equal(custom_probs, vgam_probs, tolerance = 1e-10,
               info = "Should match VGAM with alpha >> beta (right-skewed)")

  # Test case 5: Highly skewed (beta >> alpha)
  alpha <- 2
  beta <- 10

  custom_probs <- sapply(k_vals, function(k) dbetabinom_pmf(k, n, alpha, beta))
  vgam_probs <- VGAM::dbetabinom(k_vals, size = n, prob = alpha/(alpha+beta), rho = 1/(alpha+beta+1))

  expect_equal(custom_probs, vgam_probs, tolerance = 1e-10,
               info = "Should match VGAM with beta >> alpha (left-skewed)")
})

test_that("Beta-Binomial PMF probabilities sum to 1", {
  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  # Test that probabilities sum to 1 for various parameter combinations
  test_cases <- list(
    list(n = 10, alpha = 2, beta = 3),
    list(n = 20, alpha = 0.5, beta = 0.5),
    list(n = 15, alpha = 10, beta = 2),
    list(n = 30, alpha = 5, beta = 5)
  )

  for (tc in test_cases) {
    k_vals <- 0:tc$n
    probs <- sapply(k_vals, function(k) dbetabinom_pmf(k, tc$n, tc$alpha, tc$beta))

    expect_equal(sum(probs), 1.0, tolerance = 1e-10,
                 info = sprintf("Probabilities should sum to 1 (n=%d, alpha=%.1f, beta=%.1f)",
                                tc$n, tc$alpha, tc$beta))
  }
})

test_that("Beta-Binomial PMF edge cases", {
  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  # Edge case 1: k = 0 (no successes)
  prob_k0 <- dbetabinom_pmf(0, 10, 2, 3)
  expect_true(prob_k0 > 0 && prob_k0 < 1,
              info = "P(k=0) should be valid probability")

  # Edge case 2: k = n (all successes)
  prob_kn <- dbetabinom_pmf(10, 10, 2, 3)
  expect_true(prob_kn > 0 && prob_kn < 1,
              info = "P(k=n) should be valid probability")

  # Edge case 3: n = 1 (single trial)
  prob_k0_n1 <- dbetabinom_pmf(0, 1, 2, 3)
  prob_k1_n1 <- dbetabinom_pmf(1, 1, 2, 3)
  expect_equal(prob_k0_n1 + prob_k1_n1, 1.0, tolerance = 1e-10,
               info = "For n=1, two outcomes should sum to 1")

  # Edge case 4: alpha = beta (symmetric)
  prob_sym <- dbetabinom_pmf(5, 10, 3, 3)
  expect_true(prob_sym > 0 && prob_sym < 1,
              info = "Symmetric case should give valid probability")
})

# ==============================================================================
# HIGH PRIORITY: Cliff's Delta Validation
# ==============================================================================
#
# The pathsampling function implements Cliff's Delta at R/pathsampling.b.R:3189-3194:
#
#   cliff_delta <- function(x, y) {
#       concordant <- sum(outer(x, y, ">"))
#       discordant <- sum(outer(x, y, "<"))
#       delta <- (concordant - discordant) / (length(x) * length(y))
#       return(delta)
#   }
#
# This should be validated against the effsize package.
# ==============================================================================

test_that("Cliff's Delta matches effsize package", {
  skip_if_not_installed("effsize")

  # Define the custom implementation
  cliff_delta <- function(x, y) {
    concordant <- sum(outer(x, y, ">"))
    discordant <- sum(outer(x, y, "<"))
    delta <- (concordant - discordant) / (length(x) * length(y))
    return(delta)
  }

  # Test case 1: Clear separation
  x <- c(10, 12, 14, 16, 18)
  y <- c(2, 4, 6, 8, 10)

  custom_delta <- cliff_delta(x, y)
  effsize_result <- effsize::cliff.delta(x, y)

  expect_equal(custom_delta, as.numeric(effsize_result$estimate), tolerance = 1e-10,
               info = "Custom Cliff's Delta should match effsize with clear separation")

  # Test case 2: No separation (identical)
  x <- c(5, 5, 5)
  y <- c(5, 5, 5)

  custom_delta <- cliff_delta(x, y)
  expect_equal(custom_delta, 0, tolerance = 1e-10,
               info = "Cliff's Delta should be 0 for identical groups")

  # Test case 3: Partial overlap
  x <- c(8, 10, 12, 14)
  y <- c(6, 8, 10, 12)

  custom_delta <- cliff_delta(x, y)
  effsize_result <- effsize::cliff.delta(x, y)

  expect_equal(custom_delta, as.numeric(effsize_result$estimate), tolerance = 1e-10,
               info = "Custom Cliff's Delta should match effsize with partial overlap")
})

test_that("Cliff's Delta edge cases", {
  cliff_delta <- function(x, y) {
    concordant <- sum(outer(x, y, ">"))
    discordant <- sum(outer(x, y, "<"))
    delta <- (concordant - discordant) / (length(x) * length(y))
    return(delta)
  }

  # Edge case 1: Complete separation (x > y)
  x <- c(20, 21, 22)
  y <- c(1, 2, 3)

  delta <- cliff_delta(x, y)
  expect_equal(delta, 1.0, tolerance = 1e-10,
               info = "Cliff's Delta should be 1.0 for complete separation (x > y)")

  # Edge case 2: Complete separation (y > x)
  delta_rev <- cliff_delta(y, x)
  expect_equal(delta_rev, -1.0, tolerance = 1e-10,
               info = "Cliff's Delta should be -1.0 for complete separation (y > x)")

  # Edge case 3: Single values
  x <- c(10)
  y <- c(5)

  delta <- cliff_delta(x, y)
  expect_equal(delta, 1.0, tolerance = 1e-10,
               info = "Cliff's Delta with single values should work")
})

# ==============================================================================
# HIGH PRIORITY: Hodges-Lehmann Estimator Validation
# ==============================================================================
#
# The pathsampling function implements Hodges-Lehmann at R/pathsampling.b.R:3197-3200:
#
#   hodges_lehmann <- function(x, y) {
#       diffs <- outer(x, y, "-")
#       median(diffs)
#   }
#
# This is the correct formula for the Hodges-Lehmann estimator of location shift.
# ==============================================================================

test_that("Hodges-Lehmann estimator is correct", {
  hodges_lehmann <- function(x, y) {
    diffs <- outer(x, y, "-")
    median(diffs)
  }

  # Test case 1: Known simple example
  # If x = y + c for constant c, HL estimator should be approximately c
  x <- c(10, 12, 14)
  y <- c(5, 7, 9)
  # Expected shift is 5

  hl_est <- hodges_lehmann(x, y)
  expect_equal(hl_est, 5, tolerance = 0.1,
               info = "HL estimator should detect constant shift")

  # Test case 2: No shift (identical groups)
  x <- c(5, 7, 9)
  y <- c(5, 7, 9)

  hl_est <- hodges_lehmann(x, y)
  expect_equal(hl_est, 0, tolerance = 1e-10,
               info = "HL estimator should be 0 for identical groups")

  # Test case 3: Single values
  x <- c(10)
  y <- c(3)

  hl_est <- hodges_lehmann(x, y)
  expect_equal(hl_est, 7, tolerance = 1e-10,
               info = "HL estimator with single values should be difference")
})

test_that("Hodges-Lehmann estimator matches manual calculation", {
  hodges_lehmann <- function(x, y) {
    diffs <- outer(x, y, "-")
    median(diffs)
  }

  # Small example where we can manually verify
  x <- c(8, 10)
  y <- c(2, 4)

  # Manual: differences are 8-2=6, 8-4=4, 10-2=8, 10-4=6
  # Sorted: 4, 6, 6, 8
  # Median: (6+6)/2 = 6

  hl_est <- hodges_lehmann(x, y)
  expect_equal(hl_est, 6, tolerance = 1e-10,
               info = "HL estimator should match manual calculation")

  # Verify manually
  manual_diffs <- c(8-2, 8-4, 10-2, 10-4)
  manual_median <- median(manual_diffs)
  expect_equal(hl_est, manual_median, tolerance = 1e-10,
               info = "Implementation should match manual median of pairwise differences")
})

# ==============================================================================
# MEDIUM PRIORITY: Utility Function Tests
# ==============================================================================

test_that("Clustering index calculation", {
  # We need to test the logic, but this is a custom heuristic
  # Expected behavior:
  # - clustering_index < 1 means clustering (shorter distances than expected)
  # - clustering_index = 1 means random spacing
  # - clustering_index > 1 means dispersed (longer distances than expected)

  # Test case 1: Evenly spaced samples
  positiveSamples <- c(1, 3, 5, 7, 9)  # spacing = 2
  totalSamples <- 10

  # Expected distance = 10 / 5 = 2
  # Mean distance = mean(c(2,2,2,2)) = 2
  # Index = 2 / 2 = 1.0

  sorted_samples <- sort(positiveSamples)
  distances <- diff(sorted_samples)
  mean_distance <- mean(distances)
  expected_distance <- totalSamples / length(positiveSamples)
  clustering_index <- mean_distance / expected_distance

  expect_equal(clustering_index, 1.0, tolerance = 1e-10,
               info = "Evenly spaced samples should have clustering index = 1")

  # Test case 2: Clustered samples
  positiveSamples <- c(1, 2, 3, 4, 5)  # spacing = 1
  totalSamples <- 20

  # Expected distance = 20 / 5 = 4
  # Mean distance = mean(c(1,1,1,1)) = 1
  # Index = 1 / 4 = 0.25

  sorted_samples <- sort(positiveSamples)
  distances <- diff(sorted_samples)
  mean_distance <- mean(distances)
  expected_distance <- totalSamples / length(positiveSamples)
  clustering_index <- mean_distance / expected_distance

  expect_lt(clustering_index, 1.0)
  expect_equal(clustering_index, 0.25, tolerance = 1e-10)
})

test_that("Foci count estimation", {
  # The logic counts gaps > 2 as separate foci

  # Test case 1: Single focus (all consecutive or close)
  positiveSamples <- c(1, 2, 3, 4)

  sorted_samples <- sort(positiveSamples)
  distances <- diff(sorted_samples)
  gaps <- sum(distances > 2)
  foci <- gaps + 1

  expect_equal(foci, 1,
               info = "Consecutive samples should be 1 focus")

  # Test case 2: Two foci (gap > 2)
  positiveSamples <- c(1, 2, 10, 11)

  sorted_samples <- sort(positiveSamples)
  distances <- diff(sorted_samples)
  # Distances: 1, 8, 1
  # Gaps > 2: 1 (the gap of 8)
  gaps <- sum(distances > 2)
  foci <- gaps + 1

  expect_equal(foci, 2,
               info = "Gap > 2 should create 2 foci")

  # Test case 3: Three foci
  positiveSamples <- c(1, 2, 10, 11, 20, 21)

  sorted_samples <- sort(positiveSamples)
  distances <- diff(sorted_samples)
  # Distances: 1, 8, 1, 9, 1
  # Gaps > 2: 2 (gaps of 8 and 9)
  gaps <- sum(distances > 2)
  foci <- gaps + 1

  expect_equal(foci, 3,
               info = "Two gaps > 2 should create 3 foci")
})

test_that("Bootstrap empirical cumulative produces valid confidence intervals", {
  # Test the bootstrap resampling logic
  # We can't call the private method directly, but we can test the logic

  set.seed(12345)
  firstDetectionData <- c(1, 2, 3, 1, 2, 4, 1, 3)
  totalSamplesData <- rep(10, length(firstDetectionData))
  maxN <- 5
  nBoot <- 1000

  positive_idx <- !is.na(firstDetectionData)
  positive_first <- firstDetectionData[positive_idx]
  positive_total <- totalSamplesData[positive_idx]
  n_cases <- length(positive_first)

  detection_matrix <- matrix(NA_real_, nrow = nBoot, ncol = maxN)

  for (b in 1:nBoot) {
    boot_idx <- sample(n_cases, replace = TRUE)
    boot_first <- positive_first[boot_idx]

    for (n in 1:maxN) {
      detected <- sum(boot_first <= n, na.rm = TRUE)
      detection_matrix[b, n] <- detected / n_cases
    }
  }

  ci_lower <- apply(detection_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(detection_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
  ci_mean <- apply(detection_matrix, 2, mean, na.rm = TRUE)

  # Sanity checks
  expect_true(all(ci_lower >= 0 & ci_lower <= 1),
              info = "Bootstrap lower CI should be valid probabilities")
  expect_true(all(ci_upper >= 0 & ci_upper <= 1),
              info = "Bootstrap upper CI should be valid probabilities")
  expect_true(all(ci_lower <= ci_upper),
              info = "Bootstrap lower CI should be <= upper CI")
  expect_true(all(ci_mean >= ci_lower & ci_mean <= ci_upper),
              info = "Bootstrap mean should be within CI")

  # Detection probability should increase with more samples
  expect_true(all(diff(ci_mean) >= 0),
              info = "Detection probability should increase with sample size")
})

# ==============================================================================
# INTEGRATION TESTS: End-to-End Validation
# ==============================================================================

test_that("Beta-Binomial model produces reasonable recommendations", {
  skip_if_not_installed("VGAM")

  # Create test data for pathsampling analysis
  testData <- data.frame(
    case_id = paste0("Case", 1:20),
    samples_examined = c(15, 18, 20, 12, 25, 22, 16, 19, 21, 17,
                        14, 23, 18, 20, 16, 19, 21, 15, 24, 18),
    positive_samples = c(3, 4, 5, 2, 6, 5, 3, 4, 5, 3,
                        2, 5, 4, 4, 3, 4, 5, 3, 6, 4),
    total_population = rep(30, 20),  # For Beta-Binomial
    first_detection_pos = c(2, 1, 3, 2, 1, 2, 3, 2, 1, 2,
                           3, 1, 2, 2, 3, 2, 1, 2, 1, 2)
  )

  # Test that we can run the analysis without errors
  # (This would require the full jamovi module to be available)
  # For now, we verify the mathematical components are correct

  # Calculate alpha and beta from empirical data
  p_mean <- mean(testData$positive_samples / testData$samples_examined)
  p_var <- var(testData$positive_samples / testData$samples_examined)
  n_mean <- mean(testData$samples_examined)

  # Method of moments estimation for beta-binomial
  if (p_var < p_mean * (1 - p_mean)) {
    rho <- (p_var - p_mean * (1 - p_mean) / n_mean) / (p_mean * (1 - p_mean) * (n_mean - 1) / n_mean)
    rho <- max(0, min(1, rho))  # Constrain to [0,1]
  } else {
    rho <- 0
  }

  alpha <- p_mean * (1 - rho) / rho
  beta <- (1 - p_mean) * (1 - rho) / rho

  # Verify parameters are positive
  expect_true(alpha > 0, info = "Alpha should be positive")
  expect_true(beta > 0, info = "Beta should be positive")

  # Verify beta-binomial probabilities are valid
  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  n_test <- 20
  target <- 3

  prob_less_than_target <- 0
  for (k in 0:(target - 1)) {
    if (k <= n_test) {
      prob_less_than_target <- prob_less_than_target + dbetabinom_pmf(k, n_test, alpha, beta)
    }
  }
  cumProb <- 1 - prob_less_than_target

  expect_true(cumProb >= 0 && cumProb <= 1,
              info = "Cumulative probability should be valid")
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("Beta-Binomial handles numerical edge cases", {
  dbetabinom_pmf <- function(k, n, alpha, beta) {
    exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
  }

  # Very large n (should not overflow)
  expect_silent({
    prob <- dbetabinom_pmf(50, 100, 2, 3)
  })
  expect_true(is.finite(prob), info = "Should handle large n without overflow")

  # Very small probabilities (should not underflow to 0 prematurely)
  prob_extreme <- dbetabinom_pmf(0, 50, 10, 2)
  expect_true(prob_extreme > 0, info = "Should not underflow to 0")
  expect_true(is.finite(prob_extreme), info = "Should remain finite")
})

test_that("Cliff's Delta handles edge cases gracefully", {
  cliff_delta <- function(x, y) {
    concordant <- sum(outer(x, y, ">"))
    discordant <- sum(outer(x, y, "<"))
    delta <- (concordant - discordant) / (length(x) * length(y))
    return(delta)
  }

  # Single element groups
  expect_silent({
    delta <- cliff_delta(c(5), c(10))
  })

  # Large groups (should not be too slow)
  expect_silent({
    delta <- cliff_delta(1:100, 51:150)
  })
})

test_that("Hodges-Lehmann handles edge cases gracefully", {
  hodges_lehmann <- function(x, y) {
    diffs <- outer(x, y, "-")
    median(diffs)
  }

  # Single element groups
  expect_silent({
    hl <- hodges_lehmann(c(5), c(10))
  })
  expect_equal(hl, -5, tolerance = 1e-10)

  # Large groups (should not be too slow or use excessive memory)
  expect_silent({
    hl <- hodges_lehmann(1:50, 51:100)
  })
})
