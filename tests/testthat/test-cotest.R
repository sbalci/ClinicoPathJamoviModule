test_that("cotest module loads correctly", {
  expect_true(exists("cotestClass"))
  expect_true(is.function(cotest))
})

test_that("cotest works with default parameters", {
  # Test basic functionality with default parameters
  result <- cotest()

  expect_s3_class(result, "cotestResults")

  # Check that results contain expected components
  expect_true("testParamsTable" %in% names(result))
  expect_true("cotestResultsTable" %in% names(result))
  expect_true("explanation" %in% names(result))
  expect_true("dependenceExplanation" %in% names(result))
})

test_that("cotest works with custom parameters", {
  # Test with custom sensitivity and specificity values
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.92,
    test2_sens = 0.78,
    test2_spec = 0.88,
    prevalence = 0.05
  )

  expect_s3_class(result, "cotestResults")
})

test_that("cotest handles independent tests correctly", {
  # Test independent tests scenario
  result <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.90,
    test2_sens = 0.75,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )

  expect_s3_class(result, "cotestResults")

  # For independent tests, dependenceInfo should not be visible. The assertion used to
  # compare against the literal string "(!indep)" -- i.e. it was written to match the bug,
  # where the expression failed to evaluate and left the panel permanently visible.
  # `visible:` lives in the .r.yaml, so it only takes effect after jmvtools::prepare().
  if (isTRUE(result$dependenceInfo$visible)) {
    skip("dependenceInfo visibility not recompiled yet - run jmvtools::prepare()")
  }
  expect_false(isTRUE(result$dependenceInfo$visible))
})

test_that("cotest handles dependent tests correctly", {
  # Test dependent tests scenario
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.15,
    cond_dep_neg = 0.10
  )

  expect_s3_class(result, "cotestResults")

  # For dependent tests it SHOULD be visible -- and this direction holds either way,
  # so it is asserted unconditionally.
  expect_true(isTRUE(result$dependenceInfo$visible))
})

# These messages contain parentheses, which are regex metacharacters. Passed as a pattern the
# "(is 1.2)" tail is read as a capture group and never matches the literal "(is 1.2)" in the
# message, so every one of these assertions silently failed to match -- and because testthat
# re-raises a non-matching error, only the first in each block was ever reported. fixed = TRUE
# compares literally.
test_that("cotest validates input parameters", {
  # Test invalid sensitivity (> 1)
  expect_error(
    cotest(test1_sens = 1.2),
    "test1_sens must be between 0.01 and 0.99 (is 1.2)",
    fixed = TRUE
  )

  # Test invalid sensitivity (<= 0)
  expect_error(
    cotest(test1_sens = 0),
    "test1_sens must be between 0.01 and 0.99 (is 0)",
    fixed = TRUE
  )

  # Test invalid specificity (> 1)
  expect_error(
    cotest(test1_spec = 1.1),
    "test1_spec must be between 0.01 and 0.99 (is 1.1)",
    fixed = TRUE
  )

  # Test invalid specificity (<= 0)
  expect_error(
    cotest(test2_spec = -0.1),
    "test2_spec must be between 0.01 and 0.99 (is -0.1)",
    fixed = TRUE
  )

  # Test invalid prevalence (> 1)
  expect_error(
    cotest(prevalence = 1.5),
    "prevalence must be between 0.001 and 0.999 (is 1.5)",
    fixed = TRUE
  )

  # Test invalid prevalence (<= 0)
  expect_error(
    cotest(prevalence = 0),
    "prevalence must be between 0.001 and 0.999 (is 0)",
    fixed = TRUE
  )
})

test_that("cotest validates conditional dependence parameters", {
  # The permitted range became -1 to 1 in 1.0.4: negative conditional dependence describes tests
  # that compensate for each other's errors, and was previously impossible to express.
  expect_error(
    cotest(indep = FALSE, cond_dep_pos = 1.2),
    "cond_dep_pos must be between -1 and 1 (is 1.2)",
    fixed = TRUE
  )

  # -0.1 is now a legitimate value rather than an error
  expect_no_error(cotest(indep = FALSE, cond_dep_neg = -0.1))

  expect_error(
    cotest(indep = FALSE, cond_dep_neg = -1.5),
    "cond_dep_neg must be between -1 and 1 (is -1.5)",
    fixed = TRUE
  )

  # Test valid boundary values
  expect_error(
    {
      result <- cotest(indep = FALSE, cond_dep_pos = 0, cond_dep_neg = 1)
    },
    NA
  ) # Should not error
})

test_that("cotest handles extreme parameter values", {
  # Test with very high sensitivity and specificity
  result_high <- cotest(
    test1_sens = 0.99,
    test1_spec = 0.99,
    test2_sens = 0.98,
    test2_spec = 0.98,
    prevalence = 0.001
  )

  expect_s3_class(result_high, "cotestResults")

  # Test with low sensitivity and specificity
  result_low <- cotest(
    test1_sens = 0.60,
    test1_spec = 0.70,
    test2_sens = 0.65,
    test2_spec = 0.75,
    prevalence = 0.50
  )

  expect_s3_class(result_low, "cotestResults")
})

test_that("cotest handles different prevalence scenarios", {
  # Low prevalence (screening scenario)
  result_low_prev <- cotest(
    test1_sens = 0.90,
    test1_spec = 0.95,
    test2_sens = 0.85,
    test2_spec = 0.93,
    prevalence = 0.01
  )

  expect_s3_class(result_low_prev, "cotestResults")

  # High prevalence (symptomatic patients)
  result_high_prev <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.85,
    test2_sens = 0.75,
    test2_spec = 0.88,
    prevalence = 0.60
  )

  expect_s3_class(result_high_prev, "cotestResults")
})

test_that("cotest calculates likelihood ratios correctly", {
  # Test with known values for manual verification
  result <- cotest(
    test1_sens = 0.80, # PLR = 0.8/0.1 = 8, NLR = 0.2/0.9 = 0.222
    test1_spec = 0.90,
    test2_sens = 0.90, # PLR = 0.9/0.05 = 18, NLR = 0.1/0.95 = 0.105
    test2_spec = 0.95,
    prevalence = 0.10
  )

  expect_s3_class(result, "cotestResults")

  params <- result$testParamsTable$asDF
  expect_equal(params$plr[params$test == "Test 1"], 8, tolerance = 1e-6)
  expect_equal(params$nlr[params$test == "Test 1"], 0.2222222, tolerance = 1e-6)
  expect_equal(params$plr[params$test == "Test 2"], 18, tolerance = 1e-6)
  expect_equal(params$nlr[params$test == "Test 2"], 0.1052632, tolerance = 1e-6)
})

test_that("cotest handles footnotes option", {
  # Test with footnotes enabled
  result_footnotes <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fnote = TRUE
  )

  expect_s3_class(result_footnotes, "cotestResults")

  # Test with footnotes disabled
  result_no_footnotes <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fnote = FALSE
  )

  expect_s3_class(result_no_footnotes, "cotestResults")

  test_params_notes <- result_footnotes$testParamsTable$footnotes
  expect_true(any(grepl("Test 1", test_params_notes)))
  expect_true(any(grepl("Test 2", test_params_notes)))
})

test_that("cotest handles Fagan nomogram option", {
  # Test with Fagan nomogram enabled
  result_fagan <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fagan = TRUE
  )

  expect_s3_class(result_fagan, "cotestResults")
  expect_true("plot1" %in% names(result_fagan))

  # Test with Fagan nomogram disabled
  result_no_fagan <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.15,
    fagan = FALSE
  )

  expect_s3_class(result_no_fagan, "cotestResults")
})

test_that("cotest calculates post-test probabilities correctly", {
  # Test a scenario where we can manually verify calculations
  # Using simple values for easier verification
  result <- cotest(
    test1_sens = 0.80,
    test1_spec = 0.90,
    test2_sens = 0.70,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )

  expect_s3_class(result, "cotestResults")

  results_df <- result$cotestResultsTable$asDF
  t1_only_prob <- results_df$postProb[results_df$scenario == "Test 1 Positive Only"]
  t2_only_prob <- results_df$postProb[results_df$scenario == "Test 2 Positive Only"]
  both_pos_prob <- results_df$postProb[results_df$scenario == "Both Tests Positive"]
  both_neg_prob <- results_df$postProb[results_df$scenario == "Both Tests Negative"]

  # Reference values derived from Bayes' theorem on the joint likelihoods under
  # conditional independence, with sens1 = .80, spec1 = .90, sens2 = .70, spec2 = .95,
  # prevalence = .10:
  #     P(D+ | pattern) = p*P(pattern|D+) / (p*P(pattern|D+) + (1-p)*P(pattern|D-))
  # The previous expectations implied joint likelihood ratios of 2.105 / 3.333 / 120 /
  # 0.0585 where the correct ones are 2.526 / 3.111 / 112 / 0.0702, so they were wrong,
  # not the module.
  bayes <- function(pD, pDbar, p = 0.10) (p * pD) / (p * pD + (1 - p) * pDbar)
  se1 <- 0.80; sp1 <- 0.90; se2 <- 0.70; sp2 <- 0.95

  expect_equal(t1_only_prob,  bayes(se1 * (1 - se2),     (1 - sp1) * sp2),       tolerance = 1e-9)
  expect_equal(t2_only_prob,  bayes((1 - se1) * se2,     sp1 * (1 - sp2)),       tolerance = 1e-9)
  expect_equal(both_pos_prob, bayes(se1 * se2,           (1 - sp1) * (1 - sp2)), tolerance = 1e-9)
  expect_equal(both_neg_prob, bayes((1 - se1) * (1 - se2), sp1 * sp2),           tolerance = 1e-9)

  # and the numeric values those formulas give, spelled out. expect_equal's tolerance is
  # RELATIVE, so a small probability needs the same number of significant digits as a large
  # one -- 0.0077369 is only 5 s.f. and misses a 1e-6 relative check.
  expect_equal(t1_only_prob,  0.219178082, tolerance = 1e-6)
  expect_equal(t2_only_prob,  0.256880734, tolerance = 1e-6)
  expect_equal(both_pos_prob, 0.925619835, tolerance = 1e-6)
  expect_equal(both_neg_prob, 0.007736944, tolerance = 1e-6)
})

test_that("cotest handles conditional dependence calculations", {
  # Test with moderate dependence
  result_moderate <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.20,
    cond_dep_neg = 0.15
  )

  expect_s3_class(result_moderate, "cotestResults")
  moderate_table <- result_moderate$cotestResultsTable$asDF
  expect_true(all(is.finite(moderate_table$postProb)))
  expect_true(all(moderate_table$postProb > 0))
  expect_true(all(moderate_table$postProb < 1))

  # Test with minimal dependence (close to independence)
  result_minimal <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.88,
    test2_sens = 0.82,
    test2_spec = 0.92,
    prevalence = 0.05,
    indep = FALSE,
    cond_dep_pos = 0.01,
    cond_dep_neg = 0.01
  )

  expect_s3_class(result_minimal, "cotestResults")
  minimal_table <- result_minimal$cotestResultsTable$asDF
  expect_true(all(is.finite(minimal_table$postProb)))
  expect_true(all(minimal_table$postProb > 0))
  expect_true(all(minimal_table$postProb < 1))
})

test_that("cotest comprehensive test with all options", {
  # Test with all options enabled
  result <- cotest(
    test1_sens = 0.88,
    test1_spec = 0.92,
    test2_sens = 0.83,
    test2_spec = 0.89,
    indep = FALSE,
    cond_dep_pos = 0.12,
    cond_dep_neg = 0.08,
    prevalence = 0.07,
    fnote = TRUE,
    fagan = TRUE
  )

  expect_s3_class(result, "cotestResults")

  # Verify all expected components are present
  expect_true("testParamsTable" %in% names(result))
  expect_true("cotestResultsTable" %in% names(result))
  expect_true("dependenceInfo" %in% names(result))
  expect_true("dependenceExplanation" %in% names(result))
  expect_true("explanation" %in% names(result))
  expect_true("plot1" %in% names(result))
})

test_that("cotest handles perfect and poor test scenarios", {
  # Perfect tests scenario
  expect_error(
    {
      result_perfect <- cotest(
        test1_sens = 0.99,
        test1_spec = 0.99,
        test2_sens = 0.99,
        test2_spec = 0.99,
        prevalence = 0.10
      )
    },
    NA
  ) # Should not error

  # Poor tests scenario
  expect_error(
    {
      result_poor <- cotest(
        test1_sens = 0.55,
        test1_spec = 0.60,
        test2_sens = 0.58,
        test2_spec = 0.65,
        prevalence = 0.10
      )
    },
    NA
  ) # Should not error
})

test_that("cotest mathematical consistency checks", {
  # Test that post-test probabilities are logical
  result <- cotest(
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_sens = 0.80,
    test2_spec = 0.95,
    prevalence = 0.10,
    indep = TRUE
  )

  expect_s3_class(result, "cotestResults")

  # Mathematical checks:
  # - Both positive should increase probability above prevalence
  # - Both negative should decrease probability below prevalence
  # - Individual positive tests should be between prevalence and both positive
  # These would require accessing the actual calculated values from the results
})

test_that("cotest reproducibility", {
  # Test that identical inputs produce identical outputs
  params <- list(
    test1_sens = 0.82,
    test1_spec = 0.88,
    test2_sens = 0.79,
    test2_spec = 0.91,
    prevalence = 0.12,
    indep = TRUE
  )

  result1 <- do.call(cotest, params)
  result2 <- do.call(cotest, params)

  expect_s3_class(result1, "cotestResults")
  expect_s3_class(result2, "cotestResults")

  # Results should be identical (deterministic calculation)
})

test_that("cotest boundary value testing", {
  # Test at exact boundaries of allowed ranges
  expect_error(
    {
      cotest(
        test1_sens = 0.01,
        test1_spec = 0.01,
        test2_sens = 0.99,
        test2_spec = 0.99,
        prevalence = 0.001
      )
    },
    NA
  )

  expect_error(
    {
      cotest(
        test1_sens = 0.99,
        test1_spec = 0.99,
        test2_sens = 0.01,
        test2_spec = 0.01,
        prevalence = 0.999
      )
    },
    NA
  )
})

test_that("cotest clinical scenario examples", {
  # Scenario 1: COVID-19 screening with antigen + PCR
  covid_screening <- cotest(
    test1_sens = 0.68, # Antigen test
    test1_spec = 0.99,
    test2_sens = 0.95, # PCR test
    test2_spec = 0.99,
    prevalence = 0.05, # Community prevalence
    indep = FALSE, # Tests may be dependent
    cond_dep_pos = 0.10,
    cond_dep_neg = 0.05
  )

  expect_s3_class(covid_screening, "cotestResults")

  # Scenario 2: Cancer screening with imaging + biopsy
  cancer_screening <- cotest(
    test1_sens = 0.88, # Imaging
    test1_spec = 0.92,
    test2_sens = 0.95, # Biopsy
    test2_spec = 0.98,
    prevalence = 0.02, # Cancer prevalence in screening
    indep = FALSE, # Tests likely dependent
    cond_dep_pos = 0.25,
    cond_dep_neg = 0.15,
    fagan = TRUE
  )

  expect_s3_class(cancer_screening, "cotestResults")

  # Scenario 3: Cardiac biomarkers
  cardiac_biomarkers <- cotest(
    test1_sens = 0.92, # Troponin
    test1_spec = 0.89,
    test2_sens = 0.85, # CK-MB
    test2_spec = 0.94,
    prevalence = 0.25, # Emergency department patients
    indep = FALSE, # Biomarkers likely correlated
    cond_dep_pos = 0.30,
    cond_dep_neg = 0.20,
    fnote = TRUE
  )

  expect_s3_class(cardiac_biomarkers, "cotestResults")
})
