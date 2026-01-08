# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: sequentialtests
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)

test_that("sequentialtests handles perfect sensitivity", {
  result <- sequentialtests(
    test1_sens = 0.99,
    test1_spec = 0.85,
    test2_sens = 0.99,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles perfect specificity", {
  result <- sequentialtests(
    test1_sens = 0.85,
    test1_spec = 0.99,
    test2_sens = 0.90,
    test2_spec = 0.99,
    prevalence = 0.10,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles poor test performance", {
  result <- sequentialtests(
    test1_sens = 0.60,
    test1_spec = 0.65,
    test2_sens = 0.60,
    test2_spec = 0.70,
    prevalence = 0.10,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles very rare disease", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.95,
    test2_sens = 0.85,
    test2_spec = 0.98,
    prevalence = 0.001,  # 0.1% prevalence
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles very common disease", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.95,  # 95% prevalence
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles minimum population size", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    population_size = 100,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles maximum population size", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    population_size = 100000,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles identical test characteristics", {
  # Both tests have same sens/spec
  result <- sequentialtests(
    test1_sens = 0.85,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.85,
    prevalence = 0.10,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests validates sensitivity bounds", {
  # Sensitivity below minimum
  expect_error(
    sequentialtests(
      test1_sens = 0.005,  # Below 0.01
      test1_spec = 0.85,
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 0.10
    ),
    regexp = "sensitivity|0.01|0.99",
    ignore.case = TRUE
  )

  # Sensitivity above maximum
  expect_error(
    sequentialtests(
      test1_sens = 1.0,  # Above 0.99
      test1_spec = 0.85,
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 0.10
    ),
    regexp = "sensitivity|0.01|0.99",
    ignore.case = TRUE
  )
})

test_that("sequentialtests validates specificity bounds", {
  # Specificity below minimum
  expect_error(
    sequentialtests(
      test1_sens = 0.85,
      test1_spec = 0.005,  # Below 0.01
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 0.10
    ),
    regexp = "specificity|0.01|0.99",
    ignore.case = TRUE
  )

  # Specificity above maximum
  expect_error(
    sequentialtests(
      test1_sens = 0.85,
      test1_spec = 1.0,  # Above 0.99
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 0.10
    ),
    regexp = "specificity|0.01|0.99",
    ignore.case = TRUE
  )
})

test_that("sequentialtests validates prevalence bounds", {
  # Prevalence below minimum
  expect_error(
    sequentialtests(
      test1_sens = 0.85,
      test1_spec = 0.85,
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 0.0005  # Below 0.001
    ),
    regexp = "prevalence|0.001|0.999",
    ignore.case = TRUE
  )

  # Prevalence above maximum
  expect_error(
    sequentialtests(
      test1_sens = 0.85,
      test1_spec = 0.85,
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 1.0  # Above 0.999
    ),
    regexp = "prevalence|0.001|0.999",
    ignore.case = TRUE
  )
})

test_that("sequentialtests handles extreme prevalence values", {
  # Near minimum
  result_low <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.95,
    test2_sens = 0.85,
    test2_spec = 0.98,
    prevalence = 0.001
  )
  expect_no_error(result_low)

  # Near maximum
  result_high <- sequentialtests(
    test1_sens = 0.85,
    test1_spec = 0.85,
    test2_sens = 0.90,
    test2_spec = 0.95,
    prevalence = 0.999
  )
  expect_no_error(result_high)
})

test_that("sequentialtests handles zero costs", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 0,
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 0,
    prevalence = 0.10,
    show_cost_analysis = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests handles very expensive tests", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 100,
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 10000,
    prevalence = 0.10,
    show_cost_analysis = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests handles all strategies with extreme values", {
  strategies <- c("serial_positive", "serial_negative", "parallel")

  for (strategy in strategies) {
    result <- sequentialtests(
      test1_sens = 0.99,
      test1_spec = 0.99,
      test2_sens = 0.99,
      test2_spec = 0.99,
      prevalence = 0.001,
      population_size = 100000,
      strategy = strategy
    )
    expect_no_error(result)
  }
})
