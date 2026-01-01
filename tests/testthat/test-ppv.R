test_that("ppv function exists and basic functionality works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test function existence
  expect_true(exists("ppv"))
  expect_true(is.function(ppv))
  
  # Test with default parameters - should not error
  expect_no_error({
    result <- ppv()
  })
  
  # Check result structure
  result <- ppv()
  expect_s3_class(result, "ppvResults")
  expect_true("confusion" %in% names(result))
  expect_true("ppv" %in% names(result))
  expect_true("dotPlot" %in% names(result))
})

test_that("ppv calculations are correct with known values", {
  # Test case with 50% prior, 0.05 alpha, 0.8 power, no hacking
  # Expected PPV ≈ 94.1%
  result <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.8,
    percHack = 0
  )
  
  # Extract PPV value from HTML content
  ppv_content <- result$ppv$content
  ppv_match <- regmatches(ppv_content, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content))
  ppv_value <- as.numeric(gsub("[^0-9.]", "", ppv_match))
  
  # Check PPV is approximately correct (allowing for rounding)
  expect_true(ppv_value > 93 && ppv_value < 95)
})

test_that("ppv handles extreme parameter values correctly", {
  # Test with very low prior probability
  result_low_prior <- ppv(
    percTrue = 1,  # Only 1% true hypotheses
    alpha = 0.05,
    power = 0.8,
    percHack = 0
  )
  
  # PPV should be low when prior is very low
  ppv_content <- result_low_prior$ppv$content
  ppv_match <- regmatches(ppv_content, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content))
  ppv_value <- as.numeric(gsub("[^0-9.]", "", ppv_match))
  expect_true(ppv_value < 20)  # Should be around 13.8%
  
  # Test with very high prior probability
  result_high_prior <- ppv(
    percTrue = 90,  # 90% true hypotheses
    alpha = 0.05,
    power = 0.8,
    percHack = 0
  )
  
  # PPV should be high when prior is high
  ppv_content <- result_high_prior$ppv$content
  ppv_match <- regmatches(ppv_content, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content))
  ppv_value <- as.numeric(gsub("[^0-9.]", "", ppv_match))
  expect_true(ppv_value > 98)  # Should be around 99.3%
})

test_that("ppv handles p-hacking correctly", {
  # Test without p-hacking
  result_no_hack <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.8,
    percHack = 0
  )
  
  # Test with p-hacking
  result_with_hack <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.8,
    percHack = 50  # 50% p-hacked studies
  )
  
  # PPV should be lower with p-hacking
  ppv_content_no_hack <- result_no_hack$ppv$content
  ppv_match_no_hack <- regmatches(ppv_content_no_hack, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content_no_hack))
  ppv_value_no_hack <- as.numeric(gsub("[^0-9.]", "", ppv_match_no_hack))
  
  ppv_content_hack <- result_with_hack$ppv$content
  ppv_match_hack <- regmatches(ppv_content_hack, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content_hack))
  ppv_value_hack <- as.numeric(gsub("[^0-9.]", "", ppv_match_hack))
  
  expect_true(ppv_value_hack < ppv_value_no_hack)
})

test_that("ppv confusion table has correct structure", {
  result <- ppv()
  
  # Check confusion table exists and has data
  expect_true(!is.null(result$confusion))
  expect_true(result$confusion$rowCount == 1)
  
  # Check table columns exist
  columns <- result$confusion$columns
  expect_true(length(columns) > 0)
})

test_that("ppv dotPlot is generated correctly", {
  result <- ppv()
  
  # Check that dotPlot exists
  expect_true(!is.null(result$dotPlot))
  
  # Check plot properties
  expect_equal(result$dotPlot$width, 550)
  expect_equal(result$dotPlot$height, 400)
})

test_that("ppv handles different power levels correctly", {
  # Low power
  result_low_power <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.2,  # Very low power
    percHack = 0
  )
  
  # High power
  result_high_power <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.95,  # Very high power
    percHack = 0
  )
  
  # PPV should be higher with higher power
  ppv_content_low <- result_low_power$ppv$content
  ppv_match_low <- regmatches(ppv_content_low, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content_low))
  ppv_value_low <- as.numeric(gsub("[^0-9.]", "", ppv_match_low))
  
  ppv_content_high <- result_high_power$ppv$content
  ppv_match_high <- regmatches(ppv_content_high, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content_high))
  ppv_value_high <- as.numeric(gsub("[^0-9.]", "", ppv_match_high))
  
  expect_true(ppv_value_high > ppv_value_low)
})

test_that("ppv handles different alpha levels correctly", {
  # Standard alpha
  result_standard <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.8,
    percHack = 0
  )
  
  # Stringent alpha
  result_stringent <- ppv(
    percTrue = 50,
    alpha = 0.01,  # More stringent
    power = 0.8,
    percHack = 0
  )
  
  # PPV should be higher with more stringent alpha
  ppv_content_standard <- result_standard$ppv$content
  ppv_match_standard <- regmatches(ppv_content_standard, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content_standard))
  ppv_value_standard <- as.numeric(gsub("[^0-9.]", "", ppv_match_standard))
  
  ppv_content_stringent <- result_stringent$ppv$content
  ppv_match_stringent <- regmatches(ppv_content_stringent, regexpr("PPV\\)</b>: [0-9.]+%", ppv_content_stringent))
  ppv_value_stringent <- as.numeric(gsub("[^0-9.]", "", ppv_match_stringent))
  
  expect_true(ppv_value_stringent > ppv_value_standard)
})

test_that("ppv input validation works", {
  # Test invalid percTrue = 0
  expect_error({
    ppv(percTrue = 0)
  }, "Percentage of true hypotheses cannot be 0%")
  
  # Test invalid combination: percTrue = 100 with percHack > 0
  expect_error({
    ppv(percTrue = 100, percHack = 10)
  }, "Cannot have p-hacking when all hypotheses are true")
})

test_that("ppv FDR calculations are correct", {
  result <- ppv(
    percTrue = 50,
    alpha = 0.05,
    power = 0.8,
    percHack = 0
  )
  
  # Extract PPV and FDR values
  content <- result$ppv$content
  ppv_match <- regmatches(content, regexpr("PPV\\)</b>: [0-9.]+%", content))
  ppv_value <- as.numeric(gsub("[^0-9.]", "", ppv_match))
  
  fdr_match <- regmatches(content, regexpr("FDR\\)</b>: [0-9.]+%", content))
  fdr_value <- as.numeric(gsub("[^0-9.]", "", fdr_match))
  
  # PPV + FDR should equal 100%
  expect_equal(ppv_value + fdr_value, 100, tolerance = 0.1)
})

test_that("ppv produces reasonable results for common scenarios", {
  # Scenario 1: Exploratory genomics research
  result_genomics <- ppv(
    percTrue = 1,      # Very few true associations (1%)
    alpha = 0.05,
    power = 0.6,       # Often underpowered
    percHack = 20      # Some bias
  )
  
  # Scenario 2: Confirmatory clinical trial
  result_clinical <- ppv(
    percTrue = 70,     # Strong prior evidence
    alpha = 0.05,
    power = 0.9,       # Well-powered
    percHack = 5       # Minimal bias
  )
  
  # Scenario 3: Psychology research
  result_psych <- ppv(
    percTrue = 30,     # Moderate priors
    alpha = 0.05,
    power = 0.35,      # Often underpowered
    percHack = 40      # Significant QRPs
  )
  
  # All should produce valid results
  expect_s3_class(result_genomics, "ppvResults")
  expect_s3_class(result_clinical, "ppvResults")
  expect_s3_class(result_psych, "ppvResults")
})
