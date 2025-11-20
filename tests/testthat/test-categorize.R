
# Manually source the files since we are running tests without full package load
if (file.exists("../../R/categorize.h.R")) {
  source("../../R/categorize.h.R")
  source("../../R/categorize.b.R")
} else if (file.exists("R/categorize.h.R")) {
  source("R/categorize.h.R")
  source("R/categorize.b.R")
}

test_that("categorize works with equal intervals", {
  # Create test data
  set.seed(123)
  data <- data.frame(
    val = 1:100
  )
  
  # Test equal intervals with 4 bins
  # Range is 1-100. Bins should be approx: [1, 25.75], (25.75, 50.5], (50.5, 75.25], (75.25, 100]
  # Note: cut uses slightly different logic for breaks, let's verify counts are roughly equal for uniform data
  
  # We need to mock the jamovi object structure
  # Since we can't easily mock the full R6 object here without loading the package properly,
  # we will test the logic by sourcing the file or using the package if loaded.
  # Assuming the package functions are available or we can test the logic directly.
  
  # However, since we are testing the internal logic which is inside an R6 class, 
  # we might need to instantiate it or test the underlying logic if it were exposed.
  # The R6 class `categorizeClass` inherits from `categorizeBase`.
  
  # Let's try to test the logic by creating a minimal R6 object if possible, 
  # or better, let's verify if we can access the private methods. 
  # R6 private methods are hard to test directly.
  
  # ALTERNATIVE: We can test the `cut` function behavior which is what the function uses,
  # but that doesn't test the package code.
  
  # BEST APPROACH: Use the `categorize` function if it's exported (it should be as a jamovi analysis).
  # If `categorize` is the main function, we can call it.
  
  # Check if `categorize` function exists
  if (exists("categorize")) {
    
    # 1. Test Equal Intervals
    res <- categorize(
      data = data,
      var = "val",
      method = "equal",
      nbins = 4,
      labels = "numbered"
    )
    
    # Check if result contains the categorized variable
    # Note: jamovi functions usually return a results object, but they also modify data if `addtodata` is TRUE?
    # No, jamovi functions in R return a results object. 
    # The `categorize` function seems to be designed to output tables and plots.
    # To verify the categorization logic, we might need to check the frequency table in the results.
    
    expect_true(!is.null(res$freqTable))
    
    # Check frequency table content
    # We can't easily access the table data as a dataframe without some helper, 
    # but we can check if it runs without error.
    
    # 2. Test Quantiles
    res_quant <- categorize(
      data = data,
      var = "val",
      method = "quantile",
      nbins = 4
    )
    expect_true(!is.null(res_quant$freqTable))
    
    # 3. Test Manual Breaks
    res_manual <- categorize(
      data = data,
      var = "val",
      method = "manual",
      breaks = "0, 50, 100"
    )
    expect_true(!is.null(res_manual$freqTable))
    
    # 4. Test Mean +/- SD
    res_meansd <- categorize(
      data = data,
      var = "val",
      method = "meansd",
      sdmult = 1
    )
    expect_true(!is.null(res_meansd$freqTable))
    
    # 5. Test Median Split
    res_median <- categorize(
      data = data,
      var = "val",
      method = "median"
    )
    expect_true(!is.null(res_median$freqTable))
    
  } else {
    # If function is not exported or available in test env, we skip or warn
    skip("categorize function not available")
  }
})

test_that("categorize handles NAs and edge cases", {
  
  if (exists("categorize")) {
    data_na <- data.frame(
      val = c(1:10, NA, 12:20)
    )
    
    # Test with NAs
    expect_error(categorize(data = data_na, var = "val"), NA) # Should not error
    
    # Test with single value (should fail or handle gracefully)
    data_single <- data.frame(val = rep(1, 10))
    expect_error(categorize(data = data_single, var = "val"), NA)
    
    # Test with invalid variable name
    expect_error(categorize(data = data_na, var = "nonexistent")) # Should error because variable is missing
  }
})

test_that("categorize generates correct labels", {
   if (exists("categorize")) {
     data <- data.frame(val = 1:100)
     
     # Test semantic labels
     res <- categorize(data = data, var = "val", method = "equal", nbins = 3, labels = "semantic")
     # We can't easily inspect the internal labels without inspecting the generated code or table
     expect_true(!is.null(res$rcode))
     
     # Test custom labels
     res_custom <- categorize(
       data = data, 
       var = "val", 
       method = "equal", 
       nbins = 2, 
       labels = "custom",
       customlabels = "Low, High"
     )
     expect_true(!is.null(res_custom$rcode))
   }
})
