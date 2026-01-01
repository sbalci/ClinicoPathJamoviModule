testthat::test_that("riverplot works with basic functionality", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create test data for longitudinal analysis
  test_data_long <- data.frame(
    time = factor(rep(c("Baseline", "Month3", "Month6"), each = 30)),
    response = factor(sample(c("Complete", "Partial", "Stable", "Progressive"), 
                            90, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2))),
    patient_id = rep(paste0("P", 1:30), times = 3),
    cost = runif(90, 1000, 10000),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Basic alluvial plot
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_long,
      time = "time",
      strata = "response",
      plotType = "alluvial"
    )
  })
  
  # Test 2: Alluvial plot with weight
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_long,
      time = "time",
      strata = "response",
      weight = "cost",
      plotType = "alluvial"
    )
  })
  
  # Test 3: Alluvial plot with ID variable
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_long,
      id = "patient_id",
      time = "time",
      strata = "response",
      plotType = "alluvial"
    )
  })
})

testthat::test_that("riverplot works with wide format data", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create test data in wide format
  test_data_wide <- data.frame(
    stage1 = factor(sample(c("Low", "Medium", "High"), 50, replace = TRUE)),
    stage2 = factor(sample(c("Improved", "Stable", "Declined"), 50, replace = TRUE)),
    stage3 = factor(sample(c("Success", "Failure"), 50, replace = TRUE)),
    weight = runif(50, 1, 10),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Multiple strata without weight
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_wide,
      strata = c("stage1", "stage2", "stage3"),
      plotType = "alluvial"
    )
  })
  
  # Test 2: Multiple strata with weight
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_wide,
      strata = c("stage1", "stage2", "stage3"),
      weight = "weight",
      plotType = "alluvial"
    )
  })
  
  # Test 3: Two strata variables
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_wide,
      strata = c("stage1", "stage2"),
      plotType = "alluvial"
    )
  })
})

testthat::test_that("riverplot works with different plot types", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create test data
  test_data <- data.frame(
    baseline = factor(sample(c("A", "B", "C"), 40, replace = TRUE)),
    followup = factor(sample(c("X", "Y", "Z"), 40, replace = TRUE)),
    final = factor(sample(c("P", "Q"), 40, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Test Sankey plot
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data,
      strata = c("baseline", "followup"),
      plotType = "sankey"
    )
  })
  
  # Test Stream plot with long format data
  stream_data <- data.frame(
    time = factor(rep(c("Q1", "Q2", "Q3", "Q4"), each = 25)),
    category = factor(sample(c("Type A", "Type B", "Type C"), 100, replace = TRUE)),
    value = runif(100, 1, 20),
    stringsAsFactors = FALSE
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = stream_data,
      time = "time",
      strata = "category",
      weight = "value",
      plotType = "stream"
    )
  })
  
  # Test Stream plot fallback with multiple strata (should warn and use alluvial)
  testthat::expect_warning({
    result <- ClinicoPath::riverplot(
      data = test_data,
      strata = c("baseline", "followup"),
      plotType = "stream"
    )
  }, "Stream plot with multiple strata variables requires data reshaping. Falling back to alluvial plot.")
})

testthat::test_that("riverplot works with customization options", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create test data
  test_data <- data.frame(
    time = factor(rep(c("Pre", "Post"), each = 30)),
    outcome = factor(sample(c("Good", "Fair", "Poor"), 60, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Test with different fill types
  fill_types <- c("first", "last", "frequency")
  
  for (fill_type in fill_types) {
    testthat::expect_no_error({
      result <- ClinicoPath::riverplot(
        data = test_data,
        time = "time",
        strata = "outcome",
        fillType = fill_type
      )
    })
  }
  
  # Test with different curve types
  curve_types <- c("linear", "cardinal", "basis", "step")
  
  for (curve_type in curve_types) {
    testthat::expect_no_error({
      result <- ClinicoPath::riverplot(
        data = test_data,
        time = "time",
        strata = "outcome",
        curveType = curve_type
      )
    })
  }
  
  # Test with customization options
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data,
      time = "time",
      strata = "outcome",
      labelNodes = TRUE,
      showCounts = TRUE,
      showLegend = FALSE,
      sortStreams = TRUE,
      mytitle = "Test River Plot",
      xtitle = "Time Period",
      ytitle = "Frequency"
    )
  })
  
  # Test with original theme
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data,
      time = "time",
      strata = "outcome",
      originaltheme = TRUE
    )
  })
})

testthat::test_that("riverplot handles edge cases", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create test data with edge cases
  test_data <- data.frame(
    time = factor(rep(c("T1", "T2"), each = 20)),
    group = factor(sample(c("A", "B"), 40, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Test with missing values
  test_data_na <- test_data
  test_data_na[1:5, "group"] <- NA
  
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data_na,
      time = "time",
      strata = "group"
    )
  })
  
  # Test with single category
  single_cat_data <- data.frame(
    time = factor(rep(c("Before", "After"), each = 10)),
    outcome = factor(rep("Success", 20)),
    stringsAsFactors = FALSE
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = single_cat_data,
      time = "time",
      strata = "outcome"
    )
  })
  
  # Test error handling - empty data
  testthat::expect_error({
    ClinicoPath::riverplot(
      data = data.frame(),
      time = "time",
      strata = "group"
    )
  })
  
  # Test error handling - non-existent columns
  testthat::expect_error({
    ClinicoPath::riverplot(
      data = test_data,
      time = "nonexistent",
      strata = "group"
    )
  })
})

testthat::test_that("riverplot results structure", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create simple test data
  test_data <- data.frame(
    period = factor(rep(c("Start", "End"), each = 15)),
    status = factor(sample(c("Active", "Inactive"), 30, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Run analysis
  result <- ClinicoPath::riverplot(
    data = test_data,
    time = "period",
    strata = "status"
  )
  
  # Test that result has expected structure
  testthat::expect_true("todo" %in% names(result))
  testthat::expect_true("plot" %in% names(result))
  
  # Test result class
  testthat::expect_s3_class(result, "Group")
})

testthat::test_that("riverplot handles complex real-world scenarios", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create realistic clinical trial data
  n_patients <- 50
  n_timepoints <- 4
  
  clinical_data <- data.frame(
    patient_id = rep(paste0("PT", sprintf("%03d", 1:n_patients)), each = n_timepoints),
    timepoint = factor(rep(c("Screening", "Month1", "Month3", "Month6"), times = n_patients)),
    response = factor(sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 
                            n_patients * n_timepoints, replace = TRUE, 
                            prob = c(0.25, 0.35, 0.25, 0.15))),
    treatment_cost = runif(n_patients * n_timepoints, 5000, 50000),
    stringsAsFactors = FALSE
  )
  
  # Test comprehensive scenario
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = clinical_data,
      id = "patient_id",
      time = "timepoint",
      strata = "response",
      weight = "treatment_cost",
      plotType = "alluvial",
      fillType = "first",
      labelNodes = TRUE,
      showCounts = FALSE,
      showLegend = TRUE,
      mytitle = "Clinical Trial Response Flow",
      xtitle = "Study Timepoint",
      ytitle = "Response Frequency"
    )
  })
})

testthat::test_that("riverplot performance with large datasets", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create larger dataset
  large_data <- data.frame(
    time = factor(rep(c("T1", "T2", "T3", "T4"), each = 250)),
    category = factor(sample(c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5"), 1000, replace = TRUE)),
    weight = runif(1000, 1, 100),
    stringsAsFactors = FALSE
  )
  
  # Test performance - should complete without timeout
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = large_data,
      time = "time",
      strata = "category",
      weight = "weight",
      plotType = "alluvial"
    )
  })
})

testthat::test_that("riverplot input validation", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  testthat::skip_if_not_installed("ggalluvial")
  testthat::skip_if_not_installed("dplyr")
  
  # Create test data
  test_data <- data.frame(
    time = factor(rep(c("Before", "After"), each = 20)),
    group = factor(sample(c("A", "B", "C"), 40, replace = TRUE)),
    weight = runif(40, 1, 10),
    stringsAsFactors = FALSE
  )
  
  # Test invalid plot type (should default to alluvial)
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data,
      time = "time",
      strata = "group",
      plotType = "invalid_type"  # Should default to alluvial
    )
  })
  
  # Test invalid fill type (should default to first)
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data,
      time = "time",
      strata = "group",
      fillType = "invalid_fill"  # Should default to first
    )
  })
  
  # Test invalid curve type (should default to cardinal)
  testthat::expect_no_error({
    result <- ClinicoPath::riverplot(
      data = test_data,
      time = "time",
      strata = "group",
      curveType = "invalid_curve"  # Should default to cardinal
    )
  })
})
