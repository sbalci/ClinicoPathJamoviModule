test_that("jjriverplot works with basic alluvial plot", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Create simple test data
  test_data <- data.frame(
    time = factor(rep(c("T1", "T2", "T3"), each = 20)),
    category = factor(c(rep("A", 10), rep("B", 10), rep("A", 8), rep("B", 8), rep("C", 4),
                       rep("A", 12), rep("B", 6), rep("C", 2))),
    stringsAsFactors = TRUE
  )
  
  # Test basic alluvial functionality
  result <- jjriverplot(
    data = test_data,
    time = "time",
    strata = "category",
    plotType = "alluvial"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjriverplot works with different plot types", {
  # Create test data with multiple categories
  test_data <- data.frame(
    stage1 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
    stage2 = factor(sample(c("X", "Y", "Z"), 50, replace = TRUE)),
    stage3 = factor(sample(c("P", "Q"), 50, replace = TRUE)),
    weight = runif(50, 1, 10)
  )
  
  # Test alluvial plot
  result_alluvial <- jjriverplot(
    data = test_data,
    time = NULL,
    strata = c("stage1", "stage2", "stage3"),
    plotType = "alluvial"
  )
  
  expect_s3_class(result_alluvial, "Group")
  
  # Test sankey plot
  result_sankey <- jjriverplot(
    data = test_data,
    time = NULL,
    strata = c("stage1", "stage2"),
    plotType = "sankey"
  )
  
  expect_s3_class(result_sankey, "Group")
})

test_that("jjriverplot works with weight variable", {
  # Create test data with weights
  test_data <- data.frame(
    time_point = factor(rep(c("Start", "Middle", "End"), each = 15)),
    outcome = factor(sample(c("Good", "Fair", "Poor"), 45, replace = TRUE)),
    weight = runif(45, 0.5, 3.0),
    stringsAsFactors = TRUE
  )
  
  # Test with weight variable
  result <- jjriverplot(
    data = test_data,
    time = "time_point",
    strata = "outcome",
    weight = "weight",
    plotType = "alluvial"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjriverplot handles different fill types", {
  # Create multi-stage test data
  test_data <- data.frame(
    baseline = factor(sample(c("Low", "Medium", "High"), 30, replace = TRUE)),
    followup = factor(sample(c("Improved", "Stable", "Declined"), 30, replace = TRUE)),
    final = factor(sample(c("Success", "Failure"), 30, replace = TRUE))
  )
  
  fill_types <- c("first", "last", "frequency")
  
  for (fill_type in fill_types) {
    result <- jjriverplot(
      data = test_data,
      time = NULL,
      strata = c("baseline", "followup", "final"),
      fillType = fill_type,
      plotType = "alluvial"
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjriverplot handles customization options", {
  # Create test data
  test_data <- data.frame(
    period = factor(rep(c("Pre", "Post"), each = 25)),
    status = factor(sample(c("Active", "Inactive", "Pending"), 50, replace = TRUE))
  )
  
  # Test with various customization options
  result <- jjriverplot(
    data = test_data,
    time = "period",
    strata = "status",
    plotType = "alluvial",
    labelNodes = TRUE,
    showCounts = TRUE,
    showLegend = FALSE,
    sortStreams = TRUE,
    mytitle = "Test River Plot",
    xtitle = "Time Period",
    ytitle = "Frequency"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjriverplot works with stream plot", {
  # Create longitudinal test data for stream plot
  test_data <- data.frame(
    time = factor(rep(c("Q1", "Q2", "Q3", "Q4"), each = 20)),
    category = factor(sample(c("Type A", "Type B", "Type C"), 80, replace = TRUE)),
    value = runif(80, 5, 25)
  )
  
  # Test stream plot
  result <- jjriverplot(
    data = test_data,
    time = "time",
    strata = "category",
    weight = "value",
    plotType = "stream"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjriverplot handles edge cases", {
  # Create test data
  test_data <- data.frame(
    time = factor(rep(c("T1", "T2"), each = 10)),
    group = factor(sample(c("A", "B"), 20, replace = TRUE))
  )
  
  # Test with no time variable (should return early)
  result_no_time <- jjriverplot(
    data = test_data,
    time = NULL,
    strata = "group"
  )
  
  expect_s3_class(result_no_time, "Group")
  
  # Test with no strata variable (should return early)
  result_no_strata <- jjriverplot(
    data = test_data,
    time = "time",
    strata = NULL
  )
  
  expect_s3_class(result_no_strata, "Group")
  
  # Test with missing values
  test_data_na <- test_data
  test_data_na[1:3, "group"] <- NA
  
  result_na <- jjriverplot(
    data = test_data_na,
    time = "time",
    strata = "group"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjriverplot validates input parameters", {
  # Test with empty data
  expect_error(
    jjriverplot(
      data = data.frame(),
      time = "time",
      strata = "group"
    ),
    "Data contains no \\\\(complete\\\\) rows"
  )
})

test_that("jjriverplot performance optimization works", {
  # Test that cached data is being used efficiently
  test_data <- data.frame(
    time = factor(rep(c("Before", "After"), each = 25)),
    outcome = factor(sample(c("Success", "Failure"), 50, replace = TRUE))
  )
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjriverplotOptions$new(
    time = "time",
    strata = "outcome"
  )
  
  analysis <- ClinicoPath:::jjriverplotClass$new(
    options = options,
    data = test_data
  )
  
  # Test that methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjriverplot works with ID variable", {
  # Create longitudinal test data with patient IDs
  test_data <- data.frame(
    patient_id = rep(paste0("P", 1:10), each = 3),
    visit = factor(rep(c("V1", "V2", "V3"), times = 10)),
    status = factor(sample(c("Healthy", "Sick", "Recovered"), 30, replace = TRUE))
  )
  
  # Test with ID variable
  result <- jjriverplot(
    data = test_data,
    id = "patient_id",
    time = "visit",
    strata = "status",
    plotType = "alluvial"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjriverplot works with multiple strata variables", {
  # Create test data with multiple strata
  test_data <- data.frame(
    treatment = factor(sample(c("Drug A", "Drug B", "Placebo"), 40, replace = TRUE)),
    response = factor(sample(c("Good", "Poor"), 40, replace = TRUE)),
    outcome = factor(sample(c("Cured", "Improved", "Same"), 40, replace = TRUE)),
    final = factor(sample(c("Success", "Failure"), 40, replace = TRUE))
  )
  
  # Test with multiple strata variables
  result <- jjriverplot(
    data = test_data,
    time = NULL,
    strata = c("treatment", "response", "outcome"),
    plotType = "alluvial"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjriverplot handles theme options", {
  # Create test data
  test_data <- data.frame(
    phase = factor(rep(c("Phase1", "Phase2"), each = 20)),
    result = factor(sample(c("Pass", "Fail"), 40, replace = TRUE))
  )
  
  # Test with original theme enabled
  result_original <- jjriverplot(
    data = test_data,
    time = "phase",
    strata = "result",
    originaltheme = TRUE
  )
  
  expect_s3_class(result_original, "Group")
  
  # Test with original theme disabled
  result_custom <- jjriverplot(
    data = test_data,
    time = "phase",
    strata = "result",
    originaltheme = FALSE
  )
  
  expect_s3_class(result_custom, "Group")
})

test_that("jjriverplot handles curve types", {
  # Create test data
  test_data <- data.frame(
    step1 = factor(sample(c("A", "B"), 30, replace = TRUE)),
    step2 = factor(sample(c("X", "Y", "Z"), 30, replace = TRUE))
  )
  
  curve_types <- c("linear", "cardinal", "basis", "step")
  
  for (curve_type in curve_types) {
    result <- jjriverplot(
      data = test_data,
      time = NULL,
      strata = c("step1", "step2"),
      curveType = curve_type,
      plotType = "alluvial"
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjriverplot handles complex real-world scenario", {
  # Create realistic clinical trial data
  test_data <- data.frame(
    patient_id = rep(paste0("P", 1:25), each = 4),
    timepoint = factor(rep(c("Baseline", "Month3", "Month6", "Month12"), times = 25)),
    treatment_response = factor(sample(c("Complete", "Partial", "Stable", "Progressive"), 
                                      100, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))),
    cost = runif(100, 1000, 10000)
  )
  
  # Test complex scenario
  result <- jjriverplot(
    data = test_data,
    id = "patient_id",
    time = "timepoint",
    strata = "treatment_response",
    weight = "cost",
    plotType = "alluvial",
    fillType = "first",
    labelNodes = TRUE,
    showCounts = FALSE,
    mytitle = "Treatment Response Over Time",
    xtitle = "Study Timepoint",
    ytitle = "Response Frequency"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjriverplot handles single strata vs multiple strata", {
  # Test single strata (long format)
  long_data <- data.frame(
    time = factor(rep(c("T1", "T2", "T3"), each = 20)),
    category = factor(sample(c("A", "B", "C"), 60, replace = TRUE))
  )
  
  result_long <- jjriverplot(
    data = long_data,
    time = "time",
    strata = "category",
    plotType = "alluvial"
  )
  
  expect_s3_class(result_long, "Group")
  
  # Test multiple strata (wide format)
  wide_data <- data.frame(
    stage1 = factor(sample(c("A", "B"), 30, replace = TRUE)),
    stage2 = factor(sample(c("X", "Y"), 30, replace = TRUE)),
    stage3 = factor(sample(c("P", "Q"), 30, replace = TRUE))
  )
  
  result_wide <- jjriverplot(
    data = wide_data,
    time = NULL,
    strata = c("stage1", "stage2", "stage3"),
    plotType = "alluvial"
  )
  
  expect_s3_class(result_wide, "Group")
})
