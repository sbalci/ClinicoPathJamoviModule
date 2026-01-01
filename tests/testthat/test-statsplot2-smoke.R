
context("StatsPlot2 Smoke Test")

test_that("Type inference works correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Mock data
  df <- data.frame(
    y_cont = rnorm(50),
    y_cat_small = rep(1:3, length.out=50), # Integer 1-3
    y_cat_factor = factor(rep(c("A","B"), 25)),
    group_factor = factor(rep(c("G1","G2"), 25))
  )

  # Initialize module
  module <- statsplot2(
    data = df,
    dep = "y_cat_small",
    group = "group_factor"
  )

  # Check explanation for "bar chart" or "categorical" to confirm inference
  # Content is in module$ExplanationMessage$content
  
  exp_msg <- module$ExplanationMessage$content
  expect_match(exp_msg, "bar chart", label = "Small integer should be inferred as factor (bar chart)")
})

test_that("Sampling notice is generated for large datasets", {
  # Create large dataset > 10000 rows
  df_large <- data.frame(
    y = rnorm(10005),
    g = factor(rep(c("A","B"), length.out=10005))
  )
  
  module <- statsplot2(
    data = df_large,
    dep = "y",
    group = "g",
    sampleLarge = TRUE
  )
  
  # Check if "Observations used: 5,000" is in success message
  success_note <- module$get("analysisComplete")
  expect_true(!is.null(success_note), "Success notice should exist")
  expect_match(success_note$content, "Observations used: 5,000")
})

test_that("NA exclusion notice works", {
  df_na <- data.frame(
    y = c(rnorm(10), NA, NA),
    g = factor(rep(c("A","B"), 6))
  )
  
  module <- statsplot2(
    data = df_na,
    dep = "y",
    group = "g",
    excl = TRUE
  )
  
  # Success notice should show dropped count or used count
  success_note <- module$get("analysisComplete")
  expect_true(!is.null(success_note), "Success notice should exist")
  expect_match(success_note$content, "Observations used: 10")
})

test_that("Alluvial notice appears when not applicable", {
  df <- data.frame(
    y = rnorm(20),
    g = factor(rep(c("A","B"), 10))
  )
  
  # Independent Factor vs Continuous (Violin) -> Alluvial NOT applicable
  module <- statsplot2(
    data = df,
    dep = "y",
    group = "g",
    direction = "independent",
    alluvsty = "t1" 
  )
  
  # Check for notice 'alluvialNotApplicable'
  notice <- module$get("alluvialNotApplicable")
  expect_true(!is.null(notice), "Should have alluvial not applicable notice")
})
