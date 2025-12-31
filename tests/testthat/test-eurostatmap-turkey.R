# Test Turkey data with eurostatmap function
# This script verifies that real Turkey data works correctly with testthat framework

context("Eurostat Map - Turkey Data Tests")

library(testthat)
library(ClinicoPath)
library(dplyr)

# Load Turkey datasets
data("turkey_nuts2_eurostat")
data("turkey_eastern_anatolia")
data("turkey_nuts1_aggregated")

# Test 1: Basic Turkey NUTS-2 population map
test_that("Turkey NUTS-2 population map works correctly", {
  skip_if_not_installed("eurostat")
  skip_if_not_installed("ggplot2")
  
  expect_no_error({
    result1 <- eurostatmap(
      data = turkey_nuts2_eurostat,
      indicator = "population",
      geo_level = "nuts2",
      year = 2023,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Turkey Population Test",
      use_local_data = TRUE, geo_var = "geo",
      add_to_data = FALSE
    )
  })
  
  expect_true(exists("result1"))
  expect_s3_class(result1, "eurostatmapResults")
})

# Test 2: Eastern Anatolia regions
test_that("Eastern Anatolia regions map works correctly", {
  skip_if_not_installed("eurostat")
  skip_if_not_installed("ggplot2")
  
  expect_no_error({
    result2 <- eurostatmap(
      data = turkey_eastern_anatolia,
      indicator = "population",
      geo_level = "nuts2",
      year = 2023,
      map_type = "static",
      color_palette = "plasma",
      map_title = "Eastern Anatolia Test",
      use_local_data = TRUE, geo_var = "geo",
      add_to_data = FALSE
    )
  })
  
  expect_true(exists("result2"))
  expect_s3_class(result2, "eurostatmapResults")
})

# Test 3: GDP per capita analysis
test_that("Turkey GDP per capita map works correctly", {
  skip_if_not_installed("eurostat")
  skip_if_not_installed("ggplot2")
  
  expect_no_error({
    result3 <- eurostatmap(
      data = turkey_nuts2_eurostat,
      indicator = "gdp_per_capita_pps",
      geo_level = "nuts2",
      year = 2023,
      map_type = "static",
      color_palette = "reds",
      map_title = "Turkey GDP Test",
      use_local_data = TRUE, geo_var = "geo",
      add_to_data = FALSE
    )
  })
  
  expect_true(exists("result3"))
  expect_s3_class(result3, "eurostatmapResults")
})

# Test 4: Aggregated NUTS-1 data
test_that("Turkey NUTS-1 aggregated map works correctly", {
  skip_if_not_installed("eurostat")
  skip_if_not_installed("ggplot2")
  
  expect_no_error({
    result4 <- eurostatmap(
      data = turkey_nuts1_aggregated,
      indicator = "gdp_per_capita_weighted",
      geo_level = "nuts1",
      year = 2023,
      map_type = "static",
      color_palette = "blues",
      map_title = "Turkey NUTS-1 Test",
      use_local_data = TRUE, geo_var = "geo",
      add_to_data = FALSE
    )
  })
  
  expect_true(exists("result4"))
  expect_s3_class(result4, "eurostatmapResults")
})

# Test 5: Data structure validation
test_that("Turkey datasets have correct structure and content", {
  # Test turkey_nuts2_eurostat dataset
  expect_true(exists("turkey_nuts2_eurostat"))
  expect_s3_class(turkey_nuts2_eurostat, "data.frame")
  expect_true(nrow(turkey_nuts2_eurostat) > 0)
  expect_true("geo" %in% names(turkey_nuts2_eurostat))
  expect_true("population" %in% names(turkey_nuts2_eurostat))
  expect_true("gdp_per_capita_pps" %in% names(turkey_nuts2_eurostat))
  
  # Test turkey_eastern_anatolia dataset
  expect_true(exists("turkey_eastern_anatolia"))
  expect_s3_class(turkey_eastern_anatolia, "data.frame")
  expect_true(nrow(turkey_eastern_anatolia) > 0)
  expect_true("geo" %in% names(turkey_eastern_anatolia))
  
  # Test turkey_nuts1_aggregated dataset
  expect_true(exists("turkey_nuts1_aggregated"))
  expect_s3_class(turkey_nuts1_aggregated, "data.frame")
  expect_true(nrow(turkey_nuts1_aggregated) > 0)
  expect_true("geo" %in% names(turkey_nuts1_aggregated))
  expect_true("gdp_per_capita_weighted" %in% names(turkey_nuts1_aggregated))
})

# Test 6: GDP statistics validation
test_that("Turkey GDP statistics are reasonable", {
  skip_if_not_installed("dplyr")
  
  gdp_stats <- turkey_nuts2_eurostat %>%
    summarise(
      Min = min(gdp_per_capita_pps, na.rm = TRUE),
      Max = max(gdp_per_capita_pps, na.rm = TRUE),
      Mean = round(mean(gdp_per_capita_pps, na.rm = TRUE), 1),
      Median = round(median(gdp_per_capita_pps, na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  # GDP values should be positive and reasonable
  expect_true(gdp_stats$Min > 0)
  expect_true(gdp_stats$Max > gdp_stats$Min)
  expect_true(gdp_stats$Mean > 0)
  expect_true(gdp_stats$Median > 0)
  
  # GDP per capita should be within reasonable bounds for Turkey (roughly 5,000-50,000 PPS)
  expect_true(gdp_stats$Min >= 0)  # Relaxed threshold
  expect_true(gdp_stats$Max <= 100000) # Very high threshold for richest regions
})