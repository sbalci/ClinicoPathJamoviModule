context("Eurostat Map - eurostatmap")

# Test data preparation
set.seed(12345)

# Create simple test data for eurostatmap
test_eurostat_data <- data.frame(
  geo = c("AT", "BE", "DE", "FR", "IT", "ES", "NL", "PL", "SE", "DK"),
  country_name = c("Austria", "Belgium", "Germany", "France", "Italy", 
                   "Spain", "Netherlands", "Poland", "Sweden", "Denmark"),
  life_expectancy = c(81.2, 81.9, 80.8, 82.3, 82.7, 83.1, 82.1, 77.5, 82.4, 80.6),
  population_density = c(109, 383, 240, 119, 206, 94, 518, 124, 25, 137),
  gdp_per_capita = c(48.1, 46.4, 46.3, 39.0, 31.0, 27.1, 52.3, 15.4, 51.9, 60.2),
  healthcare_expenditure = c(10.3, 10.7, 11.1, 11.3, 9.0, 9.2, 10.1, 6.5, 10.9, 10.1),
  TIME_PERIOD = rep(2022, 10)
)

# Create comprehensive test for jamovi functionality
test_that("eurostatmap function works with local data", {
  # Test with complete parameter set
  result <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = TRUE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result))
})

test_that("eurostatmap function works with different indicators", {
  # Test with population density
  result1 <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "population_density",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "blues",
      map_title = "Population Density in Europe",
      use_local_data = TRUE,
      classification_method = "equal",
      n_classes = 4,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result1))
  
  # Test with GDP per capita
  result2 <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "gdp_per_capita",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "greens",
      map_title = "GDP per Capita in Europe",
      use_local_data = TRUE,
      classification_method = "jenks",
      n_classes = 6,
      cache_data = TRUE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result2))
})

test_that("eurostatmap function works with different color palettes", {
  # Test different color palettes
  palettes <- c("viridis", "plasma", "blues", "reds", "greens")
  
  for (palette in palettes) {
    result <- expect_silent(
      eurostatmap(
        data = test_eurostat_data,
        indicator = "healthcare_expenditure",
        geo_level = "nuts0",
        year = 2022,
        map_type = "static",
        color_palette = palette,
        map_title = paste("Healthcare Expenditure -", palette),
        use_local_data = TRUE,
        classification_method = "quantile",
        n_classes = 5,
        cache_data = FALSE,
        add_to_data = FALSE
      )
    )
    expect_true(is.list(result))
  }
})

test_that("eurostatmap function works with different classification methods", {
  # Test different classification methods
  methods <- c("quantile", "equal", "jenks", "pretty")
  
  for (method in methods) {
    result <- expect_silent(
      eurostatmap(
        data = test_eurostat_data,
        indicator = "life_expectancy",
        geo_level = "nuts0",
        year = 2022,
        map_type = "static",
        color_palette = "viridis",
        map_title = paste("Life Expectancy -", method),
        use_local_data = TRUE,
        classification_method = method,
        n_classes = 5,
        cache_data = FALSE,
        add_to_data = FALSE
      )
    )
    expect_true(is.list(result))
  }
})

test_that("eurostatmap function works with different geographic levels", {
  # Test different geographic levels
  levels <- c("nuts0", "nuts1", "nuts2", "nuts3")
  
  for (level in levels) {
    result <- expect_silent(
      eurostatmap(
        data = test_eurostat_data,
        indicator = "life_expectancy",
        geo_level = level,
        year = 2022,
        map_type = "static",
        color_palette = "viridis",
        map_title = paste("Life Expectancy -", level),
        use_local_data = TRUE,
        classification_method = "quantile",
        n_classes = 5,
        cache_data = FALSE,
        add_to_data = FALSE
      )
    )
    expect_true(is.list(result))
  }
})

test_that("eurostatmap function works with different number of classes", {
  # Test different number of classes
  n_classes_options <- c(3, 4, 5, 6, 7, 8, 9, 10)
  
  for (n_classes in n_classes_options) {
    result <- expect_silent(
      eurostatmap(
        data = test_eurostat_data,
        indicator = "life_expectancy",
        geo_level = "nuts0",
        year = 2022,
        map_type = "static",
        color_palette = "viridis",
        map_title = paste("Life Expectancy -", n_classes, "classes"),
        use_local_data = TRUE,
        classification_method = "quantile",
        n_classes = n_classes,
        cache_data = FALSE,
        add_to_data = FALSE
      )
    )
    expect_true(is.list(result))
  }
})

test_that("eurostatmap function handles missing geo column appropriately", {
  # Test with data missing geo column
  data_no_geo <- test_eurostat_data
  data_no_geo$geo <- NULL
  
  # This should produce an error or warning
  expect_error(
    eurostatmap(
      data = data_no_geo,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
})

test_that("eurostatmap function handles missing indicator appropriately", {
  # Test with missing indicator
  expect_error(
    eurostatmap(
      data = test_eurostat_data,
      indicator = NULL,
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
})

test_that("eurostatmap function works with realistic eurostat data", {
  # Test with eurostat_health_data if available
  if (exists("eurostat_health_data")) {
    result <- expect_silent(
      eurostatmap(
        data = eurostat_health_data,
        indicator = "life_expectancy",
        geo_level = "nuts0",
        year = 2022,
        map_type = "static",
        color_palette = "viridis",
        map_title = "Life Expectancy in Europe",
        use_local_data = TRUE,
        classification_method = "quantile",
        n_classes = 5,
        cache_data = FALSE,
        add_to_data = FALSE
      )
    )
    expect_true(is.list(result))
  }
})

# Test parameter validation
test_that("eurostatmap function validates parameters correctly", {
  # Test invalid geographic level
  expect_error(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "invalid_level",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  
  # Test invalid color palette
  expect_error(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "invalid_palette",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  
  # Test invalid classification method
  expect_error(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "invalid_method",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  
  # Test invalid number of classes (too few)
  expect_error(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 2,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  
  # Test invalid number of classes (too many)
  expect_error(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy in Europe",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 15,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
})

# Test edge cases
test_that("eurostatmap function handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    geo = c("DE", "FR"),
    indicator_value = c(80.5, 82.1),
    TIME_PERIOD = c(2022, 2022)
  )
  
  result <- expect_silent(
    eurostatmap(
      data = minimal_data,
      indicator = "indicator_value",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Minimal Test",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 3,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result))
  
  # Test with empty data
  empty_data <- data.frame(
    geo = character(0),
    indicator_value = numeric(0),
    TIME_PERIOD = numeric(0)
  )
  
  expect_error(
    eurostatmap(
      data = empty_data,
      indicator = "indicator_value",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Empty Test",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
})

# Test with different map types
test_that("eurostatmap function works with different map types", {
  # Test static map
  result_static <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy - Static",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result_static))
  
  # Test interactive map
  result_interactive <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "interactive",
      color_palette = "viridis",
      map_title = "Life Expectancy - Interactive",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result_interactive))
})

# Test caching functionality
test_that("eurostatmap function caching works", {
  # Test with caching enabled
  result_cached <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy - Cached",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = TRUE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result_cached))
  
  # Test with caching disabled
  result_no_cache <- expect_silent(
    eurostatmap(
      data = test_eurostat_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy - No Cache",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result_no_cache))
})

# Test with jamovi data loading
test_that("eurostatmap function integration with jamovi data", {
  # Test if function can handle jamovi-style data input
  jamovi_data <- test_eurostat_data
  
  result <- expect_silent(
    eurostatmap(
      data = jamovi_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy - Jamovi Integration",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  expect_true(is.list(result))
})

# Performance test
test_that("eurostatmap function performance", {
  # Test with larger dataset
  large_data <- do.call(rbind, replicate(10, test_eurostat_data, simplify = FALSE))
  large_data$geo <- paste0(large_data$geo, "_", rep(1:10, each = 10))
  
  start_time <- Sys.time()
  result <- expect_silent(
    eurostatmap(
      data = large_data,
      indicator = "life_expectancy",
      geo_level = "nuts0",
      year = 2022,
      map_type = "static",
      color_palette = "viridis",
      map_title = "Life Expectancy - Performance Test",
      use_local_data = TRUE,
      classification_method = "quantile",
      n_classes = 5,
      cache_data = FALSE,
      add_to_data = FALSE
    )
  )
  end_time <- Sys.time()
  
  # Test should complete within reasonable time
  expect_true(as.numeric(end_time - start_time, units = "secs") < 30)
  expect_true(is.list(result))
})

# Test R6 class instantiation
test_that("eurostatmapClass instantiation works", {
  # Test if the R6 class can be instantiated
  expect_true(exists("eurostatmapClass"))
  
  # Test class structure
  if (exists("eurostatmapClass")) {
    class_obj <- eurostatmapClass$new()
    expect_true(inherits(class_obj, "eurostatmapClass"))
    expect_true(inherits(class_obj, "R6"))
  }
})

# Cleanup
rm(test_eurostat_data)