# Test Turkey data with eurostatmap function
# This script verifies that real Turkey data works correctly

library(ClinicoPath)
library(dplyr)

cat("=== TESTING TURKEY DATA WITH EUROSTATMAP ===\n")

# Load Turkey datasets
data("turkey_nuts2_eurostat")
data("turkey_eastern_anatolia")
data("turkey_nuts1_aggregated")

# Test 1: Basic Turkey NUTS-2 population map
cat("Test 1: Turkey NUTS-2 Population Map\n")
tryCatch({
  result1 <- eurostatmap(
    data = turkey_nuts2_eurostat,
    indicator = "population",
    geo_level = "nuts2",
    year = 2023,
    map_type = "static",
    color_palette = "viridis",
    map_title = "Turkey Population Test",
    use_local_data = TRUE,
    add_to_data = FALSE
  )
  cat("✓ Test 1 PASSED - Turkey NUTS-2 population map created successfully\n")
}, error = function(e) {
  cat("✗ Test 1 FAILED:", e$message, "\n")
})

# Test 2: Eastern Anatolia regions
cat("\nTest 2: Eastern Anatolia Regions Map\n")
tryCatch({
  result2 <- eurostatmap(
    data = turkey_eastern_anatolia,
    indicator = "population",
    geo_level = "nuts2",
    year = 2023,
    map_type = "static",
    color_palette = "plasma",
    map_title = "Eastern Anatolia Test",
    use_local_data = TRUE,
    add_to_data = FALSE
  )
  cat("✓ Test 2 PASSED - Eastern Anatolia map created successfully\n")
}, error = function(e) {
  cat("✗ Test 2 FAILED:", e$message, "\n")
})

# Test 3: GDP per capita analysis
cat("\nTest 3: Turkey GDP per Capita Map\n")
tryCatch({
  result3 <- eurostatmap(
    data = turkey_nuts2_eurostat,
    indicator = "gdp_per_capita_pps",
    geo_level = "nuts2",
    year = 2023,
    map_type = "static",
    color_palette = "reds",
    map_title = "Turkey GDP Test",
    use_local_data = TRUE,
    add_to_data = FALSE
  )
  cat("✓ Test 3 PASSED - Turkey GDP map created successfully\n")
}, error = function(e) {
  cat("✗ Test 3 FAILED:", e$message, "\n")
})

# Test 4: Aggregated NUTS-1 data
cat("\nTest 4: Turkey NUTS-1 Aggregated Map\n")
tryCatch({
  result4 <- eurostatmap(
    data = turkey_nuts1_aggregated,
    indicator = "gdp_per_capita_weighted",
    geo_level = "nuts1",
    year = 2023,
    map_type = "static",
    color_palette = "blues",
    map_title = "Turkey NUTS-1 Test",
    use_local_data = TRUE,
    add_to_data = FALSE
  )
  cat("✓ Test 4 PASSED - Turkey NUTS-1 map created successfully\n")
}, error = function(e) {
  cat("✗ Test 4 FAILED:", e$message, "\n")
})

# Display data summaries
cat("\n=== DATA SUMMARIES ===\n")
cat("Turkey NUTS-2 data:", nrow(turkey_nuts2_eurostat), "regions\n")
cat("Available NUTS-2 codes:", paste(sort(unique(turkey_nuts2_eurostat$geo)), collapse = ", "), "\n")

cat("\nEastern Anatolia data:", nrow(turkey_eastern_anatolia), "regions\n")
cat("Eastern codes:", paste(sort(unique(turkey_eastern_anatolia$geo)), collapse = ", "), "\n")

cat("\nTurkey NUTS-1 aggregated:", nrow(turkey_nuts1_aggregated), "regions\n")
cat("NUTS-1 codes:", paste(sort(unique(turkey_nuts1_aggregated$geo)), collapse = ", "), "\n")

# GDP statistics
gdp_stats <- turkey_nuts2_eurostat %>%
  summarise(
    Min = min(gdp_per_capita_pps, na.rm = TRUE),
    Max = max(gdp_per_capita_pps, na.rm = TRUE),
    Mean = round(mean(gdp_per_capita_pps, na.rm = TRUE), 1),
    Median = round(median(gdp_per_capita_pps, na.rm = TRUE), 1)
  )

cat("\nGDP per Capita Statistics (PPS):\n")
cat("Min:", gdp_stats$Min, "| Max:", gdp_stats$Max, "| Mean:", gdp_stats$Mean, "| Median:", gdp_stats$Median, "\n")

cat("\n🎯 ALL TESTS COMPLETED - Real Turkey Eurostat data works with eurostatmap!\n")