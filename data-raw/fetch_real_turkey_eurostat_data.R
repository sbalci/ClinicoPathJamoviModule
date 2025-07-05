# Fetch real Turkey data from Eurostat API
# This script attempts to download actual Turkey data from Eurostat

library(eurostat)
library(dplyr)

# Set cache to TRUE for faster subsequent runs
cache_setting <- TRUE

# Function to safely get data with error handling
safe_get_eurostat <- function(dataset_id, cache = TRUE) {
  tryCatch({
    cat("Attempting to download dataset:", dataset_id, "\n")
    data <- get_eurostat(dataset_id, time_format = "num", cache = cache)
    cat("Successfully downloaded", nrow(data), "rows for", dataset_id, "\n")
    return(data)
  }, error = function(e) {
    cat("Error downloading", dataset_id, ":", e$message, "\n")
    return(NULL)
  })
}

# Function to filter Turkey data
filter_turkey_data <- function(data, dataset_name) {
  if (is.null(data)) return(NULL)
  
  cat("Filtering Turkey data from", dataset_name, "\n")
  
  # Filter for Turkey (TR) codes
  turkey_data <- data %>%
    filter(grepl("^TR", geo)) %>%
    arrange(geo, TIME_PERIOD)
  
  cat("Found", nrow(turkey_data), "Turkey records in", dataset_name, "\n")
  
  if (nrow(turkey_data) > 0) {
    cat("Turkey geo codes found:", unique(turkey_data$geo), "\n")
    cat("Time periods:", range(turkey_data$TIME_PERIOD, na.rm = TRUE), "\n")
  }
  
  return(turkey_data)
}

cat("=== SEARCHING FOR TURKEY DATA IN EUROSTAT ===\n\n")

# 1. Population data (demo_r_gind3) - Regional demographic data
cat("1. POPULATION CHANGE - DEMOGRAPHIC BALANCE (demo_r_gind3)\n")
demo_data <- safe_get_eurostat("demo_r_gind3", cache = cache_setting)
turkey_demo <- filter_turkey_data(demo_data, "demo_r_gind3")

# 2. Population by NUTS2 regions (tgs00096)
cat("\n2. POPULATION BY NUTS2 REGIONS (tgs00096)\n")
pop_nuts2_data <- safe_get_eurostat("tgs00096", cache = cache_setting)
turkey_pop_nuts2 <- filter_turkey_data(pop_nuts2_data, "tgs00096")

# 3. GDP per capita by NUTS2 regions (tgs00006)
cat("\n3. GDP PER CAPITA BY NUTS2 (tgs00006)\n")
gdp_data <- safe_get_eurostat("tgs00006", cache = cache_setting)
turkey_gdp <- filter_turkey_data(gdp_data, "tgs00006")

# 4. Employment rate by NUTS2 (tgs00007)
cat("\n4. EMPLOYMENT RATE BY NUTS2 (tgs00007)\n")
employment_data <- safe_get_eurostat("tgs00007", cache = cache_setting)
turkey_employment <- filter_turkey_data(employment_data, "tgs00007")

# 5. Life expectancy (demo_r_mlifexp)
cat("\n5. LIFE EXPECTANCY AT BIRTH (demo_r_mlifexp)\n")
life_exp_data <- safe_get_eurostat("demo_r_mlifexp", cache = cache_setting)
turkey_life_exp <- filter_turkey_data(life_exp_data, "demo_r_mlifexp")

# 6. Population density (demo_r_d3dens)
cat("\n6. POPULATION DENSITY (demo_r_d3dens)\n")
density_data <- safe_get_eurostat("demo_r_d3dens", cache = cache_setting)
turkey_density <- filter_turkey_data(density_data, "demo_r_d3dens")

# 7. Try another approach - search for datasets mentioning Turkey
cat("\n=== SEARCHING EUROSTAT TABLE OF CONTENTS FOR TURKEY ===\n")
toc <- get_eurostat_toc()
turkey_datasets <- toc[grepl("Turkey|TR", toc$title, ignore.case = TRUE), ]

if (nrow(turkey_datasets) > 0) {
  cat("Found", nrow(turkey_datasets), "datasets mentioning Turkey:\n")
  print(turkey_datasets[c("code", "title")])
} else {
  cat("No datasets specifically mentioning Turkey found in TOC\n")
}

# Create summary of what we found
cat("\n=== SUMMARY OF AVAILABLE TURKEY DATA ===\n")

available_datasets <- list()

if (!is.null(turkey_demo) && nrow(turkey_demo) > 0) {
  available_datasets[["demographic_balance"]] <- turkey_demo
  cat("✓ Demographic balance data:", nrow(turkey_demo), "records\n")
}

if (!is.null(turkey_pop_nuts2) && nrow(turkey_pop_nuts2) > 0) {
  available_datasets[["population_nuts2"]] <- turkey_pop_nuts2
  cat("✓ Population NUTS2 data:", nrow(turkey_pop_nuts2), "records\n")
}

if (!is.null(turkey_gdp) && nrow(turkey_gdp) > 0) {
  available_datasets[["gdp_nuts2"]] <- turkey_gdp
  cat("✓ GDP per capita data:", nrow(turkey_gdp), "records\n")
}

if (!is.null(turkey_employment) && nrow(turkey_employment) > 0) {
  available_datasets[["employment_nuts2"]] <- turkey_employment
  cat("✓ Employment rate data:", nrow(turkey_employment), "records\n")
}

if (!is.null(turkey_life_exp) && nrow(turkey_life_exp) > 0) {
  available_datasets[["life_expectancy"]] <- turkey_life_exp
  cat("✓ Life expectancy data:", nrow(turkey_life_exp), "records\n")
}

if (!is.null(turkey_density) && nrow(turkey_density) > 0) {
  available_datasets[["population_density"]] <- turkey_density
  cat("✓ Population density data:", nrow(turkey_density), "records\n")
}

if (length(available_datasets) == 0) {
  cat("⚠ No Turkey data found in any of the tested datasets\n")
  cat("This could be because:\n")
  cat("1. Turkey data is not available in these specific datasets\n")
  cat("2. Turkey data uses different geo codes\n")
  cat("3. Network/API access issues\n")
} else {
  cat("\nTotal datasets with Turkey data:", length(available_datasets), "\n")
}

# Try to create a combined dataset if we have multiple sources
if (length(available_datasets) >= 2) {
  cat("\n=== CREATING COMBINED TURKEY DATASET ===\n")
  
  # Focus on most recent year and NUTS1/NUTS2 levels
  recent_year <- 2022
  
  # Start with population data as base
  if ("population_nuts2" %in% names(available_datasets)) {
    base_data <- available_datasets[["population_nuts2"]] %>%
      filter(TIME_PERIOD == recent_year) %>%
      select(geo, values) %>%
      rename(population = values)
    
    cat("Using population data as base with", nrow(base_data), "records\n")
    
    # Add other indicators
    for (dataset_name in names(available_datasets)) {
      if (dataset_name != "population_nuts2") {
        other_data <- available_datasets[[dataset_name]] %>%
          filter(TIME_PERIOD == recent_year) %>%
          select(geo, values)
        
        if (nrow(other_data) > 0) {
          colnames(other_data)[2] <- dataset_name
          base_data <- left_join(base_data, other_data, by = "geo")
          cat("Added", dataset_name, "data\n")
        }
      }
    }
    
    # Add TIME_PERIOD column
    base_data$TIME_PERIOD <- recent_year
    
    # Save the combined dataset
    turkey_real_eurostat_data <- base_data
    
    cat("Final combined dataset:\n")
    print(head(turkey_real_eurostat_data))
    print(paste("Dimensions:", nrow(turkey_real_eurostat_data), "x", ncol(turkey_real_eurostat_data)))
    
    # Save to RDA file
    usethis::use_data(turkey_real_eurostat_data, overwrite = TRUE)
    cat("Saved combined Turkey data as 'turkey_real_eurostat_data'\n")
  }
}

cat("\n=== SCRIPT COMPLETED ===\n")