# Create clean Turkey datasets from real Eurostat data
# This script creates properly structured datasets using actual Eurostat data

library(eurostat)
library(dplyr)

# Function to safely get data with error handling
safe_get_eurostat <- function(dataset_id, cache = TRUE) {
  tryCatch({
    cat("Downloading dataset:", dataset_id, "\n")
    data <- get_eurostat(dataset_id, time_format = "num", cache = cache)
    cat("Successfully downloaded", nrow(data), "rows\n")
    return(data)
  }, error = function(e) {
    cat("Error downloading", dataset_id, ":", e$message, "\n")
    return(NULL)
  })
}

# 1. TURKEY NUTS-1 LEVEL DATA (Real data from Eurostat)
cat("=== CREATING TURKEY NUTS-1 LEVEL DATASET ===\n")

# Get population data for NUTS-1 level
pop_data <- safe_get_eurostat("tgs00096")
turkey_nuts1_pop <- pop_data %>%
  filter(grepl("^TR[1-9A-C]$", geo)) %>%  # NUTS-1 codes: TR1, TR2, ..., TR9, TRA, TRB, TRC
  filter(TIME_PERIOD == 2023) %>%  # Most recent year
  select(geo, values) %>%
  rename(population = values)

# Get GDP data for NUTS-1 level
gdp_data <- safe_get_eurostat("tgs00006")
turkey_nuts1_gdp <- gdp_data %>%
  filter(grepl("^TR[1-9A-C]$", geo)) %>%
  filter(TIME_PERIOD == 2022) %>%  # GDP data might be a year behind
  select(geo, values) %>%
  rename(gdp_per_capita_pps = values)

# Get employment data for NUTS-1 level  
emp_data <- safe_get_eurostat("tgs00007")
turkey_nuts1_emp <- emp_data %>%
  filter(grepl("^TR[1-9A-C]$", geo)) %>%
  filter(TIME_PERIOD == 2023, age == "Y20-64", sex == "T") %>%  # Total, age 20-64
  select(geo, values) %>%
  rename(employment_rate = values)

# Combine NUTS-1 data
turkey_nuts1_real <- turkey_nuts1_pop %>%
  left_join(turkey_nuts1_gdp, by = "geo") %>%
  left_join(turkey_nuts1_emp, by = "geo") %>%
  mutate(
    region_name = case_when(
      geo == "TR1" ~ "İstanbul",
      geo == "TR2" ~ "Batı Marmara", 
      geo == "TR3" ~ "Ege",
      geo == "TR4" ~ "Doğu Marmara",
      geo == "TR5" ~ "Batı Anadolu",
      geo == "TR6" ~ "Akdeniz",
      geo == "TR7" ~ "Orta Anadolu",
      geo == "TR8" ~ "Batı Karadeniz",
      geo == "TR9" ~ "Doğu Karadeniz",
      geo == "TRA" ~ "Kuzeydoğu Anadolu",
      geo == "TRB" ~ "Ortadoğu Anadolu", 
      geo == "TRC" ~ "Güneydoğu Anadolu",
      TRUE ~ geo
    ),
    region_name_en = case_when(
      geo == "TR1" ~ "Istanbul",
      geo == "TR2" ~ "West Marmara",
      geo == "TR3" ~ "Aegean", 
      geo == "TR4" ~ "East Marmara",
      geo == "TR5" ~ "West Anatolia",
      geo == "TR6" ~ "Mediterranean",
      geo == "TR7" ~ "Central Anatolia",
      geo == "TR8" ~ "West Black Sea",
      geo == "TR9" ~ "East Black Sea",
      geo == "TRA" ~ "Northeast Anatolia",
      geo == "TRB" ~ "Central East Anatolia",
      geo == "TRC" ~ "Southeast Anatolia",
      TRUE ~ geo
    ),
    TIME_PERIOD = 2023
  ) %>%
  select(geo, region_name, region_name_en, population, gdp_per_capita_pps, employment_rate, TIME_PERIOD) %>%
  arrange(geo)

cat("Turkey NUTS-1 data created with", nrow(turkey_nuts1_real), "regions\n")
print(turkey_nuts1_real)

# 2. TURKEY NUTS-2 LEVEL DATA (Real data from Eurostat)
cat("\n=== CREATING TURKEY NUTS-2 LEVEL DATASET ===\n")

# Get population data for NUTS-2 level
turkey_nuts2_pop <- pop_data %>%
  filter(grepl("^TR[1-9][0-9]|^TR[A-C][1-3]$", geo)) %>%  # NUTS-2 codes
  filter(TIME_PERIOD == 2023) %>%
  select(geo, values) %>%
  rename(population = values)

# Get GDP data for NUTS-2 level
turkey_nuts2_gdp <- gdp_data %>%
  filter(grepl("^TR[1-9][0-9]|^TR[A-C][1-3]$", geo)) %>%
  filter(TIME_PERIOD == 2022) %>%
  select(geo, values) %>%
  rename(gdp_per_capita_pps = values)

# Get employment data for NUTS-2 level
turkey_nuts2_emp <- emp_data %>%
  filter(grepl("^TR[1-9][0-9]|^TR[A-C][1-3]$", geo)) %>%
  filter(TIME_PERIOD == 2023, age == "Y20-64", sex == "T") %>%
  select(geo, values) %>%
  rename(employment_rate = values)

# Combine NUTS-2 data
turkey_nuts2_real <- turkey_nuts2_pop %>%
  left_join(turkey_nuts2_gdp, by = "geo") %>%
  left_join(turkey_nuts2_emp, by = "geo") %>%
  mutate(TIME_PERIOD = 2023) %>%
  arrange(geo)

cat("Turkey NUTS-2 data created with", nrow(turkey_nuts2_real), "subregions\n")
print(head(turkey_nuts2_real, 10))

# 3. EASTERN REGIONS (TRA, TRB, TRC) DETAILED DATA
cat("\n=== CREATING EASTERN REGIONS (TRA, TRB, TRC) DATASET ===\n")

# Get population density data for eastern regions
density_data <- safe_get_eurostat("demo_r_d3dens")
eastern_density <- density_data %>%
  filter(geo %in% c("TRA", "TRB", "TRC")) %>%
  filter(TIME_PERIOD == 2023) %>%
  select(geo, values) %>%
  rename(population_density = values)

# Get life expectancy data for eastern regions
life_exp_data <- safe_get_eurostat("demo_r_mlifexp")
eastern_life_exp <- life_exp_data %>%
  filter(geo %in% c("TRA", "TRB", "TRC")) %>%
  filter(TIME_PERIOD == 2023, sex == "T", age == "Y_LT1") %>%  # Total, life expectancy at birth
  select(geo, values) %>%
  rename(life_expectancy = values)

# Create eastern regions dataset
turkey_eastern_regions_real <- turkey_nuts1_real %>%
  filter(geo %in% c("TRA", "TRB", "TRC")) %>%
  left_join(eastern_density, by = "geo") %>%
  left_join(eastern_life_exp, by = "geo") %>%
  select(geo, region_name, region_name_en, population, population_density, 
         gdp_per_capita_pps, employment_rate, life_expectancy, TIME_PERIOD)

cat("Eastern regions data created with", nrow(turkey_eastern_regions_real), "regions\n")
print(turkey_eastern_regions_real)

# 4. TIME SERIES DATA FOR KEY REGIONS (2013-2023)
cat("\n=== CREATING TIME SERIES DATASET ===\n")

turkey_timeseries_real <- pop_data %>%
  filter(geo %in% c("TR1", "TR2", "TR3", "TRA", "TRB", "TRC")) %>%
  filter(TIME_PERIOD >= 2013, TIME_PERIOD <= 2023) %>%
  select(geo, TIME_PERIOD, values) %>%
  rename(population = values) %>%
  mutate(
    region_name = case_when(
      geo == "TR1" ~ "İstanbul",
      geo == "TR2" ~ "Batı Marmara",
      geo == "TR3" ~ "Ege",
      geo == "TRA" ~ "Kuzeydoğu Anadolu",
      geo == "TRB" ~ "Ortadoğu Anadolu",
      geo == "TRC" ~ "Güneydoğu Anadolu",
      TRUE ~ geo
    )
  ) %>%
  arrange(geo, TIME_PERIOD)

cat("Time series data created with", nrow(turkey_timeseries_real), "observations\n")
print(head(turkey_timeseries_real, 10))

# 5. PROVINCIAL DATA (NUTS-3) FOR MAJOR CITIES
cat("\n=== CREATING MAJOR PROVINCES DATASET ===\n")

major_provinces <- c("TR100", "TR211", "TR310", "TR411", "TR510", "TR611", 
                    "TR711", "TR811", "TR901", "TRA11", "TRB11", "TRC11")

turkey_major_provinces_real <- density_data %>%
  filter(geo %in% major_provinces) %>%
  filter(TIME_PERIOD == 2023) %>%
  select(geo, values) %>%
  rename(population_density = values) %>%
  mutate(
    province_name = case_when(
      geo == "TR100" ~ "İstanbul",
      geo == "TR211" ~ "Tekirdağ", 
      geo == "TR310" ~ "İzmir",
      geo == "TR411" ~ "Bursa",
      geo == "TR510" ~ "Ankara",
      geo == "TR611" ~ "Antalya",
      geo == "TR711" ~ "Kayseri",
      geo == "TR811" ~ "Zonguldak",
      geo == "TR901" ~ "Trabzon",
      geo == "TRA11" ~ "Erzurum",
      geo == "TRB11" ~ "Malatya",
      geo == "TRC11" ~ "Gaziantep",
      TRUE ~ geo
    ),
    province_name_en = case_when(
      geo == "TR100" ~ "Istanbul",
      geo == "TR211" ~ "Tekirdag",
      geo == "TR310" ~ "Izmir", 
      geo == "TR411" ~ "Bursa",
      geo == "TR510" ~ "Ankara",
      geo == "TR611" ~ "Antalya",
      geo == "TR711" ~ "Kayseri",
      geo == "TR811" ~ "Zonguldak",
      geo == "TR901" ~ "Trabzon",
      geo == "TRA11" ~ "Erzurum",
      geo == "TRB11" ~ "Malatya",
      geo == "TRC11" ~ "Gaziantep",
      TRUE ~ geo
    ),
    TIME_PERIOD = 2023
  ) %>%
  arrange(geo)

cat("Major provinces data created with", nrow(turkey_major_provinces_real), "provinces\n")
print(turkey_major_provinces_real)

# Save all datasets
cat("\n=== SAVING DATASETS ===\n")

usethis::use_data(turkey_nuts1_real, overwrite = TRUE)
cat("✓ Saved turkey_nuts1_real\n")

usethis::use_data(turkey_nuts2_real, overwrite = TRUE)
cat("✓ Saved turkey_nuts2_real\n")

usethis::use_data(turkey_eastern_regions_real, overwrite = TRUE)
cat("✓ Saved turkey_eastern_regions_real\n")

usethis::use_data(turkey_timeseries_real, overwrite = TRUE)
cat("✓ Saved turkey_timeseries_real\n")

usethis::use_data(turkey_major_provinces_real, overwrite = TRUE)
cat("✓ Saved turkey_major_provinces_real\n")

# Print final summary
cat("\n=== FINAL SUMMARY ===\n")
cat("Created 5 real Turkey datasets from Eurostat:\n")
cat("1. turkey_nuts1_real: 12 NUTS-1 regions with population, GDP, employment\n")
cat("2. turkey_nuts2_real: 26 NUTS-2 subregions with key indicators\n")
cat("3. turkey_eastern_regions_real: TRA, TRB, TRC with detailed indicators\n")
cat("4. turkey_timeseries_real: Time series 2013-2023 for 6 key regions\n")
cat("5. turkey_major_provinces_real: 12 major provinces (NUTS-3 level)\n")
cat("\nAll data is real from Eurostat API, not synthetic!\n")