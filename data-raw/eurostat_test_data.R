# Generate test data for eurostatmap function
# This script creates sample datasets that mimic Eurostat data structure

library(dplyr)

# Create sample eurostat map data
# This should include the 'geo' column with NUTS codes and various health indicators

# EU28 countries with their NUTS0 codes
eu_countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
  "PL", "PT", "RO", "SK", "SI", "ES", "SE", "UK"
)

# Set seed for reproducibility
set.seed(42)

# Create sample health indicator data
eurostat_health_data <- data.frame(
  geo = eu_countries,
  country_name = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
    "Spain", "Sweden", "United Kingdom"
  ),
  # Life expectancy at birth (years)
  life_expectancy = round(runif(28, 74, 84), 1),
  # Infant mortality rate (per 1000 live births)
  infant_mortality = round(runif(28, 2.5, 8.5), 1),
  # Healthcare expenditure as % of GDP
  healthcare_expenditure = round(runif(28, 6, 12), 1),
  # Population density (per km²)
  population_density = round(runif(28, 15, 500), 0),
  # GDP per capita (thousands of euros)
  gdp_per_capita = round(runif(28, 12, 85), 1),
  # Hospital beds per 100,000 inhabitants
  hospital_beds = round(runif(28, 250, 800), 0),
  # Doctors per 100,000 inhabitants
  doctors_per_capita = round(runif(28, 200, 600), 0),
  # Cancer incidence rate (per 100,000)
  cancer_incidence = round(runif(28, 250, 450), 0),
  # Cardiovascular mortality rate (per 100,000)
  cardiovascular_mortality = round(runif(28, 150, 350), 0),
  # Smoking prevalence (%)
  smoking_prevalence = round(runif(28, 15, 35), 1),
  # Alcohol consumption (liters per capita)
  alcohol_consumption = round(runif(28, 8, 15), 1),
  # Year of data
  TIME_PERIOD = 2022
)

# Create additional dataset with NUTS1 regions for more detailed mapping
# Sample NUTS1 regions from major EU countries
nuts1_regions <- c(
  # Germany
  "DE1", "DE2", "DE3", "DE4", "DE5", "DE6", "DE7", "DE8", "DE9", "DEA", "DEB", "DEC", "DED", "DEE", "DEF", "DEG",
  # France
  "FR1", "FR2", "FR3", "FR4", "FR5", "FR6", "FR7", "FR8",
  # Italy
  "ITC", "ITF", "ITG", "ITH", "ITI",
  # Spain
  "ES1", "ES2", "ES3", "ES4", "ES5", "ES6", "ES7",
  # Poland
  "PL1", "PL2", "PL3", "PL4", "PL5", "PL6",
  # United Kingdom
  "UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN"
)

eurostat_nuts1_data <- data.frame(
  geo = nuts1_regions,
  # Regional health indicators
  life_expectancy = round(runif(length(nuts1_regions), 76, 83), 1),
  healthcare_access = round(runif(length(nuts1_regions), 70, 95), 1),
  air_quality_index = round(runif(length(nuts1_regions), 25, 80), 0),
  unemployment_rate = round(runif(length(nuts1_regions), 3, 12), 1),
  education_level = round(runif(length(nuts1_regions), 60, 85), 1),
  TIME_PERIOD = 2022
)

# Create a comprehensive test dataset combining multiple indicators
eurostat_comprehensive_data <- data.frame(
  geo = eu_countries,
  country_name = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
    "Spain", "Sweden", "United Kingdom"
  ),
  # Demographic indicators
  population_total = round(runif(28, 0.5, 85) * 1000000, 0),
  population_growth = round(runif(28, -0.5, 1.5), 2),
  median_age = round(runif(28, 35, 48), 1),
  birth_rate = round(runif(28, 8, 18), 1),
  death_rate = round(runif(28, 8, 16), 1),
  
  # Economic indicators
  gdp_growth = round(runif(28, -2, 5), 1),
  inflation_rate = round(runif(28, 0.5, 8), 1),
  unemployment_rate = round(runif(28, 3, 15), 1),
  
  # Health indicators
  life_expectancy = round(runif(28, 74, 84), 1),
  infant_mortality = round(runif(28, 2.5, 8.5), 1),
  healthcare_expenditure = round(runif(28, 6, 12), 1),
  
  # Environmental indicators
  co2_emissions = round(runif(28, 4, 20), 1),
  renewable_energy = round(runif(28, 15, 65), 1),
  
  # Education indicators
  tertiary_education = round(runif(28, 25, 55), 1),
  
  TIME_PERIOD = 2022
)

# Create time series data for multiple years
years <- 2018:2022
eurostat_timeseries_data <- data.frame()

for (year in years) {
  yearly_data <- data.frame(
    geo = rep(eu_countries, 1),
    life_expectancy = round(runif(28, 74, 84) + (year - 2018) * 0.2, 1),
    gdp_per_capita = round(runif(28, 12, 85) + (year - 2018) * 1.5, 1),
    TIME_PERIOD = year
  )
  eurostat_timeseries_data <- rbind(eurostat_timeseries_data, yearly_data)
}

# Save all datasets
usethis::use_data(eurostat_health_data, overwrite = TRUE)
usethis::use_data(eurostat_nuts1_data, overwrite = TRUE)
usethis::use_data(eurostat_comprehensive_data, overwrite = TRUE)
usethis::use_data(eurostat_timeseries_data, overwrite = TRUE)

# Print summary
cat("Created eurostat test datasets:\n")
cat("1. eurostat_health_data:", nrow(eurostat_health_data), "rows,", ncol(eurostat_health_data), "columns\n")
cat("2. eurostat_nuts1_data:", nrow(eurostat_nuts1_data), "rows,", ncol(eurostat_nuts1_data), "columns\n")
cat("3. eurostat_comprehensive_data:", nrow(eurostat_comprehensive_data), "rows,", ncol(eurostat_comprehensive_data), "columns\n")
cat("4. eurostat_timeseries_data:", nrow(eurostat_timeseries_data), "rows,", ncol(eurostat_timeseries_data), "columns\n")
cat("All datasets include the required 'geo' column with NUTS codes.\n")