# Generate Turkey-specific Eurostat test data
# This script creates sample datasets that mimic Eurostat data structure for Turkey

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)

# Set seed for reproducibility
set.seed(42)

## NUTS-1 LEVEL DATA FOR TURKEY (12 regions)
# Turkey NUTS-1 codes and regions
turkey_nuts1 <- data.frame(
  geo = c("TR1", "TR2", "TR3", "TR4", "TR5", "TR6", 
          "TR7", "TR8", "TR9", "TRA", "TRB", "TRC"),
  region_name = c(
    "İstanbul", "Batı Marmara", "Ege", "Doğu Marmara", 
    "Batı Anadolu", "Akdeniz", "Orta Anadolu", "Batı Karadeniz",
    "Doğu Karadeniz", "Kuzeydoğu Anadolu", "Ortadoğu Anadolu", "Güneydoğu Anadolu"
  ),
  region_name_en = c(
    "Istanbul", "West Marmara", "Aegean", "East Marmara",
    "West Anatolia", "Mediterranean", "Central Anatolia", "West Black Sea",
    "East Black Sea", "Northeast Anatolia", "Central East Anatolia", "Southeast Anatolia"
  ),
  # Population data (in thousands, 2022 estimates)
  population_total = c(15840, 2950, 9680, 4830, 7550, 9890, 
                      3580, 3720, 2130, 1180, 1680, 8950),
  # Population density (per km²)
  population_density = c(2987, 131, 220, 204, 201, 215,
                        93, 82, 139, 22, 32, 120),
  # Birth rate (per 1000)
  birth_rate = c(12.8, 11.2, 10.9, 11.5, 10.8, 13.2,
                10.5, 10.8, 11.1, 12.5, 14.2, 16.8),
  # Death rate (per 1000) 
  death_rate = c(6.2, 8.5, 7.8, 7.9, 8.1, 6.9,
                8.8, 9.2, 8.7, 7.8, 7.2, 6.1),
  # Life expectancy
  life_expectancy = c(78.5, 77.8, 78.9, 78.2, 78.7, 78.3,
                     77.9, 77.5, 78.1, 76.8, 76.2, 75.9),
  # GDP per capita (thousands of TL, approximate)
  gdp_per_capita = c(85.2, 45.3, 52.8, 58.7, 48.9, 42.1,
                    38.9, 35.2, 32.8, 28.5, 31.2, 29.7),
  # Unemployment rate (%)
  unemployment_rate = c(8.9, 12.5, 11.2, 9.8, 10.5, 12.1,
                       13.8, 11.9, 10.3, 15.2, 14.8, 16.5),
  # Education level (% with higher education)
  higher_education_pct = c(28.5, 15.2, 22.8, 19.5, 18.9, 17.3,
                          16.8, 14.2, 16.5, 12.8, 13.5, 14.1),
  TIME_PERIOD = rep(2022, 12)
)

## NUTS-2 LEVEL DATA FOR TURKEY (26 subregions) - Sample of key regions
turkey_nuts2_sample <- data.frame(
  geo = c("TR10", "TR21", "TR22", "TR31", "TR32", "TR33", "TR34",
          "TR41", "TR42", "TR51", "TR52", "TR61", "TR62", "TR63",
          "TR71", "TR72", "TR81", "TR82", "TR83", "TR90",
          "TRA1", "TRA2", "TRB1", "TRB2", "TRC1", "TRC2", "TRC3"),
  subregion_name = c(
    "İstanbul", "Tekirdağ, Edirne, Kırklareli", "Balıkesir, Çanakkale",
    "İzmir", "Aydın, Denizli, Muğla", "Manisa, Afyonkarahisar, Kütahya, Uşak",
    "Bursa, Eskişehir, Bilecik", "Kocaeli, Sakarya, Düzce, Bolu, Yalova",
    "Ankara", "Konya, Karaman", "Antalya, Isparta, Burdur",
    "Adana, Mersin", "Hatay, Kahramanmaraş, Osmaniye", "Kırıkkale, Aksaray, Niğde, Nevşehir, Kırşehir",
    "Kayseri, Sivas, Yozgat", "Zonguldak, Karabük, Bartın",
    "Kastamonu, Çankırı, Sinop", "Samsun, Tokat, Çorum, Amasya",
    "Trabzon, Ordu, Giresun, Rize, Artvin, Gümüşhane",
    "Erzurum, Erzincan, Bayburt", "Ağrı, Kars, Iğdır, Ardahan",
    "Malatya, Elazığ, Bingöl, Tunceli", "Van, Muş, Bitlis, Hakkari",
    "Gaziantep, Adıyaman, Kilis", "Şanlıurfa, Diyarbakır", "Mardin, Batman, Şırnak, Siirt"
  ),
  # Health indicators
  infant_mortality = round(runif(27, 8, 25), 1),
  maternal_mortality = round(runif(27, 15, 45), 1),
  hospital_beds_per_1000 = round(runif(27, 1.8, 4.2), 1),
  doctors_per_1000 = round(runif(27, 1.2, 2.8), 1),
  # Economic indicators
  poverty_rate = round(runif(27, 8, 35), 1),
  employment_rate = round(runif(27, 35, 65), 1),
  TIME_PERIOD = rep(2022, 27)
)

## NUTS-3 LEVEL DATA FOR TURKEY (Selected major provinces)
turkey_major_provinces <- data.frame(
  geo = c("TR100", "TR211", "TR310", "TR411", "TR510", "TR611", "TR711",
          "TR811", "TR901", "TRA11", "TRB11", "TRC11"),
  province_name = c("İstanbul", "Tekirdağ", "İzmir", "Bursa", "Ankara", 
                   "Antalya", "Kayseri", "Zonguldak", "Trabzon", 
                   "Erzurum", "Malatya", "Gaziantep"),
  province_name_en = c("Istanbul", "Tekirdag", "Izmir", "Bursa", "Ankara",
                      "Antalya", "Kayseri", "Zonguldak", "Trabzon",
                      "Erzurum", "Malatya", "Gaziantep"),
  # Detailed health and social indicators
  cancer_incidence = round(runif(12, 180, 320), 0),
  cardiovascular_mortality = round(runif(12, 120, 280), 0),
  air_pollution_pm25 = round(runif(12, 15, 55), 1),
  green_space_per_capita = round(runif(12, 8, 35), 1),
  internet_access_pct = round(runif(12, 70, 95), 1),
  waste_recycling_rate = round(runif(12, 12, 45), 1),
  TIME_PERIOD = rep(2022, 12)
)

## Time series data for Turkey (2018-2022)
years <- 2018:2022
turkey_timeseries <- data.frame()

for (year in years) {
  yearly_data <- data.frame(
    geo = rep(c("TR1", "TR2", "TR3", "TRA", "TRB", "TRC"), 1),
    region_name = rep(c("İstanbul", "Batı Marmara", "Ege", 
                       "Kuzeydoğu Anadolu", "Ortadoğu Anadolu", "Güneydoğu Anadolu"), 1),
    population_growth = round(runif(6, -0.5, 2.5) + (year - 2018) * 0.1, 2),
    gdp_growth = round(runif(6, -2, 8) + (year - 2018) * 0.3, 1),
    unemployment_rate = round(runif(6, 8, 18) - (year - 2018) * 0.2, 1),
    TIME_PERIOD = year
  )
  turkey_timeseries <- rbind(turkey_timeseries, yearly_data)
}

## Special dataset focusing on Eastern regions (TRA, TRB, TRC)
turkey_eastern_regions <- data.frame(
  geo = c("TRA", "TRB", "TRC"),
  region_name = c("Kuzeydoğu Anadolu", "Ortadoğu Anadolu", "Güneydoğu Anadolu"),
  region_name_en = c("Northeast Anatolia", "Central East Anatolia", "Southeast Anatolia"),
  # Socioeconomic indicators
  population_total = c(1180, 1680, 8950), # in thousands
  rural_population_pct = c(35.2, 42.1, 28.5),
  literacy_rate = c(92.8, 89.5, 87.2),
  internet_penetration = c(78.5, 75.2, 72.8),
  # Health indicators
  infant_mortality_rate = c(18.5, 21.2, 19.8),
  life_expectancy = c(76.8, 76.2, 75.9),
  vaccination_coverage = c(94.2, 91.8, 88.5),
  # Economic indicators
  gdp_per_capita = c(28.5, 31.2, 29.7),
  poverty_headcount = c(18.9, 22.1, 24.5),
  employment_rate = c(42.3, 38.9, 35.2),
  # Infrastructure
  road_density_km_per_km2 = c(0.45, 0.38, 0.52),
  hospital_beds_per_1000 = c(2.1, 1.9, 2.3),
  TIME_PERIOD = rep(2022, 3)
)

# Save all Turkey datasets
use_data_multi_format(turkey_nuts1, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(turkey_nuts2_sample, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(turkey_major_provinces, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(turkey_timeseries, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(turkey_eastern_regions, overwrite = TRUE, save_csv = TRUE)

# Print summary
cat("Created Turkey-specific Eurostat datasets:\n")
cat("1. turkey_nuts1:", nrow(turkey_nuts1), "rows (NUTS-1 regions),", ncol(turkey_nuts1), "columns\n")
cat("2. turkey_nuts2_sample:", nrow(turkey_nuts2_sample), "rows (NUTS-2 sample),", ncol(turkey_nuts2_sample), "columns\n")
cat("3. turkey_major_provinces:", nrow(turkey_major_provinces), "rows (major provinces),", ncol(turkey_major_provinces), "columns\n")
cat("4. turkey_timeseries:", nrow(turkey_timeseries), "rows (time series),", ncol(turkey_timeseries), "columns\n")
cat("5. turkey_eastern_regions:", nrow(turkey_eastern_regions), "rows (TRA, TRB, TRC),", ncol(turkey_eastern_regions), "columns\n")
cat("All datasets include the required 'geo' column with Turkey NUTS codes.\n")
cat("Eastern regions TRA, TRB, TRC are specifically included with detailed indicators.\n")
