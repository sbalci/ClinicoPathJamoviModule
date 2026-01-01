# IHC Heterogeneity Analysis - Test Data Generator
# Generates realistic immunohistochemistry measurements with spatial heterogeneity

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(2025)

generate_ihc_data <- function(n_cases = 50) {
  # Generate case IDs
  case_id <- sprintf("CASE_%03d", 1:n_cases)

  # Spatial compartments
  spatial_region <- sample(c("Central", "Invasive Front", "Peripheral"),
                           n_cases, replace = TRUE, prob = c(0.4, 0.35, 0.25))

  # Whole section measurements (Ki67 % proliferation index, 0-100%)
  # Mean varies by spatial region
  region_means <- c("Central" = 25, "Invasive Front" = 45, "Peripheral" = 15)
  true_means <- region_means[spatial_region]

  whole_section <- pmax(0, pmin(100, rnorm(n_cases, mean = true_means, sd = 12)))

  # Regional biopsies - add heterogeneity
  # Biopsy measurements deviate from whole section with varying CV
  biopsy_1 <- pmax(0, pmin(100, whole_section + rnorm(n_cases, 0, sd = 8)))
  biopsy_2 <- pmax(0, pmin(100, whole_section + rnorm(n_cases, 0, sd = 10)))
  biopsy_3 <- pmax(0, pmin(100, whole_section + rnorm(n_cases, 0, sd = 12)))
  biopsy_4 <- pmax(0, pmin(100, whole_section + rnorm(n_cases, 0, sd = 15)))

  # Create data frame
  data.frame(
    case_id = case_id,
    spatial_region = factor(spatial_region),
    whole_section_ki67 = round(whole_section, 1),
    region_1_ki67 = round(biopsy_1, 1),
    region_2_ki67 = round(biopsy_2, 1),
    region_3_ki67 = round(biopsy_3, 1),
    region_4_ki67 = round(biopsy_4, 1),
    stringsAsFactors = FALSE
  )
}

# Generate datasets
ihc_heterogeneity_basic <- generate_ihc_data(50)
ihc_heterogeneity_large <- generate_ihc_data(200)

# Special cases
ihc_heterogeneity_edge_cases <- data.frame(
  case_id = c("EDGE_01", "EDGE_02", "EDGE_03"),
  spatial_region = factor(c("Central", "Central", "Peripheral")),
  whole_section_ki67 = c(0.0, 100.0, 50.0),  # Edge values
  region_1_ki67 = c(0.5, 99.5, 48.0),
  region_2_ki67 = c(1.0, 98.0, 52.0),
  region_3_ki67 = c(NA, 100.0, 51.0),  # Missing value
  region_4_ki67 = c(0.0, 100.0, NA),
  stringsAsFactors = FALSE
)

# Variable names with spaces (test escaping)
ihc_special_names <- ihc_heterogeneity_basic
names(ihc_special_names)[3:7] <- c(
  "Whole Section Ki67",  # Space
  "Region 1 (Center)",   # Space + parens
  "Region-2",            # Hyphen
  "Region.3",            # Dot
  "Region_4"             # Underscore (safe)
)

# Save
write.csv(ihc_heterogeneity_basic, "data/ihc_heterogeneity_basic.csv", row.names = FALSE)
write.csv(ihc_heterogeneity_large, "data/ihc_heterogeneity_large.csv", row.names = FALSE)
write.csv(ihc_heterogeneity_edge_cases, "data/ihc_heterogeneity_edge_cases.csv", row.names = FALSE)
write.csv(ihc_special_names, "data/ihc_special_names.csv", row.names = FALSE)

message("✅ Generated 4 test datasets:")
message("  - ihc_heterogeneity_basic.csv (50 cases)")
message("  - ihc_heterogeneity_large.csv (200 cases)")
message("  - ihc_heterogeneity_edge_cases.csv (3 edge cases)")
message("  - ihc_special_names.csv (50 cases, special variable names)")
