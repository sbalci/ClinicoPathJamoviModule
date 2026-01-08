# ═══════════════════════════════════════════════════════════
# Test Data Generation: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the ihcheterogeneity jamovi function
# Simulates IHC biomarker heterogeneity across tissue regions
#
# Generated: 2026-01-06
# Seed: 42
# Cases: 50

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# Number of cases
n_cases <- 50

# Helper function to add realistic regional variability
add_regional_variability <- function(reference, cv_percent) {
  # Add heterogeneity based on coefficient of variation
  sd_val <- reference * (cv_percent / 100)
  measurement <- rnorm(length(reference), mean = reference, sd = sd_val)
  # Ensure non-negative and within valid range
  measurement <- pmax(0, measurement)
  return(measurement)
}

# ═══════════════════════════════════════════════════════════
# 1. MAIN TEST DATASET - General IHC Heterogeneity
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_test <- tibble(
  # Case identifier
  case_id = paste0("Case_", 1:n_cases),

  # Reference measurement (whole section average, 0-100%)
  wholesection = runif(n_cases, min = 10, max = 90),

  # Regional measurements with varying heterogeneity
  # Low heterogeneity cases (CV ~10-15%)
  biopsy1 = wholesection + rnorm(n_cases, 0, wholesection * 0.12),
  biopsy2 = wholesection + rnorm(n_cases, 0, wholesection * 0.12),
  biopsy3 = wholesection + rnorm(n_cases, 0, wholesection * 0.15),
  biopsy4 = wholesection + rnorm(n_cases, 0, wholesection * 0.15),

  # Spatial compartment identifier
  spatial_id = sample(c("Central", "Invasive", "Peripheral"), n_cases, replace = TRUE)
) %>%
  mutate(
    # Ensure non-negative values
    across(c(biopsy1, biopsy2, biopsy3, biopsy4), ~pmax(0, pmin(100, .))),

    # Add cases with higher heterogeneity (last 10 cases)
    biopsy1 = ifelse(row_number() > 40,
                     wholesection + rnorm(n(), 0, wholesection * 0.30),
                     biopsy1),
    biopsy2 = ifelse(row_number() > 40,
                     wholesection + rnorm(n(), 0, wholesection * 0.30),
                     biopsy2)
  ) %>%
  mutate(
    # Final capping
    across(c(biopsy1, biopsy2, biopsy3, biopsy4), ~pmax(0, pmin(100, .)))
  )

# Add some missing values (5% in additional measurements)
n_missing <- round(n_cases * 0.05)
ihcheterogeneity_test$biopsy4[sample(n_cases, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# 2. KI67 PROLIFERATION INDEX DATASET
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_ki67 <- tibble(
  case_id = paste0("Ki67_", 1:n_cases),

  # Ki67 % (0-100%, typically lower in many cancers)
  wholesection = pmin(100, pmax(0, rnorm(n_cases, mean = 25, sd = 15))),

  # Regional measurements with typical Ki67 heterogeneity (CV ~20-30%)
  biopsy1 = add_regional_variability(wholesection, cv_percent = 22),
  biopsy2 = add_regional_variability(wholesection, cv_percent = 22),
  biopsy3 = add_regional_variability(wholesection, cv_percent = 25),
  biopsy4 = add_regional_variability(wholesection, cv_percent = 25),

  # Tumor region
  spatial_id = sample(c("Tumor_Center", "Invasive_Front", "Periphery"),
                     n_cases, replace = TRUE,
                     prob = c(0.4, 0.4, 0.2))
) %>%
  mutate(
    across(c(biopsy1, biopsy2, biopsy3, biopsy4), ~pmax(0, pmin(100, .)))
  )

# ═══════════════════════════════════════════════════════════
# 3. ER H-SCORE DATASET (0-300 scale)
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_er_hscore <- tibble(
  case_id = paste0("ER_", 1:n_cases),

  # ER H-score (0-300, typically bimodal: high or low)
  wholesection = c(
    rnorm(n_cases * 0.6, mean = 250, sd = 30),  # ER positive
    rnorm(n_cases * 0.4, mean = 30, sd = 20)    # ER negative/low
  )[1:n_cases],

  # Regional measurements with moderate heterogeneity (CV ~15-20%)
  biopsy1 = add_regional_variability(wholesection, cv_percent = 18),
  biopsy2 = add_regional_variability(wholesection, cv_percent = 18),
  biopsy3 = add_regional_variability(wholesection, cv_percent = 20),
  biopsy4 = add_regional_variability(wholesection, cv_percent = 20),

  spatial_id = sample(c("Central", "Invasive"), n_cases, replace = TRUE)
) %>%
  mutate(
    across(c(wholesection, biopsy1, biopsy2, biopsy3, biopsy4),
           ~pmax(0, pmin(300, .)))
  )

# ═══════════════════════════════════════════════════════════
# 4. SPATIAL COMPARTMENT COMPARISON DATASET
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_compartments <- tibble(
  case_id = paste0("Comp_", 1:n_cases),

  # Reference measurement
  wholesection = runif(n_cases, min = 20, max = 80),

  # Measurements vary by compartment
  spatial_id = rep(c("Preinvasive", "Invasive", "Lymph_Node"), length.out = n_cases)
) %>%
  mutate(
    biopsy1 = case_when(
      spatial_id == "Preinvasive" ~ wholesection * runif(n(), 0.7, 0.9),
      spatial_id == "Invasive" ~ wholesection * runif(n(), 0.9, 1.1),
      spatial_id == "Lymph_Node" ~ wholesection * runif(n(), 1.0, 1.3),
      TRUE ~ wholesection
    ),

    biopsy2 = case_when(
      spatial_id == "Preinvasive" ~ wholesection * runif(n(), 0.7, 0.9),
      spatial_id == "Invasive" ~ wholesection * runif(n(), 0.9, 1.1),
      spatial_id == "Lymph_Node" ~ wholesection * runif(n(), 1.0, 1.3),
      TRUE ~ wholesection
    ),

    biopsy3 = case_when(
      spatial_id == "Preinvasive" ~ wholesection * runif(n(), 0.7, 0.9),
      spatial_id == "Invasive" ~ wholesection * runif(n(), 0.9, 1.1),
      spatial_id == "Lymph_Node" ~ wholesection * runif(n(), 1.0, 1.3),
      TRUE ~ wholesection
    ),

    biopsy4 = case_when(
      spatial_id == "Preinvasive" ~ wholesection * runif(n(), 0.7, 0.9),
      spatial_id == "Invasive" ~ wholesection * runif(n(), 0.9, 1.1),
      spatial_id == "Lymph_Node" ~ wholesection * runif(n(), 1.0, 1.3),
      TRUE ~ wholesection
    )
  ) %>%
  mutate(
    across(c(biopsy1, biopsy2, biopsy3, biopsy4), ~pmax(0, pmin(100, .)))
  )

# ═══════════════════════════════════════════════════════════
# 5. SMALL DATASET (Edge Case Testing)
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_test_small <- ihcheterogeneity_test %>%
  slice(1:5)

# ═══════════════════════════════════════════════════════════
# 6. HIGH HETEROGENEITY DATASET
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_high_hetero <- tibble(
  case_id = paste0("HighHet_", 1:n_cases),

  wholesection = runif(n_cases, min = 30, max = 70),

  # Very high regional variability (CV ~40-60%)
  biopsy1 = add_regional_variability(wholesection, cv_percent = 45),
  biopsy2 = add_regional_variability(wholesection, cv_percent = 45),
  biopsy3 = add_regional_variability(wholesection, cv_percent = 50),
  biopsy4 = add_regional_variability(wholesection, cv_percent = 50),

  spatial_id = sample(c("Heterogeneous_A", "Heterogeneous_B"),
                     n_cases, replace = TRUE)
) %>%
  mutate(
    across(c(biopsy1, biopsy2, biopsy3, biopsy4), ~pmax(0, pmin(100, .)))
  )

# ═══════════════════════════════════════════════════════════
# 7. NO REFERENCE DATASET (Inter-regional comparison only)
# ═══════════════════════════════════════════════════════════

ihcheterogeneity_no_reference <- tibble(
  case_id = paste0("NoRef_", 1:n_cases),

  # No wholesection measurement
  # Only regional measurements
  biopsy1 = runif(n_cases, 20, 80),
  biopsy2 = biopsy1 + rnorm(n_cases, 0, biopsy1 * 0.15),
  biopsy3 = biopsy1 + rnorm(n_cases, 0, biopsy1 * 0.20),
  biopsy4 = biopsy1 + rnorm(n_cases, 0, biopsy1 * 0.20),

  spatial_id = sample(c("Region_A", "Region_B", "Region_C"),
                     n_cases, replace = TRUE)
) %>%
  mutate(
    across(c(biopsy1, biopsy2, biopsy3, biopsy4), ~pmax(0, pmin(100, .)))
  )

# ═══════════════════════════════════════════════════════════
# SAVE ALL DATASETS IN MULTIPLE FORMATS
# ═══════════════════════════════════════════════════════════

# Main test dataset
save(ihcheterogeneity_test,
     file = here::here("data", "ihcheterogeneity_test.rda"))
write.csv(ihcheterogeneity_test,
          file = here::here("data", "ihcheterogeneity_test.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_test,
                    path = here::here("data", "ihcheterogeneity_test.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_test,
                       here::here("data", "ihcheterogeneity_test.omv"))

# Ki67 dataset
save(ihcheterogeneity_ki67,
     file = here::here("data", "ihcheterogeneity_ki67.rda"))
write.csv(ihcheterogeneity_ki67,
          file = here::here("data", "ihcheterogeneity_ki67.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_ki67,
                    path = here::here("data", "ihcheterogeneity_ki67.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_ki67,
                       here::here("data", "ihcheterogeneity_ki67.omv"))

# ER H-score dataset
save(ihcheterogeneity_er_hscore,
     file = here::here("data", "ihcheterogeneity_er_hscore.rda"))
write.csv(ihcheterogeneity_er_hscore,
          file = here::here("data", "ihcheterogeneity_er_hscore.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_er_hscore,
                    path = here::here("data", "ihcheterogeneity_er_hscore.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_er_hscore,
                       here::here("data", "ihcheterogeneity_er_hscore.omv"))

# Spatial compartments dataset
save(ihcheterogeneity_compartments,
     file = here::here("data", "ihcheterogeneity_compartments.rda"))
write.csv(ihcheterogeneity_compartments,
          file = here::here("data", "ihcheterogeneity_compartments.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_compartments,
                    path = here::here("data", "ihcheterogeneity_compartments.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_compartments,
                       here::here("data", "ihcheterogeneity_compartments.omv"))

# Small dataset
save(ihcheterogeneity_test_small,
     file = here::here("data", "ihcheterogeneity_test_small.rda"))
write.csv(ihcheterogeneity_test_small,
          file = here::here("data", "ihcheterogeneity_test_small.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_test_small,
                    path = here::here("data", "ihcheterogeneity_test_small.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_test_small,
                       here::here("data", "ihcheterogeneity_test_small.omv"))

# High heterogeneity dataset
save(ihcheterogeneity_high_hetero,
     file = here::here("data", "ihcheterogeneity_high_hetero.rda"))
write.csv(ihcheterogeneity_high_hetero,
          file = here::here("data", "ihcheterogeneity_high_hetero.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_high_hetero,
                    path = here::here("data", "ihcheterogeneity_high_hetero.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_high_hetero,
                       here::here("data", "ihcheterogeneity_high_hetero.omv"))

# No reference dataset
save(ihcheterogeneity_no_reference,
     file = here::here("data", "ihcheterogeneity_no_reference.rda"))
write.csv(ihcheterogeneity_no_reference,
          file = here::here("data", "ihcheterogeneity_no_reference.csv"),
          row.names = FALSE)
writexl::write_xlsx(ihcheterogeneity_no_reference,
                    path = here::here("data", "ihcheterogeneity_no_reference.xlsx"))
jmvReadWrite::write_omv(ihcheterogeneity_no_reference,
                       here::here("data", "ihcheterogeneity_no_reference.omv"))

# Document the datasets
cat("
═══════════════════════════════════════════════════════════
IHC Heterogeneity Test Datasets Generated
═══════════════════════════════════════════════════════════

1. ihcheterogeneity_test (MAIN)
   - Cases: 50
   - Variables: 7 (case_id, wholesection, biopsy1-4, spatial_id)
   - Description: General IHC heterogeneity with varying CV (10-30%)
   - Use case: General testing of all features

2. ihcheterogeneity_ki67
   - Cases: 50
   - Variables: 7
   - Description: Ki67 proliferation index data (0-100%)
   - Heterogeneity: CV ~20-30% (typical for Ki67)
   - Compartments: Tumor_Center, Invasive_Front, Periphery

3. ihcheterogeneity_er_hscore
   - Cases: 50
   - Variables: 7
   - Description: ER H-score data (0-300 scale)
   - Heterogeneity: CV ~15-20% (typical for hormone receptors)
   - Distribution: Bimodal (ER+ high, ER- low)

4. ihcheterogeneity_compartments
   - Cases: 50
   - Variables: 7
   - Description: Spatial compartment comparison
   - Compartments: Preinvasive, Invasive, Lymph_Node
   - Use case: Testing compartmentTests option

5. ihcheterogeneity_test_small
   - Cases: 5
   - Variables: 7
   - Description: Minimal dataset for edge case testing
   - Use case: Testing with small sample size

6. ihcheterogeneity_high_hetero
   - Cases: 50
   - Variables: 7
   - Description: High heterogeneity dataset (CV ~40-60%)
   - Use case: Testing extreme variability scenarios

7. ihcheterogeneity_no_reference
   - Cases: 50
   - Variables: 6 (no wholesection)
   - Description: Inter-regional comparison only
   - Use case: Testing without reference measurement

Variable descriptions:
- case_id: Case identifier (character)
- wholesection: Reference measurement (0-100% or 0-300 for H-score)
- biopsy1-4: Regional measurements from different areas
- spatial_id: Spatial compartment identifier (factor)

All datasets saved in 4 formats:
  ✓ .rda (R native)
  ✓ .csv (universal)
  ✓ .xlsx (Excel)
  ✓ .omv (jamovi)

Usage examples:
  # Load main test data
  data(ihcheterogeneity_test, package = 'ClinicoPath')

  # Basic heterogeneity analysis
  ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = 'wholesection',
    biopsy1 = 'biopsy1',
    biopsy2 = 'biopsy2'
  )

  # With spatial compartment comparison
  ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = 'wholesection',
    biopsy1 = 'biopsy1',
    biopsy2 = 'biopsy2',
    biopsy3 = 'biopsy3',
    spatial_id = 'spatial_id',
    compareCompartments = TRUE,
    compartmentTests = TRUE
  )

Generated: ", Sys.time(), "
Seed: 42
═══════════════════════════════════════════════════════════
")
