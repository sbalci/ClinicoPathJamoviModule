# Generate Waterfall Test Data
# This script creates comprehensive test datasets for the waterfall function
# Use this to regenerate test data or create new test scenarios

library(dplyr)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# Helper Functions
# =============================================================================

#' Generate RECIST response values with controlled distribution
#' @param n Number of patients
#' @param orr Target Objective Response Rate (0-1)
#' @param dcr Target Disease Control Rate (0-1)
#' @return Vector of response values
generate_recist_responses <- function(n, orr = 0.40, dcr = 0.70) {

  # Calculate category counts
  n_responders <- round(n * orr)
  n_stable <- round(n * dcr) - n_responders
  n_progressive <- n - n_responders - n_stable

  # Allocate CR and PR (60% PR, 40% CR among responders)
  n_pr <- round(n_responders * 0.6)
  n_cr <- n_responders - n_pr

  # Generate values for each category
  cr_values <- runif(n_cr, -100, -100)  # Exactly -100% for CR
  pr_values <- runif(n_pr, -80, -30)    # PR range: -80% to -30%
  sd_values <- runif(n_stable, -25, 20)  # SD range: -25% to +20%
  pd_values <- runif(n_progressive, 21, 100)  # PD range: +21% to +100%

  # Combine and shuffle
  responses <- c(cr_values, pr_values, sd_values, pd_values)
  sample(responses)
}


#' Generate longitudinal tumor measurements
#' @param patient_id Patient ID
#' @param baseline_size Baseline tumor size
#' @param response_type Type of response (shrink, stable, grow, complete)
#' @param n_timepoints Number of follow-up time points
#' @return Data frame with patient measurements over time
generate_longitudinal_data <- function(patient_id,
                                       baseline_size = 50,
                                       response_type = "shrink",
                                       n_timepoints = 4,
                                       time_unit = 2) {

  times <- seq(0, (n_timepoints - 1) * time_unit, by = time_unit)

  measurements <- switch(
    response_type,
    "shrink" = {
      # Progressive shrinkage
      decay_rate <- runif(1, 0.15, 0.25)
      baseline_size * exp(-decay_rate * times) + rnorm(n_timepoints, 0, 2)
    },
    "stable" = {
      # Stable disease with minor fluctuations
      baseline_size + rnorm(n_timepoints, 0, 5)
    },
    "grow" = {
      # Progressive growth
      growth_rate <- runif(1, 0.05, 0.15)
      baseline_size * exp(growth_rate * times) + rnorm(n_timepoints, 0, 3)
    },
    "complete" = {
      # Complete response (shrinks to 0)
      c(baseline_size, baseline_size * 0.4, baseline_size * 0.1, 0)
    },
    "mixed" = {
      # Initial response then progression
      c(baseline_size, baseline_size * 0.6, baseline_size * 0.7, baseline_size * 1.2)
    }
  )

  # Ensure non-negative measurements
  measurements <- pmax(measurements, 0)

  data.frame(
    PatientID = patient_id,
    Time = times,
    Measurement = round(measurements, 1)
  )
}


# =============================================================================
# Dataset 1: Basic Percentage Data
# =============================================================================

waterfall_percentage_basic <- data.frame(
  PatientID = paste0("PT", sprintf("%02d", 1:20)),
  Response = c(
    -100, -85, -60, -45, -35, -32, -30,  # Responders (CR/PR)
    -25, -20, -15, -10, -5, 0, 5, 10, 15, 20,  # Stable Disease
    25, 35, 50  # Progressive Disease
  ),
  Treatment = rep(c("Drug A", "Drug B"), each = 10)
)

# Save in multiple formats
write.csv(waterfall_percentage_basic,
          "data/waterfall_percentage_basic.csv",
          row.names = FALSE)
save(waterfall_percentage_basic,
     file = "data/waterfall_percentage_basic.rda")

cat("✅ Created: waterfall_percentage_basic\n")
cat("   - 20 patients\n")
cat("   - ORR: 35% (7/20)\n")
cat("   - DCR: 85% (17/20)\n\n")


# =============================================================================
# Dataset 2: Raw Longitudinal Data
# =============================================================================

longitudinal_list <- list(
  generate_longitudinal_data("PT01", 50, "shrink"),
  generate_longitudinal_data("PT02", 60, "shrink"),
  generate_longitudinal_data("PT03", 55, "stable"),
  generate_longitudinal_data("PT04", 45, "grow"),
  generate_longitudinal_data("PT05", 40, "complete"),
  generate_longitudinal_data("PT06", 70, "shrink"),
  generate_longitudinal_data("PT07", 35, "shrink"),
  generate_longitudinal_data("PT08", 80, "shrink"),
  generate_longitudinal_data("PT09", 65, "shrink"),
  generate_longitudinal_data("PT10", 75, "stable"),
  generate_longitudinal_data("PT11", 42, "shrink"),
  generate_longitudinal_data("PT12", 58, "shrink"),
  generate_longitudinal_data("PT13", 90, "shrink"),
  generate_longitudinal_data("PT14", 48, "shrink"),
  generate_longitudinal_data("PT15", 52, "mixed")
)

waterfall_raw_longitudinal <- bind_rows(longitudinal_list)

write.csv(waterfall_raw_longitudinal,
          "data/waterfall_raw_longitudinal.csv",
          row.names = FALSE)
save(waterfall_raw_longitudinal,
     file = "data/waterfall_raw_longitudinal.rda")

cat("✅ Created: waterfall_raw_longitudinal\n")
cat("   - 15 patients\n")
cat("   - 4 time points each (0, 2, 4, 6 months)\n")
cat("   - Various response patterns\n\n")


# =============================================================================
# Dataset 3: Realistic Oncology Trial
# =============================================================================

n_patients <- 50

waterfall_oncology_trial <- data.frame(
  PatientID = paste0("PT", sprintf("%03d", 1:n_patients)),
  Response = generate_recist_responses(n_patients, orr = 0.40, dcr = 0.70),
  Age = round(runif(n_patients, 40, 79)),
  Gender = sample(c("Male", "Female"), n_patients, replace = TRUE),
  Stage = sample(c("I", "II", "III", "IV"), n_patients, replace = TRUE,
                 prob = c(0.1, 0.2, 0.4, 0.3)),
  Treatment = rep(c("Experimental", "Control"), each = n_patients/2)
) %>%
  arrange(desc(Response))  # Sort by best response

write.csv(waterfall_oncology_trial,
          "data/waterfall_oncology_trial.csv",
          row.names = FALSE)
save(waterfall_oncology_trial,
     file = "data/waterfall_oncology_trial.rda")

# Calculate actual ORR and DCR
n_cr <- sum(waterfall_oncology_trial$Response <= -100)
n_pr <- sum(waterfall_oncology_trial$Response > -100 &
            waterfall_oncology_trial$Response <= -30)
n_sd <- sum(waterfall_oncology_trial$Response > -30 &
            waterfall_oncology_trial$Response <= 20)
orr_actual <- (n_cr + n_pr) / n_patients * 100
dcr_actual <- (n_cr + n_pr + n_sd) / n_patients * 100

cat("✅ Created: waterfall_oncology_trial\n")
cat(sprintf("   - %d patients\n", n_patients))
cat(sprintf("   - ORR: %.1f%% (%d CR + %d PR)\n", orr_actual, n_cr, n_pr))
cat(sprintf("   - DCR: %.1f%% (+ %d SD)\n", dcr_actual, n_sd))
cat("   - Balanced treatment arms\n\n")


# =============================================================================
# Dataset 4: Edge Cases
# =============================================================================

waterfall_edge_cases <- data.frame(
  PatientID = paste0("PT", 1:10),
  Response = c(
    -150,    # Invalid: shrinkage >100%
    -100,    # Exact CR boundary
    -99,     # Just above CR boundary
    -30,     # Exact PR boundary
    -29,     # Just above PR boundary
    0,       # Zero change
    20,      # Exact SD boundary
    21,      # Just above SD boundary
    500,     # Extreme growth
    NA       # Missing value
  )
)

write.csv(waterfall_edge_cases,
          "data/waterfall_edge_cases.csv",
          row.names = FALSE)
save(waterfall_edge_cases,
     file = "data/waterfall_edge_cases.rda")

cat("✅ Created: waterfall_edge_cases\n")
cat("   - Tests boundary values\n")
cat("   - Tests invalid shrinkage\n")
cat("   - Tests extreme growth\n")
cat("   - Tests missing values\n\n")


# =============================================================================
# Dataset 5: Single Patient
# =============================================================================

waterfall_single_patient <- data.frame(
  PatientID = "PT001",
  Response = -45
)

write.csv(waterfall_single_patient,
          "data/waterfall_single_patient.csv",
          row.names = FALSE)
save(waterfall_single_patient,
     file = "data/waterfall_single_patient.rda")

cat("✅ Created: waterfall_single_patient\n")
cat("   - Minimum viable data (n=1)\n\n")


# =============================================================================
# Dataset 6: Missing Baseline (Should Fail)
# =============================================================================

waterfall_missing_baseline <- data.frame(
  PatientID = rep(paste0("PT", 1:3), each = 2),
  Time = rep(c(2, 4), 3),  # No time = 0 (baseline)
  Measurement = c(30, 25, 45, 40, 55, 50)
)

write.csv(waterfall_missing_baseline,
          "data/waterfall_missing_baseline.csv",
          row.names = FALSE)
save(waterfall_missing_baseline,
     file = "data/waterfall_missing_baseline.rda")

cat("✅ Created: waterfall_missing_baseline\n")
cat("   - Missing baseline measurements (time=0)\n")
cat("   - Should produce validation error\n\n")


# =============================================================================
# Dataset 7: Time-to-Event Data
# =============================================================================

set.seed(123)
n_tte <- 30

waterfall_time_to_event <- data.frame(
  PatientID = paste0("PT", sprintf("%03d", 1:n_tte)),
  Response = generate_recist_responses(n_tte, orr = 0.45, dcr = 0.75),
  Event = sample(0:1, n_tte, replace = TRUE, prob = c(0.3, 0.7)),
  EventTime = round(runif(n_tte, 1, 24), 1)  # 1-24 months
)

write.csv(waterfall_time_to_event,
          "data/waterfall_time_to_event.csv",
          row.names = FALSE)
save(waterfall_time_to_event,
     file = "data/waterfall_time_to_event.rda")

cat("✅ Created: waterfall_time_to_event\n")
cat(sprintf("   - %d patients\n", n_tte))
cat(sprintf("   - %d events, %d censored\n",
            sum(waterfall_time_to_event$Event == 1),
            sum(waterfall_time_to_event$Event == 0)))
cat("   - For person-time analysis\n\n")


# =============================================================================
# Create OMV files (jamovi format)
# =============================================================================

cat("\n📊 Creating OMV (jamovi) files...\n")

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {

  datasets <- list(
    "waterfall_percentage_basic" = waterfall_percentage_basic,
    "waterfall_raw_longitudinal" = waterfall_raw_longitudinal,
    "waterfall_oncology_trial" = waterfall_oncology_trial,
    "waterfall_edge_cases" = waterfall_edge_cases,
    "waterfall_single_patient" = waterfall_single_patient,
    "waterfall_missing_baseline" = waterfall_missing_baseline,
    "waterfall_time_to_event" = waterfall_time_to_event
  )

  for (name in names(datasets)) {
    omv_path <- paste0("data/", name, ".omv")
    jmvReadWrite::write_omv(datasets[[name]], omv_path, frcWrt = TRUE)
    cat(sprintf("   ✅ %s.omv\n", name))
  }

} else {
  cat("   ⚠️  Package 'jmvReadWrite' not available\n")
  cat("   Install with: install.packages('jmvReadWrite')\n")
}


# =============================================================================
# Summary Report
# =============================================================================

cat("\n" %>% strrep(60))
cat("\n📋 TEST DATA GENERATION COMPLETE\n")
cat(strrep("=", 60))
cat("\n\nGenerated 7 test datasets:\n\n")

cat("1. waterfall_percentage_basic     - Basic percentage data (n=20)\n")
cat("2. waterfall_raw_longitudinal     - Longitudinal measurements (n=15)\n")
cat("3. waterfall_oncology_trial       - Realistic trial data (n=50)\n")
cat("4. waterfall_edge_cases           - Edge cases and boundaries (n=10)\n")
cat("5. waterfall_single_patient       - Minimum data (n=1)\n")
cat("6. waterfall_missing_baseline     - Validation error test (n=3)\n")
cat("7. waterfall_time_to_event        - Person-time analysis (n=30)\n")

cat("\nFormats created:\n")
cat("  ✅ CSV (human-readable)\n")
cat("  ✅ RDA (R binary)\n")
cat("  ✅ OMV (jamovi native)\n")

cat("\nNext steps:\n")
cat("  1. Review generated data: ls -lh data/waterfall_*.csv\n")
cat("  2. Run tests: Rscript tests/verify_waterfall.R\n")
cat("  3. Run test suite: testthat::test_file('tests/testthat/test-waterfall.R')\n")
cat("  4. Manual testing: Open *.omv files in jamovi\n")

cat("\n" %>% strrep(60))
cat("\n")
