# ═══════════════════════════════════════════════════════════
# Test Data Generation: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Generate comprehensive test datasets for the datetimeconverter jamovi function
# Creates multiple scenarios for datetime conversion and component extraction testing

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)
library(lubridate)

set.seed(42) # For reproducibility

# ───────────────────────────────────────────────────────────
# Dataset 1: Main Datetime Formats Dataset
# ───────────────────────────────────────────────────────────
# Various standard datetime formats for format detection and conversion testing

n_main <- 50

# Generate base dates
base_dates <- seq(as.Date("2020-01-15"), as.Date("2024-12-20"), length.out = n_main)
base_times <- paste0(
  sprintf("%02d", sample(0:23, n_main, replace = TRUE)), ":",
  sprintf("%02d", sample(0:59, n_main, replace = TRUE)), ":",
  sprintf("%02d", sample(0:59, n_main, replace = TRUE))
)

datetimeconverter_test <- tibble(
  id = 1:n_main,

  # Standard YMD format
  date_ymd = format(base_dates, "%Y-%m-%d"),

  # DMY format (common in Europe)
  date_dmy = format(base_dates, "%d-%m-%Y"),

  # MDY format (common in US)
  date_mdy = format(base_dates, "%m-%d-%Y"),

  # YMD with time (HMS)
  datetime_ymdhms = paste(format(base_dates, "%Y-%m-%d"), base_times),

  # DMY with time
  datetime_dmyhms = paste(format(base_dates, "%d/%m/%Y"), base_times),

  # MDY with time
  datetime_mdyhms = paste(format(base_dates, "%m/%d/%Y"), base_times),

  # YMD with hour and minute only
  datetime_ymdhm = paste(
    format(base_dates, "%Y-%m-%d"),
    sprintf("%02d:%02d", sample(0:23, n_main, replace = TRUE),
            sample(0:59, n_main, replace = TRUE))
  ),

  # Additional formats for edge testing
  date_ydm = format(base_dates, "%Y-%d-%m"),
  date_myd = format(base_dates, "%m-%Y-%d"),

  # Category for grouping
  category = sample(c("Patient", "Sample", "Visit"), n_main, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 2: Excel Serial Dates
# ───────────────────────────────────────────────────────────
# Excel serial dates (days since 1900-01-01)

n_excel <- 40

# Excel serial date calculation
# Excel's date system: 1 = 1900-01-01 (but Excel has a 1900 leap year bug)
excel_base_date <- as.Date("1899-12-30") # Adjusted for Excel's bug

excel_dates <- seq(as.Date("2018-01-01"), as.Date("2024-12-31"), length.out = n_excel)
excel_serial <- as.numeric(excel_dates - excel_base_date)

datetimeconverter_excel <- tibble(
  id = 1:n_excel,

  # Excel serial dates as numbers
  excel_serial_date = excel_serial,

  # Expected date for verification
  expected_date = format(excel_dates, "%Y-%m-%d"),

  # Excel datetime (with fractional day for time)
  excel_serial_datetime = excel_serial + runif(n_excel, 0, 0.99),

  # Data source
  source = sample(c("Lab_System", "Hospital_Records", "Research_DB"),
                 n_excel, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 3: Unix Epoch Timestamps
# ───────────────────────────────────────────────────────────
# Unix timestamps (seconds since 1970-01-01 00:00:00 UTC)

n_unix <- 40

unix_dates <- seq(as.POSIXct("2019-01-01 00:00:00", tz = "UTC"),
                  as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
                  length.out = n_unix)
unix_timestamps <- as.numeric(unix_dates)

datetimeconverter_unix <- tibble(
  id = 1:n_unix,

  # Unix epoch seconds
  unix_timestamp = unix_timestamps,

  # Unix milliseconds (common in JavaScript/APIs)
  unix_milliseconds = unix_timestamps * 1000,

  # Expected datetime for verification
  expected_datetime = format(unix_dates, "%Y-%m-%d %H:%M:%S"),

  # Event type
  event_type = sample(c("Login", "Data_Entry", "Analysis", "Report_Generated"),
                     n_unix, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 4: Mixed Formats (Auto-Detection Challenge)
# ───────────────────────────────────────────────────────────
# Mix of different formats to test auto-detection

n_mixed <- 30

sample_dates <- seq(as.Date("2020-03-15"), as.Date("2024-09-20"), length.out = n_mixed)

# Create mixed format datetime strings
mixed_formats <- character(n_mixed)
for (i in 1:n_mixed) {
  format_choice <- sample(1:5, 1)
  mixed_formats[i] <- switch(format_choice,
    format(sample_dates[i], "%Y-%m-%d"),           # YMD
    format(sample_dates[i], "%d/%m/%Y"),           # DMY
    format(sample_dates[i], "%m-%d-%Y"),           # MDY
    paste(format(sample_dates[i], "%Y-%m-%d"),
          sprintf("%02d:%02d:%02d",
                  sample(0:23, 1), sample(0:59, 1), sample(0:59, 1))), # YMD HMS
    paste(format(sample_dates[i], "%d-%m-%Y"),
          sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1))) # DMY HM
  )
}

datetimeconverter_mixed <- tibble(
  id = 1:n_mixed,

  # Mixed datetime formats
  mixed_datetime = mixed_formats,

  # Participant ID
  participant_id = paste0("P", sprintf("%03d", 1:n_mixed)),

  # Study phase
  study_phase = sample(c("Screening", "Treatment", "Follow-up"), n_mixed, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 5: Edge Cases and Invalid Dates
# ───────────────────────────────────────────────────────────
# Testing error handling and edge cases

datetimeconverter_edge <- tibble(
  id = 1:26,

  # Valid dates with edge cases
  datetime_string = c(
    # Leap year
    "2020-02-29",  # Valid leap day
    "2024-02-29",  # Valid leap day
    "2021-02-29",  # Invalid - not a leap year

    # Month boundaries
    "2023-01-31",  # Last day of January
    "2023-02-28",  # Last day of February (non-leap)
    "2023-04-30",  # Last day of April
    "2023-04-31",  # Invalid - April only has 30 days

    # Year boundaries
    "2019-12-31 23:59:59",  # End of year
    "2020-01-01 00:00:00",  # Start of year

    # Time edge cases
    "2023-06-15 00:00:00",  # Midnight
    "2023-06-15 23:59:59",  # Last second of day

    # Different separators
    "2023/07/15",
    "2023.07.15",
    "15-Jul-2023",

    # Obviously invalid
    "2023-13-01",  # Invalid month
    "2023-00-15",  # Invalid month
    "2023-06-00",  # Invalid day
    "2023-06-32",  # Invalid day

    # Empty/NA
    "",
    "NA",
    "NULL",

    # Ambiguous dates (could be MM-DD or DD-MM)
    "01-02-2023",  # Could be Jan 2 or Feb 1
    "12-11-2023",  # Could be Dec 11 or Nov 12
    "05-08-2023",  # Could be May 8 or Aug 5

    # Valid with different formats
    "2023-W25",    # ISO week
    "2023-Q2"      # Quarter format
  ),

  # Expected outcome (valid/invalid)
  expected_status = c(
    "valid", "valid", "invalid",
    "valid", "valid", "valid", "invalid",
    "valid", "valid",
    "valid", "valid",
    "valid", "valid", "valid",
    "invalid", "invalid", "invalid", "invalid",
    "invalid", "invalid", "invalid",
    "ambiguous", "ambiguous", "ambiguous",
    "special", "special"
  )
)

# ───────────────────────────────────────────────────────────
# Dataset 6: Clinical Timestamps
# ───────────────────────────────────────────────────────────
# Realistic medical/clinical datetime data

n_clinical <- 40

# Patient enrollment dates
enrollment_dates <- seq(as.Date("2022-01-10"), as.Date("2024-06-30"), length.out = n_clinical)

# Create realistic clinical event times
surgery_dates <- enrollment_dates + sample(7:30, n_clinical, replace = TRUE)
followup1_dates <- surgery_dates + sample(30:90, n_clinical, replace = TRUE)
followup2_dates <- followup1_dates + sample(60:180, n_clinical, replace = TRUE)

datetimeconverter_clinical <- tibble(
  patient_id = paste0("PT", sprintf("%04d", 1:n_clinical)),

  # Enrollment timestamp (YMD HMS)
  enrollment_timestamp = paste(
    format(enrollment_dates, "%Y-%m-%d"),
    sprintf("%02d:%02d:%02d",
            sample(8:17, n_clinical, replace = TRUE), # Business hours
            sample(0:59, n_clinical, replace = TRUE),
            sample(0:59, n_clinical, replace = TRUE))
  ),

  # Surgery date (YMD only)
  surgery_date = format(surgery_dates, "%Y-%m-%d"),

  # First follow-up (DMY format from European system)
  followup1_date = format(followup1_dates, "%d/%m/%Y"),

  # Second follow-up (MDY format from US system)
  followup2_date = format(followup2_dates, "%m-%d-%Y"),

  # Lab result timestamp (Unix epoch - exported from lab system)
  lab_timestamp = as.numeric(as.POSIXct(
    paste(format(enrollment_dates + sample(1:7, n_clinical, replace = TRUE), "%Y-%m-%d"),
          sprintf("%02d:%02d:00", sample(6:18, n_clinical, replace = TRUE),
                  sample(0:59, n_clinical, replace = TRUE))),
    tz = "UTC"
  )),

  # Diagnosis
  diagnosis = sample(c("Benign", "Malignant", "Inconclusive"),
                    n_clinical, replace = TRUE),

  # Treatment group
  treatment = sample(c("Surgery", "Radiation", "Chemotherapy", "Combined"),
                    n_clinical, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 7: Timezone Challenge Dataset
# ───────────────────────────────────────────────────────────
# Same moment in time expressed in different timezones

n_tz <- 20

utc_times <- seq(as.POSIXct("2024-01-15 12:00:00", tz = "UTC"),
                 as.POSIXct("2024-06-30 18:00:00", tz = "UTC"),
                 length.out = n_tz)

datetimeconverter_timezone <- tibble(
  id = 1:n_tz,

  # UTC time
  datetime_utc = format(utc_times, "%Y-%m-%d %H:%M:%S", tz = "UTC"),

  # New York time (EST/EDT)
  datetime_newyork = format(utc_times, "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),

  # London time (GMT/BST)
  datetime_london = format(utc_times, "%Y-%m-%d %H:%M:%S", tz = "Europe/London"),

  # Tokyo time (JST)
  datetime_tokyo = format(utc_times, "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo"),

  # Unix timestamp (same for all timezones)
  unix_timestamp = as.numeric(utc_times),

  # Event description
  event = sample(c("Meeting", "Webinar", "Conference_Call", "Data_Upload"),
                n_tz, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 8: Component Extraction Test
# ───────────────────────────────────────────────────────────
# Designed to test extraction of all datetime components

n_component <- 30

component_dates <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), length.out = n_component)
component_times <- sprintf("%02d:%02d:%02d",
                          sample(0:23, n_component, replace = TRUE),
                          sample(0:59, n_component, replace = TRUE),
                          sample(0:59, n_component, replace = TRUE))

datetimeconverter_components <- tibble(
  id = 1:n_component,

  # Full datetime
  full_datetime = paste(format(component_dates, "%Y-%m-%d"), component_times),

  # Expected components for verification
  expected_year = year(component_dates),
  expected_month = month(component_dates),
  expected_monthname = month(component_dates, label = TRUE, abbr = FALSE),
  expected_day = day(component_dates),
  expected_dayname = wday(component_dates, label = TRUE, abbr = FALSE),
  expected_weeknum = week(component_dates),
  expected_quarter = quarter(component_dates),
  expected_dayofyear = yday(component_dates),

  # Sample type
  sample_type = sample(c("Blood", "Tissue", "Urine", "Other"),
                      n_component, replace = TRUE)
)

# ───────────────────────────────────────────────────────────
# Dataset 9: Small Edge Case Dataset
# ───────────────────────────────────────────────────────────
# Minimal dataset for quick testing

datetimeconverter_small <- tibble(
  id = 1:5,

  # Different formats
  datetime_ymd = c("2023-01-15", "2023-06-20", "2024-02-29", "2024-12-31", "2020-03-01"),
  datetime_dmy = c("15/01/2023", "20/06/2023", "29/02/2024", "31/12/2024", "01/03/2020"),
  excel_serial = c(44941, 45097, 45351, 45657, 43891),
  unix_epoch = c(1673740800, 1687219200, 1709164800, 1735603200, 1583020800),

  # Category
  category = c("A", "B", "A", "C", "B")
)

# ═══════════════════════════════════════════════════════════
# Save all datasets in multiple formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  "datetimeconverter_test" = datetimeconverter_test,
  "datetimeconverter_excel" = datetimeconverter_excel,
  "datetimeconverter_unix" = datetimeconverter_unix,
  "datetimeconverter_mixed" = datetimeconverter_mixed,
  "datetimeconverter_edge" = datetimeconverter_edge,
  "datetimeconverter_clinical" = datetimeconverter_clinical,
  "datetimeconverter_timezone" = datetimeconverter_timezone,
  "datetimeconverter_components" = datetimeconverter_components,
  "datetimeconverter_small" = datetimeconverter_small
)

# Save as .rda files
for (name in names(datasets)) {
  assign(name, datasets[[name]])
  save(list = name, file = here::here("data", paste0(name, ".rda")))
  message("✓ Saved ", name, ".rda")
}

# Save as .csv files
for (name in names(datasets)) {
  write.csv(datasets[[name]],
           here::here("data", paste0(name, ".csv")),
           row.names = FALSE)
  message("✓ Saved ", name, ".csv")
}

# Save as .xlsx files
for (name in names(datasets)) {
  writexl::write_xlsx(datasets[[name]],
                     here::here("data", paste0(name, ".xlsx")))
  message("✓ Saved ", name, ".xlsx")
}

# Save as .omv files (jamovi format)
for (name in names(datasets)) {
  jmvReadWrite::write_omv(datasets[[name]],
                         here::here("data", paste0(name, ".omv")))
  message("✓ Saved ", name, ".omv")
}

message("\n═══════════════════════════════════════════════════════════")
message("✓ All datetimeconverter test datasets generated successfully!")
message("═══════════════════════════════════════════════════════════")
message("\nDatasets created:")
message("  1. datetimeconverter_test (50 obs, various formats)")
message("  2. datetimeconverter_excel (40 obs, Excel serial dates)")
message("  3. datetimeconverter_unix (40 obs, Unix epoch timestamps)")
message("  4. datetimeconverter_mixed (30 obs, mixed formats)")
message("  5. datetimeconverter_edge (25 obs, edge cases)")
message("  6. datetimeconverter_clinical (40 obs, clinical data)")
message("  7. datetimeconverter_timezone (20 obs, timezone tests)")
message("  8. datetimeconverter_components (30 obs, component extraction)")
message("  9. datetimeconverter_small (5 obs, quick testing)")
message("\nTotal: 9 datasets × 4 formats = 36 files")
message("═══════════════════════════════════════════════════════════")
