# ═══════════════════════════════════════════════════════════
# Example Usage: datetimeconverter
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples demonstrating datetimeconverter usage
# for datetime format conversion and component extraction

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Date Format Conversion
# ═══════════════════════════════════════════════════════════
# Convert standard date formats (YMD, DMY, MDY)

data(datetimeconverter_test, package = "ClinicoPath")

# Convert YMD format
result_ymd <- datetimeconverter(
  data = datetimeconverter_test,
  datetime_var = "date_ymd",
  datetime_format = "ymd"
)

# Convert DMY format (European)
result_dmy <- datetimeconverter(
  data = datetimeconverter_test,
  datetime_var = "date_dmy",
  datetime_format = "dmy"
)

# Convert MDY format (US)
result_mdy <- datetimeconverter(
  data = datetimeconverter_test,
  datetime_var = "date_mdy",
  datetime_format = "mdy"
)

# Clinical Interpretation:
# - YMD (ISO 8601): International standard, unambiguous
# - DMY: Common in Europe, Australia, medical records
# - MDY: Common in US healthcare systems

# ═══════════════════════════════════════════════════════════
# Example 2: Datetime with Time Components
# ═══════════════════════════════════════════════════════════
# Convert datetime values including hours, minutes, seconds

# Convert datetime with HMS
result_ymdhms <- datetimeconverter(
  data = datetimeconverter_test,
  datetime_var = "datetime_ymdhms",
  datetime_format = "ymdhms",
  extract_hour = TRUE,
  extract_minute = TRUE
)

# Clinical Application:
# - Surgery start/end times
# - Medication administration timestamps
# - Laboratory sample collection times
# - ICU monitoring timestamps

# ═══════════════════════════════════════════════════════════
# Example 3: Excel Serial Date Conversion
# ═══════════════════════════════════════════════════════════
# Convert Excel serial dates to standard datetime format

data(datetimeconverter_excel, package = "ClinicoPath")

# Excel Windows format (1900 date system)
result_excel <- datetimeconverter(
  data = datetimeconverter_excel,
  datetime_var = "excel_serial_date",
  datetime_format = "excel_serial"
)

# Excel Mac format (1904 date system)
result_excel_1904 <- datetimeconverter(
  data = datetimeconverter_excel,
  datetime_var = "excel_1904_date",
  datetime_format = "excel_1904"
)

# Clinical Context:
# Excel serial dates are common in:
# - Exported hospital information systems
# - Laboratory databases
# - Clinical trial data management systems
# - Registry data exports

# ═══════════════════════════════════════════════════════════
# Example 4: Unix Epoch Timestamp Conversion
# ═══════════════════════════════════════════════════════════
# Convert Unix timestamps from database systems

data(datetimeconverter_unix, package = "ClinicoPath")

# Unix epoch seconds
result_unix <- datetimeconverter(
  data = datetimeconverter_unix,
  datetime_var = "unix_timestamp",
  datetime_format = "unix_epoch",
  timezone = "UTC"
)

# Unix epoch milliseconds
result_unix_ms <- datetimeconverter(
  data = datetimeconverter_unix,
  datetime_var = "unix_milliseconds",
  datetime_format = "unix_milliseconds",
  timezone = "UTC"
)

# Clinical Application:
# Unix timestamps common in:
# - Electronic health record systems
# - Medical device data exports
# - Web-based clinical applications
# - API data feeds

# ═══════════════════════════════════════════════════════════
# Example 5: Auto-Detection of Date Format
# ═══════════════════════════════════════════════════════════
# Let the function automatically detect the datetime format

data(datetimeconverter_mixed, package = "ClinicoPath")

# Auto-detect format
result_auto <- datetimeconverter(
  data = datetimeconverter_mixed,
  datetime_var = "mixed_dates",
  datetime_format = "auto",
  show_quality_metrics = TRUE
)

# Clinical Context:
# Useful when:
# - Working with data from multiple sources
# - Uncertain about date format
# - Cleaning legacy data
# - Quality control checks needed

# ═══════════════════════════════════════════════════════════
# Example 6: Temporal Component Extraction
# ═══════════════════════════════════════════════════════════
# Extract year, month, day, and other temporal components

data(datetimeconverter_components, package = "ClinicoPath")

# Extract date components
result_components <- datetimeconverter(
  data = datetimeconverter_components,
  datetime_var = "date_standard",
  datetime_format = "ymd",
  extract_year = TRUE,
  extract_month = TRUE,
  extract_monthname = TRUE,
  extract_day = TRUE,
  extract_dayname = TRUE,
  extract_weeknum = TRUE,
  extract_quarter = TRUE,
  extract_dayofyear = TRUE
)

# Clinical Applications:
# - Year: Temporal trends, cohort analysis
# - Month/Quarter: Seasonal patterns (flu, allergies)
# - Week number: Epidemiological surveillance
# - Day of week: Healthcare utilization patterns
# - Day of year: Outbreak detection

# ═══════════════════════════════════════════════════════════
# Example 7: Time Component Extraction
# ═══════════════════════════════════════════════════════════
# Extract hour, minute, second from datetime

# Extract time components
result_time <- datetimeconverter(
  data = datetimeconverter_components,
  datetime_var = "datetime_full",
  datetime_format = "ymdhms",
  extract_hour = TRUE,
  extract_minute = TRUE,
  extract_second = TRUE,
  extract_dayname = TRUE
)

# Clinical Applications:
# - Hour: Circadian rhythm studies, shift analysis
# - Minute/Second: Precise event timing (cardiac events)
# - Day of week + Hour: Emergency department patterns
# - Operating room utilization by time of day

# ═══════════════════════════════════════════════════════════
# Example 8: Timezone Conversion
# ═══════════════════════════════════════════════════════════
# Convert datetimes across timezones for multi-center trials

data(datetimeconverter_timezone, package = "ClinicoPath")

# Convert UTC to New York time
result_ny <- datetimeconverter(
  data = datetimeconverter_timezone,
  datetime_var = "datetime_utc",
  datetime_format = "ymdhms",
  timezone = "America/New_York",
  extract_hour = TRUE
)

# Convert UTC to London time
result_london <- datetimeconverter(
  data = datetimeconverter_timezone,
  datetime_var = "datetime_utc",
  datetime_format = "ymdhms",
  timezone = "Europe/London",
  extract_hour = TRUE
)

# Convert UTC to Tokyo time
result_tokyo <- datetimeconverter(
  data = datetimeconverter_timezone,
  datetime_var = "datetime_utc",
  datetime_format = "ymdhms",
  timezone = "Asia/Tokyo",
  extract_hour = TRUE
)

# Clinical Context:
# Multi-center international trials:
# - Synchronize adverse event reporting
# - Coordinate study visits across sites
# - Analyze circadian patterns globally
# - Telemedicine consultations

# ═══════════════════════════════════════════════════════════
# Example 9: Clinical Admission Date Analysis
# ═══════════════════════════════════════════════════════════
# Analyze hospital admission patterns

data(datetimeconverter_clinical, package = "ClinicoPath")

# Extract temporal features from admission dates
result_admission <- datetimeconverter(
  data = datetimeconverter_clinical,
  datetime_var = "AdmissionDate",
  datetime_format = "ymd",
  extract_year = TRUE,
  extract_month = TRUE,
  extract_monthname = TRUE,
  extract_dayname = TRUE,
  extract_weeknum = TRUE,
  extract_quarter = TRUE
)

# Clinical Applications:
# - Admission patterns by day of week (weekend effect)
# - Seasonal admission trends (respiratory illness)
# - Quarterly volume for resource planning
# - Year-over-year comparisons
# - Epidemiological week for outbreak detection

# Typical Findings:
# - Monday peaks in elective admissions
# - Winter peaks for respiratory conditions
# - Summer peaks for trauma/accidents
# - Weekend admissions often more acute

# ═══════════════════════════════════════════════════════════
# Example 10: Surgery Scheduling Analysis
# ═══════════════════════════════════════════════════════════
# Analyze operating room utilization patterns

# Extract scheduling components from surgery datetime
result_surgery <- datetimeconverter(
  data = datetimeconverter_clinical,
  datetime_var = "SurgeryDateTime",
  datetime_format = "ymdhms",
  timezone = "America/New_York",
  extract_dayname = TRUE,
  extract_hour = TRUE,
  extract_month = TRUE
)

# Clinical Applications:
# - OR block utilization by day of week
# - First-case start times (typically 7:30 AM)
# - Evening/weekend emergency surgery patterns
# - Surgeon preference patterns
# - Seasonal case mix variations

# Typical Patterns:
# - Monday-Thursday: Higher elective volume
# - 7 AM-3 PM: Peak OR utilization
# - Evening/night: Emergency cases
# - Weekend: Trauma and urgent oncology

# ═══════════════════════════════════════════════════════════
# Example 11: Epidemiological Week Analysis
# ═══════════════════════════════════════════════════════════
# Disease surveillance using epidemiological weeks

# Extract week number and year for epi week reporting
result_epiweek <- datetimeconverter(
  data = datetimeconverter_clinical,
  datetime_var = "AdmissionDate",
  datetime_format = "ymd",
  extract_year = TRUE,
  extract_weeknum = TRUE,
  extract_month = TRUE,
  show_quality_metrics = TRUE
)

# Clinical Context:
# CDC Epidemiological Week (MMWR Week):
# - Week 1: First week with ≥4 days in new year
# - Used for disease surveillance (flu, COVID-19)
# - Standardized reporting to public health agencies
# - Outbreak detection and response

# Applications:
# - Influenza surveillance (week 40 to week 20)
# - Foodborne illness outbreak detection
# - Vaccine effectiveness monitoring
# - Healthcare-associated infection trends

# ═══════════════════════════════════════════════════════════
# Example 12: Data Quality Assessment
# ═══════════════════════════════════════════════════════════
# Check datetime conversion quality before analysis

# Comprehensive quality check
result_quality <- datetimeconverter(
  data = datetimeconverter_test,
  datetime_var = "date_ymd",
  datetime_format = "auto",
  show_quality_metrics = TRUE,
  show_preview = TRUE,
  preview_rows = 20
)

# Quality Metrics Interpretation:
# - Total values: Dataset size
# - Successfully converted: Valid datetime values
# - Failed conversions: Invalid or unparseable values
# - NA values: Missing data
# - Conversion rate: % successfully parsed
# - Date range: Min/max dates (detect outliers)

# Quality Thresholds:
# - >95% conversion rate: Good data quality
# - 90-95%: Acceptable with review
# - <90%: Investigate format issues
# - Check min/max dates for plausibility

# ═══════════════════════════════════════════════════════════
# Example 13: Legacy System Migration
# ═══════════════════════════════════════════════════════════
# Migrate dates from legacy Excel system to ISO 8601

# Convert Excel dates to standardized format
result_migration <- datetimeconverter(
  data = datetimeconverter_excel,
  datetime_var = "excel_serial_date",
  datetime_format = "excel_serial",
  output_as_text = TRUE,
  custom_output_format = "%Y-%m-%d",
  show_quality_metrics = TRUE
)

# Migration Workflow:
# 1. Identify source format (Excel, Unix, etc.)
# 2. Convert to standard ISO 8601 format
# 3. Validate conversion quality (>95% success)
# 4. Extract components if needed for analysis
# 5. Document conversion process

# Common Migration Scenarios:
# - Excel → ISO 8601 for database import
# - Unix epoch → human-readable for reports
# - Mixed formats → standardized format
# - Add timezone information where missing

# ═══════════════════════════════════════════════════════════
# Example 14: Clinical Trial Temporal Analysis
# ═══════════════════════════════════════════════════════════
# Extract multiple temporal features for trial analysis

# Comprehensive temporal feature extraction
result_trial <- datetimeconverter(
  data = datetimeconverter_clinical,
  datetime_var = "AdmissionDate",
  datetime_format = "ymd",
  timezone = "UTC",
  extract_year = TRUE,
  extract_month = TRUE,
  extract_monthname = TRUE,
  extract_day = TRUE,
  extract_dayname = TRUE,
  extract_weeknum = TRUE,
  extract_quarter = TRUE,
  extract_dayofyear = TRUE,
  show_quality_metrics = TRUE
)

# Clinical Trial Applications:
# - Enrollment patterns over time
# - Seasonal effects on outcomes
# - Site activation timeline
# - Visit window compliance
# - Retention analysis by enrollment period

# Downstream Analysis:
# After component extraction, can analyze:
# - Seasonal variation in disease activity
# - Weekend vs weekday outcome differences
# - Monthly enrollment targets
# - Quarterly milestone tracking

# ═══════════════════════════════════════════════════════════
# Example 15: Laboratory Timestamp Processing
# ═══════════════════════════════════════════════════════════
# Process laboratory result timestamps with timezone

# Convert lab timestamps with timezone awareness
result_lab <- datetimeconverter(
  data = datetimeconverter_clinical,
  datetime_var = "LabTimestamp",
  datetime_format = "ymdhms",
  timezone = "America/New_York",
  extract_hour = TRUE,
  extract_dayname = TRUE,
  show_preview = TRUE,
  preview_rows = 10
)

# Clinical Context:
# Laboratory timestamp considerations:
# - Collection time vs result time
# - Timezone of collection site
# - Time-sensitive tests (glucose, cortisol)
# - Circadian rhythm (morning vs evening)
# - Fasting status verification

# Applications:
# - Time to result analysis
# - Circadian biomarker patterns
# - Shift-based quality control
# - Stat vs routine turnaround time
# - Pre-analytical error detection

# ═══════════════════════════════════════════════════════════
# Reporting Guidelines
# ═══════════════════════════════════════════════════════════

# When reporting datetime conversion results:
#
# 1. Data Source Description
#    - Original datetime format
#    - Source system (EHR, Excel, database)
#    - Timezone information
#    - Date range
#
# 2. Conversion Methodology
#    - Format detection method (auto vs manual)
#    - Timezone handling approach
#    - Custom format specifications
#    - Quality thresholds applied
#
# 3. Quality Metrics
#    - Total values processed
#    - Successful conversion rate
#    - Failed conversions and reasons
#    - Date range plausibility check
#    - Missing value handling
#
# 4. Extracted Components
#    - Components extracted and why
#    - Clinical relevance of each component
#    - Downstream analysis planned
#
# 5. Validation Steps
#    - Spot checks of converted values
#    - Cross-reference with source
#    - Timezone verification
#    - Edge case handling (leap years, DST)

# ═══════════════════════════════════════════════════════════
# Common Clinical Datetime Formats
# ═══════════════════════════════════════════════════════════

# ISO 8601: "2024-03-15" or "2024-03-15 14:30:00"
# - International standard
# - Sortable and unambiguous
# - Recommended for databases

# US Format: "03/15/2024" or "3/15/2024 2:30 PM"
# - Common in US healthcare
# - Can be ambiguous (month vs day)
# - Often requires clarification

# European: "15/03/2024" or "15.03.2024"
# - Common in Europe and Australia
# - Day-first convention
# - Ambiguous for US readers

# Excel Serial: 45373 (days since 1900-01-01)
# - Common in Excel exports
# - Two systems: 1900 (Windows) and 1904 (Mac)
# - Requires conversion for readability

# Unix Epoch: 1710511800 (seconds since 1970-01-01 UTC)
# - Common in databases and APIs
# - Always UTC timezone
# - Precise to the second

# ═══════════════════════════════════════════════════════════
# Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Always specify timezone explicitly for datetime with time
# 2. Use ISO 8601 format for storage and data exchange
# 3. Document original format and conversion method
# 4. Validate conversion quality (>95% success rate)
# 5. Check date range plausibility (no future dates for past events)
# 6. Preserve original datetime column until validation complete
# 7. Extract temporal components before time-to-event analysis
# 8. Consider daylight saving time for timezone conversions
# 9. Use quality metrics to identify conversion issues
# 10. Preview converted values before proceeding with analysis
