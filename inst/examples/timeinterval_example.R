# ═══════════════════════════════════════════════════════════
# Time Interval Calculation Examples
# ═══════════════════════════════════════════════════════════

library(ClinicoPath)

# ═══ EXAMPLE 1: Basic Time Interval Calculation ═══
data(timeinterval_test)

timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  time_format = "ymd",
  output_unit = "months"
)

# ═══ EXAMPLE 2: Different Date Formats ═══

# European format (DD-MM-YYYY)
data(timeinterval_dmy)
timeinterval(
  data = timeinterval_dmy,
  dx_date = "start_date",
  fu_date = "end_date",
  time_format = "dmy"
)

# US format (MM-DD-YYYY)
data(timeinterval_mdy)
timeinterval(
  data = timeinterval_mdy,
  dx_date = "start_date",
  fu_date = "end_date",
  time_format = "mdy"
)

# ═══ EXAMPLE 3: Automatic Format Detection ═══
timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  time_format = "auto"
)

# ═══ EXAMPLE 4: Different Time Units ═══

# Days
timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  output_unit = "days"
)

# Weeks
timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  output_unit = "weeks"
)

# Years
timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  output_unit = "years"
)

# ═══ EXAMPLE 5: Landmark Analysis ═══
data(timeinterval_landmark)

# 6-month landmark analysis
timeinterval(
  data = timeinterval_landmark,
  dx_date = "enrollment_date",
  fu_date = "last_contact",
  use_landmark = TRUE,
  landmark_time = 6,
  output_unit = "months"
)

# ═══ EXAMPLE 6: Data Quality Assessment ═══
timeinterval(
  data = timeinterval_test,
  dx_date = "diagnosis_date",
  fu_date = "followup_date",
  remove_negative = TRUE,
  remove_extreme = TRUE,
  extreme_multiplier = 2.0,
  include_quality_metrics = TRUE
)

# ═══ EXAMPLE 7: Clinical Trial Analysis ═══
data(timeinterval_trial)

timeinterval(
  data = timeinterval_trial,
  dx_date = "enrollment_date",
  fu_date = "followup_date",
  output_unit = "months",
  add_times = TRUE,
  include_quality_metrics = TRUE,
  confidence_level = 95,
  show_summary = TRUE
)

# ═══ EXAMPLE 8: Short-term Study (Days) ═══
data(timeinterval_shortterm)

timeinterval(
  data = timeinterval_shortterm,
  dx_date = "admission_date",
  fu_date = "discharge_date",
  output_unit = "days",
  time_basis = "calendar"
)

# ═══ EXAMPLE 9: Long-term Study (Years) ═══
data(timeinterval_longterm)

timeinterval(
  data = timeinterval_longterm,
  dx_date = "baseline_date",
  fu_date = "last_followup",
  output_unit = "years",
  time_basis = "standardized"
)

# ═══ EXAMPLE 10: Person-Time Calculation ═══
timeinterval(
  data = timeinterval_trial,
  dx_date = "enrollment_date",
  fu_date = "followup_date",
  output_unit = "months",
  time_basis = "standardized",
  add_times = TRUE,
  show_summary = TRUE,
  show_glossary = TRUE
)

# ═══ EXAMPLE 11: Complete Analysis Pipeline ═══
timeinterval(
  data = timeinterval_trial,
  dx_date = "enrollment_date",
  fu_date = "followup_date",
  time_format = "ymd",
  output_unit = "months",
  time_basis = "standardized",
  use_landmark = TRUE,
  landmark_time = 6,
  remove_negative = TRUE,
  remove_extreme = TRUE,
  extreme_multiplier = 2.0,
  add_times = TRUE,
  include_quality_metrics = TRUE,
  confidence_level = 95,
  show_summary = TRUE,
  show_glossary = TRUE
)
