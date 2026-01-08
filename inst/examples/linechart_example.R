# ═══════════════════════════════════════════════════════════
# Example Usage: linechart (Line Chart - Time Series Analysis)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the linechart jamovi function
# which creates line charts for time series analysis and trend visualization.
#
# Created: 2026-01-05
# Test data: linechart_simple, linechart_grouped, linechart_clinical,
#            linechart_short, linechart_long, linechart_irregular,
#            linechart_multiple, linechart_patterns

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Line Chart
# ═══════════════════════════════════════════════════════════
# Scenario: Simple time series visualization

data(linechart_simple, package = "ClinicoPath")

linechart(
  data = linechart_simple,
  xvar = "time_point",
  yvar = "value"
)

# ═══════════════════════════════════════════════════════════
# Example 2: Line Chart with Points
# ═══════════════════════════════════════════════════════════
# Scenario: Show individual data points on the line

linechart(
  data = linechart_simple,
  xvar = "time_point",
  yvar = "value",
  points = TRUE,
  title = "Time Series with Data Points"
)

# ═══════════════════════════════════════════════════════════
# Example 3: With Custom Labels
# ═══════════════════════════════════════════════════════════
# Scenario: Add descriptive axis labels and title

linechart(
  data = linechart_simple,
  xvar = "time_point",
  yvar = "temperature",
  xlabel = "Time Point",
  ylabel = "Temperature (°C)",
  title = "Temperature Variation Over Time"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Grouped Line Chart
# ═══════════════════════════════════════════════════════════
# Scenario: Multiple treatment groups

data(linechart_grouped, package = "ClinicoPath")

linechart(
  data = linechart_grouped,
  xvar = "time_point",
  yvar = "lab_value",
  groupby = "treatment"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Grouped with Confidence Intervals
# ═══════════════════════════════════════════════════════════
# Scenario: Show uncertainty in grouped time series

linechart(
  data = linechart_grouped,
  xvar = "time_point",
  yvar = "lab_value",
  groupby = "treatment",
  confidence = TRUE,
  title = "Treatment Response with 95% CI"
)

# ═══════════════════════════════════════════════════════════
# Example 6: With Trend Line
# ═══════════════════════════════════════════════════════════
# Scenario: Add linear regression trend line

linechart(
  data = linechart_grouped,
  xvar = "time_point",
  yvar = "lab_value",
  groupby = "treatment",
  trendline = TRUE,
  title = "Treatment Response with Trend Lines"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Confidence Intervals and Trend Lines
# ═══════════════════════════════════════════════════════════
# Scenario: Combine confidence intervals with trends

linechart(
  data = linechart_grouped,
  xvar = "time_point",
  yvar = "lab_value",
  groupby = "treatment",
  confidence = TRUE,
  trendline = TRUE,
  title = "Complete Analysis with CI and Trends"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Clinical Time Series
# ═══════════════════════════════════════════════════════════
# Scenario: Disease progression monitoring

data(linechart_clinical, package = "ClinicoPath")

linechart(
  data = linechart_clinical,
  xvar = "week",
  yvar = "tumor_marker",
  groupby = "disease_stage",
  xlabel = "Week",
  ylabel = "Tumor Marker Level",
  title = "Tumor Marker Progression by Disease Stage"
)

# ═══════════════════════════════════════════════════════════
# Example 9: With Reference Line
# ═══════════════════════════════════════════════════════════
# Scenario: Show normal range or threshold

linechart(
  data = linechart_clinical,
  xvar = "week",
  yvar = "tumor_marker",
  groupby = "disease_stage",
  refline = 100,
  reflineLabel = "Normal Range",
  title = "Tumor Marker with Reference Line"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Clinical with All Features
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive clinical visualization

linechart(
  data = linechart_clinical,
  xvar = "week",
  yvar = "tumor_marker",
  groupby = "disease_stage",
  confidence = TRUE,
  trendline = TRUE,
  refline = 100,
  reflineLabel = "Upper Limit Normal",
  xlabel = "Week",
  ylabel = "Tumor Marker (ng/mL)",
  title = "Clinical Trial: Tumor Marker Response"
)

# ═══════════════════════════════════════════════════════════
# Example 11: WBC Count Monitoring
# ═══════════════════════════════════════════════════════════
# Scenario: Monitor white blood cell count during treatment

linechart(
  data = linechart_clinical,
  xvar = "week",
  yvar = "wbc_count",
  groupby = "disease_stage",
  refline = 4.5,
  reflineLabel = "Lower Limit Normal",
  xlabel = "Week",
  ylabel = "WBC Count (×10⁹/L)",
  title = "White Blood Cell Count During Treatment"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Short Time Series
# ═══════════════════════════════════════════════════════════
# Scenario: Limited time points with clear trends

data(linechart_short, package = "ClinicoPath")

linechart(
  data = linechart_short,
  xvar = "month",
  yvar = "measurement",
  groupby = "response_group",
  points = TRUE,
  xlabel = "Month",
  ylabel = "Measurement",
  title = "Response Assessment (Short Follow-up)"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Short Series with Trend Analysis
# ═══════════════════════════════════════════════════════════
# Scenario: Identify response patterns

linechart(
  data = linechart_short,
  xvar = "month",
  yvar = "measurement",
  groupby = "response_group",
  trendline = TRUE,
  confidence = TRUE,
  title = "Treatment Response Pattern Analysis"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Long Time Series (Daily Data)
# ═══════════════════════════════════════════════════════════
# Scenario: Annual seasonal pattern

data(linechart_long, package = "ClinicoPath")

linechart(
  data = linechart_long,
  xvar = "day",
  yvar = "seasonal_value",
  xlabel = "Day of Year",
  ylabel = "Value",
  title = "Annual Seasonal Pattern"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Long Series with Smoothing
# ═══════════════════════════════════════════════════════════
# Scenario: Apply smoothing to reveal underlying trend

linechart(
  data = linechart_long,
  xvar = "day",
  yvar = "cyclic_value",
  smooth = TRUE,
  title = "Cyclic Pattern with Loess Smoothing"
)

# ═══════════════════════════════════════════════════════════
# Example 16: Irregular Time Intervals
# ═══════════════════════════════════════════════════════════
# Scenario: Non-uniform measurement times

data(linechart_irregular, package = "ClinicoPath")

linechart(
  data = linechart_irregular,
  xvar = "time",
  yvar = "value",
  groupby = "obs_type",
  xlabel = "Time (irregular intervals)",
  ylabel = "Value",
  title = "Irregular Time Series"
)

# ═══════════════════════════════════════════════════════════
# Example 17: Irregular with Smoothing
# ═══════════════════════════════════════════════════════════
# Scenario: Handle irregular spacing with smoothing

linechart(
  data = linechart_irregular,
  xvar = "time",
  yvar = "value",
  groupby = "obs_type",
  smooth = TRUE,
  title = "Irregular Time Series (Smoothed)"
)

# ═══════════════════════════════════════════════════════════
# Example 18: Multiple Measurements per Time Point
# ═══════════════════════════════════════════════════════════
# Scenario: Intervention study with multiple patients

data(linechart_multiple, package = "ClinicoPath")

linechart(
  data = linechart_multiple,
  xvar = "visit",
  yvar = "systolic_bp",
  groupby = "intervention",
  xlabel = "Visit Number",
  ylabel = "Systolic Blood Pressure (mmHg)",
  title = "Blood Pressure Response by Intervention"
)

# ═══════════════════════════════════════════════════════════
# Example 19: Multiple with Confidence Intervals
# ═══════════════════════════════════════════════════════════
# Scenario: Show variability across patients

linechart(
  data = linechart_multiple,
  xvar = "visit",
  yvar = "systolic_bp",
  groupby = "intervention",
  confidence = TRUE,
  xlabel = "Visit Number",
  ylabel = "Systolic Blood Pressure (mmHg)",
  title = "Blood Pressure with 95% CI"
)

# ═══════════════════════════════════════════════════════════
# Example 20: Multiple with Reference Line
# ═══════════════════════════════════════════════════════════
# Scenario: Treatment goal threshold

linechart(
  data = linechart_multiple,
  xvar = "visit",
  yvar = "systolic_bp",
  groupby = "intervention",
  confidence = TRUE,
  refline = 120,
  reflineLabel = "Treatment Goal",
  xlabel = "Visit Number",
  ylabel = "Systolic BP (mmHg)",
  title = "Blood Pressure with Treatment Goal"
)

# ═══════════════════════════════════════════════════════════
# Example 21: Cholesterol Monitoring
# ═══════════════════════════════════════════════════════════
# Scenario: Track cholesterol during intervention

linechart(
  data = linechart_multiple,
  xvar = "visit",
  yvar = "cholesterol",
  groupby = "intervention",
  refline = 200,
  reflineLabel = "Target",
  xlabel = "Visit",
  ylabel = "Total Cholesterol (mg/dL)",
  title = "Cholesterol Reduction Over Time"
)

# ═══════════════════════════════════════════════════════════
# Example 22: Different Trend Patterns
# ═══════════════════════════════════════════════════════════
# Scenario: Compare increasing, decreasing, stable trends

data(linechart_patterns, package = "ClinicoPath")

linechart(
  data = linechart_patterns,
  xvar = "time_index",
  yvar = "value",
  groupby = "pattern_type",
  xlabel = "Time Index",
  ylabel = "Value",
  title = "Different Trend Patterns"
)

# ═══════════════════════════════════════════════════════════
# Example 23: Patterns with Trend Lines
# ═══════════════════════════════════════════════════════════
# Scenario: Quantify trend differences

linechart(
  data = linechart_patterns,
  xvar = "time_index",
  yvar = "value",
  groupby = "pattern_type",
  trendline = TRUE,
  title = "Trend Pattern Comparison"
)

# ═══════════════════════════════════════════════════════════
# Example 24: Colorblind-Safe Palette
# ═══════════════════════════════════════════════════════════
# Scenario: Accessible color scheme

linechart(
  data = linechart_grouped,
  xvar = "time_point",
  yvar = "lab_value",
  groupby = "treatment",
  colorPalette = "colorblind",
  title = "Treatment Response (Colorblind-Safe)"
)

# ═══════════════════════════════════════════════════════════
# Example 25: Viridis Palette
# ═══════════════════════════════════════════════════════════
# Scenario: Perceptually uniform colors

linechart(
  data = linechart_multiple,
  xvar = "visit",
  yvar = "systolic_bp",
  groupby = "intervention",
  colorPalette = "viridis",
  confidence = TRUE,
  title = "Blood Pressure (Viridis Palette)"
)

# ═══════════════════════════════════════════════════════════
# Example 26: Clinical Palette
# ═══════════════════════════════════════════════════════════
# Scenario: Professional clinical presentation

linechart(
  data = linechart_clinical,
  xvar = "week",
  yvar = "tumor_marker",
  groupby = "disease_stage",
  colorPalette = "clinical",
  confidence = TRUE,
  title = "Clinical Trial Results"
)

# ═══════════════════════════════════════════════════════════
# Example 27: Minimal Theme
# ═══════════════════════════════════════════════════════════
# Scenario: Clean, minimal appearance

linechart(
  data = linechart_grouped,
  xvar = "time_point",
  yvar = "lab_value",
  groupby = "treatment",
  theme = "minimal",
  title = "Minimal Theme"
)

# ═══════════════════════════════════════════════════════════
# Example 28: Publication Theme
# ═══════════════════════════════════════════════════════════
# Scenario: Publication-ready figure

linechart(
  data = linechart_clinical,
  xvar = "week",
  yvar = "tumor_marker",
  groupby = "disease_stage",
  confidence = TRUE,
  trendline = TRUE,
  refline = 100,
  reflineLabel = "ULN",
  theme = "publication",
  colorPalette = "clinical",
  xlabel = "Week",
  ylabel = "Tumor Marker (ng/mL)",
  title = "Disease Progression by Stage",
  width = 1000,
  height = 700
)

# ═══════════════════════════════════════════════════════════
# Example 29: Without Points (Lines Only)
# ═══════════════════════════════════════════════════════════
# Scenario: Clean line visualization

linechart(
  data = linechart_long,
  xvar = "day",
  yvar = "seasonal_value",
  points = FALSE,
  smooth = TRUE,
  title = "Seasonal Pattern (Lines Only)"
)

# ═══════════════════════════════════════════════════════════
# Example 30: Complete Publication Figure
# ═══════════════════════════════════════════════════════════
# Scenario: All features for manuscript submission

linechart(
  data = linechart_multiple,
  xvar = "visit",
  yvar = "systolic_bp",
  groupby = "intervention",
  confidence = TRUE,
  trendline = TRUE,
  points = TRUE,
  refline = 120,
  reflineLabel = "Treatment Goal (<120 mmHg)",
  theme = "publication",
  colorPalette = "colorblind",
  xlabel = "Study Visit",
  ylabel = "Systolic Blood Pressure (mmHg)",
  title = "Antihypertensive Drug Comparison Study",
  width = 1200,
  height = 800
)
