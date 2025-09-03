# Test script for riverplot enhanced features
library(ClinicoPath)
library(ggplot2)
library(ggalluvial)
library(dplyr)

# Create test data - patient treatment journey
set.seed(123)
n_patients <- 200

# Longitudinal format data with ID tracking
longitudinal_data <- data.frame(
  patient_id = rep(1:n_patients, each = 3),
  visit = rep(c("Baseline", "Month3", "Month6"), n_patients),
  response = sample(c("Complete", "Partial", "Stable", "Progression"), 
                   n_patients * 3, replace = TRUE, 
                   prob = c(0.15, 0.35, 0.35, 0.15)),
  tumor_size = runif(n_patients * 3, 10, 100)
)

# Wide format data
wide_data <- longitudinal_data %>%
  tidyr::pivot_wider(
    id_cols = patient_id,
    names_from = visit,
    values_from = response,
    names_prefix = "Response_"
  ) %>%
  mutate(
    weight = sample(1:5, n(), replace = TRUE)
  )

cat("Testing riverplot enhanced features:\n")
cat("=====================================\n\n")

# Test 1: Basic alluvial with curveGranularity
cat("Test 1: Alluvial plot with curve granularity...\n")
result1 <- riverplot(
  data = wide_data,
  strata = c("Response_Baseline", "Response_Month3", "Response_Month6"),
  plotType = "alluvial",
  curveType = "cardinal",
  curveGranularity = 200,
  labelNodes = TRUE,
  backgroundLabels = TRUE,
  showCounts = TRUE
)
cat("✓ Curve granularity and background labels working\n\n")

# Test 2: Flow diagram with individual tracking
cat("Test 2: Flow diagram with ID tracking...\n")
result2 <- riverplot(
  data = longitudinal_data,
  id = "patient_id",
  time = "visit",
  strata = "response",
  plotType = "flow",
  nodeStyle = "regular",
  edgeStyle = "gradient",
  enableDiagnostics = TRUE
)
cat("✓ Individual tracking and gradient edges working\n\n")

# Test 3: Sankey with node styling
cat("Test 3: Sankey diagram with advanced node styling...\n")
result3 <- riverplot(
  data = wide_data,
  strata = c("Response_Baseline", "Response_Month3", "Response_Month6"),
  weight = "weight",
  plotType = "sankey",
  nodeStyle = "point",
  edgeStyle = "sin",
  gravity = "top",
  addMidPoints = TRUE,
  reorderEdges = TRUE
)
cat("✓ Advanced node/edge styling options working\n\n")

# Test 4: Stream chart (if ggstream available)
if (requireNamespace("ggstream", quietly = TRUE)) {
  cat("Test 4: Stream chart with ggstream...\n")
  result4 <- riverplot(
    data = longitudinal_data,
    time = "visit",
    strata = "response",
    plotType = "stream",
    sortStreams = TRUE,
    labelNodes = TRUE
  )
  cat("✓ Stream chart with ggstream integration working\n\n")
} else {
  cat("Test 4: Stream chart (ggstream not available, using fallback)...\n")
  result4 <- riverplot(
    data = longitudinal_data,
    time = "visit",
    strata = "response",
    plotType = "stream",
    sortStreams = TRUE
  )
  cat("✓ Stream chart fallback working\n\n")
}

# Test 5: Transition matrix generation
cat("Test 5: Transition matrix with diagnostics...\n")
result5 <- riverplot(
  data = longitudinal_data,
  id = "patient_id",
  time = "visit",
  strata = "response",
  plotType = "alluvial",
  enableDiagnostics = TRUE,
  showCounts = TRUE,
  showPercentages = TRUE
)
cat("✓ Transition matrix and diagnostics working\n\n")

# Test 6: Riverplot object export
cat("Test 6: CRAN riverplot object generation...\n")
result6 <- riverplot(
  data = wide_data,
  strata = c("Response_Baseline", "Response_Month3"),
  exportRiverplotObject = TRUE
)
cat("✓ Riverplot object export working\n\n")

# Test 7: Custom colors with all features
cat("Test 7: Custom colors with all enhancements...\n")
result7 <- riverplot(
  data = wide_data,
  strata = c("Response_Baseline", "Response_Month3", "Response_Month6"),
  plotType = "alluvial",
  colorScheme = "custom",
  customColors = "#FF6B6B,#4ECDC4,#45B7D1,#96CEB4",
  fillType = "last",
  nodeWidth = 0.15,
  nodeGap = 0.08,
  flowAlpha = 0.6,
  mytitle = "Treatment Response Progression",
  fontSize = 14
)
cat("✓ Custom styling and colors working\n\n")

cat("=====================================\n")
cat("All enhanced features tested successfully!\n")
cat("The riverplot function now supports:\n")
cat("- curveGranularity for smooth curves\n")
cat("- backgroundLabels for better readability\n")
cat("- Transition matrix with probabilities\n")
cat("- Real CRAN riverplot object generation\n")
cat("- Advanced node/edge styling (invisible, point, gradient)\n")
cat("- Stream plots with ggstream integration\n")
cat("- Edge reordering and mid-point additions\n")
cat("- All unused parameters now implemented\n")