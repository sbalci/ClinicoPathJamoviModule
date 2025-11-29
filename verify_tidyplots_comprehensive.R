
# Verification Script for tidyplots
# This script generates several plots using tidyplots to verify functionality and aesthetics.

library(jmvcore)
library(tidyplots)
library(ggplot2)

# Source the files
source("R/tidyplots.h.R")
source("R/tidyplots.b.R")

# Define . function if not exists
if (!exists(".")) . <- function(x, ...) x

# Create test data
set.seed(123)
data <- data.frame(
  Group = rep(c("Control", "Treatment"), each = 50),
  Dose = rep(c("Low", "High"), each = 25, times = 2),
  Response = c(rnorm(50, 10, 2), rnorm(50, 15, 3)),
  Time = rep(1:50, 2),
  Category = sample(c("A", "B", "C"), 100, replace = TRUE)
)

# 1. Basic Boxplot with Statistics
print("Generating Boxplot...")
boxplot_res <- tidyplots(
  data = data,
  xvar = "Group",
  yvar = "Response",
  color = "Group",
  plotType = "boxplot",
  boxplotOutliers = TRUE,
  showMean = TRUE,
  meanType = "dot",
  showPValue = TRUE,
  plotTitle = "Treatment Response by Group",
  xLabel = "Study Group",
  yLabel = "Response Value",
  colorScheme = "friendly"
)
# Save plot (if possible, otherwise just print)
# The result is a jmvcore object. The plot is in boxplot_res$plot.
# But tidyplots returns a list of results.
# boxplot_res$plot is an Image object.
# We can't easily save it without running the analysis in jamovi engine.
# However, the .plot function returns a ggplot object if we call it directly?
# No, .plot returns TRUE/FALSE and prints the plot.
# But we can capture the print output?
# Or we can inspect the state.

# 2. Violin Plot with Points and Stats
print("Generating Violin Plot...")
violin_res <- tidyplots(
  data = data,
  xvar = "Dose",
  yvar = "Response",
  group = "Group",
  plotType = "violin",
  violinPoints = TRUE,
  showMedian = TRUE,
  medianType = "dash",
  showCI = TRUE,
  ciType = "errorbar",
  plotTitle = "Dose Response Distribution",
  colorScheme = "viridis"
)

# 3. Histogram (No Y variable)
print("Generating Histogram...")
hist_res <- tidyplots(
  data = data,
  xvar = "Response",
  plotType = "histogram",
  histogramBins = 20,
  showDistribution = TRUE,
  distributionType = "density",
  plotTitle = "Response Distribution"
)

print("Verification completed successfully.")
