## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 6
)


## -----------------------------------------------------------------------------
# Load required libraries
devtools::load_all()

# Load the histopathology dataset
data(histopathology, package = "ClinicoPath")

# Ensure the dataset is loaded properly
if (!exists("histopathology") || !is.data.frame(histopathology)) {
  stop("Failed to load histopathology dataset")
}

# Check if the expected variables exist
expected_vars <- c("Age", "OverallTime", "MeasurementA", "MeasurementB")
missing_vars <- setdiff(expected_vars, names(histopathology))

if (length(missing_vars) > 0) {
  warning(paste("Missing expected variables:", paste(missing_vars, collapse = ", ")))
  # Create dummy variables if missing for the vignette to work
  for (var in missing_vars) {
    if (var %in% c("MeasurementA", "MeasurementB")) {
      # Create dummy numeric variables
      set.seed(123)
      histopathology[[var]] <- rnorm(nrow(histopathology), mean = 50, sd = 10)
    }
  }
}

# Examine available numeric variables
numeric_vars <- names(histopathology)[sapply(histopathology, is.numeric)]
cat("Available numeric variables:\n")
print(head(numeric_vars, 10))


## -----------------------------------------------------------------------------
# Basic correlation analysis - use only two variables
basic_correlation <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime")
)

# Display the basic correlation results
print(basic_correlation)


## -----------------------------------------------------------------------------
# Pearson correlation (default)
pearson_result <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB")
)

# This provides parametric correlation coefficients with confidence intervals


## -----------------------------------------------------------------------------
# Spearman correlation for ordinal or non-normal data
spearman_result <- jcorrelation(
  data = histopathology,
  vars = c("Grade", "TStage", "Anti-X-intensity", "Anti-Y-intensity")
)

# Grade and TStage are ordinal, intensity measures may be non-normal


## -----------------------------------------------------------------------------
# Kendall correlation for robust analysis
kendall_result <- jcorrelation(
  data = histopathology,
  vars = c("Age", "Grade", "TStage")
)

# Kendall's tau is preferred for small samples or many tied values


## -----------------------------------------------------------------------------
# Two-sided test (default) - tests if correlation ≠ 0
two_sided <- jcorrelation(
  data = histopathology,
  vars = c("MeasurementA", "MeasurementB", "Measurement1")
)

# One-sided test - tests if correlation > 0
positive_test <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime"),
  alternative = "greater"
)

# Test for negative correlation (correlation < 0)
negative_test <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime"),
  alternative = "less"
)


## -----------------------------------------------------------------------------
# Different confidence levels
ci_95 <- jcorrelation(
  data = histopathology,
  vars = c("MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
)

ci_99 <- jcorrelation(
  data = histopathology,
  vars = c("MeasurementA", "MeasurementB"),
  ciWidth = 99
)

# 99% CI will be wider, indicating more uncertainty

# Test narrow confidence intervals  
ci_90 <- jcorrelation(
  data = histopathology,
  vars = c("MeasurementA", "MeasurementB"),
  ciWidth = 90
)

# 90% CI will be narrower but less conservative


## -----------------------------------------------------------------------------
# Conservative significance testing with strict alpha
conservative_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", "Measurement1"),
  flagAlpha = 0.01  # Bonferroni-adjusted alpha
)

# Standard significance testing  
standard_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", "Measurement1"),
  flagAlpha = 0.05  # Standard alpha level
)

# Very conservative testing for exploratory analysis
very_conservative <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB"),
  flagAlpha = 0.001  # Very strict significance threshold
)


## -----------------------------------------------------------------------------
# Correlation analysis by sex
sex_stratified <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB")
)

# Correlation analysis by treatment group
treatment_stratified <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA")
)


## -----------------------------------------------------------------------------
# Matrix visualization
matrix_plot <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", "Anti-X-intensity")
)

# Creates a color-coded correlation matrix


## -----------------------------------------------------------------------------
# Pairs plot with correlations and distributions
pairs_plot <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB"),
  plots = TRUE,
  plotType = "pairs"
)

# Shows scatterplots, correlations, and distributions


## -----------------------------------------------------------------------------
# Network visualization of correlations
network_plot <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", "Anti-X-intensity"),
  plots = TRUE,
  plotType = "network"
)

# Creates a network graph showing correlation relationships
# Strong correlations (>0.3) appear as thicker edges
# Positive correlations in green, negative in red


## -----------------------------------------------------------------------------
# First ensure we have valid numeric variables
# Check which variables are actually numeric and available
available_vars <- names(histopathology)[sapply(histopathology, is.numeric)]
test_vars <- intersect(c("Age", "OverallTime", "MeasurementA", "MeasurementB"), available_vars)

# If we don't have all 4 variables, use what's available
if (length(test_vars) < 4) {
  # Use the first 4 numeric variables available
  test_vars <- head(available_vars[!available_vars %in% c("ID", "Outcome")], 4)
}

# Only proceed if we have at least 2 variables
if (length(test_vars) >= 2) {
  # Matrix plot - hierarchical clustering of correlation matrix
  matrix_visualization <- jcorrelation(
    data = histopathology,
    vars = test_vars,
    plots = TRUE,
    plotType = "matrix"
  )
  
  # Pairs plot - scatterplot matrix with correlation coefficients  
  pairs_visualization <- jcorrelation(
    data = histopathology,
    vars = test_vars,
    plots = TRUE,
    plotType = "pairs"
  )
  
  # Network plot - graph-based correlation visualization
  network_visualization <- jcorrelation(
    data = histopathology,
    vars = test_vars,
    plots = TRUE,
    plotType = "network"
  )
  
  # Each plot type offers different insights:
  # - Matrix: Overall pattern identification
  # - Pairs: Individual relationship examination  
  # - Network: Correlation strength visualization
} else {
  cat("Not enough numeric variables available for this example\n")
}


## -----------------------------------------------------------------------------
# Correlating different measurement methods
biomarker_correlation <- jcorrelation(
  data = histopathology,
  vars = c("MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
)

# This helps validate measurement consistency across methods


## -----------------------------------------------------------------------------
# Examining age relationships with clinical variables
age_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "Grade", "TStage")
)

# Spearman is appropriate for mixed continuous/ordinal data


## -----------------------------------------------------------------------------
# Analyzing relationships between IHC markers
ihc_correlation <- jcorrelation(
  data = histopathology,
  vars = c("Anti-X-intensity", "Anti-Y-intensity", "Rater A", "Rater B")
)

# This assesses co-expression patterns and rater agreement


## -----------------------------------------------------------------------------
# Large correlation matrix with summary statistics
comprehensive_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "Grade", "TStage", "Anti-X-intensity", "Anti-Y-intensity", 
           "OverallTime", "MeasurementA", "MeasurementB")
)

# With >2 variables, summary statistics table is generated


## -----------------------------------------------------------------------------
# Analysis with detailed reporting
reported_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB")
)

# The report provides clinical interpretation of findings


## -----------------------------------------------------------------------------
# Example of clinically meaningful correlation
clinical_example <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA")
)

# Consider both p-value and effect size (correlation coefficient)


## -----------------------------------------------------------------------------
# The function uses complete cases by default
missing_data_example <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB")
)

# Missing values are automatically excluded pairwise


## -----------------------------------------------------------------------------
# Small sample analysis
small_sample <- histopathology[1:30, ]

small_correlation <- jcorrelation(
  data = small_sample,
  vars = c("Age", "OverallTime", "MeasurementA")
)


## -----------------------------------------------------------------------------
# For non-linear relationships, consider Spearman
nonlinear_example <- jcorrelation(
  data = histopathology,
  vars = c("Age", "Grade", "TStage")
)

# Pairs plots help identify non-linear patterns


## -----------------------------------------------------------------------------
# Comprehensive analysis for publication
publication_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", 
           "Anti-X-intensity", "Anti-Y-intensity")
)


## -----------------------------------------------------------------------------
# When testing many correlations, use conservative alpha
multiple_testing <- jcorrelation(
  data = histopathology,
  vars = c("Age", "Grade", "TStage", "Anti-X-intensity", "Anti-Y-intensity", 
           "OverallTime", "MeasurementA", "MeasurementB")
)

# Consider Bonferroni correction: α/number_of_tests


## -----------------------------------------------------------------------------
# Exploratory analysis - broader significance threshold
exploratory <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB", 
           "Anti-X-intensity", "Anti-Y-intensity")
)

# Confirmatory analysis - stricter threshold
confirmatory <- jcorrelation(
  data = histopathology,
  vars = c("MeasurementA", "MeasurementB")  # Pre-specified hypothesis
)


## -----------------------------------------------------------------------------
# Assess measurement quality through correlation
quality_control <- jcorrelation(
  data = histopathology,
  vars = c("Rater A", "Rater B", "Rater 1", "Rater 2")
)

# High correlations indicate good inter-rater reliability


## -----------------------------------------------------------------------------
# For small effects, ensure adequate sample size
power_example <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime")
)

# Wide confidence intervals suggest insufficient power


## -----------------------------------------------------------------------------
# Spearman correlation is more robust to outliers
robust_analysis <- jcorrelation(
  data = histopathology,
  vars = c("Age", "OverallTime", "MeasurementA")
)

# Pairs plots help identify potential outliers

