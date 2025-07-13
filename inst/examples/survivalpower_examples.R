# Survival Power Analysis Examples
# Examples demonstrating survivalpower function capabilities

library(ClinicoPath)

# Example 1: Oncology Trial - Sample Size Calculation
# Scenario: Phase III trial comparing new cancer treatment vs standard care
# Primary endpoint: Overall survival
# Expected median survival: Control 12 months, Treatment 24 months
# Corresponding hazard rates: Control 0.083, Treatment 0.042 (per month)

oncology_sample_size <- survivalpower(
  data = data.frame(),
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,      # ~12 month median survival
  hazard_treatment = 0.042,    # ~24 month median survival  
  study_duration = 36,         # 3 year total study
  accrual_duration = 24,       # 2 year accrual period
  alpha = 0.025,              # One-sided 2.5%
  beta = 0.1,                 # 90% power
  allocation_ratio = 1,        # 1:1 randomization
  show_summary = TRUE,
  show_interpretation = TRUE
)

# Example 2: Cardiology Trial - Events-Based Calculation
# Scenario: Cardiovascular outcomes trial
# Target 25% risk reduction (HR = 0.75)
# Quick calculation using Schoenfeld method

cardio_events <- survivalpower(
  data = data.frame(),
  calculation_type = "events",
  method = "schoenfeld",
  hazard_ratio = 0.75,        # 25% risk reduction
  alpha = 0.025,              # One-sided 2.5%
  beta = 0.2,                 # 80% power
  allocation_ratio = 1,        # 1:1 randomization
  sided = "1",                # One-sided test
  show_summary = TRUE,
  show_interpretation = TRUE
)

# Example 3: Neurology Trial - Power Analysis
# Scenario: Existing trial with 300 patients, what power do we have?
# Using Schoenfeld method for quick assessment

neurology_power <- survivalpower(
  data = data.frame(),
  calculation_type = "power",
  method = "schoenfeld",
  hazard_ratio = 0.6,         # Expected 40% risk reduction
  events_input = 150,         # Expected 150 events
  alpha = 0.025,              # One-sided 2.5%
  allocation_ratio = 1,        # 1:1 randomization
  sided = "1",
  show_summary = TRUE,
  show_interpretation = TRUE
)

# Example 4: Rare Disease Trial - Hazard Ratio Detection
# Scenario: Small rare disease study, what effect size can we detect?
# Limited to 100 patients due to rarity

rare_disease_hr <- survivalpower(
  data = data.frame(),
  calculation_type = "hazard_ratio",
  method = "schoenfeld",
  events_input = 60,          # Expected 60 events from 100 patients
  alpha = 0.05,               # Two-sided 5% (regulatory preference)
  beta = 0.2,                 # 80% power
  allocation_ratio = 1,        # 1:1 randomization
  sided = "2",                # Two-sided test
  show_summary = TRUE,
  show_interpretation = TRUE
)

# Example 5: Complex Study Design - Non-uniform Accrual
# Scenario: Multicenter trial with exponential accrual pattern
# Slow start-up, accelerating enrollment

complex_design <- survivalpower(
  data = data.frame(),
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.05,      # Low event rate condition
  hazard_treatment = 0.03,    # 40% risk reduction
  study_duration = 48,        # 4 year study
  accrual_duration = 30,      # 2.5 year accrual
  entry_type = "expo",        # Exponential entry
  gamma = 1,                  # Accelerating accrual
  dropout_rate = 0.02,        # 2% annual dropout
  alpha = 0.025,
  beta = 0.1,
  allocation_ratio = 2,       # 2:1 randomization (more to treatment)
  show_summary = TRUE,
  show_interpretation = TRUE,
  show_formulas = TRUE
)

# Example 6: Power Curve Analysis
# Scenario: Explore power across different sample sizes
# Useful for protocol planning and budget discussions

power_curve_analysis <- survivalpower(
  data = data.frame(),
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  study_duration = 36,
  accrual_duration = 24,
  alpha = 0.025,
  beta = 0.1,
  show_power_plot = TRUE,     # Show power curve
  show_timeline_plot = TRUE,  # Show study timeline
  power_plot_range = "100,800", # Custom range for power plot
  show_summary = TRUE,
  show_interpretation = TRUE
)

# Example 7: Export for External Analysis
# Scenario: Generate data for regulatory submission documents

export_example <- survivalpower(
  data = data.frame(),
  calculation_type = "sample_size",
  method = "lachin_foulkes",
  hazard_control = 0.083,
  hazard_treatment = 0.042,
  study_duration = 36,
  accrual_duration = 24,
  alpha = 0.025,
  beta = 0.1,
  export_results = TRUE,      # Export main results
  export_power_curve = TRUE,  # Export power curve data
  power_plot_range = "200,600",
  show_summary = TRUE
)

# Print example scenarios summary
cat("Survival Power Analysis Examples:\n")
cat("================================\n")
cat("1. Oncology Trial - Sample size for Phase III trial\n")
cat("2. Cardiology Trial - Events calculation for CV outcomes\n") 
cat("3. Neurology Trial - Power analysis for existing study\n")
cat("4. Rare Disease Trial - Detectable effect size\n")
cat("5. Complex Design - Non-uniform accrual patterns\n")
cat("6. Power Curve Analysis - Visualization and planning\n")
cat("7. Export Example - Data export for external analysis\n")
cat("\nUse these examples as templates for your own power analyses.\n")