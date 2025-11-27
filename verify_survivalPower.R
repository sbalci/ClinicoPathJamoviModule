
library(R6)
library(testthat)
library(gsDesign)
library(survival)
library(ggplot2)
library(scales)

# Mock jmvcore classes
Option <- R6Class("Option",
  public = list(
    value = NULL,
    initialize = function(value = NULL) {
      self$value <- value
    }
  )
)

Table <- R6Class("Table",
  public = list(
    rows = list(),
    columns = list(),
    title = "",
    visible = TRUE,
    rowCount = 0,
    initialize = function(...) {
      self$rows <- list()
    },
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <- values
      self$rowCount <- length(self$rows)
    },
    deleteRows = function() {
      self$rows <- list()
      self$rowCount <- 0
    },
    setRow = function(rowNo, values) {
      self$rows[[as.character(rowNo)]] <- values
      self$rowCount <- length(self$rows) # Update rowCount
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    asDF = function() {
      if (length(self$rows) == 0) return(data.frame())
      df <- do.call(rbind, lapply(self$rows, function(x) {
        x <- lapply(x, function(val) if(is.null(val)) NA else val)
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
      return(df)
    }
  )
)

Html <- R6Class("Html",
  public = list(
    content = "",
    visible = TRUE,
    initialize = function(...) {},
    setContent = function(content) {
      self$content <- content
    },
    setVisible = function(visible) {
      self$visible <- visible
    }
  )
)

Image <- R6Class("Image",
  public = list(
    state = NULL,
    visible = TRUE,
    initialize = function(...) {},
    setState = function(state) {
      self$state <- state
    },
    setVisible = function(visible) {
      self$visible <- visible
    }
  )
)

# Mock Results class based on jamovi/survivalPower.r.yaml
Results <- R6Class("Results",
  public = list(
    instructions = NULL,
    power_summary = NULL,
    sample_size_results = NULL,
    power_results = NULL,
    effect_size_results = NULL,
    study_duration_results = NULL,
    assumptions_table = NULL,
    competing_risks_table = NULL,
    non_inferiority_table = NULL,
    rmst_analysis_table = NULL,
    snp_analysis_table = NULL,
    multi_arm_table = NULL,
    interim_analysis_table = NULL,
    sensitivity_analysis_table = NULL,
    regulatory_table = NULL,
    power_curve_plot = NULL,
    sample_size_plot = NULL,
    survival_curves_plot = NULL,
    accrual_timeline_plot = NULL,
    sensitivity_plot = NULL,
    clinical_interpretation = NULL,
    natural_language_summary = NULL,
    educational_explanations = NULL,
    statistical_glossary = NULL,
    guided_workflow = NULL,
    
    initialize = function() {
      self$instructions <- Html$new()
      self$power_summary <- Table$new()
      self$power_summary$addRow(1, list()) # Initialize with 1 row as per YAML
      self$sample_size_results <- Table$new()
      self$power_results <- Table$new()
      self$effect_size_results <- Table$new()
      self$study_duration_results <- Table$new()
      self$assumptions_table <- Table$new()
      self$competing_risks_table <- Table$new()
      self$non_inferiority_table <- Table$new()
      self$rmst_analysis_table <- Table$new()
      self$snp_analysis_table <- Table$new()
      self$multi_arm_table <- Table$new()
      self$interim_analysis_table <- Table$new()
      self$sensitivity_analysis_table <- Table$new()
      self$regulatory_table <- Table$new()
      self$power_curve_plot <- Image$new()
      self$sample_size_plot <- Image$new()
      self$survival_curves_plot <- Image$new()
      self$accrual_timeline_plot <- Image$new()
      self$sensitivity_plot <- Image$new()
      self$clinical_interpretation <- Html$new()
      self$natural_language_summary <- Html$new()
      self$educational_explanations <- Html$new()
      self$statistical_glossary <- Html$new()
      self$guided_workflow <- Html$new()
    }
  )
)

# Mock survivalPowerBase class
survivalPowerBase <- R6Class("survivalPowerBase",
  public = list(
    options = NULL,
    results = NULL,
    initialize = function(options = list(), results = NULL) {
      self$options <- options
      self$results <- if (is.null(results)) Results$new() else results
    },
    run = function() {
      private$.run()
    }
  )
)

# Read and evaluate the survivalPowerClass definition
source_file <- "R/survivalPower.b.R"
if (!file.exists(source_file)) {
  stop("Could not find R/survivalPower.b.R")
}

# Read the file content
file_content <- readLines(source_file)

# Extract the class definition
# We need to handle the fact that it inherits from survivalPowerBase which we just mocked
# and it uses private methods.
# The file content assigns to survivalPowerClass. We can just evaluate the code 
# after removing the imports and ensuring survivalPowerBase is available.

# Filter out roxygen comments and imports
code_lines <- file_content[!grepl("^#'", file_content)]
code_text <- paste(code_lines, collapse = "\n")

# Evaluate the code to define survivalPowerClass
eval(parse(text = code_text))

# Helper to create options list with defaults
create_options <- function(
    analysis_type = "sample_size",
    test_type = "log_rank",
    study_design = "two_arm_parallel",
    primary_endpoint = "overall_survival",
    effect_size_type = "hazard_ratio",
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.80,
    allocation_ratio = 1.0,
    sample_size_input = 200,
    control_median_survival = 12.0,
    survival_distribution = "exponential",
    weibull_shape = 1.0,
    accrual_period = 24.0,
    follow_up_period = 12.0,
    accrual_pattern = "uniform",
    dropout_rate = 0.05,
    ni_margin = 1.25,
    ni_type = "relative_margin",
    competing_risk_rate = 0.1,
    competing_risk_hr = 1.0,
    rmst_tau = 36.0,
    rmst_difference = 3.0,
    snp_maf = 0.3,
    genetic_model = "additive",
    number_of_arms = 3,
    multiple_comparisons = "none",
    interim_analyses = 0,
    alpha_spending = "none",
    stratification_factors = 0,
    cluster_size = 50,
    icc = 0.05,
    sensitivity_analysis = FALSE,
    simulation_runs = 10000,
    show_summary = FALSE,
    show_explanations = FALSE,
    show_glossary = FALSE,
    guided_mode = FALSE
) {
  list(
    clinical_preset = "custom",
    analysis_type = analysis_type,
    test_type = test_type,
    study_design = study_design,
    primary_endpoint = primary_endpoint,
    effect_size_type = effect_size_type,
    effect_size = effect_size,
    alpha_level = alpha_level,
    power_level = power_level,
    allocation_ratio = allocation_ratio,
    sample_size_input = sample_size_input,
    control_median_survival = control_median_survival,
    survival_distribution = survival_distribution,
    weibull_shape = weibull_shape,
    accrual_period = accrual_period,
    follow_up_period = follow_up_period,
    accrual_pattern = accrual_pattern,
    dropout_rate = dropout_rate,
    ni_margin = ni_margin,
    ni_type = ni_type,
    competing_risk_rate = competing_risk_rate,
    competing_risk_hr = competing_risk_hr,
    rmst_tau = rmst_tau,
    rmst_difference = rmst_difference,
    snp_maf = snp_maf,
    genetic_model = genetic_model,
    number_of_arms = number_of_arms,
    multiple_comparisons = multiple_comparisons,
    interim_analyses = interim_analyses,
    alpha_spending = alpha_spending,
    stratification_factors = stratification_factors,
    cluster_size = cluster_size,
    icc = icc,
    sensitivity_analysis = sensitivity_analysis,
    simulation_runs = simulation_runs,
    show_summary = show_summary,
    show_explanations = show_explanations,
    show_glossary = show_glossary,
    guided_mode = guided_mode
  )
}

# --- Tests ---

test_that("Log-rank Sample Size Calculation runs correctly", {
  options <- create_options(
    analysis_type = "sample_size",
    test_type = "log_rank",
    effect_size = 0.7,
    power_level = 0.8,
    alpha_level = 0.05,
    control_median_survival = 12,
    accrual_period = 24,
    follow_up_period = 12
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  # Check if sample size table is populated
  table <- analysis$results$sample_size_results
  expect_true(table$rowCount > 0)
  
  # Check power summary
  summary <- analysis$results$power_summary
  expect_true(summary$rowCount > 0)
  
  # Verify calculated value format (should contain "Total Sample Size")
  calc_val <- summary$rows[[1]]$calculated_value
  expect_true(grepl("Total Sample Size", calc_val))
})

test_that("Log-rank Power Calculation runs correctly", {
  options <- create_options(
    analysis_type = "power",
    test_type = "log_rank",
    effect_size = 0.7,
    sample_size_input = 300,
    alpha_level = 0.05,
    control_median_survival = 12,
    accrual_period = 24,
    follow_up_period = 12
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  # Check if power results table is populated
  table <- analysis$results$power_results
  expect_true(table$rowCount > 0)
  
  # Check power summary
  summary <- analysis$results$power_summary
  calc_val <- summary$rows[[1]]$calculated_value
  expect_true(grepl("Statistical Power", calc_val))
})

test_that("Cox Regression Power Calculation runs correctly", {
  options <- create_options(
    analysis_type = "power",
    test_type = "cox_regression",
    effect_size = 0.7,
    sample_size_input = 300,
    alpha_level = 0.05,
    control_median_survival = 12,
    accrual_period = 24,
    follow_up_period = 12
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  # Check if power results table is populated
  table <- analysis$results$power_results
  expect_true(table$rowCount > 0)
  
  # Check power summary
  summary <- analysis$results$power_summary
  calc_val <- summary$rows[[1]]$calculated_value
  expect_true(grepl("Statistical Power", calc_val))
})

test_that("Non-inferiority Analysis runs correctly", {
  options <- create_options(
    analysis_type = "sample_size",
    test_type = "non_inferiority",
    effect_size = 1.0, # Assumed HR (true)
    ni_margin = 1.3,
    power_level = 0.8,
    alpha_level = 0.025, # One-sided
    control_median_survival = 12,
    accrual_period = 24,
    follow_up_period = 12
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  # Check if non-inferiority table is populated
  table <- analysis$results$non_inferiority_table
  expect_true(table$rowCount > 0)
  
  # Check power summary
  summary <- analysis$results$power_summary
  calc_val <- summary$rows[[1]]$calculated_value
  expect_true(grepl("Total Sample Size", calc_val))
})

test_that("Input Validation works", {
  # Test invalid HR
  options <- create_options(
    effect_size = -0.5
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  # Should show error in instructions
  content <- analysis$results$instructions$content
  expect_true(grepl("Input Validation Error", content))
  expect_true(grepl("Hazard ratio must be between 0 and 5", content))
  
  # Test invalid Alpha
  options <- create_options(
    alpha_level = 1.5
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  content <- analysis$results$instructions$content
  expect_true(grepl("Alpha level must be between 0 and 1", content))
})

test_that("Study Duration Calculation runs correctly", {
  options <- create_options(
    analysis_type = "duration",
    test_type = "log_rank",
    effect_size = 0.7,
    power_level = 0.8,
    sample_size_input = 300,
    control_median_survival = 12,
    accrual_period = 24
  )
  
  analysis <- survivalPowerClass$new(options = options)
  analysis$run()
  
  # Check duration results table
  table <- analysis$results$study_duration_results
  expect_true(table$rowCount > 0)
  
  # Check power summary
  summary <- analysis$results$power_summary
  calc_val <- summary$rows[[1]]$calculated_value
  expect_true(grepl("Required Study Duration", calc_val))
})
