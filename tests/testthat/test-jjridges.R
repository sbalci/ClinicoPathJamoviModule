
library(testthat)
library(jmvcore)
library(ClinicoPathJamoviModule) # Replace with actual package name if different
library(ggplot2)
library(dplyr)
library(ggridges)

# Mock a basic jjridges class for testing internal functions
# This is needed because some internal functions rely on self$options and self$data
# For comprehensive testing, ideally, one would instantiate the actual jmv object
# but for unit testing internal R6 methods, this mock approach is often simpler.

# Helper function to create a mock self object
create_mock_self <- function(data, options = list()) {
  mock_self <- list(
    data = data,
    options = options,
    results = list(
      instructions = jmvcore::Output$new(options=NULL, name="instructions"),
      plot = jmvcore::Output$new(options=NULL, name="plot"),
      statistics = jmvcore::Output$new(options=NULL, name="statistics"),
      tests = jmvcore::Output$new(options=NULL, name="tests"),
      interpretation = jmvcore::Output$new(options=NULL, name="interpretation"),
      clinicalSummary = jmvcore::Output$new(options=NULL, name="clinicalSummary"),
      warnings = jmvcore::Output$new(options=NULL, name="warnings", type='html')
    )
  )
  
  # Ensure results items have the necessary methods
  for (res_name in names(mock_self$results)) {
    res <- mock_self$results[[res_name]]
    if (is.null(res$setContent)) res$setContent <- function(...) {}
    if (is.null(res$setVisible)) res$setVisible <- function(...) {}
    if (is.null(res$setState)) res$setState <- function(...) {}
    if (is.null(res$addRow)) res$addRow <- function(...) {}
    if (is.null(res$clear)) res$clear <- function(...) {} # Add clear method
  }
  
  # Mock private methods as well, or ensure they can be called
  mock_self$private <- list(
    .option = function(opt_name) {
      if (opt_name %in% names(mock_self$options)) {
        return(mock_self$options[[opt_name]])
      }
      return(NULL)
    },
    .MIN_SAMPLE_SIZE = 10,
    .MIN_GROUP_SIZE = 3,
    .MAX_OUTLIER_PROP = 0.05,
    .VIRIDIS_FALLBACK = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
    .CLINICAL_CB_SAFE_COLORS = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
    
    # Mock checkpoint (needed by jamovi modules)
    .checkpoint = function(...) {},
    
    # Placeholder for methods that need to be called
    .applyClinicalPreset = function() {},
    .validateInputs = function() {},
    .validateData = function(...) {},
    .prepareData = function() {
      x_var <- mock_self$options$x_var
      y_var <- mock_self$options$y_var
      plot_data <- data.frame(
        x = jmvcore::toNumeric(mock_self$data[[x_var]]),
        y = as.factor(mock_self$data[[y_var]]),
        stringsAsFactors = FALSE
      )
      if (!is.null(mock_self$options$fill_var)) {
        plot_data$fill <- as.factor(mock_self$data[[mock_self$options$fill_var]])
      }
      if (!is.null(mock_self$options$facet_var)) {
        plot_data$facet <- as.factor(mock_self$data[[mock_self$options$facet_var]])
      }
      na.omit(plot_data)
    },
    .generateStatistics = function(...) {},
    .generateTests = function(...) {},
    .createPlot = function(...) { ggplot2::ggplot() }, # Mock for now
    .generateInterpretation = function(...) {},
    .generateClinicalSummary = function(...) {"<p>Clinical Summary</p>"},
    .validateQuantiles = function(q_str) { as.numeric(strsplit(q_str, ",")[[1]]) } # Simplified
  )
  
  # Add other private methods from jjridges.b.R to mock_self$private
  # This makes it easier to test specific parts of the R6 class logic
  # For example, to test .validateInputs or .validateData directly:
  
  # Bind methods from the actual jjridgesClass definition (from jjridges.b.R)
  # This requires the source file to be loaded.
  # For now, we manually mock the critical ones.
  
  mock_self$private$.validateInputs <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.validateInputs
  mock_self$private$.validateData <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.validateData
  mock_self$private$.generateStatistics <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.generateStatistics
  mock_self$private$.performSingleTest <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.performSingleTest
  mock_self$private$.calculateEffectSizeWithCI <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.calculateEffectSizeWithCI
  mock_self$private$.adjustPValues <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.adjustPValues
  mock_self$private$.addTestRow <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.addTestRow
  mock_self$private$.generateTests <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.generateTests
  mock_self$private$.createDensityPlot <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.createDensityPlot
  mock_self$private$.createGradientPlot <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.createGradientPlot
  mock_self$private$.createHistogramPlot <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.createHistogramPlot
  mock_self$private$.createViolinPlot <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.createViolinPlot
  mock_self$private$.createPlot <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.createPlot
  mock_self$private$.applyColorPalette <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.applyColorPalette
  mock_self$private$.applyTheme <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.applyTheme
  mock_self$private$.applyLabels <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.applyLabels
  mock_self$private$.calculateBandwidth <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.calculateBandwidth
  mock_self$private$.calculateCliffsDelta <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.calculateCliffsDelta
  mock_self$private$.calculateHodgesLehmann <- ClinicoPathJamoviModule:::jjridgesClass$private_methods$.calculateHodgesLehmann

  # Need to explicitly set the environment for private methods so they can access 'self'
  # and other private methods correctly.
  for (method_name in names(mock_self$private)) {
    if (is.function(mock_self$private[[method_name]])) {
      environment(mock_self$private[[method_name]]) <- list2env(list(self = mock_self, private = mock_self$private), parent = baseenv())
    }
  }

  class(mock_self) <- c("jjridgesClass", class(mock_self))
  return(mock_self)
}


test_that("jjridgesClass initializes correctly", {
  data <- iris
  options <- list(x_var = "Sepal.Length", y_var = "Species")
  self_obj <- create_mock_self(data, options)
  
  # Ensure basic components are present
  expect_true("data" %in% names(self_obj))
  expect_true("options" %in% names(self_obj))
  expect_true("results" %in% names(self_obj))
  
  # Test instruction visibility on initial load
  # self_obj$private$.init() # init usually called by jamovi itself
  # expect_true(self_obj$results$instructions$visible) # Cannot test visible directly from mock
})

test_that("data preparation (.prepareData) works correctly", {
  data <- iris
  options <- list(x_var = "Sepal.Length", y_var = "Species")
  self_obj <- create_mock_self(data, options)
  
  plot_data <- self_obj$private$.prepareData()
  
  expect_s3_class(plot_data, "data.frame")
  expect_equal(names(plot_data), c("x", "y"))
  expect_equal(nrow(plot_data), nrow(iris))
  expect_true(is.numeric(plot_data$x))
  expect_true(is.factor(plot_data$y))
  
  # Test with fill_var and facet_var
  data_facet <- data.frame(
    x = 1:10, y = factor(rep(c("A", "B"), 5)),
    fill = factor(rep(c("F1", "F2"), each = 5)),
    facet = factor(rep(c("G1", "G2"), 5))
  )
  options_facet <- list(x_var = "x", y_var = "y", fill_var = "fill", facet_var = "facet")
  self_obj_facet <- create_mock_self(data_facet, options_facet)
  plot_data_facet <- self_obj_facet$private$.prepareData()
  
  expect_equal(names(plot_data_facet), c("x", "y", "fill", "facet"))
  expect_true(is.factor(plot_data_facet$fill))
  expect_true(is.factor(plot_data_facet$facet))
})

test_that("input validation (.validateInputs) works correctly", {
  data <- iris
  
  # No x_var selected
  options_no_x <- list(y_var = "Species")
  self_obj_no_x <- create_mock_self(data, options_no_x)
  expect_error(self_obj_no_x$private$.validateInputs(), "Please select a continuous variable for X")
  
  # No y_var selected
  options_no_y <- list(x_var = "Sepal.Length")
  self_obj_no_y <- create_mock_self(data, options_no_y)
  expect_error(self_obj_no_y$private$.validateInputs(), "Please select a grouping variable for Y")
  
  # Empty data
  options_empty <- list(x_var = "Sepal.Length", y_var = "Species")
  self_obj_empty <- create_mock_self(data[0,], options_empty)
  expect_error(self_obj_empty$private$.validateInputs(), "No data available for analysis")
  
  # Small sample size (should produce warning)
  small_data <- data[1:5,] # 5 rows < 10
  options_small <- list(x_var = "Sepal.Length", y_var = "Species")
  self_obj_small <- create_mock_self(small_data, options_small)
  warnings_small <- self_obj_small$private$.validateInputs()
  expect_true(any(grepl("Sample size .* below recommended minimum", warnings_small)))
  
  # Valid inputs
  options_valid <- list(x_var = "Sepal.Length", y_var = "Species")
  self_obj_valid <- create_mock_self(data, options_valid)
  warnings_valid <- self_obj_valid$private$.validateInputs()
  expect_length(warnings_valid, 0)
})

test_that("data validation (.validateData) works correctly", {
  # Mock plot_data directly for .validateData
  
  # Less than 2 groups
  plot_data_single_group <- data.frame(x = 1:10, y = factor(rep("A", 10)))
  options_single_group <- list(x_var = "x", y_var = "y")
  self_obj_single_group <- create_mock_self(plot_data_single_group, options_single_group)
  expect_error(self_obj_single_group$private$.validateData(plot_data_single_group), "At least 2 groups required")
  
  # Small group size warning
  plot_data_small_group <- data.frame(x = c(1:10, 11, 12), y = factor(c(rep("A", 10), rep("B", 2))))
  options_small_group <- list(x_var = "x", y_var = "y")
  self_obj_small_group <- create_mock_self(plot_data_small_group, options_small_group)
  warnings_small_group <- self_obj_small_group$private$.validateData(plot_data_small_group)
  expect_true(any(grepl("Groups with fewer than .* observations", warnings_small_group)))
  
  # High proportion of outliers warning
  plot_data_outliers <- data.frame(x = c(rnorm(90), rnorm(10, mean = 10, sd = 1)), y = factor(rep("A", 100)))
  # Need at least two groups for .validateData to not error first
  plot_data_outliers$y[1] <- "B" 
  options_outliers <- list(x_var = "x", y_var = "y")
  self_obj_outliers <- create_mock_self(plot_data_outliers, options_outliers)
  warnings_outliers <- self_obj_outliers$private$.validateData(plot_data_outliers)
  expect_true(any(grepl("High proportion of extreme outliers detected", warnings_outliers)))
  
  # Highly skewed data warning
  plot_data_skewed <- data.frame(x = rgamma(100, shape = 1, scale = 2), y = factor(rep("A", 100)))
  # Need at least two groups for .validateData to not error first
  plot_data_skewed$y[1] <- "B" 
  options_skewed <- list(x_var = "x", y_var = "y")
  self_obj_skewed <- create_mock_self(plot_data_skewed, options_skewed)
  warnings_skewed <- self_obj_skewed$private$.validateData(plot_data_skewed)
  expect_true(any(grepl("Data shows high skewness", warnings_skewed)))
})

test_that("statistical summary (.generateStatistics) works correctly", {
  data <- iris
  options <- list(x_var = "Sepal.Length", y_var = "Species")
  self_obj <- create_mock_self(data, options)
  
  # Mock the results$statistics table
  mock_stats_table <- list(
    rows = list(),
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <<- values
    },
    clear = function() { self$rows <<- list() }
  )
  self_obj$results$statistics <- mock_stats_table
  environment(self_obj$results$statistics$addRow) <- environment() # Bind to parent env
  environment(self_obj$results$statistics$clear) <- environment() # Bind to parent env

  plot_data <- self_obj$private$.prepareData()
  self_obj$private$.generateStatistics(plot_data)
  
  expect_true(length(mock_stats_table$rows) > 0)
  expect_equal(mock_stats_table$rows[["1"]]$group, "setosa")
  expect_equal(round(mock_stats_table$rows[["1"]]$mean, 2), 5.01)
  expect_equal(mock_stats_table$rows[["1"]]$n, 50)
})

# Add tests for effect size calculations
test_that("effect size calculations (.calculateEffectSizeWithCI) work correctly", {
  data1 <- rnorm(30, mean = 0, sd = 1)
  data2 <- rnorm(30, mean = 1, sd = 1)
  
  # Cohen's d
  self_obj_d <- create_mock_self(data.frame(), list(effsize_type = "d"))
  result_d <- self_obj_d$private$.calculateEffectSizeWithCI(data1, data2)
  expect_true(is.numeric(result_d$effect_size))
  expect_true(is.numeric(result_d$ci_lower))
  
  # Hedge's g
  self_obj_g <- create_mock_self(data.frame(), list(effsize_type = "g"))
  result_g <- self_obj_g$private$.calculateEffectSizeWithCI(data1, data2)
  expect_true(is.numeric(result_g$effect_size))
  
  # Cliff's Delta
  self_obj_cliff <- create_mock_self(data.frame(), list(effsize_type = "cliff_delta"))
  result_cliff <- self_obj_cliff$private$.calculateEffectSizeWithCI(data1, data2)
  expect_true(is.numeric(result_cliff$effect_size))
  # CI for Cliff's Delta requires 'boot' package, check if it's available or result is NA
  if (requireNamespace("boot", quietly = TRUE)) {
      expect_true(is.numeric(result_cliff$ci_lower))
  } else {
      expect_true(is.na(result_cliff$ci_lower))
  }

  # Hodges-Lehmann
  self_obj_hodges <- create_mock_self(data.frame(), list(effsize_type = "hodges_lehmann"))
  result_hodges <- self_obj_hodges$private$.calculateEffectSizeWithCI(data1, data2)
  expect_true(is.numeric(result_hodges$effect_size))
  expect_true(is.numeric(result_hodges$ci_lower))
})

# Add tests for p-value adjustment
test_that("p-value adjustment (.adjustPValues) works correctly", {
  p_values <- c(0.01, 0.05, 0.1, 0.001)
  
  # No adjustment
  self_obj_none <- create_mock_self(data.frame(), list(p_adjust_method = "none"))
  adjusted_none <- self_obj_none$private$.adjustPValues(p_values)
  expect_equal(adjusted_none, p_values)
  
  # Bonferroni
  self_obj_bonf <- create_mock_self(data.frame(), list(p_adjust_method = "bonferroni"))
  adjusted_bonf <- self_obj_bonf$private$.adjustPValues(p_values)
  expect_equal(adjusted_bonf, p.adjust(p_values, method = "bonferroni"))
  
  # Handle NAs
  p_values_na <- c(0.01, NA, 0.05)
  adjusted_na <- self_obj_bonf$private$.adjustPValues(p_values_na)
  expect_true(is.na(adjusted_na[2]))
  expect_equal(adjusted_na[c(1,3)], p.adjust(c(0.01, 0.05), method = "bonferroni"))
})

# Add tests for statistical tests (.performSingleTest)
test_that("statistical tests (.performSingleTest) work correctly", {
  data1 <- rnorm(30, mean = 0, sd = 1)
  data2 <- rnorm(30, mean = 1, sd = 1)
  
  # Parametric (t-test)
  self_obj_parametric <- create_mock_self(data.frame(), list(test_type = "parametric", effsize_type = "d"))
  result_parametric <- self_obj_parametric$private$.performSingleTest(data1, data2, "G1", "G2")
  expect_true(is.numeric(result_parametric$p_value))
  expect_equal(result_parametric$test_method, "t-test")
  
  # Non-parametric (Wilcoxon)
  self_obj_nonparametric <- create_mock_self(data.frame(), list(test_type = "nonparametric", effsize_type = "cliff_delta"))
  result_nonparametric <- self_obj_nonparametric$private$.performSingleTest(data1, data2, "G1", "G2")
  expect_true(is.numeric(result_nonparametric$p_value))
  expect_equal(result_nonparametric$test_method, "Wilcoxon")
  
  # Robust (Yuen's t-test or fallback)
  self_obj_robust <- create_mock_self(data.frame(), list(test_type = "robust", effsize_type = "d"))
  result_robust <- self_obj_robust$private$.performSingleTest(data1, data2, "G1", "G2")
  expect_true(is.numeric(result_robust$p_value))
  if (requireNamespace("WRS2", quietly = TRUE)) {
    expect_equal(result_robust$test_method, "Yuen (robust)")
  } else {
    expect_equal(result_robust$test_method, "t-test (WRS2 unavailable)")
    expect_true(grepl("WRS2 package not available", result_robust$warning))
  }
  
  # Bayesian (or fallback)
  self_obj_bayes <- create_mock_self(data.frame(), list(test_type = "bayes", effsize_type = "d"))
  result_bayes <- self_obj_bayes$private$.performSingleTest(data1, data2, "G1", "G2")
  if (requireNamespace("BayesFactor", quietly = TRUE)) {
    expect_equal(result_bayes$test_method, "Bayesian t-test")
    expect_true(is.numeric(result_bayes$statistic)) # Bayes Factor
    expect_true(is.na(result_bayes$p_value)) # No p-value in Bayesian
  } else {
    expect_equal(result_bayes$test_method, "Bayesian (unavailable)")
    expect_true(grepl("BayesFactor package not available", result_bayes$warning))
  }
})

# Test the main .generateTests function with strata
test_that(".generateTests works with and without stratification", {
  # Data with fill and facet variables
  data_complex <- data.frame(
    x = c(rnorm(20, 0), rnorm(20, 1), rnorm(20, 2), rnorm(20, 3)),
    y = factor(rep(c("G1", "G2"), each = 40)),
    fill = factor(rep(c("F1", "F2"), each = 20, times = 2)),
    facet = factor(rep(c("S1", "S2"), each = 40))
  )
  
  # Test without stratification
  options_no_strata <- list(x_var = "x", y_var = "y", show_stats = TRUE, test_type = "parametric", p_adjust_method = "none", effsize_type = "d")
  self_obj_no_strata <- create_mock_self(data_complex, options_no_strata)
  
  mock_tests_table <- list(
    rows = list(),
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <<- values
    },
    clear = function() { self$rows <<- list() }
  )
  self_obj_no_strata$results$tests <- mock_tests_table
  environment(self_obj_no_strata$results$tests$addRow) <- environment()
  environment(self_obj_no_strata$results$tests$clear) <- environment()
  
  plot_data_no_strata <- self_obj_no_strata$private$.prepareData()
  self_obj_no_strata$private$.generateTests(plot_data_no_strata)
  expect_true(length(mock_tests_table$rows) > 0)
  expect_equal(mock_tests_table$rows[["1"]]$comparison, "G1 vs G2")
  
  # Test with stratification (facet)
  options_facet_strata <- list(x_var = "x", y_var = "y", facet_var = "facet", show_stats = TRUE, test_type = "parametric", p_adjust_method = "none", effsize_type = "d")
  self_obj_facet_strata <- create_mock_self(data_complex, options_facet_strata)
  
  mock_tests_table_facet <- list(
    rows = list(),
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <<- values
    },
    clear = function() { self$rows <<- list() }
  )
  self_obj_facet_strata$results$tests <- mock_tests_table_facet
  environment(self_obj_facet_strata$results$tests$addRow) <- environment()
  environment(self_obj_facet_strata$results$tests$clear) <- environment()

  plot_data_facet_strata <- self_obj_facet_strata$private$.prepareData()
  self_obj_facet_strata$private$.generateTests(plot_data_facet_strata)
  expect_true(length(mock_tests_table_facet$rows) > 0)
  # Expecting two comparisons (G1 vs G2 within S1, G1 vs G2 within S2)
  expect_equal(length(mock_tests_table_facet$rows), 2)
  expect_true(grepl("G1 vs G2 (facet=S1)", mock_tests_table_facet$rows[["1"]]$comparison, fixed=TRUE))
  expect_true(grepl("G1 vs G2 (facet=S2)", mock_tests_table_facet$rows[["2"]]$comparison, fixed=TRUE))
  
  # Test with stratification (fill and facet)
  options_all_strata <- list(x_var = "x", y_var = "y", fill_var = "fill", facet_var = "facet", show_stats = TRUE, test_type = "parametric", p_adjust_method = "none", effsize_type = "d")
  self_obj_all_strata <- create_mock_self(data_complex, options_all_strata)
  
  mock_tests_table_all <- list(
    rows = list(),
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <<- values
    },
    clear = function() { self$rows <<- list() }
  )
  self_obj_all_strata$results$tests <- mock_tests_table_all
  environment(self_obj_all_strata$results$tests$addRow) <- environment()
  environment(self_obj_all_strata$results$tests$clear) <- environment()

  plot_data_all_strata <- self_obj_all_strata$private$.prepareData()
  self_obj_all_strata$private$.generateTests(plot_data_all_strata)
  expect_true(length(mock_tests_table_all$rows) > 0)
  # Expecting four comparisons (G1 vs G2 within F1/S1, G1 vs G2 within F2/S1, etc.)
  # Wait, the way generateTests is implemented, it generates comparisons for y groups
  # within each combination of facet and fill.
  # data_complex has 2 y levels, 2 fill levels, 2 facet levels.
  # G1/F1/S1, G1/F2/S1, G2/F1/S1, G2/F2/S1, etc.
  # The combinations are created from distinct(select(strata_vars))
  # So, unique combinations of fill and facet will be (F1,S1), (F2,S1), (F1,S2), (F2,S2)
  # And within each of those, G1 vs G2 is compared.
  # Resulting in 4 comparisons.
  expect_equal(length(mock_tests_table_all$rows), 4) 
  expect_true(grepl("G1 vs G2 (fill=F1 / facet=S1)", mock_tests_table_all$rows[["1"]]$comparison, fixed=TRUE))
  expect_true(grepl("G1 vs G2 (fill=F2 / facet=S1)", mock_tests_table_all$rows[["2"]]$comparison, fixed=TRUE))
  expect_true(grepl("G1 vs G2 (fill=F1 / facet=S2)", mock_tests_table_all$rows[["3"]]$comparison, fixed=TRUE))
  expect_true(grepl("G1 vs G2 (fill=F2 / facet=S2)", mock_tests_table_all$rows[["4"]]$comparison, fixed=TRUE))
})

# Test plot creation
test_that("plot creation (.createPlot) returns a ggplot object", {
  data <- iris
  options <- list(x_var = "Sepal.Length", y_var = "Species", plot_type = "density_ridges")
  self_obj <- create_mock_self(data, options)
  plot_data <- self_obj$private$.prepareData()
  
  plot_obj <- self_obj$private$.createPlot(plot_data)
  expect_s3_class(plot_obj, "ggplot")
  
  options_gradient <- list(x_var = "Sepal.Length", y_var = "Species", plot_type = "density_ridges_gradient", gradient_low = "blue", gradient_high = "red")
  self_obj_gradient <- create_mock_self(data, options_gradient)
  plot_obj_gradient <- self_obj_gradient$private$.createPlot(plot_data)
  expect_s3_class(plot_obj_gradient, "ggplot")
  
  options_histogram <- list(x_var = "Sepal.Length", y_var = "Species", plot_type = "histogram_ridges", binwidth = 0.5)
  self_obj_histogram <- create_mock_self(data, options_histogram)
  plot_obj_histogram <- self_obj_histogram$private$.createPlot(plot_data)
  expect_s3_class(plot_obj_histogram, "ggplot")
  
  options_violin <- list(x_var = "Sepal.Length", y_var = "Species", plot_type = "violin_ridges")
  self_obj_violin <- create_mock_self(data, options_violin)
  plot_obj_violin <- self_obj_violin$private$.createPlot(plot_data)
  expect_s3_class(plot_obj_violin, "ggplot")
})

# Test that applyClinicalPreset overrides options correctly
test_that(".applyClinicalPreset applies overrides correctly", {
  # Biomarker distribution preset
  options_biomarker <- list(clinicalPreset = "biomarker_distribution", plot_type = "ridgeline")
  self_obj_biomarker <- create_mock_self(iris, options_biomarker)
  self_obj_biomarker$private$.applyClinicalPreset()
  
  expect_equal(self_obj_biomarker$private$overrides$plot_type, "density_ridges")
  expect_true(self_obj_biomarker$private$overrides$add_boxplot)
  expect_equal(self_obj_biomarker$private$overrides$theme_style, "theme_pubr")
  
  # Treatment response preset
  options_treatment <- list(clinicalPreset = "treatment_response", show_stats = FALSE)
  self_obj_treatment <- create_mock_self(iris, options_treatment)
  self_obj_treatment$private$.applyClinicalPreset()
  
  expect_equal(self_obj_treatment$private$overrides$plot_type, "violin_ridges")
  expect_true(self_obj_treatment$private$overrides$show_stats)
  expect_equal(self_obj_treatment$private$overrides$effsize_type, "cliff_delta")
})

# Test the .option helper to ensure it respects overrides
test_that(".option helper respects overrides", {
  options_test <- list(plot_type = "ridgeline", scale = 1.0)
  self_obj_test <- create_mock_self(iris, options_test)
  
  # Simulate an override
  self_obj_test$private$overrides <- list(plot_type = "density_ridges_gradient")
  
  expect_equal(self_obj_test$private$.option("plot_type"), "density_ridges_gradient")
  expect_equal(self_obj_test$private$.option("scale"), 1.0) # Should get from self$options
  expect_null(self_obj_test$private$.option("non_existent_option"))
})
