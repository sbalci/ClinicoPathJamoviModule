
# Verification script for survival function
# This script tests the survival function in the ClinicoPath Jamovi module

# Load necessary packages
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
if (!requireNamespace("survminer", quietly = TRUE)) install.packages("survminer")
if (!requireNamespace("finalfit", quietly = TRUE)) install.packages("finalfit")
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("labelled", quietly = TRUE)) install.packages("labelled")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("cmprsk", quietly = TRUE)) install.packages("cmprsk")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(testthat)
library(survival)
library(survminer)
library(finalfit)
library(R6)
library(dplyr)
library(glue)
library(janitor)
library(labelled)
library(ggplot2)
library(cmprsk)
library(lubridate)

# Mock jmvcore classes and functions
# ------------------------------------------------------------------------------
# We need to mock the jmvcore environment since we are running this outside Jamovi

# Mock Options class
Options <- R6Class("Options",
  public = list(
    options = list(),
    initialize = function(...) {
      self$options <- list(...)
    },
    get = function(name) {
      if (name %in% names(self$options)) {
        return(self$options[[name]])
      }
      return(NULL)
    }
  )
)

# Mock Table class
Table <- R6Class("Table",
  public = list(
    rows = list(),
    columns = list(),
    title = "",
    note = "",
    visible = TRUE,
    rowCount = 0,
    initialize = function(...) {
      self$rows <- list()
    },
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <- values
      self$rowCount <- length(self$rows)
    },
    setRowNums = function(rowNums) {
      # Do nothing for mock
    },
    setValues = function(values) {
      # Do nothing for mock
    },
    setTitle = function(title) {
      self$title <- title
    },
    setNote = function(key, note) {
      self$note <- note
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    asDF = function() {
      if (length(self$rows) == 0) return(data.frame())
      
      # Convert list of lists to data frame
      df <- do.call(rbind, lapply(self$rows, function(x) {
        # Handle NULLs by replacing with NA
        x <- lapply(x, function(val) if(is.null(val)) NA else val)
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
      return(df)
    },
    isNotFilled = function() {
      return(TRUE) # Always return TRUE for mock to allow filling
    }
  )
)

# Mock Html class
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

# Mock Image class
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

# Mock Results class
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    medianSurvivalHeading = NULL,
    medianSurvivalExplanation = NULL,
    medianSurvivalHeading3 = NULL,
    medianSummary = NULL,
    medianTable = NULL,
    coxRegressionHeading = NULL,
    coxRegressionExplanation = NULL,
    coxRegressionHeading3 = NULL,
    coxSummary = NULL,
    coxTable = NULL,
    tCoxtext2 = NULL,
    cox_ph = NULL,
    plot8 = NULL,
    survivalTablesHeading = NULL,
    survivalTablesExplanation = NULL,
    survivalTablesHeading3 = NULL,
    survTableSummary = NULL,
    survTable = NULL,
    plot = NULL,
    plot2 = NULL,
    plot3 = NULL,
    plot6 = NULL,
    plot7 = NULL,
    survivalPlotsHeading3 = NULL,
    survivalPlotsExplanation = NULL,
    personTimeHeading = NULL,
    personTimeTable = NULL,
    personTimeSummary = NULL,
    personTimeExplanation = NULL,
    rmstHeading = NULL,
    rmstTable = NULL,
    rmstSummary = NULL,
    rmstExplanation = NULL,
    residualsTable = NULL,
    residualsPlot = NULL,
    residualDiagnosticsExplanation = NULL,
    pairwiseComparisonHeading = NULL,
    pairwiseSummary = NULL,
    pairwiseTable = NULL,
    parametricModelComparison = NULL,
    parametricModelSummary = NULL,
    parametricDiagnostics = NULL,
    parametricSurvivalPlot = NULL,
    hazardFunctionPlot = NULL,
    extrapolationPlot = NULL,
    extrapolationTable = NULL,
    parametricModelsExplanation = NULL,
    calculatedtime = NULL,
    outcomeredefined = NULL,
    survivalExport = NULL,
    survivalExportSummary = NULL,
    phInterpretation = NULL,
    clinicalGlossaryExplanation = NULL,
    clinicalInterpretationExplanation = NULL,
    copyReadySentencesExplanation = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$medianSurvivalHeading <- Html$new()
      self$medianSurvivalExplanation <- Html$new()
      self$medianSurvivalHeading3 <- Html$new()
      self$medianSummary <- Html$new()
      self$medianTable <- Table$new()
      self$coxRegressionHeading <- Html$new()
      self$coxRegressionExplanation <- Html$new()
      self$coxRegressionHeading3 <- Html$new()
      self$coxSummary <- Html$new()
      self$coxTable <- Table$new()
      self$tCoxtext2 <- Html$new()
      self$cox_ph <- Html$new()
      self$plot8 <- Image$new()
      self$survivalTablesHeading <- Html$new()
      self$survivalTablesExplanation <- Html$new()
      self$survivalTablesHeading3 <- Html$new()
      self$survTableSummary <- Html$new()
      self$survTable <- Table$new()
      self$plot <- Image$new()
      self$plot2 <- Image$new()
      self$plot3 <- Image$new()
      self$plot6 <- Image$new()
      self$plot7 <- Image$new()
      self$survivalPlotsHeading3 <- Html$new()
      self$survivalPlotsExplanation <- Html$new()
      self$personTimeHeading <- Html$new()
      self$personTimeTable <- Table$new()
      self$personTimeSummary <- Html$new()
      self$personTimeExplanation <- Html$new()
      self$rmstHeading <- Html$new()
      self$rmstTable <- Table$new()
      self$rmstSummary <- Html$new()
      self$rmstExplanation <- Html$new()
      self$residualsTable <- Table$new()
      self$residualsPlot <- Image$new()
      self$residualDiagnosticsExplanation <- Html$new()
      self$pairwiseComparisonHeading <- Html$new()
      self$pairwiseSummary <- Html$new()
      self$pairwiseTable <- Table$new()
      self$parametricModelComparison <- Table$new()
      self$parametricModelSummary <- Table$new()
      self$parametricDiagnostics <- Html$new()
      self$parametricSurvivalPlot <- Image$new()
      self$hazardFunctionPlot <- Image$new()
      self$extrapolationPlot <- Image$new()
      self$extrapolationTable <- Table$new()
      self$parametricModelsExplanation <- Html$new()
      self$calculatedtime <- Table$new()
      self$outcomeredefined <- Table$new()
      self$survivalExport <- Table$new()
      self$survivalExportSummary <- Html$new()
      self$phInterpretation <- Html$new()
      self$clinicalGlossaryExplanation <- Html$new()
      self$clinicalInterpretationExplanation <- Html$new()
      self$copyReadySentencesExplanation <- Html$new()
    }
  )
)

# Mock jmvcore functions
jmvcore_toNumeric <- function(x) {
  as.numeric(as.character(x))
}

jmvcore_constructFormula <- function(terms) {
  if (length(terms) == 0) return("")
  if (length(terms) == 1) return(terms)
  paste(terms, collapse = "+")
}

jmvcore_naOmit <- function(data) {
  na.omit(data)
}

jmvcore_select <- function(data, vars) {
  data[, vars, drop = FALSE]
}

jmvcore_reject <- function(message, code = NULL) {
  stop(paste0("REJECT: ", message))
}

# Define a translation function mock
. <- function(text) {
  text
}

# Source the survival function code
# We need to modify the source code slightly to make it work in this standalone script
# Specifically, we need to handle the R6 class definition and imports

source_file <- "R/survival.b.R"
source_code <- readLines(source_file)

# Remove the 'survivalClass <- if (requireNamespace('jmvcore'))' wrapper
# and the closing parenthesis/brace at the end
start_line <- grep("survivalClass <- if", source_code)[1]
end_line <- length(source_code)

# Extract the class definition body
class_def <- source_code[(start_line + 1):end_line]

# Replace 'inherit = survivalBase' with 'inherit = R6::R6Class' for testing
class_def <- gsub("inherit = survivalBase,", "inherit = R6::R6Class,", class_def)

# Replace jmvcore:: calls with our mocks
class_def <- gsub("jmvcore::toNumeric", "jmvcore_toNumeric", class_def)
class_def <- gsub("jmvcore::constructFormula", "jmvcore_constructFormula", class_def)
class_def <- gsub("jmvcore::naOmit", "jmvcore_naOmit", class_def)
class_def <- gsub("jmvcore::select", "jmvcore_select", class_def)
class_def <- gsub("jmvcore::reject", "jmvcore_reject", class_def)

# Write the modified code to a temporary file and source it
temp_file <- tempfile(fileext = ".R")
writeLines(c(
  "survivalClass <- R6::R6Class(",
  "  'survivalClass',",
  "  public = list(",
  "    options = NULL,",
  "    data = NULL,",
  "    results = NULL,",
  "    initialize = function(options, data) {",
  "      self$options <- options",
  "      self$data <- data",
  "      self$results <- Results$new()",
  "      private$.init()",
  "    },",
  "    run = function() {",
  "      private$.run()",
  "    }",
  "  ),",
  paste(class_def, collapse = "\n")
), temp_file)

source(temp_file)

# Helper function to create default options
create_options <- function(
  outcome = "status",
  explanatory = "group",
  elapsedtime = "time",
  tint = FALSE,
  dxdate = NULL,
  fudate = NULL,
  outcomeLevel = "1",
  dod = NULL,
  dooc = NULL,
  awd = NULL,
  awod = NULL,
  analysistype = "overall",
  cutp = "12, 36, 60",
  timetypedata = "ymd",
  timetypeoutput = "months",
  uselandmark = FALSE,
  landmark = 0,
  pw = FALSE,
  padjustmethod = "holm",
  ph_cox = FALSE,
  stratified_cox = FALSE,
  strata_variable = NULL,
  rmst_analysis = FALSE,
  rmst_tau = NULL,
  residual_diagnostics = FALSE,
  export_survival_data = FALSE,
  person_time = FALSE,
  time_intervals = "",
  rate_multiplier = 100,
  sc = FALSE,
  ce = FALSE,
  ch = FALSE,
  kmunicate = FALSE,
  loglog = FALSE,
  endplot = 60,
  ybegin_plot = 0,
  yend_plot = 1,
  byplot = 12,
  multievent = FALSE,
  ci95 = TRUE,
  risktable = FALSE,
  censored = TRUE,
  pplot = TRUE,
  medianline = "none",
  showSummaries = TRUE,
  showExplanations = TRUE,
  calculatedtime = FALSE,
  outcomeredefined = FALSE,
  use_parametric = FALSE,
  parametric_distribution = "weibull",
  compare_distributions = FALSE,
  parametric_covariates = TRUE,
  parametric_diagnostics = FALSE,
  parametric_survival_plots = FALSE,
  hazard_plots = FALSE,
  parametric_extrapolation = FALSE,
  extrapolation_time = 0,
  spline_knots = 2,
  spline_scale = "hazard"
) {
  list(
    outcome = outcome,
    explanatory = explanatory,
    elapsedtime = elapsedtime,
    tint = tint,
    dxdate = dxdate,
    fudate = fudate,
    outcomeLevel = outcomeLevel,
    dod = dod,
    dooc = dooc,
    awd = awd,
    awod = awod,
    analysistype = analysistype,
    cutp = cutp,
    timetypedata = timetypedata,
    timetypeoutput = timetypeoutput,
    uselandmark = uselandmark,
    landmark = landmark,
    pw = pw,
    padjustmethod = padjustmethod,
    ph_cox = ph_cox,
    stratified_cox = stratified_cox,
    strata_variable = strata_variable,
    rmst_analysis = rmst_analysis,
    rmst_tau = rmst_tau,
    residual_diagnostics = residual_diagnostics,
    export_survival_data = export_survival_data,
    person_time = person_time,
    time_intervals = time_intervals,
    rate_multiplier = rate_multiplier,
    sc = sc,
    ce = ce,
    ch = ch,
    kmunicate = kmunicate,
    loglog = loglog,
    endplot = endplot,
    ybegin_plot = ybegin_plot,
    yend_plot = yend_plot,
    byplot = byplot,
    multievent = multievent,
    ci95 = ci95,
    risktable = risktable,
    censored = censored,
    pplot = pplot,
    medianline = medianline,
    showSummaries = showSummaries,
    showExplanations = showExplanations,
    calculatedtime = calculatedtime,
    outcomeredefined = outcomeredefined,
    use_parametric = use_parametric,
    parametric_distribution = parametric_distribution,
    compare_distributions = compare_distributions,
    parametric_covariates = parametric_covariates,
    parametric_diagnostics = parametric_diagnostics,
    parametric_survival_plots = parametric_survival_plots,
    hazard_plots = hazard_plots,
    parametric_extrapolation = parametric_extrapolation,
    extrapolation_time = extrapolation_time,
    spline_knots = spline_knots,
    spline_scale = spline_scale
  )
}

# Test Data Generation
# ------------------------------------------------------------------------------
set.seed(123)
n <- 200
test_data <- data.frame(
  time = rexp(n, rate = 0.1) * 12, # Time in months
  status = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
  group = sample(c("A", "B"), n, replace = TRUE),
  age = rnorm(n, 60, 10),
  sex = sample(c("Male", "Female"), n, replace = TRUE)
)

# Add competing risks data
test_data$status_comp <- sample(0:2, n, replace = TRUE, prob = c(0.3, 0.5, 0.2))

# Add date data
start_date <- as.Date("2020-01-01")
test_data$dx_date <- start_date + sample(0:365, n, replace = TRUE)
test_data$fu_date <- test_data$dx_date + test_data$time * 30.44 # Approx days

# Test 1: Basic Survival Analysis
# ------------------------------------------------------------------------------
test_that("Basic Survival Analysis runs correctly", {
  options <- create_options(
    outcome = "status",
    explanatory = "group",
    elapsedtime = "time",
    outcomeLevel = "1",
    sc = TRUE
  )
  
  analysis <- survivalClass$new(options, test_data)
  analysis$run()
  
  # Check median table
  median_table <- analysis$results$medianTable$asDF()
  expect_true(nrow(median_table) > 0)
  expect_true("median" %in% names(median_table))
  
  # Check Cox table
  cox_table <- analysis$results$coxTable$asDF()
  expect_true(nrow(cox_table) > 0)
  expect_true("HR_univariable" %in% names(cox_table))
  
  # Check Survival table
  surv_table <- analysis$results$survTable$asDF()
  expect_true(nrow(surv_table) > 0)
  expect_true("surv" %in% names(surv_table))
})

# Test 2: Person-Time Analysis
# ------------------------------------------------------------------------------
test_that("Person-Time Analysis runs correctly", {
  options <- create_options(
    outcome = "status",
    explanatory = "group",
    elapsedtime = "time",
    outcomeLevel = "1",
    person_time = TRUE,
    rate_multiplier = 1000
  )
  
  analysis <- survivalClass$new(options, test_data)
  analysis$run()
  
  # Check person-time table
  pt_table <- analysis$results$personTimeTable$asDF()
  expect_true(nrow(pt_table) > 0)
  expect_true("person_time" %in% names(pt_table))
  expect_true("rate" %in% names(pt_table))
  
  # Check summary
  expect_true(analysis$results$personTimeSummary$content != "")
})

# Test 3: RMST Analysis
# ------------------------------------------------------------------------------
test_that("RMST Analysis runs correctly", {
  options <- create_options(
    outcome = "status",
    explanatory = "group",
    elapsedtime = "time",
    outcomeLevel = "1",
    rmst_analysis = TRUE,
    rmst_tau = 24
  )
  
  analysis <- survivalClass$new(options, test_data)
  analysis$run()
  
  # Check RMST table
  rmst_table <- analysis$results$rmstTable$asDF()
  expect_true(nrow(rmst_table) > 0)
  expect_true("rmst" %in% names(rmst_table))
  expect_equal(unique(rmst_table$tau), 24)
})

# Test 4: Competing Risks Analysis
# ------------------------------------------------------------------------------
test_that("Competing Risks Analysis runs correctly", {
  options <- create_options(
    outcome = "status_comp",
    explanatory = "group",
    elapsedtime = "time",
    multievent = TRUE,
    analysistype = "compete",
    dod = "1",
    dooc = "2",
    awd = "0", # Using 0 as censored/alive
    awod = "0" # Using 0 as censored/alive
  )
  
  analysis <- survivalClass$new(options, test_data)
  analysis$run()
  
  # Check median table (should be populated from CIF)
  median_table <- analysis$results$medianTable$asDF()
  expect_true(nrow(median_table) > 0)
})

# Test 5: Parametric Survival Models
# ------------------------------------------------------------------------------
test_that("Parametric Survival Models run correctly", {
  # Skip if flexsurv not installed
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    skip("flexsurv package not available")
  }
  
  options <- create_options(
    outcome = "status",
    explanatory = "group",
    elapsedtime = "time",
    outcomeLevel = "1",
    use_parametric = TRUE,
    parametric_distribution = "weibull",
    parametric_diagnostics = TRUE
  )
  
  analysis <- survivalClass$new(options, test_data)
  analysis$run()
  
  # Check model summary
  model_table <- analysis$results$parametricModelSummary$asDF()
  expect_true(nrow(model_table) > 0)
  expect_true("estimate" %in% names(model_table))
  
  # Check diagnostics
  expect_true(analysis$results$parametricDiagnostics$content != "")
})

# Test 6: Date-based Calculation
# ------------------------------------------------------------------------------
test_that("Date-based calculation runs correctly", {
  options <- create_options(
    outcome = "status",
    explanatory = "group",
    tint = TRUE,
    dxdate = "dx_date",
    fudate = "fu_date",
    outcomeLevel = "1",
    timetypedata = "ymd",
    timetypeoutput = "months"
  )
  
  # Convert dates to character for the test as they would come from Jamovi
  test_data_dates <- test_data
  test_data_dates$dx_date <- as.character(test_data$dx_date)
  test_data_dates$fu_date <- as.character(test_data$fu_date)
  
  analysis <- survivalClass$new(options, test_data_dates)
  analysis$run()
  
  # Check if analysis ran
  median_table <- analysis$results$medianTable$asDF()
  expect_true(nrow(median_table) > 0)
})

# Test 7: Pairwise Comparisons
# ------------------------------------------------------------------------------
test_that("Pairwise comparisons run correctly", {
  options <- create_options(
    outcome = "status",
    explanatory = "group",
    elapsedtime = "time",
    outcomeLevel = "1",
    pw = TRUE
  )
  
  analysis <- survivalClass$new(options, test_data)
  analysis$run()
  
  # Check pairwise table
  pairwise_table <- analysis$results$pairwiseTable$asDF()
  expect_true(nrow(pairwise_table) > 0)
  expect_true("value" %in% names(pairwise_table)) # p-value
})

# Run tests
test_result <- test_dir(dirname(temp_file), reporter = "summary")
print(test_result)
