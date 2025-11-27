
# Verification script for survivalcont function
# This script tests the survivalcont function in the ClinicoPath Jamovi module

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
    deleteRows = function() {
      self$rows <- list()
      self$rowCount <- 0
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
    survivalTablesHeading = NULL,
    survivalTablesExplanation = NULL,
    survivalTablesHeading3 = NULL,
    survTableSummary = NULL,
    survTable = NULL,
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
    cutoffAnalysisHeading = NULL,
    rescutTable = NULL,
    cutoffAnalysisHeading3 = NULL,
    cutoffAnalysisExplanation = NULL,
    plot4 = NULL,
    plot5 = NULL,
    multipleCutTable = NULL,
    multipleMedianTable = NULL,
    multipleCutoffsExplanation = NULL,
    multipleSurvTable = NULL,
    plotMultipleCutoffs = NULL,
    plotMultipleSurvival = NULL,
    plot2 = NULL,
    plot3 = NULL,
    plot6 = NULL,
    plot7 = NULL,
    survivalPlotsHeading3 = NULL,
    survivalPlotsExplanation = NULL,
    loglogPlotExplanation = NULL,
    calculatedtime = NULL,
    outcomeredefined = NULL,
    calculatedcutoff = NULL,
    calculatedmulticut = NULL,
    
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
      self$survivalTablesHeading <- Html$new()
      self$survivalTablesExplanation <- Html$new()
      self$survivalTablesHeading3 <- Html$new()
      self$survTableSummary <- Html$new()
      self$survTable <- Table$new()
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
      self$cutoffAnalysisHeading <- Html$new()
      self$rescutTable <- Table$new()
      self$cutoffAnalysisHeading3 <- Html$new()
      self$cutoffAnalysisExplanation <- Html$new()
      self$plot4 <- Image$new()
      self$plot5 <- Image$new()
      self$multipleCutTable <- Table$new()
      self$multipleMedianTable <- Table$new()
      self$multipleCutoffsExplanation <- Html$new()
      self$multipleSurvTable <- Table$new()
      self$plotMultipleCutoffs <- Image$new()
      self$plotMultipleSurvival <- Image$new()
      self$plot2 <- Image$new()
      self$plot3 <- Image$new()
      self$plot6 <- Image$new()
      self$plot7 <- Image$new()
      self$survivalPlotsHeading3 <- Html$new()
      self$survivalPlotsExplanation <- Html$new()
      self$loglogPlotExplanation <- Html$new()
      self$calculatedtime <- Table$new()
      self$outcomeredefined <- Table$new()
      self$calculatedcutoff <- Table$new()
      self$calculatedmulticut <- Table$new()
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

# Source the survivalcont function code
# We need to modify the source code slightly to make it work in this standalone script
# Specifically, we need to handle the R6 class definition and imports

source_file <- "R/survivalcont.b.R"
source_code <- readLines(source_file)

# Find the start of the private list
private_start_line <- grep("private = list\\(", source_code)[1]
end_line <- length(source_code)

# Extract the private member definitions (content inside private = list(...))
# We start from the line after 'private = list('
private_body <- source_code[(private_start_line + 1):end_line]

# Apply mock replacements to private_body
private_body <- gsub("jmvcore::toNumeric", "jmvcore_toNumeric", private_body)
private_body <- gsub("jmvcore::constructFormula", "jmvcore_constructFormula", private_body)
private_body <- gsub("jmvcore::naOmit", "jmvcore_naOmit", private_body)
private_body <- gsub("jmvcore::select", "jmvcore_select", private_body)
private_body <- gsub("jmvcore::reject", "jmvcore_reject", private_body)

# Mock .checkpoint calls
private_body <- gsub("private$.checkpoint()", "# private$.checkpoint()", private_body, fixed = TRUE)
private_body <- gsub("private$.checkpoint(FALSE)", "# private$.checkpoint(FALSE)", private_body, fixed = TRUE)

# Remove the last line (the closing brace of the if block)
# The original file ends with '}', which closes the if(requireNamespace) block
# We don't want that in our new class definition
private_body <- private_body[-length(private_body)]

# Remove the last closing parenthesis and brace to close the list and class
# This is a bit hacky, but we need to reconstruct the class definition
# We'll just extract the functions and put them in a new R6 class

# Create a temporary file with the modified code
temp_file <- tempfile(fileext = ".R")
writeLines(c(
  "survivalcontClass <- R6::R6Class(",
  "  'survivalcontClass',",
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
  "  private = list(",
  private_body
), temp_file)

# Source the temporary file
source(temp_file)

# Helper function to create options
create_options <- function(...) {
  args <- list(...)
  defaults <- list(
    elapsedtime = "time",
    outcome = "status",
    contexpl = "age",
    outcomeLevel = "1",
    tint = FALSE,
    dxdate = "dx_date",
    fudate = "fu_date",
    timetypedata = "ymd",
    timetypeoutput = "months",
    analysistype = "overall",
    multievent = FALSE,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    findcut = FALSE,
    multiple_cutoffs = FALSE,
    num_cutoffs = "two",
    cutoff_method = "quantile",
    min_group_size = 10,
    calculatedcutoff = FALSE,
    calculatedmulticut = FALSE,
    sc = FALSE,
    kmunicate = FALSE,
    ce = FALSE,
    ch = FALSE,
    endplot = 60,
    ybegin_plot = 0,
    yend_plot = 1,
    byplot = 12,
    ci95 = FALSE,
    risktable = FALSE,
    censored = FALSE,
    medianline = "none",
    person_time = FALSE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100,
    rmst_analysis = FALSE,
    rmst_tau = 0,
    residual_diagnostics = FALSE,
    stratified_cox = FALSE,
    strata_variable = NULL,
    loglog = FALSE,
    showExplanations = FALSE,
    showSummaries = FALSE,
    calculatedtime = FALSE,
    outcomeredefined = FALSE,
    uselandmark = FALSE,
    landmark = 3,
    cutp = "12, 36, 60"
  )
  
  for (name in names(args)) {
    defaults[[name]] <- args[[name]]
  }
  
  # Return as a list (mocking the behavior of options object with $ access)
  return(defaults)
}

# Test Data Generation
# ------------------------------------------------------------------------------
set.seed(123)
n <- 200
test_data <- data.frame(
  time = rexp(n, rate = 0.1) * 12, # Time in months
  status = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
  age = rnorm(n, 60, 10),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  biomarker = rnorm(n, 50, 15)
)

# Add competing risks data
test_data$status_comp <- sample(0:2, n, replace = TRUE, prob = c(0.3, 0.5, 0.2))

# Add date data
start_date <- as.Date("2020-01-01")
test_data$dx_date <- start_date + sample(0:365, n, replace = TRUE)
test_data$fu_date <- test_data$dx_date + test_data$time * 30.44 # Approx days

# Test 1: Basic Survival Analysis with Continuous Variable
# ------------------------------------------------------------------------------
test_that("Basic Survival Analysis runs correctly", {
  options <- create_options(
    outcome = "status",
    contexpl = "age",
    elapsedtime = "time",
    outcomeLevel = "1"
  )
  
  analysis <- survivalcontClass$new(options, test_data)
  analysis$run()
  
  # Check Cox table
  cox_table <- analysis$results$coxTable$asDF()
  expect_true(nrow(cox_table) > 0)
  expect_true("HR_univariable" %in% names(cox_table))
})

# Test 2: Optimal Cut-off Finding
# ------------------------------------------------------------------------------
test_that("Optimal Cut-off Finding runs correctly", {
  options <- create_options(
    outcome = "status",
    contexpl = "age",
    elapsedtime = "time",
    outcomeLevel = "1",
    findcut = TRUE,
    sc = TRUE
  )
  
  analysis <- survivalcontClass$new(options, test_data)
  analysis$run()
  
  # Check cut-off table
  rescut_table <- analysis$results$rescutTable$asDF()
  expect_true(nrow(rescut_table) > 0)
  expect_true("cutpoint" %in% names(rescut_table))
  
  # Check median table (should be populated after cut-off)
  median_table <- analysis$results$medianTable$asDF()
  expect_true(nrow(median_table) > 0)
})

# Test 3: Multiple Cut-offs
# ------------------------------------------------------------------------------
test_that("Multiple Cut-offs runs correctly", {
  options <- create_options(
    outcome = "status",
    contexpl = "age",
    elapsedtime = "time",
    outcomeLevel = "1",
    multiple_cutoffs = TRUE,
    num_cutoffs = "two",
    cutoff_method = "quantile"
  )
  
  analysis <- survivalcontClass$new(options, test_data)
  analysis$run()
  
  # Check multiple cut table
  multicut_table <- analysis$results$multipleCutTable$asDF()
  expect_true(nrow(multicut_table) > 0)
})

# Test 4: Person-Time Analysis
# ------------------------------------------------------------------------------
test_that("Person-Time Analysis runs correctly", {
  options <- create_options(
    outcome = "status",
    contexpl = "age",
    elapsedtime = "time",
    outcomeLevel = "1",
    person_time = TRUE,
    rate_multiplier = 1000
  )
  
  analysis <- survivalcontClass$new(options, test_data)
  analysis$run()
  
  # Check person-time table
  pt_table <- analysis$results$personTimeTable$asDF()
  expect_true(nrow(pt_table) > 0)
  expect_true("person_time" %in% names(pt_table))
})

# Test 5: RMST Analysis
# ------------------------------------------------------------------------------
test_that("RMST Analysis runs correctly", {
  options <- create_options(
    outcome = "status",
    contexpl = "age",
    elapsedtime = "time",
    outcomeLevel = "1",
    rmst_analysis = TRUE,
    rmst_tau = 24
  )
  
  analysis <- survivalcontClass$new(options, test_data)
  analysis$run()
  
  # Check RMST table
  rmst_table <- analysis$results$rmstTable$asDF()
  expect_true(nrow(rmst_table) > 0)
  expect_true("rmst" %in% names(rmst_table))
})

# Test 6: Date-based Calculation
# ------------------------------------------------------------------------------
test_that("Date-based calculation runs correctly", {
  options <- create_options(
    outcome = "status",
    contexpl = "age",
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
  
  analysis <- survivalcontClass$new(options, test_data_dates)
  analysis$run()
  
  # Check if analysis ran
  cox_table <- analysis$results$coxTable$asDF()
  expect_true(nrow(cox_table) > 0)
})

# Run tests
# test_result <- test_dir(dirname(temp_file), reporter = "summary")
# print(test_result)
