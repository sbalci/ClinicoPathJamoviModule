
# Verification script for singlearm function
# This script mocks the Jamovi environment and tests the singlearm function

# Install necessary packages if not present
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("labelled", quietly = TRUE)) install.packages("labelled")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
if (!requireNamespace("cmprsk", quietly = TRUE)) install.packages("cmprsk")
if (!requireNamespace("finalfit", quietly = TRUE)) install.packages("finalfit")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(R6)
library(dplyr)
library(glue)
library(janitor)
library(labelled)
library(ggplot2)
library(survival)
library(cmprsk)
library(finalfit)
library(lubridate)

# --- Mocking jmvcore ---

# Mock Options class
Options <- R6::R6Class("Options",
  public = list(
    elapsedtime = NULL,
    outcome = NULL,
    outcomeLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    analysistype = "overall",
    multievent = FALSE,
    cutp = "12, 36, 60",
    timetypedata = "ymd",
    timetypeoutput = "months",
    uselandmark = FALSE,
    landmark = 3,
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
    baseline_hazard = FALSE,
    hazard_smoothing = FALSE,
    showExplanations = FALSE,
    showSummaries = FALSE,
    advancedDiagnostics = FALSE,
    clinical_preset = "overall_survival",
    guided_mode = FALSE,
    tint = FALSE,
    dxdate = NULL,
    fudate = NULL,
    calculatedtime = FALSE,
    outcomeredefined = FALSE,
    
    initialize = function(elapsedtime = NULL, outcome = NULL, outcomeLevel = NULL, 
                          analysistype = "overall", multievent = FALSE, ...) {
      self$elapsedtime <- elapsedtime
      self$outcome <- outcome
      self$outcomeLevel <- outcomeLevel
      self$analysistype <- analysistype
      self$multievent <- multievent
      
      args <- list(...)
      for (name in names(args)) {
        self[[name]] <- args[[name]]
      }
    }
  )
)

# Mock Table class
Table <- R6::R6Class("Table",
  public = list(
    rows = list(),
    columns = list(),
    visible = TRUE,
    rowCount = 0,
    initialize = function() {
      self$rows <- list()
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    addRow = function(rowKey, values) {
      self$rows[[length(self$rows) + 1]] <- values
      self$rowCount <- self$rowCount + 1
    },
    getCell = function(rowNo, colName) {
      if (rowNo > length(self$rows)) return(NULL)
      row <- self$rows[[rowNo]]
      if (colName %in% names(row)) {
        return(list(value = row[[colName]]))
      } else {
        # Try to find by index if names don't match
        return(list(value = NULL))
      }
    }
  )
)

# Mock Image class
Image <- R6::R6Class("Image",
  public = list(
    state = NULL,
    visible = TRUE,
    setVisible = function(visible) {
      self$visible <- visible
    },
    setState = function(state) {
      self$state <- state
    }
  )
)

# Mock Html class
Html <- R6::R6Class("Html",
  public = list(
    content = NULL,
    visible = TRUE,
    setVisible = function(visible) {
      self$visible <- visible
    },
    setContent = function(content) {
      self$content <- content
    }
  )
)

# Mock Output class
Output <- R6::R6Class("Output",
  public = list(
    values = NULL,
    rowNums = NULL,
    visible = TRUE,
    isNotFilled = function() {
      return(is.null(self$values))
    },
    setRowNums = function(rowNums) {
      self$rowNums <- rowNums
    },
    setValues = function(values) {
      self$values <- values
    },
    setVisible = function(visible) {
      self$visible <- visible
    }
  )
)

# Mock Results class
Results <- R6::R6Class("Results",
  public = list(
    todo = NULL,
    medianTable = NULL,
    medianSummary = NULL,
    survTable = NULL,
    survTableSummary = NULL,
    plot = NULL,
    plot2 = NULL,
    plot3 = NULL,
    plot6 = NULL,
    personTimeTable = NULL,
    personTimeSummary = NULL,
    baselineHazardTable = NULL,
    baselineHazardPlot = NULL,
    smoothedHazardPlot = NULL,
    baselineHazardSummary = NULL,
    dataQualityTable = NULL,
    dataQualitySummary = NULL,
    clinicalSummary = NULL,
    calculatedtime = NULL,
    outcomeredefined = NULL,
    
    # Headings and Explanations
    medianHeading3 = NULL,
    medianSurvivalExplanation = NULL,
    survivalProbabilityExplanation = NULL,
    survivalPlotsHeading3 = NULL,
    survivalPlotsExplanation = NULL,
    personTimeHeading = NULL,
    personTimeHeading2 = NULL,
    personTimeHeading3 = NULL,
    personTimeExplanation = NULL,
    baselineHazardHeading = NULL,
    baselineHazardHeading3 = NULL,
    baselineHazardExplanation = NULL,
    dataQualityHeading = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$medianTable <- Table$new()
      self$medianSummary <- Html$new()
      self$survTable <- Table$new()
      self$survTableSummary <- Html$new()
      self$plot <- Image$new()
      self$plot2 <- Image$new()
      self$plot3 <- Image$new()
      self$plot6 <- Image$new()
      self$personTimeTable <- Table$new()
      self$personTimeSummary <- Html$new()
      self$baselineHazardTable <- Table$new()
      self$baselineHazardPlot <- Image$new()
      self$smoothedHazardPlot <- Image$new()
      self$baselineHazardSummary <- Html$new()
      self$dataQualityTable <- Table$new()
      self$dataQualitySummary <- Html$new()
      self$clinicalSummary <- Html$new()
      self$calculatedtime <- Output$new()
      self$outcomeredefined <- Output$new()
      
      # Initialize headings and explanations
      self$medianHeading3 <- Html$new()
      self$medianSurvivalExplanation <- Html$new()
      self$survivalProbabilityExplanation <- Html$new()
      self$survivalPlotsHeading3 <- Html$new()
      self$survivalPlotsExplanation <- Html$new()
      self$personTimeHeading <- Html$new()
      self$personTimeHeading2 <- Html$new()
      self$personTimeHeading3 <- Html$new()
      self$personTimeExplanation <- Html$new()
      self$baselineHazardHeading <- Html$new()
      self$baselineHazardHeading3 <- Html$new()
      self$baselineHazardExplanation <- Html$new()
      self$dataQualityHeading <- Html$new()
    }
  )
)

# Mock jmvcore functions
jmvcore_toNumeric <- function(x) {
  as.numeric(as.character(x))
}

jmvcore_constructFormula <- function(terms) {
  return(terms)
}

jmvcore_select <- function(df, cols) {
  df[, cols, drop = FALSE]
}

jmvcore_naOmit <- function(df) {
  na.omit(df)
}

# Mock localization function
. <- function(text) {
  return(text)
}

# Define singlearmBase
singlearmBase <- R6::R6Class("singlearmBase",
  public = list(
    options = NULL,
    data = NULL,
    results = NULL,
    initialize = function(options, data) {
      self$options <- options
      self$data <- data
      self$results <- Results$new()
    }
  ),
  private = list(
    .checkpoint = function() {}
  )
)

# Mock jmvcore package
jmvcore <- list(
  toNumeric = jmvcore_toNumeric,
  constructFormula = jmvcore_constructFormula,
  select = jmvcore_select,
  naOmit = jmvcore_naOmit,
  Option = R6::R6Class("Option"), # Dummy
  OptionList = R6::R6Class("OptionList"), # Dummy
  OptionBool = R6::R6Class("OptionBool"), # Dummy
  OptionInteger = R6::R6Class("OptionInteger"), # Dummy
  OptionNumber = R6::R6Class("OptionNumber"), # Dummy
  OptionString = R6::R6Class("OptionString") # Dummy
)

# Source the file, replacing jmvcore:: calls and removing the conditional class definition
source_file <- "R/singlearm.b.R"
file_content <- readLines(source_file)

# Replace jmvcore:: calls
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Remove the conditional class definition
# We want to keep the class body but remove the "if (requireNamespace...)" wrapper
# Find the start and end of the class definition
start_line <- grep("singlearmClass <- if", file_content)
# We will just replace the first line with the direct assignment
file_content[start_line] <- "singlearmClass <- R6::R6Class("

# Execute the modified code
eval(parse(text = file_content))

# Check if class exists
if (!exists("singlearmClass")) {
  stop("singlearmClass not defined!")
} else {
  print("singlearmClass successfully created.")
}

# --- Helper Function to Run Tests ---

# Helper for defaults
default <- function(x, val) {
  if (is.null(x)) val else x
}

run_test <- function(test_name, data, options_list, class_def) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  options <- Options$new(
    elapsedtime = options_list$elapsedtime,
    outcome = options_list$outcome,
    outcomeLevel = options_list$outcomeLevel,
    analysistype = default(options_list$analysistype, "overall"),
    multievent = default(options_list$multievent, FALSE),
    dod = options_list$dod,
    dooc = options_list$dooc,
    awd = options_list$awd,
    awod = options_list$awod,
    sc = default(options_list$sc, FALSE),
    ce = default(options_list$ce, FALSE),
    ch = default(options_list$ch, FALSE),
    person_time = default(options_list$person_time, FALSE),
    baseline_hazard = default(options_list$baseline_hazard, FALSE),
    hazard_smoothing = default(options_list$hazard_smoothing, FALSE),
    showSummaries = default(options_list$showSummaries, FALSE),
    showExplanations = default(options_list$showExplanations, FALSE),
    advancedDiagnostics = default(options_list$advancedDiagnostics, FALSE),
    tint = default(options_list$tint, FALSE),
    dxdate = options_list$dxdate,
    fudate = options_list$fudate,
    timetypedata = default(options_list$timetypedata, "ymd")
  )
  
  analysis <- class_def$new(options = options, data = data)
  
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    return(analysis$results)
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    traceback()
    return(NULL)
  })
}

# --- Test Data Setup ---

# 1. Basic Survival Data
set.seed(123)
n <- 100
basic_data <- data.frame(
  time = rexp(n, rate = 0.1) * 12, # Time in months
  status = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
  stringsAsFactors = FALSE
)
basic_data$status_factor <- factor(basic_data$status, levels = c(0, 1), labels = c("Alive", "Dead"))

# 2. Competing Risks Data
competing_data <- data.frame(
  time = rexp(n, rate = 0.1) * 12,
  status = sample(c("Alive", "Dead of Disease", "Dead of Other"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  stringsAsFactors = FALSE
)

# 3. Date-based Data
date_data <- data.frame(
  dx_date = as.Date("2020-01-01") + sample(0:365, n, replace = TRUE),
  fu_date = as.Date("2023-01-01") + sample(0:365, n, replace = TRUE),
  status = sample(c(0, 1), n, replace = TRUE),
  stringsAsFactors = FALSE
)
# Ensure fu_date > dx_date
date_data$fu_date <- pmax(date_data$fu_date, date_data$dx_date + 1)


# --- Test Cases ---

# Test 1: Basic Overall Survival (KM)
print("Running Test 1: Basic Overall Survival (KM)")
results1 <- run_test(
  "Basic Overall Survival",
  basic_data,
  list(
    elapsedtime = "time",
    outcome = "status_factor",
    outcomeLevel = "Dead",
    analysistype = "overall",
    multievent = FALSE,
    sc = TRUE,
    showSummaries = TRUE
  ),
  singlearmClass
)

if (!is.null(results1)) {
  print("Median Table:")
  print(results1$medianTable$rows)
  print("Survival Table Rows:")
  print(length(results1$survTable$rows))
}

# Test 2: Competing Risks
print("Running Test 2: Competing Risks")
results2 <- run_test(
  "Competing Risks",
  competing_data,
  list(
    elapsedtime = "time",
    outcome = "status",
    analysistype = "compete",
    multievent = TRUE,
    dod = "Dead of Disease",
    dooc = "Dead of Other",
    awd = "Alive",
    awod = NULL,
    showSummaries = TRUE
  ),
  singlearmClass
)

if (!is.null(results2)) {
  print("Median Table (Competing Risk):")
  print(results2$medianTable$rows)
}

# Test 3: Person-Time Analysis
print("Running Test 3: Person-Time Analysis")
results3 <- run_test(
  "Person-Time Analysis",
  basic_data,
  list(
    elapsedtime = "time",
    outcome = "status_factor",
    outcomeLevel = "Dead",
    analysistype = "overall",
    person_time = TRUE,
    showSummaries = TRUE
  ),
  singlearmClass
)

if (!is.null(results3)) {
  print("Person-Time Table:")
  print(results3$personTimeTable$rows)
}

# Test 4: Baseline Hazard
print("Running Test 4: Baseline Hazard")
results4 <- run_test(
  "Baseline Hazard",
  basic_data,
  list(
    elapsedtime = "time",
    outcome = "status_factor",
    outcomeLevel = "Dead",
    analysistype = "overall",
    baseline_hazard = TRUE,
    hazard_smoothing = TRUE,
    showSummaries = TRUE
  ),
  singlearmClass
)

if (!is.null(results4)) {
  print("Baseline Hazard Table Rows:")
  print(length(results4$baselineHazardTable$rows))
}

# Test 5: Date Calculation
print("Running Test 5: Date Calculation")
results5 <- run_test(
  "Date Calculation",
  date_data,
  list(
    tint = TRUE,
    dxdate = "dx_date",
    fudate = "fu_date",
    timetypedata = "ymd",
    outcome = "status",
    outcomeLevel = "1",
    analysistype = "overall"
  ),
  singlearmClass
)

if (!is.null(results5)) {
  print("Median Table (Date Calc):")
  print(results5$medianTable$rows)
}

# Test 6: Validation - Missing Outcome Variable
print("Running Test 6: Validation - Missing Outcome Variable")
results6 <- run_test(
  "Missing Outcome Variable",
  basic_data,
  list(
    elapsedtime = "time",
    outcome = "non_existent_var",
    outcomeLevel = "Dead"
  ),
  singlearmClass
)

# Test 7: Validation - Missing Outcome Level
print("Running Test 7: Validation - Missing Outcome Level")
results7 <- run_test(
  "Missing Outcome Level",
  basic_data,
  list(
    elapsedtime = "time",
    outcome = "status_factor",
    outcomeLevel = NULL
  ),
  singlearmClass
)

