
library(R6)
library(testthat)
library(lubridate)
library(glue)

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
    setRow = function(rowNo, values) {
      self$rows[[as.character(rowNo)]] <- values
      self$rowCount <- length(self$rows)
    },
    setRowNums = function(rowNums) {
        # Mock method for setting row numbers (used in add_times)
    },
    setValues = function(values) {
        # Mock method for setting values (used in add_times)
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

Output <- R6Class("Output",
    public = list(
        value = NULL,
        initialize = function(...) {},
        setRowNums = function(rowNums) {},
        setValues = function(values) {
            self$value <- values
        }
    )
)

# Mock Results class based on jamovi/timeinterval.r.yaml
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    aboutPanel = NULL,
    personTimeInfo = NULL,
    qualityAssessment = NULL,
    caveatsPanel = NULL,
    summary = NULL,
    nlSummary = NULL,
    glossaryPanel = NULL,
    calculated_time = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$aboutPanel <- Html$new()
      self$personTimeInfo <- Html$new()
      self$qualityAssessment <- Html$new()
      self$caveatsPanel <- Html$new()
      self$summary <- Html$new()
      self$nlSummary <- Html$new()
      self$glossaryPanel <- Html$new()
      self$calculated_time <- Output$new()
    }
  )
)

# Mock timeintervalBase class
timeintervalBase <- R6Class("timeintervalBase",
  public = list(
    options = NULL,
    results = NULL,
    data = NULL,
    initialize = function(options = list(), results = NULL, data = NULL) {
      self$options <- options
      self$results <- if (is.null(results)) Results$new() else results
      self$data <- data
    },
    run = function() {
      private$.run()
    }
  )
)

# Read and evaluate the timeintervalClass definition
source_file <- "R/timeinterval.b.R"
if (!file.exists(source_file)) {
  stop("Could not find R/timeinterval.b.R")
}

# Read the file content
file_content <- readLines(source_file)

# Extract the class definition
# Filter out roxygen comments and imports
code_lines <- file_content[!grepl("^#'", file_content)]
code_text <- paste(code_lines, collapse = "\n")

# Remove the "timeintervalClass <- if (requireNamespace('jmvcore')) " part and the closing parenthesis
# This is a bit hacky but necessary to load the class in a standalone script
code_text <- gsub("timeintervalClass <- if \\(requireNamespace\\('jmvcore'\\)\\) ", "", code_text)
# We need to find the matching closing parenthesis for the R6Class definition.
# Since it's at the end of the file, we can just evaluate it.
# But wait, the file assigns it to timeintervalClass.
# Let's just eval the R6Class definition directly.

# Find the start of R6::R6Class
start_idx <- grep("R6::R6Class", code_lines)[1]
# Assume the class definition goes to the end of the file
class_def_lines <- code_lines[start_idx:length(code_lines)]
class_def_text <- paste(class_def_lines, collapse = "\n")

# Assign to timeintervalClass
timeintervalClass <- eval(parse(text = class_def_text))


# Helper to create options list
create_options <- function(
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months",
    use_landmark = FALSE,
    landmark_time = 6,
    remove_negative = FALSE,
    remove_extreme = FALSE,
    extreme_multiplier = 2.0,
    add_times = FALSE,
    include_quality_metrics = FALSE,
    confidence_level = 95,
    show_summary = FALSE,
    show_glossary = FALSE,
    timezone = "system"
) {
  list(
    dx_date = dx_date,
    fu_date = fu_date,
    time_format = time_format,
    output_unit = output_unit,
    use_landmark = use_landmark,
    landmark_time = landmark_time,
    remove_negative = remove_negative,
    remove_extreme = remove_extreme,
    extreme_multiplier = extreme_multiplier,
    add_times = add_times,
    include_quality_metrics = include_quality_metrics,
    confidence_level = confidence_level,
    show_summary = show_summary,
    show_glossary = show_glossary,
    timezone = timezone
  )
}

# --- Tests ---

test_that("Basic Time Interval Calculation (Months)", {
  data <- data.frame(
    start_date = c("2023-01-01", "2023-02-01"),
    end_date = c("2023-03-01", "2023-05-01")
  )
  
  options <- create_options(
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months",
    add_times = TRUE
  )
  
  analysis <- timeintervalClass$new(options = options, data = data)
  analysis$run()
  
  # Check calculated values
  # 2023-01-01 to 2023-03-01 is roughly 2 months (59 days)
  # 2023-02-01 to 2023-05-01 is roughly 3 months (89 days)
  # lubridate::time_length(interval, "months") uses average month length (30.4375 days)
  
  calc_values <- analysis$results$calculated_time$value
  expect_equal(length(calc_values), 2)
  expect_true(abs(calc_values[1] - 2) < 0.1) # Approx 2 months
  expect_true(abs(calc_values[2] - 3) < 0.1) # Approx 3 months
})

test_that("Date Format Auto-Detection", {
  data <- data.frame(
    start_date = c("01/01/2023", "01/02/2023"), # DMY or MDY? Ambiguous without context, but let's assume DMY
    end_date = c("01/03/2023", "01/05/2023")
  )
  
  # Test explicit format
  options <- create_options(
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "dmy",
    output_unit = "months",
    add_times = TRUE
  )
  
  analysis <- timeintervalClass$new(options = options, data = data)
  analysis$run()
  
  calc_values <- analysis$results$calculated_time$value
  expect_equal(length(calc_values), 2)
  expect_true(abs(calc_values[1] - 2) < 0.1)
})

test_that("Landmark Analysis", {
  data <- data.frame(
    start_date = c("2023-01-01", "2023-01-01", "2023-01-01"),
    end_date = c("2023-02-01", "2023-04-01", "2023-08-01")
  )
  # Intervals: ~1 month, ~3 months, ~7 months
  
  options <- create_options(
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months",
    use_landmark = TRUE,
    landmark_time = 2, # Landmark at 2 months
    add_times = TRUE
  )
  
  analysis <- timeintervalClass$new(options = options, data = data)
  analysis$run()
  
  calc_values <- analysis$results$calculated_time$value
  
  # Should exclude the first subject (< 2 months)
  # Should subtract 2 months from the others
  # Subject 2: 3 - 2 = 1 month
  # Subject 3: 7 - 2 = 5 months
  
  expect_equal(length(calc_values), 2)
  expect_true(abs(calc_values[1] - 1) < 0.2)
  expect_true(abs(calc_values[2] - 5) < 0.2)
})

test_that("Negative Interval Detection", {
  data <- data.frame(
    start_date = c("2023-05-01"),
    end_date = c("2023-01-01") # Negative interval
  )
  
  options <- create_options(
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months"
  )
  
  analysis <- timeintervalClass$new(options = options, data = data)
  
  # Should stop with error
  expect_error(analysis$run(), "CRITICAL: 1 negative time interval")
})

test_that("Missing Data Handling", {
  data <- data.frame(
    start_date = c("2023-01-01", NA),
    end_date = c("2023-03-01", "2023-05-01")
  )
  
  options <- create_options(
    dx_date = "start_date",
    fu_date = "end_date",
    time_format = "ymd",
    output_unit = "months",
    add_times = TRUE
  )
  
  analysis <- timeintervalClass$new(options = options, data = data)
  analysis$run()
  
  calc_values <- analysis$results$calculated_time$value
  expect_equal(length(calc_values), 2)
  expect_true(!is.na(calc_values[1]))
  expect_true(is.na(calc_values[2]))
})
