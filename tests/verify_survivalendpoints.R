
library(R6)
library(testthat)
library(survival)
library(dplyr)

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
    visible = TRUE,
    initialize = function(...) {},
    setVisible = function(visible) {
      self$visible <- visible
    },
    setState = function(state) {}
  )
)

# Mock Results class based on jamovi/survivalendpoints.r.yaml
Results <- R6Class("Results",
  public = list(
    instructions = NULL,
    dataWarning = NULL,
    dataInfo = NULL,
    derivedEndpoints = NULL,
    summaryStats = NULL,
    eventRates = NULL,
    milestoneTable = NULL,
    kmPlot = NULL,
    usageGuide = NULL,
    exportMessage = NULL,
    
    initialize = function() {
      self$instructions <- Html$new()
      self$dataWarning <- Html$new()
      self$dataInfo <- Table$new()
      self$derivedEndpoints <- Table$new()
      self$summaryStats <- Table$new()
      self$eventRates <- Table$new()
      self$milestoneTable <- Table$new()
      self$kmPlot <- Image$new()
      self$usageGuide <- Html$new()
      self$exportMessage <- Html$new()
    }
  )
)

# Mock survivalendpointsBase class
survivalendpointsBase <- R6Class("survivalendpointsBase",
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

# Mock jmvcore::toNumeric
if (!exists("jmvcore")) {
    jmvcore <- list()
}
jmvcore$toNumeric <- function(x) {
    as.numeric(as.character(x))
}

# Read and evaluate the survivalendpointsClass definition
source_file <- "R/survivalendpoints.b.R"
if (!file.exists(source_file)) {
  stop("Could not find R/survivalendpoints.b.R")
}

# Read the file content
file_content <- readLines(source_file)

# Extract the class definition
# Filter out roxygen comments and imports
code_lines <- file_content[!grepl("^#'", file_content)]
code_text <- paste(code_lines, collapse = "\n")

# Remove the "survivalendpointsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) " part
code_text <- gsub("survivalendpointsClass <- if \\(requireNamespace\\('jmvcore', quietly=TRUE\\)\\) ", "", code_text)

# Find the start of R6::R6Class
start_idx <- grep("R6::R6Class", code_lines)[1]
# Assume the class definition goes to the end of the file
class_def_lines <- code_lines[start_idx:length(code_lines)]
class_def_text <- paste(class_def_lines, collapse = "\n")

# Assign to survivalendpointsClass
survivalendpointsClass <- eval(parse(text = class_def_text))


# Helper to create options list
create_options <- function(
    patientId = "patient",
    startDate = "start_date",
    lastFollowup = "last_followup",
    progressionDate = NULL,
    deathDate = NULL,
    progressionEvent = NULL,
    deathEvent = NULL,
    treatmentEnd = NULL,
    responseDate = NULL,
    timeUnit = "months",
    inputType = "dates",
    calculatePFS = FALSE,
    calculateOS = FALSE,
    calculateTTP = FALSE,
    calculateDOR = FALSE,
    calculateTOT = FALSE,
    showDerivedData = TRUE,
    showSummaryStats = TRUE,
    showEventRates = FALSE,
    showKMPlot = FALSE,
    showMilestones = FALSE,
    milestones = "6,12,24",
    exportEndpoints = FALSE,
    showUsageGuide = FALSE
) {
  list(
    patientId = patientId,
    startDate = startDate,
    lastFollowup = lastFollowup,
    progressionDate = progressionDate,
    deathDate = deathDate,
    progressionEvent = progressionEvent,
    deathEvent = deathEvent,
    treatmentEnd = treatmentEnd,
    responseDate = responseDate,
    timeUnit = timeUnit,
    inputType = inputType,
    calculatePFS = calculatePFS,
    calculateOS = calculateOS,
    calculateTTP = calculateTTP,
    calculateDOR = calculateDOR,
    calculateTOT = calculateTOT,
    showDerivedData = showDerivedData,
    showSummaryStats = showSummaryStats,
    showEventRates = showEventRates,
    showKMPlot = showKMPlot,
    showMilestones = showMilestones,
    milestones = milestones,
    exportEndpoints = exportEndpoints,
    showUsageGuide = showUsageGuide
  )
}

# --- Tests ---

test_that("PFS Calculation (Progression Event)", {
  data <- data.frame(
    patient = c("P1", "P2", "P3"),
    start_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    last_followup = as.Date(c("2023-06-01", "2023-06-01", "2023-06-01")),
    prog_date = as.Date(c("2023-03-01", NA, NA)),
    death_date = as.Date(c(NA, "2023-04-01", NA))
  )
  
  options <- create_options(
    patientId = "patient",
    startDate = "start_date",
    lastFollowup = "last_followup",
    progressionDate = "prog_date",
    deathDate = "death_date",
    calculatePFS = TRUE,
    timeUnit = "months"
  )
  
  analysis <- survivalendpointsClass$new(options = options, data = data)
  analysis$run()
  
  res <- analysis$results$derivedEndpoints$asDF()
  
  # P1: Progression at 2 months (approx) -> Event = 1, Time = 2
  # P2: Death at 3 months (approx) -> Event = 1, Time = 3
  # P3: Censored at 5 months (approx) -> Event = 0, Time = 5
  
  expect_equal(nrow(res), 3)
  expect_equal(res$pfs_event, c(1, 1, 0))
  expect_true(abs(res$pfs_time[1] - 1.93) < 0.1) # ~2 months
  expect_true(abs(res$pfs_time[2] - 2.95) < 0.1) # ~3 months
  expect_true(abs(res$pfs_time[3] - 4.92) < 0.1) # ~5 months
})

test_that("PFS Calculation (Prioritization)", {
  # Case where both progression and death occur
  data <- data.frame(
    patient = c("P1"),
    start_date = as.Date("2023-01-01"),
    last_followup = as.Date("2023-12-01"),
    prog_date = as.Date("2023-03-01"), # 2 months
    death_date = as.Date("2023-06-01")  # 5 months
  )
  
  options <- create_options(
    patientId = "patient",
    startDate = "start_date",
    lastFollowup = "last_followup",
    progressionDate = "prog_date",
    deathDate = "death_date",
    calculatePFS = TRUE,
    timeUnit = "months"
  )
  
  analysis <- survivalendpointsClass$new(options = options, data = data)
  analysis$run()
  
  res <- analysis$results$derivedEndpoints$asDF()
  
  # Should take earliest event (Progression at 2 months)
  expect_equal(res$pfs_event, 1)
  expect_true(abs(res$pfs_time - 1.93) < 0.1)
})

test_that("OS Calculation", {
  data <- data.frame(
    patient = c("P1", "P2"),
    start_date = as.Date(c("2023-01-01", "2023-01-01")),
    last_followup = as.Date(c("2023-06-01", "2023-06-01")),
    death_date = as.Date(c("2023-03-01", NA))
  )
  
  options <- create_options(
    patientId = "patient",
    startDate = "start_date",
    lastFollowup = "last_followup",
    deathDate = "death_date",
    calculateOS = TRUE,
    timeUnit = "months"
  )
  
  analysis <- survivalendpointsClass$new(options = options, data = data)
  analysis$run()
  
  res <- analysis$results$derivedEndpoints$asDF()
  
  # P1: Death at 2 months -> Event = 1
  # P2: Censored at 5 months -> Event = 0
  
  expect_equal(res$os_event, c(1, 0))
  expect_true(abs(res$os_time[1] - 1.93) < 0.1)
  expect_true(abs(res$os_time[2] - 4.92) < 0.1)
})

test_that("TTP Calculation", {
  # TTP censors death without progression
  data <- data.frame(
    patient = c("P1", "P2", "P3"),
    start_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    last_followup = as.Date(c("2023-06-01", "2023-06-01", "2023-06-01")),
    prog_date = as.Date(c("2023-03-01", NA, NA)),
    death_date = as.Date(c(NA, "2023-04-01", NA))
  )
  
  options <- create_options(
    patientId = "patient",
    startDate = "start_date",
    lastFollowup = "last_followup",
    progressionDate = "prog_date",
    deathDate = "death_date",
    calculateTTP = TRUE,
    timeUnit = "months"
  )
  
  analysis <- survivalendpointsClass$new(options = options, data = data)
  analysis$run()
  
  res <- analysis$results$derivedEndpoints$asDF()
  
  # P1: Progression at 2 months -> Event = 1
  # P2: Death at 3 months -> Censored at death time -> Event = 0, Time = 3
  # P3: No event -> Censored at last followup -> Event = 0, Time = 5
  
  expect_equal(res$ttp_event, c(1, 0, 0))
  expect_true(abs(res$ttp_time[1] - 1.93) < 0.1)
  expect_true(abs(res$ttp_time[2] - 2.95) < 0.1) # Censored at death
})

test_that("DOR Calculation", {
  data <- data.frame(
    patient = c("P1", "P2", "P3"),
    start_date = as.Date("2023-01-01"), # Irrelevant for DOR
    last_followup = as.Date(c("2023-08-01", "2023-08-01", "2023-08-01")),
    response_date = as.Date(c("2023-02-01", "2023-03-01", NA)),
    prog_date = as.Date(c("2023-05-01", NA, NA))
  )
  
  options <- create_options(
    patientId = "patient",
    startDate = "start_date",
    lastFollowup = "last_followup",
    responseDate = "response_date",
    progressionDate = "prog_date",
    calculateDOR = TRUE,
    timeUnit = "months"
  )
  
  analysis <- survivalendpointsClass$new(options = options, data = data)
  analysis$run()
  
  res <- analysis$results$derivedEndpoints$asDF()
  
  # P1: Response Feb 1, Prog May 1 -> Duration 3 months -> Event = 1
  # P2: Response Mar 1, No Prog -> Censored Aug 1 -> Duration 5 months -> Event = 0
  # P3: No Response -> NA
  
  expect_equal(res$dor_event, c(1, 0, NA))
  expect_true(abs(res$dor_time[1] - 2.92) < 0.1)
  expect_true(abs(res$dor_time[2] - 5.03) < 0.1)
  expect_true(is.na(res$dor_time[3]))
})
