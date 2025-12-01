
library(R6)
library(testthat)
library(ggplot2)

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

Output <- R6Class("Output",
    public = list(
        value = NULL,
        rowNums = NULL,
        initialize = function(...) {},
        setRowNums = function(rowNums) {
            self$rowNums <- rowNums
        },
        setValues = function(values) {
            self$value <- values
        },
        isNotFilled = function() {
            return(TRUE) # Always return TRUE for testing to allow overwrite
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

# Mock Results class based on jamovi/categorize.r.yaml
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    warnings = NULL,
    summaryText = NULL,
    breakpointsTable = NULL,
    freqTable = NULL,
    addtodata = NULL,
    rcode = NULL,
    plot = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$warnings <- Html$new()
      self$summaryText <- Html$new()
      self$breakpointsTable <- Table$new()
      self$freqTable <- Table$new()
      self$addtodata <- Output$new()
      self$rcode <- Html$new()
      self$plot <- Image$new()
    }
  )
)

# Mock categorizeBase class
categorizeBase <- R6Class("categorizeBase",
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

# Read and evaluate the categorizeClass definition
source_file <- "R/categorize.b.R"
if (!file.exists(source_file)) {
  stop("Could not find R/categorize.b.R")
}

# Read the file content
file_content <- readLines(source_file)

# Extract the class definition
# Filter out roxygen comments and imports
code_lines <- file_content[!grepl("^#'", file_content)]
code_text <- paste(code_lines, collapse = "\n")

# Remove the "categorizeClass <- if (requireNamespace('jmvcore')) " part
code_text <- gsub("categorizeClass <- if \\(requireNamespace\\('jmvcore'\\)\\) ", "", code_text)

# Find the start of R6::R6Class
start_idx <- grep("R6::R6Class", code_lines)[1]
# Assume the class definition goes to the end of the file
class_def_lines <- code_lines[start_idx:length(code_lines)]
class_def_text <- paste(class_def_lines, collapse = "\n")

# Assign to categorizeClass
categorizeClass <- eval(parse(text = class_def_text))


# Helper to create options list
create_options <- function(
    var = "x",
    method = "quantile",
    nbins = 4,
    breaks = "",
    sdmult = 1,
    labels = "auto",
    customlabels = "",
    newvarname = "",
    addtodata = FALSE,
    includelowest = TRUE,
    rightclosed = TRUE,
    ordered = TRUE,
    excl = TRUE,
    showcode = FALSE,
    showplot = FALSE
) {
  list(
    var = var,
    method = method,
    nbins = nbins,
    breaks = breaks,
    sdmult = sdmult,
    labels = labels,
    customlabels = customlabels,
    newvarname = newvarname,
    addtodata = addtodata,
    includelowest = includelowest,
    rightclosed = rightclosed,
    ordered = ordered,
    excl = excl,
    showcode = showcode,
    showplot = showplot
  )
}

# --- Tests ---

test_that("Equal Intervals Binning", {
  data <- data.frame(x = 1:100)
  
  options <- create_options(
    var = "x",
    method = "equal",
    nbins = 4,
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  # Check breaks
  breaks_df <- analysis$results$breakpointsTable$asDF()
  breaks <- breaks_df$value
  
  # Should be 1, 25.75, 50.5, 75.25, 100
  expect_equal(length(breaks), 5)
  expect_equal(breaks[1], 1)
  expect_equal(breaks[5], 100)
  
  # Check frequency table
  freq_df <- analysis$results$freqTable$asDF()
  # Should have 4 bins
  expect_equal(nrow(freq_df), 4)
})

test_that("Quantile Binning", {
  data <- data.frame(x = 1:100)
  
  options <- create_options(
    var = "x",
    method = "quantile",
    nbins = 4,
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  # Check breaks
  breaks_df <- analysis$results$breakpointsTable$asDF()
  breaks <- breaks_df$value
  
  # Should be 1, 25.75, 50.5, 75.25, 100 (for 1:100, quantiles are similar to equal intervals)
  expect_equal(length(breaks), 5)
  
  # Check frequency table - counts should be equal (25 each)
  freq_df <- analysis$results$freqTable$asDF()
  expect_equal(freq_df$n, c(25, 25, 25, 25))
})

test_that("Manual Breaks", {
  data <- data.frame(x = 1:100)
  
  options <- create_options(
    var = "x",
    method = "manual",
    breaks = "0, 20, 80, 100",
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  breaks_df <- analysis$results$breakpointsTable$asDF()
  breaks <- breaks_df$value
  
  expect_equal(breaks, c(0, 20, 80, 100))
  
  freq_df <- analysis$results$freqTable$asDF()
  expect_equal(nrow(freq_df), 3)
  
  # Count in 0-20: 1 to 20 = 20
  # Count in 20-80: 21 to 80 = 60
  # Count in 80-100: 81 to 100 = 20
  expect_equal(freq_df$n, c(20, 60, 20))
})

test_that("Mean +/- SD Binning", {
  # Create normal data
  set.seed(123)
  data <- data.frame(x = rnorm(1000, mean = 50, sd = 10))
  
  options <- create_options(
    var = "x",
    method = "meansd",
    sdmult = 1,
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  breaks_df <- analysis$results$breakpointsTable$asDF()
  breaks <- breaks_df$value
  
  # Should have 5 breaks: min, mean-sd, mean, mean+sd, max
  expect_equal(length(breaks), 5)
  
  m <- mean(data$x)
  s <- sd(data$x)
  
  expect_true(abs(breaks[2] - (m - s)) < 0.1)
  expect_true(abs(breaks[3] - m) < 0.1)
  expect_true(abs(breaks[4] - (m + s)) < 0.1)
})

test_that("Median Split", {
  data <- data.frame(x = 1:100)
  
  options <- create_options(
    var = "x",
    method = "median",
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  breaks_df <- analysis$results$breakpointsTable$asDF()
  breaks <- breaks_df$value
  
  # Should have 3 breaks: min, median, max
  expect_equal(length(breaks), 3)
  expect_equal(breaks[2], median(data$x))
  
  freq_df <- analysis$results$freqTable$asDF()
  expect_equal(nrow(freq_df), 2)
  expect_equal(freq_df$n, c(50, 50))
})

test_that("Label Generation", {
  data <- data.frame(x = 1:100)
  
  # Semantic labels
  options <- create_options(
    var = "x",
    method = "quantile",
    nbins = 3,
    labels = "semantic",
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  freq_df <- analysis$results$freqTable$asDF()
  expect_equal(freq_df$category, c("Low", "Medium", "High"))
  
  # Custom labels
  options <- create_options(
    var = "x",
    method = "quantile",
    nbins = 2,
    labels = "custom",
    customlabels = "Group A, Group B",
    addtodata = TRUE
  )
  
  analysis <- categorizeClass$new(options = options, data = data)
  analysis$run()
  
  freq_df <- analysis$results$freqTable$asDF()
  expect_equal(freq_df$category, c("Group A", "Group B"))
})
