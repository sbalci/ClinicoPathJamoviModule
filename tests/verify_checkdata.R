
library(R6)
library(testthat)

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

# Mock Results class based on jamovi/checkdata.r.yaml
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    qualityText = NULL,
    missingVals = NULL,
    outliers = NULL,
    noOutliers = NULL,
    distribution = NULL,
    duplicates = NULL,
    patterns = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$qualityText <- Html$new()
      self$missingVals <- Table$new()
      self$outliers <- Table$new()
      self$noOutliers <- Html$new()
      self$distribution <- Table$new()
      self$duplicates <- Table$new()
      self$patterns <- Table$new()
    }
  )
)

# Mock checkdataBase class
checkdataBase <- R6Class("checkdataBase",
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

# Read and evaluate the checkdataClass definition
source_file <- "R/checkdata.b.R"
if (!file.exists(source_file)) {
  stop("Could not find R/checkdata.b.R")
}

# Read the file content
file_content <- readLines(source_file)

# Extract the class definition
# Filter out roxygen comments and imports
code_lines <- file_content[!grepl("^#'", file_content)]
code_text <- paste(code_lines, collapse = "\n")

# Remove the "checkdataClass <- if (requireNamespace('jmvcore')) " part
code_text <- gsub("checkdataClass <- if \\(requireNamespace\\('jmvcore'\\)\\) ", "", code_text)

# Find the start of R6::R6Class
start_idx <- grep("R6::R6Class", code_lines)[1]
# Assume the class definition goes to the end of the file
class_def_lines <- code_lines[start_idx:length(code_lines)]
class_def_text <- paste(class_def_lines, collapse = "\n")

# Assign to checkdataClass
checkdataClass <- eval(parse(text = class_def_text))


# Helper to create options list
create_options <- function(
    var = "x",
    showOutliers = TRUE,
    showDistribution = TRUE,
    showDuplicates = TRUE,
    showPatterns = TRUE
) {
  list(
    var = var,
    showOutliers = showOutliers,
    showDistribution = showDistribution,
    showDuplicates = showDuplicates,
    showPatterns = showPatterns
  )
}

# --- Tests ---

test_that("Numeric Variable Analysis", {
  set.seed(123)
  data <- data.frame(x = rnorm(100, mean = 50, sd = 10))
  
  options <- create_options(var = "x")
  analysis <- checkdataClass$new(options = options, data = data)
  analysis$run()
  
  # Check distribution table
  dist_df <- analysis$results$distribution$asDF()
  expect_equal(dist_df[dist_df$metric == "Mean", "value"], round(mean(data$x), 4))
  expect_equal(dist_df[dist_df$metric == "Median", "value"], round(median(data$x), 4))
  expect_equal(dist_df[dist_df$metric == "Standard Deviation", "value"], round(sd(data$x), 4))
  
  # Check no outliers detected (normal data)
  expect_true(analysis$results$noOutliers$visible)
  expect_false(analysis$results$outliers$visible)
})

test_that("Outlier Detection", {
  set.seed(123)
  x <- rnorm(100, mean = 50, sd = 10)
  # Add extreme outliers
  x[1] <- 500  # Extreme high
  x[2] <- -200 # Extreme low
  data <- data.frame(x = x)
  
  options <- create_options(var = "x")
  analysis <- checkdataClass$new(options = options, data = data)
  analysis$run()
  
  # Check outliers table
  expect_true(analysis$results$outliers$visible)
  outliers_df <- analysis$results$outliers$asDF()
  
  # Should detect at least the two extreme outliers
  expect_true(any(outliers_df$value == 500))
  expect_true(any(outliers_df$value == -200))
  
  # Check severity
  expect_true(any(grepl("Extreme", outliers_df$severity)))
})

test_that("Categorical Variable Analysis", {
  data <- data.frame(cat = factor(c(rep("A", 50), rep("B", 30), rep("C", 20))))
  
  options <- create_options(var = "cat")
  analysis <- checkdataClass$new(options = options, data = data)
  analysis$run()
  
  # Check distribution table
  dist_df <- analysis$results$distribution$asDF()
  expect_equal(as.numeric(dist_df[dist_df$metric == "Number of Categories", "value"]), 3)
  expect_true(grepl("A \\(50\\)", dist_df[dist_df$metric == "Modal Category", "value"]))
  
  # Check duplicates (frequencies)
  dup_df <- analysis$results$duplicates$asDF()
  expect_equal(dup_df[dup_df$value == "A", "count"], 50)
  expect_equal(dup_df[dup_df$value == "B", "count"], 30)
})

test_that("Missing Data Analysis", {
  x <- rep(1, 100)
  x[1:20] <- NA # 20% missing
  data <- data.frame(x = x)
  
  options <- create_options(var = "x")
  analysis <- checkdataClass$new(options = options, data = data)
  analysis$run()
  
  # Check missing values table
  missing_df <- analysis$results$missingVals$asDF()
  expect_equal(missing_df[missing_df$metric == "Missing Values", "value"], "20 (20.0%)")
  
  # Check quality text for missing data warning
  quality_text <- analysis$results$qualityText$content
  expect_true(grepl("MISSING DATA", quality_text))
})

test_that("Clinical Validation - Age", {
  data <- data.frame(age = c(25, 30, -5, 150)) # Negative age and >120
  
  options <- create_options(var = "age")
  analysis <- checkdataClass$new(options = options, data = data)
  analysis$run()
  
  # Check patterns table for clinical issues
  patterns_df <- analysis$results$patterns$asDF()
  expect_true(any(grepl("Negative age values", patterns_df$description)))
  expect_true(any(grepl("Age values >120", patterns_df$description)))
})

test_that("Clinical Validation - Lab Values", {
  data <- data.frame(hemoglobin = c(14, 12, 2, 25)) # Low and high Hb
  
  options <- create_options(var = "hemoglobin")
  analysis <- checkdataClass$new(options = options, data = data)
  analysis$run()
  
  patterns_df <- analysis$results$patterns$asDF()
  expect_true(any(grepl("Hemoglobin values outside typical range", patterns_df$description)))
})

test_that("Quality Grading", {
  # Grade A: Perfect data
  data_a <- data.frame(x = rnorm(100))
  options <- create_options(var = "x")
  analysis_a <- checkdataClass$new(options = options, data = data_a)
  analysis_a$run()
  expect_true(grepl("OVERALL QUALITY GRADE: A", analysis_a$results$qualityText$content))
  
  # Grade D: Poor quality (high missing, outliers, small sample)
  x_d <- c(rnorm(5), rep(NA, 6)) # >50% missing, small sample
  data_d <- data.frame(x = x_d)
  options <- create_options(var = "x")
  analysis_d <- checkdataClass$new(options = options, data = data_d)
  analysis_d$run()
  # Small sample size and missing data should lower the grade
  expect_false(grepl("OVERALL QUALITY GRADE: A", analysis_d$results$qualityText$content))
})
