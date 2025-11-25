
# Verification script for lollipop function
# This script mocks jmvcore and tests the lollipop function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(R6)
library(ggplot2)
library(dplyr)

# Define Mocks String
mocks_string <- '
library(R6)
library(ggplot2)
library(dplyr)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    data = NULL,
    dep = NULL,
    group = NULL,
    useHighlight = FALSE,
    highlight = NULL,
    aggregation = "none",
    sortBy = "original",
    orientation = "vertical",
    showValues = FALSE,
    showMean = FALSE,
    colorScheme = "default",
    theme = "default",
    pointSize = 3,
    lineWidth = 1,
    lineType = "solid",
    baseline = 0,
    conditionalColor = FALSE,
    colorThreshold = 0,
    xlabel = "",
    ylabel = "",
    title = "",
    width = 800,
    height = 600,
    initialize = function(...) {
      args <- list(...)
      for (name in names(args)) {
        self[[name]] <- args[[name]]
      }
    },
    set = function(key, value) {
      self[[key]] <- value
    }
  )
)

# Mock Image
Image <- R6Class("Image",
  public = list(
    state = NULL,
    visible = TRUE,
    width = 800,
    height = 600,
    initialize = function(...) {},
    setSize = function(width, height) {
      self$width <- width
      self$height <- height
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    setState = function(state) {
      self$state <- state
    }
  )
)

# Mock Html
Html <- R6Class("Html",
  public = list(
    content = NULL,
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

# Mock Table
Table <- R6Class("Table",
  public = list(
    rows = list(),
    visible = TRUE,
    initialize = function(...) {},
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <- values
    },
    deleteRows = function() {
      self$rows <- list()
    },
    setVisible = function(visible) {
      self$visible <- visible
    }
  )
)

# Mock Results
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    summary = NULL,
    plot = NULL,
    initialize = function() {
      self$todo <- Html$new()
      self$summary <- Table$new()
      self$plot <- Image$new()
    }
  )
)

# Mock Analysis class (Base)
lollipopBase <- R6Class("lollipopBase",
  lock_objects = FALSE,
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
    .checkpoint = function(flush = TRUE) {}
  )
)

# Mock . function for localization
. <- function(x) x

# Mock jmvcore::toNumeric
if (!exists("jmvcore")) {
    jmvcore <- list()
}
jmvcore$toNumeric <- function(x) {
    as.numeric(as.character(x))
}
'

# 2. Source the function ----
source_file <- "R/lollipop.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("lollipopClass <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found lollipopClass definition at line:", start_line_idx))
  print(paste("Original line:", file_content[start_line_idx]))
  file_content[start_line_idx] <- "lollipopClass <- R6::R6Class("
  print(paste("Replaced line:", file_content[start_line_idx]))
} else {
  stop("Could not find lollipopClass definition to replace!")
}

# Replace jmvcore::toNumeric with as.numeric
file_content <- gsub("jmvcore::toNumeric", "as.numeric", file_content)

# Combine mocks and file content
full_content <- c(mocks_string, file_content)

# Write to temp file and source
temp_file <- tempfile(fileext = ".R")
writeLines(full_content, temp_file)
source(temp_file)

# Ensure lollipopClass is available
if (!exists("lollipopClass")) {
    stop("lollipopClass not defined after sourcing!")
}
if (is.null(lollipopClass)) {
    stop("lollipopClass is NULL after sourcing! Replacement failed?")
}
print("lollipopClass successfully created:")
print(lollipopClass)

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  # Re-instantiate Options from the sourced environment
  options <- Options$new()
  for (name in names(options_list)) {
    options$set(name, options_list[[name]])
  }
  
  analysis <- lollipopClass$new(options = options, data = data)
  
  # Initialize
  if (!is.null(analysis$.__enclos_env__$private$.init)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # Run
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Check plot generation
    cat("Generating plot...\n")
    if (!is.null(analysis$results$plot$state)) {
        cat("Plot state saved successfully.\n")
        analysis$.__enclos_env__$private$.plot(image = analysis$results$plot)
        cat("Plot generated successfully.\n")
    } else {
        cat("Plot state is NULL.\n")
    }
    
    # Check summary table
    cat("Checking Summary Table...\n")
    if (length(analysis$results$summary$rows) > 0) {
        cat("Summary table populated.\n")
    } else {
        cat("Summary table is empty.\n")
    }
    
    # Check clinical summary
    if (!is.null(analysis$results$todo$content)) {
        cat("Clinical summary generated.\n")
    }
    
    return(analysis)
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    traceback()
    return(NULL)
  })
}

# 4. Run Tests ----

# Prepare Data
data(mtcars)
mtcars$car_name <- rownames(mtcars)
test_data <- mtcars
test_data$cyl <- as.factor(test_data$cyl)
test_data$gear <- as.factor(test_data$gear)

# Test 1: Basic Functionality
cat("Running Test 1: Basic Functionality\n")
options_1 <- list(
    dep = "mpg",
    group = "car_name",
    aggregation = "none",
    sortBy = "original"
)
res1 <- run_test("Basic Functionality", test_data, options_1)

# Test 2: Orientation and Sorting
cat("Running Test 2: Orientation and Sorting\n")
options_2 <- list(
    dep = "mpg",
    group = "car_name",
    aggregation = "none",
    sortBy = "value_desc",
    orientation = "horizontal",
    showValues = TRUE
)
res2 <- run_test("Orientation and Sorting", test_data, options_2)

# Test 3: Aggregation
cat("Running Test 3: Aggregation\n")
options_3 <- list(
    dep = "mpg",
    group = "cyl",
    aggregation = "mean",
    showMean = TRUE
)
res3 <- run_test("Aggregation", test_data, options_3)

# Test 4: Highlighting
cat("Running Test 4: Highlighting\n")
options_4 <- list(
    dep = "mpg",
    group = "car_name",
    useHighlight = TRUE,
    highlight = "Mazda RX4",
    colorScheme = "clinical"
)
res4 <- run_test("Highlighting", test_data, options_4)

# Test 5: Conditional Coloring
cat("Running Test 5: Conditional Coloring\n")
options_5 <- list(
    dep = "mpg",
    group = "car_name",
    conditionalColor = TRUE,
    colorThreshold = 20
)
res5 <- run_test("Conditional Coloring", test_data, options_5)

# Test 6: Error Handling - Missing Variables
cat("Running Test 6: Error Handling - Missing Variables\n")
options_6 <- list(
    dep = NULL,
    group = NULL
)
res6 <- run_test("Error Handling: Missing Variables", test_data, options_6)

# Test 7: Error Handling - Invalid Data Type
cat("Running Test 7: Error Handling - Invalid Data Type\n")
test_data_bad <- test_data
test_data_bad$bad_dep <- as.character(test_data_bad$mpg)
options_7 <- list(
    dep = "bad_dep",
    group = "car_name"
)
res7 <- run_test("Error Handling: Invalid Data Type", test_data_bad, options_7)
