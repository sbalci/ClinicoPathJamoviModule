
# Verification script for jwaffle function
# This script mocks jmvcore and tests the jwaffle function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("waffle", quietly = TRUE)) install.packages("waffle")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")

library(R6)
library(ggplot2)
library(dplyr)
library(waffle)
library(glue)
library(digest)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    data = NULL,
    counts = NULL,
    groups = NULL,
    facet = NULL,
    rows = 5,
    flip = FALSE,
    color_palette = "default",
    show_legend = TRUE,
    mytitle = "",
    legendtitle = "",
    showSummaries = FALSE,
    showExplanations = FALSE,
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
    width = 600,
    height = 500,
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

# Mock Results
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    plot = NULL,
    analysisSummary = NULL,
    methodExplanation = NULL,
    initialize = function() {
      self$todo <- Html$new()
      self$plot <- Image$new()
      self$analysisSummary <- Html$new()
      self$methodExplanation <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
jwaffleBase <- R6Class("jwaffleBase",
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
  )
)

# 2. Source the function ----
source_file <- "R/jwaffle.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
# We need to find the line: jwaffleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
# And replace it with: jwaffleClass <- R6::R6Class(
# Also need to handle the closing brace if it's wrapped in the if block, but here it seems it's an assignment.

# Let's try to just source the body of the class definition.
# A robust way is to replace the specific line.

# Find the line starting with "jwaffleClass <-"
start_line_idx <- grep("jwaffleClass <-", file_content)
if (length(start_line_idx) > 0) {
  file_content[start_line_idx] <- "jwaffleClass <- R6::R6Class("
}

# Write to temp file and source
temp_file <- tempfile(fileext = ".R")
writeLines(file_content, temp_file)
source(temp_file)

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  options <- Options$new()
  for (name in names(options_list)) {
    options$set(name, options_list[[name]])
  }
  
  analysis <- jwaffleClass$new(options = options, data = data)
  
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
    plot_res <- analysis$.__enclos_env__$private$.plot(image = NULL)
    if (isTRUE(plot_res)) {
        cat("Plot generated successfully.\n")
    } else {
        cat("Plot generation returned FALSE or NULL.\n")
    }
    
    # Check summaries if requested
    if (options$showSummaries) {
        cat("Checking Analysis Summary...\n")
        if (!is.null(analysis$results$analysisSummary$content)) {
            cat("Analysis Summary generated.\n")
            # print(substr(analysis$results$analysisSummary$content, 1, 200)) # Print first 200 chars
        } else {
            cat("Analysis Summary is NULL.\n")
        }
    }
    
    if (options$showExplanations) {
        cat("Checking Explanations...\n")
        if (!is.null(analysis$results$methodExplanation$content)) {
            cat("Explanations generated.\n")
        } else {
            cat("Explanations is NULL.\n")
        }
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
data(iris)
iris$Species <- as.factor(iris$Species)

# Test 1: Basic Functionality
cat("Running Test 1: Basic Functionality\n")
options_1 <- list(
    groups = "Species",
    rows = 5,
    show_legend = TRUE,
    color_palette = "default"
)
res1 <- run_test("Basic Functionality", iris, options_1)


# Test 2: Counts Variable
cat("Running Test 2: Counts Variable\n")
# Create aggregated data
agg_data <- data.frame(
    Category = c("A", "B", "C"),
    Count = c(10, 20, 30)
)
options_2 <- list(
    groups = "Category",
    counts = "Count",
    rows = 5,
    color_palette = "professional"
)
res2 <- run_test("Counts Variable", agg_data, options_2)


# Test 3: Faceting
cat("Running Test 3: Faceting\n")
# Create faceted data
facet_data <- data.frame(
    Group = rep(c("A", "B"), each = 20),
    Subgroup = rep(c("X", "Y"), 20),
    Value = rnorm(40)
)
options_3 <- list(
    groups = "Group",
    facet = "Subgroup",
    rows = 5,
    color_palette = "journal",
    mytitle = "Faceted Waffle Chart"
)
res3 <- run_test("Faceting", facet_data, options_3)


# Test 4: Summaries and Explanations
cat("Running Test 4: Summaries and Explanations\n")
options_4 <- list(
    groups = "Species",
    rows = 5,
    showSummaries = TRUE,
    showExplanations = TRUE
)
res4 <- run_test("Summaries and Explanations", iris, options_4)


# Test 5: Error Handling - Missing Group
cat("Running Test 5: Error Handling - Missing Group\n")
options_5 <- list(
    groups = NULL
)
res5 <- run_test("Error Handling: Missing Group", iris, options_5)
if (!is.null(res5)) {
    if (grepl("Welcome to ClinicoPath", res5$results$todo$content)) {
        cat("VERIFIED: Welcome message shown for missing group.\n")
    }
}

# Test 6: Error Handling - Invalid Data Type
cat("Running Test 6: Error Handling - Invalid Data Type\n")
# Using a numeric variable as group (should fail or warn)
options_6 <- list(
    groups = "Sepal.Length"
)
# Note: The code checks for factor/character/logical. Numeric might fail validation.
res6 <- run_test("Error Handling: Invalid Data Type", iris, options_6)

