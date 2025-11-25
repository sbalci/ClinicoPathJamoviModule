
# Verification script for jjsegmentedtotalbar table
# This script mocks jmvcore and tests the jjsegmentedtotalbar function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")

library(R6)
library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    x_var = NULL,
    y_var = NULL,
    fill_var = NULL,
    facet_var = NULL,
    plot_width = 6,
    plot_height = 4,
    show_ggplot2_plot = TRUE,
    show_ggsegmented_plot = FALSE,
    show_statistical_tests = FALSE,
    analysis_preset = "custom",
    showExplanations = FALSE,
    sort_categories = "none",
    color_palette = "viridis",
    chart_style = "clean",
    plot_title = "",
    x_title = "",
    y_title = "",
    legend_title = "",
    legend_position = "right",
    orientation = "vertical",
    bar_width = 0.9,
    show_percentages = TRUE,
    initialize = function(...) {
      args <- list(...)
      for (name in names(args)) {
        self[[name]] <- args[[name]]
      }
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

# Mock Table
Table <- R6Class("Table",
  public = list(
    rows = list(),
    visible = TRUE,
    initialize = function(...) {},
    addRow = function(rowKey, values) {
      self$rows[[as.character(rowKey)]] <- values
    },
    setRow = function(rowNo, values) {
      self$rows[[as.character(rowNo)]] <- values
    },
    setVisible = function(visible) {
      self$visible <- visible
    }
  )
)

# Mock Results
Results <- R6Class("Results",
  public = list(
    instructions = NULL,
    plot = NULL,
    plot_ggsegmented = NULL,
    summary = NULL,
    composition_table = NULL,
    detailed_stats = NULL,
    interpretation = NULL,
    clinical_summary = NULL,
    statistical_tests = NULL,
    preset_guidance = NULL,
    explanations = NULL,
    initialize = function() {
      self$instructions <- Html$new()
      self$plot <- Image$new()
      self$plot_ggsegmented <- Image$new()
      self$summary <- Table$new()
      self$composition_table <- Table$new()
      self$detailed_stats <- Table$new()
      self$interpretation <- Html$new()
      self$clinical_summary <- Html$new()
      self$statistical_tests <- Table$new()
      self$preset_guidance <- Html$new()
      self$explanations <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
jjsegmentedtotalbarBase <- R6Class("jjsegmentedtotalbarBase",
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
    .checkpoint = function() {}
  )
)

# Mock . function for localization
. <- function(x) x

# 2. Source the function ----
source_file <- "R/jjsegmentedtotalbar.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("jjsegmentedtotalbarClass <-", file_content)
if (length(start_line_idx) > 0) {
  file_content[start_line_idx] <- "jjsegmentedtotalbarClass <- R6::R6Class("
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
    options[[name]] <- options_list[[name]]
  }
  
  analysis <- jjsegmentedtotalbarClass$new(options = options, data = data)
  
  # Initialize
  if (!is.null(analysis$.__enclos_env__$private$.init)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # Run
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Check composition table
    cat("Checking Composition Table...\n")
    rows <- analysis$results$composition_table$rows
    if (length(rows) > 0) {
        df <- do.call(rbind, lapply(rows, as.data.frame))
        print(df)
        
        # Verify percentages
        cat("\nVerifying Percentages:\n")
        # Check group percentages sum to 1 (approx) per category
        group_sums <- df %>%
            dplyr::group_by(category) %>%
            dplyr::summarise(sum_pct = sum(percentage), .groups = 'drop')
        print(group_sums)
        
        # Check overall percentages sum to 1 (approx) total
        overall_sum <- sum(df$overall_percentage)
        cat(paste("Overall Percentage Sum:", overall_sum, "\n"))
        
    } else {
        cat("Composition Table is empty.\n")
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
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
# Add a count variable (just 1s for raw data)
mtcars$count <- 1

# Test 1: Basic Functionality
cat("Running Test 1: Basic Functionality\n")
options_1 <- list(
    x_var = "cyl",
    y_var = "count",
    fill_var = "am"
)
res1 <- run_test("Basic Functionality", mtcars, options_1)

