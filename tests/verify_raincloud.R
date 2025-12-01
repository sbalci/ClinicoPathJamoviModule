
# Verification script for raincloud function
# This script mocks jmvcore and tests the raincloud function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggdist", quietly = TRUE)) install.packages("ggdist")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("moments", quietly = TRUE)) install.packages("moments")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(R6)
library(ggplot2)
library(dplyr)
library(ggdist)

# Define Mocks String
mocks_string <- '
library(R6)
library(ggplot2)
library(dplyr)
library(ggdist)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    data = NULL,
    dep_var = NULL,
    group_var = NULL,
    facet_var = NULL,
    color_var = NULL,
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    dots_side = "left",
    violin_width = 0.7,
    box_width = 0.2,
    dots_size = 1.2,
    alpha_violin = 0.7,
    alpha_dots = 0.8,
    orientation = "horizontal",
    color_palette = "clinical",
    plot_theme = "clinical",
    plot_title = "Raincloud Plot",
    x_label = "",
    y_label = "",
    show_statistics = TRUE,
    show_outliers = FALSE,
    outlier_method = "iqr",
    normality_test = FALSE,
    comparison_test = FALSE,
    comparison_method = "auto",
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
    height = 450,
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
    statistics = NULL,
    outliers = NULL,
    normality = NULL,
    comparison = NULL,
    interpretation = NULL,
    initialize = function() {
      self$todo <- Html$new()
      self$plot <- Image$new()
      self$statistics <- Html$new()
      self$outliers <- Html$new()
      self$normality <- Html$new()
      self$comparison <- Html$new()
      self$interpretation <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
raincloudBase <- R6Class("raincloudBase",
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
'

# 2. Source the function ----
source_file <- "R/raincloud.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("raincloudClass <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found raincloudClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "raincloudClass <- R6::R6Class("
} else {
  stop("Could not find raincloudClass definition to replace!")
}

# Combine mocks and file content
full_content <- c(mocks_string, file_content)

# Write to temp file and source
temp_file <- tempfile(fileext = ".R")
writeLines(full_content, temp_file)
source(temp_file)

# Ensure raincloudClass is available
if (!exists("raincloudClass")) {
    stop("raincloudClass not defined after sourcing!")
}
print("raincloudClass successfully created.")

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  # Re-instantiate Options from the sourced environment
  options <- Options$new()
  for (name in names(options_list)) {
    options$set(name, options_list[[name]])
  }
  
  analysis <- raincloudClass$new(options = options, data = data)
  
  # Initialize
  if (!is.null(analysis$.__enclos_env__$private$.init)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # Run
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Check Plot Generation
    cat("Generating Plot...\n")
    tryCatch({
        analysis$.__enclos_env__$private$.plot(image = analysis$results$plot, ggtheme = NULL, theme = NULL)
        cat("Plot generated successfully.\n")
    }, error = function(e) {
        cat("Plot generation failed:", conditionMessage(e), "\n")
    })
    
    # Check Statistics
    if (!is.null(analysis$results$statistics$content)) {
        cat("Statistics table generated.\n")
    }
    
    # Check Outliers
    if (!is.null(analysis$results$outliers$content)) {
        cat("Outlier analysis generated.\n")
    }
    
    # Check Normality
    if (!is.null(analysis$results$normality$content)) {
        cat("Normality tests generated.\n")
    }
    
    # Check Comparison
    if (!is.null(analysis$results$comparison$content)) {
        cat("Group comparisons generated.\n")
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
    dep_var = "Sepal.Length",
    group_var = "Species"
)
res1 <- run_test("Basic Functionality", iris, options_1)

# Test 2: Plot Options
cat("Running Test 2: Plot Options\n")
options_2 <- list(
    dep_var = "Sepal.Length",
    group_var = "Species",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    dots_side = "right",
    orientation = "vertical",
    color_palette = "viridis"
)
res2 <- run_test("Plot Options", iris, options_2)

# Test 3: Statistical Analysis
cat("Running Test 3: Statistical Analysis\n")
options_3 <- list(
    dep_var = "Sepal.Length",
    group_var = "Species",
    show_statistics = TRUE,
    show_outliers = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    comparison_method = "anova"
)
res3 <- run_test("Statistical Analysis", iris, options_3)

# Test 4: Faceting and Coloring
cat("Running Test 4: Faceting and Coloring\n")
# Create dummy facet variable
iris$Facet <- rep(c("A", "B"), length.out = nrow(iris))
options_4 <- list(
    dep_var = "Sepal.Length",
    group_var = "Species",
    facet_var = "Facet",
    color_var = "Species"
)
res4 <- run_test("Faceting and Coloring", iris, options_4)

# Test 5: Error Handling - Missing Variables
cat("Running Test 5: Error Handling - Missing Variables\n")
options_5 <- list(
    dep_var = NULL,
    group_var = NULL
)
res5 <- run_test("Error Handling: Missing Variables", iris, options_5)

# Test 6: Error Handling - Empty Dataset
cat("Running Test 6: Error Handling - Empty Dataset\n")
options_6 <- list(
    dep_var = "Sepal.Length",
    group_var = "Species"
)
res6 <- run_test("Error Handling: Empty Dataset", iris[0, ], options_6)
