
# Verification Script for jjwithinstats
# This script mocks jmvcore and tests the jjwithinstats function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggstatsplot", quietly = TRUE)) install.packages("ggstatsplot")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")

library(R6)
library(ggplot2)
library(tidyr)
library(dplyr)
library(glue)
library(digest)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    initialize = function(...) {
      args <- list(...)
      for (name in names(args)) {
        self[[name]] <- args[[name]]
      }
    },
    get = function(name) {
      return(self[[name]])
    },
    set = function(name, value) {
      self[[name]] <- value
    }
  )
)

# Mock Table class
Table <- R6Class("Table",
  public = list(
    rows = list(),
    columns = list(),
    visible = TRUE,
    initialize = function() {
      self$rows <- list()
    },
    addRow = function(rowKey, values) {
      self$rows[[length(self$rows) + 1]] <- values
    },
    setVisible = function(visible) {
      self$visible <- visible
    }
  )
)

# Mock Image class
Image <- R6Class("Image",
  public = list(
    plot = NULL,
    width = 600,
    height = 450,
    visible = TRUE,
    initialize = function() {},
    setSize = function(width, height) {
      self$width <- width
      self$height <- height
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    plotFun = NULL
  )
)

# Mock Html class
Html <- R6Class("Html",
  public = list(
    content = "",
    visible = TRUE,
    initialize = function() {},
    setContent = function(content) {
      self$content <- content
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    state = "" # For appending content
  )
)

# Mock Results class
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    plot = NULL,
    interpretation = NULL,
    explanations = NULL,
    summary = NULL,
    warnings = NULL,
    initialize = function() {
      self$todo <- Html$new()
      self$plot <- Image$new()
      self$interpretation <- Html$new()
      self$explanations <- Html$new()
      self$summary <- Html$new()
      self$warnings <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
jjwithinstatsBase <- R6Class("jjwithinstatsBase",
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
    .checkpoint = function(flush = TRUE) {
      # Mock checkpoint
    }
  )
)

# Helper functions to replace jmvcore calls
toNumeric <- function(x) {
  as.numeric(as.character(x))
}

naOmit <- function(x) {
  na.omit(x)
}

# Mock localization function
. <- function(text) {
  return(text)
}


# 2. Source the function ----
source_file <- "R/jjwithinstats.b.R"
file_content <- readLines(source_file)

# Modify file content to handle R6 class definition and jmvcore calls

# 1. Unlock R6 class and remove conditional check
start_line <- grep("jjwithinstatsClass <- if", file_content)
if (length(start_line) > 0) {
    file_content[start_line] <- "jjwithinstatsClass <- R6::R6Class(lock_objects = FALSE,"
}

# 2. Replace jmvcore:: calls
file_content <- gsub("jmvcore::toNumeric", "toNumeric", file_content)
file_content <- gsub("jmvcore::naOmit", "naOmit", file_content)

# Write to temp file and source
temp_file <- tempfile(fileext = ".R")
writeLines(file_content, temp_file)
source(temp_file)

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list) {
  cat("\n--- Test:", test_name, "---\n")
  
  opts <- Options$new()
  for (n in names(options_list)) {
    opts$set(n, options_list[[n]])
  }
  
  analysis <- jjwithinstatsClass$new(options = opts, data = data)
  
  if (exists(".init", envir = analysis)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Check plot generation
    cat("Generating plot...\n")
    # We need to mock ggstatsplot::ggwithinstats if it's not available or if we want to avoid dependency issues
    # But for verification, we ideally want to run it.
    # If ggstatsplot is not installed, this will fail.
    
    if (requireNamespace("ggstatsplot", quietly = TRUE)) {
        plot_res <- analysis$.__enclos_env__$private$.plot(image = NULL, ggtheme = ggplot2::theme_bw(), theme = NULL)
        if (isTRUE(plot_res)) {
            cat("Plot generated successfully.\n")
        } else {
            cat("Plot generation returned FALSE or NULL.\n")
        }
    } else {
        cat("Skipping plot generation (ggstatsplot not installed).\n")
    }
    
    return(analysis)
    
  # }, error = function(e) {
  #   cat("ERROR:", conditionMessage(e), "\n")
  #   traceback()
  #   return(NULL)
  # })
}

# 4. Run Tests ----

# Prepare Data
data(iris)
# Create wide format data (simulating repeated measures)
iris_wide <- data.frame(
    Subject = 1:50,
    Time1 = iris$Sepal.Length[1:50],
    Time2 = iris$Sepal.Width[1:50] * 2.5, # Scale up to make comparable
    Time3 = iris$Petal.Length[1:50] + 2
)

# Default options
default_options <- list(
    dep1 = "Time1",
    dep2 = "Time2",
    dep3 = "Time3",
    dep4 = NULL,
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    pairwisedisplay = "significant",
    padjustmethod = "holm",
    effsizetype = "unbiased",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    pointpath = TRUE,
    centralitypath = FALSE,
    violin = TRUE,
    boxplot = TRUE,
    point = FALSE,
    mytitle = "Test Plot",
    xtitle = "Time",
    ytitle = "Value",
    originaltheme = FALSE,
    resultssubtitle = TRUE,
    bfmessage = FALSE,
    conflevel = 0.95,
    k = 2,
    plotwidth = 650,
    plotheight = 450,
    clinicalpreset = "custom",
    showExplanations = TRUE,
    addGGPubrPlot = FALSE
)

# Test 1: Basic Functionality
res1 <- run_test("Basic Functionality", iris_wide, default_options)

# Test 2: Nonparametric
options_2 <- default_options
options_2$typestatistics <- "nonparametric"
options_2$centralitytype <- "nonparametric"
res2 <- run_test("Nonparametric Analysis", iris_wide, options_2)

# Test 3: Clinical Preset - Biomarker
options_3 <- default_options
options_3$clinicalpreset <- "biomarker"
# Note: Preset logic in .init() or .run() might modify messages or options
res3 <- run_test("Clinical Preset: Biomarker", iris_wide, options_3)
if (!is.null(res3)) {
    cat("Todo content (checking for preset messages):\n")
    print(res3$results$todo$content)
}

# Test 4: Error Handling - Missing Data
iris_missing <- iris_wide
iris_missing$Time1[1] <- NA
res4 <- run_test("Error Handling: Missing Data", iris_missing, default_options)
# Should warn or fail gracefully. The code removes NA values.
# If it removes NA, it should run with remaining data.
if (!is.null(res4)) {
    cat("Run successful with missing data (NAs omitted).\n")
}

# Test 5: Error Handling - Insufficient Variables
options_5 <- default_options
options_5$dep2 <- NULL
res5 <- run_test("Error Handling: Insufficient Variables", iris_wide, options_5)
# Should return early or show welcome message
if (!is.null(res5)) {
    cat("Todo content (should be welcome message):\n")
    # print(res5$results$todo$content) 
    if (grepl("Welcome to ClinicoPath", res5$results$todo$content)) {
        cat("VERIFIED: Welcome message shown for insufficient variables.\n")
    }
}

# Test 6: ggpubr Plot
options_6 <- default_options
options_6$addGGPubrPlot <- TRUE
options_6$ggpubrPlotType <- "paired"
res6 <- run_test("ggpubr Plot", iris_wide, options_6)
if (!is.null(res6) && requireNamespace("ggpubr", quietly = TRUE)) {
    # Manually trigger ggpubr plot generation since run_test only triggers main plot
    cat("Generating ggpubr plot...\n")
    tryCatch({
        res6$.__enclos_env__$private$.plotGGPubr(image = NULL)
        cat("ggpubr plot generated successfully.\n")
    }, error = function(e) {
        cat("ERROR in ggpubr plot:", conditionMessage(e), "\n")
    })
}
