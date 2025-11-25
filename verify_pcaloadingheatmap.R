
# Verification script for pcaloadingheatmap function
# This script mocks jmvcore and tests the pcaloadingheatmap function

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

# Define Mocks String
mocks_string <- '
library(R6)
library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    data = NULL,
    vars = NULL,
    ncomp = 4,
    cutoff = 0.5,
    center = TRUE,
    scale = TRUE,
    textvalues = TRUE,
    starvalues = FALSE,
    textsize = 2,
    plotlegend = TRUE,
    plotcutoff = TRUE,
    gradientcolor = TRUE,
    colorlow = "steelblue1",
    colormid = "white",
    colorhigh = "firebrick1",
    plotwidth = 600,
    plotheight = 450,
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
    heatmap = NULL,
    barmap = NULL,
    initialize = function() {
      self$todo <- Html$new()
      self$heatmap <- Image$new()
      self$barmap <- Image$new()
    }
  )
)

# Mock Analysis class (Base)
pcaloadingheatmapBase <- R6Class("pcaloadingheatmapBase",
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
source_file <- "R/pcaloadingheatmap.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("pcaloadingheatmapClass <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found pcaloadingheatmapClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "pcaloadingheatmapClass <- R6::R6Class("
} else {
  stop("Could not find pcaloadingheatmapClass definition to replace!")
}

# Combine mocks and file content
full_content <- c(mocks_string, file_content)

# Write to temp file and source
temp_file <- tempfile(fileext = ".R")
writeLines(full_content, temp_file)
source(temp_file)

# Ensure pcaloadingheatmapClass is available
if (!exists("pcaloadingheatmapClass")) {
    stop("pcaloadingheatmapClass not defined after sourcing!")
}
print("pcaloadingheatmapClass successfully created.")

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  # Re-instantiate Options from the sourced environment
  options <- Options$new()
  for (name in names(options_list)) {
    options$set(name, options_list[[name]])
  }
  
  analysis <- pcaloadingheatmapClass$new(options = options, data = data)
  
  # Initialize
  if (!is.null(analysis$.__enclos_env__$private$.init)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # Run
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Check PCA Results
    if (!is.null(analysis$.__enclos_env__$private$.pcaResults)) {
        cat("PCA Results stored successfully.\n")
        print(summary(analysis$.__enclos_env__$private$.pcaResults))
    } else {
        cat("PCA Results are NULL.\n")
    }
    
    # Check Heatmap Generation
    cat("Generating Heatmap...\n")
    tryCatch({
        analysis$.__enclos_env__$private$.heatmap(image = analysis$results$heatmap, ggtheme = NULL, theme = NULL)
        cat("Heatmap generated successfully.\n")
    }, error = function(e) {
        cat("Heatmap generation failed:", conditionMessage(e), "\n")
    })
    
    # Check Barmap Generation
    cat("Generating Barmap...\n")
    tryCatch({
        analysis$.__enclos_env__$private$.barmap(image = analysis$results$barmap, ggtheme = NULL, theme = NULL)
        cat("Barmap generated successfully.\n")
    }, error = function(e) {
        cat("Barmap generation failed:", conditionMessage(e), "\n")
    })
    
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
vars <- c("mpg", "disp", "hp", "drat", "wt", "qsec")

# Test 1: Basic Functionality
cat("Running Test 1: Basic Functionality\n")
options_1 <- list(
    vars = vars,
    ncomp = 3,
    cutoff = 0.5,
    center = TRUE,
    scale = TRUE
)
res1 <- run_test("Basic Functionality", mtcars, options_1)

# Test 2: Unscaled PCA
cat("Running Test 2: Unscaled PCA\n")
options_2 <- list(
    vars = vars,
    ncomp = 3,
    cutoff = 0.5,
    center = TRUE,
    scale = FALSE
)
res2 <- run_test("Unscaled PCA", mtcars, options_2)

# Test 3: Plot Options (Stars, No Gradient)
cat("Running Test 3: Plot Options\n")
options_3 <- list(
    vars = vars,
    ncomp = 3,
    cutoff = 0.6,
    textvalues = FALSE,
    starvalues = TRUE,
    gradientcolor = FALSE
)
res3 <- run_test("Plot Options", mtcars, options_3)

# Test 4: Error Handling - Missing Variables
cat("Running Test 4: Error Handling - Missing Variables\n")
options_4 <- list(
    vars = c("mpg", "disp", "MISSING_VAR")
)
res4 <- run_test("Error Handling: Missing Variables", mtcars, options_4)

# Test 5: Error Handling - Non-numeric Variables
cat("Running Test 5: Error Handling - Non-numeric Variables\n")
mtcars_bad <- mtcars
mtcars_bad$char_var <- as.character(mtcars_bad$mpg)
options_5 <- list(
    vars = c("mpg", "disp", "char_var")
)
res5 <- run_test("Error Handling: Non-numeric Variables", mtcars_bad, options_5)

# Test 6: Verify Loading Standardization Logic
cat("\n--- Test 6: Verify Loading Standardization Logic ---\n")
# Manual calculation
pca_base <- prcomp(mtcars[, vars], center = TRUE, scale. = TRUE)
loadings_base <- sweep(pca_base$rotation, 2, pca_base$sdev, `*`)
# Function calculation
loadings_func <- pcaloadingheatmap_normalized_loadings(pca_base, mtcars[, vars], scaled = TRUE)
# Compare
diff <- max(abs(loadings_base - loadings_func))
cat(paste("Max difference in standardized loadings:", diff, "\n"))
if (diff < 1e-10) {
    cat("Standardization logic verified.\n")
} else {
    cat("Standardization logic MISMATCH!\n")
}
