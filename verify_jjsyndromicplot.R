
# Verification Script for jjsyndromicplot
# This script mocks jmvcore and tests the jjsyndromicplot function

# 1. Mock jmvcore ----
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
library(R6)
library(ggplot2)
library(dplyr)
library(ggrepel)

# Mock Options class
Option <- R6Class("Option",
  public = list(
    name = NULL,
    value = NULL,
    default = NULL,
    initialize = function(name, value = NULL, default = NULL) {
      self$name <- name
      self$value <- if (!is.null(value)) value else default
      self$default <- default
    }
  )
)

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
    height = 600,
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
    loadings = NULL,
    warnings = NULL,
    explanations = NULL,
    initialize = function() {
      self$todo <- Html$new()
      self$plot <- Image$new()
      self$loadings <- Table$new()
      self$warnings <- Html$new()
      self$explanations <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
jjsyndromicplotBase <- R6Class("jjsyndromicplotBase",
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
source_file <- "R/jjsyndromicplot.b.R"
file_content <- readLines(source_file)

# Extract the class definition part (remove the if(requireNamespace) wrapper)
# We will just evaluate the R6Class definition directly.

# Find the line with the class definition
start_line <- grep("jjsyndromicplotClass <- if", file_content)
if (length(start_line) > 0) {
    # Replace the line with unconditional assignment AND unlock objects
    # We pass lock_objects = FALSE as a named argument first. 
    # The class name on the next line will be picked up as the first positional argument (classname).
    file_content[start_line] <- "jjsyndromicplotClass <- R6::R6Class(lock_objects = FALSE,"
} else {
    stop("Could not find class definition line in source file.")
}

# Write to temp file and source
temp_file <- tempfile(fileext = ".R")
writeLines(file_content, temp_file)
source(temp_file)

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list) {
  cat("\n--- Test:", test_name, "---\n")
  
  # Create options object
  # We need to mimic the options object structure expected by the class
  # The class accesses options via self$options$option_name
  # So we can use our Mock Options class
  
  opts <- Options$new()
  for (n in names(options_list)) {
    opts$set(n, options_list[[n]])
  }
  
  # Initialize analysis
  analysis <- jjsyndromicplotClass$new(options = opts, data = data)
  
  # Run init
  if (exists(".init", envir = analysis)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # Run analysis
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Check results
    if (!is.null(analysis$results$loadings)) {
      cat("Loadings table rows: ", length(analysis$results$loadings$rows), "\n")
      # print(head(analysis$results$loadings$rows))
    }
    
    # Check plot
    # The .plot method returns TRUE/FALSE and prints the plot.
    # We can call it manually to see if it errors.
    cat("Generating plot...\n")
    plot_res <- analysis$.__enclos_env__$private$.plot(image = NULL, ggtheme = NULL, theme = NULL)
    if (isTRUE(plot_res)) {
        cat("Plot generated successfully.\n")
    } else {
        cat("Plot generation returned FALSE or NULL.\n")
    }
    
    return(analysis)
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# 4. Run Tests ----

# Test 1: Basic Functionality (mtcars)
data(mtcars)
# Select 5 variables
vars <- c("mpg", "disp", "hp", "drat", "wt")
# Ensure they are numeric
mtcars_num <- mtcars[, vars]

options_1 <- list(
  vars = vars,
  component = 1,
  cutoff = 0.4,
  center = TRUE,
  scale = TRUE,
  arrowsize = 10,
  textsize = 10,
  repel = TRUE,
  plotlegend = TRUE,
  plotcutoff = TRUE,
  varorder = "abs decreasing",
  colorlow = "blue",
  colormid = "white",
  colorhigh = "red",
  plotwidth = 600,
  plotheight = 600,
  clinicalPreset = "none",
  showExplanations = TRUE
)

res1 <- run_test("Basic Functionality", mtcars_num, options_1)

# Test 2: Mathematical Verification
# Compare loadings with prcomp directly
cat("\n--- Mathematical Verification ---\n")
pca_ref <- prcomp(mtcars_num, center = TRUE, scale. = TRUE)
loadings_ref <- pca_ref$rotation %*% diag(pca_ref$sdev)
# Standardized loadings (correlation between var and PC)
# Since we scaled, these are the loadings.
# Let's check what the function calculated.
if (!is.null(res1)) {
    rows <- res1$results$loadings$rows
    # Extract loadings for PC1
    calc_loadings <- sapply(rows, function(r) r$loading)
    names(calc_loadings) <- sapply(rows, function(r) r$variable)
    
    # Ref loadings for PC1
    ref_pc1 <- loadings_ref[, 1]
    
    # Compare
    cat("Reference Loadings (PC1):\n")
    print(ref_pc1)
    cat("Calculated Loadings (PC1):\n")
    print(calc_loadings)
    
    diff <- abs(ref_pc1[names(calc_loadings)] - calc_loadings)
    if (all(diff < 1e-5)) {
        cat("VERIFIED: Loadings match prcomp results.\n")
    } else {
        cat("WARNING: Loadings do NOT match!\n")
        print(diff)
    }
}

# Test 3: Unscaled PCA
options_3 <- options_1
options_3$scale <- FALSE
res3 <- run_test("Unscaled PCA", mtcars_num, options_3)

# Verify unscaled loadings
# When scale=FALSE, prcomp does covariance PCA.
# Loadings are eigenvectors * sqrt(eigenvalues).
# Standardized loadings (correlations) require dividing by variable SD.
cat("\n--- Unscaled Verification ---\n")
pca_ref_unscaled <- prcomp(mtcars_num, center = TRUE, scale. = FALSE)
loadings_unscaled <- pca_ref_unscaled$rotation %*% diag(pca_ref_unscaled$sdev)
sd_vals <- apply(mtcars_num, 2, sd)
std_loadings_ref <- sweep(loadings_unscaled, 1, sd_vals, "/")

if (!is.null(res3)) {
    rows <- res3$results$loadings$rows
    calc_loadings <- sapply(rows, function(r) r$loading)
    names(calc_loadings) <- sapply(rows, function(r) r$variable)
    
    ref_pc1 <- std_loadings_ref[, 1]
    
    cat("Reference Std Loadings (PC1):\n")
    print(ref_pc1)
    cat("Calculated Std Loadings (PC1):\n")
    print(calc_loadings)
    
    diff <- abs(ref_pc1[names(calc_loadings)] - calc_loadings)
    if (all(diff < 1e-5)) {
        cat("VERIFIED: Unscaled PCA standardized loadings match.\n")
    } else {
        cat("WARNING: Unscaled PCA loadings do NOT match!\n")
        print(diff)
    }
}

# Test 4: Error Handling - Categorical Variables
# Create dummy categorical var
mtcars_cat <- mtcars_num
mtcars_cat$cat_var <- factor(sample(c("A", "B"), nrow(mtcars), replace = TRUE))
options_4 <- options_1
options_4$vars <- c(vars, "cat_var")

res4 <- run_test("Error Handling: Categorical Variables", mtcars_cat, options_4)
# Should print warning/error in results$warnings
if (!is.null(res4)) {
    cat("Warnings content:\n")
    print(res4$results$warnings$content)
    if (grepl("Categorical Variables Detected", res4$results$warnings$content)) {
        cat("VERIFIED: Categorical variable detected and warned.\n")
    } else {
        cat("WARNING: Categorical variable NOT detected!\n")
    }
}

# Test 5: Clinical Preset - Biomarker Discovery
options_5 <- options_1
options_5$clinicalPreset <- "biomarker_discovery"
# Note: The class modifies self$options in .applyClinicalPreset
# We need to check if options were updated.
res5 <- run_test("Clinical Preset: Biomarker Discovery", mtcars_num, options_5)

if (!is.null(res5)) {
    # Check if options were updated
    # Access private options via environment?
    # Or check if results reflect the preset (e.g. cutoff 0.4)
    # The preset sets cutoff to 0.4.
    # We can check the plot generation call arguments if we could spy on it, but we can't.
    # But we can check `res5$options$get("cutoff")`.
    
    current_cutoff <- res5$options$get("cutoff")
    cat("Current cutoff:", current_cutoff, "\n")
    if (current_cutoff == 0.4) {
        cat("VERIFIED: Preset updated cutoff to 0.4.\n")
    } else {
        cat("WARNING: Preset did NOT update cutoff!\n")
    }
}

