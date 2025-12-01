
# Verification script for jjsegmentedtotalbar
# This script mocks the jamovi environment to test the jjsegmentedtotalbar function

# 1. Define Mock Classes and Functions
# ------------------------------------------------------------------------------

# Mock jmvcore R6 classes
if (!requireNamespace("jmvcore", quietly = TRUE)) {
  # Define minimal mock if jmvcore is not available
  library(R6)
  
  Option <- R6Class("Option",
    public = list(
      name = NULL,
      value = NULL,
      initialize = function(name, value = NULL, ...) {
        self$name <- name
        self$value <- value
      }
    )
  )
  
  OptionList <- R6Class("OptionList", inherit = Option)
  OptionVariable <- R6Class("OptionVariable", inherit = Option)
  OptionBool <- R6Class("OptionBool", inherit = Option)
  OptionNumber <- R6Class("OptionNumber", inherit = Option)
  OptionString <- R6Class("OptionString", inherit = Option)
  
  Analysis <- R6Class("Analysis",
    public = list(
      options = NULL,
      data = NULL,
      results = NULL,
      initialize = function(options, data = NULL, results = NULL, ...) {
        self$options <- options
        self$data <- data
        self$results <- results
      },
      run = function() {
        private$.run()
      }
    )
  )
  
  Group <- R6Class("Group",
    public = list(
      items = list(),
      add = function(item) {
        self$items[[item$name]] <- item
      },
      setVisible = function(visible) {
        # Mock method
      }
    )
  )
  
  Html <- R6Class("Html",
    public = list(
      name = NULL,
      content = NULL,
      visible = TRUE,
      initialize = function(options, name, title, visible = TRUE) {
        self$name <- name
        self$visible <- visible
      },
      setContent = function(content) {
        self$content <- content
        # print(paste("Content set for", self$name))
      },
      setVisible = function(visible) {
        self$visible <- visible
      }
    )
  )
  
  Image <- R6Class("Image",
    public = list(
      name = NULL,
      visible = TRUE,
      plot = NULL,
      width = NULL,
      height = NULL,
      renderFun = NULL,
      initialize = function(options, name, title, width, height, renderFun, visible = TRUE, ...) {
        self$name <- name
        self$width <- width
        self$height <- height
        self$renderFun <- renderFun
        self$visible <- visible
      },
      setVisible = function(visible) {
        self$visible <- visible
      },
      setSize = function(width, height) {
        self$width <- width
        self$height <- height
      },
      setState = function(plot) {
        self$plot <- plot
      }
    )
  )
  
  Table <- R6Class("Table",
    public = list(
      name = NULL,
      rows = list(),
      columns = list(),
      visible = TRUE,
      initialize = function(options, name, title, rows = 0, columns = list(), visible = TRUE, ...) {
        self$name <- name
        self$columns <- columns
        self$visible <- visible
      },
      addRow = function(rowKey, values) {
        self$rows[[as.character(rowKey)]] <- values
      },
      setRow = function(rowNo, values) {
        self$rows[[as.character(rowNo)]] <- values
      },
      setVisible = function(visible) {
        self$visible <- visible
      },
      asDF = function() {
        # Convert rows to data frame for inspection
        if (length(self$rows) == 0) return(data.frame())
        do.call(rbind, lapply(self$rows, as.data.frame))
      }
    )
  )
  
  # Mock jmvcore environment
  jmvcore <- list(
    Option = Option,
    OptionList = OptionList,
    OptionVariable = OptionVariable,
    OptionBool = OptionBool,
    OptionNumber = OptionNumber,
    OptionString = OptionString,
    Analysis = Analysis,
    Group = Group,
    Html = Html,
    Image = Image,
    Table = Table
  )
} else {
  library(jmvcore)
}

library(magrittr)
library(dplyr)
library(ggplot2)

# Mock translation function
. <- function(text) text

# 2. Source the Function Code
# ------------------------------------------------------------------------------

# Function to source with mock replacements
source_mocked <- function(file) {
  lines <- readLines(file)
  # Replace jmvcore:: with jmvcore$ (if using local mock)
  if (!requireNamespace("jmvcore", quietly = TRUE)) {
    lines <- gsub("jmvcore::", "jmvcore$", lines)
  }
  # Handle requireNamespace check
  lines <- gsub('requireNamespace\\("jmvcore", quietly=TRUE\\)', 'TRUE', lines)
  
  eval(parse(text = lines), envir = .GlobalEnv)
}

# Source the files
source_mocked("R/jjsegmentedtotalbar.h.R")
source_mocked("R/jjsegmentedtotalbar.b.R")

# 3. Create Test Data
# ------------------------------------------------------------------------------

set.seed(123)
n <- 200
test_data <- data.frame(
  Group = sample(c("A", "B", "C"), n, replace = TRUE),
  Subgroup = sample(c("X", "Y", "Z"), n, replace = TRUE),
  Value = runif(n, 10, 100),
  Facet = sample(c("F1", "F2"), n, replace = TRUE)
)

# 4. Define Test Cases
# ------------------------------------------------------------------------------

run_test <- function(test_name, options_list) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  # Initialize options
  options <- jjsegmentedtotalbarOptions$new(
    x_var = options_list$x_var,
    y_var = options_list$y_var,
    fill_var = options_list$fill_var,
    facet_var = options_list$facet_var,
    show_ggplot2_plot = options_list$show_ggplot2_plot %||% FALSE,
    show_ggsegmented_plot = options_list$show_ggsegmented_plot %||% FALSE,
    chart_style = options_list$chart_style %||% "clinical",
    color_palette = options_list$color_palette %||% "clinical",
    show_percentages = options_list$show_percentages %||% TRUE,
    show_counts = options_list$show_counts %||% FALSE,
    sort_categories = options_list$sort_categories %||% "none",
    show_statistical_tests = options_list$show_statistical_tests %||% FALSE,
    analysis_preset = options_list$analysis_preset %||% "custom"
  )
  
  # Initialize analysis
  analysis <- jjsegmentedtotalbarClass$new(
    options = options,
    data = test_data
  )
  
  # Run analysis
  tryCatch({
    analysis$run()
    cat("Run complete.\n")
    
    # Check results
    if (!is.null(analysis$results$summary)) {
      summary_df <- as.data.frame(analysis$results$summary)
      cat("Summary table rows: ", nrow(summary_df), "\n")
      print(summary_df)
    }
    
    if (!is.null(analysis$results$composition_table)) {
      comp_df <- as.data.frame(analysis$results$composition_table)
      cat("Composition table rows: ", nrow(comp_df), "\n")
      
      # Verify percentages sum to 100 (approx) per category
      # Need to parse percentage string "XX.X%" to numeric
      if (nrow(comp_df) > 0) {
        comp_df$pct_val <- as.numeric(sub("%", "", comp_df$percentage))
        
        # Group by category (and facet if present, but table doesn't show facet column easily in this flat view?)
        # The table has 'category' column.
        # If faceted, 'category' might be repeated or unique?
        # In .updateComposition, it adds rows.
        # Let's just check if sums are close to 100.
        
        sums <- comp_df %>% 
          group_by(category) %>% 
          summarise(total_pct = sum(pct_val), .groups = 'drop')
        
        cat("Percentage sums per category:\n")
        print(sums)
        
        # Check if faceting is active
        facet_var <- analysis$options$facet_var
        
        # Values are proportions (0-1), so sum should be 1 (or integer if faceted)
        # If faceted, we expect sum to be integer (number of facets)
        
        if (!is.null(facet_var) && facet_var != "") {
           cat("Faceting is active. Expecting sums to be integers (number of facets).\n")
           is_valid <- all(abs(sums$total_pct %% 1) < 0.01 | abs(sums$total_pct %% 1 - 1) < 0.01)
        } else {
           is_valid <- all(abs(sums$total_pct - 1) < 0.01)
        }

        if (!is_valid) {
           cat("WARNING: Some categories do not sum to expected 1 (or integer)!\n")
        } else {
           cat("VERIFIED: Categories sum to expected values.\n")
        }
      }
    }
    
    # Check plots
    if (options_list$show_ggplot2_plot %||% FALSE) {
      cat("Generating ggplot2 plot...\n")
      # Manually call .plot since jamovi isn't doing it
      # We need to access the private method
      plot_obj <- analysis$.__enclos_env__$private$.plot(image = NULL, ggtheme = NULL, theme = NULL)
      if (!is.null(plot_obj)) {
        cat("ggplot2 plot generated successfully.\n")
        cat("Is ggplot object:", inherits(plot_obj, "ggplot"), "\n")
        # print(plot_obj) # Avoid printing to prevent font errors in this environment
      } else {
        cat("ggplot2 plot is NULL.\n")
      }
    }
    
    if (options_list$show_ggsegmented_plot %||% FALSE) {
      cat("Generating ggsegmented plot...\n")
      # Note: .plot_ggsegmented might not be defined in the .b.R file yet?
      # Let's check if it exists
      if (exists(".plot_ggsegmented", envir = analysis$.__enclos_env__$private)) {
         plot_obj <- analysis$.__enclos_env__$private$.plot_ggsegmented(image = NULL, ggtheme = NULL, theme = NULL)
         if (!is.null(plot_obj)) {
           cat("ggsegmented plot generated successfully.\n")
           cat("Is ggplot object:", inherits(plot_obj, "ggplot"), "\n")
           # print(plot_obj)
         } else {
           cat("ggsegmented plot is NULL.\n")
         }
      } else {
        cat("Method .plot_ggsegmented not found in private scope.\n")
      }
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    traceback()
  })
}

# Helper for default values
`%||%` <- function(a, b) if (is.null(a)) b else a

# 5. Run Tests
# ------------------------------------------------------------------------------

# Sanity Check: Can we create a simple ggplot?
cat("\n--- Sanity Check: Simple ggplot ---\n")
tryCatch({
  df_sanity <- data.frame(x=factor(c("A","A","B","B")), fill=factor(c("1","2","1","2")), value=c(10,20,30,40))
  p <- ggplot(df_sanity, aes(x=x, y=value, fill=fill)) + 
    geom_col(position="fill") + 
    scale_fill_manual(values=c("red", "blue")) +
    theme_bw() +
    theme(text = element_text(face = "bold"))
  cat("Simple ggplot created successfully.\n")
  cat("Is ggplot object:", inherits(p, "ggplot"), "\n")
}, error = function(e) {
  cat("Sanity check failed:", e$message, "\n")
})

# Test 1: Basic Functionality
run_test("Basic Functionality", list(
  x_var = "Group",
  y_var = "Value",
  fill_var = "Subgroup",
  show_ggplot2_plot = TRUE
))

# Test 2: With Faceting
run_test("With Faceting", list(
  x_var = "Group",
  y_var = "Value",
  fill_var = "Subgroup",
  facet_var = "Facet",
  show_ggplot2_plot = TRUE
))

# Test 3: Statistical Tests
run_test("Statistical Tests", list(
  x_var = "Group",
  y_var = "Value",
  fill_var = "Subgroup",
  show_statistical_tests = TRUE
))

# Test 4: Sorting
run_test("Sorting by Total", list(
  x_var = "Group",
  y_var = "Value",
  fill_var = "Subgroup",
  sort_categories = "total",
  show_ggplot2_plot = TRUE
))

# Test 5: Clinical Preset
run_test("Clinical Preset - Treatment Response", list(
  x_var = "Group",
  y_var = "Value",
  fill_var = "Subgroup",
  analysis_preset = "treatment_response",
  show_ggplot2_plot = TRUE
))
