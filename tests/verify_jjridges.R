
# Verification Script for jjridges Function

# Load necessary libraries
library(R6)
library(ggplot2)
library(dplyr)
library(ggridges)

# Mock jmvcore environment
jmvcore <- list(
    Option = R6::R6Class("Option", public = list(initialize = function(...) {})),
    OptionVariable = R6::R6Class("OptionVariable", inherit = R6::R6Class("Option"), public = list(initialize = function(...) {}, value = NULL)),
    OptionList = R6::R6Class("OptionList", inherit = R6::R6Class("Option"), public = list(initialize = function(...) {}, value = NULL)),
    OptionNumber = R6::R6Class("OptionNumber", inherit = R6::R6Class("Option"), public = list(initialize = function(...) {}, value = NULL)),
    OptionBool = R6::R6Class("OptionBool", inherit = R6::R6Class("Option"), public = list(initialize = function(...) {}, value = NULL)),
    OptionString = R6::R6Class("OptionString", inherit = R6::R6Class("Option"), public = list(initialize = function(...) {}, value = NULL)),
    Analysis = R6::R6Class("Analysis", public = list(
        initialize = function(...) {},
        results = NULL,
        options = NULL,
        data = NULL
    )),
    Group = R6::R6Class("Group", public = list(
        initialize = function(...) {},
        add = function(...) {},
        setVisible = function(...) {},
        setContent = function(...) {},
        items = list()
    )),
    Html = R6::R6Class("Html", inherit = R6::R6Class("Group"), public = list(
        initialize = function(...) {},
        setContent = function(content) { print(paste("HTML Content Set:", substr(content, 1, 100))) },
        setVisible = function(visible) { print(paste("HTML Visible:", visible)) }
    )),
    Table = R6::R6Class("Table", inherit = R6::R6Class("Group"), public = list(
        initialize = function(...) {},
        addRow = function(...) { print("Row Added to Table") },
        setVisible = function(visible) { print(paste("Table Visible:", visible)) }
    )),
    Image = R6::R6Class("Image", inherit = R6::R6Class("Group"), public = list(
        initialize = function(...) {},
        setState = function(...) { print("Plot State Set") },
        setSize = function(...) {},
        setVisible = function(visible) { print(paste("Image Visible:", visible)) }
    )),
    toNumeric = function(x) as.numeric(x)
)

# Mock translation function
. <- function(text) text

# Source the implementation files
source("R/jjridges.h.R")
source("R/jjridges.b.R")

# Mock Data
set.seed(123)
test_data <- data.frame(
    value = c(rnorm(50, 10, 2), rnorm(50, 12, 2)),
    group = factor(rep(c("A", "B"), each = 50)),
    fill_group = factor(rep(c("X", "Y"), 50))
)

# Helper to run analysis
run_jjridges <- function(options_list) {
    print(paste("Running jjridges with options:", paste(names(options_list), collapse = ", ")))
    
    # Initialize options
    options <- jjridgesOptions$new()
    
    # Set options manually (since we can't easily use the generated initialize with defaults)
    for (opt in names(options_list)) {
        # We need to access the private fields directly or use the public active bindings if settable
        # In jmvcore, options are usually read-only active bindings, but the private fields are accessible
        # The generated code uses private$..optionName
        private_name <- paste0("..", opt)
        if (exists(private_name, envir = options$.__enclos_env__$private)) {
             options$.__enclos_env__$private[[private_name]]$value <- options_list[[opt]]
        } else {
            warning(paste("Option", opt, "not found in jjridgesOptions"))
        }
    }
    
    # Initialize analysis
    analysis <- jjridgesClass$new(
        options = options,
        data = test_data
    )
    
    # Run analysis
    # We need to access the private .run method
    analysis$.__enclos_env__$private$.run()
    
    return(analysis)
}

print("--- Test 1: Basic Functionality ---")
run_jjridges(list(
    x_var = "value",
    y_var = "group",
    plot_type = "density_ridges"
))

print("\n--- Test 2: Unimplemented Feature (Double Ridges) ---")
# This should trigger a warning or fallback
run_jjridges(list(
    x_var = "value",
    y_var = "group",
    plot_type = "double_ridges"
))

print("\n--- Test 3: Unimplemented Feature (Correlation Analysis Preset) ---")
# This should trigger a warning
run_jjridges(list(
    x_var = "value",
    y_var = "group",
    clinicalPreset = "correlation_analysis"
))

print("\n--- Test 4: Critical Fix - Robust Test (WRS2) ---")
# This checks if it falls back gracefully if WRS2 is missing (or runs if present)
run_jjridges(list(
    x_var = "value",
    y_var = "group",
    show_stats = TRUE,
    test_type = "robust"
))

print("\n--- Test 5: Critical Fix - Bayesian Test ---")
# This checks if it falls back gracefully if BayesFactor is missing
run_jjridges(list(
    x_var = "value",
    y_var = "group",
    show_stats = TRUE,
    test_type = "bayes"
))

print("\n--- Test 6: Critical Fix - Omega Squared ---")
run_jjridges(list(
    x_var = "value",
    y_var = "group",
    show_stats = TRUE,
    effsize_type = "omega"
))
