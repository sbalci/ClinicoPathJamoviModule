
# Verification Script for dataquality Function

# 1. Mocking jmvcore Environment ------------------------------------------

library(R6)
library(testthat)
library(visdat)
library(ggplot2)

# Mock Option Classes
Option <- R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value = NULL) {
            self$name <- name
            self$value <- value
        }
    )
)

OptionData <- R6Class("OptionData", inherit = Option)
OptionVariables <- R6Class("OptionVariables", inherit = Option)
OptionBool <- R6Class("OptionBool", inherit = Option)
OptionNumber <- R6Class("OptionNumber", inherit = Option)

# Mock Html Class
Html <- R6Class("Html",
    public = list(
        name = NULL,
        content = NULL,
        initialize = function(name) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock Image Class
Image <- R6Class("Image",
    public = list(
        name = NULL,
        state = NULL,
        visible = TRUE,
        initialize = function(name) {
            self$name <- name
        },
        setState = function(state) {
            self$state <- state
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        todo = NULL,
        text = NULL,
        plotDataOverview = NULL,
        plotMissingPatterns = NULL,
        plotDataTypes = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$text <- Html$new("text")
            self$plotDataOverview <- Image$new("plotDataOverview")
            self$plotMissingPatterns <- Image$new("plotMissingPatterns")
            self$plotDataTypes <- Image$new("plotDataTypes")
        }
    )
)

# Mock Base Class
dataqualityBase <- R6Class("dataqualityBase",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- Results$new()
        },
        run = function() {
            private$.run()
        }
    )
)

# Load the function code
source("R/dataquality.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(vars = NULL, 
                          check_duplicates = FALSE, 
                          check_missing = FALSE, 
                          complete_cases_only = FALSE,
                          plot_data_overview = FALSE,
                          plot_missing_patterns = FALSE,
                          plot_data_types = FALSE,
                          missing_threshold_visual = 10) {
    
    list(
        vars = vars,
        check_duplicates = check_duplicates,
        check_missing = check_missing,
        complete_cases_only = complete_cases_only,
        plot_data_overview = plot_data_overview,
        plot_missing_patterns = plot_missing_patterns,
        plot_data_types = plot_data_types,
        missing_threshold_visual = missing_threshold_visual
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Duplicate Detection (Rows) Works", {
    # Create data with duplicate rows
    data <- data.frame(
        ID = c(1, 2, 3, 1),
        Value = c("A", "B", "C", "A")
    )
    
    options <- create_options(vars = c("ID", "Value"), check_duplicates = TRUE, complete_cases_only = TRUE)
    analysis <- dataqualityClass$new(options, data)
    analysis$run()
    
    # Check output for duplicate row count
    expect_match(analysis$results$text$content, "Duplicate Row Analysis")
    expect_match(analysis$results$text$content, "Duplicate rows: 1")
})

test_that("Duplicate Detection (Values) Works", {
    data <- data.frame(
        ID = c(1, 2, 3, 1),
        Value = c("A", "B", "C", "A")
    )
    
    options <- create_options(vars = c("ID", "Value"), check_duplicates = TRUE, complete_cases_only = FALSE)
    analysis <- dataqualityClass$new(options, data)
    analysis$run()
    
    # Check output for duplicate value count per variable
    expect_match(analysis$results$text$content, "Duplicate Value Analysis")
    expect_match(analysis$results$text$content, "ID: Unique: 3, Duplicates: 1")
})

test_that("Missing Value Analysis Works", {
    data <- data.frame(
        ID = c(1, 2, 3, 4),
        Value = c("A", NA, "C", NA)
    )
    
    options <- create_options(vars = c("ID", "Value"), check_missing = TRUE)
    analysis <- dataqualityClass$new(options, data)
    analysis$run()
    
    # Check output for missing value count
    expect_match(analysis$results$text$content, "Missing Value Analysis")
    expect_match(analysis$results$text$content, "Value: Missing: 2/4 \\(50%\\)")
})

test_that("Data Completeness Works", {
    data <- data.frame(
        ID = c(1, 2, 3, 4),
        Value = c("A", NA, "C", NA)
    )
    
    options <- create_options(vars = c("ID", "Value"))
    analysis <- dataqualityClass$new(options, data)
    analysis$run()
    
    # Check output for complete cases
    expect_match(analysis$results$text$content, "Data Completeness")
    expect_match(analysis$results$text$content, "Complete cases: 2/4 \\(50%\\)")
})

test_that("Visual Analysis (visdat) Works", {
    data <- data.frame(
        ID = c(1, 2, 3, 4),
        Value = c("A", NA, "C", NA)
    )
    
    options <- create_options(vars = c("ID", "Value"), 
                              plot_data_overview = TRUE, 
                              plot_missing_patterns = TRUE, 
                              plot_data_types = TRUE)
    analysis <- dataqualityClass$new(options, data)
    analysis$run()
    
    # Check if plot states are set
    expect_true(!is.null(analysis$results$plotDataOverview$state))
    expect_true(!is.null(analysis$results$plotMissingPatterns$state))
    expect_true(!is.null(analysis$results$plotDataTypes$state))
    
    # Check if text output contains visual analysis summary
    expect_match(analysis$results$text$content, "Visual Data Exploration")
})

print("All tests completed successfully!")
