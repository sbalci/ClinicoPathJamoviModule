
# Debug Script for outlierdetection Function

library(R6)

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
OptionList <- R6Class("OptionList", inherit = Option)
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
        interpretation = NULL,
        outlier_table = NULL,
        method_comparison = NULL,
        exclusion_summary = NULL,
        plot = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$interpretation <- Html$new("interpretation")
            self$outlier_table <- Html$new("outlier_table")
            self$method_comparison <- Html$new("method_comparison")
            self$exclusion_summary <- Html$new("exclusion_summary")
            self$plot <- Image$new("plot")
        }
    )
)

# Mock Base Class
outlierdetectionBase <- R6Class("outlierdetectionBase",
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
source("R/outlierdetection.b.R")

create_options <- function(vars = NULL, 
                          method_category = "univariate",
                          univariate_methods = "zscore_robust",
                          multivariate_methods = "mahalanobis",
                          composite_threshold = 0.5,
                          zscore_threshold = 3.29,
                          iqr_multiplier = 1.7,
                          confidence_level = 0.999,
                          show_outlier_table = TRUE,
                          show_method_comparison = FALSE,
                          show_exclusion_summary = FALSE,
                          show_visualization = FALSE,
                          show_interpretation = FALSE) {
    
    list(
        vars = vars,
        method_category = method_category,
        univariate_methods = univariate_methods,
        multivariate_methods = multivariate_methods,
        composite_threshold = composite_threshold,
        zscore_threshold = zscore_threshold,
        iqr_multiplier = iqr_multiplier,
        confidence_level = confidence_level,
        show_outlier_table = show_outlier_table,
        show_method_comparison = show_method_comparison,
        show_exclusion_summary = show_exclusion_summary,
        show_visualization = show_visualization,
        show_interpretation = show_interpretation
    )
}

print("Running Multivariate Debug...")

data <- data.frame(
    X = c(1, 2, 3, 100, 5, 6, 7, 8, 9, 10),
    Y = c(1, 3, 2, 100, 4, 7, 5, 9, 8, 11)
)

options <- create_options(vars = c("X", "Y"), 
                          method_category = "multivariate", 
                          multivariate_methods = "mahalanobis_robust",
                          show_outlier_table = TRUE)

print(paste("Option multivariate_methods:", options$multivariate_methods))

analysis <- outlierdetectionClass$new(options, data)
analysis$run()

if (is.null(analysis$results$outlier_table$content)) {
    print("Outlier Table is NULL. Checking Interpretation for Errors:")
    print(analysis$results$interpretation$content)
} else {
    print("Multivariate Output Content (First 500 chars):")
    print(substr(analysis$results$outlier_table$content, 1, 500))
}

print("Debug Complete.")
