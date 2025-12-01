
# Verification script for jjpubr function
# Mocks jmvcore environment and runs the analysis

if (!requireNamespace("R6", quietly = TRUE)) {
    stop("R6 package is required for verification")
}
library(R6)
library(ggplot2)
library(ggpubr)

# Mock jmvcore classes
Options <- R6Class("Options",
    public = list(
        initialize = function(...) {},
        .addOption = function(...) {}
    )
)

OptionVariable <- R6Class("OptionVariable",
    public = list(
        value = NULL,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionVariables <- R6Class("OptionVariables",
    public = list(
        value = NULL,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionList <- R6Class("OptionList",
    public = list(
        value = NULL,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionBool <- R6Class("OptionBool",
    public = list(
        value = FALSE,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionNumber <- R6Class("OptionNumber",
    public = list(
        value = 0,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionInteger <- R6Class("OptionInteger",
    public = list(
        value = 0,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionString <- R6Class("OptionString",
    public = list(
        value = "",
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

OptionLevel <- R6Class("OptionLevel",
    public = list(
        value = NULL,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)

# Mock Analysis class
Analysis <- R6Class("Analysis",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, results = NULL, data = NULL, ...) {
            self$options <- options
            self$data <- data
            self$results <- results
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .run = function() {}
    )
)

# Mock Group class
Group <- R6Class("Group",
    public = list(
        name = NULL,
        initialize = function(options, name = "", title = "", visible = TRUE, ...) {
            self$name <- name
            private$.items <- list()
        },
        add = function(item) {
            private$.items[[item$name]] <- item
        },
        get = function(name) {
            return(private$.items[[name]])
        },
        setVisible = function(...) {},
        insert = function(...) {}
    ),
    private = list(
        .items = list()
    )
)

# Mock Image class
Image <- R6Class("Image",
    public = list(
        state = NULL,
        name = NULL,
        initialize = function(options, name = "", title = "", ...) {
            self$name <- name
        },
        setState = function(state) {
            self$state <- state
        },
        setSize = function(...) {},
        setVisible = function(...) {}
    )
)

# Mock Table class
Table <- R6Class("Table",
    public = list(
        name = NULL,
        initialize = function(options, name = "", title = "", ...) {
            self$name <- name
        },
        addRow = function(...) {},
        setRow = function(...) {},
        setNote = function(...) {},
        setVisible = function(...) {}
    )
)

# Mock Html class
Html <- R6Class("Html",
    public = list(
        content = NULL,
        name = NULL,
        initialize = function(options, name = "", title = "", ...) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        },
        setVisible = function(...) {}
    )
)

# Mock Notice class
Notice <- R6Class("Notice",
    public = list(
        content = NULL,
        type = NULL,
        initialize = function(options, name, type = NULL) {
            self$type <- type
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

NoticeType <- list(
    STRONG_WARNING = "strong_warning",
    WARNING = "warning",
    INFO = "info"
)

# Mock jmvcore environment
jmvcore_env <- new.env()
jmvcore_env$Options <- Options
jmvcore_env$OptionVariable <- OptionVariable
jmvcore_env$OptionVariables <- OptionVariables
jmvcore_env$OptionList <- OptionList
jmvcore_env$OptionBool <- OptionBool
jmvcore_env$OptionNumber <- OptionNumber
jmvcore_env$OptionInteger <- OptionInteger
jmvcore_env$OptionString <- OptionString
jmvcore_env$OptionLevel <- OptionLevel
jmvcore_env$Analysis <- Analysis
jmvcore_env$Group <- Group
jmvcore_env$Image <- Image
jmvcore_env$Table <- Table
jmvcore_env$Html <- Html
jmvcore_env$Notice <- Notice
jmvcore_env$NoticeType <- NoticeType
jmvcore_env$marshalData <- function(...) {
    args <- list(...)
    # Return the first argument which is usually the data frame in the mock context
    # But marshalData signature is (parentFrame, var1, var2, ...)
    # In the mock, we might just return the data frame passed to the function
    return(NULL) # Placeholder
}

# Helper to source files with mocked environment
source_modified <- function(file_path) {
    lines <- readLines(file_path)
    
    # Modify requireNamespace calls to always return TRUE for jmvcore
    lines <- gsub('requireNamespace\\("jmvcore", quietly=TRUE\\)', 'TRUE', lines)
    lines <- gsub("requireNamespace\\('jmvcore', quietly=TRUE\\)", 'TRUE', lines)
    
    # Remove jmvcore:: prefix
    lines <- gsub("jmvcore::", "", lines)
    
    # Write to temp file and source
    temp_file <- tempfile(fileext = ".R")
    writeLines(lines, temp_file)
    source(temp_file)
}

# Source the files
print("Sourcing: R/jjpubr.h.R")
source_modified("R/jjpubr.h.R")

print("Sourcing: R/jjpubr.b.R")
source_modified("R/jjpubr.b.R")

# Create test data
print("Creating test data...")
set.seed(123)
test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 20)),
    value = c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 15, 2)),
    cont_x = rnorm(60),
    cont_y = rnorm(60)
)

# Initialize options for Box Plot
print("Initializing options for Box Plot...")
options <- jjpubrOptions$new(
    plotType = "boxplot",
    xvar = "group",
    yvar = "value",
    addStats = TRUE,
    statMethod = "anova",
    pairwiseComparisons = TRUE,
    palette = "jco"
)

# Initialize analysis
print("Initializing analysis...")
analysis <- jjpubrClass$new(
    options = options,
    data = test_data
)

# Run analysis
print("Running analysis...")
tryCatch({
    analysis$run()
    print("Analysis run completed.")
}, error = function(e) {
    print(paste("Error during analysis run:", e$message))
    traceback()
})

# Check results
print("Checking results...")
if (!is.null(analysis$results$plot)) {
    print("Plot object exists.")
} else {
    print("Plot object is NULL.")
}

# Test Scatter Plot
print("Initializing options for Scatter Plot...")
options_scatter <- jjpubrOptions$new(
    plotType = "scatter",
    xvar = "cont_x",
    yvar = "cont_y",
    addCorr = TRUE,
    addMarginal = TRUE
)

analysis_scatter <- jjpubrClass$new(
    options = options_scatter,
    data = test_data
)

print("Running scatter plot analysis...")
tryCatch({
    analysis_scatter$run()
    print("Scatter plot analysis run completed.")
}, error = function(e) {
    print(paste("Error during scatter plot analysis run:", e$message))
})

