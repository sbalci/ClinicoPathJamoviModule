
# Verification script for consortdiagram
# This script mocks the jamovi environment and runs the consortdiagram function

# 1. Mock jmvcore classes -------------------------------------------------
library(R6)

# Mock Image class
Image <- R6Class("Image",
    public = list(
        name = NULL,
        initialize = function(options, name = "image", ...) {
            self$name <- name
        },
        setVisible = function(...) {},
        setState = function(...) {},
        plot = NULL
    )
)

# Mock Table class
Table <- R6Class("Table",
    public = list(
        name = NULL,
        rowKeys = character(0),
        rows = list(),
        initialize = function(options, name = "table", ...) {
            self$name <- name
        },
        setVisible = function(...) {},
        addRow = function(rowKey, values) {
            self$rowKeys <- c(self$rowKeys, rowKey)
            self$rows[[as.character(rowKey)]] <- values
        },
        removeRow = function(rowKey) {
            idx <- which(self$rowKeys == rowKey)
            if (length(idx) > 0) {
                self$rowKeys <- self$rowKeys[-idx]
                self$rows[[as.character(rowKey)]] <- NULL
            }
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            do.call(rbind, lapply(self$rows, as.data.frame))
        }
    )
)

# Mock Html class
Html <- R6Class("Html",
    public = list(
        name = NULL,
        content = NULL,
        initialize = function(options, name = "html", ...) {
            self$name <- name
        },
        setVisible = function(...) {},
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock Options class
Options <- R6Class("Options",
    public = list(
        initialize = function(...) {
            # Base initialization
        },
        .addOption = function(option) {
            # Mock implementation
        }
    )
)

# Mock OptionVariable and OptionVariables
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
OptionString <- R6Class("OptionString",
    public = list(
        value = "",
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

# Mock Results class
Results <- R6Class("Results",
    public = list(
        consortPlot = NULL,
        flowSummary = NULL,
        armSummary = NULL,
        exclusionBreakdown = NULL,
        caveatsAssumptions = NULL,
        consortValidation = NULL,
        exportInfo = NULL,
        todo = NULL,
        
        initialize = function(options) {
            self$consortPlot <- Image$new(options, name="consortPlot")
            self$flowSummary <- Table$new(options, name="flowSummary")
            self$armSummary <- Table$new(options, name="armSummary")
            self$exclusionBreakdown <- Table$new(options, name="exclusionBreakdown")
            self$caveatsAssumptions <- Html$new(options, name="caveatsAssumptions")
            self$consortValidation <- Html$new(options, name="consortValidation")
            self$exportInfo <- Html$new(options, name="exportInfo")
            self$todo <- Html$new(options, name="todo")
        }
    )
)

# Mock Analysis class
Analysis <- R6Class("Analysis",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- Results$new(options)
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .run = function() {}
    )
)

# Helper to read, modify, and eval
source_modified <- function(file_path) {
    print(paste("Sourcing:", file_path))
    lines <- readLines(file_path)
    
    # Remove jmvcore:: prefixes
    lines <- gsub("jmvcore::", "", lines)
    
    # DEBUG: Print first few lines
    print("First 10 lines of modified code:")
    print(head(lines, 10))
    
    # Eval in global environment
    eval(parse(text = lines), envir = .GlobalEnv)
}

# Mock requireNamespace in GlobalEnv
requireNamespace <- function(...) {
    print(paste("requireNamespace called for:", list(...)[[1]]))
    return(TRUE)
}

# Mock jmvcore environment
jmvcore_env <- new.env()
jmvcore_env$Options <- Options
jmvcore_env$OptionVariable <- OptionVariable
jmvcore_env$OptionVariables <- OptionVariables
jmvcore_env$OptionBool <- OptionBool
jmvcore_env$OptionNumber <- OptionNumber
jmvcore_env$OptionString <- OptionString
OptionInteger <- R6Class("OptionInteger",
    public = list(
        value = 0,
        initialize = function(name, value, ...) {
            self$value <- value
        }
    )
)
jmvcore_env$OptionInteger <- OptionInteger
# Mock Group class
Group <- R6Class("Group",
    public = list(
        initialize = function(options, name = "", title = "", visible = TRUE, ...) {
            private$.items <- list()
        },
        add = function(item) {
            private$.items[[item$name]] <- item
        },
        setVisible = function(...) {}
    ),
    private = list(
        .items = list()
    )
)
jmvcore_env$Group <- Group

# Mock Results class (Simplified as it inherits from Group in reality, but here we can keep it or rely on Group)
# Actually, consortdiagramResults inherits from Group.
# So we don't need to mock Results class separately if we source .h.R which defines consortdiagramResults.
# But verify_consortdiagram.R previously defined Results class.
# Since we source .h.R, we should NOT define Results class manually if it conflicts.
# But consortdiagramBase uses consortdiagramResults.
# Let's remove Results mock and rely on sourced .h.R and our Group mock.

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
        .run = function() {},
        .checkpoint = function() {}
    )
)

# Source .h.R
source_modified("R/consortdiagram.h.R")

# Source .b.R
source_modified("R/consortdiagram.b.R")

# DEBUG: Check if classes are defined
print("Objects in GlobalEnv:")
print(ls(envir = .GlobalEnv))
print("Checking consortdiagramOptions:")
if (exists("consortdiagramOptions")) {
    print("consortdiagramOptions exists")
} else {
    print("consortdiagramOptions DOES NOT EXIST")
}
print("Checking consortdiagramBase:")
if (exists("consortdiagramBase")) {
    print("consortdiagramBase exists")
} else {
    print("consortdiagramBase DOES NOT EXIST")
}
print("Checking consortdiagramResults:")
if (exists("consortdiagramResults")) {
    print("consortdiagramResults exists")
} else {
    print("consortdiagramResults DOES NOT EXIST")
}

# Mock jmvcore helper functions
toB64 <- function(x) x
fromB64 <- function(x) x
constructFormula <- function(...) ""
composeFormula <- function(...) ""
decomposeFormula <- function(...) list()

# 3. Run Verification -----------------------------------------------------

# Create test data
n <- 100
test_data <- data.frame(
    id = 1:n,
    screening_fail = c(rep(NA, 90), rep("Protocol violation", 10)),
    enrollment_fail = c(rep(NA, 85), rep("Withdrew consent", 5), rep(NA, 10)),
    stringsAsFactors = FALSE
)

# Setup options
options <- consortdiagramOptions$new(
    participant_id = "id",
    screening_exclusions = "screening_fail",
    enrollment_exclusions = "enrollment_fail",
    show_exclusion_details = TRUE
)

# Initialize analysis
analysis <- consortdiagramClass$new(
    options = options,
    data = test_data
)

# Run analysis
print("Running analysis...")
tryCatch({
    analysis$run()
    print("Analysis run completed successfully.")
}, error = function(e) {
    print(paste("Error running analysis:", e$message))
    print(e)
})

# Check results
print("Checking results...")

# Flow Summary
print("Flow Summary:")
if (!is.null(analysis$results$flowSummary)) {
    print(analysis$results$flowSummary$asDF())
} else {
    print("Flow Summary is NULL")
}

# Exclusion Breakdown
print("Exclusion Breakdown:")
if (!is.null(analysis$results$exclusionBreakdown)) {
    print(analysis$results$exclusionBreakdown$asDF())
} else {
    print("Exclusion Breakdown is NULL")
}

# Check Plot
print("Checking Plot...")
if (!is.null(analysis$results$consortPlot)) {
    print("Plot object exists.")
}
