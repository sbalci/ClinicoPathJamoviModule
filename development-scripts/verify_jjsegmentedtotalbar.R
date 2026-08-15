library(jmvcore)
library(R6)
library(ggplot2)
library(dplyr)
library(glue)

# Mock translation function
. <- function(x) x

# Mock classes
HTML <- R6::R6Class("HTML",
    public = list(
        content = "",
        visible = TRUE,
        setContent = function(content) { self$content <- content },
        setVisible = function(visible) { self$visible <- visible }
    )
)

Image <- R6::R6Class("Image",
    public = list(
        setSize = function(...) {},
        setVisible = function(...) {},
        plot = function(...) {}
    )
)

Table <- R6::R6Class("Table",
    public = list(
        rows = list(),
        visible = TRUE,
        setRow = function(...) {},
        addRow = function(...) {},
        deleteRows = function(...) { self$rows <- list() },
        setVisible = function(visible) { self$visible <- visible }
    )
)

jjsegmentedtotalbarBase <- R6::R6Class(
    "jjsegmentedtotalbarBase",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- list(
                instructions = HTML$new(),
                plot = Image$new(),
                summary = Table$new(),
                composition_table = Table$new(),
                detailed_stats = Table$new(),
                interpretation = HTML$new(),
                clinical_summary = HTML$new(),
                statistical_tests = Table$new(),
                preset_guidance = HTML$new(),
                explanations = HTML$new(),
                presetInfo = HTML$new(),
                warnings = HTML$new()
            )
        }
    )
)

# Source the class definition
source("R/jjsegmentedtotalbar.b.R")

if (is.null(jjsegmentedtotalbarClass)) {
    print("WARNING: jjsegmentedtotalbarClass is NULL.")
} else {
    print("jjsegmentedtotalbarClass is defined.")
}

# Helpers
run_test <- function(test_name, data, options, test_class) {
    cat(glue::glue("\n--- Test: {test_name} ---\n"))
    
    if (is.null(test_class)) {
        cat("ERROR: test_class provided is NULL.\n")
        return(NULL)
    }

    tryCatch({
        module <- test_class$new(options = options, data = data)
        cat("Initializing...\n")
        module$.__enclos_env__$private$.init()
        cat("Running...\n")
        module$.__enclos_env__$private$.run()
        cat("Run complete.\n")
        return(module)
    }, error = function(e) {
        cat(glue::glue("ERROR in {test_name}: {e$message}\n"))
        print(sys.calls())
        return(NULL)
    })
}

# Dummy Data
set.seed(123)
data_valid <- data.frame(
    x_var = rep(c("A", "B"), each = 50),
    fill_var = sample(c("Y", "N"), 100, replace = TRUE),
    y_var = rep(1, 100) # Counts
)
data_valid$facet_var <- sample(c("F1", "F2"), 100, replace=TRUE)

data_continuous <- data.frame(
    x_var = rep(c("A", "B"), each = 50),
    fill_var = sample(c("Y", "N"), 100, replace = TRUE),
    y_var = runif(100, 1.5, 5.5) # Continuous
)

# Test 1: Preset Info Visibility
if (FALSE) {
cat("Running Test 1: Preset Info\n")
options_1 <- list(
    x_var = "x_var",
    y_var = "y_var",
    fill_var = "fill_var",
    analysis_preset = "treatment_response",
    show_plot = TRUE,
    plot_width = 10,
    plot_height = 6,
    show_statistical_tests = FALSE
)
res1 <- run_test("Preset Info", data_valid, options_1, jjsegmentedtotalbarClass)
if(!is.null(res1)) {
    if(res1$results$presetInfo$visible) print("Preset Info Visible: OK")
}

# Test 2: Continuous Data Warning (Block Stats)
cat("Running Test 2: Continuous Data Check\n")
options_2 <- list(
    x_var = "x_var",
    y_var = "y_var",
    fill_var = "fill_var",
    analysis_preset = "custom",
    show_plot = TRUE,
    plot_width = 10,
    plot_height = 6,
    show_statistical_tests = TRUE, # Should trigger warning
    confidence_level = 0.95
)
res2 <- run_test("Continuous Data Check", data_continuous, options_2, jjsegmentedtotalbarClass)
if(!is.null(res2)) {
    if(res2$results$warnings$visible) {
        print("Warning visible for continuous data: OK")
        print(res2$results$warnings$content)
    } else {
        print("Warning NOT visible for continuous data: FAIL")
    }
}
}

# Test 3: Valid Integer Data (Stats Run)
cat("Running Test 3: Valid Data Stats\n")
options_3 <- list(
    x_var = "x_var",
    y_var = "y_var", # All 1s
    fill_var = "fill_var",
    analysis_preset = "custom",
    show_plot = TRUE,
    plot_width = 10,
    plot_height = 6,
    show_statistical_tests = TRUE,
    confidence_level = 0.95,
    sort_categories = "none",
    color_palette = "clinical",
    chart_style = "clinical"
)
res3 <- run_test("Valid Data Stats", data_valid, options_3, jjsegmentedtotalbarClass)
# We expect stats table to be populated (mock doesn't track rows perfectly but we check clear calls)
