
library(R6)
library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(glue)
library(rlang)

options(error = function() traceback(2))

# Mock localization function
. <- function(x) x


# Mock Jamovi Classes
Option <- R6::R6Class("Option",
    public = list(
        value = NULL,
        initialize = function(value) self$value <- value
    )
)

Analysis <- R6::R6Class("Analysis",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- list(
                todo = Html$new(),
                about = Html$new(),
                summary = Html$new(),
                assumptions = Html$new(),
                interpretation = Html$new(),
                plot = Image$new(),
                plot2 = Image$new()
            )
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

Html <- R6::R6Class("Html",
    public = list(
        content = "",
        setContent = function(value) self$content <- value,
        visible = TRUE
    )
)

Image <- R6::R6Class("Image",
    public = list(
        width = 0,
        height = 0,
        setSize = function(w, h) {
            self$width <- w
            self$height <- h
        },
        plot = NULL,
        setState = function(p) self$plot <- p,
        visible = TRUE
    )
)

jjcorrmatBase <- R6::R6Class("jjcorrmatBase",
    inherit = Analysis
)

# Load the class definition manually to bypass jmvcore check
file_content <- readLines("R/jjcorrmat.b.R")
start_line <- grep("jjcorrmatClass <- if", file_content)
if (length(start_line) > 0) {
    file_content[start_line] <- "jjcorrmatClass <- R6::R6Class("
}

# Remove the trailing "else NULL"
end_line <- grep("\\) else NULL", file_content)
if (length(end_line) > 0) {
    file_content[end_line] <- ")"
}

eval(parse(text = file_content))

# Helper to create options list
create_options <- function(...) {
    opts <- list(...)
    # Set defaults
    defaults <- list(
        dep = NULL,
        grvar = NULL,
        typestatistics = "parametric",
        matrixtype = "upper",
        matrixmethod = "square",
        siglevel = 0.05,
        conflevel = 0.95,
        padjustmethod = "holm",
        k = 2,
        partial = FALSE,
        clinicalpreset = "custom",
        lowcolor = "#E69F00",
        midcolor = "white",
        highcolor = "#009E73",
        title = "",
        subtitle = "",
        caption = "",
        showexplanations = TRUE,
        plotwidth = 600,
        plotheight = 450
    )
    
    for (n in names(defaults)) {
        if (is.null(opts[[n]])) opts[[n]] <- defaults[[n]]
    }
    return(opts)
}

# Test 1: Basic Correlation Matrix
print("=== Test 1: Basic Correlation Matrix ===")
set.seed(123)
data_basic <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100)
)

options_basic <- create_options(
    dep = c("var1", "var2", "var3"),
    typestatistics = "parametric"
)

analysis_basic <- jjcorrmatClass$new(options_basic, data_basic)
analysis_basic$run()

print("Analysis Summary:")
print(analysis_basic$results$summary$content)
print("Interpretation:")
print(analysis_basic$results$interpretation$content)

# Test 2: Grouped Correlation Matrix
print("\n=== Test 2: Grouped Correlation Matrix ===")
data_grouped <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    group = factor(rep(c("A", "B"), each = 50))
)

options_grouped <- create_options(
    dep = c("var1", "var2"),
    grvar = "group",
    typestatistics = "nonparametric"
)

analysis_grouped <- jjcorrmatClass$new(options_grouped, data_grouped)
analysis_grouped$run()

print("Grouped Analysis Completed")

# Test 3: Partial Correlations
print("\n=== Test 3: Partial Correlations ===")
data_partial <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100)
)

options_partial <- create_options(
    dep = c("var1", "var2", "var3"),
    partial = TRUE
)

analysis_partial <- jjcorrmatClass$new(options_partial, data_partial)
analysis_partial$run()

print("Partial Correlation Summary:")
print(analysis_partial$results$summary$content)

# Test 4: Clinical Preset (Biomarker)
print("\n=== Test 4: Clinical Preset (Biomarker) ===")
options_preset <- create_options(
    dep = c("var1", "var2", "var3"),
    clinicalpreset = "biomarker"
)

analysis_preset <- jjcorrmatClass$new(options_preset, data_basic)
analysis_preset$run()

print("Preset Analysis Interpretation:")
print(analysis_preset$results$interpretation$content)
