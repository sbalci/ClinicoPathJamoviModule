
library(R6)
library(dplyr)
library(survival)
library(lme4)
library(broom)
library(ggstatsplot)
library(ggplot2)
library(glue)
library(janitor)

options(error = function() traceback(2))

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
                coefficientTable = Table$new(),
                modelMetrics = Table$new(),
                modelSummary = Html$new(),
                about = Html$new(),
                instructions = Html$new(),
                coefPlot = Image$new()
            )
        },
        run = function() {
            private$.run()
        }
    )
)

Table <- R6::R6Class("Table",
    public = list(
        rows = list(),
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- values
        },
        asDF = function() {
            do.call(rbind, lapply(self$rows, as.data.frame))
        }
    )
)

Html <- R6::R6Class("Html",
    public = list(
        content = "",
        setContent = function(value) self$content <- value
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
        setState = function(p) self$plot <- p
    )
)

# Load the class definition
# We need to source the file but bypass the jmvcore requirement check if possible
# Or just copy the class definition here for testing if sourcing is hard.
# Sourcing is better to test the actual file.
# We need to mock jjcoefstatsBase first.

jjcoefstatsBase <- R6::R6Class("jjcoefstatsBase",
    inherit = Analysis
)

# Source the file manually to bypass requireNamespace check
# We read the file, remove the condition, and evaluate
file_content <- readLines("R/jjcoefstats.b.R")
# Remove the first few lines that might contain imports or the assignment with condition
# We want to extract the R6::R6Class definition
# The definition starts around line 6: jjcoefstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
# We will replace this line with: jjcoefstatsClass <- R6::R6Class(

# Find the line with the class definition
start_line <- grep("jjcoefstatsClass <- if", file_content)
if (length(start_line) > 0) {
    file_content[start_line] <- "jjcoefstatsClass <- R6::R6Class("
}

# Inject debug prints
run_line <- grep("\\.run = function\\(\\)", file_content)
if (length(run_line) > 0) {
    file_content[run_line] <- ".run = function() { print('DEBUG: Entering .run')"
}

about_line <- grep("\\.generateAboutContent = function\\(\\)", file_content)
if (length(about_line) > 0) {
    file_content[about_line] <- ".generateAboutContent = function() { print('DEBUG: Entering .generateAboutContent')"
}

# Evaluate the modified content
eval(parse(text = file_content))

# Helper to create options list
create_options <- function(...) {
    opts <- list(...)
    # Set defaults
    defaults <- list(
        inputMode = "fitmodel",
        ciLevel = 0.95,
        exponentiate = FALSE,
        sortCoefs = FALSE,
        excludeIntercept = TRUE,
        referenceValue = 0,
        showPValues = TRUE,
        pSymbols = FALSE,
        plotTheme = "default",
        plotWidth = 700,
        plotHeight = 500,
        showexplanations = TRUE,
        # Precomputed defaults
        degreesOfFreedom = NULL,
        confLow = NULL,
        confHigh = NULL,
        pValue = NULL
    )
    
    for (n in names(defaults)) {
        if (is.null(opts[[n]])) opts[[n]] <- defaults[[n]]
    }
    return(opts)
}

# Test 1: Linear Model
print("=== Test 1: Linear Model ===")
set.seed(123)
data_lm <- data.frame(
    outcome = rnorm(100),
    pred1 = rnorm(100),
    pred2 = rnorm(100)
)

options_lm <- create_options(
    inputMode = "fitmodel",
    modelType = "lm",
    outcome = "outcome",
    predictors = c("pred1", "pred2")
)

analysis_lm <- jjcoefstatsClass$new(options_lm, data_lm)
analysis_lm$run()

print("Coefficient Table:")
print(analysis_lm$results$coefficientTable$asDF())
print("Model Metrics:")
print(analysis_lm$results$modelMetrics$asDF())

# Test 2: Logistic Model with Exponentiation
print("\n=== Test 2: Logistic Model (OR) ===")
data_glm <- data.frame(
    outcome = sample(c(0, 1), 100, replace = TRUE),
    pred1 = rnorm(100),
    pred2 = rnorm(100)
)

options_glm <- create_options(
    inputMode = "fitmodel",
    modelType = "glm",
    outcome = "outcome",
    predictors = c("pred1", "pred2"),
    exponentiate = TRUE,
    referenceValue = 1
)

analysis_glm <- jjcoefstatsClass$new(options_glm, data_glm)
analysis_glm$run()

print("Coefficient Table (OR):")
print(analysis_glm$results$coefficientTable$asDF())

# Test 3: Cox Model
print("\n=== Test 3: Cox Model ===")
data_cox <- data.frame(
    time = rexp(100),
    status = sample(c(0, 1), 100, replace = TRUE),
    pred1 = rnorm(100)
)

options_cox <- create_options(
    inputMode = "fitmodel",
    modelType = "cox",
    survivalTime = "time",
    eventStatus = "status",
    predictors = c("pred1"),
    exponentiate = TRUE
)

analysis_cox <- jjcoefstatsClass$new(options_cox, data_cox)
analysis_cox$run()

print("Coefficient Table (HR):")
print(analysis_cox$results$coefficientTable$asDF())

# Test 4: Precomputed Mode
print("\n=== Test 4: Precomputed Mode ===")
data_pre <- data.frame(
    term = c("A", "B"),
    est = c(0.5, -0.2),
    se = c(0.1, 0.1)
)

options_pre <- create_options(
    inputMode = "precomputed",
    term = "term",
    estimate = "est",
    stdError = "se"
)

analysis_pre <- jjcoefstatsClass$new(options_pre, data_pre)
analysis_pre$run()

print("Coefficient Table (Precomputed):")
print(analysis_pre$results$coefficientTable$asDF())

