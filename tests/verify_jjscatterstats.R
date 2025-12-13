library(jmvcore)
library(R6)
library(ggplot2)
library(ggstatsplot)
library(glue)

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
        setSize = function(...) { cat("DEBUG: Image$setSize called\n") },
        setVisible = function(...) { cat("DEBUG: Image$setVisible called\n") },
        plot = function(...) { cat("DEBUG: Image$plot called\n") }
    )
)

jjscatterstatsBase <- R6::R6Class(
    "jjscatterstatsBase",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            cat("DEBUG: jjscatterstatsBase initialize called\n")
            self$options <- options
            self$data <- data
            self$results <- list(
                todo = HTML$new(),
                plot = Image$new(),
                plot2 = Image$new(),
                plot3 = Image$new(),
                ggpubrPlot = Image$new(),
                ggpubrPlot2 = Image$new(),
                explanations = HTML$new(),
                warnings = HTML$new(),
                presetInfo = HTML$new()
            )
            cat("DEBUG: results list initialized\n")
        }
    )
)

# Source the class definition
source("R/jjscatterstats.b.R")

if (is.null(jjscatterstatsClass)) {
    print("WARNING: jjscatterstatsClass is NULL. This suggests requireNamespace('jmvcore') failed or class definition syntax is problematic.")
    # Attempt to redefine mostly for testing logic, or stop
} else {
    print("jjscatterstatsClass is defined.")
}

# Helpers
run_test <- function(test_name, data, options, test_class) {
    cat(glue::glue("\n--- Test: {test_name} ---\n"))
    
    if (is.null(test_class)) {
        cat("ERROR: test_class provided is NULL. Aborting test.\n")
        return(NULL)
    }
    
    # Debug print for jjscatterstatsClass existence
    if (is.null(jjscatterstatsClass)) {
        cat("DEBUG: jjscatterstatsClass is NULL before test_class instantiation.\n")
    } else {
        cat("DEBUG: jjscatterstatsClass is defined before test_class instantiation.\n")
    }

    tryCatch({
        cat("Initializing module...\n")
        module <- test_class$new(options = options, data = data)
        cat("Calling .init()...\n")
        module$.__enclos_env__$private$.init() # Call private init
        cat("Calling .run()...\n")
        module$.__enclos_env__$private$.run()  # Call private run
        cat("Run complete.\n")
        return(module)
    }, error = function(e) {
        cat(glue::glue("ERROR in {test_name}: {e$message}\nTraceback:\n"))
        print(sys.calls())
        return(NULL)
    })
}

# Create dummy data
set.seed(123)
test_data <- data.frame(
    dep = rnorm(100),
    group = rnorm(100),
    grvar = sample(c("A", "B"), 100, replace = TRUE),
    colorvar = sample(c("C1", "C2"), 100, replace = TRUE)
)

# Test 1: Basic Run (Defaults)
cat("Running Test 1: Basic Defaults\n")
options_1 <- list(
    dep = "dep",
    group = "group",
    typestatistics = "parametric",
    clinicalPreset = "custom",
    showExplanations = TRUE,
    smoothMethod = "lm",
    marginalType = "none"
)
res1 <- run_test("Basic Defaults", test_data, options_1, jjscatterstatsClass)

if(!is.null(res1)) {
    if(res1$results$explanations$visible) print("Explanations visible: OK")
}

# Test 2: Logic Wiring (Smooth + Rug + Marginal)
cat("Running Test 2: Logic Wiring\n")
options_2 <- list(
    dep = "dep",
    group = "group",
    grvar = "grvar",
    smoothMethod = "loess",
    showRugPlot = TRUE,
    marginal = TRUE,
    marginalType = "density",
    clinicalPreset = "custom"
)
res2 <- run_test("Logic Wiring", test_data, options_2, jjscatterstatsClass)
# We can't easily inspect the plot object without rendering, but successful run implies no crash.

# Test 3: Clinical Preset
cat("Running Test 3: Clinical Preset\n")
options_3 <- list(
    dep = "dep",
    group = "group",
    clinicalPreset = "treatment_response_analysis"
)
res3 <- run_test("Clinical Preset", test_data, options_3, jjscatterstatsClass)

if(!is.null(res3)) {
    if(res3$options$typestatistics == "robust") print("Preset mutation worked (robust): OK")
    if(res3$results$presetInfo$visible) print("Preset Info visible: OK")
    print(res3$results$presetInfo$content)
}

# Test 4: Enhanced Plot Warning (Robust fallback)
cat("Running Test 4: Enhanced Plot Robust Fallback\n")
options_4 <- list(
    dep = "dep",
    group = "group",
    colorvar = "colorvar",
    typestatistics = "robust",
    smoothMethod = "lm",
    clinicalPreset = "custom",
    showExplanations = FALSE,
    showRugPlot = FALSE,
    resultssubtitle = TRUE,
    conflevel = 0.95,
    bfmessage = FALSE,
    k = 2,
    smoothlinesize = 1,
    smoothlinecolor = "blue",
    pointsize = 2,
    pointalpha = 1,
    originaltheme = FALSE,
    marginalType = "none"
)
res4 <- run_test("Enhanced Plot Fallback", test_data, options_4, jjscatterstatsClass)
# We need to invoke .plot3 explicitly as .run doesn't call it (jamovi engine calls it)
tryCatch({
    if (!is.null(res4)) {
        res4$.__enclos_env__$private$.plot3(NULL)
        cat(".plot3 executed.\n")
        if(res4$results$warnings$visible) print("Warnings visible: OK")
        print(res4$results$warnings$content)
    }
}, error = function(e) print(paste("Plot3 Error:", e$message)))
