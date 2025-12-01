
# Verification Script for nogoldstandard Function

# 0. Setup ----------------------------------------------------------------
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
if (!requireNamespace("R6", quietly = TRUE)) install.packages("R6")
if (!requireNamespace("poLCA", quietly = TRUE)) install.packages("poLCA")

library(testthat)
library(R6)
library(poLCA)

# Unload jmvcore if loaded to avoid conflict with mocks
if ("package:jmvcore" %in% search()) {
    detach("package:jmvcore", unload = TRUE)
}

# 1. Mocks for jmvcore and dependencies -----------------------------------

# Mock jmvcore::Option classes
Option <- R6::R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value = NULL) {
            self$name <- name
            self$value <- value
        }
    )
)

OptionBool <- R6::R6Class("OptionBool", inherit = Option)
OptionList <- R6::R6Class("OptionList", inherit = Option)
OptionNumber <- R6::R6Class("OptionNumber", inherit = Option)
OptionString <- R6::R6Class("OptionString", inherit = Option)
OptionInteger <- R6::R6Class("OptionInteger", inherit = Option)
OptionVariables <- R6::R6Class("OptionVariables", inherit = Option)

# Mock jmvcore::Analysis
Analysis <- R6::R6Class("Analysis",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- list(
                instructions = MockHtml$new(),
                prevalence = MockTable$new(),
                test_metrics = MockTable$new(),
                model_fit = MockTable$new(),
                clinical_summary = MockHtml$new(),
                method_guide = MockHtml$new(),
                crosstab = MockTable$new(),
                agreement_plot = MockImage$new(),
                agreement_plot2 = MockImage$new()
            )
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .run = function() {},
        .checkpoint = function(...) {}
    )
)

# Mock Table, Html, Image classes
MockTable <- R6::R6Class("MockTable",
    public = list(
        rows = list(),
        columns = list(),
        visible = TRUE,
        initialize = function() {
            self$rows <- list()
        },
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- list(rowKey = rowKey, values = values)
        },
        setRow = function(rowNo, values) {
             if (rowNo > length(self$rows)) {
                 self$rows[[rowNo]] <- list(values = values)
             } else {
                 for (name in names(values)) {
                     self$rows[[rowNo]]$values[[name]] <- values[[name]]
                 }
             }
        },
        deleteRows = function() {
            self$rows <- list()
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            col_names <- unique(unlist(lapply(self$rows, function(r) names(r$values))))
            df_list <- lapply(col_names, function(col) {
                sapply(self$rows, function(r) {
                    val <- r$values[[col]]
                    if (is.null(val)) NA else val
                })
            })
            names(df_list) <- col_names
            as.data.frame(df_list, stringsAsFactors = FALSE)
        },
        setVisible = function(visible) { self$visible <- visible },
        isFilled = function() { return(length(self$rows) > 0) },
        setTitle = function(title) {},
        setNote = function(key, note) {},
        addFootnote = function(rowKey, col, note) {}
    )
)

MockHtml <- R6::R6Class("MockHtml",
    public = list(
        content = NULL,
        visible = TRUE,
        setContent = function(content) { self$content <- content },
        setVisible = function(visible) { self$visible <- visible }
    )
)

MockImage <- R6::R6Class("MockImage",
    public = list(
        visible = TRUE,
        state = list(),
        setVisible = function(visible) { self$visible <- visible },
        setState = function(state) { self$state <- state }
    )
)

# Mock jmvcore environment
assign("Table", MockTable, envir = .GlobalEnv)
assign("Html", MockHtml, envir = .GlobalEnv)
assign("Image", MockImage, envir = .GlobalEnv)
assign("Analysis", Analysis, envir = .GlobalEnv)

# Mock jmvcore functions
mock_jmvcore <- new.env()
mock_jmvcore$format <- function(fmt, ...) {
    args <- list(...)
    for (name in names(args)) {
        fmt <- gsub(paste0("\\{", name, "\\}"), args[[name]], fmt)
    }
    fmt
}
mock_jmvcore$. <- function(text) text
mock_jmvcore$naOmit <- function(data) na.omit(data)
. <- function(text) text

if ("package:jmvcore" %in% search()) detach("package:jmvcore", unload = TRUE)
attach(mock_jmvcore, name = "jmvcore_mock", warn.conflicts = FALSE)

# Source the nogoldstandard implementation
# We need nogoldstandardBase class definition first, usually in .h.R
# Since we don't have .h.R sourced, we define a dummy base class
nogoldstandardBase <- R6::R6Class(
    "nogoldstandardBase",
    inherit = Analysis,
    public = list(
        initialize = function(options, data) {
            super$initialize(options, data)
            private$.init()
        }
    )
)

source("R/nogoldstandard.b.R")

# 2. Test Data Generation -------------------------------------------------

set.seed(123)
n <- 200
# Simulate disease status (latent)
disease <- rbinom(n, 1, 0.3)

# Simulate 3 tests
test1 <- ifelse(disease == 1, rbinom(sum(disease), 1, 0.85), rbinom(sum(1-disease), 1, 0.15))
test2 <- ifelse(disease == 1, rbinom(sum(disease), 1, 0.80), rbinom(sum(1-disease), 1, 0.10))
test3 <- ifelse(disease == 1, rbinom(sum(disease), 1, 0.90), rbinom(sum(1-disease), 1, 0.05))

test_data <- data.frame(
    test1 = factor(test1, levels=c(0,1), labels=c("Neg", "Pos")),
    test2 = factor(test2, levels=c(0,1), labels=c("Neg", "Pos")),
    test3 = factor(test3, levels=c(0,1), labels=c("Neg", "Pos"))
)

# 3. Test Cases -----------------------------------------------------------

test_that("LCA with 3 tests works", {
    options <- list(
        test1 = "test1", test1Positive = "Pos",
        test2 = "test2", test2Positive = "Pos",
        test3 = "test3", test3Positive = "Pos",
        method = "latent_class",
        bootstrap = FALSE,
        verbose = FALSE,
        clinicalPreset = "none"
    )
    
    analysis <- nogoldstandardClass$new(options, test_data)
    analysis$run()
    
    # Check prevalence
    prev_table <- analysis$results$prevalence$asDF()
    expect_equal(nrow(prev_table), 1)
    expect_true(prev_table$estimate > 0.2 && prev_table$estimate < 0.4)
    
    # Check metrics
    metrics <- analysis$results$test_metrics$asDF()
    expect_equal(nrow(metrics), 3)
})

test_that("LCA with 2 tests (Unidentifiable)", {
    options <- list(
        test1 = "test1", test1Positive = "Pos",
        test2 = "test2", test2Positive = "Pos",
        method = "latent_class",
        bootstrap = FALSE,
        verbose = TRUE,
        clinicalPreset = "none"
    )
    
    analysis <- nogoldstandardClass$new(options, test_data)
    
    # This might fail or produce garbage. We want to see what happens.
    # If it errors, that's good (if handled). If it runs and gives garbage, that's bad.
    # poLCA usually warns about df.
    
    expect_warning(analysis$run()) # Expecting some warning from poLCA or our code
    
    # Check if results are produced
    prev_table <- analysis$results$prevalence$asDF()
    print(prev_table)
})

test_that("Composite Method works", {
    options <- list(
        test1 = "test1", test1Positive = "Pos",
        test2 = "test2", test2Positive = "Pos",
        test3 = "test3", test3Positive = "Pos",
        method = "composite",
        bootstrap = FALSE,
        verbose = FALSE,
        clinicalPreset = "none"
    )
    
    analysis <- nogoldstandardClass$new(options, test_data)
    analysis$run()
    
    metrics <- analysis$results$test_metrics$asDF()
    expect_equal(nrow(metrics), 3)
})

print("Verification Script Created Successfully")
