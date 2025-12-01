
# Verification Script for pcaloadingtest Function

library(R6)
library(testthat)
library(dplyr)
library(ggplot2)

# 1. Mocking jmvcore Environment ------------------------------------------

# Mock Option Class
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

# Mock Table Class
Table <- R6Class("Table",
    public = list(
        name = NULL,
        rows = list(),
        initialize = function(name) {
            self$name <- name
        },
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- values
        }
    )
)

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
        width = NULL,
        height = NULL,
        initialize = function(name) {
            self$name <- name
        },
        setState = function(state) {
            self$state <- state
        },
        setVisible = function(visible) {
            self$visible <- visible
        },
        setSize = function(width, height) {
            self$width <- width
            self$height <- height
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        todo = NULL,
        results = NULL,
        loadingplot = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$results <- Table$new("results")
            self$loadingplot <- Image$new("loadingplot")
        }
    )
)

# Mock Base Class
pcaloadingtestBase <- R6Class("pcaloadingtestBase",
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
    ),
    private = list(
        .checkpoint = function() {
            # Mock checkpoint
        }
    )
)

# Mock jmvcore::reject
mock_jmvcore <- new.env()
mock_jmvcore$reject <- function(message) {
    stop(paste("JMVCORE_REJECT:", message))
}

jmvcore <- mock_jmvcore

# Source the implementation
source("R/pcaloadingtest.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(vars, 
                          ncomp = 3,
                          nperm = 50, # Low for testing speed
                          componentfilter = 0,
                          center = TRUE,
                          scale = TRUE,
                          conflevel = 0.95,
                          adjustmethod = "BH",
                          colorlow = "steelblue1",
                          colormid = "white",
                          colorhigh = "firebrick1",
                          plotwidth = 700,
                          plotheight = 450) {
    
    list(
        vars = vars,
        ncomp = ncomp,
        nperm = nperm,
        componentfilter = componentfilter,
        center = center,
        scale = scale,
        conflevel = conflevel,
        adjustmethod = adjustmethod,
        colorlow = colorlow,
        colormid = colormid,
        colorhigh = colorhigh,
        plotwidth = plotwidth,
        plotheight = plotheight
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Basic Functionality with mtcars", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp", "drat", "wt", "qsec")
    
    options <- create_options(vars = vars, ncomp = 3, nperm = 20)
    
    analysis <- pcaloadingtestClass$new(options, data)
    analysis$run()
    
    # Check if results table is populated
    expect_gt(length(analysis$results$results$rows), 0)
    
    # Check first row content
    first_row <- analysis$results$results$rows[[1]]
    expect_true(is.character(first_row$variable))
    expect_true(is.character(first_row$component))
    expect_true(is.numeric(first_row$original))
    expect_true(is.numeric(first_row$pvalue))
})

test_that("Data Validation: Non-numeric Variables", {
    data <- mtcars
    data$char_var <- as.character(data$mpg)
    vars <- c("mpg", "disp", "char_var")
    
    options <- create_options(vars = vars)
    
    analysis <- pcaloadingtestClass$new(options, data)
    
    expect_error(analysis$run(), "Non-numeric variables detected")
})

test_that("Data Validation: Centering/Scaling Warning", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp") # Different scales
    
    options <- create_options(vars = vars, center = FALSE, scale = FALSE)
    
    analysis <- pcaloadingtestClass$new(options, data)
    
    expect_error(analysis$run(), "WARNING: Centering and/or scaling are disabled")
})

test_that("Plot Generation", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp", "drat", "wt")
    
    options <- create_options(vars = vars, ncomp = 2, nperm = 20)
    
    analysis <- pcaloadingtestClass$new(options, data)
    analysis$run()
    
    # Check if private fields for plot are populated
    # Hack to access private fields
    expect_true(!is.null(analysis$.__enclos_env__$private$.resultsDF))
})

test_that("Component Filter Works", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp", "drat", "wt")
    
    # Filter for PC2 only
    options <- create_options(vars = vars, ncomp = 3, nperm = 20, componentfilter = 2)
    
    analysis <- pcaloadingtestClass$new(options, data)
    analysis$run()
    
    # Check that only PC2 rows are in the table
    rows <- analysis$results$results$rows
    components <- sapply(rows, function(r) r$component)
    expect_true(all(components == "PC2"))
})

print("Verification Script Created Successfully")
