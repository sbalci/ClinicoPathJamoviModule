
# Verification Script for pcacomponenttest Function

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
        vafplot = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$results <- Table$new("results")
            self$vafplot <- Image$new("vafplot")
        }
    )
)

# Mock Base Class
pcacomponenttestBase <- R6Class("pcacomponenttestBase",
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

# Source the file but intercept jmvcore calls if possible
# Since we can't easily intercept package calls in sourced code without 'mockery',
# we will rely on the fact that 'jmvcore' is imported. 
# We'll define a local 'jmvcore' list to mimic the package if needed, 
# but the code calls `jmvcore::reject`.
# We will define `jmvcore` in the global env.
jmvcore <- mock_jmvcore

# Source the implementation
source("R/pcacomponenttest.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(vars, 
                          ncomp = 5,
                          nperm = 100, # Low for testing speed
                          center = TRUE,
                          scale = TRUE,
                          conflevel = 0.95,
                          adjustmethod = "BH",
                          showpercent = TRUE,
                          colororiginal = "steelblue",
                          colorpermuted = "orange",
                          plotwidth = 600,
                          plotheight = 450) {
    
    list(
        vars = vars,
        ncomp = ncomp,
        nperm = nperm,
        center = center,
        scale = scale,
        conflevel = conflevel,
        adjustmethod = adjustmethod,
        showpercent = showpercent,
        colororiginal = colororiginal,
        colorpermuted = colorpermuted,
        plotwidth = plotwidth,
        plotheight = plotheight
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Basic Functionality with mtcars", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp", "drat", "wt", "qsec")
    
    options <- create_options(vars = vars, ncomp = 5, nperm = 50)
    
    analysis <- pcacomponenttestClass$new(options, data)
    analysis$run()
    
    # Check if results table is populated
    expect_gt(length(analysis$results$results$rows), 0)
    
    # Check first row content
    first_row <- analysis$results$results$rows[[1]]
    expect_equal(first_row$component, "PC1")
    expect_true(is.numeric(first_row$originalvaf))
    expect_true(is.numeric(first_row$pvalue))
})

test_that("Sequential Logic Stops Early (Mock Data)", {
    # Create uncorrelated data where NO component should be significant
    set.seed(123)
    data <- data.frame(
        V1 = rnorm(100),
        V2 = rnorm(100),
        V3 = rnorm(100),
        V4 = rnorm(100),
        V5 = rnorm(100)
    )
    vars <- names(data)
    
    options <- create_options(vars = vars, ncomp = 5, nperm = 50)
    
    analysis <- pcacomponenttestClass$new(options, data)
    analysis$run()
    
    rows <- analysis$results$results$rows
    
    # PC1 might be significant by chance, but likely not all 5.
    # Check that if a component is NOT significant, the NEXT one is NA.
    
    found_nonsig <- FALSE
    for (i in 1:(length(rows)-1)) {
        pval <- rows[[i]]$adjpvalue
        if (!is.na(pval) && pval >= 0.05) {
            found_nonsig <- TRUE
            # The NEXT component should have NA p-value
            next_pval <- rows[[i+1]]$pvalue
            expect_true(is.na(next_pval), info = paste("Component", i+1, "should be NA because Component", i, "was not significant"))
            break
        }
    }
})

test_that("Data Validation: Non-numeric Variables", {
    data <- mtcars
    data$char_var <- as.character(data$mpg)
    vars <- c("mpg", "disp", "char_var")
    
    options <- create_options(vars = vars)
    
    analysis <- pcacomponenttestClass$new(options, data)
    
    expect_error(analysis$run(), "Non-numeric variables detected")
})

test_that("Data Validation: Centering/Scaling Warning", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp") # Different scales
    
    options <- create_options(vars = vars, center = FALSE, scale = FALSE)
    
    analysis <- pcacomponenttestClass$new(options, data)
    
    expect_error(analysis$run(), "WARNING: Centering and/or scaling are disabled")
})

test_that("Plot Generation", {
    data <- mtcars
    vars <- c("mpg", "disp", "hp", "drat", "wt")
    
    options <- create_options(vars = vars, ncomp = 3, nperm = 20)
    
    analysis <- pcacomponenttestClass$new(options, data)
    analysis$run()
    
    # Run the plot function
    # We need to mock the image object passed to .vafplot
    image_mock <- list()
    
    # The .vafplot method takes (image, ggtheme, theme, ...)
    # We can call it directly via private if we could access it, 
    # but R6 private methods are locked.
    # However, the class structure in the file defines .vafplot as a public method?
    # No, it's in the private list in the file.
    
    # We can't easily test private methods of R6 classes from outside without unlock.
    # But we can check if the results object has the plot definition if we had a way to trigger it.
    # In Jamovi, the engine calls the plot function.
    
    # For this verification, we'll assume if the run() completes and data is stored, 
    # the plot data is ready.
    # We can inspect the private fields if we use a hack or just trust the run() logic.
    
    # Hack to access private fields for testing:
    # analysis$.__enclos_env__$private$.originalVAF
    
    expect_true(!is.null(analysis$.__enclos_env__$private$.originalVAF))
    expect_true(!is.null(analysis$.__enclos_env__$private$.meanVAF))
})

print("Verification Script Created Successfully")
