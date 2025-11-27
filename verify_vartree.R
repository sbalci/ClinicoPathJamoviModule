
# Verification Script for vartree Function

library(R6)
library(testthat)
library(dplyr)
library(janitor)
library(labelled)
# We need vtree and DiagrammeRsvg for the actual function to work
# If they are not installed, we might need to mock them or skip those tests
if (!requireNamespace("vtree", quietly = TRUE)) {
    stop("Package 'vtree' is required for verification.")
}
if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Package 'DiagrammeRsvg' is required for verification.")
}

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

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        todo = NULL,
        text1 = NULL, # Main output
        text2 = NULL, # Pattern table
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$text1 <- Html$new("text1")
            self$text2 <- Html$new("text2")
        }
    )
)

# Mock Base Class
vartreeBase <- R6Class("vartreeBase",
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
        .getRSyntax = function() {
            return("vartree(...)")
        }
    )
)

# Mock jmvcore functions
mock_jmvcore <- new.env()
mock_jmvcore$select <- function(df, columnNames) {
    df[, columnNames, drop = FALSE]
}
mock_jmvcore$naOmit <- function(df) {
    na.omit(df)
}
mock_jmvcore$composeTerm <- function(term) {
    paste0("`", term, "`")
}

jmvcore <- mock_jmvcore

# Mock localization function
. <- function(text) {
    return(text)
}

# Source the implementation
source("R/vartree.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(vars, 
                          percvar = NULL,
                          percvarLevel = NULL,
                          summaryvar = NULL,
                          summarylocation = "leafonly",
                          prunebelow = NULL,
                          pruneLevel1 = NULL,
                          pruneLevel2 = NULL,
                          follow = NULL,
                          followLevel1 = NULL,
                          followLevel2 = NULL,
                          style = "default",
                          horizontal = FALSE,
                          sline = TRUE,
                          varnames = TRUE,
                          nodelabel = TRUE,
                          pct = TRUE,
                          showcount = TRUE,
                          legend = TRUE,
                          pattern = FALSE,
                          sequence = FALSE,
                          ptable = FALSE,
                          mytitle = "Variable Tree",
                          useprunesmaller = FALSE,
                          prunesmaller = 5,
                          showInterpretation = TRUE,
                          maxwidth = 600,
                          excl = FALSE,
                          vp = TRUE) {
    
    list(
        vars = vars,
        percvar = percvar,
        percvarLevel = percvarLevel,
        summaryvar = summaryvar,
        summarylocation = summarylocation,
        prunebelow = prunebelow,
        pruneLevel1 = pruneLevel1,
        pruneLevel2 = pruneLevel2,
        follow = follow,
        followLevel1 = followLevel1,
        followLevel2 = followLevel2,
        style = style,
        horizontal = horizontal,
        sline = sline,
        varnames = varnames,
        nodelabel = nodelabel,
        pct = pct,
        showcount = showcount,
        legend = legend,
        pattern = pattern,
        sequence = sequence,
        ptable = ptable,
        mytitle = mytitle,
        useprunesmaller = useprunesmaller,
        prunesmaller = prunesmaller,
        showInterpretation = showInterpretation,
        maxwidth = maxwidth,
        excl = excl,
        vp = vp
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Basic Functionality with mtcars", {
    data <- mtcars
    # Convert to factors for vtree
    data$cyl <- as.factor(data$cyl)
    data$gear <- as.factor(data$gear)
    data$am <- factor(data$am, labels = c("Auto", "Manual"))
    
    vars <- c("cyl", "gear", "am")
    
    options <- create_options(vars = vars)
    
    analysis <- vartreeClass$new(options, data)
    analysis$run()
    
    # Check if output is generated
    expect_true(!is.null(analysis$results$text1$content))
    # Check for SVG content
    expect_true(grepl("<svg", analysis$results$text1$content))
})

test_that("Summary Variable", {
    data <- mtcars
    data$cyl <- as.factor(data$cyl)
    data$gear <- as.factor(data$gear)
    
    vars <- c("cyl", "gear")
    summaryvar <- "mpg"
    
    options <- create_options(vars = vars, summaryvar = summaryvar, summarylocation = "allnodes")
    
    analysis <- vartreeClass$new(options, data)
    analysis$run()
    
    # Check if interpretation mentions summary
    expect_true(grepl("Statistical summaries", analysis$results$text1$content))
    # Check if SVG contains summary variable name
    expect_true(grepl("mpg", analysis$results$text1$content))
})

test_that("Pruning Logic", {
    data <- mtcars
    data$cyl <- as.factor(data$cyl)
    data$gear <- as.factor(data$gear)
    
    vars <- c("cyl", "gear")
    prunebelow <- "cyl"
    pruneLevel1 <- "4" # Prune below 4 cylinders
    
    options <- create_options(vars = vars, prunebelow = prunebelow, pruneLevel1 = pruneLevel1)
    
    analysis <- vartreeClass$new(options, data)
    analysis$run()
    
    # We can't easily check the tree structure without parsing SVG, 
    # but we can check if it runs without error and produces output.
    expect_true(!is.null(analysis$results$text1$content))
})

test_that("Data Validation: Missing Values", {
    data <- mtcars
    data$cyl <- as.factor(data$cyl)
    data$gear <- as.factor(data$gear)
    # Introduce missing values
    data$cyl[1:5] <- NA
    
    vars <- c("cyl", "gear")
    
    # Exclude missing
    options <- create_options(vars = vars, excl = TRUE)
    
    analysis <- vartreeClass$new(options, data)
    analysis$run()
    
    # Check for exclusion warning in interpretation
    expect_true(grepl("CASE EXCLUSION", analysis$results$text1$content))
})

test_that("Styles", {
    data <- mtcars
    data$cyl <- as.factor(data$cyl)
    
    vars <- c("cyl")
    
    options <- create_options(vars = vars, style = "clean")
    
    analysis <- vartreeClass$new(options, data)
    analysis$run()
    
    expect_true(grepl("Clean style applied", analysis$results$text1$content))
})

print("Verification Script Created Successfully")
