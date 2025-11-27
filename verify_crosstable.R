
# Verification Script for crosstable Function

# 1. Mocking jmvcore Environment ------------------------------------------

library(R6)
library(testthat)
library(gtsummary)
library(finalfit)
library(arsenal)
library(tangram)
library(janitor)
library(labelled)

# Mock Option Classes
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

OptionData <- R6Class("OptionData", inherit = Option)
OptionVariable <- R6Class("OptionVariable", inherit = Option)
OptionVariables <- R6Class("OptionVariables", inherit = Option)
OptionList <- R6Class("OptionList", inherit = Option)
OptionBool <- R6Class("OptionBool", inherit = Option)
OptionNumber <- R6Class("OptionNumber", inherit = Option)
OptionInteger <- R6Class("OptionInteger", inherit = Option)

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
        subtitle = NULL,
        todo2 = NULL,
        tablestyle1 = NULL, # arsenal
        tablestyle2 = NULL, # finalfit
        tablestyle3 = NULL, # gtsummary
        tablestyle4 = NULL, # nejm/lancet/hmisc
        qvalueExplanation = NULL,
        testInformation = NULL,
        mantelHaenszelResults = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$subtitle <- Html$new("subtitle")
            self$todo2 <- Html$new("todo2")
            self$tablestyle1 <- Html$new("tablestyle1")
            self$tablestyle2 <- Html$new("tablestyle2")
            self$tablestyle3 <- Html$new("tablestyle3")
            self$tablestyle4 <- Html$new("tablestyle4")
            self$qvalueExplanation <- Html$new("qvalueExplanation")
            self$testInformation <- Html$new("testInformation")
            self$mantelHaenszelResults <- Html$new("mantelHaenszelResults")
        }
    )
)

# Mock Base Class
crosstableBase <- R6Class("crosstableBase",
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

# Mock localization function
. <- function(x) x

# Load the function code
source("R/crosstable.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(vars, group, 
                          sty = "gtsummary", 
                          excl = FALSE, 
                          cont = "mean", 
                          pcat = "chisq",
                          stratify = NULL,
                          mantel_haenszel = FALSE,
                          breslow_day = FALSE,
                          p_adjust = "none") {
    
    list(
        vars = vars,
        group = group,
        sty = sty,
        excl = excl,
        cont = cont,
        pcat = pcat,
        stratify = stratify,
        mantel_haenszel = mantel_haenszel,
        breslow_day = breslow_day,
        p_adjust = p_adjust
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("gtsummary Style Generates Correctly", {
    set.seed(123)
    data <- data.frame(
        Group = factor(sample(c("A", "B"), 100, replace = TRUE)),
        Age = rnorm(100, 50, 10),
        Status = factor(sample(c("Alive", "Dead"), 100, replace = TRUE))
    )
    
    options <- create_options(vars = c("Age", "Status"), group = "Group", sty = "gtsummary")
    analysis <- crosstableClass$new(options, data)
    analysis$run()
    
    # Check if content is populated
    expect_true(!is.null(analysis$results$tablestyle3$content))
    # Check for HTML table structure
    expect_match(analysis$results$tablestyle3$content, "<table")
})

test_that("finalfit Style Generates Correctly", {
    set.seed(123)
    data <- data.frame(
        Group = factor(sample(c("A", "B"), 100, replace = TRUE)),
        Age = rnorm(100, 50, 10),
        Status = factor(sample(c("Alive", "Dead"), 100, replace = TRUE))
    )
    
    options <- create_options(vars = c("Age", "Status"), group = "Group", sty = "finalfit")
    analysis <- crosstableClass$new(options, data)
    analysis$run()
    
    expect_true(!is.null(analysis$results$tablestyle2$content))
    expect_match(analysis$results$tablestyle2$content, "<table")
})

test_that("arsenal Style Generates Correctly", {
    set.seed(123)
    data <- data.frame(
        Group = factor(sample(c("A", "B"), 100, replace = TRUE)),
        Age = rnorm(100, 50, 10),
        Status = factor(sample(c("Alive", "Dead"), 100, replace = TRUE))
    )
    
    options <- create_options(vars = c("Age", "Status"), group = "Group", sty = "arsenal")
    analysis <- crosstableClass$new(options, data)
    analysis$run()
    
    expect_true(!is.null(analysis$results$tablestyle1$content))
})

test_that("Multiple Testing Correction (FDR) Works", {
    set.seed(123)
    data <- data.frame(
        Group = factor(sample(c("A", "B"), 100, replace = TRUE)),
        Var1 = rnorm(100),
        Var2 = rnorm(100),
        Var3 = rnorm(100)
    )
    
    options <- create_options(vars = c("Var1", "Var2", "Var3"), group = "Group", sty = "gtsummary", p_adjust = "BH")
    analysis <- crosstableClass$new(options, data)
    analysis$run()
    
    # Check for q-value explanation
    expect_true(!is.null(analysis$results$qvalueExplanation$content))
    expect_match(analysis$results$qvalueExplanation$content, "Benjamini-Hochberg")
    
    # Check if table contains adjusted p-values (q-values)
    # gtsummary adds a column for q-values
    expect_match(analysis$results$tablestyle3$content, "adjusted p")
})

test_that("Stratified Analysis (Mantel-Haenszel) Works", {
    # Create data suitable for M-H test (Simpson's paradox example)
    data <- data.frame(
        Treatment = factor(c(rep("A", 10), rep("B", 10), rep("A", 10), rep("B", 10))),
        Outcome = factor(c(rep("Success", 8), rep("Failure", 2), rep("Success", 2), rep("Failure", 8),
                           rep("Success", 2), rep("Failure", 8), rep("Success", 8), rep("Failure", 2))),
        Strata = factor(c(rep("Male", 20), rep("Female", 20)))
    )
    
    options <- create_options(vars = "Outcome", group = "Treatment", stratify = "Strata", mantel_haenszel = TRUE)
    analysis <- crosstableClass$new(options, data)
    analysis$run()
    
    # Check for M-H results
    expect_true(!is.null(analysis$results$mantelHaenszelResults$content))
    expect_match(analysis$results$mantelHaenszelResults$content, "Mantel-Haenszel")
    expect_match(analysis$results$mantelHaenszelResults$content, "Common odds ratio")
})

test_that("Data Quality Warnings Are Generated", {
    # Small sample size
    data <- data.frame(
        Group = factor(sample(c("A", "B"), 10, replace = TRUE)),
        Var1 = rnorm(10)
    )
    
    options <- create_options(vars = "Var1", group = "Group")
    analysis <- crosstableClass$new(options, data)
    analysis$run()
    
    # Check for warning in todo2
    expect_match(analysis$results$todo2$content, "Data Quality Warnings")
    expect_match(analysis$results$todo2$content, "Small group detected")
})

print("All tests completed successfully!")
