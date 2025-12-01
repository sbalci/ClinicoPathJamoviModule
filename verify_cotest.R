
# Verification Script for cotest Function

library(R6)
library(testthat)

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
        visible = TRUE,
        initialize = function(name) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

# Mock Image Class
Image <- R6Class("Image",
    public = list(
        name = NULL,
        state = NULL,
        width = NULL,
        height = NULL,
        initialize = function(name) {
            self$name <- name
        },
        setState = function(state) {
            self$state <- state
        },
        setSize = function(width, height) {
            self$width <- width
            self$height <- height
        }
    )
)

# Mock Table Class
Table <- R6Class("Table",
    public = list(
        name = NULL,
        rows = list(),
        columns = list(),
        footnotes = list(),
        initialize = function(name) {
            self$name <- name
        },
        addRow = function(rowKey, values) {
            self$rows[[rowKey]] <- values
        },
        setRow = function(rowKey, values) {
            self$rows[[rowKey]] <- values
        },
        addFootnote = function(rowKey, col, note) {
            self$footnotes[[paste(rowKey, col, sep="_")]] <- note
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        instructions = NULL,
        notices = NULL,
        testParamsTable = NULL,
        cotestResultsTable = NULL,
        dependenceInfo = NULL,
        dependenceExplanation = NULL,
        explanation = NULL,
        plot1 = NULL,
        
        initialize = function() {
            self$instructions <- Html$new("instructions")
            self$notices <- Html$new("notices")
            self$testParamsTable <- Table$new("testParamsTable")
            self$cotestResultsTable <- Table$new("cotestResultsTable")
            self$dependenceInfo <- Html$new("dependenceInfo")
            self$dependenceExplanation <- Html$new("dependenceExplanation")
            self$explanation <- Html$new("explanation")
            self$plot1 <- Image$new("plot1")
        }
    )
)

# Mock Base Class
cotestBase <- R6Class("cotestBase",
    public = list(
        options = NULL,
        results = NULL,
        initialize = function(options) {
            self$options <- options
            self$results <- Results$new()
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .checkpoint = function() {}
    )
)

# Mock nomogrammer function
nomogrammer <- function(...) {
    return("Mock Nomogram Plot")
}

# Source the implementation
source("R/cotest.b.R")

# 2. Helper Functions -----------------------------------------------------

create_options <- function(test1_sens = 0.8, test1_spec = 0.9,
                          test2_sens = 0.75, test2_spec = 0.95,
                          prevalence = 0.1, indep = TRUE,
                          cond_dep_pos = 0.05, cond_dep_neg = 0.05,
                          preset = "custom", fnote = FALSE, fagan = FALSE) {
    
    list(
        test1_sens = test1_sens,
        test1_spec = test1_spec,
        test2_sens = test2_sens,
        test2_spec = test2_spec,
        prevalence = prevalence,
        indep = indep,
        cond_dep_pos = cond_dep_pos,
        cond_dep_neg = cond_dep_neg,
        preset = preset,
        fnote = fnote,
        fagan = fagan
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Independent Tests Calculation", {
    # Manual calculation check
    # Test 1: Sens=0.8, Spec=0.9 -> PLR=8, NLR=0.222
    # Test 2: Sens=0.75, Spec=0.95 -> PLR=15, NLR=0.263
    # Prev=0.1 -> Pre-odds=0.111
    
    options <- create_options(
        test1_sens = 0.8, test1_spec = 0.9,
        test2_sens = 0.75, test2_spec = 0.95,
        prevalence = 0.1, indep = TRUE
    )
    
    analysis <- cotestClass$new(options)
    analysis$run()
    
    # Check Both Positive
    # Combined PLR = 8 * 15 = 120
    # Post-odds = 0.111 * 120 = 13.33
    # Post-prob = 13.33 / 14.33 = 0.930
    
    res_both_pos <- analysis$results$cotestResultsTable$rows[["both_pos"]]
    expect_equal(res_both_pos$postProb, 0.9302326, tolerance = 1e-4)
    
    # Check Both Negative
    # Combined NLR = 0.222 * 0.263 = 0.0584
    # Post-odds = 0.111 * 0.0584 = 0.00649
    # Post-prob = 0.00649 / 1.00649 = 0.00645
    
    res_both_neg <- analysis$results$cotestResultsTable$rows[["both_neg"]]
    expect_equal(res_both_neg$postProb, 0.006456, tolerance = 1e-4)
})

test_that("Dependent Tests Calculation", {
    # Using preset values for HPV+Pap (known dependent)
    # Sens1=0.95, Spec1=0.85
    # Sens2=0.80, Spec2=0.95
    # Prev=0.05, DepPos=0.15, DepNeg=0.10
    
    options <- create_options(
        preset = "hpv_pap"
    )
    
    analysis <- cotestClass$new(options)
    analysis$run()
    
    # Check if preset values were loaded correctly
    # Note: Logic inside .run() overrides options with preset values
    # We check the results which reflect these values
    
    # Verify dependence info is generated
    expect_true(!is.null(analysis$results$dependenceInfo$content))
    expect_true(grepl("0.15", analysis$results$dependenceInfo$content)) # Check for dep parameter
})

test_that("Preset Loading", {
    options <- create_options(preset = "psa_dre")
    analysis <- cotestClass$new(options)
    analysis$run()
    
    # PSA+DRE preset has prevalence 0.20
    # Check explanation text for prevalence
    expect_true(grepl("20.0%", analysis$results$explanation$content))
})

test_that("Input Validation", {
    # Invalid sensitivity
    options <- create_options(test1_sens = 1.5)
    analysis <- cotestClass$new(options)
    
    expect_error(analysis$run(), "Test 1 sensitivity must be between 0 and 1")
    
    # Invalid prevalence
    options <- create_options(prevalence = -0.1)
    analysis <- cotestClass$new(options)
    
    expect_error(analysis$run(), "Disease prevalence must be between 0 and 1")
})

test_that("Either Test Positive Rule", {
    # Independent case
    # P(Either+|D) = 1 - (1-Sens1)(1-Sens2)
    # Sens1=0.8, Sens2=0.75 -> 1 - (0.2)(0.25) = 1 - 0.05 = 0.95
    # P(Either+|nD) = 1 - Spec1*Spec2
    # Spec1=0.9, Spec2=0.95 -> 1 - (0.9)(0.95) = 1 - 0.855 = 0.145
    # PLR = 0.95 / 0.145 = 6.55
    # Prev=0.1 -> Pre-odds=0.111
    # Post-odds = 0.111 * 6.55 = 0.728
    # Post-prob = 0.728 / 1.728 = 0.421
    
    options <- create_options(
        test1_sens = 0.8, test1_spec = 0.9,
        test2_sens = 0.75, test2_spec = 0.95,
        prevalence = 0.1, indep = TRUE
    )
    
    analysis <- cotestClass$new(options)
    analysis$run()
    
    res_either <- analysis$results$cotestResultsTable$rows[["either_pos"]]
    expect_equal(res_either$postProb, 0.421, tolerance = 1e-3)
})

test_that("Dependent either-positive uses realized joint probabilities", {
    options <- create_options(
        test1_sens = 0.95, test1_spec = 0.95,
        test2_sens = 0.95, test2_spec = 0.95,
        prevalence = 0.1, indep = FALSE,
        cond_dep_pos = 0.9, cond_dep_neg = 0.9
    )
    
    analysis <- cotestClass$new(options)
    analysis$run()
    
    # Recompute using the same internal dependent routine to ensure coherence
    pretest_odds <- options$prevalence / (1 - options$prevalence)
    dep <- analysis$.__enclos_env__$private$.calculateDependentTestProbabilities(
        options$test1_sens, options$test1_spec,
        options$test2_sens, options$test2_spec,
        options$cond_dep_pos, options$cond_dep_neg,
        pretest_odds
    )
    p_either_pos_D <- 1 - dep$p_both_neg_D
    p_either_pos_nD <- 1 - dep$p_both_neg_nD
    lr_either <- p_either_pos_D / p_either_pos_nD
    post_odds <- pretest_odds * lr_either
    expected <- post_odds / (1 + post_odds)
    
    res_either <- analysis$results$cotestResultsTable$rows[["either_pos"]]
    expect_equal(res_either$postProb, expected, tolerance = 1e-6)
})

test_that("Negative dependence parameters are accepted and reported", {
    options <- create_options(
        test1_sens = 0.85, test1_spec = 0.9,
        test2_sens = 0.8, test2_spec = 0.9,
        prevalence = 0.15, indep = FALSE,
        cond_dep_pos = -0.4, cond_dep_neg = -0.4
    )
    
    analysis <- cotestClass$new(options)
    analysis$run()
    
    expect_true(grepl("Realized phi", analysis$results$dependenceInfo$content))
})

test_that("Fagan plot uses parallel rule (either-positive, both-negative)", {
    options <- create_options(
        test1_sens = 0.8, test1_spec = 0.9,
        test2_sens = 0.75, test2_spec = 0.95,
        prevalence = 0.1, indep = TRUE,
        fagan = TRUE
    )
    
    analysis <- cotestClass$new(options)
    analysis$run()
    
    state <- analysis$results$plot1$state
    p_either_pos_D <- 1 - ((1 - options$test1_sens) * (1 - options$test2_sens))
    p_either_pos_nD <- 1 - (options$test1_spec * options$test2_spec)
    expect_equal(state$Plr_PositiveRule, p_either_pos_D / p_either_pos_nD, tolerance = 1e-6)
    expect_equal(state$Nlr_NegativeRule, (1 - options$test1_sens) * (1 - options$test2_sens) / (options$test1_spec * options$test2_spec), tolerance = 1e-6)
})

print("Verification Script Created Successfully")
