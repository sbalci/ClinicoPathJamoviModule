
# Verification Script for decisioncombine Function

library(testthat)
library(R6)
library(dplyr)
library(forcats)

# 1. Mocks for jmvcore and dependencies -----------------------------------

# Mock jmvcore classes
Option <- R6::R6Class("Option",
    public = list(
        value = NULL,
        initialize = function(value=NULL, default=NULL) {
            self$value <- if(is.null(value)) default else value
        }
    )
)

OptionVariable <- R6::R6Class("OptionVariable", inherit = Option)
OptionLevel <- R6::R6Class("OptionLevel", inherit = Option)
OptionBool <- R6::R6Class("OptionBool", inherit = Option)
OptionList <- R6::R6Class("OptionList", inherit = Option)

Html <- R6::R6Class("Html",
    public = list(
        content = NULL,
        visible = TRUE,
        initialize = function(options, name, title, visible) {
            self$visible <- visible
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

Table <- R6::R6Class("Table",
    public = list(
        rows = list(),
        columns = list(),
        visible = TRUE,
        rowCount = 0,
        initialize = function(options, name, title, visible=TRUE, rows=0, columns=list(), swapRowsColumns=FALSE, clearWith=NULL, refs=NULL) {
            self$visible <- visible
            self$columns <- columns
        },
        addRow = function(rowKey, values) {
            self$rows[[as.character(rowKey)]] <- values
            self$rowCount <- length(self$rows)
        },
        setRow = function(rowNo=NULL, rowKey=NULL, values) {
            if (!is.null(rowKey)) {
                self$rows[[as.character(rowKey)]] <- values
            } else if (!is.null(rowNo)) {
                self$rows[[rowNo]] <- values
            }
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            do.call(rbind, lapply(self$rows, as.data.frame))
        }
    )
)

Image <- R6::R6Class("Image",
    public = list(
        state = NULL,
        visible = TRUE,
        renderFun = NULL,
        initialize = function(options, name, title, visible=TRUE, width=NULL, height=NULL, renderFun=NULL, ...) {
            self$visible <- visible
            self$renderFun <- renderFun
        },
        setState = function(state) {
            self$state <- state
        }
    )
)

Notice <- R6::R6Class("Notice",
    public = list(
        content = NULL,
        initialize = function(options, name, type) {},
        setContent = function(content) {
            self$content <- content
        }
    )
)

NoticeType <- list(ERROR = "ERROR", WARNING = "WARNING", INFO = "INFO")

Results <- R6::R6Class("Results",
    public = list(
        combinationTable = NULL,
        combinationTableCI = NULL,
        goldFreqTable = NULL,
        crossTabTable = NULL,
        individualTest1 = NULL,
        individualTest2 = NULL,
        individualTest3 = NULL,
        barPlot = NULL,
        heatmapPlot = NULL,
        forestPlot = NULL,
        decisionTreePlot = NULL,
        recommendationTable = NULL,
        
        initialize = function() {
            self$combinationTable <- Table$new(options=NULL, name="combinationTable", title="")
            self$combinationTableCI <- Table$new(options=NULL, name="combinationTableCI", title="")
            self$goldFreqTable <- Table$new(options=NULL, name="goldFreqTable", title="")
            self$crossTabTable <- Table$new(options=NULL, name="crossTabTable", title="")
            self$recommendationTable <- Table$new(options=NULL, name="recommendationTable", title="")
            
            # Mock individual test groups
            self$individualTest1 <- list(
                test1Contingency = Table$new(options=NULL, name="test1Contingency", title=""),
                test1Stats = Table$new(options=NULL, name="test1Stats", title="")
            )
            self$individualTest2 <- list(
                test2Contingency = Table$new(options=NULL, name="test2Contingency", title=""),
                test2Stats = Table$new(options=NULL, name="test2Stats", title="")
            )
            self$individualTest3 <- list(
                test3Contingency = Table$new(options=NULL, name="test3Contingency", title=""),
                test3Stats = Table$new(options=NULL, name="test3Stats", title="")
            )
            
            self$barPlot <- Image$new(options=NULL, name="barPlot", title="")
            self$heatmapPlot <- Image$new(options=NULL, name="heatmapPlot", title="")
            self$forestPlot <- Image$new(options=NULL, name="forestPlot", title="")
            self$decisionTreePlot <- Image$new(options=NULL, name="decisionTreePlot", title="")
        },
        insert = function(index, item) {
            print(paste("Notice inserted:", item$content))
        },
        setColumn = function(name, values) {
            print(paste("Column set:", name))
        }
    )
)

# Mock epiR::epi.tests
mock_epi_tests <- function(dat, conf.level = 0.95) {
    structure(list(data = dat), class = "epi.tests")
}

summary.epi.tests <- function(object, ...) {
    tp <- object$data[1,1]; fp <- object$data[1,2]
    fn <- object$data[2,1]; tn <- object$data[2,2]
    
    # Simple calculation for verification
    se <- tp / (tp + fn)
    sp <- tn / (tn + fp)
    ppv <- tp / (tp + fp)
    npv <- tn / (tn + fn)
    
    # Construct detail data frame as expected by the code
    data.frame(
        statistic = c("se", "sp", "pv.pos", "pv.neg"),
        est = c(se, sp, ppv, npv),
        lower = c(0,0,0,0),
        upper = c(1,1,1,1)
    )
}

# Mock ggplot2
mock_ggplot <- function(...) { structure(list(...), class = "ggplot") }
mock_aes <- function(...) { list(...) }
mock_geom_bar <- function(...) { list(...) }
mock_geom_tile <- function(...) { list(...) }
mock_geom_point <- function(...) { list(...) }
mock_geom_errorbarh <- function(...) { list(...) }
mock_geom_text <- function(...) { list(...) }
mock_scale_fill_gradient2 <- function(...) { list(...) }
mock_scale_y_continuous <- function(...) { list(...) }
mock_scale_x_continuous <- function(...) { list(...) }
mock_labs <- function(...) { list(...) }
mock_theme_minimal <- function(...) { list(...) }
mock_theme <- function(...) { list(...) }
mock_element_text <- function(...) { list(...) }
mock_facet_wrap <- function(...) { list(...) }

# Inject mocks
assign("jmvcore", list(
    Option = Option,
    OptionVariable = OptionVariable,
    OptionLevel = OptionLevel,
    OptionBool = OptionBool,
    OptionList = OptionList,
    Html = Html,
    Table = Table,
    Image = Image,
    Notice = Notice,
    NoticeType = NoticeType,
    Group = R6::R6Class("Group"),
    Analysis = R6::R6Class("Analysis"),
    resolveQuo = function(x) x,
    enquo = function(x) x,
    marshalData = function(...) data.frame(),
    naOmit = function(x) na.omit(x)
), envir = .GlobalEnv)

assign("epi.tests", mock_epi_tests, envir = .GlobalEnv)
# Note: summary.epi.tests is S3, so it should be found automatically if defined in global env
# But we might need to ensure epiR namespace doesn't override it if loaded. 
# Since we don't load epiR, it should be fine. But the code imports epiR.
# We will mock the package function call directly if possible or rely on S3 dispatch.
# The code calls epiR::epi.tests. We can mock that.
# But it calls as.data.frame(result$detail). 
# Our mock_epi_tests returns a list where we can put $detail directly.
mock_epi_tests_direct <- function(dat, conf.level = 0.95) {
    tp <- dat[1,1]; fp <- dat[1,2]
    fn <- dat[2,1]; tn <- dat[2,2]
    
    se <- if((tp+fn)>0) tp/(tp+fn) else NA
    sp <- if((tn+fp)>0) tn/(tn+fp) else NA
    ppv <- if((tp+fp)>0) tp/(tp+fp) else NA
    npv <- if((tn+fn)>0) tn/(tn+fn) else NA
    
    detail <- data.frame(
        statistic = c("se", "sp", "pv.pos", "pv.neg"),
        est = c(se, sp, ppv, npv)
    )
    list(detail = detail)
}
assign("epi.tests", mock_epi_tests_direct, envir = .GlobalEnv)

# Mock ggplot2 functions in a list to attach or just assign to global
ggplot2_mocks <- list(
    ggplot = mock_ggplot,
    aes = mock_aes,
    geom_bar = mock_geom_bar,
    geom_tile = mock_geom_tile,
    geom_point = mock_geom_point,
    geom_errorbarh = mock_geom_errorbarh,
    geom_text = mock_geom_text,
    scale_fill_gradient2 = mock_scale_fill_gradient2,
    scale_y_continuous = mock_scale_y_continuous,
    scale_x_continuous = mock_scale_x_continuous,
    labs = mock_labs,
    theme_minimal = mock_theme_minimal,
    theme = mock_theme,
    element_text = mock_element_text,
    facet_wrap = mock_facet_wrap
)
# We need to make sure the code finds these. The code uses ggplot2::ggplot.
# We can't easily mock `::` calls without mocking the package.
# However, we can use `mockery` or just define a fake package environment.
# For simplicity, we'll assume the code might use `library(ggplot2)` or we can try to overwrite the namespace if loaded.
# Or simpler: we just check if the plot functions run without error, assuming ggplot2 is installed.
# If ggplot2 is installed, we can just let it run. The output will be a plot object.
# We just need to verify the data passed to it.
# But the code uses `print(p)`. We can mock `print`? No, that's risky.
# Let's just let ggplot2 run if available.

# Mock translation
. <- function(text) text


# Mock decisioncombineBase
decisioncombineBase <- R6::R6Class("decisioncombineBase",
    public = list(
        options = NULL,
        results = NULL,
        data = NULL,
        initialize = function(options, data=NULL) {
            self$options <- options
            self$results <- Results$new()
            self$data <- data
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .checkpoint = function() {}
    )
)

# Load class
source("R/decisioncombine.b.R")

# 2. Helper Functions -----------------------------------------------------

create_options <- function(
    gold="gold", goldPositive="Pos",
    test1="test1", test1Positive="Pos",
    test2=NULL, test2Positive=NULL,
    test3=NULL, test3Positive=NULL,
    showIndividual=FALSE, showFrequency=FALSE, showRecommendation=FALSE,
    addPatternToData=FALSE
) {
    list(
        gold = gold, goldPositive = goldPositive,
        test1 = test1, test1Positive = test1Positive,
        test2 = test2, test2Positive = test2Positive,
        test3 = test3, test3Positive = test3Positive,
        showIndividual = showIndividual,
        showFrequency = showFrequency,
        showBarPlot = FALSE,
        showHeatmap = FALSE,
        showForest = FALSE,
        showDecisionTree = FALSE,
        showRecommendation = showRecommendation,
        addPatternToData = addPatternToData,
        filterStatistic = "all",
        filterPattern = "all"
    )
}

create_data_2tests <- function(n=100) {
    set.seed(123)
    gold <- sample(c("Pos", "Neg"), n, replace=TRUE)
    # Test 1 correlates with gold
    test1 <- ifelse(gold=="Pos", 
                   sample(c("Pos", "Neg"), n, replace=TRUE, prob=c(0.8, 0.2)),
                   sample(c("Pos", "Neg"), n, replace=TRUE, prob=c(0.2, 0.8)))
    # Test 2 correlates with gold
    test2 <- ifelse(gold=="Pos", 
                   sample(c("Pos", "Neg"), n, replace=TRUE, prob=c(0.7, 0.3)),
                   sample(c("Pos", "Neg"), n, replace=TRUE, prob=c(0.3, 0.7)))
    
    data.frame(gold=factor(gold), test1=factor(test1), test2=factor(test2))
}

create_data_3tests <- function(n=100) {
    d <- create_data_2tests(n)
    # Test 3
    d$test3 <- ifelse(d$gold=="Pos", 
                     sample(c("Pos", "Neg"), n, replace=TRUE, prob=c(0.9, 0.1)),
                     sample(c("Pos", "Neg"), n, replace=TRUE, prob=c(0.1, 0.9)))
    d$test3 <- factor(d$test3)
    d
}

# 3. Test Cases -----------------------------------------------------------

test_that("2-Test Combination Analysis", {
    data <- create_data_2tests(200)
    options <- create_options(test2="test2", test2Positive="Pos")
    
    analysis <- decisioncombineClass$new(options, data=data)
    analysis$run()
    
    # Check Combination Table
    comb_table <- analysis$results$combinationTable$asDF()
    
    # Should have 4 patterns (+/+, +/-, -/+, -/-) + 2 strategies (Parallel, Serial)
    # Total 6 rows
    expect_equal(nrow(comb_table), 6)
    
    patterns <- comb_table$pattern
    expect_true(all(c("+/+", "+/-", "-/+", "-/-") %in% patterns))
    expect_true("Parallel (≥1 pos)" %in% patterns)
    expect_true("Serial (all pos)" %in% patterns)
    
    # Verify Parallel Strategy Logic
    # Parallel is Positive if T1=Pos OR T2=Pos
    # Manual check
    t1_pos <- data$test1 == "Pos"
    t2_pos <- data$test2 == "Pos"
    parallel_pos <- t1_pos | t2_pos
    
    tp_manual <- sum(parallel_pos & data$gold == "Pos")
    fp_manual <- sum(parallel_pos & data$gold == "Neg")
    
    parallel_row <- comb_table[comb_table$pattern == "Parallel (≥1 pos)", ]
    expect_equal(parallel_row$tp, tp_manual)
    expect_equal(parallel_row$fp, fp_manual)
})

test_that("3-Test Combination Analysis", {
    data <- create_data_3tests(200)
    options <- create_options(
        test2="test2", test2Positive="Pos",
        test3="test3", test3Positive="Pos"
    )
    
    analysis <- decisioncombineClass$new(options, data=data)
    analysis$run()
    
    comb_table <- analysis$results$combinationTable$asDF()
    
    # Should have 8 patterns + 3 strategies (Parallel, Serial, Majority)
    # Total 11 rows
    expect_equal(nrow(comb_table), 11)
    
    patterns <- comb_table$pattern
    expect_true("+/+/+" %in% patterns)
    expect_true("-/-/-" %in% patterns)
    expect_true("Majority (≥2/3 pos)" %in% patterns)
    
    # Verify Majority Rule Logic
    t1_pos <- data$test1 == "Pos"
    t2_pos <- data$test2 == "Pos"
    t3_pos <- data$test3 == "Pos"
    majority_pos <- (as.integer(t1_pos) + as.integer(t2_pos) + as.integer(t3_pos)) >= 2
    
    tp_manual <- sum(majority_pos & data$gold == "Pos")
    
    majority_row <- comb_table[comb_table$pattern == "Majority (≥2/3 pos)", ]
    expect_equal(majority_row$tp, tp_manual)
})

test_that("Individual Test Analysis", {
    data <- create_data_2tests(100)
    options <- create_options(test2="test2", test2Positive="Pos", showIndividual=TRUE)
    
    analysis <- decisioncombineClass$new(options, data=data)
    analysis$run()
    
    # Check Test 1 Stats
    t1_stats <- analysis$results$individualTest1$test1Stats$asDF()
    expect_true(nrow(t1_stats) > 0)
    expect_true("Sensitivity" %in% t1_stats$statistic)
})

test_that("Input Validation", {
    # Missing Data
    options <- create_options()
    analysis <- decisioncombineClass$new(options, data=NULL)
    analysis$run()
    # Should insert error notice
    # We can't easily check notice content in this mock setup without capturing stdout or modifying mock
    # But we can check that no results were generated
    expect_equal(analysis$results$combinationTable$rowCount, 0)
})

test_that("Recommendation Feature", {
    data <- create_data_2tests(100)
    options <- create_options(test2="test2", test2Positive="Pos", showRecommendation=TRUE)
    
    analysis <- decisioncombineClass$new(options, data=data)
    analysis$run()
    
    rec_table <- analysis$results$recommendationTable$asDF()
    expect_equal(nrow(rec_table), 1)
    expect_true(!is.na(rec_table$youden))
})

print("Verification Script Created Successfully")
