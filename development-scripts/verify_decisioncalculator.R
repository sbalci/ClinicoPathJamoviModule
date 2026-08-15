
# Verification Script for decisioncalculator Function
# Verifies mathematical accuracy, clinical suitability, and robustness

library(testthat)
library(R6)
library(dplyr)

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

OptionNumber <- R6::R6Class("OptionNumber", inherit = Option)
OptionBool <- R6::R6Class("OptionBool", inherit = Option)
OptionString <- R6::R6Class("OptionString", inherit = Option)

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
        initialize = function(options, name, title, visible=TRUE, rows=0, columns=list(), swapRowsColumns=FALSE, clearWith=NULL, refs=NULL) {
            self$visible <- visible
            self$columns <- columns
        },
        addRow = function(rowKey, values) {
            self$rows[[as.character(rowKey)]] <- values
        },
        setRow = function(rowNo=NULL, rowKey=NULL, values) {
            if (!is.null(rowKey)) {
                self$rows[[as.character(rowKey)]] <- values
            } else if (!is.null(rowNo)) {
                self$rows[[rowNo]] <- values
            }
        },
        addFootnote = function(rowNo=NULL, rowKey=NULL, col, note) {
            # Mock footnote addition
        },
        asDF = function() {
            # Convert rows to data frame for checking
            if (length(self$rows) == 0) return(data.frame())
            do.call(rbind, lapply(self$rows, as.data.frame))
        }
    )
)

Image <- R6::R6Class("Image",
    public = list(
        state = NULL,
        visible = TRUE,
        initialize = function(options, name, title, visible=TRUE, width=NULL, height=NULL, renderFun=NULL, ...) {
            self$visible <- visible
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

NoticeType <- list(ERROR = "ERROR", WARNING = "WARNING")

Results <- R6::R6Class("Results",
    public = list(
        welcome = NULL,
        summary = NULL,
        about = NULL,
        assumptions = NULL,
        glossary = NULL,
        cTable = NULL,
        nTable = NULL,
        ratioTable = NULL,
        advancedMetricsTable = NULL,
        epirTable_ratio = NULL,
        epirTable_number = NULL,
        plot1 = NULL,
        multipleCutoffTable = NULL,
        
        initialize = function() {
            self$welcome <- Html$new(options=NULL, name="welcome", title="", visible=TRUE)
            self$summary <- Html$new(options=NULL, name="summary", title="", visible=TRUE)
            self$about <- Html$new(options=NULL, name="about", title="", visible=TRUE)
            self$assumptions <- Html$new(options=NULL, name="assumptions", title="", visible=TRUE)
            self$glossary <- Html$new(options=NULL, name="glossary", title="", visible=TRUE)
            
            self$cTable <- Table$new(options=NULL, name="cTable", title="")
            self$nTable <- Table$new(options=NULL, name="nTable", title="")
            self$ratioTable <- Table$new(options=NULL, name="ratioTable", title="")
            self$advancedMetricsTable <- Table$new(options=NULL, name="advancedMetricsTable", title="")
            self$epirTable_ratio <- Table$new(options=NULL, name="epirTable_ratio", title="")
            self$epirTable_number <- Table$new(options=NULL, name="epirTable_number", title="")
            self$multipleCutoffTable <- Table$new(options=NULL, name="multipleCutoffTable", title="")
            
            self$plot1 <- Image$new(options=NULL, name="plot1", title="")
        },
        insert = function(index, item) {
            # Mock insert for notices
            print(paste("Notice inserted:", item$content))
        }
    )
)

# Mock epiR::epi.tests
mock_epi_tests <- function(dat, conf.level = 0.95) {
    # Return a minimal structure that summary() can handle or return a pre-summarized mock
    # Since the code calls summary(epirresult) then as.data.frame, we'll mock the summary output directly
    # But wait, the code does: epirresult <- epiR::epi.tests(dat); epirresult2 <- summary(epirresult)
    
    # Let's create a mock object that has a summary method
    structure(list(data = dat), class = "epi.tests")
}

# Mock summary.epi.tests
summary.epi.tests <- function(object, ...) {
    # Return a matrix/data.frame consistent with epiR output
    # Columns: statistic, est, lower, upper
    
    # Basic calculations for the mock
    tp <- object$data[1,1]; fp <- object$data[1,2]
    fn <- object$data[2,1]; tn <- object$data[2,2]
    
    stats <- c("ap", "tp", "se", "sp", "diag.ac", "diag.or", "nndx", "youden", "pv.pos", "pv.neg", "lr.pos", "lr.neg", "p.tpdn", "p.tndp", "p.dntp", "p.dptn")
    
    # Dummy values for verification (we verify manual calcs mostly)
    res <- matrix(0.5, nrow = length(stats), ncol = 3)
    colnames(res) <- c("est", "lower", "upper")
    rownames(res) <- stats
    
    # Fill in some key values to ensure they flow through
    res["se", "est"] <- tp / (tp + fn)
    res["sp", "est"] <- tn / (tn + fp)
    
    res
}

# Mock nomogrammer
nomogrammer <- function(...) {
    print("Nomogram generated")
    return("Plot Object")
}

# Mock decisioncalculatorBase
decisioncalculatorBase <- R6::R6Class("decisioncalculatorBase",
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

# Inject mocks into the environment
assign("jmvcore", list(
    Option = Option,
    OptionNumber = OptionNumber,
    OptionBool = OptionBool,
    OptionString = OptionString,
    Html = Html,
    Table = Table,
    Image = Image,
    Notice = Notice,
    NoticeType = NoticeType,
    Group = R6::R6Class("Group"),
    Analysis = R6::R6Class("Analysis")
), envir = .GlobalEnv)

assign("epi.tests", mock_epi_tests, envir = .GlobalEnv)
assign("summary.epi.tests", summary.epi.tests, envir = .GlobalEnv)
assign("nomogrammer", nomogrammer, envir = .GlobalEnv)

# Mock translation function
. <- function(text) text


# 2. Helper Functions -----------------------------------------------------

create_options <- function(
    TP=90, TN=80, FP=30, FN=20,
    pp=FALSE, pprob=0.3,
    ci=FALSE, multiplecuts=FALSE,
    cutoff1="Conservative", tp1=85, fp1=10, tn1=190, fn1=15,
    cutoff2="Aggressive", tp2=95, fp2=25, tn2=175, fn2=5,
    showSummary=FALSE, showAbout=FALSE, showGlossary=FALSE, fnote=FALSE
) {
    list(
        TP = TP, TN = TN, FP = FP, FN = FN,
        pp = pp, pprob = pprob,
        ci = ci,
        fagan = FALSE,
        showWelcome = TRUE,
        showSummary = showSummary,
        showAbout = showAbout,
        showGlossary = showGlossary,
        fnote = fnote,
        multiplecuts = multiplecuts,
        cutoff1 = cutoff1, tp1 = tp1, fp1 = fp1, tn1 = tn1, fn1 = fn1,
        cutoff2 = cutoff2, tp2 = tp2, fp2 = fp2, tn2 = tn2, fn2 = fn2
    )
}

# Load the class definition
source("R/decisioncalculator.b.R")

# 3. Test Cases -----------------------------------------------------------

test_that("Basic Calculations", {
    # TP=90, FP=20, TN=80, FN=10
    # Sens = 90/100 = 0.9
    # Spec = 80/100 = 0.8
    # PPV = 90/110 = 0.818
    # NPV = 80/90 = 0.888
    # LR+ = 0.9 / 0.2 = 4.5
    # LR- = 0.1 / 0.8 = 0.125
    
    options <- create_options(TP=90, FP=20, TN=80, FN=10)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    ratio_table <- analysis$results$ratioTable$rows[[1]]
    
    expect_equal(ratio_table$Sens, 0.9)
    expect_equal(ratio_table$Spec, 0.8)
    expect_equal(ratio_table$PPV, 90/110)
    expect_equal(ratio_table$NPV, 80/90)
    expect_equal(ratio_table$LRP, 4.5)
    expect_equal(ratio_table$LRN, 0.125)
})

test_that("Population Prevalence Adjustment", {
    # TP=90, FP=20, TN=80, FN=10 (Sample Prev = 0.5)
    # Population Prev = 0.1
    # Sens = 0.9, Spec = 0.8
    # PPV = (0.9 * 0.1) / (0.9*0.1 + 0.2*0.9) = 0.09 / (0.09 + 0.18) = 0.09 / 0.27 = 0.333
    # NPV = (0.8 * 0.9) / (0.8*0.9 + 0.1*0.1) = 0.72 / (0.72 + 0.01) = 0.72 / 0.73 = 0.986
    
    options <- create_options(TP=90, FP=20, TN=80, FN=10, pp=TRUE, pprob=0.1)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    ratio_table <- analysis$results$ratioTable$rows[[1]]
    
    expect_equal(ratio_table$PrevalenceD, 0.1)
    expect_equal(ratio_table$PPV, 1/3, tolerance=0.001)
    expect_equal(ratio_table$NPV, 0.72/0.73, tolerance=0.001)
    
    # Post-test probabilities should match PPV/NPV when pp=TRUE
    expect_equal(ratio_table$PostTestProbDisease, ratio_table$PPV)
    expect_equal(ratio_table$PostTestProbHealthy, ratio_table$NPV)
})

test_that("Advanced Metrics", {
    # TP=90, FP=10, TN=90, FN=10
    # Sens = 0.9, Spec = 0.9
    # Youden = 0.9 + 0.9 - 1 = 0.8
    # Balanced Accuracy = (0.9 + 0.9) / 2 = 0.9
    # PPV = 0.9
    # F1 = 2 * (0.9 * 0.9) / (0.9 + 0.9) = 1.62 / 1.8 = 0.9
    
    options <- create_options(TP=90, FP=10, TN=90, FN=10)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    adv_table <- analysis$results$advancedMetricsTable$rows[[1]]
    
    expect_equal(adv_table$youdenIndex, 0.8)
    expect_equal(adv_table$balancedAccuracy, 0.9)
    expect_equal(adv_table$f1Score, 0.9)
})

test_that("Multiple Cut-off Evaluation", {
    # Cutoff 1: TP=85, FP=10, TN=190, FN=15 (Sens=0.85, Spec=0.95)
    # Cutoff 2: TP=95, FP=25, TN=175, FN=5  (Sens=0.95, Spec=0.875)
    
    options <- create_options(
        multiplecuts=TRUE,
        cutoff1="C1", tp1=85, fp1=10, tn1=190, fn1=15,
        cutoff2="C2", tp2=95, fp2=25, tn2=175, fn2=5
    )
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    cutoff_table <- analysis$results$multipleCutoffTable$rows
    
    # Check Row 1 (Cutoff 1)
    c1 <- cutoff_table[[1]]
    expect_equal(c1$cutoffName, "C1")
    expect_equal(c1$sensitivity, 85/100)
    expect_equal(c1$specificity, 190/200)
    
    # Check Row 2 (Cutoff 2)
    c2 <- cutoff_table[[2]]
    expect_equal(c2$cutoffName, "C2")
    expect_equal(c2$sensitivity, 95/100)
    expect_equal(c2$specificity, 175/200)
    
    # Check Row 3 (Current/Reference)
    # Default inputs in create_options are TP=90, TN=80, FP=30, FN=20
    # Sens = 90/110 = 0.818, Spec = 80/110 = 0.727
    c3 <- cutoff_table[[3]]
    expect_equal(c3$cutoffName, "Current (Reference)")
    expect_equal(c3$sensitivity, 90/110)
})

test_that("Input Validation", {
    # Negative counts
    options <- create_options(TP=-1)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    # Should not crash, but likely return early or show notice
    # Since we mocked Notice, we can't easily check if it was inserted, 
    # but we can check if tables are empty or default
    
    # Zero counts
    options <- create_options(TP=0, FP=0, TN=0, FN=0)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    # No disease
    options <- create_options(TP=0, FN=0, TN=50, FP=50)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    # No healthy
    options <- create_options(TP=50, FN=50, TN=0, FP=0)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
})

test_that("Text Outputs", {
    options <- create_options(showSummary=TRUE, showAbout=TRUE, showGlossary=TRUE)
    analysis <- decisioncalculatorClass$new(options)
    analysis$run()
    
    expect_true(!is.null(analysis$results$summary$content))
    expect_true(!is.null(analysis$results$about$content))
    expect_true(!is.null(analysis$results$glossary$content))
})

print("Verification Script Created Successfully")
