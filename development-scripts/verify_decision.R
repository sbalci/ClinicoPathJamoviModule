
# Verification Script for decision Function

library(R6)
library(testthat)
library(dplyr)

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
        note = NULL,
        initialize = function(name) {
            self$name <- name
        },
        addRow = function(rowKey, values) {
            self$rows[[as.character(rowKey)]] <- values
        },
        setRow = function(rowKey, values, rowNo=NULL) {
            if (!is.null(rowNo)) {
                self$rows[[as.character(rowNo)]] <- values
            } else {
                self$rows[[as.character(rowKey)]] <- values
            }
        },
        addFootnote = function(rowKey, col, note, rowNo=NULL) {
            key <- if(!is.null(rowNo)) as.character(rowNo) else as.character(rowKey)
            self$footnotes[[paste(key, col, sep="_")]] <- note
        },
        setNote = function(key, note) {
            self$note <- note
        },
        clear = function() {
            self$rows <- list()
        },
        getColumn = function(name) {
            # Mock column object
            list(
                setTitle = function(title) {},
                setSuperTitle = function(title) {}
            )
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        welcome = NULL,
        rawContingency = NULL,
        rawCounts = NULL,
        cTable = NULL,
        nTable = NULL,
        ratioTable = NULL,
        missingDataSummary = NULL,
        epirTable_ratio = NULL,
        epirTable_number = NULL,
        plot1 = NULL,
        naturalLanguageSummary = NULL,
        clinicalInterpretation = NULL,
        reportTemplate = NULL,
        aboutAnalysis = NULL,
        misclassifiedHeading = NULL,
        confusionMatrixSummary = NULL,
        falsePositiveTable = NULL,
        falseNegativeTable = NULL,
        misclassificationInterpretation = NULL,
        saveClassifications = NULL,
        
        initialize = function() {
            self$welcome <- Html$new("welcome")
            self$rawContingency <- Table$new("rawContingency")
            self$rawCounts <- Table$new("rawCounts")
            self$cTable <- Table$new("cTable")
            self$nTable <- Table$new("nTable")
            self$ratioTable <- Table$new("ratioTable")
            self$missingDataSummary <- Html$new("missingDataSummary")
            self$epirTable_ratio <- Table$new("epirTable_ratio")
            self$epirTable_number <- Table$new("epirTable_number")
            self$plot1 <- Image$new("plot1")
            self$naturalLanguageSummary <- Html$new("naturalLanguageSummary")
            self$clinicalInterpretation <- Html$new("clinicalInterpretation")
            self$reportTemplate <- Html$new("reportTemplate")
            self$aboutAnalysis <- Html$new("aboutAnalysis")
            self$misclassifiedHeading <- Html$new("misclassifiedHeading")
            self$confusionMatrixSummary <- Table$new("confusionMatrixSummary")
            self$falsePositiveTable <- Table$new("falsePositiveTable")
            self$falseNegativeTable <- Table$new("falseNegativeTable")
            self$misclassificationInterpretation <- Html$new("misclassificationInterpretation")
            self$saveClassifications <- list(setValue = function(val) {})
        },
        insert = function(index, item) {
            # Mock insert method
        }
    )
)

# Mock Base Class
decisionBase <- R6Class("decisionBase",
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

# Mock jmvcore namespace functions
jmvcore <- list(
    Notice = R6Class("Notice", 
        public = list(
            initialize = function(...) {},
            setContent = function(...) {}
        )
    ),
    NoticeType = list(ERROR = "ERROR", WARNING = "WARNING", INFO = "INFO", STRONG_WARNING = "STRONG_WARNING"),
    constructFormula = function(terms) terms,
    decomposeFormula = function(formula) formula,
    naOmit = function(data) na.omit(data)
)

# Mock epiR::epi.tests
mock_epi_tests <- function(dat, ...) {
    # Return mock structure matching epiR output
    list(
        detail = data.frame(
            statistic = c("se", "sp", "pv.pos", "pv.neg", "lr.pos", "lr.neg"),
            est = c(0.8, 0.9, 0.88, 0.82, 8.0, 0.22),
            lower = c(0.7, 0.8, 0.8, 0.7, 5.0, 0.1),
            upper = c(0.9, 0.95, 0.95, 0.9, 12.0, 0.4),
            stringsAsFactors = FALSE
        )
    )
}

# Mock nomogrammer
nomogrammer <- function(...) {
    return("Mock Nomogram Plot")
}

# Source the implementation
source("R/decision.b.R")

# Inject mocks into the environment where decisionClass is defined
assign("epi.tests", mock_epi_tests, envir = .GlobalEnv)

# Mock translation function
. <- function(text) text


# 2. Helper Functions -----------------------------------------------------

create_test_data <- function(n_tp=80, n_fp=10, n_fn=20, n_tn=90) {
    # Create a dataset with known confusion matrix
    # Gold: Positive/Negative
    # Test: Positive/Negative
    
    gold <- c(rep("Positive", n_tp + n_fn), rep("Negative", n_fp + n_tn))
    test <- c(rep("Positive", n_tp), rep("Negative", n_fn), 
              rep("Positive", n_fp), rep("Negative", n_tn))
    
    data.frame(
        Gold = factor(gold, levels = c("Positive", "Negative")),
        Test = factor(test, levels = c("Positive", "Negative")),
        stringsAsFactors = FALSE
    )
}

create_options <- function(gold="Gold", newtest="Test", 
                          goldPositive="Positive", testPositive="Positive",
                          goldNegative="Negative", testNegative="Negative",
                          pp=FALSE, pprob=0.3, ci=FALSE, fagan=FALSE,
                          showMisclassified=FALSE, maxCasesShow=50,
                          showNaturalLanguage=FALSE, showClinicalInterpretation=FALSE,
                          showReportTemplate=FALSE, showAboutAnalysis=FALSE,
                          od=FALSE, fnote=FALSE, saveClassifications=NULL) {
    
    list(
        gold = gold,
        newtest = newtest,
        goldPositive = goldPositive,
        testPositive = testPositive,
        goldNegative = goldNegative,
        testNegative = testNegative,
        pp = pp,
        pprob = pprob,
        ci = ci,
        fagan = fagan,
        showMisclassified = showMisclassified,
        maxCasesShow = maxCasesShow,
        showNaturalLanguage = showNaturalLanguage,
        showClinicalInterpretation = showClinicalInterpretation,
        showReportTemplate = showReportTemplate,
        showAboutAnalysis = showAboutAnalysis,
        od = od,
        fnote = fnote,
        saveClassifications = saveClassifications
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Basic Calculations", {
    # TP=80, FP=10, FN=20, TN=90
    # Total = 200
    # DiseaseP = 100, DiseaseN = 100
    # Sens = 80/100 = 0.8
    # Spec = 90/100 = 0.9
    # PPV = 80/90 = 0.888...
    # NPV = 90/110 = 0.818...
    # LR+ = 0.8 / 0.1 = 8
    # LR- = 0.2 / 0.9 = 0.222...
    
    data <- create_test_data(80, 10, 20, 90)
    options <- create_options()
    
    analysis <- decisionClass$new(options, data)
    analysis$run()
    
    res <- analysis$results$ratioTable$rows[["1"]]
    
    expect_equal(res$Sens, 0.8)
    expect_equal(res$Spec, 0.9)
    expect_equal(res$PPV, 80/90)
    expect_equal(res$NPV, 90/110)
    expect_equal(res$LRP, 8)
    expect_equal(res$LRN, 0.2/0.9)
})

test_that("Population Prevalence Adjustment", {
    # Using same data but with population prevalence 0.05
    # Sens=0.8, Spec=0.9
    # Prior = 0.05
    # Post+ = (0.05*0.8) / (0.05*0.8 + 0.95*0.1) = 0.04 / (0.04 + 0.095) = 0.04 / 0.135 = 0.296
    
    data <- create_test_data(80, 10, 20, 90)
    options <- create_options(pp=TRUE, pprob=0.05)
    
    analysis <- decisionClass$new(options, data)
    analysis$run()
    
    res <- analysis$results$ratioTable$rows[["1"]]
    
    expect_equal(res$PrevalenceD, 0.05)
    expect_equal(res$PostTestProbDisease, 0.2962963, tolerance=1e-5)
})

test_that("Misclassification Analysis", {
    # TP=80, FP=10, FN=20, TN=90
    # Should have 10 FP cases and 20 FN cases
    
    data <- create_test_data(80, 10, 20, 90)
    options <- create_options(showMisclassified=TRUE)
    
    analysis <- decisionClass$new(options, data)
    analysis$run()
    
    # Check summary table
    summary_rows <- analysis$results$confusionMatrixSummary$rows
    expect_equal(summary_rows[["2"]]$count, 10) # FP
    expect_equal(summary_rows[["3"]]$count, 20) # FN
    
    # Check tables populated
    expect_gt(length(analysis$results$falsePositiveTable$rows), 0)
    expect_gt(length(analysis$results$falseNegativeTable$rows), 0)
})

test_that("Confidence Intervals (Mocked)", {
    data <- create_test_data(80, 10, 20, 90)
    options <- create_options(ci=TRUE)
    
    # Need to ensure epiR::epi.tests is called
    # We mocked it globally, but need to ensure it's found
    
    analysis <- decisionClass$new(options, data)
    analysis$run()
    
    # Check if CI table is populated from our mock
    # Mock returns est=0.8 for sensitivity (first row)
    expect_equal(analysis$results$epirTable_ratio$rows[["1"]]$est, 0.8)
})

test_that("Plotting State", {
    data <- create_test_data(80, 10, 20, 90)
    options <- create_options(fagan=TRUE)
    
    analysis <- decisionClass$new(options, data)
    analysis$run()
    
    state <- analysis$results$plot1$state
    expect_equal(state$Sens, 0.8)
    expect_equal(state$Spec, 0.9)
    expect_equal(state$Prevalence, 0.5) # 100/200 in sample
})

test_that("Input Validation - Missing Data", {
    data <- create_test_data(80, 10, 20, 90)
    # Add some missing values
    data[1, "Gold"] <- NA
    data[2, "Test"] <- NA
    
    options <- create_options(od=TRUE)
    
    analysis <- decisionClass$new(options, data)
    analysis$run()
    
    # Should have removed 2 rows
    # Total analyzed should be 198
    res <- analysis$results$nTable$rows[["1"]]
    expect_equal(res$TotalPop, 198)
    
    # Check missing data summary
    expect_true(grepl("Missing data summary", analysis$results$missingDataSummary$content))
})

print("Verification Script Created Successfully")
