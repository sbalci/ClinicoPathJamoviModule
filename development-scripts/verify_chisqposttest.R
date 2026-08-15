
# Verification Script for chisqposttest Function

# 1. Mocking jmvcore Environment ------------------------------------------

library(R6)
library(testthat)

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
OptionList <- R6Class("OptionList", inherit = Option)
OptionBool <- R6Class("OptionBool", inherit = Option)
OptionNumber <- R6Class("OptionNumber", inherit = Option)

# Mock Table Class
Table <- R6Class("Table",
    public = list(
        name = NULL,
        rows = list(),
        columns = list(),
        visible = TRUE,
        initialize = function(name) {
            self$name <- name
        },
        addRow = function(rowKey, values) {
            self$rows[[as.character(rowKey)]] <- values
        },
        setRow = function(rowNo, values) {
            self$rows[[as.character(rowNo)]] <- values
        },
        setVisible = function(visible) {
            self$visible <- visible
        },
        asDF = function() {
            if (length(self$rows) == 0) return(data.frame())
            do.call(rbind, lapply(self$rows, as.data.frame))
        },
        setContent = function(content) {
             # For tables that accept content (like HTML tables in jamovi)
             self$rows <- content
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

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        todo = NULL,
        weightedDataInfo = NULL,
        chisqTable = NULL,
        contingencyTable = NULL,
        residualsAnalysis = NULL,
        multipleTestingInfo = NULL,
        posthocTable = NULL,
        detailedComparisons = NULL,
        clinicalSummary = NULL,
        assumptionsCheck = NULL,
        educationalOverview = NULL,
        exampleInterpretations = NULL,
        reportSentences = NULL,
        glossaryPanel = NULL,
        exportTable = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$weightedDataInfo <- Html$new("weightedDataInfo")
            self$chisqTable <- Table$new("chisqTable")
            self$contingencyTable <- Html$new("contingencyTable") # Mocking as HTML for simplicity in this script
            self$residualsAnalysis <- Html$new("residualsAnalysis")
            self$multipleTestingInfo <- Html$new("multipleTestingInfo")
            self$posthocTable <- Table$new("posthocTable")
            self$detailedComparisons <- Html$new("detailedComparisons")
            self$clinicalSummary <- Html$new("clinicalSummary")
            self$assumptionsCheck <- Html$new("assumptionsCheck")
            self$educationalOverview <- Html$new("educationalOverview")
            self$exampleInterpretations <- Html$new("exampleInterpretations")
            self$reportSentences <- Html$new("reportSentences")
            self$glossaryPanel <- Html$new("glossaryPanel")
            self$exportTable <- Table$new("exportTable")
        }
    )
)

# Mock Base Class
chisqposttestBase <- R6Class("chisqposttestBase",
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
    )
)

# Load the function code
source("R/chisqposttest.b.R")

# Mock localization function
. <- function(x) x

# 2. Helper Functions -----------------------------------------------------

create_options <- function(rows, cols, counts = NULL, posthoc = "bonferroni", 
                          sig = 0.05, excl = FALSE, exp = FALSE, 
                          plot = FALSE, showResiduals = FALSE, 
                          showEducational = FALSE, showDetailedTables = FALSE,
                          residualsCutoff = 2.0, testSelection = "auto",
                          exportResults = FALSE, showClinicalSummary = TRUE,
                          showExampleInterpretations = FALSE, copyReadySentences = FALSE,
                          showAssumptionsCheck = TRUE, showGlossary = FALSE,
                          colorBlindSafe = FALSE) {
    
    list(
        rows = rows,
        cols = cols,
        counts = counts,
        posthoc = posthoc,
        sig = sig,
        excl = excl,
        exp = exp,
        plot = plot,
        showResiduals = showResiduals,
        showEducational = showEducational,
        showDetailedTables = showDetailedTables,
        residualsCutoff = residualsCutoff,
        testSelection = testSelection,
        exportResults = exportResults,
        showClinicalSummary = showClinicalSummary,
        showExampleInterpretations = showExampleInterpretations,
        copyReadySentences = copyReadySentences,
        showAssumptionsCheck = showAssumptionsCheck,
        showGlossary = showGlossary,
        colorBlindSafe = colorBlindSafe
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Overall Chi-Square Test is Accurate", {
    # Create dummy data
    set.seed(123)
    data <- data.frame(
        Group = factor(rep(c("A", "B"), each = 50)),
        Outcome = factor(c(rep(c("Yes", "No"), c(30, 20)), rep(c("Yes", "No"), c(10, 40))))
    )
    
    options <- create_options(rows = "Group", cols = "Outcome")
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    # Check Chi-Square Table
    chisq_res <- analysis$results$chisqTable$rows[["1"]]
    
    # Compare with stats::chisq.test
    ref_test <- chisq.test(table(data$Group, data$Outcome), correct = FALSE)
    
    expect_equal(chisq_res$value, ref_test$statistic, tolerance = 1e-4)
    expect_equal(chisq_res$df, ref_test$parameter)
    expect_equal(chisq_res$p, ref_test$p.value, tolerance = 1e-4)
})

test_that("Post-Hoc Tests are Performed Correctly (Bonferroni)", {
    # 3x2 table to ensure multiple comparisons
    data <- data.frame(
        Treatment = factor(rep(c("A", "B", "C"), each = 40)),
        Response = factor(c(
            rep(c("Pos", "Neg"), c(30, 10)), # A: High positive
            rep(c("Pos", "Neg"), c(20, 20)), # B: Medium positive
            rep(c("Pos", "Neg"), c(10, 30))  # C: Low positive
        ))
    )
    
    options <- create_options(rows = "Treatment", cols = "Response", posthoc = "bonferroni")
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    # Check Post-Hoc Table
    posthoc_res <- analysis$results$posthocTable$asDF()
    
    expect_gt(nrow(posthoc_res), 0)
    
    # Manual check for A vs C (should be most significant)
    # A: 30/10, C: 10/30
    subtable_ac <- matrix(c(30, 10, 10, 30), nrow=2, byrow=TRUE)
    ref_chisq <- chisq.test(subtable_ac, correct = FALSE)
    ref_p <- ref_chisq$p.value
    
    # Find A vs C comparison in results
    ac_row <- posthoc_res[grep("A vs C", posthoc_res$comparison), ]
    
    expect_equal(ac_row$p, ref_p, tolerance = 1e-4)
    
    # Check Bonferroni adjustment
    # 3 groups -> 3 comparisons (A vs B, A vs C, B vs C)
    # For rows: 3 comparisons. For cols: 1 comparison (2 levels). Total = 3 row comparisons.
    # Wait, the function does row-wise AND column-wise comparisons.
    # Response has 2 levels, so only 1 comparison (Pos vs Neg), which is redundant with row comparisons in a 2x2 context?
    # Let's check how many comparisons are actually generated.
    
    # The function generates row comparisons (A vs B, A vs C, B vs C) AND column comparisons (Pos vs Neg).
    # Total comparisons = 3 + 1 = 4?
    # Let's verify the adjustment factor.
    
    # Actually, for a 3x2 table:
    # Row comparisons: (A,B), (A,C), (B,C) -> 3 comparisons
    # Col comparisons: (Pos,Neg) -> 1 comparison
    # Total = 4.
    
    expect_equal(ac_row$padj, min(1, ac_row$p * 4), tolerance = 1e-4)
})

test_that("Post-Hoc Tests are NOT Run when Overall Test is Non-Significant", {
    # Create non-significant data
    set.seed(123)
    data <- data.frame(
        Group = factor(rep(c("A", "B", "C"), each = 30)),
        Outcome = factor(sample(c("Yes", "No"), 90, replace = TRUE))
    )
    
    options <- create_options(rows = "Group", cols = "Outcome", posthoc = "bonferroni")
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    # Post-hoc table should be empty
    posthoc_res <- analysis$results$posthocTable$asDF()
    expect_equal(nrow(posthoc_res), 0)
    
    # Message should indicate why
    msg <- analysis$results$multipleTestingInfo$content
    expect_match(msg, "Overall chi-square test is not significant")
})

test_that("Residuals Analysis Identifies Significant Cells", {
    # Data with known strong association
    data <- data.frame(
        Group = factor(rep(c("A", "B"), each = 50)),
        Outcome = factor(c(rep(c("Yes", "No"), c(45, 5)), rep(c("Yes", "No"), c(5, 45))))
    )
    
    options <- create_options(rows = "Group", cols = "Outcome", showResiduals = TRUE)
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    # Check residuals content
    residuals_html <- analysis$results$residualsAnalysis$content
    
    # Should contain interpretation
    expect_match(residuals_html, "Standardized Residuals Interpretation")
    
    # Should identify significant cells (values > 2)
    # With 45/5 vs 5/45, residuals will be very large
    expect_match(residuals_html, "Over-represented")
})

test_that("Weighted Data (Counts) is Handled Correctly", {
    # Create weighted data
    data <- data.frame(
        Row = factor(c("A", "A", "B", "B")),
        Col = factor(c("X", "Y", "X", "Y")),
        Count = c(30, 10, 10, 30)
    )
    
    options <- create_options(rows = "Row", cols = "Col", counts = "Count")
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    # Check Chi-Square results
    chisq_res <- analysis$results$chisqTable$rows[["1"]]
    
    # Compare with manual calculation
    # Table:
    #    X  Y
    # A 30 10
    # B 10 30
    manual_table <- matrix(c(30, 10, 10, 30), nrow=2, byrow=TRUE)
    ref_test <- chisq.test(manual_table, correct = FALSE)
    
    expect_equal(chisq_res$value, ref_test$statistic, tolerance = 1e-4)
    
    # Check warning about weighted data
    # Note: showEducational must be TRUE for the warning to appear in weightedDataInfo
    options_edu <- create_options(rows = "Row", cols = "Col", counts = "Count", showEducational = TRUE)
    analysis_edu <- chisqposttestClass$new(options_edu, data)
    analysis_edu$run()
    
    expect_match(analysis_edu$results$weightedDataInfo$content, "Weighted Data Analysis")
})

test_that("Assumptions Check Warns for Low Expected Counts", {
    # Data with low counts
    data <- data.frame(
        Group = factor(c("A", "A", "B", "B")),
        Outcome = factor(c("Yes", "No", "Yes", "No")),
        Count = c(100, 1, 100, 2) # Very low counts in "No" category
    )
    
    options <- create_options(rows = "Group", cols = "Outcome", counts = "Count", showAssumptionsCheck = TRUE)
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    # Clinical summary should contain warning
    summary_text <- analysis$results$assumptionsCheck$content
    expect_match(summary_text, "Warning:.*expected counts.*5")
})

test_that("Clinical Summary is Generated", {
    data <- data.frame(
        Group = factor(rep(c("A", "B"), each = 50)),
        Outcome = factor(c(rep(c("Yes", "No"), c(30, 20)), rep(c("Yes", "No"), c(20, 30))))
    )
    
    options <- create_options(rows = "Group", cols = "Outcome", showClinicalSummary = TRUE)
    analysis <- chisqposttestClass$new(options, data)
    analysis$run()
    
    summary_text <- analysis$results$clinicalSummary$content
    expect_match(summary_text, "The association between")
    expect_match(summary_text, "Cramér's V")
})

print("All tests completed successfully!")
