
# Verification Script for venn Function

library(R6)
library(testthat)
library(dplyr)
library(ggplot2)

# Check for required packages
required_packages <- c("ggvenn", "ggVennDiagram", "UpSetR", "ComplexUpset")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
    warning(paste("Missing packages for full verification:", paste(missing_packages, collapse = ", ")))
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
        note = NULL,
        state = NULL,
        initialize = function(name) {
            self$name <- name
        },
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- values
        },
        deleteRows = function() {
            self$rows <- list()
        },
        addColumn = function(name, title, type) {
            self$columns[[name]] <- list(title = title, type = type)
        },
        getColumn = function(name) {
            return(self$columns[[name]])
        },
        setState = function(state) {
            self$state <- state
        },
        setNote = function(key, note) {
            self$note <- note
        }
    )
)

# Mock Output Class (for membershipGroups)
Output <- R6Class("Output",
    public = list(
        name = NULL,
        values = NULL,
        rowNums = NULL,
        initialize = function(name) {
            self$name <- name
        },
        isNotFilled = function() {
            return(TRUE)
        },
        setValues = function(values) {
            self$values <- values
        },
        setRowNums = function(rowNums) {
            self$rowNums <- rowNums
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        todo = NULL,
        welcome = NULL,
        aboutAnalysis = NULL,
        clinicalSummary = NULL,
        reportSentences = NULL,
        assumptions = NULL,
        glossary = NULL,
        setCalculations = NULL,
        summary = NULL,
        membershipTable = NULL,
        membershipGroups = NULL,
        plotGgvenn = NULL,
        plotGgVennDiagram = NULL,
        plotUpsetR = NULL,
        plotComplexUpset = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$welcome <- Html$new("welcome")
            self$aboutAnalysis <- Html$new("aboutAnalysis")
            self$clinicalSummary <- Html$new("clinicalSummary")
            self$reportSentences <- Html$new("reportSentences")
            self$assumptions <- Html$new("assumptions")
            self$glossary <- Html$new("glossary")
            self$setCalculations <- Html$new("setCalculations")
            self$summary <- Table$new("summary")
            self$membershipTable <- Table$new("membershipTable")
            self$membershipGroups <- Output$new("membershipGroups")
            self$plotGgvenn <- Image$new("plotGgvenn")
            self$plotGgVennDiagram <- Image$new("plotGgVennDiagram")
            self$plotUpsetR <- Image$new("plotUpsetR")
            self$plotComplexUpset <- Image$new("plotComplexUpset")
        }
    )
)

# Mock Base Class
vennBase <- R6Class("vennBase",
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
        .checkpoint = function() {}
    )
)

# Mock jmvcore functions
mock_jmvcore <- new.env()
mock_jmvcore$naOmit <- function(df) {
    na.omit(df)
}
jmvcore <- mock_jmvcore

# Mock localization function
. <- function(text) {
    return(text)
}

# Source the implementation
source("R/venn.b.R")


# 2. Helper Functions -----------------------------------------------------

create_options <- function(var1 = NULL, var1true = NULL,
                          var2 = NULL, var2true = NULL,
                          var3 = NULL, var3true = NULL,
                          var4 = NULL, var4true = NULL,
                          var5 = NULL, var5true = NULL,
                          var6 = NULL, var6true = NULL,
                          var7 = NULL, var7true = NULL,
                          show_ggvenn = FALSE,
                          show_ggVennDiagram = FALSE,
                          show_upsetR = FALSE,
                          show_complexUpset = FALSE,
                          explanatory = FALSE,
                          aboutAnalysis = FALSE,
                          clinicalSummary = FALSE,
                          reportSentences = FALSE,
                          assumptions = FALSE,
                          showGlossary = FALSE,
                          showSetCalculations = FALSE,
                          calculateOverlap = FALSE,
                          calculateDiscern = FALSE,
                          calculateUnite = FALSE,
                          showMembershipTable = FALSE,
                          membershipGroups = FALSE,
                          sortBy = "freq",
                          minSize = 0,
                          showAnnotations = FALSE,
                          shapeType = "auto",
                          regionLabels = "count",
                          labelGeometry = "label",
                          labelPrecisionDigits = 1,
                          setNameSize = 5,
                          labelSize = 4,
                          edgeSize = 1,
                          edgeColor = "black",
                          edgeLineType = "solid",
                          edgeAlpha = 1,
                          fillAlpha = 0.5,
                          showSetLabels = TRUE,
                          setLabelColor = "black",
                          fillColorMapping = TRUE,
                          colorPalette = "default") {
    
    list(
        var1 = var1, var1true = var1true,
        var2 = var2, var2true = var2true,
        var3 = var3, var3true = var3true,
        var4 = var4, var4true = var4true,
        var5 = var5, var5true = var5true,
        var6 = var6, var6true = var6true,
        var7 = var7, var7true = var7true,
        show_ggvenn = show_ggvenn,
        show_ggVennDiagram = show_ggVennDiagram,
        show_upsetR = show_upsetR,
        show_complexUpset = show_complexUpset,
        explanatory = explanatory,
        aboutAnalysis = aboutAnalysis,
        clinicalSummary = clinicalSummary,
        reportSentences = reportSentences,
        assumptions = assumptions,
        showGlossary = showGlossary,
        showSetCalculations = showSetCalculations,
        calculateOverlap = calculateOverlap,
        calculateDiscern = calculateDiscern,
        calculateUnite = calculateUnite,
        showMembershipTable = showMembershipTable,
        membershipGroups = membershipGroups,
        sortBy = sortBy,
        minSize = minSize,
        showAnnotations = showAnnotations,
        shapeType = shapeType,
        regionLabels = regionLabels,
        labelGeometry = labelGeometry,
        labelPrecisionDigits = labelPrecisionDigits,
        setNameSize = setNameSize,
        labelSize = labelSize,
        edgeSize = edgeSize,
        edgeColor = edgeColor,
        edgeLineType = edgeLineType,
        edgeAlpha = edgeAlpha,
        fillAlpha = fillAlpha,
        showSetLabels = showSetLabels,
        setLabelColor = setLabelColor,
        fillColorMapping = fillColorMapping,
        colorPalette = colorPalette
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Basic Functionality (2 variables)", {
    data <- mtcars
    data$vs <- factor(data$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
    data$am <- factor(data$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
    
    options <- create_options(
        var1 = "vs", var1true = "V-shaped",
        var2 = "am", var2true = "Manual",
        show_ggvenn = TRUE,
        clinicalSummary = TRUE
    )
    
    analysis <- vennClass$new(options, data)
    analysis$run()
    
    # Check if plot state is set
    expect_true(!is.null(analysis$results$plotGgvenn$state))
    # Check if summary table is populated
    expect_gt(length(analysis$results$summary$rows), 0)
    # Check if clinical summary is generated
    expect_true(!is.null(analysis$results$clinicalSummary$content))
})

test_that("Data Validation: Missing Values", {
    data <- mtcars
    data$vs <- factor(data$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
    data$am <- factor(data$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
    # Introduce missing values
    data$vs[1:5] <- NA
    
    options <- create_options(
        var1 = "vs", var1true = "V-shaped",
        var2 = "am", var2true = "Manual"
    )
    
    analysis <- vennClass$new(options, data)
    analysis$run()
    
    # Check for exclusion warning
    expect_true(grepl("Case Exclusion Warning", analysis$results$todo$content))
})

test_that("Data Validation: Invalid True Level", {
    data <- mtcars
    data$vs <- factor(data$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
    data$am <- factor(data$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
    
    options <- create_options(
        var1 = "vs", var1true = "NonExistentLevel",
        var2 = "am", var2true = "Manual"
    )
    
    analysis <- vennClass$new(options, data)
    analysis$run()
    
    # Check for error message in todo
    expect_true(grepl("True Level Not Found", analysis$results$todo$content))
})

test_that("Set Calculations", {
    data <- mtcars
    data$vs <- factor(data$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
    data$am <- factor(data$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
    
    options <- create_options(
        var1 = "vs", var1true = "V-shaped",
        var2 = "am", var2true = "Manual",
        showSetCalculations = TRUE,
        calculateOverlap = TRUE
    )
    
    analysis <- vennClass$new(options, data)
    analysis$run()
    
    # Check if set calculations content is generated
    # Note: This depends on ggVennDiagram being installed
    if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
        expect_true(!is.null(analysis$results$setCalculations$content))
        expect_true(grepl("Set Calculations", analysis$results$setCalculations$content))
    }
})

test_that("Membership Table", {
    data <- mtcars
    data$vs <- factor(data$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
    data$am <- factor(data$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
    
    options <- create_options(
        var1 = "vs", var1true = "V-shaped",
        var2 = "am", var2true = "Manual",
        showMembershipTable = TRUE,
        showSetCalculations = TRUE # Prerequisite
    )
    
    analysis <- vennClass$new(options, data)
    analysis$run()
    
    # Check if membership table has columns
    expect_gt(length(analysis$results$membershipTable$columns), 0)
})

print("Verification Script Created Successfully")
