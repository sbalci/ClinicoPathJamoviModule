
# Verification script for oddsratio function
# This script mocks jmvcore and tests the oddsratio function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "finalfit", "dplyr", "glue", "janitor", "labelled", "rms", "knitr", "stringr")
for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
}

# Mock NoticeType
NoticeType <- list(
    ERROR = "ERROR",
    WARNING = "WARNING",
    STRONG_WARNING = "STRONG_WARNING",
    INFO = "INFO"
)

# Mock Notice
Notice <- R6Class("Notice",
    public = list(
        content = NULL,
        options = NULL,
        name = NULL,
        type = NULL,
        initialize = function(options, name, type) {
            self$options <- options
            self$name <- name
            self$type <- type
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    data = NULL,
    explanatory = NULL,
    outcome = NULL,
    outcomeLevel = NULL,
    showNomogram = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE,
    
    initialize = function(...) {
      args <- list(...)
      for (name in names(args)) {
        self[[name]] <- args[[name]]
      }
    },
    set = function(key, value) {
      self[[key]] <- value
    }
  )
)

# Mock Html
Html <- R6Class("Html",
  public = list(
    content = NULL,
    visible = TRUE,
    initialize = function(...) {},
    setContent = function(content) {
      self$content <- content
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    setSize = function(width, height) {
      # Mock setSize
    }
  )
)

# Mock Table
Table <- R6Class("Table",
    public = list(
        rows = list(),
        visible = TRUE,
        initialize = function(...) {},
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- list(key = rowKey, values = values)
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

# Mock Image
Image <- R6Class("Image",
    public = list(
        visible = TRUE,
        state = NULL,
        initialize = function(...) {},
        setVisible = function(visible) {
            self$visible <- visible
        },
        setSize = function(width, height) {
            # Mock setSize
        },
        setState = function(state) {
            self$state <- state
        }
    )
)

# Mock Results
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    text = NULL,
    text2 = NULL,
    plot = NULL,
    nomogram = NULL,
    
    oddsRatioSummaryHeading = NULL,
    oddsRatioSummary = NULL,
    nomogramSummaryHeading = NULL,
    nomogramSummary = NULL,
    
    oddsRatioExplanationHeading = NULL,
    oddsRatioExplanation = NULL,
    riskMeasuresExplanation = NULL,
    diagnosticTestExplanation = NULL,
    nomogramExplanationHeading = NULL,
    nomogramAnalysisExplanation = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$text <- Html$new()
      self$text2 <- Html$new()
      self$plot <- Image$new()
      self$nomogram <- Html$new()
      
      self$oddsRatioSummaryHeading <- Html$new()
      self$oddsRatioSummary <- Html$new()
      self$nomogramSummaryHeading <- Html$new()
      self$nomogramSummary <- Html$new()
      
      self$oddsRatioExplanationHeading <- Html$new()
      self$oddsRatioExplanation <- Html$new()
      self$riskMeasuresExplanation <- Html$new()
      self$diagnosticTestExplanation <- Html$new()
      self$nomogramExplanationHeading <- Html$new()
      self$nomogramAnalysisExplanation <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
oddsratioBase <- R6Class("oddsratioBase",
  lock_objects = FALSE,
  public = list(
    options = NULL,
    data = NULL,
    results = NULL,
    initialize = function(options, data) {
      self$options <- options
      self$data <- data
      self$results <- Results$new()
    },
    .finalize = function() {}
  ),
  private = list(
    .checkpoint = function() {}
  )
)

# Define jmvcore_ mock objects
jmvcore_Notice <- Notice
jmvcore_NoticeType <- NoticeType
jmvcore_naOmit <- function(x) na.omit(x)
jmvcore_constructFormula <- function(terms) paste(terms, collapse = " + ")
jmvcore_composeTerms <- function(listOfComponents) paste(listOfComponents, collapse = " + ")
. <- function(x) x # Mock localization function

# 2. Source the function ----
source_file <- "R/oddsratio.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("oddsratioClass <- if \\(requireNamespace\\('jmvcore'\\)\\)", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found oddsratioClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "oddsratioClass <- R6::R6Class("
} else {
  # Try simpler grep if exact match fails
  start_line_idx <- grep("oddsratioClass <-", file_content)
  if (length(start_line_idx) > 0) {
     print(paste("Found oddsratioClass definition at line (fallback):", start_line_idx))
     file_content[start_line_idx] <- "oddsratioClass <- R6::R6Class("
  } else {
     stop("Could not find oddsratioClass definition to replace!")
  }
}

# Replace jmvcore:: with jmvcore_ where appropriate (functions)
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
if (!exists("oddsratioClass")) {
    stop("oddsratioClass not defined after sourcing!")
}
print("oddsratioClass successfully created.")

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list, class_def) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  # Re-instantiate Options
  options <- Options$new()
  for (name in names(options_list)) {
    options$set(name, options_list[[name]])
  }
  
  analysis <- class_def$new(options = options, data = data)
  
  # Run
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Return results for inspection
    return(analysis$results)
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    traceback()
    return(NULL)
  })
}

# 4. Run Tests ----

# Prepare Data
set.seed(123)
n <- 200
test_data <- data.frame(
    outcome = factor(sample(c("Dead", "Alive"), n, replace = TRUE)),
    treatment = factor(sample(c("Drug A", "Drug B"), n, replace = TRUE)),
    age = rnorm(n, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    stringsAsFactors = FALSE
)

# Test 1: Basic Odds Ratio Analysis
cat("Running Test 1: Basic Odds Ratio Analysis\n")
options_1 <- list(
    outcome = "outcome",
    explanatory = c("treatment", "age", "sex"),
    outcomeLevel = "Dead",
    showSummaries = TRUE
)
res1 <- run_test("Basic Odds Ratio Analysis", test_data, options_1, oddsratioClass)

# Test 2: Nomogram Generation
cat("Running Test 2: Nomogram Generation\n")
options_2 <- list(
    outcome = "outcome",
    explanatory = c("treatment", "age"),
    outcomeLevel = "Dead",
    showNomogram = TRUE,
    showSummaries = TRUE
)
res2 <- run_test("Nomogram Generation", test_data, options_2, oddsratioClass)

# Test 3: Diagnostic Metrics (Binary Predictor)
cat("Running Test 3: Diagnostic Metrics\n")
options_3 <- list(
    outcome = "outcome",
    explanatory = c("treatment"),
    outcomeLevel = "Dead",
    showNomogram = TRUE,
    showSummaries = TRUE
)
res3 <- run_test("Diagnostic Metrics", test_data, options_3, oddsratioClass)

# Test 4: Input Validation (Missing Outcome Level)
cat("Running Test 4: Input Validation (Missing Outcome Level)\n")
options_4 <- list(
    outcome = "outcome",
    explanatory = c("treatment")
)
res4 <- run_test("Missing Outcome Level", test_data, options_4, oddsratioClass)

# Test 5: Input Validation (Invalid Outcome Level)
cat("Running Test 5: Input Validation (Invalid Outcome Level)\n")
options_5 <- list(
    outcome = "outcome",
    explanatory = c("treatment"),
    outcomeLevel = "InvalidLevel"
)
res5 <- run_test("Invalid Outcome Level", test_data, options_5, oddsratioClass)

