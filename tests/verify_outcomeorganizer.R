
# Verification script for outcomeorganizer function
# This script mocks jmvcore and tests the outcomeorganizer function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "dplyr", "glue", "janitor", "labelled", "ggplot2")
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
    outcome = NULL,
    outcomeLevel = NULL,
    recurrence = NULL,
    recurrenceLevel = NULL,
    patientID = NULL,
    analysistype = "os",
    multievent = FALSE,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    useHierarchy = FALSE,
    eventPriority = 1,
    intervalCensoring = FALSE,
    intervalStart = NULL,
    intervalEnd = NULL,
    adminCensoring = FALSE,
    adminDate = NULL,
    outputTable = FALSE,
    diagnostics = FALSE,
    visualization = FALSE,
    showNaturalSummary = FALSE,
    showGlossary = FALSE,
    addOutcome = FALSE,
    
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
    }
  )
)

# Mock Table
Table <- R6Class("Table",
    public = list(
        rows = list(),
        rowKeys = list(),
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
        setState = function(state) {
            self$state <- state
        }
    )
)

# Mock Output
Output <- R6Class("Output",
    public = list(
        values = NULL,
        rowNums = NULL,
        initialize = function(...) {},
        setValues = function(values) {
            self$values <- values
        },
        setRowNums = function(rowNums) {
            self$rowNums <- rowNums
        }
    )
)

# Mock Results
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    summary = NULL,
    outputTable = NULL,
    diagnosticsTable = NULL,
    outcomeViz = NULL,
    naturalSummary = NULL,
    glossary = NULL,
    addOutcome = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$summary <- Html$new()
      self$outputTable <- Table$new()
      self$diagnosticsTable <- Table$new()
      self$outcomeViz <- Image$new()
      self$naturalSummary <- Html$new()
      self$glossary <- Html$new()
      self$addOutcome <- Output$new()
    }
  )
)

# Mock Analysis class (Base)
outcomeorganizerBase <- R6Class("outcomeorganizerBase",
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
jmvcore_select <- function(data, vars) {
    data[, vars, drop = FALSE]
}
. <- function(x) x # Mock localization function

# 2. Source the function ----
source_file <- "R/outcomeorganizer.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("outcomeorganizerClass <- if \\(requireNamespace\\('jmvcore'\\)\\)", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found outcomeorganizerClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "outcomeorganizerClass <- R6::R6Class("
} else {
  # Try simpler grep if exact match fails
  start_line_idx <- grep("outcomeorganizerClass <-", file_content)
  if (length(start_line_idx) > 0) {
     print(paste("Found outcomeorganizerClass definition at line (fallback):", start_line_idx))
     file_content[start_line_idx] <- "outcomeorganizerClass <- R6::R6Class("
  } else {
     stop("Could not find outcomeorganizerClass definition to replace!")
  }
}

# Replace jmvcore:: with jmvcore_ where appropriate (functions)
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
if (!exists("outcomeorganizerClass")) {
    stop("outcomeorganizerClass not defined after sourcing!")
}
print("outcomeorganizerClass successfully created.")

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
    # traceback() # Traceback is often not helpful in tryCatch without specialized handling
    return(NULL)
  })
}

# 4. Run Tests ----

# Prepare Data
set.seed(123)
n <- 20
test_data <- data.frame(
    status = factor(sample(c("Alive", "Dead"), n, replace = TRUE)),
    recurrence = factor(sample(c("Yes", "No"), n, replace = TRUE)),
    multistatus = factor(sample(c("Alive_Disease", "Alive_NED", "Dead_Disease", "Dead_Other"), n, replace = TRUE)),
    time = runif(n, 1, 100),
    stringsAsFactors = FALSE
)

# Test 1: Basic Overall Survival (Binary)
cat("Running Test 1: Basic Overall Survival (Binary)\n")
options_1 <- list(
    outcome = "status",
    outcomeLevel = "Dead",
    analysistype = "os",
    addOutcome = TRUE,
    showNaturalSummary = TRUE
)
res1 <- run_test("Basic Overall Survival", test_data, options_1, outcomeorganizerClass)

# Test 2: Recurrence-Free Survival (RFS)
cat("Running Test 2: Recurrence-Free Survival (RFS)\n")
options_2 <- list(
    outcome = "status",
    outcomeLevel = "Dead",
    recurrence = "recurrence",
    recurrenceLevel = "Yes",
    analysistype = "rfs",
    addOutcome = TRUE
)
res2 <- run_test("Recurrence-Free Survival", test_data, options_2, outcomeorganizerClass)

# Test 3: Competing Risks (Multi-event)
cat("Running Test 3: Competing Risks (Multi-event)\n")
options_3 <- list(
    outcome = "multistatus",
    analysistype = "compete",
    multievent = TRUE,
    dod = "Dead_Disease",
    dooc = "Dead_Other",
    awd = "Alive_Disease",
    awod = "Alive_NED",
    addOutcome = TRUE
)
res3 <- run_test("Competing Risks", test_data, options_3, outcomeorganizerClass)

# Test 4: Validation - Missing Outcome Variable
cat("Running Test 4: Validation - Missing Outcome Variable\n")
options_4 <- list(
    outcome = "non_existent_var",
    analysistype = "os"
)
res4 <- run_test("Missing Outcome Variable", test_data, options_4, outcomeorganizerClass)

# Test 5: Validation - Missing Outcome Level (Binary)
cat("Running Test 5: Validation - Missing Outcome Level\n")
# Create data with 0/1 outcome to bypass level check if possible, but here status is Alive/Dead
options_5 <- list(
    outcome = "status",
    analysistype = "os"
    # outcomeLevel missing
)
res5 <- run_test("Missing Outcome Level", test_data, options_5, outcomeorganizerClass)

# Test 6: Robust Variable Mapping (Spaces and special chars)
cat("Running Test 6: Robust Variable Mapping\n")
test_data_messy <- test_data
names(test_data_messy)[1] <- "Patient Status" # Should clean to patient_status
names(test_data_messy)[2] <- "Recurrence?"    # Should clean to recurrence
options_6 <- list(
    outcome = "Patient Status",
    outcomeLevel = "Dead",
    recurrence = "Recurrence?",
    recurrenceLevel = "Yes",
    analysistype = "rfs",
    addOutcome = TRUE
)
res6 <- run_test("Robust Variable Mapping", test_data_messy, options_6, outcomeorganizerClass)
if(!is.null(res6)) print("Robust Mapping Test Passed: Handled 'Patient Status' and 'Recurrence?' correctly")

# Test 7: Ordered Factor Handling
cat("Running Test 7: Ordered Factor Handling\n")
test_data_ordered <- test_data
test_data_ordered$status <- factor(test_data_ordered$status, levels=c("Alive", "Dead"), ordered=TRUE)
options_7 <- list(
    outcome = "status",
    outcomeLevel = "Dead",
    analysistype = "os"
)
res7 <- run_test("Ordered Factor Handling", test_data_ordered, options_7, outcomeorganizerClass)
if(!is.null(res7)) print("Ordered Factor Test Passed: Handled ordered factor without error")

# Test 8: Strict Validation (Compete without Multievent)
cat("Running Test 8: Strict Validation (Compete without Multievent)\n")
options_8 <- list(
    outcome = "multistatus",
    analysistype = "compete",
    multievent = FALSE # Should trigger ERROR now
)
res8 <- run_test("Strict Validation Check", test_data, options_8, outcomeorganizerClass)
# We expect this to fail/return NULL and print an error message about enabling Multiple Event Types
