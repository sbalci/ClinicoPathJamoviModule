
# Verification script for datecorrection function
# This script mocks jmvcore and tests the datecorrection function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "datefixR", "anytime", "lubridate", "dplyr", "htmltools", "magrittr")
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
    date_vars = NULL,
    correction_method = "datefixr",
    date_format = "auto",
    day_impute = 1,
    month_impute = 7,
    handle_excel = FALSE,
    timezone = "UTC",
    show_correction_table = FALSE,
    show_quality_assessment = FALSE,
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE,
    
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
        initialize = function(...) {},
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- values
        }
    )
)

# Mock Results
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    correction_table = NULL,
    quality_assessment = NULL,
    format_analysis = NULL,
    correction_summary = NULL,
    interpretation = NULL,
    corrected_data = NULL,
    notices = list(),
    
    initialize = function() {
      self$todo <- Html$new()
      self$correction_table <- Html$new()
      self$quality_assessment <- Html$new()
      self$format_analysis <- Html$new()
      self$correction_summary <- Html$new()
      self$interpretation <- Html$new()
      self$corrected_data <- Table$new()
    },
    
    insert = function(index, notice) {
        self$notices[[length(self$notices) + 1]] <- notice
        print(paste("Notice [", notice$type, "]: ", notice$name, sep=""))
    }
  )
)

# Mock Analysis class (Base)
datecorrectionBase <- R6Class("datecorrectionBase",
  lock_objects = FALSE,
  public = list(
    options = NULL,
    data = NULL,
    results = NULL,
    initialize = function(options, data) {
      self$options <- options
      self$data <- data
      self$results <- Results$new()
    }
  )
)

# Define jmvcore_ mock objects
jmvcore_Notice <- Notice
jmvcore_NoticeType <- NoticeType

# 2. Source the function ----
source_file <- "R/datecorrection.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("datecorrectionClass <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found datecorrectionClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "datecorrectionClass <- R6::R6Class("
} else {
  stop("Could not find datecorrectionClass definition to replace!")
}

# Replace jmvcore:: with jmvcore_
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
if (!exists("datecorrectionClass")) {
    stop("datecorrectionClass not defined after sourcing!")
}
print("datecorrectionClass successfully created.")

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
test_data <- data.frame(
    id = 1:5,
    date_clean = c("2023-01-01", "2023-02-15", "2023-03-30", "2023-04-10", "2023-05-20"),
    date_messy = c("01/01/2023", "15-Feb-23", "March 30, 2023", "2023.04.10", "20/05/2023"),
    date_missing_day = c("2023-01", "Feb 2023", "03/2023", "2023-04", "May 23"),
    date_excel = c("44927", "44972", "45015", "45026", "45066"), # Approx dates
    stringsAsFactors = FALSE
)

# Test 1: Basic Correction (datefixR)
cat("Running Test 1: Basic Correction (datefixR)\n")
options_1 <- list(
    date_vars = c("date_messy"),
    correction_method = "datefixr",
    show_correction_table = TRUE
)
res1 <- run_test("Basic Correction", test_data, options_1, datecorrectionClass)

# Test 2: Flexible Parsing (anytime)
cat("Running Test 2: Flexible Parsing (anytime)\n")
options_2 <- list(
    date_vars = c("date_messy"),
    correction_method = "anytime",
    show_correction_table = TRUE
)
res2 <- run_test("Flexible Parsing", test_data, options_2, datecorrectionClass)

# Test 3: Format Specific (lubridate)
cat("Running Test 3: Format Specific (lubridate)\n")
options_3 <- list(
    date_vars = c("date_clean"),
    correction_method = "lubridate",
    date_format = "ymd",
    show_correction_table = TRUE
)
res3 <- run_test("Format Specific", test_data, options_3, datecorrectionClass)

# Test 4: Consensus Method
cat("Running Test 4: Consensus Method\n")
options_4 <- list(
    date_vars = c("date_messy"),
    correction_method = "consensus",
    show_quality_assessment = TRUE
)
res4 <- run_test("Consensus Method", test_data, options_4, datecorrectionClass)

# Test 5: Imputation
cat("Running Test 5: Imputation\n")
options_5 <- list(
    date_vars = c("date_missing_day"),
    correction_method = "datefixr",
    day_impute = 15,
    show_correction_table = TRUE
)
res5 <- run_test("Imputation", test_data, options_5, datecorrectionClass)

# Test 6: Excel Dates
cat("Running Test 6: Excel Dates\n")
options_6 <- list(
    date_vars = c("date_excel"),
    correction_method = "datefixr",
    handle_excel = TRUE,
    show_correction_table = TRUE
)
res6 <- run_test("Excel Dates", test_data, options_6, datecorrectionClass)

# Test 7: Error Handling (Empty Data)
cat("Running Test 7: Error Handling (Empty Data)\n")
options_7 <- list(
    date_vars = c("date_messy")
)
res7 <- run_test("Empty Data", test_data[0, ], options_7, datecorrectionClass)

# Test 8: Error Handling (Missing Variables)
cat("Running Test 8: Error Handling (Missing Variables)\n")
options_8 <- list(
    date_vars = c("non_existent_var")
)
res8 <- run_test("Missing Variables", test_data, options_8, datecorrectionClass)
