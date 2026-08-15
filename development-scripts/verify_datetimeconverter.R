
# Verification script for datetimeconverter function
# This script mocks jmvcore and tests the datetimeconverter function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "lubridate", "dplyr", "glue")
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
    datetime_var = NULL,
    datetime_format = "auto",
    timezone = "system",
    preview_rows = 20,
    extract_year = FALSE,
    extract_month = FALSE,
    extract_monthname = FALSE,
    extract_day = FALSE,
    extract_hour = FALSE,
    extract_minute = FALSE,
    extract_second = FALSE,
    extract_dayname = FALSE,
    extract_weeknum = FALSE,
    extract_quarter = FALSE,
    extract_dayofyear = FALSE,
    show_quality_metrics = FALSE,
    show_summary = FALSE,
    show_explanations = FALSE,
    show_glossary = FALSE,
    corrected_datetime_char = FALSE,
    corrected_datetime_numeric = FALSE,
    year_out = FALSE,
    month_out = FALSE,
    monthname_out = FALSE,
    day_out = FALSE,
    hour_out = FALSE,
    minute_out = FALSE,
    second_out = FALSE,
    dayname_out = FALSE,
    weeknum_out = FALSE,
    quarter_out = FALSE,
    dayofyear_out = FALSE,
    
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

# Mock Output (for output variables)
Output <- R6Class("Output",
    public = list(
        values = NULL,
        rowNums = NULL,
        title = NULL,
        description = NULL,
        initialize = function(...) {},
        setValues = function(values) {
            self$values <- values
        },
        setRowNums = function(rowNums) {
            self$rowNums <- rowNums
        },
        setTitle = function(title) {
            self$title <- title
        },
        setDescription = function(desc) {
            self$description <- desc
        },
        isNotFilled = function() {
            return(is.null(self$values))
        }
    )
)

# Mock Results
Results <- R6Class("Results",
  public = list(
    welcome = NULL,
    formatInfo = NULL,
    qualityMetrics = NULL,
    previewTable = NULL,
    componentPreview = NULL,
    qualityAssessment = NULL,
    nlSummary = NULL,
    aboutPanel = NULL,
    caveatsPanel = NULL,
    glossaryPanel = NULL,
    
    corrected_datetime_char = NULL,
    corrected_datetime_numeric = NULL,
    year_out = NULL,
    month_out = NULL,
    monthname_out = NULL,
    day_out = NULL,
    hour_out = NULL,
    minute_out = NULL,
    second_out = NULL,
    dayname_out = NULL,
    weeknum_out = NULL,
    quarter_out = NULL,
    dayofyear_out = NULL,
    
    notices = list(),
    
    initialize = function() {
      self$welcome <- Html$new()
      self$formatInfo <- Html$new()
      self$qualityMetrics <- Html$new()
      self$previewTable <- Html$new()
      self$componentPreview <- Html$new()
      self$qualityAssessment <- Html$new()
      self$nlSummary <- Html$new()
      self$aboutPanel <- Html$new()
      self$caveatsPanel <- Html$new()
      self$glossaryPanel <- Html$new()
      
      self$corrected_datetime_char <- Output$new()
      self$corrected_datetime_numeric <- Output$new()
      self$year_out <- Output$new()
      self$month_out <- Output$new()
      self$monthname_out <- Output$new()
      self$day_out <- Output$new()
      self$hour_out <- Output$new()
      self$minute_out <- Output$new()
      self$second_out <- Output$new()
      self$dayname_out <- Output$new()
      self$weeknum_out <- Output$new()
      self$quarter_out <- Output$new()
      self$dayofyear_out <- Output$new()
    },
    
    insert = function(index, notice) {
        self$notices[[length(self$notices) + 1]] <- notice
        print(paste("Notice [", notice$type, "]: ", notice$name, sep=""))
    }
  )
)

# Mock Analysis class (Base)
datetimeconverterBase <- R6Class("datetimeconverterBase",
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
source_file <- "R/datetimeconverter.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("datetimeconverterClass <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found datetimeconverterClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "datetimeconverterClass <- R6::R6Class("
} else {
  stop("Could not find datetimeconverterClass definition to replace!")
}

# Replace jmvcore:: with jmvcore_
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
if (!exists("datetimeconverterClass")) {
    stop("datetimeconverterClass not defined after sourcing!")
}
print("datetimeconverterClass successfully created.")

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
    date_ymd = c("2023-01-01", "2023-02-15", "2023-03-30", "2023-04-10", "2023-05-20"),
    date_dmy = c("01/01/2023", "15/02/2023", "30/03/2023", "10/04/2023", "20/05/2023"),
    date_mdy = c("01/01/2023", "02/15/2023", "03/30/2023", "04/10/2023", "05/20/2023"),
    date_iso_time = c("2023-01-01 10:00:00", "2023-02-15 11:30:00", "2023-03-30 14:45:00", "2023-04-10 09:15:00", "2023-05-20 16:20:00"),
    date_excel = c(44927, 44972, 45015, 45026, 45066), # Approx dates
    date_unix = c(1672531200, 1676419200, 1680134400, 1681084800, 1684540800),
    stringsAsFactors = FALSE
)

# Test 1: Auto Detection (YMD)
cat("Running Test 1: Auto Detection (YMD)\n")
options_1 <- list(
    datetime_var = "date_ymd",
    datetime_format = "auto",
    show_quality_metrics = TRUE
)
res1 <- run_test("Auto Detection (YMD)", test_data, options_1, datetimeconverterClass)

# Test 2: Auto Detection (DMY)
cat("Running Test 2: Auto Detection (DMY)\n")
options_2 <- list(
    datetime_var = "date_dmy",
    datetime_format = "auto"
)
res2 <- run_test("Auto Detection (DMY)", test_data, options_2, datetimeconverterClass)

# Test 3: Manual Format (MDY)
cat("Running Test 3: Manual Format (MDY)\n")
options_3 <- list(
    datetime_var = "date_mdy",
    datetime_format = "mdy"
)
res3 <- run_test("Manual Format (MDY)", test_data, options_3, datetimeconverterClass)

# Test 4: ISO with Time
cat("Running Test 4: ISO with Time\n")
options_4 <- list(
    datetime_var = "date_iso_time",
    datetime_format = "auto",
    extract_hour = TRUE,
    extract_minute = TRUE
)
res4 <- run_test("ISO with Time", test_data, options_4, datetimeconverterClass)

# Test 5: Excel Serial Dates
cat("Running Test 5: Excel Serial Dates\n")
options_5 <- list(
    datetime_var = "date_excel",
    datetime_format = "auto"
)
res5 <- run_test("Excel Serial Dates", test_data, options_5, datetimeconverterClass)

# Test 6: Unix Epoch
cat("Running Test 6: Unix Epoch\n")
options_6 <- list(
    datetime_var = "date_unix",
    datetime_format = "auto"
)
res6 <- run_test("Unix Epoch", test_data, options_6, datetimeconverterClass)

# Test 7: Component Extraction
cat("Running Test 7: Component Extraction\n")
options_7 <- list(
    datetime_var = "date_ymd",
    datetime_format = "ymd",
    extract_year = TRUE,
    extract_month = TRUE,
    extract_day = TRUE,
    extract_dayname = TRUE,
    extract_quarter = TRUE,
    year_out = TRUE,
    month_out = TRUE
)
res7 <- run_test("Component Extraction", test_data, options_7, datetimeconverterClass)

# Test 8: Error Handling (Missing Variable)
cat("Running Test 8: Error Handling (Missing Variable)\n")
options_8 <- list(
    datetime_var = "non_existent_var"
)
res8 <- run_test("Missing Variable", test_data, options_8, datetimeconverterClass)
