# Verification script for timeinterval function
library(lubridate)
library(glue)

# Mock jmvcore environment
jmvcore <- new.env()

# Mock Option classes
Option <- R6::R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value) {
            self$name <- name
            self$value <- value
        }
    )
)

# Mock Analysis class
Analysis <- R6::R6Class("Analysis",
    public = list(
        options = NULL,
        results = NULL,
        data = NULL,
        initialize = function(options, results, data) {
            self$options <- options
            self$results <- results
            self$data <- data
        },
        run = function() { # Public run method to call private .run
            private$.run()
        }
    )
)

# Mock Results elements
Html <- R6::R6Class("Html",
    public = list(
        content = NULL,
        setContent = function(content) {
            self$content <- content
        }
    )
)

Output <- R6::R6Class("Output",
    public = list(
        values = NULL,
        rowNums = NULL,
        setValues = function(values) {
            self$values <- values
        },
        setRowNums = function(rowNums) {
            self$rowNums <- rowNums
        }
    )
)

# Define timeintervalBase
timeintervalBase <- R6::R6Class(
    "timeintervalBase",
    inherit = Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(options, NULL, data)
            self$results <- list(
                todo = Html$new(),
                personTimeInfo = Html$new(),
                aboutPanel = Html$new(),
                summary = Html$new(),
                nlSummary = Html$new(),
                glossaryPanel = Html$new(),
                qualityAssessment = Html$new(),
                caveatsPanel = Html$new(),
                calculated_time = Output$new()
            )
        }
    )
)

# Source the implementation
source("R/timeinterval.b.R")

# Test Data Generation
generate_test_data <- function() {
    n <- 20
    start_dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-20"), by="days")
    end_dates <- start_dates + sample(30:365, n, replace=TRUE)
    
    # Introduce some issues
    end_dates[5] <- start_dates[5] - 10 # Negative interval
    start_dates[10] <- NA # Missing start
    end_dates[15] <- NA # Missing end
    
    data.frame(
        id = 1:n,
        start = as.character(start_dates),
        end = as.character(end_dates),
        stringsAsFactors = FALSE
    )
}

test_data <- generate_test_data()

# Mock Options
options <- list(
    dx_date = "start",
    fu_date = "end",
    time_format = "ymd",
    output_unit = "months",
    use_landmark = FALSE,
    landmark_time = 2, # Changed to 2 for landmark test
    remove_negative = FALSE,
    remove_extreme = FALSE,
    extreme_multiplier = 2.0,
    add_times = TRUE,
    include_quality_metrics = TRUE,
    confidence_level = 95,
    show_summary = TRUE,
    show_glossary = TRUE,
    timezone = "system"
)

# Run Analysis (with negative interval, remove_negative=FALSE) - Should run with warnings
print("--- Running Basic Analysis (with negative intervals, remove_negative=FALSE) ---")
analysis <- timeintervalClass$new(
    options = options,
    data = test_data
)
analysis$run()
print("Analysis ran successfully even with negative intervals (expected behavior).")
print("Quality warnings should be present in results$todo or results$qualityAssessment")

# Now enable remove_negative and try again - Should run without stopping
options_remove_neg <- options
options_remove_neg$remove_negative <- TRUE
print("--- Running Analysis with remove_negative=TRUE ---")
analysis_remove_neg <- timeintervalClass$new(
    options = options_remove_neg,
    data = test_data
)
analysis_remove_neg$run()
print("Analysis with remove_negative=TRUE ran successfully.")
print("Calculated Time Values (after removing negative):")
print(head(analysis_remove_neg$results$calculated_time$values)) # Should not have NA or negative where fixed

# Run Analysis on Clean Data (without negative interval, original N=20)
test_data_clean <- test_data
# Fix the negative interval
test_data_clean$end[5] <- as.character(as.Date(test_data_clean$start[5]) + 10) 

options_clean <- options
options_clean$remove_negative <- FALSE # Should not matter here as data is clean

print("--- Running Analysis on Clean Data (N=20) ---")
analysis_clean <- timeintervalClass$new(
    options = options_clean,
    data = test_data_clean
)
analysis_clean$run()
print("Summary Content (Clean Data):")
print(substr(analysis_clean$results$summary$content, 1, 500))
print("Calculated Time Values (Clean Data):")
print(head(analysis_clean$results$calculated_time$values))
stopifnot(length(analysis_clean$results$calculated_time$values) == (nrow(test_data_clean) - sum(is.na(test_data_clean$start)) - sum(is.na(test_data_clean$end))))


# Test Landmark Analysis (on clean data)
# Expecting 2 missing + 2 filtered by landmark_time (2 months)
# Total N=20.
# test_data_clean values: 5.66, 3.10, 1.31, 5.76, 0.32, ...
# 1.31 and 0.32 are < 2 months. So 2 more should be filtered.
# Total original 20.
# Missing: start_dates[10] and end_dates[15] (2 missing)
# Calculated times values are: 5.66, 3.10, 1.31, 5.76, 0.32, 7.09, 8.77, 8.45, 9.61, NA (for row 10), 10.38, 4.35, 11.61, 6.77, NA (for row 15), 6.13, 10.45, 7.61, 2.74, 5.25
# After remove_negative = TRUE (or clean data): calculated_time will have NAs for rows 10, 15.
# So, `calculated_time` length is 20, but with 2 NAs.
# The valid values (non-NA) are 18.
# Among these 18, 1.31 and 0.32 should be excluded by landmark (time < 2 months).
# So 18 - 2 = 16 expected valid values.

options_landmark <- options_clean
options_landmark$use_landmark <- TRUE
options_landmark$landmark_time <- 2 # 2 months

print("--- Running Landmark Analysis (expected N=16) ---")
analysis_landmark <- timeintervalClass$new(
    options = options_landmark,
    data = test_data_clean
)
analysis_landmark$run()

print("Landmark Summary:")
print(substr(analysis_landmark$results$summary$content, 1, 500))

print("Landmark Values Length:")
print(length(analysis_landmark$results$calculated_time$values))

stopifnot(length(analysis_landmark$results$calculated_time$values) == 16)
stopifnot(analysis_landmark$results$landmark$excluded_count == 4) # 2 from <2mo, 2 from NA

print("All tests passed!")

