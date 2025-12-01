
# Verification script for multisurvival function
# This script mocks jmvcore and tests the multisurvival function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "survival", "dplyr", "glue", "janitor", "labelled", "finalfit", "survminer", "cmprsk", "lubridate")
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
    elapsedtime = NULL,
    tint = FALSE,
    dxdate = NULL,
    fudate = NULL,
    timetypedata = "ymd",
    timetypeoutput = "months",
    uselandmark = FALSE,
    landmark = 3,
    calculatedtime = FALSE,
    outcome = NULL,
    outcomeLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    analysistype = "overall",
    outcomeredefined = FALSE,
    explanatory = NULL,
    contexpl = NULL,
    multievent = FALSE,
    hr = FALSE,
    sty = "t1",
    ph_cox = FALSE,
    km = FALSE,
    endplot = 60,
    byplot = 12,
    ci95 = FALSE,
    risktable = FALSE,
    censored = FALSE,
    medianline = "none",
    pplot = FALSE,
    cutp = "12, 36, 60",
    calculateRiskScore = FALSE,
    numRiskGroups = "three",
    plotRiskGroups = FALSE,
    addRiskScore = FALSE,
    addRiskGroup = FALSE,
    ac = FALSE,
    adjexplanatory = NULL,
    ac_method = "average",
    showNomogram = FALSE,
    use_stratify = FALSE,
    stratvar = NULL,
    use_aft = FALSE,
    aft_distribution = "weibull",
    aft_show_hr_equivalent = FALSE,
    aft_show_interpretation = FALSE,
    person_time = FALSE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100,
    show_survmetrics = FALSE,
    survmetrics_timepoints = "12, 24, 36, 60",
    survmetrics_ibs_points = 100,
    survmetrics_show_plots = FALSE,
    showExplanations = FALSE,
    showSummaries = FALSE,
    ml_method = "none",
    
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
        initialize = function(...) {},
        setVisible = function(visible) {
            self$visible <- visible
        },
        setSize = function(width, height) {
            # Mock setSize
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
    todo = NULL,
    text = NULL,
    text2 = NULL,
    plot = NULL,
    plot3 = NULL,
    plotKM = NULL,
    plot_adj = NULL,
    plot_nomogram = NULL,
    plot8 = NULL,
    
    multivariableCoxSummaryHeading = NULL,
    multivariableCoxSummary = NULL,
    personTimeSummaryHeading = NULL,
    personTimeSummary = NULL,
    personTimeTable = NULL,
    adjustedSurvivalSummaryHeading = NULL,
    adjustedSurvivalSummary = NULL,
    nomogramSummaryHeading = NULL,
    nomogramSummary = NULL,
    riskScoreSummaryHeading = NULL,
    riskScoreTable = NULL,
    riskScoreSummary = NULL,
    treeSummaryHeading = NULL,
    tree_summary = NULL,
    ml_ensemble_summary = NULL,
    
    multivariableCoxHeading3 = NULL,
    multivariableCoxExplanation = NULL,
    adjustedSurvivalExplanation = NULL,
    riskScoreExplanation = NULL,
    nomogramExplanation = NULL,
    personTimeExplanation = NULL,
    stratifiedAnalysisExplanation = NULL,
    survivalPlotsHeading3 = NULL,
    survivalPlotsExplanation = NULL,
    
    aftModelTable = NULL,
    aftSummary = NULL,
    aftModelInfo = NULL,
    
    survMetricsSummary = NULL,
    survMetricsTable = NULL,
    
    calculatedtime = NULL,
    outcomeredefined = NULL,
    addRiskScore = NULL,
    addRiskGroup = NULL,
    
    initialize = function() {
      self$todo <- Html$new()
      self$text <- Html$new()
      self$text2 <- Html$new()
      self$plot <- Image$new()
      self$plot3 <- Image$new()
      self$plotKM <- Image$new()
      self$plot_adj <- Image$new()
      self$plot_nomogram <- Image$new()
      self$plot8 <- Image$new()
      
      self$multivariableCoxSummaryHeading <- Html$new()
      self$multivariableCoxSummary <- Html$new()
      self$personTimeSummaryHeading <- Html$new()
      self$personTimeSummary <- Html$new()
      self$personTimeTable <- Table$new()
      self$adjustedSurvivalSummaryHeading <- Html$new()
      self$adjustedSurvivalSummary <- Html$new()
      self$nomogramSummaryHeading <- Html$new()
      self$nomogramSummary <- Html$new()
      self$riskScoreSummaryHeading <- Html$new()
      self$riskScoreTable <- Table$new()
      self$riskScoreSummary <- Html$new()
      self$treeSummaryHeading <- Html$new()
      self$tree_summary <- Html$new()
      self$ml_ensemble_summary <- Html$new()
      
      self$multivariableCoxHeading3 <- Html$new()
      self$multivariableCoxExplanation <- Html$new()
      self$adjustedSurvivalExplanation <- Html$new()
      self$riskScoreExplanation <- Html$new()
      self$nomogramExplanation <- Html$new()
      self$personTimeExplanation <- Html$new()
      self$stratifiedAnalysisExplanation <- Html$new()
      self$survivalPlotsHeading3 <- Html$new()
      self$survivalPlotsExplanation <- Html$new()
      
      self$aftModelTable <- Table$new()
      self$aftSummary <- Html$new()
      self$aftModelInfo <- Html$new()
      
      self$survMetricsSummary <- Html$new()
      self$survMetricsTable <- Table$new()
      
      self$calculatedtime <- Output$new()
      self$outcomeredefined <- Output$new()
      self$addRiskScore <- Output$new()
      self$addRiskGroup <- Output$new()
    }
  )
)

# Mock Analysis class (Base)
multisurvivalBase <- R6Class("multisurvivalBase",
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
jmvcore_toNumeric <- function(x) as.numeric(as.character(x))
jmvcore_naOmit <- function(x) na.omit(x)
jmvcore_select <- function(df, cols) df[, cols, drop = FALSE]
jmvcore_constructFormula <- function(terms) terms # Simplified

# 2. Source the function ----
source_file <- "R/multisurvival.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("multisurvivalClass <- if \\(requireNamespace\\('jmvcore'\\)\\)", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found multisurvivalClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "multisurvivalClass <- "
} else {
  # Try simpler grep if exact match fails
  start_line_idx <- grep("multisurvivalClass <-", file_content)
  if (length(start_line_idx) > 0) {
     print(paste("Found multisurvivalClass definition at line (fallback):", start_line_idx))
     file_content[start_line_idx] <- "multisurvivalClass <- "
  } else {
     stop("Could not find multisurvivalClass definition to replace!")
  }
}

# Replace jmvcore:: with jmvcore_ where appropriate (functions)
# Note: We are mocking specific functions, so we can just use gsub for simplicity
# or define the functions in the global environment.
# Let's define them globally as jmvcore::... will fail.
# Better approach: replace jmvcore:: with jmvcore_ in the content
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
if (!exists("multisurvivalClass")) {
    stop("multisurvivalClass not defined after sourcing!")
}
print("multisurvivalClass successfully created.")

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
# Create a simulated survival dataset
set.seed(123)
n <- 100
test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    status = sample(0:1, n, replace = TRUE),
    age = rnorm(n, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    treatment = factor(sample(c("A", "B"), n, replace = TRUE)),
    stringsAsFactors = FALSE
)

# Test 1: Basic Cox Regression
cat("Running Test 1: Basic Cox Regression\n")
options_1 <- list(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = c("treatment", "sex"),
    contexpl = "age",
    hr = TRUE,
    showSummaries = TRUE
)
res1 <- run_test("Basic Cox Regression", test_data, options_1, multisurvivalClass)

# Test 2: Person-Time Analysis
cat("Running Test 2: Person-Time Analysis\n")
options_2 <- list(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = "treatment",
    person_time = TRUE,
    showSummaries = TRUE
)
res2 <- run_test("Person-Time Analysis", test_data, options_2, multisurvivalClass)

# Test 3: AFT Model
cat("Running Test 3: AFT Model\n")
options_3 <- list(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = c("treatment"),
    use_aft = TRUE,
    aft_distribution = "weibull",
    showSummaries = TRUE
)
res3 <- run_test("AFT Model", test_data, options_3, multisurvivalClass)

# Test 4: Risk Score Calculation
cat("Running Test 4: Risk Score Calculation\n")
options_4 <- list(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = c("treatment", "sex"),
    contexpl = "age",
    calculateRiskScore = TRUE,
    numRiskGroups = "three",
    showSummaries = TRUE
)
res4 <- run_test("Risk Score Calculation", test_data, options_4, multisurvivalClass)

# Test 5: Input Validation (Missing Time)
cat("Running Test 5: Input Validation (Missing Time)\n")
options_5 <- list(
    outcome = "status",
    outcomeLevel = 1,
    explanatory = "treatment"
)
res5 <- run_test("Missing Time", test_data, options_5, multisurvivalClass)

# Test 6: Input Validation (Missing Outcome)
cat("Running Test 6: Input Validation (Missing Outcome)\n")
options_6 <- list(
    elapsedtime = "time",
    explanatory = "treatment"
)
res6 <- run_test("Missing Outcome", test_data, options_6, multisurvivalClass)

