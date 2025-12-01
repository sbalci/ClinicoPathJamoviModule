
# Verification script for statsplot2 function
# This script mocks jmvcore and tests the statsplot2 function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "ggplot2", "dplyr", "ggstatsplot", "ggalluvial", "easyalluvial", "glue")
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
            # cat(paste0("[", self$type, "] ", self$name, ": ", content, "\n"))
        }
    )
)

# Mock Options List
Options <- R6Class("Options",
  lock_objects = FALSE,
  public = list(
    data = NULL,
    dep = NULL,
    group = NULL,
    grvar = NULL,
    direction = "independent",
    distribution = "p",
    alluvsty = "t1",
    excl = FALSE,
    sampleLarge = TRUE,
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

# Mock Image
Image <- R6Class("Image",
  public = list(
    state = NULL,
    visible = TRUE,
    width = 600,
    height = 450,
    initialize = function(...) {},
    setSize = function(width, height) {
      self$width <- width
      self$height <- height
    },
    setVisible = function(visible) {
      self$visible <- visible
    },
    setState = function(state) {
      self$state <- state
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

# Mock Results
Results <- R6Class("Results",
  public = list(
    todo = NULL,
    plot = NULL,
    ExplanationMessage = NULL,
    items = list(),
    initialize = function() {
      self$todo <- Html$new()
      self$plot <- Image$new()
      self$ExplanationMessage <- Html$new()
    },
    insert = function(index, item) {
        self$items <- append(self$items, list(item))
    }
  )
)

# Mock Analysis class (Base)
statsplot2Base <- R6Class("statsplot2Base",
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
  ),
  private = list(
    .checkpoint = function(flush = TRUE) {}
  )
)

# Mock . function for localization
. <- function(x) x

# Mock naOmit
naOmit <- function(data) {
    na.omit(data)
}

# 2. Source the function ----
source_file <- "R/statsplot2.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("statsplot2Class <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found statsplot2Class definition at line:", start_line_idx))
  file_content[start_line_idx] <- "statsplot2Class <-"
} else {
  stop("Could not find statsplot2Class definition to replace!")
}

# Replace jmvcore:: with jmvcore_
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Fix glue format specifiers that might cause issues
file_content <- gsub("\\{n_used:,\\}", "{n_used}", file_content)
file_content <- gsub("\\{n_total:,\\}", "{n_total}", file_content)
file_content <- gsub("\\{original_nrow:,\\}", "{original_nrow}", file_content)
file_content <- gsub("\\{sample_size:,\\}", "{sample_size}", file_content)

# Define jmvcore_ mock objects matching the gsub replacement
jmvcore_Notice <- Notice
jmvcore_NoticeType <- NoticeType
jmvcore_reject <- function(...) stop(paste(...))
jmvcore_naOmit <- naOmit

# Also define jmvcore_ list just in case
jmvcore_ <- list(
    Notice = Notice,
    NoticeType = NoticeType,
    reject = jmvcore_reject,
    naOmit = naOmit
)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
print("Objects in global environment:")
print(ls())

# Ensure statsplot2Class is available
if (!exists("statsplot2Class")) {
    stop("statsplot2Class not defined after sourcing!")
}
print("statsplot2Class successfully created.")

# 3. Test Helper Function ----
run_test <- function(test_name, data, options_list, class_def) {
  cat(paste0("\n--- Test: ", test_name, " ---\n"))
  
  # Re-instantiate Options
  options <- Options$new()
  for (name in names(options_list)) {
    options$set(name, options_list[[name]])
  }
  
  analysis <- class_def$new(options = options, data = data)
  
  # Initialize
  if (!is.null(analysis$.__enclos_env__$private$.init)) {
      analysis$.__enclos_env__$private$.init()
  }
  
  # Run
  tryCatch({
    analysis$.__enclos_env__$private$.run()
    cat("Run complete.\n")
    
    # Trigger Plot Generation
    cat("Generating Plot...\n")
    if (!is.null(analysis$.__enclos_env__$private$.generatePlot)) {
         analysis_info <- analysis$.__enclos_env__$private$.detectAnalysisType()
         if (!is.null(analysis_info)) {
             prepared_data <- analysis$.__enclos_env__$private$.prepareDataForPlot(analysis_info)
             plot <- analysis$.__enclos_env__$private$.generatePlot(analysis_info, prepared_data)
             if (!is.null(plot)) {
                 cat("Plot object generated successfully.\n")
             } else {
                 cat("Plot generation returned NULL.\n")
             }
         } else {
             cat("Analysis info is NULL (likely due to error in run).\n")
         }
    } else {
         cat("Could not find .generatePlot method.\n")
    }

    # Check Notices/Explanation
    if (!is.null(analysis$results$ExplanationMessage$content)) {
        cat("Explanation message generated.\n")
    }
    
    if (length(analysis$results$items) > 0) {
        cat(paste("Generated", length(analysis$results$items), "notices.\n"))
        for (item in analysis$results$items) {
             if (inherits(item, "Notice")) {
                 cat(paste0("Notice [", item$type, "]: ", item$name, "\n"))
             }
        }
    }
    
    return(analysis)
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    traceback()
    return(NULL)
  })
}

# 4. Run Tests ----

# Prepare Data
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)

# Test 1: Independent - Factor vs Continuous (Violin)
cat("Running Test 1: Independent - Factor vs Continuous (Violin)\n")
options_1 <- list(
    dep = "mpg",
    group = "cyl",
    direction = "independent",
    distribution = "p"
)
res1 <- run_test("Violin Plot", mtcars, options_1, statsplot2Class)

# Test 2: Independent - Continuous vs Continuous (Scatter)
cat("Running Test 2: Independent - Continuous vs Continuous (Scatter)\n")
options_2 <- list(
    dep = "mpg",
    group = "wt",
    direction = "independent",
    distribution = "p"
)
res2 <- run_test("Scatter Plot", mtcars, options_2, statsplot2Class)

# Test 3: Independent - Factor vs Factor (Bar)
cat("Running Test 3: Independent - Factor vs Factor (Bar)\n")
options_3 <- list(
    dep = "am",
    group = "cyl",
    direction = "independent"
)
res3 <- run_test("Bar Chart", mtcars, options_3, statsplot2Class)

# Test 4: Grouping (Split By)
cat("Running Test 4: Grouping (Split By)\n")
options_4 <- list(
    dep = "mpg",
    group = "cyl",
    grvar = "am",
    direction = "independent"
)
res4 <- run_test("Grouped Violin Plot", mtcars, options_4, statsplot2Class)

# Test 5: Data Validation - Insufficient Data
cat("Running Test 5: Data Validation - Insufficient Data\n")
options_5 <- list(
    dep = "mpg",
    group = "cyl",
    direction = "independent"
)
res5 <- run_test("Insufficient Data", mtcars[1, ], options_5, statsplot2Class)

# Test 6: Assumption Checks - Small Sample
cat("Running Test 6: Assumption Checks - Small Sample\n")
options_6 <- list(
    dep = "mpg",
    group = "cyl",
    direction = "independent",
    distribution = "p"
)
res6 <- run_test("Small Sample Warning", mtcars[1:10, ], options_6, statsplot2Class)

# Test 7: Fallback Mechanism (Unsupported Combination)
cat("Running Test 7: Fallback Mechanism\n")
options_7 <- list(
    dep = "mpg",
    group = "wt",
    direction = "repeated"
)
res7 <- run_test("Fallback Plot", mtcars, options_7, statsplot2Class)
