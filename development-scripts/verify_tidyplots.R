
# Verification script for tidyplots function
# This script mocks jmvcore and tests the tidyplots function

# 1. Mock jmvcore and Dependencies ----
options(error = traceback)

# Install/Load dependencies
pkgs <- c("R6", "ggplot2", "dplyr", "tidyplots", "rlang")
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
    xvar = NULL,
    yvar = NULL,
    color = NULL,
    group = NULL,
    facet = NULL,
    plotType = "points",
    pointType = "basic",
    lineType = "direct",
    barType = "mean",
    stackType = "absolute",
    showMean = FALSE,
    meanType = "dash",
    showMedian = FALSE,
    medianType = "dash",
    showSum = FALSE,
    sumType = "dash",
    showCount = FALSE,
    countType = "dash",
    showSEM = FALSE,
    semType = "errorbar",
    showSD = FALSE,
    sdType = "errorbar",
    showCI = FALSE,
    ciType = "errorbar",
    showRange = FALSE,
    rangeType = "errorbar",
    showDistribution = FALSE,
    distributionType = "density",
    violinPoints = FALSE,
    histogramBins = 30,
    showPValue = FALSE,
    showSignificance = FALSE,
    showReferenceLines = FALSE,
    referenceX = "",
    referenceY = "",
    colorScheme = "friendly",
    alpha = 1.0,
    fontSize = 12,
    plotTitle = "",
    xLabel = "",
    yLabel = "",
    legendTitle = "",
    plotCaption = "",
    showHowTo = FALSE,
    showGlossary = FALSE,
    showSummary = FALSE,
    pointShape = 16,
    removeLegend = FALSE,
    removeLegendTitle = FALSE,
    removePadding = FALSE,
    removeXAxis = FALSE,
    removeXAxisLabels = FALSE,
    removeXAxisTitle = FALSE,
    removeYAxis = FALSE,
    removeYAxisLabels = FALSE,
    removeYAxisTitle = FALSE,
    sortXAxisLabels = FALSE,
    reverseXAxisLabels = FALSE,
    plotTheme = "default",
    removeXAxisLine = FALSE,
    removeYAxisLine = FALSE,
    showDataLabels = FALSE,
    dataLabelType = "direct",
    legendPosition = "default",
    flipOrientation = FALSE,
    paddingTop = 0.1,
    paddingBottom = 0.1,
    paddingLeft = 0.1,
    paddingRight = 0.1,
    colorSaturation = 1.0,
    dodgeWidth = 0.8,
    pointSize = 2.5,
    pointWhiteBorder = FALSE,
    lineWidth = 1.0,
    useRasterization = FALSE,
    rasterDPI = 300,
    setXLimits = FALSE,
    xMin = 0,
    xMax = 100,
    setYLimits = FALSE,
    yMin = 0,
    yMax = 100,
    xAxisTickCount = 5,
    yAxisTickCount = 5,
    fontFamily = "default",
    titleAlignment = "left",
    captionAlignment = "right",
    showMedianLine = FALSE,
    maxDataPoints = 10000,
    errorBarWidth = 0.1,
    violinScale = "area",
    boxplotOutliers = FALSE,
    boxplotNotch = FALSE,
    addSmoother = FALSE,
    smootherMethod = "lm",
    smootherSE = TRUE,
    useAutoSize = FALSE,
    plotWidth = 200,
    plotHeight = 150,
    
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
    instructions = NULL,
    plot = NULL,
    howto = NULL,
    glossary = NULL,
    summary = NULL,
    initialize = function() {
      self$instructions <- Html$new()
      self$plot <- Image$new()
      self$howto <- Html$new()
      self$glossary <- Html$new()
      self$summary <- Html$new()
    }
  )
)

# Mock Analysis class (Base)
tidyplotsBase <- R6Class("tidyplotsBase",
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

# Mock . function for localization
. <- function(x) x

# Define jmvcore_ mock objects
jmvcore_Notice <- Notice
jmvcore_NoticeType <- NoticeType
jmvcore_reject <- function(...) stop(paste(...))
jmvcore_naOmit <- na.omit

# 2. Source the function ----
source_file <- "R/tidyplots.b.R"
file_content <- readLines(source_file)

# Remove the conditional check for jmvcore to allow sourcing
start_line_idx <- grep("tidyplotsClass <-", file_content)
if (length(start_line_idx) > 0) {
  print(paste("Found tidyplotsClass definition at line:", start_line_idx))
  file_content[start_line_idx] <- "tidyplotsClass <- R6::R6Class("
} else {
  stop("Could not find tidyplotsClass definition to replace!")
}

# Replace jmvcore:: with jmvcore_
file_content <- gsub("jmvcore::", "jmvcore_", file_content)

# Eval parse directly
eval(parse(text = file_content))

# Debug: Check if class exists
if (!exists("tidyplotsClass")) {
    stop("tidyplotsClass not defined after sourcing!")
}
print("tidyplotsClass successfully created.")

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
    if (!is.null(analysis$.__enclos_env__$private$.plot)) {
        # Create a mock image object
        image_mock <- Image$new()
        result <- analysis$.__enclos_env__$private$.plot(image = image_mock)
        if (isTRUE(result)) {
            cat("Plot generated successfully.\n")
        } else {
            cat("Plot generation returned FALSE.\n")
        }
    } else {
         cat("Could not find .plot method.\n")
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

# Test 1: Basic Points Plot
cat("Running Test 1: Basic Points Plot\n")
options_1 <- list(
    xvar = "wt",
    yvar = "mpg",
    plotType = "points",
    color = "cyl"
)
res1 <- run_test("Points Plot", mtcars, options_1, tidyplotsClass)

# Test 2: Boxplot with Statistics
cat("Running Test 2: Boxplot with Statistics\n")
options_2 <- list(
    xvar = "cyl",
    yvar = "mpg",
    plotType = "boxplot",
    color = "cyl",
    showMean = TRUE,
    showPValue = TRUE
)
res2 <- run_test("Boxplot Stats", mtcars, options_2, tidyplotsClass)

# Test 3: Violin Plot with Points
cat("Running Test 3: Violin Plot with Points\n")
options_3 <- list(
    xvar = "cyl",
    yvar = "mpg",
    plotType = "violin",
    violinPoints = TRUE,
    color = "cyl"
)
res3 <- run_test("Violin Points", mtcars, options_3, tidyplotsClass)

# Test 4: Bar Plot (Mean)
cat("Running Test 4: Bar Plot (Mean)\n")
options_4 <- list(
    xvar = "cyl",
    yvar = "mpg",
    plotType = "bar",
    barType = "mean",
    color = "cyl",
    showSEM = TRUE
)
res4 <- run_test("Bar Plot Mean", mtcars, options_4, tidyplotsClass)

# Test 5: Faceting
cat("Running Test 5: Faceting\n")
options_5 <- list(
    xvar = "wt",
    yvar = "mpg",
    plotType = "points",
    facet = "am"
)
res5 <- run_test("Faceting", mtcars, options_5, tidyplotsClass)

# Test 6: Customization (Theme, Colors)
cat("Running Test 6: Customization\n")
options_6 <- list(
    xvar = "wt",
    yvar = "mpg",
    plotType = "points",
    color = "cyl",
    plotTheme = "minimal_xy",
    colorScheme = "viridis",
    plotTitle = "Custom Plot"
)
res6 <- run_test("Customization", mtcars, options_6, tidyplotsClass)

# Test 7: Error Handling (Missing Variables)
cat("Running Test 7: Error Handling (Missing Variables)\n")
options_7 <- list(
    xvar = NULL,
    yvar = NULL
)
res7 <- run_test("Missing Variables", mtcars, options_7, tidyplotsClass)
