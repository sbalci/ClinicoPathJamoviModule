
# Verification script for jjscatterstats
# This script mocks jmvcore to run the analysis logic outside of jamovi

# 1. Mock jmvcore R6 classes ----------------------------------------------
library(R6)
library(ggplot2)
library(ggstatsplot)
library(ggpubr)

# Mock jmvcore environment
jmvcore <- list()

# Mock Option classes
jmvcore$Option <- R6::R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        default = NULL,
        initialize = function(name, value=NULL, default=NULL, ...) {
            self$name <- name
            self$value <- if(is.null(value)) default else value
            self$default <- default
        }
    )
)

jmvcore$OptionString <- R6::R6Class("OptionString", inherit = jmvcore$Option)
jmvcore$OptionBool <- R6::R6Class("OptionBool", inherit = jmvcore$Option)
jmvcore$OptionInteger <- R6::R6Class("OptionInteger", inherit = jmvcore$Option)
jmvcore$OptionNumber <- R6::R6Class("OptionNumber", inherit = jmvcore$Option)
jmvcore$OptionList <- R6::R6Class("OptionList", inherit = jmvcore$Option,
    public = list(
        options = NULL,
        initialize = function(name, value=NULL, options=NULL, default=NULL, ...) {
            super$initialize(name, value, default)
            self$options <- options
        }
    )
)
jmvcore$OptionVariable <- R6::R6Class("OptionVariable", inherit = jmvcore$Option)
jmvcore$OptionVariables <- R6::R6Class("OptionVariables", inherit = jmvcore$Option)

# Mock Options container
jmvcore$Options <- R6::R6Class("Options",
    public = list(
        initialize = function(...) {},
        .addOption = function(option) {
            self[[option$name]] <- option$value
        },
        check = function(...) {}
    )
)

# Mock Analysis class
jmvcore$Analysis <- R6::R6Class("Analysis",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data=NULL, results=NULL, ...) {
            self$options <- options
            self$data <- data
            self$results <- results
            if (is.null(results)) self$results <- list()
        },
        run = function() {
            private$.run()
        }
    ),
    private = list(
        .run = function() {}
    )
)

# Mock Group class
jmvcore$Group <- R6::R6Class("Group",
    public = list(
        items = list(),
        initialize = function(...) {},
        add = function(item) {
            self$items[[item$name]] <- item
        },
        get = function(name) {
            return(self$items[[name]])
        },
        setVisible = function(...) {}
    )
)

# Mock Output classes
jmvcore$Image <- R6::R6Class("Image",
    public = list(
        name = NULL,
        plot = NULL,
        visible = TRUE,
        initialize = function(name, ...) {
            self$name <- name
        },
        setSize = function(...) {},
        setVisible = function(visible) {
            self$visible <- visible
        },
        isNotMissing = function() TRUE
    )
)

jmvcore$Html <- R6::R6Class("Html",
    public = list(
        name = NULL,
        content = NULL,
        visible = TRUE,
        initialize = function(name, ...) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        },
        setVisible = function(visible) {
            self$visible <- visible
        },
        isNotMissing = function() TRUE
    )
)

jmvcore$Preformatted <- R6::R6Class("Preformatted",
    public = list(
        name = NULL,
        content = NULL,
        visible = TRUE,
        initialize = function(name, ...) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        },
        setVisible = function(visible) {
            self$visible <- visible
        },
        isNotMissing = function() TRUE
    )
)

# Mock utility functions
jmvcore$toNumeric <- function(x) as.numeric(as.character(x))
jmvcore$format <- function(x) x
jmvcore$resolveQuo <- function(x) x
jmvcore$enquo <- function(x) x
jmvcore$marshalData <- function(...) return(list(...)[[2]]) # Return data

# 2. Define jjscatterstats classes manually --------------------------------

# Define jjscatterstatsOptions
jjscatterstatsOptions <- R6::R6Class(
    "jjscatterstatsOptions",
    inherit = jmvcore$Options,
    public = list(
        dep = NULL,
        group = NULL,
        grvar = NULL,
        colorvar = NULL,
        sizevar = NULL,
        shapevar = NULL,
        alphavar = NULL,
        labelvar = NULL,
        showRugPlot = NULL,
        marginalType = NULL,
        smoothMethod = NULL,
        typestatistics = NULL,
        mytitle = NULL,
        xtitle = NULL,
        ytitle = NULL,
        originaltheme = NULL,
        resultssubtitle = NULL,
        conflevel = NULL,
        bfmessage = NULL,
        k = NULL,
        marginal = NULL,
        xsidefill = NULL,
        ysidefill = NULL,
        pointsize = NULL,
        pointalpha = NULL,
        smoothlinesize = NULL,
        smoothlinecolor = NULL,
        plotwidth = NULL,
        plotheight = NULL,
        addGGPubrPlot = NULL,
        ggpubrPalette = NULL,
        ggpubrAddCorr = NULL,
        ggpubrCorrMethod = NULL,
        ggpubrAddSmooth = NULL,
        showExplanations = NULL,
        clinicalPreset = NULL,
        
        initialize = function(
            dep = NULL,
            group = NULL,
            grvar = NULL,
            colorvar = NULL,
            sizevar = NULL,
            shapevar = NULL,
            alphavar = NULL,
            labelvar = NULL,
            showRugPlot = FALSE,
            marginalType = "none",
            smoothMethod = "lm",
            typestatistics = "parametric",
            mytitle = "",
            xtitle = "",
            ytitle = "",
            originaltheme = FALSE,
            resultssubtitle = FALSE,
            conflevel = 0.95,
            bfmessage = FALSE,
            k = 2,
            marginal = FALSE,
            xsidefill = "#009E73",
            ysidefill = "#D55E00",
            pointsize = 3,
            pointalpha = 0.4,
            smoothlinesize = 1.5,
            smoothlinecolor = "blue",
            plotwidth = 600,
            plotheight = 450,
            addGGPubrPlot = FALSE,
            ggpubrPalette = "jco",
            ggpubrAddCorr = TRUE,
            ggpubrCorrMethod = "pearson",
            ggpubrAddSmooth = TRUE,
            showExplanations = TRUE,
            clinicalPreset = "custom", ...) {
            
            self$dep <- dep
            self$group <- group
            self$grvar <- grvar
            self$colorvar <- colorvar
            self$sizevar <- sizevar
            self$shapevar <- shapevar
            self$alphavar <- alphavar
            self$labelvar <- labelvar
            self$showRugPlot <- showRugPlot
            self$marginalType <- marginalType
            self$smoothMethod <- smoothMethod
            self$typestatistics <- typestatistics
            self$mytitle <- mytitle
            self$xtitle <- xtitle
            self$ytitle <- ytitle
            self$originaltheme <- originaltheme
            self$resultssubtitle <- resultssubtitle
            self$conflevel <- conflevel
            self$bfmessage <- bfmessage
            self$k <- k
            self$marginal <- marginal
            self$xsidefill <- xsidefill
            self$ysidefill <- ysidefill
            self$pointsize <- pointsize
            self$pointalpha <- pointalpha
            self$smoothlinesize <- smoothlinesize
            self$smoothlinecolor <- smoothlinecolor
            self$plotwidth <- plotwidth
            self$plotheight <- plotheight
            self$addGGPubrPlot <- addGGPubrPlot
            self$ggpubrPalette <- ggpubrPalette
            self$ggpubrAddCorr <- ggpubrAddCorr
            self$ggpubrCorrMethod <- ggpubrCorrMethod
            self$ggpubrAddSmooth <- ggpubrAddSmooth
            self$showExplanations <- showExplanations
            self$clinicalPreset <- clinicalPreset
        }
    )
)

# Define jjscatterstatsResults
jjscatterstatsResults <- R6::R6Class(
    "jjscatterstatsResults",
    inherit = jmvcore$Group,
    public = list(
        todo = NULL,
        plot2 = NULL,
        plot = NULL,
        plot3 = NULL,
        ggpubrPlot = NULL,
        ggpubrPlot2 = NULL,
        explanations = NULL,
        warnings = NULL,
        presetInfo = NULL,
        
        initialize = function(options) {
            super$initialize(options=options)
            self$todo <- jmvcore$Html$new(name="todo")
            self$plot2 <- jmvcore$Image$new(name="plot2")
            self$plot <- jmvcore$Image$new(name="plot")
            self$plot3 <- jmvcore$Image$new(name="plot3")
            self$ggpubrPlot <- jmvcore$Image$new(name="ggpubrPlot")
            self$ggpubrPlot2 <- jmvcore$Image$new(name="ggpubrPlot2")
            self$explanations <- jmvcore$Html$new(name="explanations")
            self$warnings <- jmvcore$Html$new(name="warnings")
            self$presetInfo <- jmvcore$Html$new(name="presetInfo")
            
            self$add(self$todo)
            self$add(self$plot2)
            self$add(self$plot)
            self$add(self$plot3)
            self$add(self$ggpubrPlot)
            self$add(self$ggpubrPlot2)
            self$add(self$explanations)
            self$add(self$warnings)
            self$add(self$presetInfo)
        }
    )
)

# Define jjscatterstatsBase
jjscatterstatsBase <- R6::R6Class(
    "jjscatterstatsBase",
    inherit = jmvcore$Analysis,
    public = list(
        initialize = function(options, data=NULL, results=NULL, ...) {
            if(is.null(results)) results <- jjscatterstatsResults$new(options)
            super$initialize(options=options, data=data, results=results)
        }
    )
)

# 3. Source the implementation file ---------------------------------------

source_mocked <- function(file) {
    print(paste("DEBUG: Sourcing", file))
    lines <- readLines(file)
    # Replace jmvcore:: with jmvcore$
    lines <- gsub("jmvcore::", "jmvcore$", lines)
    # Handle requireNamespace
    lines <- gsub('requireNamespace\\("jmvcore", quietly=TRUE\\)', 'TRUE', lines)
    
    # Eval in global env
    eval(parse(text = lines), envir = .GlobalEnv)
    print("DEBUG: Sourcing complete")
}

source_mocked("R/jjscatterstats.b.R")


# 4. Run Verification Tests -----------------------------------------------

print("=================================================================")
print("STARTING VERIFICATION OF jjscatterstats")
print("=================================================================")

# Create test data
set.seed(123)
test_data <- data.frame(
    dep = rnorm(100, 50, 10),
    group = rnorm(100, 25, 5),
    grvar = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    colorvar = factor(sample(c("Low", "High"), 100, replace = TRUE)),
    sizevar = runif(100, 1, 5),
    shapevar = factor(sample(c("S1", "S2"), 100, replace = TRUE)),
    alphavar = runif(100, 0.1, 1),
    labelvar = paste0("ID", 1:100)
)

# Test 1: Basic Scatter Plot (Parametric)
print("\n--- Test 1: Basic Scatter Plot (Parametric) ---")
options1 <- jjscatterstatsOptions$new(
    dep = "dep",
    group = "group",
    typestatistics = "parametric",
    resultssubtitle = TRUE
)
analysis1 <- jjscatterstatsClass$new(options = options1, data = test_data)
analysis1$run()
print("Run complete")
print(paste("Plot created:", !is.null(analysis1$results$plot)))
# Verify plot generation
if(!is.null(analysis1$results$plot)) {
    print("Generating plot...")
    tryCatch({
        analysis1$.__enclos_env__$private$.plot(NULL, NULL, NULL)
        print("Plot generation successful")
    }, error = function(e) {
        print(paste("Plot generation failed:", e$message))
    })
}

# Test 2: Grouped Scatter Plot
print("\n--- Test 2: Grouped Scatter Plot ---")
options2 <- jjscatterstatsOptions$new(
    dep = "dep",
    group = "group",
    grvar = "grvar",
    typestatistics = "nonparametric"
)
analysis2 <- jjscatterstatsClass$new(options = options2, data = test_data)
analysis2$run()
print("Run complete")
print(paste("Plot2 created:", !is.null(analysis2$results$plot2)))
if(!is.null(analysis2$results$plot2)) {
    print("Generating grouped plot...")
    tryCatch({
        analysis2$.__enclos_env__$private$.plot2(NULL, NULL, NULL)
        print("Grouped plot generation successful")
    }, error = function(e) {
        print(paste("Grouped plot generation failed:", e$message))
    })
}

# Test 3: Enhanced Scatter Plot
print("\n--- Test 3: Enhanced Scatter Plot ---")
options3 <- jjscatterstatsOptions$new(
    dep = "dep",
    group = "group",
    colorvar = "colorvar",
    sizevar = "sizevar",
    shapevar = "shapevar",
    showRugPlot = TRUE,
    smoothMethod = "lm"
)
analysis3 <- jjscatterstatsClass$new(options = options3, data = test_data)
analysis3$run()
print("Run complete")
print(paste("Plot3 created:", !is.null(analysis3$results$plot3)))
if(!is.null(analysis3$results$plot3)) {
    print("Generating enhanced plot...")
    tryCatch({
        analysis3$.__enclos_env__$private$.plot3(NULL, NULL, NULL)
        print("Enhanced plot generation successful")
    }, error = function(e) {
        print(paste("Enhanced plot generation failed:", e$message))
    })
}

# Test 4: Marginal Plots
print("\n--- Test 4: Marginal Plots ---")
options4 <- jjscatterstatsOptions$new(
    dep = "dep",
    group = "group",
    marginal = TRUE,
    marginalType = "histogram"
)
analysis4 <- jjscatterstatsClass$new(options = options4, data = test_data)
analysis4$run()
print("Run complete")
# Verify plot with marginals
if(!is.null(analysis4$results$plot)) {
    print("Generating plot with marginals...")
    tryCatch({
        analysis4$.__enclos_env__$private$.plot(NULL, NULL, NULL)
        print("Marginal plot generation successful")
    }, error = function(e) {
        print(paste("Marginal plot generation failed:", e$message))
    })
}

# Test 5: Clinical Preset - Biomarker Correlation
print("\n--- Test 5: Clinical Preset - Biomarker Correlation ---")
options5 <- jjscatterstatsOptions$new(
    dep = "dep",
    group = "group",
    clinicalPreset = "biomarker_correlation"
)
analysis5 <- jjscatterstatsClass$new(options = options5, data = test_data)
analysis5$run()
print("Run complete")
print(paste("Preset info visible:", analysis5$results$presetInfo$visible))
print(paste("ggpubr plot visible:", analysis5$results$ggpubrPlot$visible))
print(paste("Type statistics updated:", analysis5$options$typestatistics))
if(analysis5$results$ggpubrPlot$visible) {
    print("Generating ggpubr plot...")
    tryCatch({
        analysis5$.__enclos_env__$private$.plotGGPubr(NULL)
        print("ggpubr plot generation successful")
    }, error = function(e) {
        print(paste("ggpubr plot generation failed:", e$message))
    })
}

# Test 6: Clinical Preset - Treatment Response
print("\n--- Test 6: Clinical Preset - Treatment Response ---")
options6 <- jjscatterstatsOptions$new(
    dep = "dep",
    group = "group",
    clinicalPreset = "treatment_response_analysis"
)
analysis6 <- jjscatterstatsClass$new(options = options6, data = test_data)
analysis6$run()
print("Run complete")
print(paste("Marginal enabled:", analysis6$options$marginal))
print(paste("Type statistics updated:", analysis6$options$typestatistics))

print("\n=================================================================")
print("VERIFICATION COMPLETE")
print("=================================================================")
