
# Verification script for sequentialtests
# Mocks jmvcore and runs the function

# Mock jmvcore environment
jmvcore <- new.env()

# Mock Option classes
Option <- R6::R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value, ...) {
            self$name <- name
            self$value <- value
        }
    )
)

OptionString <- R6::R6Class("OptionString", inherit = Option)
OptionNumber <- R6::R6Class("OptionNumber", inherit = Option,
    public = list(
        initialize = function(name, value, default, min=NULL, max=NULL, ...) {
            super$initialize(name, value)
        }
    )
)
OptionBool <- R6::R6Class("OptionBool", inherit = Option,
    public = list(
        initialize = function(name, value, default, ...) {
            super$initialize(name, value)
        }
    )
)
OptionList <- R6::R6Class("OptionList", inherit = Option,
    public = list(
        initialize = function(name, value, options, default, ...) {
            super$initialize(name, value)
        }
    )
)

# Mock Result classes
ResultElement <- R6::R6Class("ResultElement",
    public = list(
        name = NULL,
        visible = TRUE,
        initialize = function(options, name, title, visible=TRUE, ...) {
            self$name <- name
            self$visible <- visible
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

Table <- R6::R6Class("Table", inherit = ResultElement,
    public = list(
        rows = list(),
        columns = list(),
        initialize = function(options, name, title, rows, columns, ...) {
            super$initialize(options, name, title, ...)
            self$columns <- columns
        },
        addRow = function(rowKey, values) {
            self$rows[[length(self$rows) + 1]] <- list(key=rowKey, values=values)
        },
        setRow = function(rowNo=NULL, rowKey=NULL, values) {
            # Simple mock implementation
            # print(paste("Setting row", rowNo, rowKey))
        }
    )
)

Html <- R6::R6Class("Html", inherit = ResultElement,
    public = list(
        content = NULL,
        setContent = function(content) {
            self$content <- content
        }
    )
)

Image <- R6::R6Class("Image", inherit = ResultElement,
    public = list(
        state = NULL,
        initialize = function(options, name, title, width, height, renderFun, ...) {
            super$initialize(options, name, title, ...)
        },
        setState = function(state) {
            self$state <- state
        }
    )
)

Group <- R6::R6Class("Group", inherit = ResultElement,
    public = list(
        initialize = function(options, name, title, ...) {
            super$initialize(options, name, title, ...)
        },
        add = function(item) {
            private$.items[[item$name]] <- item
        }
    ),
    active = list(
        items = function() private$.items
    ),
    private = list(
        .items = list()
    )
)

Analysis <- R6::R6Class("Analysis",
    public = list(
        options = NULL,
        results = NULL,
        data = NULL,
        initialize = function(options, results, data, ...) {
            self$options <- options
            self$results <- results
            self$data <- data
        },
        readDataset = function(...) {
            return(NULL)
        },
        init = function() {
            # Mock init
        },
        run = function() {
            print("DEBUG: Analysis$run called")
            print(paste("Class of self:", paste(class(self), collapse=", ")))
            print("Private members:")
            print(ls(private))
            private$.run()
        },
        .addOption = function(option) {
            # Mock .addOption
        }
    ),
    private = list(
        .run = function() {}
    )
)

Options <- R6::R6Class("Options",
    public = list(
        initialize = function(...) {
        },
        .addOption = function(option) {
            # Mock .addOption
        }
    )
)

# Assign mocks to jmvcore env
jmvcore$Option <- Option
jmvcore$OptionString <- OptionString
jmvcore$OptionNumber <- OptionNumber
jmvcore$OptionBool <- OptionBool
jmvcore$OptionList <- OptionList
jmvcore$Table <- Table
jmvcore$Html <- Html
jmvcore$Image <- Image
jmvcore$Group <- Group
jmvcore$Analysis <- Analysis
jmvcore$Options <- Options

# Simplified mock using public fields to avoid R6 active binding issues
sequentialtestsOptions <- R6::R6Class(
    "sequentialtestsOptions",
    inherit = jmvcore$Options,
    public = list(
        preset = NULL,
        test1_name = NULL,
        test1_sens = NULL,
        test1_spec = NULL,
        test2_name = NULL,
        test2_sens = NULL,
        test2_spec = NULL,
        strategy = NULL,
        prevalence = NULL,
        show_explanation = NULL,
        show_formulas = NULL,
        show_nomogram = NULL,
        
        initialize = function(
            preset = "custom",
            test1_name = "Screening Test",
            test1_sens = 0.95,
            test1_spec = 0.7,
            test2_name = "Confirmatory Test",
            test2_sens = 0.8,
            test2_spec = 0.98,
            strategy = "serial_positive",
            prevalence = 0.1,
            show_explanation = TRUE,
            show_formulas = FALSE,
            show_nomogram = FALSE, ...) {

            super$initialize(
                package="ClinicoPath",
                name="sequentialtests",
                requiresData=FALSE,
                ...)

            self$preset <- preset
            self$test1_name <- test1_name
            self$test1_sens <- test1_sens
            self$test1_spec <- test1_spec
            self$test2_name <- test2_name
            self$test2_sens <- test2_sens
            self$test2_spec <- test2_spec
            self$strategy <- strategy
            self$prevalence <- prevalence
            self$show_explanation <- show_explanation
            self$show_formulas <- show_formulas
            self$show_nomogram <- show_nomogram
        })
)

sequentialtestsResults <- R6::R6Class(
    "sequentialtestsResults",
    inherit = jmvcore$Group,
    active = list(
        summary_table = function() private$.items[["summary_table"]],
        individual_tests_table = function() private$.items[["individual_tests_table"]],
        population_flow_table = function() private$.items[["population_flow_table"]],
        explanation_text = function() private$.items[["explanation_text"]],
        formulas_text = function() private$.items[["formulas_text"]],
        plot_flow_diagram = function() private$.items[["plot_flow_diagram"]],
        plot_performance = function() private$.items[["plot_performance"]],
        plot_probability = function() private$.items[["plot_probability"]],
        plot_population_flow = function() private$.items[["plot_population_flow"]],
        clinical_guidance = function() private$.items[["clinical_guidance"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Sequential Testing Analysis")
            self$add(jmvcore$Table$new(
                options=options,
                name="summary_table",
                title="Summary of Testing Strategy",
                rows=1,
                columns=list()))
            self$add(jmvcore$Table$new(
                options=options,
                name="individual_tests_table",
                title="Individual Test Performance",
                rows=0,
                columns=list()))
            self$add(jmvcore$Table$new(
                options=options,
                name="population_flow_table",
                title="Population Flow Analysis",
                rows=0,
                columns=list()))
            self$add(jmvcore$Html$new(
                options=options,
                name="explanation_text",
                title="Explanation",
                visible="(show_explanation)"))
            self$add(jmvcore$Html$new(
                options=options,
                name="formulas_text",
                title="Formulas Used",
                visible="(show_formulas)"))
            self$add(jmvcore$Image$new(
                options=options,
                name="plot_flow_diagram",
                title="Testing Strategy Flow Diagram",
                width=600,
                height=400,
                renderFun=".plot_flow_diagram",
                visible="(show_nomogram)"))
            self$add(jmvcore$Image$new(
                options=options,
                name="plot_performance",
                title="Test Performance Comparison",
                width=600,
                height=400,
                renderFun=".plot_performance",
                visible="(show_nomogram)"))
            self$add(jmvcore$Image$new(
                options=options,
                name="plot_probability",
                title="Probability Transformation",
                width=600,
                height=400,
                renderFun=".plot_probability",
                visible="(show_nomogram)"))
            self$add(jmvcore$Image$new(
                options=options,
                name="plot_population_flow",
                title="Population Flow Visualization",
                width=600,
                height=400,
                renderFun=".plot_population_flow",
                visible="(show_nomogram)"))
            self$add(jmvcore$Html$new(
                options=options,
                name="clinical_guidance",
                title="Clinical Guidance"))}))

sequentialtestsBase <- R6::R6Class(
    "sequentialtestsBase",
    inherit = jmvcore$Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                options = options,
                results = sequentialtestsResults$new(options=options),
                data = data)
        }))

# Source the implementation file
source("R/sequentialtests.b.R")

# Test 1: Serial Positive Strategy
print("--- Test 1: Serial Positive Strategy ---")
options1 <- sequentialtestsOptions$new(
    preset = "custom",
    test1_name = "Screening",
    test1_sens = 0.95,
    test1_spec = 0.70,
    test2_name = "Confirmation",
    test2_sens = 0.80,
    test2_spec = 0.98,
    strategy = "serial_positive",
    prevalence = 0.10,
    show_explanation = TRUE,
    show_formulas = TRUE,
    show_nomogram = TRUE
)

print("DEBUG: Checking options1")
print(paste("test1_sens:", options1$test1_sens))

analysis1 <- sequentialtestsClass$new(
    options = options1,
    data = NULL
)

analysis1$init()
print("DEBUG: analysis1 initialized")
print(paste("Class of analysis1:", paste(class(analysis1), collapse=", ")))
print("Names of analysis1:")
print(names(analysis1))
print("Is run a function?")
print(is.function(analysis1$run))
analysis1$run()
print("Test 1 completed successfully")

# Test 2: Serial Negative Strategy
print("\n--- Test 2: Serial Negative Strategy ---")
options2 <- sequentialtestsOptions$new(
    preset = "custom",
    test1_name = "Test A",
    test1_sens = 0.85,
    test1_spec = 0.90,
    test2_name = "Test B",
    test2_sens = 0.75,
    test2_spec = 0.85,
    strategy = "serial_negative",
    prevalence = 0.05
)

analysis2 <- sequentialtestsClass$new(
    options = options2,
    data = NULL
)

analysis2$init()
analysis2$run()
print("Test 2 completed successfully")

# Test 3: Parallel Strategy
print("\n--- Test 3: Parallel Strategy ---")
options3 <- sequentialtestsOptions$new(
    preset = "custom",
    test1_name = "Test X",
    test1_sens = 0.80,
    test1_spec = 0.95,
    test2_name = "Test Y",
    test2_sens = 0.70,
    test2_spec = 0.90,
    strategy = "parallel",
    prevalence = 0.15
)

analysis3 <- sequentialtestsClass$new(
    options = options3,
    data = NULL
)

analysis3$init()
analysis3$run()
print("Test 3 completed successfully")

print("\nVerification completed.")
