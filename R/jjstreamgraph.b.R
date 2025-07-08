# jjstreamgraph.b.R

#' @title StreamGraphs
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%

jjstreamgraphClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjstreamgraphClass",
    inherit = jjstreamgraphBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,

        # init ----
        .init = function() {
            # Pre-prepare data and options for performance
            private$.prepareData()
            private$.prepareOptions()
        },

        # Performance optimization methods ----
        
        .prepareData = function() {
            # Create a simple hash of current data to detect changes
            current_hash <- paste(self$options$timeVar, self$options$valueVar, self$options$groupVar, nrow(self$data), collapse = "_")
            
            # Only reprocess if data has changed
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                
                if (!is.null(self$options$timeVar) && !is.null(self$options$valueVar) && !is.null(self$options$groupVar)) {
                    mydata <- self$data
                    
                    # Convert variables to numeric
                    mydata[[self$options$timeVar]] <- jmvcore::toNumeric(mydata[[self$options$timeVar]])
                    mydata[[self$options$valueVar]] <- jmvcore::toNumeric(mydata[[self$options$valueVar]])
                    
                    # Exclude NA values once
                    mydata <- jmvcore::naOmit(mydata)
                    
                    private$.prepared_data <- mydata
                    private$.data_hash <- current_hash
                } else {
                    private$.prepared_data <- NULL
                }
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            # Cache processed options
            if (is.null(private$.prepared_options)) {
                
                private$.prepared_options <- list(
                    timeVar = self$options$timeVar,
                    valueVar = self$options$valueVar,
                    groupVar = self$options$groupVar,
                    offset = self$options$offset,
                    interpolate = self$options$interpolate,
                    palette = self$options$palette,
                    width = self$options$width,
                    height = self$options$height
                )
            }
            
            return(private$.prepared_options)
        },

        # run ----
        .run = function() {

            # Initial Message and Validation ----
            if (is.null(self$options$timeVar) ||
                is.null(self$options$valueVar) ||
                is.null(self$options$groupVar)) {

                # Set informational message for user
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath StreamGraphs
                <br><br>
                This tool will help you create interactive stream graphs for time series data.
                <br><br>
                Please select:
                <br>• Time Variable: Represents the time axis (x-axis)
                <br>• Value Variable: Represents the measured values (y-axis height)
                <br>• Grouping Variable: Categorizes data into different streams
                <br><br>
                This function uses the streamgraph R package. See documentation 
                <a href='https://cran.r-project.org/web/packages/streamgraph/streamgraph.pdf' target='_blank'>here</a>.
                <br><hr>"
                )

                self$results$StreamGraph$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use prepared data and options ----
            data <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            if (is.null(data)) {
                return()
            }

            # Additional validation
            if (nrow(data) == 0) {
                warning_msg <- "<br>No complete data rows available after removing missing values.<br>Please check your data for missing values in the selected variables.<br><hr>"
                self$results$StreamGraph$setContent(warning_msg)
                return()
            }

            # Create streamgraph using optimized data and options
            tryCatch({
                plot <- streamgraph::streamgraph(
                    data = data,
                    key = opts$groupVar,
                    value = opts$valueVar,
                    date = opts$timeVar,
                    offset = opts$offset,
                    interpolate = opts$interpolate,
                    width = opts$width,
                    height = opts$height
                )

                # Add color palette
                plot <- streamgraph::sg_fill_brewer(plot, opts$palette)

                # Send to output
                self$results$StreamGraph$setContent(plot)
                
            }, error = function(e) {
                error_msg <- paste0(
                    "<br>Error creating streamgraph: ", e$message, 
                    "<br><br>Please check that:",
                    "<br>• Time variable contains numeric or date values",
                    "<br>• Value variable contains numeric values",
                    "<br>• Grouping variable is categorical",
                    "<br>• Data has multiple time points for each group",
                    "<br><hr>"
                )
                self$results$StreamGraph$setContent(error_msg)
            })
        }
    )
)
