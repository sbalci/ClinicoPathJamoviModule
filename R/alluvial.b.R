#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
#' @import easyalluvial
#'
#' @description 
#' This tool creates Alluvial Diagrams (Alluvial Plots) to visualize the flow of 
#' categorical data across multiple dimensions. Alluvial diagrams are particularly 
#' useful for showing how categorical variables relate to each other and how 
#' observations flow between different categories.
#'
#' Features:
#' - Multiple variable alluvial plots with configurable maximum variables
#' - Condensation plots for detailed variable analysis  
#' - Marginal histograms for additional context
#' - Flexible orientation (horizontal/vertical)
#' - Customizable bin labels and fill options
#' - Comprehensive data validation for optimal results
#'

alluvialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "alluvialClass",
    inherit = alluvialBase,
    private = list(
        
        # Shared validation helper to reduce duplication
        .validateAlluvialInputs = function() {
            if (is.null(self$options$vars) || length(self$options$vars) == 0)
                return(FALSE)
            
            if (length(self$options$vars) < 2)
                stop('Alluvial diagrams require at least 2 variables. Please select additional variables.')
            
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows. Please check your data.')
            
            # Validate that variables are appropriate for alluvial diagrams
            private$.validateVariableTypes(self$options$vars)
            
            return(TRUE)
        },

        # Data type validation helper
        .validateVariableTypes = function(vars) {
            for (var in vars) {
                if (!(var %in% names(self$data))) {
                    stop(paste("Variable '", var, "' not found in the data."))
                }
                
                var_data <- self$data[[var]]
                
                # Check if variable is numeric with too many unique values (likely continuous)
                if (is.numeric(var_data)) {
                    unique_values <- length(unique(var_data[!is.na(var_data)]))
                    total_values <- sum(!is.na(var_data))
                    
                    # If more than 20 unique values or >50% unique values, warn user
                    if (unique_values > 20 || (unique_values / total_values) > 0.5) {
                        warning(paste("Variable '", var, "' appears to be continuous (", unique_values, 
                                    " unique values). Alluvial diagrams work best with categorical variables. ",
                                    "Consider binning this variable first."))
                    }
                }
            }
        },
        .run = function() {

            # Input Validation ----

            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                # ToDo Message ----
                todo <- "
                <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
                  <h2>Welcome to ClinicoPath</h2>
                  <p>This tool will help you form Alluvial Diagrams (Alluvial Plots).</p>
                  <p><strong>Instructions:</strong> Please select at least <em>2 variables</em> to create an alluvial diagram.</p>
                  <p>Alluvial diagrams show the flow of categorical data across multiple dimensions.</p>
                  <hr>
                </div>
                "

                html <- self$results$todo
                html$setContent(todo)

            } else {
                # Clear the to-do message
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

                # Use shared validation logic
                private$.validateAlluvialInputs()

                # Validate condensation variable if provided
                if (!is.null(self$options$condensationvar) && 
                    length(self$options$condensationvar) > 0 && 
                    !(self$options$condensationvar %in% names(self$data))) {
                    stop(paste("Condensation variable '", self$options$condensationvar, 
                              "' not found in the data. Please select a valid variable."))
                }

            }

        }

        ,

        .plot = function(image, ggtheme, theme, ...) {
            # Main alluvial plot generation function
            
            # Input validation using shared helper
            if (!private$.validateAlluvialInputs())
                return()

            # Data Preparation ----
            # Extract selected variables and create working dataset
            varsName <- self$options$vars
            mydata <- jmvcore::select(self$data, c(varsName))

            # Handle missing values based on user preference
            excl <- self$options$excl
            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Configure plot aesthetics ----
            # Set color fill strategy for the alluvial flows
            fill <- jmvcore::composeTerm(self$options$fill)

            # Configure bin labels with custom override capability
            bin <- self$options$bin
            custombinlabels <- self$options$custombinlabels

            # Use custom bin labels if provided, otherwise use bin option
            if (!is.null(custombinlabels) && custombinlabels != "") {
                bin <- trimws(strsplit(custombinlabels, ",")[[1]])
            } else if (bin == "default") {
                bin <- c("LL", "ML", "M", "MH", "HH")
            }

            # Generate core alluvial plot ----
            # Using easyalluvial package for robust alluvial diagram generation
            # Reference: https://erblast.github.io/easyalluvial/
            maxvars <- self$options$maxvars

            plot <- easyalluvial::alluvial_wide(
                data = mydata,
                max_variables = maxvars,
                fill_by = fill,
                verbose = TRUE,
                bin_labels = bin
            )

            # Add marginal histograms if requested ----
            # Marginal plots provide additional context by showing distributions
            marg <- self$options$marg
            if (marg) {
                plot <- easyalluvial::add_marginal_histograms(
                    p = plot,
                    data_input = mydata,
                    keep_labels = TRUE,
                    top = TRUE,
                    plot = TRUE
                )
            }

            # Configure plot orientation ----
            # Support both vertical (default) and horizontal layouts
            orient <- self$options$orient
            if (orient != "vert") {
                plot <- plot + ggplot2::coord_flip()
            }

            # Apply custom title if specified ----
            # Note: Custom titles are incompatible with marginal plots
            usetitle <- self$options$usetitle
            if (marg && usetitle) {
                stop("Custom titles cannot be used with marginal plots. Please either disable marginal plots or use the default title.")
            }
            
            if (!marg && usetitle) {
                mytitle <- self$options$mytitle
                plot <- plot + ggplot2::ggtitle(mytitle)
            }

            # Render the final plot
            print(plot)
            TRUE
        }

        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # Condensation plot generation function
            # Creates a detailed view of how one specific variable relates to others
            
            # Input validation - requires both variables and condensation variable
            if (is.null(self$options$condensationvar) || is.null(self$options$vars))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows. Please check your data.')

            # Data preparation for condensation analysis ----
            # Extract the primary condensation variable
            condvarName <- self$options$condensationvar
            condvarName <- jmvcore::composeTerm(components = condvarName)
            mydata <- self$data

            # Generate condensation plot ----
            # Condensation plots show detailed relationships between the primary variable
            # and all other variables in the dataset
            plot2 <- easyalluvial::plot_condensation(
                df = mydata,
                first = condvarName
            )

            # Render the condensation plot
            print(plot2)
            TRUE
        }
    )
)
