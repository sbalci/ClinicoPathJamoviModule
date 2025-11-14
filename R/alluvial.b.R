#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom easyalluvial alluvial_wide add_marginal_histograms plot_condensation
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

            if (length(self$options$vars) < 2) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>⚠️ Insufficient Variables</h4>",
                    "<p style='color: #721c24;'>Alluvial diagrams require at least <strong>2 variables</strong>.</p>",
                    "<p>Please select additional variables from the left panel.</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                return(FALSE)
            }

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>⚠️ No Data Available</h4>",
                    "<p style='color: #721c24;'>Data contains no (complete) rows.</p>",
                    "<p>Please check your data for missing values or filtering issues.</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                return(FALSE)
            }

            # Validate that variables are appropriate for alluvial diagrams
            if (!private$.validateVariableTypes(self$options$vars)) {
                return(FALSE)
            }

            # Clear warnings if everything is valid
            self$results$dataWarning$setContent("")
            return(TRUE)
        },

        # Data type validation and discretization helper
        .validateVariableTypes = function(vars) {
            for (var in vars) {
                if (!(var %in% names(self$data))) {
                    html <- paste0(
                        "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #721c24;'>⚠️ Variable Not Found</h4>",
                        "<p style='color: #721c24;'>Variable '<strong>", var, "</strong>' not found in the data.</p>",
                        "<p>Please ensure all selected variables exist in your dataset.</p>",
                        "</div>"
                    )
                    self$results$dataWarning$setContent(html)
                    return(FALSE)
                }

                var_data <- self$data[[var]]

                # Check if variable is numeric with too many unique values (likely continuous)
                if (is.numeric(var_data)) {
                    unique_values <- length(unique(var_data[!is.na(var_data)]))
                    total_values <- sum(!is.na(var_data))

                    # If more than 10 unique values, strongly recommend binning
                    if (unique_values > 10) {
                        html <- paste0(
                            "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                            "<h4 style='margin-top: 0; color: #856404;'>⚠️ Continuous Variable Detected</h4>",
                            "<p style='color: #856404;'>Variable '<strong>", var, "</strong>' has <strong>", unique_values, "</strong> unique values and appears continuous.</p>",
                            "<p><strong>⚠️ Security Warning:</strong> Continuous variables can expose individual patient measurements in alluvial plots.</p>",
                            "<hr style='border-color: #ffc107;'>",
                            "<p><strong>Required Action:</strong> Please bin this variable before creating the plot.</p>",
                            "<p style='margin: 10px 0;'><strong>Option 1: jamovi Transform</strong></p>",
                            "<ol style='margin-left: 20px;'>",
                            "<li>Go to <strong>Data → Compute</strong></li>",
                            "<li>Create a new variable with binned categories</li>",
                            "<li>Example formula: <code>IF(", var, " &lt; 25, '0-25', IF(", var, " &lt; 50, '25-50', '50+'))</code></li>",
                            "</ol>",
                            "<p style='margin: 10px 0;'><strong>Option 2: R Console</strong></p>",
                            "<pre style='background-color: #fff; padding: 10px; border: 1px solid #ddd; border-radius: 4px;'>",
                            "# Create binned variable\ndata$", var, "_binned <- cut(data$", var, ", \n    breaks = 5, \n    labels = c('Very Low', 'Low', 'Medium', 'High', 'Very High'))",
                            "</pre>",
                            "<p><strong>After binning:</strong> Select the new binned variable instead of the continuous one.</p>",
                            "</div>"
                        )
                        self$results$dataWarning$setContent(html)
                        return(FALSE)
                    }
                }
            }
            return(TRUE)
        },

        # Automatic discretization helper for numeric variables
        .discretizeData = function(data, bin_method = "default") {
            # Apply discretization to numeric columns
            for (col in names(data)) {
                if (is.numeric(data[[col]])) {
                    unique_vals <- length(unique(data[[col]][!is.na(data[[col]])]))

                    # Only discretize if more than 5 unique values
                    if (unique_vals > 5) {
                        # Use quantile-based binning for discretization
                        data[[col]] <- cut(
                            data[[col]],
                            breaks = 5,
                            labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                            include.lowest = TRUE,
                            ordered_result = TRUE
                        )
                    }
                }
            }
            return(data)
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

            # Configure bin labels with proper binning method
            bin_option <- self$options$bin
            custombinlabels <- self$options$custombinlabels

            # Determine bin labels based on user selection
            # Note: easyalluvial's bin_labels parameter only controls label display,
            # not the binning method itself. Actual binning is done by easyalluvial internally.
            if (!is.null(custombinlabels) && custombinlabels != "") {
                # Custom labels provided by user
                bin <- trimws(strsplit(custombinlabels, ",")[[1]])
            } else {
                # Use predefined labels based on bin option
                bin <- switch(bin_option,
                    "default" = c("LL", "ML", "M", "MH", "HH"),
                    "mean" = "mean",
                    "median" = "median",
                    "min_max" = c("min", "max"),
                    "cuts" = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                    c("LL", "ML", "M", "MH", "HH")  # fallback
                )
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

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>⚠️ No Data Available</h4>",
                    "<p style='color: #721c24;'>Data contains no (complete) rows.</p>",
                    "<p>Please check your data for missing values or filtering issues.</p>",
                    "</div>"
                )
                self$results$condensationWarning$setContent(html)
                return()
            }

            # Data preparation for condensation analysis ----
            # CRITICAL: Only use selected variables to prevent PHI leakage
            condvarName <- self$options$condensationvar
            condvarName <- jmvcore::composeTerm(components = condvarName)

            # Ensure condensation variable is included in selected variables
            varsName <- self$options$vars
            if (!(condvarName %in% varsName)) {
                varsName <- c(condvarName, varsName)
            }

            # Extract ONLY selected variables (prevent PHI leakage)
            mydata <- jmvcore::select(self$data, varsName)

            # Validate condensation variable is appropriate (not continuous)
            condvar_data <- mydata[[condvarName]]
            if (is.numeric(condvar_data)) {
                unique_vals <- length(unique(condvar_data[!is.na(condvar_data)]))
                if (unique_vals > 10) {
                    html <- paste0(
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>⚠️ Continuous Condensation Variable</h4>",
                        "<p style='color: #856404;'>Condensation variable '<strong>", condvarName, "</strong>' has <strong>", unique_vals, "</strong> unique values.</p>",
                        "<p><strong>⚠️ Security Warning:</strong> Continuous variables can expose patient-level data in condensation plots.</p>",
                        "<hr style='border-color: #ffc107;'>",
                        "<p><strong>Required Action:</strong> Condensation plots require categorical variables.</p>",
                        "<p style='margin: 10px 0;'><strong>Solutions:</strong></p>",
                        "<ol style='margin-left: 20px;'>",
                        "<li>Select a different <strong>categorical</strong> variable for condensation</li>",
                        "<li>Or bin '<strong>", condvarName, "</strong>' first using Data → Compute</li>",
                        "<li>Example: Create categories like 'Low', 'Medium', 'High'</li>",
                        "</ol>",
                        "</div>"
                    )
                    self$results$condensationWarning$setContent(html)
                    return()
                }
            }

            # Clear warnings if validation passes
            self$results$condensationWarning$setContent("")

            # Handle missing values based on user preference
            excl <- self$options$excl
            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Generate condensation plot ----
            # Condensation plots show detailed relationships between the primary variable
            # and selected variables only (not the entire dataset)
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
