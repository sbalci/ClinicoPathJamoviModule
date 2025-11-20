#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom easyalluvial alluvial_wide add_marginal_histograms plot_condensation
#' @importFrom ggalluvial geom_alluvium geom_stratum stat_alluvium stat_stratum StatStratum
#' @importFrom rlang sym
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
#' - Multiple plot engines (easyalluvial and ggalluvial)
#' - Color palettes and theme styling
#' - Sankey diagram styling with curve types
#' - Comprehensive data validation for optimal results
#'

alluvialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "alluvialClass",
    inherit = alluvialBase,
    private = list(

        # Utility to escape variable names for safe use in outputs
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Shared validation helper to reduce duplication
        .validateAlluvialInputs = function() {
            if (is.null(self$options$vars) || length(self$options$vars) == 0)
                return(FALSE)

            if (length(self$options$vars) < 2) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>Insufficient Variables</h4>",
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
                    "<h4 style='margin-top: 0; color: #721c24;'>No Data Available</h4>",
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
            warning_messages <- c()

            for (var in vars) {
                if (!(var %in% names(self$data))) {
                    var_safe <- htmltools::htmlEscape(var)
                    html <- paste0(
                        "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #721c24;'>Variable Not Found</h4>",
                        "<p style='color: #721c24;'>Variable '<strong>", var_safe, "</strong>' not found in the data.</p>",
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

                    # If more than 10 unique values, show warning but allow plot to proceed
                    if (unique_values > 10) {
                        var_safe <- htmltools::htmlEscape(var)

                        # Collect warning messages for the notice banner
                        warning_messages <- c(warning_messages, sprintf(
                            'Variable "%s" has %d unique values and appears continuous.',
                            var_safe, unique_values
                        ))

                        # Keep Html for detailed guidance
                        html <- paste0(
                            "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                            "<h4 style='margin-top: 0; color: #856404;'>Continuous Variable Detected - Plot Will Proceed</h4>",
                            "<p style='color: #856404;'>Variable '<strong>", var_safe, "</strong>' has <strong>", unique_values, "</strong> unique values and appears continuous.</p>",
                            "<p><strong>Note:</strong> The plot will be generated, but continuous variables may create many thin flows that are difficult to interpret. For better visualization, consider creating categorical bins.</p>",
                            "<hr style='border-color: #ffc107;'>",
                            "<p><strong>Recommended: Create Categories for Better Visualization</strong></p>",
                            "<p style='margin: 10px 0;'><strong>Option 1: jamovi Transform</strong></p>",
                            "<ol style='margin-left: 20px;'>",
                            "<li>Go to <strong>Data -> Compute</strong></li>",
                            "<li>Create a new variable with binned categories</li>",
                            "<li>Example formula: <code>IF(", var, " &lt; 25, '0-25', IF(", var, " &lt; 50, '25-50', '50+'))</code></li>",
                            "</ol>",
                            "<p style='margin: 10px 0;'><strong>Option 2: R Console</strong></p>",
                            "<pre style='background-color: #fff; padding: 10px; border: 1px solid #ddd; border-radius: 4px;'>",
                            "# Create binned variable\ndata$", var, "_binned <- cut(data$", var, ", \n    breaks = 5, \n    labels = c('Very Low', 'Low', 'Medium', 'High', 'Very High'))",
                            "</pre>",
                            "<p><strong>After binning:</strong> Select the new binned variable instead of the continuous one for clearer flow patterns.</p>",
                            "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'><em>Privacy Note: Continuous variables with many unique values may reveal individual data points. Consider binning for aggregated views.</em></p>",
                            "</div>"
                        )
                        self$results$dataWarning$setContent(html)
                        # Continue with plot generation - don't return FALSE
                    }
                }
            }

            # Set warning notice if there are continuous variables
            if (length(warning_messages) > 0) {
                warning_html <- paste0(
                    '<div style="background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 5px 0;">',
                    '<strong>Warning:</strong> ', paste(warning_messages, collapse = ' '), ' Consider binning for clearer visualization.',
                    '</div>'
                )
                self$results$noticeWarning$setContent(warning_html)
                self$results$noticeWarning$setVisible(TRUE)
            } else {
                self$results$noticeWarning$setVisible(FALSE)
            }

            return(TRUE)
        },

        # Helper method to create ggalluvial plots
        .createGgalluvialPlot = function(data, vars, fill_var, weight_var = NULL) {
            # Check for required package
            if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                stop("Package 'ggalluvial' is required for manual control engine. Please install it.")
            }

            # Prepare data - convert to factors
            for (var in vars) {
                data[[var]] <- as.factor(data[[var]])
            }

            # Get options
            sankey_style <- self$options$sankeyStyle
            curve_type <- self$options$curveType
            label_nodes <- self$options$labelNodes
            show_counts <- self$options$showCounts

            # Force sigmoid for Sankey style
            if (sankey_style) {
                curve_type <- "sigmoid"
            }

            # Create axis aesthetics dynamically
            n_vars <- length(vars)
            axis_names <- paste0("axis", 1:n_vars)

            # Build the aes call
            aes_args <- list()
            for (i in 1:n_vars) {
                aes_args[[axis_names[i]]] <- rlang::sym(vars[i])
            }

            # Add weight if provided
            if (!is.null(weight_var) && weight_var %in% names(data)) {
                aes_args$y <- rlang::sym(weight_var)
            }

            # Create base plot
            plot <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_args))

            # Set widths based on style
            if (sankey_style) {
                stratum_width <- 1/8
                alluvium_width <- 1/2
            } else {
                stratum_width <- 1/3
                alluvium_width <- 1/3
            }

            # Add alluvium (flows)
            plot <- plot +
                ggalluvial::geom_alluvium(
                    ggplot2::aes(fill = !!rlang::sym(fill_var)),
                    alpha = 0.8,
                    curve_type = curve_type,
                    width = alluvium_width
                )

            # Add stratum (nodes)
            plot <- plot +
                ggalluvial::geom_stratum(
                    width = stratum_width,
                    alpha = 0.8,
                    color = "white"
                )

            # Add labels if requested
            if (label_nodes) {
                plot <- plot +
                    ggplot2::geom_text(
                        stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3
                    )
            }

            # Add counts if requested
            if (show_counts) {
                plot <- plot +
                    ggplot2::geom_text(
                        stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(count)),
                        size = 2.5,
                        vjust = -0.5
                    )
            }

            return(plot)
        },

        # Helper to apply color palette
        .applyColorPalette = function(plot, palette) {
            if (palette == "default") {
                return(plot)
            }

            colors <- switch(palette,
                "viridis" = ggplot2::scale_fill_viridis_d(),
                "plasma" = ggplot2::scale_fill_viridis_d(option = "plasma"),
                "set3" = ggplot2::scale_fill_brewer(type = "qual", palette = "Set3"),
                "pastel1" = ggplot2::scale_fill_brewer(type = "qual", palette = "Pastel1"),
                "dark2" = ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2"),
                NULL
            )

            if (!is.null(colors)) {
                plot <- plot + colors
            }

            return(plot)
        },

        # Helper to apply theme style
        .applyThemeStyle = function(plot, theme_style) {
            theme_func <- switch(theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "grey" = ggplot2::theme_grey(),
                "bw" = ggplot2::theme_bw(),
                NULL
            )

            if (!is.null(theme_func)) {
                plot <- plot + theme_func
            }

            return(plot)
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

                # Clear notices when no variables selected
                self$results$noticeError$setVisible(FALSE)
                self$results$noticeWarning$setVisible(FALSE)
                self$results$noticeInfo$setVisible(FALSE)

            } else {
                # Clear the to-do message
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

                # Use shared validation logic
                if (!private$.validateAlluvialInputs()) {
                    # Use pre-defined HTML output instead of dynamic Notice
                    error_html <- paste0(
                        '<div style="background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0;">',
                        '<strong>Error:</strong> Alluvial diagram requires at least 2 variables with valid data. Please check variable selection and data.',
                        '</div>'
                    )
                    self$results$noticeError$setContent(error_html)
                    self$results$noticeError$setVisible(TRUE)
                    return()
                }

                # Validate condensation variable if provided
                if (!is.null(self$options$condensationvar) &&
                    length(self$options$condensationvar) > 0 &&
                    !(self$options$condensationvar %in% names(self$data))) {
                    # Use pre-defined HTML output instead of dynamic Notice
                    error_html <- sprintf(
                        '<div style="background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0;">
                        <strong>Error:</strong> Condensation variable "%s" not found in data. Please select a valid variable.
                        </div>',
                        htmltools::htmlEscape(self$options$condensationvar)
                    )
                    self$results$noticeError$setContent(error_html)
                    self$results$noticeError$setVisible(TRUE)
                    return()
                }

                # Clear error notice if validation passes
                self$results$noticeError$setVisible(FALSE)

                # Add INFO notice for successful validation
                n_vars <- length(self$options$vars)
                n_obs <- nrow(self$data)
                info_html <- sprintf(
                    '<div style="background-color: #d4edda; border-left: 4px solid #28a745; padding: 10px; margin: 5px 0;">
                    <strong>Ready:</strong> Alluvial diagram will be generated using %d variables and %d observations.
                    </div>',
                    n_vars, n_obs
                )
                self$results$noticeInfo$setContent(info_html)
                self$results$noticeInfo$setVisible(TRUE)
            }

        }

        ,

        .plot = function(image, ggtheme, theme, ...) {
            # Main alluvial plot generation function

            # Input validation using shared helper
            if (!private$.validateAlluvialInputs())
                return()

            # Wrap plot generation in error handler
            tryCatch({
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

                # Warn user if more variables selected than maxvars allows
                num_selected <- length(varsName)
                if (num_selected > maxvars) {
                    # Use pre-defined HTML output instead of dynamic Notice
                    info_html <- sprintf(
                        '<div style="background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin: 5px 0;">
                        <strong>Note:</strong> Variables truncated: %d selected but maximum is %d. Only first %d variables displayed.
                        </div>',
                        num_selected, maxvars, maxvars
                    )

                    # Update the info notice
                    existing <- self$results$noticeInfo$content
                    if (!is.null(existing) && nchar(existing) > 0) {
                        self$results$noticeInfo$setContent(paste0(existing, info_html))
                    } else {
                        self$results$noticeInfo$setContent(info_html)
                    }
                    self$results$noticeInfo$setVisible(TRUE)

                    # Keep detailed warning in dataWarning
                    warning_html <- paste0(
                        "<div style='background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #0c5460;'>Variables Truncated</h4>",
                        "<p style='color: #0c5460;'>You selected <strong>", num_selected, "</strong> variables, but maximum is set to <strong>", maxvars, "</strong>.</p>",
                        "<p>Only the first <strong>", maxvars, "</strong> variables will be displayed in the plot.</p>",
                        "<p><strong>To show more variables:</strong> Increase the 'Maximum variables' setting (up to 20).</p>",
                        "</div>"
                    )
                    # Append to existing warning if any
                    existing_warning <- self$results$dataWarning$content
                    if (!is.null(existing_warning) && nchar(existing_warning) > 0) {
                        self$results$dataWarning$setContent(paste0(existing_warning, warning_html))
                    } else {
                        self$results$dataWarning$setContent(warning_html)
                    }
                }

                # Get engine selection and weight variable
                engine <- self$options$engine
                weight_var <- self$options$weight

                # Generate plot based on selected engine ----
                if (engine == "ggalluvial") {
                    # Use ggalluvial engine for manual control
                    fill_var <- if (!is.null(self$options$fillGgalluvial)) {
                        self$options$fillGgalluvial
                    } else {
                        varsName[1]
                    }

                    plot <- private$.createGgalluvialPlot(
                        data = mydata,
                        vars = varsName,
                        fill_var = fill_var,
                        weight_var = weight_var
                    )
                } else {
                    # Use easyalluvial engine (default)
                    plot <- easyalluvial::alluvial_wide(
                        data = mydata,
                        max_variables = maxvars,
                        fill_by = fill,
                        verbose = FALSE,  # Disabled to prevent console clutter in jamovi
                        bin_labels = bin
                    )
                }

                # Add marginal histograms if requested (easyalluvial only) ----
                marg <- self$options$marg
                if (marg && engine == "easyalluvial") {
                    plot <- easyalluvial::add_marginal_histograms(
                        p = plot,
                        data_input = mydata,
                        keep_labels = TRUE,
                        top = TRUE,
                        plot = TRUE
                    )
                }

                # Apply color palette ----
                colorPalette <- self$options$colorPalette
                plot <- private$.applyColorPalette(plot, colorPalette)

                # Apply enhanced gradients if requested ----
                if (self$options$enhancedGradients && colorPalette == "default") {
                    plot <- plot +
                        ggplot2::scale_fill_viridis_d(option = "plasma", alpha = 0.8)
                }

                # Apply theme style ----
                themeStyle <- self$options$themeStyle
                plot <- private$.applyThemeStyle(plot, themeStyle)

                # Configure plot orientation / flow direction ----
                orient <- self$options$orient
                flowDirection <- self$options$flowDirection

                # Handle orientation (legacy) and flow direction (new)
                if (orient != "vert") {
                    plot <- plot + ggplot2::coord_flip()
                } else if (flowDirection == "top_bottom") {
                    plot <- plot + ggplot2::coord_flip()
                } else if (flowDirection == "right_left") {
                    plot <- plot + ggplot2::scale_x_reverse()
                } else if (flowDirection == "bottom_top") {
                    plot <- plot + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
                }

                # Apply custom title and subtitle ----
                usetitle <- self$options$usetitle
                plotSubtitle <- self$options$plotSubtitle

                if (marg && usetitle) {
                    warning_html <- paste0(
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>Custom Titles Not Supported with Marginal Plots</h4>",
                        "<p style='color: #856404;'>Custom titles cannot be used when marginal plots are enabled. The plot will be generated with the default title.</p>",
                        "<p><strong>Suggestion:</strong> To use a custom title, please disable the 'Marginal plots' option.</p>",
                        "</div>"
                    )
                    self$results$dataWarning$setContent(warning_html)
                }

                if (!marg && usetitle) {
                    mytitle <- self$options$mytitle
                    if (!is.null(plotSubtitle) && plotSubtitle != "") {
                        plot <- plot + ggplot2::labs(title = mytitle, subtitle = plotSubtitle)
                    } else {
                        plot <- plot + ggplot2::ggtitle(mytitle)
                    }
                } else if (!is.null(plotSubtitle) && plotSubtitle != "") {
                    plot <- plot + ggplot2::labs(subtitle = plotSubtitle)
                }

                # Render the final plot
                print(plot)
                TRUE

            }, error = function(e) {
                # Handle plot generation errors gracefully
                error_html <- sprintf(
                    '<div style="background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;">
                    <h4 style="margin-top: 0; color: #721c24;">Plot Generation Failed</h4>
                    <p style="color: #721c24;">%s</p>
                    <p><strong>Suggestions:</strong></p>
                    <ul>
                        <li>Try using the easyalluvial engine instead of ggalluvial</li>
                        <li>Reduce the number of variables</li>
                        <li>Check for variables with too many unique categories</li>
                        <li>Ensure all selected variables exist in the data</li>
                    </ul>
                    </div>',
                    htmltools::htmlEscape(e$message)
                )
                self$results$noticeError$setContent(error_html)
                self$results$noticeError$setVisible(TRUE)
            })
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
                    "<h4 style='margin-top: 0; color: #721c24;'>No Data Available</h4>",
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
                    condvarName_safe <- htmltools::htmlEscape(condvarName)
                    html <- paste0(
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>Continuous Condensation Variable</h4>",
                        "<p style='color: #856404;'>Condensation variable '<strong>", condvarName_safe, "</strong>' has <strong>", unique_vals, "</strong> unique values.</p>",
                        "<p><strong>Security Warning:</strong> Continuous variables can expose patient-level data in condensation plots.</p>",
                        "<hr style='border-color: #ffc107;'>",
                        "<p><strong>Required Action:</strong> Condensation plots require categorical variables.</p>",
                        "<p style='margin: 10px 0;'><strong>Solutions:</strong></p>",
                        "<ol style='margin-left: 20px;'>",
                        "<li>Select a different <strong>categorical</strong> variable for condensation</li>",
                        "<li>Or bin '<strong>", condvarName_safe, "</strong>' first using Data -> Compute</li>",
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
