#' @title Auto-Plot using esquisse
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2

esquisseautoClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "esquisseautoClass",
        inherit = esquisseautoBase,
        private = list(
            .init = function() {
                # Populate instructions
                private$.populateInstructions()
            },

            .run = function() {
                # Populate plot info
                private$.populatePlotInfo()
            },

            .plot = function(image, ...) {
                # Check if esquisse is available
                if (!requireNamespace("esquisse", quietly = TRUE)) {
                    stop("The 'esquisse' package is required but not installed. Please install it with: install.packages('esquisse')")
                }

                # Check if x variable is selected
                if (is.null(self$options$xvar)) {
                    return(FALSE)
                }

                # Get data
                plotData <- self$data

                # Prepare aesthetic mapping
                xvar <- self$options$xvar
                yvar <- self$options$yvar
                colorvar <- self$options$colorvar
                fillvar <- self$options$fillvar
                sizevar <- self$options$sizevar
                groupvar <- self$options$groupvar

                # Get variable types for error messages
                xdata <- plotData[[xvar]]
                xdata_type <- if (is.numeric(xdata)) "continuous" else if (is.factor(xdata)) "discrete" else "unknown"

                ydata_type <- "none"
                if (!is.null(yvar)) {
                    ydata <- plotData[[yvar]]
                    ydata_type <- if (is.numeric(ydata)) "continuous" else if (is.factor(ydata)) "discrete" else "unknown"
                }

                # Build aesthetics list
                aes_list <- list(x = xvar)
                if (!is.null(yvar)) {
                    aes_list$y <- yvar
                }
                if (!is.null(colorvar) && colorvar != "") {
                    aes_list$color <- colorvar
                }
                if (!is.null(fillvar) && fillvar != "") {
                    aes_list$fill <- fillvar
                }
                if (!is.null(sizevar) && sizevar != "") {
                    aes_list$size <- sizevar
                }
                if (!is.null(groupvar) && groupvar != "") {
                    aes_list$group <- groupvar
                }

                # Convert to aes() call
                aes_mapping <- do.call(ggplot2::aes_string, aes_list)

                # Determine geom to use
                if (isTRUE(self$options$autoGeom)) {
                    # Auto-select geom using esquisse
                    potential_geoms <- esquisse::potential_geoms(
                        data = plotData,
                        mapping = aes_mapping,
                        auto = TRUE
                    )
                    selected_geom <- potential_geoms[1]
                } else {
                    # Use manual selection
                    selected_geom <- self$options$manualGeom
                    if (selected_geom == "auto") {
                        # Fall back to auto if manual is set to auto
                        potential_geoms <- esquisse::potential_geoms(
                            data = plotData,
                            mapping = aes_mapping,
                            auto = TRUE
                        )
                        selected_geom <- potential_geoms[1]
                    }
                }

                # Prepare geom arguments
                geom_args <- list()
                if (selected_geom %in% c("point", "jitter")) {
                    geom_args$alpha <- 0.7
                }

                # Prepare labels
                labs_list <- list()
                if (!is.null(self$options$plotTitle) && self$options$plotTitle != "") {
                    labs_list$title <- self$options$plotTitle
                }
                if (!is.null(self$options$plotSubtitle) && self$options$plotSubtitle != "") {
                    labs_list$subtitle <- self$options$plotSubtitle
                }
                if (!is.null(self$options$plotCaption) && self$options$plotCaption != "") {
                    labs_list$caption <- self$options$plotCaption
                }
                if (!is.null(self$options$xLabel) && self$options$xLabel != "") {
                    labs_list$x <- self$options$xLabel
                }
                if (!is.null(self$options$yLabel) && self$options$yLabel != "") {
                    labs_list$y <- self$options$yLabel
                }

                # Generate plot using ggcall
                plot_call <- esquisse::ggcall(
                    data = "plotData",
                    mapping = aes_list,
                    geom = selected_geom,
                    geom_args = geom_args,
                    labs = labs_list,
                    theme = paste0("theme_", self$options$plotTheme)
                )

                # Evaluate the call to create plot with error handling
                p <- tryCatch({
                    eval(plot_call)
                }, error = function(e) {
                    # Provide user-friendly error messages
                    error_msg <- conditionMessage(e)

                    # Detect common error patterns and provide helpful guidance
                    if (grepl("must only have an x or y aesthetic", error_msg, ignore.case = TRUE)) {
                        stop(glue::glue(
                            "❌ Bar chart error: You've selected both X and Y variables.\n\n",
                            "📊 For bar charts with '{selected_geom}' geom:\n",
                            "  • Use ONLY X variable (for counts/frequencies)\n",
                            "  • OR use X + Y if Y is already aggregated\n\n",
                            "💡 Current selection:\n",
                            "  • X: {xvar}\n",
                            "  • Y: {if (!is.null(yvar)) yvar else '(none)'}\n\n",
                            "🔧 Solutions:\n",
                            "  1. Remove Y variable for automatic counting\n",
                            "  2. Switch to 'Column' geom if Y contains values to plot\n",
                            "  3. Enable manual geom selection and choose appropriate plot type"
                        ))
                    } else if (grepl("Discrete value supplied to continuous scale", error_msg, ignore.case = TRUE)) {
                        stop(glue::glue(
                            "❌ Scale mismatch: A categorical variable was used where continuous is expected.\n\n",
                            "💡 Suggestions:\n",
                            "  • Check if X/Y variables are the correct type\n",
                            "  • Size aesthetic requires continuous variables\n",
                            "  • Try a different plot type for categorical data"
                        ))
                    } else if (grepl("Continuous value supplied to discrete scale", error_msg, ignore.case = TRUE)) {
                        stop(glue::glue(
                            "❌ Scale mismatch: A continuous variable was used where categorical is expected.\n\n",
                            "💡 Suggestions:\n",
                            "  • Convert continuous variable to factor for grouping\n",
                            "  • Try scatter plot for continuous-continuous relationships"
                        ))
                    } else if (grepl("not found|object.*not found", error_msg, ignore.case = TRUE)) {
                        stop(glue::glue(
                            "❌ Variable not found in data.\n\n",
                            "🔍 Please check:\n",
                            "  • All selected variables exist in your dataset\n",
                            "  • Variable names are spelled correctly\n",
                            "  • No variables have been removed from data"
                        ))
                    } else if (grepl("stat.*aesthetic", error_msg, ignore.case = TRUE)) {
                        stop(glue::glue(
                            "❌ Incompatible variable combination for '{selected_geom}' plot.\n\n",
                            "📊 Current setup:\n",
                            "  • X: {xvar} ({xdata_type})\n",
                            "  • Y: {if (!is.null(yvar)) paste0(yvar, ' (', ydata_type, ')') else '(none)'}\n\n",
                            "💡 Suggestions:\n",
                            "  1. Try a different plot type (disable Auto-Select)\n",
                            "  2. Check variable types match plot requirements\n",
                            "  3. Remove incompatible aesthetic mappings (color, fill, size)\n\n",
                            "Original error: {error_msg}"
                        ))
                    } else {
                        # Generic error with context
                        stop(glue::glue(
                            "❌ Plot generation failed.\n\n",
                            "📊 Your configuration:\n",
                            "  • X: {xvar}\n",
                            "  • Y: {if (!is.null(yvar)) yvar else '(none)'}\n",
                            "  • Plot Type: {selected_geom}\n",
                            "  • Color: {if (!is.null(colorvar) && colorvar != '') colorvar else '(none)'}\n",
                            "  • Fill: {if (!is.null(fillvar) && fillvar != '') fillvar else '(none)'}\n\n",
                            "💡 Try:\n",
                            "  1. Simplify: Remove optional aesthetics (color, fill, size)\n",
                            "  2. Different geom: Disable Auto-Select and choose manually\n",
                            "  3. Check data: Ensure variables have valid values\n\n",
                            "🔧 Technical details:\n{error_msg}"
                        ))
                    }
                })

                # Apply legend customization
                if (!is.null(self$options$legendTitle) && self$options$legendTitle != "") {
                    p <- p + ggplot2::labs(color = self$options$legendTitle,
                                          fill = self$options$legendTitle,
                                          size = self$options$legendTitle)
                }

                if (self$options$legendPosition == "none") {
                    p <- p + ggplot2::theme(legend.position = "none")
                } else if (self$options$legendPosition != "right") {
                    p <- p + ggplot2::theme(legend.position = self$options$legendPosition)
                }

                # Apply axis label rotation
                if (self$options$xAxisAngle > 0) {
                    p <- p + ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = self$options$xAxisAngle, hjust = 1)
                    )
                }
                if (self$options$yAxisAngle > 0) {
                    p <- p + ggplot2::theme(
                        axis.text.y = ggplot2::element_text(angle = self$options$yAxisAngle, hjust = 1)
                    )
                }

                # Add smoother if requested
                if (isTRUE(self$options$addSmoother) && !is.null(yvar)) {
                    p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.2)
                }

                # Flip axes if requested
                if (isTRUE(self$options$flipAxes)) {
                    p <- p + ggplot2::coord_flip()
                }

                # Add faceting if specified
                facetvar <- self$options$facetvar
                if (!is.null(facetvar) && facetvar != "") {
                    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facetvar)))
                }

                # Store generated code if requested
                if (isTRUE(self$options$showCode)) {
                    code_text <- deparse(plot_call)
                    self$results$generatedCode$setContent(paste(code_text, collapse = "\n"))
                }

                print(p)
                TRUE
            },

            .populateInstructions = function() {
                html <- "
                <div style='padding: 15px; background: #f8f9fa; border-radius: 8px;'>
                    <h3 style='margin-top: 0;'>How to Use Auto-Plot</h3>

                    <h4>1. Select Variables:</h4>
                    <ul>
                        <li><strong>X Variable (Required):</strong> Main variable for horizontal axis</li>
                        <li><strong>Y Variable (Optional):</strong> For bivariate plots (leave empty for histograms/bar charts)</li>
                        <li><strong>Color Variable (Optional):</strong> Outline color for points/bars/lines</li>
                        <li><strong>Fill Variable (Optional):</strong> Fill color for bars/areas (different from outline)</li>
                        <li><strong>Size Variable (Optional):</strong> Point/line size (typically continuous)</li>
                        <li><strong>Group Variable (Optional):</strong> Group lines/paths (for connecting points)</li>
                        <li><strong>Facet Variable (Optional):</strong> Create small multiple plots by groups</li>
                    </ul>

                    <h4>2. Plot Type Selection:</h4>
                    <ul>
                        <li><strong>Auto-Select (Recommended):</strong> esquisse automatically chooses the best plot type</li>
                        <li><strong>Manual Selection:</strong> Uncheck Auto-Select to choose your preferred plot type</li>
                    </ul>

                    <h4>3. Automatic Plot Type Selection:</h4>
                    <table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>
                        <tr style='background: #e9ecef;'>
                            <th style='padding: 8px; text-align: left; border: 1px solid #dee2e6;'>X Variable</th>
                            <th style='padding: 8px; text-align: left; border: 1px solid #dee2e6;'>Y Variable</th>
                            <th style='padding: 8px; text-align: left; border: 1px solid #dee2e6;'>Auto Plot Type</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Continuous</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>(Empty)</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Histogram</td>
                        </tr>
                        <tr style='background: #f8f9fa;'>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Categorical</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>(Empty)</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Bar Chart</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Continuous</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Continuous</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Scatter Plot</td>
                        </tr>
                        <tr style='background: #f8f9fa;'>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Categorical</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Continuous</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Column Chart</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Continuous</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Categorical</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Bar Chart (Grouped)</td>
                        </tr>
                        <tr style='background: #f8f9fa;'>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Categorical</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Categorical</td>
                            <td style='padding: 8px; border: 1px solid #dee2e6;'>Tile/Heatmap</td>
                        </tr>
                    </table>

                    <h4 style='margin-top: 15px;'>Quick Tips:</h4>
                    <ul>
                        <li>Use <strong>Color</strong> for point/line outlines, <strong>Fill</strong> for bar/area interiors</li>
                        <li>Use <strong>Size</strong> with continuous variables to show magnitude (bubble charts)</li>
                        <li>Use <strong>Group</strong> to connect points in line plots (time series by category)</li>
                        <li>Enable <strong>Show Generated Code</strong> to see the R code behind your plot</li>
                        <li>Try different <strong>Themes</strong> to find the style that works best</li>
                    </ul>
                </div>
                "
                self$results$instructions$setContent(html)
            },

            .populatePlotInfo = function() {
                if (is.null(self$options$xvar)) {
                    html <- "<p><em>Select an X variable to begin.</em></p>"
                    self$results$plotinfo$setContent(html)
                    return()
                }

                # Get variable info
                xvar <- self$options$xvar
                yvar <- self$options$yvar
                xdata <- self$data[[xvar]]

                # Classify x variable type
                xtype <- if (is.numeric(xdata)) {
                    "continuous"
                } else if (is.factor(xdata)) {
                    "discrete"
                } else {
                    "unknown"
                }

                # Classify y variable type if present
                ytype <- if (!is.null(yvar)) {
                    ydata <- self$data[[yvar]]
                    if (is.numeric(ydata)) {
                        "continuous"
                    } else if (is.factor(ydata)) {
                        "discrete"
                    } else {
                        "unknown"
                    }
                } else {
                    "empty"
                }

                # Determine selected geom
                if (isTRUE(self$options$autoGeom)) {
                    # Get auto-selected geom
                    aes_list <- list(x = xvar)
                    if (!is.null(yvar)) {
                        aes_list$y <- yvar
                    }
                    aes_mapping <- do.call(ggplot2::aes_string, aes_list)

                    selected_geom <- tryCatch({
                        esquisse::potential_geoms(
                            data = self$data,
                            mapping = aes_mapping,
                            auto = TRUE
                        )[1]
                    }, error = function(e) {
                        "auto-detection failed"
                    })

                    geom_source <- "Auto-selected"
                } else {
                    selected_geom <- self$options$manualGeom
                    geom_source <- "Manually selected"
                }

                # Build aesthetic info list
                aes_info <- list()
                aes_info <- c(aes_info, paste0("X: <strong>", xvar, "</strong> (", xtype, ")"))
                if (!is.null(yvar)) {
                    aes_info <- c(aes_info, paste0("Y: <strong>", yvar, "</strong> (", ytype, ")"))
                }

                colorvar <- self$options$colorvar
                fillvar <- self$options$fillvar
                sizevar <- self$options$sizevar
                groupvar <- self$options$groupvar
                facetvar <- self$options$facetvar

                if (!is.null(colorvar) && colorvar != "") {
                    aes_info <- c(aes_info, paste0("Color: <strong>", colorvar, "</strong>"))
                }
                if (!is.null(fillvar) && fillvar != "") {
                    aes_info <- c(aes_info, paste0("Fill: <strong>", fillvar, "</strong>"))
                }
                if (!is.null(sizevar) && sizevar != "") {
                    aes_info <- c(aes_info, paste0("Size: <strong>", sizevar, "</strong>"))
                }
                if (!is.null(groupvar) && groupvar != "") {
                    aes_info <- c(aes_info, paste0("Group: <strong>", groupvar, "</strong>"))
                }
                if (!is.null(facetvar) && facetvar != "") {
                    aes_info <- c(aes_info, paste0("Facet: <strong>", facetvar, "</strong>"))
                }

                aes_list_html <- paste(paste0("<li>", aes_info, "</li>"), collapse = "\n")

                html <- glue::glue("
                <div style='padding: 10px; background: #e7f3ff; border-left: 4px solid #2196F3; border-radius: 4px;'>
                    <p style='margin: 0;'><strong>Plot Configuration:</strong></p>
                    <ul style='margin-bottom: 5px;'>
                        {aes_list_html}
                        <li>Plot Type: <strong>{selected_geom}</strong> ({geom_source})</li>
                    </ul>
                </div>
                ")

                self$results$plotinfo$setContent(html)
            }
        )
    )
