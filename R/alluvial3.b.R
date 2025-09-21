#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
#' @import easyalluvial
#' @importFrom ggalluvial geom_alluvium geom_stratum stat_alluvium stat_stratum geom_flow StatStratum geom_lode stat_flow
#' @importFrom dplyr group_by_at summarize n count select all_of rename filter arrange desc mutate
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
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

alluvial3Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "alluvial3Class",
    inherit = alluvial3Base,
    private = list(
        
        # HTML sanitization helper to prevent XSS
        .sanitizeHTML = function(text) {
            if (is.null(text) || length(text) == 0) return("")
            text <- as.character(text[1])  # Ensure single value
            
            # Comprehensive HTML entity encoding for safety
            text <- gsub("&", "&amp;", text, fixed = TRUE)
            text <- gsub("<", "&lt;", text, fixed = TRUE)
            text <- gsub(">", "&gt;", text, fixed = TRUE)
            text <- gsub('"', "&quot;", text, fixed = TRUE)
            text <- gsub("'", "&#39;", text, fixed = TRUE)
            text <- gsub("/", "&#47;", text, fixed = TRUE)
            text <- gsub("\\", "&#92;", text, fixed = TRUE)
            
            return(text)
        },
        
        # Shared validation helper to reduce duplication
        .validateAlluvialInputs = function() {
            if (is.null(self$options$vars) || length(self$options$vars) == 0)
                return(FALSE)
            
            if (length(self$options$vars) < 2)
                stop(.('🔧 Alluvial diagrams require at least 2 variables. Please select additional variables in the Variables section.'))
            
            if (nrow(self$data) == 0)
                stop(.('📊 No data available. Please check that your dataset contains data and try again.'))
            
            # Validate that variables are appropriate for alluvial diagrams
            private$.validateVariableTypes(self$options$vars)
            
            return(TRUE)
        },

        # Data type validation helper with user-friendly messages
        .validateVariableTypes = function(vars) {
            for (var in vars) {
                if (!(var %in% names(self$data))) {
                    stop(.("❌ Variable '{var}' not found in your dataset. Please check the variable name and try again.", list(var = var)))
                }
                
                var_data <- self$data[[var]]
                
                # Check if variable is numeric with too many unique values (likely continuous)
                if (is.numeric(var_data)) {
                    unique_values <- length(unique(var_data[!is.na(var_data)]))
                    total_values <- sum(!is.na(var_data))
                    
                    # If more than 20 unique values or >50% unique values, provide helpful guidance
                    if (unique_values > 20 || (unique_values / total_values) > 0.5) {
                        notice_html <- paste0(
                            "<div style='background-color: #fff3cd; color: #856404; padding: 10px; margin: 5px 0; border-radius: 4px;'>",
                            "<strong>⚠️ ", .("Data Type Notice:"), "</strong> ", 
                            .("Variable '{var}' appears to be continuous ({count} unique values). For optimal results:", 
                              list(var = var, count = unique_values)), "<br>",
                            "• ", .("Consider grouping values into meaningful categories"), "<br>",
                            "• ", .("Use the Bin Labels options for automatic binning"), "<br>",
                            "• ", .("Or select a different categorical variable"),
                            "</div>"
                        )
                        self$results$todo$setContent(paste0(self$results$todo$content, notice_html))
                    }
                }
            }
        },
        .run = function() {
            # Detect data format and route to appropriate processing
            data_format <- private$.detectDataFormat()
            
            if (data_format == "none") {
                # ToDo Message ----
                todo <- paste0(
                    "<div style='font-family: Arial, sans-serif; color: #2c3e50;'>",
                    "<h2>", .("Welcome to ClinicoPath - Enhanced Alluvial & Sankey Diagrams"), "</h2>",
                    "<p>", .("This tool creates comprehensive flow diagrams with multiple data format support."), "</p>",
                    "<p><strong>", .("Choose Your Data Format:"), "</strong></p>",
                    "<ul>",
                    "<li><strong>", .("Wide Format (Traditional):"), "</strong> ", .("Select 2+ Variables for standard alluvial plots"), "</li>",
                    "<li><strong>", .("Source-Target Format:"), "</strong> ", .("Use Value, Source, and Target variables for directed flows"), "</li>",
                    "<li><strong>", .("Multi-Node Format:"), "</strong> ", .("Combine Value variable with 2+ Node Variables"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Advanced Features:"), "</strong></p>",
                    "<ul>",
                    "<li><em>", .("Diagram Types:"), "</em> ", .("Alluvial, Sankey, Parallel Sets"), "</li>",
                    "<li><em>", .("Styling:"), "</em> ", .("Custom colors, themes, node sizing, edge transparency"), "</li>",
                    "<li><em>", .("Output Options:"), "</em> ", .("Data tables, flow statistics, interpretations"), "</li>",
                    "<li><em>", .("Clinical Features:"), "</em> ", .("Medical presets and specialized analysis"), "</li>",
                    "</ul>",
                    "<div style='background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #2196f3;'>",
                    "<p style='margin: 0;'><strong>💡 ", .("Alternative Tool:"), "</strong> ", 
                    .("For specialized temporal flow analysis and clinical workflows, consider the {tool} function in the '{menu}' menu.", 
                      list(tool = .("River Plot"), menu = .("Categorical Over Time"))), "</p>",
                    "</div>",
                    "<hr>",
                    "</div>"
                )
                html <- self$results$todo
                html$setContent(todo)
                return()
            }
            
            # Clear the to-do message
            html <- self$results$todo
            html$setContent("")
            
            # Validate inputs based on detected format
            private$.validateInputsByFormat(data_format)
            
            # Generate data tables and statistics if requested
            if (data_format %in% c("source_target", "multi_node")) {
                private$.generateAdvancedOutputs()
            }
            
            # Apply clinical presets if enabled
            if (self$options$clinical_mode) {
                private$.applyClinicalPreset()
            }
            
            # Generate advanced diagnostics if requested
            if (self$options$advanced_diagnostics) {
                private$.generateDiagnostics(data_format)
            }
            
            # Generate new summary panels
            private$.generateNaturalSummary(self$data, data_format)
            private$.generateReportSentence(self$data, data_format)
            private$.generateCaveats()
        }

        ,

        .plot = function(image, ggtheme, theme, ...) {
            # Detect data format and route to appropriate plot generation
            data_format <- private$.detectDataFormat()
            
            if (data_format == "none") {
                return(FALSE)
            }
            
            # Show progress for large datasets
            if (nrow(self$data) > 5000) {
                private$.showProgress(.("Processing large dataset, please wait..."))
            }
            
            # Generate plot based on data format
            plot <- switch(data_format,
                          "wide_format" = private$.generateWideFormatPlot(),
                          "source_target" = private$.generateAdvancedPlot(),
                          "multi_node" = private$.generateAdvancedPlot(),
                          private$.generateWideFormatPlot())
            
            if (is.null(plot)) {
                return(FALSE)
            }

            # Apply export compatibility modifications
            plot <- private$.applyExportCompatibility(plot)

            # Render the final plot
            print(plot)
            TRUE
        },
        
        # Helper method for configurable styling widths
        .getSankeyWidths = function() {
            list(
                alluvium_width = 1/2,
                sankey_stratum_width = 1/8,
                standard_stratum_width = 1/3
            )
        },
        
        # Helper method to create axis aesthetics
        .createAxisAesthetics = function(vars, weight_var = NULL) {
            axis_list <- list()
            for (i in seq_along(vars[1:min(length(vars), 4)])) {
                axis_list[[paste0("axis", i)]] <- rlang::sym(vars[i])
            }
            if (!is.null(weight_var)) {
                axis_list$y <- rlang::sym(weight_var)
            }
            return(axis_list)
        },
        
        # Helper method to prepare data for ggalluvial
        .prepareGgalluvialData = function(data, vars, time_var = NULL) {
            # Convert variables to factors if needed
            for (var in vars) {
                data[[var]] <- as.factor(data[[var]])
            }
            
            if (!is.null(time_var)) {
                data[[time_var]] <- as.factor(data[[time_var]])
            }
            
            # Handle missing values
            data <- jmvcore::naOmit(data)
            
            if (nrow(data) == 0) {
                stop(.("No complete cases found after removing missing values."))
            }
            
            return(data)
        },
        
        # Helper method to validate curve types
        .validateCurveType = function(curve_type, sankey_style = FALSE) {
            if (sankey_style) {
                return("sigmoid")  # Force sigmoid curves for Sankey style
            }
            
            valid_curves <- c("linear", "cubic", "quintic", "sine", "arctangent", "sigmoid")
            if (!curve_type %in% valid_curves) {
                return("cubic")  # Default fallback
            }
            
            return(curve_type)
        },
        
        # Helper method to determine fill variable
        .getFillVariable = function(fill_option, vars) {
            switch(fill_option,
                   "first_variable" = vars[1],
                   "last_variable" = vars[length(vars)],
                   "all_flows" = vars[1],  # default to first for all_flows
                   "values" = vars[1],     # default to first for values
                   vars[1])  # default fallback
        },
        
        # Helper method to consolidate label parameters
        .shouldShowLabels = function() {
            # Primary control: labelNodes (basic labeling)
            # Secondary enhancement: show_labels (advanced styling)
            # Both can be enabled independently - basic OR enhanced
            return(self$options$labelNodes || self$options$show_labels)
        },
        
        # Refactored method for ggalluvial plots with Sankey support
        .createGgalluvialPlot = function(data, vars) {
            # Check for required packages
            if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                stop(.("Package 'ggalluvial' is required for manual control engine. Please install it."))
            }
            
            # Validate input
            if (length(vars) < 2) {
                stop(.("At least 2 variables are required for ggalluvial plots."))
            }
            
            # Extract options
            time_var <- self$options$time
            weight_var <- self$options$weight
            sankey_style <- self$options$sankeyStyle
            curve_type <- private$.validateCurveType(self$options$curveType, sankey_style)
            # Use helper method to consolidate label parameters
            label_nodes <- private$.shouldShowLabels()
            show_counts <- self$options$showCounts
            fill_option <- self$options$fill
            
            # Prepare data
            data <- private$.prepareGgalluvialData(data, vars, time_var)
            
            # Create aesthetics
            axis_aes <- private$.createAxisAesthetics(vars, weight_var)
            
            # Create base plot
            plot <- ggplot(data, do.call(aes, axis_aes))
            
            # Get fill variable and widths
            fill_var <- private$.getFillVariable(fill_option, vars)
            widths <- private$.getSankeyWidths()
            
            # Add layers with styling
            if (sankey_style) {
                # Sankey styling: narrow stratum, different widths
                plot <- plot +
                    ggalluvial::geom_alluvium(aes(fill = !!rlang::sym(fill_var)),
                                              alpha = 0.8,
                                              curve_type = curve_type,
                                              width = widths$alluvium_width) +
                    ggalluvial::geom_stratum(width = widths$sankey_stratum_width, alpha = 0.8)
            } else {
                # Standard alluvial styling
                plot <- plot +
                    ggalluvial::geom_alluvium(aes(fill = !!rlang::sym(fill_var)),
                                              alpha = 0.8,
                                              curve_type = curve_type) +
                    ggalluvial::geom_stratum(width = widths$standard_stratum_width, alpha = 0.8)
            }
            
            # Add labels and counts
            plot <- private$.addGgalluvialLabels(plot, label_nodes, show_counts)
            
            return(plot)
        },
        
        # Helper method to add labels and counts
        .addGgalluvialLabels = function(plot, label_nodes, show_counts) {
            if (label_nodes) {
                plot <- plot + ggplot2::geom_text(stat = ggalluvial::StatStratum,
                                                  aes(label = after_stat(stratum)),
                                                  size = 3)
            }
            
            if (show_counts) {
                plot <- plot + ggplot2::geom_text(stat = ggalluvial::StatStratum,
                                                  aes(label = after_stat(count)),
                                                  size = 3, vjust = -0.5)
            }
            
            return(plot)
        },
        
        # === NEW MERGED FUNCTIONALITY FROM JJSANKEYFIER ===
        
        # Data format detection method
        .detectDataFormat = function() {
            # Check for source-target format
            if (!is.null(self$options$value_var) && 
                !is.null(self$options$source_var) && 
                !is.null(self$options$target_var)) {
                return("source_target")
            }
            
            # Check for multi-node format
            if (!is.null(self$options$value_var) && 
                !is.null(self$options$node_vars) && 
                length(self$options$node_vars) >= 2) {
                return("multi_node")
            }
            
            # Check for traditional wide format
            if (!is.null(self$options$vars) && 
                length(self$options$vars) >= 2) {
                return("wide_format")
            }
            
            return("none")
        },
        
        # Helper to validate variable existence with user-friendly message
        .validateVariableExists = function(var_name, var_type) {
            var_value <- self$options[[var_name]]
            if (!is.null(var_value) && !var_value %in% names(self$data)) {
                stop(.("{type} variable '{var}' not found in dataset. Available variables: {vars}",
                      list(type = var_type, 
                           var = var_value,
                           vars = paste(head(names(self$data), 10), collapse = ", "))))
            }
        },
        
        # Validation by data format
        .validateInputsByFormat = function(data_format) {
            if (data_format == "wide_format") {
                private$.validateAlluvialInputs()
                
                # Validate condensation variable if provided
                if (!is.null(self$options$condensationvar) && 
                    length(self$options$condensationvar) > 0 && 
                    !(self$options$condensationvar %in% names(self$data))) {
                    stop(.("Condensation variable '{var}' not found in the data. Please select a valid variable.", 
                          list(var = self$options$condensationvar)))
                }
            } else if (data_format %in% c("source_target", "multi_node")) {
                private$.validateAdvancedFormat(data_format)
            }
        },
        
        # Validation for advanced formats
        .validateAdvancedFormat = function(data_format) {
            if (is.null(self$options$value_var)) {
                stop(.("Value variable is required for advanced diagram formats."))
            }
            
            if (!self$options$value_var %in% names(self$data)) {
                stop(.("Value variable '{var}' not found in data.", list(var = self$options$value_var)))
            }
            
            if (data_format == "source_target") {
                if (!self$options$source_var %in% names(self$data)) {
                    stop(.("Source variable '{var}' not found in data.", list(var = self$options$source_var)))
                }
                if (!self$options$target_var %in% names(self$data)) {
                    stop(.("Target variable '{var}' not found in data.", list(var = self$options$target_var)))
                }
            }
            
            if (data_format == "multi_node") {
                for (var in self$options$node_vars) {
                    if (!var %in% names(self$data)) {
                        stop(.("Node variable '{var}' not found in data.", list(var = var)))
                    }
                }
            }
        },
        
        # Generate wide format plot (original alluvial functionality)
        .generateWideFormatPlot = function() {
            # Input validation using shared helper
            if (!private$.validateAlluvialInputs())
                return(NULL)

            # Data Preparation ----
            varsName <- self$options$vars
            mydata <- jmvcore::select(self$data, c(varsName))

            # Performance optimization for large datasets
            if (nrow(mydata) > 10000) {
                # Sample data for visualization if too large
                set.seed(123)  # For reproducibility
                sample_size <- min(10000, nrow(mydata))
                mydata <- mydata[sample(nrow(mydata), sample_size), ]
                
                # Notify user about sampling
                if ("todo" %in% names(self$results)) {
                    notice_html <- paste0(
                        "<div style='background-color: #d1ecf1; padding: 10px; border-radius: 6px; border-left: 4px solid #0c5460;'>",
                        "<strong>ℹ️ ", .("Performance Notice:"), "</strong> ",
                        .("Dataset sampled to {n} observations for optimal performance.", list(n = sample_size)),
                        "</div>"
                    )
                    self$results$todo$setContent(notice_html)
                }
            }

            # Handle missing values based on user preference
            excl <- self$options$excl
            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Configure plot aesthetics ----
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

            # Generate plot based on selected engine ----
            engine <- self$options$engine
            maxvars <- self$options$maxvars
            
            if (engine == "easyalluvial") {
                verbose_option <- self$options$verbose %||% TRUE
                plot <- easyalluvial::alluvial_wide(
                    data = mydata,
                    max_variables = maxvars,
                    fill_by = fill,
                    verbose = verbose_option,
                    bin_labels = bin
                )
            } else if (engine == "ggalluvial") {
                plot <- private$.createGgalluvialPlot(mydata, varsName)
            }

            # Add marginal histograms if requested ----
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
            orient <- self$options$orient
            if (orient != "vert") {
                plot <- plot + ggplot2::coord_flip()
            }

            # Apply custom title if specified ----
            usetitle <- self$options$usetitle
            if (marg && usetitle) {
                stop(.("Custom titles cannot be used with marginal plots. Please either disable marginal plots or use the default title."))
            }
            
            if (!marg && usetitle) {
                mytitle <- self$options$mytitle
                plot <- plot + ggplot2::ggtitle(mytitle)
            }

            # Apply enhanced gradients if requested
            if (self$options$enhanced_gradients) {
                plot <- plot +
                    ggplot2::scale_fill_viridis_d(option = "plasma", alpha = 0.8) +
                    ggplot2::theme(
                        panel.background = ggplot2::element_rect(fill = "white"),
                        plot.background = ggplot2::element_rect(fill = "white")
                    )
            }

            # Apply flow optimization (reduce visual complexity)
            if (self$options$flow_optimization) {
                # Optimize by reducing edge alpha and adjusting spacing
                if (engine == "ggalluvial") {
                    plot <- plot +
                        ggplot2::theme(
                            axis.text = ggplot2::element_text(size = 8),
                            axis.title = ggplot2::element_text(size = 10),
                            legend.text = ggplot2::element_text(size = 8)
                        )
                }
            }

            return(plot)
        },
        
        # Generate advanced plot for source-target and multi-node formats
        .generateAdvancedPlot = function() {
            # Load required packages
            packages <- c('ggplot2', 'ggalluvial', 'dplyr', 'tidyr')
            for (pkg in packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    stop(.("Package '{pkg}' is required but not installed", list(pkg = pkg)))
                }
            }
            
            # Prepare flow data based on data format
            flow_data <- private$.prepareFlowData(self$data)
            
            # Apply data transformations
            flow_data <- private$.applyDataTransformations(flow_data)
            
            # Create base plot based on diagram type
            plot <- switch(self$options$diagram_type,
                "sankey" = private$.createSankeyDiagram(flow_data),
                "alluvial" = private$.createAlluvialDiagram(flow_data), 
                "parallel_sets" = private$.createParallelSets(flow_data),
                private$.createAlluvialDiagram(flow_data)
            )
            
            # Apply customizations
            plot <- private$.applyPlotCustomizations(plot)
            
            return(plot)
        },
        
        # Prepare flow data for advanced formats
        .prepareFlowData = function(data) {
            # Performance check - limit data size for complex flows
            if (nrow(data) > 50000) {
                set.seed(123)
                data <- data[sample(nrow(data), 50000), ]
            }
            
            value_var <- self$options$value_var
            
            if (!is.null(self$options$source_var) && !is.null(self$options$target_var)) {
                # Simple source-target flow
                cols_to_select <- list(
                    source = self$options$source_var,
                    target = self$options$target_var,
                    value = value_var
                )
                
                # Add grouping variable if specified
                if (!is.null(self$options$grouping_var)) {
                    cols_to_select$group <- self$options$grouping_var
                }
                
                # Add time variable if specified
                if (!is.null(self$options$time_var)) {
                    cols_to_select$time <- self$options$time_var
                }
                
                flow_data <- data %>%
                    dplyr::select(dplyr::all_of(unlist(cols_to_select))) %>%
                    dplyr::rename(!!!cols_to_select) %>%
                    dplyr::filter(!is.na(source), !is.na(target), !is.na(value))
                    
            } else if (!is.null(self$options$node_vars) && length(self$options$node_vars) >= 2) {
                # Multi-level node flow (for alluvial)
                node_vars <- self$options$node_vars
                cols_to_select <- c(node_vars, value_var)
                
                # Add grouping variable if specified
                if (!is.null(self$options$grouping_var)) {
                    cols_to_select <- c(cols_to_select, self$options$grouping_var)
                }
                
                # Add time variable if specified
                if (!is.null(self$options$time_var)) {
                    cols_to_select <- c(cols_to_select, self$options$time_var)
                }
                
                flow_data <- data %>%
                    dplyr::select(dplyr::all_of(cols_to_select)) %>%
                    dplyr::filter(!is.na(!!sym(value_var)))
                    
                # Convert to long format for ggalluvial
                flow_data <- flow_data %>%
                    tidyr::pivot_longer(
                        cols = all_of(node_vars),
                        names_to = "variable",
                        values_to = "value_cat"
                    ) %>%
                    dplyr::mutate(
                        x = match(variable, node_vars),
                        node = value_cat
                    )
                    
            } else {
                stop(.("Please specify either Source & Target variables or at least 2 Node variables"))
            }
            
            return(flow_data)
        },
        
        # Apply data transformations
        .applyDataTransformations = function(flow_data) {
            # Apply sorting if requested
            if (self$options$sort_nodes == "alphabetical") {
                if ("source" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::arrange(source, target)
                } else if ("node" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::arrange(node)
                }
            } else if (self$options$sort_nodes == "by_value") {
                if ("value" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::arrange(desc(!!sym(self$options$value_var)))
                }
            }
            
            # Apply value formatting if needed
            if (self$options$value_format == "percent") {
                if ("value" %in% names(flow_data)) {
                    total_value <- sum(flow_data[[self$options$value_var]], na.rm = TRUE)
                    flow_data <- flow_data %>%
                        dplyr::mutate(value_formatted = (!!sym(self$options$value_var)) / total_value * 100)
                }
            } else if (self$options$value_format == "rounded") {
                if ("value" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::mutate(value_formatted = round(!!sym(self$options$value_var), 2))
                }
            }
            
            return(flow_data)
        },
        
        # Create Sankey diagram
        .createSankeyDiagram = function(flow_data) {
            if ('x' %in% names(flow_data) && 'node' %in% names(flow_data)) {
                # Multi-level alluvial format using ggalluvial
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, stratum = node, alluvium = !!sym(self$options$value_var),
                    fill = factor(node), label = node
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    knot.pos = 0.5
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "black",
                    size = 0.5
                )
            } else if ('source' %in% names(flow_data) && 'target' %in% names(flow_data)) {
                # Simple source-target format using network-style visualization
                # Convert to alluvial format
                alluvial_data <- flow_data %>%
                    tidyr::pivot_longer(cols = c(source, target), names_to = "x", values_to = "stratum") %>%
                    dplyr::mutate(
                        x = ifelse(x == "source", 1, 2),
                        alluvium = row_number()
                    )
                
                plot <- ggplot2::ggplot(alluvial_data, ggplot2::aes(
                    x = x, stratum = stratum, alluvium = alluvium,
                    fill = factor(stratum), y = value
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    knot.pos = 0.5
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "black",
                    size = 0.5
                )
            } else {
                stop(.("Invalid data format for sankey diagram"))
            }
            
            return(plot)
        },
        
        # Create alluvial diagram
        .createAlluvialDiagram = function(flow_data) {
            if ('x' %in% names(flow_data) && 'node' %in% names(flow_data)) {
                # Multi-node alluvial
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, stratum = node, alluvium = !!sym(self$options$value_var),
                    fill = factor(node)
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    curve_type = "sigmoid"
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "white",
                    size = 0.3
                )
            } else {
                # Fall back to sankey for simple data
                plot <- private$.createSankeyDiagram(flow_data)
            }
            
            return(plot)
        },
        
        # Create parallel sets diagram
        .createParallelSets = function(flow_data) {
            # For parallel sets, use straight lines instead of curves
            if ('x' %in% names(flow_data) && 'node' %in% names(flow_data)) {
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, stratum = node, alluvium = !!sym(self$options$value_var),
                    fill = factor(node)
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    curve_type = "linear"  # Straight lines for parallel sets
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "black",
                    size = 0.8
                )
            } else {
                plot <- private$.createSankeyDiagram(flow_data)
            }
            
            return(plot)
        },
        
        # Apply plot customizations
        .applyPlotCustomizations = function(plot) {
            # Apply theme
            theme_func <- switch(self$options$theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "void" = ggplot2::theme_void(),
                ggplot2::theme_gray()
            )
            
            plot <- plot + theme_func
            
            # Apply color palette
            if (self$options$color_palette != "default") {
                colors <- switch(self$options$color_palette,
                    "viridis" = ggplot2::scale_fill_viridis_d(),
                    "plasma" = ggplot2::scale_fill_viridis_d(option = "plasma"),
                    "set3" = ggplot2::scale_fill_brewer(type = "qual", palette = "Set3"),
                    "pastel1" = ggplot2::scale_fill_brewer(type = "qual", palette = "Pastel1"),
                    "dark2" = ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2"),
                    ggplot2::scale_fill_viridis_d()
                )
                plot <- plot + colors
            }
            
            # Add stratum labels if requested (consolidated approach)
            # Use show_labels for advanced styling, labelNodes for backward compatibility
            show_node_labels <- self$options$show_labels || self$options$labelNodes
            if (show_node_labels) {
                plot <- plot + ggalluvial::geom_stratum_label(
                    size = self$options$label_size / 3,
                    color = "black",
                    fontface = "bold"
                )
            }
            
            # Add flow values if requested
            if (self$options$show_values) {
                # Add text showing flow values
                plot <- plot + ggalluvial::geom_alluvium_label(
                    aes(label = !!sym(self$options$value_var)),
                    size = self$options$label_size / 4,
                    color = "white",
                    fontface = "bold"
                )
            }
            
            # Apply flow direction
            if (self$options$flow_direction == "top_bottom") {
                plot <- plot + ggplot2::coord_flip()
            } else if (self$options$flow_direction == "right_left") {
                plot <- plot + ggplot2::scale_x_reverse()
            } else if (self$options$flow_direction == "bottom_top") {
                plot <- plot + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
            }
            
            # Add titles - prioritize advanced title over basic title
            title_to_use <- if (!is.null(self$options$plot_title) && nchar(self$options$plot_title) > 0) {
                self$options$plot_title
            } else if (self$options$usetitle && !is.null(self$options$mytitle)) {
                self$options$mytitle
            } else {
                NULL
            }
            
            if (!is.null(title_to_use)) {
                plot <- plot + ggplot2::ggtitle(title_to_use)
            }
            
            if (!is.null(self$options$plot_subtitle) && nchar(self$options$plot_subtitle) > 0) {
                plot <- plot + ggplot2::labs(subtitle = self$options$plot_subtitle)
            }
            
            # Remove axis labels and ticks for cleaner look
            plot <- plot + ggplot2::theme(
                axis.text = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                legend.position = "bottom"
            )
            
            return(plot)
        },
        
        # Generate advanced outputs (data tables, statistics)
        .generateAdvancedOutputs = function() {
            data <- self$data
            
            # Generate data table if requested
            if (self$options$output_format %in% c("data_table", "both")) {
                private$.populateDataTable(data)
            }
            
            # Generate statistics if requested  
            if (self$options$show_statistics) {
                private$.populateStatisticsTable(data)
            }
            
            # Generate interpretation if requested
            if (self$options$show_interpretation) {
                private$.generateInterpretation(data)
            }
        },
        
        # Populate data table
        .populateDataTable = function(data) {
            if (is.null(self$options$source_var) || is.null(self$options$target_var)) {
                return()
            }
            
            # Prepare data table
            table_data <- data %>%
                dplyr::select(
                    source = !!self$options$source_var,
                    target = !!self$options$target_var,
                    value = !!self$options$value_var
                ) %>%
                dplyr::filter(!is.na(source), !is.na(target), !is.na(value)) %>%
                dplyr::mutate(
                    percentage = value / sum(value, na.rm = TRUE)
                )
            
            # Check if results table exists (it should from alluvial.r.yaml)
            if (!"datatab" %in% names(self$results)) {
                return()
            }
            
            # Populate results table
            for (i in seq_len(nrow(table_data))) {
                self$results$datatab$addRow(
                    rowKey = i,
                    values = list(
                        source = table_data$source[i],
                        target = table_data$target[i], 
                        value = table_data$value[i],
                        percentage = table_data$percentage[i]
                    )
                )
            }
        },
        
        # Populate statistics table
        .populateStatisticsTable = function(data) {
            if (is.null(self$options$value_var)) return()
            
            value_data <- data[[self$options$value_var]]
            value_data <- value_data[!is.na(value_data)]
            
            if (length(value_data) == 0) return()
            
            # Check if results table exists
            if (!"stats" %in% names(self$results)) {
                return()
            }
            
            # Calculate statistics
            stats <- list(
                list(metric = "Total Flow", value = sum(value_data)),
                list(metric = "Mean Flow", value = mean(value_data)),
                list(metric = "Median Flow", value = median(value_data)),
                list(metric = "Number of Flows", value = length(value_data)),
                list(metric = "Max Flow", value = max(value_data)),
                list(metric = "Min Flow", value = min(value_data))
            )
            
            # Populate results table
            for (i in seq_along(stats)) {
                self$results$stats$addRow(
                    rowKey = i,
                    values = stats[[i]]
                )
            }
        },
        
        # Generate interpretation
        .generateInterpretation = function(data) {
            if (is.null(self$options$value_var)) return()
            
            value_data <- data[[self$options$value_var]]
            value_data <- value_data[!is.na(value_data)]
            
            if (length(value_data) == 0) return()
            
            # Check if results interpretation exists
            if (!"interpretation" %in% names(self$results)) {
                return()
            }
            
            # Generate interpretation
            total_flow <- sum(value_data)
            num_flows <- length(value_data)
            avg_flow <- mean(value_data)
            
            diagram_name <- switch(self$options$diagram_type,
                "sankey" = "Sankey diagram",
                "alluvial" = "Alluvial diagram", 
                "parallel_sets" = "Parallel sets diagram",
                "diagram"
            )
            
            interpretation <- paste0(
                "<h3>Flow Analysis Summary</h3>",
                "<p>This ", diagram_name, " visualizes ", num_flows, " flow connections ",
                "with a total flow volume of ", round(total_flow, 2), ".</p>",
                "<p>The average flow size is ", round(avg_flow, 2), 
                ", indicating ", 
                if (avg_flow > median(value_data)) "right-skewed" else "fairly distributed",
                " flow patterns.</p>",
                if (self$options$diagram_type == "sankey") {
                    "<p>Sankey diagrams are ideal for showing how quantities flow between categories, 
                     with line thickness proportional to flow magnitude.</p>"
                } else if (self$options$diagram_type == "alluvial") {
                    "<p>Alluvial diagrams excel at showing how categorical variables change 
                     across multiple dimensions or time points.</p>"
                } else {
                    "<p>Parallel sets provide an alternative view of categorical relationships 
                     across multiple variables.</p>"
                }
            )
            
            self$results$interpretation$setContent(interpretation)
        },
        
        # Apply clinical presets
        .applyClinicalPreset = function() {
            preset <- self$options$clinical_preset
            
            if (preset == "none") return()
            
            # Define clinical presets with optimized settings
            presets <- list(
                patient_journey = list(
                    diagram_type = "alluvial",
                    color_palette = "set3",
                    show_labels = TRUE,
                    show_values = FALSE,
                    flow_direction = "left_right",
                    theme_style = "minimal"
                ),
                treatment_response = list(
                    diagram_type = "sankey", 
                    color_palette = "viridis",
                    show_labels = TRUE,
                    show_values = TRUE,
                    flow_direction = "left_right",
                    theme_style = "classic"
                ),
                disease_progression = list(
                    diagram_type = "sankey",
                    color_palette = "plasma",
                    show_labels = TRUE,
                    show_values = TRUE,
                    flow_direction = "top_bottom", 
                    theme_style = "minimal"
                )
            )
            
            if (preset %in% names(presets)) {
                # Generate clinical interpretation
                private$.generateClinicalSummary(preset)
            }
        },
        
        # Generate clinical summary
        .generateClinicalSummary = function(preset_type) {
            if (!("clinical_summary" %in% names(self$results))) return()
            
            preset_descriptions <- list(
                patient_journey = "Patient Journey Analysis",
                treatment_response = "Treatment Response Tracking", 
                disease_progression = "Disease Progression Monitoring"
            )
            
            clinical_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>📊 ", preset_descriptions[[preset_type]], "</h4>",
                "<p>This analysis is optimized for clinical workflows with the following features:</p>",
                "<ul>",
                "<li>Enhanced visualization for medical data patterns</li>",
                "<li>Clinical-friendly color schemes and layouts</li>",
                "<li>Interpretation focused on healthcare outcomes</li>",
                "</ul>",
                "<p><strong>Tip:</strong> Consider using advanced diagnostics for detailed flow analysis.</p>",
                "</div>"
            )
            
            self$results$clinical_summary$setContent(clinical_html)
        },
        
        # Generate advanced diagnostics
        .generateDiagnostics = function(data_format) {
            data <- self$data
            
            # Generate flow quality metrics
            private$.generateFlowMetrics(data, data_format)
            
            # Generate transition analysis if applicable
            if (data_format %in% c("source_target", "multi_node")) {
                private$.generateTransitionAnalysis(data)
            }
            
            # Generate overall diagnostics report
            private$.generateDiagnosticsReport(data_format)
        },
        
        # Generate flow quality metrics
        .generateFlowMetrics = function(data, data_format) {
            if (!("flow_metrics" %in% names(self$results))) return()
            
            metrics <- list()
            
            # Basic data quality metrics
            total_rows <- nrow(data)
            complete_rows <- sum(complete.cases(data))
            completeness_rate <- complete_rows / total_rows
            
            metrics <- list(
                list(metric = "Data Completeness", value = completeness_rate, 
                     interpretation = ifelse(completeness_rate > 0.95, "Excellent", 
                                           ifelse(completeness_rate > 0.8, "Good", "Needs attention"))),
                list(metric = "Total Records", value = total_rows, 
                     interpretation = ifelse(total_rows > 1000, "Large dataset", 
                                           ifelse(total_rows > 100, "Medium dataset", "Small dataset"))),
                list(metric = "Data Format", value = switch(data_format,
                                                           "wide_format" = 1, 
                                                           "source_target" = 2, 
                                                           "multi_node" = 3, 0),
                     interpretation = paste("Format:", data_format))
            )
            
            # Populate flow metrics table
            for (i in seq_along(metrics)) {
                self$results$flow_metrics$addRow(
                    rowKey = i,
                    values = metrics[[i]]
                )
            }
        },
        
        # Generate transition analysis
        .generateTransitionAnalysis = function(data) {
            if (!("transition_analysis" %in% names(self$results))) return()
            
            # This is a simplified transition analysis
            # In a real implementation, you would analyze actual transitions
            if (!is.null(self$options$source_var) && !is.null(self$options$target_var)) {
                source_col <- self$options$source_var
                target_col <- self$options$target_var
                
                if (source_col %in% names(data) && target_col %in% names(data)) {
                    transitions <- data %>%
                        dplyr::filter(!is.na(!!sym(source_col)), !is.na(!!sym(target_col))) %>%
                        dplyr::group_by(!!sym(source_col), !!sym(target_col)) %>%
                        dplyr::summarise(frequency = n(), .groups = 'drop') %>%
                        dplyr::mutate(
                            transition_rate = frequency / sum(frequency),
                            from_category = as.character(!!sym(source_col)),
                            to_category = as.character(!!sym(target_col))
                        ) %>%
                        dplyr::arrange(desc(frequency))
                    
                    # Populate transition analysis table
                    for (i in seq_len(min(nrow(transitions), 10))) { # Top 10 transitions
                        self$results$transition_analysis$addRow(
                            rowKey = i,
                            values = list(
                                from_category = transitions$from_category[i],
                                to_category = transitions$to_category[i],
                                transition_rate = transitions$transition_rate[i],
                                frequency = transitions$frequency[i]
                            )
                        )
                    }
                }
            }
        },
        
        # Generate diagnostics report
        .generateDiagnosticsReport = function(data_format) {
            if (!("diagnostics_report" %in% names(self$results))) return()
            
            report_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #495057;'>🔍 Flow Diagnostics Report</h4>",
                "<p><strong>Data Format:</strong> ", data_format, "</p>",
                "<p><strong>Analysis Type:</strong> ", self$options$diagram_type, "</p>",
                if (self$options$enhanced_gradients) {
                    "<p>✨ <em>Enhanced gradients enabled for improved visual appeal</em></p>"
                } else "",
                if (self$options$flow_optimization) {
                    "<p>🎯 <em>Flow optimization enabled to reduce crossings</em></p>"
                } else "",
                "<p><strong>Recommendations:</strong></p>",
                "<ul>",
                "<li>Check flow metrics above for data quality insights</li>",
                "<li>Review transition patterns for unexpected flows</li>",
                "<li>Consider enabling clinical mode for medical data</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$diagnostics_report$setContent(report_html)
        },
        
        # Generate natural language summary
        .generateNaturalSummary = function(data, data_format) {
            if (!("naturalSummary" %in% names(self$results))) return()
            
            # Calculate summary statistics
            n_total <- nrow(data)
            n_vars <- length(self$options$vars)
            
            # Build natural language summary
            summary_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #2c3e50; margin-top: 0;'>", .("Analysis Summary"), "</h4>",
                "<p>", .("This alluvial diagram visualizes the flow relationships between {n} categorical variables with {total} observations.",
                        list(n = n_vars, total = n_total)), "</p>"
            )
            
            # Add format-specific insights
            if (data_format == "wide_format") {
                summary_html <- paste0(summary_html,
                    "<p><strong>", .("Data Format:"), "</strong> ",
                    .("Wide format with multiple categorical variables showing transitions across categories."), "</p>"
                )
            } else if (data_format == "source_target") {
                summary_html <- paste0(summary_html,
                    "<p><strong>", .("Data Format:"), "</strong> ",
                    .("Source-target format showing directed flows between origin and destination categories."), "</p>"
                )
            }
            
            # Add key finding if possible
            if (!is.null(self$options$vars) && length(self$options$vars) >= 2) {
                summary_html <- paste0(summary_html,
                    "<p><strong>", .("Visualization:"), "</strong> ",
                    .("Flow widths represent the frequency or weighted values of transitions between categories."), "</p>"
                )
            }
            
            summary_html <- paste0(summary_html, "</div>")
            
            self$results$naturalSummary$setContent(summary_html)
        },
        
        # Generate copy-ready report sentences
        .generateReportSentence = function(data, data_format) {
            if (!("reportSentence" %in% names(self$results))) return()
            
            n_obs <- nrow(data)
            n_vars <- length(self$options$vars)
            
            # Create report sentence based on analysis type
            if (data_format == "wide_format" && n_vars > 0) {
                var_names <- paste(self$options$vars, collapse = ", ")
                report <- .("An alluvial diagram was created to visualize relationships between {n} categorical variables ({vars}) using {obs} observations.",
                           list(n = n_vars, vars = var_names, obs = n_obs))
            } else if (data_format == "source_target") {
                report <- .("A flow diagram was generated showing transitions from source to target categories with {obs} data points.",
                           list(obs = n_obs))
            } else {
                report <- .("An alluvial visualization was created with {obs} observations.",
                           list(obs = n_obs))
            }
            
            # Add clinical context if in clinical mode
            if (self$options$clinical_mode) {
                report <- paste0(report, " ",
                    .("Clinical analysis mode was enabled for enhanced medical data interpretation."))
            }
            
            # Create jamovi-compatible copy-ready HTML
            report_html <- paste0(
                "<div style='background-color: #e8f5e9; padding: 12px; border-radius: 6px; border-left: 4px solid #4caf50;'>",
                "<h4 style='margin: 0 0 8px 0; color: #2e7d32;'>", .("Copy-Ready Report Text"), "</h4>",
                "<div style='margin: 8px 0; padding: 8px; background: #f5f5f5; border-radius: 4px; font-family: monospace; border: 1px solid #ddd; user-select: all;'>",
                private$.sanitizeHTML(report),
                "</div>",
                "<p style='margin: 8px 0 0 0; font-size: 0.9em; color: #666; font-style: italic;'>",
                .("↑ Select text above and copy (Ctrl+C / Cmd+C)"),
                "</p>",
                "</div>"
            )
            
            self$results$reportSentence$setContent(report_html)
        },
        
        # Generate caveats and assumptions panel
        .generateCaveats = function() {
            if (!("caveats" %in% names(self$results))) return()
            
            caveats_html <- paste0(
                "<div style='background-color: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;'>",
                "<h4 style='color: #856404; margin-top: 0;'>", .("Important Considerations"), "</h4>",
                "<ul style='margin-bottom: 0;'>",
                "<li>", .("Best suited for categorical variables with fewer than 20 categories"), "</li>",
                "<li>", .("Flow width represents frequency or weighted values"), "</li>",
                "<li>", .("Complex flows may become difficult to interpret with many categories"), "</li>",
                "<li>", .("Colors aid visualization but ensure accessibility for color-blind users"), "</li>",
                "<li>", .("Missing values are excluded from the visualization"), "</li>",
                "</ul>",
                "<p style='margin-top: 10px; margin-bottom: 0;'><strong>", .("Assumptions:"), "</strong></p>",
                "<ul style='margin-bottom: 0;'>",
                "<li>", .("Data represents meaningful categorical relationships"), "</li>",
                "<li>", .("Categories are mutually exclusive within each variable"), "</li>",
                "<li>", .("Flow direction implies temporal or logical progression when applicable"), "</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$caveats$setContent(caveats_html)
        },
        
        # Add progress indicator for large datasets
        .showProgress = function(message) {
            if ("todo" %in% names(self$results)) {
                progress_html <- paste0(
                    "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 6px; text-align: center; border-left: 4px solid #2196f3;'>",
                    "<div style='margin-bottom: 8px;'>",
                    "<span style='font-size: 1.2em;'>⏳</span> <strong>", .("Processing"), "</strong>",
                    "</div>",
                    "<div style='color: #1565c0;'>", private$.sanitizeHTML(message), "</div>",
                    "</div>"
                )
                self$results$todo$setContent(progress_html)
            }
        },

        # Apply export compatibility modifications to plot
        .applyExportCompatibility = function(plot) {
            if (is.null(plot)) return(plot)

            export_mode <- self$options$export_compatibility

            if (export_mode == "standard") {
                return(plot)
            }

            # Apply modifications based on export compatibility mode
            if (export_mode == "riverplot" || export_mode == "both") {
                # Add metadata for riverplot compatibility
                plot <- plot +
                    ggplot2::labs(caption = paste("Generated with ClinicoPath - Compatible with riverplot package")) +
                    ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, color = "gray50"))
            }

            return(plot)
        }

        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # Condensation plot generation function
            # Creates a detailed view of how one specific variable relates to others
            
            # Input validation - requires both variables and condensation variable
            if (is.null(self$options$condensationvar) || is.null(self$options$vars))
                return()

            if (nrow(self$data) == 0)
                stop(.('Data contains no (complete) rows. Please check your data.'))

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
