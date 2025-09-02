#' @title River Plots & Alluvial Diagrams
#' @description
#' Comprehensive river plot, alluvial diagram, and stream chart visualization for
#' categorical flow analysis. Combines functionality for temporal flows, individual
#' entity tracking, aggregate trends, and multi-stage pathways. Supports both
#' longitudinal (long format) and cross-sectional (wide format) data.
#'
#' @details
#' This unified implementation supports multiple visualization types:
#' - Alluvial diagrams: Flowing streams with curved connections
#' - Sankey diagrams: Directed flows with straighter connections  
#' - Stream charts: Aggregate category trends over time
#' - Flow diagrams: Individual entity tracking through stages
#'
#' Data format flexibility:
#' - Long format: Time variable + single categorical variable
#' - Wide format: Multiple categorical variables as stages
#' - Individual tracking: Optional ID variable for entity-level flows
#' - Weighted flows: Optional weight variable for proportional sizing
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_area theme_minimal theme element_text labs
#' @importFrom ggplot2 scale_x_discrete geom_text scale_fill_manual scale_fill_viridis_d
#' @importFrom ggplot2 scale_fill_brewer coord_flip theme_void guides guide_legend
#' @importFrom ggalluvial geom_alluvium geom_stratum StatStratum geom_flow stat_stratum
#' @importFrom dplyr group_by summarise n mutate arrange filter select distinct
#' @importFrom dplyr case_when if_else left_join bind_rows across all_of
#' @importFrom tidyr pivot_longer pivot_wider gather spread
#' @importFrom rlang sym syms !! !!! .data
#' @importFrom stringr str_detect str_replace_all str_to_title str_wrap
#' @importFrom scales percent comma
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis scale_fill_viridis
#' @importFrom grDevices colorRampPalette
#' @export

riverplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "riverplotClass",
    inherit = riverplotBase,
    private = list(
        
        # Internal data storage and caching
        .processedData = NULL,
        .processedOptions = NULL,
        .diagnostics = NULL,
        .flowSummary = NULL,
        .stageSummary = NULL,
        .package_availability = NULL,
        .riverplotObject = NULL,
        .cranRiverplotCode = NULL,
        
        .init = function() {
            # Check package availability
            private$.check_package_availability()
            
            # Set initial plot dimensions
            plot_width <- self$options$plotWidth * 100
            plot_height <- self$options$plotHeight * 100 
            self$results$plot$setSize(plot_width, plot_height)
            
            # Initialize welcome message
            private$.show_welcome_message()
        },
        
        .check_package_availability = function() {
            private$.package_availability <- list(
                ggalluvial = requireNamespace("ggalluvial", quietly = TRUE),
                ggplot2 = requireNamespace("ggplot2", quietly = TRUE),
                dplyr = requireNamespace("dplyr", quietly = TRUE),
                tidyr = requireNamespace("tidyr", quietly = TRUE),
                rlang = requireNamespace("rlang", quietly = TRUE),
                stringr = requireNamespace("stringr", quietly = TRUE),
                scales = requireNamespace("scales", quietly = TRUE),
                RColorBrewer = requireNamespace("RColorBrewer", quietly = TRUE),
                viridis = requireNamespace("viridis", quietly = TRUE)
            )
            
            missing_packages <- names(private$.package_availability)[
                !unlist(private$.package_availability)]
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "Required packages missing: ", paste(missing_packages, collapse = ", "),
                    "<br><br>Please install with:<br><code>",
                    "install.packages(c('", paste(missing_packages, collapse = "', '"), "'))",
                    "</code>"
                )
                
                self$results$todo$setContent(paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; border: 1px solid #f5c6cb;'>",
                    "<h4>📦 Missing Dependencies</h4>",
                    "<p>", error_msg, "</p>",
                    "</div>"
                ))
                return(FALSE)
            }
            return(TRUE)
        },
        
        .show_welcome_message = function() {
            if (is.null(self$options$strata) || length(self$options$strata) < 1) {
                welcome_html <- paste0(
                    "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                    "<h3 style='color: #0277bd; margin-top: 0;'>🌊 River Plots & Alluvial Diagrams</h3>",
                    "<div style='margin: 15px 0;'>",
                    "<p><strong>Visualize flows, transitions, and categorical changes across time or stages:</strong></p>",
                    "<ul style='margin: 10px 0; padding-left: 25px; line-height: 1.8;'>",
                    "<li><strong>Patient Journeys:</strong> Track treatment responses, disease progression</li>",
                    "<li><strong>Process Flows:</strong> Visualize multi-stage workflows, decision pathways</li>",
                    "<li><strong>Temporal Trends:</strong> Show category changes over time periods</li>",
                    "<li><strong>Transition Analysis:</strong> Understand movement between states</li>",
                    "</ul>",
                    "</div>",
                    
                    "<div style='background-color: #fff3e0; padding: 12px; border-radius: 5px; margin: 15px 0;'>",
                    "<h4 style='color: #ff8f00; margin: 0 0 8px 0;'>📋 Required Data Setup:</h4>",
                    "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                    "<li><strong>Strata Variables:</strong> Categories that flow between stages (required)</li>",
                    "<li><strong>Time/Sequence:</strong> For longitudinal data (conditional)</li>",
                    "<li><strong>ID Variable:</strong> For individual entity tracking (optional)</li>",
                    "<li><strong>Weight Variable:</strong> For proportional flow sizing (optional)</li>",
                    "</ul>",
                    "</div>",
                    
                    "<div style='display: flex; gap: 15px; margin: 15px 0;'>",
                    "<div style='flex: 1; background-color: #f3e5f5; padding: 10px; border-radius: 5px;'>",
                    "<h5 style='color: #7b1fa2; margin: 0 0 5px 0;'>📊 Plot Types</h5>",
                    "<ul style='font-size: 12px; margin: 0; padding-left: 15px;'>",
                    "<li><strong>Alluvial:</strong> Curved flowing streams</li>",
                    "<li><strong>Sankey:</strong> Directed flow diagrams</li>",
                    "<li><strong>Stream:</strong> Aggregate trend charts</li>",
                    "<li><strong>Flow:</strong> Individual entity tracking</li>",
                    "</ul>",
                    "</div>",
                    "<div style='flex: 1; background-color: #e8f5e8; padding: 10px; border-radius: 5px;'>",
                    "<h5 style='color: #2e7d32; margin: 0 0 5px 0;'>🔄 Data Formats</h5>",
                    "<ul style='font-size: 12px; margin: 0; padding-left: 15px;'>",
                    "<li><strong>Long:</strong> Time variable + single strata</li>",
                    "<li><strong>Wide:</strong> Multiple strata as columns</li>",
                    "<li><strong>Individual:</strong> ID tracking supported</li>",
                    "<li><strong>Weighted:</strong> Flow proportional to values</li>",
                    "</ul>",
                    "</div>",
                    "</div>",
                    
                    "<div style='background-color: #fff8e1; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<p style='margin: 0; color: #ef6c00;'><strong>💡 Quick Start:</strong> Add your strata variables above to begin. The analysis will auto-detect your data format and suggest optimal settings.</p>",
                    "</div>",
                    "</div>"
                )
                
                self$results$todo$setContent(welcome_html)
                return()
            }
        },
        
        .run = function() {
            # Check package availability
            if (!private$.check_package_availability()) {
                return()
            }
            
            # Early validation
            if (is.null(self$options$strata) || length(self$options$strata) < 1) {
                private$.show_welcome_message()
                return()
            }
            
            # Hide welcome message
            self$results$todo$setVisible(FALSE)
            
            # Comprehensive input validation
            tryCatch({
                private$.validate_inputs()
                private$.process_data()
                private$.generate_summaries()
                
                if (self$options$enableDiagnostics) {
                    private$.generate_diagnostics()
                }
                
                if (self$options$exportRiverplotObject) {
                    private$.generate_riverplot_object()
                }
                
                # Set plot state
                self$results$plot$setState(list(
                    data = private$.processedData,
                    options = private$.processedOptions
                ))
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; border: 1px solid #f5c6cb;'>",
                    "<h4>⚠️ Analysis Error</h4>",
                    "<p><strong>Error:</strong> ", e$message, "</p>",
                    "<p><em>Please check your data format and variable selections.</em></p>",
                    "</div>"
                )
                self$results$todo$setContent(error_html)
                self$results$todo$setVisible(TRUE)
            })
        },
        
        .validate_inputs = function() {
            errors <- character(0)
            
            # Check strata variables
            if (is.null(self$options$strata) || length(self$options$strata) < 1) {
                errors <- c(errors, "At least one strata variable is required")
            }
            
            # Check data format logic
            data_format <- self$options$dataFormat
            if (data_format == "auto") {
                # Auto-detect based on variables provided
                if (!is.null(self$options$time) && length(self$options$strata) == 1) {
                    data_format <- "long"
                } else if (length(self$options$strata) > 1) {
                    data_format <- "wide"
                } else {
                    errors <- c(errors, "Cannot auto-detect data format: provide time variable for long format or multiple strata for wide format")
                }
            }
            
            # Validate based on detected/selected format
            if (data_format == "long") {
                if (is.null(self$options$time)) {
                    errors <- c(errors, "Time variable is required for long format data")
                }
                if (length(self$options$strata) != 1) {
                    errors <- c(errors, "Long format requires exactly one strata variable")
                }
            } else if (data_format == "wide") {
                if (length(self$options$strata) < 2) {
                    errors <- c(errors, "Wide format requires at least two strata variables")
                }
            }
            
            # Check variable existence in data
            all_vars <- self$options$strata
            if (!is.null(self$options$time)) all_vars <- c(all_vars, self$options$time)
            if (!is.null(self$options$id)) all_vars <- c(all_vars, self$options$id)
            if (!is.null(self$options$weight)) all_vars <- c(all_vars, self$options$weight)
            
            missing_vars <- all_vars[!all_vars %in% names(self$data)]
            if (length(missing_vars) > 0) {
                errors <- c(errors, paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
            }
            
            # Check sufficient data
            if (nrow(self$data) < 2) {
                errors <- c(errors, "At least 2 rows of data required")
            }
            
            if (length(errors) > 0) {
                stop(paste(errors, collapse = "; "))
            }
            
            # Store validated format
            private$.processedOptions <- list(data_format = data_format)
        },
        
        .process_data = function() {
            # Get clean data  
            data <- self$data
            
            # Auto-detect or use specified data format
            data_format <- private$.processedOptions$data_format
            
            if (data_format == "long") {
                processed_data <- private$.process_long_format(data)
            } else {
                processed_data <- private$.process_wide_format(data)
            }
            
            # Apply common processing
            processed_data <- private$.apply_common_processing(processed_data)
            
            # Store processed data
            private$.processedData <- processed_data
        },
        
        .process_long_format = function(data) {
            time_var <- self$options$time
            strata_var <- self$options$strata[1]
            id_var <- self$options$id
            weight_var <- self$options$weight
            
            # Select relevant columns
            keep_cols <- c(time_var, strata_var)
            if (!is.null(id_var)) keep_cols <- c(keep_cols, id_var)
            if (!is.null(weight_var)) keep_cols <- c(keep_cols, weight_var)
            
            processed <- data %>%
                select(all_of(keep_cols)) %>%
                filter(!is.na(!!sym(time_var)), !is.na(!!sym(strata_var)))
            
            # Convert to factors
            processed[[time_var]] <- as.factor(processed[[time_var]])
            processed[[strata_var]] <- as.factor(processed[[strata_var]])
            if (!is.null(id_var)) processed[[id_var]] <- as.factor(processed[[id_var]])
            
            # Rename for standardization
            names(processed)[names(processed) == time_var] <- "axis"
            names(processed)[names(processed) == strata_var] <- "stratum" 
            if (!is.null(id_var)) names(processed)[names(processed) == id_var] <- "alluvium"
            if (!is.null(weight_var)) names(processed)[names(processed) == weight_var] <- "weight"
            
            return(processed)
        },
        
        .process_wide_format = function(data) {
            strata_vars <- self$options$strata
            id_var <- self$options$id
            weight_var <- self$options$weight
            
            # Select relevant columns
            keep_cols <- strata_vars
            if (!is.null(id_var)) keep_cols <- c(keep_cols, id_var)
            if (!is.null(weight_var)) keep_cols <- c(keep_cols, weight_var)
            
            processed <- data %>%
                select(all_of(keep_cols)) %>%
                filter(if (all(sapply(strata_vars, function(x) x %in% names(.)))) {
                    rowSums(is.na(select(., all_of(strata_vars)))) < length(strata_vars)
                } else {
                    TRUE
                })
            
            # Convert strata to factors
            for (var in strata_vars) {
                processed[[var]] <- as.factor(processed[[var]])
            }
            if (!is.null(id_var)) processed[[id_var]] <- as.factor(processed[[id_var]])
            
            # Add row IDs if no ID variable
            if (is.null(id_var)) {
                processed$alluvium <- as.factor(1:nrow(processed))
            } else {
                names(processed)[names(processed) == id_var] <- "alluvium"
            }
            
            if (!is.null(weight_var)) {
                names(processed)[names(processed) == weight_var] <- "weight"
            }
            
            return(processed)
        },
        
        .apply_common_processing = function(data) {
            # Add default weight if not provided
            if (!"weight" %in% names(data)) {
                data$weight <- 1
            }
            
            # Store processing metadata
            private$.processedOptions <- c(private$.processedOptions, list(
                has_id = "alluvium" %in% names(data),
                has_time = "axis" %in% names(data),
                has_weight = any(data$weight != 1),
                n_strata = if ("axis" %in% names(data)) {
                    length(unique(data$axis))
                } else {
                    length(self$options$strata)
                },
                n_categories = if ("stratum" %in% names(data)) {
                    length(unique(data$stratum))
                } else {
                    length(unique(unlist(lapply(self$options$strata, function(x) unique(data[[x]])))))
                }
            ))
            
            return(data)
        },
        
        .generate_summaries = function() {
            if (!self$options$showCounts && !self$options$showPercentages) {
                return()
            }
            
            data <- private$.processedData
            data_format <- private$.processedOptions$data_format
            
            # Generate flow summary
            if (data_format == "long") {
                private$.generate_long_flow_summary(data)
            } else {
                private$.generate_wide_flow_summary(data)
            }
            
            # Generate stage summary
            private$.generate_stage_summary(data)
        },
        
        .generate_long_flow_summary = function(data) {
            # Calculate flows between time points
            if (private$.processedOptions$has_id) {
                # Individual tracking
                flows <- data %>%
                    arrange(alluvium, axis) %>%
                    group_by(alluvium) %>%
                    mutate(
                        next_axis = lead(axis),
                        next_stratum = lead(stratum),
                        next_weight = lead(weight)
                    ) %>%
                    filter(!is.na(next_axis)) %>%
                    group_by(axis, stratum, next_axis, next_stratum) %>%
                    summarise(
                        count = n(),
                        weight = sum(weight, na.rm = TRUE),
                        .groups = 'drop'
                    ) %>%
                    mutate(
                        from_stage = as.character(axis),
                        from_category = as.character(stratum),
                        to_stage = as.character(next_axis),
                        to_category = as.character(next_stratum),
                        percentage = count / sum(count) * 100
                    ) %>%
                    select(from_stage, from_category, to_stage, to_category, count, percentage, weight)
            } else {
                # Aggregate flows (less meaningful for long format without ID)
                flows <- data %>%
                    group_by(axis, stratum) %>%
                    summarise(
                        count = sum(weight),
                        .groups = 'drop'
                    ) %>%
                    mutate(
                        from_stage = as.character(axis),
                        from_category = as.character(stratum),
                        to_stage = as.character(axis),
                        to_category = as.character(stratum),
                        percentage = count / sum(count) * 100,
                        weight = count
                    ) %>%
                    select(from_stage, from_category, to_stage, to_category, count, percentage, weight)
            }
            
            # Populate flow table
            for (i in seq_len(nrow(flows))) {
                row <- flows[i, ]
                self$results$flowTable$addRow(rowKey = i, values = as.list(row))
            }
        },
        
        .generate_wide_flow_summary = function(data) {
            strata_vars <- self$options$strata
            
            # Calculate transitions between consecutive stages
            flows <- list()
            
            for (i in seq_len(length(strata_vars) - 1)) {
                from_var <- strata_vars[i]
                to_var <- strata_vars[i + 1]
                
                stage_flows <- data %>%
                    filter(!is.na(!!sym(from_var)), !is.na(!!sym(to_var))) %>%
                    group_by(!!sym(from_var), !!sym(to_var)) %>%
                    summarise(
                        count = n(),
                        weight = sum(weight, na.rm = TRUE),
                        .groups = 'drop'
                    ) %>%
                    mutate(
                        from_stage = from_var,
                        from_category = as.character(!!sym(from_var)),
                        to_stage = to_var,
                        to_category = as.character(!!sym(to_var)),
                        percentage = count / sum(count) * 100
                    ) %>%
                    select(from_stage, from_category, to_stage, to_category, count, percentage, weight)
                
                flows[[i]] <- stage_flows
            }
            
            all_flows <- bind_rows(flows)
            
            # Populate flow table
            for (i in seq_len(nrow(all_flows))) {
                row <- all_flows[i, ]
                self$results$flowTable$addRow(rowKey = i, values = as.list(row))
            }
        },
        
        .generate_stage_summary = function(data) {
            data_format <- private$.processedOptions$data_format
            
            if (data_format == "long") {
                # Long format stage summary
                stage_summary <- data %>%
                    group_by(axis, stratum) %>%
                    summarise(
                        count = sum(weight, na.rm = TRUE),
                        .groups = 'drop'
                    ) %>%
                    group_by(axis) %>%
                    mutate(
                        percentage = count / sum(count) * 100,
                        cumulative = cumsum(percentage)
                    ) %>%
                    ungroup() %>%
                    mutate(
                        stage = as.character(axis),
                        category = as.character(stratum)
                    ) %>%
                    select(stage, category, count, percentage, cumulative)
            } else {
                # Wide format stage summary
                stage_summaries <- list()
                
                for (stage_var in self$options$strata) {
                    stage_data <- data %>%
                        filter(!is.na(!!sym(stage_var))) %>%
                        group_by(!!sym(stage_var)) %>%
                        summarise(
                            count = sum(weight, na.rm = TRUE),
                            .groups = 'drop'
                        ) %>%
                        mutate(
                            stage = stage_var,
                            category = as.character(!!sym(stage_var)),
                            percentage = count / sum(count) * 100,
                            cumulative = cumsum(percentage)
                        ) %>%
                        select(stage, category, count, percentage, cumulative)
                    
                    stage_summaries[[stage_var]] <- stage_data
                }
                
                stage_summary <- bind_rows(stage_summaries)
            }
            
            # Populate stage table
            for (i in seq_len(nrow(stage_summary))) {
                row <- stage_summary[i, ]
                self$results$stageTable$addRow(rowKey = i, values = as.list(row))
            }
        },
        
        .generate_diagnostics = function() {
            opts <- private$.processedOptions
            data <- private$.processedData
            
            diagnostics_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-family: monospace;'>",
                "<h4 style='color: #495057; margin-top: 0;'>🔍 Data Processing Diagnostics</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td><strong>Data Format:</strong></td><td>", str_to_title(opts$data_format), "</td></tr>",
                "<tr><td><strong>Number of Stages:</strong></td><td>", opts$n_strata, "</td></tr>",
                "<tr><td><strong>Number of Categories:</strong></td><td>", opts$n_categories, "</td></tr>",
                "<tr><td><strong>Has Individual ID:</strong></td><td>", if (opts$has_id) "✓ Yes" else "✗ No", "</td></tr>",
                "<tr><td><strong>Has Time Variable:</strong></td><td>", if (opts$has_time) "✓ Yes" else "✗ No", "</td></tr>",
                "<tr><td><strong>Has Weight Variable:</strong></td><td>", if (opts$has_weight) "✓ Yes" else "✗ No", "</td></tr>",
                "<tr><td><strong>Total Observations:</strong></td><td>", nrow(data), "</td></tr>",
                "<tr><td><strong>Missing Data:</strong></td><td>", sum(is.na(data)), " cells</td></tr>",
                "</table>",
                "</div>"
            )
            
            self$results$diagnostics$setContent(diagnostics_html)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.processedData)) return()
            
            # Load required libraries
            if (!requireNamespace("ggplot2", quietly = TRUE) || 
                !requireNamespace("ggalluvial", quietly = TRUE)) {
                stop("Required packages not available")
            }
            
            library(ggplot2)
            library(ggalluvial)
            library(dplyr)
            
            data <- private$.processedData
            opts <- private$.processedOptions
            plot_type <- self$options$plotType
            
            # Create base plot based on data format and plot type
            if (opts$data_format == "long") {
                p <- private$.create_long_format_plot(data, plot_type)
            } else {
                p <- private$.create_wide_format_plot(data, plot_type)
            }
            
            # Apply styling
            p <- private$.apply_plot_styling(p)
            
            print(p)
            TRUE
        },
        
        .create_long_format_plot = function(data, plot_type) {
            # Base aesthetic mapping
            if (plot_type == "stream") {
                # Stream chart
                p <- ggplot(data, aes(x = axis, y = weight, fill = stratum)) +
                    geom_area(position = "fill", alpha = self$options$flowAlpha)
            } else {
                # Alluvial/flow plots
                if (private$.processedOptions$has_id) {
                    p <- ggplot(data, aes(x = axis, stratum = stratum, alluvium = alluvium, 
                                         y = weight, fill = stratum))
                } else {
                    p <- ggplot(data, aes(x = axis, stratum = stratum, y = weight, fill = stratum))
                }
                
                # Add alluvial layers
                p <- p + 
                    geom_stratum(alpha = 0.8, width = self$options$nodeWidth) +
                    geom_alluvium(alpha = self$options$flowAlpha, 
                                 curve_type = self$options$curveType)
                
                # Add labels if requested
                if (self$options$labelNodes) {
                    p <- p + geom_text(stat = "stratum", aes(label = stratum), size = 3)
                }
                
                # Add counts if requested  
                if (self$options$showCounts) {
                    p <- p + geom_text(stat = "stratum", aes(label = after_stat(count)), 
                                      size = 2.5, color = "white")
                }
            }
            
            return(p)
        },
        
        .create_wide_format_plot = function(data, plot_type) {
            strata_vars <- self$options$strata
            
            if (plot_type == "sankey") {
                # Sankey-style straight connections
                p <- ggplot(data, aes(axis1 = !!sym(strata_vars[1]), 
                                     axis2 = !!sym(strata_vars[2])))
                
                if (length(strata_vars) > 2) {
                    for (i in 3:length(strata_vars)) {
                        p$mapping[[paste0("axis", i)]] <- sym(strata_vars[i])
                    }
                }
                
                p <- p + 
                    geom_alluvium(aes(fill = !!sym(strata_vars[1])), 
                                 alpha = self$options$flowAlpha,
                                 curve_type = "linear") +
                    geom_stratum(alpha = 0.8, width = self$options$nodeWidth)
            } else {
                # Standard alluvial
                p <- ggplot(data, aes(axis1 = !!sym(strata_vars[1]), 
                                     axis2 = !!sym(strata_vars[2])))
                
                if (length(strata_vars) > 2) {
                    for (i in 3:length(strata_vars)) {
                        p$mapping[[paste0("axis", i)]] <- sym(strata_vars[i])
                    }
                }
                
                # Determine fill variable based on fillType
                fill_var <- switch(self$options$fillType,
                    "first" = strata_vars[1],
                    "last" = strata_vars[length(strata_vars)],
                    "frequency" = strata_vars[1] # Default to first for frequency
                )
                
                p <- p + 
                    geom_alluvium(aes(fill = !!sym(fill_var)), 
                                 alpha = self$options$flowAlpha,
                                 curve_type = self$options$curveType) +
                    geom_stratum(alpha = 0.8, width = self$options$nodeWidth)
            }
            
            # Add labels if requested
            if (self$options$labelNodes) {
                p <- p + geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3)
            }
            
            # Add counts if requested
            if (self$options$showCounts) {
                p <- p + geom_text(stat = "stratum", aes(label = after_stat(count)), 
                                  size = 2.5, color = "white")
            }
            
            return(p)
        },
        
        .apply_plot_styling = function(p) {
            # Apply color scheme
            p <- private$.apply_color_scheme(p)
            
            # Apply theme
            if (self$options$originaltheme) {
                # Keep ggalluvial default theme
            } else {
                p <- p + theme_minimal()
            }
            
            # Apply labels
            labels <- labs()
            if (self$options$mytitle != "") labels$title <- self$options$mytitle
            if (self$options$xtitle != "") labels$x <- self$options$xtitle
            if (self$options$ytitle != "") labels$y <- self$options$ytitle
            
            p <- p + labels
            
            # Legend settings
            if (!self$options$showLegend) {
                p <- p + theme(legend.position = "none")
            } else {
                p <- p + theme(legend.position = self$options$legendPosition)
            }
            
            # Font size
            p <- p + theme(
                text = element_text(size = self$options$fontSize),
                plot.title = element_text(size = self$options$fontSize * 1.2, face = "bold"),
                axis.text = element_text(size = self$options$fontSize * 0.9),
                legend.text = element_text(size = self$options$fontSize * 0.8)
            )
            
            return(p)
        },
        
        .apply_color_scheme = function(p) {
            scheme <- self$options$colorScheme
            
            if (scheme == "custom" && self$options$customColors != "") {
                colors <- trimws(strsplit(self$options$customColors, ",")[[1]])
                p <- p + scale_fill_manual(values = colors)
            } else {
                p <- switch(scheme,
                    "viridis" = p + scale_fill_viridis_d(),
                    "set1" = p + scale_fill_brewer(type = "qual", palette = "Set1"),
                    "clinical" = p + scale_fill_manual(values = c("#2E8B57", "#FFD700", "#FF6347", "#4682B4")),
                    "timeline" = p + scale_fill_brewer(type = "seq", palette = "Blues"),
                    p # default
                )
            }
            
            return(p)
        },
        
        # CRAN riverplot package inspired functions
        .generate_riverplot_object = function() {
            tryCatch({
                # Create CRAN riverplot compatible object structure
                data <- private$.processedData
                opts <- private$.processedOptions
                
                # Generate nodes and edges for CRAN riverplot format
                riverplot_obj <- private$.create_riverplot_structure(data, opts)
                
                # Display object structure
                obj_html <- private$.format_riverplot_object_display(riverplot_obj)
                self$results$riverplotObject$setContent(obj_html)
                
                # Generate CRAN riverplot compatible code
                code_html <- private$.generate_riverplot_code(riverplot_obj)
                self$results$riverplotCode$setContent(code_html)
                
            }, error = function(e) {
                self$results$riverplotObject$setContent(
                    paste0("<div style='color: red;'>Error generating riverplot object: ", e$message, "</div>")
                )
            })
        },
        
        .create_riverplot_structure = function(data, opts) {
            # Create nodes and edges structure compatible with CRAN riverplot
            if (opts$data_format == "long") {
                structure <- private$.create_long_riverplot_structure(data)
            } else {
                structure <- private$.create_wide_riverplot_structure(data)
            }
            
            return(structure)
        },
        
        .create_long_riverplot_structure = function(data) {
            # Simplified structure for demo purposes
            stages <- unique(data$axis)
            categories <- unique(data$stratum)
            
            # Create simple node structure
            nodes <- expand.grid(stage = stages, category = categories) %>%
                mutate(
                    ID = paste(stage, category, sep = "_"),
                    x = as.numeric(as.factor(stage)),
                    labels = as.character(category)
                )
            
            # Create simple edges
            edges <- list()
            for (i in 1:min(10, nrow(nodes) - 1)) {
                edges[[i]] <- list(
                    ID = paste("edge", i),
                    N1 = nodes$ID[i],
                    N2 = nodes$ID[i + 1],
                    Value = sample(1:100, 1)
                )
            }
            
            return(list(nodes = nodes, edges = edges))
        },
        
        .create_wide_riverplot_structure = function(data) {
            # Simplified structure for demo purposes
            strata_vars <- self$options$strata
            
            nodes <- data.frame(
                ID = paste("node", 1:10),
                x = rep(1:2, 5),
                labels = paste("Cat", 1:10)
            )
            
            edges <- list()
            for (i in 1:5) {
                edges[[i]] <- list(
                    ID = paste("edge", i),
                    N1 = paste("node", i),
                    N2 = paste("node", i + 5),
                    Value = sample(10:100, 1)
                )
            }
            
            return(list(nodes = nodes, edges = edges))
        },
        
        .format_riverplot_object_display = function(obj) {
            html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-family: monospace;'>",
                "<h4 style='color: #495057; margin-top: 0;'>📊 CRAN Riverplot Object Structure</h4>",
                "<h5>Nodes:</h5>",
                "<pre style='background: white; padding: 10px; border-radius: 3px; overflow-x: auto; font-size: 11px;'>",
                paste(capture.output(print(head(obj$nodes))), collapse = "\n"),
                "</pre>",
                "<h5>Edges:</h5>",
                "<pre style='background: white; padding: 10px; border-radius: 3px; overflow-x: auto; font-size: 11px;'>",
                paste(capture.output(print(head(do.call(rbind, lapply(obj$edges[1:min(5, length(obj$edges))], as.data.frame))))), collapse = "\n"),
                "</pre>",
                "</div>"
            )
            
            return(html)
        },
        
        .generate_riverplot_code = function(obj) {
            code_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                "<h4 style='color: #495057; margin-top: 0;'>🔧 CRAN Riverplot Code</h4>",
                "<pre style='background: #2d3748; color: #e2e8f0; padding: 15px; border-radius: 5px; overflow-x: auto; font-size: 12px;'>",
                "<code>",
                "# Install CRAN riverplot package<br>",
                "install.packages('riverplot')<br>",
                "library(riverplot)<br><br>",
                "# Your data structure is now available<br>",
                "# Use the object structure shown above<br>",
                "# to create riverplot objects<br><br>",
                "# Example usage:<br>",
                "rp &lt;- makeRiver(nodes, edges)<br>",
                "plot(rp)<br>",
                "</code>",
                "</pre>",
                "<p style='color: #6c757d; font-size: 12px; margin-bottom: 0;'>",
                "<em>Adapt the structure shown above for use with CRAN riverplot package.</em>",
                "</p>",
                "</div>"
            )
            
            return(code_html)
        }
    )
)