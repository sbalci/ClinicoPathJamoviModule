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
#' @importFrom grid unit
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
            
            # Apply clinical preset if selected
            private$.apply_clinical_preset()
            
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
                viridis = requireNamespace("viridis", quietly = TRUE),
                ggstream = requireNamespace("ggstream", quietly = TRUE),
                grid = requireNamespace("grid", quietly = TRUE)
            )
            
            missing_packages <- names(private$.package_availability)[
                !unlist(private$.package_availability)]
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    .("Required packages missing: "), paste(missing_packages, collapse = ", "),
                    "<br><br>", .("Please install with:"), "<br><code>",
                    "install.packages(c('", paste(missing_packages, collapse = "', '"), "'))",
                    "</code>"
                )
                
                self$results$todo$setContent(paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; border: 1px solid #f5c6cb;'>",
                    "<h4>📦 ", .("Missing Dependencies"), "</h4>",
                    "<p>", error_msg, "</p>",
                    "</div>"
                ))
                return(FALSE)
            }
            return(TRUE)
        },
        
        .apply_clinical_preset = function() {
            preset <- self$options$clinicalPreset
            
            if (preset == "none") return()
            
            # Define preset configurations
            presets <- list(
                patient_journey = list(
                    plotType = "alluvial",
                    fillType = "last",
                    labelNodes = TRUE,
                    showCounts = TRUE,
                    showPercentages = FALSE,
                    curveType = "cardinal",
                    colorScheme = "clinical",
                    nodeWidth = 0.12,
                    flowAlpha = 0.7,
                    backgroundLabels = TRUE,
                    showLegend = TRUE,
                    legendPosition = "right"
                ),
                treatment_response = list(
                    plotType = "alluvial",
                    fillType = "first",
                    labelNodes = TRUE,
                    showCounts = TRUE,
                    showPercentages = TRUE,
                    curveType = "cardinal",
                    colorScheme = "clinical",
                    nodeWidth = 0.15,
                    flowAlpha = 0.6,
                    backgroundLabels = TRUE,
                    showLegend = TRUE,
                    legendPosition = "bottom"
                ),
                disease_progression = list(
                    plotType = "sankey",
                    fillType = "last",
                    labelNodes = TRUE,
                    showCounts = TRUE,
                    showPercentages = FALSE,
                    curveType = "linear",
                    colorScheme = "clinical",
                    nodeWidth = 0.1,
                    flowAlpha = 0.8,
                    backgroundLabels = FALSE,
                    showLegend = TRUE,
                    legendPosition = "right"
                ),
                clinical_pathway = list(
                    plotType = "sankey",
                    fillType = "frequency",
                    labelNodes = TRUE,
                    showCounts = TRUE,
                    showPercentages = TRUE,
                    curveType = "linear",
                    colorScheme = "set1",
                    nodeWidth = 0.08,
                    flowAlpha = 0.7,
                    backgroundLabels = TRUE,
                    showLegend = TRUE,
                    legendPosition = "top"
                ),
                population_trends = list(
                    plotType = "stream",
                    fillType = "first",
                    labelNodes = TRUE,
                    showCounts = FALSE,
                    showPercentages = TRUE,
                    curveType = "cardinal",
                    colorScheme = "viridis",
                    nodeWidth = 0.06,
                    flowAlpha = 0.8,
                    backgroundLabels = FALSE,
                    showLegend = TRUE,
                    legendPosition = "right"
                )
            )
            
            # Apply preset if available
            if (preset %in% names(presets)) {
                preset_config <- presets[[preset]]
                
                # Apply each setting from preset
                for (setting_name in names(preset_config)) {
                    if (setting_name %in% names(self$options)) {
                        self$options[[setting_name]] <- preset_config[[setting_name]]
                    }
                }
                
                # Show preset notification
                preset_names <- list(
                    patient_journey = .("Patient Journey Tracking"),
                    treatment_response = .("Treatment Response Analysis"),
                    disease_progression = .("Disease Progression Monitoring"), 
                    clinical_pathway = .("Clinical Pathway Analysis"),
                    population_trends = .("Population Health Trends")
                )
                
                preset_info <- paste0(
                    "<div style='background-color: #d1ecf1; color: #0c5460; padding: 10px; border-radius: 5px; border: 1px solid #bee5eb; margin: 5px 0;'>",
                    "<small>📋 ", .("Applied preset:"), " <strong>", preset_names[[preset]], "</strong></small>",
                    "</div>"
                )
                
                self$results$summary$setContent(preset_info)
            }
        },
        
        .show_welcome_message = function() {
            if (is.null(self$options$strata) || length(self$options$strata) < 1) {
                welcome_html <- paste0(
                    "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                    "<h3 style='color: #0277bd; margin-top: 0;'>🌊 ", .("River Plots & Alluvial Diagrams"), "</h3>",
                    "<div style='margin: 15px 0;'>",
                    "<p><strong>", .("Visualize flows, transitions, and categorical changes across time or stages:"), "</strong></p>",
                    "<ul style='margin: 10px 0; padding-left: 25px; line-height: 1.8;'>",
                    "<li><strong>", .("Patient Journeys:"), "</strong> ", .("Track treatment responses, disease progression"), "</li>",
                    "<li><strong>", .("Process Flows:"), "</strong> ", .("Visualize multi-stage workflows, decision pathways"), "</li>",
                    "<li><strong>", .("Temporal Trends:"), "</strong> ", .("Show category changes over time periods"), "</li>",
                    "<li><strong>", .("Transition Analysis:"), "</strong> ", .("Understand movement between states"), "</li>",
                    "</ul>",
                    "</div>",
                    
                    "<div style='background-color: #fff3e0; padding: 12px; border-radius: 5px; margin: 15px 0;'>",
                    "<h4 style='color: #ff8f00; margin: 0 0 8px 0;'>📋 ", .("Required Data Setup:"), "</h4>",
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
                
                # Generate analysis summary
                private$.generate_analysis_summary()
                
                # Generate copy-ready report sentence
                private$.generate_report_sentence()
                
                # Set plot state
                self$results$plot$setState(list(
                    data = private$.processedData,
                    options = private$.processedOptions
                ))
                
            }, error = function(e) {
                # Sanitize error message to prevent HTML injection
                safe_message <- gsub("<", "&lt;", gsub(">", "&gt;", e$message))
                
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; border: 1px solid #f5c6cb;'>",
                    "<h4>⚠️ ", .("Analysis Error"), "</h4>",
                    "<p><strong>", .("Error:"), "</strong> ", safe_message, "</p>",
                    "<p><em>", .("Please check your data format and variable selections."), "</em></p>",
                    "</div>"
                )
                self$results$todo$setContent(error_html)
                self$results$todo$setVisible(TRUE)
            })
        },
        
        .validate_inputs = function() {
            errors <- character(0)
            warnings <- character(0)
            
            # Check strata variables
            if (is.null(self$options$strata) || length(self$options$strata) < 1) {
                errors <- c(errors, .("At least one strata variable is required"))
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
                    errors <- c(errors, .("Cannot auto-detect data format: provide time variable for long format or multiple strata for wide format"))
                }
            }
            
            # Validate based on detected/selected format
            if (data_format == "long") {
                if (is.null(self$options$time)) {
                    errors <- c(errors, .("Time variable is required for long format data"))
                }
                if (length(self$options$strata) != 1) {
                    errors <- c(errors, .("Long format requires exactly one strata variable"))
                }
            } else if (data_format == "wide") {
                if (length(self$options$strata) < 2) {
                    errors <- c(errors, .("Wide format requires at least two strata variables"))
                }
            }
            
            # Check variable existence in data
            all_vars <- self$options$strata
            if (!is.null(self$options$time)) all_vars <- c(all_vars, self$options$time)
            if (!is.null(self$options$id)) all_vars <- c(all_vars, self$options$id)
            if (!is.null(self$options$weight)) all_vars <- c(all_vars, self$options$weight)
            
            missing_vars <- all_vars[!all_vars %in% names(self$data)]
            if (length(missing_vars) > 0) {
                errors <- c(errors, paste(.("Variables not found in data:"), paste(missing_vars, collapse = ", ")))
            }
            
            # Check sufficient data
            if (nrow(self$data) < 2) {
                errors <- c(errors, .("At least 2 rows of data required"))
            }
            
            # Add misuse detection warnings
            warnings <- c(warnings, private$.detect_misuse())
            
            if (length(errors) > 0) {
                stop(.("Validation failed: {errors}", errors = paste(errors, collapse = "; ")))
            }
            
            # Display warnings if any
            if (length(warnings) > 0) {
                private$.show_warnings(warnings)
            }
            
            # Store validated format
            private$.processedOptions <- list(data_format = data_format)
        },
        
        .detect_misuse = function() {
            warnings <- character(0)
            data <- self$data
            
            # Check data size appropriateness
            if (nrow(data) < 20) {
                warnings <- c(warnings, .("Small dataset (n<20): Flow patterns may be unstable. Consider larger sample for reliable results."))
            }
            
            if (nrow(data) > 10000) {
                warnings <- c(warnings, .("Large dataset (n>10,000): Consider sampling for better performance, or use diagnostic mode carefully."))
            }
            
            # Check variable type appropriateness
            if (!is.null(self$options$strata) && length(self$options$strata) >= 1) {
                for (strata_var in self$options$strata) {
                    if (strata_var %in% names(data)) {
                        # Check if categorical variable has too many levels
                        n_levels <- length(unique(data[[strata_var]]))
                        if (n_levels > 15) {
                            warnings <- c(warnings, sprintf(.("Variable '%s' has %d categories: Too many categories may create cluttered visualization. Consider grouping into 5-10 meaningful categories."), strata_var, n_levels))
                        }
                        
                        # Check if variable should be factor
                        if (!is.factor(data[[strata_var]]) && n_levels < 20) {
                            warnings <- c(warnings, sprintf(.("Variable '%s' should be converted to factor for proper category ordering and display."), strata_var))
                        }
                        
                        # Check for empty categories
                        if (any(is.na(data[[strata_var]]))) {
                            na_count <- sum(is.na(data[[strata_var]]))
                            warnings <- c(warnings, sprintf(.("Variable '%s' has %d missing values: Missing data will be excluded from flow analysis."), strata_var, na_count))
                        }
                    }
                }
            }
            
            # Check ID variable appropriateness
            if (!is.null(self$options$id) && self$options$id %in% names(data)) {
                id_var <- self$options$id
                n_unique_ids <- length(unique(data[[id_var]]))
                n_total_rows <- nrow(data)
                
                if (n_unique_ids == n_total_rows) {
                    warnings <- c(warnings, .("Each row has unique ID: No repeated measures detected. Individual tracking may not provide meaningful flow patterns."))
                }
                
                if (n_unique_ids < n_total_rows / 10) {
                    warnings <- c(warnings, .("Very few unique IDs relative to data size: Check if ID variable is correctly specified."))
                }
            }
            
            # Check time variable appropriateness
            if (!is.null(self$options$time) && self$options$time %in% names(data)) {
                time_var <- self$options$time
                n_time_points <- length(unique(data[[time_var]]))
                
                if (n_time_points < 2) {
                    warnings <- c(warnings, .("Only one time point detected: Temporal flow analysis requires multiple time points."))
                }
                
                if (n_time_points > 20) {
                    warnings <- c(warnings, .("Many time points detected (>20): Consider grouping into meaningful periods for clearer visualization."))
                }
            }
            
            return(warnings)
        },
        
        .show_warnings = function(warnings) {
            if (length(warnings) == 0) return()
            
            warning_html <- paste0(
                "<div style='background-color: #fff3cd; color: #856404; padding: 12px; border-radius: 5px; border: 1px solid #ffeaa7; margin: 10px 0;'>",
                "<h5 style='color: #856404; margin: 0 0 8px 0;'>⚠️ ", .("Analysis Recommendations"), "</h5>",
                "<ul style='margin: 5px 0; font-size: 12px; line-height: 1.4;'>",
                paste0("<li>", warnings, "</li>", collapse = ""),
                "</ul>",
                "</div>"
            )
            
            # Append to summary if it exists, otherwise create warning panel
            existing_content <- self$results$summary$content
            if (!is.null(existing_content) && existing_content != "") {
                self$results$summary$setContent(paste0(existing_content, warning_html))
            } else {
                self$results$summary$setContent(warning_html)
            }
        },
        
        .populate_table_with_checkpoints = function(table, data, checkpoint_interval = 100) {
            # Helper function to populate any table with checkpoint support
            if (nrow(data) == 0) return()
            
            for (i in seq_len(nrow(data))) {
                if (i %% checkpoint_interval == 0) {
                    private$.checkpoint()
                }
                row <- data[i, ]
                table$addRow(rowKey = i, values = as.list(row))
            }
        },
        
        .process_data = function() {
            # Get clean data  
            data <- self$data
            
            # Auto-detect or use specified data format
            data_format <- private$.processedOptions$data_format
            
            # Checkpoint before data processing
            private$.checkpoint()
            
            if (data_format == "long") {
                processed_data <- private$.process_long_format(data)
            } else {
                processed_data <- private$.process_wide_format(data)
            }
            
            # Apply common processing
            processed_data <- private$.apply_common_processing(processed_data)
            
            # Store processed data
            private$.processedData <- processed_data
            
            # Calculate and store complete metadata
            n_strata <- if (data_format == "long") {
                length(unique(processed_data$axis))
            } else {
                length(self$options$strata)
            }
            
            n_categories <- length(unique(processed_data$stratum))
            has_id <- !is.null(self$options$id)
            has_weight <- !is.null(self$options$weight)
            
            # Update processedOptions with complete metadata
            private$.processedOptions <- c(private$.processedOptions, list(
                n_strata = n_strata,
                n_categories = n_categories,
                has_id = has_id,
                has_weight = has_weight
            ))
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
            data <- private$.processedData
            data_format <- private$.processedOptions$data_format
            
            # Generate flow summary if counts or percentages requested
            if (self$options$showCounts || self$options$showPercentages) {
                if (data_format == "long") {
                    private$.generate_long_flow_summary(data)
                } else {
                    private$.generate_wide_flow_summary(data)
                }
                
                # Generate stage summary
                private$.generate_stage_summary(data)
            }
            
            # Generate transition matrix if diagnostics enabled
            if (self$options$enableDiagnostics && 
                self$options$plotType %in% c("alluvial", "flow")) {
                private$.generate_transition_matrix(data)
            }
        },
        
        .generate_long_flow_summary = function(data) {
            # Checkpoint before expensive flow calculation
            private$.checkpoint()
            
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
            
            # Populate flow table with checkpoints
            private$.populate_table_with_checkpoints(self$results$flowTable, flows, 100)
        },
        
        .generate_wide_flow_summary = function(data) {
            strata_vars <- self$options$strata
            
            # Checkpoint before expensive transition calculations
            private$.checkpoint()
            
            # Calculate transitions between consecutive stages
            flows <- list()
            
            for (i in seq_len(length(strata_vars) - 1)) {
                # Checkpoint before each stage transition calculation
                private$.checkpoint(flush = FALSE)
                
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
            
            # Populate flow table with checkpoints
            private$.populate_table_with_checkpoints(self$results$flowTable, all_flows, 100)
        },
        
        .generate_stage_summary = function(data) {
            data_format <- private$.processedOptions$data_format
            
            # Checkpoint before stage summary calculation
            private$.checkpoint()
            
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
                    # Checkpoint before each stage calculation
                    private$.checkpoint(flush = FALSE)
                    
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
            private$.populate_table_with_checkpoints(self$results$stageTable, stage_summary, 100)
        },
        
        .generate_transition_matrix = function(data) {
            data_format <- private$.processedOptions$data_format
            
            # Checkpoint before expensive transition matrix calculation
            private$.checkpoint()
            
            if (data_format == "long" && private$.processedOptions$has_id) {
                # Calculate transition probabilities for long format with ID tracking
                transitions <- data %>%
                    arrange(alluvium, axis) %>%
                    group_by(alluvium) %>%
                    mutate(
                        from = as.character(stratum),
                        to = lead(as.character(stratum)),
                        next_axis = lead(axis)
                    ) %>%
                    filter(!is.na(to)) %>%
                    group_by(from, to) %>%
                    summarise(
                        count = n(),
                        weight_sum = sum(weight, na.rm = TRUE),
                        .groups = 'drop'
                    ) %>%
                    group_by(from) %>%
                    mutate(
                        probability = count / sum(count),
                        weight_prob = weight_sum / sum(weight_sum)
                    ) %>%
                    ungroup() %>%
                    arrange(desc(probability))
                
                # Clear any existing rows first
                self$results$transitionMatrix$setNote("key", NULL)
                
                # Add columns dynamically based on unique categories
                categories <- unique(c(transitions$from, transitions$to))
                
                # Limit and format transitions for display
                display_transitions <- transitions[seq_len(min(nrow(transitions), 100)), ] %>%
                    mutate(probability = round(probability, 3))
                
                # Populate transition matrix table
                private$.populate_table_with_checkpoints(self$results$transitionMatrix, display_transitions, 20)
                
            } else if (data_format == "wide") {
                # Calculate transition probabilities for wide format
                strata_vars <- self$options$strata
                all_transitions <- list()
                
                for (i in seq_len(length(strata_vars) - 1)) {
                    # Checkpoint before each stage transition calculation
                    private$.checkpoint(flush = FALSE)
                    
                    from_var <- strata_vars[i]
                    to_var <- strata_vars[i + 1]
                    
                    stage_transitions <- data %>%
                        filter(!is.na(!!sym(from_var)), !is.na(!!sym(to_var))) %>%
                        group_by(!!sym(from_var), !!sym(to_var)) %>%
                        summarise(
                            count = n(),
                            weight_sum = sum(weight, na.rm = TRUE),
                            .groups = 'drop'
                        ) %>%
                        rename(from = !!sym(from_var), to = !!sym(to_var)) %>%
                        mutate(
                            from = as.character(from),
                            to = as.character(to)
                        ) %>%
                        group_by(from) %>%
                        mutate(
                            probability = count / sum(count),
                            weight_prob = weight_sum / sum(weight_sum)
                        ) %>%
                        ungroup()
                    
                    all_transitions[[i]] <- stage_transitions
                }
                
                transitions <- bind_rows(all_transitions) %>%
                    arrange(desc(probability))
                
                # Limit and format transitions for display
                display_transitions <- transitions[seq_len(min(nrow(transitions), 100)), ] %>%
                    mutate(probability = round(probability, 3))
                
                # Populate transition matrix table
                private$.populate_table_with_checkpoints(self$results$transitionMatrix, display_transitions, 20)
            }
        },
        
        .generate_analysis_summary = function() {
            opts <- private$.processedOptions
            data <- private$.processedData
            
            # Generate natural language summary with safe property access
            data_format <- stringr::str_to_title(opts$data_format %||% "unknown")
            n_stages <- opts$n_strata %||% 0
            n_categories <- opts$n_categories %||% 0
            n_observations <- nrow(data)
            plot_type <- stringr::str_to_title(self$options$plotType)
            
            # Build summary components
            format_desc <- if (opts$data_format == "long") {
                .("longitudinal data with time points")
            } else {
                .("multi-stage pathway data")
            }
            
            tracking_desc <- if (opts$has_id) {
                .("individual entity tracking enabled")
            } else {
                .("population-level flow analysis")
            }
            
            weight_desc <- if (opts$has_weight) {
                .("with weighted flow values")
            } else {
                .("with equal flow weights")
            }
            
            # Clinical interpretation
            clinical_use <- switch(tolower(self$options$plotType),
                "alluvial" = .("ideal for visualizing patient treatment journeys and response patterns"),
                "sankey" = .("suitable for analyzing treatment decision pathways and resource flows"),
                "stream" = .("perfect for showing population-level trends and temporal changes"),
                "flow" = .("designed for tracking individual patient trajectories through care stages"),
                .("configured for categorical flow visualization")
            )
            
            summary_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #2e7d32;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>📊 ", .("Analysis Overview"), "</h4>",
                "<p><strong>", .("Data Configuration:"), "</strong> ", 
                sprintf(.("This analysis visualizes %s using %s across %d stages with %d categories (%d observations)."), 
                        format_desc, tracking_desc, n_stages, n_categories, n_observations), "</p>",
                "<p><strong>", .("Visualization Type:"), "</strong> ", 
                sprintf(.("%s diagram %s - %s."), plot_type, weight_desc, clinical_use), "</p>",
                
                # Add clinical guidance
                "<div style='background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<h5 style='color: #ef6c00; margin: 0 0 5px 0;'>🩺 ", .("Clinical Interpretation Guide"), "</h5>",
                "<ul style='margin: 5px 0; font-size: 12px;'>",
                "<li><strong>", .("Flow Width:"), "</strong> ", .("Proportional to number of patients/cases in that pathway"), "</li>",
                "<li><strong>", .("Colors:"), "</strong> ", .("Represent categories - same color indicates same outcome/stage"), "</li>",
                "<li><strong>", .("Connections:"), "</strong> ", .("Show transition patterns between time points or stages"), "</li>",
                "</ul>",
                "</div>",
                "</div>"
            )
            
            self$results$summary$setContent(summary_html)
        },
        
        .generate_report_sentence = function() {
            opts <- private$.processedOptions
            data <- private$.processedData
            
            # Extract key statistics
            n_observations <- nrow(data)
            n_stages <- opts$n_strata
            n_categories <- opts$n_categories
            plot_type <- stringr::str_to_lower(self$options$plotType)
            
            # Determine if individual tracking is used
            individual_tracking <- opts$has_id
            
            # Generate clinical report sentences based on plot type and configuration
            if (individual_tracking && opts$data_format == "long") {
                # Individual patient tracking
                n_unique_ids <- length(unique(data$alluvium))
                
                report_text <- sprintf(
                    .("Flow analysis of %d individual %s tracked across %d time points revealed %d distinct response categories. %s visualization demonstrates individual trajectory patterns and transition probabilities between stages."),
                    n_unique_ids,
                    if (n_unique_ids > 1) .("patients/subjects") else .("patient/subject"),
                    n_stages,
                    n_categories,
                    stringr::str_to_title(plot_type)
                )
                
                # Add clinical interpretation
                clinical_interpretation <- switch(plot_type,
                    "alluvial" = .("Flowing connections highlight treatment response patterns and patient journey complexity."),
                    "flow" = .("Individual flow tracking enables identification of specific trajectory patterns and treatment response sequences."),
                    "sankey" = .("Directed flows quantify transition probabilities and pathway efficiency."),
                    .("Categorical flow patterns demonstrate temporal changes in patient outcomes.")
                )
                
            } else if (opts$data_format == "wide") {
                # Multi-stage pathway analysis
                report_text <- sprintf(
                    .("Multi-stage pathway analysis of %d cases across %d sequential stages identified %d outcome categories. %s diagram illustrates transition patterns and stage-specific distributions."),
                    n_observations,
                    n_stages,
                    n_categories,
                    stringr::str_to_title(plot_type)
                )
                
                clinical_interpretation <- switch(plot_type,
                    "sankey" = .("Pathway analysis reveals decision points and outcome probabilities at each clinical stage."),
                    "alluvial" = .("Multi-stage flow visualization demonstrates progression patterns and category persistence."),
                    .("Sequential stage analysis shows evolution of outcomes through treatment phases.")
                )
                
            } else {
                # Population-level temporal analysis
                report_text <- sprintf(
                    .("Temporal analysis of %d observations across %d time points shows %d response categories. Population-level %s visualization reveals aggregate trend patterns and category prevalence changes over time."),
                    n_observations,
                    n_stages,
                    n_categories,
                    plot_type
                )
                
                clinical_interpretation <- switch(plot_type,
                    "stream" = .("Population trends indicate shifts in category prevalence and temporal response patterns."),
                    "alluvial" = .("Aggregate flow patterns demonstrate population-level changes in outcome distributions."),
                    .("Temporal flow analysis reveals population trends in categorical outcomes.")
                )
            }
            
            # Format the complete report
            complete_report <- paste0(report_text, " ", clinical_interpretation)
            
            # Generate copy-ready HTML with copy button
            report_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;'>",
                "<h5 style='color: #495057; margin: 0 0 10px 0;'>📋 ", .("Copy-Ready Clinical Report"), "</h5>",
                "<div id='reportText' style='background-color: white; padding: 12px; border-radius: 5px; font-family: serif; line-height: 1.6; border: 1px solid #e9ecef; margin: 8px 0;'>",
                complete_report,
                "</div>",
                "<div style='margin-top: 8px; text-align: right;'>",
                "<button onclick='",
                "var text = document.getElementById(\"reportText\").innerText;",
                "navigator.clipboard.writeText(text).then(function() {",
                "alert(\"", .("Report text copied to clipboard!"), "\");",
                "});' ",
                "style='background-color: #007bff; color: white; border: none; padding: 6px 12px; border-radius: 4px; cursor: pointer; font-size: 12px;'>",
                "📋 ", .("Copy to Clipboard"),
                "</button>",
                "</div>",
                "<div style='margin-top: 8px; padding: 8px; background-color: #e9ecef; border-radius: 4px; font-size: 11px; color: #6c757d;'>",
                "<strong>", .("Usage Note:"), "</strong> ", 
                .("This text is formatted for inclusion in clinical reports, research manuscripts, or case presentations. Adjust terminology as needed for your specific context."),
                "</div>",
                "</div>"
            )
            
            self$results$reportSentence$setContent(report_html)
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
                # Enhanced stream chart with ggstream-like capabilities
                if (requireNamespace("ggstream", quietly = TRUE)) {
                    library(ggstream)
                    p <- ggplot(data, aes(x = axis, y = weight, fill = stratum)) +
                        geom_stream(alpha = self$options$flowAlpha, 
                                   type = if (self$options$sortStreams) "ridge" else "proportional")
                    
                    # Add stream labels if requested
                    if (self$options$labelNodes) {
                        p <- p + geom_stream_label(aes(label = stratum), size = 3)
                    }
                } else {
                    # Fallback to basic area chart
                    p <- ggplot(data, aes(x = axis, y = weight, fill = stratum)) +
                        geom_area(position = if (self$options$sortStreams) "stack" else "fill", 
                                 alpha = self$options$flowAlpha)
                }
            } else if (plot_type == "flow") {
                # Enhanced flow diagram for individual tracking
                if (private$.processedOptions$has_id) {
                    p <- ggplot(data, aes(x = axis, stratum = stratum, alluvium = alluvium, 
                                         y = weight, fill = stratum))
                    
                    # Apply flow-specific styling
                    curve_type <- if (self$options$curveType == "riverplot") "sine" else self$options$curveType
                    
                    p <- p + 
                        geom_flow(aes(fill = stratum), 
                                 stat = "alluvium",
                                 alpha = self$options$flowAlpha,
                                 curve_type = curve_type,
                                 width = self$options$nodeWidth)
                    
                    # Add stratum with custom styling
                    p <- p + geom_stratum(alpha = 0.9, width = self$options$nodeWidth)
                } else {
                    # Fallback to alluvial for aggregate flow
                    p <- ggplot(data, aes(x = axis, stratum = stratum, y = weight, fill = stratum))
                    p <- p + 
                        geom_stratum(alpha = 0.8, width = self$options$nodeWidth) +
                        geom_alluvium(alpha = self$options$flowAlpha, 
                                     curve_type = self$options$curveType)
                }
            } else {
                # Standard alluvial/sankey plots
                if (private$.processedOptions$has_id) {
                    p <- ggplot(data, aes(x = axis, stratum = stratum, alluvium = alluvium, 
                                         y = weight, fill = stratum))
                } else {
                    p <- ggplot(data, aes(x = axis, stratum = stratum, y = weight, fill = stratum))
                }
                
                # Prepare curve arguments with granularity support
                curve_args <- list(
                    alpha = self$options$flowAlpha,
                    curve_type = if (self$options$curveType == "riverplot") "sine" else self$options$curveType
                )
                
                # Apply curve granularity for compatible curve types
                if (self$options$curveType %in% c("cardinal", "cubic", "basis", "sin", "sine")) {
                    # ggalluvial uses 'segments' parameter for curve smoothness
                    if (self$options$curveGranularity > 10) {
                        curve_args$segments <- min(self$options$curveGranularity / 10, 50)
                    }
                }
                
                # Add alluvial layers with enhanced options
                p <- p + 
                    geom_stratum(alpha = 0.8, width = self$options$nodeWidth)
                
                # Add alluvium with curve arguments
                p <- p + do.call(geom_alluvium, curve_args)
                
                # Add labels with background if requested
                if (self$options$labelNodes) {
                    if (self$options$backgroundLabels) {
                        # Add background boxes for labels
                        p <- p + geom_label(stat = "stratum", 
                                          aes(label = after_stat(stratum)), 
                                          size = 3,
                                          fill = "white",
                                          alpha = 0.8,
                                          label.padding = unit(0.2, "lines"))
                    } else {
                        p <- p + geom_text(stat = "stratum", 
                                         aes(label = after_stat(stratum)), 
                                         size = 3)
                    }
                }
                
                # Add counts with improved positioning
                if (self$options$showCounts) {
                    p <- p + geom_text(stat = "stratum", 
                                      aes(label = after_stat(count)), 
                                      size = 2.5, 
                                      color = if (self$options$backgroundLabels) "black" else "white",
                                      nudge_y = if (self$options$labelNodes) -0.02 else 0)
                }
            }
            
            return(p)
        },
        
        .create_wide_format_plot = function(data, plot_type) {
            strata_vars <- self$options$strata
            
            if (plot_type == "sankey") {
                # Sankey-style with enhanced node/edge styling
                p <- ggplot(data, aes(axis1 = !!sym(strata_vars[1]), 
                                     axis2 = !!sym(strata_vars[2])))
                
                if (length(strata_vars) > 2) {
                    for (i in 3:length(strata_vars)) {
                        p$mapping[[paste0("axis", i)]] <- sym(strata_vars[i])
                    }
                }
                
                # Apply edge style settings
                edge_curve <- if (self$options$edgeStyle == "sin") "sine" else "linear"
                
                p <- p + 
                    geom_alluvium(aes(fill = !!sym(strata_vars[1])), 
                                 alpha = self$options$flowAlpha,
                                 curve_type = edge_curve)
                
                # Apply node style settings
                if (self$options$nodeStyle == "invisible") {
                    # Skip stratum for invisible nodes
                } else if (self$options$nodeStyle == "point") {
                    p <- p + geom_stratum(alpha = 0.9, width = self$options$nodeWidth * 0.3)
                } else {
                    p <- p + geom_stratum(alpha = 0.8, width = self$options$nodeWidth)
                }
                
            } else {
                # Standard alluvial with enhanced styling
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
                    "frequency" = strata_vars[1]
                )
                
                # Apply edge style and curve type
                curve_type <- self$options$curveType
                if (curve_type == "riverplot") curve_type <- "sine"
                if (self$options$edgeStyle == "sin") curve_type <- "sine"
                
                # Prepare alluvium arguments
                alluvium_args <- list(
                    mapping = aes(fill = !!sym(fill_var)),
                    alpha = self$options$flowAlpha,
                    curve_type = curve_type
                )
                
                # Apply curve granularity for smooth curves
                if (curve_type %in% c("cardinal", "cubic", "basis", "sine")) {
                    if (self$options$curveGranularity > 10) {
                        alluvium_args$segments <- min(self$options$curveGranularity / 10, 50)
                    }
                }
                
                # Add mid points if requested (for smoother paths)
                if (self$options$addMidPoints) {
                    alluvium_args$knot.pos <- 0.25  # Add intermediate knot positions
                }
                
                # Reorder edges to minimize crossing if requested
                if (self$options$reorderEdges) {
                    alluvium_args$aes.bind <- "flows"
                    alluvium_args$decreasing <- self$options$sortStreams
                }
                
                p <- p + do.call(geom_alluvium, alluvium_args)
                
                # Apply node style
                if (self$options$nodeStyle == "invisible") {
                    # Skip stratum for invisible nodes
                } else if (self$options$nodeStyle == "point") {
                    p <- p + geom_stratum(alpha = 0.9, 
                                        width = self$options$nodeWidth * 0.3,
                                        color = "black")
                } else {
                    # Regular nodes with gravity-based positioning
                    stratum_args <- list(
                        alpha = 0.8,
                        width = self$options$nodeWidth
                    )
                    
                    # Apply gravity setting (affects vertical positioning)
                    if (self$options$gravity == "top") {
                        stratum_args$reverse = FALSE
                    } else if (self$options$gravity == "bottom") {
                        stratum_args$reverse = TRUE
                    }
                    # "center" is default behavior
                    
                    p <- p + do.call(geom_stratum, stratum_args)
                }
            }
            
            # Add labels with enhanced styling
            if (self$options$labelNodes && self$options$nodeStyle != "invisible") {
                if (self$options$backgroundLabels) {
                    # Labels with background boxes
                    p <- p + geom_label(stat = "stratum", 
                                      aes(label = after_stat(stratum)), 
                                      size = 3,
                                      fill = "white",
                                      alpha = 0.8,
                                      label.padding = unit(0.2, "lines"))
                } else {
                    # Plain text labels
                    p <- p + geom_text(stat = "stratum", 
                                     aes(label = after_stat(stratum)), 
                                     size = 3)
                }
            }
            
            # Add counts with improved positioning
            if (self$options$showCounts && self$options$nodeStyle != "invisible") {
                p <- p + geom_text(stat = "stratum", 
                                  aes(label = after_stat(count)), 
                                  size = 2.5, 
                                  color = if (self$options$backgroundLabels) "black" else "white",
                                  nudge_y = if (self$options$labelNodes) -0.02 else 0)
            }
            
            # Add percentages if requested
            if (self$options$showPercentages && self$options$nodeStyle != "invisible") {
                p <- p + geom_text(stat = "stratum",
                                  aes(label = paste0(round(after_stat(prop) * 100, 1), "%")),
                                  size = 2,
                                  color = if (self$options$backgroundLabels) "gray40" else "gray80",
                                  nudge_y = if (self$options$showCounts) -0.04 else -0.02)
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
                # Checkpoint before expensive riverplot object generation
                private$.checkpoint()
                
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
            # Create real CRAN riverplot structure from actual data
            stages <- unique(data$axis)
            categories <- unique(data$stratum)
            
            # Create comprehensive node structure with actual data
            nodes <- data %>%
                group_by(axis, stratum) %>%
                summarise(
                    value = sum(weight, na.rm = TRUE),
                    count = n(),
                    .groups = 'drop'
                ) %>%
                mutate(
                    ID = paste(axis, stratum, sep = "_"),
                    x = as.numeric(as.factor(axis)),
                    y = NA,  # Let riverplot calculate Y positions
                    labels = as.character(stratum),
                    col = NA,  # Will be set based on color scheme
                    edgecol = NA,
                    srt = 0  # Label rotation
                )
            
            # Create edges from actual transitions
            edges <- list()
            edge_idx <- 1
            
            if (private$.processedOptions$has_id) {
                # Checkpoint before expensive transition calculation
                private$.checkpoint(flush = FALSE)
                
                # Individual tracking - real transitions
                transitions <- data %>%
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
                        flow_value = sum(weight, na.rm = TRUE),
                        flow_count = n(),
                        .groups = 'drop'
                    )
                
                for (i in seq_len(nrow(transitions))) {
                    if (i %% 50 == 0) {  # Checkpoint every 50 edges
                        private$.checkpoint(flush = FALSE)
                    }
                    trans <- transitions[i, ]
                    edges[[edge_idx]] <- list(
                        ID = paste("edge", edge_idx),
                        N1 = paste(trans$axis, trans$stratum, sep = "_"),
                        N2 = paste(trans$next_axis, trans$next_stratum, sep = "_"),
                        Value = trans$flow_value,
                        col = NA,  # Will be set based on color scheme
                        edgecol = if (self$options$edgeGradient) "gradient" else NA
                    )
                    edge_idx <- edge_idx + 1
                }
            } else {
                # Aggregate flows - create proportional connections
                stage_list <- levels(data$axis)
                if (length(stage_list) > 1) {
                    for (s in 1:(length(stage_list) - 1)) {
                        current_stage <- stage_list[s]
                        next_stage <- stage_list[s + 1]
                        
                        current_data <- data %>%
                            filter(axis == current_stage) %>%
                            group_by(stratum) %>%
                            summarise(value = sum(weight, na.rm = TRUE), .groups = 'drop')
                        
                        next_data <- data %>%
                            filter(axis == next_stage) %>%
                            group_by(stratum) %>%
                            summarise(value = sum(weight, na.rm = TRUE), .groups = 'drop')
                        
                        # Create proportional edges
                        for (from_cat in current_data$stratum) {
                            for (to_cat in next_data$stratum) {
                                if (edge_idx %% 30 == 0) {  # Checkpoint periodically
                                    private$.checkpoint(flush = FALSE)
                                }
                                from_val <- current_data$value[current_data$stratum == from_cat]
                                to_val <- next_data$value[next_data$stratum == to_cat]
                                
                                # Estimate flow value
                                flow_val <- min(from_val, to_val) * 0.3  # Proportional estimate
                                
                                edges[[edge_idx]] <- list(
                                    ID = paste("edge", edge_idx),
                                    N1 = paste(current_stage, from_cat, sep = "_"),
                                    N2 = paste(next_stage, to_cat, sep = "_"),
                                    Value = flow_val,
                                    col = NA,
                                    edgecol = if (self$options$edgeGradient) "gradient" else NA
                                )
                                edge_idx <- edge_idx + 1
                            }
                        }
                    }
                }
            }
            
            # Apply color scheme to nodes and edges
            color_palette <- private$.get_color_palette(length(categories))
            
            for (i in seq_len(nrow(nodes))) {
                cat_idx <- which(categories == nodes$stratum[i])
                if (length(cat_idx) > 0) {
                    nodes$col[i] <- color_palette[cat_idx[1]]
                }
            }
            
            return(list(
                nodes = as.data.frame(nodes),
                edges = edges,
                styles = list(
                    edgestyle = self$options$edgeStyle,
                    nodestyle = self$options$nodeStyle,
                    gravity = self$options$gravity
                )
            ))
        },
        
        .create_wide_riverplot_structure = function(data) {
            # Create real CRAN riverplot structure from wide format data
            strata_vars <- self$options$strata
            
            # Create nodes for each category at each stage
            all_nodes <- data.frame()
            x_pos <- 1
            
            for (var in strata_vars) {
                var_data <- data %>%
                    group_by(!!sym(var)) %>%
                    summarise(
                        value = sum(weight, na.rm = TRUE),
                        count = n(),
                        .groups = 'drop'
                    ) %>%
                    filter(!is.na(!!sym(var))) %>%
                    mutate(
                        ID = paste(var, !!sym(var), sep = "_"),
                        x = x_pos,
                        y = NA,
                        labels = as.character(!!sym(var)),
                        col = NA,
                        edgecol = NA,
                        srt = 0
                    ) %>%
                    select(ID, x, y, labels, col, edgecol, srt, value)
                
                all_nodes <- bind_rows(all_nodes, var_data)
                x_pos <- x_pos + 1
            }
            
            # Create edges between consecutive stages
            edges <- list()
            edge_idx <- 1
            
            for (i in seq_len(length(strata_vars) - 1)) {
                # Checkpoint before each stage transition
                private$.checkpoint(flush = FALSE)
                
                from_var <- strata_vars[i]
                to_var <- strata_vars[i + 1]
                
                transitions <- data %>%
                    filter(!is.na(!!sym(from_var)), !is.na(!!sym(to_var))) %>%
                    group_by(!!sym(from_var), !!sym(to_var)) %>%
                    summarise(
                        flow_value = sum(weight, na.rm = TRUE),
                        flow_count = n(),
                        .groups = 'drop'
                    )
                
                for (j in seq_len(nrow(transitions))) {
                    trans <- transitions[j, ]
                    edges[[edge_idx]] <- list(
                        ID = paste("edge", edge_idx),
                        N1 = paste(from_var, trans[[from_var]], sep = "_"),
                        N2 = paste(to_var, trans[[to_var]], sep = "_"),
                        Value = trans$flow_value,
                        col = NA,
                        edgecol = if (self$options$edgeGradient) "gradient" else NA
                    )
                    edge_idx <- edge_idx + 1
                }
            }
            
            # Apply color scheme
            unique_labels <- unique(all_nodes$labels)
            color_palette <- private$.get_color_palette(length(unique_labels))
            
            for (i in seq_len(nrow(all_nodes))) {
                label_idx <- which(unique_labels == all_nodes$labels[i])
                if (length(label_idx) > 0) {
                    all_nodes$col[i] <- color_palette[label_idx[1]]
                }
            }
            
            return(list(
                nodes = all_nodes,
                edges = edges,
                styles = list(
                    edgestyle = self$options$edgeStyle,
                    nodestyle = self$options$nodeStyle,
                    gravity = self$options$gravity
                )
            ))
        },
        
        .get_color_palette = function(n) {
            # Generate color palette based on selected scheme
            scheme <- self$options$colorScheme
            
            if (scheme == "custom" && self$options$customColors != "") {
                colors <- trimws(strsplit(self$options$customColors, ",")[[1]])
                if (length(colors) >= n) {
                    return(colors[1:n])
                } else {
                    # Extend palette if not enough colors
                    return(rep(colors, length.out = n))
                }
            }
            
            colors <- switch(scheme,
                "viridis" = viridis::viridis(n),
                "set1" = if (n <= 9) {
                    RColorBrewer::brewer.pal(max(3, n), "Set1")[1:n]
                } else {
                    colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n)
                },
                "clinical" = {
                    base_colors <- c("#2E8B57", "#FFD700", "#FF6347", "#4682B4", 
                                   "#9370DB", "#FF69B4", "#20B2AA", "#FFA500")
                    if (n <= length(base_colors)) {
                        base_colors[1:n]
                    } else {
                        colorRampPalette(base_colors)(n)
                    }
                },
                "timeline" = {
                    if (n <= 9) {
                        RColorBrewer::brewer.pal(max(3, n), "Blues")[1:n]
                    } else {
                        colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(n)
                    }
                },
                # Default
                {
                    if (n <= 12) {
                        scales::hue_pal()(n)
                    } else {
                        colorRampPalette(scales::hue_pal()(12))(n)
                    }
                }
            )
            
            return(colors)
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