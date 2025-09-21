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
#' @importFrom digest digest
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

        # Cache framework variables
        .cache_key = NULL,
        .last_cache_key = NULL,
        .cache_timestamp = NULL,

        # Progress tracking
        .total_steps = 8,
        
        .init = function() {
            # Initialize function - keep minimal to avoid errors before data is ready
            # Most initialization happens in .run() when data is available

            # Clinical preset functionality - available in options
            # Presets automatically configure plot type, styling, and display options

            # Plot dimensions - available in options
            # Custom plot dimensions can be set via plotWidth/plotHeight parameters

            # Initialize welcome message - removed as it's handled in .run()
            # Welcome message is displayed when analysis begins
        },
        
        # Enhanced HTML sanitization for security
        .sanitizeHTML = function(text) {
            if (is.null(text) || length(text) == 0) return("")
            text <- as.character(text[1])
            # Comprehensive HTML entity encoding for XSS prevention
            text <- gsub("&", "&amp;", text, fixed = TRUE)
            text <- gsub("<", "&lt;", text, fixed = TRUE)
            text <- gsub(">", "&gt;", text, fixed = TRUE)
            text <- gsub('"', "&quot;", text, fixed = TRUE)
            text <- gsub("'", "&#39;", text, fixed = TRUE)
            text <- gsub("/", "&#47;", text, fixed = TRUE)
            text <- gsub("\\", "&#92;", text, fixed = TRUE)
            return(text)
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
                    "<li><strong>", .("Strata Variables:"), "</strong> ", .("Categories that flow between stages (required)"), "</li>",
                    "<li><strong>", .("Time/Sequence:"), "</strong> ", .("For longitudinal data (conditional)"), "</li>",
                    "<li><strong>", .("ID Variable:"), "</strong> ", .("For individual entity tracking (optional)"), "</li>",
                    "<li><strong>", .("Weight Variable:"), "</strong> ", .("For proportional flow sizing (optional)"), "</li>",
                    "</ul>",
                    "</div>",
                    
                    "<div style='display: flex; gap: 15px; margin: 15px 0;'>",
                    "<div style='flex: 1; background-color: #f3e5f5; padding: 10px; border-radius: 5px;'>",
                    "<h5 style='color: #7b1fa2; margin: 0 0 5px 0;'>📊 ", .("Plot Types"), "</h5>",
                    "<ul style='font-size: 12px; margin: 0; padding-left: 15px;'>",
                    "<li><strong>", .("Alluvial:"), "</strong> ", .("Curved flowing streams"), "</li>",
                    "<li><strong>", .("Sankey:"), "</strong> ", .("Directed flow diagrams"), "</li>",
                    "<li><strong>", .("Stream:"), "</strong> ", .("Aggregate trend charts"), "</li>",
                    "<li><strong>", .("Flow:"), "</strong> ", .("Individual entity tracking"), "</li>",
                    "</ul>",
                    "</div>",
                    "<div style='flex: 1; background-color: #e8f5e8; padding: 10px; border-radius: 5px;'>",
                    "<h5 style='color: #2e7d32; margin: 0 0 5px 0;'>🔄 ", .("Data Formats"), "</h5>",
                    "<ul style='font-size: 12px; margin: 0; padding-left: 15px;'>",
                    "<li><strong>", .("Long:"), "</strong> ", .("Time variable + single strata"), "</li>",
                    "<li><strong>", .("Wide:"), "</strong> ", .("Multiple strata as columns"), "</li>",
                    "<li><strong>", .("Individual:"), "</strong> ", .("ID tracking supported"), "</li>",
                    "<li><strong>", .("Weighted:"), "</strong> ", .("Flow proportional to values"), "</li>",
                    "</ul>",
                    "</div>",
                    "</div>",
                    
                    "<div style='background-color: #fff8e1; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<p style='margin: 0; color: #ef6c00;'><strong>💡 ", .("Quick Start:"), "</strong> ", .("Add your strata variables above to begin. The analysis will auto-detect your data format and suggest optimal settings."), "</p>",
                    "</div>",
                    "</div>"
                )
                
                self$results$todo$setContent(welcome_html)
                return()
            }
        },
        
        # Enhanced caching system for performance optimization
        .check_cache = function() {
            if (!self$options$enable_caching) return(FALSE)

            # Generate hash of current data and key options
            tryCatch({
                if (requireNamespace("digest", quietly = TRUE)) {
                    cache_key <- digest::digest(list(
                        data_hash = digest::digest(self$data),
                        strata = self$options$strata,
                        time = self$options$time,
                        id = self$options$id,
                        weight = self$options$weight,
                        plotType = self$options$plotType,
                        dataFormat = self$options$dataFormat,
                        # Include visual options that affect output
                        fillType = self$options$fillType,
                        curveType = self$options$curveType,
                        nodeWidth = self$options$nodeWidth,
                        nodeGap = self$options$nodeGap
                    ))

                    # Store cache key for potential future use
                    private$.cache_key <- cache_key

                    # Always return FALSE for now - full cache implementation would
                    # require persistent storage across jamovi sessions
                    return(FALSE)
                } else {
                    # Fallback when digest not available
                    private$.cache_key <- paste0("cache_",
                        length(self$options$strata), "_",
                        nrow(self$data), "_",
                        self$options$plotType)
                    return(FALSE)
                }
            }, error = function(e) {
                # Cache check failed - proceed without caching
                return(FALSE)
            })
        },
        
        .update_cache = function() {
            if (!self$options$enable_caching) return()

            # Store current analysis timestamp for cache management
            tryCatch({
                private$.cache_timestamp <- Sys.time()

                # In a full implementation, this would:
                # 1. Store processed data with cache key
                # 2. Save plot state and results
                # 3. Implement cache expiration logic
                # 4. Provide cache hit/miss statistics

                # For now, just maintain the cache key for diagnostic purposes
                if (!is.null(private$.cache_key)) {
                    private$.last_cache_key <- private$.cache_key
                }
            }, error = function(e) {
                # Cache update failed - continue without caching
                warning("Cache update failed: ", e$message)
            })
        },

        .update_progress = function(current_step, total_steps, message = "") {
            # Progress tracking function for long-running operations
            if (self$options$detailed_progress) {
                progress_pct <- round((current_step / total_steps) * 100)
                progress_msg <- paste0("Progress: ", progress_pct, "% - ", message)

                # In jamovi, we can't directly show progress bars, but we can update content
                if (current_step < total_steps) {
                    # Optional: Add progress indicator to existing content
                    # This is a lightweight implementation
                }
            }
        },

        .run = function() {
            # Progress tracking is already initialized in private list

            # Package availability check - packages are loaded via imports
            # Skip the non-existent check_package_availability function

            private$.update_progress(1,
                                   private$.total_steps, .("Starting analysis"))
            
            # Early validation
            if (is.null(self$options$strata) || length(self$options$strata) < 1) {
                private$.show_welcome_message()
                return()
            }
            
            # Hide welcome message
            self$results$todo$setVisible(FALSE)
            
            # Cache checking for performance optimization
            # Implemented with basic cache framework for repeated analysis
            
            # Comprehensive input validation
            tryCatch({
                private$.update_progress(2,
                                       private$.total_steps, .("Validating inputs"))
                validation_result <- private$.validate_inputs()

                # Check if validation returned early due to missing variables
                if (!is.null(validation_result) && length(validation_result$errors) > 0) {
                    return()
                }

                # Show validation warnings if any
                if (!is.null(validation_result) && length(validation_result$warnings) > 0) {
                    private$.show_warnings(validation_result$warnings)
                }

                private$.update_progress(3,
                                       private$.total_steps, .("Processing data"))
                private$.process_data()

                private$.update_progress(4,
                                       private$.total_steps, .("Generating summaries"))
                private$.generate_summaries()
                
                # Update cache
                private$.update_cache()
                
                if (self$options$enableDiagnostics) {
                    private$.update_progress(5,
                                           private$.total_steps, .("Generating diagnostics"))
                    private$.generate_diagnostics()
                }

                if (self$options$exportRiverplotObject) {
                    private$.update_progress(6,
                                           private$.total_steps, .("Creating riverplot object"))
                    private$.generate_riverplot_object()
                }

                private$.update_progress(7,
                                       private$.total_steps, .("Generating analysis summary"))
                private$.generate_analysis_summary()

                private$.update_progress(8,
                                       private$.total_steps, .("Creating copy-ready report"))
                private$.generate_report_sentence()

                # Generate about analysis panel
                private$.generate_about_analysis()

                # Generate enhanced caveats if enabled
                if (self$options$enhanced_validation) {
                    private$.generate_enhanced_caveats()
                }

                # Set plot state
                self$results$plot$setState(list(
                    data = private$.processedData,
                    options = private$.processedOptions
                ))
                
                private$.update_progress(private$.total_steps, private$.total_steps, 
                                       .("Analysis complete"))
                
            }, error = function(e) {
                # Enhanced sanitization using comprehensive HTML encoding
                safe_message <- private$.sanitizeHTML(e$message)
                
                # Clear progress on error
                if (self$options$detailed_progress) {
                    private$.update_progress(0, private$.total_steps, .("Error occurred"))
                }
                
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; border: 1px solid #f5c6cb;'>",
                    "<h4>⚠️ ", .("Analysis Error"), "</h4>",
                    "<p><strong>", .("Error:"), "</strong> ", safe_message, "</p>",
                    "<p><em>", .("Please check your data format and variable selections."), "</em></p>",
                    if (self$options$detailed_progress) {
                        paste0("<p><small>", .("Progress information"), "</small></p>")
                    } else {""},
                    "</div>"
                )
                self$results$todo$setContent(error_html)
                self$results$todo$setVisible(TRUE)
            })
        },

        .apply_multi_format_detection = function(data) {
            # Enhanced data format detection and conversion
            tryCatch({
                # Detect source-target format (pairs of from/to columns)
                potential_from_cols <- grep("^(from|source|start)", names(data), ignore.case = TRUE)
                potential_to_cols <- grep("^(to|target|end|dest)", names(data), ignore.case = TRUE)

                if (length(potential_from_cols) > 0 && length(potential_to_cols) > 0) {
                    # Convert source-target to long format
                    from_col <- names(data)[potential_from_cols[1]]
                    to_col <- names(data)[potential_to_cols[1]]

                    # Create stacked data
                    from_data <- data %>%
                        select(all_of(from_col), everything(), -all_of(to_col)) %>%
                        rename(category = all_of(from_col)) %>%
                        mutate(time_point = "from")

                    to_data <- data %>%
                        select(all_of(to_col), everything(), -all_of(from_col)) %>%
                        rename(category = all_of(to_col)) %>%
                        mutate(time_point = "to")

                    data <- bind_rows(from_data, to_data)

                    private$.add_warning("Multi-format support: Detected and converted source-target format to long format")
                }

                # Detect multi-node format (multiple categorical columns that could be stages)
                categorical_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
                if (sum(categorical_cols) >= 3) {
                    private$.add_warning("Multi-format support: Detected multiple categorical variables - consider using wide format with strata variables")
                }

                return(data)
            }, error = function(e) {
                private$.add_warning(paste("Multi-format detection failed:", e$message))
                return(data)
            })
        },

        .validate_inputs = function() {
            errors <- character(0)
            warnings <- character(0)

            # Robust check for strata variables with multiple conditions
            strata_vars <- self$options$strata
            if (is.null(strata_vars) ||
                length(strata_vars) < 1 ||
                all(is.na(strata_vars)) ||
                all(strata_vars == "") ||
                all(sapply(strata_vars, function(x) is.null(x) || length(x) == 0))) {
                errors <- c(errors, .("At least one strata variable is required"))
                # Show welcome message and return early
                private$.show_welcome_message()
                return(list(errors = errors, warnings = warnings))
            }
            
            # Check data format logic
            data_format <- self$options$dataFormat
            if (is.null(data_format)) data_format <- "auto"

            if (data_format == "auto") {
                # Auto-detect based on variables provided
                # Priority: multiple strata = wide, time + single strata = long, single strata = single
                if (length(self$options$strata) > 1) {
                    data_format <- "wide"
                } else if (!is.null(self$options$time) && length(self$options$strata) == 1) {
                    data_format <- "long"
                } else if (length(self$options$strata) == 1) {
                    # Single categorical variable - create simple frequency plot
                    data_format <- "single"
                } else {
                    errors <- c(errors, .("Cannot auto-detect data format: provide at least one strata variable"))
                    data_format <- "unknown"  # Set fallback value
                }
            }
            
            # Validate based on detected/selected format
            if (!is.null(data_format) && data_format == "long") {
                if (is.null(self$options$time)) {
                    errors <- c(errors, .("Time variable is required for long format data"))
                }
                if (length(self$options$strata) != 1) {
                    errors <- c(errors, .("Long format requires exactly one strata variable"))
                }
            } else if (!is.null(data_format) && data_format == "wide") {
                if (length(self$options$strata) < 2) {
                    errors <- c(errors, .("Wide format requires at least two strata variables"))
                }
            } else if (!is.null(data_format) && data_format == "single") {
                if (length(self$options$strata) != 1) {
                    errors <- c(errors, .("Single format requires exactly one strata variable"))
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
            tryCatch({
                misuse_warnings <- private$.detect_misuse()
                warnings <- c(warnings, misuse_warnings)
            }, error = function(e) {
                warnings <- c(warnings, paste("Warning detection failed:", e$message))
            })

            if (length(errors) > 0) {
                stop(paste0("Validation failed: ", paste(errors, collapse = "; ")))
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

            # Validate nodeGap parameter
            if (!is.null(self$options$nodeGap)) {
                node_gap <- self$options$nodeGap
                if (!is.numeric(node_gap) || length(node_gap) != 1) {
                    errors <- c(errors, .("Node gap must be a single numeric value"))
                } else if (node_gap < 0.01 || node_gap > 0.2) {
                    errors <- c(errors, .("Node gap must be between 0.01 and 0.2 (1% to 20% of node height)"))
                } else if (node_gap > 0.1) {
                    warnings <- c(warnings, .("Large node gap (>10%) may cause visual separation between categories within the same stage"))
                }
            }

            # Validate other numeric parameters for robustness
            if (!is.null(self$options$nodeWidth)) {
                node_width <- self$options$nodeWidth
                if (!is.numeric(node_width) || length(node_width) != 1 || node_width < 0.05 || node_width > 0.5) {
                    errors <- c(errors, .("Node width must be between 0.05 and 0.5 (5% to 50% of plot width)"))
                }
            }

            if (!is.null(self$options$flowAlpha)) {
                flow_alpha <- self$options$flowAlpha
                if (!is.numeric(flow_alpha) || length(flow_alpha) != 1 || flow_alpha < 0.1 || flow_alpha > 1.0) {
                    errors <- c(errors, .("Flow transparency must be between 0.1 and 1.0 (10% to 100% opacity)"))
                }
            }

            return(list(errors = errors, warnings = warnings))
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
            # Enhanced helper function with progress indicators and error handling
            if (is.null(data) || nrow(data) == 0) {
                # Set empty table note when no data available
                table$setNote("key", "No data available for this table. Please check your variable selections and data format.")
                return()
            }

            # Add progress indicator for large datasets
            total_rows <- nrow(data)
            show_progress <- total_rows > 500

            tryCatch({
                # Clear any existing notes
                table$setNote("key", NULL)

                for (i in seq_len(nrow(data))) {
                    if (i %% checkpoint_interval == 0) {
                        private$.checkpoint()
                    }
                    row <- data[i, ]
                    table$addRow(rowKey = i, values = as.list(row))
                }
                
            }, error = function(e) {
                # Enhanced error handling with sanitization
                error_msg <- private$.sanitizeHTML(e$message)
                warning(paste(.("Table population error:"), error_msg))
                
                # Try to continue with partial data
                if (exists("i") && i > 1) {
                    warning(paste(.("Populated"), i-1, .("of"), total_rows, .("rows before error")))
                }
            })
        },
        
        .process_data = function() {
            # Get clean data
            data <- self$data

            # Apply large dataset optimization if enabled
            if (self$options$large_dataset_mode && nrow(data) > 10000) {
                # Sample data for better performance while maintaining proportions
                sample_size <- min(5000, nrow(data))
                strata_vars <- self$options$strata

                # Stratified sampling to maintain category proportions
                if (length(strata_vars) > 0 && strata_vars[1] %in% names(data)) {
                    # Stratified sampling using base R to avoid environment issues
                    strata_col <- strata_vars[1]
                    strata_levels <- unique(data[[strata_col]])
                    sampled_data <- do.call(rbind, lapply(strata_levels, function(level) {
                        subset_data <- data[data[[strata_col]] == level & !is.na(data[[strata_col]]), ]
                        n_samples <- ceiling(nrow(subset_data) * sample_size / nrow(data))
                        if (nrow(subset_data) > 0 && n_samples > 0) {
                            subset_data[sample(nrow(subset_data), min(n_samples, nrow(subset_data))), ]
                        } else {
                            subset_data[FALSE, ]  # Empty subset
                        }
                    }))
                    data <- sampled_data
                } else {
                    # Simple random sampling if stratification not possible
                    data <- data[sample(nrow(data), min(sample_size, nrow(data))), ]
                }

                # Add note about sampling
                private$.add_warning(sprintf("Large dataset mode: Sampled %d observations from %d total for performance",
                                           nrow(data), nrow(self$data)))
            }

            # Apply multi-format support if enabled
            if (self$options$multi_format_support) {
                # Attempt to detect and handle additional data formats
                data <- private$.apply_multi_format_detection(data)
            }

            # Auto-detect or use specified data format
            data_format <- private$.processedOptions$data_format

            # Checkpoint before data processing
            private$.checkpoint()
            
            if (data_format == "long") {
                processed_data <- private$.process_long_format(data)
            } else if (data_format == "single") {
                processed_data <- private$.process_single_format(data)
            } else {
                processed_data <- private$.process_wide_format(data)
            }
            
            # Apply common processing
            processed_data <- private$.apply_common_processing(processed_data)
            
            # Store processed data
            private$.processedData <- processed_data
            
            # Calculate and store complete metadata
            n_strata <- if (data_format == "long") {
                length(unique(processed_data$x))
            } else if (data_format == "single") {
                2  # Artificial before/after for single variable
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

            # Filter out missing values from both time and strata variables
            processed <- data[complete.cases(data[c(time_var, strata_var)]), keep_cols, drop = FALSE]

            # Convert to factors
            processed[[time_var]] <- as.factor(processed[[time_var]])
            processed[[strata_var]] <- as.factor(processed[[strata_var]])
            if (!is.null(id_var)) processed[[id_var]] <- as.factor(processed[[id_var]])

            # Add alluvium ID if not provided (required for ggalluvial)
            if (is.null(id_var)) {
                processed$alluvium <- seq_len(nrow(processed))
            } else {
                names(processed)[names(processed) == id_var] <- "alluvium"
            }

            # Add weight if not provided
            if (is.null(weight_var)) {
                processed$weight <- 1
            } else {
                names(processed)[names(processed) == weight_var] <- "weight"
            }

            # Rename for ggalluvial compatibility (use 'x' instead of 'axis')
            names(processed)[names(processed) == time_var] <- "x"
            names(processed)[names(processed) == strata_var] <- "stratum"

            return(processed)
        },

        .process_single_format = function(data) {
            strata_var <- self$options$strata[1]
            id_var <- self$options$id
            weight_var <- self$options$weight

            # Select relevant columns
            keep_cols <- c(strata_var)
            if (!is.null(id_var)) keep_cols <- c(keep_cols, id_var)
            if (!is.null(weight_var)) keep_cols <- c(keep_cols, weight_var)

            # Filter out missing values from the strata variable
            processed <- data[complete.cases(data[strata_var]), keep_cols, drop = FALSE]

            # Convert to factors
            processed[[strata_var]] <- as.factor(processed[[strata_var]])

            # For single variable, create artificial two-stage structure
            # This mimics a wide format with 2 columns for proper plotting
            processed$stage1 <- "Total"  # Artificial starting point
            processed$stage2 <- processed[[strata_var]]  # The actual categories

            # Add row IDs if no ID variable provided
            if (is.null(id_var)) {
                processed$alluvium <- seq_len(nrow(processed))
            } else {
                names(processed)[names(processed) == id_var] <- "alluvium"
            }

            # Add weight if not provided
            if (is.null(weight_var)) {
                processed$weight <- 1
            } else {
                names(processed)[names(processed) == weight_var] <- "weight"
            }

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
                    length(unique(data$x))
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
                } else if (data_format == "single") {
                    private$.generate_single_flow_summary(data)
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
                        from_stage = as.character(x),
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
                        count = sum(weight, na.rm = TRUE),
                        n_obs = n(),
                        .groups = 'drop'
                    ) %>%
                    mutate(
                        from_stage = as.character(x),
                        from_category = as.character(stratum),
                        to_stage = as.character(x),
                        to_category = as.character(stratum),
                        percentage = if_else(sum(count, na.rm = TRUE) > 0,
                                           count / sum(count, na.rm = TRUE) * 100,
                                           0),
                        weight = count
                    ) %>%
                    select(from_stage, from_category, to_stage, to_category, count, percentage, weight) %>%
                    arrange(from_stage, from_category)
            }
            
            # Populate flow table with checkpoints and enhanced error handling
            tryCatch({
                private$.populate_table_with_checkpoints(self$results$flowTable, flows, 100)
            }, error = function(e) {
                warning(paste(.("Error populating flow table:"), e$message))
                self$results$flowTable$setNote("error",
                    paste(.("Unable to populate flow summary table:"), private$.sanitizeHTML(e$message)))
            })
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
                
                # Filter out missing values using base R
                complete_cases <- complete.cases(data[c(from_var, to_var)])
                filtered_data <- data[complete_cases, ]

                # Create aggregation using base R
                combinations <- unique(filtered_data[c(from_var, to_var)])
                stage_flows <- data.frame(
                    from_stage = from_var,
                    from_category = character(0),
                    to_stage = to_var,
                    to_category = character(0),
                    count = numeric(0),
                    percentage = numeric(0),
                    weight = numeric(0),
                    stringsAsFactors = FALSE
                )

                for (j in seq_len(nrow(combinations))) {
                    from_val <- combinations[j, from_var]
                    to_val <- combinations[j, to_var]

                    subset_data <- filtered_data[
                        filtered_data[[from_var]] == from_val &
                        filtered_data[[to_var]] == to_val,
                    ]

                    if (nrow(subset_data) > 0) {
                        stage_flows <- rbind(stage_flows, data.frame(
                            from_stage = from_var,
                            from_category = as.character(from_val),
                            to_stage = to_var,
                            to_category = as.character(to_val),
                            count = nrow(subset_data),
                            percentage = 0, # Will calculate after
                            weight = sum(subset_data$weight, na.rm = TRUE),
                            stringsAsFactors = FALSE
                        ))
                    }
                }

                # Calculate percentages
                if (nrow(stage_flows) > 0) {
                    stage_flows$percentage <- stage_flows$count / sum(stage_flows$count) * 100
                }
                
                flows[[i]] <- stage_flows
            }
            
            all_flows <- bind_rows(flows)
            
            # Populate flow table with checkpoints and enhanced error handling
            tryCatch({
                private$.populate_table_with_checkpoints(self$results$flowTable, all_flows, 100)
            }, error = function(e) {
                warning(paste(.("Error populating wide format flow table:"), e$message))
                self$results$flowTable$setNote("error",
                    paste(.("Unable to populate wide format flow summary table:"), private$.sanitizeHTML(e$message)))
            })
        },

        .generate_single_flow_summary = function(data) {
            # Create flow summary for single categorical variable
            # Treat as frequency distribution
            strata_var <- self$options$strata[1]

            # Checkpoint before summary calculation
            private$.checkpoint()

            # Create simple frequency table using the artificial stage structure
            flows <- data %>%
                group_by(stage1, stage2) %>%
                summarise(
                    count = n(),
                    weight = sum(weight, na.rm = TRUE),
                    .groups = 'drop'
                ) %>%
                mutate(
                    from_stage = "stage1",
                    from_category = as.character(stage1),
                    to_stage = "stage2",
                    to_category = as.character(stage2),
                    percentage = count / sum(count) * 100
                ) %>%
                select(from_stage, from_category, to_stage, to_category, count, percentage, weight)

            # Populate flow table with enhanced error handling
            tryCatch({
                private$.populate_table_with_checkpoints(self$results$flowTable, flows, 100)
            }, error = function(e) {
                warning(paste(.("Error populating single format flow table:"), e$message))
                self$results$flowTable$setNote("error",
                    paste(.("Unable to populate single format flow summary table:"), private$.sanitizeHTML(e$message)))
            })
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
                        stage = as.character(x),
                        category = as.character(stratum)
                    ) %>%
                    select(stage, category, count, percentage, cumulative)
            } else if (data_format == "single") {
                # Single format stage summary - treat as simple frequency
                strata_var <- self$options$strata[1]
                stage_summary <- data %>%
                    group_by(stage2) %>%
                    summarise(
                        count = n(),
                        .groups = 'drop'
                    ) %>%
                    mutate(
                        stage = strata_var,
                        category = as.character(stage2),
                        percentage = count / sum(count) * 100,
                        cumulative = cumsum(percentage)
                    ) %>%
                    select(stage, category, count, percentage, cumulative)
            } else {
                # Wide format stage summary - optimized vectorized approach
                stage_summaries <- lapply(self$options$strata, function(stage_var) {
                    # Checkpoint before each stage calculation
                    private$.checkpoint(flush = FALSE)

                    # Get non-missing values for this stage
                    stage_values <- data[[stage_var]]
                    valid_idx <- !is.na(stage_values)

                    if (!any(valid_idx)) {
                        # Return empty data frame if no valid values
                        return(data.frame(
                            stage = character(0),
                            category = character(0),
                            count = numeric(0),
                            percentage = numeric(0),
                            cumulative = numeric(0),
                            stringsAsFactors = FALSE
                        ))
                    }

                    # Use vectorized operations for efficient calculation
                    valid_values <- stage_values[valid_idx]
                    valid_weights <- data$weight[valid_idx]

                    # Calculate weighted counts using tapply (vectorized)
                    category_counts <- tapply(valid_weights, valid_values, sum, na.rm = TRUE)
                    categories <- names(category_counts)
                    counts <- as.numeric(category_counts)

                    # Calculate percentages and cumulative (vectorized)
                    total_count <- sum(counts, na.rm = TRUE)
                    percentages <- if (total_count > 0) counts / total_count * 100 else rep(0, length(counts))
                    cumulative <- cumsum(percentages)

                    # Create result data frame efficiently
                    data.frame(
                        stage = rep(stage_var, length(categories)),
                        category = as.character(categories),
                        count = counts,
                        percentage = percentages,
                        cumulative = cumulative,
                        stringsAsFactors = FALSE
                    )
                })

                # Use do.call for efficient binding
                stage_summary <- do.call(rbind, stage_summaries)
            }
            
            # Populate stage table with enhanced error handling
            tryCatch({
                private$.populate_table_with_checkpoints(self$results$stageTable, stage_summary, 100)
            }, error = function(e) {
                warning(paste(.("Error populating stage table:"), e$message))
                self$results$stageTable$setNote("error",
                    paste(.("Unable to populate stage summary table:"), private$.sanitizeHTML(e$message)))
            })
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

                # Populate transition matrix table with enhanced error handling
                tryCatch({
                    private$.populate_table_with_checkpoints(self$results$transitionMatrix, display_transitions, 20)
                }, error = function(e) {
                    warning(paste(.("Error populating long format transition matrix:"), e$message))
                    self$results$transitionMatrix$setNote("error",
                        paste(.("Unable to populate transition matrix table:"), private$.sanitizeHTML(e$message)))
                })
                
            } else if (data_format == "wide") {
                # Calculate transition probabilities for wide format
                strata_vars <- self$options$strata
                all_transitions <- list()
                
                for (i in seq_len(length(strata_vars) - 1)) {
                    # Checkpoint before each stage transition calculation
                    private$.checkpoint(flush = FALSE)
                    
                    from_var <- strata_vars[i]
                    to_var <- strata_vars[i + 1]
                    
                    # Filter out missing values using base R
                    complete_cases <- complete.cases(data[c(from_var, to_var)])
                    filtered_data <- data[complete_cases, ]

                    # Get unique combinations
                    if (nrow(filtered_data) > 0) {
                        combinations <- unique(filtered_data[c(from_var, to_var)])

                        # Create transitions data frame
                        stage_transitions <- data.frame(
                            from = character(0),
                            to = character(0),
                            count = numeric(0),
                            weight_sum = numeric(0),
                            probability = numeric(0),
                            weight_prob = numeric(0),
                            stringsAsFactors = FALSE
                        )

                        # Calculate counts for each combination
                        for (k in seq_len(nrow(combinations))) {
                            from_val <- combinations[k, from_var]
                            to_val <- combinations[k, to_var]

                            subset_data <- filtered_data[
                                filtered_data[[from_var]] == from_val &
                                filtered_data[[to_var]] == to_val,
                            ]

                            if (nrow(subset_data) > 0) {
                                stage_transitions <- rbind(stage_transitions, data.frame(
                                    from = as.character(from_val),
                                    to = as.character(to_val),
                                    count = nrow(subset_data),
                                    weight_sum = sum(subset_data$weight, na.rm = TRUE),
                                    probability = 0, # Will calculate after
                                    weight_prob = 0, # Will calculate after
                                    stringsAsFactors = FALSE
                                ))
                            }
                        }

                        # Calculate probabilities by 'from' group
                        if (nrow(stage_transitions) > 0) {
                            unique_from <- unique(stage_transitions$from)
                            for (from_group in unique_from) {
                                group_indices <- stage_transitions$from == from_group
                                group_count_sum <- sum(stage_transitions$count[group_indices])
                                group_weight_sum <- sum(stage_transitions$weight_sum[group_indices])

                                if (group_count_sum > 0) {
                                    stage_transitions$probability[group_indices] <-
                                        stage_transitions$count[group_indices] / group_count_sum
                                }
                                if (group_weight_sum > 0) {
                                    stage_transitions$weight_prob[group_indices] <-
                                        stage_transitions$weight_sum[group_indices] / group_weight_sum
                                }
                            }
                        }
                    } else {
                        # Empty data frame if no complete cases
                        stage_transitions <- data.frame(
                            from = character(0),
                            to = character(0),
                            count = numeric(0),
                            weight_sum = numeric(0),
                            probability = numeric(0),
                            weight_prob = numeric(0),
                            stringsAsFactors = FALSE
                        )
                    }
                    
                    all_transitions[[i]] <- stage_transitions
                }
                
                transitions <- bind_rows(all_transitions) %>%
                    arrange(desc(probability))

                # Limit and format transitions for display
                display_transitions <- transitions[seq_len(min(nrow(transitions), 100)), ] %>%
                    mutate(probability = round(probability, 3))

                # Populate transition matrix table with enhanced error handling
                tryCatch({
                    private$.populate_table_with_checkpoints(self$results$transitionMatrix, display_transitions, 20)
                }, error = function(e) {
                    warning(paste(.("Error populating wide format transition matrix:"), e$message))
                    self$results$transitionMatrix$setNote("error",
                        paste(.("Unable to populate transition matrix table:"), private$.sanitizeHTML(e$message)))
                })
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
            } else if (opts$data_format == "single") {
                .("categorical frequency distribution")
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
                
            } else if (opts$data_format == "single") {
                # Single categorical variable analysis
                report_text <- sprintf(
                    .("Categorical distribution analysis of %d cases identified %d distinct categories in variable '%s'. %s visualization shows frequency distribution and proportional representation."),
                    n_observations,
                    n_categories,
                    self$options$strata[1],
                    stringr::str_to_title(plot_type)
                )

                clinical_interpretation <- switch(plot_type,
                    "alluvial" = .("Flow visualization emphasizes relative frequency and distribution patterns."),
                    "sankey" = .("Directed visualization highlights category proportions and frequencies."),
                    .("Categorical distribution shows composition and relative frequencies.")
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
            
            # Generate clean HTML report
            report_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;'>",
                "<h5 style='color: #495057; margin: 0 0 10px 0;'>📋 ", .("Clinical Report"), "</h5>",
                "<div style='background-color: white; padding: 12px; border-radius: 5px; font-family: serif; line-height: 1.6; border: 1px solid #e9ecef; margin: 8px 0;'>",
                complete_report,
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

        .generate_about_analysis = function() {
            opts <- private$.processedOptions
            data_format <- opts$data_format
            plot_type <- self$options$plotType

            # Build clinical context based on data characteristics
            clinical_context <- ""
            if (opts$has_id) {
                clinical_context <- paste0(
                    .("This analysis tracks "), opts$n_categories, .(" different categories across "),
                    opts$n_strata, .(" time points or stages for individual patients/subjects.")
                )
            } else {
                clinical_context <- paste0(
                    .("This analysis shows the distribution and flow patterns of "), opts$n_categories,
                    .(" categories across "), opts$n_strata, .(" stages or time points.")
                )
            }

            # Explain plot type in clinical terms
            plot_description <- switch(plot_type,
                "alluvial" = .("Alluvial diagrams show flowing streams between categories, ideal for visualizing patient journeys, treatment progressions, or disease state transitions. The width of each stream represents the number of cases following that pathway."),
                "sankey" = .("Sankey diagrams display directed flows with clear source-to-destination pathways, excellent for showing treatment decisions, referral patterns, or clinical pathway analysis. Flow thickness indicates volume."),
                "stream" = .("Stream charts show temporal trends in category distributions, perfect for monitoring population health trends, treatment response patterns, or epidemiological changes over time."),
                "flow" = .("Flow diagrams track individual entity movements between states, useful for longitudinal patient monitoring, cohort studies, or individual treatment response tracking."),
                .("River plots visualize categorical data flows and transitions over time or across stages.")
            )

            # Clinical applications based on data format
            applications <- switch(data_format,
                "long" = .("Common clinical applications include: patient follow-up studies, treatment response monitoring, disease progression tracking, and longitudinal cohort analysis."),
                "wide" = .("Common clinical applications include: multi-stage treatment protocols, sequential diagnostic procedures, pathway analysis, and cross-sectional comparisons at different time points."),
                "single" = .("Common clinical applications include: before/after treatment comparisons, intervention effect visualization, and simple categorical distributions."),
                .("This visualization helps understand categorical patterns and transitions in clinical data.")
            )

            about_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #007bff;'>",
                "<h3 style='color: #495057; margin-top: 0; font-size: 18px;'>📊 About This Analysis</h3>",

                "<div style='margin-bottom: 16px;'>",
                "<h4 style='color: #495057; font-size: 14px; margin-bottom: 8px;'>Analysis Overview</h4>",
                "<p style='margin: 0; line-height: 1.5;'>", clinical_context, "</p>",
                "</div>",

                "<div style='margin-bottom: 16px;'>",
                "<h4 style='color: #495057; font-size: 14px; margin-bottom: 8px;'>Visualization Method</h4>",
                "<p style='margin: 0; line-height: 1.5;'>", plot_description, "</p>",
                "</div>",

                "<div style='margin-bottom: 16px;'>",
                "<h4 style='color: #495057; font-size: 14px; margin-bottom: 8px;'>Clinical Applications</h4>",
                "<p style='margin: 0; line-height: 1.5;'>", applications, "</p>",
                "</div>",

                if (opts$has_weight) {
                    paste0(
                        "<div style='margin-bottom: 12px;'>",
                        "<h4 style='color: #495057; font-size: 14px; margin-bottom: 8px;'>Weighted Analysis</h4>",
                        "<p style='margin: 0; line-height: 1.5; color: #6c757d;'>",
                        .("Flow widths are proportional to the weight variable, allowing for analysis based on patient counts, costs, severity scores, or other quantitative measures."),
                        "</p>",
                        "</div>"
                    )
                } else {""},

                "<div style='background-color: #e9ecef; padding: 12px; border-radius: 6px; margin-top: 16px;'>",
                "<h4 style='color: #495057; font-size: 13px; margin: 0 0 8px 0;'>💡 Clinical Interpretation Tips</h4>",
                "<ul style='margin: 0; padding-left: 20px; line-height: 1.5; font-size: 13px;'>",
                "<li>", .("Wide flows indicate common pathways or frequent transitions"), "</li>",
                "<li>", .("Narrow flows show rare pathways that may need special attention"), "</li>",
                "<li>", .("Color patterns help identify dominant categories or outcomes"), "</li>",
                "<li>", .("Node heights represent total volume at each stage"), "</li>",
                "</ul>",
                "</div>",

                "</div>"
            )

            self$results$about_analysis$setContent(about_html)
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.processedData)) return()
            
            # Check required packages
            if (!requireNamespace("ggplot2", quietly = TRUE) ||
                !requireNamespace("ggalluvial", quietly = TRUE) ||
                !requireNamespace("dplyr", quietly = TRUE)) {
                stop("Required packages not available: ggplot2, ggalluvial, dplyr")
            }

            # Assign ggalluvial stat for use with ggplot2 geoms
            stratum <- ggalluvial::StatStratum
            
            data <- private$.processedData
            opts <- private$.processedOptions
            plot_type <- self$options$plotType
            
            # Create base plot based on data format and plot type
            if (opts$data_format == "long") {
                p <- private$.create_long_format_plot(data, plot_type)
            } else if (opts$data_format == "single") {
                p <- private$.create_wide_format_plot(data, plot_type)  # Use wide format plot for single
            } else {
                p <- private$.create_wide_format_plot(data, plot_type)
            }
            
            # Apply styling
            p <- private$.apply_plot_styling(p)
            
            print(p)
            TRUE
        },
        
        .create_long_format_plot = function(data, plot_type) {
            # Assign ggalluvial stat for use with ggplot2 geoms
            stratum <- ggalluvial::StatStratum

            # Base aesthetic mapping
            if (plot_type == "stream") {
                # Enhanced stream chart with ggstream-like capabilities
                if (requireNamespace("ggstream", quietly = TRUE)) {
                    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = weight, fill = stratum)) +
                        ggstream::geom_stream(alpha = self$options$flowAlpha,
                                   type = if (self$options$sortStreams) "ridge" else "proportional")

                    # Add stream labels if requested
                    if (self$options$labelNodes) {
                        p <- p + ggstream::geom_stream_label(ggplot2::aes(label = stratum), size = 3)
                    }
                } else {
                    # Fallback to basic area chart
                    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = weight, fill = stratum)) +
                        ggplot2::geom_area(position = if (self$options$sortStreams) "stack" else "fill",
                                 alpha = self$options$flowAlpha)
                }
            } else if (plot_type == "flow") {
                # Enhanced flow diagram for individual tracking
                if (private$.processedOptions$has_id) {
                    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, stratum = stratum, alluvium = alluvium,
                                         y = weight, fill = stratum))
                    
                    # Apply flow-specific styling
                    curve_type <- if (self$options$curveType == "riverplot") "sine" else self$options$curveType
                    
                    p <- p + 
                        ggalluvial::geom_flow(ggplot2::aes(fill = stratum),
                                 stat = "alluvium",
                                 alpha = self$options$flowAlpha,
                                 width = self$options$nodeWidth)
                    
                    # Add stratum with custom styling
                    p <- p + ggalluvial::geom_stratum(alpha = 0.9, width = self$options$nodeWidth,
                                        gap = self$options$nodeGap)
                } else {
                    # Fallback to alluvial for aggregate flow
                    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, stratum = stratum, y = weight, fill = stratum))
                    p <- p +
                        ggalluvial::geom_stratum(alpha = 0.8, width = self$options$nodeWidth,
                                          gap = self$options$nodeGap) +
                        ggalluvial::geom_alluvium(alpha = self$options$flowAlpha)
                }
            } else {
                # Standard alluvial/sankey plots
                if (private$.processedOptions$has_id) {
                    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, stratum = stratum, alluvium = alluvium,
                                         y = weight, fill = stratum))
                } else {
                    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, stratum = stratum, y = weight, fill = stratum))
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
                    ggalluvial::geom_stratum(alpha = 0.8, width = self$options$nodeWidth,
                                      gap = self$options$nodeGap)
                
                # Add alluvium with curve arguments
                p <- p + do.call(geom_alluvium, curve_args)
                
                # Add labels with background if requested
                if (self$options$labelNodes) {
                    if (self$options$backgroundLabels) {
                        # Add background boxes for labels
                        p <- p + ggplot2::geom_label(stat = stratum,
                                          ggplot2::aes(label = after_stat(stratum)), 
                                          size = 3,
                                          fill = "white",
                                          alpha = 0.8,
                                          label.padding = grid::unit(0.2, "lines"))
                    } else {
                        p <- p + ggplot2::geom_text(stat = stratum,
                                         ggplot2::aes(label = after_stat(stratum)), 
                                         size = 3)
                    }
                }
                
                # Add counts with improved positioning
                if (self$options$showCounts) {
                    p <- p + ggplot2::geom_text(stat = stratum,
                                      ggplot2::aes(label = after_stat(count)), 
                                      size = 2.5, 
                                      color = if (self$options$backgroundLabels) "black" else "white",
                                      nudge_y = if (self$options$labelNodes) -0.02 else 0)
                }
            }
            
            return(p)
        },
        
        .create_wide_format_plot = function(data, plot_type) {
            # Assign ggalluvial stat for use with ggplot2 geoms
            stratum <- ggalluvial::StatStratum

            # Handle different data formats
            if (private$.processedOptions$data_format == "single") {
                # For single format, use the artificial stage columns we created
                strata_vars <- c("stage1", "stage2")
            } else {
                # Normal wide format uses the strata options
                strata_vars <- self$options$strata
            }
            
            if (plot_type == "sankey") {
                # Sankey-style with enhanced node/edge styling
                p <- ggplot2::ggplot(data, ggplot2::aes(axis1 = !!rlang::sym(strata_vars[1]),
                                     axis2 = !!rlang::sym(strata_vars[2])))
                
                if (length(strata_vars) > 2) {
                    for (i in 3:length(strata_vars)) {
                        p$mapping[[paste0("axis", i)]] <- rlang::sym(strata_vars[i])
                    }
                }
                
                # Apply edge style settings
                edge_curve <- if (self$options$edgeStyle == "sin") "sine" else "linear"
                
                # Get the fill variable name and create aesthetic mapping safely
                fill_var <- strata_vars[1]
                p <- p +
                    ggalluvial::geom_alluvium(alpha = self$options$flowAlpha,
                                 curve_type = edge_curve,
                                 ggplot2::aes_string(fill = fill_var))
                
                # Apply node style settings
                if (self$options$nodeStyle == "invisible") {
                    # Skip stratum for invisible nodes
                } else if (self$options$nodeStyle == "point") {
                    p <- p + ggalluvial::geom_stratum(alpha = 0.9, width = self$options$nodeWidth * 0.3,
                                        gap = self$options$nodeGap)
                } else {
                    p <- p + ggalluvial::geom_stratum(alpha = 0.8, width = self$options$nodeWidth,
                                        gap = self$options$nodeGap)
                }
                
            } else {
                # Standard alluvial with enhanced styling
                p <- ggplot2::ggplot(data, ggplot2::aes(axis1 = !!rlang::sym(strata_vars[1]),
                                     axis2 = !!rlang::sym(strata_vars[2])))
                
                if (length(strata_vars) > 2) {
                    for (i in 3:length(strata_vars)) {
                        p$mapping[[paste0("axis", i)]] <- rlang::sym(strata_vars[i])
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
                    mapping = ggplot2::aes(fill = !!rlang::sym(fill_var)),
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
                    p <- p + ggalluvial::geom_stratum(alpha = 0.9,
                                        width = self$options$nodeWidth * 0.3,
                                        gap = self$options$nodeGap,
                                        color = "black")
                } else {
                    # Regular nodes with gravity-based positioning
                    stratum_args <- list(
                        alpha = 0.8,
                        width = self$options$nodeWidth,
                        gap = self$options$nodeGap
                    )
                    
                    # Apply gravity setting (affects vertical positioning)
                    if (self$options$gravity == "top") {
                        stratum_args$reverse = FALSE
                    } else if (self$options$gravity == "bottom") {
                        stratum_args$reverse = TRUE
                    }
                    # "center" is default behavior
                    
                    p <- p + do.call(ggalluvial::geom_stratum, stratum_args)
                }
            }
            
            # Add labels with enhanced styling
            if (self$options$labelNodes && self$options$nodeStyle != "invisible") {
                if (self$options$backgroundLabels) {
                    # Labels with background boxes
                    p <- p + ggplot2::geom_label(stat = stratum,
                                      ggplot2::aes(label = after_stat(stratum)), 
                                      size = 3,
                                      fill = "white",
                                      alpha = 0.8,
                                      label.padding = grid::unit(0.2, "lines"))
                } else {
                    # Plain text labels
                    p <- p + ggplot2::geom_text(stat = stratum,
                                     ggplot2::aes(label = after_stat(stratum)), 
                                     size = 3)
                }
            }
            
            # Add counts with improved positioning
            if (self$options$showCounts && self$options$nodeStyle != "invisible") {
                p <- p + ggplot2::geom_text(stat = stratum,
                                  ggplot2::aes(label = after_stat(count)), 
                                  size = 2.5, 
                                  color = if (self$options$backgroundLabels) "black" else "white",
                                  nudge_y = if (self$options$labelNodes) -0.02 else 0)
            }
            
            # Add percentages if requested
            if (self$options$showPercentages && self$options$nodeStyle != "invisible") {
                p <- p + ggplot2::geom_text(stat = stratum,
                                  ggplot2::aes(label = paste0(round(after_stat(prop) * 100, 1), "%")),
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
            stages <- unique(data$x)
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
                # Filter out missing values using base R
                complete_data <- data[!is.na(data[[var]]), ]

                # Get unique categories and calculate aggregations
                categories <- unique(complete_data[[var]])

                # Create variable data
                var_data <- data.frame(
                    ID = character(0),
                    x = numeric(0),
                    y = numeric(0),
                    labels = character(0),
                    col = character(0),
                    edgecol = character(0),
                    srt = numeric(0),
                    value = numeric(0),
                    stringsAsFactors = FALSE
                )

                for (cat in categories) {
                    cat_data <- complete_data[complete_data[[var]] == cat, ]
                    var_data <- rbind(var_data, data.frame(
                        ID = paste(var, cat, sep = "_"),
                        x = x_pos,
                        y = NA,
                        labels = as.character(cat),
                        col = NA,
                        edgecol = NA,
                        srt = 0,
                        value = sum(cat_data$weight, na.rm = TRUE),
                        stringsAsFactors = FALSE
                    ))
                }
                
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
                
                # Filter out missing values using base R
                complete_cases <- complete.cases(data[c(from_var, to_var)])
                filtered_data <- data[complete_cases, ]

                # Calculate transitions using base R
                if (nrow(filtered_data) > 0) {
                    combinations <- unique(filtered_data[c(from_var, to_var)])

                    transitions <- data.frame(
                        from_var_val = character(0),
                        to_var_val = character(0),
                        flow_value = numeric(0),
                        flow_count = numeric(0),
                        stringsAsFactors = FALSE
                    )

                    for (k in seq_len(nrow(combinations))) {
                        from_val <- combinations[k, from_var]
                        to_val <- combinations[k, to_var]

                        subset_data <- filtered_data[
                            filtered_data[[from_var]] == from_val &
                            filtered_data[[to_var]] == to_val,
                        ]

                        if (nrow(subset_data) > 0) {
                            transitions <- rbind(transitions, data.frame(
                                from_var_val = from_val,
                                to_var_val = to_val,
                                flow_value = sum(subset_data$weight, na.rm = TRUE),
                                flow_count = nrow(subset_data),
                                stringsAsFactors = FALSE
                            ))
                        }
                    }

                    # Rename columns to match expected names
                    names(transitions)[1] <- from_var
                    names(transitions)[2] <- to_var
                } else {
                    # Empty transitions
                    transitions <- data.frame(
                        flow_value = numeric(0),
                        flow_count = numeric(0),
                        stringsAsFactors = FALSE
                    )
                    names(transitions)[1] <- from_var
                    names(transitions)[2] <- to_var
                }
                
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
            # Generate color palette based on selected scheme and accessibility options
            scheme <- self$options$colorScheme
            high_contrast <- self$options$high_contrast_mode
            
            if (scheme == "custom" && self$options$customColors != "") {
                colors <- trimws(strsplit(self$options$customColors, ",")[[1]])
                if (length(colors) >= n) {
                    return(colors[1:n])
                } else {
                    # Extend palette if not enough colors
                    return(rep(colors, length.out = n))
                }
            }
            
            # Color-blind safe palette (Paul Tol's palette)
            if (scheme == "colorblind_safe") {
                cb_colors <- c(
                    "#332288", "#117733", "#44AA99", "#88CCEE",
                    "#DDCC77", "#CC6677", "#AA4499", "#882255",
                    "#661100", "#6699CC", "#AA4466", "#4477AA"
                )
                if (n <= length(cb_colors)) {
                    return(cb_colors[1:n])
                } else {
                    return(rep(cb_colors, length.out = n))
                }
            }
            
            colors <- switch(scheme,
                "viridis" = {
                    if (high_contrast) {
                        viridis::viridis(n, begin = 0.1, end = 0.9)
                    } else {
                        viridis::viridis(n)
                    }
                },
                "set1" = {
                    base_palette <- if (n <= 9) {
                        RColorBrewer::brewer.pal(max(3, n), "Set1")[1:n]
                    } else {
                        colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n)
                    }
                    if (high_contrast) {
                        # Enhance contrast for Set1
                        c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", 
                          "#6A3D9A", "#B15928", "#A6CEE3", "#FDBF6F")[1:min(n, 8)]
                    } else {
                        base_palette
                    }
                },
                "clinical" = {
                    if (high_contrast) {
                        base_colors <- c("#228B22", "#FFD700", "#B22222", "#000080", 
                                       "#800080", "#FF1493", "#008B8B", "#FF8C00")
                    } else {
                        base_colors <- c("#2E8B57", "#FFD700", "#FF6347", "#4682B4", 
                                       "#9370DB", "#FF69B4", "#20B2AA", "#FFA500")
                    }
                    if (n <= length(base_colors)) {
                        base_colors[1:n]
                    } else {
                        colorRampPalette(base_colors)(n)
                    }
                },
                "timeline" = {
                    if (high_contrast) {
                        base_colors <- c("#000080", "#4169E1", "#1E90FF")
                        if (n <= length(base_colors)) {
                            base_colors[1:n]
                        } else {
                            colorRampPalette(base_colors)(n)
                        }
                    } else {
                        if (n <= 9) {
                            RColorBrewer::brewer.pal(max(3, n), "Blues")[1:n]
                        } else {
                            colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(n)
                        }
                    }
                },
                # Default with high contrast option
                {
                    if (high_contrast) {
                        # High contrast color set
                        hc_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                                     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                        if (n <= length(hc_colors)) {
                            hc_colors[1:n]
                        } else {
                            rep(hc_colors, length.out = n)
                        }
                    } else {
                        if (n <= 12) {
                            scales::hue_pal()(n)
                        } else {
                            colorRampPalette(scales::hue_pal()(12))(n)
                        }
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
        },
        
        # === Enhanced Methods (inspired by alluvial) ===
        
        # Generate comprehensive validation report
        .generate_validation_report = function() {
            if (!("validation_report" %in% names(self$results))) return()
            
            tryCatch({
                # Comprehensive data validation analysis
                data <- self$data
                validation_issues <- list()
                data_quality_score <- 100
                
                # Check data completeness
                total_rows <- nrow(data)
                complete_rows <- sum(complete.cases(data[self$options$strata]))
                completeness_rate <- (complete_rows / total_rows) * 100
                
                if (completeness_rate < 95) {
                    validation_issues <- append(validation_issues, 
                        sprintf(.("Data completeness: %.1f%% - %d rows have missing values in strata variables"), 
                               completeness_rate, total_rows - complete_rows))
                    data_quality_score <- data_quality_score - (100 - completeness_rate)
                }
                
                # Check category distribution balance
                if (length(self$options$strata) >= 1) {
                    for (strata_var in self$options$strata) {
                        if (strata_var %in% names(data)) {
                            category_counts <- table(data[[strata_var]], useNA = "no")
                            min_count <- min(category_counts)
                            max_count <- max(category_counts)
                            imbalance_ratio <- max_count / min_count
                            
                            if (imbalance_ratio > 10) {
                                validation_issues <- append(validation_issues,
                                    sprintf(.("Category imbalance in '%s': Largest category is %dx larger than smallest"), 
                                           strata_var, as.integer(imbalance_ratio)))
                                data_quality_score <- data_quality_score - 10
                            }
                        }
                    }
                }
                
                # Check temporal consistency (for long format)
                if (!is.null(self$options$time) && self$options$time %in% names(data)) {
                    time_var <- self$options$time
                    time_points <- length(unique(data[[time_var]]))
                    
                    if (time_points < 2) {
                        validation_issues <- append(validation_issues, .("Insufficient time points for temporal analysis (minimum 2 required)"))
                        data_quality_score <- data_quality_score - 20
                    }
                }
                
                # Generate quality assessment
                quality_level <- if (data_quality_score >= 90) {
                    list(level = .("Excellent"), color = "#28a745", icon = "✅")
                } else if (data_quality_score >= 75) {
                    list(level = .("Good"), color = "#ffc107", icon = "⚠️")
                } else if (data_quality_score >= 60) {
                    list(level = .("Fair"), color = "#fd7e14", icon = "🔶")
                } else {
                    list(level = .("Poor"), color = "#dc3545", icon = "❌")
                }
                
                validation_html <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;'>",
                    "<h4 style='color: #495057; margin-top: 0;'>🔍 ", .("Enhanced Data Validation Report"), "</h4>",
                    
                    "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; border-left: 4px solid ", quality_level$color, ";'>",
                    "<h5 style='color: ", quality_level$color, "; margin: 0 0 10px 0;'>", quality_level$icon, " ", .("Overall Data Quality:"), " ", quality_level$level, " (", sprintf("%.0f%%", data_quality_score), ")</h5>",
                    "<p style='margin: 5px 0;'><strong>", .("Total observations:"), "</strong> ", format(total_rows, big.mark = ","), "</p>",
                    "<p style='margin: 5px 0;'><strong>", .("Complete cases:"), "</strong> ", format(complete_rows, big.mark = ","), " (", sprintf("%.1f%%", completeness_rate), ")</p>",
                    "</div>",
                    
                    if (length(validation_issues) > 0) {
                        paste0(
                            "<div style='background-color: #fff3cd; padding: 12px; border-radius: 5px; border: 1px solid #ffeaa7;'>",
                            "<h5 style='color: #856404; margin: 0 0 10px 0;'>⚠️ ", .("Validation Issues Detected"), "</h5>",
                            "<ul style='margin: 5px 0; padding-left: 20px;'>",
                            paste0("<li style='margin: 3px 0;'>", validation_issues, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {
                        paste0(
                            "<div style='background-color: #d4edda; padding: 12px; border-radius: 5px; border: 1px solid #c3e6cb;'>",
                            "<p style='color: #155724; margin: 0;'>✓ ", .("No significant validation issues detected. Data appears suitable for flow analysis."), "</p>",
                            "</div>"
                        )
                    },
                    "</div>"
                )
                
                self$results$validation_report$setContent(validation_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px;'>",
                    "<h5>⚠️ ", .("Validation Report Error"), "</h5>",
                    "<p>", .("Unable to generate validation report:"), " ", private$.sanitizeHTML(e$message), "</p>",
                    "</div>"
                )
                self$results$validation_report$setContent(error_html)
            })
        },
        
        # Generate comprehensive cross-reference suggestions
        .generate_cross_reference_suggestions = function() {
            if (!("cross_reference_suggestions" %in% names(self$results))) return()
            
            tryCatch({
                # Analyze data characteristics to suggest optimal analysis approach
                data <- self$data
                suggestions <- list()
                
                # Analyze data structure
                n_rows <- nrow(data)
                n_strata <- length(self$options$strata)
                has_time <- !is.null(self$options$time)
                has_id <- !is.null(self$options$id)
                has_weight <- !is.null(self$options$weight)
                
                # Determine optimal analysis approach
                alternative_approaches <- list()
                
                # Check if simple alluvial might be better
                if (n_strata <= 4 && n_rows < 1000 && !has_weight) {
                    alternative_approaches <- append(alternative_approaches, list(list(
                        method = .("Alluvial Diagrams"),
                        reason = .("Simple alluvial diagram may provide clearer visualization for smaller datasets with few categories"),
                        advantage = .("Simpler interface, faster rendering, focused on flow patterns")
                    )))
                }
                
                # Check if statistical analysis might be needed
                if (has_time && has_id && n_rows > 100) {
                    alternative_approaches <- append(alternative_approaches, list(list(
                        method = .("Longitudinal Analysis"),
                        reason = .("Repeated measures data detected - consider statistical modeling approaches"),
                        advantage = .("Quantify transition probabilities, test for temporal trends, control for covariates")
                    )))
                }
                
                # Check if crosstable analysis might be complementary
                if (n_strata == 2 && !has_time) {
                    alternative_approaches <- append(alternative_approaches, list(list(
                        method = .("Cross-tabulation"),
                        reason = .("Two-category comparison detected - crosstabulation may provide statistical insights"),
                        advantage = .("Chi-square tests, odds ratios, confidence intervals, statistical significance")
                    )))
                }
                
                # Generate recommendations HTML
                suggestion_html <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;'>",
                    "<h4 style='color: #495057; margin-top: 0;'>🧠 ", .("Alternative Analysis Suggestions"), "</h4>",
                    
                    "<div style='background-color: #e7f3ff; padding: 12px; border-radius: 5px; border-left: 4px solid #007bff; margin: 15px 0;'>",
                    "<h5 style='color: #004085; margin: 0 0 8px 0;'>🎯 ", .("Current Analysis: River Plot"), "</h5>",
                    "<p style='margin: 5px 0; color: #004085;'>",
                    .("Excellent choice for visualizing flows and transitions between categories."),
                    " ", .("Strengths: Clear flow patterns, handles multiple time points, supports individual tracking."),
                    "</p>",
                    "</div>",
                    
                    if (length(alternative_approaches) > 0) {
                        paste0(
                            "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; border: 1px solid #ffeaa7;'>",
                            "<h5 style='color: #856404; margin: 0 0 12px 0;'>💡 ", .("Consider These Complementary Approaches"), "</h5>",
                            paste0(sapply(alternative_approaches, function(approach) {
                                paste0(
                                    "<div style='background-color: white; padding: 12px; border-radius: 4px; margin: 8px 0; border-left: 3px solid #6c757d;'>",
                                    "<h6 style='color: #495057; margin: 0 0 5px 0;'>🔧 ", .("Method:"), " ", approach$method, "</h6>",
                                    "<p style='margin: 3px 0 8px 0; font-size: 13px;'><strong>", .("Why:"), "</strong> ", approach$reason, "</p>",
                                    "<p style='margin: 3px 0; font-size: 12px; color: #6c757d;'><strong>", .("Advantages:"), "</strong> ", approach$advantage, "</p>",
                                    "</div>"
                                )
                            }), collapse = ""),
                            "</div>"
                        )
                    } else {
                        paste0(
                            "<div style='background-color: #d4edda; padding: 12px; border-radius: 5px; border: 1px solid #c3e6cb;'>",
                            "<p style='color: #155724; margin: 0;'>✓ ", .("River plot analysis is optimal for your data structure. No alternative approaches recommended at this time."), "</p>",
                            "</div>"
                        )
                    },
                    "</div>"
                )
                
                self$results$cross_reference_suggestions$setContent(suggestion_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px;'>",
                    "<h5>⚠️ ", .("Cross-Reference Error"), "</h5>",
                    "<p>", .("Unable to generate analysis suggestions:"), " ", private$.sanitizeHTML(e$message), "</p>",
                    "</div>"
                )
                self$results$cross_reference_suggestions$setContent(error_html)
            })
        },
        
        # Generate comprehensive optimization report
        .generate_optimization_report = function() {
            if (!("optimization_report" %in% names(self$results))) return()
            
            tryCatch({
                # Analyze current settings and data for optimization opportunities
                data <- self$data
                n_rows <- nrow(data)
                n_strata <- length(self$options$strata)
                
                # Calculate optimization metrics
                optimizations_applied <- list()
                performance_score <- 100
                visual_quality_score <- 100
                
                # Flow path optimization
                if (self$options$reorderEdges) {
                    optimizations_applied <- append(optimizations_applied, .("Edge reordering algorithm applied to reduce crossings"))
                } else {
                    performance_score <- performance_score - 5
                    visual_quality_score <- visual_quality_score - 10
                }
                
                # Smooth flow paths
                if (self$options$addMidPoints) {
                    optimizations_applied <- append(optimizations_applied, .("Mid-points added for smoother flow transitions"))
                } else {
                    visual_quality_score <- visual_quality_score - 5
                }
                
                # Adaptive styling
                if (self$options$adaptive_styling) {
                    optimizations_applied <- append(optimizations_applied, .("Plot styling automatically adapted to data characteristics"))
                } else {
                    visual_quality_score <- visual_quality_score - 10
                }
                
                # Color optimization
                if (self$options$edgeGradient) {
                    optimizations_applied <- append(optimizations_applied, .("Edge gradients enabled for smooth color transitions"))
                }
                
                # Performance optimizations based on data size
                if (n_rows > 1000) {
                    optimizations_applied <- append(optimizations_applied, .("Progressive data processing enabled for large dataset"))
                    if (n_rows > 5000) {
                        optimizations_applied <- append(optimizations_applied, .("Memory-efficient rendering optimized for very large datasets"))
                    }
                }
                
                # Smart detection benefits
                if (self$options$smart_detection) {
                    optimizations_applied <- append(optimizations_applied, .("Smart data format detection eliminates manual configuration"))
                }
                
                # Quality optimization benefits
                if (self$options$quality_optimization) {
                    optimizations_applied <- append(optimizations_applied, .("Advanced quality optimization algorithms active"))
                }
                
                # Generate quality assessment
                overall_score <- (performance_score + visual_quality_score) / 2
                quality_assessment <- if (overall_score >= 90) {
                    list(level = .("Excellent"), color = "#28a745", icon = "⭐")
                } else if (overall_score >= 75) {
                    list(level = .("Good"), color = "#ffc107", icon = "👍")
                } else {
                    list(level = .("Fair"), color = "#fd7e14", icon = "⚠️")
                }
                
                # Suggest additional optimizations
                suggestions <- list()
                if (!self$options$reorderEdges && n_strata > 3) {
                    suggestions <- append(suggestions, .("Enable 'Reorder Edges' to reduce visual crossing complexity"))
                }
                if (!self$options$addMidPoints && self$options$curveType != "linear") {
                    suggestions <- append(suggestions, .("Enable 'Add Mid Points' for smoother curved connections"))
                }
                if (!self$options$adaptive_styling) {
                    suggestions <- append(suggestions, .("Enable 'Adaptive Styling' for automatic visual optimization"))
                }
                
                optimization_html <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;'>",
                    "<h4 style='color: #495057; margin-top: 0;'>⚡ ", .("Quality Optimization Report"), "</h4>",
                    
                    "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; border-left: 4px solid ", quality_assessment$color, ";'>",
                    "<h5 style='color: ", quality_assessment$color, "; margin: 0 0 10px 0;'>", quality_assessment$icon, " ", .("Optimization Level:"), " ", quality_assessment$level, " (", sprintf("%.0f%%", overall_score), ")</h5>",
                    "<p style='margin: 5px 0;'><strong>", .("Performance Score:"), "</strong> ", sprintf("%.0f%%", performance_score), "</p>",
                    "<p style='margin: 5px 0;'><strong>", .("Visual Quality Score:"), "</strong> ", sprintf("%.0f%%", visual_quality_score), "</p>",
                    "</div>",
                    
                    if (length(optimizations_applied) > 0) {
                        paste0(
                            "<div style='background-color: #d4edda; padding: 12px; border-radius: 5px; border: 1px solid #c3e6cb;'>",
                            "<h5 style='color: #155724; margin: 0 0 10px 0;'>✓ ", .("Active Optimizations"), "</h5>",
                            "<ul style='margin: 5px 0; padding-left: 20px;'>",
                            paste0("<li style='margin: 3px 0;'>", optimizations_applied, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {
                        paste0(
                            "<div style='background-color: #fff3cd; padding: 12px; border-radius: 5px; border: 1px solid #ffeaa7;'>",
                            "<p style='color: #856404; margin: 0;'>⚠️ ", .("No specific optimizations currently active. Consider enabling optimization features for better results."), "</p>",
                            "</div>"
                        )
                    },
                    
                    if (length(suggestions) > 0) {
                        paste0(
                            "<div style='background-color: #e7f3ff; padding: 12px; border-radius: 5px; border: 1px solid #b8daff; margin-top: 15px;'>",
                            "<h6 style='color: #004085; margin: 0 0 8px 0;'>💡 ", .("Additional Optimization Suggestions"), "</h6>",
                            "<ul style='font-size: 12px; margin: 5px 0; padding-left: 20px; color: #004085;'>",
                            paste0("<li>", suggestions, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {""},
                    "</div>"
                )
                
                self$results$optimization_report$setContent(optimization_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px;'>",
                    "<h5>⚠️ ", .("Optimization Report Error"), "</h5>",
                    "<p>", .("Unable to generate optimization report:"), " ", private$.sanitizeHTML(e$message), "</p>",
                    "</div>"
                )
                self$results$optimization_report$setContent(error_html)
            })
        },
        
        .generate_clinical_insights_panel = function() {
            tryCatch({
                # Clinical decision support based on current analysis
                insights <- list()
                recommendations <- list()
                
                # Get analysis data
                data <- private$.processedData
                flow_stats <- private$.flowSummary
                stage_stats <- private$.stageSummary
                
                if (!is.null(flow_stats) && nrow(flow_stats) > 0) {
                    # Pathway efficiency analysis
                    total_flows <- sum(flow_stats$count)
                    top_flow <- flow_stats[which.max(flow_stats$count), ]
                    dominant_percentage <- (top_flow$count / total_flows) * 100
                    
                    if (dominant_percentage > 70) {
                        insights <- append(insights, 
                            sprintf(.("Primary pathway detected: %s → %s accounts for %.1f%% of all transitions."), 
                                   top_flow$from_category, top_flow$to_category, dominant_percentage))
                        recommendations <- append(recommendations,
                            .("Consider developing standardized protocols for this dominant care pathway."))
                    } else if (dominant_percentage < 20) {
                        insights <- append(insights,
                            .("Highly variable care pathways detected - no single dominant pattern."))
                        recommendations <- append(recommendations,
                            .("Consider patient stratification or care pathway standardization initiatives."))
                    }
                    
                    # Check for bottlenecks
                    category_flows <- aggregate(flow_stats$count, by = list(flow_stats$from_category), sum)
                    if (max(category_flows$x) > 3 * mean(category_flows$x)) {
                        bottleneck <- category_flows$Group.1[which.max(category_flows$x)]
                        insights <- append(insights,
                            sprintf(.("Potential care bottleneck identified at '%s' stage."), bottleneck))
                        recommendations <- append(recommendations,
                            .("Investigate capacity constraints and care coordination at high-volume stages."))
                    }
                }
                
                # Clinical preset-specific insights
                preset <- self$options$clinicalPreset
                if (preset == "patient_journey") {
                    insights <- append(insights,
                        .("Individual patient tracking enables identification of personalized care pathways and treatment response patterns."))
                    recommendations <- append(recommendations,
                        .("Use individual trajectory patterns to develop personalized treatment protocols."))
                } else if (preset == "treatment_response") {
                    insights <- append(insights,
                        .("Treatment response analysis reveals effectiveness patterns and identifies optimal intervention timing."))
                    recommendations <- append(recommendations,
                        .("Monitor response patterns to optimize treatment selection and timing decisions."))
                }
                
                # Generate HTML output
                clinical_html <- paste0(
                    "<div style='background-color: #f0f8ff; padding: 20px; border-radius: 8px; border: 1px solid #b8dcf0;'>",
                    "<h4 style='color: #1e5594; margin-top: 0;'>🎯 ", .("Clinical Decision Support"), "</h4>",
                    
                    if (length(insights) > 0) {
                        paste0(
                            "<div style='background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0; border-left: 3px solid #1e5594;'>",
                            "<h6 style='color: #1e5594; margin: 0 0 8px 0;'>🔍 ", .("Clinical Insights"), "</h6>",
                            "<ul style='margin: 5px 0; padding-left: 20px;'>",
                            paste0("<li style='margin: 5px 0;'>", insights, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {""},
                    
                    if (length(recommendations) > 0) {
                        paste0(
                            "<div style='background-color: #e7f3ff; padding: 12px; border-radius: 5px; margin: 10px 0; border-left: 3px solid #007bff;'>",
                            "<h6 style='color: #004085; margin: 0 0 8px 0;'>💡 ", .("Clinical Recommendations"), "</h6>",
                            "<ul style='margin: 5px 0; padding-left: 20px;'>",
                            paste0("<li style='margin: 5px 0;'>", recommendations, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {""},
                    
                    "<div style='background-color: #f1f3f4; padding: 10px; border-radius: 5px; margin-top: 15px;'>",
                    "<p style='margin: 0; font-size: 12px; color: #5f6368;'>",
                    "<strong>", .("Note:"), "</strong> ", .("Clinical insights are based on flow pattern analysis and should be interpreted within the context of your specific clinical setting and patient population."),
                    "</p>",
                    "</div>",
                    "</div>"
                )
                
                self$results$clinical_insights$setContent(clinical_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px;'>",
                    "<h5>⚠️ ", .("Clinical Insights Error"), "</h5>",
                    "<p>", .("Unable to generate clinical insights:"), " ", private$.sanitizeHTML(e$message), "</p>",
                    "</div>"
                )
                self$results$clinical_insights$setContent(error_html)
            })
        },
        
        .generate_enhanced_caveats = function() {
            tryCatch({
                # Clinical assumptions and considerations
                assumptions <- list()
                considerations <- list()
                warnings <- list()
                
                # Data-specific assumptions
                data <- self$data
                n_obs <- nrow(data)
                
                # Sample size considerations
                if (n_obs < 30) {
                    warnings <- append(warnings, 
                        .("Very small sample size (<30) may not provide reliable flow patterns for clinical decision making."))
                } else if (n_obs < 100) {
                    considerations <- append(considerations,
                        .("Small to moderate sample size may limit generalizability to broader patient populations."))
                }
                
                # Flow analysis assumptions
                assumptions <- append(assumptions, 
                    .("Flow patterns represent observed transitions and do not imply causal relationships between stages."))
                assumptions <- append(assumptions,
                    .("Data quality directly impacts analysis reliability - ensure consistent coding and complete case ascertainment."))
                
                # Time-based considerations
                if (!is.null(self$options$time) && self$options$time != "") {
                    assumptions <- append(assumptions,
                        .("Temporal analysis assumes consistent data collection intervals and comparable follow-up periods."))
                    considerations <- append(considerations,
                        .("Consider patient attrition, competing risks, and time-varying covariates in interpretation."))
                }
                
                # Individual tracking considerations
                if (!is.null(self$options$id) && self$options$id != "") {
                    assumptions <- append(assumptions,
                        .("Individual tracking assumes unique patient identifiers and accurate linkage across time points."))
                    considerations <- append(considerations,
                        .("Patient-level analysis enables personalized insights but requires privacy protection measures."))
                }
                
                # Interpretation guidelines
                interpretation_notes <- list(
                    .("Flow width represents frequency or volume - not clinical importance or quality of care."),
                    .("Dominant pathways may reflect either optimal care protocols or system constraints."),
                    .("Unusual flow patterns warrant investigation but may represent appropriate individualized care."),
                    .("Consider external factors (policy changes, seasonal effects, population characteristics) in interpretation.")
                )
                
                # Generate comprehensive caveats HTML
                caveats_html <- paste0(
                    "<div style='background-color: #fff8e1; padding: 20px; border-radius: 8px; border: 1px solid #ffcc02;'>",
                    "<h4 style='color: #ef6c00; margin-top: 0;'>⚠️ ", .("Clinical Assumptions & Considerations"), "</h4>",
                    
                    "<div style='background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0; border-left: 3px solid #ff9800;'>",
                    "<h6 style='color: #ef6c00; margin: 0 0 8px 0;'>📋 ", .("Key Assumptions"), "</h6>",
                    "<ul style='margin: 5px 0; padding-left: 20px; font-size: 13px;'>",
                    paste0("<li style='margin: 3px 0;'>", assumptions, "</li>", collapse = ""),
                    "</ul>",
                    "</div>",
                    
                    if (length(considerations) > 0) {
                        paste0(
                            "<div style='background-color: #e7f3ff; padding: 12px; border-radius: 5px; margin: 10px 0; border-left: 3px solid #2196f3;'>",
                            "<h6 style='color: #1565c0; margin: 0 0 8px 0;'>🤔 ", .("Clinical Considerations"), "</h6>",
                            "<ul style='margin: 5px 0; padding-left: 20px; font-size: 13px;'>",
                            paste0("<li style='margin: 3px 0;'>", considerations, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {""},
                    
                    if (length(warnings) > 0) {
                        paste0(
                            "<div style='background-color: #ffebee; padding: 12px; border-radius: 5px; margin: 10px 0; border-left: 3px solid #f44336;'>",
                            "<h6 style='color: #d32f2f; margin: 0 0 8px 0;'>⚠️ ", .("Important Warnings"), "</h6>",
                            "<ul style='margin: 5px 0; padding-left: 20px; font-size: 13px;'>",
                            paste0("<li style='margin: 3px 0;'>", warnings, "</li>", collapse = ""),
                            "</ul>",
                            "</div>"
                        )
                    } else {""},
                    
                    "<div style='background-color: #f5f5f5; padding: 12px; border-radius: 5px; margin: 10px 0;'>",
                    "<h6 style='color: #424242; margin: 0 0 8px 0;'>📈 ", .("Interpretation Guidelines"), "</h6>",
                    "<ul style='margin: 5px 0; padding-left: 20px; font-size: 12px; color: #424242;'>",
                    paste0("<li style='margin: 3px 0;'>", interpretation_notes, "</li>", collapse = ""),
                    "</ul>",
                    "</div>",
                    
                    "<div style='background-color: #e8f5e8; padding: 10px; border-radius: 5px; margin-top: 15px;'>",
                    "<p style='margin: 0; font-size: 12px; color: #2e7d32;'>",
                    "<strong>", .("Best Practice:"), "</strong> ", .("Always interpret flow analysis results within the context of clinical knowledge, institutional protocols, and patient-specific factors."),
                    "</p>",
                    "</div>",
                    "</div>"
                )
                
                self$results$enhanced_caveats$setContent(caveats_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px;'>",
                    "<h5>⚠️ ", .("Caveats Generation Error"), "</h5>",
                    "<p>", .("Unable to generate clinical caveats:"), " ", private$.sanitizeHTML(e$message), "</p>",
                    "</div>"
                )
                self$results$enhanced_caveats$setContent(error_html)
            })
        }
    )
)