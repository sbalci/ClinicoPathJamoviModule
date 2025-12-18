#' @title Lollipop Charts for Categorical Data Visualization
#' @description
#' Creates comprehensive lollipop charts for categorical data visualization with 
#' emphasis on clinical applications. Lollipop charts are particularly effective 
#' for displaying categorical data with a focus on individual values, making them 
#' ideal for patient timelines, treatment outcomes, biomarker levels, and 
#' comparative clinical assessments.
#' 
#' @details
#' The lollipop chart function is designed specifically for clinical research 
#' applications where categorical data visualization with emphasis on individual 
#' values is crucial. Unlike bar charts, lollipop charts reduce ink-to-data ratio 
#' and provide cleaner visualization for sparse data or when highlighting specific 
#' categories.
#' 
#' Key features:
#' - Flexible orientation (vertical/horizontal)
#' - Advanced sorting options (by value, alphabetical)
#' - Clinical color schemes and themes
#' - Highlighting capabilities for specific categories
#' - Statistical summary integration
#' - Professional publication-ready appearance
#' 
#' Common clinical applications:
#' - Patient timeline visualization
#' - Biomarker level comparisons
#' - Treatment outcome rankings
#' - Survey response visualization
#' - Quality metric displays
#' - Diagnostic test results
#' 
#' @examples
#' \dontrun{
#' # Basic lollipop chart
#' result <- lollipop(
#'   data = patient_data,
#'   dep = "biomarker_level",
#'   group = "patient_id"
#' )
#' 
#' # Horizontal lollipop with sorting
#' result <- lollipop(
#'   data = treatment_data,
#'   dep = "response_score",
#'   group = "treatment_type",
#'   sortBy = "value_desc",
#'   orientation = "horizontal",
#'   showValues = TRUE
#' )
#' 
#' # Clinical timeline with highlighting
#' result <- lollipop(
#'   data = timeline_data,
#'   dep = "days_to_event",
#'   group = "patient_id",
#'   highlight = "high_risk_patient",
#'   showMean = TRUE
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore

lollipopClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,
    private = list(
        .notices = list(),

        # Helper method for adding notices with priority
        .addNotice = function(type, message, name = NULL) {
            if (is.null(name)) {
                name <- paste0('notice', length(private$.notices) + 1)
            }

            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(message)

            # Store with priority for sorting
            priority <- switch(
                as.character(type),
                "1" = 1,  # ERROR
                "2" = 2,  # STRONG_WARNING
                "3" = 3,  # WARNING
                "4" = 4,  # INFO
                3         # Default to WARNING
            )

            private$.notices[[length(private$.notices) + 1]] <- list(
                notice = notice,
                priority = priority
            )
        },

        # Insert all notices in priority order
        .insertNotices = function() {
            if (length(private$.notices) == 0) return()

            # Sort by priority (ERROR > STRONG_WARNING > WARNING > INFO)
            notices_sorted <- private$.notices[order(sapply(private$.notices, function(x) x$priority))]

            # Insert in order
            position <- 1
            for (n in notices_sorted) {
                self$results$insert(position, n$notice)
                position <- position + 1
            }
        },

        # Reset notices for new analysis
        .resetNotices = function() {
            private$.notices <- list()
        },
        
        # Initialize results and validate dependencies
        .init = function() {
            # Reset notices for new analysis
            private$.resetNotices()

            # Check for required packages
            missing_packages <- c()
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "ggplot2")
            }
            if (!requireNamespace("dplyr", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "dplyr")
            }

            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "The following required packages are not installed: ",
                    paste(missing_packages, collapse = ", "),
                    ". Please install them using: install.packages(c(",
                    paste0("'", missing_packages, "'", collapse = ", "), "))"
                )

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingPackages',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(error_msg)
                self$results$insert(1, notice)
                return()
            }
            
            # Initialize with welcome message if no variables selected
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                welcome_msg <- paste0("
                <div class='alert alert-info'>
                <h4>", .("Welcome to Lollipop Chart Analysis"), "</h4>
                <p>", .("This function creates lollipop charts for categorical data visualization with clinical applications."), "</p>
                
                <h5>", .("Required inputs:"), "</h5>
                <ul>
                <li><strong>", .("Dependent Variable"), "</strong>: ", .("Numeric values (biomarker levels, scores, measurements)"), "</li>
                <li><strong>", .("Grouping Variable"), "</strong>: ", .("Categories (patient IDs, treatments, conditions)"), "</li>
                </ul>
                
                <h5>", .("Key features:"), "</h5>
                <ul>
                <li><strong>Flexible Layout</strong>: Vertical or horizontal orientation</li>
                <li><strong>Smart Sorting</strong>: Order by value, alphabetical, or original order</li>
                <li><strong>Highlighting</strong>: Emphasize specific categories or patients</li>
                <li><strong>Clinical Themes</strong>: Professional color schemes and layouts</li>
                <li><strong>Statistical Integration</strong>: Summary statistics and reference lines</li>
                </ul>
                
                <h5>", .("Data Handling Notes:"), "</h5>
                <ul>
                <li>Rows with missing values (NA) in the selected variables will be automatically removed.</li>
                <li>If your data has multiple rows per group, use the <strong>Data Aggregation</strong> option (Mean/Median/Sum) to avoid over-plotting.</li>
                </ul>

                <h5>Clinical applications:</h5>
                <ul>
                <li>Patient timeline visualization (days to event, treatment progression)</li>
                <li>Biomarker level comparisons across patients</li>
                <li>Treatment outcome rankings</li>
                <li>Survey response visualization</li>
                <li>Quality metric displays</li>
                <li>Diagnostic test result comparisons</li>
                </ul>
                
                <h5>Advantages over bar charts:</h5>
                <ul>
                <li>Reduced visual clutter (lower ink-to-data ratio)</li>
                <li>Better for sparse data or many categories</li>
                <li>Emphasizes individual data points</li>
                <li>", .("Professional appearance for publications"), "</li>
                </ul>
                </div>
                ")
                
                self$results$todo$setContent(welcome_msg)
                
                # Hide results until data is provided
                self$results$summary$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
                self$results$warnings$setVisible(FALSE)
            }
        },
        
        .run = function() {
            # Early exits for missing data or variables
            if (is.null(self$data) || nrow(self$data) == 0) {
                return()
            }
            
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                return()
            }
            
            # Hide welcome message and show results
            self$results$todo$setVisible(FALSE)
            self$results$summary$setVisible(TRUE)
            self$results$plot$setVisible(TRUE)

            # Reset notices for new analysis run
            private$.resetNotices()

            # Main analysis pipeline with comprehensive error handling
            tryCatch({
                # Checkpoint before expensive data cleaning
                private$.checkpoint()
                
                # Prepare and validate data
                data <- private$.cleanData()
                if (is.null(data)) return()
                
                # Checkpoint before statistical calculations
                private$.checkpoint()
                
                # Calculate summary statistics
                summary_stats <- private$.calculateSummary(data)
                private$.populateSummary(summary_stats)

                # Check for potential issues and warnings
                private$.checkForMisuseAndWarnings(data, summary_stats)

                # Add note about conditional coloring if enabled
                if (self$options$conditionalColor) {
                    private$.addNotice(
                        jmvcore::NoticeType$INFO,
                        sprintf("Conditional coloring applied. Values > %.2f colored orange (above threshold), others blue (below).",
                                self$options$colorThreshold),
                        'conditionalColorNote'
                    )
                }

                # Insert all notices in priority order (ERROR > STRONG_WARNING > WARNING > INFO)
                private$.insertNotices()

                # Generate and display clinical summary
                clinical_summary <- private$.generateClinicalSummary(summary_stats, self$options$dep, self$options$group)
                self$results$todo$setContent(clinical_summary)
                self$results$todo$setVisible(TRUE)
                
                # Checkpoint before plot data preparation
                private$.checkpoint()
                
                # Save plot data for rendering
                private$.savePlotData(data)
                
            }, error = function(e) {
                error_msg <- paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>Analysis Error</h4>",
                    "<p><strong>Error:</strong> ", e$message, "</p>",
                    "<p>Please check your data and variable selections.</p>",
                    "</div>"
                )
                self$results$todo$setContent(error_msg)
                self$results$todo$setVisible(TRUE)
            })
        },
        
        # Comprehensive data cleaning and validation
        .cleanData = function() {
            # Extract variables
            dep_var <- self$options$dep
            group_var <- self$options$group
            
            # Check if variables exist in data
            missing_vars <- setdiff(c(dep_var, group_var), names(self$data))
            if (length(missing_vars) > 0) {
                stop(paste(.("Variables not found in data:"), paste(missing_vars, collapse = ", ")))
            }
            
            # Select and clean data
            data <- self$data[c(dep_var, group_var)]
            
            # Checkpoint before expensive data validation and conversion
            private$.checkpoint(flush = FALSE)  # Only poll for changes, don't push results yet
            
            # Validate dependent variable (must be numeric)
            dep_data <- jmvcore::toNumeric(data[[dep_var]])
            if (all(is.na(dep_data))) {
                stop(.("Dependent variable must be numeric (continuous variable)."))
            }
            data[[dep_var]] <- dep_data
            
            # Validate grouping variable
            group_data <- data[[group_var]]
            if (is.character(group_data)) {
                data[[group_var]] <- factor(group_data)
            } else if (!is.factor(group_data)) {
                data[[group_var]] <- factor(group_data)
            }
            
            # Check number of groups
            n_groups <- length(unique(data[[group_var]]))
            if (n_groups < 2) {
                stop(.("Grouping variable must have at least 2 different categories."))
            }

            if (n_groups > 50) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("Grouping variable has more than 50 levels (%d levels detected). Consider reducing categories for better visualization.", n_groups),
                    'manyGroups'
                )
            }
            
            # Checkpoint before potentially expensive missing data removal
            private$.checkpoint(flush = FALSE)
            
            # Remove rows with missing values
            complete_before <- nrow(data)
            data <- data[complete.cases(data), ]
            complete_after <- nrow(data)
            
            if (complete_after == 0) {
                stop(.("No complete cases found. Please check for missing values in selected variables."))
            }
            
            if (complete_after < complete_before) {
                n_removed <- complete_before - complete_after
                pct_removed <- round(100 * n_removed / complete_before, 1)
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("%d rows (%g%%) with missing values were removed from analysis.", n_removed, pct_removed),
                    'missingData'
                )
            }
            
            # Check minimum data requirements
            if (nrow(data) < 2) {
                stop(.("At least 2 complete observations are required for lollipop chart analysis."))
            }
            
            # Validate highlight level if provided and highlighting is enabled
            highlight_level <- if (self$options$useHighlight) {
                self$options$highlight
            } else {
                NULL  # Disable highlighting if useHighlight is FALSE
            }
            
            if (self$options$useHighlight && !is.null(highlight_level) && !highlight_level %in% data[[group_var]]) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("Highlight level '%s' not found in grouping variable. Highlight will be ignored.", highlight_level),
                    'highlightNotFound'
                )
                highlight_level <- NULL  # Disable highlighting for this case
            }

            # CRITICAL FIX: Check for duplicates and aggregate if requested
            # Without aggregation, multiple rows per group will over-plot
            group_counts <- table(data[[group_var]])
            has_duplicates <- any(group_counts > 1)

            if (has_duplicates && self$options$aggregation == "none") {
                max_count <- max(group_counts)
                groups_with_dups <- names(group_counts[group_counts > 1])
                private$.addNotice(
                    jmvcore::NoticeType$STRONG_WARNING,
                    sprintf(
                        "Multiple observations per group detected (max=%d per group). Groups with duplicates: %s. Use aggregation (mean/median/sum) to avoid over-plotting and misleading visualization.",
                        max_count,
                        paste(head(groups_with_dups, 5), collapse = ", ")
                    ),
                    'duplicateGroups'
                )
            }

            # Apply aggregation if requested
            if (self$options$aggregation != "none") {
                data <- private$.aggregateData(data, dep_var, group_var, self$options$aggregation)
            }

            # Apply sorting if requested
            data <- private$.applySorting(data, dep_var, group_var)
            
            # Add column names for easier reference
            colnames(data) <- c("dependent", "group")
            
            return(data)
        },
        
        # Apply sorting based on user selection
        .applySorting = function(data, dep_var, group_var) {
            sort_method <- self$options$sortBy

            # CRITICAL FIX: Must relevel factor, not just reorder rows
            # ggplot2 uses factor levels order, not data frame row order
            if (sort_method == "value_asc") {
                # Sort by ascending values
                data <- data[order(data[[dep_var]]), ]
                # Relevel factor to match sorted order
                data[[group_var]] <- factor(data[[group_var]], levels = unique(data[[group_var]]))
            } else if (sort_method == "value_desc") {
                # Sort by descending values
                data <- data[order(-data[[dep_var]]), ]
                # Relevel factor to match sorted order
                data[[group_var]] <- factor(data[[group_var]], levels = unique(data[[group_var]]))
            } else if (sort_method == "group_alpha") {
                # Sort alphabetically by group
                data <- data[order(data[[group_var]]), ]
                # Relevel factor to match sorted order
                data[[group_var]] <- factor(data[[group_var]], levels = unique(data[[group_var]]))
            }
            # "original" keeps the original order (no releveling needed)

            return(data)
        },

        # Aggregate data by group to prevent over-plotting
        .aggregateData = function(data, dep_var, group_var, method) {
            # Use dplyr-style aggregation
            agg_func <- switch(method,
                "mean" = function(x) mean(x, na.rm = TRUE),
                "median" = function(x) median(x, na.rm = TRUE),
                "sum" = function(x) sum(x, na.rm = TRUE),
                function(x) mean(x, na.rm = TRUE)  # Default to mean
            )

            # Aggregate by group
            agg_data <- aggregate(
                data[[dep_var]],
                by = list(group = data[[group_var]]),
                FUN = agg_func
            )

            # Rename columns to match original
            colnames(agg_data) <- c(group_var, dep_var)

            # Ensure group column is factor with same levels
            agg_data[[group_var]] <- factor(agg_data[[group_var]], levels = levels(data[[group_var]]))
            
            # Reorder to [dep, group] to match .cleanData expectation
            agg_data <- agg_data[, c(dep_var, group_var)]

            return(agg_data)
        },

        # Calculate summary statistics
        .calculateSummary = function(data) {
            summary_stats <- list()
            
            # Basic data information
            summary_stats$n_observations <- nrow(data)
            summary_stats$n_groups <- length(unique(data$group))
            
            # Dependent variable statistics
            dep_data <- data$dependent
            summary_stats$dep_mean <- mean(dep_data, na.rm = TRUE)
            summary_stats$dep_median <- median(dep_data, na.rm = TRUE)
            summary_stats$dep_sd <- sd(dep_data, na.rm = TRUE)
            summary_stats$dep_min <- min(dep_data, na.rm = TRUE)
            summary_stats$dep_max <- max(dep_data, na.rm = TRUE)
            summary_stats$dep_range <- summary_stats$dep_max - summary_stats$dep_min
            
            # Checkpoint before group-by operations which can be expensive for large datasets
            private$.checkpoint(flush = FALSE)
            
            # Group information
            group_summary <- data %>%
                dplyr::group_by(group) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = mean(dependent, na.rm = TRUE),
                    .groups = 'drop'
                )
            
            summary_stats$group_names <- paste(unique(data$group), collapse = ", ")
            summary_stats$groups_with_highest <- group_summary$group[which.max(group_summary$mean)]
            summary_stats$groups_with_lowest <- group_summary$group[which.min(group_summary$mean)]
            
            return(summary_stats)
        },
        
        # Generate clinical summary for easier interpretation
        .generateClinicalSummary = function(summary_stats, dep_var, group_var) {
            summary_html <- paste0(
                "<div class='alert alert-success'>",
                "<h5>", .("Clinical Summary"), "</h5>",
                "<p><strong>", .("Analysis Overview"), ":</strong> ",
                .("This analysis compared"), " <strong>", summary_stats$n_observations, "</strong> ",
                .("observations across"), " <strong>", summary_stats$n_groups, "</strong> ", 
                .("groups"), ".</p>",
                
                "<p><strong>", .("Key Findings"), ":</strong></p>",
                "<ul>",
                "<li>", .("Mean value"), ": <strong>", round(summary_stats$dep_mean, 2), "</strong> ",
                "(", .("Standard Deviation"), " = ", round(summary_stats$dep_sd, 2), ")</li>",
                "<li>", .("Value range"), ": ", round(summary_stats$dep_min, 2), " - ", round(summary_stats$dep_max, 2), "</li>",
                "<li>", .("Highest values found in"), ": <strong>", summary_stats$groups_with_highest, "</strong></li>",
                "<li>", .("Lowest values found in"), ": <strong>", summary_stats$groups_with_lowest, "</strong></li>",
                "</ul>",
                
                "<p><strong>", .("Clinical Interpretation"), ":</strong> ",
                ifelse(summary_stats$dep_range > 2 * summary_stats$dep_sd,
                    .("Notable variation observed between groups, suggesting clinically meaningful differences."),
                    .("Relatively consistent values across groups with minimal variation.")
                ), "</p>",
                "</div>"
            )
            
            return(summary_html)
        },
        
        # Advanced misuse detection and contextual warnings
        .checkForMisuseAndWarnings = function(data, summary_stats) {
            # Check for too many groups relative to sample size
            if (summary_stats$n_groups > summary_stats$n_observations / 3) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("Many groups (%d) relative to sample size (%d). Consider grouping categories or using a different visualization.",
                            summary_stats$n_groups, summary_stats$n_observations),
                    'manyGroupsVsN'
                )
            }

            # Check for highly skewed data
            if (summary_stats$dep_range > 5 * summary_stats$dep_sd) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("Data appears highly variable (range = %.2f, SD = %.2f). Consider log transformation or outlier investigation.",
                            summary_stats$dep_range, summary_stats$dep_sd),
                    'highVariability'
                )
            }

            # Check for groups with very different sample sizes
            group_counts <- table(data$group)
            max_count <- max(group_counts)
            min_count <- min(group_counts)
            if (max_count > 5 * min_count && length(group_counts) > 2) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("Unbalanced group sizes detected (range: %d to %d observations per group). Interpretation should account for different sample sizes.",
                            min_count, max_count),
                    'unbalancedGroups'
                )
            }

            # Check for small overall sample size
            if (summary_stats$n_observations < 10) {
                private$.addNotice(
                    jmvcore::NoticeType$WARNING,
                    sprintf("Small sample size (n=%d). Results should be interpreted with caution.",
                            summary_stats$n_observations),
                    'smallSample'
                )
            }
        },
        
        # Populate summary table
        .populateSummary = function(summary_stats) {
            table <- self$results$summary
            table$deleteRows()
            
            row_num <- 1
            
            # Data characteristics
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Number of Observations"),
                value = as.character(summary_stats$n_observations)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Number of Groups"),
                value = as.character(summary_stats$n_groups)
            ))
            row_num <- row_num + 1
            
            # Dependent variable statistics
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Mean Value"),
                value = format(summary_stats$dep_mean, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Median Value"),
                value = format(summary_stats$dep_median, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Standard Deviation"),
                value = format(summary_stats$dep_sd, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Value Range"),
                value = paste(format(summary_stats$dep_min, digits = 3), "-", 
                             format(summary_stats$dep_max, digits = 3))
            ))
            row_num <- row_num + 1
            
            # Group information
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Highest Value Group"),
                value = as.character(summary_stats$groups_with_highest)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Lowest Value Group"),
                value = as.character(summary_stats$groups_with_lowest)
            ))
        },
        
        # Get color scheme
        .getColorScheme = function(n_colors, highlight_level = NULL) {
            scheme_name <- self$options$colorScheme
            
            base_colors <- switch(scheme_name,
                "default" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A"),
                "clinical" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
                "viridis" = c("#440154", "#31688e", "#35b779", "#fde725", "#21908c", "#5dc863"),
                "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")  # fallback
            )
            
            # Extend colors if needed
            if (n_colors > length(base_colors)) {
                base_colors <- rep(base_colors, length.out = n_colors)
            } else {
                base_colors <- base_colors[1:n_colors]
            }
            
            # Handle highlighting
            if (!is.null(highlight_level)) {
                highlight_color <- "#FF0000"  # Red for highlight
                normal_color <- "#CCCCCC"     # Gray for non-highlighted
                
                return(list(
                    colors = base_colors,
                    highlight_color = highlight_color,
                    normal_color = normal_color,
                    has_highlight = TRUE
                ))
            } else {
                return(list(
                    colors = base_colors,
                    has_highlight = FALSE
                ))
            }
        },
        
        # Get plot theme
        .getPlotTheme = function() {
            theme_name <- self$options$theme
            
            base_theme <- switch(theme_name,
                "default" = ggplot2::theme_gray(),
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "publication" = ggplot2::theme_bw() + 
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        strip.background = ggplot2::element_blank(),
                        legend.position = "bottom",
                        text = ggplot2::element_text(size = 12),
                        axis.title = ggplot2::element_text(size = 14),
                        plot.title = ggplot2::element_text(size = 16, hjust = 0.5)
                    ),
                ggplot2::theme_gray()  # fallback
            )
            
            return(base_theme)
        },
        
        # Create base plot with proper orientation
        .createBasePlot = function(plot_data, orientation) {
            if (orientation == "horizontal") {
                return(ggplot2::ggplot(plot_data, ggplot2::aes(x = dependent, y = group)))
            } else {
                return(ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = dependent)))
            }
        },
        
        # Add lollipop elements (refactored to eliminate duplication)
        .addLollipopElements = function(p, plot_data, orientation, color_scheme, highlight_level, point_size, line_width, line_type = "solid", baseline = 0, conditional_color = FALSE, color_threshold = 0) {
            has_highlight <- color_scheme$has_highlight && 
                            !is.null(highlight_level) && 
                            highlight_level %in% plot_data$group
            
            # Always create color_category column, determine coloring strategy
            if (conditional_color) {
                # Conditional coloring based on threshold
                plot_data$color_category <- ifelse(plot_data$dependent > color_threshold, "above_threshold", "below_threshold")
                use_color_mapping <- TRUE
            } else if (has_highlight) {
                # Highlight-based coloring
                plot_data$color_category <- ifelse(plot_data$group == highlight_level, "highlighted", "normal")
                use_color_mapping <- TRUE
            } else {
                # No special coloring - all same category
                plot_data$color_category <- "normal"
                use_color_mapping <- FALSE
            }
            
            # Update the plot data in the ggplot object
            p$data <- plot_data
            
            # Get segment coordinates based on orientation
            if (orientation == "horizontal") {
                if (use_color_mapping) {
                    segment_aes <- ggplot2::aes(x = baseline, xend = dependent, y = group, yend = group, color = color_category)
                    point_aes <- ggplot2::aes(color = color_category)
                } else {
                    segment_aes <- ggplot2::aes(x = baseline, xend = dependent, y = group, yend = group)
                    point_aes <- ggplot2::aes()
                }
            } else {
                if (use_color_mapping) {
                    segment_aes <- ggplot2::aes(x = group, xend = group, y = baseline, yend = dependent, color = color_category)
                    point_aes <- ggplot2::aes(color = color_category)
                } else {
                    segment_aes <- ggplot2::aes(x = group, xend = group, y = baseline, yend = dependent)
                    point_aes <- ggplot2::aes()
                }
            }
            
            # Add segments and points
            p <- p + ggplot2::geom_segment(segment_aes, linewidth = line_width, linetype = line_type)
            
            if (use_color_mapping) {
                # Apply custom colors
                if (conditional_color) {
                    colors <- c("above_threshold" = "#E69F00", "below_threshold" = "#56B4E9")  # Orange/Blue
                } else if (has_highlight) {
                    colors <- c("highlighted" = color_scheme$highlight_color, "normal" = color_scheme$normal_color)
                } else {
                    colors <- c("normal" = color_scheme$colors[1])
                }
                
                p <- p + 
                    ggplot2::geom_point(point_aes, size = point_size) +
                    ggplot2::scale_color_manual(values = colors, guide = "none")
            } else {
                # No special coloring - use default color
                p <- p + ggplot2::geom_point(color = color_scheme$colors[1], size = point_size)
            }
            
            return(p)
        },
        
        # Main plotting function
        .plot = function(image, ggtheme, theme, ...) {
            # Get plot state (contains data + visual options)
            plot_state <- image$state
            if (is.null(plot_state)) return()

            # Extract data from state
            plot_data <- plot_state$data
            if (is.null(plot_data)) return()
            
            # Get options
            orientation <- self$options$orientation
            show_values <- self$options$showValues
            show_mean <- self$options$showMean
            highlight_level <- if (self$options$useHighlight) {
                self$options$highlight
            } else {
                NULL  # Disable highlighting if useHighlight is FALSE
            }
            point_size <- self$options$pointSize
            line_width <- self$options$lineWidth
            line_type <- self$options$lineType
            baseline <- self$options$baseline
            conditional_color <- self$options$conditionalColor
            color_threshold <- self$options$colorThreshold
            
            # Set up colors
            n_groups <- length(unique(plot_data$group))
            color_scheme <- private$.getColorScheme(n_groups, highlight_level)
            
            # Checkpoint before expensive plot generation
            private$.checkpoint(flush = FALSE)
            
            # Create base plot
            p <- private$.createBasePlot(plot_data, orientation)
            
            # Add lollipop elements (refactored to eliminate duplication)
            p <- private$.addLollipopElements(p, plot_data, orientation, color_scheme, highlight_level, point_size, line_width, line_type, baseline, conditional_color, color_threshold)
            
            # Add value labels if requested
            if (show_values) {
                if (orientation == "horizontal") {
                    p <- p + ggplot2::geom_text(
                        ggplot2::aes(label = round(dependent, 2)),
                        hjust = -0.2,
                        size = 3
                    )
                } else {
                    p <- p + ggplot2::geom_text(
                        ggplot2::aes(label = round(dependent, 2)),
                        vjust = -0.5,
                        size = 3
                    )
                }
            }
            
            # Add mean line if requested
            if (show_mean) {
                mean_value <- mean(plot_data$dependent, na.rm = TRUE)
                if (orientation == "horizontal") {
                    p <- p + ggplot2::geom_vline(
                        xintercept = mean_value,
                        linetype = "dashed",
                        color = "red",
                        linewidth = 1
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = mean_value,
                        y = Inf,
                        label = paste(.("Mean ="), round(mean_value, 2)),
                        hjust = 1.1,
                        vjust = 1.5,
                        color = "red",
                        size = 3
                    )
                } else {
                    p <- p + ggplot2::geom_hline(
                        yintercept = mean_value,
                        linetype = "dashed",
                        color = "red",
                        linewidth = 1
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = Inf,
                        y = mean_value,
                        label = paste(.("Mean ="), round(mean_value, 2)),
                        hjust = 1.1,
                        vjust = -0.5,
                        color = "red",
                        size = 3
                    )
                }
            }
            
            # Add labels
            dep_var <- self$options$dep
            group_var <- self$options$group
            
            xlabel <- if (!is.null(self$options$xlabel) && nchar(self$options$xlabel) > 0) {
                self$options$xlabel
            } else {
                if (orientation == "horizontal") dep_var else group_var
            }
            
            ylabel <- if (!is.null(self$options$ylabel) && nchar(self$options$ylabel) > 0) {
                self$options$ylabel
            } else {
                if (orientation == "horizontal") group_var else dep_var
            }
            
            plot_title <- if (!is.null(self$options$title) && nchar(self$options$title) > 0) {
                self$options$title
            } else {
                paste(.("Lollipop Chart:"), dep_var, .("by"), group_var)
            }
            
            p <- p + ggplot2::labs(
                x = xlabel,
                y = ylabel,
                title = plot_title
            )
            
            # Handle axis rotation for vertical orientation with many groups
            if (orientation == "vertical" && n_groups > 10) {
                p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            }
            
            # Apply theme
            p <- p + private$.getPlotTheme()
            
            # Print plot
            print(p)
            TRUE
        },
        
        # Save plot data
        .savePlotData = function(data) {
            # CRITICAL: Include ALL visual options in state to trigger updates
            # when user changes colors, themes, sizes, etc.
            plotState <- list(
                data = data,
                # Visual appearance options
                orientation = self$options$orientation,
                colorScheme = self$options$colorScheme,
                theme = self$options$theme,
                pointSize = self$options$pointSize,
                lineWidth = self$options$lineWidth,
                lineType = self$options$lineType,
                baseline = self$options$baseline,
                # Display options
                showValues = self$options$showValues,
                showMean = self$options$showMean,
                # Highlighting
                useHighlight = self$options$useHighlight,
                highlight = self$options$highlight,
                # Conditional coloring
                conditionalColor = self$options$conditionalColor,
                colorThreshold = self$options$colorThreshold,
                # Labels
                xlabel = self$options$xlabel,
                ylabel = self$options$ylabel,
                title = self$options$title
            )

            self$results$plot$setState(plotState)
        }
    )
)