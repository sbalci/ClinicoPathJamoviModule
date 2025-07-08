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
        
        # Initialize results and validate dependencies
        .init = function() {
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
                    "\n\nPlease install them using:\n",
                    "install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))"
                )
                
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>Missing Dependencies</h4>",
                    "<p>", gsub("\n", "<br>", error_msg), "</p>",
                    "</div>"
                ))
                return()
            }
            
            # Initialize with welcome message if no variables selected
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                welcome_msg <- "
                <div class='alert alert-info'>
                <h4>Welcome to Lollipop Chart Analysis</h4>
                <p>This function creates lollipop charts for categorical data visualization with clinical applications.</p>
                
                <h5>Required inputs:</h5>
                <ul>
                <li><strong>Dependent Variable</strong>: Numeric values (biomarker levels, scores, measurements)</li>
                <li><strong>Grouping Variable</strong>: Categories (patient IDs, treatments, conditions)</li>
                </ul>
                
                <h5>Key features:</h5>
                <ul>
                <li><strong>Flexible Layout</strong>: Vertical or horizontal orientation</li>
                <li><strong>Smart Sorting</strong>: Order by value, alphabetical, or original order</li>
                <li><strong>Highlighting</strong>: Emphasize specific categories or patients</li>
                <li><strong>Clinical Themes</strong>: Professional color schemes and layouts</li>
                <li><strong>Statistical Integration</strong>: Summary statistics and reference lines</li>
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
                <li>Professional appearance for publications</li>
                </ul>
                </div>
                "
                
                self$results$todo$setContent(welcome_msg)
                
                # Hide results until data is provided
                self$results$summary$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
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
            
            # Main analysis pipeline with comprehensive error handling
            tryCatch({
                # Prepare and validate data
                data <- private$.cleanData()
                if (is.null(data)) return()
                
                # Calculate summary statistics
                summary_stats <- private$.calculateSummary(data)
                private$.populateSummary(summary_stats)
                
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
                stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
            }
            
            # Select and clean data
            data <- self$data[c(dep_var, group_var)]
            
            # Validate dependent variable (must be numeric)
            dep_data <- jmvcore::toNumeric(data[[dep_var]])
            if (all(is.na(dep_data))) {
                stop("Dependent variable must be numeric (continuous variable).")
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
                stop("Grouping variable must have at least 2 different categories.")
            }
            
            if (n_groups > 50) {
                warning("Grouping variable has more than 50 levels. Consider reducing categories for better visualization.")
            }
            
            # Remove rows with missing values
            complete_before <- nrow(data)
            data <- data[complete.cases(data), ]
            complete_after <- nrow(data)
            
            if (complete_after == 0) {
                stop("No complete cases found. Please check for missing values in selected variables.")
            }
            
            if (complete_after < complete_before) {
                n_removed <- complete_before - complete_after
                warning(paste(n_removed, "rows with missing values were removed from analysis."))
            }
            
            # Check minimum data requirements
            if (nrow(data) < 2) {
                stop("At least 2 complete observations are required for lollipop chart analysis.")
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
            
            if (sort_method == "value_asc") {
                data <- data[order(data[[dep_var]]), ]
            } else if (sort_method == "value_desc") {
                data <- data[order(-data[[dep_var]]), ]
            } else if (sort_method == "group_alpha") {
                data <- data[order(data[[group_var]]), ]
            }
            # "original" keeps the original order
            
            return(data)
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
            
            # Group information
            group_summary <- data %>%
                group_by(group) %>%
                summarise(
                    n = n(),
                    mean = mean(dependent, na.rm = TRUE),
                    .groups = 'drop'
                )
            
            summary_stats$group_names <- paste(unique(data$group), collapse = ", ")
            summary_stats$groups_with_highest <- group_summary$group[which.max(group_summary$mean)]
            summary_stats$groups_with_lowest <- group_summary$group[which.min(group_summary$mean)]
            
            return(summary_stats)
        },
        
        # Populate summary table
        .populateSummary = function(summary_stats) {
            table <- self$results$summary
            table$deleteRows()
            
            row_num <- 1
            
            # Data characteristics
            table$addRow(rowKey = row_num, values = list(
                statistic = "Number of Observations",
                value = as.character(summary_stats$n_observations)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Number of Groups",
                value = as.character(summary_stats$n_groups)
            ))
            row_num <- row_num + 1
            
            # Dependent variable statistics
            table$addRow(rowKey = row_num, values = list(
                statistic = "Mean Value",
                value = format(summary_stats$dep_mean, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Median Value",
                value = format(summary_stats$dep_median, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Standard Deviation",
                value = format(summary_stats$dep_sd, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Value Range",
                value = paste(format(summary_stats$dep_min, digits = 3), "-", 
                             format(summary_stats$dep_max, digits = 3))
            ))
            row_num <- row_num + 1
            
            # Group information
            table$addRow(rowKey = row_num, values = list(
                statistic = "Highest Value Group",
                value = as.character(summary_stats$groups_with_highest)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Lowest Value Group",
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
        
        # Main plotting function
        .plot = function(image, ggtheme, theme, ...) {
            # Get plot data
            plot_data <- image$state
            if (is.null(plot_data)) return()
            
            # Get options
            orientation <- self$options$orientation
            show_values <- self$options$showValues
            show_mean <- self$options$showMean
            highlight_level <- self$options$highlight
            point_size <- self$options$pointSize
            line_width <- self$options$lineWidth
            
            # Set up colors
            n_groups <- length(unique(plot_data$group))
            color_scheme <- private$.getColorScheme(n_groups, highlight_level)
            
            # Create base plot
            if (orientation == "horizontal") {
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dependent, y = group))
            } else {
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = dependent))
            }
            
            # Add lollipop elements
            if (color_scheme$has_highlight && !is.null(highlight_level)) {
                # Create highlight indicator
                plot_data$is_highlight <- plot_data$group == highlight_level
                
                if (orientation == "horizontal") {
                    p <- p + 
                        ggplot2::geom_segment(
                            ggplot2::aes(x = 0, xend = dependent, y = group, yend = group,
                                        color = is_highlight),
                            size = line_width
                        ) +
                        ggplot2::geom_point(
                            ggplot2::aes(color = is_highlight),
                            size = point_size
                        ) +
                        ggplot2::scale_color_manual(
                            values = c("TRUE" = color_scheme$highlight_color, 
                                     "FALSE" = color_scheme$normal_color),
                            guide = FALSE
                        )
                } else {
                    p <- p + 
                        ggplot2::geom_segment(
                            ggplot2::aes(x = group, xend = group, y = 0, yend = dependent,
                                        color = is_highlight),
                            size = line_width
                        ) +
                        ggplot2::geom_point(
                            ggplot2::aes(color = is_highlight),
                            size = point_size
                        ) +
                        ggplot2::scale_color_manual(
                            values = c("TRUE" = color_scheme$highlight_color, 
                                     "FALSE" = color_scheme$normal_color),
                            guide = FALSE
                        )
                }
            } else {
                # Regular coloring
                if (orientation == "horizontal") {
                    p <- p + 
                        ggplot2::geom_segment(
                            ggplot2::aes(x = 0, xend = dependent, y = group, yend = group),
                            color = color_scheme$colors[1],
                            size = line_width
                        ) +
                        ggplot2::geom_point(
                            color = color_scheme$colors[1],
                            size = point_size
                        )
                } else {
                    p <- p + 
                        ggplot2::geom_segment(
                            ggplot2::aes(x = group, xend = group, y = 0, yend = dependent),
                            color = color_scheme$colors[1],
                            size = line_width
                        ) +
                        ggplot2::geom_point(
                            color = color_scheme$colors[1],
                            size = point_size
                        )
                }
            }
            
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
                        size = 1
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = mean_value,
                        y = Inf,
                        label = paste("Mean =", round(mean_value, 2)),
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
                        size = 1
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = Inf,
                        y = mean_value,
                        label = paste("Mean =", round(mean_value, 2)),
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
                paste("Lollipop Chart:", dep_var, "by", group_var)
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
            # Set plot state for rendering
            self$results$plot$setState(data)
        }
    )
)