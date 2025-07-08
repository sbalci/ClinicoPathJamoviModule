#' @title Line Chart for Time Series and Trend Analysis
#' @description
#' Creates comprehensive line charts for time series analysis and trend visualization
#' in clinical and pathological research. This function supports multiple groups,
#' confidence intervals, trend lines, and statistical overlays, making it ideal for
#' analyzing longitudinal data, treatment responses, and biomarker trends over time.
#' 
#' @details
#' The line chart function is designed specifically for clinical research applications
#' where visualization of trends and patterns over time or ordered categories is crucial.
#' It provides extensive customization options and statistical features to create
#' publication-ready plots for clinical studies.
#' 
#' Key features:
#' - Multiple group support for comparative analysis
#' - Confidence intervals and trend lines
#' - Clinical color palettes and themes
#' - Reference lines for normal ranges/thresholds
#' - Statistical correlation analysis
#' - Professional publication-ready appearance
#' 
#' Common clinical applications:
#' - Laboratory values over time
#' - Treatment response monitoring
#' - Biomarker evolution
#' - Dose-response relationships
#' - Survival probability trends
#' - Quality metrics tracking
#' 
#' @examples
#' \dontrun{
#' # Basic time series analysis
#' result <- linechart(
#'   data = patient_data,
#'   xvar = "visit_month",
#'   yvar = "hemoglobin_level"
#' )
#' 
#' # Grouped analysis with confidence intervals
#' result <- linechart(
#'   data = treatment_data,
#'   xvar = "time_point",
#'   yvar = "tumor_size",
#'   groupby = "treatment_arm",
#'   confidence = TRUE,
#'   trendline = TRUE
#' )
#' 
#' # Clinical monitoring with reference line
#' result <- linechart(
#'   data = lab_data,
#'   xvar = "days_post_treatment",
#'   yvar = "white_blood_cell_count",
#'   refline = 4000,
#'   reflineLabel = "Normal Lower Limit"
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore

linechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linechartClass",
    inherit = linechartBase,
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
            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
                welcome_msg <- "
                <div class='alert alert-info'>
                <h4>Welcome to Line Chart Analysis</h4>
                <p>This function creates comprehensive line charts for time series and trend analysis.</p>
                
                <h5>Required inputs:</h5>
                <ul>
                <li><strong>X-axis Variable</strong>: Time points, sequence, or ordered categories</li>
                <li><strong>Y-axis Variable</strong>: Numeric values to plot (continuous variable)</li>
                </ul>
                
                <h5>Optional features:</h5>
                <ul>
                <li><strong>Group By</strong>: Create multiple lines by categories (treatment groups, patient types)</li>
                <li><strong>Confidence Intervals</strong>: Display uncertainty around lines</li>
                <li><strong>Trend Lines</strong>: Add linear regression lines</li>
                <li><strong>Reference Lines</strong>: Mark normal ranges or thresholds</li>
                <li><strong>Statistical Analysis</strong>: Correlation and trend analysis</li>
                </ul>
                
                <h5>Clinical applications:</h5>
                <ul>
                <li>Laboratory values over time</li>
                <li>Treatment response monitoring</li>
                <li>Biomarker evolution</li>
                <li>Dose-response relationships</li>
                <li>Quality metrics tracking</li>
                </ul>
                </div>
                "
                
                self$results$todo$setContent(welcome_msg)
                
                # Hide results until data is provided
                self$results$summary$setVisible(FALSE)
                self$results$correlation$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
            }
        },
        
        .run = function() {
            # Early exits for missing data or variables
            if (is.null(self$data) || nrow(self$data) == 0) {
                return()
            }
            
            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
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
                
                # Calculate correlation if trend line requested
                if (self$options$trendline) {
                    correlation_stats <- private$.calculateCorrelation(data)
                    private$.populateCorrelation(correlation_stats)
                    self$results$correlation$setVisible(TRUE)
                } else {
                    self$results$correlation$setVisible(FALSE)
                }
                
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
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby
            
            # Select required columns
            required_vars <- c(xvar, yvar)
            if (!is.null(groupby)) {
                required_vars <- c(required_vars, groupby)
            }
            
            # Check if variables exist in data
            missing_vars <- setdiff(required_vars, names(self$data))
            if (length(missing_vars) > 0) {
                stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
            }
            
            # Select and clean data
            data <- self$data[required_vars]
            
            # Validate X variable
            x_data <- data[[xvar]]
            if (is.character(x_data)) {
                # Try to convert to factor with meaningful ordering
                unique_values <- unique(x_data)
                if (all(grepl("^[0-9.-]+$", unique_values, perl = TRUE))) {
                    # Looks numeric, convert
                    x_data <- as.numeric(x_data)
                    if (any(is.na(x_data))) {
                        stop("X-axis variable contains non-numeric values that cannot be converted.")
                    }
                } else {
                    # Convert to ordered factor
                    x_data <- factor(x_data)
                }
                data[[xvar]] <- x_data
            } else if (is.factor(x_data)) {
                # Check if factor levels are meaningful for ordering
                levels_numeric <- suppressWarnings(as.numeric(levels(x_data)))
                if (!any(is.na(levels_numeric))) {
                    # Reorder factor levels numerically
                    data[[xvar]] <- factor(x_data, levels = levels(x_data)[order(levels_numeric)])
                }
            }
            
            # Validate Y variable (must be numeric)
            y_data <- jmvcore::toNumeric(data[[yvar]])
            if (all(is.na(y_data))) {
                stop("Y-axis variable must be numeric (continuous variable).")
            }
            data[[yvar]] <- y_data
            
            # Validate grouping variable if provided
            if (!is.null(groupby)) {
                group_data <- data[[groupby]]
                if (is.character(group_data)) {
                    data[[groupby]] <- factor(group_data)
                } else if (!is.factor(group_data)) {
                    data[[groupby]] <- factor(group_data)
                }
                
                # Check number of groups
                n_groups <- length(unique(data[[groupby]]))
                if (n_groups > 10) {
                    warning("Grouping variable has more than 10 levels. Consider reducing groups for clarity.")
                }
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
            if (nrow(data) < 3) {
                stop("At least 3 complete observations are required for line chart analysis.")
            }
            
            # Check for sufficient variation in Y variable
            if (var(data[[yvar]], na.rm = TRUE) == 0) {
                stop("Y-axis variable has no variation (all values are identical).")
            }
            
            # Validate reference line if provided
            if (!is.null(self$options$refline) && !is.na(self$options$refline)) {
                refline_value <- as.numeric(self$options$refline)
                if (is.na(refline_value)) {
                    warning("Reference line value is not numeric and will be ignored.")
                }
            }
            
            return(data)
        },
        
        # Calculate summary statistics
        .calculateSummary = function(data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby
            
            summary_stats <- list()
            
            # Basic data information
            summary_stats$n_observations <- nrow(data)
            summary_stats$n_x_points <- length(unique(data[[xvar]]))
            
            # Y variable statistics
            y_data <- data[[yvar]]
            summary_stats$y_mean <- mean(y_data, na.rm = TRUE)
            summary_stats$y_median <- median(y_data, na.rm = TRUE)
            summary_stats$y_sd <- sd(y_data, na.rm = TRUE)
            summary_stats$y_min <- min(y_data, na.rm = TRUE)
            summary_stats$y_max <- max(y_data, na.rm = TRUE)
            summary_stats$y_range <- summary_stats$y_max - summary_stats$y_min
            
            # Group information if applicable
            if (!is.null(groupby)) {
                summary_stats$n_groups <- length(unique(data[[groupby]]))
                summary_stats$group_names <- paste(unique(data[[groupby]]), collapse = ", ")
            } else {
                summary_stats$n_groups <- 1
                summary_stats$group_names <- "Single group"
            }
            
            return(summary_stats)
        },
        
        # Calculate correlation statistics
        .calculateCorrelation = function(data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby
            
            correlation_stats <- list()
            
            # Overall correlation (if X is numeric)
            x_data <- data[[xvar]]
            y_data <- data[[yvar]]
            
            if (is.numeric(x_data)) {
                # Pearson correlation
                cor_result <- cor.test(x_data, y_data, method = "pearson")
                correlation_stats$pearson_r <- cor_result$estimate
                correlation_stats$pearson_p <- cor_result$p.value
                correlation_stats$pearson_ci_lower <- cor_result$conf.int[1]
                correlation_stats$pearson_ci_upper <- cor_result$conf.int[2]
                
                # Spearman correlation (rank-based)
                cor_spearman <- cor.test(x_data, y_data, method = "spearman")
                correlation_stats$spearman_r <- cor_spearman$estimate
                correlation_stats$spearman_p <- cor_spearman$p.value
                
                # Linear regression statistics
                lm_result <- lm(y_data ~ x_data)
                correlation_stats$slope <- coef(lm_result)[2]
                correlation_stats$intercept <- coef(lm_result)[1]
                correlation_stats$r_squared <- summary(lm_result)$r.squared
                correlation_stats$regression_p <- summary(lm_result)$coefficients[2, 4]
                
            } else {
                # For categorical X, calculate trend test if ordered
                if (is.ordered(x_data) || is.factor(x_data)) {
                    # Jonckheere-Terpstra trend test would be ideal here
                    # For now, use basic ANOVA
                    anova_result <- anova(lm(y_data ~ x_data))
                    correlation_stats$anova_f <- anova_result$`F value`[1]
                    correlation_stats$anova_p <- anova_result$`Pr(>F)`[1]
                }
            }
            
            return(correlation_stats)
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
                statistic = "Number of X-axis Points",
                value = as.character(summary_stats$n_x_points)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Number of Groups",
                value = as.character(summary_stats$n_groups)
            ))
            row_num <- row_num + 1
            
            if (summary_stats$n_groups > 1) {
                table$addRow(rowKey = row_num, values = list(
                    statistic = "Group Names",
                    value = summary_stats$group_names
                ))
                row_num <- row_num + 1
            }
            
            # Y variable statistics
            table$addRow(rowKey = row_num, values = list(
                statistic = "Y Mean",
                value = format(summary_stats$y_mean, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Y Median",
                value = format(summary_stats$y_median, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Y Standard Deviation",
                value = format(summary_stats$y_sd, digits = 3)
            ))
            row_num <- row_num + 1
            
            table$addRow(rowKey = row_num, values = list(
                statistic = "Y Range",
                value = paste(format(summary_stats$y_min, digits = 3), "-", 
                             format(summary_stats$y_max, digits = 3))
            ))
        },
        
        # Populate correlation table
        .populateCorrelation = function(correlation_stats) {
            table <- self$results$correlation
            table$deleteRows()
            
            row_num <- 1
            
            # Pearson correlation
            if (!is.null(correlation_stats$pearson_r)) {
                table$addRow(rowKey = row_num, values = list(
                    measure = "Pearson Correlation",
                    value = correlation_stats$pearson_r,
                    interpretation = private$.interpretCorrelation(correlation_stats$pearson_r, 
                                                                   correlation_stats$pearson_p)
                ))
                row_num <- row_num + 1
                
                table$addRow(rowKey = row_num, values = list(
                    measure = "R-squared",
                    value = correlation_stats$r_squared,
                    interpretation = paste0(round(correlation_stats$r_squared * 100, 1), 
                                          "% of variance explained")
                ))
                row_num <- row_num + 1
                
                table$addRow(rowKey = row_num, values = list(
                    measure = "Regression Slope",
                    value = correlation_stats$slope,
                    interpretation = if (correlation_stats$slope > 0) "Positive trend" else "Negative trend"
                ))
                row_num <- row_num + 1
            }
            
            # Spearman correlation
            if (!is.null(correlation_stats$spearman_r)) {
                table$addRow(rowKey = row_num, values = list(
                    measure = "Spearman Correlation",
                    value = correlation_stats$spearman_r,
                    interpretation = private$.interpretCorrelation(correlation_stats$spearman_r, 
                                                                   correlation_stats$spearman_p)
                ))
                row_num <- row_num + 1
            }
            
            # ANOVA for categorical X
            if (!is.null(correlation_stats$anova_f)) {
                table$addRow(rowKey = row_num, values = list(
                    measure = "ANOVA F-statistic",
                    value = correlation_stats$anova_f,
                    interpretation = if (correlation_stats$anova_p < 0.05) 
                                       "Significant group differences" else "No significant differences"
                ))
            }
        },
        
        # Interpret correlation values
        .interpretCorrelation = function(r, p_value) {
            if (is.na(r) || is.na(p_value)) return("Not available")
            
            # Significance
            sig_text <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else if (p_value < 0.05) "*" else "ns"
            
            # Strength interpretation
            abs_r <- abs(r)
            strength <- if (abs_r < 0.1) "negligible" else
                       if (abs_r < 0.3) "weak" else
                       if (abs_r < 0.5) "moderate" else
                       if (abs_r < 0.7) "strong" else "very strong"
            
            direction <- if (r > 0) "positive" else "negative"
            
            return(paste0(strength, " ", direction, " correlation (", sig_text, ")"))
        },
        
        # Get color palette
        .getColorPalette = function(n_colors) {
            palette_name <- self$options$colorPalette
            
            if (n_colors == 1) {
                return("#2E86AB")  # Single color
            }
            
            switch(palette_name,
                "default" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A"),
                "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                "viridis" = c("#440154", "#31688e", "#35b779", "#fde725"),
                "clinical" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
                c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")  # fallback
            )[1:min(n_colors, 6)]
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
                        legend.position = "bottom"
                    ),
                ggplot2::theme_gray()  # fallback
            )
            
            # Add custom modifications for clinical publications
            if (theme_name == "publication") {
                base_theme <- base_theme + 
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 12),
                        axis.title = ggplot2::element_text(size = 14),
                        plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 11)
                    )
            }
            
            return(base_theme)
        },
        
        # Main plotting function
        .plot = function(image, ggtheme, theme, ...) {
            # Get plot data
            plot_data <- image$state
            if (is.null(plot_data)) return()
            
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby
            
            # Create base plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = xvar, y = yvar))
            
            # Add grouping if specified
            if (!is.null(groupby)) {
                n_groups <- length(unique(plot_data[[groupby]]))
                colors <- private$.getColorPalette(n_groups)
                
                # Add lines with grouping
                p <- p + ggplot2::aes_string(color = groupby, group = groupby)
                
                # Add confidence intervals if requested
                if (self$options$confidence) {
                    if (self$options$smooth) {
                        p <- p + ggplot2::geom_smooth(method = "loess", alpha = 0.2)
                    } else {
                        p <- p + ggplot2::geom_smooth(method = "lm", alpha = 0.2)
                    }
                }
                
                # Add main lines
                if (self$options$smooth) {
                    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, size = 1.2)
                } else {
                    p <- p + ggplot2::geom_line(size = 1.2)
                }
                
                # Add points if requested
                if (self$options$points) {
                    p <- p + ggplot2::geom_point(size = 2.5, alpha = 0.8)
                }
                
                # Set colors
                p <- p + ggplot2::scale_color_manual(values = colors)
                
            } else {
                # Single group
                color <- private$.getColorPalette(1)
                
                # Add confidence interval if requested
                if (self$options$confidence) {
                    if (self$options$smooth) {
                        p <- p + ggplot2::geom_smooth(method = "loess", alpha = 0.2, color = color, fill = color)
                    } else {
                        p <- p + ggplot2::geom_smooth(method = "lm", alpha = 0.2, color = color, fill = color)
                    }
                }
                
                # Add main line
                if (self$options$smooth) {
                    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, color = color, size = 1.2)
                } else {
                    p <- p + ggplot2::geom_line(color = color, size = 1.2)
                }
                
                # Add points if requested
                if (self$options$points) {
                    p <- p + ggplot2::geom_point(color = color, size = 2.5, alpha = 0.8)
                }
            }
            
            # Add trend line if requested
            if (self$options$trendline && is.numeric(plot_data[[xvar]])) {
                if (!is.null(groupby)) {
                    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed", alpha = 0.7)
                } else {
                    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed", 
                                                 color = "black", alpha = 0.7)
                }
            }
            
            # Add reference line if specified
            if (!is.null(self$options$refline) && !is.na(self$options$refline)) {
                refline_value <- as.numeric(self$options$refline)
                if (!is.na(refline_value)) {
                    refline_label <- if (!is.null(self$options$reflineLabel) && 
                                        nchar(self$options$reflineLabel) > 0) {
                        self$options$reflineLabel
                    } else {
                        "Reference"
                    }
                    
                    p <- p + ggplot2::geom_hline(yintercept = refline_value, 
                                                linetype = "dotted", color = "red", size = 1) +
                             ggplot2::annotate("text", x = Inf, y = refline_value, 
                                              label = refline_label, hjust = 1.1, vjust = -0.5, 
                                              color = "red", size = 3)
                }
            }
            
            # Add labels
            xlabel <- if (!is.null(self$options$xlabel) && nchar(self$options$xlabel) > 0) {
                self$options$xlabel
            } else {
                xvar
            }
            
            ylabel <- if (!is.null(self$options$ylabel) && nchar(self$options$ylabel) > 0) {
                self$options$ylabel
            } else {
                yvar
            }
            
            plot_title <- if (!is.null(self$options$title) && nchar(self$options$title) > 0) {
                self$options$title
            } else {
                if (!is.null(groupby)) {
                    paste(ylabel, "by", xlabel, "and", groupby)
                } else {
                    paste(ylabel, "by", xlabel)
                }
            }
            
            p <- p + ggplot2::labs(
                x = xlabel,
                y = ylabel,
                title = plot_title,
                color = if (!is.null(groupby)) groupby else NULL
            )
            
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