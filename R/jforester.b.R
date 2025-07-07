#' @title Forest Plot Visualization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @export

jforesterClass <- R6::R6Class(
    "jforesterClass",
    inherit = jforesterBase,
    private = list(
        .forest_data = NULL,
        .summary_stats = NULL,
        
        .init = function() {
            # Check for required variables and show instructions
            if (is.null(self$options$study_labels) || 
                is.null(self$options$estimates) ||
                is.null(self$options$ci_lower) ||
                is.null(self$options$ci_upper)) {
                private$.showInstructions("setup")
            } else {
                private$.showInstructions("ready")
            }
        },
        
        .run = function() {
            # Check if we have required variables
            if (!private$.hasRequiredVariables()) {
                return()
            }
            
            # Prepare data for forest plot
            data <- private$.prepareForestData()
            if (is.null(data)) return()
            
            # Populate results tables
            private$.populateDataTable(data)
            private$.populateSummaryStats(data)
            private$.populateInterpretation()
        },
        
        .hasRequiredVariables = function() {
            !is.null(self$options$study_labels) && 
            !is.null(self$options$estimates) &&
            !is.null(self$options$ci_lower) &&
            !is.null(self$options$ci_upper)
        },
        
        .showInstructions = function(type) {
            instructions <- switch(type,
                "setup" = private$.createInstructionsHTML("setup"),
                "ready" = private$.createInstructionsHTML("ready")
            )
            
            self$results$instructions$setContent(instructions)
        },
        
        .createInstructionsHTML = function(type) {
            base_style <- "
            <html>
            <head>
            <style>
                .main {
                    margin: 20px;
                    font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;
                }
                .header {
                    color: #2e7d32;
                    font-size: 18px;
                    font-weight: bold;
                    margin-bottom: 10px;
                }
                .description {
                    margin-bottom: 20px;
                    line-height: 1.5;
                }
                .requirements {
                    background-color: #f5f5f5;
                    padding: 15px;
                    border-left: 4px solid #4caf50;
                    margin-bottom: 20px;
                }
                .steps {
                    counter-reset: step-counter;
                }
                .step {
                    counter-increment: step-counter;
                    margin-bottom: 10px;
                    padding-left: 30px;
                    position: relative;
                }
                .step::before {
                    content: counter(step-counter);
                    position: absolute;
                    left: 0;
                    background-color: #4caf50;
                    color: white;
                    width: 20px;
                    height: 20px;
                    border-radius: 50%;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    font-size: 12px;
                    font-weight: bold;
                }
            </style>
            </head>
            <body>
            <div class='main'>
                <div class='header'>📊 Forest Plot Visualization</div>"
            
            content <- switch(type,
                "setup" = "
                <div class='description'>
                    Create publication-ready forest plots for meta-analyses, clinical trials, 
                    and systematic reviews. Display point estimates with confidence intervals 
                    in a professional format.
                </div>
                <div class='requirements'>
                    <strong>Required Variables:</strong><br>
                    • Study/Group Labels: Names or identifiers for each study/group<br>
                    • Effect Estimates: Point estimates (odds ratios, mean differences, etc.)<br>
                    • Lower CI: Lower confidence interval bounds<br>
                    • Upper CI: Upper confidence interval bounds
                </div>
                <div class='steps'>
                    <div class='step'>Select study or group labels variable</div>
                    <div class='step'>Select effect estimates variable</div>
                    <div class='step'>Select lower confidence interval variable</div>
                    <div class='step'>Select upper confidence interval variable</div>
                    <div class='step'>Choose effect type and customize appearance</div>
                </div>",
                
                "ready" = "
                <div class='description'>
                    Forest plot ready! Your data includes point estimates with confidence intervals 
                    for visualization. Customize colors, labels, and layout options as needed.
                </div>
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;'>
                    <strong>✓ Analysis configured correctly</strong><br>
                    Review your forest plot settings and examine the results below.
                </div>"
            )
            
            footer <- "
                <div style='margin-top: 20px; font-style: italic; color: #666;'>
                    Uses ggplot2 and forestmodel packages for professional forest plot generation.
                </div>
            </div>
            </body>
            </html>"
            
            paste0(base_style, content, footer)
        },
        
        .prepareForestData = function() {
            data <- self$data
            
            # Get required variables
            study_var <- self$options$study_labels
            estimate_var <- self$options$estimates
            ci_lower_var <- self$options$ci_lower
            ci_upper_var <- self$options$ci_upper
            
            # Get optional variables
            sample_var <- self$options$sample_sizes
            events_var <- self$options$events
            
            # Extract data
            forest_data <- data.frame(
                study = as.character(data[[study_var]]),
                estimate = as.numeric(data[[estimate_var]]),
                ci_lower = as.numeric(data[[ci_lower_var]]),
                ci_upper = as.numeric(data[[ci_upper_var]]),
                stringsAsFactors = FALSE
            )
            
            # Add optional variables
            if (!is.null(sample_var)) {
                forest_data$sample_size <- as.numeric(data[[sample_var]])
            } else {
                forest_data$sample_size <- NA
            }
            
            if (!is.null(events_var)) {
                forest_data$events <- as.numeric(data[[events_var]])
            } else {
                forest_data$events <- NA
            }
            
            # Remove rows with missing required data
            complete_rows <- complete.cases(forest_data[c("study", "estimate", "ci_lower", "ci_upper")])
            forest_data <- forest_data[complete_rows, ]
            
            if (nrow(forest_data) == 0) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ No complete data</strong><br>
                    All required variables have missing values.
                    </div>"
                )
                return(NULL)
            }
            
            # Calculate weights if sample sizes available
            if (self$options$include_weights && any(!is.na(forest_data$sample_size))) {
                total_n <- sum(forest_data$sample_size, na.rm = TRUE)
                forest_data$weight <- (forest_data$sample_size / total_n) * 100
            } else {
                forest_data$weight <- NA
            }
            
            private$.forest_data <- forest_data
            return(forest_data)
        },
        
        .populateDataTable = function(data) {
            if (!self$options$show_table) return()
            
            table <- self$results$data_table
            
            for (i in 1:nrow(data)) {
                table$addRow(rowKey = i, values = list(
                    study = data$study[i],
                    estimate = data$estimate[i],
                    ci_lower = data$ci_lower[i],
                    ci_upper = data$ci_upper[i],
                    sample_size = if (!is.na(data$sample_size[i])) data$sample_size[i] else NULL,
                    events = if (!is.na(data$events[i])) data$events[i] else NULL,
                    weight = if (!is.na(data$weight[i])) data$weight[i] else NULL
                ))
            }
        },
        
        .populateSummaryStats = function(data) {
            if (!self$options$show_summary && !self$options$show_heterogeneity) return()
            
            table <- self$results$summary_statistics
            
            if (self$options$show_summary) {
                # Add summary effect statistics
                table$addRow(rowKey = "summary_est", values = list(
                    statistic = "Summary Estimate",
                    value = sprintf("%.3f", self$options$summary_estimate)
                ))
                
                ci_text <- sprintf("%.3f to %.3f", 
                                 self$options$summary_ci_lower,
                                 self$options$summary_ci_upper)
                table$addRow(rowKey = "summary_ci", values = list(
                    statistic = paste0(self$options$confidence_level, "% CI"),
                    value = ci_text
                ))
            }
            
            if (self$options$show_heterogeneity) {
                # Calculate comprehensive heterogeneity statistics
                estimates <- data$estimate
                n_studies <- length(estimates)
                
                if (n_studies > 1) {
                    # Calculate weights (inverse variance if sample sizes available)
                    weights <- if (any(!is.na(data$sample_size))) {
                        data$sample_size / sum(data$sample_size, na.rm = TRUE)
                    } else {
                        rep(1/n_studies, n_studies)  # Equal weights
                    }
                    
                    # Calculate weighted mean
                    weighted_mean <- sum(estimates * weights, na.rm = TRUE)
                    
                    # Calculate Q statistic (Cochran's Q test)
                    Q <- sum(weights * (estimates - weighted_mean)^2, na.rm = TRUE)
                    df <- n_studies - 1
                    
                    # Calculate I² statistic
                    I_squared <- max(0, (Q - df) / Q) * 100
                    
                    # P-value for Q test (approximate)
                    Q_p_value <- if (df > 0) pchisq(Q, df, lower.tail = FALSE) else NA
                    
                    # Add statistics to table
                    table$addRow(rowKey = "n_studies", values = list(
                        statistic = "Number of Studies",
                        value = as.character(n_studies)
                    ))
                    
                    table$addRow(rowKey = "range", values = list(
                        statistic = "Range of Estimates",
                        value = sprintf("%.3f to %.3f", min(estimates, na.rm = TRUE), max(estimates, na.rm = TRUE))
                    ))
                    
                    table$addRow(rowKey = "q_statistic", values = list(
                        statistic = "Q Statistic",
                        value = sprintf("%.2f (df = %d)", Q, df)
                    ))
                    
                    if (!is.na(Q_p_value)) {
                        table$addRow(rowKey = "q_p_value", values = list(
                            statistic = "Q Test P-value",
                            value = if (Q_p_value < 0.001) "< 0.001" else sprintf("%.3f", Q_p_value)
                        ))
                    }
                    
                    table$addRow(rowKey = "i_squared", values = list(
                        statistic = "I² (Heterogeneity)",
                        value = sprintf("%.1f%%", I_squared)
                    ))
                    
                    # Interpretation of I²
                    i_squared_interpretation <- if (I_squared < 25) {
                        "Low heterogeneity"
                    } else if (I_squared < 50) {
                        "Moderate heterogeneity"
                    } else if (I_squared < 75) {
                        "Substantial heterogeneity"
                    } else {
                        "Considerable heterogeneity"
                    }
                    
                    table$addRow(rowKey = "heterogeneity_interpretation", values = list(
                        statistic = "Heterogeneity Assessment",
                        value = i_squared_interpretation
                    ))
                }
            }
        },
        
        .populateInterpretation = function() {
            effect_type <- self$options$effect_type
            
            # Create interpretation based on effect type
            effect_interpretation <- switch(effect_type,
                "or" = "Odds ratios > 1 indicate increased odds in the treatment group.",
                "rr" = "Risk ratios > 1 indicate increased risk in the treatment group.",
                "hr" = "Hazard ratios > 1 indicate increased hazard in the treatment group.",
                "md" = "Mean differences > 0 indicate higher values in the treatment group.",
                "smd" = "Standardized mean differences > 0 indicate higher values in the treatment group.",
                "Custom effect measure - interpret based on your specific context."
            )
            
            interpretation_html <- paste0("
            <div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>
                <h3 style='color: #2e7d32; margin-bottom: 15px;'>📊 Forest Plot Interpretation</h3>
                
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50; margin-bottom: 15px;'>
                    <strong>Effect Measure: ", effect_type, "</strong><br>
                    ", effect_interpretation, "
                </div>
                
                <div style='background-color: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0; margin-bottom: 15px;'>
                    <strong>Reading the Forest Plot:</strong><br>
                    • Each horizontal line represents a study with point estimate and confidence interval<br>
                    • Point size may reflect sample size or study weight<br>
                    • Vertical reference line typically represents 'no effect' (", self$options$reference_line, ")<br>
                    • Confidence intervals crossing the reference line suggest non-significance
                </div>
                
                <div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #ff9800;'>
                    <strong>⚠️ Important Considerations:</strong><br>
                    • Consider heterogeneity between studies when interpreting results<br>
                    • Check for outliers or studies with unusually wide confidence intervals<br>
                    • Publication bias may affect the pattern of results<br>
                    • Clinical significance may differ from statistical significance
                </div>
            </div>")
            
            self$results$interpretation$setContent(interpretation_html)
        },
        
        .plot_forest = function(image, ggtheme, theme, ...) {
            if (is.null(private$.forest_data)) return()
            
            # Check if we can create a forest plot with available packages
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "ggplot2 package required"), 
                                     size = 5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(plot)
                return(TRUE)
            }
            
            tryCatch({
                data <- private$.forest_data
                
                # Prepare data for plotting
                data$study_num <- nrow(data):1  # Reverse order for plotting
                
                # Add summary row if requested
                if (self$options$show_summary) {
                    summary_row <- data.frame(
                        study = "Summary",
                        estimate = self$options$summary_estimate,
                        ci_lower = self$options$summary_ci_lower,
                        ci_upper = self$options$summary_ci_upper,
                        sample_size = NA,
                        events = NA,
                        weight = NA,
                        study_num = 0,
                        stringsAsFactors = FALSE
                    )
                    data <- rbind(data, summary_row)
                }
                
                # Set up colors
                colors <- private$.getColorScheme()
                point_color <- colors$point
                ci_color <- colors$ci
                
                # Set up point sizes
                size_range <- private$.getPointSizeRange()
                
                # Create base plot
                plot <- ggplot2::ggplot(data, ggplot2::aes(x = estimate, y = study_num)) +
                    ggplot2::geom_vline(xintercept = self$options$reference_line, 
                                      linetype = "dashed", color = "gray50", size = 0.8)
                
                # Add confidence intervals
                plot <- plot + ggplot2::geom_errorbarh(
                    ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                    height = 0.3, color = ci_color, size = 0.8
                )
                
                # Add points with size based on sample size if available
                if (any(!is.na(data$sample_size))) {
                    plot <- plot + ggplot2::geom_point(
                        ggplot2::aes(size = sample_size), 
                        color = point_color, 
                        shape = 18
                    ) +
                    ggplot2::scale_size_continuous(
                        range = size_range,
                        guide = ggplot2::guide_legend(title = "Sample Size")
                    )
                } else {
                    plot <- plot + ggplot2::geom_point(
                        color = point_color, 
                        size = size_range[2], 
                        shape = 18
                    )
                }
                
                # Customize scales
                if (self$options$log_scale && all(data$estimate > 0, na.rm = TRUE)) {
                    plot <- plot + ggplot2::scale_x_log10()
                }
                
                # Add labels
                x_label <- if (self$options$x_axis_label != "") {
                    self$options$x_axis_label
                } else {
                    private$.getDefaultXLabel()
                }
                
                plot_title <- if (self$options$plot_title != "") {
                    self$options$plot_title
                } else {
                    "Forest Plot"
                }
                
                plot <- plot + ggplot2::scale_y_continuous(
                    breaks = data$study_num,
                    labels = data$study,
                    limits = c(-0.5, max(data$study_num) + 0.5)
                ) +
                ggplot2::labs(
                    x = x_label,
                    y = "",
                    title = plot_title
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.y = ggplot2::element_text(hjust = 1),
                    axis.ticks.y = ggplot2::element_blank(),
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(hjust = 0.5),
                    text = ggplot2::element_text(family = self$options$font_family)
                )
                
                # Add arrows if requested
                if (self$options$arrow_labels) {
                    # Calculate plot limits for arrow positioning
                    x_range <- range(c(data$estimate, data$ci_lower, data$ci_upper), na.rm = TRUE)
                    x_span <- x_range[2] - x_range[1]
                    y_bottom <- -0.8
                    
                    # Position arrows at the bottom of the plot
                    left_x <- x_range[1] + 0.1 * x_span
                    right_x <- x_range[2] - 0.1 * x_span
                    
                    plot <- plot + 
                        ggplot2::annotate("text", 
                                        x = left_x, 
                                        y = y_bottom, 
                                        label = paste0("← ", self$options$left_arrow_label),
                                        hjust = 0, 
                                        size = 3.5, 
                                        color = "gray40") +
                        ggplot2::annotate("text", 
                                        x = right_x, 
                                        y = y_bottom, 
                                        label = paste0(self$options$right_arrow_label, " →"),
                                        hjust = 1, 
                                        size = 3.5, 
                                        color = "gray40")
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = paste("Error:", e$message)), 
                                     size = 4) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(plot)
                TRUE
            })
        },
        
        .getColorScheme = function() {
            scheme <- self$options$color_scheme
            
            switch(scheme,
                "default" = list(point = "#2166AC", ci = "#4D4D4D"),
                "medical" = list(point = "#D32F2F", ci = "#757575"),
                "forest" = list(point = "#388E3C", ci = "#616161"),
                "grayscale" = list(point = "#424242", ci = "#757575"),
                "custom" = list(
                    point = self$options$custom_point_color,
                    ci = self$options$custom_ci_color
                ),
                list(point = "#2166AC", ci = "#4D4D4D")  # fallback
            )
        },
        
        .getPointSizeRange = function() {
            range_type <- self$options$point_size_range
            
            switch(range_type,
                "small" = c(2, 4),
                "medium" = c(3, 6),
                "large" = c(4, 8),
                c(3, 6)  # fallback
            )
        },
        
        .getDefaultXLabel = function() {
            effect_type <- self$options$effect_type
            
            switch(effect_type,
                "or" = "Odds Ratio",
                "rr" = "Risk Ratio", 
                "hr" = "Hazard Ratio",
                "md" = "Mean Difference",
                "smd" = "Standardized Mean Difference",
                "Effect Estimate"
            )
        }
    )
)