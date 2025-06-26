#' @title Advanced Bar Charts - 5 Ways
#' @description
#' Advanced bar chart visualization module implementing 5 different approaches 
#' for creating professional bar charts. Inspired by "A Bar Chart 5 Ways" methodology,
#' each approach is optimized for different use cases in clinical research.
#'
#' @details
#' This module provides 5 distinct approaches to bar chart visualization:
#' 1. Basic ggplot2: Clean, straightforward implementation
#' 2. Polished Presentation: Enhanced styling for presentations
#' 3. Statistical Annotations: Integrated statistical tests and annotations
#' 4. Interactive Plotly: Interactive web-based visualization
#' 5. Publication Ready: Journal-quality formatting and styling
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_bar geom_col stat_summary position_dodge
#' @importFrom ggplot2 theme_minimal theme_classic theme_bw labs scale_fill_brewer
#' @importFrom ggplot2 scale_fill_viridis_d coord_flip geom_text element_text
#' @importFrom ggplot2 theme element_rect element_line margin
#' @importFrom dplyr group_by summarise mutate arrange
#' @importFrom plotly ggplotly plot_ly
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis scale_fill_viridis_d
#' @importFrom scales percent comma
#' @importFrom stats aov t.test wilcox.test chisq.test kruskal.test
#' @importFrom stringr str_to_title
#' @export

advancedbarplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "advancedbarplotClass",
    inherit = advancedbarplotBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .summary_stats = NULL,
        .statistical_results = NULL,
        
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #1565c0; margin-top: 0;'>Advanced Bar Charts - 5 Professional Approaches</h3>",
                "<div style='margin: 10px 0;'>",
                "<p><strong>Choose from 5 different bar chart approaches, each optimized for specific use cases:</strong></p>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Basic ggplot2:</strong> Clean, straightforward implementation for data exploration</li>",
                "<li><strong>Polished Presentation:</strong> Enhanced styling perfect for presentations and reports</li>",
                "<li><strong>Statistical Annotations:</strong> Integrated statistical tests with automated annotations</li>",
                "<li><strong>Interactive Plotly:</strong> Web-based interactive visualization for exploration</li>",
                "<li><strong>Publication Ready:</strong> Journal-quality formatting meeting publication standards</li>",
                "</ol>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #1976d2; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Select X Variable:</strong> Choose categorical variable for x-axis</li>",
                "<li><strong>Select Y Variable:</strong> Choose numeric variable for bar heights</li>",
                "<li><strong>Choose Approach:</strong> Select one of the 5 visualization approaches</li>",
                "<li><strong>Customize:</strong> Adjust colors, statistics, and styling options</li>",
                "</ol>",
                "</div>",
                "<div style='background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #ef6c00;'><strong>Pro Tip:</strong> Each approach includes automatic code generation to help you reproduce the visualization in R.</p>",
                "</div>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
            
            # Set plot dimensions
            plot_width <- self$options$plot_width * 100
            plot_height <- self$options$plot_height * 100
            self$results$main_plot$setSize(plot_width, plot_height)
        },
        
        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$x_var) && !is.null(self$options$y_var)) {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (is.null(self$options$x_var)) {
                return()
            }
            
            if (is.null(self$options$y_var)) {
                return()
            }
            
            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Process data
            private$.processed_data <- private$.process_data()
            
            # Generate approach description
            private$.generate_approach_description()
            
            # Generate summary statistics
            private$.generate_summary_stats()
            
            # Generate statistical results if requested
            if (self$options$add_statistics) {
                private$.generate_statistical_results()
            }
            
            # Generate R code example
            private$.generate_code_example()
            
            # Generate interpretation guide
            private$.generate_interpretation_guide()
            
            # Set plot state for rendering
            self$results$main_plot$setState(private$.processed_data)
        },
        
        .process_data = function() {
            mydata <- self$data
            
            # Clean variable names and handle labels
            original_names <- names(mydata)
            labels <- setNames(original_names, original_names)
            
            # Get variable names
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            fill_var <- self$options$fill_var
            facet_var <- self$options$facet_var
            
            # Select relevant variables
            vars_to_select <- c(x_var, y_var)
            if (!is.null(fill_var)) vars_to_select <- c(vars_to_select, fill_var)
            if (!is.null(facet_var)) vars_to_select <- c(vars_to_select, facet_var)
            
            mydata <- mydata[vars_to_select]
            
            # Remove missing values
            mydata <- mydata[complete.cases(mydata), ]
            
            # Apply statistical summary based on stat_type
            stat_type <- self$options$stat_type
            
            if (stat_type == "count") {
                # For count, we don't need to summarise y_var
                summary_data <- mydata
            } else {
                # Group by x_var (and fill_var if present) and summarise
                group_vars <- x_var
                if (!is.null(fill_var)) group_vars <- c(group_vars, fill_var)
                if (!is.null(facet_var)) group_vars <- c(group_vars, facet_var)
                
                if (stat_type == "mean") {
                    summary_data <- mydata %>%
                        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
                        dplyr::summarise(
                            value = mean(.data[[y_var]], na.rm = TRUE),
                            se = sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()),
                            sd = sd(.data[[y_var]], na.rm = TRUE),
                            n = dplyr::n(),
                            .groups = "drop"
                        )
                } else if (stat_type == "median") {
                    summary_data <- mydata %>%
                        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
                        dplyr::summarise(
                            value = median(.data[[y_var]], na.rm = TRUE),
                            mad = mad(.data[[y_var]], na.rm = TRUE),
                            n = dplyr::n(),
                            .groups = "drop"
                        )
                } else if (stat_type == "sum") {
                    summary_data <- mydata %>%
                        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
                        dplyr::summarise(
                            value = sum(.data[[y_var]], na.rm = TRUE),
                            n = dplyr::n(),
                            .groups = "drop"
                        )
                } else if (stat_type == "prop") {
                    summary_data <- mydata %>%
                        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
                        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
                        dplyr::mutate(value = n / sum(n))
                }
            }
            
            # Sort if requested
            if (self$options$sort_bars != "none" && stat_type != "count") {
                if (self$options$sort_bars == "asc") {
                    summary_data <- summary_data %>% dplyr::arrange(value)
                } else if (self$options$sort_bars == "desc") {
                    summary_data <- summary_data %>% dplyr::arrange(desc(value))
                } else if (self$options$sort_bars == "alpha") {
                    summary_data <- summary_data %>% dplyr::arrange(.data[[x_var]])
                }
            }
            
            return(summary_data)
        },
        
        .generate_approach_description = function() {
            approach <- self$options$chart_approach
            
            descriptions <- list(
                basic = list(
                    title = "Basic ggplot2 Approach",
                    description = "Clean, straightforward bar chart using standard ggplot2 aesthetics. Perfect for data exploration and quick visualization.",
                    strengths = c("Fast rendering", "Easy to understand", "Highly customizable", "Standard ggplot2 syntax"),
                    use_case = "Data exploration, initial analysis, simple reports"
                ),
                polished = list(
                    title = "Polished Presentation Approach", 
                    description = "Enhanced styling with professional color schemes, improved typography, and refined visual elements.",
                    strengths = c("Professional appearance", "Enhanced readability", "Consistent styling", "Presentation-ready"),
                    use_case = "Business presentations, stakeholder reports, dashboard displays"
                ),
                statistical = list(
                    title = "Statistical Annotations Approach",
                    description = "Integrated statistical testing with automated significance annotations, error bars, and detailed statistical reporting.",
                    strengths = c("Built-in statistics", "Automated annotations", "Error bar options", "Hypothesis testing"),
                    use_case = "Research publications, clinical studies, comparative analysis"
                ),
                interactive = list(
                    title = "Interactive Plotly Approach",
                    description = "Web-based interactive visualization with hover tooltips, zoom capabilities, and dynamic filtering options.",
                    strengths = c("Interactive exploration", "Hover information", "Zoom and pan", "Web-friendly"),
                    use_case = "Web applications, interactive reports, data exploration tools"
                ),
                publication = list(
                    title = "Publication Ready Approach",
                    description = "Journal-quality formatting meeting publication standards with precise typography, optimal spacing, and export optimization.",
                    strengths = c("Publication standards", "High-resolution export", "Journal formatting", "Citation ready"),
                    use_case = "Academic papers, journal submissions, conference presentations"
                ),
                bbc_style = list(
                    title = "BBC News Style Approach",
                    description = "Professional news graphics following BBC Visual and Data Journalism standards with Helvetica typography, BBC brand colors, and clean minimalist design.",
                    strengths = c("BBC design standards", "News-quality aesthetics", "Professional typography", "Brand consistency"),
                    use_case = "News reports, journalism, public communications, professional presentations"
                )
            )
            
            desc <- descriptions[[approach]]
            
            html_content <- paste(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                paste0("<h4 style='color: #495057; margin-top: 0;'>", desc$title, "</h4>"),
                paste0("<p><strong>Description:</strong> ", desc$description, "</p>"),
                "<p><strong>Key Strengths:</strong></p>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                paste(paste0("<li>", desc$strengths, "</li>"), collapse = ""),
                "</ul>",
                paste0("<p><strong>Best Use Case:</strong> ", desc$use_case, "</p>"),
                "</div>"
            )
            
            self$results$approach_description$setContent(html_content)
        },
        
        .generate_summary_stats = function() {
            if (is.null(private$.processed_data)) return()
            
            data <- private$.processed_data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            stat_type <- self$options$stat_type
            
            # Create summary statistics table
            summary_html <- "<h4>Summary Statistics</h4>"
            summary_html <- paste0(summary_html, "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            summary_html <- paste0(summary_html, "<table class='table table-striped' style='margin: 10px 0; max-width: 600px;'>")
            summary_html <- paste0(summary_html, "<thead><tr>")
            summary_html <- paste0(summary_html, "<th>", x_var, "</th>")
            
            if (stat_type == "count") {
                summary_html <- paste0(summary_html, "<th>Count</th><th>Percentage</th>")
            } else if (stat_type %in% c("mean", "median", "sum")) {
                summary_html <- paste0(summary_html, "<th>", tools::toTitleCase(stat_type), "</th>")
                if ("se" %in% names(data)) summary_html <- paste0(summary_html, "<th>SE</th>")
                if ("sd" %in% names(data)) summary_html <- paste0(summary_html, "<th>SD</th>")
                summary_html <- paste0(summary_html, "<th>N</th>")
            }
            
            summary_html <- paste0(summary_html, "</tr></thead><tbody>")
            
            if (stat_type == "count") {
                # For count data, create frequency table
                freq_data <- table(self$data[[x_var]])
                total_n <- sum(freq_data)
                
                for (i in 1:length(freq_data)) {
                    category <- names(freq_data)[i]
                    count <- freq_data[i]
                    percentage <- round(count / total_n * 100, 1)
                    
                    summary_html <- paste0(summary_html, "<tr>")
                    summary_html <- paste0(summary_html, "<td><strong>", category, "</strong></td>")
                    summary_html <- paste0(summary_html, "<td>", count, "</td>")
                    summary_html <- paste0(summary_html, "<td>", percentage, "%</td>")
                    summary_html <- paste0(summary_html, "</tr>")
                }
            } else {
                # For other statistics
                for (i in 1:nrow(data)) {
                    category <- data[[x_var]][i]
                    value <- round(data$value[i], 3)
                    
                    summary_html <- paste0(summary_html, "<tr>")
                    summary_html <- paste0(summary_html, "<td><strong>", category, "</strong></td>")
                    summary_html <- paste0(summary_html, "<td>", value, "</td>")
                    
                    if ("se" %in% names(data)) {
                        summary_html <- paste0(summary_html, "<td>", round(data$se[i], 3), "</td>")
                    }
                    if ("sd" %in% names(data)) {
                        summary_html <- paste0(summary_html, "<td>", round(data$sd[i], 3), "</td>")
                    }
                    if ("n" %in% names(data)) {
                        summary_html <- paste0(summary_html, "<td>", data$n[i], "</td>")
                    }
                    
                    summary_html <- paste0(summary_html, "</tr>")
                }
            }
            
            summary_html <- paste0(summary_html, "</tbody></table>")
            summary_html <- paste0(summary_html, "</div>")
            
            self$results$summary_stats$setContent(summary_html)
        },
        
        .generate_statistical_results = function() {
            if (!self$options$add_statistics) return()
            
            mydata <- self$data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            stat_method <- self$options$stat_method
            
            # Select complete cases
            selected_data <- mydata[c(x_var, y_var)]
            selected_data <- selected_data[complete.cases(selected_data), ]
            
            tryCatch({
                stat_html <- "<h4>Statistical Test Results</h4>"
                stat_html <- paste0(stat_html, "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
                
                if (stat_method == "anova") {
                    # One-way ANOVA
                    formula_str <- paste(y_var, "~", x_var)
                    aov_result <- aov(as.formula(formula_str), data = selected_data)
                    aov_summary <- summary(aov_result)
                    
                    f_stat <- round(aov_summary[[1]]$`F value`[1], 3)
                    p_value <- aov_summary[[1]]$`Pr(>F)`[1]
                    df1 <- aov_summary[[1]]$Df[1]
                    df2 <- aov_summary[[1]]$Df[2]
                    
                    stat_html <- paste0(stat_html, "<p><strong>One-way ANOVA Results:</strong></p>")
                    stat_html <- paste0(stat_html, "<p>F(", df1, ", ", df2, ") = ", f_stat, ", p = ", format.pval(p_value, digits = 3), "</p>")
                    
                } else if (stat_method == "ttest") {
                    # T-test (assuming two groups)
                    groups <- unique(selected_data[[x_var]])
                    if (length(groups) == 2) {
                        group1_data <- selected_data[selected_data[[x_var]] == groups[1], y_var]
                        group2_data <- selected_data[selected_data[[x_var]] == groups[2], y_var]
                        
                        t_result <- t.test(group1_data, group2_data)
                        
                        stat_html <- paste0(stat_html, "<p><strong>Independent t-test Results:</strong></p>")
                        stat_html <- paste0(stat_html, "<p>t(", round(t_result$parameter, 1), ") = ", round(t_result$statistic, 3), ", p = ", format.pval(t_result$p.value, digits = 3), "</p>")
                        stat_html <- paste0(stat_html, "<p>Mean difference: ", round(diff(t_result$estimate), 3), "</p>")
                    } else {
                        stat_html <- paste0(stat_html, "<p style='color: orange;'>t-test requires exactly 2 groups. Found ", length(groups), " groups.</p>")
                    }
                    
                } else if (stat_method == "wilcox") {
                    # Wilcoxon test
                    groups <- unique(selected_data[[x_var]])
                    if (length(groups) == 2) {
                        group1_data <- selected_data[selected_data[[x_var]] == groups[1], y_var]
                        group2_data <- selected_data[selected_data[[x_var]] == groups[2], y_var]
                        
                        w_result <- wilcox.test(group1_data, group2_data)
                        
                        stat_html <- paste0(stat_html, "<p><strong>Wilcoxon Rank Sum Test Results:</strong></p>")
                        stat_html <- paste0(stat_html, "<p>W = ", w_result$statistic, ", p = ", format.pval(w_result$p.value, digits = 3), "</p>")
                    } else {
                        stat_html <- paste0(stat_html, "<p style='color: orange;'>Wilcoxon test requires exactly 2 groups. Found ", length(groups), " groups.</p>")
                    }
                    
                } else if (stat_method == "kruskal") {
                    # Kruskal-Wallis test
                    formula_str <- paste(y_var, "~", x_var)
                    k_result <- kruskal.test(as.formula(formula_str), data = selected_data)
                    
                    stat_html <- paste0(stat_html, "<p><strong>Kruskal-Wallis Test Results:</strong></p>")
                    stat_html <- paste0(stat_html, "<p>χ² = ", round(k_result$statistic, 3), ", df = ", k_result$parameter, ", p = ", format.pval(k_result$p.value, digits = 3), "</p>")
                }
                
                # Add interpretation
                if (exists("p_value")) {
                    if (p_value < 0.001) {
                        interp <- "Highly significant difference (p < 0.001)"
                        color <- "green"
                    } else if (p_value < 0.01) {
                        interp <- "Very significant difference (p < 0.01)"
                        color <- "green"
                    } else if (p_value < 0.05) {
                        interp <- "Significant difference (p < 0.05)"
                        color <- "green"
                    } else {
                        interp <- "No significant difference (p ≥ 0.05)"
                        color <- "orange"
                    }
                    
                    stat_html <- paste0(stat_html, "<p style='color: ", color, "; font-weight: bold;'>", interp, "</p>")
                }
                
                stat_html <- paste0(stat_html, "</div>")
                
                self$results$statistical_results$setContent(stat_html)
                
            }, error = function(e) {
                error_msg <- paste("Error in statistical analysis:", e$message)
                self$results$statistical_results$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .generate_code_example = function() {
            approach <- self$options$chart_approach
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            
            code_examples <- list(
                basic = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col() +\n  theme_minimal()',
                polished = 'ggplot(data, aes(x = {x_var}, y = {y_var}, fill = {x_var})) +\n  geom_col(alpha = 0.8) +\n  scale_fill_viridis_d() +\n  theme_minimal() +\n  theme(legend.position = "none")',
                statistical = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col() +\n  stat_compare_means() +\n  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2)',
                interactive = 'p <- ggplot(data, aes(x = {x_var}, y = {y_var})) + geom_col()\nggplotly(p)',
                publication = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col(fill = "steelblue", alpha = 0.7) +\n  theme_classic() +\n  theme(text = element_text(size = 12, family = "serif"))'
            )
            
            code_template <- code_examples[[approach]]
            actual_code <- gsub("\\{x_var\\}", x_var %||% "x_variable", code_template)
            actual_code <- gsub("\\{y_var\\}", y_var %||% "y_variable", actual_code)
            
            code_html <- paste(
                "<h4>R Code Example</h4>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p><strong>Reproducible R code for this approach:</strong></p>",
                "<pre style='background-color: #2d3748; color: #e2e8f0; padding: 15px; border-radius: 5px; overflow-x: auto;'>",
                "<code>", actual_code, "</code>",
                "</pre>",
                "<p style='color: #666; font-size: 0.9em; margin-top: 10px;'>",
                "Copy this code to reproduce the visualization in your R environment.",
                "</p>",
                "</div>"
            )
            
            self$results$code_example$setContent(code_html)
        },
        
        .generate_interpretation_guide = function() {
            approach <- self$options$chart_approach
            stat_type <- self$options$stat_type
            
            interp_html <- "<h4>Interpretation Guide</h4>"
            interp_html <- paste0(interp_html, "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            # Approach-specific guidance
            if (approach == "statistical") {
                interp_html <- paste0(interp_html, "<p><strong>Statistical Approach Interpretation:</strong></p>")
                interp_html <- paste0(interp_html, "<ul>")
                interp_html <- paste0(interp_html, "<li>Error bars represent the chosen uncertainty measure (SE, SD, or CI)</li>")
                interp_html <- paste0(interp_html, "<li>Statistical test results indicate whether group differences are significant</li>")
                interp_html <- paste0(interp_html, "<li>P-values < 0.05 typically indicate statistically significant differences</li>")
                interp_html <- paste0(interp_html, "</ul>")
            }
            
            # Statistical summary guidance
            interp_html <- paste0(interp_html, "<p><strong>Statistical Summary (", tools::toTitleCase(stat_type), "):</strong></p>")
            
            if (stat_type == "mean") {
                interp_html <- paste0(interp_html, "<p>Bars represent the average value for each category. Use when data is approximately normally distributed.</p>")
            } else if (stat_type == "median") {
                interp_html <- paste0(interp_html, "<p>Bars represent the middle value for each category. More robust to outliers than mean.</p>")
            } else if (stat_type == "count") {
                interp_html <- paste0(interp_html, "<p>Bars represent the frequency of observations in each category.</p>")
            } else if (stat_type == "sum") {
                interp_html <- paste0(interp_html, "<p>Bars represent the total sum of values for each category.</p>")
            } else if (stat_type == "prop") {
                interp_html <- paste0(interp_html, "<p>Bars represent the proportion of total observations in each category.</p>")
            }
            
            # General interpretation guidelines
            interp_html <- paste0(interp_html, "<h5>General Guidelines:</h5>")
            interp_html <- paste0(interp_html, "<ul>")
            interp_html <- paste0(interp_html, "<li><strong>Bar Height:</strong> Indicates the magnitude of the measured value</li>")
            interp_html <- paste0(interp_html, "<li><strong>Comparisons:</strong> Compare relative heights between bars</li>")
            interp_html <- paste0(interp_html, "<li><strong>Patterns:</strong> Look for trends, outliers, or unexpected values</li>")
            interp_html <- paste0(interp_html, "<li><strong>Sample Size:</strong> Consider the number of observations (N) when interpreting results</li>")
            interp_html <- paste0(interp_html, "</ul>")
            
            interp_html <- paste0(interp_html, "</div>")
            
            self$results$interpretation_guide$setContent(interp_html)
        },
        
        # Plot rendering function
        .plot_main = function(image, ggtheme, theme, ...) {
            data <- image$state
            if (is.null(data)) return()
            
            approach <- self$options$chart_approach
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            fill_var <- self$options$fill_var
            
            # Determine y variable name based on stat_type
            if (self$options$stat_type == "count") {
                y_column <- y_var
            } else {
                y_column <- "value"
            }
            
            tryCatch({
                if (approach == "basic") {
                    plot <- private$.create_basic_plot(data, x_var, y_column, fill_var)
                } else if (approach == "polished") {
                    plot <- private$.create_polished_plot(data, x_var, y_column, fill_var)
                } else if (approach == "statistical") {
                    plot <- private$.create_statistical_plot(data, x_var, y_column, fill_var)
                } else if (approach == "interactive") {
                    plot <- private$.create_interactive_plot(data, x_var, y_column, fill_var)
                    # For interactive plots, we still need a ggplot for the image
                    plot <- private$.create_polished_plot(data, x_var, y_column, fill_var)
                } else if (approach == "publication") {
                    plot <- private$.create_publication_plot(data, x_var, y_column, fill_var)
                } else if (approach == "bbc_style") {
                    plot <- private$.create_bbc_style_plot(data, x_var, y_column, fill_var)
                } else if (approach == "prism_style") {
                    plot <- private$.create_prism_style_plot(data, x_var, y_column, fill_var)
                }
                
                # Apply orientation
                if (self$options$orientation == "horizontal") {
                    plot <- plot + ggplot2::coord_flip()
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                warning(paste("Error creating plot:", e$message))
                # Fallback to basic plot
                basic_plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])) +
                    ggplot2::geom_col() +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(title = "Bar Chart", x = x_var, y = y_var)
                print(basic_plot)
                TRUE
            })
        },
        
        .create_basic_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])
            
            if (!is.null(fill_var)) {
                aes_mapping$fill <- rlang::sym(fill_var)
            }
            
            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    position = self$options$bar_position,
                    width = self$options$bar_width,
                    alpha = self$options$transparency
                ) +
                ggplot2::theme_minimal()
            
            if (self$options$show_values) {
                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(label = round(.data[[y_column]], 2)),
                    position = ggplot2::position_dodge(width = self$options$bar_width),
                    vjust = -0.5
                )
            }
            
            return(plot)
        },
        
        .create_polished_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])
            
            if (!is.null(fill_var)) {
                aes_mapping$fill <- rlang::sym(fill_var)
            } else {
                aes_mapping$fill <- rlang::sym(x_var)
            }
            
            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    position = self$options$bar_position,
                    width = self$options$bar_width,
                    alpha = self$options$transparency
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    text = ggplot2::element_text(size = 12),
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    axis.text = ggplot2::element_text(size = 10),
                    legend.position = self$options$legend_position
                )
            
            # Apply color palette
            plot <- private$.apply_color_palette(plot, self$options$color_palette)
            
            if (self$options$show_values) {
                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(label = private$.format_values(.data[[y_column]])),
                    position = ggplot2::position_dodge(width = self$options$bar_width),
                    vjust = -0.5,
                    size = 3.5
                )
            }
            
            return(plot)
        },
        
        .create_statistical_plot = function(data, x_var, y_column, fill_var) {
            plot <- private$.create_polished_plot(data, x_var, y_column, fill_var)
            
            # Add error bars if available
            if ("se" %in% names(data) && self$options$error_bars != "none") {
                error_column <- switch(self$options$error_bars,
                                     "se" = "se",
                                     "sd" = "sd",
                                     "ci95" = "se", # Will multiply by 1.96
                                     "se")
                
                multiplier <- if (self$options$error_bars == "ci95") 1.96 else 1
                
                plot <- plot + ggplot2::geom_errorbar(
                    ggplot2::aes(
                        ymin = .data[[y_column]] - multiplier * .data[[error_column]],
                        ymax = .data[[y_column]] + multiplier * .data[[error_column]]
                    ),
                    position = ggplot2::position_dodge(width = self$options$bar_width),
                    width = 0.2
                )
            }
            
            return(plot)
        },
        
        .create_interactive_plot = function(data, x_var, y_column, fill_var) {
            # Create the base plot
            plot <- private$.create_polished_plot(data, x_var, y_column, fill_var)
            
            # Convert to plotly and store in HTML result
            if (requireNamespace("plotly", quietly = TRUE)) {
                interactive_plot <- plotly::ggplotly(plot)
                
                # Convert plotly object to HTML
                html_content <- paste(
                    "<div style='width: 100%; height: 600px;'>",
                    htmltools::HTML(plotly::as_widget(interactive_plot)),
                    "</div>"
                )
                
                self$results$interactive_plot$setContent(html_content)
            }
            
            return(plot)
        },
        
        .create_publication_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])
            
            if (!is.null(fill_var)) {
                aes_mapping$fill <- rlang::sym(fill_var)
            }
            
            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    position = self$options$bar_position,
                    width = self$options$bar_width,
                    alpha = self$options$transparency,
                    color = "black",
                    size = 0.3
                ) +
                ggplot2::theme_classic() +
                ggplot2::theme(
                    text = ggplot2::element_text(size = 11, family = "serif"),
                    plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                    axis.text = ggplot2::element_text(size = 10, color = "black"),
                    axis.title = ggplot2::element_text(size = 11, face = "bold"),
                    legend.position = self$options$legend_position,
                    panel.grid = ggplot2::element_blank(),
                    axis.line = ggplot2::element_line(color = "black", size = 0.5)
                )
            
            # Apply appropriate titles
            titles <- private$.get_plot_titles()
            plot <- plot + ggplot2::labs(
                title = titles$title,
                x = titles$x,
                y = titles$y
            )
            
            return(plot)
        },
        
        .create_bbc_style_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])
            
            if (!is.null(fill_var)) {
                aes_mapping$fill <- rlang::sym(fill_var)
            }
            
            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    position = self$options$bar_position,
                    width = self$options$bar_width,
                    alpha = 1.0  # BBC style uses solid colors
                )
            
            # Apply BBC style theme (recreating bbplot::bbc_style())
            plot <- plot + ggplot2::theme(
                # Text elements
                text = ggplot2::element_text(family = "Helvetica", size = 18, color = "#222222"),
                plot.title = ggplot2::element_text(family = "Helvetica", size = 28, face = "bold", color = "#222222"),
                plot.subtitle = ggplot2::element_text(family = "Helvetica", size = 22, margin = ggplot2::margin(9, 0, 9, 0)),
                
                # Legend
                legend.position = "top",
                legend.text = ggplot2::element_text(family = "Helvetica", size = 18, color = "#222222"),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank(),
                legend.background = ggplot2::element_blank(),
                
                # Axis
                axis.title = ggplot2::element_blank(),
                axis.text = ggplot2::element_text(family = "Helvetica", size = 18, color = "#222222"),
                axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                axis.ticks = ggplot2::element_blank(),
                axis.line = ggplot2::element_blank(),
                
                # Grid lines (BBC style: horizontal only)
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
                panel.grid.major.x = ggplot2::element_blank(),
                
                # Background
                panel.background = ggplot2::element_blank(),
                plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
            
            # Apply BBC colors
            if (!is.null(fill_var)) {
                bbc_colors <- c("#1380A1", "#FAAB18", "#007f7f", "#333333", "#990000", "#007A54")
                plot <- plot + ggplot2::scale_fill_manual(values = bbc_colors)
            } else {
                plot <- plot + ggplot2::scale_fill_manual(values = "#1380A1")
            }
            
            # Apply appropriate titles
            titles <- private$.get_plot_titles()
            plot <- plot + ggplot2::labs(
                title = titles$title,
                subtitle = if (self$options$subtitle_text != "") self$options$subtitle_text else NULL,
                caption = if (self$options$source_text != "") paste("Source:", self$options$source_text) else NULL
            )
            
            return(plot)
        },
        
        .create_prism_style_plot = function(data, x_var, y_column, fill_var) {
            # Create basic bar plot
            if (!is.null(fill_var) && fill_var != "") {
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_column, fill = fill_var))
            } else {
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_column))
            }
            
            # Add bars with position adjustment
            position <- self$options$bar_position
            if (position == "dodge") {
                plot <- plot + ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), 
                                                alpha = 0.9, width = self$options$bar_width)
            } else if (position == "stack") {
                plot <- plot + ggplot2::geom_col(alpha = 0.9, width = self$options$bar_width)
            } else if (position == "fill") {
                plot <- plot + ggplot2::geom_col(position = "fill", alpha = 0.9, width = self$options$bar_width)
            }
            
            # Apply GraphPad Prism theme
            plot <- plot + private$.get_prism_theme()
            
            # Apply appropriate color palette (will be handled by color_palette option)
            if (is.null(fill_var) || fill_var == "") {
                # Single color for single variable plots
                plot <- plot + ggplot2::scale_fill_manual(values = "#1f77b4")
            }
            
            # Apply appropriate titles
            titles <- private$.get_plot_titles()
            plot <- plot + ggplot2::labs(
                title = titles$title,
                subtitle = if (self$options$subtitle_text != "") self$options$subtitle_text else "GraphPad Prism Style Visualization",
                caption = if (self$options$source_text != "") paste("Source:", self$options$source_text) else NULL
            )
            
            return(plot)
        },
        
        .get_prism_theme = function() {
            theme_name <- self$options$theme_style
            base_size <- 12  # Standard Prism size
            
            if (!requireNamespace("ggprism", quietly = TRUE)) {
                # Fallback to approximate Prism styling
                return(ggplot2::theme_minimal(base_size = base_size) +
                    ggplot2::theme(
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
                        axis.line = ggplot2::element_line(colour = "black"),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5)
                    ))
            }
            
            # Apply authentic GraphPad Prism themes
            switch(theme_name,
                "prism_default" = ggprism::theme_prism(base_size = base_size),
                "prism_white" = ggprism::theme_prism(base_size = base_size, axis_text_angle = 0),
                "prism_minimal" = ggprism::theme_prism(base_size = base_size) + 
                    ggplot2::theme(panel.grid.major = ggplot2::element_blank()),
                "prism_publication" = ggprism::theme_prism(base_size = base_size) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = base_size + 1, face = "bold"),
                        legend.background = ggplot2::element_rect(fill = "white", colour = "black"),
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA)
                    ),
                # Default fallback
                ggplot2::theme_minimal(base_size = base_size) +
                    ggplot2::theme(
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
                        axis.line = ggplot2::element_line(colour = "black")
                    )
            )
        },
        
        .apply_prism_palette = function(plot, palette_name) {
            if (!requireNamespace("ggprism", quietly = TRUE)) {
                # Fallback to viridis if ggprism not available
                return(plot + viridis::scale_fill_viridis_d())
            }
            
            # Apply ggprism color scales
            plot + ggprism::scale_fill_prism(palette = palette_name)
        },
        
        .apply_color_palette = function(plot, palette_name) {
            if (palette_name == "default") {
                return(plot)
            } else if (palette_name == "bbc_blue") {
                return(plot + ggplot2::scale_fill_manual(values = "#1380A1"))
            } else if (palette_name == "bbc_orange") {
                return(plot + ggplot2::scale_fill_manual(values = "#FAAB18"))
            } else if (palette_name == "bbc_multi") {
                bbc_colors <- c("#1380A1", "#FAAB18", "#007f7f", "#333333", "#990000", "#007A54")
                return(plot + ggplot2::scale_fill_manual(values = bbc_colors))
            } else if (palette_name == "viridis") {
                return(plot + viridis::scale_fill_viridis_d())
            } else if (palette_name %in% c("set1", "dark2")) {
                return(plot + ggplot2::scale_fill_brewer(type = "qual", palette = tools::toTitleCase(palette_name)))
            } else if (palette_name == "clinical") {
                clinical_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
                return(plot + ggplot2::scale_fill_manual(values = clinical_colors))
            } else if (palette_name == "colorblind") {
                cb_colors <- c("#0173B2", "#DE8F05", "#029E73", "#CC78BC", "#CA9161", "#FBAFE4")
                return(plot + ggplot2::scale_fill_manual(values = cb_colors))
            # GraphPad Prism color palettes
            } else if (palette_name == "prism_floral") {
                return(private$.apply_prism_palette(plot, "floral"))
            } else if (palette_name == "prism_candy_bright") {
                return(private$.apply_prism_palette(plot, "candy_bright"))
            } else if (palette_name == "prism_office") {
                return(private$.apply_prism_palette(plot, "office"))
            } else if (palette_name == "prism_pastels") {
                return(private$.apply_prism_palette(plot, "pastels"))
            } else if (palette_name == "prism_colorblind_safe") {
                return(private$.apply_prism_palette(plot, "colorblind_safe"))
            } else if (palette_name == "prism_blueprint") {
                return(private$.apply_prism_palette(plot, "blueprint"))
            } else if (palette_name == "prism_neon") {
                return(private$.apply_prism_palette(plot, "neon"))
            } else if (palette_name == "prism_ocean") {
                return(private$.apply_prism_palette(plot, "ocean"))
            } else if (palette_name == "prism_spring") {
                return(private$.apply_prism_palette(plot, "spring"))
            } else if (palette_name == "prism_dark") {
                return(private$.apply_prism_palette(plot, "prism_dark"))
            } else if (palette_name == "prism_light") {
                return(private$.apply_prism_palette(plot, "prism_light"))
            }
            
            return(plot)
        },
        
        .format_values = function(values) {
            format_type <- self$options$value_format
            
            if (format_type == "integer") {
                return(as.character(round(values)))
            } else if (format_type == "decimal1") {
                return(sprintf("%.1f", values))
            } else if (format_type == "decimal2") {
                return(sprintf("%.2f", values))
            } else if (format_type == "percentage") {
                return(scales::percent(values, accuracy = 0.1))
            } else if (format_type == "scientific") {
                return(format(values, scientific = TRUE, digits = 3))
            } else {
                # Auto format
                if (all(values == round(values), na.rm = TRUE)) {
                    return(as.character(round(values)))
                } else {
                    return(sprintf("%.2f", values))
                }
            }
        },
        
        .get_plot_titles = function() {
            list(
                title = if (self$options$plot_title != "") self$options$plot_title else NULL,
                x = if (self$options$x_title != "") self$options$x_title else self$options$x_var,
                y = if (self$options$y_title != "") self$options$y_title else self$options$y_var
            )
        }
    )
)

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x