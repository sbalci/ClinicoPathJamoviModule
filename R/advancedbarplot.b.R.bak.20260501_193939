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
#' @importFrom ggplot2 theme element_rect element_line margin geom_errorbar
#' @importFrom ggplot2 scale_fill_manual position_dodge theme_void annotate coord_polar
#' @importFrom ggplot2 geom_hline scale_color_identity
#' @importFrom dplyr group_by summarise mutate arrange across all_of desc
#' @importFrom plotly ggplotly plot_ly layout as_widget
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent comma
#' @importFrom stats aov t.test wilcox.test chisq.test kruskal.test quantile median mad
#' @importFrom stringr str_to_title
#' @importFrom rlang sym .data
#' @importFrom tools toTitleCase
#' @importFrom utils head tail
#' @export

advancedbarplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "advancedbarplotClass",
    inherit = advancedbarplotBase,
    private = list(

        # Internal data storage
        .processed_data = NULL,
        .summary_stats = NULL,
        .statistical_results = NULL,
        .plot_cache = NULL,
        .package_availability = NULL,

        .init = function() {
            # Check package availability once during initialization
            private$.check_package_availability()
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
                "<li><strong>BBC News Style:</strong> Professional news graphics following BBC standards</li>",
                "<li><strong>GraphPad Prism:</strong> Scientific publication style with Prism themes</li>",
                "<li><strong>Diverging Bars:</strong> Emphasize positive/negative deviations from baseline</li>",
                "<li><strong>Circular/Radial:</strong> Transform bars into engaging circular layouts</li>",
                "<li><strong>Economist Style:</strong> Clean horizontal bars with smart labeling</li>",
                "<li><strong>Pattern/Textured:</strong> Black & white friendly with patterns for accessibility</li>",
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

        .check_package_availability = function() {
            private$.package_availability <- list(
                plotly = requireNamespace("plotly", quietly = TRUE),
                htmltools = requireNamespace("htmltools", quietly = TRUE),
                ggprism = requireNamespace("ggprism", quietly = TRUE),
                patchwork = requireNamespace("patchwork", quietly = TRUE),
                gridExtra = requireNamespace("gridExtra", quietly = TRUE),
                viridis = requireNamespace("viridis", quietly = TRUE),
                scales = requireNamespace("scales", quietly = TRUE),
                ggpattern = requireNamespace("ggpattern", quietly = TRUE),
                shadowtext = requireNamespace("shadowtext", quietly = TRUE)
            )
        },

        .validate_inputs = function() {
            # Enhanced input validation
            errors <- character(0)

            # Validate required variables
            if (is.null(self$options$x_var)) {
                errors <- c(errors, "X variable is required")
            }
            if (is.null(self$options$y_var)) {
                errors <- c(errors, "Y variable is required")
            }

            # Validate highlight_bars format
            if (self$options$highlight_bars != "") {
                highlight_list <- trimws(strsplit(self$options$highlight_bars, ",")[[1]])
                if (any(nchar(highlight_list) == 0)) {
                    errors <- c(errors, "Invalid highlight_bars format. Use comma-separated category names.")
                }
            }

            # Validate numeric ranges
            if (self$options$bar_width < 0.1 || self$options$bar_width > 1.0) {
                errors <- c(errors, "Bar width must be between 0.1 and 1.0")
            }
            if (self$options$transparency < 0.1 || self$options$transparency > 1.0) {
                errors <- c(errors, "Transparency must be between 0.1 and 1.0")
            }

            # Check data compatibility with statistical tests
            if (self$options$add_statistics) {
                if (self$options$stat_method %in% c("ttest", "wilcox")) {
                    x_levels <- length(unique(self$data[[self$options$x_var]]))
                    if (x_levels != 2) {
                        errors <- c(errors, paste("Selected statistical test requires exactly 2 groups, found", x_levels))
                    }
                }
            }

            if (length(errors) > 0) {
                stop(paste(errors, collapse = "; "))
            }
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

            # Enhanced input validation
            tryCatch({
                private$.validate_inputs()
            }, error = function(e) {
                self$results$approach_description$setContent(
                    paste0("<div style='color: red; background-color: #fee; padding: 10px; border-radius: 5px;'>",
                           "<strong>Validation Error:</strong> ", e$message, "</div>")
                )
                return()
            })

            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }

            # Process data with caching
            cached_data <- private$.get_cached_data()
            if (!is.null(cached_data) && requireNamespace("digest", quietly = TRUE)) {
                private$.processed_data <- cached_data
            } else {
                private$.processed_data <- private$.process_data()
            }

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

            # Performance optimization for large datasets
            if (nrow(mydata) > 10000) {
                # Use data.table-like operations for better performance
                message("Processing large dataset (", nrow(mydata), " rows). This may take a moment...")
            }

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

            # Remove missing values with progress indication for large datasets
            initial_rows <- nrow(mydata)
            mydata <- mydata[complete.cases(mydata), ]
            final_rows <- nrow(mydata)

            if (initial_rows - final_rows > 0) {
                message("Removed ", initial_rows - final_rows, " rows with missing values")
            }

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
                    # Optimized mean calculation
                    summary_data <- mydata %>%
                        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
                        dplyr::summarise(
                            value = mean(.data[[y_var]], na.rm = TRUE),
                            se = sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()),
                            sd = sd(.data[[y_var]], na.rm = TRUE),
                            n = dplyr::n(),
                            min_val = min(.data[[y_var]], na.rm = TRUE),
                            max_val = max(.data[[y_var]], na.rm = TRUE),
                            .groups = "drop"
                        )
                } else if (stat_type == "median") {
                    # Optimized median calculation
                    summary_data <- mydata %>%
                        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
                        dplyr::summarise(
                            value = stats::median(.data[[y_var]], na.rm = TRUE),
                            mad = stats::mad(.data[[y_var]], na.rm = TRUE),
                            q25 = stats::quantile(.data[[y_var]], 0.25, na.rm = TRUE),
                            q75 = stats::quantile(.data[[y_var]], 0.75, na.rm = TRUE),
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
                    summary_data <- summary_data %>% dplyr::arrange(dplyr::desc(value))
                } else if (self$options$sort_bars == "alpha") {
                    summary_data <- summary_data %>% dplyr::arrange(.data[[x_var]])
                }
            }

            # Cache processed data for performance (if digest available)
            if (requireNamespace("digest", quietly = TRUE)) {
                private$.plot_cache <- list(
                    data = summary_data,
                    timestamp = Sys.time(),
                    options_hash = digest::digest(list(
                        x_var = self$options$x_var,
                        y_var = self$options$y_var,
                        fill_var = self$options$fill_var,
                        stat_type = self$options$stat_type,
                        sort_bars = self$options$sort_bars
                    ))
                )
            }

            return(summary_data)
        },

        .get_cached_data = function() {
            # Simple caching mechanism for performance (requires digest package)
            if (!is.null(private$.plot_cache) && requireNamespace("digest", quietly = TRUE)) {
                tryCatch({
                    current_hash <- digest::digest(list(
                        x_var = self$options$x_var,
                        y_var = self$options$y_var,
                        fill_var = self$options$fill_var,
                        stat_type = self$options$stat_type,
                        sort_bars = self$options$sort_bars
                    ))

                    if (private$.plot_cache$options_hash == current_hash) {
                        time_diff <- as.numeric(difftime(Sys.time(), private$.plot_cache$timestamp, units = "secs"))
                        if (time_diff < 30) {  # Cache valid for 30 seconds
                            return(private$.plot_cache$data)
                        }
                    }
                }, error = function(e) {
                    # Silent failure for caching
                    return(NULL)
                })
            }
            return(NULL)
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
                ),
                diverging = list(
                    title = "Diverging Bar Plot Approach",
                    description = "Emphasizes positive and negative deviations from a baseline value with contrasting colors, inspired by NYTimes data visualizations.",
                    strengths = c("Clear positive/negative distinction", "Baseline emphasis", "Intuitive comparisons", "Change visualization"),
                    use_case = "Before/after comparisons, profit/loss, survey responses, climate data"
                ),
                circular = list(
                    title = "Circular/Radial Bar Plot Approach",
                    description = "Transforms traditional bars into a circular layout using polar coordinates, creating visually striking radial visualizations.",
                    strengths = c("Space efficient", "Visually engaging", "Cyclical data display", "360-degree comparison"),
                    use_case = "Time-based cycles, seasonal data, directional data, dashboard displays"
                ),
                economist = list(
                    title = "Economist Style Approach",
                    description = "Clean horizontal bars with intelligent label placement following The Economist's data journalism standards.",
                    strengths = c("Professional journalism style", "Smart labeling", "Red accent signature", "Horizontal readability"),
                    use_case = "Economic reports, rankings, survey results, professional publications"
                ),
                pattern = list(
                    title = "Pattern/Textured Bar Plot Approach",
                    description = "Uses patterns and textures instead of colors for black & white printing and accessibility.",
                    strengths = c("Printer friendly", "Colorblind accessible", "Pattern variety", "Publication ready"),
                    use_case = "Black & white publications, accessibility requirements, scientific journals"
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
                statistical = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col() +\n  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2) +\n  theme_minimal()',
                interactive = 'library(plotly)\np <- ggplot(data, aes(x = {x_var}, y = {y_var})) + geom_col()\nggplotly(p)',
                publication = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col(fill = "steelblue", alpha = 0.7) +\n  theme_classic() +\n  theme(text = element_text(size = 12, family = "serif"))',
                bbc_style = 'library(ggplot2)\nggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col(fill = "#1380A1") +\n  theme_minimal() +\n  theme(text = element_text(family = "Helvetica", size = 18))',
                prism_style = 'library(ggprism)\nggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col() +\n  theme_prism() +\n  scale_fill_prism()',
                diverging = 'ggplot(data, aes(x = {x_var}, y = {y_var},\n       fill = ifelse({y_var} > 0, "positive", "negative"))) +\n  geom_col() +\n  geom_hline(yintercept = 0) +\n  scale_fill_manual(values = c("positive" = "#2ca02c", "negative" = "#d62728"))',
                circular = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col() +\n  coord_polar(theta = "x") +\n  theme_minimal()',
                economist = 'ggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col(fill = "#0073e6") +\n  coord_flip() +\n  theme_minimal() +\n  # Add smart labels inside/outside bars',
                pattern = 'library(ggpattern)\nggplot(data, aes(x = {x_var}, y = {y_var})) +\n  geom_col_pattern(fill = "white", colour = "black",\n                   pattern_density = 0.5) +\n  theme_bw()'
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
                "<div style='margin-top: 10px;'>",
                "<p style='color: #666; font-size: 0.9em;'>Copy this code to reproduce the visualization in your R environment.</p>",
                "<details style='margin-top: 10px;'>",
                "<summary style='cursor: pointer; color: #007bff;'>Export Options</summary>",
                "<div style='padding: 10px; background-color: #f8f9fa; border-radius: 5px; margin-top: 5px;'>",
                "<p style='margin: 5px 0; font-size: 0.9em;'><strong>High-Quality Export:</strong></p>",
                "<code style='background-color: #e9ecef; padding: 2px 4px; border-radius: 3px; font-size: 0.8em;'>",
                "ggsave('plot.png', width = 10, height = 6, dpi = 300)",
                "</code><br>",
                "<code style='background-color: #e9ecef; padding: 2px 4px; border-radius: 3px; font-size: 0.8em;'>",
                "ggsave('plot.svg', width = 10, height = 6)  # Vector format",
                "</code><br>",
                "<code style='background-color: #e9ecef; padding: 2px 4px; border-radius: 3px; font-size: 0.8em;'>",
                "ggsave('plot.pdf', width = 10, height = 6)  # Publication ready",
                "</code>",
                "</div>",
                "</details>",
                "</div>",
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

        # Shared plotting utilities
        .create_base_aesthetic = function(data, x_var, y_column, fill_var) {
            aes_mapping <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])

            if (!is.null(fill_var) && fill_var != "") {
                aes_mapping$fill <- rlang::sym(fill_var)
            }

            return(aes_mapping)
        },

        .apply_common_elements = function(plot, show_values = TRUE, y_column = "value") {
            if (show_values && self$options$show_values) {
                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(label = private$.format_values(.data[[y_column]])),
                    position = ggplot2::position_dodge(width = self$options$bar_width),
                    vjust = -0.5,
                    size = 3.5
                )
            }

            return(plot)
        },

        .create_error_plot = function(error_message) {
            ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                label = paste("Error creating plot:\n", error_message,
                                             "\n\nPlease check your data and options."),
                                hjust = 0.5, vjust = 0.5, size = 4, color = "red") +
                ggplot2::theme_void() +
                ggplot2::labs(title = "Plot Generation Error")
        },

        .safe_require_package = function(package_name, feature_name) {
            if (!private$.package_availability[[package_name]]) {
                warning(paste(feature_name, "requires the", package_name, "package. Using fallback approach."))
                return(FALSE)
            }
            return(TRUE)
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
                # Create plot based on approach with enhanced error handling
                plot <- switch(approach,
                    "basic" = private$.create_basic_plot(data, x_var, y_column, fill_var),
                    "polished" = private$.create_polished_plot(data, x_var, y_column, fill_var),
                    "statistical" = private$.create_statistical_plot(data, x_var, y_column, fill_var),
                    "interactive" = {
                        private$.create_interactive_plot(data, x_var, y_column, fill_var)
                        # For interactive plots, we still need a ggplot for the image
                        private$.create_polished_plot(data, x_var, y_column, fill_var)
                    },
                    "publication" = private$.create_publication_plot(data, x_var, y_column, fill_var),
                    "bbc_style" = private$.create_bbc_style_plot(data, x_var, y_column, fill_var),
                    "prism_style" = private$.create_prism_style_plot(data, x_var, y_column, fill_var),
                    "diverging" = private$.create_diverging_plot(data, x_var, y_column, fill_var),
                    "circular" = private$.create_circular_plot(data, x_var, y_column, fill_var),
                    "economist" = private$.create_economist_plot(data, x_var, y_column, fill_var),
                    "pattern" = private$.create_pattern_plot(data, x_var, y_column, fill_var),
                    # Default fallback
                    private$.create_basic_plot(data, x_var, y_column, fill_var)
                )

                # Apply orientation
                if (self$options$orientation == "horizontal") {
                    plot <- plot + ggplot2::coord_flip()
                }

                # Apply plot titles if not already applied
                titles <- private$.get_plot_titles()
                if (!is.null(titles$title) || !is.null(titles$x) || !is.null(titles$y)) {
                    plot <- plot + ggplot2::labs(
                        title = titles$title,
                        x = titles$x,
                        y = titles$y
                    )
                }

                print(plot)
                TRUE

            }, error = function(e) {
                warning(paste("Error creating plot:", e$message))
                # Enhanced fallback plot with error message
                error_plot <- private$.create_error_plot(e$message)
                print(error_plot)
                TRUE
            })
        },

        .create_basic_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    position = self$options$bar_position,
                    width = self$options$bar_width,
                    alpha = self$options$transparency
                ) +
                ggplot2::theme_minimal()

            # Apply common elements
            plot <- private$.apply_common_elements(plot, TRUE, y_column)

            return(plot)
        },

        .create_polished_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

            # Use fill variable or default to x_var for coloring
            if (is.null(fill_var)) {
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

            # Apply color palette with error handling
            plot <- tryCatch({
                private$.apply_color_palette(plot, self$options$color_palette)
            }, error = function(e) {
                warning(paste("Color palette error:", e$message, "Using default colors."))
                plot
            })

            # Apply common elements
            plot <- private$.apply_common_elements(plot, TRUE, y_column)

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

            # Enhanced error handling for interactive plots
            tryCatch({
                if (private$.safe_require_package("plotly", "Interactive plots") &&
                    private$.safe_require_package("htmltools", "Interactive plots")) {

                    interactive_plot <- plotly::ggplotly(plot, tooltip = c("x", "y", "fill"))

                    # Enhanced interactivity
                    interactive_plot <- interactive_plot %>%
                        plotly::layout(
                            title = list(text = paste("Interactive:", self$options$plot_title)),
                            xaxis = list(title = self$options$x_title %||% x_var),
                            yaxis = list(title = self$options$y_title %||% y_column)
                        )

                    # Convert plotly object to HTML with error handling
                    html_content <- tryCatch({
                        paste(
                            "<div style='width: 100%; height: 600px; border: 1px solid #ddd; border-radius: 5px;'>",
                            htmltools::HTML(plotly::as_widget(interactive_plot)),
                            "</div>"
                        )
                    }, error = function(e) {
                        paste(
                            "<div style='color: orange; padding: 15px; background-color: #fff3e0; border-radius: 5px;'>",
                            "<strong>Interactive Plot Error:</strong> ", e$message,
                            "<br>Displaying static version instead.",
                            "</div>"
                        )
                    })

                    self$results$interactive_plot$setContent(html_content)
                } else {
                    # Fallback message
                    fallback_html <- paste(
                        "<div style='color: #666; padding: 15px; background-color: #f5f5f5; border-radius: 5px;'>",
                        "<strong>Interactive Plot Unavailable:</strong><br>",
                        "Install 'plotly' and 'htmltools' packages for interactive functionality.<br>",
                        "Static plot is displayed in the main plot section.",
                        "</div>"
                    )
                    self$results$interactive_plot$setContent(fallback_html)
                }
            }, error = function(e) {
                error_html <- paste(
                    "<div style='color: red; padding: 15px; background-color: #fee; border-radius: 5px;'>",
                    "<strong>Interactive Plot Error:</strong> ", e$message,
                    "</div>"
                )
                self$results$interactive_plot$setContent(error_html)
            })

            return(plot)
        },

        .create_publication_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

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

            # Apply common elements
            plot <- private$.apply_common_elements(plot, TRUE, y_column)

            return(plot)
        },

        .create_bbc_style_plot = function(data, x_var, y_column, fill_var) {
            aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

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

        .create_diverging_plot = function(data, x_var, y_column, fill_var) {
            # Diverging bar plot implementation inspired by NYTimes style
            center_value <- self$options$diverging_center

            # Create base aesthetic
            aes_mapping <- ggplot2::aes(
                x = .data[[x_var]],
                y = .data[[y_column]],
                fill = ifelse(.data[[y_column]] > center_value, "positive", "negative"),
                color = ifelse(.data[[y_column]] > center_value, "positive", "negative")
            )

            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(width = self$options$bar_width, alpha = self$options$transparency) +
                ggplot2::geom_hline(yintercept = center_value, linetype = "solid", color = "black", size = 0.5) +
                ggplot2::scale_fill_manual(values = c("positive" = "#2ca02c", "negative" = "#d62728")) +
                ggplot2::scale_color_manual(values = c("positive" = "#2ca02c", "negative" = "#d62728")) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    legend.position = "none",
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_blank(),
                    text = ggplot2::element_text(size = 12),
                    plot.title = ggplot2::element_text(size = 14, face = "bold")
                )

            # Add value labels with smart positioning
            if (self$options$show_values) {
                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(
                        label = private$.format_values(.data[[y_column]]),
                        vjust = ifelse(.data[[y_column]] > center_value, -0.5, 1.5)
                    ),
                    size = 3.5
                )
            }

            # Add reference line label
            plot <- plot + ggplot2::annotate(
                "text",
                x = -Inf,
                y = center_value,
                label = paste("Baseline:", center_value),
                hjust = -0.1,
                vjust = -0.5,
                size = 3,
                color = "gray40"
            )

            return(plot)
        },

        .create_circular_plot = function(data, x_var, y_column, fill_var) {
            # Circular/radial bar plot implementation
            start_angle <- self$options$radial_start_angle

            # Ensure x_var is a factor for proper ordering in circular plot
            data[[x_var]] <- factor(data[[x_var]], levels = unique(data[[x_var]]))

            # Create base aesthetic
            aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    width = self$options$bar_width,
                    alpha = self$options$transparency
                ) +
                ggplot2::coord_polar(theta = "x", start = start_angle * pi / 180) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks = ggplot2::element_blank(),
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                    legend.position = self$options$legend_position
                )

            # Apply color palette
            if (!is.null(fill_var) || self$options$color_palette != "default") {
                plot <- private$.apply_color_palette(plot, self$options$color_palette)
            }

            # Add radial labels if requested
            if (self$options$show_values) {
                # Calculate label positions
                n_bars <- nrow(data)
                angles <- seq(start_angle, start_angle + 360 - 360/n_bars, length.out = n_bars)

                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(
                        label = private$.format_values(.data[[y_column]]),
                        y = .data[[y_column]] + max(data[[y_column]]) * 0.05
                    ),
                    size = 3,
                    angle = 0
                )
            }

            return(plot)
        },

        .create_economist_plot = function(data, x_var, y_column, fill_var) {
            # Economist style horizontal bar plot with labels
            aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

            plot <- ggplot2::ggplot(data, aes_mapping) +
                ggplot2::geom_col(
                    width = self$options$bar_width,
                    fill = "#0073e6",  # Economist blue
                    alpha = 0.9
                ) +
                ggplot2::coord_flip() +  # Always horizontal for Economist style
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    # Economist specific styling
                    text = ggplot2::element_text(family = "sans", size = 11),
                    plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 12, color = "gray40", hjust = 0),
                    plot.caption = ggplot2::element_text(size = 9, color = "gray50", hjust = 1),
                    # Clean axes
                    axis.title = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(size = 10),
                    axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
                    # Grid lines
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_line(color = "gray80", size = 0.3),
                    # Remove legend by default
                    legend.position = "none"
                )

            # Add value labels intelligently positioned
            if (self$options$show_values || self$options$label_inside) {
                # Determine label position based on bar size
                max_val <- max(data[[y_column]], na.rm = TRUE)
                threshold <- max_val * 0.15  # Threshold for inside vs outside

                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(
                        label = private$.format_values(.data[[y_column]]),
                        x = .data[[x_var]],
                        y = ifelse(.data[[y_column]] > threshold,
                                  .data[[y_column]] * 0.95,  # Inside bar
                                  .data[[y_column]] + max_val * 0.02),  # Outside bar
                        hjust = ifelse(.data[[y_column]] > threshold, 1, 0),
                        color = ifelse(.data[[y_column]] > threshold, "white", "black")
                    ),
                    size = 3.5,
                    fontface = "bold"
                ) +
                ggplot2::scale_color_identity()
            }

            # Add red accent line (Economist signature)
            plot <- plot + ggplot2::annotate(
                "segment",
                x = -Inf, xend = -Inf,
                y = -Inf, yend = Inf,
                color = "#d7282a",
                size = 2
            )

            return(plot)
        },

        .create_pattern_plot = function(data, x_var, y_column, fill_var) {
            # Pattern/textured bar plot for black & white printing
            pattern_type <- self$options$pattern_type

            # Check if ggpattern is available
            if (requireNamespace("ggpattern", quietly = TRUE)) {
                aes_mapping <- ggplot2::aes(
                    x = .data[[x_var]],
                    y = .data[[y_column]]
                )

                if (!is.null(fill_var) && fill_var != "") {
                    aes_mapping$pattern = rlang::sym(fill_var)
                }

                plot <- ggplot2::ggplot(data, aes_mapping) +
                    ggpattern::geom_col_pattern(
                        aes(pattern = if (!is.null(fill_var)) .data[[fill_var]] else NULL),
                        fill = 'white',
                        colour = 'black',
                        pattern_density = 0.5,
                        pattern_fill = 'black',
                        pattern_colour = 'darkgrey',
                        width = self$options$bar_width
                    ) +
                    ggplot2::theme_bw() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 12, face = "bold"),
                        legend.position = self$options$legend_position
                    )

                # Map pattern types if specified
                if (pattern_type != "none" && pattern_type != "auto") {
                    pattern_values <- switch(pattern_type,
                        "stripe" = "stripe",
                        "crosshatch" = "crosshatch",
                        "dot" = "circle",
                        "grid" = "grid",
                        "wave" = "wave",
                        "gradient" = "gradient",
                        "stripe"  # default
                    )

                    if (!is.null(fill_var)) {
                        n_categories <- length(unique(data[[fill_var]]))
                        patterns <- rep(c("stripe", "crosshatch", "circle", "none"),
                                      length.out = n_categories)
                        plot <- plot + ggpattern::scale_pattern_manual(values = patterns)
                    }
                }
            } else {
                # Fallback to grayscale if ggpattern not available
                warning("ggpattern package not found. Using grayscale fallback.")

                aes_mapping <- private$.create_base_aesthetic(data, x_var, y_column, fill_var)

                plot <- ggplot2::ggplot(data, aes_mapping) +
                    ggplot2::geom_col(
                        width = self$options$bar_width,
                        color = "black",
                        size = 0.5
                    ) +
                    ggplot2::scale_fill_grey(start = 0.2, end = 0.8) +
                    ggplot2::theme_bw() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 12, face = "bold"),
                        legend.position = self$options$legend_position
                    )
            }

            # Add value labels
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

        .create_prism_style_plot = function(data, x_var, y_column, fill_var) {
            # Create basic bar plot with modern aes() syntax
            aes_mapping <- ggplot2::aes(x = .data[[x_var]], y = .data[[y_column]])

            if (!is.null(fill_var) && fill_var != "") {
                aes_mapping$fill <- rlang::sym(fill_var)
            }

            plot <- ggplot2::ggplot(data, aes_mapping)

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

            if (private$.safe_require_package("ggprism", "GraphPad Prism themes")) {
                tryCatch({
                    # Apply authentic GraphPad Prism themes
                    return(switch(theme_name,
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
                        # Default Prism theme
                        ggprism::theme_prism(base_size = base_size)
                    ))
                }, error = function(e) {
                    warning(paste("Prism theme error:", e$message, "Using fallback theme."))
                    return(private$.get_prism_fallback_theme(base_size))
                })
            } else {
                return(private$.get_prism_fallback_theme(base_size))
            }
        },

        .get_prism_fallback_theme = function(base_size = 12) {
            # High-quality fallback that approximates Prism styling
            ggplot2::theme_classic(base_size = base_size) +
                ggplot2::theme(
                    # Panel and background
                    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                    panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),

                    # Axes
                    axis.line = ggplot2::element_line(colour = "black", size = 0.8),
                    axis.ticks = ggplot2::element_line(colour = "black", size = 0.6),
                    axis.text = ggplot2::element_text(colour = "black", size = base_size - 1),
                    axis.title = ggplot2::element_text(colour = "black", size = base_size, face = "bold"),

                    # Title and legend
                    plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5),
                    legend.background = ggplot2::element_rect(fill = "white", colour = "black"),
                    legend.key = ggplot2::element_rect(fill = "white", colour = NA),

                    # Grid
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank()
                )
        },

        .apply_prism_palette = function(plot, palette_name) {
            if (private$.safe_require_package("ggprism", "GraphPad Prism palettes")) {
                tryCatch({
                    return(plot + ggprism::scale_fill_prism(palette = palette_name))
                }, error = function(e) {
                    warning(paste("Prism palette '", palette_name, "' not found. Using fallback."))
                    return(private$.apply_prism_fallback(plot, palette_name))
                })
            } else {
                return(private$.apply_prism_fallback(plot, palette_name))
            }
        },

        .apply_prism_fallback = function(plot, palette_name) {
            # Fallback colors that approximate Prism palettes
            fallback_colors <- switch(palette_name,
                "floral" = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#F9CA24", "#F0932B", "#EB4D4B"),
                "candy_bright" = c("#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43", "#C44569"),
                "office" = c("#3742FA", "#2F3542", "#57606F", "#A4B0BE", "#747D8C", "#DDD"),
                "pastels" = c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFDFBA", "#E1BAFF"),
                "colorblind_safe" = c("#0173B2", "#DE8F05", "#029E73", "#CC78BC", "#CA9161", "#FBAFE4"),
                "blueprint" = c("#1B2951", "#3C5A99", "#A8DADC", "#457B9D", "#1D3557", "#F1FAEE"),
                "neon" = c("#FF073A", "#39FF14", "#FF5F1F", "#FFFF33", "#BC13FE", "#00FFFF"),
                "ocean" = c("#006994", "#13A8C0", "#84C3CE", "#B8E6E6", "#A7F0F0", "#0077BE"),
                "spring" = c("#90EE90", "#98FB98", "#00FF7F", "#32CD32", "#7CFC00", "#ADFF2F"),
                # Default fallback
                c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
            )

            return(plot + ggplot2::scale_fill_manual(values = fallback_colors))
        },

        .apply_color_palette = function(plot, palette_name) {
            tryCatch({
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
                    if (private$.safe_require_package("viridis", "Viridis color palette")) {
                        return(plot + viridis::scale_fill_viridis_d())
                    } else {
                        # Fallback to similar colors
                        viridis_like <- c("#440154", "#31688e", "#35b779", "#fde725")
                        return(plot + ggplot2::scale_fill_manual(values = viridis_like))
                    }
                } else if (palette_name %in% c("set1", "dark2")) {
                    return(plot + ggplot2::scale_fill_brewer(type = "qual", palette = tools::toTitleCase(palette_name)))
                } else if (palette_name == "clinical") {
                    clinical_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
                    return(plot + ggplot2::scale_fill_manual(values = clinical_colors))
                } else if (palette_name == "colorblind") {
                    cb_colors <- c("#0173B2", "#DE8F05", "#029E73", "#CC78BC", "#CA9161", "#FBAFE4")
                    return(plot + ggplot2::scale_fill_manual(values = cb_colors))
                } else if (palette_name == "nature") {
                    nature_colors <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4")
                    return(plot + ggplot2::scale_fill_manual(values = nature_colors))
                } else if (palette_name == "science") {
                    science_colors <- c("#3182bd", "#e6550d", "#31a354", "#756bb1", "#636363", "#969696")
                    return(plot + ggplot2::scale_fill_manual(values = science_colors))
                # GraphPad Prism color palettes with fallbacks
                } else if (startsWith(palette_name, "prism_")) {
                    prism_name <- gsub("prism_", "", palette_name)
                    return(private$.apply_prism_palette(plot, prism_name))
                }

                return(plot)
            }, error = function(e) {
                warning(paste("Color palette '", palette_name, "' failed:", e$message, "Using default colors."))
                return(plot)
            })
        },

        .format_values = function(values) {
            format_type <- self$options$value_format

            tryCatch({
                if (format_type == "integer") {
                    return(as.character(round(values)))
                } else if (format_type == "decimal1") {
                    return(sprintf("%.1f", values))
                } else if (format_type == "decimal2") {
                    return(sprintf("%.2f", values))
                } else if (format_type == "percentage") {
                    if (private$.safe_require_package("scales", "Percentage formatting")) {
                        return(scales::percent(values, accuracy = 0.1))
                    } else {
                        return(paste0(round(values * 100, 1), "%"))
                    }
                } else if (format_type == "scientific") {
                    return(format(values, scientific = TRUE, digits = 3))
                } else {
                    # Auto format with better logic
                    if (all(values == round(values), na.rm = TRUE)) {
                        return(as.character(round(values)))
                    } else if (all(abs(values) < 0.01, na.rm = TRUE)) {
                        return(format(values, scientific = TRUE, digits = 2))
                    } else {
                        return(sprintf("%.2f", values))
                    }
                }
            }, error = function(e) {
                warning(paste("Value formatting error:", e$message))
                return(as.character(round(values, 2)))
            })
        },

        .get_plot_titles = function() {
            list(
                title = if (self$options$plot_title != "") self$options$plot_title else NULL,
                x = if (self$options$x_title != "") self$options$x_title else self$options$x_var,
                y = if (self$options$y_title != "") self$options$y_title else self$options$y_var
            )
        },

        # Enhanced comparison grid plot function with memory optimization
        .plot_comparison = function(image, ggtheme, theme, ...) {
            if (!self$options$show_comparison) return()

            data <- private$.processed_data
            if (is.null(data)) return()

            # Memory optimization for large datasets
            if (nrow(data) > 1000) {
                warning("Large dataset detected. Comparison grid may be slow to render.")
            }

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
                # Create all 5 approach plots
                plot_basic <- private$.create_basic_plot(data, x_var, y_column, fill_var) +
                    ggplot2::ggtitle("1. Basic ggplot2") +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold"))

                plot_polished <- private$.create_polished_plot(data, x_var, y_column, fill_var) +
                    ggplot2::ggtitle("2. Polished Presentation") +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold"))

                plot_statistical <- private$.create_statistical_plot(data, x_var, y_column, fill_var) +
                    ggplot2::ggtitle("3. Statistical Annotations") +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold"))

                plot_interactive <- private$.create_polished_plot(data, x_var, y_column, fill_var) +
                    ggplot2::ggtitle("4. Interactive Style") +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold"))

                plot_publication <- private$.create_publication_plot(data, x_var, y_column, fill_var) +
                    ggplot2::ggtitle("5. Publication Ready") +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold"))

                # Combine plots using patchwork if available, otherwise gridExtra
                if (requireNamespace("patchwork", quietly = TRUE)) {
                    combined_plot <- (plot_basic + plot_polished) /
                                   (plot_statistical + plot_interactive) /
                                   (plot_publication + ggplot2::ggplot() + ggplot2::theme_void())

                    combined_plot <- combined_plot +
                        patchwork::plot_annotation(
                            title = "Advanced Bar Charts - 5 Approaches Comparison",
                            subtitle = paste("Data:", x_var, "vs", y_var),
                            theme = ggplot2::theme(
                                plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
                                plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
                            )
                        )

                } else if (requireNamespace("gridExtra", quietly = TRUE)) {
                    # Fallback to gridExtra
                    title_text <- grid::textGrob("Advanced Bar Charts - 5 Approaches Comparison",
                                               gp = grid::gpar(fontsize = 16, fontface = "bold"))

                    combined_plot <- gridExtra::grid.arrange(
                        plot_basic, plot_polished, plot_statistical,
                        plot_interactive, plot_publication,
                        ncol = 2, nrow = 3,
                        top = title_text
                    )

                } else {
                    # Simple arrangement fallback
                    combined_plot <- plot_basic +
                        ggplot2::labs(title = "Comparison Plot - Install 'patchwork' package for full comparison grid")
                }

                print(combined_plot)
                TRUE

            }, error = function(e) {
                warning(paste("Error creating comparison plot:", e$message))

                # Fallback error message plot
                error_plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = paste("Comparison plot error:\n", e$message,
                                                "\n\nInstall 'patchwork' or 'gridExtra' packages\nfor full comparison functionality"),
                                    hjust = 0.5, vjust = 0.5, size = 4) +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Comparison Grid - Error")

                print(error_plot)
                TRUE
            })
        }
    )
)

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
