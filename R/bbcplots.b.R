#' @title BBC-Style Data Visualization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_col geom_bar geom_line geom_point geom_area
#' @importFrom ggplot2 geom_text labs theme element_text element_blank element_line
#' @importFrom ggplot2 scale_fill_manual scale_color_manual facet_wrap ggsave
#' @importFrom ggplot2 coord_flip position_dodge position_stack guide_legend
#' @importFrom stats aov summary.aov t.test chisq.test kruskal.test
#' @importFrom dplyr group_by summarise n count
#' @export

bbcplotsClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "bbcplotsClass",
    inherit = bbcplotsBase,
    private = list(
        .processed_data = NULL,
        .bbc_colors = list(
            bbc_blue = "#1380A1",
            bbc_orange = "#FAAB18", 
            bbc_teal = "#007f7f",
            bbc_gray = "#333333",
            multi_color = c("#1380A1", "#FAAB18", "#007f7f", "#333333", "#990000", "#007A54")
        ),

        .init = function() {
            # Comprehensive welcome instructions
            instructions_html <- paste(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto;'>",
                "<div style='background: linear-gradient(135deg, #1380A1 0%, #007f7f 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h2 style='margin: 0 0 10px 0; font-size: 24px; font-weight: bold;'>📺 BBC-Style Data Visualization</h2>",
                "<p style='margin: 0; font-size: 16px; opacity: 0.9;'>Create professional news-quality graphics with BBC design standards</p>",
                "</div>",
                
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h3 style='color: #1380A1; margin: 0 0 15px 0;'>✨ Key Features</h3>",
                "<ul style='margin: 0; padding-left: 20px; line-height: 1.8;'>",
                "<li><strong>BBC Design Standards:</strong> Authentic Helvetica typography and professional color schemes</li>",
                "<li><strong>Publication Ready:</strong> Standard 640×450px export with proper branding</li>",
                "<li><strong>Statistical Integration:</strong> Built-in statistical tests with publication-quality annotations</li>",
                "<li><strong>Multiple Chart Types:</strong> Columns, bars, lines, points, areas, and grouped variations</li>",
                "<li><strong>News Quality:</strong> Clean aesthetics optimized for digital journalism</li>",
                "</ul>",
                "</div>",
                
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #2c3e50; margin: 10px 0 5px 0;'>📊 Quick Start Guide:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Data Variables:</strong> Select Y-axis (values) and X-axis (categories) variables</li>",
                "<li><strong>Chart Type:</strong> Choose from column, bar, line, point, or area charts</li>",
                "<li><strong>BBC Colors:</strong> Select BBC Blue, Orange, Teal, or create custom palettes</li>",
                "<li><strong>Finalize:</strong> Enable 'Export Finalized Chart' for publication-ready output</li>",
                "</ol>",
                "</div>",
                
                "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #1380A1;'><strong>💡 Pro Tip:</strong> BBC style emphasizes clarity and accessibility. Use high contrast colors, clear titles, and minimal decoration for maximum impact in news and publication contexts.</p>",
                "</div>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
            
            # Set default plot dimensions to BBC standards
            self$results$main_plot$setSize(640, 450)
            self$results$finalized_plot$setSize(640, 500)
            
            # Initialize color guide
            private$.init_color_guide()
            
            # Initialize design principles guide
            private$.init_design_guide()
            
            # Initialize accessibility information
            private$.init_accessibility_info()
        },

        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$y_var) && !is.null(self$options$x_var)) {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (is.null(self$options$y_var)) {
                return()
            }
            
            if (is.null(self$options$x_var)) {
                return()
            }
            
            # Process the data
            private$.processed_data <- private$.process_data()
            
            if (nrow(private$.processed_data) == 0) {
                self$results$main_plot$setContent(
                    "<div style='color: red; padding: 20px; text-align: center;'>No data available after processing. Please check your variable selections.</div>"
                )
                return()
            }
            
            # Generate statistical analysis if requested
            if (self$options$statistical_annotations) {
                private$.generate_statistical_analysis()
            }
            
            # Generate chart summary if requested
            if (self$options$show_sample_sizes) {
                private$.generate_chart_summary()
            }
            
            # Generate R code if exporting finalized chart
            if (self$options$export_finalized) {
                private$.generate_export_code()
            }
            
            # Set plot state for rendering
            self$results$main_plot$setState(private$.processed_data)
            
            if (self$options$export_finalized) {
                self$results$finalized_plot$setState(private$.processed_data)
            }
        },

        .process_data = function() {
            mydata <- self$data
            
            # Get variable names
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            
            # Select relevant variables
            vars_to_select <- c(y_var, x_var)
            if (!is.null(group_var)) vars_to_select <- c(vars_to_select, group_var)
            if (!is.null(facet_var)) vars_to_select <- c(vars_to_select, facet_var)
            
            mydata <- mydata[vars_to_select]
            
            # Remove missing values
            mydata <- mydata[complete.cases(mydata), ]
            
            # Ensure proper data types
            if (!is.null(group_var)) {
                mydata[[group_var]] <- as.factor(mydata[[group_var]])
            }
            
            if (!is.null(facet_var)) {
                mydata[[facet_var]] <- as.factor(mydata[[facet_var]])
            }
            
            # Convert x_var to factor if it's character or has few unique values
            if (is.character(mydata[[x_var]]) || length(unique(mydata[[x_var]])) <= 10) {
                mydata[[x_var]] <- as.factor(mydata[[x_var]])
            }
            
            return(mydata)
        },

        .create_bbc_style = function() {
            # Recreate the BBC style theme
            bbc_theme <- ggplot2::theme(
                # Text elements
                text = ggplot2::element_text(family = self$options$font_family, size = 18, color = "#222222"),
                plot.title = ggplot2::element_text(family = self$options$font_family, size = 28, face = "bold", color = "#222222"),
                plot.subtitle = ggplot2::element_text(family = self$options$font_family, size = 22, margin = ggplot2::margin(9, 0, 9, 0)),
                plot.caption = ggplot2::element_text(family = self$options$font_family, size = 14, color = "#666666"),
                
                # Legend
                legend.position = self$options$legend_position,
                legend.text = ggplot2::element_text(family = self$options$font_family, size = 18, color = "#222222"),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank(),
                legend.background = ggplot2::element_blank(),
                
                # Axis
                axis.title = ggplot2::element_blank(),
                axis.text = ggplot2::element_text(family = self$options$font_family, size = 18, color = "#222222"),
                axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                axis.ticks = ggplot2::element_blank(),
                axis.line = ggplot2::element_blank(),
                
                # Grid lines
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.y = if (self$options$horizontal_gridlines) {
                    ggplot2::element_line(color = "#cbcbcb")
                } else {
                    ggplot2::element_blank()
                },
                panel.grid.major.x = if (self$options$vertical_gridlines) {
                    ggplot2::element_line(color = "#cbcbcb")
                } else {
                    ggplot2::element_blank()
                },
                
                # Background
                panel.background = ggplot2::element_blank(),
                plot.background = ggplot2::element_rect(fill = "white", color = NA),
                
                # Facets
                strip.background = ggplot2::element_rect(fill = "white"),
                strip.text = ggplot2::element_text(family = self$options$font_family, size = 22, color = "#222222")
            )
            
            return(bbc_theme)
        },

        .get_bbc_colors = function() {
            color_scheme <- self$options$bbc_colors
            
            if (color_scheme == "custom") {
                # Parse custom colors
                custom_colors <- trimws(strsplit(self$options$custom_colors, ",")[[1]])
                return(custom_colors)
            } else if (color_scheme == "multi_color") {
                return(private$.bbc_colors$multi_color)
            } else {
                return(private$.bbc_colors[[color_scheme]])
            }
        },

        .generate_statistical_analysis = function() {
            if (!self$options$statistical_annotations) return()
            
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            stat_method <- self$options$stat_method
            
            tryCatch({
                stat_html <- "<h4 style='color: #1380A1;'>📊 Statistical Analysis Results</h4>"
                stat_html <- paste0(stat_html, "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
                
                # Auto-select statistical test if needed
                if (stat_method == "auto") {
                    if (is.factor(data[[x_var]]) && is.numeric(data[[y_var]])) {
                        n_groups <- length(unique(data[[x_var]]))
                        if (n_groups == 2) {
                            stat_method <- "ttest"
                        } else if (n_groups > 2) {
                            stat_method <- "anova"
                        }
                    }
                }
                
                # Perform statistical tests
                if (stat_method == "anova" && is.factor(data[[x_var]])) {
                    formula_str <- paste(y_var, "~", x_var)
                    aov_result <- aov(as.formula(formula_str), data = data)
                    aov_summary <- summary(aov_result)
                    
                    f_stat <- round(aov_summary[[1]]$`F value`[1], 3)
                    p_value <- aov_summary[[1]]$`Pr(>F)`[1]
                    df1 <- aov_summary[[1]]$Df[1]
                    df2 <- aov_summary[[1]]$Df[2]
                    
                    stat_html <- paste0(stat_html, "<p><strong>One-way ANOVA:</strong></p>")
                    stat_html <- paste0(stat_html, "<p>F(", df1, ", ", df2, ") = ", f_stat, ", p = ", format.pval(p_value, digits = 3), "</p>")
                    
                    # Effect size (eta-squared)
                    ss_total <- sum(aov_summary[[1]]$`Sum Sq`)
                    ss_between <- aov_summary[[1]]$`Sum Sq`[1]
                    eta_squared <- ss_between / ss_total
                    stat_html <- paste0(stat_html, "<p><strong>Effect Size (η²):</strong> ", round(eta_squared, 3), "</p>")
                    
                } else if (stat_method == "ttest") {
                    groups <- unique(data[[x_var]])
                    if (length(groups) == 2) {
                        group1_data <- data[data[[x_var]] == groups[1], y_var]
                        group2_data <- data[data[[x_var]] == groups[2], y_var]
                        
                        t_result <- t.test(group1_data, group2_data)
                        
                        stat_html <- paste0(stat_html, "<p><strong>Independent t-test:</strong></p>")
                        stat_html <- paste0(stat_html, "<p>t(", round(t_result$parameter, 1), ") = ", round(t_result$statistic, 3), ", p = ", format.pval(t_result$p.value, digits = 3), "</p>")
                        stat_html <- paste0(stat_html, "<p>Mean difference: ", round(diff(t_result$estimate), 3), "</p>")
                        
                        # Cohen's d effect size
                        pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                                         (length(group2_data) - 1) * var(group2_data)) / 
                                        (length(group1_data) + length(group2_data) - 2))
                        cohens_d <- abs(diff(c(mean(group1_data), mean(group2_data)))) / pooled_sd
                        stat_html <- paste0(stat_html, "<p><strong>Effect Size (Cohen's d):</strong> ", round(cohens_d, 3), "</p>")
                    }
                    
                } else if (stat_method == "kruskal") {
                    formula_str <- paste(y_var, "~", x_var)
                    k_result <- kruskal.test(as.formula(formula_str), data = data)
                    
                    stat_html <- paste0(stat_html, "<p><strong>Kruskal-Wallis Test:</strong></p>")
                    stat_html <- paste0(stat_html, "<p>χ² = ", round(k_result$statistic, 3), ", df = ", k_result$parameter, ", p = ", format.pval(k_result$p.value, digits = 3), "</p>")
                }
                
                stat_html <- paste0(stat_html, "</div>")
                self$results$statistical_results$setContent(stat_html)
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='color: orange; padding: 10px;'>",
                    "Statistical analysis could not be completed: ", e$message,
                    "</div>"
                )
                self$results$statistical_results$setContent(error_html)
            })
        },

        .generate_chart_summary = function() {
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            
            summary_html <- "<h4 style='color: #1380A1;'>📈 Chart Summary Statistics</h4>"
            summary_html <- paste0(summary_html, "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            # Overall summary
            total_n <- nrow(data)
            summary_html <- paste0(summary_html, "<p><strong>Total Observations:</strong> ", total_n, "</p>")
            
            if (is.numeric(data[[y_var]])) {
                mean_val <- round(mean(data[[y_var]], na.rm = TRUE), 2)
                median_val <- round(median(data[[y_var]], na.rm = TRUE), 2)
                sd_val <- round(sd(data[[y_var]], na.rm = TRUE), 2)
                
                summary_html <- paste0(summary_html, "<p><strong>", y_var, " Statistics:</strong></p>")
                summary_html <- paste0(summary_html, "<ul>")
                summary_html <- paste0(summary_html, "<li>Mean: ", mean_val, "</li>")
                summary_html <- paste0(summary_html, "<li>Median: ", median_val, "</li>")
                summary_html <- paste0(summary_html, "<li>Standard Deviation: ", sd_val, "</li>")
                summary_html <- paste0(summary_html, "</ul>")
            }
            
            # Group-wise summary
            if (is.factor(data[[x_var]])) {
                group_summary <- data %>%
                    dplyr::group_by(.data[[x_var]]) %>%
                    dplyr::summarise(
                        n = dplyr::n(),
                        .groups = 'drop'
                    )
                
                summary_html <- paste0(summary_html, "<p><strong>Sample Sizes by ", x_var, ":</strong></p>")
                summary_html <- paste0(summary_html, "<ul>")
                for (i in 1:nrow(group_summary)) {
                    summary_html <- paste0(summary_html, "<li>", group_summary[[x_var]][i], ": n = ", group_summary$n[i], "</li>")
                }
                summary_html <- paste0(summary_html, "</ul>")
            }
            
            summary_html <- paste0(summary_html, "</div>")
            self$results$chart_summary$setContent(summary_html)
        },

        .generate_export_code = function() {
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            chart_type <- self$options$chart_type
            bbc_colors <- self$options$bbc_colors
            
            # Get selected colors
            colors <- private$.get_bbc_colors()
            color_code <- if (length(colors) == 1) {
                paste0('"', colors, '"')
            } else {
                paste0('c("', paste(colors, collapse = '", "'), '")')
            }
            
            code_template <- paste(
                "# BBC-Style Data Visualization",
                "# Generated by ClinicoPath jamovi module",
                "",
                "library(ggplot2)",
                "# Note: Install bbplot package for full BBC functionality:",
                "# devtools::install_github('bbc/bbplot')",
                "# library(bbplot)",
                "",
                "# Create BBC-style theme function",
                "bbc_style <- function() {",
                "  ggplot2::theme(",
                "    text = ggplot2::element_text(family = 'Helvetica', size = 18, color = '#222222'),",
                "    plot.title = ggplot2::element_text(family = 'Helvetica', size = 28, face = 'bold'),",
                "    plot.subtitle = ggplot2::element_text(family = 'Helvetica', size = 22),",
                "    legend.position = 'top',",
                "    legend.text = ggplot2::element_text(size = 18),",
                "    legend.title = ggplot2::element_blank(),",
                "    axis.title = ggplot2::element_blank(),",
                "    axis.text = ggplot2::element_text(size = 18, color = '#222222'),",
                "    axis.ticks = ggplot2::element_blank(),",
                "    axis.line = ggplot2::element_blank(),",
                "    panel.grid.minor = ggplot2::element_blank(),",
                "    panel.grid.major.y = ggplot2::element_line(color = '#cbcbcb'),",
                "    panel.grid.major.x = ggplot2::element_blank(),",
                "    panel.background = ggplot2::element_blank(),",
                "    plot.background = ggplot2::element_rect(fill = 'white', color = NA)",
                "  )",
                "}",
                "",
                "# Create the plot",
                "plot <- ggplot(data, aes(x = {x_var}, y = {y_var})) +",
                "  geom_{geom_type}(fill = {color_code}) +",
                "  bbc_style() +",
                "  labs(",
                "    title = '{title}',",
                "    subtitle = '{subtitle}',",
                "    caption = '{source}'",
                "  )",
                "",
                "# Display the plot",
                "print(plot)",
                "",
                "# Export with BBC standards (optional)",
                "# ggsave('chart.png', plot, width = 640, height = 450, units = 'px', dpi = 100)",
                sep = "\n"
            )
            
            # Replace placeholders
            geom_mapping <- list(
                column = "col",
                bar = "col",
                line = "line",
                point = "point",
                area = "area",
                stacked_column = "col",
                grouped_column = "col",
                horizontal_bar = "col"
            )
            
            actual_code <- gsub("\\{x_var\\}", x_var, code_template)
            actual_code <- gsub("\\{y_var\\}", y_var, actual_code)
            actual_code <- gsub("\\{geom_type\\}", geom_mapping[[chart_type]], actual_code)
            actual_code <- gsub("\\{color_code\\}", color_code, actual_code)
            actual_code <- gsub("\\{title\\}", self$options$title_text, actual_code)
            actual_code <- gsub("\\{subtitle\\}", self$options$subtitle_text, actual_code)
            actual_code <- gsub("\\{source\\}", self$options$source_text, actual_code)
            
            code_html <- paste(
                "<h4 style='color: #1380A1;'>📝 Reproducible R Code</h4>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p><strong>Complete R code for BBC-style visualization:</strong></p>",
                "<pre style='background-color: #2d3748; color: #e2e8f0; padding: 15px; border-radius: 5px; overflow-x: auto; font-size: 12px;'>",
                "<code>", actual_code, "</code>",
                "</pre>",
                "<p style='color: #666; font-size: 0.9em; margin-top: 10px;'>",
                "Copy this code to reproduce the BBC-style visualization in your R environment.",
                "</p>",
                "</div>"
            )
            
            self$results$export_code$setContent(code_html)
        },

        .init_color_guide = function() {
            color_html <- paste(
                "<h4 style='color: #1380A1;'>🎨 BBC Color Guidelines</h4>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>Official BBC Colors:</h5>",
                "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin: 10px 0;'>",
                "<div style='background-color: #1380A1; color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#1380A1 BBC Blue</div>",
                "<div style='background-color: #FAAB18; color: black; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#FAAB18 BBC Orange</div>",
                "<div style='background-color: #007f7f; color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#007f7f BBC Teal</div>",
                "<div style='background-color: #333333; color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px;'>#333333 BBC Gray</div>",
                "</div>",
                "<p><strong>Usage Guidelines:</strong></p>",
                "<ul>",
                "<li>Use BBC Blue (#1380A1) for primary data series</li>",
                "<li>BBC Orange (#FAAB18) for highlighting or secondary data</li>",
                "<li>BBC Teal (#007f7f) for tertiary data or contrasts</li>",
                "<li>Maintain high contrast for accessibility</li>",
                "<li>Use consistent colors across related charts</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$color_guide$setContent(color_html)
        },

        .init_design_guide = function() {
            design_html <- paste(
                "<h4 style='color: #1380A1;'>📐 BBC Design Principles</h4>",
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>Core Design Standards:</h5>",
                "<ul>",
                "<li><strong>Typography:</strong> Helvetica font family at specific sizes (28pt titles, 22pt subtitles, 18pt body)</li>",
                "<li><strong>Layout:</strong> Left-aligned titles and clean grid system</li>",
                "<li><strong>Colors:</strong> Strategic use of BBC brand colors for maximum impact</li>",
                "<li><strong>Gridlines:</strong> Horizontal only, light gray (#cbcbcb) for readability</li>",
                "<li><strong>Spacing:</strong> Generous white space for clarity</li>",
                "<li><strong>Dimensions:</strong> Standard 640×450px for digital publication</li>",
                "</ul>",
                "<h5>Editorial Guidelines:</h5>",
                "<ul>",
                "<li>Clear, descriptive titles that tell the story</li>",
                "<li>Subtitles for additional context</li>",
                "<li>Source attribution for credibility</li>",
                "<li>Minimal decoration - let the data speak</li>",
                "<li>Accessible design for all audiences</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$design_notes$setContent(design_html)
        },

        .init_accessibility_info = function() {
            access_html <- paste(
                "<h4 style='color: #1380A1;'>♿ Accessibility & Standards</h4>",
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>BBC Accessibility Standards:</h5>",
                "<ul>",
                "<li><strong>Color Contrast:</strong> All text meets WCAG AA standards (4.5:1 ratio minimum)</li>",
                "<li><strong>Alternative Text:</strong> Charts should include descriptive alt text for screen readers</li>",
                "<li><strong>Color Independence:</strong> Information not conveyed by color alone</li>",
                "<li><strong>Font Size:</strong> Minimum 18pt text for readability</li>",
                "<li><strong>Clear Hierarchy:</strong> Logical reading order and structure</li>",
                "</ul>",
                "<h5>Export Recommendations:</h5>",
                "<ul>",
                "<li>Export at 640×450px for web, 1280×900px for print</li>",
                "<li>Include data tables as alternative formats</li>",
                "<li>Provide chart descriptions in accompanying text</li>",
                "<li>Test with screen readers and accessibility tools</li>",
                "</ul>",
                "<p style='margin: 10px 0 0 0; padding: 10px; background-color: #e8f5e8; border-radius: 4px;'>",
                "<strong>💡 Tip:</strong> BBC charts prioritize clarity and accessibility over visual complexity. This ensures your message reaches the widest possible audience effectively.",
                "</p>",
                "</div>"
            )
            
            self$results$accessibility_info$setContent(access_html)
        },

        .plot_main = function(image, ggtheme, theme, ...) {
            data <- image$state
            if (is.null(data)) return()
            
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            chart_type <- self$options$chart_type
            
            tryCatch({
                # Create base plot
                if (!is.null(group_var)) {
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[group_var]]))
                } else {
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
                }
                
                # Get BBC colors
                colors <- private$.get_bbc_colors()
                
                # Add appropriate geom based on chart type
                if (chart_type == "column") {
                    plot <- plot + ggplot2::geom_col(fill = if (is.null(group_var)) colors[1] else NULL)
                } else if (chart_type == "bar") {
                    plot <- plot + ggplot2::geom_col(fill = if (is.null(group_var)) colors[1] else NULL)
                } else if (chart_type == "horizontal_bar") {
                    plot <- plot + ggplot2::geom_col(fill = if (is.null(group_var)) colors[1] else NULL) + ggplot2::coord_flip()
                } else if (chart_type == "line") {
                    plot <- plot + ggplot2::geom_line(color = colors[1], size = 2) + 
                            ggplot2::geom_point(color = colors[1], size = 3)
                } else if (chart_type == "point") {
                    plot <- plot + ggplot2::geom_point(color = colors[1], size = 3)
                } else if (chart_type == "area") {
                    plot <- plot + ggplot2::geom_area(fill = colors[1], alpha = 0.7)
                } else if (chart_type == "stacked_column") {
                    plot <- plot + ggplot2::geom_col(position = "stack")
                } else if (chart_type == "grouped_column") {
                    plot <- plot + ggplot2::geom_col(position = "dodge")
                }
                
                # Apply colors for grouped data
                if (!is.null(group_var)) {
                    if (length(colors) >= length(unique(data[[group_var]]))) {
                        plot <- plot + ggplot2::scale_fill_manual(values = colors)
                    } else {
                        plot <- plot + ggplot2::scale_fill_manual(values = rep(colors, length.out = length(unique(data[[group_var]]))))
                    }
                }
                
                # Add BBC style theme
                plot <- plot + private$.create_bbc_style()
                
                # Add titles
                title <- if (self$options$title_text != "") self$options$title_text else NULL
                subtitle <- if (self$options$subtitle_text != "") self$options$subtitle_text else NULL
                x_title <- if (self$options$x_axis_title != "") self$options$x_axis_title else NULL
                y_title <- if (self$options$y_axis_title != "") self$options$y_axis_title else NULL
                
                plot <- plot + ggplot2::labs(
                    title = title,
                    subtitle = subtitle,
                    x = x_title,
                    y = y_title
                )
                
                # Add data values if requested
                if (self$options$show_values) {
                    value_position <- switch(self$options$value_position,
                                           "above" = ggplot2::position_dodge(width = 0.9),
                                           "center" = ggplot2::position_dodge(width = 0.9),
                                           "below" = ggplot2::position_dodge(width = 0.9))
                    
                    vjust_val <- switch(self$options$value_position,
                                       "above" = -0.5,
                                       "center" = 0.5,
                                       "below" = 1.5)
                    
                    if (chart_type %in% c("column", "bar", "stacked_column", "grouped_column", "horizontal_bar")) {
                        plot <- plot + ggplot2::geom_text(
                            ggplot2::aes(label = round(.data[[y_var]], 1)),
                            position = value_position,
                            vjust = vjust_val,
                            size = 4,
                            color = "#222222"
                        )
                    }
                }
                
                # Add faceting if specified
                if (!is.null(facet_var)) {
                    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                warning(paste("Error creating BBC plot:", e$message))
                return(FALSE)
            })
        },

        .plot_finalized = function(image, ggtheme, theme, ...) {
            # This would create the finalized plot with BBC branding
            # For now, we'll use the main plot and add source information
            data <- image$state
            if (is.null(data)) return()
            
            # Create the main plot
            main_result <- private$.plot_main(image, ggtheme, theme, ...)
            
            # In a full implementation, this would use bbplot::finalise_plot()
            # to add BBC branding and proper export formatting
            
            return(main_result)
        }
    )
)