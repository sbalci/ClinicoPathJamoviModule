#' @title Raincloud Plot for Distribution Visualization
#' @return Raincloud plot using ggdist for comprehensive distribution analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_boxplot coord_flip labs theme_minimal theme_classic
#' @importFrom ggplot2 scale_fill_manual scale_color_manual theme element_text facet_wrap
#' @importFrom ggplot2 theme_bw scale_fill_viridis_d scale_color_viridis_d
#' @importFrom ggdist stat_halfeye stat_dots
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales alpha
#' @importFrom stringr str_to_title
#' @importFrom htmltools HTML

raincloudClass <- if (requireNamespace("jmvcore")) R6::R6Class("raincloudClass",
    inherit = raincloudBase,
    private = list(

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$dep_var) || is.null(self$options$group_var)) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>☁️ Welcome to Raincloud Plot Visualization!</h3>
                <p><strong>Comprehensive distribution analysis</strong> using ggdist raincloud plots</p>
                <p>Based on R-Bloggers tutorial: 'Make a Raincloud Plot to Visualize Distribution in ggplot2'</p>
                
                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>Dependent Variable:</strong> Continuous variable for distribution analysis</li>
                <li><strong>Grouping Variable:</strong> Categorical variable for group comparisons</li>
                </ol>
                
                <h4 style='color: #1976d2;'>What Raincloud Plots Show:</h4>
                <ul>
                <li><strong>Half-Violin Plot:</strong> Probability density distribution shape</li>
                <li><strong>Box Plot:</strong> Median, quartiles, and outliers</li>
                <li><strong>Data Points:</strong> Individual observations as dots</li>
                <li><strong>Distribution Patterns:</strong> Multimodality, skewness, spread</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Clinical Research:</strong> Compare patient groups and biomarker distributions</li>
                <li><strong>Data Exploration:</strong> Reveal patterns hidden in traditional box plots</li>
                <li><strong>Quality Control:</strong> Identify outliers and distribution anomalies</li>
                <li><strong>Publications:</strong> Comprehensive, publication-ready visualizations</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Raincloud plots combine three visualization techniques for complete distribution understanding</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Safely require ggdist
            if (!requireNamespace("ggdist", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>ggdist Package Required</h4>
                <p>The ggdist package is required for raincloud plot functionality.</p>
                <p>Please install it using: <code>install.packages('ggdist')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Get data and variables
            dataset <- self$data
            dep_var <- self$options$dep_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            color_var <- self$options$color_var

            # Prepare analysis data
            required_vars <- c(dep_var, group_var)
            if (!is.null(facet_var) && facet_var != "") {
                required_vars <- c(required_vars, facet_var)
            }
            if (!is.null(color_var) && color_var != "") {
                required_vars <- c(required_vars, color_var)
            }
            
            analysis_data <- dataset[required_vars]
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert variables to appropriate types
            analysis_data[[dep_var]] <- as.numeric(analysis_data[[dep_var]])
            analysis_data[[group_var]] <- as.factor(analysis_data[[group_var]])
            
            if (!is.null(facet_var) && facet_var != "") {
                analysis_data[[facet_var]] <- as.factor(analysis_data[[facet_var]])
            }
            if (!is.null(color_var) && color_var != "" && color_var != group_var) {
                analysis_data[[color_var]] <- as.factor(analysis_data[[color_var]])
            }

            # Generate summary statistics if requested
            if (self$options$show_statistics) {
                stats_html <- private$.generate_statistics(analysis_data, dep_var, group_var)
                self$results$statistics$setContent(stats_html)
            }
            
            # Generate outlier analysis if requested
            if (self$options$show_outliers) {
                outlier_html <- private$.generate_outlier_analysis(analysis_data, dep_var, group_var)
                self$results$outliers$setContent(outlier_html)
            }
            
            # Generate normality tests if requested
            if (self$options$normality_test) {
                normality_html <- private$.generate_normality_tests(analysis_data, dep_var, group_var)
                self$results$normality$setContent(normality_html)
            }
            
            # Generate group comparisons if requested
            if (self$options$comparison_test) {
                comparison_html <- private$.generate_group_comparisons(analysis_data, dep_var, group_var)
                self$results$comparison$setContent(comparison_html)
            }
            
            # Generate interpretation guide
            interpretation_html <- private$.generate_interpretation_guide(analysis_data, dep_var, group_var)
            self$results$interpretation$setContent(interpretation_html)

            # Store data for plotting
            private$.analysis_data <- analysis_data

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            # Check if analysis was performed
            if (is.null(private$.analysis_data)) {
                return()
            }
            
            analysis_data <- private$.analysis_data
            dep_var <- self$options$dep_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            color_var <- self$options$color_var
            
            # Determine color mapping
            color_mapping <- if (!is.null(color_var) && color_var != "" && color_var != group_var) {
                color_var
            } else {
                group_var
            }
            
            # Create base plot
            p <- ggplot2::ggplot(analysis_data, ggplot2::aes_string(x = group_var, y = dep_var, 
                                                                   fill = color_mapping, color = color_mapping))
            
            # Add raincloud components based on options
            if (self$options$show_violin) {
                p <- p + ggdist::stat_halfeye(
                    alpha = self$options$alpha_violin,
                    width = self$options$violin_width,
                    .width = 0,
                    point_colour = NA
                )
            }
            
            if (self$options$show_boxplot) {
                p <- p + ggplot2::geom_boxplot(
                    width = self$options$box_width,
                    alpha = 0.7,
                    outlier.shape = if (self$options$show_outliers) 16 else NA
                )
            }
            
            if (self$options$show_dots) {
                dots_side <- self$options$dots_side
                p <- p + ggdist::stat_dots(
                    side = dots_side,
                    dotsize = self$options$dots_size,
                    alpha = self$options$alpha_dots
                )
            }
            
            # Apply orientation
            if (self$options$orientation == "horizontal") {
                p <- p + ggplot2::coord_flip()
            }
            
            # Add faceting if specified
            if (!is.null(facet_var) && facet_var != "") {
                p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))
            }
            
            # Apply color palette
            colors <- private$.get_color_palette(length(levels(analysis_data[[color_mapping]])))
            p <- p + ggplot2::scale_fill_manual(values = scales::alpha(colors, self$options$alpha_violin))
            p <- p + ggplot2::scale_color_manual(values = colors)
            
            # Apply theme
            p <- p + private$.get_plot_theme()
            
            # Add labels
            x_label <- if (self$options$x_label != "") self$options$x_label else 
                      if (self$options$orientation == "horizontal") dep_var else group_var
            y_label <- if (self$options$y_label != "") self$options$y_label else 
                      if (self$options$orientation == "horizontal") group_var else dep_var
            plot_title <- if (self$options$plot_title != "") self$options$plot_title else "Raincloud Plot - Distribution Visualization"
            
            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_label,
                y = y_label,
                fill = color_mapping,
                color = color_mapping
            )
            
            print(p)
            TRUE
        },

        .get_color_palette = function(n_colors) {
            palette_name <- self$options$color_palette
            
            if (palette_name == "viridis") {
                return(viridis::viridis(n_colors, discrete = TRUE))
            } else if (palette_name == "clinical") {
                clinical_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")
                return(rep(clinical_colors, length.out = n_colors))
            } else if (palette_name == "pastel") {
                pastel_colors <- c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFDFBA", "#E0BBE4")
                return(rep(pastel_colors, length.out = n_colors))
            } else if (palette_name %in% c("set1", "set2", "dark2")) {
                palette_r_name <- switch(palette_name,
                    "set1" = "Set1",
                    "set2" = "Set2", 
                    "dark2" = "Dark2"
                )
                if (n_colors <= 8) {
                    return(RColorBrewer::brewer.pal(min(max(3, n_colors), 8), palette_r_name))
                } else {
                    base_colors <- RColorBrewer::brewer.pal(8, palette_r_name)
                    return(grDevices::colorRampPalette(base_colors)(n_colors))
                }
            # GraphPad Prism color palettes
            } else if (palette_name %in% c("prism_floral", "prism_candy_bright", "prism_office", 
                                          "prism_pastels", "prism_colorblind_safe", "prism_ocean", "prism_spring")) {
                return(private$.get_prism_palette_colors(palette_name, n_colors))
            } else {
                # Default ggplot2 colors
                return(scales::hue_pal()(n_colors))
            }
        },
        
        .get_prism_palette_colors = function(palette_name, n_colors) {
            # Convert internal palette names to ggprism names
            prism_name <- switch(palette_name,
                "prism_floral" = "floral",
                "prism_candy_bright" = "candy_bright",
                "prism_office" = "office",
                "prism_pastels" = "pastels",
                "prism_colorblind_safe" = "colorblind_safe",
                "prism_ocean" = "ocean",
                "prism_spring" = "spring",
                "floral"  # fallback
            )
            
            if (requireNamespace("ggprism", quietly = TRUE)) {
                # Get colors from ggprism package
                tryCatch({
                    ggprism::prism_colour_pal(palette = prism_name)(n_colors)
                }, error = function(e) {
                    # Fallback to viridis if ggprism palette fails
                    viridis::viridis(n_colors, discrete = TRUE)
                })
            } else {
                # Fallback to viridis if ggprism not available
                viridis::viridis(n_colors, discrete = TRUE)
            }
        },

        .get_plot_theme = function() {
            theme_name <- self$options$plot_theme
            
            base_theme <- switch(theme_name,
                "clinical" = ggplot2::theme_minimal() + 
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(fill = NA, color = "grey20"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        strip.text = ggplot2::element_text(size = 11, face = "bold")
                    ),
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "publication" = ggplot2::theme_bw() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.text = ggplot2::element_text(size = 12),
                        legend.title = ggplot2::element_text(size = 12, face = "bold"),
                        strip.text = ggplot2::element_text(size = 12, face = "bold")
                    ),
                "tidyquant" = ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.grid.major = ggplot2::element_line(color = "grey90"),
                        panel.grid.minor = ggplot2::element_blank(),
                        plot.title = ggplot2::element_text(size = 14, face = "bold")
                    ),
                # GraphPad Prism themes
                "prism_default" = private$.get_prism_theme("default"),
                "prism_white" = private$.get_prism_theme("white"),
                "prism_publication" = private$.get_prism_theme("publication"),
                ggplot2::theme_minimal()  # fallback
            )
            
            return(base_theme)
        },
        
        .get_prism_theme = function(theme_type) {
            base_size <- 12
            
            if (requireNamespace("ggprism", quietly = TRUE)) {
                # Use authentic ggprism themes
                switch(theme_type,
                    "default" = ggprism::theme_prism(base_size = base_size),
                    "white" = ggprism::theme_prism(base_size = base_size, axis_text_angle = 0),
                    "publication" = ggprism::theme_prism(base_size = base_size) +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5),
                            axis.title = ggplot2::element_text(size = base_size + 1, face = "bold"),
                            legend.background = ggplot2::element_rect(fill = "white", colour = "black"),
                            panel.border = ggplot2::element_rect(colour = "black", fill = NA)
                        ),
                    ggprism::theme_prism(base_size = base_size)  # fallback
                )
            } else {
                # Fallback themes that approximate Prism styling
                ggplot2::theme_minimal(base_size = base_size) +
                    ggplot2::theme(
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
                        axis.line = ggplot2::element_line(colour = "black"),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5)
                    )
            }
        },

        .generate_statistics = function(data, dep_var, group_var) {
            # Generate comprehensive statistics table
            
            stats_summary <- data %>%
                dplyr::group_by(.data[[group_var]]) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = round(mean(.data[[dep_var]], na.rm = TRUE), 3),
                    median = round(median(.data[[dep_var]], na.rm = TRUE), 3),
                    sd = round(sd(.data[[dep_var]], na.rm = TRUE), 3),
                    mad = round(mad(.data[[dep_var]], na.rm = TRUE), 3),
                    q1 = round(quantile(.data[[dep_var]], 0.25, na.rm = TRUE), 3),
                    q3 = round(quantile(.data[[dep_var]], 0.75, na.rm = TRUE), 3),
                    iqr = round(q3 - q1, 3),
                    min_val = round(min(.data[[dep_var]], na.rm = TRUE), 3),
                    max_val = round(max(.data[[dep_var]], na.rm = TRUE), 3),
                    skewness = round(moments::skewness(.data[[dep_var]], na.rm = TRUE), 3),
                    kurtosis = round(moments::kurtosis(.data[[dep_var]], na.rm = TRUE), 3),
                    .groups = 'drop'
                )
            
            stats_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📊 Distribution Summary Statistics</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Group</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>N</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Mean</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Median</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>SD</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>IQR</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Range</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Skewness</th>",
                "</tr></thead><tbody>"
            )
            
            for (i in 1:nrow(stats_summary)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                stats_html <- paste0(stats_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>", stats_summary[[group_var]][i], "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$n[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$mean[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$median[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$sd[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$iqr[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$min_val[i], " - ", stats_summary$max_val[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$skewness[i], "</td>",
                    "</tr>"
                )
            }
            
            stats_html <- paste0(stats_html, 
                "</tbody></table>",
                "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                "<em>IQR = Interquartile Range, SD = Standard Deviation. Skewness: 0 = symmetric, >0 = right-skewed, <0 = left-skewed.</em>",
                "</p></div>"
            )
            
            return(stats_html)
        },

        .generate_outlier_analysis = function(data, dep_var, group_var) {
            # Detect and analyze outliers
            
            outlier_method <- self$options$outlier_method
            outliers_list <- list()
            
            for (group in levels(data[[group_var]])) {
                group_data <- data[data[[group_var]] == group, dep_var]
                
                if (outlier_method == "iqr") {
                    q1 <- quantile(group_data, 0.25, na.rm = TRUE)
                    q3 <- quantile(group_data, 0.75, na.rm = TRUE)
                    iqr <- q3 - q1
                    outliers <- which(group_data < (q1 - 1.5 * iqr) | group_data > (q3 + 1.5 * iqr))
                } else if (outlier_method == "zscore") {
                    z_scores <- abs((group_data - mean(group_data, na.rm = TRUE)) / sd(group_data, na.rm = TRUE))
                    outliers <- which(z_scores > 3)
                } else if (outlier_method == "modified_zscore") {
                    median_val <- median(group_data, na.rm = TRUE)
                    mad_val <- mad(group_data, na.rm = TRUE)
                    modified_z <- 0.6745 * (group_data - median_val) / mad_val
                    outliers <- which(abs(modified_z) > 3.5)
                }
                
                outliers_list[[group]] <- length(outliers)
            }
            
            outlier_html <- paste0(
                "<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #856404; margin-top: 0;'>⚠️ Outlier Detection (", stringr::str_to_title(outlier_method), " Method)</h3>",
                "<ul>"
            )
            
            total_outliers <- 0
            for (group in names(outliers_list)) {
                count <- outliers_list[[group]]
                total_outliers <- total_outliers + count
                outlier_html <- paste0(outlier_html,
                    "<li><strong>", group, ":</strong> ", count, " outliers detected</li>"
                )
            }
            
            outlier_html <- paste0(outlier_html,
                "</ul>",
                "<p><strong>Total outliers across all groups:</strong> ", total_outliers, "</p>",
                "<p style='font-size: 12px; color: #856404; margin-top: 15px;'>",
                "<em>", 
                switch(outlier_method,
                    "iqr" = "IQR Method: Values beyond 1.5 × IQR from Q1/Q3",
                    "zscore" = "Z-Score Method: |Z| > 3",
                    "modified_zscore" = "Modified Z-Score Method: |Modified Z| > 3.5"
                ),
                ". Consider investigating these points for data quality or interesting patterns.</em>",
                "</p></div>"
            )
            
            return(outlier_html)
        },

        .generate_normality_tests = function(data, dep_var, group_var) {
            # Perform normality tests for each group
            
            normality_html <- paste0(
                "<div style='background-color: #d1ecf1; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #0c5460; margin-top: 0;'>📈 Normality Tests (Shapiro-Wilk)</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr style='background-color: #e0e0e0;'>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Group</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>W Statistic</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>P-value</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Interpretation</th>",
                "</tr>"
            )
            
            for (group in levels(data[[group_var]])) {
                group_data <- data[data[[group_var]] == group, dep_var]
                
                if (length(group_data) >= 3 && length(group_data) <= 5000) {
                    sw_test <- shapiro.test(group_data)
                    w_stat <- round(sw_test$statistic, 4)
                    p_val <- round(sw_test$p.value, 4)
                    interpretation <- if (p_val > 0.05) "Normal" else "Non-normal"
                } else {
                    w_stat <- "N/A"
                    p_val <- "N/A"
                    interpretation <- "Sample size not suitable"
                }
                
                normality_html <- paste0(normality_html,
                    "<tr>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", group, "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", w_stat, "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", p_val, "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", interpretation, "</td>",
                    "</tr>"
                )
            }
            
            normality_html <- paste0(normality_html,
                "</table>",
                "<p style='font-size: 12px; color: #0c5460; margin-top: 15px;'>",
                "<em>Shapiro-Wilk test: p > 0.05 suggests normal distribution. Valid for sample sizes 3-5000.</em>",
                "</p></div>"
            )
            
            return(normality_html)
        },

        .generate_group_comparisons = function(data, dep_var, group_var) {
            # Perform statistical comparisons between groups
            
            n_groups <- length(levels(data[[group_var]]))
            comparison_method <- self$options$comparison_method
            
            # Automatic method selection
            if (comparison_method == "auto") {
                if (n_groups == 2) {
                    # Check normality for automatic test selection
                    group1_data <- data[data[[group_var]] == levels(data[[group_var]])[1], dep_var]
                    group2_data <- data[data[[group_var]] == levels(data[[group_var]])[2], dep_var]
                    
                    if (length(group1_data) >= 3 && length(group2_data) >= 3) {
                        sw1 <- shapiro.test(group1_data)$p.value
                        sw2 <- shapiro.test(group2_data)$p.value
                        use_parametric <- (sw1 > 0.05 && sw2 > 0.05)
                    } else {
                        use_parametric <- FALSE
                    }
                    
                    test_method <- if (use_parametric) "t-test" else "Wilcoxon"
                } else {
                    # Multiple groups - use ANOVA or Kruskal-Wallis
                    # Simple heuristic: check overall normality
                    overall_normal <- TRUE
                    for (group in levels(data[[group_var]])) {
                        group_data <- data[data[[group_var]] == group, dep_var]
                        if (length(group_data) >= 3 && length(group_data) <= 5000) {
                            if (shapiro.test(group_data)$p.value <= 0.05) {
                                overall_normal <- FALSE
                                break
                            }
                        }
                    }
                    test_method <- if (overall_normal) "ANOVA" else "Kruskal-Wallis"
                }
            } else {
                test_method <- switch(comparison_method,
                    "ttest" = "t-test",
                    "wilcoxon" = "Wilcoxon",
                    "anova" = "ANOVA",
                    "kruskal" = "Kruskal-Wallis"
                )
            }
            
            # Perform the selected test
            if (test_method == "t-test" && n_groups == 2) {
                group_levels <- levels(data[[group_var]])
                group1_data <- data[data[[group_var]] == group_levels[1], dep_var]
                group2_data <- data[data[[group_var]] == group_levels[2], dep_var]
                
                test_result <- t.test(group1_data, group2_data)
                test_stat <- round(test_result$statistic, 4)
                p_value <- round(test_result$p.value, 4)
                test_details <- paste0("t = ", test_stat, ", df = ", round(test_result$parameter, 1))
                
            } else if (test_method == "Wilcoxon" && n_groups == 2) {
                group_levels <- levels(data[[group_var]])
                group1_data <- data[data[[group_var]] == group_levels[1], dep_var]
                group2_data <- data[data[[group_var]] == group_levels[2], dep_var]
                
                test_result <- wilcox.test(group1_data, group2_data)
                test_stat <- round(test_result$statistic, 4)
                p_value <- round(test_result$p.value, 4)
                test_details <- paste0("W = ", test_stat)
                
            } else if (test_method == "ANOVA") {
                formula_str <- paste(dep_var, "~", group_var)
                aov_result <- aov(as.formula(formula_str), data = data)
                summary_aov <- summary(aov_result)
                
                f_stat <- round(summary_aov[[1]]$`F value`[1], 4)
                p_value <- round(summary_aov[[1]]$`Pr(>F)`[1], 4)
                df1 <- summary_aov[[1]]$Df[1]
                df2 <- summary_aov[[1]]$Df[2]
                test_details <- paste0("F(", df1, ",", df2, ") = ", f_stat)
                
            } else if (test_method == "Kruskal-Wallis") {
                formula_str <- paste(dep_var, "~", group_var)
                kw_result <- kruskal.test(as.formula(formula_str), data = data)
                
                test_stat <- round(kw_result$statistic, 4)
                p_value <- round(kw_result$p.value, 4)
                test_details <- paste0("χ² = ", test_stat, ", df = ", kw_result$parameter)
            }
            
            # Create results HTML
            significance <- if (p_value < 0.001) "Highly significant (***)" else 
                          if (p_value < 0.01) "Very significant (**)" else
                          if (p_value < 0.05) "Significant (*)" else "Not significant"
            
            comparison_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📊 Group Comparison Test</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Method:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_method, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Statistic:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_details, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>P-value:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", p_value, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Result:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", significance, "</td></tr>",
                "</table>",
                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>* p < 0.05, ** p < 0.01, *** p < 0.001. ",
                if (comparison_method == "auto") paste0("Automatically selected ", test_method, " based on data characteristics.") else "",
                "</em></p></div>"
            )
            
            return(comparison_html)
        },

        .generate_interpretation_guide = function(data, dep_var, group_var) {
            # Generate interpretation guide
            
            n_groups <- length(levels(data[[group_var]]))
            n_total <- nrow(data)
            
            interpretation_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📋 Raincloud Plot Interpretation Guide</h3>",
                
                "<h4 style='color: #2e7d32;'>Plot Summary:</h4>",
                "<ul>",
                "<li><strong>Variable:</strong> ", dep_var, " (distribution analysis)</li>",
                "<li><strong>Groups:</strong> ", n_groups, " groups defined by ", group_var, "</li>",
                "<li><strong>Observations:</strong> ", n_total, " data points</li>",
                "<li><strong>Visualization:</strong> ", 
                paste(c(
                    if (self$options$show_violin) "Half-violin (density)",
                    if (self$options$show_boxplot) "Box plot (quartiles)",
                    if (self$options$show_dots) "Data points (individual values)"
                ), collapse = " + "), "</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>How to Read Raincloud Plots:</h4>",
                "<ul>",
                "<li><strong>Half-Violin:</strong> Shows probability density - wider areas indicate more data points</li>",
                "<li><strong>Box Plot:</strong> Shows median (line), quartiles (box), and outliers (points)</li>",
                "<li><strong>Data Points:</strong> Individual observations reveal fine-grained patterns</li>",
                "<li><strong>Shape Patterns:</strong> Symmetric, skewed, bimodal, or multimodal distributions</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Distribution Patterns to Look For:</h4>",
                "<ul>",
                "<li><strong>Symmetry:</strong> Bell-shaped density indicates normal distribution</li>",
                "<li><strong>Skewness:</strong> Tail extending to one side (left-skewed or right-skewed)</li>",
                "<li><strong>Multimodality:</strong> Multiple peaks suggest subgroups within data</li>",
                "<li><strong>Outliers:</strong> Points far from the main distribution</li>",
                "<li><strong>Spread:</strong> Width of distribution indicates variability</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Clinical/Research Applications:</h4>",
                "<ul>",
                "<li><strong>Biomarker Analysis:</strong> Compare distributions across patient groups</li>",
                "<li><strong>Treatment Effects:</strong> Visualize before/after treatment distributions</li>",
                "<li><strong>Quality Control:</strong> Identify unusual patterns in laboratory values</li>",
                "<li><strong>Subgroup Discovery:</strong> Detect hidden subpopulations in data</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>💡 Raincloud plots reveal distribution nuances that traditional box plots miss, making them ideal for exploratory data analysis and publication-quality visualizations.</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        }

    )
)

# Store analysis data for plotting
.analysis_data <- NULL