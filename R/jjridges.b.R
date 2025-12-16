#' @title Advanced Ridge Plot
#' @description Creates advanced ridgeline plots with robust statistical analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import ggridges
#' @import dplyr
#' @import tidyr
#' @importFrom effectsize cohens_d hedges_g eta_squared omega_squared
#' @importFrom rstatix wilcox_test t_test
#'

jjridgesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjridgesClass",
    inherit = jjridgesBase,
    
    # Constants for validation and defaults
    private = list(
        # Clinical constants
        .MIN_SAMPLE_SIZE = 10,
        .MIN_GROUP_SIZE = 3,
        .MAX_OUTLIER_PROP = 0.05,
        .VIRIDIS_FALLBACK = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
        .CLINICAL_CB_SAFE_COLORS = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),

        # Option overrides for presets
        overrides = list(),

        # === Helper: Escape Variable Names ===
        .escapeVarName = function(var) {
            if (is.null(var) || var == "") return(var)
            # Wrap in backticks if contains spaces or special chars
            if (grepl("[^A-Za-z0-9_.]", var)) {
                return(paste0("`", var, "`"))
            }
            return(var)
        },
        
        .option = function(option) {
            if (option %in% names(private$overrides)) {
                return(private$overrides[[option]])
            }
            opt_obj <- self$options$option(option)
            if (!is.null(opt_obj))
                return(opt_obj$value)
            return(NULL)
        },

        .init = function() {
            # Apply presets first to ensure UI reflects overrides
            private$.applyClinicalPreset()


            # Check if required variables are provided
            if (is.null(self$options$x_var) || is.null(self$options$y_var)) {
                self$results$instructions$setContent(paste0(
                    "<h3>", .("Ridge Plot Instructions"), "</h3>",
                    "<p>", .("Welcome to the Advanced Ridge Plot analysis!"), "</p>",
                    
                    "<div style='background:#f0f8ff; border-left:4px solid #2196F3; padding:15px; margin:15px 0;'>",
                        "<h4 style='color:#2196F3; margin-top:0;'>📊 ", .("Clinical Guidance"), "</h4>",
                        "<p><strong>", .("When to Use Ridge Plots:"), "</strong></p>",
                        "<ul style='margin-bottom:10px;'>",
                            "<li>", .("Compare biomarker distributions between patient groups"), "</li>",
                            "<li>", .("Visualize treatment response patterns across disease stages"), "</li>",
                            "<li>", .("Show how continuous measures vary by pathological categories"), "</li>",
                            "<li>", .("Display age or tumor size distributions by clinical characteristics"), "</li>",
                        "</ul>",
                        "<p><strong>", .("Key Considerations:"), "</strong></p>",
                        "<ul style='margin-bottom:10px;'>",
                            "<li>", .("Minimum 3-5 observations per group for reliable curves"), "</li>",
                            "<li>", .("Wide ridges = high variability; narrow ridges = consistent values"), "</li>",
                            "<li>", .("Multiple peaks may indicate distinct clinical subgroups"), "</li>",
                            "<li>", .("Compare ridge positions (left/right shift) for group differences"), "</li>",
                        "</ul>",
                    "</div>",
                    
                    "<p><strong>", .("Required:"), "</strong></p>",
                    "<ul>",
                        "<li><strong>", .("X Variable:"), "</strong> ", .("Continuous variable for distributions"), "</li>",
                        "<li><strong>", .("Y Variable:"), "</strong> ", .("Grouping variable for separate ridges"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Optional:"), "</strong></p>",
                    "<ul>",
                        "<li><strong>", .("Fill Variable:"), "</strong> ", .("Color-code segments within ridges"), "</li>",
                        "<li><strong>", .("Facet Variable:"), "</strong> ", .("Create separate panels"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Features:"), "</strong></p>",
                    "<ul>",
                        "<li>", .("Multiple plot types (density, histogram, gradient, violin)"), "</li>",
                        "<li>", .("Statistical overlays with p-values and effect sizes"), "</li>",
                        "<li>", .("Boxplots, quantiles, and mean/median lines"), "</li>",
                        "<li>", .("Publication-ready themes and customization"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Export Options:"), "</strong></p>",
                    "<ul>",
                        "<li>", .("Adjust plot width and height in Export Options"), "</li>",
                        "<li>", .("Set DPI for high-resolution output (300 DPI recommended for publications)"), "</li>",
                        "<li>", .("Use right-click → Save Image As for quick export"), "</li>",
                    "</ul>"
                ))
                self$results$instructions$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = FALSE)
                self$results$statistics$setVisible(visible = FALSE)
                self$results$tests$setVisible(visible = FALSE)
                self$results$interpretation$setVisible(visible = FALSE)
                return()
            }
            
            # Set visibility based on options
            self$results$instructions$setVisible(visible = FALSE)
            self$results$plot$setVisible(visible = TRUE)
            self$results$statistics$setVisible(visible = TRUE)
            self$results$tests$setVisible(visible = private$.option("show_stats"))
            self$results$interpretation$setVisible(visible = TRUE)
            
            # Set plot dimensions - increase size if legends are likely to be shown
            plot_width <- self$options$width
            plot_height <- self$options$height
            
            # Increase width if fill legend is shown and legend position is right/left
            if (!is.null(self$options$fill_var) && 
                (self$options$show_fill_legend %||% TRUE) &&
                self$options$legend_position %in% c("right", "left")) {
                plot_width <- max(plot_width, 900)  # Minimum 900px when right/left legend
            }
            
            # Increase height if fill legend is shown and legend position is top/bottom
            if (!is.null(self$options$fill_var) && 
                (self$options$show_fill_legend %||% TRUE) &&
                self$options$legend_position %in% c("top", "bottom")) {
                plot_height <- max(plot_height, 650)  # Minimum 650px when top/bottom legend
            }
            
            # Increase height if faceting is used
            if (!is.null(self$options$facet_var)) {
                plot_height <- max(plot_height, 700)  # Minimum 700px when faceting
            }
            
            self$results$plot$setSize(plot_width, plot_height)
        },
        
        .validateInputs = function() {
            warnings <- c()
            
            # Check variable selection
            if (is.null(self$options$x_var)) {
                stop(.("Please select a continuous variable for X (Distribution)"))
            }
            if (is.null(self$options$y_var)) {
                stop(.("Please select a grouping variable for Y (Groups)"))
            }

            # Check data availability
            if (is.null(self$data) || nrow(self$data) == 0) {
                stop(.("No data available for analysis"))
            }

            # Type checks
            x_col <- self$data[[self$options$x_var]]
            y_col <- self$data[[self$options$y_var]]
            if (!is.numeric(x_col)) {
                stop(paste0(.("X variable must be numeric for ridge plotting. Selected"), ": ", self$options$x_var))
            }
            if (!is.factor(y_col) && !is.character(y_col)) {
                stop(paste0(.("Y variable should be categorical (factor/character). Selected"), ": ", self$options$y_var))
            }
            
            # Check minimum sample size
            if (nrow(self$data) < private$.MIN_SAMPLE_SIZE) {
                warnings <- c(warnings, paste0(.("Sample size (n="), nrow(self$data), 
                                               .(") is below recommended minimum of "), private$.MIN_SAMPLE_SIZE, 
                                               .(" for reliable ridge plot analysis")))
            }

            # Check basic variability of X
            n_unique_x <- length(unique(x_col[!is.na(x_col)]))
            if (n_unique_x < 3) {
                warnings <- c(warnings, .("X variable has very few unique values; density shapes may be misleading."))
            }
            
            return(warnings)
        },
        
        .validateData = function(plot_data) {
            warnings <- c()
            
            # Check for minimum group count
            n_groups <- length(unique(plot_data$y))
            if (n_groups < 2) {
                stop(.("At least 2 groups required for ridge plot comparison"))
            }
            
            # Check group sizes with clinical context
            group_counts <- table(plot_data$y)
            small_groups <- group_counts[group_counts < private$.MIN_GROUP_SIZE]
            if (length(small_groups) > 0) {
                warnings <- c(warnings, paste0(.("Groups with fewer than "), private$.MIN_GROUP_SIZE, 
                                               .(" observations may show unreliable density estimates: "), 
                                               paste(names(small_groups), collapse=", ")))
            }
            
            # Check for extreme outliers that could affect interpretation
            if (length(plot_data$x) > 0) {
                z_scores <- abs(scale(plot_data$x))
                outlier_prop <- sum(z_scores > 3, na.rm = TRUE) / length(z_scores)
                if (outlier_prop > private$.MAX_OUTLIER_PROP) {
                    warnings <- c(warnings, paste0(.("High proportion of extreme outliers detected ("), 
                                                   round(outlier_prop * 100, 1), 
                                                   .("%). Consider reviewing data quality or using robust statistical options.")))
                }
            }
            
            # Check for highly skewed distributions
            if (length(plot_data$x) > 10) {
                if (requireNamespace("moments", quietly = TRUE)) {
                    skewness <- moments::skewness(plot_data$x, na.rm = TRUE)
                } else {
                    # Fallback calculation for skewness when moments package is not available
                    x <- plot_data$x[!is.na(plot_data$x)]
                    if (length(x) > 0) {
                        m <- mean(x)
                        s <- sd(x)
                        if (s > 0) {
                            skewness <- sum(((x - m) / s)^3) / length(x)
                        } else {
                            skewness <- 0
                        }
                    } else {
                        skewness <- 0
                    }
                }
                if (abs(skewness) > 2) {
                    warnings <- c(warnings, .("Data shows high skewness. Consider log transformation if appropriate for your clinical context."))
                }
            }
            
            return(warnings)
        },
        
        .validateQuantiles = function(quantile_string) {
            tryCatch({
                # Split and trim whitespace
                vals <- as.numeric(trimws(strsplit(quantile_string, ",")[[1]]))
                
                # Check for parsing errors
                if (any(is.na(vals))) {
                    stop(.("Non-numeric values detected in quantiles"))
                }
                
                # Check valid range
                if (any(vals < 0 | vals > 1)) {
                    stop(.("Quantile values must be between 0 and 1"))
                }
                
                # Check not empty
                if (length(vals) == 0) {
                    stop(.("No quantile values provided"))
                }
                
                # Sort quantiles
                vals <- sort(vals)
                return(vals)
                
            }, error = function(e) {
                # Log warning and use defaults
                warning(.("Invalid quantiles specified: "), e$message, .(", using defaults"))
                c(0.25, 0.5, 0.75)
            })
        },
        
        .generateClinicalSummary = function(data, has_stats = FALSE) {
            n_groups <- length(unique(data$y))
            n_total <- nrow(data)
            
            # Basic summary
            summary <- paste0(
                "<div class='clinical-summary' style='background:#f8f9fa; border-left:4px solid #007bff; padding:15px; margin:10px 0;'>",
                "<h4 style='color:#007bff; margin-top:0;'>", .("Clinical Summary"), "</h4>",
                "<p><strong>", .("Analysis:"), "</strong> ", .("Ridge plot comparing the distribution of"), " <strong>", 
                self$options$x_var, "</strong> ", .("across"), " <strong>", n_groups, " ", .("groups"), "</strong> ", .("defined by"), " <strong>", 
                self$options$y_var, "</strong>.</p>",
                "<p><strong>", .("Sample:"), "</strong> n = ", n_total, " ", .("total observations across all groups"), ".</p>"
            )
            
            # Add interpretation guidance
            summary <- paste0(summary,
                "<p><strong>", .("Interpretation Guide:"), "</strong></p>",
                "<ul style='margin-bottom:10px;'>",
                "<li><strong>", .("Ridge Shape:"), "</strong> ", .("Wider ridges = more variability; Narrow ridges = consistent values"), "</li>",
                "<li><strong>", .("Ridge Position:"), "</strong> ", .("Left/right shift indicates lower/higher average values"), "</li>",
                "<li><strong>", .("Multiple Peaks:"), "</strong> ", .("May indicate subgroups or distinct clinical phenotypes"), "</li>",
                "<li><strong>", .("Overlap:"), "</strong> ", .("Similar distributions between groups; Separation = distinct patterns"), "</li>",
                "</ul></div>"
            )
            
            return(summary)
        },

        .generateReportSummary = function(data, has_stats = FALSE) {
            # Generate copy-ready plain text summary for pathology reports
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            n_groups <- length(unique(data$y))
            n_total <- nrow(data)

            # Calculate basic descriptive statistics per group
            stats_by_group <- data %>%
                group_by(y) %>%
                summarise(
                    n = n(),
                    mean = mean(x, na.rm = TRUE),
                    sd = sd(x, na.rm = TRUE),
                    median = median(x, na.rm = TRUE),
                    q1 = quantile(x, 0.25, na.rm = TRUE),
                    q3 = quantile(x, 0.75, na.rm = TRUE),
                    .groups = 'drop'
                )

            # Build plain text summary
            text_summary <- paste0(
                "<div style='background:#fff8e1; border:1px solid #ffc107; padding:15px; margin:10px 0; font-family:monospace;'>",
                "<h4 style='color:#f57c00; margin-top:0;'>📋 Copy-Ready Report Summary</h4>",
                "<p style='font-size:11px; color:#666; margin-bottom:10px;'>",
                "Select and copy the text below for inclusion in reports or presentations.",
                "</p>",
                "<div style='background:white; padding:12px; border:1px solid #ddd;'>"
            )

            # Add header
            text_summary <- paste0(text_summary,
                "<p><strong>RIDGE PLOT ANALYSIS</strong></p>",
                "<p>Variable: ", x_var, " by ", y_var, "</p>",
                "<p>Total N = ", n_total, " across ", n_groups, " groups</p>",
                "<p></p>"
            )

            # Add group-specific summaries
            text_summary <- paste0(text_summary, "<p><strong>DESCRIPTIVE STATISTICS BY GROUP:</strong></p>")
            for (i in 1:nrow(stats_by_group)) {
                row <- stats_by_group[i, ]
                text_summary <- paste0(text_summary,
                    "<p>", row$y, " (n=", row$n, "): ",
                    "Mean=", sprintf("%.2f", row$mean), " (SD=", sprintf("%.2f", row$sd), "), ",
                    "Median=", sprintf("%.2f", row$median), " (IQR: ",
                    sprintf("%.2f", row$q1), "-", sprintf("%.2f", row$q3), ")",
                    "</p>"
                )
            }

            # Add statistical test results if available
            if (has_stats && !is.null(self$results$tests) && self$results$tests$rowCount() > 0) {
                text_summary <- paste0(text_summary, "<p></p><p><strong>STATISTICAL COMPARISONS:</strong></p>")
                test_type <- self$options$test_type
                test_name <- switch(test_type,
                    parametric = "parametric test",
                    nonparametric = "non-parametric test",
                    robust = "robust test",
                    bayes = "Bayesian test",
                    "statistical test"
                )

                # Get first row as example (multiple comparisons handled in full table)
                if (self$results$tests$rowCount() > 0) {
                    text_summary <- paste0(text_summary,
                        "<p>Method: ", tools::toTitleCase(gsub("_", " ", test_type)), "</p>",
                        "<p>See full statistical table for p-values, effect sizes, and confidence intervals.</p>"
                    )
                }
            }

            text_summary <- paste0(text_summary,
                "</div>",
                "<p style='font-size:10px; color:#999; margin-top:10px;'>",
                "Generated by ClinicoPath Advanced Ridge Plot Analysis",
                "</p>",
                "</div>"
            )

            return(text_summary)
        },

        .generateAboutPanel = function() {
            # Comprehensive educational guide about ridge plots
            html <- paste0(
                "<div style='background:#e8f5e9; border-left:4px solid #4caf50; padding:15px; margin:10px 0;'>",
                "<h3 style='color:#2e7d32; margin-top:0;'>📚 About Ridge Plots</h3>",

                "<h4 style='color:#2e7d32;'>What are Ridge Plots?</h4>",
                "<p>Ridge plots (also called ridgeline plots or joyplots) display the distribution of a ",
                "continuous variable across multiple groups as partially overlapping density curves. ",
                "Each 'ridge' shows how values are distributed within one group, making it easy to ",
                "compare distribution shapes, central tendencies, and variability across groups.</p>",

                "<h4 style='color:#2e7d32;'>Clinical Applications</h4>",
                "<ul>",
                "<li><strong>Biomarker Analysis:</strong> Compare Ki-67 index, CD3+ counts, or receptor expression across tumor grades</li>",
                "<li><strong>Treatment Response:</strong> Visualize tumor size reduction across different treatment arms</li>",
                "<li><strong>Disease Staging:</strong> Show age or biomarker distributions across TNM stages</li>",
                "<li><strong>Temporal Patterns:</strong> Display lab values over time periods or disease progression stages</li>",
                "<li><strong>Pathology Scoring:</strong> Compare continuous measurements (mitotic counts, vessel density) by diagnosis</li>",
                "</ul>",

                "<h4 style='color:#2e7d32;'>How to Interpret Ridge Plots</h4>",
                "<ul>",
                "<li><strong>Ridge Width:</strong> Wide = high variability; Narrow = consistent values within group</li>",
                "<li><strong>Ridge Position:</strong> Left-shifted = lower values; Right-shifted = higher values</li>",
                "<li><strong>Ridge Height:</strong> Indicates probability density (taller = more observations in that range)</li>",
                "<li><strong>Multiple Peaks:</strong> May indicate distinct subpopulations or bimodal distributions</li>",
                "<li><strong>Overlap:</strong> Substantial overlap = similar distributions; Separation = distinct patterns</li>",
                "<li><strong>Skewness:</strong> Asymmetric ridges indicate skewed data (common in pathology measurements)</li>",
                "</ul>",

                "<h4 style='color:#2e7d32;'>When to Use Ridge Plots vs. Alternatives</h4>",
                "<p><strong>Use Ridge Plots When:</strong></p>",
                "<ul>",
                "<li>You have 3+ groups to compare (works well up to ~10 groups)</li>",
                "<li>Understanding distribution shape is important (not just mean differences)</li>",
                "<li>You want to detect bimodality or subgroups within categories</li>",
                "<li>Data is continuous and you have sufficient observations per group (n≥20 recommended)</li>",
                "</ul>",

                "<p><strong>Consider Alternatives When:</strong></p>",
                "<ul>",
                "<li>Only 2 groups → Use violin plots or density plots side-by-side</li>",
                "<li>Focus on summary statistics only → Use box plots or error bars</li>",
                "<li>Categorical data → Use bar charts or mosaic plots</li>",
                "<li>Very few observations per group (n<10) → Individual data points may be better</li>",
                "</ul>",

                "<h4 style='color:#2e7d32;'>Plot Type Options</h4>",
                "<ul>",
                "<li><strong>Density Ridges:</strong> Smooth curves showing probability density (default, best for most cases)</li>",
                "<li><strong>Density with Gradient:</strong> Color intensity shows density within each ridge</li>",
                "<li><strong>Histogram Ridges:</strong> Discrete bins instead of smooth curves (good for small samples)</li>",
                "<li><strong>Violin Ridges:</strong> Symmetric mirrored densities (emphasizes shape comparison)</li>",
                "</ul>",

                "<h4 style='color:#2e7d32;'>Statistical Considerations</h4>",
                "<ul>",
                "<li><strong>Independence:</strong> Statistical tests assume independent observations (one per subject)</li>",
                "<li><strong>Sample Size:</strong> Minimum 3-5 observations per group for meaningful curves; 20+ recommended</li>",
                "<li><strong>Effect Size:</strong> Use nonparametric effect sizes (Cliff's Delta, Hodges-Lehmann) for skewed pathology data</li>",
                "<li><strong>Multiple Testing:</strong> Apply p-value correction (Bonferroni, FDR) when comparing many groups</li>",
                "</ul>",

                "<h4 style='color:#2e7d32;'>Publication Tips</h4>",
                "<ul>",
                "<li>Set DPI to 300 for journal submissions (Export Options)</li>",
                "<li>Use colorblind-safe palettes (Clinical or Viridis)</li>",
                "<li>Add boxplots inside ridges to show quartiles and outliers</li>",
                "<li>Label axes clearly with units (e.g., 'Ki-67 Index (%)', 'Tumor Size (mm)')</li>",
                "<li>Report statistical test choice and justification in figure legend</li>",
                "</ul>",

                "</div>"
            )

            return(html)
        },

        .generateAssumptionsPanel = function() {
            # Statistical assumptions and caveats for selected test type
            test_type <- self$options$test_type
            has_stats <- self$options$show_stats

            if (!has_stats) {
                html <- paste0(
                    "<div style='background:#fff3cd; border-left:4px solid #ffc107; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#856404; margin-top:0;'>ℹ️ Statistical Testing Disabled</h4>",
                    "<p>Enable 'Show Statistics' to view statistical assumptions and methodological notes.</p>",
                    "</div>"
                )
                return(html)
            }

            # Build assumptions based on test type
            html <- paste0(
                "<div style='background:#fff3cd; border-left:4px solid #ffc107; padding:15px; margin:10px 0;'>",
                "<h3 style='color:#856404; margin-top:0;'>⚠️ Statistical Assumptions & Caveats</h3>",
                "<p><strong>Selected Test Type:</strong> ", tools::toTitleCase(gsub("_", " ", test_type)), "</p>"
            )

            if (test_type == "parametric") {
                html <- paste0(html,
                    "<h4 style='color:#856404;'>Parametric Test Assumptions</h4>",
                    "<ul>",
                    "<li><strong>Normality:</strong> Data within each group should be approximately normally distributed. ",
                    "Ridge plots showing symmetric, bell-shaped curves suggest normality. Skewed or multimodal ridges violate this assumption.</li>",
                    "<li><strong>Independence:</strong> Each observation must be independent (one measurement per subject). ",
                    "Repeated measures, matched pairs, or clustered data violate independence.</li>",
                    "<li><strong>Homogeneity of Variance:</strong> Groups should have similar spread (variance). ",
                    "Ridge widths should be comparable across groups.</li>",
                    "</ul>",
                    "<p><strong>Tests Used:</strong> Independent t-test (2 groups) or one-way ANOVA (3+ groups)</p>",
                    "<p><strong>When Violated:</strong> Consider nonparametric tests if distributions are skewed, or robust tests if variances differ.</p>"
                )
            } else if (test_type == "nonparametric") {
                html <- paste0(html,
                    "<h4 style='color:#856404;'>Non-parametric Test Assumptions</h4>",
                    "<ul>",
                    "<li><strong>Independence:</strong> Each observation must be independent. This is still required for non-parametric tests.</li>",
                    "<li><strong>Ordinal or Continuous Data:</strong> Variables should be at least ordinal scale.</li>",
                    "<li><strong>Similar Distributions:</strong> For Mann-Whitney/Kruskal-Wallis, groups should have similar distribution shapes ",
                    "(ridges with similar shapes but different positions). If shapes differ, tests compare distributions rather than medians.</li>",
                    "</ul>",
                    "<p><strong>Tests Used:</strong> Mann-Whitney U test (2 groups) or Kruskal-Wallis test (3+ groups)</p>",
                    "<p><strong>Advantages:</strong> Robust to outliers and skewed distributions common in pathology data (e.g., mitotic counts, lymph node involvement).</p>",
                    "<p><strong>Effect Sizes:</strong> Cliff's Delta shows probability one group has higher values; Hodges-Lehmann estimates typical difference.</p>"
                )
            } else if (test_type == "robust") {
                html <- paste0(html,
                    "<h4 style='color:#856404;'>Robust Test Assumptions</h4>",
                    "<ul>",
                    "<li><strong>Independence:</strong> Each observation must be independent.</li>",
                    "<li><strong>Outlier Resistance:</strong> Robust tests use trimmed means (default 20% trimming), reducing outlier influence.</li>",
                    "<li><strong>Heterogeneity Tolerance:</strong> Does not assume equal variances across groups.</li>",
                    "</ul>",
                    "<p><strong>Tests Used:</strong> Yuen's trimmed means test (Welch-type robust alternative to t-test/ANOVA)</p>",
                    "<p><strong>Best For:</strong> Data with unequal variances, mild outliers, or slight departures from normality.</p>",
                    "<p><strong>Clinical Scenarios:</strong> Laboratory values with occasional extreme results, tumor measurements with outliers.</p>"
                )
            } else if (test_type == "bayes") {
                html <- paste0(html,
                    "<h4 style='color:#856404;'>Bayesian Test Notes</h4>",
                    "<ul>",
                    "<li><strong>Bayes Factor Interpretation:</strong> BF10 > 3 = moderate evidence for difference; BF10 > 10 = strong evidence; ",
                    "BF10 < 1/3 = evidence for no difference.</li>",
                    "<li><strong>Prior Assumptions:</strong> Uses default Cauchy prior (scale=0.707). Results sensitive to prior choice.</li>",
                    "<li><strong>Computational Requirements:</strong> Requires BayesFactor package. May be slow for large datasets.</li>",
                    "</ul>",
                    "<p><strong>Advantages:</strong> Provides evidence for null hypothesis (no difference), not just failure to reject null.</p>",
                    "<p><strong>Limitations:</strong> Requires understanding of Bayesian inference; less familiar to many clinical reviewers.</p>"
                )
            }

            # Universal caveats
            html <- paste0(html,
                "<h4 style='color:#856404;'>Universal Considerations</h4>",
                "<ul>",
                "<li><strong>Sample Size:</strong> Small samples (n<20 per group) have low statistical power. Non-significant results may reflect insufficient data.</li>",
                "<li><strong>Multiple Testing:</strong> Comparing many groups increases false positive risk. ",
                "Apply p-value correction (Bonferroni, Holm, or FDR) when making multiple comparisons.</li>",
                "<li><strong>Clinical vs. Statistical Significance:</strong> Statistically significant differences may not be clinically meaningful. ",
                "Always report effect sizes and consider biological relevance.</li>",
                "<li><strong>Data Quality:</strong> Measurement error, missing data patterns, and selection bias affect all statistical tests.</li>",
                "<li><strong>Repeated Measures:</strong> Multiple observations per subject (biopsies, timepoints) violate independence. ",
                "Aggregate to subject-level summaries or use specialized models.</li>",
                "</ul>",

                "<h4 style='color:#856404;'>Recommendations for Pathology Data</h4>",
                "<ul>",
                "<li>Biomarker percentages (Ki-67, CD3+): Often skewed → Use nonparametric tests</li>",
                "<li>Cell counts (mitotic index, lymphocytes): Count data with overdispersion → Consider nonparametric or robust methods</li>",
                "<li>Tumor size measurements: May have outliers → Robust tests or nonparametric</li>",
                "<li>Age distributions: Often normal → Parametric tests appropriate</li>",
                "<li>Survival times: Always right-skewed → Use specialized survival analysis methods (not ridge plots)</li>",
                "</ul>",

                "</div>"
            )

            return(html)
        },

        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            private$overrides <- list()
            if (preset == "custom") {
                return()
            }

            apply_override <- function(name, value, label = NULL) {
                opt <- self$options$option(name)
                if (!is.null(opt) && !is.null(value)) {
                    if (!identical(opt$value, value)) {
                        private$overrides[[name]] <<- value
                        attr(private$overrides[[name]], "label") <<- if (!is.null(label)) label else paste0(name, " set to ", value)
                    }
                    opt$value <- value
                }
            }

            # Clinical presets optimized for medical research
            if (preset == "biomarker_distribution") {
                # For comparing biomarker levels across patient groups
                apply_override("plot_type", "density_ridges", "Plot type set to density_ridges")
                apply_override("add_boxplot", TRUE, "Boxplots enabled")
                apply_override("add_quantiles", TRUE, "Quantile lines enabled")
                apply_override("quantiles", "0.25, 0.5, 0.75")
                apply_override("theme_style", "theme_pubr", "Publication theme")
                apply_override("color_palette", "clinical_colorblind", "Clinical palette")
                apply_override("show_stats", TRUE, "Statistics enabled")
                apply_override("test_type", "nonparametric", "Nonparametric tests")
                apply_override("effsize_type", "cliff_delta", "Cliff's delta effect size")
                apply_override("p_adjust_method", "fdr", "FDR correction")

            } else if (preset == "treatment_response") {
                # For comparing treatment outcomes across groups
                apply_override("plot_type", "violin_ridges", "Plot type set to violin_ridges")
                apply_override("show_stats", TRUE, "Statistics enabled")
                apply_override("test_type", "nonparametric", "Nonparametric tests")
                apply_override("effsize_type", "cliff_delta", "Cliff's delta effect size")
                apply_override("theme_style", "theme_pubr", "Publication theme")
                apply_override("color_palette", "clinical_colorblind", "Clinical palette")
                apply_override("add_boxplot", TRUE, "Boxplots enabled")
                apply_override("p_adjust_method", "bonferroni", "Bonferroni correction")

            } else if (preset == "age_by_stage") {
                # For age distribution across disease stages
                apply_override("plot_type", "density_ridges", "Plot type set to density_ridges")
                apply_override("add_mean", TRUE, "Mean lines enabled")
                apply_override("add_median", TRUE, "Median lines enabled")
                apply_override("theme_style", "theme_pubr", "Publication theme")
                apply_override("color_palette", "viridis", "Viridis palette")
                apply_override("show_stats", TRUE, "Statistics enabled")
                apply_override("test_type", "parametric", "Parametric tests")
                apply_override("effsize_type", "d", "Cohen's d effect size")

            } else if (preset == "tumor_size_comparison") {
                # For tumor size/dimension comparisons
                apply_override("plot_type", "density_ridges", "Plot type set to density_ridges")
                apply_override("add_boxplot", TRUE, "Boxplots enabled")
                apply_override("add_quantiles", TRUE, "Quantile lines enabled")
                apply_override("quantiles", "0.25, 0.5, 0.75")
                apply_override("theme_style", "theme_pubr", "Publication theme")
                apply_override("color_palette", "clinical_colorblind", "Clinical palette")
                apply_override("show_stats", TRUE, "Statistics enabled")
                apply_override("test_type", "nonparametric", "Nonparametric tests")
                apply_override("effsize_type", "hodges_lehmann", "Hodges-Lehmann shift")
                apply_override("p_adjust_method", "holm", "Holm correction")

            } else if (preset == "lab_values_by_group") {
                # For laboratory values across patient groups
                apply_override("plot_type", "density_ridges", "Plot type set to density_ridges")
                apply_override("add_boxplot", TRUE, "Boxplots enabled")
                apply_override("theme_style", "theme_pubr", "Publication theme")
                apply_override("color_palette", "clinical_colorblind", "Clinical palette")
                apply_override("show_stats", TRUE, "Statistics enabled")
                apply_override("test_type", "robust", "Robust tests")
                apply_override("effsize_type", "g", "Hedges' g effect size")
                apply_override("p_adjust_method", "fdr", "FDR correction")

            } else if (preset == "survival_time_distribution") {
                # For survival time or time-to-event distributions
                apply_override("plot_type", "density_ridges", "Plot type set to density_ridges")
                apply_override("add_median", TRUE, "Median lines enabled")
                apply_override("add_quantiles", TRUE, "Quantile lines enabled")
                apply_override("quantiles", "0.25, 0.5, 0.75")
                apply_override("theme_style", "theme_pubr", "Publication theme")
                apply_override("color_palette", "Set2", "Set2 palette")
                apply_override("show_stats", TRUE, "Statistics enabled")
                apply_override("test_type", "nonparametric", "Nonparametric tests")
                apply_override("effsize_type", "hodges_lehmann", "Hodges-Lehmann shift")
                apply_override("p_adjust_method", "holm", "Holm correction")
            }

            if (length(private$overrides) > 0) {
                labels <- vapply(private$overrides, function(x) {
                    lbl <- attr(x, "label")
                    if (!is.null(lbl)) return(lbl)
                    return(as.character(x))
                }, character(1))
                preset_label <- gsub("_", " ", preset)
                preset_label <- tools::toTitleCase(preset_label)
                msg <- paste0(
                    "CLINICAL PRESET OVERRIDE: ", preset_label, " (",
                    paste(labels, collapse = "; "),
                    ")."
                )
                warning(msg)

                # Surface in UI warnings without clobbering other content
                current <- self$results$warnings$content
                html_msg <- paste0("<p style='color:#856404;'>", msg, "</p>")
                if (is.null(current) || current == "") {
                    self$results$warnings$setContent(html_msg)
                } else {
                    self$results$warnings$setContent(paste0(html_msg, current))
                }
                self$results$warnings$setVisible(TRUE)
            }
        },

        .run = function() {
            # Check requirements
            if (is.null(self$options$x_var) || is.null(self$options$y_var))
                return()

            # Apply clinical preset if selected (must be done before other processing)
            private$.applyClinicalPreset()

            # Validate inputs
            input_warnings <- tryCatch({
                private$.validateInputs()
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'inputValidationError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(e$message)
                self$results$insert(1, notice)
                return()
            })
            
            # Prepare data
            private$.checkpoint()
            plot_data <- private$.prepareData()
            
            if (nrow(plot_data) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noValidData',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("No valid data available for analysis. Check your variable selections and data.")
                self$results$insert(1, notice)
                return()
            }
            
            # Validate data and collect additional warnings
            data_warnings <- tryCatch({
                private$.validateData(plot_data)
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'dataValidationError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(e$message)
                self$results$insert(1, notice)
                return()
            })
            
            # Display individual warnings as separate Notices
            all_warnings <- c(input_warnings, data_warnings)
            if (length(all_warnings) > 0) {
                for (i in seq_along(all_warnings)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('dataQualityWarning', i),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(all_warnings[i])
                    self$results$insert(100, notice)
                }
            }
            
            # Generate statistics
            private$.checkpoint()
            private$.generateStatistics(plot_data)
            
            # Generate statistical tests if requested
            if (private$.option("show_stats")) {
                private$.checkpoint()

                # CRITICAL FIX: Check for potential repeated measures before running tests
                repeated_measures_warning <- private$.checkRepeatedMeasures(plot_data)
                if (!is.null(repeated_measures_warning)) {
                    # Create STRONG_WARNING Notice about independence assumption violation
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'independenceAssumption',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    # Combine key points into single-line message using bullet separators
                    notice_msg <- paste0(
                        "INDEPENDENCE ASSUMPTION: ", repeated_measures_warning, " • ",
                        "Statistical tests assume independent observations. Repeated measures, matched samples, or clustered data may produce invalid results. • ",
                        "Recommendations: Aggregate to one value per subject, use specialized repeated measures models, or treat statistics as exploratory only."
                    )
                    notice$setContent(notice_msg)
                    self$results$insert(1, notice)
                }

                private$.generateTests(plot_data)
            }
            
            # Create plot
            private$.checkpoint()
            plot <- private$.createPlot(plot_data)
            
            # Save plot state
            self$results$plot$setState(plot)
            
            # Generate interpretation
            private$.generateInterpretation(plot_data)
            
            # Set clinical summary
            clinical_summary <- private$.generateClinicalSummary(plot_data, private$.option("show_stats"))
            self$results$clinicalSummary$setContent(clinical_summary)
            self$results$clinicalSummary$setVisible(TRUE)

            # Generate and set report summary (always visible for clinicians)
            report_summary <- private$.generateReportSummary(plot_data, private$.option("show_stats"))
            self$results$reportSummary$setContent(report_summary)
            self$results$reportSummary$setVisible(TRUE)

            # Generate and set about panel (if requested)
            if (self$options$showAboutPanel) {
                about_panel <- private$.generateAboutPanel()
                self$results$aboutPanel$setContent(about_panel)
                self$results$aboutPanel$setVisible(TRUE)
            } else {
                self$results$aboutPanel$setVisible(FALSE)
            }

            # Generate and set assumptions panel (if requested)
            if (self$options$showAssumptions) {
                assumptions_panel <- private$.generateAssumptionsPanel()
                self$results$assumptionsPanel$setContent(assumptions_panel)
                self$results$assumptionsPanel$setVisible(TRUE)
            } else {
                self$results$assumptionsPanel$setVisible(FALSE)
            }

            # Add completion INFO notice
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )
            n_obs <- nrow(plot_data)
            n_groups <- length(unique(plot_data$y))
            notice_msg <- paste0(
                "Ridge plot analysis completed successfully • ",
                n_obs, " observations across ", n_groups, " groups • ",
                "Plot type: ", self$options$plot_type,
                if(self$options$show_stats) paste0(" • Statistical tests: ", self$options$test_type) else ""
            )
            notice$setContent(notice_msg)
            self$results$insert(999, notice)
        },
        
        .prepareData = function() {
            # Get data
            data <- self$data
            
            # Extract variables
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            
            # Create base data frame
            plot_data <- data.frame(
                x = jmvcore::toNumeric(data[[x_var]]),
                y = as.factor(data[[y_var]]),
                stringsAsFactors = FALSE
            )
            
            # Add optional variables
            if (!is.null(self$options$fill_var)) {
                plot_data$fill <- as.factor(data[[self$options$fill_var]])
            }
            
            if (!is.null(self$options$facet_var)) {
                plot_data$facet <- as.factor(data[[self$options$facet_var]])
            }
            
            # Remove missing values
            plot_data <- na.omit(plot_data)
            
            # Reverse order if requested
            if (self$options$reverse_order) {
                plot_data$y <- factor(plot_data$y, levels = rev(levels(plot_data$y)))
            }
            
            return(plot_data)
        },
        
        .createPlot = function(data) {
            # Base plot setup
            plot_type <- private$.option("plot_type")
            
            # Create base plot based on type
            if (plot_type == "ridgeline" || plot_type == "density_ridges") {
                p <- private$.createDensityPlot(data)
            } else if (plot_type == "density_ridges_gradient") {
                p <- private$.createGradientPlot(data)
            } else if (plot_type == "histogram_ridges") {
                p <- private$.createHistogramPlot(data)
            } else if (plot_type == "violin_ridges") {
                p <- private$.createViolinPlot(data)
            } else {
                p <- private$.createDensityPlot(data)  # Default
            }
            
            # Add advanced features
            if (private$.option("add_boxplot")) {
                # Add boxplot elements using ggplot2 geoms
                p <- p + ggplot2::geom_boxplot(
                    width = 0.1,
                    position = ggplot2::position_nudge(y = 0.15),
                    outlier.shape = NA,
                    alpha = 0.7,
                    color = "black"
                )
            }
            
            if (self$options$add_points) {
                # Add jittered points using ggplot2
                p <- p + ggplot2::geom_point(
                    alpha = self$options$point_alpha,
                    position = ggplot2::position_jitter(height = 0.1, width = 0),
                    size = 0.5
                )
            }
            
            if (private$.option("add_quantiles")) {
                quantiles <- private$.validateQuantiles(private$.option("quantiles"))
                p <- p + ggridges::stat_density_ridges(
                    quantile_lines = TRUE,
                    quantiles = quantiles
                )
            }
            
            if (self$options$add_mean) {
                mean_data <- data %>%
                    group_by(y) %>%
                    summarise(mean_x = mean(x, na.rm = TRUE))
                
                p <- p + geom_vline(
                    data = mean_data,
                    aes(xintercept = mean_x),
                    color = "red",
                    linetype = "dashed",
                    alpha = 0.7
                )
            }
            
            if (self$options$add_median) {
                median_data <- data %>%
                    group_by(y) %>%
                    summarise(median_x = median(x, na.rm = TRUE))
                
                p <- p + geom_vline(
                    data = median_data,
                    aes(xintercept = median_x),
                    color = "blue",
                    linetype = "dashed",
                    alpha = 0.7
                )
            }
            
            # Apply faceting if requested
            if (!is.null(self$options$facet_var)) {
                show_facet_legend <- self$options$show_facet_legend %||% TRUE
                if (show_facet_legend) {
                    p <- p + facet_wrap(~ facet, scales = "free_x")
                } else {
                    p <- p + facet_wrap(~ facet, scales = "free_x") +
                        theme(strip.text = element_blank())
                }
            }
            
            # Apply theme
            p <- private$.applyTheme(p)
            
            # Apply labels
            p <- private$.applyLabels(p)
            
            # Add sample sizes if requested
            if (self$options$add_sample_size) {
                # Calculate appropriate x position (90% of x range)
                x_max <- max(data$x, na.rm = TRUE)
                x_min <- min(data$x, na.rm = TRUE)
                x_pos <- x_min + 0.9 * (x_max - x_min)
                
                sample_sizes <- data %>%
                    group_by(y) %>%
                    summarise(n = n(), .groups = 'drop') %>%
                    mutate(
                        label = paste0("n=", n),
                        x_pos = x_pos
                    )
                
                p <- p + geom_text(
                    data = sample_sizes,
                    aes(x = x_pos, y = y, label = label),
                    hjust = 0.5,
                    vjust = -0.3,
                    size = 3,
                    fontface = "bold",
                    color = "black",
                    alpha = 0.8
                )
            }
            
            # Add density peak values if requested
            if (self$options$add_density_values) {
                # Calculate density peaks for each group
                if (!is.null(self$options$fill_var)) {
                    # When fill variable exists, calculate peaks by both y and fill
                    peak_data <- data %>%
                        group_by(y, fill) %>%
                        summarise(
                            peak_x = {
                                if(length(x) > 2) {
                                    dens <- density(x, na.rm = TRUE)
                                    dens$x[which.max(dens$y)]
                                } else {
                                    mean(x, na.rm = TRUE)
                                }
                            },
                            .groups = 'drop'
                        )
                } else {
                    # When no fill variable, calculate peaks by y only
                    peak_data <- data %>%
                        group_by(y) %>%
                        summarise(
                            peak_x = {
                                if(length(x) > 2) {
                                    dens <- density(x, na.rm = TRUE)
                                    dens$x[which.max(dens$y)]
                                } else {
                                    mean(x, na.rm = TRUE)
                                }
                            },
                            .groups = 'drop'
                        )
                }
                
                p <- p + geom_text(
                    data = peak_data,
                    aes(x = peak_x, y = y, label = round(peak_x, 1)),
                    vjust = 1.2,
                    hjust = 0.5,
                    size = 2.5,
                    color = "darkred",
                    fontface = "bold",
                    alpha = 0.8
                )
            }
            
            # Add custom annotations if provided
            if (!is.null(self$options$custom_annotations) && self$options$custom_annotations != "") {
                # Parse custom annotations: format "x,y,text;x2,y2,text2"
                annotations <- strsplit(self$options$custom_annotations, ";")[[1]]
                for (ann in annotations) {
                    parts <- trimws(strsplit(ann, ",")[[1]])
                    if (length(parts) == 3) {
                        # Try to parse x and y as numeric
                        x_val <- suppressWarnings(as.numeric(parts[1]))
                        y_val <- suppressWarnings(as.numeric(parts[2]))
                        
                        if (!is.na(x_val) && !is.na(y_val)) {
                            p <- p + annotate("text", 
                                x = x_val, 
                                y = y_val, 
                                label = parts[3],
                                size = 3.5,
                                color = "blue")
                        }
                    }
                }
            }
            
            return(p)
        },
        
        .createDensityPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            # Handle fill_ridges option
            if (self$options$fill_ridges) {
                if (!is.null(self$options$fill_var)) {
                    # Show legend based on user preference when fill variable is used
                    show_fill_legend <- self$options$show_fill_legend %||% TRUE
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = fill),
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        bandwidth = private$.calculateBandwidth(),
                        color = "black",
                        linewidth = 0.5,
                        show.legend = show_fill_legend
                    )
                } else {
                    # Hide legend for y-variable coloring (redundant with y-axis)
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = y),
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        bandwidth = private$.calculateBandwidth(),
                        color = "black",
                        linewidth = 0.5,
                        show.legend = FALSE
                    )
                }
                # Apply color palette for filled ridges
                p <- private$.applyColorPalette(p)
            } else {
                # Outline only - no fill
                p <- p + ggridges::geom_density_ridges(
                    scale = self$options$scale,
                    fill = NA,
                    color = "black",
                    linewidth = 0.75,
                    bandwidth = private$.calculateBandwidth()
                )
            }
            
            return(p)
        },
        
        .createGradientPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            p <- p + ggridges::geom_density_ridges_gradient(
                aes(fill = after_stat(x)),
                scale = self$options$scale,
                gradient_lwd = 1.0,
                bandwidth = private$.calculateBandwidth()
            )
            
            # Apply gradient colors
            p <- p + scale_fill_gradient(
                low = self$options$gradient_low,
                high = self$options$gradient_high,
                name = self$options$x_var
            )
            
            return(p)
        },
        
        .createHistogramPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            if (self$options$fill_ridges) {
                if (!is.null(self$options$fill_var)) {
                    # Show legend based on user preference when fill variable is used
                    show_fill_legend <- self$options$show_fill_legend %||% TRUE
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = fill),
                        stat = "binline",
                        binwidth = self$options$binwidth,
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        color = "black",
                        linewidth = 0.5,
                        show.legend = show_fill_legend
                    )
                } else {
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = y),
                        stat = "binline",
                        binwidth = self$options$binwidth,
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        color = "black",
                        linewidth = 0.5,
                        show.legend = FALSE
                    )
                }
                # Apply color palette
                p <- private$.applyColorPalette(p)
            } else {
                # Outline only
                p <- p + ggridges::geom_density_ridges(
                    stat = "binline",
                    binwidth = self$options$binwidth,
                    scale = self$options$scale,
                    fill = NA,
                    color = "black",
                    linewidth = 0.75
                )
            }
            
            return(p)
        },
        
        .createViolinPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            if (!is.null(self$options$fill_var)) {
                # Show legend based on user preference when fill variable is used
                show_fill_legend <- self$options$show_fill_legend %||% TRUE
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = fill, height = stat(density)),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    show.legend = show_fill_legend,
                    bandwidth = private$.calculateBandwidth()
                )
            } else {
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = y, height = stat(density)),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    show.legend = FALSE,
                    bandwidth = private$.calculateBandwidth()
                )
            }
            
            # Apply color palette
            p <- private$.applyColorPalette(p)
            
            return(p)
        },
        
        .calculateBandwidth = function() {
            if (self$options$bandwidth == "custom") {
                return(self$options$bandwidth_value)
            } else if (self$options$bandwidth != "nrd0") {
                return(self$options$bandwidth)
            }
            return(NULL)  # Use default
        },
        
        .applyColorPalette = function(p) {
            palette <- self$options$color_palette
            
            # Determine legend title
            legend_title <- if (!is.null(self$options$fill_var)) {
                self$options$fill_var
            } else {
                self$options$y_var
            }
            
            if (palette == "custom") {
                colors <- strsplit(self$options$custom_colors, ",")[[1]]
                colors <- trimws(colors)
                p <- p + scale_fill_manual(values = colors, name = legend_title)
            } else if (palette == "clinical_colorblind") {
                # Clinical colorblind-safe palette
                p <- p + scale_fill_manual(values = private$.CLINICAL_CB_SAFE_COLORS, name = legend_title)
            } else if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
                if (requireNamespace("viridis", quietly = TRUE)) {
                    p <- p + scale_fill_viridis_d(option = tolower(palette), name = legend_title)
                } else {
                    # Use constant instead of hardcoded values
                    p <- p + scale_fill_manual(values = private$.VIRIDIS_FALLBACK, name = legend_title)
                }
            } else if (palette %in% c("Set1", "Set2", "Dark2", "Paired")) {
                # Use RColorBrewer palettes
                if (requireNamespace("RColorBrewer", quietly = TRUE)) {
                    p <- p + scale_fill_brewer(palette = palette, name = legend_title)
                } else {
                    # Fallback to clinical colors
                    p <- p + scale_fill_manual(values = private$.CLINICAL_CB_SAFE_COLORS, name = legend_title)
                }
            } else {
                # Default to clinical colorblind-safe palette
                p <- p + scale_fill_manual(values = private$.CLINICAL_CB_SAFE_COLORS, name = legend_title)
            }
            
            return(p)
        },
        
        .applyTheme = function(p) {
            theme_style <- private$.option("theme_style")
            
            if (theme_style == "theme_ridges") {
                p <- p + ggridges::theme_ridges(grid = self$options$grid_lines)
            } else if (theme_style == "theme_minimal") {
                p <- p + theme_minimal()
            } else if (theme_style == "theme_classic") {
                p <- p + theme_classic()
            } else if (theme_style == "theme_dark") {
                # Create a dark theme
                p <- p + theme_minimal() +
                    theme(
                        plot.background = element_rect(fill = "#2E2E2E", color = NA),
                        panel.background = element_rect(fill = "#2E2E2E", color = NA),
                        panel.grid.major = element_line(color = "#555555", linewidth = 0.3),
                        panel.grid.minor = element_line(color = "#444444", linewidth = 0.2),
                        text = element_text(color = "#E0E0E0"),
                        axis.text = element_text(color = "#E0E0E0"),
                        axis.title = element_text(color = "#E0E0E0"),
                        legend.background = element_rect(fill = "#2E2E2E", color = NA),
                        legend.text = element_text(color = "#E0E0E0"),
                        legend.title = element_text(color = "#E0E0E0")
                    )
            } else if (theme_style == "theme_pubr") {
                p <- p + theme_minimal() +
                    theme(
                        panel.border = element_rect(fill = NA, color = "black"),
                        axis.line = element_line(color = "black")
                    )
            }
            
            # Apply additional theme modifications
            if (self$options$expand_panels) {
                p <- p + theme(
                    plot.margin = margin(0, 0, 0, 0),
                    panel.spacing = unit(0, "lines")
                )
            }
            
            # Set legend position
            p <- p + theme(legend.position = self$options$legend_position)
            
            return(p)
        },
        
        .applyLabels = function(p) {
            x_label <- if (self$options$x_label != "") self$options$x_label else self$options$x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else self$options$y_var
            
            p <- p + labs(
                x = x_label,
                y = y_label,
                title = if (self$options$plot_title != "") self$options$plot_title else NULL,
                subtitle = if (self$options$plot_subtitle != "") self$options$plot_subtitle else NULL,
                caption = if (self$options$plot_caption != "") self$options$plot_caption else NULL
            )
            
            return(p)
        },
        
        .generateStatistics = function(data) {
            stats_table <- self$results$statistics

            # Build grouping variables: always include y, optionally add fill and facet
            grouping_vars <- "y"
            if (!is.null(self$options$fill_var) && "fill" %in% names(data)) {
                grouping_vars <- c(grouping_vars, "fill")
            }
            if (!is.null(self$options$facet_var) && "facet" %in% names(data)) {
                grouping_vars <- c(grouping_vars, "facet")
            }

            # Calculate statistics for each combination of grouping variables
            stats <- data %>%
                group_by(across(all_of(grouping_vars))) %>%
                summarise(
                    n = n(),
                    mean = mean(x, na.rm = TRUE),
                    sd = sd(x, na.rm = TRUE),
                    median = median(x, na.rm = TRUE),
                    q1 = quantile(x, 0.25, na.rm = TRUE),
                    q3 = quantile(x, 0.75, na.rm = TRUE),
                    min = min(x, na.rm = TRUE),
                    max = max(x, na.rm = TRUE),
                    .groups = "drop"
                )

            # Populate table
            for (i in seq_len(nrow(stats))) {
                private$.checkpoint()

                # Build group label from all grouping variables
                group_label <- as.character(stats$y[i])
                if ("fill" %in% names(stats)) {
                    group_label <- paste0(group_label, " [", stats$fill[i], "]")
                }
                if ("facet" %in% names(stats)) {
                    group_label <- paste0(group_label, " (", stats$facet[i], ")")
                }

                stats_table$addRow(
                    rowKey = i,
                    values = list(
                        group = group_label,
                        n = stats$n[i],
                        mean = stats$mean[i],
                        sd = stats$sd[i],
                        median = stats$median[i],
                        q1 = stats$q1[i],
                        q3 = stats$q3[i],
                        min = stats$min[i],
                        max = stats$max[i]
                    )
                )
            }
        },

        # Helper method: Perform a single statistical test with proper warnings
        .performSingleTest = function(data1, data2, group1, group2, stratum_label = "") {
            test_type <- private$.option("test_type")
            statistic <- NA
            p_value <- NA
            ci_lower <- NA
            ci_upper <- NA
            test_method <- ""
            warning_msg <- NULL

            # Guard for insufficient observations
            if (length(data1) < 2 || length(data2) < 2) {
                warning_msg <- paste0(
                    "Insufficient observations for comparison ",
                    group1, " vs ", group2,
                    if (stratum_label != "") paste0(" (", stratum_label, ")") else "",
                    ". Need at least 2 observations per group."
                )
                return(list(
                    comparison = paste(group1, "vs", group2),
                    statistic = NA,
                    p_value = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    effect_size = NA,
                    effect_ci_lower = NA,
                    effect_ci_upper = NA,
                    test_method = test_type,
                    warning = warning_msg
                ))
            }

            if (test_type == "parametric") {
                assumption_violations <- c()

                # Normality checks
                if (length(data1) >= 3 && length(data1) <= 500) {
                    sw1 <- tryCatch(shapiro.test(data1), error = function(e) NULL)
                    if (!is.null(sw1) && !is.na(sw1$p.value) && sw1$p.value < 0.05) {
                        assumption_violations <- c(assumption_violations, paste0(group1, " non-normal (Shapiro p=", round(sw1$p.value, 3), ")"))
                    }
                }
                if (length(data2) >= 3 && length(data2) <= 500) {
                    sw2 <- tryCatch(shapiro.test(data2), error = function(e) NULL)
                    if (!is.null(sw2) && !is.na(sw2$p.value) && sw2$p.value < 0.05) {
                        assumption_violations <- c(assumption_violations, paste0(group2, " non-normal (Shapiro p=", round(sw2$p.value, 3), ")"))
                    }
                }

                # Variance check
                if (requireNamespace("car", quietly = TRUE)) {
                    df_lv <- data.frame(
                        val = c(data1, data2),
                        grp = factor(rep(c("g1", "g2"), c(length(data1), length(data2))))
                    )
                    lv <- tryCatch(car::leveneTest(val ~ grp, data = df_lv, center = median), error = function(e) NULL)
                    if (!is.null(lv) && !is.na(lv$`Pr(>F)`[1]) && lv$`Pr(>F)`[1] < 0.05) {
                        assumption_violations <- c(assumption_violations, paste0("Variance heterogeneity (Levene p=", round(lv$`Pr(>F)`[1], 3), ")"))
                    }
                }

                if (length(assumption_violations) > 0) {
                    # Auto-suggest and switch to nonparametric for robustness
                    # Create WARNING Notice about assumption violations
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('assumptionViolation_', group1, '_', group2),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice_msg <- paste0(
                        "Assumption check failed: ", paste(assumption_violations, collapse = "; "),
                        " • Using Wilcoxon test instead of t-test (auto-suggested)."
                    )
                    notice$setContent(notice_msg)
                    self$results$insert(100, notice)

                    test_result <- wilcox.test(data1, data2, conf.int = TRUE)
                    statistic <- test_result$statistic
                    p_value <- test_result$p.value
                    ci_lower <- if(!is.null(test_result$conf.int)) test_result$conf.int[1] else NA
                    ci_upper <- if(!is.null(test_result$conf.int)) test_result$conf.int[2] else NA
                    test_method <- "Wilcoxon (auto due to assumptions)"
                } else {
                    test_result <- t.test(data1, data2)
                    statistic <- test_result$statistic
                    p_value <- test_result$p.value
                    ci_lower <- test_result$conf.int[1]
                    ci_upper <- test_result$conf.int[2]
                    test_method <- "t-test"
                }

            } else if (test_type == "nonparametric") {
                test_result <- wilcox.test(data1, data2, conf.int = TRUE)
                statistic <- test_result$statistic
                p_value <- test_result$p.value
                ci_lower <- if(!is.null(test_result$conf.int)) test_result$conf.int[1] else NA
                ci_upper <- if(!is.null(test_result$conf.int)) test_result$conf.int[2] else NA
                test_method <- "Wilcoxon"

            } else if (test_type == "robust") {
                # CRITICAL FIX: Warn if WRS2 unavailable instead of silent fallback
                if (requireNamespace("WRS2", quietly = TRUE)) {
                    # WRS2::yuen requires formula interface
                    df_robust <- data.frame(
                        val = c(data1, data2),
                        grp = factor(rep(c("g1", "g2"), c(length(data1), length(data2))))
                    )
                    test_result <- WRS2::yuen(val ~ grp, data = df_robust)
                    statistic <- test_result$test
                    p_value <- test_result$p.value
                    ci_lower <- test_result$conf.int[1]
                    ci_upper <- test_result$conf.int[2]
                    test_method <- "Yuen (robust)"
                } else {
                    # Fallback with warning
                    test_result <- t.test(data1, data2)
                    statistic <- test_result$statistic
                    p_value <- test_result$p.value
                    ci_lower <- test_result$conf.int[1]
                    ci_upper <- test_result$conf.int[2]
                    test_method <- "t-test (WRS2 unavailable)"

                    # Create WARNING Notice about WRS2 unavailability
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('wrs2Unavailable_', group1, '_', group2),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice_msg <- paste0(
                        "WRS2 package not available for robust test. Falling back to standard t-test for comparison: ",
                        group1, " vs ", group2,
                        if(stratum_label != "") paste0(" (", stratum_label, ")") else ""
                    )
                    notice$setContent(notice_msg)
                    self$results$insert(100, notice)
                }

            } else if (test_type == "bayes") {
                # CRITICAL FIX: Implement Bayesian test or warn
                if (requireNamespace("BayesFactor", quietly = TRUE)) {
                    # Use BayesFactor for Bayesian t-test
                    bf_result <- tryCatch({
                        BayesFactor::ttestBF(x = data1, y = data2)
                    }, error = function(e) NULL)

                    if (!is.null(bf_result)) {
                        statistic <- exp(bf_result@bayesFactor$bf)  # Bayes Factor
                        p_value <- NA  # Bayesian inference doesn't use p-values
                        ci_lower <- NA
                        ci_upper <- NA
                        test_method <- "Bayesian t-test"
                    } else {
                        statistic <- NA
                        p_value <- NA
                        ci_lower <- NA
                        ci_upper <- NA
                        test_method <- "Bayesian (failed)"

                        # Create WARNING Notice about Bayesian test failure
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = paste0('bayesianTestFailed_', group1, '_', group2),
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(paste0("Bayesian test failed for: ", group1, " vs ", group2))
                        self$results$insert(100, notice)
                    }
                } else {
                    statistic <- NA
                    p_value <- NA
                    ci_lower <- NA
                    ci_upper <- NA
                    test_method <- "Bayesian (unavailable)"

                    # Create WARNING Notice about BayesFactor unavailability
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('bayesFactorUnavailable_', group1, '_', group2),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(paste0(
                        "BayesFactor package not available. Cannot perform Bayesian test for: ",
                        group1, " vs ", group2
                    ))
                    self$results$insert(100, notice)
                }

            } else {
                # Create WARNING Notice about unknown test type
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = paste0('unknownTestType_', group1, '_', group2),
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(paste0("Unknown test type '", test_type, "' for: ", group1, " vs ", group2))
                self$results$insert(100, notice)
            }

            # Calculate effect size with proper CIs
            effect_result <- private$.calculateEffectSizeWithCI(data1, data2)

            # Build comparison label
            comparison_label <- paste(group1, "vs", group2)
            if (stratum_label != "") {
                comparison_label <- paste0(comparison_label, " (", stratum_label, ")")
            }

            return(list(
                comparison = comparison_label,
                statistic = statistic,
                p_value = p_value,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                effect_size = effect_result$effect_size,
                effect_ci_lower = effect_result$ci_lower,
                effect_ci_upper = effect_result$ci_upper,
                test_method = test_method
            ))
        },

        # Helper method: Calculate effect size using effectsize package (robust and validated)
        .calculateEffectSizeWithCI = function(data1, data2) {
            effsize_type <- private$.option("effsize_type")
            effect_size <- NA
            ci_lower <- NA
            ci_upper <- NA
            warning_msg <- NULL

            n1 <- length(data1)
            n2 <- length(data2)

            # Check minimum sample sizes
            if (n1 < 3 || n2 < 3) {
                return(list(
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    warning = paste0("Insufficient sample size for effect size (n1=", n1, ", n2=", n2, "). ",
                                   "Minimum 3 observations per group required.")
                ))
            }

            # Use effectsize package for robust, validated calculations
            tryCatch({
                if (effsize_type == "d") {
                    # Cohen's d using effectsize package
                    if (requireNamespace("effectsize", quietly = TRUE)) {
                        result <- effectsize::cohens_d(data1, data2, pooled_sd = TRUE, ci = 0.95)
                        effect_size <- as.numeric(result$Cohens_d)
                        ci_lower <- result$CI_low
                        ci_upper <- result$CI_high
                    } else {
                        warning_msg <- "effectsize package not available for Cohen's d"
                    }

                } else if (effsize_type == "g") {
                    # Hedges' g using effectsize package
                    if (requireNamespace("effectsize", quietly = TRUE)) {
                        result <- effectsize::hedges_g(data1, data2, pooled_sd = TRUE, ci = 0.95)
                        effect_size <- as.numeric(result$Hedges_g)
                        ci_lower <- result$CI_low
                        ci_upper <- result$CI_high
                    } else {
                        warning_msg <- "effectsize package not available for Hedges' g"
                    }

                } else if (effsize_type == "eta") {
                    # Eta squared using effectsize package
                    if (requireNamespace("effectsize", quietly = TRUE)) {
                        # Create a simple data frame for eta_squared
                        df <- data.frame(
                            value = c(data1, data2),
                            group = factor(rep(c("g1", "g2"), c(n1, n2)))
                        )
                        model <- aov(value ~ group, data = df)
                        result <- effectsize::eta_squared(model, ci = 0.95)
                        effect_size <- as.numeric(result$Eta2)
                        ci_lower <- result$CI_low
                        ci_upper <- result$CI_high
                    } else {
                        warning_msg <- "effectsize package not available for Eta squared"
                    }

                } else if (effsize_type == "omega") {
                    # Omega squared using effectsize package
                    if (requireNamespace("effectsize", quietly = TRUE)) {
                        df <- data.frame(
                            value = c(data1, data2),
                            group = factor(rep(c("g1", "g2"), c(n1, n2)))
                        )
                        model <- aov(value ~ group, data = df)
                        result <- effectsize::omega_squared(model, ci = 0.95)
                        effect_size <- as.numeric(result$Omega2)
                        ci_lower <- result$CI_low
                        ci_upper <- result$CI_high
                    } else {
                        warning_msg <- "effectsize package not available for Omega squared"
                    }

                } else if (effsize_type == "cliff_delta") {
                    # Cliff's Delta - keep custom implementation (not in effectsize core)
                    effect_size <- private$.calculateCliffsDelta(data1, data2)

                    # Bootstrap CI for Cliff's Delta
                    if (requireNamespace("boot", quietly = TRUE)) {
                        boot_fn <- function(data, indices) {
                            d1 <- data[indices[indices <= n1]]
                            d2 <- data[indices[indices > n1]]
                            private$.calculateCliffsDelta(d1, d2)
                        }
                        boot_result <- tryCatch({
                            boot::boot(c(data1, data2), boot_fn, R = 1000)
                        }, error = function(e) NULL)

                        if (!is.null(boot_result)) {
                            ci_result <- boot::boot.ci(boot_result, type = "perc")
                            if (!is.null(ci_result$percent)) {
                                ci_lower <- ci_result$percent[4]
                                ci_upper <- ci_result$percent[5]
                            }
                        }
                    }

                } else if (effsize_type == "hodges_lehmann") {
                    # Hodges-Lehmann shift (median of pairwise differences)
                    effect_size <- private$.calculateHodgesLehmann(data1, data2)

                    # CI from Wilcoxon test
                    wilcox_result <- wilcox.test(data1, data2, conf.int = TRUE)
                    if (!is.null(wilcox_result$conf.int)) {
                        ci_lower <- wilcox_result$conf.int[1]
                        ci_upper <- wilcox_result$conf.int[2]
                    }
                }

            }, error = function(e) {
                # Create WARNING Notice about effect size calculation error
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = paste0('effectSizeError_', as.integer(runif(1, 1, 1e6))),
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(paste0("Error calculating effect size: ", e$message))
                self$results$insert(100, notice)
            })

            return(list(
                effect_size = effect_size,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },

        # Helper method: Check for potential repeated measures
        .checkRepeatedMeasures = function(data) {
            # CRITICAL FIX: Detect data patterns suggesting repeated measures/clustering
            # This helps warn users when independence assumption is likely violated

            # Calculate observations per group level
            group_counts <- table(data$y)
            total_obs <- nrow(data)
            n_groups <- length(group_counts)

            # Heuristic: If average observations per group is much higher than expected
            # for independent samples, warn about possible repeated measures
            avg_obs_per_group <- mean(group_counts)

            # Check 1: High replication rate (>30 obs per group on average)
            # Suggests longitudinal data or repeated biopsies
            if (avg_obs_per_group > 30 && n_groups <= 5) {
                return(paste0(
                    "Data shows high observation density (avg ", round(avg_obs_per_group, 1),
                    " observations per group). This pattern is common in repeated measures designs ",
                    "(e.g., multiple time points, repeated biopsies per patient)."
                ))
            }

            # Check 2: Look for integer-valued X variable with repeated values
            # Common pattern: time points, visit numbers, days post-treatment
            x_values <- data$x
            if (all(x_values == floor(x_values), na.rm = TRUE)) {
                # Count unique X values
                n_unique_x <- length(unique(x_values))
                # If we have many observations but few unique X values, likely repeated measures
                if (total_obs > 50 && n_unique_x < 15) {
                    return(paste0(
                        "Data contains ", n_unique_x, " unique X values with ",
                        total_obs, " total observations. This pattern suggests time series ",
                        "or repeated measurements (e.g., Day 1, Day 7, Day 14)."
                    ))
                }
            }

            # Check 3: Look for variables with "patient", "subject", "id" in the data
            # (checked in parent data, not just plot_data)
            if (!is.null(self$data)) {
                col_names <- tolower(names(self$data))
                has_id_vars <- any(grepl("patient|subject|id|case", col_names))

                if (has_id_vars && total_obs > 50) {
                    return(paste0(
                        "Dataset contains identifier variables (patient/subject ID columns). ",
                        "If analyzing multiple observations per patient, results assume independence ",
                        "which may not be appropriate."
                    ))
                }
            }

            # No obvious repeated measures pattern detected
            return(NULL)
        },

        # Helper method: Adjust p-values handling NA correctly
        .adjustPValues = function(p_values) {
            if (self$options$p_adjust_method == "none") {
                return(p_values)
            }

            # CRITICAL FIX: Handle NA values correctly
            # Apply adjustment only to non-NA values, preserve NA positions
            non_na_indices <- which(!is.na(p_values))

            if (length(non_na_indices) == 0) {
                # All NA, return as is
                return(p_values)
            }

            # Adjust only non-NA p-values
            adjusted <- p_values
            adjusted[non_na_indices] <- p.adjust(p_values[non_na_indices],
                                                 method = self$options$p_adjust_method)

            return(adjusted)
        },

        # Helper method: Add test row to table
        .addTestRow = function(tests_table, test_result, adjusted_p, row_key) {
            # Warnings are now handled via jmvcore::Notice API directly where they occur
            # No need to accumulate warnings in HTML

            tests_table$addRow(
                rowKey = row_key,
                values = list(
                    comparison = test_result$comparison,
                    statistic = test_result$statistic,
                    p_value = test_result$p_value,
                    p_adjusted = adjusted_p,
                    effect_size = test_result$effect_size,
                    ci_lower = test_result$effect_ci_lower,  # Effect size CI, not test CI
                    ci_upper = test_result$effect_ci_upper   # Effect size CI, not test CI
                )
            )
        },

        .generateTests = function(data) {
            tests_table <- self$results$tests

            # Build stratification variables (facet and fill if present)
            strata_vars <- c()
            if (!is.null(self$options$facet_var) && "facet" %in% names(data)) {
                strata_vars <- c(strata_vars, "facet")
            }
            if (!is.null(self$options$fill_var) && "fill" %in% names(data)) {
                strata_vars <- c(strata_vars, "fill")
            }

            # If there are strata, perform comparisons within each stratum
            if (length(strata_vars) > 0) {
                # Get all unique combinations of strata
                strata_combinations <- data %>%
                    select(all_of(strata_vars)) %>%
                    distinct()

                all_test_results <- list()
                all_p_values <- numeric()

                for (s in seq_len(nrow(strata_combinations))) {
                    # Filter data for this stratum
                    stratum_data <- data
                    stratum_label <- ""
                    for (sv in strata_vars) {
                        stratum_val <- strata_combinations[[sv]][s]
                        stratum_data <- stratum_data[stratum_data[[sv]] == stratum_val, ]
                        stratum_label <- paste0(stratum_label, if(stratum_label != "") " / " else "", sv, "=", stratum_val)
                    }

                    # Perform pairwise comparisons within this stratum
                    groups <- unique(stratum_data$y)
                    if (length(groups) > 1) {
                        comparisons <- combn(groups, 2, simplify = FALSE)

                        for (i in seq_along(comparisons)) {
                            private$.checkpoint(flush = FALSE)
                            group1 <- comparisons[[i]][1]
                            group2 <- comparisons[[i]][2]

                            data1 <- stratum_data$x[stratum_data$y == group1]
                            data2 <- stratum_data$x[stratum_data$y == group2]

                            # Perform test and store results with stratum label
                            test_result <- private$.performSingleTest(data1, data2, group1, group2, stratum_label)
                            all_test_results <- c(all_test_results, list(test_result))
                            all_p_values <- c(all_p_values, test_result$p_value)
                        }
                    }
                }

                # Apply p-value adjustment across all tests
                adjusted_p <- private$.adjustPValues(all_p_values)

                # Add rows to table
                for (i in seq_along(all_test_results)) {
                    private$.addTestRow(tests_table, all_test_results[[i]], adjusted_p[i], i)
                }

            } else {
                # No strata: perform simple pairwise comparisons as before
                groups <- unique(data$y)
                if (length(groups) > 1) {
                    comparisons <- combn(groups, 2, simplify = FALSE)
                    p_values <- numeric(length(comparisons))
                    test_results <- list()

                    for (i in seq_along(comparisons)) {
                        private$.checkpoint(flush = FALSE)
                        group1 <- comparisons[[i]][1]
                        group2 <- comparisons[[i]][2]

                        data1 <- data$x[data$y == group1]
                        data2 <- data$x[data$y == group2]

                        # Use helper method for single test
                        test_result <- private$.performSingleTest(data1, data2, group1, group2, "")
                        test_results[[i]] <- test_result
                        p_values[i] <- test_result$p_value
                    }

                    # Apply p-value adjustment using helper method
                    adjusted_p <- private$.adjustPValues(p_values)

                    # Add rows to table using helper method
                    for (i in seq_along(test_results)) {
                        private$.addTestRow(tests_table, test_results[[i]], adjusted_p[i], i)
                    }
                }
            }
        },
        
        .generateInterpretation = function(data) {
            # Generate clinical summary first
            clinical_summary <- private$.generateClinicalSummary(data, private$.option("show_stats"))
            
            # Traditional interpretation
            n_groups <- length(unique(data$y))
            n_total <- nrow(data)
            
            interpretation <- paste0(
                clinical_summary,
                "<h4>", .("Technical Interpretation"), "</h4>",
                "<p>", .("The ridge plot displays the distribution of"), " <strong>", self$options$x_var, "</strong> ",
                .("across"), " <strong>", n_groups, " ", .("groups"), "</strong> ", .("defined by"), " <strong>", self$options$y_var, "</strong>.</p>",
                "<ul>",
                "<li><strong>", .("Each ridge:"), "</strong> ", .("Shows the probability density or frequency distribution for a group"), "</li>",
                "<li><strong>", .("Overlapping areas:"), "</strong> ", .("Indicate similar value ranges between groups"), "</li>",
                "<li><strong>", .("Ridge height and spread:"), "</strong> ", .("Indicate the concentration and variability of values"), "</li>",
                "<li><strong>", .("Peaks:"), "</strong> ", .("Show the most common values (modes) within each group"), "</li>",
                "</ul>"
            )
            
            # Add plot-specific interpretations
            if (private$.option("plot_type") == "density_ridges_gradient") {
                interpretation <- paste0(
                    interpretation,
                    "<div style='background:#e3f2fd; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<strong>🎨 ", .("Gradient Coloring:"), "</strong> ", .("Colors represent the value of"), " ", 
                    self$options$x_var, " ", .("along each ridge, helping visualize how values are distributed within groups."), "</div>"
                )
            }
            
            if (private$.option("add_boxplot")) {
                interpretation <- paste0(
                    interpretation,
                    "<div style='background:#e8f5e8; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<strong>📊 ", .("Boxplots:"), "</strong> ", .("Show median (center line), quartiles (box boundaries), and outliers for each group."), " ",
                    .("Compare medians and quartile ranges across groups for clinical significance."), "</div>"
                )
            }
            
            # Add statistical context if tests were run
            if (private$.option("show_stats")) {
                interpretation <- paste0(
                    interpretation,
                    "<div style='background:#fff3e0; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<strong>📈 ", .("Statistical Tests:"), "</strong> ", .("Pairwise comparisons test whether group differences are statistically significant."), " ",
                    .("Consider effect sizes alongside p-values for clinical importance. Adjust for multiple comparisons when appropriate."), "</div>"
                )
            }
            
            self$results$interpretation$setContent(interpretation)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$x_var) || is.null(self$options$y_var))
                return()

            plot <- image$state

            if (!is.null(plot)) {
                # Apply DPI setting for high-resolution output
                dpi <- self$options$dpi
                if (!is.null(dpi) && dpi > 0) {
                    # Store current setting
                    old_dpi <- getOption("device.dpi", 72)
                    # Set requested DPI
                    options(device.dpi = dpi)
                    # Print plot with new DPI
                    print(plot)
                    # Restore original DPI
                    options(device.dpi = old_dpi)
                } else {
                    print(plot)
                }
                return(TRUE)
            }

            return(FALSE)
        },
        
        .calculateCliffsDelta = function(x, y) {
            # Prefer effectsize::cliff_delta when available for efficiency and validation
            if (requireNamespace("effectsize", quietly = TRUE)) {
                res <- effectsize::cliff_delta(x, y, ci = NULL, alternative = "two.sided", distribution = "continuous")
                if (!is.null(res$CLES)) {
                    return(as.numeric(res$CLES * 2 - 1))  # convert common language effect size to delta scale
                }
                if (!is.null(res$delta)) {
                    return(as.numeric(res$delta))
                }
            }

            # Fallback manual computation: proportion of pairs where x > y minus proportion where x < y
            x <- x[!is.na(x)]
            y <- y[!is.na(y)]
            n1 <- length(x)
            n2 <- length(y)
            if (n1 == 0 || n2 == 0) return(NA_real_)

            greater <- 0
            less <- 0

            for (xi in x) {
                for (yj in y) {
                    if (xi > yj) greater <- greater + 1
                    else if (xi < yj) less <- less + 1
                }
            }

            delta <- (greater - less) / (n1 * n2)
            return(delta)
        },
        
        .calculateHodgesLehmann = function(x, y) {
            # Hodges-Lehmann shift: median of all pairwise differences (x - y)
            # Shows typical difference in lymph node counts between techniques
            x <- x[!is.na(x)]
            y <- y[!is.na(y)]
            if (length(x) == 0 || length(y) == 0) return(NA_real_)

            # Use vectorised outer for efficiency
            differences <- as.vector(outer(x, y, "-"))
            return(median(differences))
        }
    )
)
