#' @title Summary of Continuous Variables with Distribution Diagnostics
#' @return Text and an HTML summary table (with optional distribution diagnostics)
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt tab_header fmt_number cols_label md cell_fill cells_column_labels cell_text tab_style opt_stylize tab_options
#' @importFrom gtExtras gt_plt_summary
#' @importFrom htmltools HTML
#' @importFrom moments kurtosis skewness
#' @importFrom utils packageVersion
#' @noRd
NULL

summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass",
    inherit = summarydataBase, private = list(

        # Per-run cache of Shapiro-Wilk results keyed by variable name
        .shapiroCache = NULL,

        .run = function() {
        # Reset per-run Shapiro-Wilk cache (R6 instance persists across runs)
        private$.shapiroCache <- list()
        # Check if variables have been selected. If not, display a welcoming message with instructions.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
          <h3>Welcome to ClinicoPath Descriptives!</h3>
          <p>This tool helps you generate descriptive statistics for your numeric variables.
          Please select one or more continuous variables from the options panel.</p>
          <p>If you want to inspect distribution characteristics, enable the 'Distribution Diagnostics' option.</p>"
            self$results$todo$setContent(intro_msg)
            return()
        } else {
            # Clear any introductory message if variables are selected.
            self$results$todo$setContent("")
            # Validate that the dataset contains complete rows.
            if (nrow(self$data) == 0) {
                jmvcore::reject(.("Error: The provided dataset contains no complete rows. Please check your data and try again."))
            }
            
            vars <- self$options$vars

            # Remove non-numeric variables and variables with all NAs
            vars_to_remove <- c()
            warning_msgs <- c()

            for (var in vars) {
                if (!is.numeric(self$data[[var]])) {
                    vars_to_remove <- c(vars_to_remove, var)
                    warning_msgs <- c(warning_msgs, paste0("Variable '", htmltools::htmlEscape(var), "' is not numeric"))
                } else if (all(is.na(self$data[[var]]))) {
                    vars_to_remove <- c(vars_to_remove, var)
                    warning_msgs <- c(warning_msgs, paste0("Variable '", htmltools::htmlEscape(var), "' contains only missing values"))
                }
            }

            if (length(warning_msgs) > 0) {
                self$results$todo$setContent(paste0("<div style='color: #856404; background: #fff3cd; padding: 10px; border-radius: 4px;'>",
                    paste(warning_msgs, collapse="<br>"),
                    "</div>"))
            }

            vars <- setdiff(vars, vars_to_remove)

            if (length(vars) == 0) {
                return()
            }

            # Retrieve the data and construct the list of variables.
            dataset <- self$data
            # Use the filtered variable names directly as data[[]] keys. Round-tripping
            # through constructFormula/decomposeFormula can mangle names containing
            # spaces or special characters, breaking downstream dataset[[var]] lookups.
            var_list <- vars
            # mysummary function with optimized calculations
            mysummary <- function(myvar) {
                # Cache numeric conversion to avoid repeated calls
                numeric_data <- jmvcore::toNumeric(dataset[[myvar]])
                
                # Calculate all statistics at once with specified decimal places
                decimal_places <- self$options$decimal_places
                
                stats <- round(c(
                    mean = mean(numeric_data, na.rm = TRUE),
                    sd = sd(numeric_data, na.rm = TRUE),
                    median = median(numeric_data, na.rm = TRUE),
                    min = min(numeric_data, na.rm = TRUE),
                    max = max(numeric_data, na.rm = TRUE)
                ), digits = decimal_places)
                
                mean_x <- stats["mean"]
                sd_x <- stats["sd"]
                median_x <- stats["median"]
                min_x <- stats["min"]
                max_x <- stats["max"]
                dist_text <- ""
                # If the distribution diagnostics option is enabled, add additional tests.
                if (self$options$distr) {
                    # Shapiro-Wilk test (only valid if 3 <= sample size <= 5000)
                    # Use already cached numeric_data
                    valid_data <- na.omit(numeric_data)

                    # Initialize variables
                    p_val <- NA
                    distribution_assessment <- NULL

                    # Check if data has variance (not all values identical)
                    n_unique <- length(unique(valid_data))

                    if (n_unique == 1) {
                        # All values are identical - no variance
                        distribution_assessment <- .("The data are constant and have no variance.")
                    } else {
                        # Shapiro-Wilk test (cached per run, shared with report sentences).
                        # Returns NULL when n is outside the valid 3-5000 range.
                        sw_test <- private$.shapiroResult(valid_data, key = myvar)
                        if (!is.null(sw_test)) {
                            p_val <- round(sw_test$p.value, 3)

                            # Interpret normality result
                            distribution_assessment <- if (p_val > 0.05) {
                                .("The data are consistent with a normal distribution.")
                            } else {
                                .("The data are not consistent with a normal distribution; inspect the distribution visually and use appropriate tests.")
                            }
                        }
                    }

                    if (is.null(distribution_assessment)) {
                        distribution_assessment <- .("Normality was not assessed because the sample size is outside the Shapiro-Wilk test range.")
                    }

                    # Calculate skewness and kurtosis using the moments package.
                    skew_val <- round(moments::skewness(numeric_data, na.rm = TRUE), 2)
                    kurt_val <- round(moments::kurtosis(numeric_data, na.rm = TRUE), 2)

                    if (is.na(p_val)) {
                        # Normality was not assessed (constant data or n outside 3-5000);
                        # show only the assessment, not NA/NaN diagnostics.
                        dist_text <- jmvcore::format(
                            .("<br><em>Distribution diagnostics for {variable}:</em> {assessment}"),
                            variable = htmltools::htmlEscape(myvar),
                            assessment = distribution_assessment
                        )
                    } else {
                        dist_text <- jmvcore::format(
                            .("<br><em>Distribution diagnostics for {variable}:</em> Shapiro-Wilk p-value = {p}; skewness = {skewness}; kurtosis = {kurtosis}. {assessment}"),
                            variable = htmltools::htmlEscape(myvar),
                            p = p_val,
                            skewness = skew_val,
                            kurtosis = kurt_val,
                            assessment = distribution_assessment
                        )
                    }
                }
                # Per-variable sample size and missingness for a self-describing headline.
                n_x <- sum(!is.na(numeric_data))
                missing_x <- sum(is.na(numeric_data))
                summary_text <- jmvcore::format(
                    .("<strong>{variable}</strong> (N = {n}, missing = {missing}): Mean {mean} \u{00B1} {sd}. Median: {median} (minimum: {minimum}; maximum: {maximum})."),
                    variable = htmltools::htmlEscape(myvar),
                    n = n_x,
                    missing = missing_x,
                    mean = mean_x,
                    sd = sd_x,
                    median = median_x,
                    minimum = min_x,
                    maximum = max_x
                )
                paste0(summary_text, dist_text, "<br><br>")
            }
            results <- purrr::map(.x = var_list, .f = mysummary)
            # Collapse the per-variable summary strings into a single HTML string.
            # setContent() on an Html result item writes to a non-repeated protobuf
            # field, so a length > 1 vector (multiple variables) triggers a
            # serialization error. Each element already ends with <br><br>.
            results <- paste(unlist(results), collapse = "")
            self$results$text$setContent(results)
            # CORRECT IMPLEMENTATION: Use gtExtras as intended by the package
            plot_dataset <- tryCatch({
                # Filter to numeric variables for gtExtras
                numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]

                # Filter and validate numeric variables

                if (length(numeric_vars) > 0) {
                    # Ensure clean data.frame conversion from jamovi data structure
                    clean_data <- as.data.frame(dataset[numeric_vars], stringsAsFactors = FALSE)

                    # Ensure proper data types and remove any attributes that might interfere
                    clean_data <- as.data.frame(lapply(clean_data, function(x) {
                        x <- if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
                        # Remove any attributes that might cause issues
                        attributes(x) <- NULL
                        x
                    }), stringsAsFactors = FALSE)

                    # Restore column names
                    names(clean_data) <- numeric_vars

                    # Use gtExtras with default styling as intended
                    private$.checkpoint()
                    summary_table <- clean_data %>%
                        gtExtras::gt_plt_summary()

                    # Convert to HTML with improved compatibility
                    html_result <- tryCatch({
                        # Primary method: as_raw_html for clean HTML output
                        as.character(gt::as_raw_html(summary_table))
                    }, error = function(e2) {
                        # Fallback: Direct table conversion
                        tryCatch({
                            as.character(summary_table)
                        }, error = function(e3) {
                            # Final fallback: use custom method
                            as.character(private$.gtExtras_style_fallback(dataset, var_list))
                        })
                    })

                    htmltools::HTML(html_result)
                } else {
                    htmltools::HTML(paste0("<p>", .("No numeric variables selected for summary table."), "</p>"))
                }
            }, error = function(e) {
                # If gtExtras fails, use the comprehensive fallback without error message
                # This is a design choice to avoid alarming users when the fallback works perfectly
                simple_table <- private$.gtExtras_style_fallback(dataset, var_list)
                htmltools::HTML(as.character(simple_table))
            })
            
            
            self$results$text1$setContent(plot_dataset)
            
            # Generate clinical interpretation content
            clinical_interpretation <- private$.generateClinicalInterpretation(var_list, dataset)
            self$results$clinicalInterpretation$setContent(clinical_interpretation)
            
            # Generate about analysis content
            about_content <- private$.generateAboutContent()
            self$results$aboutAnalysis$setContent(about_content)
            
            # Generate outlier detection report if enabled
            if (self$options$outliers) {
                outlier_report <- private$.generateOutlierReport(var_list, dataset)
                self$results$outlierReport$setContent(outlier_report)
            }
            
            # Generate report sentences if enabled
            if (self$options$report_sentences) {
                report_sentences <- private$.generateReportSentences(var_list, dataset)
                self$results$reportSentences$setContent(report_sentences)
            }
            
            # Generate statistical glossary
            glossary_content <- private$.generateGlossary()
            self$results$glossary$setContent(glossary_content)
        }
        },
        # Compute (and cache per run) the Shapiro-Wilk test for a numeric vector.
        # Returns the htest object, or NULL when the test is not applicable
        # (n outside 3-5000, constant data, or an error). Shared by the diagnostics
        # text and the report sentences so the test runs at most once per variable.
        .shapiroResult = function(x, key = NULL) {
            if (is.null(private$.shapiroCache))
                private$.shapiroCache <- list()
            if (!is.null(key) && !is.null(private$.shapiroCache[[key]]))
                return(private$.shapiroCache[[key]]$value)
            valid <- x[!is.na(x)]
            result <- NULL
            if (length(valid) >= 3 && length(valid) <= 5000 && length(unique(valid)) > 1)
                result <- tryCatch(shapiro.test(valid), error = function(e) NULL)
            if (!is.null(key))
                private$.shapiroCache[[key]] <- list(value = result)
            result
        },
        # Fallback with gtExtras-style appearance
        .gtExtras_style_fallback = function(dataset, var_list) {
            # Get numeric variables only
            numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]
            
            if (length(numeric_vars) == 0) {
                return(htmltools::HTML("<p>No numeric variables available for summary table.</p>"))
            }

            # Match the text summary's precision (see decimal_places option)
            dp <- self$options$decimal_places

            # Calculate comprehensive summary statistics using vectorized operations
            calc_stats <- function(x) {
                x <- as.numeric(x)
                x_clean <- x[!is.na(x)]
                c(
                    n = length(x_clean),
                    missing = sum(is.na(x)),
                    mean = mean(x_clean, na.rm = TRUE),
                    sd = sd(x_clean, na.rm = TRUE),
                    min = min(x_clean, na.rm = TRUE),
                    q25 = unname(quantile(x_clean, 0.25, na.rm = TRUE)),
                    median = median(x_clean, na.rm = TRUE),
                    q75 = unname(quantile(x_clean, 0.75, na.rm = TRUE)),
                    max = max(x_clean, na.rm = TRUE)
                )
            }
            
            # Apply vectorized calculation to all variables
            stats_matrix <- vapply(dataset[numeric_vars], calc_stats, numeric(9))

            # Ensure stats_matrix is always a matrix (vapply returns vector for single variable)
            if (!is.matrix(stats_matrix)) {
                stats_matrix <- matrix(stats_matrix, ncol = 1)
                rownames(stats_matrix) <- c("n", "missing", "mean", "sd", "min", "q25", "median", "q75", "max")
                colnames(stats_matrix) <- numeric_vars
            }

            summary_stats <- data.frame(
                Variable = numeric_vars,
                Type = rep("numeric", length(numeric_vars)),
                N = round(stats_matrix["n", ]),
                Missing = round(stats_matrix["missing", ]),
                Mean = round(stats_matrix["mean", ], dp),
                SD = round(stats_matrix["sd", ], dp),
                Min = round(stats_matrix["min", ], dp),
                Q25 = round(stats_matrix["q25", ], dp),
                Median = round(stats_matrix["median", ], dp),
                Q75 = round(stats_matrix["q75", ], dp),
                Max = round(stats_matrix["max", ], dp),
                stringsAsFactors = FALSE
            )
            
            # Create gtExtras-style table
            gt_table <- summary_stats %>%
                gt::gt() %>%
                gt::tab_header(
                    title = gt::md(paste0("**", .("Dataset Summary"), "**")),
                    subtitle = gt::md(paste0("*", .("Comprehensive statistics for numeric variables"), "*"))
                ) %>%
                gt::fmt_number(
                    columns = c("Mean", "SD", "Min", "Q25", "Median", "Q75", "Max"),
                    decimals = dp
                ) %>%
                gt::cols_label(
                    Variable = .("Variable"),
                    Type = .("Type"),
                    N = .("N"),
                    Missing = .("Missing"),
                    Mean = .("Mean"),
                    SD = .("SD"),
                    Min = .("Min"),
                    Q25 = .("Q25"),
                    Median = .("Median"),
                    Q75 = .("Q75"),
                    Max = .("Max")
                ) %>%
                gt::tab_style(
                    style = gt::cell_fill(color = "#f8f9fa"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::opt_stylize(style = 6, color = "blue") %>%
                gt::tab_options(
                    table.font.size = 12,
                    heading.title.font.size = 16,
                    heading.subtitle.font.size = 12
                )
            
            # Convert to HTML
            return(htmltools::HTML(as.character(gt::as_raw_html(gt_table))))
        },

        # Generate clinical interpretation for continuous variables
        .generateClinicalInterpretation = function(variables, dataset) {
            if (length(variables) == 0) return("")
            
            # Calculate comprehensive clinical metrics
            n_vars <- length(variables)
            total_obs <- nrow(dataset)
            
            # Assess data completeness
            completeness_info <- sapply(variables, function(var) {
                var_data <- dataset[[var]]
                round(sum(is.na(var_data)) / length(var_data) * 100, 1)
            })

            avg_missing <- round(mean(completeness_info), 1)
            
            # Generate clinical context
            clinical_text <- paste0(
                "<div style='padding: 15px; background-color: #e8f5e8; border-left: 4px solid #4caf50; margin: 10px 0; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #2e7d32;'>", .("Clinical Interpretation Guide"), "</h4>",
                "<p><strong>", .("Dataset Overview"), ":</strong> ", 
                paste0("Analysis of ", n_vars, " continuous variable(s) from ", total_obs, " patient records"), "</p>",
                
                "<p><strong>", .("Data Quality Assessment"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", paste0("Average missing data: ", avg_missing, "%"), "</li>",
                if (avg_missing > 20) paste0("<li style='color: #d32f2f;'>", .("A high missing-data rate may affect interpretation."), "</li>") else "",
                if (avg_missing <= 5) paste0("<li style='color: #388e3c;'>", .("Data completeness is excellent."), "</li>") else "",
                "</ul>",
                
                "<p><strong>", .("Clinical Applications"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Biomarker distribution assessment"), "</li>",
                "<li>", .("Reference range validation"), "</li>",
                "<li>", .("Quality control and outlier detection"), "</li>",
                "<li>", .("Statistical assumption verification"), "</li>",
                "</ul>",
                
                if (any(sapply(dataset[variables], function(x) any(is.infinite(x)))))
                    paste0("<p style='color: #d32f2f;'><strong>",
                           .("Data quality alert: Some variables contain infinite or extreme values that may require investigation."),
                           "</strong></p>") else "",
                
                "</div>"
            )
            
            return(clinical_text)
        },
        
        # Generate explanatory content about the analysis
        .generateAboutContent = function() {
            about_text <- paste0(
                "<div style='padding: 15px; background-color: #f3f4f6; border-left: 4px solid #6b7280; margin: 10px 0; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #374151;'>", .("About This Analysis"), "</h4>",
                
                "<p><strong>", .("What this analysis provides"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Descriptive statistics: mean, median, standard deviation, min/max"), "</li>",
                "<li>", .("Visual summary tables with distribution plots"), "</li>",
                "<li>", .("Optional normality testing and distribution diagnostics"), "</li>",
                "</ul>",
                
                "<p><strong>", .("When to use this analysis"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Initial data exploration and quality assessment"), "</li>",
                "<li>", .("Biomarker characterization studies"), "</li>",
                "<li>", .("Preparation for statistical modeling"), "</li>",
                "<li>", .("Laboratory reference range studies"), "</li>",
                "</ul>",
                
                "<p><strong>", .("Key considerations"), ":</strong></p>",
                "<ul style='margin: 5px 0 10px 20px;'>",
                "<li>", .("Enable 'Distribution Diagnostics' for normality assessment"), "</li>",
                "<li>", .("Consider data transformations if distributions are highly skewed"), "</li>",
                "<li>", .("Investigate outliers before proceeding with inferential statistics"), "</li>",
                "</ul>",
                
                "</div>"
            )
            
            return(about_text)
        },
        
        # Outlier detection using IQR method
        .detectOutliers = function(data, variables) {
            outlier_results <- list()
            
            for (var in variables) {
                var_data <- as.numeric(data[[var]])
                var_data_clean <- var_data[!is.na(var_data)]
                
                if (length(var_data_clean) < 4) {
                    outlier_results[[var]] <- list(outliers = integer(0), method = "insufficient_data")
                    next
                }
                
                # IQR method
                Q1 <- quantile(var_data_clean, 0.25, na.rm = TRUE)
                Q3 <- quantile(var_data_clean, 0.75, na.rm = TRUE)
                IQR_val <- Q3 - Q1
                
                lower_bound <- Q1 - 1.5 * IQR_val
                upper_bound <- Q3 + 1.5 * IQR_val
                
                outlier_indices <- which(var_data < lower_bound | var_data > upper_bound)
                outlier_values <- var_data[outlier_indices]
                
                outlier_results[[var]] <- list(
                    outliers = outlier_indices,
                    values = outlier_values,
                    lower_bound = round(lower_bound, 3),
                    upper_bound = round(upper_bound, 3),
                    method = "iqr"
                )
            }
            
            return(outlier_results)
        },
        
        # Generate outlier detection report
        .generateOutlierReport = function(variables, dataset) {
            if (length(variables) == 0) return("")
            
            outlier_results <- private$.detectOutliers(dataset, variables)
            
            report_html <- paste0(
                "<div style='padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; margin: 10px 0; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #856404;'>", .("Outlier Detection Results"), "</h4>",
                "<p>", .("Outliers detected using IQR method (values beyond Q1-1.5\u{D7}IQR or Q3+1.5\u{D7}IQR):"), "</p>"
            )
            
            for (var in variables) {
                result <- outlier_results[[var]]
                safe_var <- htmltools::htmlEscape(var)

                if (result$method == "insufficient_data") {
                    report_html <- paste0(report_html,
                        "<p><strong>", safe_var, ":</strong> ", .("Insufficient data for outlier detection"), "</p>")
                } else if (length(result$outliers) == 0) {
                    report_html <- paste0(report_html,
                        "<p><strong>", safe_var, ":</strong> ", .("No outliers detected"),
                        " (", .("Range"), ": ", result$lower_bound, " - ", result$upper_bound, ")</p>")
                } else {
                    report_html <- paste0(report_html,
                        "<p><strong>", safe_var, ":</strong> ", length(result$outliers), " ", .("outliers detected"), 
                        " (", .("Values"), ": ", paste(round(result$values, 2), collapse = ", "), ") ",
                        "<br><span style='color: #856404; font-size: 0.9em;'>",
                        .("Expected range"), ": ", result$lower_bound, " - ", result$upper_bound, "</span></p>")
                }
            }
            
            report_html <- paste0(report_html,
                "<p style='margin-top: 15px; padding-top: 10px; border-top: 1px solid #ffc107; font-size: 0.9em;'>",
                "<strong>", .("Clinical Note"), ":</strong> ", 
                .("Outliers may indicate data entry errors, measurement issues, or genuine extreme values. Review outliers in clinical context before exclusion."),
                "</p></div>"
            )
            
            return(report_html)
        },
        
        # Generate copy-ready report sentences
        .generateReportSentences = function(variables, dataset) {
            if (length(variables) == 0) return("")
            
            sentences <- c()
            
            for (var in variables) {
                var_data <- as.numeric(dataset[[var]])
                var_clean <- var_data[!is.na(var_data)]

                if (length(var_clean) == 0) next

                # Calculate statistics
                n <- length(var_clean)
                mean_val <- round(mean(var_clean), 2)
                sd_val <- round(sd(var_clean), 2)
                median_val <- round(median(var_clean), 2)
                min_val <- round(min(var_clean), 2)
                max_val <- round(max(var_clean), 2)

                # Basic descriptive sentence (escape var name; output is rendered as HTML)
                sentence <- paste0(
                    "For ", htmltools::htmlEscape(var), ", analysis of ", n, " observations showed mean ",
                    mean_val, " \u{B1} ", sd_val, " (median ", median_val, 
                    ", range ", min_val, "-", max_val, ")"
                )
                
                # Add distribution information if enabled
                if (self$options$distr && n >= 3 && n <= 5000) {
                    # Reuse the cached Shapiro-Wilk result from the diagnostics text
                    sw_test <- private$.shapiroResult(var_clean, key = var)

                    if (!is.null(sw_test)) {
                        if (sw_test$p.value > 0.05) {
                            sentence <- paste0(sentence, ". ", 
                                "Data showed normal distribution (Shapiro-Wilk p = ", 
                                round(sw_test$p.value, 3), ")")
                        } else {
                            sentence <- paste0(sentence, ". ", 
                                "Data showed non-normal distribution (Shapiro-Wilk p = ", 
                                round(sw_test$p.value, 3), ")")
                        }
                    }
                }
                
                sentences <- c(sentences, sentence)
            }
            
            report_html <- paste0(
                "<div style='padding: 15px; background-color: #e8f5e8; border-left: 4px solid #4caf50; margin: 10px 0; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #2e7d32;'>", .("Copy-Ready Clinical Summary"), "</h4>",
                "<div style='background-color: white; padding: 10px; border-radius: 3px; border: 1px solid #c8e6c9;'>",
                paste(sentences, collapse = "<br><br>"),
                "</div>",
                "<p style='margin-top: 10px; font-size: 0.9em; color: #2e7d32;'>",
                "<strong>", .("Usage"), ":</strong> ",
                .("Copy the text above for use in clinical reports, research manuscripts, or medical documentation."),
                "</p></div>"
            )
            
            return(report_html)
        },
        
        # Generate statistical terminology glossary
        .generateGlossary = function() {
            glossary_html <- paste0(
                "<div style='padding: 15px; background-color: #f3f4f6; border-left: 4px solid #6b7280; margin: 10px 0; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #374151;'>", .("Statistical Terminology"), "</h4>",
                
                "<div style='margin-bottom: 10px;'>",
                "<strong>", .("Mean"), ":</strong> ", .("Average value of all observations. Sensitive to outliers."), "<br>",
                "<strong>", .("Median"), ":</strong> ", .("Middle value when data is ordered. Less affected by outliers."), "<br>",
                "<strong>", .("Standard Deviation (SD)"), ":</strong> ", .("Measure of variability around the mean."), "<br>",
                "<strong>", .("Range"), ":</strong> ", .("Difference between maximum and minimum values."), "<br>",
                "</div>",
                
                if (self$options$distr) {
                    paste0(
                        "<div style='margin-bottom: 10px; padding-top: 10px; border-top: 1px solid #d1d5db;'>",
                        "<strong>", .("Distribution Diagnostics"), ":</strong><br>",
                        "<strong>", .("Shapiro-Wilk Test"), ":</strong> ", .("Tests if data follows normal distribution. p > 0.05 suggests normality."), "<br>",
                        "<strong>", .("Skewness"), ":</strong> ", .("Measures asymmetry. 0 = symmetric, >0 = right-skewed, <0 = left-skewed."), "<br>",
                        "<strong>", .("Kurtosis"), ":</strong> ", .("Measures tail heaviness. 3 = normal, >3 = heavy tails, <3 = light tails."), "<br>",
                        "</div>"
                    )
                } else "",
                
                if (self$options$outliers) {
                    paste0(
                        "<div style='margin-bottom: 10px; padding-top: 10px; border-top: 1px solid #d1d5db;'>",
                        "<strong>", .("Outlier Detection"), ":</strong><br>",
                        "<strong>", .("IQR Method"), ":</strong> ", .("Values beyond Q1-1.5\u{D7}IQR or Q3+1.5\u{D7}IQR are considered outliers."), "<br>",
                        "<strong>", .("Q1, Q3"), ":</strong> ", .("First and third quartiles (25th and 75th percentiles)."), "<br>",
                        "</div>"
                    )
                } else "",
                
                "<p style='margin-top: 15px; font-size: 0.9em; color: #6b7280;'>",
                .("These statistics help assess data distribution, identify unusual values, and guide appropriate statistical analyses."),
                "</p></div>"
            )
            
            return(glossary_html)
        }

        # NOTE: R code generation feature deferred to future release
        # See SUMMARYDATA_FIXES.md for details
        # Implementation can be restored from git history if needed
    ))
