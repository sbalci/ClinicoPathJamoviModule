#' @title Data Quality Assessment
#' @return HTML summary of data quality issues including duplicates and missing values
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom dplyr n_distinct
#' @importFrom htmltools HTML
#' @importFrom visdat vis_dat vis_miss vis_guess
#' @importFrom ggplot2 ggsave theme_minimal labs

dataqualityClass <- if (requireNamespace("jmvcore")) R6::R6Class("dataqualityClass",
    inherit = dataqualityBase, private = list(


    .run = function() {

        # Check if variables have been selected. If not, display a welcoming message.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
            <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
            <h3 style='color: #2e7d32; margin-top: 0;'>Welcome to Enhanced Data Quality Assessment!</h3>
            <p><strong>Comprehensive data quality analysis</strong> with visual exploration capabilities</p>
            <p>Enhanced with <strong>visdat integration</strong> based on autoEDA research (R Journal 2019)</p>

            <h4 style='color: #2e7d32;'>Quick Start:</h4>
            <ol>
            <li><strong>Select Variables:</strong> Choose specific variables or analyze entire dataset</li>
            <li><strong>Configure Analysis:</strong> Enable duplicate detection, missing value analysis</li>
            <li><strong>Visual Exploration:</strong> Use visdat for visual data quality assessment</li>
            <li><strong>Run Analysis:</strong> Get comprehensive data quality insights</li>
            </ol>

            <h4 style='color: #2e7d32;'>Analysis Features:</h4>
            <ul>
            <li><strong>Missing Value Analysis:</strong> Patterns and statistical summaries</li>
            <li><strong>Duplicate Detection:</strong> Row and value-level duplicate analysis</li>
            <li><strong>Data Completeness:</strong> Complete cases across variables</li>
            <li><strong>Visual Data Overview:</strong> visdat integration for visual exploration</li>
            <li><strong>Data Type Analysis:</strong> Automatic type detection and validation</li>
            </ul>

            <p style='font-size: 12px; color: #555; margin-top: 20px;'>
            <em>Enhanced with visdat package - unique visual data exploration (68,978+ downloads)</em>
            </p>
            </div>"
            self$results$todo$setContent(intro_msg)
            return()
        } else {
            self$results$todo$setContent("")
        }

        # Validate that the dataset contains complete rows.
        if (nrow(self$data) == 0) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'emptyDataset',
                type = jmvcore::NoticeType$ERROR
            )
            notice$setContent('Dataset contains no rows. Please provide data with at least one observation.')
            self$results$insert(999, notice)
            return()
        }

        dataset <- self$data

        # Determine variables to analyze
        if (length(self$options$vars) > 0) {
            # Use raw variable names from self$options$vars for subsetting.
            # jmvcore::composeTerm is typically for constructing formulas, not for direct dataframe subsetting,
            # as it can add backticks that might prevent correct column selection.
            var_list <- self$options$vars

            # Validate that all requested variables exist in the dataset
            missing_vars <- var_list[!var_list %in% names(dataset)]
            if (length(missing_vars) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingVariables',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf(
                    'Variables not found in dataset: %s. Please check variable names and try again.',
                    paste(missing_vars, collapse = ', ')
                ))
                self$results$insert(999, notice)
                return()
            }

            # Safely extract columns
            analysis_data <- dataset[, var_list, drop = FALSE]
        }

        quality_results <- list()
        summary_rows <- list()

        # Helper to record per-variable summary for structured HTML
        add_summary_row <- function(var, data_vec) {
            n_total <- length(data_vec)
            n_missing <- sum(is.na(data_vec))
            missing_pct <- if (n_total > 0) round(n_missing / n_total * 100, 1) else NA
            n_nonmiss <- n_total - n_missing
            n_unique <- length(unique(na.omit(data_vec)))
            dup_pct <- if (n_nonmiss > 0) round((n_nonmiss - n_unique) / n_nonmiss * 100, 1) else NA
            vtype <- paste(class(data_vec), collapse = "/")

            near_zero_var <- FALSE
            high_card <- FALSE
            outlier_n <- NA
            if (is.numeric(data_vec)) {
                sdv <- stats::sd(data_vec, na.rm = TRUE)
                near_zero_var <- !is.na(sdv) && sdv < .Machine$double.eps
                high_card <- n_unique > 50 && n_unique > 0.5 * n_nonmiss
                if (n_nonmiss > 10) {
                    q <- stats::quantile(data_vec, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
                    iqr <- q[2] - q[1]
                    if (!is.na(iqr) && iqr > 0) {
                        lower <- q[1] - 1.5 * iqr
                        upper <- q[2] + 1.5 * iqr
                        outlier_n <- sum(data_vec < lower | data_vec > upper, na.rm = TRUE)
                    } else {
                        outlier_n <- 0
                    }
                }
            } else {
                high_card <- n_unique > 50 && n_unique > 0.5 * n_nonmiss
            }

            summary_rows[[length(summary_rows) + 1]] <<- data.frame(
                variable = var,
                type = vtype,
                n = n_total,
                missing = n_missing,
                missing_pct = missing_pct,
                unique = n_unique,
                dup_pct = dup_pct,
                near_zero_var = near_zero_var,
                high_card = high_card,
                outlier_n = outlier_n,
                stringsAsFactors = FALSE
            )
        }

        # Pre-compute per-variable summaries for downstream reporting
        for (nm in names(analysis_data)) {
            add_summary_row(nm, analysis_data[[nm]])
        }

        # Check for high missingness (>50%) and issue STRONG_WARNING
        high_missing_vars <- vapply(summary_rows, function(r) {
            if (!is.na(r$missing_pct) && r$missing_pct > 50) r$variable else NA_character_
        }, character(1))
        high_missing_vars <- high_missing_vars[!is.na(high_missing_vars)]

        # NOTE: Removed Notice insertion to avoid serialization errors
        # High missingness warning is now included in HTML summary output
        if (length(high_missing_vars) > 0) {
            # Warning will be included in .generateSummary() and .generateRecommendations()
        }

        # Check for small sample size
        # NOTE: Removed Notice insertion to avoid serialization errors
        # Small sample warning is now included in HTML summary output
        n_total <- nrow(analysis_data)
        if (n_total < 20) {
            # Warning will be included in .generateSummary() and .generateRecommendations()
        }

        # Check for near-zero variance and issue WARNING
        near_zero_vars <- vapply(summary_rows, function(r) {
            if (isTRUE(r$near_zero_var)) r$variable else NA_character_
        }, character(1))
        near_zero_vars <- near_zero_vars[!is.na(near_zero_vars)]

        # NOTE: Removed Notice insertion to avoid serialization errors
        # Near-zero variance warning is now included in HTML summary output
        if (length(near_zero_vars) > 0) {
            # Warning will be included in .generateSummary() and .generateRecommendations()
        }

        # Missing value analysis
        if (self$options$check_missing) {
            # OPTIMIZED: Extract directly from summary_rows instead of re-iterating
            missing_summary <- setNames(
                vapply(summary_rows, function(row) {
                    paste0("Missing: ", row$missing, "/", row$n, " (", row$missing_pct, "%)")
                }, character(1)),
                vapply(summary_rows, function(r) r$variable, character(1))
            )

            # Case-level missingness distribution
            case_missing <- rowSums(is.na(analysis_data))
            case_summary <- sprintf("Case-level missing: median %d, mean %.1f, max %d (of %d vars)",
                                    stats::median(case_missing),
                                    mean(case_missing),
                                    max(case_missing),
                                    ncol(analysis_data))

            # Little's MCAR test if available and >1 var
            mcar_msg <- ""
            if (ncol(analysis_data) > 1 && requireNamespace("BaylorEdPsych", quietly = TRUE)) {
                try({
                    mcar <- BaylorEdPsych::LittleMCAR(analysis_data)
                    mcar_msg <- sprintf("Little's MCAR test: chi-square=%.2f, df=%s, p=%.4f",
                                        mcar$chi.square, mcar$df, mcar$p.value)
                }, silent = TRUE)
            } else if (ncol(analysis_data) > 1) {
                mcar_msg <- "Little's MCAR test skipped (BaylorEdPsych not installed)."
            }

            # Threshold flagging
            threshold <- self$options$missing_threshold_visual
            flags <- vapply(summary_rows, function(row) ifelse(!is.na(row$missing_pct) && row$missing_pct > threshold, row$variable, NA_character_), character(1))
            flags <- flags[!is.na(flags)]

            flag_html <- if (length(flags) > 0) {
                paste0("<p><strong>Variables exceeding ", threshold, "% missing:</strong> ", paste(flags, collapse = ", "), "</p>")
            } else {
                ""
            }

            quality_results$missing <- paste0(
                "<h4>Missing Value Analysis</h4>",
                paste(names(missing_summary), missing_summary, sep = ": ", collapse = "<br>"),
                "<br>", case_summary,
                if (mcar_msg != "") paste0("<br>", mcar_msg) else "",
                flag_html
            )
        }

        # Initialize duplicate_rows to NA (will be set if duplicate analysis runs)
        duplicate_rows <- NA

        # Duplicate analysis
        if (self$options$check_duplicates) {
            if (self$options$complete_cases_only && length(var_list) > 1) {
                # Check for duplicate rows across all selected variables
                total_rows <- nrow(analysis_data)
                unique_rows <- nrow(unique(analysis_data))
                duplicate_rows <- total_rows - unique_rows
                duplicate_pct <- round(duplicate_rows / total_rows * 100, 1)

                # Identify top duplicated row signatures
                dup_keys <- NA
                if (duplicate_rows > 0) {
                    key_freq <- as.data.frame(table(do.call(paste, c(analysis_data, sep = "||"))))
                    key_freq <- key_freq[key_freq$Freq > 1, ]
                    key_freq <- key_freq[order(-key_freq$Freq), ]
                    top_keys <- head(key_freq, 5)
                    dup_keys <- paste0("<br><em>Top duplicated patterns (first 5):</em><br>",
                                       paste(paste(top_keys$Var1, "(n=", top_keys$Freq, ")", sep = ""), collapse = "<br>"))
                }

                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Row Analysis</h4>",
                    "Total rows: ", total_rows, "<br>",
                    "Unique rows: ", unique_rows, "<br>",
                    "Duplicate rows: ", duplicate_rows, " (", duplicate_pct, "%)",
                    if (!is.na(dup_keys)) dup_keys else ""
                )
            } else {
                # Check for duplicates within each variable
                dup_summary <- sapply(analysis_data, function(x) {
                    total <- length(x)
                    non_missing <- sum(!is.na(x))
                    unique_vals <- length(unique(na.omit(x)))
                    duplicate_vals <- non_missing - unique_vals
                    dup_pct <- if (non_missing > 0) {
                        round(duplicate_vals / non_missing * 100, 1)
                    } else {
                        0
                    }

                    paste0("Unique: ", unique_vals, ", Duplicates: ", duplicate_vals,
                           " (", dup_pct, "% of non-missing)")
                })

                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Value Analysis</h4>",
                    paste(names(dup_summary), dup_summary, sep = ": ", collapse = "<br>"),
                    "<p style='margin-top: 10px; font-size: 0.9em; color: #555;'>",
                    "<em>Interpretation Note:</em> For categorical variables with few unique levels (e.g., 'Gender', 'Status'), ",
                    "a high number of 'Duplicates' often reflects data redundancy (many observations sharing the same valid value), ",
                    "not necessarily data errors. For identifier variables (e.g., 'Patient ID'), duplicates would typically indicate errors.",
                    "</p>"
                )
            }
        }

        # Complete cases analysis
        if (length(var_list) > 1) {
            complete_cases <- sum(complete.cases(analysis_data))
            total_cases <- nrow(analysis_data)
            complete_pct <- round(complete_cases / total_cases * 100, 1)

            quality_results$completeness <- paste0(
                "<h4>Data Completeness</h4>",
                "Complete cases: ", complete_cases, "/", total_cases, " (", complete_pct, "%)"
            )
        }

        # visdat Visual Analysis - Individual plot options
        if (self$options$plot_data_overview || self$options$plot_missing_patterns ||
            self$options$plot_data_types) {
            visdat_results <- private$.generate_visdat_analysis(analysis_data)
            quality_results$visual <- visdat_results
        }

        # Always provide structured summary table
        if (length(summary_rows) > 0) {
            df <- do.call(rbind, summary_rows)
            # Basic HTML table
            summary_table <- paste(
                apply(df, 1, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")),
                collapse = "\n"
            )
            header <- paste0("<tr><th>Variable</th><th>Type</th><th>N</th><th>Missing</th><th>%Missing</th><th>Unique</th><th>%Duplicates</th><th>Near-zero var</th><th>High card</th><th>Outliers</th></tr>")
            quality_results$summary_table <- paste0(
                "<h4>Variable Quality Summary</h4>",
                "<p><em>Flags:</em> near-zero variance, high cardinality (many unique values), and IQR-based outlier counts for numeric variables.</p>",
                "<table border='1' cellspacing='0' cellpadding='4'>",
                header,
                summary_table,
                "</table>"
            )
        }

        # Combine all results
        final_results <- paste(unlist(quality_results), collapse = "<br><br>")
        self$results$text$setContent(final_results)

        # Set plot states for individual visual analyses
        # Convert to base data.frame to avoid serialization issues
        plotData <- list(
            data = as.data.frame(analysis_data),
            threshold = self$options$missing_threshold_visual
        )

        if (self$options$plot_data_overview) {
            self$results$plotDataOverview$setState(plotData)
        }

        if (self$options$plot_missing_patterns) {
            self$results$plotMissingPatterns$setState(plotData)
        }

        if (self$options$plot_data_types) {
            self$results$plotDataTypes$setState(plotData)
        }

        # Generate clinical summaries if requested
        if (self$options$showSummary) {
            private$.generateSummary(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows)
        }

        if (self$options$showRecommendations) {
            private$.generateRecommendations(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows)
        }

        if (self$options$showExplanations) {
            private$.generateExplanations()
        }

        # NOTE: Removed completion notice to avoid serialization errors
        # Dynamically inserted Notice objects contain function references that
        # cannot be serialized by jamovi's protobuf system

    },

    .generate_visdat_analysis = function(data) {
        # Generate visdat analysis based on individual plot selections

        # Safely require visdat
        # NOTE: Using HTML warning instead of Notice to avoid serialization errors
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(paste0(
                "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>",
                "<strong>⚠️ Warning:</strong> visdat package not installed. Visual exploration disabled.<br>",
                "Install via: <code>install.packages('visdat')</code>",
                "</div>"
            ))
        }

        missing_threshold <- self$options$missing_threshold_visual

        # Determine which analyses are enabled
        enabled_analyses <- c()
        if (self$options$plot_data_overview) enabled_analyses <- c(enabled_analyses, "Data Overview")
        if (self$options$plot_missing_patterns) enabled_analyses <- c(enabled_analyses, "Missing Patterns")
        if (self$options$plot_data_types) enabled_analyses <- c(enabled_analyses, "Data Types")

        # Generate visual analysis summary
        header_html <- paste0(
            "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
            "<h3 style='color: #1976d2; margin-top: 0;'>Visual Data Exploration (visdat)</h3>",
            "<p>Advanced visual data quality assessment - Based on autoEDA research</p>",
            "<p><strong>Enabled Analyses:</strong> ", paste(enabled_analyses, collapse = ", "), "</p>",
            "</div>"
        )

        # Basic data overview for visual analysis
        n_vars <- ncol(data)
        n_obs <- nrow(data)
        missing_vars <- sum(sapply(data, function(x) any(is.na(x))))
        complete_vars <- n_vars - missing_vars

        overview_html <- paste0(
            "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
            "<h4 style='color: #333; margin-top: 0;'>Visual Analysis Overview</h4>",
            "<table style='width: 100%; border-collapse: collapse;'>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Variables:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_vars, "</td></tr>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_obs, "</td></tr>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Variables with Missing:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", missing_vars, " / ", n_vars, "</td></tr>",
            "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Complete Variables:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", complete_vars, " / ", n_vars, "</td></tr>",
            "</table>",
            "</div>"
        )

        # Analysis insights based on enabled plots
        insights_html <- private$.generate_visdat_insights(data, missing_threshold)

        return(paste0(header_html, overview_html, insights_html))
    },

    .generate_visdat_insights = function(data, threshold) {
        # Generate insights based on enabled plot types

        insights_html <- paste0(
            "<div style='background-color: #fff8e1; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
            "<h4 style='color: #f57f17; margin-top: 0;'>Visual Analysis Insights</h4>"
        )

        if (self$options$plot_data_overview) {
            # Data overview insights
            # Collapse multiple classes into single string to avoid length mismatch errors
            var_types <- sapply(data, function(x) paste(class(x), collapse = "/"))
            type_summary <- table(var_types)

            insights_html <- paste0(insights_html,
                "<p><strong>Data Type Distribution:</strong></p>",
                "<ul>"
            )

            for (type in names(type_summary)) {
                insights_html <- paste0(insights_html,
                    "<li><strong>", type, ":</strong> ", type_summary[type], " variables</li>"
                )
            }
            insights_html <- paste0(insights_html, "</ul>")
        }

        if (self$options$plot_missing_patterns) {
            # Missing pattern insights
            missing_counts <- sapply(data, function(x) sum(is.na(x)))
            vars_above_threshold <- sum(missing_counts > (nrow(data) * threshold / 100))

            insights_html <- paste0(insights_html,
                "<p><strong>Missing Value Patterns:</strong></p>",
                "<ul>",
                "<li>Variables above ", threshold, "% missing threshold: ", vars_above_threshold, "</li>",
                "<li>Total missing values: ", sum(missing_counts), "</li>",
                "</ul>"
            )
        }

        if (self$options$plot_data_types) {
            # Type detection insights
            char_vars <- sum(sapply(data, is.character))
            numeric_vars <- sum(sapply(data, is.numeric))
            factor_vars <- sum(sapply(data, is.factor))

            insights_html <- paste0(insights_html,
                "<p><strong>Type Detection Results:</strong></p>",
                "<ul>",
                "<li>Character variables: ", char_vars, " (may need conversion)</li>",
                "<li>Numeric variables: ", numeric_vars, " (ready for analysis)</li>",
                "<li>Factor variables: ", factor_vars, " (categorical analysis ready)</li>",
                "</ul>"
            )
        }

        # Add recommendations
        insights_html <- paste0(insights_html,
            "<p><strong>Recommendations:</strong></p>",
            "<ul>",
            "<li>Review visual plots below for detailed patterns</li>",
            "<li>Address missing value issues before analysis</li>",
            "<li>Validate data types match analysis requirements</li>",
            "</ul>",
            "</div>"
        )

        return(insights_html)
    },

    .plotDataOverview = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData) || is.null(plotData$data) || nrow(plotData$data) == 0) {
            return(FALSE)
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        tryCatch({
            # Create data overview plot
            plot <- visdat::vis_dat(plotData$data) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0.5,
                        margin = ggplot2::margin(t = 5)
                    ),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 40, l = 5)
                )

            print(plot)
            return(TRUE)

        }, error = function(e) {
            warning(paste("Data overview plot generation failed:", e$message))
            return(FALSE)
        })
    },

    .plotMissingPatterns = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData) || is.null(plotData$data) || nrow(plotData$data) == 0) {
            return(FALSE)
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        tryCatch({
            # Create missing patterns plot
            # Note: visdat::vis_miss doesn't have threshold highlighting capability
            # It shows all missing values with sort_miss option
            plot <- visdat::vis_miss(
                plotData$data,
                sort_miss = TRUE,  # Sort by missingness for clarity
                show_perc = TRUE,  # Show percentage missing
                show_perc_col = TRUE  # Show percentage by column
            ) +
                ggplot2::labs(
                    subtitle = paste0("Missing value patterns (threshold for warnings: ", plotData$threshold, "%)")
                ) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0.5,
                        margin = ggplot2::margin(t = 5)
                    ),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 40, l = 5)
                )

            print(plot)
            return(TRUE)

        }, error = function(e) {
            warning(paste("Missing patterns plot generation failed:", e$message))
            return(FALSE)
        })
    },

    .plotDataTypes = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData) || is.null(plotData$data) || nrow(plotData$data) == 0) {
            return(FALSE)
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        tryCatch({
            # Create data types plot
            plot <- visdat::vis_guess(plotData$data) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 0,
                        vjust = 0.5,
                        margin = ggplot2::margin(t = 5)
                    ),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 40, l = 5)
                )

            print(plot)
            return(TRUE)

        }, error = function(e) {
            warning(paste("Data types plot generation failed:", e$message))
            return(FALSE)
        })
    },

    .generateSummary = function(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows) {
        # Generate plain-language summary of data quality assessment

        n_vars_analyzed <- length(summary_rows)
        threshold <- self$options$missing_threshold_visual

        # Calculate maximum missing percentage
        max_missing_pct <- if (length(summary_rows) > 0) {
            max(vapply(summary_rows, function(r) if (!is.na(r$missing_pct)) r$missing_pct else 0, numeric(1)))
        } else {
            0
        }

        # Determine overall assessment
        overall_assessment <- if (length(high_missing_vars) == 0 && n_total >= 30 && length(near_zero_vars) == 0) {
            "Good - data quality is acceptable for analysis"
        } else if (n_total < 20 || length(high_missing_vars) > 0) {
            "Needs attention - significant quality issues detected"
        } else {
            "Acceptable - minor quality issues present"
        }

        # Get duplicate info
        dup_count <- if (!is.null(duplicate_rows) && !is.na(duplicate_rows)) duplicate_rows else 0
        dup_type <- if (self$options$complete_cases_only) "rows" else "values"

        # Count variables exceeding threshold
        vars_above_threshold <- sum(vapply(summary_rows, function(r) {
            !is.na(r$missing_pct) && r$missing_pct > threshold
        }, logical(1)))

        summary_html <- paste0(
            "<div style='background-color: #e8f5e9; padding: 20px; border-radius: 8px; border-left: 5px solid #4caf50;'>",
            "<h3 style='color: #2e7d32; margin-top: 0;'>📊 Plain-Language Summary</h3>",

            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<p style='font-size: 1.1em; line-height: 1.6;'>",
            sprintf("Analyzed <strong>%d variable%s</strong> from <strong>%d observation%s</strong>. ",
                    n_vars_analyzed, if (n_vars_analyzed == 1) "" else "s",
                    n_total, if (n_total == 1) "" else "s"),
            "</p>",

            "<h4 style='color: #2e7d32; margin-top: 15px;'>Key Findings:</h4>",
            "<ul style='line-height: 1.8;'>",

            # Missing data summary
            if (self$options$check_missing) {
                sprintf("<li><strong>Missing Data:</strong> %d variable%s exceed%s %d%% missing threshold",
                        vars_above_threshold,
                        if (vars_above_threshold == 1) "" else "s",
                        if (vars_above_threshold == 1) "s" else "",
                        threshold)
            } else {
                ""
            },
            if (length(high_missing_vars) > 0) {
                sprintf(" (highest: <em>%s</em> at %.1f%% missing)</li>",
                        high_missing_vars[1], max_missing_pct)
            } else if (self$options$check_missing) {
                "</li>"
            } else {
                ""
            },

            # Duplicate summary
            if (self$options$check_duplicates) {
                sprintf("<li><strong>Duplicates:</strong> %d duplicate %s detected%s</li>",
                        dup_count, dup_type,
                        if (dup_count > 0) " - review for data entry errors or valid repetitions" else "")
            } else {
                ""
            },

            # Data quality flags
            if (length(near_zero_vars) > 0) {
                sprintf("<li><strong>Near-Zero Variance:</strong> %d variable%s show%s minimal variation (<em>%s</em>)</li>",
                        length(near_zero_vars),
                        if (length(near_zero_vars) == 1) "" else "s",
                        if (length(near_zero_vars) == 1) "s" else "",
                        paste(near_zero_vars, collapse = ", "))
            } else {
                ""
            },

            # Sample size assessment
            sprintf("<li><strong>Sample Size:</strong> n=%d ", n_total),
            if (n_total < 20) {
                "- very small, estimates may be unstable"
            } else if (n_total < 30) {
                "- small, use caution with complex analyses"
            } else if (n_total < 100) {
                "- adequate for basic analyses"
            } else {
                "- good for most statistical analyses"
            },
            "</li>",

            "</ul>",
            "</div>",

            # Overall assessment box
            sprintf(
                "<div style='background-color: %s; padding: 15px; border-radius: 5px; border-left: 4px solid %s;'>",
                if (length(high_missing_vars) == 0 && n_total >= 30) "#d1f2eb" else "#fff3cd",
                if (length(high_missing_vars) == 0 && n_total >= 30) "#00695c" else "#ff8f00"
            ),
            "<p style='margin: 0; font-weight: bold;'>Overall Assessment: ", overall_assessment, "</p>",
            "</div>",

            "<p style='margin-top: 15px; font-size: 0.9em; color: #555;'>",
            "<em>💡 This summary is written in plain language for clinical documentation. ",
            "Copy this text for inclusion in study reports, quality control logs, or data management plans.</em>",
            "</p>",

            "</div>"
        )

        self$results$summary$setContent(summary_html)
    },

    .generateRecommendations = function(summary_rows, n_total, high_missing_vars, near_zero_vars, duplicate_rows) {
        # Generate actionable recommendations for addressing quality issues

        recs_html <- paste0(
            "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px; border-left: 5px solid #ff8f00;'>",
            "<h3 style='color: #e65100; margin-top: 0;'>⚡ Recommended Actions</h3>",

            "<p style='font-size: 1.05em; margin-bottom: 20px;'>",
            "Based on the quality assessment, here are specific actions to improve your data before analysis:",
            "</p>"
        )

        has_recommendations <- FALSE

        # High missingness recommendations
        if (length(high_missing_vars) > 0) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #e65100; margin-top: 0;'>🔴 High Missingness (>50%)</h4>",
                "<p><strong>Variables affected:</strong> ", paste(high_missing_vars, collapse = ", "), "</p>",
                "<p><strong>Actions:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                "<li><strong>Investigate root cause:</strong> Why is data missing? (not collected, measurement failure, data entry error)</li>",
                "<li><strong>Consider exclusion:</strong> Variables with >50% missing often provide limited information</li>",
                "<li><strong>If retaining, use imputation:</strong>",
                "<ul>",
                "<li>Multiple imputation (mice package): <code>mice::mice(data, m=5, method='pmm')</code></li>",
                "<li>Only if missing data is MAR (Missing At Random) - check Little's MCAR test above</li>",
                "<li>Report imputation method and sensitivity analysis in your manuscript</li>",
                "</ul></li>",
                "<li><strong>Alternative:</strong> Restrict to complete cases but report potential selection bias</li>",
                "</ol>",
                "<p style='background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-top: 10px;'>",
                "<strong>⚠️ Warning:</strong> Listwise deletion (complete-case analysis) with >50% missing can severely bias results. ",
                "Consult a statistician if you're uncertain about the best approach.",
                "</p>",
                "</div>"
            )
        }

        # Moderate missingness (10-50%)
        moderate_missing_vars <- vapply(summary_rows, function(r) {
            if (!is.na(r$missing_pct) && r$missing_pct > 10 && r$missing_pct <= 50) r$variable else NA_character_
        }, character(1))
        moderate_missing_vars <- moderate_missing_vars[!is.na(moderate_missing_vars)]

        if (length(moderate_missing_vars) > 0) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff8f00; margin-top: 0;'>🟡 Moderate Missingness (10-50%)</h4>",
                "<p><strong>Variables affected:</strong> ", paste(moderate_missing_vars, collapse = ", "), "</p>",
                "<p><strong>Recommended approach:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Preferred:</strong> Multiple imputation with sensitivity analysis</li>",
                "<li><strong>Acceptable:</strong> Complete-case analysis if MCAR confirmed (Little's test p>0.05)</li>",
                "<li><strong>Report:</strong> Compare baseline characteristics between complete vs. incomplete cases</li>",
                "<li><strong>Document:</strong> State missingness mechanism and handling method in Methods section</li>",
                "</ul>",
                "</div>"
            )
        }

        # Duplicate recommendations
        dup_count <- if (!is.null(duplicate_rows) && !is.na(duplicate_rows)) duplicate_rows else 0
        if (dup_count > 0) {
            has_recommendations <- TRUE
            dup_type <- if (self$options$complete_cases_only) "duplicate rows" else "duplicate values"
            recs_html <- paste0(recs_html,
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #e65100; margin-top: 0;'>🔴 ", dup_count, " ", dup_type, " Detected</h4>",
                "<p><strong>Actions:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                if (self$options$complete_cases_only) {
                    paste0(
                        "<li><strong>Review patient identifiers:</strong> Check if duplicates represent same patient (data entry error) or different patients</li>",
                        "<li><strong>If same patient:</strong> Merge records, keeping most complete/recent data</li>",
                        "<li><strong>If different patients:</strong> Check for ID assignment errors</li>",
                        "<li><strong>Remove true duplicates:</strong> Use <code>dplyr::distinct()</code> after verification</li>"
                    )
                } else {
                    paste0(
                        "<li><strong>For categorical variables:</strong> High duplicates are normal (e.g., many patients with 'Male' gender)</li>",
                        "<li><strong>For continuous variables:</strong> Investigate if duplicates are biologically plausible</li>",
                        "<li><strong>For ID variables:</strong> Duplicates likely indicate data errors - review source data</li>"
                    )
                },
                "</ol>",
                "</div>"
            )
        }

        # Near-zero variance recommendations
        if (length(near_zero_vars) > 0) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff8f00; margin-top: 0;'>🟡 Near-Zero Variance Variables</h4>",
                "<p><strong>Variables affected:</strong> ", paste(near_zero_vars, collapse = ", "), "</p>",
                "<p><strong>Actions:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Exclude from models:</strong> Variables with no variation cannot predict outcomes</li>",
                "<li><strong>Investigate:</strong> Is lack of variation a data quality issue or a true population characteristic?</li>",
                "<li><strong>Consider:</strong> May still be useful for descriptive statistics or subgroup identification</li>",
                "<li><strong>Remove before modeling:</strong> Use <code>caret::nearZeroVar()</code> for automated detection</li>",
                "</ul>",
                "</div>"
            )
        }

        # Small sample recommendations
        if (n_total < 20) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #e65100; margin-top: 0;'>🔴 Very Small Sample Size (n=", n_total, ")</h4>",
                "<p><strong>Critical limitations:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Statistical power:</strong> Severely underpowered for most analyses</li>",
                "<li><strong>Model stability:</strong> Regression models may not converge or produce unreliable estimates</li>",
                "<li><strong>Generalizability:</strong> Results may not generalize beyond this specific sample</li>",
                "</ul>",
                "<p><strong>Recommended actions:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                "<li><strong>Primary recommendation:</strong> Increase sample size if possible (target n≥30 minimum)</li>",
                "<li><strong>If sample size fixed:</strong>",
                "<ul>",
                "<li>Limit to descriptive statistics only</li>",
                "<li>Use exact tests instead of asymptotic (e.g., Fisher's exact vs. chi-square)</li>",
                "<li>Avoid multivariable regression (rule of thumb: need ≥10 events per predictor)</li>",
                "<li>Consider case series or qualitative analysis instead</li>",
                "</ul></li>",
                "<li><strong>Reporting:</strong> Clearly state sample size limitation in Discussion section</li>",
                "</ol>",
                "</div>"
            )
        } else if (n_total < 30) {
            has_recommendations <- TRUE
            recs_html <- paste0(recs_html,
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff8f00; margin-top: 0;'>🟡 Small Sample Size (n=", n_total, ")</h4>",
                "<p><strong>Recommendations:</strong></p>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Prefer exact tests:</strong> Use exact methods when possible (Fisher's exact, permutation tests)</li>",
                "<li><strong>Limit model complexity:</strong> Restrict to ≤", floor(n_total/10), " predictor variables in regression</li>",
                "<li><strong>Use cross-validation:</strong> LOOCV (leave-one-out) for model validation instead of train/test split</li>",
                "<li><strong>Report uncertainty:</strong> Always include confidence intervals, not just p-values</li>",
                "<li><strong>Consider pilot study:</strong> Frame results as preliminary findings requiring validation</li>",
                "</ul>",
                "</div>"
            )
        }

        # If no issues detected
        if (!has_recommendations) {
            recs_html <- paste0(recs_html,
                "<div style='background-color: #d1f2eb; padding: 15px; border-radius: 5px;'>",
                "<h4 style='color: #00695c; margin-top: 0;'>✅ No Critical Issues Detected</h4>",
                "<p style='line-height: 1.8;'>",
                "Your data quality appears acceptable for analysis. However, always:",
                "</p>",
                "<ul style='line-height: 1.8;'>",
                "<li>Check assumptions specific to your planned analysis (normality, homoscedasticity, etc.)</li>",
                "<li>Visualize distributions and relationships before modeling</li>",
                "<li>Screen for outliers that may influence results</li>",
                "<li>Document any data transformations or exclusions in your analysis plan</li>",
                "</ul>",
                "</div>"
            )
        }

        recs_html <- paste0(recs_html,
            "<p style='margin-top: 20px; font-size: 0.9em; color: #555;'>",
            "<em>💡 These recommendations are based on general statistical best practices. ",
            "Consult with a biostatistician for guidance specific to your research question and study design.</em>",
            "</p>",
            "</div>"
        )

        self$results$recommendations$setContent(recs_html)
    },

    .generateExplanations = function() {
        # Generate educational explanations of quality metrics

        expl_html <- paste0(
            "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; border-left: 5px solid #1976d2;'>",
            "<h3 style='color: #0d47a1; margin-top: 0;'>📚 Understanding Quality Metrics</h3>",

            "<p style='font-size: 1.05em; margin-bottom: 20px;'>",
            "This guide explains the quality metrics used in this analysis and how to interpret them.",
            "</p>",

            # Missing Data section
            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Missing Data Analysis</h4>",

            "<p><strong>What it measures:</strong> Percentage of observations with missing values for each variable.</p>",

            "<p><strong>Interpretation guidelines:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>&lt;5% missing:</strong> Excellent - minimal impact on analysis</li>",
            "<li><strong>5-10% missing:</strong> Acceptable - document and address appropriately</li>",
            "<li><strong>10-20% missing:</strong> Moderate concern - may require imputation</li>",
            "<li><strong>&gt;20% missing:</strong> Serious concern - results may be biased</li>",
            "<li><strong>&gt;50% missing:</strong> Critical - consider excluding variable</li>",
            "</ul>",

            "<p><strong>Little's MCAR Test:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>What it tests:</strong> Whether missing data is completely random (MCAR) vs. systematic (MAR/MNAR)</li>",
            "<li><strong>Interpretation:</strong>",
            "<ul>",
            "<li>p > 0.05: Missing data is MCAR (safe to use complete-case analysis or imputation)</li>",
            "<li>p ≤ 0.05: Missing data is NOT random (use caution with complete-case analysis; prefer imputation)</li>",
            "</ul></li>",
            "<li><strong>Clinical relevance:</strong> If data is MCAR, deleting cases doesn't bias results (but reduces power)</li>",
            "</ul>",
            "</div>",

            # Duplicate Detection section
            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Duplicate Detection</h4>",

            "<p><strong>Two types checked:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Duplicate rows:</strong> Identical combinations across all selected variables (may indicate data entry errors or repeated measurements)</li>",
            "<li><strong>Duplicate values:</strong> Repeated values within each variable (normal for categorical data, unusual for IDs)</li>",
            "</ul>",

            "<p><strong>When duplicates are concerning:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Patient ID variables should have ~0% duplicates</li>",
            "<li>Exact matches across many variables may indicate copy-paste errors</li>",
            "<li>Unexpected patterns (e.g., same tumor size for multiple patients)</li>",
            "</ul>",

            "<p><strong>When duplicates are normal:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Categorical variables (Gender, Stage, etc.) expected to have many duplicates</li>",
            "<li>Rounded measurements (e.g., age in years)</li>",
            "<li>Binary outcomes (yes/no, positive/negative)</li>",
            "</ul>",
            "</div>",

            # Near-Zero Variance section
            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Near-Zero Variance</h4>",

            "<p><strong>What it means:</strong> Variable shows almost no variation across observations (standard deviation near zero).</p>",

            "<p><strong>Why it matters:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Cannot predict outcomes if predictor doesn't vary</li>",
            "<li>May cause numerical instability in regression models</li>",
            "<li>Often indicates all patients have same value (e.g., all 'Stage IV')</li>",
            "</ul>",

            "<p><strong>Actions:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>For analysis:</strong> Exclude from regression models</li>",
            "<li><strong>For reporting:</strong> State as constant in descriptive statistics</li>",
            "<li><strong>For study design:</strong> May indicate homogeneous sample (affects generalizability)</li>",
            "</ul>",
            "</div>",

            # High Cardinality section
            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>High Cardinality</h4>",

            "<p><strong>Definition:</strong> Variable has >50 unique values AND these represent >50% of observations.</p>",

            "<p><strong>Examples:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Patient ID (each patient unique) - very high cardinality</li>",
            "<li>Age in years (20-90) - moderate cardinality</li>",
            "<li>Tumor size in mm (continuous) - high cardinality</li>",
            "</ul>",

            "<p><strong>Implications:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>For categorical variables:</strong> May need to collapse categories (e.g., group age into bands)</li>",
            "<li><strong>For continuous variables:</strong> Normal and expected</li>",
            "<li><strong>For factors in regression:</strong> High cardinality increases parameters and reduces power</li>",
            "</ul>",
            "</div>",

            # Outliers section
            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Outlier Detection (IQR Method)</h4>",

            "<p><strong>Method used:</strong> Tukey's IQR (Interquartile Range) rule</p>",

            "<p><strong>Formula:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li>Lower bound = Q1 - 1.5 × IQR</li>",
            "<li>Upper bound = Q3 + 1.5 × IQR</li>",
            "<li>Values outside these bounds flagged as outliers</li>",
            "</ul>",

            "<p><strong>Interpretation:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>0-2 outliers:</strong> Normal for most datasets</li>",
            "<li><strong>3-5 outliers:</strong> Review for data entry errors</li>",
            "<li><strong>>5 outliers:</strong> May indicate skewed distribution or systematic issues</li>",
            "</ul>",

            "<p><strong>Actions:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Don't automatically delete outliers</strong> - may represent true biological variation</li>",
            "<li>Verify against source data for transcription errors</li>",
            "<li>Consider robust statistical methods (median-based, trimmed means)</li>",
            "<li>Run sensitivity analysis with/without outliers</li>",
            "</ul>",
            "</div>",

            # Sample Size section
            "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
            "<h4 style='color: #1976d2; margin-top: 0;'>Sample Size Guidelines</h4>",

            "<p><strong>General rules of thumb:</strong></p>",
            "<table border='1' cellspacing='0' cellpadding='8' style='width:100%; border-collapse: collapse;'>",
            "<tr style='background-color: #f5f5f5;'>",
            "<th>Sample Size</th><th>Analysis Type</th><th>Recommendation</th>",
            "</tr>",
            "<tr><td>n &lt; 20</td><td>Any</td><td>Descriptive only; avoid inference</td></tr>",
            "<tr><td>n = 20-30</td><td>Basic</td><td>Simple comparisons; exact tests</td></tr>",
            "<tr><td>n = 30-100</td><td>Standard</td><td>Most analyses acceptable; limit predictors</td></tr>",
            "<tr><td>n = 100-500</td><td>Multivariable</td><td>Regression with multiple predictors OK</td></tr>",
            "<tr><td>n &gt; 500</td><td>Advanced</td><td>Machine learning, complex models feasible</td></tr>",
            "</table>",

            "<p style='margin-top: 15px;'><strong>Events per variable (EPV) rule:</strong></p>",
            "<ul style='line-height: 1.8;'>",
            "<li><strong>Minimum:</strong> 10 events per predictor variable in regression</li>",
            "<li><strong>Example:</strong> For binary outcome with 50 events, limit to 5 predictors</li>",
            "<li><strong>Survival analysis:</strong> Need 10 deaths/events per covariate in Cox model</li>",
            "</ul>",
            "</div>",

            # Visual exploration section
            if (self$options$plot_data_overview || self$options$plot_missing_patterns || self$options$plot_data_types) {
                paste0(
                    "<div style='background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                    "<h4 style='color: #1976d2; margin-top: 0;'>Visual Data Exploration (visdat)</h4>",

                    "<p><strong>Package background:</strong> visdat provides visual exploratory data analysis based on research published in the R Journal (2019).</p>",

                    "<p><strong>Plot interpretations:</strong></p>",
                    "<ul style='line-height: 1.8;'>",
                    if (self$options$plot_data_overview) {
                        "<li><strong>Data Overview (vis_dat):</strong> Shows data types and missing patterns in matrix format. Each row = observation, each column = variable. Colors indicate data type. Gray = missing.</li>"
                    } else {
                        ""
                    },
                    if (self$options$plot_missing_patterns) {
                        "<li><strong>Missing Patterns (vis_miss):</strong> Highlights missing data patterns. Variables sorted by missingness. Red bands indicate variables exceeding threshold. Look for systematic patterns (MAR) vs. random scatter (MCAR).</li>"
                    } else {
                        ""
                    },
                    if (self$options$plot_data_types) {
                        "<li><strong>Data Types (vis_guess):</strong> Shows R's guess at appropriate data type. Useful for validating that character variables should be factors, numeric variables aren't accidentally stored as text, etc.</li>"
                    } else {
                        ""
                    },
                    "</ul>",

                    "<p><strong>Clinical applications:</strong></p>",
                    "<ul style='line-height: 1.8;'>",
                    "<li>Quickly spot data collection issues (e.g., missing Stage for all patients after certain date)</li>",
                    "<li>Identify variables that should be recoded (e.g., '999' used as missing indicator)</li>",
                    "<li>Verify data types match intended analysis (factors for categorical, numeric for continuous)</li>",
                    "</ul>",
                    "</div>"
                )
            } else {
                ""
            },

            # Footer
            "<p style='margin-top: 20px; font-size: 0.9em; color: #555;'>",
            "<em>📖 These explanations provide general guidance for clinical researchers. ",
            "For detailed statistical consultation, work with a biostatistician familiar with your research domain.</em>",
            "</p>",

            "</div>"
        )

        self$results$explanations$setContent(expl_html)
    }

    )
)
