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
            stop("Error: The provided dataset contains no rows. Please check your data and try again.")
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
                stop(paste0(
                    "Error: The following variables do not exist in the dataset: ",
                    paste(missing_vars, collapse = ", "),
                    ". Please check variable names and try again."
                ))
            }

            # Safely extract columns
            analysis_data <- dataset[, var_list, drop = FALSE]
        } else {
            # This block is reached if self$options$vars is empty,
            # which means the welcome message (and return) logic above was executed.
            # So, this else-branch should ideally not be reached during normal execution path.
            # Keeping it as a fallback, but the early return handles the empty vars case.
            # To be clear: if vars is empty, it returns early. If not empty, it uses the if block.
            # The 'else' here (original code's 'else') is for a theoretical 'else' to `length(self$options$vars) > 0`.
            # Reverting the `var_list <- names(dataset)` behavior here, as per user's request to return early if no variables.
            # If the code reaches here, it means vars > 0.
            # We already defined var_list in the `if` branch, so this `else` is dead.
            stop("Internal error: Should not reach this point with empty vars. Check logic.")
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

        # Missing value analysis
        if (self$options$check_missing) {
            missing_summary <- vapply(names(analysis_data), function(nm) {
                row <- summary_rows[[which(vapply(summary_rows, function(r) r$variable, character(1)) == nm)]]
                paste0("Missing: ", row$missing, "/", row$n, " (", row$missing_pct, "%)")
            }, character(1))

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
        plotData <- list(
            data = analysis_data,
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

    },

    .generate_visdat_analysis = function(data) {
        # Generate visdat analysis based on individual plot selections

        # Safely require visdat
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(paste0(
                "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                "<h4>visdat Package Required</h4>",
                "<p>The visdat package is required for visual data exploration.</p>",
                "<p>Please install it using: <code>install.packages('visdat')</code></p>",
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
            var_types <- sapply(data, class)
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
            # Create missing patterns plot with threshold highlighting
            threshold_decimal <- plotData$threshold / 100  # Convert percentage to decimal
            plot <- visdat::vis_miss(
                plotData$data,
                warn_recode = threshold_decimal,  # Highlight vars above threshold
                sort_miss = TRUE  # Sort by missingness for clarity
            ) +
                ggplot2::labs(
                    subtitle = paste0("Variables with >", plotData$threshold, "% missing highlighted")
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

    .plotValueExpectations = function(image, ggtheme, theme, ...) {
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
            # Create value expectations plot
            # NOTE: vis_expect requires expectation formula specification
            # Using vis_dat as visual overview until expectations are configurable
            plot <- visdat::vis_dat(plotData$data) +
                ggplot2::labs(
                    title = "Value Expectations Analysis",
                    subtitle = "Visual overview of data types and missingness"
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
            warning(paste("Value expectations plot generation failed:", e$message))
            return(FALSE)
        })
    }

    )
)
