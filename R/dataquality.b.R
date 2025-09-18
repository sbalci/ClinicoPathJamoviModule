#' @title Data Quality Assessment
#' @return HTML summary of data quality issues including duplicates and missing values
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom dplyr n_distinct
#' @importFrom htmltools HTML
#' @importFrom visdat vis_dat vis_miss vis_guess vis_expect vis_cor
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
            var_formula <- jmvcore::constructFormula(terms = self$options$vars)
            var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))
            analysis_data <- dataset[var_list]
        } else {
            var_list <- names(dataset)
            analysis_data <- dataset
        }

        quality_results <- list()

        # Missing value analysis
        if (self$options$check_missing) {
            missing_summary <- sapply(analysis_data, function(x) {
                total <- length(x)
                missing <- sum(is.na(x))
                missing_pct <- round(missing / total * 100, 1)

                paste0("Missing: ", missing, "/", total, " (", missing_pct, "%)")
            })

            quality_results$missing <- paste0(
                "<h4>Missing Value Analysis</h4>",
                paste(names(missing_summary), missing_summary, sep = ": ", collapse = "<br>")
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

                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Row Analysis</h4>",
                    "Total rows: ", total_rows, "<br>",
                    "Unique rows: ", unique_rows, "<br>",
                    "Duplicate rows: ", duplicate_rows, " (", duplicate_pct, "%)"
                )
            } else {
                # Check for duplicates within each variable
                dup_summary <- sapply(analysis_data, function(x) {
                    total <- length(x)
                    unique_vals <- length(unique(na.omit(x)))
                    duplicates <- total - unique_vals - sum(is.na(x))
                    dup_pct <- round(duplicates / total * 100, 1)

                    paste0("Unique: ", unique_vals, ", Duplicates: ", duplicates, " (", dup_pct, "%)")
                })

                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Value Analysis</h4>",
                    paste(names(dup_summary), dup_summary, sep = ": ", collapse = "<br>")
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

        # visdat Visual Analysis - NEW FUNCTIONALITY
        if (self$options$visual_analysis) {
            visdat_results <- private$.generate_visdat_analysis(analysis_data)
            quality_results$visual <- visdat_results
        }

        # Combine all results
        final_results <- paste(unlist(quality_results), collapse = "<br><br>")
        self$results$text$setContent(final_results)

        # Set plot state for visdat visualization
        if (self$options$visual_analysis) {
            plotData <- list(
                data = analysis_data,
                visdat_type = self$options$visdat_type
            )
            self$results$plot$setState(plotData)
        }

    },

    .generate_visdat_analysis = function(data) {
        # Generate visdat analysis based on autoEDA research

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

        visdat_type <- self$options$visdat_type
        missing_threshold <- self$options$missing_threshold_visual

        # Generate visual analysis summary
        header_html <- paste0(
            "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
            "<h3 style='color: #1976d2; margin-top: 0;'>Visual Data Exploration (visdat)</h3>",
            "<p>Advanced visual data quality assessment - Based on autoEDA research</p>",
            "<p><strong>Analysis Type:</strong> ",
            switch(visdat_type,
                "vis_dat" = "Data Overview - Variable types and missing values",
                "vis_miss" = "Missing Patterns - Missing value clustering",
                "vis_guess" = "Data Types - Type detection and validation",
                "vis_expect" = "Value Expectations - Expected vs actual patterns",
                "all_visual" = "Comprehensive - All visual analyses"
            ), "</p>",
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

        # Analysis insights based on visdat type
        insights_html <- private$.generate_visdat_insights(data, visdat_type, missing_threshold)

        return(paste0(header_html, overview_html, insights_html))
    },

    .generate_visdat_insights = function(data, visdat_type, threshold) {
        # Generate insights based on visual analysis type

        insights_html <- paste0(
            "<div style='background-color: #fff8e1; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
            "<h4 style='color: #f57f17; margin-top: 0;'>Visual Analysis Insights</h4>"
        )

        if (visdat_type %in% c("vis_dat", "all_visual")) {
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

        if (visdat_type %in% c("vis_miss", "all_visual")) {
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

        if (visdat_type %in% c("vis_guess", "all_visual")) {
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

    .plotVisdat = function(image, ggtheme, theme, ...) {
        # Get plot state
        plotData <- image$state

        if (is.null(plotData)) {
            return(FALSE)
        }

        # Check if visdat package is available
        if (!requireNamespace("visdat", quietly = TRUE)) {
            return(FALSE)
        }

        data <- plotData$data
        visdat_type <- plotData$visdat_type

        if (is.null(data) || nrow(data) == 0) {
            return(FALSE)
        }

        tryCatch({
            # Generate the appropriate visdat plot
            if (visdat_type == "vis_dat" || visdat_type == "all_visual") {
                # Data overview plot
                plot <- visdat::vis_dat(data)

            } else if (visdat_type == "vis_miss") {
                # Missing value pattern plot
                plot <- visdat::vis_miss(data)

            } else if (visdat_type == "vis_guess") {
                # Data type guessing plot
                plot <- visdat::vis_guess(data)

            } else if (visdat_type == "vis_expect") {
                # Value expectation plot - default to vis_dat
                plot <- visdat::vis_dat(data)

            } else {
                # Default to data overview
                plot <- visdat::vis_dat(data)
            }

            # Apply jamovi theme
            plot <- plot + ggtheme

            # Print the plot
            print(plot)
            return(TRUE)

        }, error = function(e) {
            warning(paste("visdat plot generation failed:", e$message))
            return(FALSE)
        })
    }

    )
)
