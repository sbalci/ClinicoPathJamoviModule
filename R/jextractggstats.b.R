
#' @title Statistical Data Extraction from ggstatsplot
#' @importFrom jmvcore .
#' @export

jextractggstatsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jextractggstatsClass",
    inherit = jextractggstatsBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$dep_var)) {
                self$results$instructions$setVisible(visible = TRUE)
                self$results$extracted_data$setVisible(visible = FALSE)
                self$results$statistical_summary$setVisible(visible = FALSE)
                self$results$interpretation$setVisible(visible = FALSE)
                return()
            }
            
            private$.checkData()
            
            self$results$instructions$setVisible(visible = FALSE)
            self$results$extracted_data$setVisible(visible = TRUE)
            self$results$statistical_summary$setVisible(visible = self$options$detailed_results)
            self$results$interpretation$setVisible(visible = self$options$show_interpretation)
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$dep_var)) {
                return()
            }
            
            # Check if ggstatsplot is available
            if (!requireNamespace("ggstatsplot", quietly = TRUE)) {
                stop("The ggstatsplot package is required but not installed.")
            }
            
            # Prepare data
            data <- private$.prepareData()
            
            # Create ggstatsplot analysis
            ggstats_result <- private$.createGgstatsplotAnalysis(data)
            
            # Extract statistical components
            extracted_data <- private$.extractStatisticalComponents(ggstats_result, data)
            
            # Format and display results
            private$.displayResults(extracted_data)
            
            # Generate interpretation
            if (self$options$show_interpretation) {
                interpretation <- private$.generateInterpretation(extracted_data, data)
                self$results$interpretation$setContent(interpretation)
            }
        },
        
        .prepareData = function() {
            data <- as.data.frame(self$data)  # Convert tibble/spec_tbl_df to data.frame
            dep_var <- self$options$dep_var
            group_var <- self$options$group_var
            
            # Create working data frame
            if (!is.null(group_var)) {
                plot_data <- data[, c(dep_var, group_var), drop = FALSE]
                names(plot_data) <- c("y", "x")
            } else {
                # Ensure we get a data frame even for single column
                plot_data <- data.frame(y = data[[dep_var]])
            }
            
            # Remove missing values
            plot_data <- plot_data[complete.cases(plot_data), , drop = FALSE]
            
            # Convert grouping variable to factor if needed
            if (!is.null(group_var) && !is.factor(plot_data$x)) {
                plot_data$x <- as.factor(plot_data$x)
            }
            
            return(plot_data)
        },
        
        .createGgstatsplotAnalysis = function(data) {
            analysis_type <- self$options$analysis_type
            statistical_test <- self$options$statistical_test
            conf_level <- self$options$conf_level
            
            # Set test type
            type <- switch(statistical_test,
                "parametric" = "parametric",
                "nonparametric" = "nonparametric", 
                "robust" = "robust",
                "bayes" = "bayes"
            )
            
            # Create analysis based on type
            result <- tryCatch({
                if (analysis_type == "between_stats" && "x" %in% names(data)) {
                    ggstatsplot::ggbetweenstats(
                        data = data,
                        x = "x",
                        y = "y",
                        type = type,
                        conf.level = conf_level,
                        pairwise.comparisons = self$options$pairwise_comparisons,
                        p.adjust.method = self$options$pairwise_correction,
                        effsize.type = private$.getEffectSizeType(),
                        bf.prior = self$options$bf_prior,
                        centrality.plotting = self$options$centrality_plotting,
                        outlier.tagging = self$options$outlier_tagging,
                        plotgrid.args = list(title = "Statistical Analysis"),
                        return.type = "plot"
                    )
                } else if (analysis_type == "within_stats" && "x" %in% names(data)) {
                    # For within-subjects analysis, assume paired data
                    ggstatsplot::ggwithinstats(
                        data = data,
                        x = "x",
                        y = "y",
                        type = type,
                        conf.level = conf_level,
                        pairwise.comparisons = self$options$pairwise_comparisons,
                        p.adjust.method = self$options$pairwise_correction,
                        effsize.type = private$.getEffectSizeType(),
                        bf.prior = self$options$bf_prior,
                        centrality.plotting = self$options$centrality_plotting,
                        return.type = "plot"
                    )
                } else if (analysis_type == "histogram") {
                    ggstatsplot::gghistostats(
                        data = data,
                        x = !!rlang::sym("y"),
                        type = type,
                        conf.level = conf_level,
                        bf.prior = self$options$bf_prior,
                        centrality.plotting = self$options$centrality_plotting,
                        return.type = "plot"
                    )
                } else if (analysis_type == "correlation" && "x" %in% names(data)) {
                    ggstatsplot::ggscatterstats(
                        data = data,
                        x = "x",
                        y = "y",
                        type = type,
                        conf.level = conf_level,
                        bf.prior = self$options$bf_prior,
                        return.type = "plot"
                    )
                } else if (analysis_type == "scatterplot" && "x" %in% names(data)) {
                    # Same as correlation but emphasizing scatterplot visualization
                    ggstatsplot::ggscatterstats(
                        data = data,
                        x = "x",
                        y = "y",
                        type = type,
                        conf.level = conf_level,
                        bf.prior = self$options$bf_prior,
                        marginal = TRUE,  # Add marginal plots
                        return.type = "plot"
                    )
                } else if (analysis_type == "bar_chart" && "x" %in% names(data)) {
                    # For categorical data analysis
                    ggstatsplot::ggbarstats(
                        data = data,
                        x = "x",
                        y = "y",
                        type = type,
                        conf.level = conf_level,
                        bf.prior = self$options$bf_prior,
                        return.type = "plot"
                    )
                } else if (analysis_type == "contingency_stats" && "x" %in% names(data)) {
                    # For contingency table analysis (two categorical variables)
                    ggstatsplot::ggbarstats(
                        data = data,
                        x = "x",
                        y = "y",
                        type = type,
                        conf.level = conf_level,
                        pairwise.comparisons = self$options$pairwise_comparisons,
                        p.adjust.method = self$options$pairwise_correction,
                        bf.prior = self$options$bf_prior,
                        return.type = "plot"
                    )
                } else if (analysis_type == "one_sample_stats") {
                    # For one-sample test against a reference value
                    test_value <- self$options$test_value
                    ggstatsplot::gghistostats(
                        data = data,
                        x = "y",
                        test.value = test_value,
                        type = type,
                        conf.level = conf_level,
                        bf.prior = self$options$bf_prior,
                        centrality.plotting = self$options$centrality_plotting,
                        return.type = "plot"
                    )
                } else {
                    # Default to histogram for single variable
                    ggstatsplot::gghistostats(
                        data = data,
                        x = "y",
                        type = type,
                        conf.level = conf_level,
                        centrality.plotting = self$options$centrality_plotting,
                        return.type = "plot"
                    )
                }
            }, error = function(e) {
                stop(paste("Error creating ggstatsplot analysis:", e$message))
            })
            
            return(result)
        },
        
        .extractStatisticalComponents = function(ggstats_result, data) {
            extract_components <- self$options$extract_components
            
            extracted <- list()
            
            tryCatch({
                # Extract subtitle data (main statistical results)
                if (extract_components %in% c("all", "subtitle_data")) {
                    subtitle_data <- ggstatsplot::extract_stats(ggstats_result, type = "subtitle")
                    if (!is.null(subtitle_data)) {
                        extracted$subtitle <- subtitle_data
                    }
                }
                
                # Extract caption data (additional info)
                if (extract_components %in% c("all", "caption_data")) {
                    caption_data <- ggstatsplot::extract_stats(ggstats_result, type = "caption")
                    if (!is.null(caption_data)) {
                        extracted$caption <- caption_data
                    }
                }
                
                # Extract pairwise comparisons
                if (extract_components %in% c("all", "pairwise_data") && self$options$pairwise_comparisons) {
                    pairwise_data <- ggstatsplot::extract_stats(ggstats_result, type = "pairwise_comparisons")
                    if (!is.null(pairwise_data)) {
                        extracted$pairwise <- pairwise_data
                    }
                }
                
                # Extract descriptive statistics
                if (extract_components %in% c("all", "descriptive_data")) {
                    descriptive_data <- ggstatsplot::extract_stats(ggstats_result, type = "descriptive")
                    if (!is.null(descriptive_data)) {
                        extracted$descriptive <- descriptive_data
                    }
                }
                
                # Include plot data if requested
                if (self$options$include_plot_data) {
                    extracted$plot_data <- data
                }
                
            }, error = function(e) {
                warning(paste("Error extracting components:", e$message))
            })
            
            return(extracted)
        },
        
        .displayResults = function(extracted_data) {
            output_format <- self$options$output_format
            
            # Create comprehensive HTML output
            html <- private$.createHtmlOutput(extracted_data)
            self$results$extracted_data$setContent(html)
            
            # Generate detailed statistical summary if requested
            if (self$options$detailed_results) {
                summary_html <- private$.createStatisticalSummary(extracted_data)
                self$results$statistical_summary$setContent(summary_html)
            }
        },
        
        .createHtmlOutput = function(extracted_data) {
            html <- "<h3>Extracted Statistical Data from ggstatsplot</h3>"
            
            # Display subtitle data (main results)
            if (!is.null(extracted_data$subtitle)) {
                html <- paste0(html, "<h4>Main Statistical Results</h4>")
                html <- paste0(html, private$.formatDataFrame(extracted_data$subtitle, "Subtitle Data"))
            }
            
            # Display caption data
            if (!is.null(extracted_data$caption)) {
                html <- paste0(html, "<h4>Additional Statistical Information</h4>")
                html <- paste0(html, private$.formatDataFrame(extracted_data$caption, "Caption Data"))
            }
            
            # Display pairwise comparisons
            if (!is.null(extracted_data$pairwise)) {
                html <- paste0(html, "<h4>Pairwise Comparisons</h4>")
                html <- paste0(html, private$.formatDataFrame(extracted_data$pairwise, "Pairwise Data"))
            }
            
            # Display descriptive statistics
            if (!is.null(extracted_data$descriptive)) {
                html <- paste0(html, "<h4>Descriptive Statistics</h4>")
                html <- paste0(html, private$.formatDataFrame(extracted_data$descriptive, "Descriptive Data"))
            }
            
            return(html)
        },
        
        .createStatisticalSummary = function(extracted_data) {
            html <- "<h3>Detailed Statistical Summary</h3>"
            
            # Count extracted components
            n_components <- length(extracted_data)
            html <- paste0(html, "<p><strong>Number of extracted components:</strong> ", n_components, "</p>")
            
            # List component types
            component_names <- names(extracted_data)
            if (length(component_names) > 0) {
                html <- paste0(html, "<p><strong>Available components:</strong> ", paste(component_names, collapse = ", "), "</p>")
            }
            
            # Analysis summary
            html <- paste0(html, "<h4>Analysis Configuration</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Analysis Type:</strong> ", self$options$analysis_type, "</li>")
            html <- paste0(html, "<li><strong>Statistical Test:</strong> ", self$options$statistical_test, "</li>")
            html <- paste0(html, "<li><strong>Effect Size Type:</strong> ", self$options$effect_size_type, "</li>")
            html <- paste0(html, "<li><strong>Confidence Level:</strong> ", self$options$conf_level, "</li>")
            if (self$options$pairwise_comparisons) {
                html <- paste0(html, "<li><strong>Pairwise Correction:</strong> ", self$options$pairwise_correction, "</li>")
            }
            html <- paste0(html, "</ul>")
            
            return(html)
        },
        
        .formatDataFrame = function(data, title) {
            if (is.null(data) || nrow(data) == 0) {
                return(paste0("<p><em>No ", title, " available</em></p>"))
            }
            
            html <- paste0("<table class='table table-striped'>")
            html <- paste0(html, "<thead><tr>")
            
            # Add column headers
            for (col_name in names(data)) {
                html <- paste0(html, "<th>", col_name, "</th>")
            }
            html <- paste0(html, "</tr></thead><tbody>")
            
            # Add data rows
            for (i in seq_len(nrow(data))) {
                html <- paste0(html, "<tr>")
                for (j in seq_len(ncol(data))) {
                    value <- data[i, j]
                    if (is.numeric(value)) {
                        value <- round(value, 4)
                    }
                    html <- paste0(html, "<td>", value, "</td>")
                }
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</tbody></table>")
            return(html)
        },
        
        .getEffectSizeType = function() {
            effect_type <- self$options$effect_size_type
            
            switch(effect_type,
                "eta" = "eta",
                "omega" = "omega",
                "cohens_d" = "d",
                "hedges_g" = "g",
                "cramers_v" = "cramers_v",
                "phi" = "phi",
                "eta" # default
            )
        },
        
        .generateInterpretation = function(extracted_data, data) {
            dep_var <- self$options$dep_var
            group_var <- self$options$group_var
            analysis_type <- self$options$analysis_type
            
            # Basic information
            n_obs <- nrow(data)
            n_components <- length(extracted_data)
            
            interpretation <- paste0(
                "<h3>Statistical Data Extraction Interpretation</h3>",
                "<p><strong>Analysis Overview:</strong> Successfully extracted statistical details from ggstatsplot analysis ",
                "of <em>", dep_var, "</em>"
            )
            
            if (!is.null(group_var)) {
                interpretation <- paste0(interpretation, " grouped by <em>", group_var, "</em>")
            }
            
            interpretation <- paste0(interpretation, " with ", n_obs, " observations.</p>")
            
            # Component summary
            interpretation <- paste0(interpretation,
                "<p><strong>Extracted Components:</strong> ", n_components, " statistical components were extracted, ",
                "including detailed results that can be used for custom reporting or further analysis.</p>"
            )
            
            # Analysis type information
            interpretation <- paste0(interpretation,
                "<p><strong>Analysis Type:</strong> ", 
                switch(analysis_type,
                    "between_stats" = "Between-groups comparison analysis suitable for comparing means or distributions across groups.",
                    "within_stats" = "Within-subjects comparison analysis for paired or repeated measures data.",
                    "histogram" = "Single variable distribution analysis with statistical tests for normality and central tendency.",
                    "correlation" = "Correlation analysis examining the relationship between two continuous variables.",
                    "scatterplot" = "Scatterplot visualization with marginal distributions and correlation analysis.",
                    "bar_chart" = "Categorical data analysis with chi-square tests and proportion comparisons.",
                    "contingency_stats" = "Contingency table analysis examining associations between two categorical variables using chi-square tests.",
                    "one_sample_stats" = "One-sample test comparing a variable against a reference value or theoretical mean.",
                    "General statistical analysis"
                ), "</p>"
            )
            
            # Usage information
            interpretation <- paste0(interpretation,
                "<p><strong>Applications:</strong> The extracted data can be used for custom reporting, ",
                "meta-analysis, or integration with other statistical workflows. All statistical details ",
                "computed by ggstatsplot are preserved and accessible in structured format.</p>"
            )
            
            return(interpretation)
        },
        
        .checkData = function() {
            dep_var <- self$options$dep_var
            group_var <- self$options$group_var
            
            if (!is.null(dep_var)) {
                dep_data <- self$data[[dep_var]]
                if (!is.numeric(dep_data)) {
                    stop("Dependent variable must be numeric for statistical analysis")
                }
                
                if (all(is.na(dep_data))) {
                    stop("Dependent variable contains only missing values")
                }
            }
            
            if (!is.null(group_var)) {
                group_data <- self$data[[group_var]]
                if (length(unique(group_data[!is.na(group_data)])) < 2) {
                    stop("Grouping variable must have at least 2 levels for between-groups analysis")
                }
            }
        }
    )
)
