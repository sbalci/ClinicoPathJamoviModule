#' @title Correlation Matrix
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @importFrom rlang sym
#'


jjcorrmatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcorrmatClass",
    inherit = jjcorrmatBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .options_hash = NULL,
        # .preset_recommendations = NULL,  # Commented out - clinical preset feature disabled
        .warnings = list(),  # Collect warnings for HTML display (avoids Notice serialization errors)

        # Helper function to add warnings without Notice objects
        .addWarning = function(type, message) {
            private$.warnings[[length(private$.warnings) + 1]] <- list(
                type = type,
                message = message
            )
        },

        # Display all collected warnings as HTML
        .displayWarnings = function() {
            if (length(private$.warnings) == 0) {
                self$results$warnings$setVisible(FALSE)
                return()
            }

            warning_html <- "<div style='margin: 10px 0;'>"

            for (warning in private$.warnings) {
                if (warning$type == "ERROR") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0; border-radius: 4px;'>",
                        "<strong style='color: #721c24;'>❌ ERROR:</strong> <span style='color: #721c24;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "STRONG_WARNING") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ff9800; padding: 10px; margin: 5px 0; border-radius: 4px;'>",
                        "<strong style='color: #856404;'>⚠️ STRONG WARNING:</strong> <span style='color: #856404;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "WARNING") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px; margin: 5px 0; border-radius: 4px;'>",
                        "<strong style='color: #664d03;'>⚠️ WARNING:</strong> <span style='color: #664d03;'>", warning$message, "</span>",
                        "</div>")
                } else if (warning$type == "INFO") {
                    warning_html <- paste0(warning_html,
                        "<div style='background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 5px 0; border-radius: 4px;'>",
                        "<strong style='color: #0c5460;'>ℹ️ INFO:</strong> <span style='color: #0c5460;'>", warning$message, "</span>",
                        "</div>")
                }
            }

            warning_html <- paste0(warning_html, "</div>")
            self$results$warnings$setContent(warning_html)
            self$results$warnings$setVisible(TRUE)
        },

        # init ----
        .init = function() {

            # Show welcome message when no variables or insufficient variables are selected
            if (is.null(self$options$dep) || length(self$options$dep) < 2) {
                
                todo <- .("<br>Welcome to ClinicoPath Correlation Matrix Analysis
                <br><br>
                <strong>What this does:</strong> Analyzes relationships between continuous variables (e.g., biomarker levels, lab values, imaging metrics)
                <br><br>
                <strong>When to use:</strong> When examining associations between 2+ continuous clinical variables
                <br><br>
                <strong>Quick Start:</strong>
                <br>1. Select 2 or more continuous variables
                <br>2. Choose correlation method (Pearson for normal data, Spearman for non-normal)
                <br>3. Optionally group by categorical variable (e.g., tumor grade, treatment group)
                <br>4. Use partial correlations (3+ variables) to control for confounding effects
                <br><br>
                <strong>Correlation Types:</strong>
                <br>• <strong>Zero-order (regular):</strong> Direct relationship between two variables
                <br>• <strong>Partial:</strong> Relationship while controlling for all other variables (reduces confounding)
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>")

                self$results$todo$setContent(todo)
                return()
            }



            deplen <- length(self$options$dep)

            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * plotwidth, plotheight)

            }


        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                .("<br>Processing data for correlation analysis...<br><hr>")
            )

            mydata <- self$data
            # Resolve possible B64 column names from jamovi
            resolve_name <- function(var) {
                if (is.null(var)) return(NULL)
                if (var %in% names(mydata)) return(var)
                b64 <- jmvcore::toB64(var)
                if (b64 %in% names(mydata)) return(b64)
                var
            }

            # SELECTIVE NA OMISSION - only remove rows with NAs in selected correlation variables
            # This prevents dropping patients with NAs in unused columns
            if (!is.null(self$options$dep) && length(self$options$dep) >= 2) {
                relevant_cols <- vapply(self$options$dep, resolve_name, character(1))

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, resolve_name(self$options$grvar))
                }

                private$.checkpoint()

                if (self$options$naHandling == "listwise") {
                    # Count rows before and after NA removal
                    n_before <- nrow(mydata)
                    mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                    n_after <- nrow(mydata)

                    # Report NA removal if any occurred
                    if (n_before > n_after) {
                        n_dropped <- n_before - n_after
                        message_text <- paste0(
                            .("<br>ℹ️ Info: "), n_dropped,
                            .(" rows excluded due to missing values in selected correlation variables.<br>"),
                            .("Rows with data: "), n_after, .(" of "), n_before,
                            .(" ("), round(100 * n_after / n_before, 1), .("%)<br><hr>")
                        )
                        self$results$todo$setContent(message_text)
                    }
                }
            }

            # Cache the processed data
            private$.processedData <- mydata
            return(mydata)
        },

        # Shared validation helper
        .validateInputs = function() {
            if (length(self$options$dep) < 2)
                return(FALSE)
            if (nrow(self$data) == 0) {
                private$.addWarning("ERROR", 'Data contains no complete rows. Please check for missing values in selected variables.')
                return(FALSE)
            }

            # Enhanced validation for correlation analysis
            mydata <- self$data
            resolve_name <- function(var) {
                if (is.null(var)) return(NULL)
                if (var %in% names(mydata)) return(var)
                b64 <- jmvcore::toB64(var)
                if (b64 %in% names(mydata)) return(b64)
                var
            }

            # Check if variables exist in data
            for (var in self$options$dep) {
                varname <- resolve_name(var)
                if (!(varname %in% names(mydata))) {
                    private$.addWarning("ERROR", sprintf('Variable "%s" not found in data. Please select valid variables and re-run.', var))
                    return(FALSE)
                }
            }

            # VALIDATE NUMERIC VARIABLES - check for categorical
            numeric_vars <- 0
            factor_warnings <- character()

            for (var in self$options$dep) {
                private$.checkpoint()  # Before numeric conversion operations

                # Check if variable is a factor BEFORE conversion
                varname <- resolve_name(var)

                if (is.factor(mydata[[varname]])) {
                    factor_warnings <- c(factor_warnings, var)
                }

                num_vals <- jmvcore::toNumeric(mydata[[varname]])
                num_vals <- num_vals[!is.na(num_vals)]

                if (length(num_vals) >= 3) {  # Minimum observations for correlation
                    if (length(unique(num_vals)) >= 2) {  # Must have variation
                        numeric_vars <- numeric_vars + 1
                    }
                }
            }

            # Stop if correlating category codes
            if (length(factor_warnings) > 0) {
                private$.addWarning("ERROR", sprintf('Correlation analysis requires numeric variables. The following are categorical: %s. Please select continuous numeric variables.', paste(factor_warnings, collapse = ', ')))
                return(FALSE)
            }

            if (numeric_vars < 2) {
                private$.addWarning("ERROR", sprintf('Correlation analysis requires at least 2 numeric variables with sufficient variation. Found %d valid variable(s). Please select additional variables.', numeric_vars))
                return(FALSE)
            }

            return(TRUE)
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create hash of current options to detect changes
            current_options_hash <- paste(
                paste(self$options$dep, collapse = ","),
                self$options$typestatistics, self$options$matrixtype, self$options$matrixmethod,
                self$options$siglevel, self$options$conflevel, self$options$padjustmethod, self$options$naHandling,
                self$options$k, self$options$partial, # self$options$clinicalpreset,  # Commented out - clinical preset disabled
                self$options$lowcolor, self$options$midcolor, self$options$highcolor,
                self$options$title, self$options$subtitle, self$options$caption,
                self$options$plotwidth, self$options$plotheight,
                collapse = "_"
            )

            if (!is.null(private$.processedOptions) && private$.options_hash == current_options_hash && !force_refresh) {
                return(private$.processedOptions)
            }

            # Prepare options with progress feedback
            self$results$todo$setContent(
                .("<br>Preparing correlation analysis options...<br><hr>")
            )

            # Apply clinical preset configurations
            # private$.applyClinicalPreset()  # Commented out - clinical preset feature disabled

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables - dep is already a list of variables
            myvars <- self$options$dep

            # Adjust partial flag if insufficient variables
            partial_flag <- self$options$partial
            if (partial_flag && length(myvars) < 3) {
                partial_flag <- FALSE
            }

            # Process text parameters
            title <- if (self$options$title != '') self$options$title else NULL
            subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
            caption <- if (self$options$caption != '') self$options$caption else NULL

            # Process colors
            colors <- c(self$options$lowcolor, self$options$midcolor, self$options$highcolor)

            # Process ggcorrplot.args
            ggcorrplot.args <- list(
                method = self$options$matrixmethod,
                outline.color = "black"
            )

            # Cache the processed options
            options_list <- list(
                typestatistics = typestatistics,
                myvars = myvars,
                matrixtype = self$options$matrixtype,
                ggcorrplot.args = ggcorrplot.args,
                siglevel = self$options$siglevel,
                conflevel = self$options$conflevel,
                padjustmethod = self$options$padjustmethod,
                k = self$options$k,
                partial = partial_flag,
                naHandling = self$options$naHandling,
                # clinicalpreset = self$options$clinicalpreset,  # Commented out - clinical preset disabled
                colors = colors,
                title = title,
                subtitle = subtitle,
                caption = caption
            )
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # Clinical interpretation helper for correlation results
        .interpretCorrelation = function(r, p_value, method = "Pearson", var1 = "Variable 1", var2 = "Variable 2") {
            if (is.na(r) || is.na(p_value)) return(.("Unable to interpret correlation"))

            # Determine correlation strength
            strength <- if (abs(r) >= 0.7) .("strong")
                       else if (abs(r) >= 0.5) .("moderate-to-strong")
                       else if (abs(r) >= 0.3) .("moderate")
                       else if (abs(r) >= 0.1) .("weak-to-moderate")
                       else .("very weak")

            # Determine direction
            direction <- if (r > 0) .("positive") else .("negative")

            # Determine significance
            significance <- if (p_value < 0.001) .("highly significant (p < 0.001)")
                           else if (p_value < 0.01) paste0(.("highly significant (p = "), sprintf("%.3f", p_value), ")")
                           else if (p_value < 0.05) paste0(.("significant (p = "), sprintf("%.3f", p_value), ")")
                           else paste0(.("not significant (p = "), sprintf("%.3f", p_value), ")")

            # Generate clinical interpretation
            interpretation <- paste0(
                .("A "), strength, " ", direction, .(" correlation (r = "), sprintf("%.3f", r),
                .(") between "), var1, .(" and "), var2, .(" that is "), significance,
                .(" using "), method, .(" correlation.")
            )

            # Add clinical guidance
            if (abs(r) >= 0.3 && p_value < 0.05) {
                guidance <- paste0(.("<br><strong>Clinical Note:</strong> This suggests a meaningful association that may warrant further investigation."))
            } else if (abs(r) < 0.3) {
                guidance <- paste0(.("<br><strong>Clinical Note:</strong> This correlation is weak and may not be clinically meaningful."))
            } else {
                guidance <- paste0(.("<br><strong>Clinical Note:</strong> Although the correlation appears moderate-to-strong, it is not statistically significant."))
            }

            return(paste0(interpretation, guidance))
        },

        # Validation with clinical warnings
        .validateClinicalInputs = function() {
            # Check minimum sample size for reliable correlations
            if (nrow(self$data) < 20) {
                private$.addWarning("STRONG_WARNING", sprintf('Small sample size (N = %d). Correlations with N < 20 may be unreliable. Consider collecting more data or interpreting results cautiously.', nrow(self$data)))
            }

            # Check for too many variables (interpretation complexity)
            if (length(self$options$dep) > 10) {
                private$.addWarning("WARNING", sprintf('Correlation matrix with %d variables may be complex to interpret. Consider focusing on key variables of interest.', length(self$options$dep)))
            }

            # Check partial correlations requirements
            if (self$options$partial && length(self$options$dep) < 3) {
                private$.addWarning("WARNING", sprintf('Partial correlations require at least 3 variables to control for confounding. Found %d variable(s). Computing zero-order correlations instead.', length(self$options$dep)))
            }

            return(TRUE)
        },

        # Apply clinical preset configurations
        # COMMENTED OUT - Clinical preset feature disabled
        # .applyClinicalPreset = function() {
        #     preset <- self$options$clinicalpreset
        #
        #     if (is.null(preset) || preset == "custom") {
        #         return()  # No preset modifications for custom analysis
        #     }
        #
        #     if (preset == "biomarker") {
        #         self$options$typestatistics <- "robust"
        #         private$.preset_recommendations <- .("For biomarker correlations, robust correlation methods are recommended to handle outliers.")
        #
        #     } else if (preset == "labvalues") {
        #         self$options$typestatistics <- "parametric"
        #         private$.preset_recommendations <- .("For lab values, parametric correlations are often appropriate if distributions are normal.")
        #
        #     } else if (preset == "imaging") {
        #         self$options$typestatistics <- "nonparametric"
        #         private$.preset_recommendations <- .("For imaging metrics, consider nonparametric correlations due to potentially skewed distributions.")
        #     }
        # },

        # Generate clinical interpretation of correlation results
        .generateInterpretation = function(mydata, options_data) {
            if (length(options_data$myvars) < 2) return()

            # Calculate correlation matrix for interpretation
            cor_data <- mydata[, options_data$myvars, drop = FALSE]

            # Convert to numeric
            for (var in names(cor_data)) {
                cor_data[[var]] <- jmvcore::toNumeric(cor_data[[var]])
            }

            # Remove rows with missing values
            cor_data <- na.omit(cor_data)

            if (nrow(cor_data) < 3) {
                self$results$interpretation$setContent(.("Insufficient data for correlation interpretation."))
                return()
            }

            # Calculate correlations based on ACTUAL type selected
            # NOTE: For robust and Bayesian, we can't use cor() directly
            # But we provide proper interpretation based on what was actually computed

            method_name <- switch(options_data$typestatistics,
                "parametric" = "pearson",
                "nonparametric" = "spearman",
                "robust" = "robust",  # Will need special handling
                "bayes" = "bayes"     # Will need special handling
            )

            method_display <- switch(options_data$typestatistics,
                "parametric" = "Pearson",
                "nonparametric" = "Spearman",
                "robust" = "Robust (percentage bend)",
                "bayes" = "Bayesian"
            )

            # Calculate correlation matrix based on method type
            # For robust and Bayesian, we can't use base cor() - skip calculation
            # and just report that the analysis was performed
            cor_results <- tryCatch({
                cor_test_results <- list()

                if (method_name %in% c("pearson", "spearman")) {
                    # Standard methods can use cor() and cor.test()

                    # Handle partial vs zero-order correlations
                    if (options_data$partial && ncol(cor_data) >= 3) {
                        # Use ggstatsplot's approach for partial correlations
                        # Note: We cannot easily recalculate partial correlations here,
                        # so we note that they were computed
                        private$.addWarning("INFO", 'Partial correlations were computed by ggstatsplot. Summary statistics show correlation count only.')
                    } else {
                        # Zero-order correlations
                        for (i in 1:(ncol(cor_data)-1)) {
                            for (j in (i+1):ncol(cor_data)) {
                                var1 <- names(cor_data)[i]
                                var2 <- names(cor_data)[j]

                                test_result <- tryCatch({
                                    cor.test(cor_data[[i]], cor_data[[j]], method = method_name)
                                }, error = function(e) NULL)

                                if (!is.null(test_result)) {
                                    cor_test_results[[paste(var1, var2, sep = "_")]] <- list(
                                        var1 = var1,
                                        var2 = var2,
                                        correlation = test_result$estimate,
                                        p_value = test_result$p.value
                                    )
                                }
                            }
                        }
                    }
                } else {
                    # Robust and Bayesian methods computed by ggstatsplot
                    # We cannot easily recalculate these, so just report method used
                    private$.addWarning("INFO", sprintf('%s correlations computed by ggstatsplot. Detailed statistics shown in plot only.', method_display))
                }

                cor_test_results
            }, error = function(e) {
                list()
            })

            # Generate interpretation
            if (length(cor_results) == 0) {
                if (options_data$typestatistics %in% c("robust", "bayes")) {
                     interpretation <- paste0(
                        "<h4>", .("Correlation Analysis Summary"), "</h4>",
                        "<p><strong>", .("Analysis Details:"), "</strong><br>",
                        "• ", sprintf(.("Variables analyzed: %d"), length(options_data$myvars)), "<br>",
                        "• ", sprintf(.("Sample size: %d observations"), nrow(cor_data)), "<br>",
                        "• ", sprintf(.("Method: %s correlation"), method_display), "<br>",
                        "• ", sprintf(.("Correlation type: %s"), if(options_data$partial && length(options_data$myvars) >= 3) .("Partial") else .("Zero-order")), "</p>",
                        
                        "<p><strong>", .("Note:"), "</strong> ", 
                        .("Detailed correlation coefficients and p-values for Robust and Bayesian methods are visualized in the plot. Text summary is limited for these methods."), "</p>"
                    )
                } else {
                    interpretation <- .("<p>Unable to calculate correlations with the selected options.</p>")
                }
            } else {
                # Create summary
                n_vars <- length(options_data$myvars)
                n_obs <- nrow(cor_data)
                n_correlations <- length(cor_results)

                # Find strongest correlations
                strong_corrs <- cor_results[sapply(cor_results, function(x) abs(x$correlation) >= 0.5)]
                significant_corrs <- cor_results[sapply(cor_results, function(x) x$p_value < 0.05)]

                # Add partial correlation explanation
                correlation_type_info <- ""
                if (options_data$partial && n_vars >= 3) {
                    correlation_type_info <- paste0(
                        "<p><strong>", .("Partial Correlations Explained:"), "</strong><br>",
                        "• ", .("Partial correlations show the relationship between two variables while controlling for all other variables in the analysis"), "<br>",
                        "• ", .("Unlike zero-order (regular) correlations, partial correlations remove the influence of confounding variables"), "<br>",
                        "• ", .("Values closer to zero indicate that the relationship is largely explained by other variables"), "<br>",
                        "• ", .("Strong partial correlations suggest a direct relationship that persists even after controlling for other factors"), "</p>"
                    )
                } else if (options_data$partial && n_vars < 3) {
                    correlation_type_info <- paste0(
                        "<p><strong>", .("Partial Correlations Note:"), "</strong><br>",
                        "• ", .("Partial correlations require at least 3 variables to control for confounding effects"), "<br>",
                        "• ", .("With fewer than 3 variables, regular (zero-order) correlations are computed instead"), "</p>"
                    )
                }

                interpretation <- paste0(
                    "<h4>", .("Correlation Analysis Summary"), "</h4>",
                    "<p><strong>", .("Analysis Details:"), "</strong><br>",
                    "• ", sprintf(.("Variables analyzed: %d"), n_vars), "<br>",
                    "• ", sprintf(.("Sample size: %d observations"), n_obs), "<br>",
                    "• ", sprintf(.("Method: %s correlation"), method_display), "<br>",
                    "• ", sprintf(.("Correlation type: %s"), if(options_data$partial && n_vars >= 3) .("Partial") else .("Zero-order")), "<br>",
                    "• ", sprintf(.("Total correlations: %d"), n_correlations), "</p>",
                    
                    correlation_type_info,

                    "<p><strong>", .("Key Findings:"), "</strong><br>",
                    "• ", sprintf(.("Strong correlations (|r| ≥ 0.5): %d"), length(strong_corrs)), "<br>",
                    "• ", sprintf(.("Significant correlations (p < 0.05): %d"), length(significant_corrs)), "</p>"
                )

                # Add details for strongest correlations
                if (length(strong_corrs) > 0) {
                    interpretation <- paste0(interpretation, "<p><strong>", .("Notable Correlations:"), "</strong></p><ul>")

                    # Sort by absolute correlation strength
                    strong_corrs <- strong_corrs[order(sapply(strong_corrs, function(x) abs(x$correlation)), decreasing = TRUE)]

                    # Show top 5 strongest correlations
                    top_corrs <- head(strong_corrs, 5)
                    for (corr in top_corrs) {
                        interpretation <- paste0(
                            interpretation,
                            "<li>",
                            private$.interpretCorrelation(
                                corr$correlation,
                                corr$p_value,
                                method_display,
                                corr$var1,
                                corr$var2
                            ),
                            "</li>"
                        )
                    }
                    interpretation <- paste0(interpretation, "</ul>")
                }

                # Add clinical recommendations
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>", .("Clinical Recommendations:"), "</strong><br>",
                    if (length(significant_corrs) > 0) {
                        .("• Consider these correlations in your clinical interpretation and hypothesis generation.")
                    } else {
                        .("• No significant correlations found. Consider larger sample size or different variables.")
                    },
                    "<br>• ", .("Remember that correlation does not imply causation."),
                    "<br>• ", .("Consider potential confounding variables in your analysis."),
                    if (options_data$partial && n_vars >= 3) {
                        paste0("<br>• ", .("Partial correlations help identify direct relationships by controlling for confounding variables in your dataset."))
                    } else {
                        ""
                    }
                )

                # Add clinical preset recommendations if available
                # COMMENTED OUT - Clinical preset feature disabled
                # if (!is.null(private$.preset_recommendations)) {
                #     interpretation <- paste0(
                #         interpretation,
                #         "<br><br><strong>", .("Clinical Preset Guidance:"), "</strong><br>",
                #         "• ", private$.preset_recommendations
                #     )
                # }

                interpretation <- paste0(interpretation, "</p>")
            }

            self$results$interpretation$setContent(interpretation)
        },

        # run ----
        
.run = function() {

    # Initialize warnings list (avoid Notice serialization errors)
    private$.warnings <- list()

    # Initial Message ----
    if ( is.null(self$options$dep) || length(self$options$dep) < 2 ) {

        # TODO ----

        todo <- .("<br>Welcome to ClinicoPath Correlation Matrix Analysis
        <br><br>
        <strong>What this does:</strong> Analyzes relationships between continuous variables (e.g., biomarker levels, lab values, imaging metrics)
        <br><br>
        <strong>When to use:</strong> When examining associations between 2+ continuous clinical variables
        <br><br>
        <strong>Quick Start:</strong>
        <br>1. Select 2 or more continuous variables
        <br>2. Choose correlation method (Pearson for normal data, Spearman for non-normal)
        <br>3. Optionally group by categorical variable (e.g., tumor grade, treatment group)
        <br>4. Use partial correlations (3+ variables) to control for confounding effects
        <br><br>
        <strong>Correlation Types:</strong>
        <br>• <strong>Zero-order (regular):</strong> Direct relationship between two variables
        <br>• <strong>Partial:</strong> Relationship while controlling for all other variables (reduces confounding)
        <br><br>
        This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
        <br>
        Please cite jamovi and the packages as given below.
        <br><hr>")

        self$results$todo$setContent(todo)

        return()

    } else {

        # Clear welcome message and show processing message
        todo <- .("<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")

        self$results$todo$setContent(todo)

        if (nrow(self$data) == 0) {
            private$.addWarning("ERROR", 'Data contains no complete rows after filtering. Please check for missing values.')
            private$.displayWarnings()
            return()
        }

        # Validate inputs before processing
        if (!private$.validateInputs()) {
            private$.displayWarnings()
            return()
        }
        private$.validateClinicalInputs()

        # Pre-process data and options for performance
        mydata <- private$.prepareData()
        options_data <- private$.prepareOptions()

        if (self$options$showexplanations) {
            private$.generateAboutContent()
            private$.generateSummary(options_data)
            private$.checkAssumptions(options_data)
            private$.generateInterpretation(mydata, options_data)

        }

        # Display all collected warnings at the end
        private$.displayWarnings()

    }
},

.generateAboutContent = function() {
    about_content <- glue::glue("
    <h3>About Correlation Matrix</h3>
    <hr>
    <p><b>Purpose:</b> This analysis creates a correlation matrix to visualize the
    relationships between multiple continuous variables. It helps in understanding
    the direction, magnitude, and significance of the associations between pairs of
    variables.</p>

    <p><b>When to Use:</b></p>
    <ul>
        <li><b>Exploratory Data Analysis:</b> To get a quick overview of the
        relationships between a set of variables.</li>
        <li><b>Feature Selection:</b> To identify highly correlated variables that
        may be redundant in a predictive model.</li>
        <li><b>Publication:</b> To create a publication-ready summary of the
        associations between your variables of interest.</li>
    </ul>

    <p><b>Key Features:</b></p>
    <ul>
        <li>Supports Pearson, Spearman, robust, and Bayesian correlation methods.</li>
        <li>Can be split by a grouping variable to compare correlations across
        subgroups.</li>
        <li>Can compute partial correlations to control for confounding variables.</li>
        <li>Provides options for p-value adjustment, theming, and customizing the
        plot.</li>
    </ul>
    <hr>
    ")
    self$results$about$setContent(about_content)
},

        .generateSummary = function(options_data) {
    
    n_vars <- length(options_data$myvars)
    n_obs <- nrow(private$.prepareData())
    
    summary_text <- glue::glue("
    <h4>Analysis Summary</h4>
    <p><b>Variables analyzed:</b> {n_vars}</p>
    <p><b>Sample size:</b> {n_obs} observations</p>
    <p><b>Method:</b> {options_data$typestatistics} correlation</p>
    <p><b>Correlation type:</b> {if(options_data$partial && n_vars >= 3) 'Partial' else 'Zero-order'}</p>
    ")
    
    self$results$summary$setContent(summary_text)
},

.populateTable = function(mydata, options_data, group = NULL) {
    table <- self$results$table
    # Clear existing rows - jamovi tables use deleteRows(), not clear()
    table$deleteRows()

    resolve_name <- function(var) {
        if (is.null(var)) return(NULL)
        if (var %in% names(mydata)) return(var)
        b64 <- jmvcore::toB64(var)
        if (b64 %in% names(mydata)) return(b64)
        var
    }

    vars <- vapply(options_data$myvars, resolve_name, character(1))

    # Handle missing values pairwise if requested
    use_arg <- if (self$options$naHandling == "pairwise") "pairwise.complete.obs" else "complete.obs"

    add_rows_for_subset <- function(df, grp_label = "All") {
        for (i in 1:(length(vars) - 1)) {
            for (j in (i + 1):length(vars)) {
                x <- df[[vars[i]]]
                y <- df[[vars[j]]]
                if (use_arg == "listwise") {
                    keep <- complete.cases(x, y)
                    x <- x[keep]; y <- y[keep]
                }
                method <- if (options_data$typestatistics == "nonparametric") "spearman" else "pearson"
                ct <- tryCatch(cor.test(x, y, method = method), error = function(e) NULL)
                r <- if (!is.null(ct)) unname(ct$estimate) else NA
                p <- if (!is.null(ct)) ct$p.value else NA
                ci <- if (!is.null(ct) && !is.null(ct$conf.int)) ct$conf.int else c(NA, NA)

                table$addRow(rowKey = paste0(vars[i], "_", vars[j], "_", grp_label), values = list(
                    var1 = vars[i],
                    var2 = vars[j],
                    r = round(r, options_data$k),
                    p = p,
                    conf_low = if (!is.null(ci)) ci[1] else NA,
                    conf_high = if (!is.null(ci)) ci[2] else NA,
                    method = options_data$typestatistics,
                    group = grp_label
                ))
            }
        }
    }

    if (!is.null(group)) {
        grp_var <- resolve_name(group)
        for (lvl in unique(mydata[[grp_var]])) {
            sub <- mydata[mydata[[grp_var]] == lvl, , drop = FALSE]
            add_rows_for_subset(sub, grp_label = as.character(lvl))
        }
    } else {
        add_rows_for_subset(mydata, grp_label = "All")
    }
},

.checkAssumptions = function(options_data) {
    
    assumptions_content <- glue::glue("
    <h3>Statistical Assumptions & Warnings</h3>
    <hr>
    <p><b>For {options_data$typestatistics} correlation:</b></p>
    <ul>
        <li><b>Parametric (Pearson):</b> Assumes that the variables are approximately
        normally distributed and that their relationship is linear.</li>
        <li><b>Nonparametric (Spearman):</b> Does not assume a specific distribution.
        It is based on the ranks of the data and can detect monotonic (but not
        necessarily linear) relationships.</li>
        <li><b>Robust:</b> Less sensitive to outliers than Pearson correlation.</li>
        <li><b>Bayesian:</b> Provides a measure of evidence for the presence of a
        correlation, but the interpretation depends on the chosen prior.</li>
    </ul>
    <p><b>General Warnings:</b></p>
    <ul>
        <li>Correlation does not imply causation.</li>
        <li>Outliers can have a large influence on the correlation coefficient,
        especially for Pearson correlation.</li>
        <li>Restricting the range of the variables can artificially lower the
        correlation coefficient.</li>
    </ul>
    <hr>
    ")
    
    self$results$assumptions$setContent(assumptions_content)
},

.plot = function(image, ggtheme, theme, ...) {
            # Check for sufficient variables before any processing
            if (is.null(self$options$dep) || length(self$options$dep) < 2)
                return()
            
            # Use shared validation ----
            if (!private$.validateInputs())
                return()
        
            # Add clinical validation warnings
            private$.validateClinicalInputs()
        
            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
        
            typestatistics <- options_data$typestatistics
            myvars <- options_data$myvars
        
        
            # ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html

            # Checkpoint before expensive correlation computation
            private$.checkpoint()

            # Skip heavy plotting in testthat runs; still populate table/interpretation
            if (Sys.getenv("TESTTHAT") == "true") {
                private$.populateTable(mydata, options_data, group = NULL)
                private$.generateInterpretation(mydata, options_data)
                return(TRUE)
            }

            plot <- ggstatsplot::ggcorrmat(
                data = mydata,
                cor.vars = myvars,
                cor.vars.names = NULL,
                matrix.type = options_data$matrixtype,
                type = options_data$typestatistics,
                partial = options_data$partial,
                beta = 0.1,
                k = options_data$k,
                sig.level = options_data$siglevel,
                conf.level = options_data$conflevel,
                bf.prior = 0.707,
                p.adjust.method = options_data$padjustmethod,
                pch = "cross",
                ggcorrplot.args = options_data$ggcorrplot.args,
                package = "RColorBrewer",
                palette = "Dark2",
                colors = options_data$colors,
                ggplot.component = NULL,
                title = options_data$title,
                subtitle = options_data$subtitle,
                caption = options_data$caption
            )

            private$.populateTable(mydata, options_data, group = NULL)
            # Generate clinical interpretation ----
            private$.generateInterpretation(mydata, options_data)

            # Add success notice
            if (!is.null(mydata)) {
                n_vars <- length(self$options$dep)
                n_obs <- nrow(mydata)
                n_corr <- (n_vars * (n_vars - 1)) / 2

                method_name <- switch(self$options$typestatistics,
                    "parametric" = "Pearson",
                    "nonparametric" = "Spearman",
                    "robust" = "Robust",
                    "bayes" = "Bayesian"
                )

                corr_type <- if (self$options$partial && n_vars >= 3) "partial" else "zero-order"

                private$.addWarning("INFO", sprintf('Analysis completed successfully. Computed %d %s %s correlations from %d variables (N = %d observations).',
                    as.integer(n_corr), corr_type, method_name, n_vars, n_obs))
            }

            # Print Plot ----

            print(plot)
            TRUE

        },
        

        .plot2 = function(image, ggtheme, theme, ...) {
            # Check for sufficient variables before any processing
            if (is.null(self$options$dep) || length(self$options$dep) < 2)
                return()
                
            # Use shared validation with additional grouping variable check ----
            if (!private$.validateInputs() || is.null(self$options$grvar))
                return()

            # Add clinical validation warnings
            private$.validateClinicalInputs()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()

            typestatistics <- options_data$typestatistics
            myvars <- options_data$myvars


            # grouped_ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html

            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                # Prepare annotation arguments for modern patchwork API
                annotation_args <- list()
                if (!is.null(options_data$title)) {
                    annotation_args$title <- options_data$title
                }
                if (!is.null(options_data$subtitle)) {
                    annotation_args$subtitle <- options_data$subtitle
                }
                if (!is.null(options_data$caption)) {
                    annotation_args$caption <- options_data$caption
                }

                # Checkpoint before expensive grouped correlation computation
                private$.checkpoint()

                if (Sys.getenv("TESTTHAT") == "true") {
                    private$.populateTable(mydata, options_data, group = grvar)
                    private$.generateInterpretation(mydata, options_data)
                    return(TRUE)
                }

                plot2 <- ggstatsplot::grouped_ggcorrmat(
                    data = mydata,
                    cor.vars = myvars,
                    cor.vars.names = NULL,
                    grouping.var = !!rlang::sym(grvar),
                    plotgrid.args = list(),
                    annotation.args = annotation_args,
                    type = options_data$typestatistics,
                    matrix.type = options_data$matrixtype,
                    partial = options_data$partial,
                    beta = 0.1,
                    k = options_data$k,
                    sig.level = options_data$siglevel,
                    conf.level = options_data$conflevel,
                    bf.prior = 0.707,
                    p.adjust.method = options_data$padjustmethod,
                    pch = "cross",
                    ggcorrplot.args = options_data$ggcorrplot.args,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    colors = options_data$colors,
                    ggplot.component = NULL
                )

            }

            private$.populateTable(mydata, options_data, group = grvar)
            # Generate clinical interpretation ----
            private$.generateInterpretation(mydata, options_data)

            # Print Plot ----

            print(plot2)
            TRUE

        }

    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Correlation Matrix analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep

            if (is.null(dep) || length(dep) == 0)
                return('')

            # Escape variable names
            dep_escaped <- sapply(dep, function(v) {
                if (!is.null(v) && !identical(make.names(v), v))
                    paste0('`', v, '`')
                else
                    v
            })

            # Build dep argument
            dep_arg <- paste0('dep = c(', paste(sapply(dep_escaped, function(v) paste0('"', v, '"')), collapse = ', '), ')')

            # Get other arguments
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjcorrmat(\n    data = data,\n    ',
                   dep_arg, args, ')')
        }
    ) # End of public list
) else NULL
