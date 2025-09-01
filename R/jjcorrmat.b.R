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
        .preset_recommendations = NULL,

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

            # Exclude NA with checkpoint
            private$.checkpoint()
            mydata <- jmvcore::naOmit(mydata)

            # Cache the processed data
            private$.processedData <- mydata
            return(mydata)
        },

        # Shared validation helper
        .validateInputs = function() {
            if (length(self$options$dep) < 2)
                return(FALSE)
            if (nrow(self$data) == 0)
                stop(.('Data contains no (complete) rows'))

            # Enhanced validation for correlation analysis
            mydata <- self$data

            # Check if variables exist in data
            for (var in self$options$dep) {
                if (!(var %in% names(mydata)))
                    stop(sprintf(.('Variable "%s" not found in data'), var))
            }

            # Convert to numeric and check for sufficient numeric data
            numeric_vars <- 0
            for (var in self$options$dep) {
                private$.checkpoint()  # Before numeric conversion operations
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]

                if (length(num_vals) >= 3) {  # Minimum observations for correlation
                    if (length(unique(num_vals)) >= 2) {  # Must have variation
                        numeric_vars <- numeric_vars + 1
                    }
                }
            }

            if (numeric_vars < 2)
                stop(.('Correlation analysis requires at least 2 numeric variables with sufficient variation and observations'))

            return(TRUE)
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create hash of current options to detect changes
            current_options_hash <- paste(
                paste(self$options$dep, collapse = ","),
                self$options$typestatistics, self$options$matrixtype, self$options$matrixmethod,
                self$options$siglevel, self$options$conflevel, self$options$padjustmethod,
                self$options$k, self$options$partial, self$options$clinicalpreset, self$options$lowcolor, self$options$midcolor, self$options$highcolor,
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
            private$.applyClinicalPreset()

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables - dep is already a list of variables
            myvars <- self$options$dep

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
                partial = self$options$partial,
                clinicalpreset = self$options$clinicalpreset,
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
                warning(.("Sample size < 20 may produce unreliable correlation estimates. Consider collecting more data."))
            }

            if (nrow(self$data) < 10) {
                stop(.("Sample size < 10 is insufficient for correlation analysis. Minimum 10 observations required."))
            }

            # Check for too many variables (interpretation complexity)
            if (length(self$options$dep) > 10) {
                warning(.("Correlation matrix with >10 variables may be difficult to interpret. Consider focusing on key variables."))
            }

            # Check partial correlations requirements
            if (self$options$partial && length(self$options$dep) < 3) {
                warning(.("Partial correlations require at least 3 variables. Using zero-order correlations instead."))
            }

            return(TRUE)
        },

        # Apply clinical preset configurations
        .applyClinicalPreset = function() {
            preset <- self$options$clinicalpreset

            if (is.null(preset) || preset == "custom") {
                return()  # No preset modifications for custom analysis
            }

            # Apply preset-specific recommendations (non-intrusive)
            if (preset == "biomarker") {
                # Biomarker correlations: document robust method recommendation
                private$.preset_recommendations <- .("For biomarker correlations, consider robust correlation methods to handle outliers.")

            } else if (preset == "labvalues") {
                # Lab value relationships: document parametric suitability
                private$.preset_recommendations <- .("For lab values, parametric correlations are often appropriate if distributions are normal.")

            } else if (preset == "imaging") {
                # Imaging metrics: document nonparametric considerations
                private$.preset_recommendations <- .("For imaging metrics, consider nonparametric correlations due to potentially skewed distributions.")
            }
        },

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

            # Calculate correlations based on type
            method_name <- switch(options_data$typestatistics,
                "parametric" = "pearson",
                "nonparametric" = "spearman",
                "robust" = "pearson",  # robust method
                "bayes" = "pearson"
            )

            method_display <- switch(options_data$typestatistics,
                "parametric" = "Pearson",
                "nonparametric" = "Spearman",
                "robust" = "Robust",
                "bayes" = "Bayesian"
            )

            # Calculate correlation matrix
            cor_results <- tryCatch({
                cor_matrix <- cor(cor_data, method = method_name, use = "complete.obs")
                cor_test_results <- list()

                # Calculate p-values for significant correlations
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

                cor_test_results
            }, error = function(e) {
                list()
            })

            # Generate interpretation
            if (length(cor_results) == 0) {
                interpretation <- .("<p>Unable to calculate correlations with the selected options.</p>")
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
                if (!is.null(private$.preset_recommendations)) {
                    interpretation <- paste0(
                        interpretation,
                        "<br><br><strong>", .("Clinical Preset Guidance:"), "</strong><br>",
                        "• ", private$.preset_recommendations
                    )
                }

                interpretation <- paste0(interpretation, "</p>")
            }

            self$results$interpretation$setContent(interpretation)
        },

        # run ----
        .run = function() {

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

                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))

                # Pre-process data and options for performance
                private$.prepareData()
                private$.prepareOptions()

            }
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

            # Generate clinical interpretation ----
            private$.generateInterpretation(mydata, options_data)

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

            # Generate clinical interpretation ----
            private$.generateInterpretation(mydata, options_data)

            # Print Plot ----

            print(plot2)
            TRUE

        }

    )
) else NULL







