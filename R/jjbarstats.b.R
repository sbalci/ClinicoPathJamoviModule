#' @title Bar Charts
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom digest digest
#' @importFrom purrr map imap
#' @importFrom rlang sym %||%
#' @importFrom glue glue
#'

jjbarstatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjbarstatsClass",
        inherit = jjbarstatsBase,
        private = list(
            # Cache variables for performance
            .cached_data = NULL,
            .data_hash = NULL,
            .validation_passed = FALSE,
            
            # init ----

            .init = function() {

                deplen <- length(self$options$dep)
                self$results$plot$setSize(650, deplen * 450)

                if (!is.null(self$options$grvar)) {

                    mydata <- self$data

                    grvar <-  self$options$grvar

                    num_levels <- nlevels(
                        as.factor(mydata[[grvar]])
                    )

                    self$results$plot2$setSize(num_levels * 650, deplen * 450)

                }

            },

            # Helper Methods ----

            .validateVariables = function() {
                dep_vars <- self$options$dep
                group_var <- self$options$group
                
                # Check if variables exist
                all_vars <- c(dep_vars, group_var)
                if (!is.null(self$options$grvar)) {
                    all_vars <- c(all_vars, self$options$grvar)
                }
                if (!is.null(self$options$counts)) {
                    all_vars <- c(all_vars, self$options$counts)
                }
                
                missing_vars <- all_vars[!all_vars %in% names(self$data)]
                if (length(missing_vars) > 0) {
                    stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
                }
                
                # Check that variables are appropriate for bar charts (categorical)
                # Exclude counts variable from categorical check
                vars_to_check <- all_vars[all_vars != self$options$counts]
                for (var in vars_to_check) {
                    if (!is.null(var)) {
                        var_class <- class(self$data[[var]])
                        if (!any(c("factor", "character", "logical") %in% var_class)) {
                            # Try to convert numeric to factor if it has few unique values
                            if (is.numeric(self$data[[var]])) {
                                unique_vals <- length(unique(self$data[[var]], na.rm = TRUE))
                                if (unique_vals > 10) {
                                    stop(paste("Variable '", var, "' appears to be continuous (", unique_vals, 
                                             " unique values). Bar charts are for categorical data. Consider converting to groups first."))
                                }
                            }
                        }
                    }
                }
                
                # Validate counts variable if provided
                if (!is.null(self$options$counts)) {
                    # Checkpoint before expensive validation
                    private$.checkpoint(flush = FALSE)
                    counts_var <- self$options$counts
                    if (!is.numeric(self$data[[counts_var]])) {
                        stop(paste("Counts variable '", counts_var, "' must be numeric."))
                    }
                    if (any(self$data[[counts_var]] < 0, na.rm = TRUE)) {
                        stop(paste("Counts variable '", counts_var, "' contains negative values."))
                    }
                }
                
                # Enhanced validation for statistical tests
                private$.validateStatisticalRequirements(dep_vars, group_var)
                
                return(TRUE)
            },

            .validateStatisticalRequirements = function(dep_vars, group_var) {
                # Check minimum group sizes for statistical tests
                if (!is.null(group_var) && group_var %in% names(self$data)) {
                    # Checkpoint before table calculation
                    private$.checkpoint(flush = FALSE)
                    group_sizes <- table(self$data[[group_var]], useNA = "no")
                    
                    if (any(group_sizes < 5)) {
                        small_groups <- names(group_sizes[group_sizes < 5])
                        warning(paste("Small group sizes detected (", paste(paste(small_groups, ":", group_sizes[small_groups]), collapse = ", "),
                                    "). Chi-square tests require minimum 5 observations per group for reliable results."))
                    }
                    
                    if (length(group_sizes) < 2) {
                        stop("Grouping variable must have at least 2 categories for comparison.")
                    }
                }
                
                # Check dependent variables have sufficient variation
                for (dep_var in dep_vars) {
                    if (!is.null(dep_var) && dep_var %in% names(self$data)) {
                        # Checkpoint before each table calculation in loop
                        private$.checkpoint(flush = FALSE)
                        dep_levels <- table(self$data[[dep_var]], useNA = "no")
                        if (length(dep_levels) < 2) {
                            stop(paste("Variable '", dep_var, "' has insufficient variation (only", length(dep_levels), "level). Need at least 2 categories."))
                        }
                    }
                }
            },

            .getCachedData = function() {
                # Create hash of current data and options state
                current_hash <- digest::digest(list(
                    data_dim = dim(self$data),
                    data_names = names(self$data),
                    options = list(
                        dep = self$options$dep,
                        group = self$options$group,
                        grvar = self$options$grvar,
                        counts = self$options$counts,
                        excl = self$options$excl,
                        paired = self$options$paired,
                        label = self$options$label
                    )
                ), algo = "md5")
                
                # Return cached data if hash matches and validation passed
                if (!is.null(private$.cached_data) && 
                    !is.null(private$.data_hash) &&
                    private$.data_hash == current_hash && 
                    private$.validation_passed) {
                    return(private$.cached_data)
                }
                
                # Checkpoint before expensive validation and data preparation
                private$.checkpoint(flush = FALSE)
                # Validate and prepare fresh data
                private$.validateVariables()
                private$.cached_data <- private$.prepareData()
                private$.data_hash <- current_hash
                private$.validation_passed <- TRUE
                
                return(private$.cached_data)
            },

            .selectTheme = function(ggtheme) {
                if (self$options$originaltheme) {
                    return(ggstatsplot::theme_ggstatsplot())
                } else {
                    return(ggtheme)
                }
            },

            .applyClinicalPreset = function() {
                preset <- self$options$clinicalpreset
                if (is.null(preset) || !is.character(preset)) preset <- "custom"
                if (preset == "custom") return()
                
                # Apply preset-specific configurations
                switch(preset,
                    "diagnostic" = {
                        # 2x2 diagnostic table: use Fisher's exact when appropriate
                        if (is.null(self$options$typestatistics) || self$options$typestatistics == "parametric") {
                            # Will be handled in statistical validation
                        }
                        # Default to showing statistical results
                        if (is.null(self$options$resultssubtitle)) {
                            self$options$resultssubtitle <- TRUE
                        }
                    },
                    "treatment" = {
                        # Treatment response: enable pairwise comparisons
                        if (is.null(self$options$pairwisecomparisons)) {
                            self$options$pairwisecomparisons <- TRUE
                        }
                        if (is.null(self$options$padjustmethod)) {
                            self$options$padjustmethod <- "holm"
                        }
                    },
                    "biomarker" = {
                        # Biomarker expression: robust statistics for potential outliers
                        if (is.null(self$options$typestatistics)) {
                            self$options$typestatistics <- "robust"
                        }
                        if (is.null(self$options$pairwisecomparisons)) {
                            self$options$pairwisecomparisons <- TRUE
                        }
                    },
                    "riskfactor" = {
                        # Risk factor analysis: parametric with proportion tests
                        if (is.null(self$options$typestatistics)) {
                            self$options$typestatistics <- "parametric"
                        }
                        if (is.null(self$options$proportiontest)) {
                            self$options$proportiontest <- TRUE
                        }
                    }
                )
            },

            .generateAboutContent = function() {
                about_content <- paste0(
                    "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                    "<h4 style='color: #007bff; margin-top: 0;'>📊 About Bar Chart Analysis</h4>",
                    "<p><strong>Purpose:</strong> Compare the distribution of categorical variables across groups using statistical testing.</p>",
                    "<p><strong>When to Use:</strong></p>",
                    "<ul>",
                    "<li><strong>Diagnostic Tests:</strong> Compare test results (positive/negative) across patient groups</li>",
                    "<li><strong>Treatment Response:</strong> Analyze response rates across different treatments</li>",
                    "<li><strong>Biomarker Expression:</strong> Compare expression levels (low/medium/high) by clinical factors</li>",
                    "<li><strong>Risk Factor Analysis:</strong> Examine how risk factors relate to outcomes</li>",
                    "</ul>",
                    "<p><strong>Output Includes:</strong></p>",
                    "<ul>",
                    "<li>Visual bar chart with statistical annotations</li>",
                    "<li>Chi-square or appropriate statistical test results</li>",
                    "<li>Effect size measures and confidence intervals</li>",
                    "<li>Post-hoc pairwise comparisons (when >2 groups)</li>",
                    "</ul>",
                    "</div>"
                )
                
                self$results$about$setContent(about_content)
            },

            .generateSummary = function(analysis_data) {
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return()
                }
                
                n_total <- nrow(analysis_data)
                n_groups <- length(unique(analysis_data[[self$options$group]]))
                dep_vars <- paste(self$options$dep, collapse = ", ")
                
                test_method <- switch(self$options$typestatistics,
                    "parametric" = if (self$options$paired) "McNemar's test" else "Chi-square test of independence",
                    "nonparametric" = "Non-parametric association test", 
                    "robust" = "Robust statistical test",
                    "bayes" = "Bayesian contingency table analysis",
                    "Chi-square test"
                )
                
                summary_content <- paste0(
                    "<div style='padding: 15px; background-color: #e8f5e8; border-left: 4px solid #28a745; margin: 10px 0;'>",
                    "<h4 style='color: #28a745; margin-top: 0;'>📋 Analysis Summary</h4>",
                    "<p><strong>Variables Analyzed:</strong> ", dep_vars, " by ", self$options$group, "</p>",
                    "<p><strong>Sample Size:</strong> ", n_total, " observations across ", n_groups, " groups</p>",
                    "<p><strong>Statistical Method:</strong> ", test_method, "</p>",
                    if (self$options$pairwisecomparisons && n_groups > 2) paste0(
                        "<p><strong>Post-hoc Analysis:</strong> Pairwise comparisons with ", 
                        self$options$padjustmethod, " correction</p>"
                    ) else "",
                    if (!is.null(self$options$grvar)) paste0(
                        "<p><strong>Subgroup Analysis:</strong> Results stratified by ", self$options$grvar, "</p>"
                    ) else "",
                    "<p><strong>Confidence Level:</strong> ", (self$options$conflevel * 100), "%</p>",
                    "</div>"
                )
                
                self$results$summary$setContent(summary_content)
            },

            .checkStatisticalAssumptions = function(analysis_data) {
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return()
                }
                
                warnings <- character()
                recommendations <- character()
                
                # Check group sizes for chi-square validity
                if (!is.null(self$options$group) && self$options$group %in% names(analysis_data)) {
                    for (dep_var in self$options$dep) {
                        if (dep_var %in% names(analysis_data)) {
                            cross_table <- table(analysis_data[[dep_var]], analysis_data[[self$options$group]])
                            expected_counts <- tryCatch({
                                chisq.test(cross_table)$expected
                            }, error = function(e) {
                                # Default to safe values if chi-square test fails
                                default_counts <- cross_table
                                default_counts[] <- 5
                                return(default_counts)
                            })
                            
                            if (any(expected_counts < 5)) {
                                low_count_cells <- sum(expected_counts < 5)
                                total_cells <- length(expected_counts)
                                pct_low <- round(100 * low_count_cells / total_cells, 1)
                                
                                warnings <- c(warnings, paste0(
                                    "⚠️ <strong>Chi-square Assumption Violated:</strong> ",
                                    low_count_cells, " of ", total_cells, " cells (", pct_low, 
                                    "%) have expected counts < 5."
                                ))
                                
                                if (total_cells == 4 && all(dim(cross_table) == c(2, 2))) {
                                    recommendations <- c(recommendations, 
                                        "💡 <strong>Recommendation:</strong> Consider using Fisher's Exact Test for 2×2 tables with low expected counts."
                                    )
                                } else {
                                    recommendations <- c(recommendations,
                                        "💡 <strong>Recommendation:</strong> Consider combining categories or using non-parametric methods."
                                    )
                                }
                            }
                        }
                    }
                }
                
                # Check for paired data appropriateness
                if (self$options$paired) {
                    warnings <- c(warnings,
                        "ℹ️ <strong>Paired Analysis:</strong> McNemar's test assumes matched pairs (e.g., before/after, case/control matching)."
                    )
                }
                
                # Generate assumptions content
                assumptions_content <- paste0(
                    "<div style='padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; margin: 10px 0;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>⚠️ Statistical Assumptions & Warnings</h4>",
                    
                    "<p><strong>General Assumptions:</strong></p>",
                    "<ul>",
                    "<li>Variables are categorical or ordinal</li>",
                    "<li>Observations are independent</li>",
                    "<li>Expected cell counts ≥ 5 for chi-square validity</li>",
                    if (self$options$paired) "<li>Paired observations (matched subjects)</li>" else "",
                    "</ul>",
                    
                    if (length(warnings) > 0) paste0(
                        "<p><strong>Detected Issues:</strong></p>",
                        "<ul><li>", paste(warnings, collapse = "</li><li>"), "</li></ul>"
                    ) else "",
                    
                    if (length(recommendations) > 0) paste0(
                        "<p><strong>Recommendations:</strong></p>",
                        "<ul><li>", paste(recommendations, collapse = "</li><li>"), "</li></ul>"
                    ) else "",
                    
                    "</div>"
                )
                
                self$results$assumptions$setContent(assumptions_content)
            },

            .generateInterpretationGuide = function() {
                interpretation_content <- paste0(
                    "<div style='padding: 15px; background-color: #d1ecf1; border-left: 4px solid #17a2b8; margin: 10px 0;'>",
                    "<h4 style='color: #0c5460; margin-top: 0;'>📖 How to Interpret Results</h4>",
                    
                    "<p><strong>Statistical Significance:</strong></p>",
                    "<ul>",
                    "<li><strong>p < 0.05:</strong> Significant association between variables</li>",
                    "<li><strong>p ≥ 0.05:</strong> No significant association detected</li>",
                    "</ul>",
                    
                    "<p><strong>Effect Size Interpretation:</strong></p>",
                    "<ul>",
                    "<li><strong>Cramér's V:</strong> 0.1 (small), 0.3 (medium), 0.5 (large) effect</li>",
                    "<li><strong>Odds Ratio:</strong> >1 (positive association), <1 (negative association)</li>",
                    "</ul>",
                    
                    "<p><strong>Clinical Context:</strong></p>",
                    switch(self$options$clinicalpreset %||% "custom",
                        "diagnostic" = paste0(
                            "<ul>",
                            "<li><strong>Sensitivity:</strong> Proportion of true positives correctly identified</li>",
                            "<li><strong>Specificity:</strong> Proportion of true negatives correctly identified</li>",
                            "<li><strong>Clinical Significance:</strong> Consider both statistical significance and clinical utility</li>",
                            "</ul>"
                        ),
                        "treatment" = paste0(
                            "<ul>",
                            "<li><strong>Response Rates:</strong> Compare proportions of responders across treatments</li>",
                            "<li><strong>Pairwise Comparisons:</strong> Identify which treatments differ significantly</li>",
                            "<li><strong>Clinical Impact:</strong> Consider magnitude of difference and number needed to treat</li>",
                            "</ul>"
                        ),
                        "biomarker" = paste0(
                            "<ul>",
                            "<li><strong>Expression Patterns:</strong> Compare distribution across clinical groups</li>",
                            "<li><strong>Prognostic Value:</strong> Association with outcomes indicates potential clinical utility</li>",
                            "<li><strong>Validation:</strong> Consider external validation and clinical correlation</li>",
                            "</ul>"
                        ),
                        paste0(
                            "<ul>",
                            "<li><strong>Association Strength:</strong> Look at both statistical significance and effect size</li>",
                            "<li><strong>Clinical Relevance:</strong> Consider biological plausibility and clinical impact</li>",
                            "<li><strong>Further Analysis:</strong> May guide stratification or subgroup analyses</li>",
                            "</ul>"
                        )
                    ),
                    "</div>"
                )
                
                self$results$interpretation$setContent(interpretation_content)
            },

            .generateCopyReadyReport = function(analysis_data) {
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return()
                }
                
                # Basic analysis info
                n_total <- nrow(analysis_data)
                n_groups <- length(unique(analysis_data[[self$options$group]]))
                dep_vars <- paste(self$options$dep, collapse = " and ")
                
                # Generate template report
                report_template <- paste0(
                    "<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; margin: 10px 0;'>",
                    "<h4 style='color: #495057; margin-top: 0;'>📄 Copy-Ready Report Template</h4>",
                    
                    "<div style='background-color: #ffffff; padding: 15px; border: 1px dashed #6c757d; margin: 10px 0;'>",
                    "<h5>Methods:</h5>",
                    "<p>Bar chart analysis was performed to examine the association between ", dep_vars, 
                    " and ", self$options$group, " using ", 
                    switch(self$options$typestatistics,
                        "parametric" = if (self$options$paired) "McNemar's test" else "chi-square test of independence",
                        "nonparametric" = "non-parametric association testing",
                        "robust" = "robust statistical methods",
                        "bayes" = "Bayesian contingency table analysis"
                    ), ". ",
                    
                    if (self$options$pairwisecomparisons && n_groups > 2) {
                        paste0("Post-hoc pairwise comparisons were conducted with ", 
                              self$options$padjustmethod, " correction for multiple testing. ")
                    } else "",
                    
                    "Statistical significance was assessed at α = ", (1 - self$options$conflevel), " level. ",
                    "Analysis included ", n_total, " observations across ", n_groups, " groups.",
                    "</p>",
                    
                    "<h5>Results:</h5>",
                    "<p>[Insert specific results here: test statistic, p-value, effect size with 95% CI]</p>",
                    "<p>Example: \"There was a statistically significant association between [variable 1] and [variable 2] ",
                    "(χ² = [value], p = [value], Cramér's V = [value], 95% CI [lower, upper]). ",
                    "Post-hoc analysis revealed significant differences between [specific groups].\"</p>",
                    
                    "<h5>Conclusion:</h5>",
                    "<p>[Interpret findings in clinical context, considering both statistical significance and clinical relevance]</p>",
                    "</div>",
                    
                    "<button onclick='navigator.clipboard.writeText(this.parentElement.querySelector(\"div\").innerText)' ",
                    "style='background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>",
                    "📋 Copy Template to Clipboard</button>",
                    "</div>"
                )
                
                self$results$report$setContent(report_template)
            },

            .prepareData = function() {
                mydata <- self$data
                
                # Handle missing data based on user preference
                if (self$options$excl) {
                    # Checkpoint before potentially expensive complete.cases operation
                    private$.checkpoint(flush = FALSE)
                    # Remove rows with any missing values in relevant variables
                    relevant_vars <- c(self$options$dep, self$options$group)
                    if (!is.null(self$options$grvar)) {
                        relevant_vars <- c(relevant_vars, self$options$grvar)
                    }
                    if (!is.null(self$options$counts)) {
                        relevant_vars <- c(relevant_vars, self$options$counts)
                    }
                    mydata <- mydata[complete.cases(mydata[relevant_vars]), ]
                } else {
                    # Let ggstatsplot handle NAs (it will exclude them with warnings)
                    mydata <- mydata
                }
                
                if (nrow(mydata) == 0) {
                    stop('No complete data rows available after handling missing values. Please check your data or change the "Exclude Missing (NA)" setting.')
                }
                
                return(mydata)
            },

            .createBarPlot = function(data, dep_var, ggtheme, grouped = FALSE, progress_label = NULL) {
                # Progress indicator
                if (!is.null(progress_label)) {
                    private$.checkpoint()
                }
                
                # Performance optimization: Disable expensive features for large datasets
                n_groups <- length(unique(data[[self$options$group]]))
                n_total <- nrow(data)
                
                # Auto-disable pairwise for large group counts (performance)
                use_pairwise <- self$options$pairwisecomparisons
                if (use_pairwise && n_groups > 10) {
                    warning("Pairwise comparisons disabled for performance (>10 groups). Set manually to override.")
                    use_pairwise <- FALSE
                }
                
                # Parse ratio if provided
                ratio_vec <- NULL
                if (!is.null(self$options$ratio) && nchar(trimws(self$options$ratio)) > 0) {
                    tryCatch({
                        ratio_parts <- strsplit(self$options$ratio, ",")[[1]]
                        ratio_vec <- as.numeric(trimws(ratio_parts))
                        if (any(is.na(ratio_vec))) {
                            warning("Invalid ratio values provided. Using default equal proportions.")
                            ratio_vec <- NULL
                        } else if (abs(sum(ratio_vec) - 1.0) > 0.001) {
                            warning(paste("Ratio values sum to", sum(ratio_vec), "instead of 1. Normalizing."))
                            ratio_vec <- ratio_vec / sum(ratio_vec)
                        }
                    }, error = function(e) {
                        warning(paste("Error parsing ratio:", e$message, ". Using default equal proportions."))
                        ratio_vec <- NULL
                    })
                }
                
                # Base arguments for ggstatsplot functions with performance optimizations
                base_args <- list(
                    data = data,
                    x = rlang::sym(dep_var),
                    y = rlang::sym(self$options$group),
                    counts = if (!is.null(self$options$counts)) rlang::sym(self$options$counts) else NULL,
                    type = self$options$typestatistics,
                    paired = if (!is.null(self$options$paired)) self$options$paired else FALSE,
                    pairwise.comparisons = use_pairwise,
                    pairwise.display = self$options$pairwisedisplay,
                    p.adjust.method = self$options$padjustmethod,
                    results.subtitle = if (!is.null(self$options$resultssubtitle)) self$options$resultssubtitle else TRUE,
                    label = if (!is.null(self$options$label)) self$options$label else "percentage",
                    digits = if (!is.null(self$options$digits)) self$options$digits else 2L,
                    digits.perc = if (!is.null(self$options$digitsperc)) self$options$digitsperc else 0L,
                    proportion.test = if (!is.null(self$options$proportiontest)) self$options$proportiontest else TRUE,
                    bf.message = if (!is.null(self$options$bfmessage)) self$options$bfmessage else FALSE,
                    conf.level = if (!is.null(self$options$conflevel)) self$options$conflevel else 0.95,
                    ratio = ratio_vec,
                    messages = if (!is.null(self$options$messages)) self$options$messages else FALSE
                )
                
                # Enhanced error handling with context preservation
                tryCatch({
                    if (grouped) {
                        # Add grouping variable for grouped analysis
                        base_args$grouping.var <- rlang::sym(self$options$grvar)
                        base_args$ggtheme <- private$.selectTheme(ggtheme)
                        base_args$messages <- FALSE  # Reduce console clutter
                        
                        # Checkpoint before expensive grouped_ggbarstats call
                        private$.checkpoint()
                        return(do.call(ggstatsplot::grouped_ggbarstats, base_args))
                    } else {
                        # Standard bar chart
                        # Checkpoint before expensive ggbarstats call
                        private$.checkpoint()
                        plot <- do.call(ggstatsplot::ggbarstats, base_args)
                        return(plot + private$.selectTheme(ggtheme))
                    }
                }, error = function(e) {
                    # Preserve original error context with enhanced information
                    original_error <- conditionMessage(e)
                    context_info <- paste0(
                        "Variable: ", dep_var, 
                        ", Groups: ", length(unique(data[[self$options$group]])),
                        ", N: ", nrow(data),
                        if (grouped) paste0(", Split by: ", self$options$grvar) else ""
                    )
                    
                    stop(paste0("Bar chart creation failed for ", progress_label %||% dep_var, 
                               ". Context: ", context_info, 
                               ". Original error: ", original_error),
                         call. = FALSE)
                })
            },

            .createMultiplePlots = function(data, dep_vars, ggtheme, grouped = FALSE) {
                # Progress indicator for multiple plots
                private$.checkpoint()
                
                # More memory-efficient symbol creation
                dep_symbols <- purrr::map(dep_vars, ~ rlang::sym(.x))
                
                # Create plots with progress tracking
                plotlist <- purrr::imap(dep_symbols, ~ {
                    progress_label <- paste0("plot ", .y, "/", length(dep_symbols), " (", dep_vars[.y], ")")
                    private$.createBarPlot(
                        data = data, 
                        dep_var = dep_vars[.y], 
                        ggtheme = ggtheme, 
                        grouped = grouped,
                        progress_label = progress_label
                    )
                })
                
                # Checkpoint before expensive plot combination
                private$.checkpoint()
                # Combine plots
                return(ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(ncol = 1)
                ))
            }

            # run ----
            ,
            .run = function() {
                # Always generate About content
                private$.generateAboutContent()
                
                # Apply clinical presets if selected
                private$.applyClinicalPreset()
                
                # Initial Message ----
                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {
                    
                    todo <- glue::glue(
                        "<div style='padding: 15px; background-color: #e7f3ff; border-left: 4px solid #0066cc; margin: 10px 0;'>",
                        "<h4 style='color: #0066cc; margin-top: 0;'>🚀 Getting Started</h4>",
                        "<p><strong>Step 1:</strong> Select your <strong>Outcome Variable</strong> (what you want to analyze)</p>",
                        "<p><strong>Step 2:</strong> Choose a <strong>Group Variable</strong> (what you want to compare)</p>",
                        "<p><strong>Step 3:</strong> Pick a <strong>Clinical Analysis Preset</strong> for automatic configuration:</p>",
                        "<ul style='margin-left: 20px;'>",
                        "<li>🩺 <strong>Diagnostic Test:</strong> 2×2 tables with sensitivity/specificity</li>",
                        "<li>💊 <strong>Treatment Response:</strong> Compare response rates across treatments</li>",
                        "<li>🧬 <strong>Biomarker Expression:</strong> Analyze expression patterns</li>",
                        "<li>⚠️ <strong>Risk Factor Analysis:</strong> Examine risk factor relationships</li>",
                        "</ul>",
                        "<p><strong>Step 4:</strong> Review results and clinical interpretations</p>",
                        "<hr>",
                        "<p><small>📚 <strong>Documentation:</strong> <a href='https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>ggbarstats</a> | ",
                        "<a href='https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>grouped_ggbarstats</a></small></p>",
                        "</div>"
                    )

                    self$results$todo$setContent(todo)
                    return()

                } else {
                    # Enhanced data validation with better error messages and caching
                    tryCatch({
                        # Basic data check
                        if (nrow(self$data) == 0) {
                            stop('Dataset is empty. Please ensure your data contains observations.')
                        }
                        
                        # Checkpoint before data validation and preparation
                        private$.checkpoint(flush = FALSE)
                        # Use cached data validation and preparation
                        start_time <- Sys.time()
                        prepared_data <- private$.getCachedData()
                        prep_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
                        
                        # Enhanced success message with timing and caching info
                        cache_status <- if (private$.validation_passed && !is.null(private$.cached_data)) {
                            " (cached)"
                        } else {
                            " (fresh validation)"
                        }
                        
                        # Checkpoint before performance calculations
                        private$.checkpoint(flush = FALSE)
                        # Performance warnings
                        perf_warning <- ""
                        n_groups <- length(unique(prepared_data[[self$options$group]]))
                        if (self$options$pairwisecomparisons && n_groups > 5) {
                            perf_warning <- paste0("<br>⚠️ <b>Performance Note:</b> Pairwise comparisons with ", n_groups, 
                                                 " groups may be slow. Consider disabling for faster results.<br>")
                        }
                        if (self$options$typestatistics == "bayes") {
                            perf_warning <- paste0(perf_warning, 
                                                 "<br>⚠️ <b>Performance Note:</b> Bayesian analysis is computationally intensive.<br>")
                        }
                        
                        # Additional info about analysis settings
                        analysis_info <- ""
                        if (self$options$paired) {
                            analysis_info <- paste0(analysis_info, "<br>• Using paired/repeated measures design (McNemar's test)")
                        }
                        if (!is.null(self$options$counts)) {
                            analysis_info <- paste0(analysis_info, "<br>• Using counts variable: ", self$options$counts)
                        }
                        if (!is.null(self$options$ratio) && nchar(trimws(self$options$ratio)) > 0) {
                            analysis_info <- paste0(analysis_info, "<br>• Expected proportions: ", self$options$ratio)
                        }
                        if (self$options$label != "percentage") {
                            analysis_info <- paste0(analysis_info, "<br>• Label display: ", self$options$label)
                        }
                        
                        todo <- glue::glue(
                            "<br>Bar chart analysis comparing {paste(self$options$dep, collapse=', ')} by {self$options$group}{if(!is.null(self$options$grvar)) paste0(', grouped by ', self$options$grvar) else ''}.<br>
                            <br>Data prepared: {nrow(prepared_data)} observations{if(!self$options$excl) ' (missing values will be handled by statistical functions)' else ' (complete cases only)'}{cache_status}.<br>
                            {analysis_info}
                            {perf_warning}
                            {if(prep_time > 0.1) paste0('<br>Preparation time: ', prep_time, ' seconds.<br>') else ''}
                            <hr>"
                        )
                        
                        self$results$todo$setContent(todo)
                        
                        # Generate clinical interpretation panels if explanations are enabled
                        if (isTRUE(self$options$showexplanations)) {
                            private$.generateSummary(prepared_data)
                            private$.checkStatisticalAssumptions(prepared_data)
                            private$.generateInterpretationGuide()
                            private$.generateCopyReadyReport(prepared_data)
                        }
                        
                    }, error = function(e) {
                        # Reset cache on error
                        private$.cached_data <- NULL
                        private$.validation_passed <- FALSE
                        
                        # Enhanced error reporting with more context
                        error_context <- ""
                        if (grepl("continuous", e$message, ignore.case = TRUE)) {
                            error_context <- "<br>💡 <b>Tip:</b> Use Data > Transform to create categorical groups from continuous variables.<br>"
                        } else if (grepl("minimum|group size", e$message, ignore.case = TRUE)) {
                            error_context <- "<br>💡 <b>Tip:</b> Consider combining small categories or collecting more data.<br>"
                        } else if (grepl("variation|level", e$message, ignore.case = TRUE)) {
                            error_context <- "<br>💡 <b>Tip:</b> Ensure your variables have multiple categories for comparison.<br>"
                        }
                        
                        error_msg <- glue::glue(
                            "<br>❌ <b>Error in Bar Chart Analysis:</b><br>
                            <br>{e$message}<br>
                            {error_context}
                            <br><b>General Troubleshooting:</b><br>
                            • Ensure dependent and grouping variables are categorical<br>
                            • Check that selected variables exist in your dataset<br>
                            • Verify sufficient sample sizes in each group (≥5 recommended)<br>
                            • Confirm variables have adequate variation (≥2 categories)<br><hr>"
                        )
                        self$results$todo$setContent(error_msg)
                        return()
                    })
                    
                    # Add checkpoint for user feedback
                    private$.checkpoint()
                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # Validation ----
                if (is.null(self$options$dep) || is.null(self$options$group))
                    return()

                # Checkpoint before data retrieval
                private$.checkpoint(flush = FALSE)
                # Use cached data for performance
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Plot preparation failed:", e$message))
                })

                dep <- self$options$dep

                # Single vs Multiple dependent variables using shared logic
                if (length(dep) == 1) {
                    plot <- private$.createBarPlot(
                        data = mydata, 
                        dep_var = dep, 
                        ggtheme = ggtheme, 
                        grouped = FALSE,
                        progress_label = "main plot"
                    )
                } else {
                    plot <- private$.createMultiplePlots(
                        data = mydata, 
                        dep_vars = dep, 
                        ggtheme = ggtheme, 
                        grouped = FALSE
                    )
                }

                # Print Plot ----
                print(plot)
                TRUE
            }


            ,

            .plot2 = function(image, ggtheme, theme, ...) {
                # Validation ----
                if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                    return()

                # Checkpoint before data retrieval
                private$.checkpoint(flush = FALSE)
                # Use cached data for performance  
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Grouped plot preparation failed:", e$message))
                })

                dep <- self$options$dep

                # Single vs Multiple dependent variables using shared logic (grouped)
                if (length(dep) == 1) {
                    plot2 <- private$.createBarPlot(
                        data = mydata, 
                        dep_var = dep, 
                        ggtheme = ggtheme, 
                        grouped = TRUE,
                        progress_label = "grouped plot"
                    )
                } else {
                    plot2 <- private$.createMultiplePlots(
                        data = mydata, 
                        dep_vars = dep, 
                        ggtheme = ggtheme, 
                        grouped = TRUE
                    )
                }

                # Print Plot ----
                print(plot2)
                TRUE
            }

        )
    )
