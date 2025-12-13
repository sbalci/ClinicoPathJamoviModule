#' @title Pie Charts
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom digest digest
#' @importFrom rlang sym
#' @importFrom glue glue
#'

jjpiestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpiestatsClass",
    inherit = jjpiestatsBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .validation_passed = FALSE,
        .plotTheme = NULL,
        .effectiveOptions = NULL,

        # init ----

        .init = function() {

            self$results$plot1$setSize(650, 450)

            self$results$plot2$setSize(650, 450)


            if (!is.null(self$options$grvar) && !is.null(self$options$group)) {

                mydata <- self$data

                group <-  self$options$group

                num_levels_group <- nlevels(
                    as.factor(mydata[[group]])
                )

                self$results$plot4$setSize(num_levels_group * 600, 450)

            }




            if (!is.null(self$options$group) && !is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                group <-  self$options$group

                num_levels_group <- nlevels(
                    as.factor(mydata[[group]])
                )

                # self$results$plot3$setSize((num_levels + num_levels_group) * 600, 450)

                self$results$plot4$setSize((num_levels + num_levels_group) * 600, 450)

            }

        },

        # Clinical Enhancement Methods ----
        
        .effectiveOptionsList = function() {
            # Build a derived options list without mutating self$options
            opts <- list(
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
                typestatistics = self$options$typestatistics,
                counts = self$options$counts,
                ratio_raw = self$options$ratio,
                paired = self$options$paired %||% FALSE,
                label = self$options$label %||% "percentage",
                digits = self$options$digits %||% 2L,
                conflevel = self$options$conflevel %||% 0.95,
                proportiontest = self$options$proportiontest %||% TRUE,
                bfmessage = self$options$bfmessage %||% TRUE,
                messages = self$options$messages %||% FALSE,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme,
                clinicalpreset = self$options$clinicalpreset %||% "custom"
            )

            preset <- opts$clinicalpreset
            if (!is.null(preset) && preset != "custom") {
                if (preset == "diagnostic") {
                    opts$resultssubtitle <- TRUE
                    opts$proportiontest <- TRUE
                    opts$conflevel <- 0.95
                } else if (preset == "treatment") {
                    opts$typestatistics <- "parametric"
                    opts$resultssubtitle <- TRUE
                    opts$proportiontest <- TRUE
                } else if (preset == "biomarker") {
                    opts$typestatistics <- "robust"
                    opts$resultssubtitle <- TRUE
                    opts$label <- "both"
                }
            }

            # Parse ratio once; capture and muffle warnings to avoid noisy console output
            ratio_warnings <- character()
            opts$ratio <- withCallingHandlers(
                private$.parseRatio(opts$ratio_raw),
                warning = function(w) {
                    ratio_warnings <<- c(ratio_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            )
            if (length(ratio_warnings) > 0 && isTRUE(self$options$messages)) {
                ratio_warnings <- unique(ratio_warnings)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'ratioParseNotice',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(paste(ratio_warnings, collapse = "<br>"))
                self$results$insert(1, notice)
            }

            private$.effectiveOptions <- opts
            opts
        },
        
        .getPlotTheme = function() {
            if (is.null(private$.plotTheme)) {
                private$.plotTheme <- if (!isTRUE(self$options$originaltheme)) {
                    ggplot2::theme_bw()
                } else {
                    ggstatsplot::theme_ggstatsplot()
                }
            }
            private$.plotTheme
        },
        
        .generateClinicalPanels = function() {
            if (!isTRUE(self$options$showexplanations)) return()
            
            # Generate About content
            about_content <- private$.generateAboutContent()
            self$results$about$setContent(about_content)
            
            # Generate other clinical panels if we have data
            if (!is.null(self$options$dep)) {
                summary_content <- private$.generateSummaryContent()
                self$results$summary$setContent(summary_content)
                
                interpretation_content <- private$.generateInterpretationContent()
                self$results$interpretation$setContent(interpretation_content)
                
                report_content <- private$.generateReportContent()
                self$results$report$setContent(report_content)
                
                if (!is.null(self$options$group)) {
                    assumptions_content <- private$.generateAssumptionsContent()
                    self$results$assumptions$setContent(assumptions_content)
                }
            }
        },
        
        .generateAboutContent = function() {
            preset_info <- switch(self$options$clinicalpreset %||% "custom",
                "diagnostic" = .('This analysis is optimized for diagnostic test evaluation, focusing on sensitivity, specificity, and predictive values.'),
                "treatment" = .('This analysis is optimized for treatment response comparison across multiple treatment groups.'), 
                "biomarker" = .('This analysis is optimized for biomarker distribution analysis with robust statistical methods.'),
                .('This analysis uses custom settings for pie chart generation with statistical testing.')
            )
            
            glue::glue(
                "<h3>{title}</h3>
                <p><strong>{what}:</strong> {description}</p>
                <p><strong>{when}:</strong> {usage}</p>
                <p><strong>{preset_label}:</strong> {preset_info}</p>
                <p><strong>{output_label}:</strong> {outputs}</p>
                <hr>",
                title = .('Pie Chart Analysis'),
                what = .('What this analysis does'),
                description = .('Generates pie charts with statistical analysis to compare categorical variables across groups. Performs chi-square tests, Fisher\'s exact tests, or other appropriate statistical tests based on your data.'),
                when = .('When to use'),
                usage = .('Use when you want to visualize proportions of categorical outcomes and test for significant differences between groups. Ideal for diagnostic test results, treatment responses, or biomarker categories.'),
                preset_label = .('Current configuration'),
                output_label = .('What you\'ll get'),
                outputs = .('Interactive pie charts with statistical test results, confidence intervals, and effect sizes. Optional grouped analysis for complex study designs.')
            )
        },
        
        .generateSummaryContent = function() {
            # Basic summary based on current selections
            dep_info <- if (!is.null(self$options$dep)) {
                paste(.('Analyzing variable:'), self$options$dep)
            } else { .('No outcome variable selected') }
            
            group_info <- if (!is.null(self$options$group) && self$options$group != "") {
                paste(.('Comparing across groups:'), self$options$group)
            } else { .('No grouping variable - single pie chart') }
            
            method_info <- paste(.('Statistical method:'), tools::toTitleCase(self$options$typestatistics))
            
            glue::glue(
                "<h4>{summary_title}</h4>
                <p>• {dep_info}</p>
                <p>• {group_info}</p>
                <p>• {method_info}</p>
                <p>• {sample_info}</p>
                <hr>",
                summary_title = .('Analysis Configuration'),
                sample_info = paste(.('Sample size:'), nrow(self$data), .('observations'))
            )
        },
        
        .generateAssumptionsContent = function() {
            warnings_list <- c()

            # Check for small sample sizes
            if (!is.null(self$options$group) && self$options$group %in% names(self$data)) {
                tryCatch({
                    # CRITICAL FIX #4: Use weighted contingency table if counts variable present
                    counts_var <- self$options$counts
                    if (!is.null(counts_var) && counts_var != "" && counts_var %in% names(self$data)) {
                        # Weighted contingency table using xtabs
                        formula_str <- paste0(counts_var, " ~ ", self$options$dep, " + ", self$options$group)
                        contingency_table <- xtabs(as.formula(formula_str), data = self$data)
                    } else {
                        # Unweighted contingency table
                        contingency_table <- table(self$data[[self$options$dep]], self$data[[self$options$group]])
                    }
                    expected_counts <- suppressWarnings(chisq.test(contingency_table)$expected)
                    
                    if (any(expected_counts < 5)) {
                        warnings_list <- c(warnings_list, 
                            paste("⚠️", .('Expected cell counts < 5 detected. Consider using Fisher\'s exact test (nonparametric option) for more reliable results.'))
                        )
                    }
                    
                    if (any(contingency_table < 2)) {
                        warnings_list <- c(warnings_list,
                            paste("⚠️", .('Some categories have very few observations. Consider combining categories or collecting more data.'))
                        )
                    }
                }, error = function(e) {
                    # Silently continue if table creation fails
                })
            }
            
            # General assumptions
            assumptions <- c(
                paste("✓", .('Data should be categorical (factors or characters)')),
                paste("✓", .('Observations should be independent')),
                paste("✓", .('For statistical tests: adequate sample size in each category'))
            )
            
            warnings_section <- if (length(warnings_list) > 0) {
                paste0("<h5>", .('Warnings'), "</h5>\n<ul>\n",
                      paste0("<li>", warnings_list, "</li>", collapse = "\n"),
                      "\n</ul>\n")
            } else {
                paste0("<p>✓ ", .('All basic assumptions appear to be met.'), "</p>\n")
            }
            
            glue::glue(
                "<h4>{assumptions_title}</h4>
                <h5>{general_title}</h5>
                <ul>
                {paste0('<li>', assumptions, '</li>', collapse = '\n')}
                </ul>
                {warnings_section}
                <hr>",
                assumptions_title = .('Statistical Assumptions & Warnings'),
                general_title = .('General Requirements')
            )
        },
        
        .generateInterpretationContent = function() {
            method_guidance <- switch(self$options$typestatistics,
                "parametric" = .('Chi-square test results show whether group differences are statistically significant. Look for p-values < 0.05 for significant associations.'),
                "nonparametric" = .('Fisher\'s exact test provides precise p-values for small samples. Recommended when expected cell counts are < 5.'),
                "robust" = .('Robust methods provide reliable results even with outliers or non-normal distributions.'),
                "bayes" = .('Bayesian analysis provides evidence for or against group differences. Bayes factors > 3 suggest evidence for differences.'),
                .('Statistical analysis will be performed based on your data characteristics.')
            )
            
            clinical_context <- switch(self$options$clinicalpreset %||% "custom",
                "diagnostic" = .('For diagnostic tests: Focus on sensitivity (true positive rate) and specificity (true negative rate). Consider positive and negative predictive values for clinical decision-making.'),
                "treatment" = .('For treatment response: Look for significant differences between treatment arms. Consider clinical significance alongside statistical significance.'),
                "biomarker" = .('For biomarker analysis: Examine distribution patterns across patient groups. Consider biological relevance of observed differences.'),
                .('Interpret results in the context of your specific research question and clinical setting.')
            )
            
            glue::glue(
                "<h4>{interpretation_title}</h4>
                <p><strong>{method_title}:</strong> {method_guidance}</p>
                <p><strong>{clinical_title}:</strong> {clinical_context}</p>
                <p><strong>{general_title}:</strong> {general_guidance}</p>
                <hr>",
                interpretation_title = .('How to Interpret Your Results'),
                method_title = .('Statistical Method'),
                clinical_title = .('Clinical Context'),
                general_title = .('General Guidance'),
                general_guidance = .('Pie charts show proportions visually - larger slices represent higher frequencies. Statistical tests determine if observed differences are likely due to chance or represent true group differences.')
            )
        },
        
        .generateReportContent = function() {
            method_name <- switch(self$options$typestatistics,
                "parametric" = .('chi-square test'),
                "nonparametric" = .('Fisher\'s exact test'), 
                "robust" = .('robust statistical analysis'),
                "bayes" = .('Bayesian analysis'),
                .('statistical analysis')
            )
            
            sample_description <- if (!is.null(self$options$group) && self$options$group != "") {
                paste(.('We compared {outcome} distributions across {groups} using {method}.'),
                     outcome = self$options$dep, groups = self$options$group, method = method_name)
            } else {
                paste(.('We analyzed the distribution of {outcome} using descriptive statistics.'),
                     outcome = self$options$dep)
            }
            
            glue::glue(
                "<h4>{report_title}</h4>
                <div style='background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;'>
                <p><strong>{methods_title}:</strong></p>
                <p>{sample_description} {additional_details}</p>
                <p><strong>{results_title}:</strong></p>
                <p>{results_placeholder}</p>
                </div>
                <p><small>{note}</small></p>
                <hr>",
                report_title = .('Copy-Ready Report Template'),
                methods_title = .('Methods'),
                results_title = .('Results'),
                additional_details = .('Statistical significance was set at p < 0.05. All analyses were performed using jamovi statistical software.'),
                results_placeholder = .('[Results will be automatically filled when analysis is complete]'),
                note = .('Copy the text above and modify as needed for your manuscript or report.')
            )
        },
        
        # Helper Methods for Validation ----
        
        .validateVariables = function() {
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar
            
            # Check if required variables exist
            all_vars <- c(dep, group, grvar)
            all_vars <- all_vars[!is.null(all_vars) & all_vars != ""]
            
            missing_vars <- all_vars[!all_vars %in% names(self$data)]
            if (length(missing_vars) > 0) {
                stop(paste(.('Variables not found in data:'), paste(missing_vars, collapse = ', ')))
            }
            
            # Validate that variables are appropriate for pie charts (categorical)
            for (var in all_vars) {
                if (!is.null(var) && var != "") {
                    var_class <- class(self$data[[var]])
                    if (!any(c("factor", "character", "logical") %in% var_class)) {
                        # Try to convert numeric to factor if it has few unique values
                        if (is.numeric(self$data[[var]])) {
                            unique_vals <- length(unique(self$data[[var]], na.rm = TRUE))
                            if (unique_vals > 10) {
                                stop(paste(.('Variable \'{var}\' appears to be continuous ({count} unique values). Pie charts are for categorical data. Consider converting to groups first.'), 
                                    list(var = var, count = unique_vals)))
                            }
                        }
                    }
                }
            }
            
            # Enhanced validation for statistical tests
            private$.validateStatisticalRequirements(dep, group)

            # CRITICAL FIX #2: Validate counts variable
            counts_var <- self$options$counts
            if (!is.null(counts_var) && counts_var != "") {
                counts_validation <- private$.validateCounts(self$data, counts_var)
                if (!counts_validation$valid) {
                    stop(counts_validation$message)
                }
            }

            return(TRUE)
        },
        
        .validateStatisticalRequirements = function(dep, group) {
            # Check minimum group sizes for statistical tests when group variable is present
            if (!is.null(group) && group != "" && group %in% names(self$data)) {
                group_sizes <- table(self$data[[group]], useNA = "no")

                if (any(group_sizes < 5)) {
                    small_groups <- names(group_sizes[group_sizes < 5])
                    group_details <- paste(paste(small_groups, ':', group_sizes[small_groups]), collapse = ', ')

                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'smallGroupSizes',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        'Small group sizes detected: %s. Chi-square tests require minimum 5 observations per group for reliable results.',
                        group_details
                    ))
                    self$results$insert(999, notice)
                }

                if (length(group_sizes) < 2) {
                    stop(.('Grouping variable must have at least 2 categories for comparison.'))
                }
            }

            # Check dependent variable has sufficient variation
            if (!is.null(dep) && dep != "" && dep %in% names(self$data)) {
                dep_levels <- table(self$data[[dep]], useNA = "no")
                if (length(dep_levels) < 2) {
                    stop(paste(.('Variable \'{var}\' has insufficient variation (only {count} level). Need at least 2 categories for meaningful pie chart.'),
                              list(var = dep, count = length(dep_levels))))
                }
            }
        },

        .validateCounts = function(data, counts_var) {
            # CRITICAL FIX #2: Validate counts variable for aggregated data

            # Check if variable exists
            if (!counts_var %in% names(data)) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" not found in data. Available variables: %s'),
                        counts_var,
                        paste(names(data), collapse = ", ")
                    )
                ))
            }

            # Check if variable is numeric
            if (!is.numeric(data[[counts_var]])) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" must be numeric. Found type: %s. The counts variable should contain frequency counts for aggregated/tabulated data.'),
                        counts_var,
                        class(data[[counts_var]])[1]
                    )
                ))
            }

            # Check for negative values
            if (any(data[[counts_var]] < 0, na.rm = TRUE)) {
                neg_count <- sum(data[[counts_var]] < 0, na.rm = TRUE)
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" contains %d negative values. Frequency counts must be non-negative (>= 0).'),
                        counts_var,
                        neg_count
                    )
                ))
            }

            # Check if all values are NA
            if (all(is.na(data[[counts_var]]))) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" contains only missing values (NA). Cannot compute statistics with no valid counts.'),
                        counts_var
                    )
                ))
            }

            # Check if total count is zero after removing NAs
            total_count <- sum(data[[counts_var]], na.rm = TRUE)
            if (total_count == 0) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" sums to zero. Need at least one observation with positive count.'),
                        counts_var
                    )
                ))
            }

            # All validations passed
            return(list(valid = TRUE, message = .('Counts variable validated successfully.')))
        },

        .checkFisherNeeded = function(data, dep_var, group_var, counts_var = NULL) {
            # CRITICAL FIX #5: Check if Fisher's exact test should be used
            # Returns list with use_fisher, low_count_cells, total_cells, pct_low

            if (is.null(group_var) || group_var == "") {
                return(list(use_fisher = FALSE, low_count_cells = 0, total_cells = 0, pct_low = 0))
            }

            tryCatch({
                # Create contingency table (weighted if counts present)
                if (!is.null(counts_var) && counts_var != "" && counts_var %in% names(data)) {
                    formula_str <- paste0(counts_var, " ~ ", dep_var, " + ", group_var)
                    cont_table <- xtabs(as.formula(formula_str), data = data)
                } else {
                    cont_table <- table(data[[dep_var]], data[[group_var]])
                }

                # Chi-square test requires at least 2x2 table
                if (nrow(cont_table) < 2 || ncol(cont_table) < 2) {
                    return(list(use_fisher = FALSE, low_count_cells = 0, total_cells = 0, pct_low = 0))
                }

                # Calculate expected counts
                expected <- suppressWarnings(chisq.test(cont_table)$expected)

                # Count cells with expected count < 5
                low_count_cells <- sum(expected < 5)
                total_cells <- length(expected)
                pct_low <- 100 * low_count_cells / total_cells

                # Fisher's exact recommended when >20% of cells have expected count < 5
                # For 2x2 tables, use if ANY cell < 5
                use_fisher <- if (total_cells == 4) {  # 2x2 table
                    low_count_cells > 0
                } else {
                    pct_low > 20
                }

                return(list(
                    use_fisher = use_fisher,
                    low_count_cells = low_count_cells,
                    total_cells = total_cells,
                    pct_low = pct_low
                ))

            }, error = function(e) {
                return(list(use_fisher = FALSE, low_count_cells = 0, total_cells = 0, pct_low = 0))
            })
        },

        .validatePairedData = function(data, dep_var, group_var) {
            # CRITICAL FIX #3: Validate paired data for McNemar test
            # McNemar test requires exactly 2x2 contingency table

            if (is.null(group_var) || group_var == "") {
                return(list(
                    valid = FALSE,
                    message = .('McNemar test requires a grouping variable (e.g., Pre vs Post, Treatment A vs B). No grouping variable specified.')
                ))
            }

            # Create contingency table (use weighted if counts variable present)
            counts_var <- self$options$counts
            if (!is.null(counts_var) && counts_var != "" && counts_var %in% names(data)) {
                # Weighted contingency table
                formula_str <- paste0(counts_var, " ~ ", dep_var, " + ", group_var)
                cross_table <- xtabs(as.formula(formula_str), data = data)
            } else {
                # Unweighted contingency table
                cross_table <- table(data[[dep_var]], data[[group_var]])
            }

            # Check if table is exactly 2x2
            if (!all(dim(cross_table) == c(2, 2))) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('McNemar test requires a 2×2 contingency table. Your data has %d×%d levels (%s: %d levels, %s: %d levels). For paired data with more than 2 categories, use Cochran Q test or marginal homogeneity test instead. Consider dichotomizing your outcome variable or use parametric/nonparametric options instead of paired analysis.'),
                        nrow(cross_table), ncol(cross_table),
                        dep_var, nrow(cross_table),
                        group_var, ncol(cross_table)
                    )
                ))
            }

            # Check minimum sample size (McNemar requires at least 10 paired observations)
            total_n <- sum(cross_table)
            if (total_n < 10) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('McNemar test requires at least 10 paired observations for reliable results. Your data has %d paired observations. Consider collecting more data or using Fisher exact test (nonparametric option) instead.'),
                        total_n
                    )
                ))
            }

            # Check for zero marginals (can cause computational issues)
            row_sums <- rowSums(cross_table)
            col_sums <- colSums(cross_table)

            if (any(row_sums == 0) || any(col_sums == 0)) {
                return(list(
                    valid = FALSE,
                    message = .('McNemar test requires non-zero marginal totals. Your contingency table has at least one row or column with zero observations. Check your data for complete coverage of all category combinations.')
                ))
            }

            # All validations passed
            return(list(
                valid = TRUE,
                message = sprintf(
                    .('Data structure validated for McNemar test: 2×2 table with %d paired observations.'),
                    total_n
                )
            ))
        },

        .checkExtremePrevalence = function(data, dep_var, group_var) {
            # ENHANCEMENT #2: Warn about extreme prevalence affecting PPV/NPV generalizability
            # Only relevant for diagnostic preset with 2x2 contingency tables

            opts <- private$.effectiveOptionsList()

            # Only check for diagnostic preset
            if (is.null(opts$clinicalpreset) || opts$clinicalpreset != "diagnostic") {
                return()
            }

            # Must have group variable for prevalence calculation
            if (is.null(group_var) || group_var == "" || !group_var %in% names(data)) {
                return()
            }

            # Create contingency table (use weighted if counts present)
            counts_var <- self$options$counts
            if (!is.null(counts_var) && counts_var != "" && counts_var %in% names(data)) {
                formula_str <- paste0(counts_var, " ~ ", dep_var, " + ", group_var)
                cont_table <- xtabs(as.formula(formula_str), data = data)
            } else {
                cont_table <- table(data[[dep_var]], data[[group_var]])
            }

            # Only check for 2x2 tables (diagnostic test scenario)
            if (!all(dim(cont_table) == c(2, 2))) {
                return()
            }

            # Calculate prevalence and report which level is used
            group_totals <- colSums(cont_table)
            total_n <- sum(group_totals)

            # Assuming first column/level represents "diseased/positive"
            diseased_level <- colnames(cont_table)[1]
            prevalence <- group_totals[1] / total_n

            # Warn if prevalence is extreme (<10% or >90%)
            if (prevalence < 0.10 || prevalence > 0.90) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'extremePrevalence',
                    type = jmvcore::NoticeType$INFO
                )

                direction <- if (prevalence < 0.10) "low" else "high"
                notice$setContent(sprintf(
                    'Disease prevalence (level "%s") in this dataset is %.1f%% (%s prevalence setting). Positive Predictive Value (PPV) and Negative Predictive Value (NPV) are highly dependent on prevalence and may not generalize to populations with substantially different disease rates. Consider reporting sensitivity, specificity, and likelihood ratios which are less affected by prevalence.',
                    diseased_level,
                    prevalence * 100,
                    direction
                ))
                self$results$insert(999, notice)
            }
        },

        .getCachedData = function() {
            # CRITICAL FIX #1: Include actual data content in hash (not just dimensions/names)
            # CRITICAL FIX: Defensive check for dep variable
            if (is.null(self$options$dep) || length(self$options$dep) == 0 ||
                (length(self$options$dep) == 1 && self$options$dep == "")) {
                stop(.('Dependent variable is required but not properly set'))
            }

            opts <- private$.effectiveOptionsList()

            # Build list of relevant columns for hash
            relevant_cols <- c(opts$dep)
            if (!is.null(opts$group) && length(opts$group) > 0 && opts$group != "") {
                relevant_cols <- c(relevant_cols, opts$group)
            }
            if (!is.null(opts$grvar) && length(opts$grvar) > 0 && opts$grvar != "") {
                relevant_cols <- c(relevant_cols, opts$grvar)
            }
            if (!is.null(opts$counts) && length(opts$counts) > 0 && opts$counts != "") {
                relevant_cols <- c(relevant_cols, opts$counts)
            }

            # Filter to columns that actually exist
            relevant_cols <- relevant_cols[relevant_cols %in% names(self$data)]

            # Additional safety check
            if (length(relevant_cols) == 0) {
                stop(.('No valid analysis variables found in dataset. Please check variable names.'))
            }

            # Hash actual data content (not just structure)
            data_content_hash <- if (length(relevant_cols) > 0) {
                digest::digest(
                    self$data[, relevant_cols, drop = FALSE],
                    algo = "md5"
                )
            } else {
                digest::digest(dim(self$data), algo = "md5")
            }

            # Create hash of current data and options state
            current_hash <- digest::digest(list(
                data_dim = dim(self$data),
                data_names = names(self$data),
                data_content = data_content_hash,  # ✅ NOW INCLUDES ACTUAL DATA VALUES
                options = opts
            ), algo = "md5")

            # Return cached data if hash matches and validation passed
            if (!is.null(private$.processedData) &&
                !is.null(private$.data_hash) &&
                private$.data_hash == current_hash &&
                private$.validation_passed) {
                return(private$.processedData)
            }

            # Validate and prepare fresh data
            # Checkpoint before validation (statistical requirements checking)
            private$.checkpoint(flush = FALSE)
            private$.validateVariables()
            private$.processedData <- private$.prepareData()
            private$.data_hash <- current_hash
            private$.validation_passed <- TRUE

            return(private$.processedData)
        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            # CRITICAL FIX: Defensive check for dep variable
            if (is.null(self$options$dep) || length(self$options$dep) == 0 ||
                (length(self$options$dep) == 1 && self$options$dep == "")) {
                stop(.('Dependent variable is required but not properly set'))
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>{msg}<br><hr>", msg = .('Processing data for pie chart analysis...'))
            )

            mydata <- self$data

            # Selective NA omission - only remove rows with NAs in relevant variables
            # Build list of relevant columns
            relevant_cols <- c(self$options$dep)
            if (!is.null(self$options$group) && length(self$options$group) > 0 && self$options$group != "") {
                relevant_cols <- c(relevant_cols, self$options$group)
            }
            if (!is.null(self$options$grvar) && length(self$options$grvar) > 0 && self$options$grvar != "") {
                relevant_cols <- c(relevant_cols, self$options$grvar)
            }
            if (!is.null(self$options$counts) && length(self$options$counts) > 0 && self$options$counts != "") {
                relevant_cols <- c(relevant_cols, self$options$counts)
            }

            # Safety check
            if (length(relevant_cols) == 0) {
                stop(.('No valid analysis variables specified'))
            }

            # Remove rows with NAs only in relevant columns
            n_before <- nrow(mydata)
            private$.checkpoint()
            # Keep data as data.frame even when only one relevant column is present
            mydata <- mydata[complete.cases(mydata[relevant_cols]), , drop = FALSE]
            n_after <- nrow(mydata)

            # Report dropped rows
            if (n_before > n_after) {
                n_dropped <- n_before - n_after
                pct_dropped <- round(100 * n_dropped / n_before, 1)
                self$results$todo$setContent(
                    glue::glue(
                        "<br>ℹ️ <b>{info_title}:</b> {n_dropped} {rows_msg} ({pct_dropped}%) {excluded_msg} {vars_msg}: {var_list}.<br><hr>",
                        info_title = .('Info'),
                        rows_msg = ngettext(n_dropped, 'row', 'rows'),
                        excluded_msg = .('excluded due to missing values in'),
                        vars_msg = .('analysis variables'),
                        var_list = paste(relevant_cols, collapse = ', ')
                    )
                )
            }

            if (nrow(mydata) == 0 || nrow(mydata) < 3) {
                stop(.('No complete data rows available after handling missing values. Please check your data for the selected variables.'))
            }

            return(mydata)
        },

        # Helper method to parse ratio string
        .parseRatio = function(ratio_string) {
            if (is.null(ratio_string) || ratio_string == "") return(NULL)
            
            tryCatch({
                ratios <- as.numeric(strsplit(ratio_string, ",")[[1]])
                if (any(is.na(ratios))) {
                    warning(.('Invalid ratio specification - contains non-numeric values. Using equal proportions.'))
                    return(NULL)
                }
                if (abs(sum(ratios) - 1) > 0.001) {
                    warning(paste(.('Ratios sum to {sum} but should sum to 1. Using equal proportions.'),
                                list(sum = round(sum(ratios), 3))))
                    return(NULL)
                }
                if (any(ratios <= 0)) {
                    warning(.('Ratios must be positive. Using equal proportions.'))
                    return(NULL)
                }
                return(ratios)
            }, error = function(e) {
                warning(paste(.('Error parsing ratio: {error}. Using equal proportions.'),
                             list(error = e$message)))
                return(NULL)
            })
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            if (!is.null(private$.processedOptions) && !force_refresh) {
                return(private$.processedOptions)
            }

            # Prepare options with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>{msg}<br><hr>", msg = .('Preparing pie chart analysis options...'))
            )

            # Defensive validation: ensure dep is not zero-length
            if (is.null(self$options$dep) || length(self$options$dep) == 0 || (length(self$options$dep) == 1 && self$options$dep == "")) {
                stop(.('Dependent variable is not properly set'))
            }
            
            # Build effective options (includes preset and parsed ratio)
            opts <- private$.effectiveOptionsList()
            
            private$.processedOptions <- opts
            return(opts)
        }



        # run ----
        ,
        .run = function() {

            # Generate clinical interpretation panels
            private$.generateClinicalPanels()

            # Initial Message ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) ||
                 length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") ) {

                # TODO ----
                todo <- glue::glue(
                "<br>{welcome}
                <br><br>
                {description}
                <br><br>
                {documentation}
                <br>
                {citation}
                <br><hr>",
                welcome = .('Welcome to ClinicoPath'),
                description = .('This tool will help you generate Pie Charts with statistical analysis.'),
                documentation = .('This function uses ggplot2 and ggstatsplot packages. See documentations <a href = \'https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html\' target=\'_blank\'>ggpiestats</a> and <a href = \'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html\' target=\'_blank\'>grouped_ggpiestats</a>.'),
                citation = .('Please cite jamovi and the packages as given below.')
                )

                self$results$todo$setContent(todo)
                return()

            } else {
                
                # Enhanced data validation with better error messages and caching
                tryCatch({
                    # Basic data check
                    if (nrow(self$data) == 0) {
                        stop(.('Dataset is empty. Please ensure your data contains observations.'))
                    }
                    
            # Use cached data validation and preparation
            start_time <- Sys.time()
            # Checkpoint before data validation and preparation
            private$.checkpoint()
            prepared_data <- private$.getCachedData()
            prep_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
                    
                    # Enhanced success message with timing and caching info
                    cache_status <- if (private$.validation_passed && !is.null(private$.processedData)) {
                        " (cached)"
                    } else {
                        " (fresh validation)"
                    }
                    
                    # Performance warnings
                    perf_warning <- ""
                    if (self$options$typestatistics == "bayes") {
                        perf_warning <- paste0("<br>⚠️ <b>", .('Performance Note:'), "</b> ", .('Bayesian analysis is computationally intensive.'), "<br>")
                    }
                    
                    # Build status message
                    dep_info <- paste(.('Variable:'), self$options$dep)
                    group_info <- if (!is.null(self$options$group) && self$options$group != "") {
                        paste(",", .('grouped by'), self$options$group)
                    } else { "" }
                    split_info <- if (!is.null(self$options$grvar) && self$options$grvar != "") {
                        paste(",", .('split by'), self$options$grvar)
                    } else { "" }
                    
                    todo <- glue::glue(
                        "<br>{ready_msg} {dep_info}{group_info}{split_info}.<br>
                        <br>{data_msg}: {nrow(prepared_data)} {obs_msg}{cache_status}.<br>
                        <br>{method_msg}: {tools::toTitleCase(self$options$typestatistics)} {analysis_msg}.<br>
                        {perf_warning}
                        {if(prep_time > 0.1) paste0('<br>', prep_time_msg, ': ', prep_time, ' ', seconds_msg, '.<br>') else ''}
                        <hr>",
                        ready_msg = .('Pie chart analysis ready'),
                        data_msg = .('Data prepared'),
                        obs_msg = .('observations'),
                        method_msg = .('Statistical method'),
                        analysis_msg = .('analysis'),
                        prep_time_msg = .('Preparation time'),
                        seconds_msg = .('seconds')
                    )
                    
                    self$results$todo$setContent(todo)
                    
                }, error = function(e) {
                    # Reset cache on error
                    private$.processedData <- NULL
                    private$.validation_passed <- FALSE
                    
                    # Enhanced error reporting with more context
                    error_context <- ""
                    if (grepl("continuous", e$message, ignore.case = TRUE)) {
                        error_context <- paste0("<br>💡 <b>", .('Tip:'), "</b> ", .('Use Data > Transform to create categorical groups from continuous variables.'), "<br>")
                    } else if (grepl("minimum|group size", e$message, ignore.case = TRUE)) {
                        error_context <- paste0("<br>💡 <b>", .('Tip:'), "</b> ", .('Consider combining small categories or collecting more data.'), "<br>")
                    } else if (grepl("variation|level", e$message, ignore.case = TRUE)) {
                        error_context <- paste0("<br>💡 <b>", .('Tip:'), "</b> ", .('Ensure your variables have multiple categories for meaningful pie charts.'), "<br>")
                    }
                    
                    error_msg <- glue::glue(
                        "<br>❌ <b>{error_title}:</b><br>
                        <br>{e$message}<br>
                        {error_context}
                        <br><b>{troubleshoot_title}:</b><br>
                        • {check1}<br>
                        • {check2}<br>
                        • {check3}<br>
                        • {check4}<br><hr>",
                        error_title = .('Error in Pie Chart Analysis'),
                        troubleshoot_title = .('General Troubleshooting'),
                        check1 = .('Ensure dependent variable is categorical'),
                        check2 = .('Check that selected variables exist in your dataset'),
                        check3 = .('Verify sufficient sample sizes in each category (≥5 recommended)'),
                        check4 = .('Confirm variables have adequate variation (≥2 categories)')
                    )
                    self$results$todo$setContent(error_msg)
                    stop(e)
                })
                
                # Add checkpoint for user feedback
                private$.checkpoint()
            }
        }


        # the plot1 function ----


        ,
        .plot1 = function(image, ggtheme, theme, ...) {

            # Validation ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) ||
                 length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
            }, error = function(e) {
                stop(glue::glue('Data preparation failed in plot1: {e$message}'))
            })

            tryCatch({
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(glue::glue('Options preparation failed in plot1: {e$message}'))
            })

            dep <- options_data$dep


            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html



            # Get counts variable if specified
            counts_var <- if (!is.null(options_data$counts) && options_data$counts != "") {
                options_data$counts
            } else {
                NULL
            }

            # CRITICAL FIX #3: Validate paired data before McNemar test
            if (options_data$paired) {
                # plot1 has no grouping variable (y=NULL), so paired doesn't apply
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'pairedWithoutGroup',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(
                    .('Paired/repeated measures analysis requires a grouping variable (e.g., Pre vs Post). Single variable pie charts cannot use paired option. Please add a grouping variable or disable the paired option.')
                )
                self$results$insert(0, notice)
                return()
            }

            # Checkpoint before expensive statistical computation
            private$.checkpoint()

            plot1 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = NULL,
                    counts = counts_var,
                    ratio = options_data$ratio,
                    paired = FALSE,  # Always FALSE for single variable
                    type = options_data$typestatistics,
                    label = options_data$label,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = options_data$bfmessage,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = options_data$conflevel,
                    legend.title = NULL,
                    digits = options_data$digits,
                    proportion.test = options_data$proportiontest,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    results.subtitle = options_data$resultssubtitle
                    )



            # Apply cached theme for better performance
            plot1 <- plot1 + private$.getPlotTheme()

            # Print Plot1 ----

            print(plot1)
            TRUE

        }


        # the plot2 function ----


        , .plot2 = function(image, ggtheme, theme, ...) {

            # Validation ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) || length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") ||
                 is.null(self$options$group) || length(self$options$group) == 0 ||
                 (length(self$options$group) == 1 && self$options$group == "") )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(glue::glue('Plot preparation failed: {e$message}'))
            })
            
            dep <- options_data$dep
            group <- options_data$group



            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html

            # Get counts variable if specified
            counts_var <- if (!is.null(options_data$counts) && options_data$counts != "") {
                options_data$counts
            } else {
                NULL
            }

            # ENHANCEMENT #6: Enhanced ratio parsing with notice-based feedback
                ratio_vec <- options_data$ratio

            # CRITICAL FIX #3: Validate paired data before McNemar test
            if (options_data$paired) {
                validation <- private$.validatePairedData(mydata, dep, group)
                if (!validation$valid) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'pairedValidationError',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(validation$message)
                    self$results$insert(0, notice)
                    return()
                }
            }

            # CRITICAL FIX #5: Auto-switch to Fisher's exact if chi-square assumptions violated
            override_type <- options_data$typestatistics
            if (options_data$typestatistics == "parametric" && !options_data$paired) {
                fisher_check <- private$.checkFisherNeeded(mydata, dep, group, counts_var)
                if (fisher_check$use_fisher) {
                    override_type <- "nonparametric"  # Fisher's exact for 2x2
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'fisherAutoSwitch',
                        type = jmvcore::NoticeType$INFO
                    )
                    notice$setContent(sprintf(
                        .('Automatically switched to Fisher\'s Exact Test: %d of %d cells (%.1f%%) have expected counts < 5 (chi-square assumption violated). For 2×2 tables, Fisher\'s exact test provides more reliable p-values when expected counts are low.'),
                        fisher_check$low_count_cells,
                        fisher_check$total_cells,
                        fisher_check$pct_low
                    ))
                    self$results$insert(1, notice)
                }
            }

            # ENHANCEMENT #2: Check for extreme prevalence in diagnostic preset
            private$.checkExtremePrevalence(mydata, dep, group)

            # Checkpoint before expensive statistical computation
            private$.checkpoint()

            plot2 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = counts_var,
                    ratio = ratio_vec,  # Use enhanced parsed ratio
                    paired = options_data$paired,
                    type = override_type,  # Use potentially overridden type
                    label = options_data$label,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = options_data$bfmessage,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = options_data$conflevel,
                    legend.title = NULL,
                    digits = options_data$digits,
                    proportion.test = options_data$proportiontest,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    results.subtitle = options_data$resultssubtitle
                )


            originaltheme <- options_data$originaltheme

            if (!originaltheme) {
                plot2 <- plot2 + ggtheme
            } else {
                plot2 <- plot2 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }


            # Print Plot2 ----
            print(plot2)
            TRUE
        }


        # the plot4 function ----




        , .plot4 = function(image, ggtheme, theme, ...) {

            # Validation ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) || length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") ||
                 is.null(self$options$group) || length(self$options$group) == 0 ||
                 (length(self$options$group) == 1 && self$options$group == "") ||
                 is.null(self$options$grvar) || length(self$options$grvar) == 0 ||
                 (length(self$options$grvar) == 1 && self$options$grvar == "") )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(glue::glue('Grouped plot preparation failed: {e$message}'))
            })
            
            dep <- options_data$dep
            group <- options_data$group
            grvar <- options_data$grvar



            # grouped_ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html

            if ( !is.null(grvar) ) {

                selected_theme <- private$.getPlotTheme()


                # Get counts variable if specified
                counts_var <- if (!is.null(options_data$counts) && options_data$counts != "") {
                    !!rlang::sym(options_data$counts)
                } else {
                    NULL
                }

                # ENHANCEMENT #6: Enhanced ratio parsing with notice-based feedback
                ratio_vec <- NULL
            ratio_vec <- options_data$ratio

                # CRITICAL FIX #3: Validate paired data before McNemar test
                if (options_data$paired) {
                    validation <- private$.validatePairedData(mydata, dep, group)
                    if (!validation$valid) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'pairedValidationError',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(validation$message)
                        self$results$insert(0, notice)
                        return()
                    }
                }

                # CRITICAL FIX #5: Auto-switch to Fisher's exact if chi-square assumptions violated
                override_type <- options_data$typestatistics
                if (options_data$typestatistics == "parametric" && !options_data$paired) {
                    fisher_check <- private$.checkFisherNeeded(mydata, dep, group, counts_var)
                    if (fisher_check$use_fisher) {
                        override_type <- "nonparametric"  # Fisher's exact for 2x2
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'fisherAutoSwitch',
                            type = jmvcore::NoticeType$INFO
                        )
                        notice$setContent(sprintf(
                            .('Automatically switched to Fisher\'s Exact Test: %d of %d cells (%.1f%%) have expected counts < 5 (chi-square assumption violated). For 2×2 tables, Fisher\'s exact test provides more reliable p-values when expected counts are low.'),
                            fisher_check$low_count_cells,
                            fisher_check$total_cells,
                            fisher_check$pct_low
                        ))
                        self$results$insert(1, notice)
                    }
                }

                # Checkpoint before expensive grouped statistical computation
                private$.checkpoint()

                plot4 <- ggstatsplot::grouped_ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = counts_var,
                    grouping.var = !!rlang::sym(grvar),
                    type = override_type,  # Use potentially overridden type
                    ratio = ratio_vec,  # Use enhanced parsed ratio
                    paired = options_data$paired,
                    label = options_data$label,
                    digits = options_data$digits,
                    conf.level = options_data$conflevel,
                    proportion.test = options_data$proportiontest,
                    bf.message = options_data$bfmessage,
                    results.subtitle = options_data$resultssubtitle,
                    ggtheme = selected_theme,
                    ggstatsplot.layer = isTRUE(self$options$originaltheme)
                )
            }


            # Print Plot4 ----
            print(plot4)
            TRUE
        }

        , .plotDonut = function(image, ggtheme, theme, ...) {

            if (!self$options$addGGPubrDonut) return()
            # CRITICAL FIX: Check for zero-length character vectors
            if (is.null(self$options$dep) || length(self$options$dep) == 0 ||
                (length(self$options$dep) == 1 && self$options$dep == "")) return()

            # Use cached data
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(glue::glue('Donut plot preparation failed: {e$message}'))
            })
            
            dep <- options_data$dep
            group <- options_data$group
            
            # Prepare data for ggdonutchart (needs summary table)
            # Handle weighted counts if present
            if (!is.null(options_data$counts) && options_data$counts != "") {
                # Weighted aggregation
                formula_str <- paste0(options_data$counts, " ~ ", dep)
                if (!is.null(group) && group != "") {
                    formula_str <- paste0(formula_str, " + ", group)
                }
                
                # Use xtabs to sum counts
                agg_table <- xtabs(as.formula(formula_str), data = mydata)
                plot_data <- as.data.frame(agg_table)
                
                # Rename count column to 'Freq' for consistency
                names(plot_data)[names(plot_data) == "Freq"] <- "count"
                
            } else {
                # Unweighted aggregation
                if (!is.null(group) && group != "") {
                    plot_data <- as.data.frame(table(mydata[[dep]], mydata[[group]]))
                    names(plot_data) <- c(dep, group, "count")
                } else {
                    plot_data <- as.data.frame(table(mydata[[dep]]))
                    names(plot_data) <- c(dep, "count")
                }
            }
            
            # Filter out zero counts to avoid plotting issues
            plot_data <- plot_data[plot_data$count > 0, ]
            
            # Create plot
            plot <- ggpubr::ggdonutchart(
                plot_data, 
                x = "count", 
                label = dep,
                fill = dep,
                color = "white",
                palette = self$options$ggpubrDonutPalette,
                lab.pos = "in", 
                lab.font = "white"
            )
            
            # Facet if grouped
            if (!is.null(group) && group != "") {
                plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", group)))
            }
            
            print(plot)
            TRUE
        }

    )
)
