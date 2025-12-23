#' @title Cross Table for Clinicopathological Comparisons
#'
#' @description
#' This function generates cross tables comparing a dependent variable (rows)
#' with a grouping variable (columns) and automatically selects hypothesis tests
#' appropriate for clinical research. The output tables are rendered in various
#' styles (e.g., arsenal, finalfit, gtsummary, NEJM, Lancet, hmisc) and are intended
#' for pathologists and oncologists.
#'
#' @details
#' The function cleans variable names and applies original labels. It then builds
#' a formula based on the cleaned data and performs the appropriate statistical
#' test (e.g. chi-square or Fisher's exact test). Detailed user guidance is provided
#' via HTML messages.
#'
#' Currently implemented features:
#' \itemize{
#'   \item Multiple table styles (arsenal, finalfit, gtsummary, NEJM, Lancet, hmisc)
#'   \item Automatic test selection (chi-square, Fisher's exact, t-test, ANOVA)
#'   \item Multiple testing correction (Bonferroni, Holm, BH, BY)
#'   \item Variable name safety (handles spaces and special characters)
#'   \item Data quality validation warnings
#' }
#'
#' Note: Advanced features including pairwise comparisons, effect size measures,
#' residual analysis, correspondence analysis, and mosaic plots are planned but
#' not currently implemented.
#'
#' @param data A data frame containing the study data.
#' @param vars A set of variables used as the dependent variables (rows).
#' @param group A variable (factor) used as the grouping variable (columns).
#' @param sty A string indicating the desired table style.
#'            Options include: "arsenal", "finalfit", "gtsummary", "nejm", "lancet", "hmisc".
#' @param excl Logical. If TRUE, rows with missing values will be excluded.
#' @param cont A string ("mean" or "median") to specify the central tendency metric.
#' @param pcat A string ("chisq" or "fisher") to specify the primary test.
#'
#' @return The function produces an HTML table output in the selected style.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom gtsummary tbl_summary modify_header add_n add_overall bold_labels add_p add_q bold_levels bold_p all_continuous all_categorical all_stat_cols style_pvalue as_kable_extra
#' @importFrom gt md
#' @importFrom purrr partial
#' @import magrittr

# Helper function to create styled HTML notice (replaces jmvcore::Notice to avoid serialization errors)
.createNoticeHTML <- function(message, type = c("ERROR", "STRONG_WARNING", "WARNING", "INFO")) {
    type <- match.arg(type)

    # Define styles for each notice type
    styles <- list(
        ERROR = list(
            bg = "#f8d7da",
            border = "#dc3545",
            icon = "❌",
            title_color = "#721c24"
        ),
        STRONG_WARNING = list(
            bg = "#fff3cd",
            border = "#ff9800",
            icon = "⚠️",
            title_color = "#856404"
        ),
        WARNING = list(
            bg = "#fff3cd",
            border = "#ffc107",
            icon = "⚠️",
            title_color = "#856404"
        ),
        INFO = list(
            bg = "#d1ecf1",
            border = "#17a2b8",
            icon = "ℹ️",
            title_color = "#0c5460"
        )
    )

    style <- styles[[type]]

    html <- paste0(
        "<div style='background-color: ", style$bg, "; ",
        "padding: 15px; margin: 10px 0; border-radius: 5px; ",
        "border-left: 4px solid ", style$border, ";'>",
        "<p style='margin: 0; color: ", style$title_color, ";'>",
        "<strong>", style$icon, " ", type, ":</strong> ",
        message,
        "</p>",
        "</div>"
    )

    return(html)
}

# Helper function to escape variable names with special characters
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

# Helper function to get display name from mapping
.getDisplayName <- function(cleaned_name, name_mapping) {
    # Get original display name from mapping
    original_name <- name_mapping[[cleaned_name]]
    if (is.null(original_name)) {
        return(cleaned_name)  # Fallback to cleaned name
    }
    return(original_name)
}

# Helper function to validate variable names and detect issues
.validateCrosstableVariableNames <- function(original_names, cleaned_names) {
    issues <- list()
    warnings <- list()

    # Check for duplicate names after cleaning
    duplicated_cleaned <- duplicated(cleaned_names) | duplicated(cleaned_names, fromLast = TRUE)
    if (any(duplicated_cleaned)) {
        duplicate_originals <- original_names[duplicated_cleaned]
        duplicate_cleaned <- unique(cleaned_names[duplicated_cleaned])

        issues <- append(issues, paste0(
            "Duplicate variable names after cleaning: ",
            paste(duplicate_originals, collapse = ", "),
            " → ", paste(duplicate_cleaned, collapse = ", ")
        ))
    }

    # Check for very long names that might be truncated
    long_names <- original_names[nchar(original_names) > 50]
    if (length(long_names) > 0) {
        warnings <- append(warnings, paste0(
            "Very long variable names detected (>50 characters): ",
            paste(substring(long_names, 1, 30), "...", sep = "", collapse = ", ")
        ))
    }

    # Check for special characters that needed escaping
    needs_escaping <- grepl("[^a-zA-Z0-9._]", original_names)
    if (any(needs_escaping)) {
        special_names <- original_names[needs_escaping]
        warnings <- append(warnings, paste0(
            "Variable names with special characters detected: ",
            paste(special_names, collapse = ", ")
        ))
    }

    return(list(issues = issues, warnings = warnings))
}

# Helper function for intelligent test selection with clinical rationale
.selectAppropriateTest <- function(contingency_table, test_preference = "auto", min_expected = 5) {
    # Calculate expected frequencies
    tryCatch({
        chi_test <- chisq.test(contingency_table)
        expected_min <- min(chi_test$expected)
        
        if (test_preference == "auto") {
            if (expected_min < min_expected) {
                return(list(
                    test = "fisher", 
                    reason = paste0("Expected counts < ", min_expected, ". Fisher's exact test recommended."),
                    warning = TRUE,
                    expected_min = expected_min
                ))
            } else {
                return(list(
                    test = "chisq", 
                    reason = paste0("All expected counts ≥ ", min_expected, ". Chi-square test appropriate."),
                    warning = FALSE,
                    expected_min = expected_min
                ))
            }
        }
        
        # User specified test - check if appropriate
        if (test_preference == "chisq" && expected_min < min_expected) {
            return(list(
                test = test_preference,
                reason = paste0("Chi-square test as requested (Note: Expected counts < ", min_expected, ")"),
                warning = TRUE,
                expected_min = expected_min
            ))
        }
        
        return(list(
            test = test_preference, 
            reason = .("Test selected as requested"),
            warning = FALSE,
            expected_min = expected_min
        ))
    }, error = function(e) {
        return(list(
            test = "fisher", 
            reason = .("Cannot calculate expected frequencies. Using Fisher's exact test."),
            warning = TRUE,
            expected_min = NA
        ))
    })
}

# Wrapper for gtsummary to automatically choose Fisher vs chi-square based on expected counts
.cat_test_auto <- function(data, variable, by, test_preference = "auto", min_expected = 5, ...) {
    tbl <- table(data[[variable]], data[[by]])
    sel <- .selectAppropriateTest(tbl, test_preference = test_preference, min_expected = min_expected)
    if (sel$test == "fisher") {
        res <- fisher.test(tbl)
    } else {
        res <- suppressWarnings(chisq.test(tbl, correct = FALSE))
    }
    res
}

# Wrapper for gtsummary to use Welch/oneway vs Wilcoxon/Kruskal depending on chosen central tendency
.cont_test_auto <- function(data, variable, by, method = c("mean", "median"), ...) {
    method <- match.arg(method)
    groups <- factor(data[[by]])
    if (length(levels(groups)) > 2) {
        if (method == "mean") {
            stats::oneway.test(data[[variable]] ~ groups, var.equal = FALSE)
        } else {
            stats::kruskal.test(data[[variable]] ~ groups)
        }
    } else {
        if (method == "mean") {
            stats::t.test(data[[variable]] ~ groups, var.equal = FALSE)
        } else {
            stats::wilcox.test(data[[variable]] ~ groups, exact = FALSE)
        }
    }
}

# Helper function to validate sample size and data quality
.validateAnalysisAssumptions <- function(mydata, myvars, mygroup) {
    issues <- list()
    warnings <- list()
    
    # Check overall sample size
    n_total <- nrow(mydata)
    if (n_total < 20) {
        issues <- append(issues, sprintf(.("Very small sample size (n = %d). Results may be unreliable."), n_total))
    }
    
    # Check group sizes
    if (!is.null(mygroup) && mygroup %in% names(mydata)) {
        group_sizes <- table(mydata[[mygroup]])
        min_group_size <- min(group_sizes)
        
        if (min_group_size < 5) {
            warnings <- append(warnings, paste0("Small group detected (n = ", min_group_size, "). Consider combining categories or using exact tests."))
        }
        
        # Check for empty cells in cross-tabulations (categorical variables only)
        for (var in myvars) {
            if (var %in% names(mydata)) {
                # Only check categorical variables for empty cells
                # Continuous variables naturally have many unique values that won't appear in all groups
                is_categorical <- is.factor(mydata[[var]]) ||
                                 is.character(mydata[[var]]) ||
                                 (is.numeric(mydata[[var]]) && length(unique(na.omit(mydata[[var]]))) <= 6)

                if (is_categorical) {
                    cont_table <- table(mydata[[var]], mydata[[mygroup]])
                    if (any(cont_table == 0)) {
                        warnings <- append(warnings, paste0("Empty cells detected in ", var, " × ", mygroup, " table. Results may be unstable."))
                    }
                }
            }
        }
    }
    
    # Check for excessive missing data
    for (var in c(myvars, mygroup)) {
        if (var %in% names(mydata)) {
            missing_pct <- mean(is.na(mydata[[var]])) * 100
            if (missing_pct > 20) {
                warnings <- append(warnings, paste0("High missing data in ", var, " (", round(missing_pct, 1), "%). Consider imputation or sensitivity analysis."))
            }
        }
    }
    
    return(list(
        critical_issues = issues,
        warnings = warnings,
        sample_size = n_total
    ))
}

# Helper function to generate clinical interpretation
.generateClinicalSummary <- function(results, myvars, mygroup, test_type = "crosstable") {
    if (is.null(results) || length(results) == 0) {
        return(.("No results available for interpretation."))
    }
    
    # Count significant associations (assuming p-value column exists)
    significant_count <- 0
    total_tests <- length(myvars)
    
    if (test_type == "crosstable") {
        summary <- sprintf(.("Cross-table analysis comparing %d variable(s) across %s groups."),
                          total_tests, mygroup)

        if (significant_count > 0) {
            summary <- paste0(summary, " ",
                sprintf(.("Found %d significant association(s) (p < 0.05)."), significant_count))
        } else {
            summary <- paste0(summary, " ",
                .("No significant associations detected (all p ≥ 0.05)."))
        }

        summary <- paste0(summary, " ",
            .("Review individual test results below for detailed findings."))
    }
    
    return(summary)
}

crosstableClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "crosstableClass",
        inherit = crosstableBase,
        private = list(
            # .labelData ----
            # Prepare data by cleaning names and setting original labels with robust handling.
            .labelData = function() {
                mydata <- self$data
                original_names <- names(mydata)

                # Clean variable names using janitor
                mydata <- mydata %>% janitor::clean_names()
                cleaned_names <- names(mydata)

                # Validate variable names and report issues
                validation_results <- .validateCrosstableVariableNames(original_names, cleaned_names)

                # Report any critical issues
                if (length(validation_results$issues) > 0) {
                    stop(paste("Variable name issues detected:",
                              paste(validation_results$issues, collapse = "; ")))
                }

                # Create bidirectional mappings for robust variable handling
                # original_names_mapping: cleaned_name -> original_name
                original_names_mapping <- setNames(original_names, cleaned_names)
                # cleaned_names_mapping: original_name -> cleaned_name
                cleaned_names_mapping <- setNames(cleaned_names, original_names)

                # Apply labels to preserve original names
                mydata <- labelled::set_variable_labels(
                    .data = mydata,
                    .labels = original_names_mapping
                )

                # Retrieve all variable labels
                all_labels <- labelled::var_label(mydata)

                # Robust variable matching with error handling
                tryCatch({
                    # Match user-specified variables to cleaned names
                    user_vars <- self$options$vars
                    if (length(user_vars) > 0) {
                        matched_indices <- match(user_vars, all_labels)
                        if (any(is.na(matched_indices))) {
                            missing_vars <- user_vars[is.na(matched_indices)]
                            warning(paste("Could not find variables:", paste(missing_vars, collapse = ", ")))
                        }
                        myvars <- names(all_labels)[matched_indices[!is.na(matched_indices)]]
                    } else {
                        myvars <- character(0)
                    }

                    # Match grouping variable
                    if (!is.null(self$options$group) && self$options$group != "") {
                        group_match <- which(all_labels == self$options$group)
                        if (length(group_match) > 0) {
                            mygroup <- names(all_labels)[group_match[1]]  # Take first match
                        } else {
                            warning(paste("Could not find grouping variable:", self$options$group))
                            mygroup <- character(0)
                        }
                    } else {
                        mygroup <- character(0)
                    }

                }, error = function(e) {
                    stop(paste("Variable matching failed:", e$message))
                })

                # Report warnings about variable names if any
                if (length(validation_results$warnings) > 0) {
                    warning_msg <- paste0(
                        "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                        "<strong>⚠️ Variable Name Warnings:</strong><br>",
                        paste(validation_results$warnings, collapse = "<br>"),
                        "</div>"
                    )
                    # Display in todo2 section
                    self$results$todo2$setContent(warning_msg)
                }

                # If any user-specified variable could not be matched, block analysis
                if (length(myvars) == 0 || length(mygroup) == 0) {
                    stop("Selected variables could not be matched to the dataset after cleaning labels. Please reselect variables.")
                }

                return(list(
                    "mydata" = mydata,
                    "myvars" = myvars,
                    "mygroup" = mygroup,
                    "original_names_mapping" = original_names_mapping,
                    "cleaned_names_mapping" = cleaned_names_mapping
                ))
            },


            # .showTestInformation ----
            .showTestInformation = function(method_type = "FDR") {
                # Generate method-specific test information
                if (method_type == "FWER") {
                    # Family-Wise Error Rate control (Bonferroni, Holm)
                    test_info <- paste0(
                        "<div style='background-color: #e8f4fd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",
                        "<h4 style='margin-top: 0; color: #1976D2;'>Adjusted P-values and FWER Control</h4>",

                        "<p><strong>What are Adjusted P-values?</strong><br>",
                        "Adjusted p-values control the Family-Wise Error Rate (FWER) - the probability of making <strong>at least one</strong> false positive across all tests in the table.</p>",

                        "<p><strong>Why use FWER control?</strong><br>",
                        "When comparing multiple variables across groups, the chance of finding at least one false positive increases. FWER methods (Bonferroni/Holm) provide <strong>strong control</strong> - ensuring the probability of ANY false positive stays below α (typically 0.05).</p>",

                        "<p><strong>Interpretation Guidelines:</strong></p>",
                        "<ul>",
                        "<li><strong>Adjusted p < 0.05:</strong> Statistically significant - strong evidence against null hypothesis</li>",
                        "<li><strong>Adjusted p ≥ 0.05:</strong> Not significant after correction for multiple testing</li>",
                        "<li><strong>Note:</strong> Adjusted p-values are typically <em>larger</em> than raw p-values (more conservative)</li>",
                        "</ul>",

                        "<p><strong>When to use FWER control:</strong></p>",
                        "<ul>",
                        "<li>✅ Confirmatory studies where even one false positive is unacceptable</li>",
                        "<li>✅ Clinical trials with regulatory requirements</li>",
                        "<li>✅ When you have strong prior hypotheses to test</li>",
                        "</ul>",

                        "<p><em>💡 FWER methods are conservative - you may miss true effects to avoid false positives. Consider FDR methods (BH/BY) for exploratory research.</em></p>",
                        "</div>"
                    )
                } else {
                    # False Discovery Rate control (BH, BY)
                    test_info <- paste0(
                        "<div style='background-color: #e8f4fd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",
                        "<h4 style='margin-top: 0; color: #1976D2;'>Q-values and FDR Control</h4>",

                        "<p><strong>What are Q-values?</strong><br>",
                        "Q-values represent the False Discovery Rate (FDR) - the expected <strong>proportion</strong> of false positives among discoveries when testing multiple variables simultaneously.</p>",

                        "<p><strong>Why use FDR control?</strong><br>",
                        "When comparing multiple variables across groups (as in this cross-table), the chance of false positives increases. Q-values (FDR control) limit the expected false discovery <em>proportion</em>, which is less conservative than FWER control (Bonferroni/Holm) that controls for ANY false positive.</p>",

                        "<p><strong>Interpretation Guidelines:</strong></p>",
                        "<ul>",
                        "<li><strong>Q < 0.05:</strong> Strong evidence - expect 5% of discoveries at this threshold to be false positives</li>",
                        "<li><strong>Q < 0.10:</strong> Moderate evidence - 10% FDR, often acceptable in exploratory research</li>",
                        "<li><strong>Q < 0.20:</strong> Suggestive evidence - warrants further investigation in hypothesis-generating studies</li>",
                        "</ul>",

                        "<p><strong>When to use FDR control:</strong></p>",
                        "<ul>",
                        "<li>✅ Exploratory analyses with many comparisons</li>",
                        "<li>✅ Genomic/proteomic studies with thousands of tests</li>",
                        "<li>✅ Hypothesis-generating research where you can tolerate some false positives</li>",
                        "</ul>",

                        "<p><em>💡 FDR methods are less conservative than FWER - you'll discover more effects but accept a small proportion of false positives.</em></p>",
                        "</div>"
                    )
                }

                self$results$testInformation$setContent(test_info)
            },

            # .run ----
            .run = function() {
                sty <- self$options$sty
                # If required options are missing, show a welcome message with instructions.
                if (is.null(self$options$vars) || is.null(self$options$group)) {
                    # Initial state - no error, just show welcome message
                    self$results$errorNotice$setVisible(FALSE)

                    todo <- paste0(
                        "<div style='background-color: #f8f9fa; padding: 20px; margin: 15px 0; border-radius: 8px; border-left: 5px solid #007bff;'>",
                        "<h3 style='margin-top: 0; color: #007bff;'>Welcome to Cross Table Analysis</h3>",

                        "<p><strong>Purpose:</strong> Compare distributions of clinical variables across groups with automatic test selection.</p>",

                        "<h4 style='margin-top: 15px;'>Quick Start:</h4>",
                        "<ol style='margin-left: 20px;'>",
                        "<li>Select <strong>dependent variables</strong> (rows) - continuous or categorical measures</li>",
                        "<li>Select <strong>grouping variable</strong> (columns) - treatment groups, disease stages, etc.</li>",
                        "<li>Choose <strong>table style</strong> from Options (NEJM, Lancet, gtsummary, etc.)</li>",
                        "</ol>",

                        "<h4 style='margin-top: 15px;'>Automatic Test Selection:</h4>",
                        "<ul style='margin-left: 20px;'>",
                        "<li><strong>Categorical variables:</strong> Chi-square or Fisher's exact test (based on expected counts)</li>",
                        "<li><strong>Continuous variables:</strong> t-test, ANOVA, or non-parametric equivalents</li>",
                        "<li><strong>Multiple testing correction:</strong> Benjamini-Hochberg (FDR) recommended for exploratory analysis</li>",
                        "</ul>",

                        "<h4 style='margin-top: 15px;'>Table Styles Available:</h4>",
                        "<ul style='margin-left: 20px;'>",
                        "<li><strong>gtsummary:</strong> Modern, publication-ready with q-values (recommended)</li>",
                        "<li><strong>NEJM / Lancet:</strong> Journal-specific formatting</li>",
                        "<li><strong>finalfit:</strong> Clinical research standard</li>",
                        "<li><strong>arsenal:</strong> Comprehensive tables with many options</li>",
                        "</ul>",

                        "<p style='margin-top: 15px;'><em>💡 Tip: Use gtsummary style for publication-ready tables with automatic q-values and FDR correction.</em></p>",
                        "</div>"
                    )
                    self$results$todo$setContent(todo)
                    self$results$todo$setVisible(TRUE)
                    return()
                } else {
                    # Hide welcome message when analysis is running
                    self$results$todo$setContent("")
                    self$results$todo$setVisible(FALSE)
                    self$results$errorNotice$setVisible(FALSE)
                }

                # Set subtitle with grouping variable
                group_display <- if (!is.null(self$options$group) && self$options$group != "") {
                    self$options$group
                } else {
                    "No group selected"
                }
                self$results$subtitle$setContent(paste0("Cross Table Analysis - Grouped by ", group_display))

                # Provide additional information when using 'finalfit' style.
                if (sty == "finalfit") {
                    todo2 <- glue::glue(
                        "<br>
                         <b>finalfit</b> style uses <em>aov (analysis of variance) or t.test for Welch two sample t-test</em>.
                         For continuous non-parametric tests, Kruskal Wallis is used (equivalent to Mann-Whitney U / Wilcoxon rank sum test in two-group settings).
                         See full documentation <a href='https://finalfit.org/reference/summary_factorlist.html'>here</a>.
                         "
                    )
                } else {
                    todo2 <- ""
                }
                self$results$todo2$setContent(todo2)

                # Check if data has complete rows.
                if (nrow(self$data) == 0) {
                    # Use HTML error notice instead of dynamic Notice to avoid serialization errors
                    error_html <- .createNoticeHTML(
                        'Dataset contains no complete rows. Please check your data and filters.',
                        type = "ERROR"
                    )
                    self$results$errorNotice$setContent(error_html)
                    self$results$errorNotice$setVisible(TRUE)
                    return()
                }

                # Performance safeguards for large datasets
                n_rows <- nrow(self$data)
                n_vars <- length(self$options$vars)
                n_combinations <- n_vars * length(unique(self$data[[self$options$group]]))
                
                if (n_rows > 50000) {
                    warning(paste(.("Large dataset detected:"), n_rows, .("rows. Analysis may take longer.")))
                }
                
                if (n_combinations > 100) {
                    warning(paste(.("Large number of variable combinations:"), n_combinations, .("Consider reducing variables.")))
                }

                # Read and label data with robust variable name handling.
                cleaneddata <- private$.labelData()
                mydata <- cleaneddata$mydata
                myvars <- cleaneddata$myvars
                mygroup <- cleaneddata$mygroup
                original_names_mapping <- cleaneddata$original_names_mapping
                cleaned_names_mapping <- cleaneddata$cleaned_names_mapping

                # Build formula using escaped variable names for safety.
                escaped_myvars <- .escapeVariableNames(myvars)
                escaped_mygroup <- .escapeVariableNames(mygroup)
                formula <- jmvcore::constructFormula(terms = escaped_myvars, dep = escaped_mygroup)
                formula <- as.formula(formula)

                # Exclude missing data if requested.
                if (self$options$excl)
                    mydata <- jmvcore::naOmit(mydata)
                
                # Validate analysis assumptions and data quality
                validation_results <- .validateAnalysisAssumptions(mydata, myvars, mygroup)
                if (length(validation_results$warnings) > 0) {
                    # Accumulate all warnings into HTML (avoid serialization errors from dynamic Notice inserts)
                    warning_html_parts <- character(length(validation_results$warnings))

                    for (i in seq_along(validation_results$warnings)) {
                        warn <- validation_results$warnings[[i]]

                        # Determine severity based on content
                        notice_type <- if (grepl("Very small|n = [0-9]+\\)", warn)) {
                            # Extract sample size if present
                            n_match <- regmatches(warn, regexec("n = ([0-9]+)", warn))
                            if (length(n_match[[1]]) > 1) {
                                n_val <- as.numeric(n_match[[1]][2])
                                if (n_val < 10) {
                                    "STRONG_WARNING"
                                } else {
                                    "WARNING"
                                }
                            } else {
                                "WARNING"
                            }
                        } else {
                            "WARNING"
                        }

                        # Create HTML for this warning
                        warning_html_parts[i] <- .createNoticeHTML(warn, type = notice_type)
                    }

                    # Combine all warnings into single HTML output
                    combined_warnings <- paste(warning_html_parts, collapse = "\n")
                    self$results$dataQualityNotice$setContent(combined_warnings)
                    self$results$dataQualityNotice$setVisible(TRUE)
                } else {
                    self$results$dataQualityNotice$setVisible(FALSE)
                }

                # Generate table based on selected style.
                if (sty == "arsenal") {
                    arsenal_control <- arsenal::tableby.control(
                        test = TRUE,
                        total = TRUE,
                        numeric.test = if (self$options$cont == "mean") "anova" else "kwt",
                        cat.test = if (self$options$pcat == "fisher") "fe" else "chisq",
                        numeric.stats = if (self$options$cont == "mean") c("Nmiss", "meansd") else c("Nmiss", "median", "q1q3"),
                        stats.labels = list(meansd = "Mean (SD)", median = "Median", q1q3 = "Q1, Q3")
                    )

                    tablearsenal <- arsenal::tableby(
                        formula = formula,
                        data = mydata,
                        control = arsenal_control,
                        digits = 1,
                        digits.count = 1
                    )
                    tablearsenal <- summary(tablearsenal, text = 'html', pfootnote = 'html')
                    tablearsenal <- capture.output(tablearsenal)
                    self$results$tablestyle1$setContent(tablearsenal)
                } else if (sty == "finalfit") {
                    myvars_term <- jmvcore::composeTerm(components = myvars)
                    myvars_term <- jmvcore::decomposeTerm(term = myvars_term)
                    private$.checkpoint()
                    # Create the finalfit summary table.
                    tablefinalfit <- mydata %>%
                        finalfit::summary_factorlist(
                            .data = .,
                            dependent = mygroup,
                            explanatory = myvars_term,
                            total_col = TRUE,
                            p = TRUE,
                            add_dependent_label = TRUE,
                            na_include = FALSE,
                            na_to_p = FALSE,
                            cont = self$options$cont,
                            cont_nonpara = NULL,
                            cont_cut = 5,
                            cont_range = TRUE,
                            p_cont_para = "aov",
                            p_cat = self$options$pcat,
                            dependent_label_prefix = "Dependent: ",
                            dependent_label_suffix = "",
                            row_totals_colname = "Total N",
                            row_missing_colname = "Missing N",
                            column = TRUE,
                            orderbytotal = FALSE,
                            digits = c(1, 1, 3, 1, 0),
                            na_include_dependent = FALSE,
                            na_complete_cases = FALSE,
                            fit_id = FALSE,
                            na_to_prop = TRUE,
                            add_col_totals = TRUE,
                            include_col_totals_percent = TRUE,
                            col_totals_rowname = NULL,
                            col_totals_prefix = "",
                            add_row_totals = FALSE,
                            include_row_totals_percent = TRUE,
                            include_row_missing_col = TRUE,
                            catTest = NULL,
                            weights = NULL
                        )
                    tablefinalfit <- kableExtra::kable(
                        tablefinalfit,
                        format = "html",
                        digits = 1,
                        escape = FALSE
                    )
                    self$results$tablestyle2$setContent(tablefinalfit)
                } else if (sty == "gtsummary") {
                    # tablegtsummary <- gtsummary::tbl_summary(data = mydata, by = mygroup)
                    # tablegtsummary <- gtsummary::as_kable_extra(tablegtsummary)
                    # self$results$tablestyle3$setContent(tablegtsummary)



                    # http://www.danieldsjoberg.com/gtsummary/articles/gallery.html


                # Select only the analysis variables and grouping variable
                analysis_vars <- c(myvars, mygroup)
                mydata_subset <- mydata[, analysis_vars, drop = FALSE]

                # Ensure grouping variable is a factor with labelled levels
                if (!is.factor(mydata_subset[[mygroup]])) {
                    mydata_subset[[mygroup]] <- factor(mydata_subset[[mygroup]])
                }

                # Heuristic: treat numeric variables with few unique values as categorical to avoid t/ANOVA on encoded factors
                # Exclude grouping variable from type specification (it's used in 'by' argument)
                all_cat_vars <- names(mydata_subset)[vapply(mydata_subset, function(v) {
                    is.factor(v) || is.character(v) || (is.numeric(v) && length(unique(na.omit(v))) <= 6)
                }, logical(1))]
                cat_vars <- setdiff(all_cat_vars, mygroup)  # Remove grouping variable
                cont_vars <- setdiff(myvars, all_cat_vars)  # Continuous = myvars minus all categoricals

                mydata_subset[cat_vars] <- lapply(mydata_subset[cat_vars], function(v) {
                    if (is.factor(v)) return(v)
                    factor(v)
                })

                # Get p-value adjustment method
                p_adjust_method <- self$options$p_adjust

                # Map option names to gtsummary method names
                method_mapping <- c(
                    "none" = "none",
                    "bonferroni" = "bonferroni",
                    "holm" = "holm",
                    "BH" = "fdr",  # Benjamini-Hochberg = FDR
                    "BY" = "BY"    # Benjamini-Yekutieli
                )

                gtsummary_method <- method_mapping[p_adjust_method]

                # Determine number of groups for correct test selection
                n_groups <- length(unique(na.omit(mydata[[mygroup]])))

                # Map user options to gtsummary syntax
                stats_cont <- if (self$options$cont == "mean") "{mean} ({sd})" else "{median} ({p25}, {p75})"

                # Note: gtsummary has excellent default test selection:
                # - Categorical: chi-square (automatically switches to Fisher when expected counts < 5)
                # - Continuous 2 groups: Welch t-test (robust to unequal variances)
                # - Continuous 3+ groups: ANOVA (oneway.test)
                # User preferences for cont/pcat are still respected in arsenal and finalfit styles

                tablegtsummary <-
                  mydata_subset %>%
                  tbl_summary(
                    by = mygroup,
                    statistic = list(
                      all_continuous()  ~ stats_cont,
                      all_categorical() ~ "{n}/{N} ({p}%)"
                    ),
                    digits       = all_continuous() ~ 2,
                    missing_text = "(Missing)",
                    type = list(
                        all_of(cat_vars) ~ "categorical",
                        all_of(cont_vars) ~ "continuous"
                    )
                  ) %>%
                  add_n() %>%
                  add_overall() %>%
                  add_p(pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3))

                # Add adjusted p-values/q-values only if adjustment method is not "none"
                if (p_adjust_method != "none") {
                    # Warn if adjusting with only 1 variable (pointless)
                    if (n_vars == 1) {
                        single_var_warning <- .createNoticeHTML(
                            paste0("P-value adjustment with only 1 variable has no effect. ",
                                   "Adjusted p-value will equal the original p-value. ",
                                   "Multiple testing correction is only meaningful when testing multiple variables simultaneously."),
                            type = "INFO"
                        )
                        self$results$dataQualityNotice$setContent(
                            paste(self$results$dataQualityNotice$state, single_var_warning, sep = "\n")
                        )
                        self$results$dataQualityNotice$setVisible(TRUE)
                    }

                    # Determine if this is FWER or FDR method
                    is_fdr <- p_adjust_method %in% c("BH", "BY")

                    # Set column header based on method type
                    adjusted_col_header <- if (is_fdr) "**q-value**" else "**adjusted p**"

                    tablegtsummary <- tablegtsummary %>%
                      add_q(
                        method = gtsummary_method,
                        pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
                      ) %>%
                      modify_header(
                        all_stat_cols() ~ "**{level}**\nN = {n} ({style_percent(p)})",
                        p.value      ~ "**p-value**",
                        q.value      ~ adjusted_col_header
                      )
                } else {
                    tablegtsummary <- tablegtsummary %>%
                      modify_header(
                        all_stat_cols() ~ "**{level}**\nN = {n} ({style_percent(p)})",
                        p.value      ~ "**p-value**"
                      )
                }

                tablegtsummary <- tablegtsummary %>%
                  bold_labels()

                tablegtsummary <-
                    gtsummary::as_kable_extra(tablegtsummary)

                self$results$tablestyle3$setContent(tablegtsummary)

                # Add adjustment explanation (only if adjustment is applied)
                if (p_adjust_method != "none") {
                    # Determine method type
                    is_fdr <- p_adjust_method %in% c("BH", "BY")
                    method_type <- if (is_fdr) "FDR" else "FWER"

                    method_names <- list(
                        "bonferroni" = "Bonferroni",
                        "holm" = "Holm",
                        "BH" = "Benjamini-Hochberg (FDR)",
                        "BY" = "Benjamini-Yekutieli (FDR)"
                    )

                    method_descriptions <- list(
                        "bonferroni" = "Conservative Family-Wise Error Rate (FWER) control. Multiplies each p-value by the number of tests. Controls probability of ANY false positive.",
                        "holm" = "Step-down Family-Wise Error Rate (FWER) control. Less conservative than Bonferroni while maintaining strong control against ANY false positive.",
                        "BH" = "False Discovery Rate (FDR) control. Controls the expected PROPORTION of false positives among discoveries. Less conservative than FWER methods.",
                        "BY" = "False Discovery Rate (FDR) control with additional correction for dependent tests. More conservative than Benjamini-Hochberg but still controls FDR not FWER."
                    )

                    # Generate method-specific explanation
                    if (is_fdr) {
                        # FDR methods - use "q-values"
                        qvalue_explanation <- paste0(
                            "<div style='background-color: #f0f8ff; padding: 15px; margin-top: 20px; border-radius: 5px; border: 1px solid #4682b4;'>",
                            "<h4 style='margin-top: 0;'>Multiple Testing Correction: ", method_names[[p_adjust_method]], "</h4>",
                            "<p><strong>Method:</strong> ", method_descriptions[[p_adjust_method]], "</p>",

                            "<p><strong>What are Q-values?</strong><br>",
                            "Q-values represent the False Discovery Rate (FDR) - the expected <strong>proportion</strong> of false positives among your discoveries. ",
                            "Unlike FWER methods that control the probability of ANY false positive, FDR methods allow a controlled proportion of false discoveries.</p>",

                            "<p><strong>How to interpret:</strong></p>",
                            "<ul>",
                            "<li><strong>Q-value = 0.05:</strong> Among all variables with q ≤ 0.05, expect ~5% to be false positives</li>",
                            "<li><strong>Q-value = 0.10:</strong> Expect ~10% false positives (acceptable in exploratory research)</li>",
                            "<li><strong>Q-values are larger than raw p-values</strong> but smaller than FWER-adjusted p-values</li>",
                            "</ul>",

                            "<p><strong>When to use FDR control:</strong></p>",
                            "<ul>",
                            "<li>✅ Exploratory analyses where discovering patterns is the goal</li>",
                            "<li>✅ Genomic/proteomic studies with hundreds or thousands of tests</li>",
                            "<li>✅ Screening studies to generate hypotheses for follow-up</li>",
                            "<li>✅ When you can tolerate a small proportion of false positives</li>",
                            "</ul>",

                            "<p><strong>Important considerations:</strong></p>",
                            "<ul>",
                            "<li>⚠️ FDR methods assume independence or positive dependence between tests</li>",
                            "<li>⚠️ Less conservative than Bonferroni/Holm - you'll find more discoveries but accept some false positives</li>",
                            "<li>⚠️ Should not replace careful hypothesis planning and validation</li>",
                            "</ul>",

                            "<p><small><em>Correction applied using ", method_names[[p_adjust_method]], " method via gtsummary::add_q()</em></small></p>",
                            "</div>"
                        )
                    } else {
                        # FWER methods - use "adjusted p-values"
                        qvalue_explanation <- paste0(
                            "<div style='background-color: #f0f8ff; padding: 15px; margin-top: 20px; border-radius: 5px; border: 1px solid #4682b4;'>",
                            "<h4 style='margin-top: 0;'>Multiple Testing Correction: ", method_names[[p_adjust_method]], "</h4>",
                            "<p><strong>Method:</strong> ", method_descriptions[[p_adjust_method]], "</p>",

                            "<p><strong>What are Adjusted P-values?</strong><br>",
                            "Adjusted p-values control the Family-Wise Error Rate (FWER) - the probability of making <strong>at least one</strong> false positive across all tests. ",
                            "This is the most conservative approach to multiple testing correction.</p>",

                            "<p><strong>How to interpret:</strong></p>",
                            "<ul>",
                            "<li><strong>Adjusted p < 0.05:</strong> Statistically significant - strong evidence even after accounting for all tests</li>",
                            "<li><strong>Adjusted p ≥ 0.05:</strong> Not significant after correction</li>",
                            "<li><strong>Adjusted p-values are much larger than raw p-values</strong> (very conservative correction)</li>",
                            "</ul>",

                            "<p><strong>When to use FWER control:</strong></p>",
                            "<ul>",
                            "<li>✅ Confirmatory studies where false positives are costly</li>",
                            "<li>✅ Clinical trials with regulatory requirements</li>",
                            "<li>✅ When testing pre-specified hypotheses</li>",
                            "<li>✅ When even a single false positive is unacceptable</li>",
                            "</ul>",

                            "<p><strong>Important considerations:</strong></p>",
                            "<ul>",
                            "<li>⚠️ Very conservative - may miss true effects (reduced power)</li>",
                            "<li>⚠️ With many tests (>10), corrections can be extremely stringent</li>",
                            "<li>⚠️ Consider FDR methods (BH/BY) for exploratory research with many tests</li>",
                            "</ul>",

                            "<p><small><em>Correction applied using ", method_names[[p_adjust_method]], " method via gtsummary::add_q()</em></small></p>",
                            "</div>"
                        )
                    }

                    self$results$qvalueExplanation$setContent(qvalue_explanation)
                    self$results$qvalueExplanation$setVisible(TRUE)

                    # Show method-specific test information
                    private$.showTestInformation(method_type)
                    self$results$testInformation$setVisible(TRUE)
                } else {
                    # No adjustment - hide both explanations
                    self$results$qvalueExplanation$setContent("")
                    self$results$qvalueExplanation$setVisible(FALSE)
                    self$results$testInformation$setContent("")
                    self$results$testInformation$setVisible(FALSE)
                }

                } else if (sty %in% c("nejm", "lancet", "hmisc")) {
                    sty_term <- jmvcore::composeTerm(components = self$options$sty)
                    tabletangram <- tangram::html5(
                        tangram::tangram(
                            formula,
                            mydata,
                            transform = tangram::hmisc,
                            id = "tbl3",
                            test = TRUE,
                            digits = 1,
                            include_p = TRUE
                        ),
                        fragment = TRUE,
                        style = sty_term,
                        caption = paste0("Cross Table for Dependent ", .getDisplayName(mygroup, original_names_mapping)),
                        id = "tbl3"
                    )
                    self$results$tablestyle4$setContent(tabletangram)
                }

                # ===== DISABLED: Stratified Analysis (Mantel-Haenszel) =====
                # Feature temporarily disabled - options not available in .a.yaml
                # if (self$options$mantel_haenszel && !is.null(self$options$stratify) && self$options$stratify != "") {
                if (FALSE) {  # DISABLED
                    # Match stratification variable to cleaned name
                    strata_var <- NULL
                    all_labels <- labelled::var_label(mydata)
                    strata_match <- which(all_labels == self$options$stratify)
                    if (length(strata_match) > 0) {
                        strata_var <- names(all_labels)[strata_match[1]]
                    }

                    if (!is.null(strata_var) && strata_var %in% names(mydata)) {
                        # Perform M-H test for each variable (only if binary)
                        mh_results_html <- ""

                        for (var in myvars) {
                            # Check if variable is binary (2 levels)
                            var_levels <- length(unique(na.omit(mydata[[var]])))
                            group_levels <- length(unique(na.omit(mydata[[mygroup]])))

                            if (var_levels == 2 && group_levels == 2) {
                                # Perform Mantel-Haenszel test
                                mh_result <- private$.performMantelHaenszel(
                                    row_var = var,
                                    col_var = mygroup,
                                    strata_var = strata_var,
                                    data = mydata
                                )

                                # Optionally perform Breslow-Day test
                                bd_result <- NULL
                                if (self$options$breslow_day && mh_result$success) {
                                    bd_result <- private$.performBreslowDay(
                                        row_var = var,
                                        col_var = mygroup,
                                        strata_var = strata_var,
                                        data = mydata
                                    )
                                }

                                # Format results with variable name header
                                var_display_name <- .getDisplayName(var, original_names_mapping)
                                mh_results_html <- paste0(
                                    mh_results_html,
                                    "<h3 style='margin-top: 20px; color: #333;'>Variable: ", var_display_name, "</h3>",
                                    private$.formatMantelHaenszelResults(mh_result, bd_result)
                                )
                            } else {
                                # Skip non-binary variables with informative message
                                var_display_name <- .getDisplayName(var, original_names_mapping)
                                mh_results_html <- paste0(
                                    mh_results_html,
                                    "<h3 style='margin-top: 20px; color: #333;'>Variable: ", var_display_name, "</h3>",
                                    "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                                    "<p><strong>Note:</strong> Mantel-Haenszel test requires binary (2-level) variables. ",
                                    "This variable has ", var_levels, " levels and grouping variable has ", group_levels, " levels. ",
                                    "Consider recoding variables to binary if you want to perform stratified analysis.</p>",
                                    "</div>"
                                )
                            }
                        }

                        # Display all M-H results
                        if (mh_results_html != "") {
                            strata_display_name <- .getDisplayName(strata_var, original_names_mapping)
                            header_html <- paste0(
                                "<div style='background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 5px; border: 1px solid #dee2e6;'>",
                                "<h2 style='margin-top: 0; color: #495057;'>Stratified Analysis Results</h2>",
                                "<p><strong>Stratification Variable:</strong> ", strata_display_name, "</p>",
                                "<p><strong>Purpose:</strong> Mantel-Haenszel test examines the association between row and column variables ",
                                "while controlling for the effect of the stratification variable. This helps identify whether an observed ",
                                "association is confounded by a third variable.</p>",
                                "</div>"
                            )
                            self$results$mantelHaenszelResults$setContent(paste0(header_html, mh_results_html))
                        }
                    } else {
                        # Stratification variable not found
                        error_html <- paste0(
                            "<div style='background-color: #f8d7da; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #dc3545;'>",
                            "<h4 style='margin-top: 0; color: #721c24;'>Stratification Variable Not Found</h4>",
                            "<p>Could not find the selected stratification variable in the dataset. ",
                            "Please verify that the variable exists and contains valid data.</p>",
                            "</div>"
                        )
                        self$results$mantelHaenszelResults$setContent(error_html)
                    }
                }

                # Add INFO notice for successful analysis completion (using HTML to avoid serialization errors)
                n_vars <- length(self$options$vars)
                group_display <- self$options$group
                style_display <- switch(self$options$sty,
                    "arsenal" = "arsenal",
                    "finalfit" = "finalfit",
                    "gtsummary" = "gtsummary",
                    "nejm" = "NEJM",
                    "lancet" = "Lancet",
                    "hmisc" = "Hmisc",
                    self$options$sty
                )

                info_message <- sprintf(
                    'Cross table analysis completed successfully. Analyzed %d variable(s) across %s groups using %s style.',
                    n_vars, group_display, style_display
                )

                info_html <- .createNoticeHTML(info_message, type = "INFO")
                self$results$analysisInfo$setContent(info_html)
                self$results$analysisInfo$setVisible(TRUE)

                # ========================================================================
                # STUBBED FEATURES - NOT IMPLEMENTED
                # The following features are commented out and NOT available to users.
                # Do not uncomment without adding corresponding UI controls in .a.yaml
                # and output definitions in .r.yaml.
                # ========================================================================

                # PAIRWISE COMPARISONS - NOT IMPLEMENTED
                # No pairwise option exists in .a.yaml
                # No pairwiseTable output exists in .r.yaml
                # if (self$options$pairwise && !is.null(self$options$group) && !is.null(self$options$vars)) {
                #     private$.performPairwiseComparisons()
                # }

                # ADVANCED POST-HOC ANALYSIS - NOT IMPLEMENTED
                # No posthoc_method, effect_size_measures, residual_analysis, or
                # correspondence_analysis options exist in .a.yaml
                # if (self$options$posthoc_method != "none" || self$options$effect_size_measures ||
                #     self$options$residual_analysis || self$options$correspondence_analysis) {
                #     private$.performAdvancedPosthoc()
                # }

                # CLINICAL SUMMARY & COPY-READY SENTENCES - NOT IMPLEMENTED
                # No clinicalSummary or reportSentences outputs exist in .r.yaml
                # tryCatch({
                #     clinical_summary <- private$.generateClinicalSummary(
                #         results = list(data = mydata, vars = myvars, group = mygroup),
                #         myvars = myvars,
                #         mygroup = mygroup,
                #         test_type = "crosstable"
                #     )
                #     
                #     # Add clinical summary to results
                #     if (!is.null(clinical_summary)) {
                #         summary_html <- paste0(
                #             "<div style='background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #28a745;'>",
                #             "<h4 style='margin-top: 0; color: #28a745;'>Clinical Summary</h4>",
                #             clinical_summary,
                #             "</div>"
                #         )
                #         self$results$clinicalSummary$setContent(summary_html)
                #         
                #         # Also populate the copy-ready sentences
                #         copy_ready <- paste0(
                #             "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                #             "<h4 style='margin-top: 0; color: #856404;'>Copy-Ready Clinical Interpretation</h4>",
                #             "<p>", gsub("<[^>]*>", "", clinical_summary), "</p>",
                #             "</div>"
                #         )
                #         self$results$reportSentences$setContent(copy_ready)
                #     }
                # }, error = function(e) {
                #     # Handle clinical summary generation errors gracefully
                #     detailed_error <- paste(.("Clinical summary generation failed:"), e$message)
                #     warning(detailed_error)
                #     
                #     # Provide fallback guidance
                #     fallback_msg <- paste0(
                #         "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                #         "<h4 style='margin-top: 0; color: #856404;'>", .("Manual Interpretation Required"), "</h4>",
                #         "<p>", .("Automatic clinical summary could not be generated. Please review results manually."), "</p>",
                #         "</div>"
                #     )
                #     self$results$reportSentences$setContent(fallback_msg)
                # })

            },

            # ========================================================================
            # STUBBED HELPER METHODS - NOT IMPLEMENTED
            # The following private methods are placeholders for future features.
            # They are NOT wired to any UI controls or output items.
            # ========================================================================

            # PAIRWISE COMPARISONS HELPER - NOT IMPLEMENTED
            # .performPairwiseComparisons ----
            # .performPairwiseComparisons = function() {
                # # Get the labeled data
                # # labelData <- private$.labelData()
                # mydata <- labelData$mydata
                # myvars <- labelData$myvars
                # mygroup <- labelData$mygroup
                # 
                # # Check if we have a valid group variable
                # if (length(mygroup) == 0 || !(mygroup %in% names(mydata))) {
                #     return()
                # }
                # 
                # # Get the group variable data
                # group_var <- mydata[[mygroup]]
                # 
                # # Check if group has more than 2 levels
                # if (!is.factor(group_var)) {
                #     group_var <- as.factor(group_var)
                # }
                # 
                # group_levels <- levels(group_var)
                # n_groups <- length(group_levels)
                # 
                # if (n_groups <= 2) {
                #     note <- paste0(
                #         "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                #         "<strong>Note:</strong> Pairwise comparisons are only performed when the grouping variable has more than 2 levels. ",
                #         "Your grouping variable has ", n_groups, " level(s).",
                #         "</div>"
                #     )
                #     self$results$pairwiseNote$setContent(note)
                #     return()
                # }
                # 
                # # Perform pairwise comparisons for each variable
                # results_list <- list()
                # test_type <- self$options$pcat
                # 
                # for (var in myvars) {
                #     if (!(var %in% names(mydata))) next
                #     
                #     var_data <- mydata[[var]]
                #     
                #     # Skip if not categorical
                #     if (!is.factor(var_data) && !is.character(var_data)) next
                #     
                #     # Generate all pairwise combinations
                #     pairs <- utils::combn(group_levels, 2)
                #     
                #     for (i in 1:ncol(pairs)) {
                #         group1 <- pairs[1, i]
                #         group2 <- pairs[2, i]
                #         
                #         # Subset data for this pair
                #         subset_idx <- group_var %in% c(group1, group2)
                #         subset_var <- var_data[subset_idx]
                #         subset_group <- group_var[subset_idx]
                #         
                #         # Create contingency table
                #         cont_table <- table(subset_var, subset_group)
                #         
                #         # Perform test
                #         test_result <- NULL
                #         # Use intelligent test selection with clinical rationale
                #         test_selection <- private$.selectAppropriateTest(
                #             cont_table,
                #             test_preference = if (test_type == "fisher") "fisher" else "auto",
                #             min_expected = 5
                #         )
                #         
                #         test_name <- test_selection$test
                #         test_rationale <- test_selection$reason
                #         
                #         # Perform the selected test
                #         if (test_selection$test == "fisher") {
                #             tryCatch({
                #                 test_result <- fisher.test(cont_table)
                #                 test_name <- .("Fisher's Exact")
                #             }, error = function(e) {
                #                 test_result <- NULL
                #             })
                #         } else {
                #             tryCatch({
                #                 test_result <- chisq.test(cont_table)
                #                 test_name <- .("Chi-square")
                #             }, error = function(e) {
                #                 test_result <- NULL
                #             })
                #         }
                #         
                #         # Add warning if test selection was based on assumption violations
                #         if (test_selection$warning) {
                #             warning_msg <- paste(.("Test selection warning:"), test_rationale)
                #             # Store warning for later display
                #             if (is.null(self$results$assumptions$content)) {
                #                 assumption_warnings <- warning_msg
                #             } else {
                #                 assumption_warnings <- paste(self$results$assumptions$content, "<br>", warning_msg)
                #             }
                #         }
                #         
                #         if (!is.null(test_result)) {
                #             results_list <- append(results_list, list(list(
                #                 variable = var,
                #                 comparison = paste0(var, ": ", group1, " vs ", group2),
                #                 group1 = group1,
                #                 group2 = group2,
                #                 test = test_name,
                #                 statistic = if (test_name == "Chi-square") test_result$statistic else NA,
                #                 df = if (test_name == "Chi-square") test_result$parameter else NA,
                #                 p_value = test_result$p.value
                #             )))
                #         }
                #     }
                # }
                # 
                # # Apply p-value adjustment if requested
                # if (length(results_list) > 0) {
                #     p_values <- sapply(results_list, function(x) x$p_value)
                #     
                #     if (self$options$p_adjust != "none") {
                #         adjusted_p <- stats::p.adjust(p_values, method = self$options$p_adjust)
                #         for (i in seq_along(results_list)) {
                #             results_list[[i]]$p_adjusted <- adjusted_p[i]
                #         }
                #     } else {
                #         for (i in seq_along(results_list)) {
                #             results_list[[i]]$p_adjusted <- NA
                #         }
                #     }
                #     
                #     # Populate the table
                #     for (result in results_list) {
                #         self$results$pairwiseTable$addRow(
                #             rowKey = paste0(result$comparison, "_", result$test),
                #             values = list(
                #                 comparison = result$comparison,
                #                 group1 = result$group1,
                #                 group2 = result$group2,
                #                 test = result$test,
                #                 statistic = result$statistic,
                #                 df = result$df,
                #                 p_value = result$p_value,
                #                 p_adjusted = result$p_adjusted
                #             )
                #         )
                #     }
                #     
                #     # Add explanatory note
                #     note <- paste0(
                #         "<div style='background-color: #e8f4fd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",
                #         "<strong>Pairwise Comparisons:</strong> All possible pairs of groups have been compared. ",
                #         "Total comparisons: ", length(results_list), ". ",
                #         if (self$options$p_adjust != "none") {
                #             paste0("P-values have been adjusted using the ", self$options$p_adjust, " method to control for multiple testing.")
                #         } else {
                #             "P-values are not adjusted for multiple comparisons."
                #         },
                #         "</div>"
                #     )
                # #     self$results$pairwiseNote$setContent(note)
                # # }
            # },

            # ADVANCED POST-HOC TESTING HELPER - NOT IMPLEMENTED
            # No corresponding UI options in .a.yaml
            # .performAdvancedPosthoc = function() {
                # # Get the labeled data
                # labelData <- private$.labelData()
                # mydata <- labelData$mydata
                # myvars <- labelData$myvars
                # mygroup <- labelData$mygroup
                # 
                # # Perform analyses for each variable
                # for (var in myvars) {
                #     # Create contingency table
                #     contingency_table <- table(mydata[[var]], mydata[[mygroup]])
                #     
                #     # Skip if table is empty or has single dimension
                #     if (length(contingency_table) == 0 || length(dim(contingency_table)) < 2) {
                #         next
                #     }
                #     
                #     # Effect size measures
                #     if (self$options$effect_size_measures) {
                #         private$.calculateEffectSizes(var, contingency_table)
                #     }
                #     
                #     # Residual analysis
                #     if (self$options$residual_analysis) {
                #         private$.performResidualAnalysis(var, contingency_table)
                #     }
                #     
                #     # Advanced post-hoc statistical tests
                #     if (self$options$posthoc_method != "none") {
                #         private$.performPosthocTests(var, contingency_table)
                #     }
                #     
                #     # Correspondence analysis
                #     if (self$options$correspondence_analysis) {
                #         private$.performCorrespondenceAnalysis(var, contingency_table)
                #     }
                # }
                # 
                # # TEMPORARILY DISABLED - plot options not available in .a.yaml
                # # Generate visualizations
                # # if (self$options$mosaic_plot) {
                # #     private$.prepareMosaicPlot()
                # # }

                # # if (self$options$correspondence_analysis) {
                # #     private$.prepareCorrespondencePlot()
                # # }
            # },

            # EFFECT SIZES CALCULATION - NOT IMPLEMENTED
            # No effectsizes output table in .r.yaml
            # .calculateEffectSizes = function(var, contingency_table) {
                # # Calculate various effect size measures
                # n <- sum(contingency_table)
                #
                # # Cramér's V
                # chi_square <- chisq.test(contingency_table)$statistic
                # k <- min(nrow(contingency_table), ncol(contingency_table))
                # cramers_v <- sqrt(chi_square / (n * (k - 1)))
                # 
                # # Phi coefficient (for 2x2 tables)
                # phi <- sqrt(chi_square / n)
                # 
                # # Calculate confidence intervals (using bootstrap approximation)
                # conf_level <- self$options$confidence_level
                # 
                # # Add Cramér's V
                # self$results$effectsizes$addRow(
                #     rowKey = paste0(var, "_cramers_v"),
                #     values = list(
                #         variable = var,
                #         measure = .("Cramér's V"),
                #         value = cramers_v,
                #         ci_lower = max(0, cramers_v - 1.96 * sqrt(cramers_v / n)),
                #         ci_upper = min(1, cramers_v + 1.96 * sqrt(cramers_v / n)),
                #         interpretation = private$.interpretEffectSize(cramers_v, "cramers_v")
                #     )
                # )
                # 
                # # Add Phi coefficient for 2x2 tables
                # if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
                #     self$results$effectsizes$addRow(
                #         rowKey = paste0(var, "_phi"),
                #         values = list(
                #             variable = var,
                #             measure = .("Phi Coefficient"),
                #             value = phi,
                #             ci_lower = max(-1, phi - 1.96 * sqrt(phi / n)),
                #             ci_upper = min(1, phi + 1.96 * sqrt(phi / n)),
                #             interpretation = private$.interpretEffectSize(phi, "phi")
                #         )
                #     )
                # # }
            # },

            # RESIDUAL ANALYSIS - NOT IMPLEMENTED
            # No residuals output table in .r.yaml
            # .performResidualAnalysis = function(var, contingency_table) {
                # # Calculate standardized and adjusted residuals
                # chi_test <- chisq.test(contingency_table)
                # expected <- chi_test$expected
                # observed <- contingency_table
                # 
                # # Standardized residuals
                # std_residuals <- (observed - expected) / sqrt(expected)
                # 
                # # Adjusted residuals
                # row_totals <- rowSums(observed)
                # col_totals <- colSums(observed)
                # n <- sum(observed)
                # 
                # adj_residuals <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
                # 
                # for (i in 1:nrow(observed)) {
                #     for (j in 1:ncol(observed)) {
                #         variance <- expected[i,j] * (1 - row_totals[i]/n) * (1 - col_totals[j]/n)
                #         adj_residuals[i,j] <- (observed[i,j] - expected[i,j]) / sqrt(variance)
                #         
                #         # Calculate contribution to chi-square
                #         contribution <- ((observed[i,j] - expected[i,j])^2 / expected[i,j]) / chi_test$statistic * 100
                #         
                #         self$results$residuals$addRow(
                #             rowKey = paste0(var, "_", i, "_", j),
                #             values = list(
                #                 variable = var,
                #                 group_level = colnames(contingency_table)[j],
                #                 var_level = rownames(contingency_table)[i],
                #                 observed = observed[i,j],
                #                 expected = expected[i,j],
                #                 std_residual = std_residuals[i,j],
                #                 adj_residual = adj_residuals[i,j],
                #                 contribution = contribution
                #             )
                #         )
                #     }
                # # }
            # },

            # POST-HOC STATISTICAL TESTS - NOT IMPLEMENTED
            # No posthocstats output table in .r.yaml
            # .performPosthocTests = function(var, contingency_table) {
                # method <- self$options$posthoc_method
                # 
                # if (method == "standardized_residuals") {
                #     # Test for significant standardized residuals
                #     chi_test <- chisq.test(contingency_table)
                #     
                #     self$results$posthocstats$addRow(
                #         rowKey = paste0(var, "_std_residuals"),
                #         values = list(
                #             variable = var,
                #             method = .("Standardized Residuals"),
                #             statistic = chi_test$statistic,
                #             df = chi_test$parameter,
                #             p_value = chi_test$p.value,
                #             interpretation = ifelse(chi_test$p.value < 0.05,
                #                 "Significant departure from independence",
                #                 "No significant departure from independence")
                #         )
                #     )
                #     
                # } else if (method == "cramers_v") {
                #     # Test significance of Cramér's V
                #     chi_test <- chisq.test(contingency_table)
                #     n <- sum(contingency_table)
                #     k <- min(nrow(contingency_table), ncol(contingency_table))
                #     cramers_v <- sqrt(chi_test$statistic / (n * (k - 1)))
                #     
                #     self$results$posthocstats$addRow(
                #         rowKey = paste0(var, "_cramers_v_test"),
                #         values = list(
                #             variable = var,
                #             method = .("Cramér's V Test"),
                #             statistic = cramers_v,
                #             df = chi_test$parameter,
                #             p_value = chi_test$p.value,
                #             interpretation = private$.interpretEffectSize(cramers_v, "cramers_v")
                #         )
                #     )
                #     
                # } else if (method == "freeman_halton") {
                #     # Freeman-Halton extension of Fisher's exact test
                #     if (self$options$exact_tests) {
                #         tryCatch({
                #             if (requireNamespace("RVAideMemoire", quietly = TRUE)) {
                #                 fh_test <- RVAideMemoire::fisher.multcomp(contingency_table)
                #                 
                #                 self$results$posthocstats$addRow(
                #                     rowKey = paste0(var, "_freeman_halton"),
                #                     values = list(
                #                         variable = var,
                #                         method = .("Freeman-Halton Extension"),
                #                         statistic = NA,
                #                         df = NA,
                #                         p_value = fh_test$p.value,
                #                         interpretation = ifelse(fh_test$p.value < 0.05,
                #                             "Significant association (exact test)",
                #                             "No significant association (exact test)")
                #                     )
                #                 )
                #             }
                #         }, error = function(e) {
                #             # Fallback to regular Fisher's test
                #             fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
                #             self$results$posthocstats$addRow(
                #                 rowKey = paste0(var, "_fisher_simulated"),
                #                 values = list(
                #                     variable = var,
                #                     method = .("Fisher's Exact (Simulated)"),
                #                     statistic = NA,
                #                     df = NA,
                #                     p_value = fisher_test$p.value,
                #                     interpretation = ifelse(fisher_test$p.value < 0.05,
                #                         "Significant association (simulated)",
                #                         "No significant association (simulated)")
                #                 )
                #             )
                #         })
                #     }
                # # }
            # },

            # CORRESPONDENCE ANALYSIS - NOT IMPLEMENTED
            # No correspondenceanalysis output table in .r.yaml
            # .performCorrespondenceAnalysis = function(var, contingency_table) {
                # tryCatch({
                #     if (requireNamespace("ca", quietly = TRUE)) {
                #         ca_result <- ca::ca(contingency_table)
                #         
                #         # Extract eigenvalues and variance explained
                #         eigenvalues <- ca_result$sv^2
                #         total_inertia <- sum(eigenvalues)
                #         variance_explained <- eigenvalues / total_inertia * 100
                #         cumulative_variance <- cumsum(variance_explained)
                #         
                #         # Add results to table
                #         for (i in 1:min(length(eigenvalues), 3)) {  # Show first 3 dimensions
                #             self$results$correspondenceanalysis$addRow(
                #                 rowKey = paste0(var, "_dim", i),
                #                 values = list(
                #                     dimension = paste(.("Dimension"), i),
                #                     eigenvalue = eigenvalues[i],
                #                     variance_explained = variance_explained[i],
                #                     cumulative_variance = cumulative_variance[i],
                #                     inertia = eigenvalues[i] / total_inertia
                #                 )
                #             )
                #         }
                #     }
                # }, error = function(e) {
                #     # Handle CA errors gracefully
                # # })
            # },

            # MOSAIC PLOT - NOT IMPLEMENTED
            # No mosaicplot output in .r.yaml
            # .prepareMosaicPlot = function() {
                # # Prepare data for mosaic plot
                # labelData <- private$.labelData()
                # plot_data <- list(
                #     data = labelData$mydata,
                #     vars = labelData$myvars,
                #     group = labelData$mygroup
                # )
                # 
                # # self$results$mosaicplot$setState(plot_data)
            # },

            # CORRESPONDENCE PLOT - NOT IMPLEMENTED
            # No correspondenceplot output in .r.yaml
            # .prepareCorrespondencePlot = function() {
                # # Prepare data for correspondence analysis plot
                # labelData <- private$.labelData()
                # plot_data <- list(
                #     data = labelData$mydata,
                #     vars = labelData$myvars,
                #     group = labelData$mygroup
                # )
                # 
                # # self$results$correspondenceplot$setState(plot_data)
            # },

            # EFFECT SIZE INTERPRETATION - NOT IMPLEMENTED
            # Helper for effect size calculations (also not implemented)
            # .interpretEffectSize = function(value, type) {
                # if (type == "cramers_v") {
                #     if (value < 0.1) return("Negligible")
                #     else if (value < 0.3) return("Small")
                #     else if (value < 0.5) return("Medium")
                #     else return("Large")
                # } else if (type == "phi") {
                #     if (abs(value) < 0.1) return("Negligible")
                #     else if (abs(value) < 0.3) return("Small")
                #     else if (abs(value) < 0.5) return("Medium")
                #     else return("Large")
                # }
                # # return("Unknown")
            # },

            # MOSAIC PLOT RENDERER - NOT IMPLEMENTED
            # No mosaicplot output in .r.yaml, no mosaic_plot option in .a.yaml
            # .mosaicplot = function(image, ggtheme, theme, ...) {
                # if (is.null(image$state)) return(FALSE)
                # 
                # plot_data <- image$state$data
                # vars <- image$state$vars
                # group <- image$state$group
                # 
                # tryCatch({
                #     if (requireNamespace("vcd", quietly = TRUE) && length(vars) >= 1) {
                #         # Create mosaic plot using vcd package
                #         var <- vars[1]  # Use first variable
                #         
                #         # Create contingency table
                #         tbl <- table(plot_data[[var]], plot_data[[group]])
                #         
                #         # Generate mosaic plot
                #         vcd::mosaic(tbl, 
                #                main = paste(.("Mosaic Plot:"), var, .("by"), group),
                #                shade = TRUE,
                #                legend = TRUE)
                #         return(TRUE)
                #     }
                # }, error = function(e) {
                #     # Fallback: create a simple bar plot
                #     if (length(vars) >= 1) {
                #         var <- vars[1]
                #         tbl <- table(plot_data[[var]], plot_data[[group]])
                #         barplot(tbl, 
                #                main = paste(.("Association:"), var, .("by"), group),
                #                beside = TRUE,
                #                legend = TRUE,
                #                col = rainbow(nrow(tbl)))
                #         return(TRUE)
                #     }
                # })
                # 
                # # return(FALSE)
            # },

            # CORRESPONDENCE PLOT RENDERER - NOT IMPLEMENTED
            # No correspondenceplot output in .r.yaml, no correspondence_analysis option in .a.yaml
            # .correspondenceplot = function(image, ggtheme, theme, ...) {
                # if (is.null(image$state)) return(FALSE)
                #
                # plot_data <- image$state$data
                # vars <- image$state$vars
                # group <- image$state$group
                #
                # tryCatch({
                #     if (requireNamespace("ca", quietly = TRUE) && length(vars) >= 1) {
                #         var <- vars[1]  # Use first variable
                #
                #         # Create contingency table
                #         tbl <- table(plot_data[[var]], plot_data[[group]])
                #
                #         # Perform correspondence analysis
                #         ca_result <- ca::ca(tbl)
                #
                #         # Create biplot
                #         plot(ca_result,
                #              main = paste(.("Correspondence Analysis:"), var, .("by"), group))
                #         return(TRUE)
                #     }
                # }, error = function(e) {
                #     # Handle errors gracefully
                # })
                #
                # # return(FALSE)
            # },

            # ===== Mantel-Haenszel Test (Private Helper Functions) =====
            # Added for Kemp 2015 study requirements - non-breaking enhancement

            # Private helper: Perform Mantel-Haenszel chi-square test
            .performMantelHaenszel = function(row_var, col_var, strata_var, data) {
                tryCatch({
                    # Create 3-way contingency table
                    tbl_3way <- table(data[[row_var]], data[[col_var]], data[[strata_var]])

                    # Check if we have at least 2 strata with data
                    n_strata <- dim(tbl_3way)[3]
                    if (n_strata < 2) {
                        return(list(
                            success = FALSE,
                            error = sprintf("Mantel-Haenszel requires at least 2 strata. Found: %d", n_strata)
                        ))
                    }

                    # Check for 2x2 tables in each stratum
                    if (dim(tbl_3way)[1] != 2 || dim(tbl_3way)[2] != 2) {
                        return(list(
                            success = FALSE,
                            error = "Mantel-Haenszel requires 2x2 tables. Use binary variables or recode."
                        ))
                    }

                    # Apply Haldane-Anscombe correction if any zero cell to stabilize OR/CI
                    if (any(tbl_3way == 0)) {
                        tbl_3way <- tbl_3way + 0.5
                    }

                    # Perform Mantel-Haenszel test (base R function)
                    mh_result <- stats::mantelhaen.test(
                        x = tbl_3way,
                        correct = TRUE  # Continuity correction
                    )

                    return(list(
                        success = TRUE,
                        statistic = mh_result$statistic,
                        df = mh_result$parameter,
                        p_value = mh_result$p.value,
                        common_or = mh_result$estimate,
                        conf_int = mh_result$conf.int,
                        n_strata = n_strata,
                        method = mh_result$method
                    ))
                }, error = function(e) {
                    return(list(
                        success = FALSE,
                        error = paste("Mantel-Haenszel test failed:", e$message)
                    ))
                })
            },

            # Private helper: Breslow-Day test for homogeneity of odds ratios
            .performBreslowDay = function(row_var, col_var, strata_var, data) {
                tryCatch({
                    # Create 3-way contingency table
                    tbl_3way <- table(data[[row_var]], data[[col_var]], data[[strata_var]])

                    # Calculate OR for each stratum
                    n_strata <- dim(tbl_3way)[3]
                    strata_names <- dimnames(tbl_3way)[[3]]
                    or_vec <- numeric(n_strata)

                    for (k in 1:n_strata) {
                        tbl_2x2 <- tbl_3way[,,k]
                        # Calculate odds ratio: (a*d) / (b*c)
                        a <- tbl_2x2[1,1]
                        b <- tbl_2x2[1,2]
                        c <- tbl_2x2[2,1]
                        d <- tbl_2x2[2,2]

                        # Avoid division by zero
                        if (b == 0 || c == 0) {
                            or_vec[k] <- NA
                        } else {
                            or_vec[k] <- (a * d) / (b * c)
                        }
                    }

                    # Prefer robust implementation if available
                    if (requireNamespace("DescTools", quietly = TRUE)) {
                        bd <- DescTools::BreslowDayTest(tbl_3way, correct = TRUE)
                        bd_chisq <- as.numeric(bd$statistic)
                        bd_df <- as.numeric(bd$parameter)
                        bd_p <- as.numeric(bd$p.value)
                    } else {
                        # Fallback: signal limitation
                        return(list(
                            success = FALSE,
                            error = "Breslow-Day test requires DescTools::BreslowDayTest; install DescTools for this feature."
                        ))
                    }

                    return(list(
                        success = TRUE,
                        statistic = bd_chisq,
                        df = bd_df,
                        p_value = bd_p,
                        strata_or = or_vec,
                        strata_names = strata_names,
                        common_or = NA
                    ))
                }, error = function(e) {
                    return(list(
                        success = FALSE,
                        error = paste("Breslow-Day test failed:", e$message)
                    ))
                })
            },

            # Private helper: Format M-H results for display
            .formatMantelHaenszelResults = function(mh_result, bd_result = NULL) {
                if (!mh_result$success) {
                    return(paste0(
                        "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>Mantel-Haenszel Test Error</h4>",
                        "<p>", mh_result$error, "</p>",
                        "</div>"
                    ))
                }

                html <- paste0(
                    "<div style='background-color: #e8f4fd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",
                    "<h4 style='margin-top: 0; color: #1976D2;'>Mantel-Haenszel Chi-Square Test</h4>",
                    "<p><strong>Purpose:</strong> Tests association between row and column variables while controlling for stratification variable.</p>",

                    "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                    "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>Chi-square statistic:</strong></td>",
                    "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", sprintf("%.3f", mh_result$statistic), "</td></tr>",
                    "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>Degrees of freedom:</strong></td>",
                    "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", mh_result$df, "</td></tr>",
                    "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>P-value:</strong></td>",
                    "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", sprintf("%.4f", mh_result$p_value), "</td></tr>",
                    "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>Common odds ratio:</strong></td>",
                    "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", sprintf("%.3f", mh_result$common_or), "</td></tr>",
                    "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>95% CI for OR:</strong></td>",
                    "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>",
                    sprintf("(%.3f, %.3f)", mh_result$conf_int[1], mh_result$conf_int[2]), "</td></tr>",
                    "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>Number of strata:</strong></td>",
                    "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", mh_result$n_strata, "</td></tr>",
                    "</table>",

                    "<p><strong>Interpretation:</strong></p>",
                    "<ul>",
                    "<li><strong>Common OR = 1:</strong> No association between variables (after controlling for strata)</li>",
                    "<li><strong>Common OR > 1:</strong> Positive association (exposure increases odds of outcome)</li>",
                    "<li><strong>Common OR < 1:</strong> Negative association (exposure decreases odds of outcome)</li>",
                    "</ul>"
                )

                # Add Breslow-Day results if available
                if (!is.null(bd_result) && bd_result$success) {
                    html <- paste0(html,
                        "<h4 style='margin-top: 15px; color: #1976D2;'>Breslow-Day Test for Homogeneity</h4>",
                        "<p><strong>Purpose:</strong> Tests whether odds ratios are homogeneous (constant) across strata.</p>",

                        "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                        "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>Chi-square statistic:</strong></td>",
                        "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", sprintf("%.3f", bd_result$statistic), "</td></tr>",
                        "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>Degrees of freedom:</strong></td>",
                        "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", bd_result$df, "</td></tr>",
                        "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'><strong>P-value:</strong></td>",
                        "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", sprintf("%.4f", bd_result$p_value), "</td></tr>",
                        "</table>",

                        "<p><strong>Interpretation:</strong></p>",
                        "<ul>",
                        "<li><strong>p > 0.05:</strong> Odds ratios are homogeneous across strata (common OR valid)</li>",
                        "<li><strong>p ≤ 0.05:</strong> Odds ratios vary across strata (use stratified analysis instead)</li>",
                        "</ul>",

                        "<p><strong>Stratum-specific odds ratios:</strong></p>",
                        "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                        "<tr style='background-color: #f0f0f0;'>",
                        "<th style='padding: 5px; border-bottom: 1px solid #ccc; text-align: left;'>Stratum</th>",
                        "<th style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>Odds Ratio</th>",
                        "</tr>"
                    )

                    for (i in seq_along(bd_result$strata_or)) {
                        or_val <- if (is.na(bd_result$strata_or[i])) "NA (zero cells)" else sprintf("%.3f", bd_result$strata_or[i])
                        html <- paste0(html,
                            "<tr><td style='padding: 5px; border-bottom: 1px solid #ccc;'>", bd_result$strata_names[i], "</td>",
                            "<td style='padding: 5px; border-bottom: 1px solid #ccc; text-align: right;'>", or_val, "</td></tr>"
                        )
                    }

                    html <- paste0(html, "</table>")
                }

                html <- paste0(html, "</div>")

                return(html)
            },

            # Dummy placeholder for last item in private list
            .dummy = function() {
                # This is a placeholder to maintain proper R6 syntax
                return(invisible(NULL))
            }
        )
    )
