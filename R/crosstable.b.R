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
#' @importFrom labelled set_variable_labels var_label
#' @importFrom purrr partial
#' @importFrom magrittr %>%
#' @noRd
NULL

# Helper function to create styled HTML notice (replaces jmvcore::Notice to avoid serialization errors)
# Security note: `message` is HTML-escaped via htmltools::htmlEscape() at the
# interpolation site below, so a future caller passing a variable name or factor
# label cannot inject markup. (Escaping is applied — do not remove it.)
.createNoticeHTML <- function(message, type = c("ERROR", "STRONG_WARNING", "WARNING", "INFO")) {
    type <- match.arg(type)

    # Define styles for each notice type
    styles <- list(
        ERROR = list(
            bg = "#f8d7da",
            border = "#dc3545",
            icon = "",
            title_color = "#721c24"
        ),
        STRONG_WARNING = list(
            bg = "#fff3cd",
            border = "#ff9800",
            icon = "",
            title_color = "#856404"
        ),
        WARNING = list(
            bg = "#fff3cd",
            border = "#ffc107",
            icon = "",
            title_color = "#856404"
        ),
        INFO = list(
            bg = "#d1ecf1",
            border = "#17a2b8",
            icon = "",
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
        htmltools::htmlEscape(message),
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
            " \u{2192} ", paste(duplicate_cleaned, collapse = ", ")
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

# Helper function to validate sample size and data quality
# name_mapping: optional named character vector mapping cleaned -> original variable
# names so user-facing warnings use the labels users actually selected.
.validateAnalysisAssumptions <- function(mydata, myvars, mygroup, name_mapping = NULL) {
    issues <- list()
    warnings <- list()

    .display <- function(name) {
        if (!is.null(name_mapping) && !is.null(name_mapping[[name]]))
            return(name_mapping[[name]])
        name
    }

    # Check overall sample size
    n_total <- nrow(mydata)
    if (n_total < 20) {
        issues <- append(issues, sprintf("Very small sample size (n = %d). Results may be unreliable.", n_total))
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
                                 (is.numeric(mydata[[var]]) && length(unique(stats::na.omit(mydata[[var]]))) <= 6)

                if (is_categorical) {
                    cont_table <- table(mydata[[var]], mydata[[mygroup]])
                    if (any(cont_table == 0)) {
                        warnings <- append(warnings, paste0("Empty cells detected in ", .display(var), " \u{D7} ", .display(mygroup), " table. Results may be unstable."))
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
                warnings <- append(warnings, paste0("High missing data in ", .display(var), " (", round(missing_pct, 1), "%). Consider imputation or sensitivity analysis."))
            }
        }
    }

    return(list(
        critical_issues = issues,
        warnings = warnings,
        sample_size = n_total
    ))
}

#' @title Cross Tables Analysis Class
#'
#' @description R6 class for generating cross tables for clinicopathological comparisons.
#' @name crosstableClass
#' @importFrom R6 R6Class
#' @return An \code{R6} class generator object for the \code{crosstableClass} backend; used internally by the jamovi analysis wrapper and not called directly.
crosstableClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "crosstableClass",
        inherit = crosstableBase,
        private = list(
            .htmlSafeTableData = function(data) {
                escape <- function(value) {
                    as.character(htmltools::htmlEscape(as.character(value)))
                }

                for (i in seq_along(data)) {
                    value <- data[[i]]
                    if (is.factor(value)) {
                        levels(value) <- escape(levels(value))
                    } else if (is.character(value)) {
                        value[] <- escape(value)
                    }

                    label <- attr(value, "label", exact = TRUE)
                    if (is.null(label))
                        label <- names(data)[i]
                    attr(value, "label") <- escape(label)

                    units <- attr(value, "units", exact = TRUE)
                    if (is.character(units))
                        attr(value, "units") <- escape(units)

                    value_labels <- attr(value, "labels", exact = TRUE)
                    if (!is.null(value_labels) && !is.null(names(value_labels))) {
                        names(value_labels) <- escape(names(value_labels))
                        attr(value, "labels") <- value_labels
                    }

                    data[[i]] <- value
                }

                data
            },

            # .reportTableError ----
            # Route a table-builder failure to the styled HTML error notice
            # instead of surfacing a raw R error. conditionMessage(e) is
            # HTML-escaped inside .createNoticeHTML().
            .reportTableError = function(e) {
                error_html <- .createNoticeHTML(
                    paste0("Table generation failed: ", conditionMessage(e)),
                    type = "ERROR"
                )
                self$results$errorNotice$setContent(error_html)
                self$results$errorNotice$setVisible(TRUE)
            },

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
                    jmvcore::reject("Variable name issues detected: {}",
                                    paste(validation_results$issues, collapse = "; "))
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
                    jmvcore::reject("Variable matching failed: {}", e$message)
                })

                # Report warnings about variable names if any
                if (length(validation_results$warnings) > 0) {
                    # Escape each warning individually before joining with <br> so the
                    # line-break markup is preserved while user-supplied column names
                    # embedded in the warning text are rendered inert.
                    warning_msg <- paste0(
                        "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                        "<strong> Variable Name Warnings:</strong><br>",
                        paste(htmltools::htmlEscape(unlist(validation_results$warnings)), collapse = "<br>"),
                        "</div>"
                    )
                    # Display variable-name warnings in their own item so they do
                    # not clobber the finalfit methodology note (which uses todo2).
                    self$results$varNameWarnings$setContent(warning_msg)
                    self$results$varNameWarnings$setVisible(TRUE)
                }

                # If any user-specified variable could not be matched, block analysis
                if (length(myvars) == 0 || length(mygroup) == 0) {
                    jmvcore::reject("Selected variables could not be matched to the dataset after cleaning labels. Please reselect variables.")
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
                        "When comparing multiple variables across groups, the chance of finding at least one false positive increases. FWER methods (Bonferroni/Holm) provide <strong>strong control</strong> - ensuring the probability of ANY false positive stays below \u{3B1} (typically 0.05).</p>",

                        "<p><strong>Interpretation Guidelines:</strong></p>",
                        "<ul>",
                        "<li><strong>Adjusted p < 0.05:</strong> Statistically significant - strong evidence against null hypothesis</li>",
                        "<li><strong>Adjusted p \u{2265} 0.05:</strong> Not significant after correction for multiple testing</li>",
                        "<li><strong>Note:</strong> Adjusted p-values are typically <em>larger</em> than raw p-values (more conservative)</li>",
                        "</ul>",

                        "<p><strong>When to use FWER control:</strong></p>",
                        "<ul>",
                        "<li> Confirmatory studies where even one false positive is unacceptable</li>",
                        "<li> Clinical trials with regulatory requirements</li>",
                        "<li> When you have strong prior hypotheses to test</li>",
                        "</ul>",

                        "<p><em> FWER methods are conservative - you may miss true effects to avoid false positives. Consider FDR methods (BH/BY) for exploratory research.</em></p>",
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
                        "<li> Exploratory analyses with many comparisons</li>",
                        "<li> Genomic/proteomic studies with thousands of tests</li>",
                        "<li> Hypothesis-generating research where you can tolerate some false positives</li>",
                        "</ul>",

                        "<p><em> FDR methods are less conservative than FWER - you'll discover more effects but accept a small proportion of false positives.</em></p>",
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

                        "<p style='margin-top: 15px;'><em> Tip: Use gtsummary style for publication-ready tables with automatic q-values and FDR correction.</em></p>",
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
                self$results$subtitle$setContent(paste0("Cross Table Analysis - Grouped by ", htmltools::htmlEscape(group_display)))

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
                formula <- jmvcore::asFormula(formula)

                # Exclude missing data if requested.
                if (self$options$excl)
                    mydata <- jmvcore::naOmit(mydata)
                
                # Validate analysis assumptions and data quality
                validation_results <- .validateAnalysisAssumptions(
                    mydata,
                    myvars,
                    mygroup,
                    name_mapping = original_names_mapping
                )
                validation_messages <- c(
                    validation_results$critical_issues,
                    validation_results$warnings
                )
                data_quality_html <- ""
                if (length(validation_messages) > 0) {
                    # Accumulate all warnings into HTML (avoid serialization errors from dynamic Notice inserts)
                    warning_html_parts <- character(length(validation_messages))

                    for (i in seq_along(validation_messages)) {
                        warn <- validation_messages[[i]]

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
                    data_quality_html <- paste(warning_html_parts, collapse = "\n")
                    self$results$dataQualityNotice$setContent(data_quality_html)
                    self$results$dataQualityNotice$setVisible(TRUE)
                } else {
                    self$results$dataQualityNotice$setContent("")
                    self$results$dataQualityNotice$setVisible(FALSE)
                }

                # Generate table based on selected style.
                if (sty == "arsenal") {
                    private$.checkpoint()
                    arsenal_control <- arsenal::tableby.control(
                        test = TRUE,
                        total = TRUE,
                        numeric.test = if (self$options$cont == "mean") "anova" else "kwt",
                        cat.test = if (self$options$pcat == "fisher") "fe" else "chisq",
                        numeric.stats = if (self$options$cont == "mean") c("Nmiss", "meansd") else c("Nmiss", "median", "q1q3"),
                        stats.labels = list(meansd = "Mean (SD)", median = "Median", q1q3 = "Q1, Q3")
                    )

                    arsenal_data <- private$.htmlSafeTableData(mydata)
                    tablearsenal <- tryCatch(arsenal::tableby(
                        formula = formula,
                        data = arsenal_data,
                        control = arsenal_control,
                        digits = 1,
                        digits.count = 1
                    ), error = function(e) { private$.reportTableError(e); NULL })
                    if (is.null(tablearsenal)) return()
                    # Render Arsenal's own markup after escaping every data-derived
                    # label and value on a render-only copy.
                    tablearsenal <- summary(
                        tablearsenal,
                        text = "html",
                        pfootnote = "html"
                    )
                    tablearsenal <- paste(
                        capture.output(tablearsenal),
                        collapse = "\n"
                    )
                    self$results$tablestyle1$setContent(tablearsenal)
                } else if (sty == "finalfit") {
                    myvars_term <- jmvcore::composeTerm(components = myvars)
                    myvars_term <- jmvcore::decomposeTerm(term = myvars_term)
                    private$.checkpoint()
                    # Create the finalfit summary table.
                    tablefinalfit <- tryCatch(mydata %>%
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
                        ), error = function(e) { private$.reportTableError(e); NULL })
                    if (is.null(tablefinalfit)) return()
                    tablefinalfit <- kableExtra::kable(
                        tablefinalfit,
                        format = "html",
                        digits = 1,
                        escape = TRUE
                    )
                    self$results$tablestyle2$setContent(tablefinalfit)
                } else if (sty == "gtsummary") {
                    private$.checkpoint()
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
                    is.factor(v) || is.character(v) || (is.numeric(v) && length(unique(stats::na.omit(v))) <= 6)
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

                # Map user options to gtsummary syntax
                stats_cont <- if (self$options$cont == "mean") "{mean} ({sd})" else "{median} ({p25}, {p75})"

                # Note: gtsummary has excellent default test selection:
                # - Categorical: chi-square (automatically switches to Fisher when expected counts < 5)
                # - Continuous 2 groups: Welch t-test (robust to unequal variances)
                # - Continuous 3+ groups: ANOVA (oneway.test)
                # User preferences for cont/pcat are still respected in arsenal and finalfit styles

                tablegtsummary <- tryCatch(
                  mydata_subset %>%
                  tbl_summary(
                    by = dplyr::all_of(mygroup),
                    statistic = list(
                      all_continuous()  ~ stats_cont,
                      all_categorical() ~ "{n}/{N} ({p}%)"
                    ),
                    digits       = all_continuous() ~ 2,
                    missing_text = "(Missing)",
                    type = list(
                        dplyr::all_of(cat_vars) ~ "categorical",
                        dplyr::all_of(cont_vars) ~ "continuous"
                    )
                  ) %>%
                  add_n() %>%
                  add_overall() %>%
                  add_p(pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)),
                  error = function(e) { private$.reportTableError(e); NULL })
                if (is.null(tablegtsummary)) return()

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
                        data_quality_html <- paste(
                            c(data_quality_html, single_var_warning),
                            collapse = "\n"
                        )
                        self$results$dataQualityNotice$setContent(data_quality_html)
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
                            "<li><strong>Q-value = 0.05:</strong> Among all variables with q \u{2264} 0.05, expect ~5% to be false positives</li>",
                            "<li><strong>Q-value = 0.10:</strong> Expect ~10% false positives (acceptable in exploratory research)</li>",
                            "<li><strong>Q-values are larger than raw p-values</strong> but smaller than FWER-adjusted p-values</li>",
                            "</ul>",

                            "<p><strong>When to use FDR control:</strong></p>",
                            "<ul>",
                            "<li> Exploratory analyses where discovering patterns is the goal</li>",
                            "<li> Genomic/proteomic studies with hundreds or thousands of tests</li>",
                            "<li> Screening studies to generate hypotheses for follow-up</li>",
                            "<li> When you can tolerate a small proportion of false positives</li>",
                            "</ul>",

                            "<p><strong>Important considerations:</strong></p>",
                            "<ul>",
                            "<li> FDR methods assume independence or positive dependence between tests</li>",
                            "<li> Less conservative than Bonferroni/Holm - you'll find more discoveries but accept some false positives</li>",
                            "<li> Should not replace careful hypothesis planning and validation</li>",
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
                            "<li><strong>Adjusted p \u{2265} 0.05:</strong> Not significant after correction</li>",
                            "<li><strong>Adjusted p-values are much larger than raw p-values</strong> (very conservative correction)</li>",
                            "</ul>",

                            "<p><strong>When to use FWER control:</strong></p>",
                            "<ul>",
                            "<li> Confirmatory studies where false positives are costly</li>",
                            "<li> Clinical trials with regulatory requirements</li>",
                            "<li> When testing pre-specified hypotheses</li>",
                            "<li> When even a single false positive is unacceptable</li>",
                            "</ul>",

                            "<p><strong>Important considerations:</strong></p>",
                            "<ul>",
                            "<li> Very conservative - may miss true effects (reduced power)</li>",
                            "<li> With many tests (>10), corrections can be extremely stringent</li>",
                            "<li> Consider FDR methods (BH/BY) for exploratory research with many tests</li>",
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
                    private$.checkpoint()
                    sty_term <- jmvcore::composeTerm(components = self$options$sty)
                    # Escape data-derived factor levels and character cell values on a
                    # render-only copy before tangram injects them into the type:Html
                    # output, mirroring the arsenal branch. Raw values would otherwise
                    # reach the jamovi webview unescaped (HTML/JS injection risk).
                    tangram_data <- private$.htmlSafeTableData(mydata)
                    tabletangram <- tryCatch(tangram::html5(
                        tangram::tangram(
                            paste(deparse(formula), collapse = " "),
                            tangram_data,
                            transform = tangram::hmisc,
                            id = "tbl3",
                            test = TRUE,
                            digits = 1,
                            include_p = TRUE
                        ),
                        fragment = TRUE,
                        style = sty_term,
                        caption = paste0(
                            "Cross Table for Dependent ",
                            htmltools::htmlEscape(
                                .getDisplayName(mygroup, original_names_mapping)
                            )
                        ),
                        id = "tbl3"
                    ), error = function(e) { private$.reportTableError(e); NULL })
                    if (is.null(tabletangram)) return()
                    self$results$tablestyle4$setContent(tabletangram)
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

                # Standardized mean differences (balance diagnostic)
                if (isTRUE(self$options$showSMD))
                    private$.populateSMD(
                        data = mydata,
                        vars = myvars,
                        group = mygroup,
                        name_mapping = original_names_mapping
                    )

            },

            # ----------------------------------------------------------------
            # Standardized mean differences (balance diagnostic for two groups)
            # ----------------------------------------------------------------
            .populateSMD = function(data, vars, group, name_mapping = NULL) {
                tab <- self$results$smdTable
                if (length(vars) == 0 || length(group) == 0) return()

                g <- data[[group]]
                if (!is.factor(g)) g <- as.factor(g)
                g <- droplevels(g)
                if (nlevels(g) != 2) {
                    tab$setNote("smd",
                        "Standardized mean differences require exactly two groups; the SMD table is shown only for a two-level grouping variable.")
                    return()
                }
                levs <- levels(g)
                i1 <- g == levs[1]; i2 <- g == levs[2]

                for (v in vars) {
                    x <- data[[v]]
                    isNum <- is.numeric(x) || (is.integer(x) && !is.factor(x))
                    smd <- NA_real_; vtype <- "categorical"
                    if (isNum) {
                        vtype <- "continuous"
                        x1 <- x[i1]; x2 <- x[i2]
                        smd <- private$.smdContinuous(x1, x2)
                    } else {
                        smd <- private$.smdCategorical(x[i1], x[i2])
                    }
                    a <- abs(smd)
                    bal <- if (is.na(a)) "-"
                           else if (a < 0.1) "negligible (< 0.1)"
                           else if (a < 0.2) "small (0.1-0.2)"
                           else "notable (>= 0.2)"
                    display_name <- .getDisplayName(v, name_mapping)
                    tab$addRow(rowKey = v, values = list(
                        variable = display_name, vtype = vtype, smd = smd,
                        absSMD = a, balance = bal))
                }
                tab$setNote("smd", sprintf(
                    "SMD between '%s' and '%s'. Continuous: (m1 - m2) / sqrt((s1^2 + s2^2)/2). Categorical: multinomial SMD (Yang & Dalton, 2012). |SMD| < 0.1 conventionally indicates negligible imbalance.",
                    levs[1], levs[2]))
            },

            .smdContinuous = function(x1, x2) {
                x1 <- x1[!is.na(x1)]; x2 <- x2[!is.na(x2)]
                if (length(x1) < 2 || length(x2) < 2) return(NA_real_)
                s1 <- stats::var(x1); s2 <- stats::var(x2)
                denom <- sqrt((s1 + s2) / 2)
                if (!is.finite(denom) || denom == 0) return(NA_real_)
                (mean(x1) - mean(x2)) / denom
            },

            .smdCategorical = function(x1, x2) {
                x1 <- x1[!is.na(x1)]; x2 <- x2[!is.na(x2)]
                lv <- union(levels(factor(x1)), levels(factor(x2)))
                k <- length(lv)
                if (k < 2 || length(x1) < 1 || length(x2) < 1) return(NA_real_)
                p1 <- as.numeric(prop.table(table(factor(x1, levels = lv))))
                p2 <- as.numeric(prop.table(table(factor(x2, levels = lv))))
                if (k == 2) {
                    # binary reduces to the two-proportion SMD
                    a <- p1[1]; b <- p2[1]
                    denom <- sqrt((a * (1 - a) + b * (1 - b)) / 2)
                    if (!is.finite(denom) || denom == 0) return(NA_real_)
                    return((a - b) / denom)
                }
                # multinomial SMD (Yang & Dalton 2012): drop last (reference) level
                P1 <- p1[-k]; P2 <- p2[-k]
                Tm <- P1 - P2
                covm <- function(P) { M <- -outer(P, P); diag(M) <- P * (1 - P); M }
                S <- (covm(P1) + covm(P2)) / 2
                Sinv <- tryCatch(MASS::ginv(S), error = function(e) NULL)
                if (is.null(Sinv)) return(NA_real_)
                val <- as.numeric(t(Tm) %*% Sinv %*% Tm)
                if (!is.finite(val) || val < 0) return(NA_real_)
                sqrt(val)
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for Cross Table analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                vars <- self$options$vars
                group <- self$options$group

                if (is.null(vars) || length(vars) == 0 || is.null(group))
                    return('')

                # `deparse()` produces correctly quoted R literals - handles spaces,
                # internal quotes, and backslashes, and is identical to the old output
                # for syntactic names. (Backticks belong on bare symbols, not inside
                # double-quoted string literals.)
                vars_arg  <- paste0('vars = ',  paste(deparse(vars),  collapse = ' '))
                group_arg <- paste0('group = ', deparse(group))

                # Get other arguments using base helper (if available).
                # .asArgs re-emits every option, so strip vars/group to avoid
                # duplicating the manually-built (correctly backtick-quoted) versions.
                args <- ''
                if (!is.null(private$.asArgs)) {
                    args <- private$.asArgs(incData = FALSE)
                }
                if (args != '') {
                    args_lines <- strsplit(args, ",\\s*\\n\\s*")[[1]]
                    args_lines <- args_lines[!grepl("^\\s*(vars|group)\\s*=", args_lines)]
                    args <- if (length(args_lines) > 0) paste(args_lines, collapse = ',\n    ') else ''
                }
                if (args != '')
                    args <- paste0(',\n    ', args)

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

                # Build complete function call
                paste0(pkg_name, '::crosstable(\n    data = data,\n    ',
                       vars_arg, ',\n    ', group_arg, args, ')')
            }
        ) # End of public list
    )
