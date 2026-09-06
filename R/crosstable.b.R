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
#'   \item Test selection (chi-square, Fisher's exact, ANOVA, Kruskal-Wallis; which one
#'         applies depends on the table style and on whether means or medians are shown)
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
#' @param cont A string ("mean" or "median") giving the summary shown for continuous
#'            variables. In the arsenal and finalfit styles this also selects the test
#'            (ANOVA for "mean", Kruskal-Wallis for "median"); in the gtsummary style it
#'            changes only the displayed statistic, since gtsummary always tests
#'            continuous variables with a rank-based test.
#' @param pcat A string ("chisq" or "fisher") giving the test for categorical variables.
#'            Applied by the arsenal, finalfit and gtsummary styles; the NEJM, Lancet and
#'            hmisc styles use their own built-in tests.
#' @param p_adjust A string naming the multiple-testing correction applied across
#'            variables ("none", "bonferroni", "holm", "BH", "BY"). Available in the
#'            gtsummary style only.
#' @param showSMD Logical. If TRUE, adds a standardized mean difference table
#'            quantifying between-group balance. Requires exactly two groups.
#' @param showSummary Logical. If TRUE, adds a copy-ready plain-language paragraph
#'            below the table: sample size, groups, the tests the chosen style applied,
#'            and the variables that differed at p < 0.05 (q < 0.05 under a gtsummary
#'            p-value adjustment).
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
# label cannot inject markup. (Escaping is applied - do not remove it.)
.crosstableNoticeHTML <- function(message, type = c("ERROR", "STRONG_WARNING", "WARNING", "INFO")) {
    type <- match.arg(type)

    # Define styles for each notice type.
    # Backgrounds are translucent tints that composite to the original pastel
    # over a white pane, and the text colour follows the pane ("color: inherit"),
    # so the panel stays readable in jamovi's dark theme. The border-left colour
    # is opaque because it carries the severity signal.
    styles <- list(
        ERROR = list(
            bg = "rgba(216, 33, 50, 0.18)",
            border = "#dc3545"
        ),
        STRONG_WARNING = list(
            bg = "rgba(255, 202, 33, 0.23)",
            border = "#ff9800"
        ),
        WARNING = list(
            bg = "rgba(255, 202, 33, 0.23)",
            border = "#ffc107"
        ),
        INFO = list(
            bg = "rgba(33, 163, 188, 0.21)",
            border = "#17a2b8"
        )
    )

    style <- styles[[type]]

    html <- paste0(
        "<div style='background-color: ", style$bg, "; color: inherit; ",
        "padding: 15px; margin: 10px 0; border-radius: 5px; ",
        "border-left: 4px solid ", style$border, ";'>",
        "<p style='margin: 0; color: inherit;'>",
        "<strong>", type, ":</strong> ",
        htmltools::htmlEscape(message),
        "</p>",
        "</div>"
    )

    return(html)
}

# Helper function to escape variable names with special characters.
# Defensive only: the sole caller passes names that janitor::clean_names() has
# already reduced to [a-z0-9_] in .labelData(), so nothing matches the pattern
# below in practice and the formula never carries a backticked term. Kept so a
# future change to the name-cleaning step cannot silently produce an invalid
# formula. Use it ONLY when building formula text - never as a data[[]] key,
# where the backticks would turn the lookup into NULL.
.crosstableEscapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

# The display-table variable-typing rule.
# A numeric column with 6 or fewer distinct non-missing values is almost always
# an encoded category (grade 1/2/3, stage codes) rather than a measurement, and
# a mean or an ANOVA on those codes is not a meaningful summary. The data-quality
# checks, the coded-variable note and the gtsummary table all call this one
# function so they cannot drift apart. The SMD table is the deliberate exception:
# it keeps numeric codes on the continuous SMD (see .populateSMD for why) and
# labels such rows "continuous (numeric codes)" so the two never contradict.
.crosstableIsCategorical <- function(v) {
    is.factor(v) || is.character(v) ||
        (is.numeric(v) && length(unique(stats::na.omit(v))) <= 6)
}

# Helper function to get display name from mapping
.crosstableDisplayName <- function(cleaned_name, name_mapping) {
    # Get original display name from mapping
    original_name <- name_mapping[[cleaned_name]]
    if (is.null(original_name)) {
        return(unname(cleaned_name))  # Fallback to cleaned name
    }
    # unname() is load-bearing: the mapping can carry names through, and
    # jmvcore's Table$setRow() matches rowKeys with identical(), which is
    # type-strict -- c(num = "num") is NOT identical to "num", so a named
    # result makes setRow() fail with "rowKey '<name>' not found".
    return(unname(original_name))
}

# Helper function to validate variable names. janitor::clean_names() already
# de-duplicates, so the duplicate check is defensive. Names with spaces or
# punctuation round-trip correctly through the label mapping and are NOT
# reported: a warning the user cannot act on is noise.
.validateCrosstableVariableNames <- function(original_names, cleaned_names) {
    issues <- list()

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

    return(list(issues = issues))
}

# Helper function to validate sample size and data quality.
# name_mapping: optional named character vector mapping cleaned -> original variable
# names so user-facing warnings use the labels users actually selected.
# full_data: the data BEFORE missing-value exclusion, used only for the
# missing-percentage check (after naOmit() there is nothing left to count).
# sty / pcat: the selected style and categorical test, which decide whether a
# chi-square will actually be run on a sparse table.
# Returns typed messages (type = STRONG_WARNING/WARNING, text) so the caller
# renders severity from data instead of parsing it back out of the wording.
.validateAnalysisAssumptions <- function(mydata, myvars, mygroup, name_mapping = NULL,
                                         full_data = mydata, sty = "gtsummary", pcat = "chisq") {
    # Accumulate into an environment so the helper needs no `<<-`.
    acc <- new.env(parent = emptyenv())
    acc$messages <- list()
    add <- function(type, text) acc$messages[[length(acc$messages) + 1]] <- list(type = type, text = text)
    group_sizes <- integer(0)

    .display <- function(name) {
        if (!is.null(name_mapping) && !is.null(name_mapping[[name]]))
            return(name_mapping[[name]])
        name
    }

    # Check overall sample size
    n_total <- nrow(mydata)
    if (n_total < 20) {
        add(if (n_total < 10) "STRONG_WARNING" else "WARNING",
            sprintf("Very small sample size (n = %d). Results may be unreliable.", n_total))
    }

    # Check group sizes
    if (!is.null(mygroup) && mygroup %in% names(mydata)) {
        # Drop levels with no observations left (row filters and the
        # missing-value setting both empty levels out) so the counts reported
        # below describe the data actually being analysed.
        group_sizes <- table(mydata[[mygroup]])
        group_sizes <- group_sizes[group_sizes > 0]
        min_group_size <- if (length(group_sizes) > 0) min(group_sizes) else 0L

        if (length(group_sizes) > 0 && min_group_size < 5) {
            add("STRONG_WARNING", paste0("Small group detected (n = ", min_group_size, "). Consider combining categories or using exact tests."))
        }

        # Check for empty cells in cross-tabulations (categorical variables only)
        for (var in myvars) {
            if (var %in% names(mydata)) {
                # Only check categorical variables for empty cells
                # Continuous variables naturally have many unique values that won't appear in all groups
                if (.crosstableIsCategorical(mydata[[var]])) {
                    cont_table <- table(mydata[[var]], mydata[[mygroup]])
                    if (any(cont_table == 0)) {
                        add("WARNING", paste0("Empty cells detected in ", .display(var), " \u{D7} ", .display(mygroup), " table. Results may be unstable."))
                    }

                    # Cochran's condition: the chi-square approximation needs
                    # expected counts of at least 5. Which test actually runs on
                    # this variable depends on the style: gtsummary switches to
                    # Fisher on its own; arsenal and finalfit run whatever `pcat`
                    # says (finalfit's R warning is invisible in jamovi); the
                    # tangram styles (NEJM/Lancet/Hmisc) always run the uncorrected
                    # Pearson chi-square and never switch (verified: a 2x2 with
                    # Fisher p = 0.065 prints "P=0.03, Pearson" under NEJM).
                    # Rows/columns with no observations are dropped first so that
                    # empty factor levels do not manufacture a zero expected count.
                    nz <- cont_table[rowSums(cont_table) > 0, colSums(cont_table) > 0, drop = FALSE]
                    if (all(dim(nz) >= 2)) {
                        expected <- tryCatch(
                            suppressWarnings(stats::chisq.test(nz)$expected),
                            error = function(e) NULL
                        )
                        fisher_selected <- sty %in% c("arsenal", "finalfit") && identical(pcat, "fisher")
                        if (!is.null(expected) && all(is.finite(expected)) && any(expected < 5) && !fisher_selected) {
                            where <- paste0(
                                "Low expected counts in ", .display(var), " \u{D7} ", .display(mygroup),
                                " table (smallest expected count ", format(round(min(expected), 2), nsmall = 2), ")."
                            )
                            if (identical(sty, "gtsummary")) {
                                add("WARNING", paste0(
                                    where,
                                    " The gtsummary style switches this variable to Fisher's exact test automatically, so its p-value is exact; the sparse cells still make its percentages imprecise."
                                ))
                            } else if (sty %in% c("nejm", "lancet", "hmisc")) {
                                add("STRONG_WARNING", paste0(
                                    where,
                                    " The chi-square approximation is unreliable here, and the NEJM, Lancet and Hmisc styles always apply an uncorrected Pearson chi-square and never switch to an exact test, so this variable's p-value should not be relied on. Choose the gtsummary style, which switches to Fisher's exact test automatically, or arsenal or finalfit with Fisher's exact test selected."
                                ))
                            } else {
                                add("STRONG_WARNING", paste0(
                                    where,
                                    " The chi-square approximation is unreliable here, so this variable's chi-square p-value should not be relied on. Select Fisher's exact test in Options."
                                ))
                            }
                        }
                    }
                }
            }
        }
    }

    # Check for excessive missing data - on the data as selected, BEFORE any
    # missing-value exclusion, otherwise this can never fire with `excl` on.
    for (var in c(myvars, mygroup)) {
        if (var %in% names(full_data)) {
            missing_pct <- mean(is.na(full_data[[var]])) * 100
            if (missing_pct > 20) {
                add("WARNING", paste0("High missing data in ", .display(var), " (", round(missing_pct, 1), "%). Consider imputation or sensitivity analysis."))
            }
        }
    }

    return(list(
        messages = acc$messages,
        # Named counts of the group levels that still hold at least one row.
        # Fewer than two means there is nothing to compare; .run() stops there.
        group_sizes = group_sizes
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

            # Notice collection helpers. Two Preformatted (plain-text) output items
            # (`notices` on top, `notes` below the tables): avoids BOTH the
            # jmvcore::Notice serialization error from self$results$insert(999, Notice)
            # AND any HTML in notices (project convention: notice content must be
            # plain text). ====
            .noticeList = list(),

            .addNotice = function(type, title, content) {
                duplicate <- vapply(private$.noticeList, function(notice) {
                    identical(notice$type, type) &&
                        identical(notice$title, title) &&
                        identical(notice$content, content)
                }, logical(1))
                if (any(duplicate))
                    return()

                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },

            .renderNotices = function() {
                # Two plain-text Preformatted sinks: errors and warnings go to
                # `notices` at the top of the output, INFO goes to `notes` below
                # the tables, so a 100-word methodology note never pushes the
                # table off the first screen. Plain text only (project
                # convention); Preformatted renders it literally.
                render <- function(item, list) {
                    if (length(list) == 0) {
                        item$setContent("")
                        item$setVisible(FALSE)
                        return()
                    }
                    blocks <- vapply(list, function(notice) {
                        prefix <- switch(notice$type,
                            ERROR          = "ERROR: ",
                            STRONG_WARNING = "WARNING: ",
                            WARNING        = "WARNING: ",
                            "")
                        paste0(prefix, notice$title, "\n", notice$content)
                    }, character(1))
                    item$setContent(paste(blocks, collapse = "\n\n"))
                    item$setVisible(TRUE)
                }
                is_info <- vapply(private$.noticeList, function(n) identical(n$type, "INFO"), logical(1))
                render(self$results$notices, private$.noticeList[!is_info])
                render(self$results$notes,   private$.noticeList[is_info])
            },

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
            # HTML-escaped inside .crosstableNoticeHTML().
            .reportTableError = function(e) {
                error_html <- .crosstableNoticeHTML(
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
                    jmvcore::reject(.("Variable name issues detected: {}"),
                                    code = NULL,
                                    paste(validation_results$issues, collapse = "; "))
                }

                # Mapping cleaned_name -> original_name, used to label every
                # user-facing name in the results.
                original_names_mapping <- setNames(original_names, cleaned_names)

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
                            private$.addNotice(
                                "WARNING",
                                .("Some selected variables were left out of the table"),
                                .fmt(
                                    .("These variables could not be matched to a column in the data: {vars} - so no rows are shown for them and they are not included in any test. This usually happens when a column was renamed, removed or re-typed after it was selected. Re-select them in the Variables box, or drop them from the selection to clear this message."),
                                    vars = paste(missing_vars, collapse = ", ")
                                )
                            )
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
                            mygroup <- character(0)
                        }
                    } else {
                        mygroup <- character(0)
                    }

                }, error = function(e) {
                    jmvcore::reject(
                        .fmt(
                            .("The selected variables could not be matched to the columns in the data, so no cross table could be built. Re-select the variables and grouping variable, or check that the data still contains those columns. Technical detail: {err}"),
                            err = conditionMessage(e)
                        ),
                        code = NULL
                    )
                })

                # If any user-specified variable could not be matched, block analysis.
                # Reported here rather than inside the tryCatch() above, whose error handler
                # would otherwise swallow the message and replace it with a generic one.
                if (length(mygroup) == 0) {
                    jmvcore::reject(
                        .fmt(
                            .("The grouping variable '{group}' could not be matched to a column in the data, so there are no groups to compare across and no table can be built. This usually happens when the column was renamed, removed or re-typed after it was selected. Re-select the grouping variable in the Group box."),
                            group = self$options$group
                        ),
                        code = NULL
                    )
                }
                if (length(myvars) == 0) {
                    jmvcore::reject(
                        .("None of the selected variables could be matched to a column in the data, so the table would have no rows. This usually happens when the columns were renamed or removed after they were selected. Re-select the variables in the Variables box.")
                    )
                }

                return(list(
                    "mydata" = mydata,
                    "myvars" = myvars,
                    "mygroup" = mygroup,
                    "original_names_mapping" = original_names_mapping
                ))
            },


            # .showTestInformation ----
            .showTestInformation = function(method_type = "FDR") {
                # Generate method-specific test information
                if (method_type == "FWER") {
                    # Family-Wise Error Rate control (Bonferroni, Holm)
                    test_info <- paste0(
                        "<div style='background-color: rgba(33, 149, 236, 0.1); padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3; color: inherit;'>",
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
                        "<div style='background-color: rgba(33, 149, 236, 0.1); padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3; color: inherit;'>",
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

            # .init ----
            .init = function() {
                # The SMD table carries one row per selected variable, which is
                # fully determined by the options. Build that structure here so the
                # table does not appear empty and then restructure on every run;
                # .populateSMD() fills the computed cells with setRow().
                # Row keys are the ORIGINAL variable names, which is what
                # .crosstableDisplayName() resolves the cleaned names back to.
                if (isTRUE(self$options$showSMD)) {
                    tab <- self$results$smdTable
                    for (v in self$options$vars) {
                        key <- unname(as.character(v))
                        tab$addRow(rowKey = key, values = list(variable = key))
                    }
                }
            },

            # .run ----
            .run = function() {
                # Reset notices so the same message is not appended once per run cycle
                private$.noticeList <- list()
                private$.renderNotices()

                sty <- self$options$sty
                # If required options are missing, show a welcome message with instructions.
                if (is.null(self$options$vars) || is.null(self$options$group)) {
                    # Initial state - no error, just show welcome message
                    self$results$errorNotice$setVisible(FALSE)

                    todo <- paste0(
                        "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; margin: 15px 0; border-radius: 8px; border-left: 5px solid #007bff; color: inherit;'>",
                        "<h3 style='margin-top: 0; color: #007bff;'>Welcome to Cross Table Analysis</h3>",

                        "<p><strong>Purpose:</strong> Compare distributions of clinical variables across groups with automatic test selection.</p>",

                        "<h4 style='margin-top: 15px;'>Quick Start:</h4>",
                        "<ol style='margin-left: 20px;'>",
                        "<li>Select <strong>dependent variables</strong> (rows) - continuous or categorical measures</li>",
                        "<li>Select <strong>grouping variable</strong> (columns) - treatment groups, disease stages, etc.</li>",
                        "<li>Choose <strong>table style</strong> from Options (gtsummary by default; NEJM, Lancet, finalfit, arsenal and Hmisc are also available)</li>",
                        "</ol>",

                        "<h4 style='margin-top: 15px;'>Test Selection:</h4>",
                        "<ul style='margin-left: 20px;'>",
                        "<li><strong>Categorical variables:</strong> Chi-square or Fisher's exact test, as chosen in Options (the gtsummary style switches to Fisher automatically when an expected cell count is below 5)</li>",
                        "<li><strong>Continuous variables:</strong> ANOVA or Kruskal-Wallis, depending on the style and on whether means or medians are displayed; the gtsummary style always uses a rank-based test</li>",
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
                    .("No group selected")
                }
                # `subtitle` is a Preformatted item, which renders its content
                # literally - it is not an HTML sink, so escaping here would print
                # "Ki-67 &gt;20%" instead of the column name the user chose.
                self$results$subtitle$setContent(.fmt(.("Cross Table Analysis - Grouped by {group}"), group = group_display))

                # Provide additional information when using 'finalfit' style.
                if (sty == "finalfit") {
                    # p_cont_para is hard-coded to "aov" in the summary_factorlist()
                    # call below, so Welch's t-test is never run. Verified against
                    # finalfit 1.1.0 on two groups of n = 30 with SD 1 vs SD 4:
                    # finalfit p = 0.581 = aov = pooled t-test; Welch = 0.583.
                    todo2 <- .("<br>
                         <b>finalfit</b> style compares continuous variables with <em>aov</em> (one-way analysis of variance) when Mean (SD) is displayed. For two groups this is the pooled-variance t-test, so it assumes equal variances in the groups; that assumption is not checked here.
                         When Median (IQR) is displayed, Kruskal-Wallis is used instead (equivalent to the Mann-Whitney U / Wilcoxon rank sum test in two-group settings).
                         Categorical variables use the chi-square or Fisher's exact test selected in Options, and numeric variables with fewer than 5 distinct values, counting missing as one of them, are summarised as categories rather than as measurements.
                         See full documentation <a href='https://finalfit.org/reference/summary_factorlist.html'>here</a>.
                         ")
                } else {
                    todo2 <- ""
                }
                self$results$todo2$setContent(todo2)

                # Check if data has complete rows.
                if (nrow(self$data) == 0) {
                    # Use HTML error notice instead of dynamic Notice to avoid serialization errors
                    error_html <- .crosstableNoticeHTML(
                        .('Dataset contains no complete rows. Please check your data and filters.'),
                        type = "ERROR"
                    )
                    self$results$errorNotice$setContent(error_html)
                    self$results$errorNotice$setVisible(TRUE)
                    return()
                }

                # Performance safeguards for large datasets
                n_rows <- nrow(self$data)
                n_vars <- length(self$options$vars)
                # na.omit(): unique() keeps NA as a value, which would inflate the
                # level count reported back to the user in the notice below and trip
                # the combination threshold one selection early.
                n_group_levels <- length(unique(stats::na.omit(self$data[[self$options$group]])))
                n_combinations <- n_vars * n_group_levels

                if (n_rows > 50000) {
                    private$.addNotice(
                        "INFO",
                        .("Large dataset - the table may take a while to appear"),
                        sprintf(
                            .("The data has %s rows, well above the 50,000-row point where building this table starts to feel slow. Nothing is wrong with the results - every row is used - but the table can take a while to appear and is rebuilt each time you change an option. If you are still exploring, apply a row filter while you settle on the options and remove it for the final table."),
                            base::format(n_rows, big.mark = ",", scientific = FALSE, trim = TRUE)
                        )
                    )
                }

                if (n_combinations > 100) {
                    private$.addNotice(
                        "INFO",
                        .("Many variable-by-group combinations"),
                        sprintf(
                            .("This selection produces %d variable-by-group combinations (%d variables x %d group levels). Each variable adds rows and one more significance test, so the table becomes long to read and the multiple-testing correction gets more conservative - genuine differences are harder to detect. Consider splitting the variables over two or three separate Cross Table analyses, or grouping rare levels of the grouping variable together."),
                            n_combinations, n_vars, n_group_levels
                        )
                    )
                }

                # Read and label data with robust variable name handling.
                cleaneddata <- private$.labelData()
                mydata <- cleaneddata$mydata
                myvars <- cleaneddata$myvars
                mygroup <- cleaneddata$mygroup
                original_names_mapping <- cleaneddata$original_names_mapping

                # Build formula using escaped variable names for safety.
                escaped_myvars <- .crosstableEscapeVariableNames(myvars)
                escaped_mygroup <- .crosstableEscapeVariableNames(mygroup)
                formula <- jmvcore::constructFormula(terms = escaped_myvars, dep = escaped_mygroup)
                formula <- jmvcore::asFormula(formula)

                # Exclude missing data if requested, and say how much went. A
                # Table One that silently drops a quarter of the cohort is not
                # something a clinician can report; the pre-exclusion data is kept
                # so the missing-percentage check below still sees what was there.
                data_before_exclusion <- mydata
                n_dropped <- 0L
                if (self$options$excl) {
                    mydata <- jmvcore::naOmit(mydata)
                    n_dropped <- nrow(data_before_exclusion) - nrow(mydata)
                    if (n_dropped > 0) {
                        pct_dropped <- 100 * n_dropped / nrow(data_before_exclusion)
                        private$.addNotice(
                            if (pct_dropped > 20) "WARNING" else "INFO",
                            .("Rows excluded for missing values"),
                            sprintf(
                                .("Missing-value exclusion removed %d of %d rows (%s%%) that had a missing value in at least one of the selected variables. Every statistic below is computed on the remaining %d rows."),
                                n_dropped, nrow(data_before_exclusion),
                                format(round(pct_dropped, 1), nsmall = 1), nrow(mydata)
                            )
                        )
                    }
                }
                
                # Empty levels of the grouping variable would appear as an all-zero
                # column in gtsummary, finalfit and the tangram layouts, and a
                # chi-square on such a table is NaN. Drop them here. Row variables
                # keep their empty levels: a "Grade 4: 0 (0%)" row is legitimate
                # Table One content.
                if (is.factor(mydata[[mygroup]])) {
                    # droplevels() returns a fresh factor and silently discards
                    # the variable label set in .labelData(); without it finalfit
                    # prints the cleaned column name as its dependent label.
                    group_label_attr <- attr(mydata[[mygroup]], "label", exact = TRUE)
                    mydata[[mygroup]] <- droplevels(mydata[[mygroup]])
                    attr(mydata[[mygroup]], "label") <- group_label_attr
                }

                # Variables no engine can tabulate. An all-missing column makes
                # arsenal and finalfit fail with an internal contrasts/dplyr error;
                # a single-valued column makes finalfit fail the same way while the
                # other engines show it without a p-value. Say so, and leave them
                # out of the engines that cannot take them, instead of surfacing
                # the engine's error as the whole table.
                n_distinct <- vapply(myvars, function(v) length(unique(stats::na.omit(mydata[[v]]))), integer(1))
                all_missing <- myvars[n_distinct == 0]
                constant <- myvars[n_distinct == 1]
                dropped_vars <- all_missing
                if (identical(sty, "finalfit")) dropped_vars <- c(dropped_vars, constant)
                if (length(all_missing) > 0) {
                    private$.addNotice(
                        "WARNING",
                        .("Variables with no values were left out"),
                        sprintf(
                            .("These variables have no non-missing values among the rows being analysed, so nothing can be tabulated for them and they were left out of the table: %s."),
                            paste(vapply(all_missing, function(v) .crosstableDisplayName(v, original_names_mapping), character(1)), collapse = ", ")
                        )
                    )
                }
                if (length(constant) > 0) {
                    constant_labels <- paste(vapply(constant, function(v) .crosstableDisplayName(v, original_names_mapping), character(1)), collapse = ", ")
                    private$.addNotice(
                        "WARNING",
                        .("Single-valued variables cannot be compared"),
                        if (identical(sty, "finalfit"))
                            sprintf(.("These variables take a single value in the rows being analysed, so no comparison between the groups is possible for them: %s. The finalfit style cannot build a table that contains them, so they were left out; choose another style to see their row."), constant_labels)
                        else
                            sprintf(.("These variables take a single value in the rows being analysed, so no comparison between the groups is possible for them and their p-value is blank: %s."), constant_labels)
                    )
                }
                if (length(dropped_vars) > 0) {
                    myvars <- setdiff(myvars, dropped_vars)
                    if (length(myvars) == 0) {
                        private$.addNotice(
                            "ERROR",
                            .("Nothing left to tabulate"),
                            .("Every selected variable was left out for the reasons above, so there is no table to build. Select at least one variable that has values in the rows being analysed.")
                        )
                        return()
                    }
                    formula <- jmvcore::asFormula(jmvcore::constructFormula(
                        terms = .crosstableEscapeVariableNames(myvars),
                        dep = .crosstableEscapeVariableNames(mygroup)))
                }

                # Validate analysis assumptions and data quality. The validator
                # returns typed messages; severity is carried as data, not parsed
                # back out of the wording.
                validation_results <- .validateAnalysisAssumptions(
                    mydata,
                    myvars,
                    mygroup,
                    name_mapping = original_names_mapping,
                    full_data = data_before_exclusion,
                    sty = sty,
                    pcat = self$options$pcat
                )
                validation_messages <- validation_results$messages
                data_quality_html <- ""
                if (length(validation_messages) > 0) {
                    # Accumulate all warnings into HTML (avoid serialization errors from dynamic Notice inserts)
                    warning_html_parts <- vapply(validation_messages, function(m) {
                        .crosstableNoticeHTML(m$text, type = m$type)
                    }, character(1))

                    # Combine all warnings into single HTML output
                    data_quality_html <- paste(warning_html_parts, collapse = "\n")
                    self$results$dataQualityNotice$setContent(data_quality_html)
                    self$results$dataQualityNotice$setVisible(TRUE)
                } else {
                    self$results$dataQualityNotice$setContent("")
                    self$results$dataQualityNotice$setVisible(FALSE)
                }

                # A grouping variable with fewer than two non-empty levels produces
                # a table with no p-values at all; arsenal and tangram both return
                # successfully after only a bare R warning, so without this the user
                # sees a complete-looking table containing no comparison.
                group_sizes <- validation_results$group_sizes
                if (length(group_sizes) < 2) {
                    group_label <- .crosstableDisplayName(mygroup, original_names_mapping)
                    present <- if (length(group_sizes) == 1)
                        sprintf(
                            .("Only the level '%s' is left, holding all %d rows."),
                            names(group_sizes)[1], as.integer(group_sizes)[1]
                        )
                    else
                        .("No level of it has any rows left.")
                    private$.addNotice(
                        "ERROR",
                        .("Only one group - nothing to compare"),
                        sprintf(
                            .("The grouping variable '%s' has fewer than two groups among the rows being analysed, so there is nothing to compare across and no significance test can be computed. %s Rows dropped by a row filter or by the missing-value setting are not counted here. Widen or remove the row filter, switch off missing-value exclusion, or choose a grouping variable that has at least two levels present in the data."),
                            group_label, present
                        )
                    )
                    return()
                }

                # One typing decision, reported once, before the style branch: which
                # numeric columns are being summarised as categories rather than as
                # measurements. The styles do not agree on this, so say it out loud.
                numeric_as_cat <- myvars[vapply(
                    mydata[myvars],
                    function(v) is.numeric(v) && .crosstableIsCategorical(v),
                    logical(1)
                )]
                if (length(numeric_as_cat) > 0) {
                    numeric_as_cat_labels <- vapply(
                        numeric_as_cat,
                        function(v) .crosstableDisplayName(v, original_names_mapping),
                        character(1)
                    )
                    private$.addNotice(
                        "INFO",
                        .("Numeric variables that look like coded categories"),
                        sprintf(
                            .("These variables are stored as numbers but take 6 or fewer distinct values, so they are most likely encoded categories such as grade 1, 2, 3 rather than measurements: %s. The table styles do not agree on how to handle them: gtsummary summarises them as counts and percentages and tests them with a chi-square or Fisher's exact test, finalfit does the same only when the column has fewer than 5 distinct values counting missing as one of them, so a four-value score with any missing data is treated as continuous there, and arsenal, NEJM, Lancet and Hmisc treat any numeric column as continuous and test the codes with ANOVA or Kruskal-Wallis. The p-value for these variables can therefore change when you switch style, with nothing else changed. To get the same handling in every style, set their measure type to Nominal or Ordinal in the data; leave them Continuous if they really are measurements."),
                            paste(numeric_as_cat_labels, collapse = ", ")
                        )
                    )
                }

                # Mean (SD) misdescribes a long-tailed variable (Ki-67, CA-125,
                # hospital stay). Sample skewness above 1 in absolute value is the
                # usual "highly skewed" cut; the note lists the offenders once.
                if (identical(self$options$cont, "mean")) {
                    skewed <- myvars[vapply(myvars, function(v) {
                        x <- mydata[[v]]
                        if (!is.numeric(x) || .crosstableIsCategorical(x)) return(FALSE)
                        x <- x[is.finite(x)]
                        if (length(x) < 10) return(FALSE)
                        s <- stats::sd(x)
                        is.finite(s) && s > 0 && abs(mean((x - mean(x))^3) / s^3) > 1
                    }, logical(1))]
                    if (length(skewed) > 0) {
                        skewed_labels <- vapply(skewed, function(v) .crosstableDisplayName(v, original_names_mapping), character(1))
                        private$.addNotice(
                            "INFO",
                            .("Skewed continuous variables shown as Mean (SD)"),
                            sprintf(
                                .("These continuous variables look markedly skewed, with a long tail on one side (skewness coefficient above 1): %s. Mean (SD) can misrepresent such distributions; consider switching the statistic to Median (Q1, Q3) in Options."),
                                paste(skewed_labels, collapse = ", ")
                            )
                        )
                    }
                }

                # Yates' continuity correction is not applied consistently across
                # the three styles that honour `pcat`: finalfit's chi-square applies
                # it on 2x2 tables, arsenal's and gtsummary's (chisq.test.no.correct)
                # do not. Verified on neg 30/12 vs pos 30/30 (n = 102, smallest
                # expected count 17.3, so no style auto-switches): arsenal 0.030,
                # gtsummary 0.030, finalfit 0.050 - opposite sides of 0.05 with only
                # the style changed. R subtracts min(0.5, |O - E|) from |O - E|, so
                # the corrected p-value is never the smaller of the two.
                if (identical(self$options$pcat, "chisq") &&
                    sty %in% c("arsenal", "finalfit", "gtsummary")) {
                    two_by_two <- myvars[vapply(myvars, function(v) {
                        if (!.crosstableIsCategorical(mydata[[v]])) return(FALSE)
                        tab <- table(mydata[[v]], mydata[[mygroup]])
                        tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
                        all(dim(tab) == 2)
                    }, logical(1))]
                    if (length(two_by_two) > 0) {
                        two_by_two_labels <- vapply(
                            two_by_two,
                            function(v) .crosstableDisplayName(v, original_names_mapping),
                            character(1)
                        )
                        private$.addNotice(
                            "INFO",
                            .("Chi-square on 2x2 tables is not the same test in every style"),
                            sprintf(
                                .("These variables form a two-by-two table with the grouping variable: %s. With Chi-square selected, the finalfit style applies Yates' continuity correction to them while the arsenal and gtsummary styles report the uncorrected Pearson chi-square. The corrected p-value is never the smaller of the two, so a two-by-two p-value can move to the other side of 0.05 when nothing but the style is changed. Fisher's exact test does not involve this choice."),
                                paste(two_by_two_labels, collapse = ", ")
                            )
                        )
                    }
                }

                # Generate table based on selected style. Each engine lives in its
                # own method and returns the rendered HTML plus the per-variable
                # p-values it displayed, or NULL after reporting its error.
                built <- switch(sty,
                    arsenal   = private$.tableArsenal(mydata, formula, original_names_mapping),
                    finalfit  = private$.tableFinalfit(mydata, myvars, mygroup),
                    gtsummary = private$.tableGtsummary(mydata, myvars, mygroup, original_names_mapping),
                    private$.tableTangram(mydata, formula, myvars, mygroup, original_names_mapping)
                )
                if (is.null(built)) return()
                table_item <- switch(sty, arsenal = "tablestyle1", finalfit = "tablestyle2",
                                     gtsummary = "tablestyle3", "tablestyle4")
                self$results$get(table_item)$setContent(built$html)


                # Completion note (INFO, rendered below the tables with the other notes)
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

                # Not every style honours every statistical option, and until now
                # nothing said so: the tangram styles (NEJM/Lancet/Hmisc; NEJM was
                # the default until 1.0.9) apply none of them, and p-value adjustment
                # only exists in gtsummary. A user could set "Fisher's exact test"
                # and "Benjamini-Hochberg", see a table, and reasonably believe both
                # had been applied. Verified per style by comparing rendered output.
                sty_now <- self$options$sty
                honours <- list(
                    arsenal   = c("pcat", "cont"),
                    finalfit  = c("pcat", "cont"),
                    gtsummary = c("pcat", "cont", "p_adjust"),
                    nejm      = character(0),
                    lancet    = character(0),
                    hmisc     = character(0)
                )
                supported <- honours[[sty_now]]
                if (is.null(supported)) supported <- character(0)

                requested <- character(0)
                if (!identical(self$options$pcat, "chisq"))
                    requested <- c(requested, pcat = "Test for categorical variables")
                if (!identical(self$options$cont, "mean"))
                    requested <- c(requested, cont = "Statistic for continuous variables")
                if (!identical(self$options$p_adjust, "none"))
                    requested <- c(requested, p_adjust = "P-value adjustment")

                ignored <- requested[!(names(requested) %in% supported)]
                if (length(ignored) > 0) {
                    ignored_html <- .crosstableNoticeHTML(
                        sprintf(
                            "The %s style does not apply the following setting(s), so the table below is unchanged by them: %s. %s",
                            style_display,
                            paste(unname(ignored), collapse = "; "),
                            # gtsummary honours every option in `requested`, so
                            # `ignored` is always empty for it and only the
                            # tangram styles and arsenal/finalfit can reach here.
                            if (sty_now %in% c("nejm", "lancet", "hmisc"))
                                "These styles use their own built-in tests. Choose arsenal, finalfit or gtsummary to control the test; p-value adjustment is available in gtsummary only."
                            else
                                "P-value adjustment is available in the gtsummary style only."),
                        type = "WARNING")
                    # Compose onto the data-quality warnings already written to this
                    # same output element earlier in .run() (lines ~602 and ~799)
                    # rather than replacing them. setContent() overwrites, and the
                    # earlier block escalates to STRONG_WARNING for cells with n < 10 -
                    # exactly the small-sample warning a pathologist most needs - so
                    # replacing it silently dropped that warning whenever the user also
                    # picked a style that ignores one of their statistical settings.
                    data_quality_html <- paste(
                        Filter(nzchar, c(data_quality_html, ignored_html)),
                        collapse = "\n"
                    )
                    self$results$dataQualityNotice$setContent(data_quality_html)
                    self$results$dataQualityNotice$setVisible(TRUE)
                }

                private$.addNotice(
                    "INFO",
                    .("Analysis completed"),
                    sprintf(
                        .("%d variable(s) compared across the levels of %s using the %s style."),
                        n_vars, group_display, style_display
                    )
                )

                # Standardized mean differences (balance diagnostic)
                if (isTRUE(self$options$showSMD))
                    private$.populateSMD(
                        data = mydata,
                        vars = myvars,
                        group = mygroup,
                        name_mapping = original_names_mapping
                    )

                # Copy-ready plain-language summary
                if (isTRUE(self$options$showSummary)) {
                    self$results$summary$setContent(private$.summarySentence(
                        mydata, myvars, mygroup, original_names_mapping,
                        group_sizes = validation_results$group_sizes,
                        n_dropped = n_dropped,
                        style_display = style_display,
                        pvalues = built$pvalues
                    ))
                }

            },

            # ----------------------------------------------------------------
            # Table engines. Each returns list(html, pvalues), or NULL after the
            # engine's error has been reported through .reportTableError().
            # pvalues is data.frame(display, p, p_text, q): the per-variable
            # p-values the rendered table shows, consumed by .summarySentence().
            # ----------------------------------------------------------------
            .tableArsenal = function(mydata, formula, name_mapping) {
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
                if (is.null(tablearsenal)) return(NULL)
                # Per-variable p-values as arsenal computed them, read before
                # the object is rendered away below.
                pvalues <- private$.pvaluesFromArsenal(tablearsenal, name_mapping)
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
                list(html = tablearsenal, pvalues = pvalues)
            },

            .tableFinalfit = function(mydata, myvars, mygroup) {
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
                        # cont_cut = 5 is finalfit's default and is KEPT DELIBERATELY here.
                        # Do not "sweep" it to 0.
                        #
                        # finalfit runs, on its own copy of the data,
                        #   cont_distinct = select(explanatory) %>% summarise_if(is.numeric, n_distinct) %>% keep(~ .x < cont_cut)
                        #   .data = mutate_at(.data, cont_distinct, as.factor)
                        # so a numeric explanatory with fewer than 5 distinct values (n_distinct
                        # counts NA) is summarised as a category and its p-value comes from
                        # p_cat (chisq/fisher) instead of p_cont_para (aov). Verified against
                        # finalfit 1.1.0: explanatory_type is read AFTER that mutate, so it is
                        # what picks the test branch.
                        #
                        # In multisurvival/survivalcont/oddsratio the same rewrite is a BUG and is
                        # disabled with cont_cut = 0, because those analyses also fit the same
                        # column with coxph/glm/lrm, which do not apply the rule -- one selection,
                        # two different models. This analysis fits NO model. It is a purely
                        # descriptive Table One, so there is nothing to disagree with, and the
                        # conversion is what a clinician wants: on the bundled histopathology data
                        # the columns it touches are Grade, Anti-X/Anti-Y intensity (1/2/3) and the
                        # 0/1 markers -- categorical variables stored as numbers. At cont_cut = 5
                        # Grade prints 25 (31.2) / 27 (33.8) / 28 (35.0), chisq p = 0.529; at
                        # cont_cut = 0 it collapses to a meaningless "Mean (SD) 2.1 (0.8)", aov
                        # p = 0.409. Genuine factors and numerics with >= 5 distinct values (Age,
                        # OverallTime, TStage) are byte-identical either way.
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
                if (is.null(tablefinalfit)) return(NULL)
                pvalues <- private$.pvaluesFromFinalfit(tablefinalfit)
                tablefinalfit <- kableExtra::kable(
                    tablefinalfit,
                    format = "html",
                    digits = 1,
                    escape = TRUE
                )
                list(html = tablefinalfit, pvalues = pvalues)
            },

            .tableGtsummary = function(mydata, myvars, mygroup, name_mapping) {
                private$.checkpoint()



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
                all_cat_vars <- names(mydata_subset)[vapply(mydata_subset, .crosstableIsCategorical, logical(1))]
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

                # gtsummary's default test selection: categorical uses chi-square
                # and switches to Fisher automatically when an expected count drops
                # below 5. Continuous is RANK-BASED - wilcox.test for two groups,
                # kruskal.test for three or more (verified against gtsummary 2.5.1;
                # an earlier comment here claimed Welch t-test / ANOVA, which is
                # wrong). That holds whether the table displays means or medians, so
                # the notice below states it rather than silently swapping in a test
                # the user never asked for.
                #
                # But an EXPLICIT request for Fisher was previously ignored here, so
                # a user who chose "Fisher's exact test" silently got chi-square.
                # On a sparse 2x2 that is not cosmetic: 0.028 (chi-square) against
                # 0.065 (Fisher) lands on opposite sides of 0.05.
                #
                # "chisq" is left as gtsummary's default rather than forced, because
                # the automatic switch to Fisher on sparse tables is a safety feature
                # and the user asking for chi-square is asking for the usual
                # behaviour, not for the safeguard to be removed. The analysis note
                # states which of the two applies.
                gts_test <- if (identical(self$options$pcat, "fisher")) {
                    list(gtsummary::all_categorical() ~ "fisher.test")
                } else {
                    NULL
                }

                # Displayed statistic and test are coupled in arsenal and finalfit
                # but not here: asking for means does not switch gtsummary off its
                # rank-based default.
                if (identical(self$options$cont, "mean") && length(cont_vars) > 0) {
                    private$.addNotice(
                        "INFO",
                        .("Continuous p-values are rank-based in the gtsummary style"),
                        .("Continuous variables are displayed as Mean (SD), but the gtsummary style tests them with a rank-based test - Wilcoxon rank-sum for two groups, Kruskal-Wallis for three or more - so the p-value compares the distributions rather than the means. The two do not have to agree. Choose the arsenal or finalfit style if you need the test to match the statistic on display: both run ANOVA (for two groups, the pooled-variance t-test, which assumes equal variances) when means are shown and Kruskal-Wallis when medians are shown.")
                    )
                }

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
                  add_p(test = gts_test,
                        pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)),
                  error = function(e) { private$.reportTableError(e); NULL })
                if (is.null(tablegtsummary)) return(NULL)

                # Add adjusted p-values/q-values only if adjustment method is not "none"
                if (p_adjust_method != "none") {
                    # Adjusting with only one variable is a no-op; say so below
                    # the table rather than in the warnings panel above it.
                    if (length(myvars) == 1) {
                        private$.addNotice(
                            "INFO",
                            .("P-value adjustment with one variable has no effect"),
                            .("The adjusted p-value equals the original p-value: a multiple-testing correction only changes anything when several variables are tested at once.")
                        )
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

                pvalues <- private$.pvaluesFromGtsummary(tablegtsummary, name_mapping)
                tablegtsummary <-
                    gtsummary::as_kable_extra(tablegtsummary)

                table_html <- tablegtsummary

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

                    # Benjamini-Yekutieli multiplies by the harmonic sum of the
                    # number of tests to stay valid under arbitrary dependence, so
                    # with few variables its q-values can EXCEED Bonferroni and Holm
                    # (p = .001, .02, .03, .20, .40 gives BY .011, .114, .114, .571,
                    # .913 against Bonferroni .005, .100, .150, 1, 1). The blanket
                    # "smaller than FWER-adjusted" claim only holds for BH.
                    q_vs_fwer_bullet <- if (identical(p_adjust_method, "BY")) {
                        "<li><strong>Q-values are larger than raw p-values.</strong> Benjamini-Yekutieli corrects for arbitrary dependence between the tests, and with a small number of variables its q-values can be larger than Bonferroni- or Holm-adjusted p-values</li>"
                    } else {
                        "<li><strong>Q-values are larger than raw p-values</strong> and generally smaller than Bonferroni- or Holm-adjusted p-values</li>"
                    }

                    # Generate method-specific explanation
                    if (is_fdr) {
                        # FDR methods - use "q-values"
                        qvalue_explanation <- paste0(
                            "<div style='background-color: rgba(33, 152, 255, 0.07); padding: 15px; margin-top: 20px; border-radius: 5px; border: 1px solid #4682b4; color: inherit;'>",
                            "<h4 style='margin-top: 0;'>Multiple Testing Correction: ", method_names[[p_adjust_method]], "</h4>",
                            "<p><strong>Method:</strong> ", method_descriptions[[p_adjust_method]], "</p>",

                            "<p><strong>What are Q-values?</strong><br>",
                            "Q-values represent the False Discovery Rate (FDR) - the expected <strong>proportion</strong> of false positives among your discoveries. ",
                            "Unlike FWER methods that control the probability of ANY false positive, FDR methods allow a controlled proportion of false discoveries.</p>",

                            "<p><strong>How to interpret:</strong></p>",
                            "<ul>",
                            "<li><strong>Q-value = 0.05:</strong> Among all variables with q \u{2264} 0.05, expect ~5% to be false positives</li>",
                            "<li><strong>Q-value = 0.10:</strong> Expect ~10% false positives (acceptable in exploratory research)</li>",
                            q_vs_fwer_bullet,
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
                            "<div style='background-color: rgba(33, 152, 255, 0.07); padding: 15px; margin-top: 20px; border-radius: 5px; border: 1px solid #4682b4; color: inherit;'>",
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

                list(html = table_html, pvalues = pvalues)
            },

            .tableTangram = function(mydata, formula, myvars, mygroup, name_mapping) {
                private$.checkpoint()
                sty_term <- jmvcore::composeTerm(components = self$options$sty)
                # No .htmlSafeTableData() here: tangram::html5() escapes the
                # labels and factor levels it emits itself (verified - a raw
                # level "<img src=x onerror=alert(1)>" and a raw label
                # "<script>alert(2)</script>" both come out fully escaped), so
                # pre-escaping produced "Ki-67 &amp;gt;20%", which the user reads
                # as "Ki-67 &gt;20%". The caption below IS escaped by hand
                # because tangram does not escape that argument. The arsenal
                # branch keeps its pre-escaping - arsenal emits raw HTML.
                tabletangram <- tryCatch(tangram::html5(
                    tangram::tangram(
                        paste(deparse(formula), collapse = " "),
                        mydata,
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
                            .crosstableDisplayName(mygroup, name_mapping)
                        )
                    ),
                    id = "tbl3"
                ), error = function(e) { private$.reportTableError(e); NULL })
                if (is.null(tabletangram)) return(NULL)
                list(html = tabletangram,
                     pvalues = private$.pvaluesRecomputed(mydata, myvars, mygroup, name_mapping))
            },

            # ----------------------------------------------------------------
            # Per-variable p-values, read back from each engine's own object so
            # the summary sentence can never disagree with the table.
            # ----------------------------------------------------------------
            .pvaluesFrame = function(display, p, p_text = NULL, q = NA_real_) {
                if (is.null(p_text)) p_text <- private$.formatP(p)
                data.frame(display = unname(display), p = as.numeric(p), p_text = p_text,
                           q = as.numeric(q), stringsAsFactors = FALSE)
            },

            # "= 0.028", "< 0.001" or "n/a"; the caller supplies the "p"/"q".
            .formatP = function(p) {
                ifelse(is.na(p), "n/a",
                       ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3))))
            },

            .pvaluesFromArsenal = function(tab, name_mapping) {
                df <- tryCatch(unique(as.data.frame(tab)[, c("variable", "p.value")]), error = function(e) NULL)
                if (is.null(df) || nrow(df) == 0) return(NULL)
                private$.pvaluesFrame(
                    display = vapply(as.character(df$variable), function(v) .crosstableDisplayName(v, name_mapping), character(1)),
                    p = df$p.value)
            },

            .pvaluesFromFinalfit = function(df) {
                # summary_factorlist() returns the displayed frame: the label is in
                # the first column on each variable's first row, and `p` is the
                # printed text ("0.068" or "<0.001"), so that text is kept as-is.
                if (!is.data.frame(df) || ncol(df) < 2 || !("p" %in% names(df))) return(NULL)
                p_txt <- trimws(as.character(df$p))
                keep <- nzchar(p_txt) & !is.na(p_txt)
                if (!any(keep)) return(NULL)
                p_txt <- p_txt[keep]
                private$.pvaluesFrame(
                    display = as.character(df[[1]][keep]),
                    p = suppressWarnings(as.numeric(sub("^<", "", p_txt))),
                    p_text = ifelse(startsWith(p_txt, "<"), paste0("< ", sub("^<", "", p_txt)), paste0("= ", p_txt)))
            },

            .pvaluesFromGtsummary = function(tbl, name_mapping) {
                tb <- tryCatch(tbl$table_body, error = function(e) NULL)
                if (is.null(tb) || !all(c("variable", "row_type", "p.value") %in% names(tb))) return(NULL)
                lab <- tb[tb$row_type == "label", , drop = FALSE]
                if (nrow(lab) == 0) return(NULL)
                private$.pvaluesFrame(
                    display = vapply(as.character(lab$variable), function(v) .crosstableDisplayName(v, name_mapping), character(1)),
                    p = lab$p.value,
                    q = if ("q.value" %in% names(lab)) lab$q.value else NA_real_)
            },

            .pvaluesRecomputed = function(mydata, myvars, mygroup, name_mapping) {
                # tangram does not expose its test results, so they are recomputed
                # with the tests its table actually prints: Pearson's chi-square
                # without continuity correction for factors, and for numeric
                # columns the Hmisc-style F approximation to the Wilcoxon /
                # Kruskal-Wallis test, i.e. an ANOVA on the ranks (verified: the
                # NEJM table prints "F 1,118 = 4.68, P = 0.03" and this reproduces
                # F = 4.6814, P = 0.0325; kruskal.test() would give 0.0331). The
                # test suite checks both against the rendered table.
                g <- mydata[[mygroup]]
                p <- vapply(myvars, function(v) {
                    x <- mydata[[v]]
                    ok <- !is.na(x) & !is.na(g)
                    tryCatch({
                        if (is.numeric(x)) {
                            gg <- droplevels(factor(g[ok]))
                            stats::anova(stats::lm(rank(x[ok]) ~ gg))[["Pr(>F)"]][1]
                        } else
                            suppressWarnings(stats::chisq.test(
                                table(droplevels(factor(x[ok])), droplevels(factor(g[ok]))),
                                correct = FALSE))$p.value
                    }, error = function(e) NA_real_)
                }, numeric(1))
                private$.pvaluesFrame(
                    display = vapply(myvars, function(v) .crosstableDisplayName(v, name_mapping), character(1)),
                    p = p)
            },

            # ----------------------------------------------------------------
            # Copy-ready plain-language summary (shown when showSummary is on)
            # ----------------------------------------------------------------
            .summarySentence = function(mydata, myvars, mygroup, name_mapping, group_sizes,
                                        n_dropped, style_display, pvalues) {
                # Assembled as plain text and HTML-escaped ONCE at the end: that
                # covers the user's variable names and the "<" in "p < 0.05"
                # alike, with no double escaping.
                sty <- self$options$sty
                cont <- self$options$cont
                pcat <- self$options$pcat
                p_adjust <- self$options$p_adjust
                n_groups <- length(group_sizes)

                var_labels <- vapply(myvars, function(v) .crosstableDisplayName(v, name_mapping), character(1))
                group_label <- .crosstableDisplayName(mygroup, name_mapping)
                group_sizes_txt <- paste(
                    sprintf("%s n = %d", names(group_sizes), as.integer(group_sizes)),
                    collapse = "; ")

                # The tests each style actually runs (see the notices for why they
                # differ); wording is kept in step with those notices.
                cat_test <- switch(sty,
                    arsenal   = if (identical(pcat, "fisher")) .("Fisher's exact test") else .("Pearson's chi-square test without continuity correction"),
                    finalfit  = if (identical(pcat, "fisher")) .("Fisher's exact test") else .("the chi-square test with Yates' continuity correction on 2x2 tables"),
                    gtsummary = if (identical(pcat, "fisher")) .("Fisher's exact test") else .("the chi-square test, switching to Fisher's exact test where an expected count was below 5"),
                    .("Pearson's chi-square test"))
                cont_test <- if (sty %in% c("arsenal", "finalfit")) {
                    if (identical(cont, "mean")) {
                        if (n_groups == 2) .("one-way ANOVA, which for two groups is the pooled-variance t-test") else .("one-way ANOVA")
                    } else {
                        .("the Kruskal-Wallis test")
                    }
                } else if (identical(sty, "gtsummary")) {
                    if (n_groups == 2) .("the Wilcoxon rank-sum test") else .("the Kruskal-Wallis test")
                } else {
                    if (n_groups == 2) .("the Wilcoxon rank-sum test, through the F approximation the NEJM, Lancet and Hmisc layouts use") else .("the Kruskal-Wallis test, through the F approximation the NEJM, Lancet and Hmisc layouts use")
                }

                parts <- sprintf(
                    .("Cross table of %d rows comparing %d variable(s) (%s) across %s (%s) using the %s style."),
                    nrow(mydata), length(myvars), paste(var_labels, collapse = ", "),
                    group_label, group_sizes_txt, style_display)
                if (n_dropped > 0) {
                    n_all <- nrow(mydata) + n_dropped
                    parts <- c(parts, sprintf(
                        .("%d of %d rows (%s%%) were excluded for missing values in the selected variables."),
                        n_dropped, n_all, format(round(100 * n_dropped / n_all, 1), nsmall = 1)))
                }
                parts <- c(parts, sprintf(
                    .("Categorical variables were compared with %s and continuous variables with %s."),
                    cat_test, cont_test))

                if (is.null(pvalues) || nrow(pvalues) == 0 || all(is.na(pvalues$p))) {
                    parts <- c(parts, .("Per-variable p-values could not be read from the table, so no significance summary is given."))
                } else {
                    use_q <- identical(sty, "gtsummary") && !identical(p_adjust, "none") && any(!is.na(pvalues$q))
                    if (use_q) {
                        method <- c(bonferroni = "Bonferroni", holm = "Holm",
                                    BH = "Benjamini-Hochberg", BY = "Benjamini-Yekutieli")[[p_adjust]]
                        sig <- pvalues[!is.na(pvalues$q) & pvalues$q < 0.05, , drop = FALSE]
                        listing <- paste(sprintf("%s (p %s, q %s)", sig$display, sig$p_text, private$.formatP(sig$q)), collapse = ", ")
                        parts <- c(parts, if (nrow(sig) > 0)
                            sprintf(.("After %s adjustment across the %d variables, %d differed between the groups at q < 0.05: %s."),
                                    method, nrow(pvalues), nrow(sig), listing)
                        else
                            sprintf(.("After %s adjustment across the %d variables, none differed between the groups at q < 0.05."),
                                    method, nrow(pvalues)))
                    } else {
                        sig <- pvalues[!is.na(pvalues$p) & pvalues$p < 0.05, , drop = FALSE]
                        listing <- paste(sprintf("%s (p %s)", sig$display, sig$p_text), collapse = ", ")
                        parts <- c(parts, if (nrow(sig) > 0)
                            sprintf(.("At p < 0.05, %d of %d variables differed between the groups: %s."),
                                    nrow(sig), nrow(pvalues), listing)
                        else
                            .("No variable differed between the groups at p < 0.05."))
                    }
                }
                paste0("<p style='color: inherit;'>",
                       htmltools::htmlEscape(paste(parts, collapse = " ")),
                       "</p>")
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
                        .("Standardized mean differences require exactly two groups; the SMD table is shown only for a two-level grouping variable."))
                    return()
                }
                levs <- levels(g)
                i1 <- g == levs[1]; i2 <- g == levs[2]

                for (v in vars) {
                    x <- data[[v]]
                    # Deliberately NOT .crosstableIsCategorical() here, unlike the
                    # display table above. That helper calls any numeric column with
                    # <= 6 distinct values an encoded category, which sends every
                    # ordinal clinical code (grade 1/2/3, TStage 1-4) to the
                    # multinomial SMD. That statistic is unusable exactly where a
                    # balance diagnostic matters most: when a level is missing from
                    # one arm the pooled covariance is rank-deficient, the difference
                    # vector lies in its null space, and ginv() projects it away.
                    # TStage {1,2} vs {3} - complete separation - then returns
                    # 7.5e-09 ("negligible") where the mean-difference SMD returns
                    # -4.02. It is also unsigned, so the direction of the imbalance
                    # is lost. Numeric columns therefore keep the continuous SMD.
                    isNum <- is.numeric(x) && !is.factor(x)
                    smd <- NA_real_; vtype <- .("categorical")
                    if (isNum) {
                        # Label numeric codes so this row cannot contradict the
                        # coded-variable note above the table.
                        vtype <- if (.crosstableIsCategorical(x)) .("continuous (numeric codes)") else .("continuous")
                        x1 <- x[i1]; x2 <- x[i2]
                        smd <- private$.smdContinuous(x1, x2)
                    } else {
                        smd <- private$.smdCategorical(x[i1], x[i2])
                    }
                    a <- abs(smd)
                    bal <- if (is.na(a)) "-"
                           else if (a < 0.1) .("negligible (< 0.1)")
                           else if (a < 0.2) .("small (0.1-0.2)")
                           else .("notable (>= 0.2)")
                    display_name <- .crosstableDisplayName(v, name_mapping)
                    tab$setRow(rowKey = display_name, values = list(
                        variable = display_name, vtype = vtype, smd = smd,
                        absSMD = a, balance = bal))
                }
                tab$setNote("smd", sprintf(
                    .("SMD between '%s' and '%s'. Continuous: (m1 - m2) / sqrt((s1^2 + s2^2)/2); numeric columns with 6 or fewer distinct values are labelled 'continuous (numeric codes)' and treat the codes as an interval scale. Categorical: multinomial SMD (Yang & Dalton, 2012). |SMD| < 0.1 conventionally indicates negligible imbalance."),
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
                # A level absent from one arm makes S rank-deficient. ginv() is a
                # pseudo-inverse, so it does not error: it silently projects the part
                # of Tm lying in the null space away and returns a value near zero -
                # the statistic reports "perfectly balanced" for the one situation
                # that is maximally imbalanced. Refuse to report a number instead.
                # Verified: Grade {1,2} vs {3} gives rank(S) = 1 where 2 is needed and
                # yields 7.5e-09; a well-overlapped 3-level split gives full rank.
                if (qr(S)$rank < (k - 1)) return(NA_real_)
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
