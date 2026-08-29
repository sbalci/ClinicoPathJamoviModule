#' @title Medical Decision Analysis
#' @description Implements comprehensive medical decision analysis including:
#' @details This module provides tools for analyzing diagnostic test performance
#'   with options for various visualization methods and statistical comparisons.
#'   - Sensitivity, specificity and predictive values
#' @section Usage:
#'   1. Provide test and reference standard data
#'   2. Select analysis options
#'   3. View results in tables and plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats binom.test
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor
#' @importFrom epiR epi.tests
#' @return An \code{R6} class generator object for the \code{decisionClass} backend; used internally by the jamovi analysis wrapper and not called directly.


#  @references
#    - DeLong et al. (1988) for ROC comparison
#   - Hanley & McNeil (1982) for AUC confidence intervals
#    - ROC curve analysis with confidence intervals
#    - Multiple test comparison
#    - Bootstrapped confidence intervals



decisionClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisionClass",
        inherit = decisionBase,
        private = list(
            .n_complete_cases = NA_integer_,
            .n_level_excluded = 0L,
            # Constants for maintainability
            NOMOGRAM_LABEL_SIZE = 14/5,

            # TODO [i18n] jamovi/i18n/ is bootstrapped and all 106 translatable
            #   strings from this file are in catalog.pot, but only 2 of them are
            #   translated in tr.po. Finish the Turkish catalog before release.
            #
            # The 2026-05-14 audit TODO that stood here was re-verified 2026-08-29:
            # its report file is gone and 5 of its 6 items were already false or
            # obsolete. See git history for the retired text.

            .init = function() {
                cTable <- self$results$cTable
                cTable$addRow(rowKey = "Test Positive", values = list(newtest = .("Test Positive")))
                cTable$addRow(rowKey = "Test Negative", values = list(newtest = .("Test Negative")))
                cTable$addRow(rowKey = "Total", values = list(newtest = .("Total")))

                summary_table <- self$results$confusionMatrixSummary
                summary_table$addRow(rowKey = 1, values = list(classification = .("True Positive")))
                summary_table$addRow(rowKey = 2, values = list(classification = .("False Positive")))
                summary_table$addRow(rowKey = 3, values = list(classification = .("False Negative")))
                summary_table$addRow(rowKey = 4, values = list(classification = .("True Negative")))

                # Populate welcome message
                self$results$welcome$setContent(paste0(
                    "<div style='padding: 20px; background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); border-radius: 8px; border-left: 4px solid #4A90E2;'>",
                    "<h3 style='margin-top: 0; color: #2c3e50;'>", .("Medical Decision Analysis"), "</h3>",
                    "<p style='font-size: 15px; color: #34495e;'>", .("Evaluate diagnostic test performance with sensitivity, specificity, and predictive values."), "</p>",

                    "<h4 style='color: #2c3e50; margin-top: 20px;'>", .("Quick Start"), ":</h4>",
                    "<ol style='font-size: 14px; color: #34495e; line-height: 1.8;'>",
                    "<li><strong>", .("Select Gold Standard"), ":</strong> ", .("Choose the reference variable representing true disease status (e.g., biopsy result, final diagnosis)"), "</li>",
                    "<li><strong>", .("Select Disease present level"), ":</strong> ", .("Choose which level indicates disease is present"), "</li>",
                    "<li><strong>", .("Select New Test"), ":</strong> ", .("Choose the diagnostic test you want to evaluate"), "</li>",
                    "<li><strong>", .("Select Test positive level"), ":</strong> ", .("Choose which level represents a positive test result"), "</li>",
                    "</ol>",

                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border-radius: 5px; margin-top: 15px; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: #2c3e50;'>", .("What You'll Get"), ":</h4>",
                    "<ul style='font-size: 13px; color: #34495e; line-height: 1.6;'>",
                    "<li><strong>", .("Sensitivity"), " &amp; ", .("Specificity"), ":</strong> ", .("How well the test identifies disease presence and absence"), "</li>",
                    "<li><strong>", .("Predictive Values"), ":</strong> ", .("Probability of disease given test results (PPV, NPV)"), "</li>",
                    "<li><strong>", .("Likelihood Ratios"), ":</strong> ", .("How much test results change disease probability"), "</li>",
                    "<li><strong>", .("Confidence Intervals"), ":</strong> ", .("Uncertainty estimates for all statistics"), "</li>",
                    "<li><strong>", .("Fagan Nomogram"), ":</strong> ", .("Visual representation of probability changes"), "</li>",
                    "<li><strong>", .("Misclassification Analysis"), ":</strong> ", .("Detailed examination of false positives and false negatives"), "</li>",
                    "</ul>",
                    "</div>",
                    "</div>"
                ))

                # Control welcome message visibility programmatically
                # Hide when all required options are set
                has_gold <- !is.null(self$options$gold) && length(self$options$gold) > 0
                has_newtest <- !is.null(self$options$newtest) && length(self$options$newtest) > 0
                has_goldPositive <- !is.null(self$options$goldPositive) && length(self$options$goldPositive) > 0 && nchar(self$options$goldPositive) > 0
                has_testPositive <- !is.null(self$options$testPositive) && length(self$options$testPositive) > 0 && nchar(self$options$testPositive) > 0

                # Show welcome when NOT all options are set
                # Logic: visible = !(gold && newtest && goldPositive && testPositive)
                show_welcome <- !(has_gold && has_newtest && has_goldPositive && has_testPositive)
                self$results$welcome$setVisible(show_welcome)
            },

            # Initialize notice collection list
            .noticeList = list(),

            # HTML sanitization for security
            .safeHtmlOutput = function(text) {
                if (is.null(text) || length(text) == 0) return("")
                text <- as.character(text)
                # Sanitize potentially dangerous characters
                text <- gsub("&", "&amp;", text, fixed = TRUE)
                text <- gsub("<", "&lt;", text, fixed = TRUE)
                text <- gsub(">", "&gt;", text, fixed = TRUE)
                text <- gsub("\"", "&quot;", text, fixed = TRUE)
                text <- gsub("'", "&#x27;", text, fixed = TRUE)
                text <- gsub("/", "&#x2F;", text, fixed = TRUE)
                return(text)
            },

            # Add a notice to the collection
            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
            },

            # Render collected notices as HTML
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    # Clear, do not just return: jamovi keeps the previous content, so a
                    # warning from an earlier run would sit beside numbers that no longer
                    # support it. Matches decisioncalculator.b.R:32.
                    self$results$notices$setContent("")
                    return()
                }

                # Map notice types to colors and icons
                typeStyles <- list(
                    # Translucent rgba tints, not opaque pastels: they composite over
                    # either jamovi theme instead of painting a white block into the
                    # dark one. Same palette as the reference implementation in
                    # waterfall.b.R.
                    ERROR = list(color = "#dc2626", bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", icon = ""),
                    STRONG_WARNING = list(color = "#ea580c", bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", icon = ""),
                    WARNING = list(color = "#ca8a04", bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", icon = ""),
                    INFO = list(color = "#2563eb", bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", icon = "")
                )

                html <- "<div style='margin: 10px 0;'>"

                for (notice in private$.noticeList) {
                    style <- typeStyles[[notice$type]] %||% typeStyles$INFO

                    html <- paste0(html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong style='color: ", style$color, ";'>",
                        style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
                        "<span style='color: inherit;'>", private$.safeHtmlOutput(notice$content), "</span>",
                        "</div>"
                    )
                }

                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },

            # Enhanced input validation for categorical diagnostic data
            .validateCategoricalInputs = function() {
                # No "variable not selected" branches here: .run() returns before
                # calling this method unless gold, newtest, goldPositive and
                # testPositive are all set (each is a scalar option, so its length
                # check summing to 4 means all four are present). The unselected
                # state is handled by the `welcome` panel instead.

                # Check data availability
                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("No data available for analysis"),
                        content = .("Please ensure your data is loaded. Check that your dataset contains observations.")
                    )
                    return(FALSE)
                }

                # Validate data has enough cases
                if (nrow(self$data) < 4) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.("Insufficient data: {n} cases found"), n = nrow(self$data)),
                        content = .("At least 4 cases are required for diagnostic test analysis. Each cell of the 2\u{00D7}2 table should have at least one observation.")
                    )
                    return(FALSE)
                }

                # Validate prior probability if specified
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.("Invalid population prevalence: {value}"),
                                                value = sprintf("%.3f", self$options$pprob)),
                        content = .("Prevalence must be between 0 and 1 (exclusive). For 5% prevalence, enter 0.05. For 20% prevalence, enter 0.20.")
                    )
                    return(FALSE)
                }

                # Warn about CI interpretation when using population prevalence
                if (self$options$pp && self$options$ci) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Confidence Intervals Interpretation"),
                        content = .("The displayed confidence intervals (95% CI) are calculated from your study sample. They apply to the sample-based Sensitivity, Specificity, PPV and NPV in the epiR tables. They do NOT apply to the PPV and NPV in the main ratio table, which are recomputed by Bayes' theorem at the fixed population prevalence you supplied.")
                    )
                }

                # Validate that selected levels actually exist in the data
                goldVar <- jmvcore::constructFormula(terms = self$options$gold) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                testVar <- jmvcore::constructFormula(terms = self$options$newtest) %>%
                          jmvcore::decomposeFormula() %>% unlist()

                # Get actual levels from data
                gold_levels <- if (is.factor(self$data[[goldVar]])) {
                    levels(self$data[[goldVar]])
                } else {
                    sort(unique(as.character(self$data[[goldVar]])))
                }

                test_levels <- if (is.factor(self$data[[testVar]])) {
                    levels(self$data[[testVar]])
                } else {
                    sort(unique(as.character(self$data[[testVar]])))
                }

                # Validate gold standard positive level
                if (!(self$options$goldPositive %in% gold_levels)) {
                    available_levels <- if (length(gold_levels) <= 10) {
                        paste(gold_levels, collapse = ", ")
                    } else {
                        paste(c(gold_levels[1:10], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('Disease-present level "{lvl}" not found in gold standard variable'), lvl = self$options$goldPositive),
                        content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                    )
                    return(FALSE)
                }

                # Validate test positive level
                if (!(self$options$testPositive %in% test_levels)) {
                    available_levels <- if (length(test_levels) <= 10) {
                        paste(test_levels, collapse = ", ")
                    } else {
                        paste(c(test_levels[1:10], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('Test-positive level "{lvl}" not found in test variable'), lvl = self$options$testPositive),
                        content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                    )
                    return(FALSE)
                }

                # Validate gold standard negative level if specified
                if (length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0) {
                    if (!(self$options$goldNegative %in% gold_levels)) {
                        available_levels <- if (length(gold_levels) <= 10) {
                            paste(gold_levels, collapse = ", ")
                        } else {
                            paste(c(gold_levels[1:10], "..."), collapse = ", ")
                        }
                        private$.addNotice(
                            type = "ERROR",
                            title = .fmt(.('Disease-absent level "{lvl}" not found in gold standard variable'), lvl = self$options$goldNegative),
                            content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                        )
                        return(FALSE)
                    }

                    # Check that positive and negative levels are different
                    if (self$options$goldNegative == self$options$goldPositive) {
                        private$.addNotice(
                            type = "ERROR",
                            title = .("Disease-present and disease-absent levels cannot be the same"),
                            content = .("Select different levels for positive and negative outcomes.")
                        )
                        return(FALSE)
                    }
                }

                # Validate test negative level if specified
                if (length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0) {
                    if (!(self$options$testNegative %in% test_levels)) {
                        available_levels <- if (length(test_levels) <= 10) {
                            paste(test_levels, collapse = ", ")
                        } else {
                            paste(c(test_levels[1:10], "..."), collapse = ", ")
                        }
                        private$.addNotice(
                            type = "ERROR",
                            title = .fmt(.('Test-negative level "{lvl}" not found in test variable'), lvl = self$options$testNegative),
                            content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                        )
                        return(FALSE)
                    }

                    # Check that positive and negative levels are different
                    if (self$options$testNegative == self$options$testPositive) {
                        private$.addNotice(
                            type = "ERROR",
                            title = .("Test-positive and test-negative levels cannot be the same"),
                            content = .("Select different levels for positive and negative outcomes.")
                        )
                        return(FALSE)
                    }
                }

                return(TRUE)
            },

            # Enhanced likelihood ratio validation with recovery
            .validateLikelihoodRatios = function(lrp, lrn, sens, spec) {
                issues <- character(0)

                # Check LR+ validity
                if (is.na(lrp) || !is.finite(lrp)) {
                    if (is.na(sens) || is.na(spec)) {
                        lrp <- NA_real_
                    } else {
                        lrp <- ifelse(spec == 0, Inf, sens / max(1 - spec, 0.001))
                    }
                    issues <- c(issues, .("LR+ recalculated due to invalid value"))
                }
                if (!is.na(lrp) && lrp <= 0) {
                    if (!is.na(sens) && !is.na(spec)) {
                        lrp <- max(0.01, sens / max(1 - spec, 0.001))
                    } else {
                        lrp <- NA_real_
                    }
                    issues <- c(issues, .("LR+ adjusted to positive value"))
                }

                # Check LR- validity
                if (is.na(lrn) || !is.finite(lrn)) {
                    if (is.na(sens) || is.na(spec)) {
                        lrn <- NA_real_
                    } else {
                        lrn <- ifelse(sens == 1, 0, (1 - sens) / max(spec, 0.001))
                    }
                    issues <- c(issues, .("LR- recalculated due to invalid value"))
                }
                if (!is.na(lrn) && lrn < 0) {
                    if (!is.na(sens) && !is.na(spec)) {
                        lrn <- max(0.001, (1 - sens) / max(spec, 0.001))
                    } else {
                        lrn <- NA_real_
                    }
                    issues <- c(issues, .("LR- adjusted to positive value"))
                }

                list(lrp = lrp, lrn = lrn, issues = issues)
            },

            # Prepare analysis data with efficient processing
            .prepareAnalysisData = function() {
                # Get variable names efficiently
                testVar <- jmvcore::constructFormula(terms = self$options$newtest) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                goldVar <- jmvcore::constructFormula(terms = self$options$gold) %>%
                          jmvcore::decomposeFormula() %>% unlist()

                vars_needed <- unique(c(testVar, goldVar))
                if (length(vars_needed) < 2) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("The gold standard and the test are the same variable"),
                        content = .("Select a different variable for the test under evaluation. Comparing a variable against itself always yields perfect agreement and tells you nothing about the test.")
                    )
                    return(NULL)
                }

                # Restrict case removal to variables used in the diagnostic table
                # Track original row indices BEFORE filtering
                subset_data <- self$data[, vars_needed, drop = FALSE]
                # Spreadsheet row identity, not a position in the filtered frame: the
                # FP/FN tables render this under a column titled "Row", and with a jamovi
                # row filter active a positional index points at the wrong patient.
                # rownames(self$data) carries the original row numbers through the filter.
                subset_data$original_row_position <- seq_len(nrow(subset_data))
                subset_data$original_row_index <- suppressWarnings(as.integer(rownames(self$data)))
                if (anyNA(subset_data$original_row_index))
                    subset_data$original_row_index <- seq_len(nrow(subset_data))

                mydata <- jmvcore::naOmit(subset_data)
                # Rows are dropped twice: here for missingness, and again below for
                # levels that are neither the positive nor the negative level. The
                # summary used to attribute both to "missing values".
                private$.n_complete_cases <- nrow(mydata)

                if (nrow(mydata) < nrow(self$data)) {
                    removed <- nrow(self$data) - nrow(mydata)
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Removed {n} row(s) with missing diagnostic data"), n = removed),
                        content = .fmt(.("Complete-case analysis uses {used} of {total} cases. Consider investigating patterns of missingness."),
                                                  used = nrow(mydata), total = nrow(self$data))
                    )
                }
                
                # Convert to factors and recode in single pipeline
                mydata[[testVar]] <- forcats::as_factor(mydata[[testVar]])
                mydata[[goldVar]] <- forcats::as_factor(mydata[[goldVar]])

                # An explicit NA level (addNA()) survives naOmit and is.na(), but means
                # "status unknown" -- scoring it as disease-absent silently biases every
                # metric. Drop those rows and disclose it.
                explicit_na <- is.na(as.character(mydata[[goldVar]])) | is.na(as.character(mydata[[testVar]]))
                if (any(explicit_na)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Removed {n} case(s) with an explicit missing level"), n = sprintf("%d", sum(explicit_na))),
                        content = .("These cases carry a level that codes missingness rather than a result. They are excluded rather than counted as negative, which would have biased sensitivity and specificity.")
                    )
                    mydata <- mydata[!explicit_na, , drop = FALSE]
                    mydata[[goldVar]] <- droplevels(mydata[[goldVar]])
                    mydata[[testVar]] <- droplevels(mydata[[testVar]])
                }

                # Get actual levels for validation
                gold_actual_levels <- levels(mydata[[goldVar]])
                test_actual_levels <- levels(mydata[[testVar]])

                # Determine negative levels (explicit or implicit)
                has_gold_negative <- length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0
                has_test_negative <- length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0

                # Exactly two levels take part: the positive one and the negative one.
                # Everything else is EXCLUDED, never pooled into the negative arm.
                #
                # Pooling was the old default and it silently changed the estimates: an
                # "Equivocal" level counted as disease-absent enlarges the specificity
                # denominator with cases the test was never right about. On a 50/40/10
                # gold standard that moved specificity from 0.875 to 0.780 with nothing
                # on screen to say why. An indeterminate result is not a negative result.
                #
                # The level is inferred only when the variable is dichotomous, where there
                # is exactly one candidate and therefore no choice to get wrong. With three
                # or more levels the user must name it.
                #
                # NA_character_ rather than NULL as the unset sentinel: case_when()
                # evaluates every branch, and `x == NULL` yields logical(0), which fails
                # the size check even on an unreachable branch.
                gold_candidates <- setdiff(gold_actual_levels, self$options$goldPositive)
                test_candidates <- setdiff(test_actual_levels, self$options$testPositive)

                gold_negative_level <- if (has_gold_negative) self$options$goldNegative
                                       else if (length(gold_candidates) == 1) gold_candidates[[1]]
                                       else NA_character_
                test_negative_level <- if (has_test_negative) self$options$testNegative
                                       else if (length(test_candidates) == 1) test_candidates[[1]]
                                       else NA_character_

                if (is.na(gold_negative_level)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Choose which gold-standard level means disease absent"),
                        content = .fmt(.('The gold standard has more than one level besides "{pos}" ({lvls}), so the disease-absent level cannot be inferred. Select it under Disease absent level. Levels you do not name are excluded from the analysis rather than counted as disease-absent.'),
                                       pos = self$options$goldPositive,
                                       lvls = paste(gold_candidates, collapse = ", "))
                    )
                    return(NULL)
                }
                if (is.na(test_negative_level)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Choose which test level means a negative result"),
                        content = .fmt(.('The test has more than one level besides "{pos}" ({lvls}), so the test-negative level cannot be inferred. Select it under Test negative level. Levels you do not name are excluded from the analysis rather than counted as negative.'),
                                       pos = self$options$testPositive,
                                       lvls = paste(test_candidates, collapse = ", "))
                    )
                    return(NULL)
                }

                # Check for levels that will be excluded (not positive, not negative)
                gold_used_levels <- c(self$options$goldPositive, gold_negative_level)
                test_used_levels <- c(self$options$testPositive, test_negative_level)

                gold_excluded <- setdiff(gold_actual_levels, gold_used_levels)
                test_excluded <- setdiff(test_actual_levels, test_used_levels)

                if (length(gold_excluded) > 0) {
                    excluded_str <- if (length(gold_excluded) <= 5) {
                        paste(gold_excluded, collapse = ", ")
                    } else {
                        paste(c(gold_excluded[1:5], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.('Gold standard levels excluded from analysis: {lvls}'), lvls = excluded_str),
                        content = .fmt(.('Only "{pos}" (disease-present) and "{neg}" (disease-absent) take part. Cases at any other level are removed, not counted as disease-absent, because an indeterminate result is not a negative result.'), pos = self$options$goldPositive, neg = gold_negative_level)
                    )
                }

                if (length(test_excluded) > 0) {
                    excluded_str <- if (length(test_excluded) <= 5) {
                        paste(test_excluded, collapse = ", ")
                    } else {
                        paste(c(test_excluded[1:5], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.('Test variable levels excluded from analysis: {lvls}'), lvls = excluded_str),
                        content = .fmt(.('Only "{pos}" (test-positive) and "{neg}" (test-negative) take part. Cases at any other level are removed, not counted as negative.'), pos = self$options$testPositive, neg = test_negative_level)
                    )
                }

                # Efficient recoding with explicit negative level handling
                mydata <- mydata %>%
                    dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            is.na(.data[[testVar]]) ~ NA_character_,
                            .data[[testVar]] == self$options$testPositive ~ "Positive",
                            .data[[testVar]] == test_negative_level ~ "Negative",
                            TRUE ~ NA_character_  # Explicit negative specified, others become NA (filtered)
                        ),
                        goldVariable2 = dplyr::case_when(
                            is.na(.data[[goldVar]]) ~ NA_character_,
                            .data[[goldVar]] == self$options$goldPositive ~ "Positive",
                            .data[[goldVar]] == gold_negative_level ~ "Negative",
                            TRUE ~ NA_character_  # Explicit negative specified, others become NA (filtered)
                        )
                    ) %>%
                    dplyr::mutate(
                        # intersect(), not fct_relevel(): a cohort filtered down to a
                        # single disease category has no "Positive" level, and
                        # fct_relevel() warns about it straight into jamovi's Analysis
                        # Notes as raw package chatter -- on exactly the run that
                        # otherwise shows nothing. Same ordering and same one-level
                        # result; the structural 2x2 check below still raises the
                        # real error.
                        testVariable2 = factor(testVariable2,
                                               levels = intersect(c("Positive", "Negative"), testVariable2)),
                        goldVariable2 = factor(goldVariable2,
                                               levels = intersect(c("Positive", "Negative"), goldVariable2))
                    )

                # Remove rows with NA in recoded variables (excluded levels when explicit negative specified)
                mydata <- mydata %>% dplyr::filter(!is.na(testVariable2), !is.na(goldVariable2))
                private$.n_level_excluded <- max(0, private$.n_complete_cases - nrow(mydata))
                
                # Validate contingency table structure after data preparation
                test_table <- table(mydata$testVariable2, mydata$goldVariable2)
                
                # Check for empty cells that would cause problems
                if (any(dim(test_table) != c(2, 2))) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Invalid data structure: Both test and gold standard variables must have exactly 2 levels each"),
                        content = .("Ensure your variables are dichotomous (binary). Check that positive/negative levels are correctly specified.")
                    )
                    return(NULL)
                }
                
                # Check for zero cells that would cause division by zero
                if (any(test_table == 0)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .("Zero counts detected in contingency table"),
                        content = .("Results may be unstable or undefined (e.g., infinite likelihood ratios). Consider collecting more data or using exact methods. Ensure both tests and gold standard have both positive and negative cases.")
                    )
                }
                
                return(list(data = mydata, testVar = testVar, goldVar = goldVar))
            },

            # Enhanced diagnostic accuracy interpretation helper
            .getDiagnosticInterpretation = function(lr_pos, lr_neg, sens, spec) {
                # Likelihood ratio interpretations based on clinical guidelines
                lr_pos_interp <- dplyr::case_when(
                    is.na(lr_pos) ~ .("Positive likelihood ratio unavailable due to data limitations"),
                    lr_pos >= 10 ~ .("Large and often conclusive increase in probability of disease"),
                    lr_pos >= 5 ~ .("Moderate increase in probability of disease"),
                    lr_pos >= 2 ~ .("Small but potentially important increase in probability"),
                    lr_pos > 1 ~ .("Minimal increase in probability of disease"),
                    lr_pos == 1 ~ .("Uninformative: a positive result leaves the probability of disease unchanged"),
                    TRUE ~ .("Decreases probability of disease (test may be flawed)")
                )
                
                lr_neg_interp <- dplyr::case_when(
                    is.na(lr_neg) ~ .("Negative likelihood ratio unavailable due to data limitations"),
                    lr_neg <= 0.1 ~ .("Large and often conclusive decrease in probability of disease"),
                    lr_neg <= 0.2 ~ .("Moderate decrease in probability of disease"),
                    lr_neg <= 0.5 ~ .("Small but potentially important decrease in probability"),
                    lr_neg < 1 ~ .("Minimal decrease in probability of disease"),
                    lr_neg == 1 ~ .("Uninformative: a negative result leaves the probability of disease unchanged"),
                    TRUE ~ .("Increases probability of disease (test may be flawed)")
                )
                
                # Overall test utility based on Youden's Index
                youden_index <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1
                test_utility <- dplyr::case_when(
                    is.na(youden_index) ~ .("Insufficient data to evaluate discriminatory power"),
                    youden_index >= 0.8 ~ .("Excellent discriminatory power (Youden's index 0.80 or above)"),
                    youden_index >= 0.6 ~ .("Good discriminatory power (Youden's index 0.60 to 0.79)"),
                    youden_index >= 0.4 ~ .("Fair discriminatory power (Youden's index 0.40 to 0.59)"),
                    TRUE ~ .("Poor discriminatory power (Youden's index below 0.40)")
                )
                
                return(list(
                    lr_pos_interp = lr_pos_interp,
                    lr_neg_interp = lr_neg_interp,
                    youden_index = youden_index,
                    test_utility = test_utility
                ))
            },
            
            # Basic missing data summary
            .analyzeMissingData = function(original_data, processed_data) {
                n_orig <- nrow(original_data)
                dropped <- n_orig - nrow(processed_data)
                if (dropped <= 0) {
                    return(.("No cases excluded."))
                }

                level_excluded <- private$.n_level_excluded
                if (!is.numeric(level_excluded) || length(level_excluded) != 1 || is.na(level_excluded))
                    level_excluded <- 0
                level_excluded <- min(max(level_excluded, 0), dropped)
                missing_count <- dropped - level_excluded

                pct <- function(k) round((k / n_orig) * 100, 1)
                parts <- character(0)
                if (missing_count > 0)
                    parts <- c(parts, .fmt(.("{n} case(s) ({pct}) removed for missing values on the test or gold-standard variable"),
                                           n = sprintf("%d", missing_count),
                                           pct = paste0(base::format(pct(missing_count)), "%")))
                if (level_excluded > 0)
                    parts <- c(parts, .fmt(.("{n} case(s) ({pct}) removed because their level was neither the positive nor the negative level you selected (these are NOT missing values)"),
                                           n = sprintf("%d", level_excluded),
                                           pct = paste0(base::format(pct(level_excluded)), "%")))

                .fmt(
                    .("Case exclusion summary: {details}. {analysed} of {total} cases analysed."),
                    details = paste(parts, collapse = "; "),
                    analysed = nrow(processed_data),
                    total = n_orig)
            },
            
            # Generate natural language summary for clinical use
            .generateNaturalLanguageSummary = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                                      prevalence, total_pop, test_name, gold_name) {
                format_percent <- function(value, fallback_label) {
                    if (is.na(value)) return(fallback_label)
                    sprintf("%.1f%%", value * 100)
                }

                # Get clinical benchmarks for enhanced interpretation
                benchmarks <- private$.addClinicalBenchmarks(sens, spec, lr_pos, lr_neg)

                # Determine test quality
                test_quality <- dplyr::case_when(
                    !is.na(sens) && !is.na(spec) && sens >= 0.9 && spec >= 0.9 ~ .("excellent"),
                    !is.na(sens) && !is.na(spec) && sens >= 0.8 && spec >= 0.8 ~ .("good"), 
                    (!is.na(sens) && sens >= 0.7) || (!is.na(spec) && spec >= 0.7) ~ .("moderate"),
                    TRUE ~ .("limited")
                )

                # Describe the discrimination profile (no use recommendation is made)
                primary_utility <- dplyr::case_when(
                    is.na(sens) || is.na(spec) ~ .("sensitivity or specificity could not be computed in this sample, so the discrimination profile is incomplete"),
                    sens >= 0.9 && spec < 0.8 ~ .("sensitivity is high (0.90 or above) while specificity is below 0.80: few false negatives, more false positives in this sample"),
                    spec >= 0.9 && sens < 0.8 ~ .("specificity is high (0.90 or above) while sensitivity is below 0.80: few false positives, more false negatives in this sample"),
                    sens >= 0.8 && spec >= 0.8 ~ .("sensitivity and specificity are both 0.80 or above in this sample"),
                    TRUE ~ .("sensitivity and specificity were not both 0.80 or above in this sample")
                )

                prevalence_text <- format_percent(prevalence, .("not reported"))
                sens_text <- format_percent(sens, .("not calculated"))
                spec_text <- format_percent(spec, .("not calculated"))
                ppv_text <- format_percent(ppv, .("not calculated"))
                npv_text <- format_percent(npv, .("not calculated"))
                # Only the complement of NPV adds anything: the post-test probability of
                # disease after a NEGATIVE result. The other two locals were PPV and NPV
                # under different names.
                post_neg_disease_text <- format_percent(1 - npv, .("not available"))

                sample_text <- if (!is.na(total_pop)) .fmt(.("{n} cases analyzed"), n = total_pop) else .("Sample size not available")

                summary_template <- .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #4CAF50; background-color: rgba(114, 184, 33, 0.1); color: inherit;'><h3 style='color: #2E7D32; margin-top: 0;'>Clinical Summary</h3><p style='font-size: 16px;'><strong>Analysis:</strong> Diagnostic test performance evaluation comparing {testname} against gold standard {goldname}.</p><p><strong>Sample:</strong> {sample}. Predictive values below are computed at a disease prevalence of {prev}.</p><p><strong>Test Performance:</strong> The test shows <strong>{quality}</strong> discriminatory ability with sensitivity of <strong>{sens}</strong> (<em>{sensnote}</em>) and specificity of <strong>{spec}</strong> (<em>{specnote}</em>).</p><p><strong>Discrimination Profile:</strong> {profile}.</p><p><strong>Likelihood Ratios:</strong> Positive LR: {lrpos} (<em>{lrposnote}</em>), Negative LR: {lrneg} (<em>{lrnegnote}</em>)</p><p><strong>Key Findings:</strong> Predictive values are post-test probabilities. After a positive result the probability of disease is the positive predictive value (PPV {ppv}). After a negative result the probability of disease is <strong>{postneg}</strong>, and the probability of being disease-free is the negative predictive value (NPV {npv}).</p></div>")

                # Only the NA arm can fire: a zero cell triggers the Haldane-Anscombe 0.5
                # correction before the LRs are formed, so neither LR is ever Inf here,
                # and both are NA exactly when sensitivity or specificity is NA.
                lr_pos_safe <- if (is.na(lr_pos)) .("not calculated (needs both sensitivity and specificity)")
                    else sprintf("%.2f", lr_pos)
                lr_neg_safe <- if (is.na(lr_neg)) .("not calculated (needs both sensitivity and specificity)")
                    else sprintf("%.2f", lr_neg)

                # Escape user-derived variable names before HTML interpolation
                test_name_safe <- private$.safeHtmlOutput(test_name)
                gold_name_safe <- private$.safeHtmlOutput(gold_name)

                summary <- .fmt(
                    summary_template,
                    testname = test_name_safe, goldname = gold_name_safe,
                    sample = sample_text, prev = prevalence_text,
                    quality = test_quality, sens = sens_text, sensnote = benchmarks$sens_quality,
                    spec = spec_text, specnote = benchmarks$spec_quality,
                    profile = primary_utility,
                    lrpos = lr_pos_safe, lrposnote = benchmarks$lr_pos_interpretation,
                    lrneg = lr_neg_safe, lrnegnote = benchmarks$lr_neg_interpretation,
                    ppv = ppv_text, postneg = post_neg_disease_text, npv = npv_text
                )

                return(summary)
            },
            
            # Generate copy-ready report template
            .generateReportTemplate = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                             sens_ci = NULL, spec_ci = NULL, test_name, gold_name,
                                             prevalence = NA_real_) {
                # Create confidence interval text if available
                ci_text <- if (!is.null(sens_ci) && !is.null(spec_ci)) {
                    .fmt(.("(95% CI: sensitivity {sens}, specificity {spec})"),
                         sens = sprintf("%.1f-%.1f%%", sens_ci[1]*100, sens_ci[2]*100),
                         spec = sprintf("%.1f-%.1f%%", spec_ci[1]*100, spec_ci[2]*100))
                } else {
                    ""
                }
                
                # Determine clinical interpretation
                interpretation <- dplyr::case_when(
                    is.na(lr_pos) ~ .("likelihood ratio unavailable due to data limitations"),
                    lr_pos >= 10 ~ .("strong evidence for disease when positive"),
                    lr_pos >= 5 ~ .("moderate evidence for disease when positive"),
                    lr_pos >= 2 ~ .("weak evidence for disease when positive"),
                    lr_pos > 1 ~ .("minimal evidence for disease when positive"),
                    lr_pos == 1 ~ .("no change in the probability of disease"),
                    # An LR+ below 1 points the other way. Calling that "minimal evidence
                    # for disease" in text a clinician pastes into a chart inverts the
                    # finding.
                    TRUE ~ .("evidence AGAINST disease when positive, which usually means the level chosen as test-positive is inverted")
                )
                
                # Generate template
                template_string <- .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background-color: rgba(33, 152, 239, 0.13); color: inherit;'><h3 style='color: #1976D2; margin-top: 0;'>Copy-Ready Clinical Report</h3><div style='background: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-radius: 5px; font-family: Arial, sans-serif;'><p><strong>DIAGNOSTIC TEST EVALUATION</strong></p><p>We evaluated the diagnostic performance of {testname} compared to the gold standard {goldname}. The test demonstrated a sensitivity of {sens} and specificity of {spec} {ci}. At a disease prevalence of {prev}, the positive predictive value was {ppv} and the negative predictive value was {npv}. The positive likelihood ratio of {lr} provides {interp}.</p></div><p style='font-size: 12px; color: inherit; opacity: 0.75;'><em>Copy the text above for your clinical report. Modify as needed for your specific context.</em></p></div>")

                # Escape user-derived variable names before HTML interpolation
                test_name_safe <- private$.safeHtmlOutput(test_name)
                gold_name_safe <- private$.safeHtmlOutput(gold_name)

                template <- .fmt(
                    template_string,
                    testname = test_name_safe, goldname = gold_name_safe,
                    sens = sprintf("%.1f%%", sens * 100),
                    spec = sprintf("%.1f%%", spec * 100),
                    ci = ci_text,
                    # Predictive values are meaningless without the prevalence they
                    # were computed at; the sentence quoted them bare.
                    prev = sprintf("%.1f%%", if (is.finite(prevalence)) prevalence * 100 else NA_real_),
                    ppv = sprintf("%.1f%%", ppv * 100),
                    npv = sprintf("%.1f%%", npv * 100),
                    lr = sprintf("%.1f", lr_pos),
                    interp = interpretation
                )

                return(template)
            },
            
            # Misuse detection.
            #
            # These used to be assembled into an HTML panel prepended to
            # clinicalInterpretation, which is visible: (showClinicalInterpretation)
            # and defaults to FALSE -- so the warnings a clinician most needs were
            # the ones hidden behind an opt-in checkbox. They now go to the
            # always-visible notices pane.
            #
            # The small-cell (< 5) and small-sample checks that used to live here
            # were duplicates of .validateSampleSize(), which already covers both
            # and already writes to notices. Only the three checks unique to this
            # function remain.
            .detectMisuse = function(conf_table, prevalence, n_total) {
                if (!is.na(prevalence) && prevalence < 0.05) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .fmt(.("Very low disease prevalence observed in this sample ({pct})"),
                                                pct = sprintf("%.1f%%", 100 * prevalence)),
                        content = .("Positive predictive value is unstable at this prevalence and will not transfer to a population with a different one. Sensitivity and specificity are unaffected. Consider supplying a population prior under Population Prevalence Settings.")
                    )
                }

                if (!is.na(prevalence) && prevalence > 0.95) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .fmt(.("Very high disease prevalence observed in this sample ({pct})"),
                                                pct = sprintf("%.1f%%", 100 * prevalence)),
                        content = .("Negative predictive value is unstable at this prevalence. Verify that the level selected as disease-present is the one you meant.")
                    )
                }

                pos_ratio <- sum(conf_table[1, ]) / n_total
                if (!is.na(pos_ratio) && (pos_ratio < 0.1 || pos_ratio > 0.9)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Highly unbalanced test results ({pct} positive)"),
                                                pct = sprintf("%.1f%%", 100 * pos_ratio)),
                        content = .("Check that the level selected under Test positive level is the one you meant: this analysis has no numeric cut-point, it simply treats that level as a positive result.")
                    )
                }

                invisible(NULL)
            },
            
            # Generate About This Analysis content
            .generateAboutAnalysis = function() {
                about_content <- paste0(
                    "<div style='margin: 15px; padding: 15px; background-color: rgba(88, 88, 88, 0.06); border-radius: 8px; color: inherit;'>",
                    "<h3 style='color: #1976D2; margin-top: 0;'>", .("About Diagnostic Test Evaluation"), "</h3>",

                    "<h4 style='color: #424242;'>", .("What This Analysis Does"), "</h4>",
                    "<p>", .("DIAGNOSTIC TEST EVALUATION: Compare test accuracy to gold standard reference. This function evaluates diagnostic test performance by comparing test results to a gold standard (reference). It calculates key diagnostic accuracy measures including sensitivity, specificity, predictive values, and likelihood ratios."), "</p>",

                    "<h4 style='color: #424242;'>", .("When to Use This Analysis"), "</h4>",
                    "<ul>",
                    "<li>", .("Validating new tests"), "</li>",
                    "<li>", .("Clinical validation studies"), "</li>",
                    "<li>", .("Test comparisons"), "</li>",
                    "<li>", .("Comparing performance of different diagnostic methods"), "</li>",
                    "<li>", .("Quality assurance for laboratory tests"), "</li>",
                    "<li>", .("Medical device evaluation"), "</li>",
                    "</ul>",

                    "<h4 style='color: #424242;'>", .("Data Requirements"), "</h4>",
                    "<p>", .("Required data: Cases with both test results and true disease status (gold standard). Both variables must be categorical (factor), each with exactly 2 levels, minimum 4 cases (preferably 30+)."), "</p>",

                    "<h4 style='color: #424242;'>", .("Key Output Measures"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("Sensitivity"), ":</strong> ", .("Proportion of diseased patients correctly identified (true positive rate). Higher is better for ruling OUT disease when negative."), "</li>",
                    "<li><strong>", .("Specificity"), ":</strong> ", .("Proportion of healthy patients correctly identified (true negative rate). Higher is better for ruling IN disease when positive."), "</li>",
                    "<li><strong>", .("PPV (Positive Predictive Value)"), ":</strong> ", .("Probability of disease given a positive test. Depends on prevalence, sensitivity and specificity."), "</li>",
                    "<li><strong>", .("NPV (Negative Predictive Value)"), ":</strong> ", .("Probability of being healthy given a negative test. Depends on prevalence, sensitivity and specificity."), "</li>",
                    "<li><strong>", .("LR+ (Positive Likelihood Ratio)"), ":</strong> ", .("How much a positive test increases disease odds. LR+ >10 strong evidence FOR disease, LR+ 5-10 moderate, LR+ 2-5 weak but useful."), "</li>",
                    "<li><strong>", .("LR- (Negative Likelihood Ratio)"), ":</strong> ", .("How much a negative test decreases disease odds. LR- <0.1 strong evidence AGAINST disease, LR- 0.1-0.2 moderate, LR- 0.2-0.5 weak."), "</li>",
                    "</ul>",
                    
                    "<h4 style='color: #424242;'>", .("Clinical Interpretation Guidelines"), "</h4>",
                    "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                    "<strong>", .("Excellent Tests"), ":</strong><br>",
                    "\u{2022} ", .("Sensitivity >90% excellent for ruling OUT disease"), "<br>",
                    "\u{2022} ", .("Specificity >90% excellent for ruling IN disease"), "<br>",
                    "\u{2022} ", .("LR+ >10 strong evidence FOR disease, LR+ 5-10 moderate, LR+ 2-5 weak but useful"), "<br>",
                    "\u{2022} ", .("LR- <0.1 strong evidence AGAINST disease, LR- 0.1-0.2 moderate, LR- 0.2-0.5 weak"), "<br>",
                    "</div>",

                    "<h4 style='color: #424242;'>", .("Analysis Options Explained"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("95% Confidence Intervals"), ":</strong> ", .("Provides uncertainty estimates using the epiR package. These intervals describe the observed sample; when a population prevalence is supplied they do not apply to the prior-adjusted predictive values in the main table."), "</li>",
                    "<li><strong>", .("Explanatory Footnotes"), ":</strong> ", .("Adds detailed clinical interpretation help to all result tables."), "</li>",
                    "<li><strong>", .("Raw Data Tables"), ":</strong> ", .("Displays original contingency tables and missing data summaries for verification."), "</li>",
                    "<li><strong>", .("Population Prevalence"), ":</strong> ", .("Use when your study sample doesn't represent the target population prevalence. Affects PPV/NPV calculations using Bayes' theorem. Enter as proportion (e.g., 0.05 for 5%, 0.15 for 15%). Common ranges: rare diseases (0.001-0.01), common conditions (0.05-0.30)."), "</li>",
                    "<li><strong>", .("Fagan Nomogram"), ":</strong> ", .("Visual tool showing how test results change disease probability. Shows relationship between pre-test probability, likelihood ratios, and post-test probability."), "</li>",
                    "</ul>",

                    "<h4 style='color: #424242;'>", .("Common Issues"), "</h4>",
                    "<ul>",
                    "<li>", .("Small cell counts (<5) may cause unstable results"), "</li>",
                    "<li>", .("Extreme prevalence affects predictive values"), "</li>",
                    "<li>", .("Predictive values depend on disease prevalence in your population"), "</li>",
                    "<li>", .("Consider clinical consequences of false positives vs false negatives"), "</li>",
                    "<li>", .("Results are only as good as your gold standard"), "</li>",
                    "</ul>",

                    "</div>"
                )
                
                return(about_content)
            },

            # Check data size and provide performance warnings
            .checkDataSize = function(data) {
                n_rows <- nrow(data)
                if (n_rows > 100000) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Very large dataset detected ({n} rows)"), n = n_rows),
                        content = .("Analysis may take longer than usual. Consider sampling for initial exploratory analysis. Full dataset will still be used for final results.")
                    )
                } else if (n_rows > 10000) {
                    private$.addNotice(
                        type = "INFO",
                        title = .fmt(.("Large dataset detected ({n} rows)"), n = n_rows),
                        content = .("Analysis may take a moment to complete.")
                    )
                }
            },

            # Validate sample size for diagnostic test evaluation
            .validateSampleSize = function(conf_table) {
                total_n <- sum(conf_table)
                min_cell <- min(conf_table)

                # Clinical best practices for diagnostic tests
                if (total_n < 20) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .fmt(.("Very small sample size: n = {n} (< 20 cases)"), n = total_n),
                        content = .("With fewer than 20 cases each proportion rests on a handful of patients, so one reclassified case moves sensitivity or specificity by several percentage points and the 95% confidence intervals (enable the 95% CI option) will be very wide. Read the intervals rather than the point estimates; diagnostic accuracy studies usually need on the order of 100 cases before the intervals narrow usefully.")
                    )
                } else if (total_n < 50) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Small sample size: n = {n} (< 50 cases)"), n = total_n),
                        content = .("Interpret results with caution. Confidence intervals may be wide. Minimum recommended: 100 cases for robust estimates.")
                    )
                } else if (total_n < 100) {
                    private$.addNotice(
                        type = "INFO",
                        title = .fmt(.("Sample size: n = {n}"), n = total_n),
                        content = .("For robust diagnostic test evaluation, 100+ cases recommended. Current sample provides preliminary estimates.")
                    )
                }

                if (min_cell < 5) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Small cell count detected (minimum = {n}, < 5)"), n = min_cell),
                        content = .("Statistical estimates may be unstable. Enable 95% confidence intervals: the exact (Clopper-Pearson) intervals shown there remain valid with small cells and will be wide enough to show it.")
                    )
                }
            },

            # Add clinical performance benchmarks for interpretation
            .addClinicalBenchmarks = function(sens, spec, lr_pos, lr_neg) {
                benchmarks <- list()

                # Sensitivity describes the false-negative side of the 2x2: how many
                # gold-standard positive cases this test called negative. The is.na()
                # guard is required -- sensitivity is NA when no case is gold-standard
                # positive, and an unguarded `sens >= 0.95` throws "missing value where
                # TRUE/FALSE needed", which the caller's tryCatch would swallow into a
                # bare fallback panel.
                benchmarks$sens_quality <- if (is.na(sens)) .("not estimable: no gold-standard positive cases")
                                           else if (sens >= 0.95) .("5% or fewer diseased cases missed in this sample")
                                           else if (sens >= 0.90) .("up to 1 diseased case in 10 missed in this sample")
                                           else if (sens >= 0.80) .("up to 1 diseased case in 5 missed in this sample")
                                           else .("more than 1 diseased case in 5 missed in this sample")

                # Specificity describes the false-positive side: how many gold-standard
                # negative cases this test called positive. Same NA guard, for a cohort
                # with no gold-standard negative cases.
                benchmarks$spec_quality <- if (is.na(spec)) .("not estimable: no gold-standard negative cases")
                                           else if (spec >= 0.95) .("5% or fewer disease-free cases flagged positive in this sample")
                                           else if (spec >= 0.90) .("up to 1 disease-free case in 10 flagged positive in this sample")
                                           else if (spec >= 0.80) .("up to 1 disease-free case in 5 flagged positive in this sample")
                                           else .("more than 1 disease-free case in 5 flagged positive in this sample")

                # Likelihood ratio benchmarks
                benchmarks$lr_pos_interpretation <- if (!is.finite(lr_pos)) .("Cannot be calculated: LR+ is sensitivity / (1 - specificity), and one of those is not estimable here")
                                                    else if (lr_pos > 10) .("Strong evidence for disease")
                                                    else if (lr_pos > 5) .("Moderate evidence for disease")
                                                    else if (lr_pos > 2) .("Weak evidence for disease")
                                                    else if (lr_pos > 1) .("Minimal evidence for disease")
                                                    else if (lr_pos == 1) .("Uninformative: a positive result does not change the probability of disease")
                                                    else .("Points AGAINST disease when positive: the test-positive level may be inverted")

                benchmarks$lr_neg_interpretation <- if (!is.finite(lr_neg)) .("Cannot be calculated: LR- is (1 - sensitivity) / specificity, and one of those is not estimable here")
                                                    else if (lr_neg < 0.1) .("Strong evidence against disease")
                                                    else if (lr_neg < 0.2) .("Moderate evidence against disease")
                                                    else if (lr_neg < 0.5) .("Weak evidence against disease")
                                                    else if (lr_neg < 1) .("Minimal evidence against disease")
                                                    else if (lr_neg == 1) .("Uninformative: a negative result does not change the probability of disease")
                                                    else .("Points TOWARD disease when negative: the test-positive level may be inverted")

                return(benchmarks)
            },

            # Centralized footnote management with clinical interpretation
            .addFootnotes = function() {
                if (!self$options$fnote) return()

                # nTable footnotes
                nTable <- self$results$nTable
                footnotes_n <- list(
                    TotalPop = .("Total Number of Subjects in complete case analysis"),
                    DiseaseP = .("Total Number of Subjects with Disease (Gold Standard Positive)"),
                    DiseaseN = .("Total Number of Healthy Subjects (Gold Standard Negative)"),
                    TestP = .("Total Number of Positive Test Results"),
                    TestN = .("Total Number of Negative Test Results"),
                    TestT = .("Total Number of True Test Results (TP + TN)"),
                    TestW = .("Total Number of Wrong Test Results (FP + FN)")
                )

                # Vectorized footnote application for better performance
                if (length(footnotes_n) > 0) {
                    mapply(function(col, note) nTable$addFootnote(rowNo = 1, col = col, note),
                           names(footnotes_n), footnotes_n)
                }

                # ratioTable footnotes with clinical interpretation
                ratioTable <- self$results$ratioTable
                footnotes_ratio <- list(
                    Sens = .("Sensitivity: Proportion of diseased patients correctly identified (TP rate). Higher is better for ruling OUT disease when negative."),
                    Spec = .("Specificity: Proportion of healthy patients correctly identified (TN rate). Higher is better for ruling IN disease when positive."),
                    PrevalenceD = if (isTRUE(self$options$pp)) {
                        # This previously claimed the predictive values were computed
                        # from the prior while the code left them at study prevalence.
                        # They now are, so the statement is true - but say plainly
                        # that this cell is not the sample's own prevalence.
                        .("Prevalence: the user-supplied population prevalence (Known population prevalence, under Population Prevalence Settings), NOT this sample's observed prevalence. Predictive values, which are the post-test probabilities, are computed from this prior; sensitivity and specificity are not affected by it.")
                    } else {
                        .("Disease Prevalence: Observed proportion with disease in this sample. Affects predictive values.")
                    },
                    PPV = .("Positive Predictive Value: Probability of disease given a positive test. This IS the post-test probability of disease. Depends on prevalence, sensitivity and specificity."),
                    NPV = .("Negative Predictive Value: Probability of being healthy given a negative test. This IS the post-test probability of health. Depends on prevalence, sensitivity and specificity."),
                    LRP = .("Positive Likelihood Ratio: How much more likely a positive result is in diseased vs healthy patients. >10 = strong evidence, >5 = moderate, >2 = weak but potentially useful."),
                    LRN = .("Negative Likelihood Ratio: How much more likely a negative result is in diseased vs healthy patients. <0.1 = strong evidence against disease, <0.2 = moderate, <0.5 = weak.")
                )

                # Vectorized footnote application for better performance
                if (length(footnotes_ratio) > 0) {
                    mapply(function(col, note) ratioTable$addFootnote(rowNo = 1, col = col, note),
                           names(footnotes_ratio), footnotes_ratio)
                }
            },

            # Consolidated content generation for improved performance
            .generateAllContent = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                         prior_prob, total_pop, test_name, gold_name,
                                         sens_ci = NULL, spec_ci = NULL) {

                results <- list(
                    clinical_summary = "",
                    about_content = "",
                    natural_summary = "",
                    report_template = ""
                )

                format_percent <- function(value, default = .("not available")) {
                    if (is.na(value)) return(default)
                    sprintf("%.1f%%", value * 100)
                }

                # Generate clinical interpretation
                results$clinical_summary <- tryCatch({
                    interpretation <- private$.getDiagnosticInterpretation(lr_pos, lr_neg, sens, spec)

                    sens_text <- format_percent(sens)
                    spec_text <- format_percent(spec)
                    youden_text <- if (is.na(interpretation$youden_index)) .("not available") else sprintf("%.3f", interpretation$youden_index)
                    lr_pos_text <- ifelse(is.na(lr_pos), .("undefined"), sprintf("%.2f", lr_pos))
                    lr_neg_text <- ifelse(is.na(lr_neg), .("undefined"), sprintf("%.2f", lr_neg))

                    paste0(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #2196F3; background-color: rgba(138, 155, 172, 0.06); color: inherit;'>",
                        "<h4 style='color: #1976D2; margin-top: 0;'>", .("Clinical Interpretation"), "</h4>",
                        "<p><strong>", .("Test Performance Summary"), ":</strong></p>",
                        "<ul>",
                        "<li><strong>", .("Sensitivity"), ":</strong> ", sens_text, " - ",
                        if (is.na(sens)) .("not estimable: no gold-standard positive cases") else if (sens >= 0.9) .("up to 1 diseased case in 10 missed here") else if (sens >= 0.8) .("up to 1 diseased case in 5 missed here") else .("more than 1 diseased case in 5 missed here"),
                        "</li>",
                        "<li><strong>", .("Specificity"), ":</strong> ", spec_text, " - ",
                        if (is.na(spec)) .("not estimable: no gold-standard negative cases") else if (spec >= 0.9) .("up to 1 disease-free case in 10 flagged positive here") else if (spec >= 0.8) .("up to 1 disease-free case in 5 flagged positive here") else .("more than 1 disease-free case in 5 flagged positive here"),
                        "</li>",
                        "<li><strong>", .("Youden's Index"), ":</strong> ", youden_text, " - ", interpretation$test_utility, "</li>",
                        "</ul>",
                        "<p><strong>", .("Likelihood Ratio Interpretation"), ":</strong></p>",
                        "<ul>",
                        # paste0, not jmvcore::format: lr_pos_text/lr_neg_text can be
                        # .("undefined"), and feeding a TRANSLATED value into a {placeholder}
                        # can hang jmvcore::format forever (it re-scans after substituting).
                        "<li><strong>", .("Positive LR"), " (", lr_pos_text, "):</strong> ", interpretation$lr_pos_interp, "</li>",
                        "<li><strong>", .("Negative LR"), " (", lr_neg_text, "):</strong> ", interpretation$lr_neg_interp, "</li>",
                        "</ul>",
                        "<p><strong>", .("Clinical Decision Making"), ":</strong></p>",
                        "<ul>",
                        "<li>", .("Pre-test probability of disease"), ": <strong>", format_percent(prior_prob, .("not provided")), "</strong></li>",
                        "<li>", .("Post-test probability of disease, test positive"), ": <strong>", format_percent(ppv, .("not available")), "</strong> ", .("(this is the PPV)"), "</li>",
                        "<li>", .("Post-test probability of disease, test negative"), ": <strong>", format_percent(1 - npv, .("not available")), "</strong> ", .("(this is 1 - NPV)"), "</li>",
                        "</ul></div>"
                    )
                }, error = function(e) {
                    paste0(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #ff9800; background-color: rgba(255, 169, 33, 0.14); color: inherit;'>",
                        "<h4 style='color: #f57c00; margin-top: 0;'>", .("Clinical Interpretation"), "</h4>",
                        "<p>", .("Unable to generate detailed clinical interpretation due to data limitations."), "</p>",
                        "<p><strong>", .("Basic Results"), ":</strong> ",
                        # paste0, not jmvcore::format: both values fall back to
                        # .("not available"), a translated string -- see the note above.
                        .("Sensitivity"), ": ", format_percent(sens, .("not available")), ", ",
                        .("Specificity"), ": ", format_percent(spec, .("not available")),
                        "</p>",
                        "</div>"
                    )
                })

                # Generate about content
                results$about_content <- tryCatch({
                    private$.generateAboutAnalysis()
                }, error = function(e) {
                    paste0("<div>", .("About analysis content unavailable"), "</div>")
                })

                # Generate natural language summary
                results$natural_summary <- tryCatch({
                    private$.generateNaturalLanguageSummary(
                        sens, spec, ppv, npv, lr_pos, lr_neg,
                        prior_prob, total_pop, test_name, gold_name
                    )
                }, error = function(e) {
                    fallback_template <- .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #FF9800; background-color: rgba(255, 169, 33, 0.14); color: inherit;'><h3 style='color: #F57C00; margin-top: 0;'>Clinical Summary</h3><p>Basic diagnostic test evaluation completed with {n} cases.</p><p><strong>Results:</strong> Sensitivity {sens}, Specificity {spec}</p></div>")

                    .fmt(
                        fallback_template,
                        n = sprintf("%d", total_pop),
                        sens = sprintf("%.1f%%", sens * 100),
                        spec = sprintf("%.1f%%", spec * 100)
                    )
                })

                # Generate report template
                results$report_template <- tryCatch({
                    private$.generateReportTemplate(
                        sens, spec, ppv, npv, lr_pos, lr_neg,
                        sens_ci = sens_ci, spec_ci = spec_ci,
                        test_name = test_name,
                        gold_name = gold_name,
                        prevalence = prior_prob
                    )
                }, error = function(e) {
                    fallback_template <- .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background-color: rgba(33, 152, 239, 0.13); color: inherit;'><h3 style='color: #1976D2; margin-top: 0;'>Copy-Ready Clinical Report</h3><p>Diagnostic test evaluation shows sensitivity of {sens} and specificity of {spec}.</p></div>")

                    .fmt(
                        fallback_template,
                        sens = sprintf("%.1f%%", sens * 100),
                        spec = sprintf("%.1f%%", spec * 100)
                    )
                })

                return(results)
            }




            ,
            .run = function() {
                # Reset accumulated notices at the start of every run cycle.
                # The R6 instance is reused across runs, so the `.noticeList = list()`
                # field default (evaluated once at instantiation) is not enough: without
                # this reset each .addNotice() message re-appends and renders N times over
                # N runs (same pattern fixed in survival.b.R).
                private$.noticeList <- list()

                # Render whatever was collected however this method exits. Five
                # early return()s sit between here and the end of .run(), and each
                # one used to discard every notice the validators had raised -- a
                # failed run showed empty tables and no explanation at all. on.exit
                # also fires while an error unwinds, so the ERROR notices added
                # inside the confusion-matrix handler reach the user too.
                on.exit(private$.renderNotices(), add = TRUE)

                # Early return if variables not selected
                if (length(self$options$testPositive) + length(self$options$newtest) +
                    length(self$options$goldPositive) + length(self$options$gold) < 4)
                    return()

                # Consolidated input validation
                if (!private$.validateCategoricalInputs()) {
                    return()
                }

                # Efficient data preparation with missing data analysis
                original_data <- self$data
                prepared_data <- private$.prepareAnalysisData()

                # Check if data preparation failed
                if (is.null(prepared_data)) {
                    return()
                }

                mydata <- prepared_data$data
                testVariable <- prepared_data$testVar
                goldVariable <- prepared_data$goldVar
                
                # Check data size for performance warnings
                private$.checkDataSize(mydata)

                # Enhanced missing data reporting
                # missingDataSummary is visible: (od), so it is written once, under the
                # `if (od)` guard further down. Writing it here as well only filled a
                # hidden element -- and the WARNING notice raised in .prepareAnalysisData()
                # is what actually tells the user cases were dropped.
                missing_analysis <- private$.analyzeMissingData(original_data, mydata)

                # Table 1 ----

                results1 <- mydata %>%
                    dplyr::select(dplyr::all_of(c(testVariable, goldVariable))) %>%
                    table()

                # self$results$text1$setContent(results1)

                # Prepare raw combination counts for both HTML and jamovi tables
                combination_counts <- mydata %>%
                    dplyr::count(.data[[testVariable]], .data[[goldVariable]]) %>%
                    dplyr::ungroup()

                # result2 <- combination_counts %>%
                #     htmlTable::htmlTable()

                # self$results$text2$setContent(result2)

                # Populate raw contingency jamovi table (using user's selected levels, not lexicographic order)
                raw_contingency <- self$results$rawContingency
                # Clear existing rows - jamovi tables use deleteRows(), not clear()
                try(raw_contingency$deleteRows(), silent = TRUE)

                # Get actual levels from the ORIGINAL variables (before recoding)
                test_levels <- if (is.factor(mydata[[testVariable]])) {
                    levels(mydata[[testVariable]])
                } else {
                    sort(unique(as.character(mydata[[testVariable]])))
                }

                gold_levels <- if (is.factor(mydata[[goldVariable]])) {
                    levels(mydata[[goldVariable]])
                } else {
                    sort(unique(as.character(mydata[[goldVariable]])))
                }

                # Determine gold negative level (explicit or infer)
                has_gold_negative <- length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0
                # MEMBERSHIP vs LABEL are two different things and must stay separate.
                # With no explicit absent level the analysis pools every non-positive level
                # (.prepareAnalysisData), so the display column must SUM them; naming one
                # arbitrary level dropped the rest while the totals still counted them.
                # Exactly one negative level takes part (explicit, or inferred when the
                # variable is dichotomous); .prepareAnalysisData has already errored out if
                # it was ambiguous, so no pooling can reach here.
                gold_negative_members <- if (has_gold_negative) self$options$goldNegative
                                         else setdiff(gold_levels, self$options$goldPositive)[1]
                gold_negative_label <- gold_negative_members[1]

                # Determine test negative level (explicit or infer)
                has_test_negative <- length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0
                test_negative_members <- if (has_test_negative) self$options$testNegative
                                         else setdiff(test_levels, self$options$testPositive)[1]
                test_negative_label <- test_negative_members[1]

                results_matrix <- as.matrix(results1)

                # Set column headers using USER'S selections, not lexicographic order
                if (!is.null(raw_contingency$getColumn("test_level"))) {
                    raw_contingency$getColumn("test_level")$setTitle(testVariable)
                    raw_contingency$getColumn("test_level")$setSuperTitle("")
                }
                if (!is.null(raw_contingency$getColumn("gold_pos"))) {
                    # Use user's goldPositive selection
                    raw_contingency$getColumn("gold_pos")$setTitle(self$options$goldPositive)
                    raw_contingency$getColumn("gold_pos")$setSuperTitle(goldVariable)
                }
                if (!is.null(raw_contingency$getColumn("gold_neg"))) {
                    # Use user's goldNegative selection (or inferred)
                    raw_contingency$getColumn("gold_neg")$setTitle(gold_negative_label)
                    raw_contingency$getColumn("gold_neg")$setSuperTitle(goldVariable)
                }
                if (!is.null(raw_contingency$getColumn("row_total"))) {
                    raw_contingency$getColumn("row_total")$setTitle(.("Total"))
                    raw_contingency$getColumn("row_total")$setSuperTitle("")
                }

                row_names <- rownames(results_matrix)
                col_names <- colnames(results_matrix)

                # Populate rows in order: positive test first, then negative test
                # One positive row, one pooled negative row. Iterating a synthetic label
                # would skip the negative row entirely (it is not a real level name).
                ordered_test_groups <- list(
                    list(label = self$options$testPositive, members = self$options$testPositive),
                    list(label = test_negative_label,       members = test_negative_members))

                if (!is.null(test_levels) && length(test_levels) > 0 &&
                    !is.null(gold_levels) && length(gold_levels) > 0) {

                    for (grp in ordered_test_groups) {
                        lvl <- grp$label
                        present <- intersect(grp$members, test_levels)
                        if (length(present) == 0) next

                        row_vector <- if (!is.null(row_names)) {
                            rows_in <- intersect(present, row_names)
                            if (length(rows_in) == 0) matrix(0, nrow = 1, ncol = length(col_names))
                            else matrix(colSums(results_matrix[rows_in, , drop = FALSE]), nrow = 1)
                        } else {
                            matrix(0, nrow = 1, ncol = length(col_names))
                        }

                        row_values <- as.numeric(row_vector)
                        if (is.null(col_names) && length(row_values) == length(gold_levels)) {
                            names(row_values) <- gold_levels
                        } else if (!is.null(col_names)) {
                            names(row_values) <- col_names
                        }

                        # Use user's selected positive/negative levels
                        val_pos <- if (self$options$goldPositive %in% names(row_values)) {
                            row_values[[self$options$goldPositive]]
                        } else {
                            NA_real_
                        }

                        neg_in <- intersect(gold_negative_members, names(row_values))
                        val_neg <- if (length(neg_in) > 0) sum(row_values[neg_in], na.rm = TRUE) else NA_real_

                        row_total <- sum(row_values, na.rm = TRUE)

                        raw_contingency$addRow(
                            rowKey = paste0("row_", lvl),
                            values = list(
                                test_level = lvl,
                                gold_pos = val_pos,
                                gold_neg = val_neg,
                                row_total = row_total
                            )
                        )
                    }

                    col_totals <- if (!is.null(col_names) && length(col_names) > 0) {
                        colSums(results_matrix)
                    } else {
                        rep(sum(results_matrix), length(gold_levels))
                    }
                    if (is.null(names(col_totals)) && length(gold_levels) == length(col_totals)) {
                        names(col_totals) <- gold_levels
                    }

                    # Use user's selected levels for totals
                    total_pos <- if (self$options$goldPositive %in% names(col_totals)) {
                        col_totals[[self$options$goldPositive]]
                    } else {
                        NA_real_
                    }

                    neg_tot_in <- intersect(gold_negative_members, names(col_totals))
                    total_neg <- if (length(neg_tot_in) > 0) {
                        sum(col_totals[neg_tot_in], na.rm = TRUE)
                    } else {
                        NA_real_
                    }

                    raw_contingency$addRow(
                        rowKey = "row_total",
                        values = list(
                            test_level = .("Total"),
                            gold_pos = total_pos,
                            gold_neg = total_neg,
                            row_total = sum(results_matrix)
                        )
                    )
                }

                # Populate raw combination count jamovi table
                raw_counts_table <- self$results$rawCounts
                # Clear existing rows - jamovi tables use deleteRows(), not clear()
                try(raw_counts_table$deleteRows(), silent = TRUE)

                if (!is.null(raw_counts_table$getColumn("test_level"))) {
                    raw_counts_table$getColumn("test_level")$setTitle(testVariable)
                    raw_counts_table$getColumn("test_level")$setSuperTitle("")
                }
                if (!is.null(raw_counts_table$getColumn("gold_level"))) {
                    raw_counts_table$getColumn("gold_level")$setTitle(goldVariable)
                    raw_counts_table$getColumn("gold_level")$setSuperTitle("")
                }

                combo_for_table <- combination_counts %>%
                    dplyr::mutate(
                        test_level = as.character(.data[[testVariable]]),
                        gold_level = as.character(.data[[goldVariable]]),
                        count = as.integer(.data$n)
                    ) %>%
                    dplyr::select(test_level, gold_level, count) %>%
                    dplyr::arrange(test_level, gold_level)

                if (nrow(combo_for_table) > 0) {
                    # addRow is O(n^2) in jmvcore; a high-cardinality pair can produce
                    # thousands of combinations and lock the UI. Cap and disclose.
                    combo_cap <- min(nrow(combo_for_table), 200L)
                    if (nrow(combo_for_table) > combo_cap)
                        raw_counts_table$setNote("truncated", .fmt(
                            .("Showing the first {shown} of {total} level combinations."),
                            shown = sprintf("%d", combo_cap), total = sprintf("%d", nrow(combo_for_table))))
                    for (i in seq_len(combo_cap)) {
                        private$.checkpoint()
                        raw_counts_table$addRow(
                            rowKey = paste0("row_", i),
                            values = list(
                                test_level = combo_for_table$test_level[i],
                                gold_level = combo_for_table$gold_level[i],
                                count = combo_for_table$count[i]
                            )
                        )
                    }
                }


                # Populate missing data summary if requested
                if (self$options$od) {
                    self$results$missingDataSummary$setContent(missing_analysis)
                }










                # conf_table ----
                # Data is already efficiently recoded in .prepareAnalysisData()
                conf_table <- table(mydata[["testVariable2"]], mydata[["goldVariable2"]])

                # Validate sample size and provide clinical guidance
                private$.validateSampleSize(conf_table)

                # Apply Haldane-Anscombe correction for zero cells to stabilize LR/OR
                conf_table_cc <- conf_table
                continuity_used <- FALSE
                if (any(conf_table == 0)) {
                    conf_table_cc <- conf_table + 0.5
                    continuity_used <- TRUE
                }


                # Extract confusion matrix values with error handling
                extraction_result <- tryCatch({
                    list(
                        TP = conf_table[1, 1],
                        FP = conf_table[1, 2],
                        FN = conf_table[2, 1],
                        TN = conf_table[2, 2]
                        ,
                        TPc = conf_table_cc[1, 1],
                        FPc = conf_table_cc[1, 2],
                        FNc = conf_table_cc[2, 1],
                        TNc = conf_table_cc[2, 2]
                    )
                }, error = function(e) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.("Error extracting confusion matrix values: {msg}"), msg = e$message),
                        content = .("Check your data formatting. Ensure both variables have exactly 2 levels. Verify positive/negative levels are correctly specified.")
                    )
                    return(NULL)
                })

                if (is.null(extraction_result)) {
                    return()
                }

                TP <- extraction_result$TP
                FP <- extraction_result$FP
                FN <- extraction_result$FN
                TN <- extraction_result$TN
                TPc <- extraction_result$TPc
                FPc <- extraction_result$FPc
                FNc <- extraction_result$FNc
                TNc <- extraction_result$TNc

                # Validate extracted values
                if (any(is.na(c(TP, FP, FN, TN))) || any(c(TP, FP, FN, TN) < 0)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Invalid contingency table values detected"),
                        content = .("Confusion matrix contains NA or negative values. Check that your data is properly formatted. Ensure sufficient observations in all categories.")
                    )
                    return()
                }




                # Cross Table in jamovi style ----

                cTable <- self$results$cTable

                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = .("Test Positive"),
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )


                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = .("Test Negative"),
                        GP = FN,
                        GN = TN,
                        Total = FN + TN
                    )
                )

                cTable$setRow(
                    rowKey = "Total",
                    values = list(
                        newtest = .("Total"),
                        GP = TP + FN,
                        GN = FP + TN,
                        Total = TP + FP + FN + TN
                    )
                )





                # Self Calculations ----

                # Self Calculation https://cran.r-project.org/web/packages/caret/caret.pdf
                # https://online.stat.psu.edu/stat509/node/150/

                # https://en.wikipedia.org/wiki/Sensitivity_and_specificity

                TotalPop <- TP + TN + FP + FN

                DiseaseP <- TP + FN

                DiseaseN <- TN + FP

                TestP <- TP + FP

                TestN <- TN + FN

                TestT <- TP + TN

                TestW <- FP + FN

                # Calculate diagnostic metrics with proper statistical handling
                # Sensitivity = TP / (TP + FN) = True Positive Rate
                Sens <- if (DiseaseP > 0) {
                    TP / DiseaseP
                } else {
                    NA  # No disease cases
                }

                # Specificity = TN / (TN + FP) = True Negative Rate
                Spec <- if (DiseaseN > 0) {
                    TN / DiseaseN
                } else {
                    NA  # No healthy cases
                }

                # Accuracy = (TP + TN) / Total
                AccurT <- if (TotalPop > 0) {
                    TestT / TotalPop
                } else {
                    NA
                }

                # Prevalence = Disease cases / Total
                PrevalenceD <- if (TotalPop > 0) {
                    DiseaseP / TotalPop
                } else {
                    NA
                }

                # Positive Predictive Value = TP / (TP + FP)
                PPV <- if (TestP > 0) {
                    TP / TestP
                } else {
                    NA  # No positive tests
                }

                # Negative Predictive Value = TN / (TN + FN)
                NPV <- if (TestN > 0) {
                    TN / TestN
                } else {
                    NA  # No negative tests
                }

                pp <- self$options$pp
                pprob <- self$options$pprob

                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }


                # Post-test probability calculations using Bayes' theorem.
                #
                # These MUST use the same proportions as the likelihood ratios and the
                # Fagan nomogram. With a zero cell the LRs and the nomogram switch to the
                # Haldane-Anscombe corrected table while these used the raw one, so the
                # ratio table reported PPV 100.0% at a 10% prior beside a nomogram
                # reporting 82.0% -- the same quantity, two numbers, one screen.
                sens_bayes <- if (isTRUE(continuity_used)) TPc / (TPc + FNc) else Sens
                spec_bayes <- if (isTRUE(continuity_used)) TNc / (TNc + FPc) else Spec

                # PPV when using population prevalence
                PostTestProbDisease <- if (TestP > 0) {
                    (PriorProb * sens_bayes) / ((PriorProb * sens_bayes) + ((1 - PriorProb) * (1 - spec_bayes)))
                } else {
                    NA
                }

                # NPV when using population prevalence (1 - probability of disease given negative test)
                PostTestProbHealthy <- if (TestN > 0) {
                    ((1 - PriorProb) * spec_bayes) / (((1 - PriorProb) * spec_bayes) + (PriorProb * (1 - sens_bayes)))
                } else {
                    NA
                }




                # Calculate likelihood ratios with proper statistical handling (use continuity-corrected counts when needed)
                if (is.na(Sens) || is.na(Spec)) {
                    LRP <- NA
                    LRN <- NA
                } else {
                    sens_cc <- TPc / (TPc + FNc)
                    spec_cc <- TNc / (TNc + FPc)

                    # LR+ = Sensitivity / (1 - Specificity)
                    LRP <- if (spec_cc < 1) {
                        sens_cc / (1 - spec_cc)
                    } else if (sens_cc == 1 && spec_cc == 1) {
                        NA  # Perfect test - undefined
                    } else {
                        Inf
                    }

                    # LR- = (1 - Sensitivity) / Specificity
                    LRN <- if (spec_cc > 0) {
                        (1 - sens_cc) / spec_cc
                    } else if (sens_cc == 1 && spec_cc == 1) {
                        NA
                    } else {
                        Inf
                    }
                }

                # Enhanced likelihood ratio validation with recovery
                lr_validation <- private$.validateLikelihoodRatios(LRP, LRN, Sens, Spec)
                LRP <- lr_validation$lrp
                LRN <- lr_validation$lrn
                if (length(lr_validation$issues) > 0) {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Likelihood ratio adjustments applied"),
                        # paste0: the issues are themselves translated strings, so a
                        # {placeholder} could hang jmvcore::format, and a leading %s makes
                        # an unorderable msgid in most target languages.
                        content = paste0(paste(lr_validation$issues, collapse = "; "), ". ",
                                         .("Results have been adjusted for statistical validity."))
                    )
                }

                if (continuity_used) {
                    private$.addNotice(
                        type = "INFO",
                        title = .("Continuity correction applied"),
                        content = .("Zero cells detected; applied Haldane-Anscombe 0.5 continuity correction for LR/OR calculations (sensitivity/specificity still use observed counts).")
                    )
                }






                # nTable Populate Table ----

                nTable <- self$results$nTable
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
                        TotalPop = TotalPop,
                        DiseaseP = DiseaseP,
                        DiseaseN = DiseaseN,
                        TestP = TestP,
                        TestN = TestN,
                        TestT = TestT,
                        TestW = TestW
                    )
                )







                # ratioTable Populate Table ----


                ratioTable <- self$results$ratioTable
                # PPV/NPV must belong to the prevalence the table reports.
                #
                # PPV *is* the post-test probability of disease given a positive
                # test. With pp=TRUE the Prevalence cell was switched to the user's
                # prior while PPV/NPV stayed at the raw 2x2 values computed at the
                # STUDY prevalence, so the row read "Prevalence 5.0%" beside
                # "PPV 88.9%" - arithmetically impossible for a test with sens 0.80
                # / spec 0.90, where Bayes gives 29.6%. The footnote made it worse
                # by asserting the predictive values had been prior-adjusted. Report
                # the values that actually correspond to the stated prevalence, and
                # keep the observed study prevalence visible either way.
                PPV_report <- PPV
                NPV_report <- NPV
                if (isTRUE(pp)) {
                    if (is.finite(PostTestProbDisease)) PPV_report <- PostTestProbDisease
                    if (is.finite(PostTestProbHealthy)) NPV_report <- PostTestProbHealthy
                    ratioTable$setNote("prior_ppv", .fmt(
                        .("Predictive values are computed by Bayes' theorem at the population prior of {prior} that you supplied, NOT at this sample's observed prevalence of {observed}. Sensitivity and specificity are unaffected by prevalence; PPV and NPV are not."),
                        prior = sprintf("%.1f%%", 100 * PriorProb),
                        observed = sprintf("%.1f%%", 100 * PrevalenceD)))
                } else {
                    ratioTable$setNote("study_ppv", .fmt(
                        .("Predictive values are computed at this sample's observed prevalence of {prevalence}. If the sample was enriched or case-control, that prevalence is not the clinical one and these predictive values do not transfer - tick Known population prevalence under Population Prevalence Settings to obtain values for your setting."),
                        prevalence = sprintf("%.1f%%", 100 * PrevalenceD)))
                }

                # Sample accuracy stays on the observed 2x2 even when a population
                # prior is supplied -- it is a property of THIS sample's case mix, not a
                # quantity that can be standardised to another prevalence without also
                # restating what "accuracy" means. decisioncalculator and decisioncompare
                # already settled this the same way (jamovi/decisioncalculator.r.yaml:145,
                # R/decisioncompare.b.R:2284), and recomputing it here would make the two
                # analyses print different accuracies for the same 2x2 and the same prior.
                # setNote, not addFootnote: .addFootnotes() returns early unless the user
                # ticks Explanatory footnotes, and a value this easy to misread cannot have
                # its only explanation behind an off-by-default switch.
                ratioTable$setNote("sample_accuracy", .fmt(
                    if (isTRUE(pp))
                        .("Sample Accuracy is the proportion of concordant results at the prevalence observed in this sample ({prevalence}), NOT at the population prior you supplied. Unlike the predictive values it is not standardised to that prior, and it changes with case mix.")
                    else
                        .("Sample Accuracy is the proportion of concordant results at the prevalence observed in this sample ({prevalence}), and it changes with case mix."),
                    prevalence = sprintf("%.1f%%", PrevalenceD * 100)))

                ratioTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
                        Sens = Sens,
                        Spec = Spec,
                        AccurT = AccurT,
                        PrevalenceD = PriorProb,
                        PPV = PPV_report,
                        NPV = NPV_report,
                        LRP = LRP,
                        LRN = LRN
                    )
                )

                # Consolidated content generation with enhanced error handling.
                # Pass RAW variable names here: the content generators
                # (.generateNaturalLanguageSummary / .generateReportTemplate) escape them
                # internally via .safeHtmlOutput. Escaping here too would double-encode names
                # containing &, <, >, or quotes (e.g. `A&B` -> `A&amp;B`).
                test_label <- if (length(self$options$newtest) > 0) {
                    paste(self$options$newtest, collapse = ", ")
                } else {
                    "Test"
                }

                gold_label <- if (length(self$options$gold) > 0) {
                    paste(self$options$gold, collapse = ", ")
                } else {
                    "Reference"
                }

                # Generate content only if requested by user
                content_results <- NULL
                if (self$options$showNaturalLanguage || self$options$showClinicalInterpretation ||
                    self$options$showReportTemplate || self$options$showAboutAnalysis) {
                    # PPV_report/NPV_report, not PPV/NPV: with pp=TRUE the narrative
                    # must quote the predictive values at the prior it also prints.
                    # A copy-ready sentence without an interval invites a clinician to
                    # quote a point estimate as if it were exact. Clopper-Pearson, to
                    # match epiR::epi.tests()'s default.
                    exact_ci <- function(x, n) {
                        if (!is.finite(x) || !is.finite(n) || n <= 0 || x < 0 || x > n) return(NULL)
                        tryCatch(as.numeric(stats::binom.test(round(x), round(n))$conf.int),
                                 error = function(e) NULL)
                    }
                    sens_ci <- exact_ci(TP, TP + FN)
                    spec_ci <- exact_ci(TN, TN + FP)

                    content_results <- private$.generateAllContent(Sens, Spec, PPV_report, NPV_report, LRP, LRN,
                                                                  PriorProb, TotalPop, test_label, gold_label,
                                                                  sens_ci = sens_ci, spec_ci = spec_ci)
                }

                # Populate content outputs based on user selections
                if (self$options$showClinicalInterpretation && !is.null(content_results) &&
                    "clinicalInterpretation" %in% names(self$results)) {
                    self$results$clinicalInterpretation$setContent(content_results$clinical_summary)
                }

                if (self$options$showAboutAnalysis && !is.null(content_results) &&
                    "aboutAnalysis" %in% names(self$results)) {
                    self$results$aboutAnalysis$setContent(content_results$about_content)
                }

                if (self$options$showNaturalLanguage && !is.null(content_results) &&
                    "naturalLanguageSummary" %in% names(self$results)) {
                    self$results$naturalLanguageSummary$setContent(content_results$natural_summary)
                }

                if (self$options$showReportTemplate && !is.null(content_results) &&
                    "reportTemplate" %in% names(self$results)) {
                    self$results$reportTemplate$setContent(content_results$report_template)
                }
                
                # Detect misuse. Emits notices directly, so there is nothing to
                # splice into an opt-in HTML panel any more.
                private$.detectMisuse(conf_table, PrevalenceD, TotalPop)

                # Misclassified Cases Analysis and Output
                tryCatch({
                    # Helper to check if output variable is properly specified
                    # isTRUE, not nchar(): an Output option's value is a LOGICAL, and
                    # nchar(FALSE) is 5, so the old guard was satisfied whether or not the
                    # user asked for the column.
                    has_output_var <- isTRUE(self$options$saveClassifications)

                    # Always create classifications if output is requested or analysis is shown
                    if (self$options$showMisclassified || has_output_var) {
                        private$.analyzeMisclassifiedCases(mydata, goldVariable, testVariable)
                    }
                }, error = function(e) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Error in misclassified cases analysis"),
                        content = paste(.("Technical details:"), e$message, .("Please report this issue if it persists."))
                    )
                })

                # Add footnotes using centralized method
                private$.addFootnotes()





                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    # addRow() appends and accepts a duplicate rowKey, and clearWith
                    # cannot see a variable change that leaves pp/pprob untouched, so
                    # without this the CI tables list every statistic twice on re-run.
                    # Cleared before the tryCatch so a failed epiR call leaves the
                    # tables empty rather than showing the previous run's numbers.
                    self$results$epirTable_ratio$deleteRows()
                    self$results$epirTable_number$deleteRows()

                    # epiR confidence intervals with error handling
                    epir_success <- FALSE
                    epirresult_ratio <- NULL
                    epirresult_number <- NULL

                    tryCatch({
                        epirresult <- epiR::epi.tests(dat = conf_table)
                        # LR+/LR-/DOR in the main tables are computed from Haldane-Anscombe
                        # corrected counts when a cell is zero. epi.tests on the raw table
                        # returns Inf with a NaN lower bound, so the CI pane disagreed with
                        # the estimate it sat next to. Sens/spec/PPV/NPV stay on raw counts.
                        epir_detail_cc <- if (continuity_used) {
                            tryCatch(as.data.frame(epiR::epi.tests(dat = conf_table_cc)$detail,
                                                   stringsAsFactors = FALSE),
                                     error = function(e) NULL)
                        } else NULL

                        # Check if epiR returned valid results
                        if (!is.null(epirresult)) {
                            epir_detail <- epirresult$detail

                            if (!is.null(epir_detail) && nrow(epir_detail) > 0) {
                                epir_detail <- as.data.frame(epir_detail, stringsAsFactors = FALSE)

                                stat_map <- c(
                                    se = .("Sensitivity"),
                                    sp = .("Specificity"),
                                    `pv.pos` = .("Positive predictive value"),
                                    `pv.neg` = .("Negative predictive value"),
                                    `lr.pos` = .("Positive likelihood ratio"),
                                    `lr.neg` = .("Negative likelihood ratio"),
                                    `diag.or` = .("Diagnostic odds ratio"),
                                    youden = .("Youden's index"),
                                    nndx = .("Number needed to diagnose")
                                )

                                selected_stats <- names(stat_map)
                                epir_detail <- epir_detail[epir_detail$statistic %in% selected_stats, , drop = FALSE]

                                if (nrow(epir_detail) > 0) {
                                    order_index <- match(epir_detail$statistic, selected_stats)
                                    epir_detail <- epir_detail[order(order_index), , drop = FALSE]
                                    epir_detail$statsnames <- unname(stat_map[match(epir_detail$statistic, selected_stats)])

                                    ratio_stats <- c("se", "sp", "pv.pos", "pv.neg")
                                    number_stats <- c("diag.or", "nndx", "youden", "lr.pos", "lr.neg")

                                    epir_ratio <- epir_detail[epir_detail$statistic %in% ratio_stats, , drop = FALSE]
                                    epir_number <- epir_detail[epir_detail$statistic %in% number_stats, , drop = FALSE]

                                    # Only the ratio-of-ratios statistics blow up on a zero
                                    # cell (Inf estimate, NaN bound). Youden and NNDx stay
                                    # finite on the raw table, and splicing corrected values
                                    # into them made this pane contradict both the Se/Sp in
                                    # the table above it and the Youden in the Clinical
                                    # Interpretation panel (Se+Sp-1).
                                    cc_stats <- c("lr.pos", "lr.neg", "diag.or")
                                    if (!is.null(epir_detail_cc) && nrow(epir_number) > 0) {
                                        m <- match(epir_number$statistic, epir_detail_cc$statistic)
                                        keep <- !is.na(m) & epir_number$statistic %in% cc_stats
                                        if (any(keep))
                                            epir_number[keep, c("est", "lower", "upper")] <-
                                                epir_detail_cc[m[keep], c("est", "lower", "upper")]
                                    }

                                    # Footnotes below are attached per statistic, so the row order
                                    # has to travel with the data.
                                    epirresult_number_stats <- epir_number$statistic

                                    epir_ratio <- epir_ratio[, c("statsnames", "est", "lower", "upper"), drop = FALSE]
                                    epir_number <- epir_number[, c("statsnames", "est", "lower", "upper"), drop = FALSE]

                                    epirresult_ratio <- epir_ratio
                                    epirresult_number <- epir_number

                                    epir_success <- nrow(epir_ratio) > 0 || nrow(epir_number) > 0
                                } else {
                                    # epiR package issue - silently skip, user won't see CI tables
                                    # warning("epiR statistical detail did not include expected measures - confidence intervals not available")
                                }
                            } else {
                                # epiR package issue - silently skip
                                # warning("epiR detail is NULL or empty - confidence intervals not available")
                            }
                        } else {
                            # epiR package issue - silently skip
                            # warning("epiR returned NULL results - confidence intervals not available")
                        }

                    }, error = function(e) {
                        # Handle epiR errors gracefully
                        # epiR error - silently skip, CI table won't be populated
                        # warning(paste("Error in epiR confidence interval calculation:", e$message))
                        epir_success <- FALSE
                    })

                    # Only populate tables if we have valid data
                    if (epir_success) {
                        # epirTable_ratio -----
                        epirTable_ratio <- self$results$epirTable_ratio

                        if (!is.null(epirresult_ratio) && nrow(epirresult_ratio) > 0) {
                            data_frame <- epirresult_ratio
                            for (i in seq_along(data_frame[, 1, drop = TRUE])) {
                                epirTable_ratio$addRow(rowKey = i,
                                                       values = c(data_frame[i, ]))
                            }

                            # epirTable_ratio footnotes ----
                            if (self$options$fnote) {
                                add_ratio_note <- function(row_no, col, text) {
                                    if (nrow(data_frame) >= row_no) {
                                        epirTable_ratio$addFootnote(rowNo = row_no, col = col, text)
                                    }
                                }

                                add_ratio_note(1, "statsnames", .("Proportion of diseased patients correctly identified (TP rate). Higher is better for ruling OUT disease when negative."))
                                add_ratio_note(2, "statsnames", .("Proportion of healthy patients correctly identified (TN rate). Higher is better for ruling IN disease when positive."))
                                add_ratio_note(3, "statsnames", .("Probability of disease given a positive test. Depends on prevalence, sensitivity and specificity."))
                                add_ratio_note(4, "statsnames", .("Probability of being healthy given a negative test. Depends on prevalence, sensitivity and specificity."))
                                add_ratio_note(1, "est", .("Confidence intervals for sensitivity, specificity, and predictive values are Clopper-Pearson exact intervals, computed as in epiR::epi.tests() with its default settings (method = \"exact\")."))
                                if (isTRUE(self$options$pp)) {
                                    # The main table reports PPV/NPV at the user's prior; these
                                    # rows are exact binomial quantities from the observed table
                                    # and cannot be moved to a different prevalence.
                                    add_ratio_note(3, "est", .("This predictive value is computed at the observed sample prevalence, so it differs from the prior-adjusted value in the main table above."))
                                    add_ratio_note(4, "est", .("This predictive value is computed at the observed sample prevalence, so it differs from the prior-adjusted value in the main table above."))
                                }
                            }
                        }

                        # epirTable_number -----
                        epirTable_number <- self$results$epirTable_number

                        if (!is.null(epirresult_number) && nrow(epirresult_number) > 0) {
                            data_frame <- epirresult_number
                            for (i in seq_along(data_frame[, 1, drop = TRUE])) {
                                epirTable_number$addRow(rowKey = i,
                                                        values = c(data_frame[i, ]))
                            }

                            if (self$options$fnote) {
                                # These rows are ordered LR+, LR-, DOR, Youden, NNDx. Attaching by
                                # row number described LR+ as the diagnostic odds ratio.
                                number_notes <- c(
                                    `lr.pos` = .("How much more likely a positive result is in a diseased than in a healthy patient. >10 is strong evidence FOR disease, 5-10 moderate, 2-5 weak."),
                                    `lr.neg` = .("How much more likely a negative result is in a diseased than in a healthy patient. <0.1 is strong evidence AGAINST disease, 0.1-0.2 moderate, 0.2-0.5 weak."),
                                    `diag.or` = .("How much more likely will the test make a correct diagnosis than an incorrect diagnosis in patients with the disease."),
                                    `nndx` = .("Number of patients that need to be tested to give one correct positive test."),
                                    `youden` = .("Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1).")
                                )

                                stat_keys <- if (exists("epirresult_number_stats", inherits = FALSE))
                                    epirresult_number_stats else character(0)

                                for (i in seq_len(nrow(data_frame))) {
                                    key <- if (i <= length(stat_keys)) stat_keys[i] else NA_character_
                                    if (!is.na(key) && key %in% names(number_notes))
                                        epirTable_number$addFootnote(rowNo = i, col = "statsnames",
                                                                     number_notes[[key]])
                                }
                            }
                        }
                    }
                }





                # Prepare Fagan Nomogram Data ----
                if (self$options$fagan) {
                    # A zero cell makes Sens or Spec exactly 1, and nomogrammer rejects
                    # the closed bounds outright ("must be between 0 and 1 (exclusive)"),
                    # so the whole nomogram silently failed to render. The LRs handed to
                    # it are already Haldane-Anscombe corrected; pass the proportions
                    # from the same corrected table so the plot is self-consistent
                    # rather than clamping to an arbitrary epsilon.
                    sens_plot <- if (isTRUE(continuity_used)) TPc / (TPc + FNc) else Sens
                    spec_plot <- if (isTRUE(continuity_used)) TNc / (TNc + FPc) else Spec

                    plotData1 <- list(
                        "Prevalence" = PriorProb,
                        "Sens" = sens_plot,
                        "Spec" = spec_plot,
                        "Plr" = LRP,
                        "Nlr" = LRN
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData1)
                }

                # Notices are rendered by the on.exit() handler registered at the
                # top of .run(), so every exit path gets them -- not just this one.

            },

            # Robust plot data validation
            .validatePlotState = function(state) {
                required_fields <- c("Prevalence", "Plr", "Nlr", "Sens", "Spec")

                if (is.null(state)) {
                    return(NULL)
                }

                missing_fields <- setdiff(required_fields, names(state))
                if (length(missing_fields) > 0) {
                    return(NULL)
                }

                # Validate numeric ranges
                numeric_fields <- c("Prevalence", "Sens", "Spec")
                for (field in numeric_fields) {
                    if (!is.numeric(state[[field]]) ||
                        state[[field]] < 0 || state[[field]] > 1) {
                        # Plot state validation - returns NULL to prevent plotting
                        # warning(paste("Invalid", field, "value:", state[[field]]))
                        state[[field]] <- max(0, min(1, as.numeric(state[[field]])))
                    }
                }

                return(state)
            }

            ,
            .plot1 = function(image1, ggtheme, ...) {
                # Validate plot state data structure
                plotData1 <- private$.validatePlotState(image1$state)

                if (is.null(plotData1)) {
                    # Return FALSE to prevent plot rendering
                    return(FALSE)
                }

                nomogram_fn <- get0("nomogrammer", mode = "function", inherits = TRUE)

                if (is.null(nomogram_fn)) {
                    # Probability shift plot as fallback when nomogrammer is unavailable
                    prevalence <- plotData1$Prevalence
                    lr_pos <- plotData1$Plr
                    lr_neg <- plotData1$Nlr
                    sens <- plotData1$Sens
                    spec <- plotData1$Spec

                    safe_prob <- function(val) {
                        if (is.na(val) || !is.finite(val)) return(NA_real_)
                        max(min(val, 0.999), 0.001)
                    }

                    if (is.na(prevalence) || prevalence <= 0 || prevalence >= 1) {
                        # Return FALSE instead of stopping
                        return(FALSE)
                    }

                    pre_odds <- prevalence / (1 - prevalence)
                    post_odds_pos <- pre_odds * lr_pos
                    post_odds_neg <- pre_odds * lr_neg

                    post_prob_pos <- safe_prob(post_odds_pos / (1 + post_odds_pos))
                    post_prob_neg <- safe_prob(post_odds_neg / (1 + post_odds_neg))

                    plot_df <- data.frame(
                        result = factor(c("Positive", "Positive", "Negative", "Negative"),
                                        levels = c("Positive", "Negative")),
                        stage = factor(c(.("Pre-test"), .("Post-test"), .("Pre-test"), .("Post-test")),
                                       levels = c(.("Pre-test"), .("Post-test"))),
                        probability = c(prevalence, post_prob_pos, prevalence, post_prob_neg)
                    )
                    plot_df <- plot_df[!is.na(plot_df$probability), , drop = FALSE]

                    plot_title <- .("Diagnostic Probability Shift")
                    subtitle <- .fmt(
                        .("Pre-test prevalence {prev} | Sensitivity {sens} | Specificity {spec}"),
                        prev = sprintf("%.1f%%", prevalence * 100),
                        sens = sprintf("%.1f%%", sens * 100),
                        spec = sprintf("%.1f%%", spec * 100))

                    plot1 <- ggplot2::ggplot(plot_df, ggplot2::aes(x = stage, y = probability,
                                                                   group = result, color = result)) +
                        ggplot2::geom_line(size = 1.2) +
                        ggplot2::geom_point(size = 3) +
                        ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", x * 100),
                                                    limits = c(0, 1)) +
                        ggplot2::scale_color_manual(values = c("Positive" = "#d32f2f", "Negative" = "#1976d2")) +
                        ggplot2::labs(title = plot_title,
                                      subtitle = subtitle,
                                      y = .("Probability"),
                                      x = "",
                                      color = .("Test result")) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(legend.position = "bottom",
                                       plot.title = ggplot2::element_text(face = "bold"))

                    print(plot1)
                    return(TRUE)
                }

                # Use nomogrammer if available
                plot1 <- nomogram_fn(
                    Prevalence = plotData1$Prevalence,
                    Sens = plotData1$Sens,
                    Spec = plotData1$Spec,
                    # Plr/Nlr are deliberately omitted: nomogrammer ignores them when
                    # Sens/Spec are supplied and warns on every render. It derives the
                    # same ratios from the (continuity-consistent) proportions above.
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = private$NOMOGRAM_LABEL_SIZE,
                    Verbose = TRUE
                )

                print(plot1)
                TRUE

            },

            # Misclassified Cases Analysis ----
            # Inspired by Orange Data Mining's interactive confusion matrix
            # Adapted for static jamovi output

            .analyzeMisclassifiedCases = function(mydata2, gold_var, test_var) {

                # Get levels
                gold_pos <- self$options$goldPositive
                test_pos <- self$options$testPositive

                # Create classification groups for all data rows (not just complete cases)
                # Initialize with NA for all rows in original dataset
                classification_vector <- rep(NA_character_, nrow(self$data))

                # Map complete case indices back to original data positions.
                # Use the purpose-built `original_row_index` column (captured before naOmit
                # in .prepareAnalysisData), NOT rownames(): dplyr/factor operations upstream
                # can reset data-frame row names to 1:n, which would write the saved TP/FP/FN/TN
                # column to the wrong original rows when missing values are interspersed.
                # The FP/FN display tables already use original_row_index for the same reason.
                # Positional index into self$data -- deliberately NOT original_row_index,
                # which now carries absolute spreadsheet row numbers for display. The
                # Output column is written positionally and aligned by setRowNums().
                complete_indices <- if (!is.null(mydata2$original_row_position)) {
                    mydata2$original_row_position
                } else {
                    as.numeric(rownames(mydata2))
                }

                # Create classification groups for complete cases
                mydata2$classification_group <- NA_character_

                # True Positive: Test+ and Disease+
                tp_idx <- mydata2[[test_var]] == test_pos & mydata2[[gold_var]] == gold_pos
                mydata2$classification_group[tp_idx] <- "True Positive"

                # False Positive: Test+ but Disease-
                fp_idx <- mydata2[[test_var]] == test_pos & mydata2[[gold_var]] != gold_pos
                mydata2$classification_group[fp_idx] <- "False Positive"

                # False Negative: Test- but Disease+
                fn_idx <- mydata2[[test_var]] != test_pos & mydata2[[gold_var]] == gold_pos
                mydata2$classification_group[fn_idx] <- "False Negative"

                # True Negative: Test- and Disease-
                tn_idx <- mydata2[[test_var]] != test_pos & mydata2[[gold_var]] != gold_pos
                mydata2$classification_group[tn_idx] <- "True Negative"

                # Map complete case classifications back to original data positions
                if (length(complete_indices) > 0) {
                    classification_vector[complete_indices] <- mydata2$classification_group
                }

                # Save classifications to dataset if output variable is specified
                if (isTRUE(self$options$saveClassifications)) {
                    output <- self$results$saveClassifications
                    if (!is.null(output) && is.function(output$setValues)) {
                        # setRowNums BEFORE setValues. classification_vector is built
                        # positionally over self$data, which under an active jamovi row
                        # filter is a SUBSET of the spreadsheet -- without the row
                        # identities the TP/FP/FN/TN labels are written to the wrong
                        # patients. Same pattern as categorize.b.R:982 and agreement.b.R:8506.
                        output$setRowNums(rownames(self$data))
                        output$setValues(classification_vector)
                    }
                }

                # Only populate tables if user requested to see misclassified cases
                if (!self$options$showMisclassified) {
                    return()
                }

                # Populate the misclassified-cases section heading/intro block.
                # (Declared in decision.r.yaml as `misclassifiedHeading`, visible when
                # showMisclassified; previously never set anywhere in the backend.)
                self$results$misclassifiedHeading$setContent(paste0(
                    "<div style='margin: 15px; padding: 12px; border-left: 4px solid #607D8B; background-color: rgba(33, 68, 92, 0.09); color: inherit;'>",
                    "<h3 style='color: #37474F; margin-top: 0;'>", .("Misclassified Cases Analysis"), "</h3>",
                    "<p>", .("Cases where the diagnostic test disagreed with the gold standard are examined below. False positives (test positive, disease absent) and false negatives (test negative, disease present) are listed with their original row numbers so individual records can be reviewed."), "</p>",
                    "</div>"
                ))

                # Summary counts
                n_total <- nrow(mydata2)
                n_tp <- sum(tp_idx, na.rm = TRUE)
                n_fp <- sum(fp_idx, na.rm = TRUE)
                n_fn <- sum(fn_idx, na.rm = TRUE)
                n_tn <- sum(tn_idx, na.rm = TRUE)

                # Populate confusion matrix summary
                summary_table <- self$results$confusionMatrixSummary
                summary_table$setRow(rowKey = 1, values = list(
                    count = n_tp,
                    percentage = n_tp / n_total
                ))
                summary_table$setRow(rowKey = 2, values = list(
                    count = n_fp,
                    percentage = n_fp / n_total
                ))
                summary_table$setRow(rowKey = 3, values = list(
                    count = n_fn,
                    percentage = n_fn / n_total
                ))
                summary_table$setRow(rowKey = 4, values = list(
                    count = n_tn,
                    percentage = n_tn / n_total
                ))

                # False Positive cases table
                fp_table <- self$results$falsePositiveTable
                try(fp_table$deleteRows(), silent = TRUE)
                if (n_fp > 0) {
                    fp_cases <- mydata2[fp_idx, ]
                    # Use original row indices from the dataset, not filtered indices
                    fp_cases$row_id <- fp_cases$original_row_index

                    max_show <- min(self$options$maxCasesShow, nrow(fp_cases))
                    try(fp_table$deleteRows(), silent = TRUE)

                    for (i in seq_len(max_show)) {
                        private$.checkpoint()
                        fp_table$addRow(rowKey = i, values = list(
                            case_id = fp_cases$row_id[i],
                            gold_value = as.character(fp_cases[[gold_var]][i]),
                            test_value = as.character(fp_cases[[test_var]][i])
                        ))
                    }

                    if (nrow(fp_cases) > max_show) {
                        note_text <- .fmt(.("Showing first {shown} of {total} false positive cases"),
                                           shown = max_show, total = nrow(fp_cases))
                        fp_table$setNote("truncated", note_text)
                    }
                }

                # False Negative cases table
                fn_table <- self$results$falseNegativeTable
                try(fn_table$deleteRows(), silent = TRUE)
                if (n_fn > 0) {
                    fn_cases <- mydata2[fn_idx, ]
                    # Use original row indices from the dataset, not filtered indices
                    fn_cases$row_id <- fn_cases$original_row_index

                    max_show <- min(self$options$maxCasesShow, nrow(fn_cases))
                    try(fn_table$deleteRows(), silent = TRUE)

                    for (i in seq_len(max_show)) {
                        private$.checkpoint()
                        fn_table$addRow(rowKey = i, values = list(
                            case_id = fn_cases$row_id[i],
                            gold_value = as.character(fn_cases[[gold_var]][i]),
                            test_value = as.character(fn_cases[[test_var]][i])
                        ))
                    }

                    if (nrow(fn_cases) > max_show) {
                        note_text <- .fmt(.("Showing first {shown} of {total} false negative cases"),
                                           shown = max_show, total = nrow(fn_cases))
                        fn_table$setNote("truncated", note_text)
                    }
                }

                # Interpretation
                private$.generateMisclassificationInterpretation(n_tp, n_fp, n_fn, n_tn)
            },

            .generateMisclassificationInterpretation = function(n_tp, n_fp, n_fn, n_tn) {

                total_errors <- n_fp + n_fn
                error_rate <- (total_errors / (n_tp + n_fp + n_fn + n_tn)) * 100

                fp_proportion <- if (total_errors > 0) (n_fp / total_errors) * 100 else 0
                fn_proportion <- if (total_errors > 0) (n_fn / total_errors) * 100 else 0

                html <- paste0("<h3>", .("Understanding Misclassifications"), "</h3>")

                html <- paste0(html,
                    "<p><b>", .("Error Summary"), ":</b> ",
                    .fmt(.("{n} total misclassifications ({pct} error rate)"),
                                    n = total_errors, pct = sprintf("%.1f%%", error_rate)),
                    "</p>")

                html <- paste0(html, "<ul>")
                html <- paste0(html,
                    "<li><b>", .("False Positives"), ":</b> ",
                    .fmt(.("{n} cases ({pct} of errors) - Test incorrectly predicts disease"),
                                    n = n_fp, pct = sprintf("%.1f%%", fp_proportion)),
                    "</li>")
                html <- paste0(html,
                    "<li><b>", .("False Negatives"), ":</b> ",
                    .fmt(.("{n} cases ({pct} of errors) - Test misses actual disease"),
                                    n = n_fn, pct = sprintf("%.1f%%", fn_proportion)),
                    "</li>")
                html <- paste0(html, "</ul>")

                # Error balance (descriptive only)
                if (n_fp > n_fn) {
                    html <- paste0(html,
                        "<p><b>", .("Error Balance"), ":</b> ",
                        .("False positives outnumbered false negatives in this sample."), "</p>")
                } else if (n_fn > n_fp) {
                    html <- paste0(html,
                        "<p><b>", .("Error Balance"), ":</b> ",
                        .("False negatives outnumbered false positives in this sample."), "</p>")
                } else {
                    html <- paste0(html,
                        "<p><b>", .("Error Balance"), ":</b> ",
                        .("False positives and false negatives occurred equally often in this sample."), "</p>")
                }

                # Follow-up checks on the analysis itself
                html <- paste0(html, "<p><b>", .("Follow-up checks on this analysis"), ":</b></p><ul>")

                if (n_fp > 0) {
                    html <- paste0(html,
                        "<li>", .("Review false positive cases to identify common characteristics"), "</li>",
                        "<li>", .("Check that the level you chose under Test positive level is the one you meant: this analysis has no numeric cutpoint, it simply treats that level as a positive result, so choosing the other level swaps every false positive with a false negative"), "</li>")
                }

                if (n_fn > 0) {
                    html <- paste0(html,
                        "<li>", .("Review false negative cases to understand what the test misses"), "</li>")
                }

                html <- paste0(html, "</ul>")

                html <- paste0(html,
                    "<p><i>", .("This analysis was inspired by Orange Data Mining's interactive confusion matrix feature, adapted for static jamovi output with comprehensive statistical tables."), "</i></p>")

                self$results$misclassificationInterpretation$setContent(html)
            }


        )
    )
