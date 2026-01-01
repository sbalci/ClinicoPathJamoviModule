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
#' @import ggplot2
#' @import boot
#' @importFrom stats quantile qnorm
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests


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
            # Constants for maintainability
            NOMOGRAM_LABEL_SIZE = 14/5,

            .init = function() {
                cTable <- self$results$cTable
                cTable$addRow(rowKey = "Test Positive", values = list(newtest = .("Test Positive")))
                cTable$addRow(rowKey = "Test Negative", values = list(newtest = .("Test Negative")))
                cTable$addRow(rowKey = "Total", values = list(newtest = .("Total")))

                # Populate welcome message
                self$results$welcome$setContent("
                    <div style='padding: 20px; background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); border-radius: 8px; border-left: 4px solid #4A90E2;'>
                        <h3 style='margin-top: 0; color: #2c3e50;'>📊 Medical Decision Analysis</h3>
                        <p style='font-size: 15px; color: #34495e;'>Evaluate diagnostic test performance with sensitivity, specificity, and predictive values.</p>

                        <h4 style='color: #2c3e50; margin-top: 20px;'>Quick Start:</h4>
                        <ol style='font-size: 14px; color: #34495e; line-height: 1.8;'>
                            <li><strong>Select Gold Standard:</strong> Choose the reference variable representing true disease status (e.g., biopsy result, final diagnosis)</li>
                            <li><strong>Select Disease Present Level:</strong> Choose which level indicates disease is present</li>
                            <li><strong>Select Test Variable:</strong> Choose the diagnostic test you want to evaluate</li>
                            <li><strong>Select Test Positive Level:</strong> Choose which level represents a positive test result</li>
                        </ol>

                        <div style='background: #fff; padding: 15px; border-radius: 5px; margin-top: 15px;'>
                            <h4 style='margin-top: 0; color: #2c3e50;'>💡 What You'll Get:</h4>
                            <ul style='font-size: 13px; color: #34495e; line-height: 1.6;'>
                                <li><strong>Sensitivity & Specificity:</strong> How well the test identifies disease presence and absence</li>
                                <li><strong>Predictive Values:</strong> Probability of disease given test results (PPV, NPV)</li>
                                <li><strong>Likelihood Ratios:</strong> How much test results change disease probability</li>
                                <li><strong>Confidence Intervals:</strong> Uncertainty estimates for all statistics</li>
                                <li><strong>Fagan Nomogram:</strong> Visual representation of probability changes</li>
                                <li><strong>Misclassification Analysis:</strong> Detailed examination of false positives and false negatives</li>
                            </ul>
                        </div>
                    </div>
                ")

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
                    return()
                }

                # Map notice types to colors and icons
                typeStyles <- list(
                    ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
                    STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
                    WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
                    INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
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
                        "<span style='color: #374151;'>", private$.safeHtmlOutput(notice$content), "</span>",
                        "</div>"
                    )
                }

                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },

            # Enhanced input validation for categorical diagnostic data
            .validateCategoricalInputs = function() {
                # Check for required variables with helpful messages
                if (length(self$options$gold) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No gold standard variable selected",
                        content = "Select a reference variable (e.g., biopsy result, final diagnosis). This represents the true disease status."
                    )
                    return(FALSE)
                }
                if (length(self$options$newtest) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No test variable selected",
                        content = "Select the diagnostic test you want to evaluate. This is typically a new, faster, or less expensive test."
                    )
                    return(FALSE)
                }
                if (length(self$options$goldPositive) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No disease-present level selected for gold standard",
                        content = "Select the level that indicates disease is present. This represents the condition you want to detect."
                    )
                    return(FALSE)
                }
                if (length(self$options$testPositive) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No positive level selected for test variable",
                        content = "Select the level that represents a positive test result. This should indicate suspected disease presence."
                    )
                    return(FALSE)
                }

                # Check data availability
                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "No data available for analysis",
                        content = "Please ensure your data is loaded. Check that your dataset contains observations."
                    )
                    return(FALSE)
                }

                # Validate data has enough cases
                if (nrow(self$data) < 4) {
                    private$.addNotice(
                        type = "ERROR",
                        title = sprintf('Insufficient data: %d cases found', nrow(self$data)),
                        content = "At least 4 cases are required for diagnostic test analysis. Each cell of the 2×2 table should have at least one observation."
                    )
                    return(FALSE)
                }

                # Validate prior probability if specified
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = sprintf('Invalid population prevalence: %.3f', self$options$pprob),
                        content = "Prevalence must be between 0 and 1 (exclusive). For 5% prevalence, enter 0.05. For 20% prevalence, enter 0.20."
                    )
                    return(FALSE)
                }

                # Warn about CI interpretation when using population prevalence
                if (self$options$pp && self$options$ci) {
                    private$.addNotice(
                        type = "WARNING",
                        title = "Confidence Intervals Interpretation",
                        content = "The displayed confidence intervals (95% CI) are calculated based on your study sample data. They apply to the sample-based Sensitivity, Specificity, PPV, and NPV. They do NOT apply to the adjusted \"Post-test Probabilities\" calculated using the fixed population prevalence."
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
                        title = sprintf('Disease-present level "%s" not found in gold standard variable', self$options$goldPositive),
                        content = sprintf('Available levels: %s. Check for typos or select the correct level from the dropdown.', available_levels)
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
                        title = sprintf('Test-positive level "%s" not found in test variable', self$options$testPositive),
                        content = sprintf('Available levels: %s. Check for typos or select the correct level from the dropdown.', available_levels)
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
                            title = sprintf('Disease-absent level "%s" not found in gold standard variable', self$options$goldNegative),
                            content = sprintf('Available levels: %s. Check for typos or select the correct level from the dropdown.', available_levels)
                        )
                        return(FALSE)
                    }

                    # Check that positive and negative levels are different
                    if (self$options$goldNegative == self$options$goldPositive) {
                        private$.addNotice(
                            type = "ERROR",
                            title = "Disease-present and disease-absent levels cannot be the same",
                            content = "Select different levels for positive and negative outcomes."
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
                            title = sprintf('Test-negative level "%s" not found in test variable', self$options$testNegative),
                            content = sprintf('Available levels: %s. Check for typos or select the correct level from the dropdown.', available_levels)
                        )
                        return(FALSE)
                    }

                    # Check that positive and negative levels are different
                    if (self$options$testNegative == self$options$testPositive) {
                        private$.addNotice(
                            type = "ERROR",
                            title = "Test-positive and test-negative levels cannot be the same",
                            content = "Select different levels for positive and negative outcomes."
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
                    issues <- c(issues, "LR+ recalculated due to invalid value")
                }
                if (!is.na(lrp) && lrp <= 0) {
                    if (!is.na(sens) && !is.na(spec)) {
                        lrp <- max(0.01, sens / max(1 - spec, 0.001))
                    } else {
                        lrp <- NA_real_
                    }
                    issues <- c(issues, "LR+ adjusted to positive value")
                }

                # Check LR- validity
                if (is.na(lrn) || !is.finite(lrn)) {
                    if (is.na(sens) || is.na(spec)) {
                        lrn <- NA_real_
                    } else {
                        lrn <- ifelse(sens == 1, 0, (1 - sens) / max(spec, 0.001))
                    }
                    issues <- c(issues, "LR- recalculated due to invalid value")
                }
                if (!is.na(lrn) && lrn < 0) {
                    if (!is.na(sens) && !is.na(spec)) {
                        lrn <- max(0.001, (1 - sens) / max(spec, 0.001))
                    } else {
                        lrn <- NA_real_
                    }
                    issues <- c(issues, "LR- adjusted to positive value")
                }

                list(lrp = lrp, lrn = lrn, issues = issues)
            },

            # Prepare analysis data with efficient processing
            .prepareAnalysisData = function() {
                # Helper function to escape variable names
                .escapeVar <- function(x) {
                    # Handle special characters in variable names
                    if (is.character(x)) {
                        x <- gsub("[^A-Za-z0-9_]", "_", make.names(x))
                    }
                    return(x)
                }

                # Get variable names efficiently with escaping
                testVar <- jmvcore::constructFormula(terms = self$options$newtest) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                goldVar <- jmvcore::constructFormula(terms = self$options$gold) %>%
                          jmvcore::decomposeFormula() %>% unlist()

                vars_needed <- unique(c(testVar, goldVar))
                if (length(vars_needed) < 2) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Selected variables are not available in the data",
                        content = "Check that variable names are correct. Ensure the data has been loaded properly."
                    )
                    return(NULL)
                }

                # Restrict case removal to variables used in the diagnostic table
                # Track original row indices BEFORE filtering
                subset_data <- self$data[, vars_needed, drop = FALSE]
                subset_data$original_row_index <- seq_len(nrow(subset_data))

                mydata <- jmvcore::naOmit(subset_data)

                if (nrow(mydata) < nrow(self$data)) {
                    removed <- nrow(self$data) - nrow(mydata)
                    private$.addNotice(
                        type = "WARNING",
                        title = sprintf('Removed %d row(s) with missing diagnostic data', removed),
                        content = sprintf('Complete-case analysis uses %d of %d cases. Consider investigating patterns of missingness.', nrow(mydata), nrow(self$data))
                    )
                }
                
                # Convert to factors and recode in single pipeline
                mydata[[testVar]] <- forcats::as_factor(mydata[[testVar]])
                mydata[[goldVar]] <- forcats::as_factor(mydata[[goldVar]])

                # Get actual levels for validation
                gold_actual_levels <- levels(mydata[[goldVar]])
                test_actual_levels <- levels(mydata[[testVar]])

                # Determine negative levels (explicit or implicit)
                has_gold_negative <- length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0
                has_test_negative <- length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0

                gold_negative_level <- if (has_gold_negative) self$options$goldNegative else NULL
                test_negative_level <- if (has_test_negative) self$options$testNegative else NULL

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
                    if (has_gold_negative) {
                        private$.addNotice(
                            type = "WARNING",
                            title = sprintf('Gold standard levels excluded from analysis: %s', excluded_str),
                            content = sprintf('Only "%s" (disease-present) and "%s" (disease-absent) will be used. Rows with excluded levels will be removed. To include all levels, leave "Disease Absent Level" empty.', self$options$goldPositive, gold_negative_level)
                        )
                    } else {
                        private$.addNotice(
                            type = "WARNING",
                            title = sprintf('Gold standard has %d level(s) beyond positive: %s', length(gold_excluded), excluded_str),
                            content = 'These will be treated as disease-absent (negative). If this is incorrect, explicitly select the "Disease Absent Level".'
                        )
                    }
                }

                if (length(test_excluded) > 0) {
                    excluded_str <- if (length(test_excluded) <= 5) {
                        paste(test_excluded, collapse = ", ")
                    } else {
                        paste(c(test_excluded[1:5], "..."), collapse = ", ")
                    }
                    if (has_test_negative) {
                        private$.addNotice(
                            type = "WARNING",
                            title = sprintf('Test variable levels excluded from analysis: %s', excluded_str),
                            content = sprintf('Only "%s" (test-positive) and "%s" (test-negative) will be used. Rows with excluded levels will be removed. To include all levels, leave "Test Negative Level" empty.', self$options$testPositive, test_negative_level)
                        )
                    } else {
                        private$.addNotice(
                            type = "WARNING",
                            title = sprintf('Test variable has %d level(s) beyond positive: %s', length(test_excluded), excluded_str),
                            content = 'These will be treated as test-negative. If this is incorrect, explicitly select the "Test Negative Level".'
                        )
                    }
                }

                # Efficient recoding with explicit negative level handling
                mydata <- mydata %>%
                    dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            is.na(.data[[testVar]]) ~ NA_character_,
                            .data[[testVar]] == self$options$testPositive ~ "Positive",
                            !has_test_negative ~ "Negative",  # If no explicit negative, treat all others as negative
                            .data[[testVar]] == test_negative_level ~ "Negative",
                            TRUE ~ NA_character_  # Explicit negative specified, others become NA (filtered)
                        ),
                        goldVariable2 = dplyr::case_when(
                            is.na(.data[[goldVar]]) ~ NA_character_,
                            .data[[goldVar]] == self$options$goldPositive ~ "Positive",
                            !has_gold_negative ~ "Negative",  # If no explicit negative, treat all others as negative
                            .data[[goldVar]] == gold_negative_level ~ "Negative",
                            TRUE ~ NA_character_  # Explicit negative specified, others become NA (filtered)
                        )
                    ) %>%
                    dplyr::mutate(
                        testVariable2 = forcats::fct_relevel(testVariable2, "Positive"),
                        goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive")
                    )

                # Remove rows with NA in recoded variables (excluded levels when explicit negative specified)
                mydata <- mydata %>% dplyr::filter(!is.na(testVariable2), !is.na(goldVariable2))
                
                # Validate contingency table structure after data preparation
                test_table <- table(mydata$testVariable2, mydata$goldVariable2)
                
                # Check for empty cells that would cause problems
                if (any(dim(test_table) != c(2, 2))) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Invalid data structure: Both test and gold standard variables must have exactly 2 levels each",
                        content = "Ensure your variables are dichotomous (binary). Check that positive/negative levels are correctly specified."
                    )
                    return(NULL)
                }
                
                # Check for zero cells that would cause division by zero
                if (any(test_table == 0)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Zero counts detected in contingency table",
                        content = "Results may be unstable or undefined (e.g., infinite likelihood ratios). Consider collecting more data or using exact methods. Ensure both tests and gold standard have both positive and negative cases."
                    )
                }
                
                return(list(data = mydata, testVar = testVar, goldVar = goldVar))
            },

            # Enhanced diagnostic accuracy interpretation helper
            .getDiagnosticInterpretation = function(lr_pos, lr_neg, sens, spec) {
                # Likelihood ratio interpretations based on clinical guidelines
                lr_pos_interp <- dplyr::case_when(
                    is.na(lr_pos) ~ "Positive likelihood ratio unavailable due to data limitations",
                    lr_pos >= 10 ~ "Large and often conclusive increase in probability of disease",
                    lr_pos >= 5 ~ "Moderate increase in probability of disease",
                    lr_pos >= 2 ~ "Small but potentially important increase in probability",
                    lr_pos > 1 ~ "Minimal increase in probability of disease",
                    TRUE ~ "Decreases probability of disease (test may be flawed)"
                )
                
                lr_neg_interp <- dplyr::case_when(
                    is.na(lr_neg) ~ "Negative likelihood ratio unavailable due to data limitations",
                    lr_neg <= 0.1 ~ "Large and often conclusive decrease in probability of disease",
                    lr_neg <= 0.2 ~ "Moderate decrease in probability of disease",
                    lr_neg <= 0.5 ~ "Small but potentially important decrease in probability",
                    lr_neg < 1 ~ "Minimal decrease in probability of disease",
                    TRUE ~ "Increases probability of disease (test may be flawed)"
                )
                
                # Overall test utility based on Youden's Index
                youden_index <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1
                test_utility <- dplyr::case_when(
                    is.na(youden_index) ~ "Insufficient data to evaluate discriminatory power",
                    youden_index >= 0.5 ~ "Excellent discriminatory power",
                    youden_index >= 0.3 ~ "Good discriminatory power",
                    youden_index >= 0.1 ~ "Fair discriminatory power",
                    TRUE ~ "Poor discriminatory power - limited clinical utility"
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
                if (nrow(original_data) == nrow(processed_data)) {
                    return(.("No missing data detected."))
                }
                
                missing_count <- nrow(original_data) - nrow(processed_data)
                missing_percent <- round((missing_count / nrow(original_data)) * 100, 1)
                
                analysis <- paste0("Missing data summary: ", missing_count, " cases (", missing_percent, "%) with missing values removed. Complete case analysis performed.")
                
                return(analysis)
            },
            
            # Generate natural language summary for clinical use
            .generateNaturalLanguageSummary = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                                      prevalence, post_pos_prob, post_neg_prob,
                                                      total_pop, test_name, gold_name) {
                format_percent <- function(value, fallback_label) {
                    if (is.na(value)) return(fallback_label)
                    sprintf("%.1f%%", value * 100)
                }

                # Get clinical benchmarks for enhanced interpretation
                benchmarks <- private$.addClinicalBenchmarks(sens, spec, lr_pos, lr_neg)

                # Determine test quality
                test_quality <- dplyr::case_when(
                    !is.na(sens) && !is.na(spec) && sens >= 0.9 && spec >= 0.9 ~ "excellent",
                    !is.na(sens) && !is.na(spec) && sens >= 0.8 && spec >= 0.8 ~ "good", 
                    (!is.na(sens) && sens >= 0.7) || (!is.na(spec) && spec >= 0.7) ~ "moderate",
                    TRUE ~ "limited"
                )

                # Determine primary clinical utility
                primary_utility <- dplyr::case_when(
                    !is.na(sens) && sens >= 0.9 && (is.na(spec) || spec < 0.8) ~ "ruling out disease",
                    !is.na(spec) && spec >= 0.9 && (is.na(sens) || sens < 0.8) ~ "confirming disease",
                    !is.na(sens) && !is.na(spec) && sens >= 0.8 && spec >= 0.8 ~ "both ruling out and confirming disease",
                    TRUE ~ "diagnostic screening"
                )

                prevalence_text <- format_percent(prevalence, "not reported")
                sens_text <- format_percent(sens, "not calculated")
                spec_text <- format_percent(spec, "not calculated")
                ppv_text <- format_percent(ppv, "not calculated")
                npv_text <- format_percent(npv, "not calculated")
                post_pos_text <- format_percent(post_pos_prob, "not available")
                post_neg_disease_text <- format_percent(1 - post_neg_prob, "not available")
                post_neg_healthy_text <- format_percent(post_neg_prob, "not available")

                sample_text <- if (!is.na(total_pop)) sprintf("%d cases analyzed", total_pop) else "Sample size not available"

                summary_template <- .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #4CAF50; background: #f1f8e9;'><h3 style='color: #2E7D32; margin-top: 0;'>Clinical Summary</h3><p style='font-size: 16px;'><strong>Analysis:</strong> Diagnostic test performance evaluation comparing %s against gold standard %s.</p><p><strong>Sample:</strong> %s with %s disease prevalence.</p><p><strong>Test Performance:</strong> The test shows <strong>%s</strong> discriminatory ability with sensitivity of <strong>%s</strong> (<em>%s</em>) and specificity of <strong>%s</strong> (<em>%s</em>).</p><p><strong>Clinical Utility:</strong> This test is most useful for <strong>%s</strong> in the clinical setting.</p><p><strong>Likelihood Ratios:</strong> Positive LR: %.2f (<em>%s</em>), Negative LR: %.2f (<em>%s</em>)</p><p><strong>Key Findings:</strong> When positive, the post-test disease probability is <strong>%s</strong> (PPV %s). When negative, the disease probability falls to <strong>%s</strong> and the probability of being disease-free is <strong>%s</strong> (NPV %s).</p></div>")

                lr_pos_safe <- if (is.na(lr_pos) || !is.finite(lr_pos)) 0 else lr_pos
                lr_neg_safe <- if (is.na(lr_neg) || !is.finite(lr_neg)) 0 else lr_neg

                summary <- sprintf(
                    summary_template,
                    test_name, gold_name,
                    sample_text, prevalence_text,
                    test_quality, sens_text, benchmarks$sens_quality, spec_text, benchmarks$spec_quality,
                    primary_utility,
                    lr_pos_safe, benchmarks$lr_pos_interpretation,
                    lr_neg_safe, benchmarks$lr_neg_interpretation,
                    post_pos_text, ppv_text, post_neg_disease_text, post_neg_healthy_text, npv_text
                )

                return(summary)
            },
            
            # Generate copy-ready report template
            .generateReportTemplate = function(sens, spec, ppv, npv, lr_pos, lr_neg, 
                                             sens_ci = NULL, spec_ci = NULL, test_name, gold_name) {
                # Create confidence interval text if available
                ci_text <- if (!is.null(sens_ci) && !is.null(spec_ci)) {
                    sprintf(.("(95%% CI: sensitivity %.1f-%.1f%%, specificity %.1f-%.1f%%)"), 
                           sens_ci[1]*100, sens_ci[2]*100, spec_ci[1]*100, spec_ci[2]*100)
                } else {
                    ""
                }
                
                # Determine clinical interpretation
                interpretation <- dplyr::case_when(
                    is.na(lr_pos) ~ .("likelihood ratio unavailable due to data limitations"),
                    lr_pos >= 10 ~ .("strong evidence for disease when positive"),
                    lr_pos >= 5 ~ .("moderate evidence for disease when positive"),
                    lr_pos >= 2 ~ .("weak evidence for disease when positive"),
                    TRUE ~ .("minimal evidence for disease when positive")
                )
                
                # Generate template
                template_string <- .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background: #e3f2fd;'><h3 style='color: #1976D2; margin-top: 0;'>Copy-Ready Clinical Report</h3><div style='background: white; padding: 10px; border-radius: 5px; font-family: Arial, sans-serif;'><p><strong>DIAGNOSTIC TEST EVALUATION</strong></p><p>We evaluated the diagnostic performance of %s compared to the gold standard %s. The test demonstrated a sensitivity of %.1f%% and specificity of %.1f%% %s. The positive predictive value was %.1f%% and negative predictive value was %.1f%%. The positive likelihood ratio of %.1f provides %s. These results suggest the test may be clinically useful for diagnostic evaluation.</p></div><p style='font-size: 12px; color: #666;'><em>Copy the text above for your clinical report. Modify as needed for your specific context.</em></p></div>")

                template <- sprintf(
                    template_string,
                    test_name, gold_name,
                    sens * 100, spec * 100, ci_text,
                    ppv * 100, npv * 100,
                    lr_pos, interpretation
                )
                
                return(template)
            },
            
            # Enhanced misuse detection and warnings
            .detectMisuse = function(conf_table, prevalence, n_total) {
                warnings <- c()
                
                # Check for small cell counts
                if (any(conf_table < 5)) {
                    warnings <- c(warnings, 
                        .("Small cell counts detected (< 5). Results may be unstable. Consider collecting more data or using exact methods."))
                }
                
                # Check for extreme prevalence
                if (!is.na(prevalence) && prevalence < 0.05) {
                    warnings <- c(warnings,
                        .("Very low disease prevalence (< 5%). Positive predictive value may be unreliable. Consider the clinical context carefully."))
                }

                if (!is.na(prevalence) && prevalence > 0.95) {
                    warnings <- c(warnings,
                        .("Very high disease prevalence (> 95%). Negative predictive value may be unreliable. Verify your data coding."))
                }

                # Check for small sample size
                if (!is.na(n_total) && n_total < 30) {
                    warnings <- c(warnings,
                        .("Small sample size (< 30). Confidence intervals may be wide. Interpret results cautiously."))
                }

                # Check for unbalanced data
                pos_ratio <- sum(conf_table[1,]) / n_total
                if (!is.na(pos_ratio) && (pos_ratio < 0.1 || pos_ratio > 0.9)) {
                    warnings <- c(warnings,
                        .("Highly unbalanced test results. Consider the appropriateness of the selected positive level."))
                }
                
                return(warnings)
            },
            
            # Generate About This Analysis content
            .generateAboutAnalysis = function() {
                about_content <- paste0(
                    "<div style='margin: 15px; padding: 15px; background: #f5f5f5; border-radius: 8px;'>",
                    "<h3 style='color: #1976D2; margin-top: 0;'>About Diagnostic Test Evaluation</h3>",

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
                    "<li><strong>", .("PPV (Positive Predictive Value)"), ":</strong> ", .("Probability of disease given positive test. Depends on prevalence and specificity."), "</li>",
                    "<li><strong>", .("NPV (Negative Predictive Value)"), ":</strong> ", .("Probability of being healthy given negative test. Depends on prevalence and sensitivity."), "</li>",
                    "<li><strong>", .("LR+ (Positive Likelihood Ratio)"), ":</strong> ", .("How much a positive test increases disease odds. LR+ >10 strong evidence FOR disease, LR+ 5-10 moderate, LR+ 2-5 weak but useful."), "</li>",
                    "<li><strong>", .("LR- (Negative Likelihood Ratio)"), ":</strong> ", .("How much a negative test decreases disease odds. LR- <0.1 strong evidence AGAINST disease, LR- 0.1-0.2 moderate, LR- 0.2-0.5 weak."), "</li>",
                    "</ul>",
                    
                    "<h4 style='color: #424242;'>", .("Clinical Interpretation Guidelines"), "</h4>",
                    "<div style='background: #e8f5e8; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<strong>", .("Excellent Tests"), ":</strong><br>",
                    "• ", .("Sensitivity >90% excellent for ruling OUT disease"), "<br>",
                    "• ", .("Specificity >90% excellent for ruling IN disease"), "<br>",
                    "• ", .("LR+ >10 strong evidence FOR disease, LR+ 5-10 moderate, LR+ 2-5 weak but useful"), "<br>",
                    "• ", .("LR- <0.1 strong evidence AGAINST disease, LR- 0.1-0.2 moderate, LR- 0.2-0.5 weak"), "<br>",
                    "</div>",

                    "<h4 style='color: #424242;'>", .("Analysis Options Explained"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("95% Confidence Intervals"), ":</strong> ", .("Provides uncertainty estimates using epiR package. Cannot be used with population prevalence."), "</li>",
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
                        title = sprintf('Very large dataset detected (%d rows)', n_rows),
                        content = "Analysis may take longer than usual. Consider sampling for initial exploratory analysis. Full dataset will still be used for final results."
                    )
                } else if (n_rows > 10000) {
                    private$.addNotice(
                        type = "INFO",
                        title = sprintf('Large dataset detected (%d rows)', n_rows),
                        content = "Analysis may take a moment to complete."
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
                        title = sprintf('Very small sample size: n = %d (< 20 cases)', total_n),
                        content = "Results may be unreliable and unstable. Confidence intervals will be very wide. Consider collecting more data before making clinical decisions. Minimum recommended: 100 cases for robust estimates."
                    )
                } else if (total_n < 50) {
                    private$.addNotice(
                        type = "WARNING",
                        title = sprintf('Small sample size: n = %d (< 50 cases)', total_n),
                        content = "Interpret results with caution. Confidence intervals may be wide. Minimum recommended: 100 cases for robust estimates."
                    )
                } else if (total_n < 100) {
                    private$.addNotice(
                        type = "INFO",
                        title = sprintf('Sample size: n = %d', total_n),
                        content = "For robust diagnostic test evaluation, 100+ cases recommended. Current sample provides preliminary estimates."
                    )
                }

                if (min_cell < 5) {
                    private$.addNotice(
                        type = "WARNING",
                        title = sprintf('Small cell count detected (minimum = %d, < 5)', min_cell),
                        content = "Statistical estimates may be unstable. Consider Fisher's exact test for small samples. Collect more data for stable estimates of sensitivity/specificity."
                    )
                }
            },

            # Add clinical performance benchmarks for interpretation
            .addClinicalBenchmarks = function(sens, spec, lr_pos, lr_neg) {
                benchmarks <- list()

                # Sensitivity benchmarks (SnNout - Sensitive test, Negative result rules OUT)
                benchmarks$sens_quality <- if (sens >= 0.95) "Excellent for ruling OUT disease"
                                           else if (sens >= 0.90) "Very good for ruling OUT disease"
                                           else if (sens >= 0.80) "Good for ruling OUT disease"
                                           else "Limited ability for ruling OUT disease"

                # Specificity benchmarks (SpPin - Specific test, Positive result rules IN)
                benchmarks$spec_quality <- if (spec >= 0.95) "Excellent for ruling IN disease"
                                           else if (spec >= 0.90) "Very good for ruling IN disease"
                                           else if (spec >= 0.80) "Good for ruling IN disease"
                                           else "Limited ability for ruling IN disease"

                # Likelihood ratio benchmarks
                benchmarks$lr_pos_interpretation <- if (!is.finite(lr_pos)) "Cannot be calculated"
                                                    else if (lr_pos > 10) "Strong evidence for disease"
                                                    else if (lr_pos > 5) "Moderate evidence for disease"
                                                    else if (lr_pos > 2) "Weak evidence for disease"
                                                    else "Minimal evidence for disease"

                benchmarks$lr_neg_interpretation <- if (!is.finite(lr_neg)) "Cannot be calculated"
                                                    else if (lr_neg < 0.1) "Strong evidence against disease"
                                                    else if (lr_neg < 0.2) "Moderate evidence against disease"
                                                    else if (lr_neg < 0.5) "Weak evidence against disease"
                                                    else "Minimal evidence against disease"

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
                    AccurT = .("Accuracy: Overall proportion of correct test results. Consider prevalence dependency."),
                    PrevalenceD = .("Disease Prevalence: Proportion with disease in this population. Affects predictive values."),
                    PPV = .("Positive Predictive Value: Probability of disease given positive test. Depends on prevalence and specificity."),
                    NPV = .("Negative Predictive Value: Probability of being healthy given negative test. Depends on prevalence and sensitivity."),
                    PostTestProbDisease = .("Post-test Probability (Disease+): Probability of disease after positive test using population prevalence."),
                    PostTestProbHealthy = .("Post-test Probability (Disease-): Probability of being healthy after negative test using population prevalence."),
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
                                         prior_prob, post_test_prob_disease, post_test_prob_healthy,
                                         total_pop, test_name, gold_name) {

                results <- list(
                    clinical_summary = "",
                    about_content = "",
                    natural_summary = "",
                    report_template = ""
                )

                format_percent <- function(value, default = "not available") {
                    if (is.na(value)) return(default)
                    sprintf("%.1f%%", value * 100)
                }

                # Generate clinical interpretation
                tryCatch({
                    interpretation <- private$.getDiagnosticInterpretation(lr_pos, lr_neg, sens, spec)

                    sens_text <- format_percent(sens)
                    spec_text <- format_percent(spec)
                    youden_text <- if (is.na(interpretation$youden_index)) "not available" else sprintf("%.3f", interpretation$youden_index)
                    lr_pos_text <- ifelse(is.na(lr_pos), "undefined", sprintf("%.2f", lr_pos))
                    lr_neg_text <- ifelse(is.na(lr_neg), "undefined", sprintf("%.2f", lr_neg))

                    results$clinical_summary <- paste0(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #2196F3; background: #f8f9fa;'>",
                        "<h4 style='color: #1976D2; margin-top: 0;'>Clinical Interpretation</h4>",
                        "<p><strong>Test Performance Summary:</strong></p>",
                        "<ul>",
                        sprintf("<li><strong>Sensitivity:</strong> %s - %s</li>", sens_text,
                                if (is.na(sens)) "Unable to assess" else if (sens >= 0.9) "Excellent for ruling out disease" else if (sens >= 0.8) "Good for ruling out disease" else "Limited ability to rule out disease"),
                        sprintf("<li><strong>Specificity:</strong> %s - %s</li>", spec_text,
                                if (is.na(spec)) "Unable to assess" else if (spec >= 0.9) "Excellent for ruling in disease" else if (spec >= 0.8) "Good for ruling in disease" else "Limited ability to rule in disease"),
                        sprintf("<li><strong>Youden's Index:</strong> %s - %s</li>", youden_text, interpretation$test_utility),
                        "</ul>",
                        "<p><strong>Likelihood Ratio Interpretation:</strong></p>",
                        "<ul>",
                        sprintf("<li><strong>Positive LR (%s):</strong> %s</li>", lr_pos_text, interpretation$lr_pos_interp),
                        sprintf("<li><strong>Negative LR (%s):</strong> %s</li>", lr_neg_text, interpretation$lr_neg_interp),
                        "</ul>",
                        "<p><strong>Clinical Decision Making:</strong></p>",
                        "<ul>",
                        sprintf("<li>Pre-test probability: <strong>%s</strong></li>", format_percent(prior_prob, "not provided")),
                        sprintf("<li>Post-test probability (if positive): <strong>%s</strong></li>", format_percent(post_test_prob_disease, "not available")),
                        sprintf("<li>Post-test probability (if negative): <strong>%s</strong></li>", format_percent(post_test_prob_healthy, "not available")),
                        "</ul></div>"
                    )
                }, error = function(e) {
                    results$clinical_summary <<- paste0(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #ff9800; background: #fff3e0;'>",
                        "<h4 style='color: #f57c00; margin-top: 0;'>Clinical Interpretation</h4>",
                        "<p>Unable to generate detailed clinical interpretation due to data limitations.</p>",
                        sprintf("<p><strong>Basic Results:</strong> Sensitivity: %s, Specificity: %s</p>",
                                format_percent(sens, "not available"),
                                format_percent(spec, "not available")),
                        "</div>"
                    )
                })

                # Generate about content
                tryCatch({
                    results$about_content <- private$.generateAboutAnalysis()
                }, error = function(e) {
                    results$about_content <<- "<div>About analysis content unavailable</div>"
                })

                # Generate natural language summary
                tryCatch({
                    results$natural_summary <- private$.generateNaturalLanguageSummary(
                        sens, spec, ppv, npv, lr_pos, lr_neg,
                        prior_prob, post_test_prob_disease, post_test_prob_healthy,
                        total_pop, test_name, gold_name
                    )
                }, error = function(e) {
                    fallback_template <- .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #FF9800; background: #fff3e0;'><h3 style='color: #F57C00; margin-top: 0;'>Clinical Summary</h3><p>Basic diagnostic test evaluation completed with %d cases.</p><p><strong>Results:</strong> Sensitivity %.1f%%, Specificity %.1f%%</p></div>")

                    results$natural_summary <<- sprintf(
                        fallback_template,
                        total_pop, sens * 100, spec * 100
                    )
                })

                # Generate report template
                tryCatch({
                    results$report_template <- private$.generateReportTemplate(
                        sens, spec, ppv, npv, lr_pos, lr_neg,
                        test_name = test_name,
                        gold_name = gold_name
                    )
                }, error = function(e) {
                    fallback_template <- .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background: #e3f2fd;'><h3 style='color: #1976D2; margin-top: 0;'>Copy-Ready Clinical Report</h3><p>Diagnostic test evaluation shows sensitivity of %.1f%% and specificity of %.1f%%.</p></div>")

                    results$report_template <<- sprintf(
                        fallback_template,
                        sens * 100, spec * 100
                    )
                })

                return(results)
            }




            ,
            .run = function() {
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
                missing_analysis <- private$.analyzeMissingData(original_data, mydata)
                if (nrow(original_data) != nrow(mydata)) {
                    # Missing data summary is already shown via missingDataSummary HTML output
                    # No need for console message - users see it in the results panel
                    self$results$missingDataSummary$setContent(missing_analysis)
                }

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
                gold_negative_level <- if (has_gold_negative) {
                    self$options$goldNegative
                } else {
                    # Infer: any level that's not positive
                    setdiff(gold_levels, self$options$goldPositive)[1]
                }

                # Determine test negative level (explicit or infer)
                has_test_negative <- length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0
                test_negative_level <- if (has_test_negative) {
                    self$options$testNegative
                } else {
                    # Infer: any level that's not positive
                    setdiff(test_levels, self$options$testPositive)[1]
                }

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
                    raw_contingency$getColumn("gold_neg")$setTitle(gold_negative_level)
                    raw_contingency$getColumn("gold_neg")$setSuperTitle(goldVariable)
                }
                if (!is.null(raw_contingency$getColumn("row_total"))) {
                    raw_contingency$getColumn("row_total")$setTitle(.("Total"))
                    raw_contingency$getColumn("row_total")$setSuperTitle("")
                }

                row_names <- rownames(results_matrix)
                col_names <- colnames(results_matrix)

                # Populate rows in order: positive test first, then negative test
                ordered_test_levels <- c(self$options$testPositive, test_negative_level)

                if (!is.null(test_levels) && length(test_levels) > 0 &&
                    !is.null(gold_levels) && length(gold_levels) > 0) {

                    for (lvl in ordered_test_levels) {
                        # Skip if this level doesn't exist in the data
                        if (!(lvl %in% test_levels)) next

                        row_vector <- if (!is.null(row_names) && lvl %in% row_names) {
                            results_matrix[lvl, , drop = FALSE]
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

                        val_neg <- if (gold_negative_level %in% names(row_values)) {
                            row_values[[gold_negative_level]]
                        } else {
                            NA_real_
                        }

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

                    total_neg <- if (gold_negative_level %in% names(col_totals)) {
                        col_totals[[gold_negative_level]]
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
                    for (i in seq_len(nrow(combo_for_table))) {
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
                        title = sprintf('Error extracting confusion matrix values: %s', e$message),
                        content = "Check your data formatting. Ensure both variables have exactly 2 levels. Verify positive/negative levels are correctly specified."
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
                        title = "Invalid contingency table values detected",
                        content = "Confusion matrix contains NA or negative values. Check that your data is properly formatted. Ensure sufficient observations in all categories."
                    )
                    return()
                }




                # Cross Table in jamovi style ----

                cTable <- self$results$cTable

                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = "Test Positive",
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )


                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = "Test Negative",
                        GP = FN,
                        GN = TN,
                        Total = FN + TN
                    )
                )

                cTable$setRow(
                    rowKey = "Total",
                    values = list(
                        newtest = "Total",
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

                # Validate all metrics are in [0,1] range with debug logging
                for (metric_name in c("Sens", "Spec", "AccurT", "PrevalenceD", "PPV", "NPV")) {
                    metric_value <- get(metric_name)
                    if (!is.na(metric_value) && (metric_value < 0 || metric_value > 1)) {
                        # Metric validation - should not occur with proper data validation
                        # warning(sprintf("%s out of valid range [0,1]: %.3f - check data", metric_name, metric_value))
                    }
                }

                # Debug logging for edge cases and unusual conditions
                if (getOption("jamovi.debug", FALSE)) {
                    # Debug message - commented out for production
                    # message(sprintf("Decision analysis debug: n=%d, prev=%.3f, sens=%.3f, spec=%.3f, ppv=%.3f, npv=%.3f",
                    #                TotalPop, PrevalenceD, Sens, Spec, PPV, NPV))

                    # Log potential issues
                    # Data quality checks - these are now handled by .validateSampleSize() via Notices
                    # Small sample size, extreme prevalence, small cell counts notifications are emitted earlier
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


                # Post-test probability calculations using Bayes' theorem
                # PPV when using population prevalence
                PostTestProbDisease <- if (TestP > 0) {
                    (PriorProb * Sens) / ((PriorProb * Sens) + ((1 - PriorProb) * (1 - Spec)))
                } else {
                    NA
                }

                # NPV when using population prevalence (1 - probability of disease given negative test)
                PostTestProbHealthy <- if (TestN > 0) {
                    ((1 - PriorProb) * Spec) / (((1 - PriorProb) * Spec) + (PriorProb * (1 - Sens)))
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
                        title = "Likelihood ratio adjustments applied",
                        content = sprintf('%s. Results have been adjusted for statistical validity.', paste(lr_validation$issues, collapse = "; "))
                    )
                }

                if (continuity_used) {
                    private$.addNotice(
                        type = "INFO",
                        title = "Continuity correction applied",
                        content = "Zero cells detected; applied Haldane-Anscombe 0.5 continuity correction for LR/OR calculations (sensitivity/specificity still use observed counts)."
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
                ratioTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
                        Sens = Sens,
                        Spec = Spec,
                        AccurT = AccurT,
                        PrevalenceD = PriorProb,
                        PPV = PPV,
                        NPV = NPV,
                        PostTestProbDisease = PostTestProbDisease,
                        PostTestProbHealthy = PostTestProbHealthy,
                        LRP = LRP,
                        LRN = LRN
                    )
                )

                # Consolidated content generation with enhanced error handling
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
                    content_results <- private$.generateAllContent(Sens, Spec, PPV, NPV, LRP, LRN,
                                                                  PriorProb, PostTestProbDisease, PostTestProbHealthy,
                                                                  TotalPop, test_label, gold_label)
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
                
                # Detect misuse and display warnings
                tryCatch({
                    warnings <- private$.detectMisuse(conf_table, PrevalenceD, TotalPop)
                    
                    if (length(warnings) > 0) {
                        warning_text <- paste(warnings, collapse = "<br>")
                        warning_panel <- sprintf(
                            "<div style='margin: 10px; padding: 10px; border-left: 4px solid #FF5722; background: #ffebee;'><h4 style='color: #D32F2F; margin-top: 0;'>Analysis Warnings</h4>%s</div>",
                            warning_text
                        )
                        
                        # Prepend warnings to clinical interpretation
                        if ("clinicalInterpretation" %in% names(self$results)) {
                            current_content <- clinical_summary
                            self$results$clinicalInterpretation$setContent(paste0(warning_panel, current_content))
                        }
                    }
                }, error = function(e) {
                    # Silently handle misuse detection errors
                })

                # Misclassified Cases Analysis and Output
                tryCatch({
                    # Helper to check if output variable is properly specified
                    has_output_var <- !is.null(self$options$saveClassifications) &&
                                     length(self$options$saveClassifications) > 0 &&
                                     nchar(self$options$saveClassifications) > 0

                    # Always create classifications if output is requested or analysis is shown
                    if (self$options$showMisclassified || has_output_var) {
                        private$.analyzeMisclassifiedCases(mydata, goldVariable, testVariable)
                    }
                }, error = function(e) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Error in misclassified cases analysis",
                        content = paste("Technical details:", e$message, "Please report this issue if it persists.")
                    )
                })

                # Add footnotes using centralized method
                private$.addFootnotes()





                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    # epiR confidence intervals with error handling
                    epir_success <- FALSE
                    epirresult_ratio <- NULL
                    epirresult_number <- NULL

                    tryCatch({
                        epirresult <- epiR::epi.tests(dat = conf_table)

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
                                add_ratio_note(3, "statsnames", .("Probability of disease given positive test. Depends on prevalence and specificity."))
                                add_ratio_note(4, "statsnames", .("Probability of being healthy given negative test. Depends on prevalence and sensitivity."))
                                add_ratio_note(1, "est", .("Confidence intervals for sensitivity, specificity, and predictive values are calculated using the Wilson score method."))
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
                                add_number_note <- function(row_no, col, text) {
                                    if (nrow(data_frame) >= row_no) {
                                        epirTable_number$addFootnote(rowNo = row_no, col = col, text)
                                    }
                                }

                                add_number_note(1, "statsnames", .("How much more likely will the test make a correct diagnosis than an incorrect diagnosis in patients with the disease."))
                                add_number_note(2, "statsnames", .("Number of patients that need to be tested to give one correct positive test."))
                                add_number_note(3, "statsnames", .("Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1)."))
                            }
                        }
                    }
                }





                # Prepare Fagan Nomogram Data ----
                if (self$options$fagan) {
                    plotData1 <- list(
                        "Prevalence" = PriorProb,
                        "Sens" = Sens,
                        "Spec" = Spec,
                        "Plr" = LRP,
                        "Nlr" = LRN
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData1)
                }

                # Render all collected notices as HTML (last step)
                private$.renderNotices()

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
                        stage = factor(c("Pre-test", "Post-test", "Pre-test", "Post-test"),
                                       levels = c("Pre-test", "Post-test")),
                        probability = c(prevalence, post_prob_pos, prevalence, post_prob_neg)
                    )
                    plot_df <- plot_df[!is.na(plot_df$probability), , drop = FALSE]

                    plot_title <- "Diagnostic Probability Shift"
                    subtitle <- sprintf("Pre-test prevalence %.1f%% | Sensitivity %.1f%% | Specificity %.1f%%",
                                        prevalence * 100, sens * 100, spec * 100)

                    plot1 <- ggplot2::ggplot(plot_df, ggplot2::aes(x = stage, y = probability,
                                                                   group = result, color = result)) +
                        ggplot2::geom_line(size = 1.2) +
                        ggplot2::geom_point(size = 3) +
                        ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", x * 100),
                                                    limits = c(0, 1)) +
                        ggplot2::scale_color_manual(values = c("Positive" = "#d32f2f", "Negative" = "#1976d2")) +
                        ggplot2::labs(title = plot_title,
                                      subtitle = subtitle,
                                      y = "Probability",
                                      x = "",
                                      color = "Test result") +
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
                    Plr = plotData1$Plr,
                    Nlr = plotData1$Nlr,
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

                # Map complete case indices back to original data indices
                # Get row names from mydata2 to match with original data
                complete_indices <- as.numeric(rownames(mydata2))

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
                if (!is.null(self$options$saveClassifications) &&
                    length(self$options$saveClassifications) > 0 &&
                    nchar(self$options$saveClassifications) > 0) {
                    output <- self$results$saveClassifications
                    if (!is.null(output) && is.function(output$setValues)) {
                        output$setValues(classification_vector)
                    }
                }

                # Only populate tables if user requested to see misclassified cases
                if (!self$options$showMisclassified) {
                    return()
                }

                # Summary counts
                n_total <- nrow(mydata2)
                n_tp <- sum(tp_idx, na.rm = TRUE)
                n_fp <- sum(fp_idx, na.rm = TRUE)
                n_fn <- sum(fn_idx, na.rm = TRUE)
                n_tn <- sum(tn_idx, na.rm = TRUE)

                # Populate confusion matrix summary
                summary_table <- self$results$confusionMatrixSummary
                try(summary_table$deleteRows(), silent = TRUE)
                summary_table$addRow(rowKey = 1, values = list(
                    classification = "True Positive",
                    count = n_tp,
                    percentage = n_tp / n_total
                ))
                summary_table$addRow(rowKey = 2, values = list(
                    classification = "False Positive",
                    count = n_fp,
                    percentage = n_fp / n_total
                ))
                summary_table$addRow(rowKey = 3, values = list(
                    classification = "False Negative",
                    count = n_fn,
                    percentage = n_fn / n_total
                ))
                summary_table$addRow(rowKey = 4, values = list(
                    classification = "True Negative",
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
                        fp_table$addRow(rowKey = i, values = list(
                            case_id = fp_cases$row_id[i],
                            gold_value = as.character(fp_cases[[gold_var]][i]),
                            test_value = as.character(fp_cases[[test_var]][i])
                        ))
                    }

                    if (nrow(fp_cases) > max_show) {
                        note_text <- sprintf("Showing first %d of %d false positive cases",
                                           max_show, nrow(fp_cases))
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
                        fn_table$addRow(rowKey = i, values = list(
                            case_id = fn_cases$row_id[i],
                            gold_value = as.character(fn_cases[[gold_var]][i]),
                            test_value = as.character(fn_cases[[test_var]][i])
                        ))
                    }

                    if (nrow(fn_cases) > max_show) {
                        note_text <- sprintf("Showing first %d of %d false negative cases",
                                           max_show, nrow(fn_cases))
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

                html <- "<h3>Understanding Misclassifications</h3>"

                html <- paste0(html, sprintf(
                    "<p><b>Error Summary:</b> %d total misclassifications (%.1f%% error rate)</p>",
                    total_errors, error_rate
                ))

                html <- paste0(html, "<ul>")
                html <- paste0(html, sprintf(
                    "<li><b>False Positives:</b> %d cases (%.1f%% of errors) - Test incorrectly predicts disease</li>",
                    n_fp, fp_proportion
                ))
                html <- paste0(html, sprintf(
                    "<li><b>False Negatives:</b> %d cases (%.1f%% of errors) - Test misses actual disease</li>",
                    n_fn, fn_proportion
                ))
                html <- paste0(html, "</ul>")

                # Clinical interpretation
                if (n_fp > n_fn) {
                    html <- paste0(html,
                        "<p><b>Clinical Implication:</b> The test tends to over-diagnose (more false positives). ",
                        "This may lead to unnecessary treatments but rarely misses disease.</p>")
                } else if (n_fn > n_fp) {
                    html <- paste0(html,
                        "<p><b>Clinical Implication:</b> The test tends to under-diagnose (more false negatives). ",
                        "This may miss cases that need treatment but reduces unnecessary interventions.</p>")
                } else {
                    html <- paste0(html,
                        "<p><b>Clinical Implication:</b> The test has balanced error types.</p>")
                }

                # Recommendations
                html <- paste0(html, "<p><b>Recommendations:</b></p><ul>")

                if (n_fp > 0) {
                    html <- paste0(html,
                        "<li>Review false positive cases to identify common characteristics</li>",
                        "<li>Consider if test cutpoint needs adjustment to reduce false positives</li>")
                }

                if (n_fn > 0) {
                    html <- paste0(html,
                        "<li>Review false negative cases to understand what the test misses</li>",
                        "<li>Consider supplementary tests for cases with high clinical suspicion</li>")
                }

                html <- paste0(html, "</ul>")

                html <- paste0(html,
                    "<p><i>This analysis was inspired by Orange Data Mining's interactive confusion matrix feature, ",
                    "adapted for static jamovi output with comprehensive statistical tables.</i></p>")

                self$results$misclassificationInterpretation$setContent(html)
            }


        )
    )
