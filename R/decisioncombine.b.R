#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent_format
#' @importFrom rlang .data
#' @return An \code{R6} class generator object for the \code{decisioncombineClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncombineClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .noticeList = list(),
            # A results item that may not exist in the compiled .h.R yet. jmvcore raises
            # "'<name>' does not exist in this results element" rather than returning NULL,
            # so a bare self$results$combinationTableCIRatios would crash every run between
            # the .r.yaml edit and the next jmvtools::prepare().
            .resultsItem = function(name) {
                tryCatch(self$results[[name]], error = function(e) NULL)
            },

            .addNotice = function(type, title, content) {
                notice <- list(
                    type = type,
                    title = title,
                    content = content
                )
                private$.noticeList[[length(private$.noticeList) + 1]] <- notice
            },
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                html <- '<div style="margin: 10px 0;">'
                for (notice in private$.noticeList) {
                    # STRONG_WARNING previously fell through to the INFO branch and was
                    # rendered as a blue informational note -- so "Gold Standard Has Only
                    # One Outcome" looked like a tip rather than a reason to distrust the
                    # numbers. ERROR also used the warning triangle; give it a stop sign so
                    # the three severities are visually distinct.
                    if (notice$type == "ERROR") {
                        color <- "#d9534f"
                        icon <- "&#x26D4;" # No-entry sign
                    } else if (notice$type == "STRONG_WARNING") {
                        color <- "#e8590c"
                        icon <- "&#x26A0;" # Warning sign
                    } else if (notice$type == "WARNING") {
                        color <- "#f0ad4e"
                        icon <- "&#x26A0;"
                    } else { # INFO
                        color <- "#5bc0de"
                        icon <- "&#x2139;" # Info sign
                    }

                    html <- paste0(
                        html,
                        '<div style="background-color: ', color, '; color: white; padding: 10px; margin: 5px 0; border-radius: 4px;">',
                        "<strong>", icon, " ", htmltools::htmlEscape(notice$title), ":</strong> ",
                        htmltools::htmlEscape(notice$content),
                        "</div>"
                    )
                }
                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },
            .extractEst = function(detail_df, stat_name, fallback = NA_real_) {
                # Robustly extract a single estimate from epiR::epi.tests()$detail.
                # Degrades gracefully (uses fallback) when the $detail shape differs
                # across epiR versions or a statistic key is absent / duplicated.
                val <- detail_df[detail_df$statistic == stat_name, "est"]
                if (length(val) != 1 || is.null(val)) {
                    return(fallback)
                }
                val
            },
            .safeProp = function(num, den) {
                # Zero-safe proportion used as a hand-computed fallback for epiR extraction.
                if (length(den) != 1 || is.na(den) || den == 0) {
                    return(NA_real_)
                }
                num / den
            },

            # TODO [meddecide audit 2026-05-14] - see docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md
            #   [hygiene/notices] custom private$.addNotice/private$.renderNotices duplicates jmvcore::Notice - consolidate
            #     reference impl: decisioncalculator.b.R (17 jmvcore::Notice uses)
            #   [hygiene/jmvcore] ~5 bare stop() calls - /jamovify-function decisioncombine --pattern=error --apply
            #   [hygiene/term] private$.escapeVariableNames (~L59) is similar to jmvcore::composeTerm - swap to jmvcore
            #   [integration] 72 declared outputs vs 21 setters (3.4×) - many pattern-specific placeholders
            #     run /check-function-full decisioncombine to verify 2-test/3-test scenarios
            #   [hygiene/notices] low-cell-count STRONG_WARNING is not quantified - add actual counts
            #   [i18n] 0 .() wraps; bootstrap jamovi/i18n/ then /prepare-translation decisioncombine
            #   [statistical-validation] /review-function decisioncombine - pattern enumeration math
            #   [testing] no tests/testthat/test-decisioncombine.R

            .init = function() {
                # Minimal initialization
                private$.noticeList <- list()

                # Control visibility of individual test performance groups
                showIndividual <- self$options$showIndividual

                # Test 1 visibility
                hasTest1 <- !is.null(self$options$test1) && self$options$test1 != ""
                self$results$individualTest1$setVisible(showIndividual && hasTest1)

                # Test 2 visibility
                hasTest2 <- !is.null(self$options$test2) && self$options$test2 != ""
                self$results$individualTest2$setVisible(showIndividual && hasTest2)

                # Test 3 visibility
                hasTest3 <- !is.null(self$options$test3) && self$options$test3 != ""
                self$results$individualTest3$setVisible(showIndividual && hasTest3)

                # Initialize fixed-structure tables for Test 1, 2, 3
                for (i in seq_len(3L)) {
                    group <- self$results[[paste0("individualTest", i)]]
                    contTable <- group[[paste0("test", i, "Contingency")]]
                    statsTable <- group[[paste0("test", i, "Stats")]]
                    
                    contTable$addRow(rowKey = "Positive", values = list(testResult = .("Test Positive")))
                    contTable$addRow(rowKey = "Negative", values = list(testResult = .("Test Negative")))
                    contTable$addRow(rowKey = "Total", values = list(testResult = .("Total")))
                    
                    statsTable$addRow(rowKey = "sens", values = list(statistic = .("Sensitivity")))
                    statsTable$addRow(rowKey = "spec", values = list(statistic = .("Specificity")))
                    statsTable$addRow(rowKey = "ppv", values = list(statistic = .("PPV")))
                    statsTable$addRow(rowKey = "npv", values = list(statistic = .("NPV")))
                }
            },
            .run = function() {
                # Main analysis flow - fail fast approach

                # Reset notices at start of each run to prevent accumulation across re-runs
                private$.noticeList <- list()

                # .run() has three early returns (failed validation, failed data prep, and
                # incomplete variable selection). Rendering only at the bottom meant every
                # notice explaining WHY the analysis stopped -- "Missing Level",
                # "No Complete Cases", every validation error -- was collected and then
                # discarded, leaving the user with a blank analysis and no message. on.exit
                # covers every exit path, including ones added later.
                on.exit(private$.renderNotices(), add = TRUE)

                # Check if we have minimum required variables
                if (!private$.hasRequiredVars()) {
                    return()
                }

                # Step 1: Validate inputs (will stop on errors)
                validation_result <- private$.validateInputs()
                if (!validation_result) {
                    return() # Halt execution if validation failed
                }

                # Step 2: Prepare data
                data_prep <- private$.prepareData()

                # Halt if data preparation failed
                if (is.null(data_prep)) {
                    return()
                }

                # Step 3: Individual test analysis (if requested)
                if (self$options$showIndividual) {
                    if (!is.null(self$options$test1) && self$options$test1 != "") {
                        private$.analyzeIndividualTest(data_prep, 1)
                    }
                    if (!is.null(self$options$test2) && self$options$test2 != "") {
                        private$.analyzeIndividualTest(data_prep, 2)
                    }
                    if (!is.null(self$options$test3) && self$options$test3 != "") {
                        private$.analyzeIndividualTest(data_prep, 3)
                    }
                }

                # Step 4: Combination analysis
                private$.analyzeCombinations(data_prep)

                # Step 5: Populate frequency tables (if requested)
                if (self$options$showFrequency) {
                    private$.populateFrequencyTables(data_prep)
                }

                # Step 6: Populate recommendation (if requested)
                if (self$options$showRecommendation) {
                    private$.populateRecommendation()
                }

                # Step 7: Add pattern to data (if requested)
                if (self$options$addPatternToData) {
                    private$.addPatternColumn(data_prep)
                }

                # Notices are rendered by the on.exit handler registered above.
            },
            .hasRequiredVars = function() {
                # Check if minimum required variables are selected
                # Returns FALSE to silently skip analysis, not throw error

                if (is.null(self$data) || nrow(self$data) == 0) {
                    return(FALSE)
                }

                if (length(self$options$gold) == 0 || self$options$gold == "") {
                    return(FALSE)
                }

                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    return(FALSE)
                }

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    return(FALSE)
                }

                if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                    return(FALSE)
                }

                return(TRUE)
            },
            .validateInputs = function() {
                # Strict validation with clear error messages using HTML notices
                # Returns TRUE if validation passes, FALSE otherwise

                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice("ERROR", "No Data", "No data available. Please load data before running analysis.")
                    return(FALSE)
                }

                if (length(self$options$gold) == 0 || self$options$gold == "") {
                    private$.addNotice("ERROR", "No Gold Standard", "Gold standard variable is required. Please select a reference test.")
                    return(FALSE)
                }

                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    private$.addNotice("ERROR", "No Gold Positive Level", "Please select the disease present level for the gold standard.")
                    return(FALSE)
                }

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    private$.addNotice("ERROR", "No Test 1", "Test 1 is required. Please select at least one test variable.")
                    return(FALSE)
                }

                if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                    private$.addNotice("ERROR", "No Test 1 Positive Level", "Please select the positive level for Test 1.")
                    return(FALSE)
                }

                # Check if we have at least 2 tests for combination analysis
                has_test2 <- !is.null(self$options$test2) && self$options$test2 != ""

                if (has_test2) {
                    if (is.null(self$options$test2Positive) || self$options$test2Positive == "") {
                        private$.addNotice("ERROR", "No Test 2 Positive Level", "Please select the positive level for Test 2.")
                        return(FALSE)
                    }
                }

                # Check test3 only if provided
                has_test3 <- !is.null(self$options$test3) && self$options$test3 != ""
                if (has_test3) {
                    if (is.null(self$options$test3Positive) || self$options$test3Positive == "") {
                        private$.addNotice("ERROR", "No Test 3 Positive Level", "Please select the positive level for Test 3.")
                        return(FALSE)
                    }
                }

                # Minimum data requirement
                if (nrow(self$data) < 4) {
                    private$.addNotice("ERROR", "Insufficient Data", "Insufficient data: At least 4 cases are required for analysis.")
                    return(FALSE)
                }

                return(TRUE) # All validation checks passed
            },
            .prepareData = function() {
                # Data preparation following decision.b.R pattern

                # Get variable names
                goldVar <- self$options$gold
                test1Var <- self$options$test1

                # Collect all variables needed
                vars_needed <- c(goldVar, test1Var)

                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    vars_needed <- c(vars_needed, self$options$test2)
                }

                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    vars_needed <- c(vars_needed, self$options$test3)
                }

                # Get subset of data
                subset_data <- self$data[, vars_needed, drop = FALSE]

                # Remove NAs
                n_before <- nrow(subset_data)
                mydata <- jmvcore::naOmit(subset_data)
                n_after <- nrow(mydata)

                if (nrow(mydata) == 0) {
                    private$.addNotice("ERROR", "No Complete Cases", "No complete cases available after removing missing data.")
                    return(NULL)
                }

                # Cases were being dropped with no disclosure at all: every statistic below
                # was computed on the complete cases while the user still saw the dataset's
                # full size. Say how many went and why.
                if (n_after < n_before) {
                    n_removed <- n_before - n_after
                    private$.addNotice(
                        "WARNING",
                        sprintf("Removed %d case(s) with missing values", n_removed),
                        sprintf("Complete-case analysis uses %d of %d cases (%.1f%%). Cases missing the gold standard or any selected test were excluded. If the data are not missing completely at random this can bias every estimate below; consider investigating the pattern of missingness.",
                                n_after, n_before, 100 * n_after / n_before)
                    )
                }

                # Convert to factors
                for (var in vars_needed) {
                    mydata[[var]] <- forcats::as_factor(mydata[[var]])
                }

                # Validate that specified positive levels exist in data
                required_levels <- list(
                    gold = list(var = goldVar, level = self$options$goldPositive, label = "gold standard"),
                    test1 = list(var = test1Var, level = self$options$test1Positive, label = "Test 1")
                )

                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    required_levels$test2 <- list(var = self$options$test2, level = self$options$test2Positive, label = "Test 2")
                }
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    required_levels$test3 <- list(var = self$options$test3, level = self$options$test3Positive, label = "Test 3")
                }

                for (rl in required_levels) {
                    if (!rl$level %in% levels(mydata[[rl$var]])) {
                        private$.addNotice(
                            "ERROR", "Missing Level",
                            sprintf(
                                'The specified positive level "%s" is not present in variable "%s" (%s). Please select a level that exists in the data.',
                                rl$level, rl$var, rl$label
                            )
                        )
                        return(NULL)
                    }
                }

                # A gold standard with only one observed level cannot support specificity
                # or NPV (there are no true negatives, or no true positives). Those came
                # back as a bare NA with nothing to explain them.
                gold_levels_present <- unique(stats::na.omit(as.character(mydata[[goldVar]])))
                if (length(gold_levels_present) < 2) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        "Gold Standard Has Only One Outcome",
                        sprintf('Every complete case has gold standard "%s", so this sample contains no %s cases. Sensitivity and PPV (or specificity and NPV) cannot be estimated and are reported as blank. Diagnostic accuracy needs both diseased and non-diseased cases.',
                                gold_levels_present[1],
                                if (identical(gold_levels_present[1], self$options$goldPositive)) "disease-absent" else "disease-present")
                    )
                }

                # Anything that is not the chosen positive level becomes "Negative" below.
                # For a variable with more than two levels that silently folds equivocal or
                # third-category results into the negative arm, which inflates specificity
                # and NPV exactly as it would in decision/decisioncompare.
                for (rl in required_levels) {
                    lv <- levels(mydata[[rl$var]])
                    if (length(lv) > 2) {
                        others <- setdiff(lv, rl$level)
                        shown <- if (length(others) <= 5) paste(others, collapse = ", ")
                                 else paste(c(others[1:5], "..."), collapse = ", ")
                        private$.addNotice(
                            "STRONG_WARNING",
                            sprintf("%s has %d levels", rl$var, length(lv)),
                            sprintf('Variable "%s" (%s) has %d levels: %s. Only "%s" is treated as positive; every other level (%s) is counted as NEGATIVE. If any of those represent equivocal or indeterminate results this inflates specificity and NPV. Recode the variable to two levels, setting equivocal results to missing, if that is not what you intend.',
                                    rl$var, rl$label, length(lv), paste(lv, collapse = ", "),
                                    rl$level, shown)
                        )
                    }
                }

                # Recode gold standard
                mydata <- mydata %>%
                    dplyr::mutate(
                        goldVariable2 = dplyr::case_when(
                            is.na(.data[[goldVar]]) ~ NA_character_,
                            .data[[goldVar]] == self$options$goldPositive ~ "Positive",
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        goldVariable2 = factor(goldVariable2, levels = c("Positive", "Negative"))
                    )

                # Recode Test 1
                mydata <- mydata %>%
                    dplyr::mutate(
                        test1Variable2 = dplyr::case_when(
                            is.na(.data[[test1Var]]) ~ NA_character_,
                            .data[[test1Var]] == self$options$test1Positive ~ "Positive",
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        test1Variable2 = factor(test1Variable2, levels = c("Positive", "Negative"))
                    )

                # Recode Test 2 if present
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    test2Var <- self$options$test2
                    mydata <- mydata %>%
                        dplyr::mutate(
                            test2Variable2 = dplyr::case_when(
                                is.na(.data[[test2Var]]) ~ NA_character_,
                                .data[[test2Var]] == self$options$test2Positive ~ "Positive",
                                TRUE ~ "Negative"
                            )
                        ) %>%
                        dplyr::mutate(
                            test2Variable2 = factor(test2Variable2, levels = c("Positive", "Negative"))
                        )
                }

                # Recode Test 3 if present
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    test3Var <- self$options$test3
                    mydata <- mydata %>%
                        dplyr::mutate(
                            test3Variable2 = dplyr::case_when(
                                is.na(.data[[test3Var]]) ~ NA_character_,
                                .data[[test3Var]] == self$options$test3Positive ~ "Positive",
                                TRUE ~ "Negative"
                            )
                        ) %>%
                        dplyr::mutate(
                            test3Variable2 = factor(test3Variable2, levels = c("Positive", "Negative"))
                        )
                }

                return(mydata)
            },
            .analyzeIndividualTest = function(data_prep, test_num) {
                # Analyze individual test performance

                # Check if epiR package is available
                if (!requireNamespace("epiR", quietly = TRUE)) {
                    private$.addNotice(
                        "ERROR", "epiR Package Missing",
                        'epiR package is required for diagnostic test analysis. Install with install.packages("epiR") or disable "Show Individual Test Statistics" option.'
                    )
                    return()
                }

                test_var_name <- paste0("test", test_num, "Variable2")

                if (!test_var_name %in% names(data_prep)) {
                    return()
                }

                # Create contingency table
                cont_table <- table(data_prep[[test_var_name]], data_prep$goldVariable2)

                # Validate table structure
                if (!all(dim(cont_table) == c(2, 2))) {
                    return()
                }

                # Extract counts
                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                # Validate counts
                if (any(is.na(c(tp, fp, fn, tn))) || any(c(tp, fp, fn, tn) < 0)) {
                    private$.addNotice(
                        "WARNING", "Invalid Counts",
                        sprintf("Invalid counts detected for Test %d. Skipping individual analysis.", test_num)
                    )
                    return()
                }

                # Check if all counts are zero
                if (tp == 0 && fp == 0 && fn == 0 && tn == 0) {
                    private$.addNotice(
                        "WARNING", "All Zero Counts",
                        sprintf("No valid observations for Test %d. Skipping individual analysis.", test_num)
                    )
                    return()
                }

                # Individual-test statistics are proportions (sens/spec/PPV/NPV) that remain
                # well-defined with zero cells, so point estimates are computed on the raw
                # (unadjusted) contingency table -- mirroring the combination-pattern path and
                # keeping the displayed statistics consistent with the integer table above.
                # (No LR/DOR/CI are reported here, so no continuity correction is required.)

                # Get results tables
                if (test_num == 1) {
                    contTable <- self$results$individualTest1$test1Contingency
                    statsTable <- self$results$individualTest1$test1Stats
                } else if (test_num == 2) {
                    contTable <- self$results$individualTest2$test2Contingency
                    statsTable <- self$results$individualTest2$test2Stats
                } else {
                    contTable <- self$results$individualTest3$test3Contingency
                    statsTable <- self$results$individualTest3$test3Stats
                }

                # Populate contingency table
                contTable$setRow(rowKey = "Positive", values = list(
                    goldPos = tp,
                    goldNeg = fp,
                    total = tp + fp
                ))
                contTable$setRow(rowKey = "Negative", values = list(
                    goldPos = fn,
                    goldNeg = tn,
                    total = fn + tn
                ))
                contTable$setRow(rowKey = "Total", values = list(
                    goldPos = tp + fn,
                    goldNeg = fp + tn,
                    total = tp + fp + fn + tn
                ))

                # Calculate statistics using epiR
                result <- epiR::epi.tests(cont_table, conf.level = 0.95)

                # Extract values - epiR returns $detail as a data frame
                # Guard against epiR $detail shape differences with hand-computed fallbacks
                detail_df <- as.data.frame(result$detail)

                sens <- private$.extractEst(detail_df, "se", private$.safeProp(tp, tp + fn))
                spec <- private$.extractEst(detail_df, "sp", private$.safeProp(tn, fp + tn))
                ppv <- private$.extractEst(detail_df, "pv.pos", private$.safeProp(tp, tp + fp))
                npv <- private$.extractEst(detail_df, "pv.neg", private$.safeProp(tn, fn + tn))

                # Populate statistics table
                statsTable$setRow(rowKey = "sens", values = list(
                    estimate = sens
                ))
                statsTable$setRow(rowKey = "spec", values = list(
                    estimate = spec
                ))
                statsTable$setRow(rowKey = "ppv", values = list(
                    estimate = ppv
                ))
                statsTable$setRow(rowKey = "npv", values = list(
                    estimate = npv
                ))
            },
            .analyzeCombinations = function(data_prep) {
                # Generate and analyze all test combinations

                # The columns are headed "Sensitivity"/"Specificity"/"PPV"/"NPV" for every
                # row, but an exhaustive pattern row ("+/-") is not a decision rule -- its
                # "sensitivity" is P(this exact pattern | diseased). Only the strategy rows
                # are rules you could actually apply to a patient. Say so.
                self$results$combinationTable$setNote(
                    "row_kinds",
                    jmvcore::.("Rows whose label is a result pattern (e.g. \"+/-\") are mutually exclusive groups, not decision rules: for those rows \"Sensitivity\" is the proportion of diseased patients showing that exact pattern, and the columns should be read that way. The named rows -- Parallel (>=1 pos), Majority (>=2/3 pos), and the all-positive pattern, which is the Serial (AND) rule -- are the strategies you can apply to a patient.")
                )
                self$results$combinationTable$setNote(
                    "haldane",
                    jmvcore::.("LR+, LR- and the diagnostic odds ratio are computed with a Haldane-Anscombe 0.5 continuity correction when a cell is zero, so they stay finite; sensitivity, specificity, PPV and NPV on the same row use the observed counts. The two therefore need not agree exactly at a zero cell.")
                )
                self$results$combinationTable$setNote(
                    "multiplicity",
                    jmvcore::.("Every pattern and strategy is reported together with no adjustment for multiple comparisons. Treat the best-looking row as a hypothesis to confirm in new data, not as an established result.")
                )

                # Both tables are filled with addRow(), so they must be emptied first.
                # jamovi re-runs .run() on the SAME analysis object whenever any option
                # changes, and without this the pattern rows accumulated on every re-run
                # (5 -> 10 -> 15) until the duplicated rowKeys made $asDF fail outright
                # with "duplicate 'row.names' are not allowed", taking the recommendation
                # and every plot down with it.
                self$results$combinationTable$deleteRows()
                self$results$combinationTableCI$deleteRows()
                ratios_tbl <- private$.resultsItem("combinationTableCIRatios")
                if (!is.null(ratios_tbl)) ratios_tbl$deleteRows()

                # Check if epiR package is available for combination analysis
                if (!requireNamespace("epiR", quietly = TRUE)) {
                    private$.addNotice(
                        "ERROR", "epiR Package Missing",
                        'epiR package is required for combination analysis. Install with install.packages("epiR").'
                    )
                    return()
                }

                # Inform users that PPV/NPV are based on sample prevalence
                private$.addNotice(
                    "INFO", "PPV/NPV Interpretation",
                    "Positive/Negative Predictive Values are calculated using the sample prevalence. Interpret cautiously if your sample does not reflect the target clinical population."
                )

                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test only - no combinations
                    private$.analyzeSinglePattern(
                        data_prep, "Test 1",
                        data_prep$test1Variable2 == "Positive",
                        row_type = "Single test"
                    )
                    return()
                }

                if (!has_test3) {
                    # Two-test combinations (4 patterns)
                    private$.analyzeTwoTestPatterns(data_prep)
                    # Add clinical strategies for 2 tests
                    private$.addTwoTestStrategies(data_prep)
                } else {
                    # Three-test combinations (8 patterns)
                    private$.analyzeThreeTestPatterns(data_prep)
                    # Add clinical strategies for 3 tests
                    private$.addThreeTestStrategies(data_prep)
                }
            },
            .calcWilsonCI = function(x, n, conf.level = 0.95) {
                # Wilson score confidence interval
                # More accurate than normal approximation, especially for small samples
                if (is.na(x) || n == 0) {
                    return(c(NA, NA))
                }

                p <- x / n
                z <- qnorm((1 + conf.level) / 2) # 1.96 for 95% CI

                # Wilson score formula
                denominator <- 1 + (z^2 / n)
                centre <- (p + (z^2 / (2 * n))) / denominator
                half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator

                # Return bounds, constrained to [0, 1]
                c(max(0, centre - half_width), min(1, centre + half_width))
            },
            .analyzeSinglePattern = function(data_prep, pattern_name, condition,
                                             row_type = "Pattern") {
                # Analyze a single test pattern

                # Create binary variable for this pattern
                data_prep$pattern_result <- ifelse(condition, "Positive", "Negative")
                data_prep$pattern_result <- factor(
                    data_prep$pattern_result,
                    levels = c("Positive", "Negative")
                )

                # Create contingency table
                cont_table <- table(data_prep$pattern_result, data_prep$goldVariable2)

                if (!all(dim(cont_table) == c(2, 2))) {
                    private$.addNotice(
                        "INFO", "Pattern Omitted",
                        sprintf('Pattern "%s" produced no variation (an empty result cell) and was omitted from the combination results.', pattern_name)
                    )
                    return()
                }

                # Extract counts
                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                # Validate counts - check for negative values
                if (any(is.na(c(tp, fp, fn, tn))) || any(c(tp, fp, fn, tn) < 0)) {
                    private$.addNotice(
                        "WARNING", "Invalid Counts",
                        sprintf('Invalid counts detected for pattern "%s". Skipping this combination.', pattern_name)
                    )
                    return()
                }

                # Check if all counts are zero
                if (tp == 0 && fp == 0 && fn == 0 && tn == 0) {
                    private$.addNotice(
                        "WARNING", "All Zero Counts",
                        sprintf('No observations found for pattern "%s". Skipping this combination.', pattern_name)
                    )
                    return()
                }

                # Apply continuity correction if any cell is zero (except when all are zero)
                # This prevents Inf/NaN in likelihood ratios and allows valid CIs
                use_continuity <- any(c(tp, fp, fn, tn) == 0)
                if (use_continuity) {
                    tp_adj <- tp + 0.5
                    fp_adj <- fp + 0.5
                    fn_adj <- fn + 0.5
                    tn_adj <- tn + 0.5
                    # Post informative notice
                    private$.addNotice(
                        "INFO", "Continuity Correction",
                        sprintf('Continuity correction (+0.5) applied to pattern "%s" due to zero cell count(s).', pattern_name)
                    )
                } else {
                    tp_adj <- tp
                    fp_adj <- fp
                    fn_adj <- fn
                    tn_adj <- tn
                }

                # Calculate statistics using epiR
                result <- epiR::epi.tests(cont_table, conf.level = 0.95)

                # Extract values - epiR returns $detail as a data frame
                # Guard against epiR $detail shape differences with hand-computed fallbacks
                detail_df <- as.data.frame(result$detail)

                sens <- private$.extractEst(detail_df, "se", private$.safeProp(tp, tp + fn))
                spec <- private$.extractEst(detail_df, "sp", private$.safeProp(tn, fp + tn))
                ppv <- private$.extractEst(detail_df, "pv.pos", private$.safeProp(tp, tp + fp))
                npv <- private$.extractEst(detail_df, "pv.neg", private$.safeProp(tn, fn + tn))
                acc <- (tp + tn) / (tp + fp + fn + tn)

                # Calculate Wilson CIs for all metrics
                total_pos <- tp + fn
                total_neg <- fp + tn
                total_test_pos <- tp + fp
                total_test_neg <- fn + tn
                total <- tp + fp + fn + tn

                sens_ci <- private$.calcWilsonCI(tp, total_pos)
                spec_ci <- private$.calcWilsonCI(tn, total_neg)
                ppv_ci <- private$.calcWilsonCI(tp, total_test_pos)
                npv_ci <- private$.calcWilsonCI(tn, total_test_neg)
                acc_ci <- private$.calcWilsonCI(tp + tn, total)

                # Calculate additional metrics using adjusted counts for LR/DOR
                n <- tp + fp + fn + tn
                prev <- (tp + fn) / n
                balanced_acc <- (sens + spec) / 2
                youden_j <- sens + spec - 1

                # Calculate sensitivity and specificity from adjusted counts for LR/DOR
                sens_adj <- tp_adj / (tp_adj + fn_adj)
                spec_adj <- tn_adj / (fp_adj + tn_adj)

                # Likelihood ratios using adjusted counts (prevents Inf/NaN)
                lr_pos <- sens_adj / (1 - spec_adj)
                lr_neg <- (1 - sens_adj) / spec_adj

                # Diagnostic odds ratio using adjusted counts
                dor <- (tp_adj * tn_adj) / (fp_adj * fn_adj)

                # Add to main table
                combTable <- self$results$combinationTable
                combTable$addRow(rowKey = pattern_name, values = list(
                    pattern = pattern_name,
                    # A "+/-" row is a mutually exclusive group of patients, not a rule you
                    # can apply: its "Sensitivity" is P(this exact pattern | diseased). The
                    # Strategy rows are the rules. The columns read very differently for the
                    # two, and nothing used to distinguish them.
                    rowType = row_type,
                    tp = tp,
                    fp = fp,
                    fn = fn,
                    tn = tn,
                    prevalence = prev,
                    sens = sens,
                    spec = spec,
                    ppv = ppv,
                    npv = npv,
                    acc = acc,
                    balancedAccuracy = balanced_acc,
                    youden = youden_j,
                    lrPos = lr_pos,
                    lrNeg = lr_neg,
                    dor = dor
                ))

                # Populate CI table with Wilson score intervals
                ciTable <- self$results$combinationTableCI
                # LR+, LR- and DOR are unbounded ratios, not proportions; they share no
                # sensible column format with sensitivity, so they get their own table.
                ratioTable <- private$.resultsItem("combinationTableCIRatios")

                # Sensitivity with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_sens"), values = list(
                    pattern = pattern_name,
                    statistic = "Sensitivity",
                    estimate = sens,
                    lower = sens_ci[1],
                    upper = sens_ci[2]
                ))

                # Specificity with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_spec"), values = list(
                    pattern = pattern_name,
                    statistic = "Specificity",
                    estimate = spec,
                    lower = spec_ci[1],
                    upper = spec_ci[2]
                ))

                # PPV with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_ppv"), values = list(
                    pattern = pattern_name,
                    statistic = "PPV",
                    estimate = ppv,
                    lower = ppv_ci[1],
                    upper = ppv_ci[2]
                ))

                # NPV with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_npv"), values = list(
                    pattern = pattern_name,
                    statistic = "NPV",
                    estimate = npv,
                    lower = npv_ci[1],
                    upper = npv_ci[2]
                ))

                # Accuracy with CI
                ciTable$addRow(rowKey = paste0(pattern_name, "_acc"), values = list(
                    pattern = pattern_name,
                    statistic = "Accuracy",
                    estimate = acc,
                    lower = acc_ci[1],
                    upper = acc_ci[2]
                ))

                # LR+ with CI (log-scale transformation for CI, using adjusted counts)
                if (!is.na(lr_pos) && lr_pos > 0) {
                    log_lr_pos <- log(lr_pos)
                    # Standard SE for log(LR+) using adjusted counts
                    se_log_lr_pos <- sqrt((1 / tp_adj) - (1 / (tp_adj + fn_adj)) +
                        (1 / fp_adj) - (1 / (fp_adj + tn_adj)))
                    lr_pos_lower <- exp(log_lr_pos - 1.96 * se_log_lr_pos)
                    lr_pos_upper <- exp(log_lr_pos + 1.96 * se_log_lr_pos)
                } else {
                    lr_pos_lower <- NA
                    lr_pos_upper <- NA
                }
                if (!is.null(ratioTable)) ratioTable$addRow(rowKey = paste0(pattern_name, "_lrPos"), values = list(
                    pattern = pattern_name,
                    statistic = "LR+",
                    estimate = lr_pos,
                    lower = lr_pos_lower,
                    upper = lr_pos_upper
                ))

                # LR- with CI (log-scale transformation for CI, using adjusted counts)
                if (!is.na(lr_neg) && lr_neg > 0) {
                    log_lr_neg <- log(lr_neg)
                    # Standard SE for log(LR-) using adjusted counts
                    se_log_lr_neg <- sqrt((1 / fn_adj) - (1 / (tp_adj + fn_adj)) +
                        (1 / tn_adj) - (1 / (fp_adj + tn_adj)))
                    lr_neg_lower <- exp(log_lr_neg - 1.96 * se_log_lr_neg)
                    lr_neg_upper <- exp(log_lr_neg + 1.96 * se_log_lr_neg)
                } else {
                    lr_neg_lower <- NA
                    lr_neg_upper <- NA
                }
                if (!is.null(ratioTable)) ratioTable$addRow(rowKey = paste0(pattern_name, "_lrNeg"), values = list(
                    pattern = pattern_name,
                    statistic = "LR-",
                    estimate = lr_neg,
                    lower = lr_neg_lower,
                    upper = lr_neg_upper
                ))

                # DOR with CI (log-scale transformation for CI, using adjusted counts)
                if (!is.na(dor) && dor > 0) {
                    log_dor <- log(dor)
                    # Approximate SE for log(DOR) using adjusted counts
                    se_log_dor <- sqrt(1 / tp_adj + 1 / fp_adj + 1 / fn_adj + 1 / tn_adj)
                    dor_lower <- exp(log_dor - 1.96 * se_log_dor)
                    dor_upper <- exp(log_dor + 1.96 * se_log_dor)
                } else {
                    dor_lower <- NA
                    dor_upper <- NA
                }
                if (!is.null(ratioTable)) ratioTable$addRow(rowKey = paste0(pattern_name, "_dor"), values = list(
                    pattern = pattern_name,
                    statistic = "DOR",
                    estimate = dor,
                    lower = dor_lower,
                    upper = dor_upper
                ))
            },
            .analyzeTwoTestPatterns = function(data_prep) {
                # Generate 4 patterns for 2-test combinations

                patterns <- list(
                    "+/+" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive",
                    "+/-" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative",
                    "-/+" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive",
                    "-/-" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative"
                )

                for (pattern_name in names(patterns)) {
                    private$.analyzeSinglePattern(data_prep, pattern_name, patterns[[pattern_name]])
                }
            },
            .analyzeThreeTestPatterns = function(data_prep) {
                # Generate 8 patterns for 3-test combinations

                patterns <- list(
                    "+/+/+" = data_prep$test1Variable2 == "Positive" &
                        data_prep$test2Variable2 == "Positive" &
                        data_prep$test3Variable2 == "Positive",
                    "+/+/-" = data_prep$test1Variable2 == "Positive" &
                        data_prep$test2Variable2 == "Positive" &
                        data_prep$test3Variable2 == "Negative",
                    "+/-/+" = data_prep$test1Variable2 == "Positive" &
                        data_prep$test2Variable2 == "Negative" &
                        data_prep$test3Variable2 == "Positive",
                    "+/-/-" = data_prep$test1Variable2 == "Positive" &
                        data_prep$test2Variable2 == "Negative" &
                        data_prep$test3Variable2 == "Negative",
                    "-/+/+" = data_prep$test1Variable2 == "Negative" &
                        data_prep$test2Variable2 == "Positive" &
                        data_prep$test3Variable2 == "Positive",
                    "-/+/-" = data_prep$test1Variable2 == "Negative" &
                        data_prep$test2Variable2 == "Positive" &
                        data_prep$test3Variable2 == "Negative",
                    "-/-/+" = data_prep$test1Variable2 == "Negative" &
                        data_prep$test2Variable2 == "Negative" &
                        data_prep$test3Variable2 == "Positive",
                    "-/-/-" = data_prep$test1Variable2 == "Negative" &
                        data_prep$test2Variable2 == "Negative" &
                        data_prep$test3Variable2 == "Negative"
                )

                for (pattern_name in names(patterns)) {
                    private$.analyzeSinglePattern(data_prep, pattern_name, patterns[[pattern_name]])
                }
            },
            .addTwoTestStrategies = function(data_prep) {
                # Add clinical strategy rows for 2-test combinations

                # Parallel strategy: Positive if ANY test is positive (high sensitivity)
                parallel_condition <- data_prep$test1Variable2 == "Positive" |
                    data_prep$test2Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Parallel (>=1 pos)", parallel_condition, row_type = "Strategy")

                # Serial (AND) is numerically identical to the all-positive pattern "+/+",
                # but a reader should not have to know that to find it. It gets its own
                # named row; .populateRecommendation de-duplicates candidates by their 2x2
                # counts so the twin does not become a spurious tie.
                serial_condition <- data_prep$test1Variable2 == "Positive" &
                    data_prep$test2Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Serial (all pos)", serial_condition, row_type = "Strategy")
            },
            .addThreeTestStrategies = function(data_prep) {
                # Add clinical strategy rows for 3-test combinations

                # Parallel strategy: Positive if ANY test is positive (high sensitivity)
                parallel_condition <- data_prep$test1Variable2 == "Positive" |
                    data_prep$test2Variable2 == "Positive" |
                    data_prep$test3Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Parallel (>=1 pos)", parallel_condition, row_type = "Strategy")

                # Serial (AND): identical to "+/+/+" but named, for the same reason as above.
                serial_condition <- data_prep$test1Variable2 == "Positive" &
                    data_prep$test2Variable2 == "Positive" &
                    data_prep$test3Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Serial (all pos)", serial_condition, row_type = "Strategy")

                # Majority rule: Positive if at least 2 of 3 tests are positive (balanced)
                t1_pos <- data_prep$test1Variable2 == "Positive"
                t2_pos <- data_prep$test2Variable2 == "Positive"
                t3_pos <- data_prep$test3Variable2 == "Positive"
                majority_condition <- (as.integer(t1_pos) + as.integer(t2_pos) + as.integer(t3_pos)) >= 2
                private$.analyzeSinglePattern(data_prep, "Majority (>=2/3 pos)", majority_condition, row_type = "Strategy")
            },
            .populateFrequencyTables = function(data_prep) {
                # Emptied for the same reason as the combination tables above.
                self$results$goldFreqTable$deleteRows()
                self$results$crossTabTable$deleteRows()

                # Gold standard frequency
                goldTable <- self$results$goldFreqTable
                gold_freq <- table(data_prep$goldVariable2)
                total <- sum(gold_freq)

                for (level in names(gold_freq)) {
                    goldTable$addRow(rowKey = level, values = list(
                        level = level,
                        count = as.integer(gold_freq[level]),
                        percent = as.numeric(gold_freq[level]) / total
                    ))
                }

                # Cross-tabulation
                crossTable <- self$results$crossTabTable
                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    return()
                }

                # Generate patterns
                if (!has_test3) {
                    patterns <- list(
                        "+/+" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive",
                        "+/-" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative",
                        "-/+" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive",
                        "-/-" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative"
                    )
                } else {
                    patterns <- list(
                        "+/+/+" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Positive",
                        "+/+/-" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Negative",
                        "+/-/+" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Positive",
                        "+/-/-" = data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Negative",
                        "-/+/+" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Positive",
                        "-/+/-" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Negative",
                        "-/-/+" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Positive",
                        "-/-/-" = data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Negative"
                    )
                }

                for (pattern_name in names(patterns)) {
                    pattern_data <- data_prep[patterns[[pattern_name]], ]
                    gold_pos <- sum(pattern_data$goldVariable2 == "Positive", na.rm = TRUE)
                    gold_neg <- sum(pattern_data$goldVariable2 == "Negative", na.rm = TRUE)

                    crossTable$addRow(rowKey = pattern_name, values = list(
                        testCombo = pattern_name,
                        goldPos = gold_pos,
                        goldNeg = gold_neg,
                        total = gold_pos + gold_neg
                    ))
                }
            },
            .populateRecommendation = function() {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) {
                    return()
                }

                # Convert to data frame
                table_df <- combTable$asDF

                # Find pattern with highest Youden's J
                # Prefer patterns with reasonable cell counts to avoid unstable choices
                table_df$min_cell <- apply(table_df[, c("tp", "fp", "fn", "tn")], 1, min)

                # "Serial (all pos)" and the all-positive pattern ("+/+" / "+/+/+") are the
                # same rule under two names. Counting both would manufacture a tie and
                # inflate the number of candidates reported below, so collapse rows that
                # share a 2x2 -- keeping the named strategy, which is the more useful label.
                signature <- paste(table_df$tp, table_df$fp, table_df$fn, table_df$tn, sep = "|")
                is_strategy <- if ("rowType" %in% names(table_df)) {
                    table_df$rowType %in% c("Strategy", "Single test")
                } else rep(FALSE, nrow(table_df))
                # Sort strategies first within each signature, then drop the duplicates, so
                # the surviving row carries the named-strategy label rather than "+/+/+".
                table_df <- table_df[order(signature, !is_strategy), , drop = FALSE]
                table_df <- table_df[!duplicated(paste(table_df$tp, table_df$fp,
                                                       table_df$fn, table_df$tn,
                                                       sep = "|")), , drop = FALSE]

                stable_df <- table_df[table_df$min_cell >= 5, ]
                if (nrow(stable_df) == 0) {
                    stable_df <- table_df # fallback to all if none meet threshold
                    stability_note <- "No pattern meets the minimum cell count of 5; recommendation is based on all patterns (may be unstable). "
                } else {
                    stability_note <- ""
                }

                max_youden_idx <- which.max(stable_df$youden)
                best_pattern <- stable_df[max_youden_idx, ]

                # An exact tie was previously broken by whichever row came first, silently.
                tied <- stable_df$pattern[
                    !is.na(stable_df$youden) &
                        abs(stable_df$youden - best_pattern$youden) < 1e-9]
                tie_note <- if (length(tied) > 1) {
                    sprintf("%d rules tie on Youden's J (%s); \"%s\" is shown only because it comes first. ",
                            length(tied), paste(tied, collapse = ", "), best_pattern$pattern)
                } else ""

                # This is an argmax over every candidate rule with no confidence interval
                # and no test, so on data with no real signal it still names a winner. Say
                # how many rules competed, and whether the winner separates from the next
                # one by more than the width of its own interval.
                n_candidates <- nrow(stable_df)
                runner_up <- if (n_candidates > 1) {
                    sort(stable_df$youden, decreasing = TRUE)[2]
                } else NA_real_

                sel_note <- sprintf(
                    "Selected by ranking %d candidate rule(s); no significance test or multiplicity correction is applied to that comparison. ",
                    n_candidates
                )

                separation_note <- ""
                if (is.finite(runner_up)) {
                    bt <- best_pattern
                    tp <- bt$tp; fp <- bt$fp; fn <- bt$fn; tn <- bt$tn
                    sens_ci <- private$.calcWilsonCI(tp, tp + fn)
                    spec_ci <- private$.calcWilsonCI(tn, tn + fp)
                    # Youden's J = sens + spec - 1; a conservative interval for it is the
                    # sum of the two component intervals shifted by 1.
                    j_lower <- sens_ci[1] + spec_ci[1] - 1
                    if (is.finite(j_lower) && j_lower <= runner_up) {
                        separation_note <- sprintf(
                            "Its advantage is not established: the lower bound of this rule's Youden's J (%.3f) falls at or below the next best rule's point estimate (%.3f), so the ranking may reflect sampling variation rather than a real difference. ",
                            j_lower, runner_up)
                    }
                }

                # Generate rationale
                rationale <- sprintf(
                    "%s%s%sHighest Youden's J (%.3f) among the rules compared. %s",
                    stability_note,
                    tie_note,
                    sel_note,
                    best_pattern$youden,
                    separation_note
                )

                # Grade the estimate, not the certainty -- and only when it is not already
                # flagged as unseparated.
                if (best_pattern$sens > 0.8 && best_pattern$spec > 0.8) {
                    rationale <- paste0(rationale, "Observed sensitivity and specificity are both above 80%.")
                } else if (best_pattern$sens > 0.7 && best_pattern$spec > 0.7) {
                    rationale <- paste0(rationale, "Observed sensitivity and specificity are both above 70%.")
                } else {
                    rationale <- paste0(rationale, "Involves a trade-off between sensitivity and specificity.")
                }

                # Populate recommendation table
                recTable <- self$results$recommendationTable
                recTable$setRow(rowNo = 1, values = list(
                    pattern = best_pattern$pattern,
                    method = "Youden's Index (Sensitivity + Specificity - 1)",
                    youden = best_pattern$youden,
                    sens = best_pattern$sens,
                    spec = best_pattern$spec,
                    acc = best_pattern$acc,
                    rationale = rationale
                ))
            },
            .addPatternColumn = function(data_prep) {
                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test pattern
                    pattern_values <- ifelse(data_prep$test1Variable2 == "Positive", "+", "-")
                } else if (!has_test3) {
                    # Two-test pattern
                    t1 <- ifelse(data_prep$test1Variable2 == "Positive", "+", "-")
                    t2 <- ifelse(data_prep$test2Variable2 == "Positive", "+", "-")
                    pattern_values <- paste0(t1, "/", t2)
                } else {
                    # Three-test pattern
                    t1 <- ifelse(data_prep$test1Variable2 == "Positive", "+", "-")
                    t2 <- ifelse(data_prep$test2Variable2 == "Positive", "+", "-")
                    t3 <- ifelse(data_prep$test3Variable2 == "Positive", "+", "-")
                    pattern_values <- paste0(t1, "/", t2, "/", t3)
                }

                output <- self$results$addedPattern
                if (output$isNotFilled()) {
                    output$setRowNums(rownames(data_prep))
                    output$setValues(pattern_values)
                }
            },
            .plotBarChart = function(image, ...) {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) {
                    return(FALSE)
                }

                table_df <- combTable$asDF

                # Apply statistic filter
                stat_filter <- self$options$filterStatistic
                if (stat_filter != "all") {
                    metrics <- stat_filter
                } else {
                    metrics <- c("sens", "spec", "ppv", "npv", "acc")
                }

                # Apply pattern filter
                pattern_filter <- self$options$filterPattern
                filtered_df <- private$.applyPatternFilter(table_df, pattern_filter)

                if (nrow(filtered_df) == 0) {
                    return(FALSE)
                }

                # Create long format
                plot_data <- data.frame()
                for (metric in metrics) {
                    if (metric %in% names(filtered_df)) {
                        temp <- data.frame(
                            Pattern = filtered_df$pattern,
                            Metric = metric,
                            Value = filtered_df[[metric]],
                            stringsAsFactors = FALSE
                        )
                        plot_data <- rbind(plot_data, temp)
                    }
                }

                # Proportion metrics are bounded to [0, 1] and shown as percentages; unbounded
                # metrics (Youden's J, LR+, LR-, DOR) must use a free auto scale, otherwise the
                # fixed [0, 1] limit clips their bars to blank.
                proportion_metrics <- c("prevalence", "sens", "spec", "ppv", "npv", "acc", "balancedAccuracy")
                all_proportions <- all(metrics %in% proportion_metrics)

                # Create plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Pattern, y = Value, fill = Metric)) +
                    ggplot2::geom_bar(stat = "identity", position = "dodge") +
                    ggplot2::labs(
                        title = "Diagnostic Performance Comparison",
                        x = "Test Pattern",
                        y = "Value"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

                if (all_proportions) {
                    p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
                }

                print(p)
                return(TRUE)
            },
            .plotHeatmap = function(image, ...) {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) {
                    return(FALSE)
                }

                table_df <- combTable$asDF
                pattern_filter <- self$options$filterPattern
                filtered_df <- private$.applyPatternFilter(table_df, pattern_filter)

                if (nrow(filtered_df) == 0) {
                    return(FALSE)
                }

                # Select metrics for heatmap; honor a single-statistic filter when the selected
                # statistic is part of the heatmap panel, otherwise show the full panel
                # (prevalence/LR+/LR-/DOR are not on the heatmap and leave it unfiltered).
                metrics <- c("sens", "spec", "ppv", "npv", "acc", "balancedAccuracy", "youden")
                stat_filter <- self$options$filterStatistic
                if (stat_filter != "all" && stat_filter %in% metrics) {
                    metrics <- stat_filter
                }
                metric_data <- filtered_df[, c("pattern", metrics), drop = FALSE]

                # Reshape to long format
                plot_data <- tidyr::pivot_longer(
                    metric_data,
                    cols = -pattern,
                    names_to = "Metric",
                    values_to = "Value"
                )

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Metric, y = pattern, fill = Value)) +
                    ggplot2::geom_tile() +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Value)), color = "white") +
                    ggplot2::scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5) +
                    ggplot2::labs(title = "Performance Heatmap", x = "", y = "Pattern") +
                    ggplot2::theme_minimal()

                print(p)
                return(TRUE)
            },
            .plotForest = function(image, ...) {
                ciTable <- self$results$combinationTableCI
                ratioTable <- private$.resultsItem("combinationTableCIRatios")
                if (ciTable$rowCount == 0 && (is.null(ratioTable) || ratioTable$rowCount == 0)) {
                    return(FALSE)
                }

                # Proportions and ratios now live in separate tables; the plot shows both.
                parts <- list()
                if (ciTable$rowCount > 0) parts[[length(parts) + 1]] <- ciTable$asDF
                if (!is.null(ratioTable) && ratioTable$rowCount > 0)
                    parts[[length(parts) + 1]] <- ratioTable$asDF
                table_df <- do.call(rbind, parts)
                rownames(table_df) <- NULL

                # Filter by statistic. The CI table stores display labels (e.g. "Sensitivity"),
                # so the option code must be mapped to its label before subsetting -- comparing
                # the code directly to the label always yielded an empty plot. Statistics not
                # present in the CI table (prevalence/balancedAccuracy/youden) leave the plot
                # unfiltered rather than blanking it.
                stat_filter <- self$options$filterStatistic
                if (stat_filter != "all") {
                    stat_label_map <- c(
                        prevalence = "Prevalence",
                        sens = "Sensitivity",
                        spec = "Specificity",
                        ppv = "PPV",
                        npv = "NPV",
                        acc = "Accuracy",
                        balancedAccuracy = "Balanced Accuracy",
                        youden = "Youden's J",
                        lrPos = "LR+",
                        lrNeg = "LR-",
                        dor = "DOR"
                    )
                    target_label <- stat_label_map[[stat_filter]]
                    if (!is.null(target_label) && target_label %in% table_df$statistic) {
                        table_df <- table_df[table_df$statistic == target_label, ]
                    }
                }

                if (nrow(table_df) == 0) {
                    return(FALSE)
                }

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = estimate, y = pattern, color = statistic)) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.2) +
                    ggplot2::labs(
                        title = "Forest Plot - 95% Confidence Intervals",
                        x = "Estimate (95% CI)",
                        y = "Pattern"
                    ) +
                    ggplot2::theme_minimal() +
                    # Sensitivity lives on 0-1 while a diagnostic odds ratio can reach the
                    # hundreds. On the shared x-axis that facet_wrap() defaults to, every
                    # proportion facet collapsed into a sliver at the left edge.
                    ggplot2::facet_wrap(~statistic, ncol = 1, scales = "free_x")

                print(p)
                return(TRUE)
            },
            .plotDecisionTree = function(image, ...) {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) {
                    return(FALSE)
                }

                table_df <- combTable$asDF

                # Simple tree structure based on patterns
                # This is a placeholder - real implementation would need tree layout algorithm

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = sens, y = spec)) +
                    ggplot2::geom_point(ggplot2::aes(size = youden, color = pattern)) +
                    ggplot2::geom_text(ggplot2::aes(label = pattern), vjust = -1) +
                    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
                    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
                    ggplot2::labs(
                        title = "Decision Space - Sensitivity vs Specificity",
                        x = "Sensitivity",
                        y = "Specificity",
                        size = "Youden's J"
                    ) +
                    ggplot2::theme_minimal()

                print(p)
                return(TRUE)
            },
            .applyPatternFilter = function(data, filter_type) {
                if (filter_type == "all" || is.null(filter_type)) {
                    return(data)
                }

                labels <- as.character(data$pattern)
                # Only the exhaustive result patterns are made of +/- tokens; the named
                # strategy rows ("Parallel (>=1 pos)") are not patterns and never match.
                is_pattern <- grepl("^[+-](/[+-])+$", labels)
                all_pos <- is_pattern & !grepl("-", labels)
                all_neg <- is_pattern & !grepl("\\+", labels)

                keep <- switch(filter_type,
                    allPositive = all_pos,
                    allNegative = all_neg,
                    # "mixed" previously excluded anything STARTING with "+/+" or "-/-",
                    # which threw away genuinely mixed three-test patterns such as "+/+/-"
                    # and "-/-/+". Mixed means: a pattern that is neither all-positive nor
                    # all-negative.
                    mixed = is_pattern & !all_pos & !all_neg,
                    return(data)
                )

                # Returning the UNFILTERED table when nothing matched meant a user who
                # selected "All Positive" was shown every pattern and had no way to tell.
                # Return the empty selection; the plot callers already decline to draw and
                # jamovi shows an empty plot rather than a misleading full one.
                data[which(keep), , drop = FALSE]
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for decisioncombine analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                gold <- self$options$gold
                test1 <- self$options$test1

                # Emit syntax whenever the analysis can run (gold + test1 present). test2/test3
                # are optional, so single-test and two-test analyses also get reproducible code.
                if (is.null(gold) || is.null(test1)) {
                    return("")
                }

                # Get arguments
                args <- ""
                if (!is.null(private$.asArgs)) {
                    args <- private$.asArgs(incData = FALSE)
                }
                if (args != "") {
                    args <- paste0(",\n    ", args)
                }

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath" # fallback

                # Build complete function call
                paste0(pkg_name, "::decisioncombine(\n    data = data", args, ")")
            }
        ) # End of public list
    )
}
