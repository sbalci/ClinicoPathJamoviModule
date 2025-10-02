#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests
#' @importFrom stats qnorm

decisioncombineClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .init = function() {
                # Dynamic image sizing based on number of tests
                has_test3 <- !is.null(self$options$test3) && self$options$test3 != ""

                if (has_test3) {
                    # 3-test tree needs more width (8 patterns at bottom)
                    self$results$decisionTreePlot$setSize(1400, 800)
                } else {
                    # 2-test tree with default size
                    self$results$decisionTreePlot$setSize(900, 700)
                }
            },

            .run = function() {
                # Main analysis flow - fail fast approach

                # Check if we have minimum required variables
                if (!private$.hasRequiredVars()) {
                    return()
                }

                # Step 1: Validate inputs
                private$.validateInputs()

                # Step 2: Prepare data
                data_prep <- private$.prepareData()

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

                # Step 4: Frequency tables (if requested)
                if (self$options$showFrequency) {
                    private$.generateFrequencyTables(data_prep)
                }

                # Step 5: Combination analysis
                private$.analyzeCombinations(data_prep)

                # Step 6: Prepare plot data (if any plots requested)
                if (self$options$showBarPlot || self$options$showHeatmap || self$options$showForest || self$options$showDecisionTree) {
                    private$.preparePlotData(data_prep)
                }

                # Step 7: Generate optimal pattern recommendation (if requested)
                if (self$options$showRecommendation) {
                    private$.generateOptimalRecommendation()
                }

                # Step 8: Add test pattern to data (if requested)
                if (self$options$addPatternToData) {
                    private$.addPatternColumn(data_prep)
                }
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
                # Strict validation with clear error messages

                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop("No data available. Please load data before running analysis.")
                }

                if (length(self$options$gold) == 0 || self$options$gold == "") {
                    stop("Gold standard variable is required. Please select a reference test.")
                }

                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    stop("Please select the disease present level for the gold standard.")
                }

                if (! (self$options$gold %in% names(self$data))) {
                    stop("Selected gold standard variable not found in the dataset.")
                }

                gold_values <- self$data[[self$options$gold]]
                gold_non_missing <- gold_values[!is.na(gold_values)]
                if (! (self$options$goldPositive %in% unique(gold_non_missing))) {
                    stop("Chosen disease present level for the gold standard is not observed in the data.")
                }
                if (length(unique(gold_non_missing)) < 2) {
                    stop("Gold standard variable must contain at least two distinct levels after removing missing values.")
                }

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    stop("Test 1 is required. Please select at least one test variable.")
                }

                if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                    stop("Please select the positive level for Test 1.")
                }

                if (! (self$options$test1 %in% names(self$data))) {
                    stop("Selected Test 1 variable not found in the dataset.")
                }

                test1_values <- self$data[[self$options$test1]]
                test1_non_missing <- test1_values[!is.na(test1_values)]
                if (! (self$options$test1Positive %in% unique(test1_non_missing))) {
                    stop("Chosen positive level for Test 1 is not observed in the data.")
                }
                if (length(unique(test1_non_missing)) < 2) {
                    stop("Test 1 must contain at least two distinct levels after removing missing values.")
                }

                # Check if we have at least 2 tests for combination analysis
                has_test2 <- !is.null(self$options$test2) && self$options$test2 != ""

                if (has_test2) {
                    if (is.null(self$options$test2Positive) || self$options$test2Positive == "") {
                        stop("Please select the positive level for Test 2.")
                    }
                    if (! (self$options$test2 %in% names(self$data))) {
                        stop("Selected Test 2 variable not found in the dataset.")
                    }
                    test2_values <- self$data[[self$options$test2]]
                    test2_non_missing <- test2_values[!is.na(test2_values)]
                    if (! (self$options$test2Positive %in% unique(test2_non_missing))) {
                        stop("Chosen positive level for Test 2 is not observed in the data.")
                    }
                    if (length(unique(test2_non_missing)) < 2) {
                        stop("Test 2 must contain at least two distinct levels after removing missing values.")
                    }
                }

                # Check test3 only if provided
                has_test3 <- !is.null(self$options$test3) && self$options$test3 != ""
                if (has_test3) {
                    if (is.null(self$options$test3Positive) || self$options$test3Positive == "") {
                        stop("Please select the positive level for Test 3.")
                    }
                    if (! (self$options$test3 %in% names(self$data))) {
                        stop("Selected Test 3 variable not found in the dataset.")
                    }
                    test3_values <- self$data[[self$options$test3]]
                    test3_non_missing <- test3_values[!is.na(test3_values)]
                    if (! (self$options$test3Positive %in% unique(test3_non_missing))) {
                        stop("Chosen positive level for Test 3 is not observed in the data.")
                    }
                    if (length(unique(test3_non_missing)) < 2) {
                        stop("Test 3 must contain at least two distinct levels after removing missing values.")
                    }
                }

                # Minimum data requirement
                if (nrow(self$data) < 4) {
                    stop("Insufficient data: At least 4 cases are required for analysis.")
                }
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
                mydata <- jmvcore::naOmit(subset_data)

                if (nrow(mydata) == 0) {
                    stop("No complete cases available after removing missing data.")
                }

                # Convert to factors
                for (var in vars_needed) {
                    mydata[[var]] <- forcats::as_factor(mydata[[var]])
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
                        goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive")
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
                        test1Variable2 = forcats::fct_relevel(test1Variable2, "Positive")
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
                            test2Variable2 = forcats::fct_relevel(test2Variable2, "Positive")
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
                            test3Variable2 = forcats::fct_relevel(test3Variable2, "Positive")
                        )
                }

                return(mydata)
            },

            .analyzeIndividualTest = function(data_prep, test_num) {
                # Analyze individual test performance

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
                contTable$addRow(rowKey = "Positive", values = list(
                    testResult = "Test Positive",
                    goldPos = tp,
                    goldNeg = fp,
                    total = tp + fp
                ))
                contTable$addRow(rowKey = "Negative", values = list(
                    testResult = "Test Negative",
                    goldPos = fn,
                    goldNeg = tn,
                    total = fn + tn
                ))
                contTable$addRow(rowKey = "Total", values = list(
                    testResult = "Total",
                    goldPos = tp + fn,
                    goldNeg = fp + tn,
                    total = tp + fp + fn + tn
                ))

                # Calculate statistics using epiR
                result <- epiR::epi.tests(cont_table, conf.level = 0.95)

                # Extract values - epiR returns $detail as a data frame
                detail_df <- as.data.frame(result$detail)

                sens <- detail_df[detail_df$statistic == "se", "est"]
                spec <- detail_df[detail_df$statistic == "sp", "est"]
                ppv <- detail_df[detail_df$statistic == "pv.pos", "est"]
                npv <- detail_df[detail_df$statistic == "pv.neg", "est"]

                # Populate statistics table
                statsTable$addRow(rowKey = "sens", values = list(
                    statistic = "Sensitivity",
                    estimate = sens
                ))
                statsTable$addRow(rowKey = "spec", values = list(
                    statistic = "Specificity",
                    estimate = spec
                ))
                statsTable$addRow(rowKey = "ppv", values = list(
                    statistic = "PPV",
                    estimate = ppv
                ))
                statsTable$addRow(rowKey = "npv", values = list(
                    statistic = "NPV",
                    estimate = npv
                ))
            },

            .generateFrequencyTables = function(data_prep) {
                # Generate frequency distribution tables for gold standard and test combinations

                # Gold standard frequency table
                goldTable <- self$results$goldFreqTable
                gold_freq <- table(data_prep$goldVariable2)
                total_n <- sum(gold_freq)

                for (level_name in names(gold_freq)) {
                    count <- as.integer(gold_freq[level_name])
                    percent <- count / total_n

                    goldTable$addRow(rowKey = level_name, values = list(
                        level = level_name,
                        count = count,
                        percent = percent
                    ))
                }

                # Cross-tabulation of test patterns with gold standard
                crossTable <- self$results$crossTabTable

                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test cross-tab
                    patterns <- c("Test 1+", "Test 1-")
                    conditions <- list(
                        data_prep$test1Variable2 == "Positive",
                        data_prep$test1Variable2 == "Negative"
                    )
                } else if (!has_test3) {
                    # Two-test cross-tab
                    patterns <- c("+/+", "+/-", "-/+", "-/-")
                    conditions <- list(
                        data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive",
                        data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative",
                        data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive",
                        data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative"
                    )
                } else {
                    # Three-test cross-tab
                    patterns <- c("+/+/+", "+/+/-", "+/-/+", "+/-/-",
                                  "-/+/+", "-/+/-", "-/-/+", "-/-/-")
                    conditions <- list(
                        data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Positive",
                        data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Negative",
                        data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Positive",
                        data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Negative",
                        data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Positive",
                        data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Positive" & data_prep$test3Variable2 == "Negative",
                        data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Positive",
                        data_prep$test1Variable2 == "Negative" & data_prep$test2Variable2 == "Negative" & data_prep$test3Variable2 == "Negative"
                    )
                }

                for (i in seq_along(patterns)) {
                    pattern_matches <- conditions[[i]]
                    gold_pos_count <- sum(pattern_matches & data_prep$goldVariable2 == "Positive", na.rm = TRUE)
                    gold_neg_count <- sum(pattern_matches & data_prep$goldVariable2 == "Negative", na.rm = TRUE)

                    crossTable$addRow(rowKey = patterns[i], values = list(
                        testCombo = patterns[i],
                        goldPos = as.integer(gold_pos_count),
                        goldNeg = as.integer(gold_neg_count),
                        total = as.integer(gold_pos_count + gold_neg_count)
                    ))
                }
            },

            .analyzeCombinations = function(data_prep) {
                # Generate and analyze all test combinations

                has_test2 <- "test2Variable2" %in% names(data_prep)
                has_test3 <- "test3Variable2" %in% names(data_prep)

                if (!has_test2) {
                    # Single test only - no combinations
                    private$.analyzeSinglePattern(data_prep, "Test 1",
                                                 data_prep$test1Variable2 == "Positive")
                    return()
                }

                if (!has_test3) {
                    # Two-test combinations (4 patterns)
                    private$.analyzeTwoTestPatterns(data_prep)
                } else {
                    # Three-test combinations (8 patterns)
                    private$.analyzeThreeTestPatterns(data_prep)
                }
            },

            .calcWilsonCI = function(x, n, conf.level = 0.95) {
                # Wilson score confidence interval
                # More accurate than normal approximation, especially for small samples
                if (is.na(x) || n == 0) return(c(NA_real_, NA_real_))

                p <- x / n
                z <- stats::qnorm((1 + conf.level) / 2)

                # Wilson score formula
                denominator <- 1 + (z^2 / n)
                centre <- (p + (z^2 / (2 * n))) / denominator
                half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator

                # Return bounds, constrained to [0, 1]
                c(max(0, centre - half_width), min(1, centre + half_width))
            },

            .calcYoudenCI = function(youden, sens, spec, total_pos, total_neg, conf.level = 0.95) {
                if (is.na(youden) || is.na(sens) || is.na(spec) || total_pos == 0 || total_neg == 0) {
                    return(c(NA_real_, NA_real_))
                }

                z <- stats::qnorm((1 + conf.level) / 2)
                var_sens <- sens * (1 - sens) / total_pos
                var_spec <- spec * (1 - spec) / total_neg
                se <- sqrt(var_sens + var_spec)

                if (!is.finite(se) || se == 0) {
                    return(c(youden, youden))
                }

                lower <- youden - z * se
                upper <- youden + z * se
                c(max(-1, lower), min(1, upper))
            },

            .haldaneCorrection = function(tp, fp, fn, tn) {
                if (any(c(tp, fp, fn, tn) == 0)) {
                    return(list(
                        tp = tp + 0.5,
                        fp = fp + 0.5,
                        fn = fn + 0.5,
                        tn = tn + 0.5
                    ))
                }

                list(tp = tp, fp = fp, fn = fn, tn = tn)
            },

            .calcLRCI = function(tp, fp, fn, tn, type = c("pos", "neg"), conf.level = 0.95) {
                type <- match.arg(type)
                corrected <- private$.haldaneCorrection(tp, fp, fn, tn)

                tp_c <- corrected$tp
                fp_c <- corrected$fp
                fn_c <- corrected$fn
                tn_c <- corrected$tn

                total_pos <- tp_c + fn_c
                total_neg <- fp_c + tn_c

                if (total_pos <= 0 || total_neg <= 0) {
                    return(c(NA_real_, NA_real_))
                }

                sens_c <- tp_c / total_pos
                spec_c <- tn_c / total_neg

                if (type == "pos") {
                    lr <- sens_c / (1 - spec_c)
                    se <- sqrt((1 / tp_c) - (1 / total_pos) + (1 / fp_c) - (1 / total_neg))
                } else {
                    lr <- (1 - sens_c) / spec_c
                    se <- sqrt((1 / fn_c) - (1 / total_pos) + (1 / tn_c) - (1 / total_neg))
                }

                if (!is.finite(lr) || !is.finite(se) || se <= 0) {
                    return(c(NA_real_, NA_real_))
                }

                z <- stats::qnorm((1 + conf.level) / 2)
                lower <- exp(log(lr) - z * se)
                upper <- exp(log(lr) + z * se)
                c(lower, upper)
            },

            .calcDORCI = function(tp, fp, fn, tn, conf.level = 0.95) {
                corrected <- private$.haldaneCorrection(tp, fp, fn, tn)

                tp_c <- corrected$tp
                fp_c <- corrected$fp
                fn_c <- corrected$fn
                tn_c <- corrected$tn

                se <- sqrt(1 / tp_c + 1 / fp_c + 1 / fn_c + 1 / tn_c)
                if (!is.finite(se) || se <= 0) {
                    return(c(NA_real_, NA_real_))
                }

                z <- stats::qnorm((1 + conf.level) / 2)
                dor_corrected <- (tp_c * tn_c) / (fp_c * fn_c)
                if (!is.finite(dor_corrected) || dor_corrected <= 0) {
                    return(c(NA_real_, NA_real_))
                }
                lower <- exp(log(dor_corrected) - z * se)
                upper <- exp(log(dor_corrected) + z * se)
                c(lower, upper)
            },

            .computeDiagnostics = function(tp, fp, fn, tn, conf.level = 0.95) {
                total_pos <- tp + fn
                total_neg <- fp + tn
                total <- total_pos + total_neg

                sens <- if (total_pos > 0) tp / total_pos else NA_real_
                spec <- if (total_neg > 0) tn / total_neg else NA_real_
                ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
                npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
                acc <- if (total > 0) (tp + tn) / total else NA_real_
                prevalence <- if (total > 0) total_pos / total else NA_real_

                sens_ci <- private$.calcWilsonCI(tp, total_pos, conf.level)
                spec_ci <- private$.calcWilsonCI(tn, total_neg, conf.level)
                ppv_ci <- private$.calcWilsonCI(tp, tp + fp, conf.level)
                npv_ci <- private$.calcWilsonCI(tn, tn + fn, conf.level)
                acc_ci <- private$.calcWilsonCI(tp + tn, total, conf.level)
                prev_ci <- private$.calcWilsonCI(total_pos, total, conf.level)

                youden <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_
                youden_ci <- private$.calcYoudenCI(youden, sens, spec, total_pos, total_neg, conf.level)
                balanced_accuracy <- if (!is.na(youden)) (youden + 1) / 2 else NA_real_
                ba_ci <- if (all(is.finite(youden_ci))) (youden_ci + 1) / 2 else c(NA_real_, NA_real_)

                lr_pos <- if (!is.na(sens) && !is.na(spec)) {
                    denom <- 1 - spec
                    if (denom == 0) {
                        if (sens == 0) 0 else Inf
                    } else {
                        sens / denom
                    }
                } else {
                    NA_real_
                }

                lr_neg <- if (!is.na(sens) && !is.na(spec)) {
                    denom <- spec
                    if (denom == 0) {
                        if (sens == 1) 0 else Inf
                    } else {
                        (1 - sens) / denom
                    }
                } else {
                    NA_real_
                }

                lr_pos_ci <- private$.calcLRCI(tp, fp, fn, tn, type = "pos", conf.level = conf.level)
                lr_neg_ci <- private$.calcLRCI(tp, fp, fn, tn, type = "neg", conf.level = conf.level)

                dor <- if (fp == 0 || fn == 0) {
                    if (tp > 0 && tn > 0) Inf else NA_real_
                } else if (tp == 0 || tn == 0) {
                    0
                } else {
                    (tp * tn) / (fp * fn)
                }
                dor_ci <- private$.calcDORCI(tp, fp, fn, tn, conf.level = conf.level)

                list(
                    tp = tp,
                    fp = fp,
                    fn = fn,
                    tn = tn,
                    sens = sens,
                    sens_ci = sens_ci,
                    spec = spec,
                    spec_ci = spec_ci,
                    ppv = ppv,
                    ppv_ci = ppv_ci,
                    npv = npv,
                    npv_ci = npv_ci,
                    acc = acc,
                    acc_ci = acc_ci,
                    prevalence = prevalence,
                    prevalence_ci = prev_ci,
                    youden = youden,
                    youden_ci = youden_ci,
                    balanced_accuracy = balanced_accuracy,
                    balanced_accuracy_ci = ba_ci,
                    lr_pos = lr_pos,
                    lr_pos_ci = lr_pos_ci,
                    lr_neg = lr_neg,
                    lr_neg_ci = lr_neg_ci,
                    dor = dor,
                    dor_ci = dor_ci
                )
            },

            .matchesPatternFilter = function(pattern_name) {
                # Determine if a pattern matches the current filter setting
                filter <- self$options$filterPattern

                if (filter == "all") {
                    return(TRUE)
                }

                # Strategy patterns
                is_parallel <- grepl("\\[Strategy\\] Parallel", pattern_name, fixed = FALSE)
                is_serial <- grepl("\\[Strategy\\] Serial", pattern_name, fixed = FALSE)
                is_majority <- grepl("\\[Strategy\\] Majority", pattern_name, fixed = FALSE)
                is_discordant <- grepl("\\[Strategy\\] Discordant", pattern_name, fixed = FALSE)

                # Filter by strategy type
                if (filter == "parallel" && is_parallel) return(TRUE)
                if (filter == "serial" && is_serial) return(TRUE)
                if (filter == "majority" && is_majority) return(TRUE)

                # Don't include strategies in pattern-based filters
                if (is_parallel || is_serial || is_majority || is_discordant) {
                    return(FALSE)
                }

                # Count positive and negative tests in pattern
                n_pos <- nchar(gsub("[^+]", "", pattern_name))
                n_neg <- nchar(gsub("[^-]", "", pattern_name))
                total_tests <- n_pos + n_neg

                # Skip if not a test pattern (e.g., "Test 1")
                if (total_tests == 0) {
                    return(filter == "all")
                }

                # Apply filters
                if (filter == "allPositive") {
                    return(n_neg == 0 && n_pos > 0)
                } else if (filter == "allNegative") {
                    return(n_pos == 0 && n_neg > 0)
                } else if (filter == "majorityPositive") {
                    return(n_pos > n_neg && n_pos > 0 && n_neg > 0)
                } else if (filter == "majorityNegative") {
                    return(n_neg > n_pos && n_pos > 0 && n_neg > 0)
                } else if (filter == "mixed") {
                    return(n_pos == n_neg && n_pos > 0)
                }

                return(FALSE)
            },

            .analyzeSinglePattern = function(data_prep, pattern_name, condition, row_key = NULL) {
                # Analyze a single test pattern

                # Check if this pattern matches the filter
                if (!private$.matchesPatternFilter(pattern_name)) {
                    return()
                }

                # Create binary variable for this pattern
                data_prep$pattern_result <- ifelse(condition, "Positive", "Negative")
                data_prep$pattern_result <- forcats::fct_relevel(
                    forcats::as_factor(data_prep$pattern_result),
                    "Positive"
                )

                # Create contingency table
                cont_table <- table(data_prep$pattern_result, data_prep$goldVariable2)

                if (!all(dim(cont_table) == c(2, 2))) {
                    return()
                }

                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                stats <- private$.computeDiagnostics(tp, fp, fn, tn)

                combTable <- self$results$combinationTable
                ciTable <- self$results$combinationTableCI
                row_id <- if (is.null(row_key)) pattern_name else row_key

                combTable$addRow(rowKey = row_id, values = list(
                    pattern = pattern_name,
                    tp = stats$tp,
                    fp = stats$fp,
                    fn = stats$fn,
                    tn = stats$tn,
                    prevalence = stats$prevalence,
                    sens = stats$sens,
                    spec = stats$spec,
                    ppv = stats$ppv,
                    npv = stats$npv,
                    acc = stats$acc,
                    balancedAccuracy = stats$balanced_accuracy,
                    youden = stats$youden,
                    lrPos = stats$lr_pos,
                    lrNeg = stats$lr_neg,
                    dor = stats$dor
                ))

                # Helper to add CI row with filtering
                add_ci_row <- function(stat_name, estimate, ci, suffix, filter_key) {
                    if (all(is.na(c(estimate, ci)))) {
                        return()
                    }

                    # Apply statistic filter
                    filter_stat <- self$options$filterStatistic
                    if (filter_stat != "all" && filter_stat != filter_key) {
                        return()
                    }

                    ciTable$addRow(rowKey = paste0(row_id, "_", suffix), values = list(
                        pattern = pattern_name,
                        statistic = stat_name,
                        estimate = estimate,
                        lower = ci[1],
                        upper = ci[2]
                    ))
                }

                add_ci_row("Prevalence", stats$prevalence, stats$prevalence_ci, "prev", "prevalence")
                add_ci_row("Sensitivity", stats$sens, stats$sens_ci, "sens", "sens")
                add_ci_row("Specificity", stats$spec, stats$spec_ci, "spec", "spec")
                add_ci_row("PPV", stats$ppv, stats$ppv_ci, "ppv", "ppv")
                add_ci_row("NPV", stats$npv, stats$npv_ci, "npv", "npv")
                add_ci_row("Accuracy", stats$acc, stats$acc_ci, "acc", "acc")
                add_ci_row("Balanced Accuracy", stats$balanced_accuracy, stats$balanced_accuracy_ci, "ba", "balancedAccuracy")
                add_ci_row("Youden's J", stats$youden, stats$youden_ci, "youden", "youden")
                add_ci_row("LR+", stats$lr_pos, stats$lr_pos_ci, "lrpos", "lrPos")
                add_ci_row("LR-", stats$lr_neg, stats$lr_neg_ci, "lrneg", "lrNeg")
                add_ci_row("Diagnostic Odds Ratio", stats$dor, stats$dor_ci, "dor", "dor")
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

                # Standard clinical strategies for two tests
                private$.analyzeSinglePattern(
                    data_prep,
                    "[Strategy] Parallel (≥1 positive)",
                    data_prep$test1Variable2 == "Positive" | data_prep$test2Variable2 == "Positive",
                    row_key = "strategy_parallel_two"
                )

                private$.analyzeSinglePattern(
                    data_prep,
                    "[Strategy] Serial (both positive)",
                    data_prep$test1Variable2 == "Positive" & data_prep$test2Variable2 == "Positive",
                    row_key = "strategy_serial_two"
                )

                private$.analyzeSinglePattern(
                    data_prep,
                    "[Strategy] Discordant results",
                    xor(data_prep$test1Variable2 == "Positive", data_prep$test2Variable2 == "Positive"),
                    row_key = "strategy_discordant_two"
                )
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

                # Clinical strategies for three tests
                private$.analyzeSinglePattern(
                    data_prep,
                    "[Strategy] Parallel (≥1 positive)",
                    data_prep$test1Variable2 == "Positive" |
                        data_prep$test2Variable2 == "Positive" |
                        data_prep$test3Variable2 == "Positive",
                    row_key = "strategy_parallel_three"
                )

                private$.analyzeSinglePattern(
                    data_prep,
                    "[Strategy] Serial (all positive)",
                    data_prep$test1Variable2 == "Positive" &
                        data_prep$test2Variable2 == "Positive" &
                        data_prep$test3Variable2 == "Positive",
                    row_key = "strategy_serial_three"
                )

                private$.analyzeSinglePattern(
                    data_prep,
                    "[Strategy] Majority rule (≥2 positive)",
                    rowSums(cbind(
                        data_prep$test1Variable2 == "Positive",
                        data_prep$test2Variable2 == "Positive",
                        data_prep$test3Variable2 == "Positive"
                    )) >= 2,
                    row_key = "strategy_majority_three"
                )
            },

            .preparePlotData = function(data_prep) {
                # Store data for all plotting functions
                # Extract both main table and CI table for comprehensive plotting

                combTable <- self$results$combinationTable
                ciTable <- self$results$combinationTableCI

                if (combTable$rowCount == 0) {
                    return()
                }

                # Convert tables to data frames
                table_df <- as.data.frame(combTable)
                ci_df <- if (ciTable$rowCount > 0) as.data.frame(ciTable) else NULL

                # Store in all plot image states
                if (self$options$showBarPlot) {
                    self$results$barPlot$setState(list(data = table_df))
                }

                if (self$options$showHeatmap) {
                    self$results$heatmapPlot$setState(list(data = table_df))
                }

                if (self$options$showForest) {
                    self$results$forestPlot$setState(list(data = table_df, ci = ci_df))
                }

                if (self$options$showDecisionTree) {
                    self$results$decisionTreePlot$setState(list(data = table_df, data_prep = data_prep))
                }
            },

            .plotBarChart = function(image, ...) {
                # Grouped bar chart for key metrics

                state <- image$state
                if (is.null(state) || is.null(state$data)) {
                    return(FALSE)
                }

                df <- state$data
                filter_stat <- self$options$filterStatistic

                # Define all possible metrics with their column names
                all_metrics <- list(
                    prevalence = list(col = "prevalence", label = "Prevalence"),
                    sens = list(col = "sens", label = "Sensitivity"),
                    spec = list(col = "spec", label = "Specificity"),
                    ppv = list(col = "ppv", label = "PPV"),
                    npv = list(col = "npv", label = "NPV"),
                    acc = list(col = "acc", label = "Accuracy"),
                    balancedAccuracy = list(col = "balancedAccuracy", label = "Balanced Acc"),
                    youden = list(col = "youden", label = "Youden's J"),
                    lrPos = list(col = "lrPos", label = "LR+"),
                    lrNeg = list(col = "lrNeg", label = "LR-"),
                    dor = list(col = "dor", label = "Diagnostic OR")
                )

                # Determine which metrics to display
                if (filter_stat == "all") {
                    # Show the 5 most interpretable metrics for bar chart
                    selected <- c("sens", "spec", "ppv", "npv", "acc")
                } else {
                    # Show only the selected metric
                    selected <- filter_stat
                }

                # Build long format data for selected metrics
                df_long <- data.frame()
                for (metric_key in selected) {
                    if (metric_key %in% names(all_metrics)) {
                        metric_info <- all_metrics[[metric_key]]
                        if (metric_info$col %in% names(df)) {
                            df_long <- rbind(df_long, data.frame(
                                Pattern = df$pattern,
                                Metric = metric_info$label,
                                Value = df[[metric_info$col]],
                                stringsAsFactors = FALSE
                            ))
                        }
                    }
                }

                # Remove NA values
                df_long <- df_long[!is.na(df_long$Value), ]

                if (nrow(df_long) == 0) {
                    return(FALSE)
                }

                # Create grouped bar plot
                p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Pattern, y = Value, fill = Metric)) +
                    ggplot2::geom_bar(stat = "identity", position = "dodge") +
                    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
                    ggplot2::labs(
                        title = "Diagnostic Performance by Test Combination",
                        x = "Test Combination Pattern",
                        y = "Performance Metric",
                        fill = "Metric"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        legend.position = "bottom"
                    )

                print(p)
                return(TRUE)
            },

            .plotHeatmap = function(image, ...) {
                # Heatmap showing all metrics for all patterns

                state <- image$state
                if (is.null(state) || is.null(state$data)) {
                    return(FALSE)
                }

                df <- state$data
                filter_stat <- self$options$filterStatistic

                # Define all metrics suitable for heatmap (proportions and indices)
                all_heatmap_metrics <- list(
                    prevalence = "Prevalence",
                    sens = "Sensitivity",
                    spec = "Specificity",
                    ppv = "PPV",
                    npv = "NPV",
                    acc = "Accuracy",
                    balancedAccuracy = "Balanced Acc",
                    youden = "Youden's J"
                )

                # Determine which metrics to display
                if (filter_stat == "all") {
                    # Show all heatmap-suitable metrics
                    selected_metrics <- all_heatmap_metrics
                } else if (filter_stat %in% names(all_heatmap_metrics)) {
                    # Show only the selected metric
                    selected_metrics <- all_heatmap_metrics[filter_stat]
                } else {
                    # If selected metric is not suitable for heatmap (lrPos, lrNeg, dor), show nothing
                    return(FALSE)
                }

                # Create long format
                df_long <- data.frame()
                for (metric_col in names(selected_metrics)) {
                    if (metric_col %in% names(df)) {
                        df_long <- rbind(df_long, data.frame(
                            Pattern = df$pattern,
                            Metric = selected_metrics[[metric_col]],
                            Value = df[[metric_col]],
                            stringsAsFactors = FALSE
                        ))
                    }
                }

                # Remove NA values
                df_long <- df_long[!is.na(df_long$Value), ]

                if (nrow(df_long) == 0) {
                    return(FALSE)
                }

                # Create heatmap
                p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Metric, y = Pattern, fill = Value)) +
                    ggplot2::geom_tile(color = "white") +
                    ggplot2::scale_fill_gradient2(
                        low = "#d73027",
                        mid = "#ffffbf",
                        high = "#1a9850",
                        midpoint = 0.5,
                        limits = c(0, 1),
                        labels = scales::percent,
                        name = "Value"
                    ) +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Value)),
                                      size = 3, color = "black") +
                    ggplot2::labs(
                        title = "Heatmap: All Diagnostic Metrics by Test Pattern",
                        x = "Diagnostic Metric",
                        y = "Test Pattern"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        panel.grid = ggplot2::element_blank()
                    )

                print(p)
                return(TRUE)
            },

            .plotForest = function(image, ...) {
                # Forest plot showing confidence intervals for key metrics

                state <- image$state
                if (is.null(state) || is.null(state$ci)) {
                    return(FALSE)
                }

                ci_df <- state$ci
                filter_stat <- self$options$filterStatistic

                # Map filter options to statistic names in CI table
                filter_to_statistic <- list(
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
                    dor = "Diagnostic OR"
                )

                # Determine which statistics to display
                if (filter_stat == "all") {
                    # Show the 5 most interpretable metrics
                    key_metrics <- c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy")
                } else if (filter_stat %in% names(filter_to_statistic)) {
                    # Show only the selected metric
                    key_metrics <- filter_to_statistic[[filter_stat]]
                } else {
                    # No valid metric selected
                    return(FALSE)
                }

                # Filter CI data
                ci_df <- ci_df[ci_df$statistic %in% key_metrics, ]

                if (nrow(ci_df) == 0) {
                    return(FALSE)
                }

                # Remove rows with missing CIs
                ci_df <- ci_df[!is.na(ci_df$estimate) & !is.na(ci_df$lower) & !is.na(ci_df$upper), ]

                if (nrow(ci_df) == 0) {
                    return(FALSE)
                }

                # Create combined label
                ci_df$label <- paste0(ci_df$pattern, " - ", ci_df$statistic)

                # Reverse order for better readability (top to bottom)
                ci_df$label <- factor(ci_df$label, levels = rev(ci_df$label))

                # Create forest plot
                p <- ggplot2::ggplot(ci_df, ggplot2::aes(x = estimate, y = label)) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                                           height = 0.2) +
                    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                                       color = "gray50") +
                    ggplot2::scale_x_continuous(
                        labels = scales::percent,
                        limits = c(0, 1)
                    ) +
                    ggplot2::labs(
                        title = "Forest Plot: 95% Confidence Intervals",
                        x = "Estimate with 95% CI",
                        y = "Pattern - Metric"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank()
                    )

                print(p)
                return(TRUE)
            },

            .plotDecisionTree = function(image, ...) {
                # Decision tree: Test sequence → Gold standard outcome
                # Shows Test1 → Test2 → Test3 (if present) → Gold Standard (Disease+/-)

                state <- image$state
                if (is.null(state) || is.null(state$data) || is.null(state$data_prep)) {
                    return(FALSE)
                }

                df <- state$data
                data_prep <- state$data_prep
                n_patterns <- nrow(df)
                if (n_patterns == 0) {
                    return(FALSE)
                }

                # Get total number of cases
                total_n <- sum(df$tp[1], df$fp[1], df$tn[1], df$fn[1], na.rm = TRUE)

                # Check if Test3 exists
                has_test3 <- !is.null(self$options$test3) && self$options$test3 != ""

                # Create node and edge data structures
                nodes <- data.frame(
                    id = character(),
                    label = character(),
                    x = numeric(),
                    y = numeric(),
                    acc = numeric(),
                    n_cases = numeric(),
                    level = integer(),
                    stringsAsFactors = FALSE
                )

                edges <- data.frame(
                    from_id = character(),
                    to_id = character(),
                    n_flow = numeric(),
                    stringsAsFactors = FALSE
                )

                # Root node (all cases)
                nodes <- rbind(nodes, data.frame(
                    id = "root",
                    label = sprintf("All Cases\nn=%d", total_n),
                    x = 0,
                    y = 0,
                    acc = mean(df$acc, na.rm = TRUE),
                    n_cases = total_n,
                    level = 0,
                    stringsAsFactors = FALSE
                ))

                if (!has_test3) {
                    # 2-test tree: Root → Test1 → Test2 → Gold Standard
                    # Access original test data for counting actual cases
                    test1_var <- data_prep$test1Variable2
                    test2_var <- data_prep$test2Variable2

                    # Level 1: Test1 (+/-)
                    test1_results <- c("+", "-")
                    x_l1 <- c(-1, 1)

                    for (i in seq_along(test1_results)) {
                        t1 <- test1_results[i]
                        pattern_regex <- paste0("^", gsub("\\+", "\\\\+", t1), "/")
                        matching_rows <- grep(pattern_regex, df$pattern)

                        if (length(matching_rows) > 0) {
                            # Count actual cases where Test1 matches this result
                            t1_level <- if (t1 == "+") "Positive" else "Negative"
                            n_cases_t1 <- sum(test1_var == t1_level, na.rm = TRUE)

                            node_id_t1 <- paste0("T1_", t1)
                            nodes <- rbind(nodes, data.frame(
                                id = node_id_t1,
                                label = sprintf("Test1: %s\nn=%d", t1, n_cases_t1),
                                x = x_l1[i],
                                y = -1,
                                acc = mean(df$acc[matching_rows]),
                                n_cases = n_cases_t1,
                                level = 1,
                                stringsAsFactors = FALSE
                            ))

                            edges <- rbind(edges, data.frame(
                                from_id = "root",
                                to_id = node_id_t1,
                                n_flow = n_cases_t1,
                                stringsAsFactors = FALSE
                            ))

                            # Level 2: Test2 (+/-)
                            test2_results <- c("+", "-")
                            x_l2 <- c(x_l1[i] - 0.6, x_l1[i] + 0.6)

                            for (j in seq_along(test2_results)) {
                                t2 <- test2_results[j]
                                pattern <- paste0(t1, "/", t2)
                                row_idx <- which(df$pattern == pattern)

                                if (length(row_idx) > 0) {
                                    # Count actual cases where Test1=t1 AND Test2=t2
                                    t2_level <- if (t2 == "+") "Positive" else "Negative"
                                    n_cases_t2 <- sum(test1_var == t1_level & test2_var == t2_level, na.rm = TRUE)

                                    node_id_t2 <- paste0("T2_", pattern)
                                    nodes <- rbind(nodes, data.frame(
                                        id = node_id_t2,
                                        label = sprintf("%s\nn=%d", pattern, n_cases_t2),
                                        x = x_l2[j],
                                        y = -2,
                                        acc = df$acc[row_idx],
                                        n_cases = n_cases_t2,
                                        level = 2,
                                        stringsAsFactors = FALSE
                                    ))

                                    edges <- rbind(edges, data.frame(
                                        from_id = node_id_t1,
                                        to_id = node_id_t2,
                                        n_flow = n_cases_t2,
                                        stringsAsFactors = FALSE
                                    ))

                                    # Level 3: Gold Standard (Disease +/-)
                                    n_disease_pos <- df$tp[row_idx] + df$fp[row_idx]
                                    n_disease_neg <- df$tn[row_idx] + df$fn[row_idx]

                                    if (n_disease_pos > 0) {
                                        node_id_gold_pos <- paste0("Gold_", pattern, "_pos")
                                        nodes <- rbind(nodes, data.frame(
                                            id = node_id_gold_pos,
                                            label = sprintf("Disease+\nn=%d", n_disease_pos),
                                            x = x_l2[j] - 0.35,
                                            y = -3,
                                            acc = df$tp[row_idx] / n_disease_pos,  # Sens
                                            n_cases = n_disease_pos,
                                            level = 3,
                                            stringsAsFactors = FALSE
                                        ))

                                        edges <- rbind(edges, data.frame(
                                            from_id = node_id_t2,
                                            to_id = node_id_gold_pos,
                                            n_flow = n_disease_pos,
                                            stringsAsFactors = FALSE
                                        ))
                                    }

                                    if (n_disease_neg > 0) {
                                        node_id_gold_neg <- paste0("Gold_", pattern, "_neg")
                                        nodes <- rbind(nodes, data.frame(
                                            id = node_id_gold_neg,
                                            label = sprintf("Disease-\nn=%d", n_disease_neg),
                                            x = x_l2[j] + 0.35,
                                            y = -3,
                                            acc = df$tn[row_idx] / n_disease_neg,  # Spec
                                            n_cases = n_disease_neg,
                                            level = 3,
                                            stringsAsFactors = FALSE
                                        ))

                                        edges <- rbind(edges, data.frame(
                                            from_id = node_id_t2,
                                            to_id = node_id_gold_neg,
                                            n_flow = n_disease_neg,
                                            stringsAsFactors = FALSE
                                        ))
                                    }
                                }
                            }
                        }
                    }
                } else {
                    # 3-test tree: Build level by level with actual flows
                    # Access original test data for counting actual cases
                    test1_var <- data_prep$test1Variable2
                    test2_var <- data_prep$test2Variable2
                    test3_var <- data_prep$test3Variable2

                    # Level 1: Test1 splits (+/-)
                    test1_results <- c("+", "-")
                    x_l1 <- c(-3, 3)  # Wider spacing for 3-test tree

                    for (i in seq_along(test1_results)) {
                        t1 <- test1_results[i]
                        # Find all patterns starting with this test1 result
                        pattern_regex <- paste0("^", gsub("\\+", "\\\\+", t1), "/")
                        matching_rows <- grep(pattern_regex, df$pattern)

                        if (length(matching_rows) > 0) {
                            # Count actual cases where Test1 matches this result
                            t1_level <- if (t1 == "+") "Positive" else "Negative"
                            n_cases <- sum(test1_var == t1_level, na.rm = TRUE)
                            avg_acc <- mean(df$acc[matching_rows], na.rm = TRUE)

                            node_id <- paste0("T1_", t1)
                            nodes <- rbind(nodes, data.frame(
                                id = node_id,
                                label = sprintf("Test1: %s\nn=%d", t1, n_cases),
                                x = x_l1[i],
                                y = -1,
                                acc = avg_acc,
                                n_cases = n_cases,
                                level = 1,
                                stringsAsFactors = FALSE
                            ))

                            edges <- rbind(edges, data.frame(
                                from_id = "root",
                                to_id = node_id,
                                n_flow = n_cases,
                                stringsAsFactors = FALSE
                            ))

                            # Level 2: Test2 splits (+/-)
                            test2_results <- c("+", "-")
                            x_l2_base <- x_l1[i]
                            x_l2 <- c(x_l2_base - 1.2, x_l2_base + 1.2)  # Wider spacing for 3-test

                            for (j in seq_along(test2_results)) {
                                t2 <- test2_results[j]
                                pattern_regex2 <- paste0("^", gsub("\\+", "\\\\+", t1), "/",
                                                        gsub("\\+", "\\\\+", t2), "/")
                                matching_rows2 <- grep(pattern_regex2, df$pattern)

                                if (length(matching_rows2) > 0) {
                                    # Count actual cases where Test1=t1 AND Test2=t2
                                    t2_level <- if (t2 == "+") "Positive" else "Negative"
                                    n_cases2 <- sum(test1_var == t1_level & test2_var == t2_level, na.rm = TRUE)
                                    avg_acc2 <- mean(df$acc[matching_rows2], na.rm = TRUE)

                                    node_id2 <- paste0("T2_", t1, t2)
                                    nodes <- rbind(nodes, data.frame(
                                        id = node_id2,
                                        label = sprintf("%s/%s\nn=%d", t1, t2, n_cases2),
                                        x = x_l2[j],
                                        y = -2,
                                        acc = avg_acc2,
                                        n_cases = n_cases2,
                                        level = 2,
                                        stringsAsFactors = FALSE
                                    ))

                                    edges <- rbind(edges, data.frame(
                                        from_id = node_id,
                                        to_id = node_id2,
                                        n_flow = n_cases2,
                                        stringsAsFactors = FALSE
                                    ))

                                    # Level 3: Test3 (+/-)
                                    test3_results <- c("+", "-")

                                    for (k in seq_along(test3_results)) {
                                        t3 <- test3_results[k]
                                        full_pattern <- paste0(t1, "/", t2, "/", t3)
                                        row_idx <- which(df$pattern == full_pattern)

                                        if (length(row_idx) > 0) {
                                            # Count actual cases where Test1=t1 AND Test2=t2 AND Test3=t3
                                            t3_level <- if (t3 == "+") "Positive" else "Negative"
                                            n_cases3 <- sum(test1_var == t1_level & test2_var == t2_level & test3_var == t3_level, na.rm = TRUE)

                                            # X position for Test3 nodes - wider spacing to prevent overlap
                                            pattern_num <- match(full_pattern,
                                                               c("+/+/+", "+/+/-", "+/-/+", "+/-/-",
                                                                 "-/+/+", "-/+/-", "-/-/+", "-/-/-"))
                                            x_l3 <- seq(-5.5, 5.5, length.out = 8)[pattern_num]

                                            node_id_t3 <- paste0("T3_", full_pattern)
                                            nodes <- rbind(nodes, data.frame(
                                                id = node_id_t3,
                                                label = sprintf("%s\nn=%d", full_pattern, n_cases3),
                                                x = x_l3,
                                                y = -3,
                                                acc = df$acc[row_idx],
                                                n_cases = n_cases3,
                                                level = 3,
                                                stringsAsFactors = FALSE
                                            ))

                                            edges <- rbind(edges, data.frame(
                                                from_id = node_id2,
                                                to_id = node_id_t3,
                                                n_flow = n_cases3,
                                                stringsAsFactors = FALSE
                                            ))

                                            # Level 4: Gold Standard (Disease +/-)
                                            n_disease_pos <- df$tp[row_idx] + df$fp[row_idx]
                                            n_disease_neg <- df$tn[row_idx] + df$fn[row_idx]

                                            if (n_disease_pos > 0) {
                                                node_id_gold_pos <- paste0("Gold_", full_pattern, "_pos")
                                                nodes <- rbind(nodes, data.frame(
                                                    id = node_id_gold_pos,
                                                    label = sprintf("Disease+\nn=%d", n_disease_pos),
                                                    x = x_l3 - 0.35,  # Increased spacing from 0.2 to 0.35
                                                    y = -4,
                                                    acc = df$tp[row_idx] / n_disease_pos,
                                                    n_cases = n_disease_pos,
                                                    level = 4,
                                                    stringsAsFactors = FALSE
                                                ))

                                                edges <- rbind(edges, data.frame(
                                                    from_id = node_id_t3,
                                                    to_id = node_id_gold_pos,
                                                    n_flow = n_disease_pos,
                                                    stringsAsFactors = FALSE
                                                ))
                                            }

                                            if (n_disease_neg > 0) {
                                                node_id_gold_neg <- paste0("Gold_", full_pattern, "_neg")
                                                nodes <- rbind(nodes, data.frame(
                                                    id = node_id_gold_neg,
                                                    label = sprintf("Disease-\nn=%d", n_disease_neg),
                                                    x = x_l3 + 0.35,  # Increased spacing from 0.2 to 0.35
                                                    y = -4,
                                                    acc = df$tn[row_idx] / n_disease_neg,
                                                    n_cases = n_disease_neg,
                                                    level = 4,
                                                    stringsAsFactors = FALSE
                                                ))

                                                edges <- rbind(edges, data.frame(
                                                    from_id = node_id_t3,
                                                    to_id = node_id_gold_neg,
                                                    n_flow = n_disease_neg,
                                                    stringsAsFactors = FALSE
                                                ))
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                # Calculate flow proportions for line thickness
                if (nrow(edges) > 0) {
                    edges$flow_prop <- edges$n_flow / total_n
                    # Scale line width: 0.3 to 3 based on proportion (reduced from 0.5-10)
                    edges$line_width <- 0.3 + (edges$flow_prop * 2.7)

                    # Add start/end coordinates
                    edges$x_start <- sapply(edges$from_id, function(fid) nodes$x[nodes$id == fid])
                    edges$y_start <- sapply(edges$from_id, function(fid) nodes$y[nodes$id == fid])
                    edges$x_end <- sapply(edges$to_id, function(tid) nodes$x[nodes$id == tid])
                    edges$y_end <- sapply(edges$to_id, function(tid) nodes$y[nodes$id == tid])
                } else {
                    return(FALSE)
                }

                # Color borders by accuracy - no fill, just colored borders
                nodes$border_color <- ifelse(nodes$acc >= 0.8, "#2d5a3d",    # Dark green
                                            ifelse(nodes$acc >= 0.7, "#5a8c65", # Medium green
                                                  ifelse(nodes$acc >= 0.6, "#b8935f", # Brown
                                                        "#994444")))          # Dark red

                # Border width based on performance
                nodes$border_width <- ifelse(nodes$acc >= 0.7, 2, 1.5)

                # Create plot with variable-width edges showing flow
                p <- ggplot2::ggplot() +
                    # Draw edges with width proportional to flow
                    ggplot2::geom_segment(data = edges,
                                         ggplot2::aes(x = x_start, y = y_start,
                                                     xend = x_end, yend = y_end,
                                                     linewidth = line_width),
                                         color = "gray60", alpha = 0.6,
                                         arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "inches"),
                                                               type = "closed")) +
                    # Draw node boxes - white fill with colored borders
                    ggplot2::geom_tile(data = nodes,
                                      ggplot2::aes(x = x, y = y, color = border_color, linewidth = border_width),
                                      fill = "white",
                                      width = 0.45, height = 0.35) +
                    # Add labels
                    ggplot2::geom_text(data = nodes,
                                      ggplot2::aes(x = x, y = y, label = label),
                                      size = 3, fontface = "bold", lineheight = 0.85,
                                      color = "black") +
                    ggplot2::scale_color_identity() +
                    ggplot2::scale_linewidth_identity() +
                    ggplot2::coord_cartesian(clip = "off") +
                    ggplot2::labs(
                        title = "Decision Tree: Test Sequence → Gold Standard",
                        subtitle = paste0("Root → Test1 → Test2",
                                        if (has_test3) " → Test3" else "",
                                        " → Disease+/- | Line thickness = case flow"),
                        caption = "Border color: Dark Green = Excellent (≥80%) | Green = Good (≥70%) | Brown = Fair (≥60%) | Red = Limited (<60%)"
                    ) +
                    ggplot2::theme_void() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 15,
                                                          margin = ggplot2::margin(b = 5)),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10,
                                                             color = "gray40",
                                                             margin = ggplot2::margin(b = 15)),
                        plot.caption = ggplot2::element_text(hjust = 0.5, size = 9,
                                                            color = "gray50",
                                                            margin = ggplot2::margin(t = 10)),
                        plot.margin = ggplot2::margin(25, 25, 25, 25),
                        plot.background = ggplot2::element_rect(fill = "white", color = NA)
                    )

                print(p)
                return(TRUE)
            },

            .generateOptimalRecommendation = function() {
                # Find and recommend the optimal test combination pattern

                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) {
                    return()
                }

                # Convert table to data frame
                df <- as.data.frame(combTable)

                # Calculate Youden's J for all patterns
                df$youden <- df$sens + df$spec - 1
                df$youden[is.na(df$youden)] <- -Inf

                # Find optimal pattern by Youden's J
                optimal_idx <- which.max(df$youden)

                if (length(optimal_idx) == 0 || is.na(optimal_idx)) {
                    return()
                }

                optimal_pattern <- df$pattern[optimal_idx]
                optimal_youden <- df$youden[optimal_idx]
                optimal_sens <- df$sens[optimal_idx]
                optimal_spec <- df$spec[optimal_idx]
                optimal_acc <- df$acc[optimal_idx]

                # Generate clinical rationale
                rationale <- sprintf(
                    "Selected based on maximum Youden's J statistic (%.3f), which optimizes sensitivity + specificity - 1. ",
                    optimal_youden
                )

                # Add clinical interpretation
                if (optimal_sens >= 0.90 && optimal_spec >= 0.90) {
                    rationale <- paste0(rationale, "This pattern provides excellent balanced performance for both ruling in and ruling out disease.")
                } else if (optimal_sens >= 0.90) {
                    rationale <- paste0(rationale, "High sensitivity makes this pattern excellent for screening and ruling out disease (negative result = low probability of disease).")
                } else if (optimal_spec >= 0.90) {
                    rationale <- paste0(rationale, "High specificity makes this pattern excellent for confirmation and ruling in disease (positive result = high probability of disease).")
                } else if (optimal_sens >= 0.80 && optimal_spec >= 0.80) {
                    rationale <- paste0(rationale, "This pattern provides good balanced diagnostic performance for clinical decision-making.")
                } else {
                    rationale <- paste0(rationale, "Consider clinical context and additional testing when interpreting results from this combination.")
                }

                # Populate recommendation table
                recTable <- self$results$recommendationTable
                recTable$setRow(rowNo = 1, values = list(
                    pattern = optimal_pattern,
                    method = "Youden's J Index",
                    youden = optimal_youden,
                    sens = optimal_sens,
                    spec = optimal_spec,
                    acc = optimal_acc,
                    rationale = rationale
                ))
            },

            .addPatternColumn = function(data_prep) {
                # Add test combination pattern to the original dataset

                if (is.null(data_prep) || is.null(self$data)) {
                    return()
                }

                # Get test variables
                test1Var <- data_prep$test1Variable
                test2Var <- data_prep$test2Variable
                test3Var <- data_prep$test3Variable

                # Create pattern labels
                n_rows <- nrow(self$data)
                patterns <- character(n_rows)

                for (i in 1:n_rows) {
                    t1 <- if (!is.null(test1Var)) as.character(test1Var[i]) else NA
                    t2 <- if (!is.null(test2Var)) as.character(test2Var[i]) else NA
                    t3 <- if (!is.null(test3Var)) as.character(test3Var[i]) else NA

                    # Build pattern string
                    pattern_parts <- c()

                    if (!is.na(t1)) {
                        symbol1 <- if (t1 == self$options$test1Positive) "+" else "-"
                        pattern_parts <- c(pattern_parts, symbol1)
                    }

                    if (!is.na(t2) && !is.null(test2Var)) {
                        symbol2 <- if (t2 == self$options$test2Positive) "+" else "-"
                        pattern_parts <- c(pattern_parts, symbol2)
                    }

                    if (!is.na(t3) && !is.null(test3Var)) {
                        symbol3 <- if (t3 == self$options$test3Positive) "+" else "-"
                        pattern_parts <- c(pattern_parts, symbol3)
                    }

                    patterns[i] <- if (length(pattern_parts) > 0) {
                        paste(pattern_parts, collapse = "/")
                    } else {
                        NA_character_
                    }
                }

                # Add or update the pattern column
                column_name <- "TestPattern"

                # Check if column already exists
                if (column_name %in% names(self$data)) {
                    self$data[[column_name]] <- patterns
                } else {
                    self$data[[column_name]] <- jmvcore::Data$new()
                    self$data[[column_name]]$setValues(patterns)
                }
            }
        )
    )
