#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent_format
#' @importFrom rlang .data

decisioncombineClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .noticeList = list(),

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
                    if (notice$type == "ERROR") {
                        color <- "#d9534f"
                        icon <- "&#x26A0;"  # Warning sign
                    } else if (notice$type == "WARNING") {
                        color <- "#f0ad4e"
                        icon <- "&#x26A0;"
                    } else {  # INFO
                        color <- "#5bc0de"
                        icon <- "&#x2139;"  # Info sign
                    }

                    html <- paste0(html,
                        '<div style="background-color: ', color, '; color: white; padding: 10px; margin: 5px 0; border-radius: 4px;">',
                        '<strong>', icon, ' ', notice$title, ':</strong> ',
                        notice$content,
                        '</div>'
                    )
                }
                html <- paste0(html, '</div>')

                self$results$notices$setContent(html)
            },

            .escapeVariableNames = function(var_names) {
                # Escape variable names with spaces/special characters
                sapply(var_names, function(x) {
                    if (grepl("[^A-Za-z0-9_.]", x)) {
                        paste0("`", x, "`")
                    } else {
                        x
                    }
                }, USE.NAMES = FALSE)
            },

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
            },

            .run = function() {
                # Main analysis flow - fail fast approach

                # Check if we have minimum required variables
                if (!private$.hasRequiredVars()) {
                    return()
                }

                # Step 1: Validate inputs (will stop on errors)
                validation_result <- private$.validateInputs()
                if (!validation_result) {
                    return()  # Halt execution if validation failed
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

                # Step 8: Render all notices
                private$.renderNotices()
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

                return(TRUE)  # All validation checks passed
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
                    private$.addNotice("ERROR", "No Complete Cases", "No complete cases available after removing missing data.")
                    return(NULL)
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
                        private$.addNotice("ERROR", "Missing Level",
                            sprintf('The specified positive level "%s" is not present in variable "%s" (%s). Please select a level that exists in the data.',
                                    rl$level, rl$var, rl$label))
                        return(NULL)
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

                # Check if epiR package is available
                if (!requireNamespace("epiR", quietly = TRUE)) {
                    private$.addNotice("ERROR", "epiR Package Missing",
                        'epiR package is required for diagnostic test analysis. Install with install.packages("epiR") or disable "Show Individual Test Statistics" option.')
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
                    private$.addNotice("WARNING", "Invalid Counts",
                        sprintf('Invalid counts detected for Test %d. Skipping individual analysis.', test_num))
                    return()
                }

                # Check if all counts are zero
                if (tp == 0 && fp == 0 && fn == 0 && tn == 0) {
                    private$.addNotice("WARNING", "All Zero Counts",
                        sprintf('No valid observations for Test %d. Skipping individual analysis.', test_num))
                    return()
                }

                # Apply continuity correction for zero cells to avoid infinite estimates
                use_continuity <- any(c(tp, fp, fn, tn) == 0)
                if (use_continuity) {
                    tp_adj <- tp + 0.5
                    fp_adj <- fp + 0.5
                    fn_adj <- fn + 0.5
                    tn_adj <- tn + 0.5
                    cont_table_for_epi <- matrix(c(tp_adj, fp_adj, fn_adj, tn_adj),
                                                 nrow = 2, byrow = TRUE,
                                                 dimnames = list(c("Positive", "Negative"), c("Positive", "Negative")))

                    private$.addNotice("INFO", "Continuity Correction",
                        sprintf('Continuity correction (+0.5) applied to Test %d due to zero cell count(s).', test_num))
                } else {
                    cont_table_for_epi <- cont_table
                }

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
                result <- epiR::epi.tests(cont_table_for_epi, conf.level = 0.95)

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

            .analyzeCombinations = function(data_prep) {
                # Generate and analyze all test combinations

                # Check if epiR package is available for combination analysis
                if (!requireNamespace("epiR", quietly = TRUE)) {
                    private$.addNotice("ERROR", "epiR Package Missing",
                        'epiR package is required for combination analysis. Install with install.packages("epiR").')
                    return()
                }

                # Inform users that PPV/NPV are based on sample prevalence
                private$.addNotice("INFO", "PPV/NPV Interpretation",
                    'Positive/Negative Predictive Values are calculated using the sample prevalence. Interpret cautiously if your sample does not reflect the target clinical population.')

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
                if (is.na(x) || n == 0) return(c(NA, NA))

                p <- x / n
                z <- qnorm((1 + conf.level) / 2)  # 1.96 for 95% CI

                # Wilson score formula
                denominator <- 1 + (z^2 / n)
                centre <- (p + (z^2 / (2 * n))) / denominator
                half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator

                # Return bounds, constrained to [0, 1]
                c(max(0, centre - half_width), min(1, centre + half_width))
            },

            .analyzeSinglePattern = function(data_prep, pattern_name, condition) {
                # Analyze a single test pattern

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

                # Extract counts
                tp <- cont_table[1, 1]
                fp <- cont_table[1, 2]
                fn <- cont_table[2, 1]
                tn <- cont_table[2, 2]

                # Validate counts - check for negative values
                if (any(is.na(c(tp, fp, fn, tn))) || any(c(tp, fp, fn, tn) < 0)) {
                    private$.addNotice("WARNING", "Invalid Counts",
                        sprintf('Invalid counts detected for pattern "%s". Skipping this combination.', pattern_name))
                    return()
                }

                # Check if all counts are zero
                if (tp == 0 && fp == 0 && fn == 0 && tn == 0) {
                    private$.addNotice("WARNING", "All Zero Counts",
                        sprintf('No observations found for pattern "%s". Skipping this combination.', pattern_name))
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
                    private$.addNotice("INFO", "Continuity Correction",
                        sprintf('Continuity correction (+0.5) applied to pattern "%s" due to zero cell count(s).', pattern_name))
                } else {
                    tp_adj <- tp
                    fp_adj <- fp
                    fn_adj <- fn
                    tn_adj <- tn
                }

                # Calculate statistics using epiR
                result <- epiR::epi.tests(cont_table, conf.level = 0.95)

                # Extract values - epiR returns $detail as a data frame
                detail_df <- as.data.frame(result$detail)

                sens <- detail_df[detail_df$statistic == "se", "est"]
                spec <- detail_df[detail_df$statistic == "sp", "est"]
                ppv <- detail_df[detail_df$statistic == "pv.pos", "est"]
                npv <- detail_df[detail_df$statistic == "pv.neg", "est"]
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
                ciTable$addRow(rowKey = paste0(pattern_name, "_lrPos"), values = list(
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
                ciTable$addRow(rowKey = paste0(pattern_name, "_lrNeg"), values = list(
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
                    se_log_dor <- sqrt(1/tp_adj + 1/fp_adj + 1/fn_adj + 1/tn_adj)
                    dor_lower <- exp(log_dor - 1.96 * se_log_dor)
                    dor_upper <- exp(log_dor + 1.96 * se_log_dor)
                } else {
                    dor_lower <- NA
                    dor_upper <- NA
                }
                ciTable$addRow(rowKey = paste0(pattern_name, "_dor"), values = list(
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
                private$.analyzeSinglePattern(data_prep, "Parallel (≥1 pos)", parallel_condition)

                # Serial strategy: Positive only if BOTH tests are positive (high specificity)
                serial_condition <- data_prep$test1Variable2 == "Positive" &
                                  data_prep$test2Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Serial (all pos)", serial_condition)
            },

            .addThreeTestStrategies = function(data_prep) {
                # Add clinical strategy rows for 3-test combinations

                # Parallel strategy: Positive if ANY test is positive (high sensitivity)
                parallel_condition <- data_prep$test1Variable2 == "Positive" |
                                    data_prep$test2Variable2 == "Positive" |
                                    data_prep$test3Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Parallel (≥1 pos)", parallel_condition)

                # Serial strategy: Positive only if ALL tests are positive (high specificity)
                serial_condition <- data_prep$test1Variable2 == "Positive" &
                                  data_prep$test2Variable2 == "Positive" &
                                  data_prep$test3Variable2 == "Positive"
                private$.analyzeSinglePattern(data_prep, "Serial (all pos)", serial_condition)

                # Majority rule: Positive if at least 2 of 3 tests are positive (balanced)
                t1_pos <- data_prep$test1Variable2 == "Positive"
                t2_pos <- data_prep$test2Variable2 == "Positive"
                t3_pos <- data_prep$test3Variable2 == "Positive"
                majority_condition <- (as.integer(t1_pos) + as.integer(t2_pos) + as.integer(t3_pos)) >= 2
                private$.analyzeSinglePattern(data_prep, "Majority (≥2/3 pos)", majority_condition)
            },

            .populateFrequencyTables = function(data_prep) {
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

                if (!has_test2) return()

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
                if (combTable$rowCount == 0) return()

                # Convert to data frame
                table_df <- combTable$asDF()

                # Find pattern with highest Youden's J
                # Prefer patterns with reasonable cell counts to avoid unstable choices
                table_df$min_cell <- apply(table_df[, c("tp", "fp", "fn", "tn")], 1, min)
                stable_df <- table_df[table_df$min_cell >= 5, ]
                if (nrow(stable_df) == 0) {
                    stable_df <- table_df  # fallback to all if none meet threshold
                    stability_note <- "No pattern meets the minimum cell count of 5; recommendation is based on all patterns (may be unstable). "
                } else {
                    stability_note <- ""
                }

                max_youden_idx <- which.max(stable_df$youden)
                best_pattern <- stable_df[max_youden_idx, ]

                # Generate rationale
                rationale <- sprintf(
                    "%sHighest Youden's J (%.3f) indicates optimal balance of sensitivity and specificity. ",
                    stability_note,
                    best_pattern$youden
                )

                if (best_pattern$sens > 0.8 && best_pattern$spec > 0.8) {
                    rationale <- paste0(rationale, "Excellent discriminatory performance.")
                } else if (best_pattern$sens > 0.7 && best_pattern$spec > 0.7) {
                    rationale <- paste0(rationale, "Good overall performance.")
                } else {
                    rationale <- paste0(rationale, "Trade-off between sensitivity and specificity.")
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

                # Add column to original dataset
                self$results$setColumn("test_pattern", pattern_values)
            },

            .plotBarChart = function(image, ...) {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) return(FALSE)

                table_df <- combTable$asDF()

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

                if (nrow(filtered_df) == 0) return(FALSE)

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

                # Create plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Pattern, y = Value, fill = Metric)) +
                    ggplot2::geom_bar(stat = "identity", position = "dodge") +
                    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
                    ggplot2::labs(
                        title = "Diagnostic Performance Comparison",
                        x = "Test Pattern",
                        y = "Value"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

                print(p)
                return(TRUE)
            },

            .plotHeatmap = function(image, ...) {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) return(FALSE)

                table_df <- combTable$asDF()
                pattern_filter <- self$options$filterPattern
                filtered_df <- private$.applyPatternFilter(table_df, pattern_filter)

                if (nrow(filtered_df) == 0) return(FALSE)

                # Select metrics for heatmap
                metrics <- c("sens", "spec", "ppv", "npv", "acc", "balancedAccuracy", "youden")
                metric_data <- filtered_df[, c("pattern", metrics)]

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
                if (ciTable$rowCount == 0) return(FALSE)

                table_df <- ciTable$asDF()

                # Filter by statistic
                stat_filter <- self$options$filterStatistic
                if (stat_filter != "all") {
                    table_df <- table_df[table_df$statistic == stat_filter, ]
                }

                if (nrow(table_df) == 0) return(FALSE)

                p <- ggplot2::ggplot(table_df, ggplot2::aes(x = estimate, y = pattern, color = statistic)) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.2) +
                    ggplot2::labs(
                        title = "Forest Plot - 95% Confidence Intervals",
                        x = "Estimate (95% CI)",
                        y = "Pattern"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::facet_wrap(~statistic, ncol = 1)

                print(p)
                return(TRUE)
            },

            .plotDecisionTree = function(image, ...) {
                combTable <- self$results$combinationTable
                if (combTable$rowCount == 0) return(FALSE)

                table_df <- combTable$asDF()

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
                if (filter_type == "all") return(data)

                # Pattern filtering logic
                if (filter_type == "allPositive") {
                    pattern <- grep("^\\+(/\\+)+$", data$pattern)
                } else if (filter_type == "allNegative") {
                    pattern <- grep("^-(/-)+$", data$pattern)
                } else if (filter_type == "mixed") {
                    pattern <- grep("[+-]/[+-]", data$pattern)
                    pattern <- pattern[!grepl("^\\+/\\+|^-/-", data$pattern[pattern])]
                } else {
                    # Default to all
                    return(data)
                }

                if (length(pattern) > 0) {
                    return(data[pattern, ])
                } else {
                    return(data)
                }
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for decisioncombine analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                gold <- self$options$gold
                test1 <- self$options$test1
                test2 <- self$options$test2

                if (is.null(gold) || is.null(test1) || is.null(test2))
                    return('')

                # Get arguments
                args <- ''
                if (!is.null(private$.asArgs)) {
                    args <- private$.asArgs(incData = FALSE)
                }
                if (args != '')
                    args <- paste0(',\n    ', args)

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

                # Build complete function call
                paste0(pkg_name, '::decisioncombine(\n    data = data', args, ')')
            }
        ) # End of public list
    )
