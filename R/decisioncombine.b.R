#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests

decisioncombineClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .init = function() {
                # Minimal initialization
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

                # Step 4: Combination analysis
                private$.analyzeCombinations(data_prep)

                # Step 5: Prepare plot data (if requested)
                if (self$options$showPlot) {
                    private$.preparePlotData(data_prep)
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

                if (length(self$options$test1) == 0 || self$options$test1 == "") {
                    stop("Test 1 is required. Please select at least one test variable.")
                }

                if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                    stop("Please select the positive level for Test 1.")
                }

                # Check if we have at least 2 tests for combination analysis
                has_test2 <- !is.null(self$options$test2) && self$options$test2 != ""

                if (has_test2) {
                    if (is.null(self$options$test2Positive) || self$options$test2Positive == "") {
                        stop("Please select the positive level for Test 2.")
                    }
                }

                # Check test3 only if provided
                has_test3 <- !is.null(self$options$test3) && self$options$test3 != ""
                if (has_test3) {
                    if (is.null(self$options$test3Positive) || self$options$test3Positive == "") {
                        stop("Please select the positive level for Test 3.")
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

                # Add to main table
                combTable <- self$results$combinationTable
                combTable$addRow(rowKey = pattern_name, values = list(
                    pattern = pattern_name,
                    tp = tp,
                    fp = fp,
                    fn = fn,
                    tn = tn,
                    sens = sens,
                    spec = spec,
                    ppv = ppv,
                    npv = npv,
                    acc = acc
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

            .preparePlotData = function(data_prep) {
                # Store data for plotting
                # This will be called by .plotPerformance when image is rendered

                # Extract combination table data
                combTable <- self$results$combinationTable

                if (combTable$rowCount == 0) {
                    return()
                }

                # Convert table to data frame (recommended method from jamovi tables guide)
                table_df <- combTable$asDF()

                # Create plot data structure from data frame
                plot_data <- list()
                for (i in seq_len(nrow(table_df))) {
                    plot_data[[i]] <- list(
                        pattern = table_df$pattern[i],
                        sens = table_df$sens[i],
                        spec = table_df$spec[i],
                        ppv = table_df$ppv[i],
                        npv = table_df$npv[i],
                        acc = table_df$acc[i]
                    )
                }

                # Store in image state
                image <- self$results$performancePlot
                image$setState(plot_data)
            },

            .plotPerformance = function(image, ...) {
                # Simple performance visualization

                plot_data <- image$state

                if (is.null(plot_data) || length(plot_data) == 0) {
                    return(FALSE)
                }

                # Convert to data frame
                patterns <- sapply(plot_data, function(x) x$pattern)
                sens <- sapply(plot_data, function(x) x$sens)
                spec <- sapply(plot_data, function(x) x$spec)
                ppv <- sapply(plot_data, function(x) x$ppv)
                npv <- sapply(plot_data, function(x) x$npv)
                acc <- sapply(plot_data, function(x) x$acc)

                # Create long format data
                df <- data.frame(
                    Pattern = rep(patterns, 5),
                    Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"),
                                each = length(patterns)),
                    Value = c(sens, spec, ppv, npv, acc),
                    stringsAsFactors = FALSE
                )

                # Create grouped bar plot
                p <- ggplot2::ggplot(df, ggplot2::aes(x = Pattern, y = Value, fill = Metric)) +
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
            }
        )
    )
