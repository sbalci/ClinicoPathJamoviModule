#' @title Combine Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#'

decisioncombineClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncombineClass",
        inherit = decisioncombineBase,
        private = list(
            .init = function() {
                # Initialization - no cTable needed anymore
            },

            .run = function() {
                if (length(self$options$gold) == 0)
                    return()

                if (nrow(self$data) == 0)
                    stop("Data contains no (complete) rows")

                # Data definition ----
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)

                goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive)
                goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)
                goldPLevel <- unlist(goldPLevel)

                goldVariable <- jmvcore::constructFormula(terms = self$options$gold)
                goldVariable <- jmvcore::decomposeFormula(formula = goldVariable)
                goldVariable <- unlist(goldVariable)

                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])

                # Handle original data display
                if (self$options$od) {
                    # Create frequency tables for original data
                    freq_table <- table(mydata[[goldVariable]])
                    self$results$text1$setContent(freq_table)

                    # Create a cross-tabulation of all tests with gold standard
                    test_vars <- c(self$options$test1,
                                   self$options$test2,
                                   self$options$test3)
                    test_vars <- test_vars[!is.null(test_vars) &
                                               test_vars != ""]

                    if (length(test_vars) > 0) {
                        html_tables <- ""
                        for (test_var in test_vars) {
                            # Create cross-tabulation
                            cross_tab <- table(mydata[[test_var]], mydata[[goldVariable]])
                            html_table <- knitr::kable(
                                cross_tab,
                                format = "html",
                                caption = paste(
                                    "Cross-tabulation of",
                                    test_var,
                                    "and",
                                    goldVariable
                                )
                            )
                            html_tables <- paste(html_tables, html_table, "<br><br>")
                        }
                        self$results$text2$setContent(html_tables)
                    }
                }

                # Get test variables and positive levels
                testVariables <- c(self$options$test1,
                                   self$options$test2,
                                   self$options$test3)
                testVariables <- testVariables[!is.null(testVariables) &
                                                   testVariables != ""]

                testPositives <- list()
                if (!is.null(self$options$test1) &&
                    self$options$test1 != "") {
                    testPositives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) &&
                    self$options$test2 != "") {
                    testPositives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) &&
                    self$options$test3 != "") {
                    testPositives[[self$options$test3]] <- self$options$test3Positive
                }

                # Create recoded data with positives and negatives for each test
                mydata2 <- mydata

                # Add gold standard recode
                mydata2 <- mydata2 %>%
                    dplyr::mutate(
                        goldVariable2 = dplyr::case_when(
                            .data[[goldVariable]] == goldPLevel ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        )
                    )

                mydata2 <- mydata2 %>%
                    dplyr::mutate(goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive"))

                # Process individual tests if requested
                showIndividual <- self$options$showIndividual

                # First create individual test binary columns
                for (i in seq_along(testVariables)) {
                    testVariable <- testVariables[i]
                    testPLevel <- testPositives[[testVariable]]

                    # Process test data
                    mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])

                    # Add recode for this test
                    binary_col_name <- paste0(testVariable, "_bin")
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            !!binary_col_name := dplyr::case_when(.data[[testVariable]] == testPLevel ~ 1, NA ~ NA_real_, TRUE ~ 0)
                        )

                    # Show individual test results if requested
                    if (showIndividual) {
                        # Recode this test
                        recode_col_name <- paste0(testVariable, "_recode")
                        mydata2 <- mydata2 %>%
                            dplyr::mutate(
                                !!recode_col_name := dplyr::case_when(
                                    .data[[testVariable]] == testPLevel ~ "Positive",
                                    NA ~ NA_character_,
                                    TRUE ~ "Negative"
                                )
                            )

                        mydata2 <- mydata2 %>%
                            dplyr::mutate(!!recode_col_name := forcats::fct_relevel(.data[[recode_col_name]], "Positive"))

                        # Create contingency table
                        conf_table <- table(mydata2[[recode_col_name]], mydata2[["goldVariable2"]])

                        # Extract values
                        TP <- conf_table[1, 1]
                        FP <- conf_table[1, 2]
                        FN <- conf_table[2, 1]
                        TN <- conf_table[2, 2]

                        # Get appropriate individual table
                        indTable <- NULL
                        if (testVariable == self$options$test1) {
                            indTable <- self$results$indTable1
                        } else if (testVariable == self$options$test2) {
                            indTable <- self$results$indTable2
                        } else if (testVariable == self$options$test3) {
                            indTable <- self$results$indTable3
                        }

                        if (!is.null(indTable)) {
                            indTable$addRow(
                                rowKey = "Test Positive",
                                values = list(
                                    newtest = "Test Positive",
                                    GP = TP,
                                    GN = FP,
                                    Total = TP + FP
                                )
                            )

                            indTable$addRow(
                                rowKey = "Test Negative",
                                values = list(
                                    newtest = "Test Negative",
                                    GP = FN,
                                    GN = TN,
                                    Total = FN + TN
                                )
                            )

                            indTable$addRow(
                                rowKey = "Total",
                                values = list(
                                    newtest = "Total",
                                    GP = TP + FN,
                                    GN = FP + TN,
                                    Total = TP + FP + FN + TN
                                )
                            )

                        }
                    }
                }

                # No combination logic - just analyze all test patterns

                # Add comprehensive test combination analysis
                if (length(testVariables) >= 2) {
                    private$.analyzeCombinations(mydata2, testVariables, goldVariable)
                }

                # End of analysis - individual tests and combinations provide all needed information
            },

            .analyzeCombinations = function(mydata2, testVariables, goldVariable) {
                # Comprehensive analysis of all test combinations with full diagnostic statistics
                if (length(testVariables) < 2) return()
                
                # Helper function to calculate diagnostic statistics with CI
                calcDiagStats <- function(tp, fp, fn, tn) {
                    total_pos <- tp + fn  # Total diseased
                    total_neg <- fp + tn  # Total healthy
                    total_test_pos <- tp + fp  # Total test positive
                    total_test_neg <- fn + tn  # Total test negative
                    total <- tp + fp + fn + tn
                    
                    # Calculate basic statistics
                    sens <- if(total_pos > 0) tp / total_pos else NA
                    spec <- if(total_neg > 0) tn / total_neg else NA
                    ppv <- if(total_test_pos > 0) tp / total_test_pos else NA
                    npv <- if(total_test_neg > 0) tn / total_test_neg else NA
                    acc <- if(total > 0) (tp + tn) / total else NA
                    
                    # Calculate 95% CI using Wilson score interval
                    calcCI <- function(x, n) {
                        if (is.na(x) || n == 0) return(c(NA, NA))
                        p <- x / n
                        z <- 1.96  # 95% CI
                        denominator <- 1 + (z^2 / n)
                        centre <- (p + (z^2 / (2 * n))) / denominator
                        half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator
                        c(max(0, centre - half_width), min(1, centre + half_width))
                    }
                    
                    sens_ci <- calcCI(tp, total_pos)
                    spec_ci <- calcCI(tn, total_neg) 
                    ppv_ci <- calcCI(tp, total_test_pos)
                    npv_ci <- calcCI(tn, total_test_neg)
                    acc_ci <- calcCI(tp + tn, total)
                    
                    list(
                        sens = sens, spec = spec, ppv = ppv, npv = npv, acc = acc,
                        sens_ci = sens_ci, spec_ci = spec_ci, ppv_ci = ppv_ci, 
                        npv_ci = npv_ci, acc_ci = acc_ci
                    )
                }
                
                # Get gold standard totals
                gold_table <- table(mydata2$goldVariable2)
                total_positive_gold <- gold_table["Positive"]
                total_negative_gold <- gold_table["Negative"]
                
                # Determine patterns based on number of tests
                if (length(testVariables) == 2) {
                    test1 <- testVariables[1]
                    test2 <- testVariables[2]
                    test1_bin <- paste0(test1, "_bin")
                    test2_bin <- paste0(test2, "_bin")
                    
                    # Create combination patterns
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            combination_pattern = dplyr::case_when(
                                .data[[test1_bin]] == 1 & .data[[test2_bin]] == 1 ~ "+/+",
                                .data[[test1_bin]] == 1 & .data[[test2_bin]] == 0 ~ "+/-",  
                                .data[[test1_bin]] == 0 & .data[[test2_bin]] == 1 ~ "-/+",
                                .data[[test1_bin]] == 0 & .data[[test2_bin]] == 0 ~ "-/-",
                                TRUE ~ "Unknown"
                            )
                        )
                    
                    patterns <- c("+/+", "+/-", "-/+", "-/-")
                    descriptions <- c(
                        "Both tests positive", 
                        paste(test1, "positive,", test2, "negative"),
                        paste(test1, "negative,", test2, "positive"), 
                        "Both tests negative"
                    )
                    
                } else if (length(testVariables) == 3) {
                    test1 <- testVariables[1]
                    test2 <- testVariables[2]
                    test3 <- testVariables[3]
                    test1_bin <- paste0(test1, "_bin")
                    test2_bin <- paste0(test2, "_bin")
                    test3_bin <- paste0(test3, "_bin")
                    
                    # Create combination patterns
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(
                            combination_pattern = dplyr::case_when(
                                .data[[test1_bin]] == 1 & .data[[test2_bin]] == 1 & .data[[test3_bin]] == 1 ~ "+/+/+",
                                .data[[test1_bin]] == 1 & .data[[test2_bin]] == 1 & .data[[test3_bin]] == 0 ~ "+/+/-",
                                .data[[test1_bin]] == 1 & .data[[test2_bin]] == 0 & .data[[test3_bin]] == 1 ~ "+/-/+",
                                .data[[test1_bin]] == 1 & .data[[test2_bin]] == 0 & .data[[test3_bin]] == 0 ~ "+/-/-",
                                .data[[test1_bin]] == 0 & .data[[test2_bin]] == 1 & .data[[test3_bin]] == 1 ~ "-/+/+",
                                .data[[test1_bin]] == 0 & .data[[test2_bin]] == 1 & .data[[test3_bin]] == 0 ~ "-/+/-",
                                .data[[test1_bin]] == 0 & .data[[test2_bin]] == 0 & .data[[test3_bin]] == 1 ~ "-/-/+",
                                .data[[test1_bin]] == 0 & .data[[test2_bin]] == 0 & .data[[test3_bin]] == 0 ~ "-/-/-",
                                TRUE ~ "Unknown"
                            )
                        )
                    
                    patterns <- c("+/+/+", "+/+/-", "+/-/+", "+/-/-", "-/+/+", "-/+/-", "-/-/+", "-/-/-")
                    
                    # Create descriptive names
                    descriptions <- c()
                    for (pattern in patterns) {
                        test_results <- strsplit(pattern, "/")[[1]]
                        desc_parts <- c()
                        for (i in seq_along(test_results)) {
                            test_name <- testVariables[i]
                            result <- if (test_results[i] == "+") "positive" else "negative"
                            desc_parts <- c(desc_parts, paste(test_name, result))
                        }
                        descriptions <- c(descriptions, paste(desc_parts, collapse = ", "))
                    }
                }
                
                # Calculate statistics for each combination
                combo_table <- table(mydata2$combination_pattern, mydata2$goldVariable2)
                
                # Create HTML summary
                num_tests <- length(testVariables)
                test_names <- paste(testVariables, collapse = ", ")
                summary_html <- paste0("<h4>Test Combination Patterns (", num_tests, " Tests)</h4>")
                summary_html <- paste0(summary_html, "<p>Analysis of all possible combinations of <b>", test_names, "</b>:</p>")
                
                # Populate diagnostic statistics tables
                for (i in seq_along(patterns)) {
                    pattern <- patterns[i]
                    description <- descriptions[i]
                    
                    if (pattern %in% rownames(combo_table)) {
                        # For each combination pattern, treat it as a "test" vs gold standard
                        # TP: Pattern present AND disease present
                        # FP: Pattern present AND disease absent
                        # FN: Pattern absent AND disease present  
                        # TN: Pattern absent AND disease absent
                        
                        tp <- combo_table[pattern, "Positive"]
                        fp <- combo_table[pattern, "Negative"]
                        fn <- total_positive_gold - tp
                        tn <- total_negative_gold - fp
                        
                        # Calculate diagnostic statistics
                        stats <- calcDiagStats(tp, fp, fn, tn)
                        
                        # Add to main statistics table
                        self$results$combStatsTable$addRow(
                            rowKey = pattern,
                            values = list(
                                combination = paste0(pattern, " (", description, ")"),
                                sens = stats$sens,
                                spec = stats$spec,
                                ppv = stats$ppv,
                                npv = stats$npv,
                                acc = stats$acc
                            )
                        )
                        
                        # Add to CI table (multiple rows per combination)
                        stat_names <- c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy")
                        stat_values <- list(stats$sens, stats$spec, stats$ppv, stats$npv, stats$acc)
                        stat_cis <- list(stats$sens_ci, stats$spec_ci, stats$ppv_ci, stats$npv_ci, stats$acc_ci)
                        
                        for (j in seq_along(stat_names)) {
                            if (!is.na(stat_values[[j]])) {
                                self$results$combStatsTableCI$addRow(
                                    rowKey = paste0(pattern, "_", j),
                                    values = list(
                                        combination = pattern,
                                        statistic = stat_names[j],
                                        estimate = stat_values[[j]],
                                        lower = stat_cis[[j]][1],
                                        upper = stat_cis[[j]][2]
                                    )
                                )
                            }
                        }
                    }
                }
                
                # Add clinical interpretation
                summary_html <- paste0(summary_html, "<p><b>Clinical Interpretation:</b></p>")
                summary_html <- paste0(summary_html, "<ul>")
                summary_html <- paste0(summary_html, "<li><b>Sensitivity:</b> Probability of combination being positive when disease is present</li>")
                summary_html <- paste0(summary_html, "<li><b>Specificity:</b> Probability of combination being negative when disease is absent</li>") 
                summary_html <- paste0(summary_html, "<li><b>PPV:</b> Probability of disease when combination is positive</li>")
                summary_html <- paste0(summary_html, "<li><b>NPV:</b> Probability of no disease when combination is negative</li>")
                summary_html <- paste0(summary_html, "<li><b>Accuracy:</b> Overall probability of correct classification</li>")
                summary_html <- paste0(summary_html, "</ul>")
                
                # Set content
                if ("combinationsAnalysis" %in% names(self$results)) {
                    self$results$combinationsAnalysis$setContent(summary_html)
                }
            }

        )
    )
