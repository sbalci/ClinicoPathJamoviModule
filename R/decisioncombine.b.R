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
                
                # Validate that at least one test is provided
                test_vars <- c(self$options$test1, self$options$test2, self$options$test3)
                test_vars <- test_vars[!is.null(test_vars) & test_vars != ""]
                
                if (length(test_vars) == 0) {
                    return()
                    # stop("At least one test variable is required for analysis")
                }

                # Data definition ----
                # Keep original data for level selection - don't remove NAs yet
                mydata <- self$data

                # Validate gold positive level is specified
                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    stop("Positive level for gold standard must be specified")
                }
                
                goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive)
                goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)
                goldPLevel <- unlist(goldPLevel)

                goldVariable <- jmvcore::constructFormula(terms = self$options$gold)
                goldVariable <- jmvcore::decomposeFormula(formula = goldVariable)
                goldVariable <- unlist(goldVariable)

                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])

                # Handle original data display with jamovi tables
                if (self$options$od) {
                    # Create frequency table for gold standard (use original data before NA removal)
                    freq_table <- table(mydata[[goldVariable]], useNA = "ifany")
                    total_n <- sum(freq_table, na.rm = TRUE)
                    
                    # Populate gold standard frequency table
                    goldFreqTable <- self$results$goldStandardFreqTable
                    for (level in names(freq_table)) {
                        level_display <- if (is.na(level)) "Missing" else as.character(level)
                        freq_count <- as.numeric(freq_table[level])
                        # Handle NA or zero counts properly
                        freq_pct <- if (!is.na(freq_count) && total_n > 0) freq_count / total_n else 0
                        
                        goldFreqTable$addRow(rowKey = level_display, values = list(
                            level = level_display,
                            frequency = freq_count,
                            percentage = freq_pct
                        ))
                    }

                    # Create cross-tabulation tables for all tests with gold standard
                    test_vars <- c(self$options$test1,
                                   self$options$test2,
                                   self$options$test3)
                    test_vars <- test_vars[!is.null(test_vars) &
                                               test_vars != ""]

                    if (length(test_vars) > 0) {
                        crossTabTable <- self$results$crossTabTable
                        
                        for (test_var in test_vars) {
                            # Create cross-tabulation (use original data with NAs shown)
                            cross_tab <- table(mydata[[test_var]], mydata[[goldVariable]], useNA = "ifany")
                            
                            # Add rows for each test level
                            for (i in seq_along(rownames(cross_tab))) {
                                test_level <- rownames(cross_tab)[i]
                                # Handle NA rows if present
                                test_level_display <- if (is.na(test_level)) "Missing" else as.character(test_level)
                                
                                # Calculate values for each gold standard level using safe indexing
                                gold_pos_count <- 0
                                tryCatch({
                                    # Use numeric indexing which is safer with tables
                                    col_idx <- which(colnames(cross_tab) == goldPLevel)
                                    if (length(col_idx) > 0) {
                                        gold_pos_count <- as.numeric(cross_tab[i, col_idx[1]])
                                    }
                                }, error = function(e) {
                                    # If any error, default to 0
                                    gold_pos_count <- 0
                                })
                                
                                # Calculate gold negative count (all non-positive levels)
                                row_total <- sum(cross_tab[i, ], na.rm = TRUE)
                                gold_neg_count <- row_total - gold_pos_count
                                
                                crossTabTable$addRow(
                                    rowKey = paste0(test_var, "_", test_level_display), 
                                    values = list(
                                        test_var = test_var,
                                        test_level = test_level_display,
                                        gold_positive = gold_pos_count,
                                        gold_negative = gold_neg_count,
                                        total = row_total
                                    )
                                )
                            }
                        }
                    }
                }

                # Get test variables and positive levels
                testVariables <- c(self$options$test1,
                                   self$options$test2,
                                   self$options$test3)
                testVariables <- testVariables[!is.null(testVariables) &
                                                   testVariables != ""]

                testPositives <- list()
                if (!is.null(self$options$test1) && self$options$test1 != "") {
                    if (is.null(self$options$test1Positive) || self$options$test1Positive == "") {
                        stop(paste("Positive level for", self$options$test1, "must be specified"))
                    }
                    testPositives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    if (is.null(self$options$test2Positive) || self$options$test2Positive == "") {
                        stop(paste("Positive level for", self$options$test2, "must be specified"))
                    }
                    testPositives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    if (is.null(self$options$test3Positive) || self$options$test3Positive == "") {
                        stop(paste("Positive level for", self$options$test3, "must be specified"))
                    }
                    testPositives[[self$options$test3]] <- self$options$test3Positive
                }

                # Create recoded data with positives and negatives for each test
                mydata2 <- mydata
                
                # Remove rows with NAs only for analysis variables (after level selection)
                analysis_vars <- c(goldVariable, testVariables)
                complete_rows <- complete.cases(mydata2[analysis_vars])
                
                if (sum(complete_rows) == 0) {
                    stop("No complete cases found for analysis variables. Please check for missing values.")
                }
                
                mydata2 <- mydata2[complete_rows, ]
                
                # Report data used for analysis
                n_original <- nrow(mydata)
                n_analysis <- nrow(mydata2)
                if (n_original != n_analysis) {
                    message(paste("Using", n_analysis, "of", n_original, "rows (", 
                                 n_original - n_analysis, "rows removed due to missing values in analysis variables)"))
                }

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

                        # Create contingency table with error handling
                        tryCatch({
                            conf_table <- table(mydata2[[recode_col_name]], mydata2[["goldVariable2"]])
                            
                            # Validate table structure
                            if (!all(dim(conf_table) == c(2, 2))) {
                                stop(paste("Invalid contingency table structure for", testVariable, 
                                          "- ensure both test and gold standard have exactly two levels"))
                            }
                            
                            # Extract values safely with bounds checking
                            TP <- if (nrow(conf_table) >= 1 && ncol(conf_table) >= 1) conf_table[1, 1] else 0
                            FP <- if (nrow(conf_table) >= 1 && ncol(conf_table) >= 2) conf_table[1, 2] else 0
                            FN <- if (nrow(conf_table) >= 2 && ncol(conf_table) >= 1) conf_table[2, 1] else 0
                            TN <- if (nrow(conf_table) >= 2 && ncol(conf_table) >= 2) conf_table[2, 2] else 0
                        }, error = function(e) {
                            stop(paste("Error creating contingency table for", testVariable, ":", e$message))
                        })

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
                    mydata2 <- private$.analyzeCombinations(mydata2, testVariables, goldVariable)
                    
                    # Export combination pattern to data if requested
                    if (self$options$addCombinationPattern && self$results$addCombinationPattern$isNotFilled()) {
                        # Create combination pattern for export
                        if ("combination_pattern" %in% names(mydata2)) {
                            # Export the combination pattern to the dataset
                            self$results$addCombinationPattern$setRowNums(rownames(mydata))
                            self$results$addCombinationPattern$setValues(mydata2$combination_pattern)
                        }
                    }
                }

                # End of analysis - individual tests and combinations provide all needed information
            },
            
            .generateTestPatterns = function(n_tests, test_names = NULL) {
                # Helper function to generate all possible test combination patterns
                if (n_tests == 2) {
                    patterns <- c("+/+", "+/-", "-/+", "-/-")
                    if (!is.null(test_names) && length(test_names) >= 2) {
                        descriptions <- c(
                            "Both tests positive",
                            paste(test_names[1], "positive,", test_names[2], "negative"),
                            paste(test_names[1], "negative,", test_names[2], "positive"),
                            "Both tests negative"
                        )
                    } else {
                        descriptions <- patterns
                    }
                } else if (n_tests == 3) {
                    patterns <- c("+/+/+", "+/+/-", "+/-/+", "+/-/-", 
                                 "-/+/+", "-/+/-", "-/-/+", "-/-/-")
                    descriptions <- c()
                    if (!is.null(test_names) && length(test_names) >= 3) {
                        for (pattern in patterns) {
                            test_results <- strsplit(pattern, "/")[[1]]
                            desc_parts <- c()
                            for (j in seq_along(test_results)) {
                                if (test_results[j] == "+") {
                                    desc_parts <- c(desc_parts, paste(test_names[j], "pos"))
                                } else {
                                    desc_parts <- c(desc_parts, paste(test_names[j], "neg"))
                                }
                            }
                            descriptions <- c(descriptions, paste(desc_parts, collapse = ", "))
                        }
                    } else {
                        descriptions <- patterns
                    }
                } else {
                    stop("Pattern generation only supports 2 or 3 tests")
                }
                
                return(list(patterns = patterns, descriptions = descriptions))
            },
            
            .calculateDiagnosticStats = function(tp, fp, fn, tn) {
                # Extracted diagnostic statistics calculation with Wilson CI
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
                
                # Calculate AUC (simplified - for single point)
                auc <- if(!is.na(sens) && !is.na(spec)) (sens + spec) / 2 else NA
                
                # Wilson score confidence interval function
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
                    sens = sens, spec = spec, ppv = ppv, npv = npv, acc = acc, auc = auc,
                    sens_ci = sens_ci, spec_ci = spec_ci, ppv_ci = ppv_ci,
                    npv_ci = npv_ci, acc_ci = acc_ci,
                    tp = tp, fp = fp, fn = fn, tn = tn
                )
            },

            .analyzeCombinations = function(mydata2, testVariables, goldVariable) {
                # Comprehensive analysis of all test combinations with full diagnostic statistics
                if (length(testVariables) < 2) return()
                
                # Add progress indication for large datasets
                if (nrow(mydata2) > 1000) {
                    private$.checkpoint("Analyzing test combinations for large dataset...")
                }
                
                # Get gold standard totals
                gold_table <- table(mydata2$goldVariable2)
                total_positive_gold <- gold_table["Positive"]
                total_negative_gold <- gold_table["Negative"]
                
                # Get pattern info using helper function
                pattern_info <- private$.generateTestPatterns(length(testVariables), testVariables)
                patterns <- pattern_info$patterns
                descriptions <- pattern_info$descriptions
                
                # Create combination patterns more efficiently using base R
                test_bins <- paste0(testVariables, "_bin")
                
                if (length(testVariables) == 2) {
                    # For 2 tests - create pattern directly
                    test1_bin <- test_bins[1]
                    test2_bin <- test_bins[2]
                    
                    # Create combination patterns using base R (more efficient)
                    mydata2$combination_pattern <- ifelse(
                        mydata2[[test1_bin]] == 1 & mydata2[[test2_bin]] == 1, "+/+",
                        ifelse(mydata2[[test1_bin]] == 1 & mydata2[[test2_bin]] == 0, "+/-",
                               ifelse(mydata2[[test1_bin]] == 0 & mydata2[[test2_bin]] == 1, "-/+",
                                      ifelse(mydata2[[test1_bin]] == 0 & mydata2[[test2_bin]] == 0, "-/-",
                                             "Unknown"))))
                    
                } else if (length(testVariables) == 3) {
                    # For 3 tests - create pattern string directly using base R
                    test1_bin <- test_bins[1]
                    test2_bin <- test_bins[2]
                    test3_bin <- test_bins[3]
                    
                    # Create combination patterns more efficiently
                    mydata2$combination_pattern <- paste(
                        ifelse(mydata2[[test1_bin]] == 1, "+", "-"),
                        ifelse(mydata2[[test2_bin]] == 1, "+", "-"),
                        ifelse(mydata2[[test3_bin]] == 1, "+", "-"),
                        sep = "/"
                    )
                    # Handle any NA or unexpected values
                    mydata2$combination_pattern[is.na(mydata2$combination_pattern) | 
                                              !mydata2$combination_pattern %in% patterns] <- "Unknown"
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
                        stats <- private$.calculateDiagnosticStats(tp, fp, fn, tn)
                        
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
                
                # Return mydata2 with combination_pattern column
                return(mydata2)
            }

        )
    )
