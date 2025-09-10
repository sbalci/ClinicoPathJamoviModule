#' @title Compare Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#'

decisioncompareClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisioncompareClass",
        inherit = decisioncompareBase,
        private = list(
            
            # Clinical thresholds for likelihood ratio interpretation
            LR_CLINICAL_THRESHOLDS = list(
                excellent_pos = 10,    # LR+ > 10 = excellent for ruling in disease
                good_pos = 5,          # LR+ 5-10 = good evidence
                fair_pos = 2,          # LR+ 2-5 = weak but potentially useful
                excellent_neg = 0.1,   # LR- < 0.1 = excellent for ruling out disease
                good_neg = 0.2,        # LR- 0.1-0.2 = good evidence
                fair_neg = 0.5         # LR- 0.2-0.5 = weak but potentially useful
            ),

            # Initialization - visibility now handled by .r.yaml
            .init = function() {
                # Initialize table rows for dynamic population
                private$.initializeTables()
                
                # Generate about this analysis panel
                private$.generateAboutPanel()
            }

            ,
            
            # Main analysis orchestrator - refactored for clarity
            .run = function() {
                tryCatch({
                    # Step 1: Comprehensive input validation
                    private$.validateInputs()
                    
                    # Step 2: Process and clean data
                    processed_data <- private$.prepareData()
                    
                    # Step 3: Process each test and calculate metrics
                    test_results <- private$.processAllTests(processed_data)
                    
                    # Step 4: Populate comparison table
                    private$.populateComparisonTable(test_results)
                    
                    # Step 5: Handle original data display if requested
                    if (self$options$od) {
                        private$.displayOriginalData(processed_data)
                    }
                    
                    # Step 6: Perform statistical comparisons if requested
                    if (self$options$statComp && length(test_results) >= 2) {
                        private$.performStatisticalComparisons(test_results)
                    }
                    
                    # Step 7: Setup visualizations if requested
                    if (self$options$plot || self$options$radarplot) {
                        private$.setupVisualizations(test_results)
                    }
                    
                    # Step 8: Generate clinical report and summary
                    private$.generateClinicalReport(test_results, processed_data)
                    
                }, error = function(e) {
                    stop(jmvcore::.("Analysis failed: {error}. Please check your data and variable selections.", error = conditionMessage(e)))
                })
            },

            
            # Comprehensive input validation with informative error messages
            .validateInputs = function() {
                # Check for required gold standard
                if (length(self$options$gold) == 0) {
                    stop(jmvcore::.("Gold standard variable is required. Please select a gold standard variable from your dataset."))
                }
                
                # Check for valid data
                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop(jmvcore::.("No data provided for analysis. Please ensure your dataset contains data."))
                }
                
                # Enhanced test validation with specific missing test identification
                private$.validateTestSelection()
                
                # Validate prevalence setting
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    stop(jmvcore::.("Prior probability must be between 0 and 1. Current value: {value}", value = self$options$pprob))
                }
                
                # Check for conflicting options
                if (self$options$pp && self$options$ci) {
                    stop(jmvcore::.("Prior probability and confidence intervals cannot both be enabled. Please choose one option."))
                }
            },
            
            # Enhanced test selection validation with specific guidance
            .validateTestSelection = function() {
                tests <- list(
                    "Test 1" = self$options$test1,
                    "Test 2" = self$options$test2, 
                    "Test 3" = self$options$test3
                )
                
                # Check which tests are properly specified
                valid_tests <- character(0)
                missing_tests <- character(0)
                
                for (test_name in names(tests)) {
                    test_value <- tests[[test_name]]
                    if (!is.null(test_value) && test_value != "") {
                        valid_tests <- c(valid_tests, test_name)
                    } else {
                        missing_tests <- c(missing_tests, test_name)
                    }
                }
                
                # Require at least 2 tests for comparison
                if (length(valid_tests) < 2) {
                    if (length(valid_tests) == 0) {
                        stop(jmvcore::.("No test variables selected. Please select at least Test 1 and Test 2 for comparison analysis."))
                    } else if (length(valid_tests) == 1) {
                        available <- paste(valid_tests, collapse = ", ")
                        needed <- paste(missing_tests[1:2], collapse = " or ")
                        stop(jmvcore::.("Only {available} is selected. Please also select {needed} to perform comparison analysis.", available = available, needed = needed))
                    }
                }
                
                # Validate that positive levels are specified for selected tests
                test_positive_pairs <- list(
                    list(test = self$options$test1, positive = self$options$test1Positive, name = "Test 1"),
                    list(test = self$options$test2, positive = self$options$test2Positive, name = "Test 2"),
                    list(test = self$options$test3, positive = self$options$test3Positive, name = "Test 3")
                )
                
                missing_positives <- character(0)
                for (pair in test_positive_pairs) {
                    if (!is.null(pair$test) && pair$test != "") {
                        if (is.null(pair$positive) || pair$positive == "") {
                            missing_positives <- c(missing_positives, pair$name)
                        }
                    }
                }
                
                if (length(missing_positives) > 0) {
                    stop(jmvcore::.("Positive level not specified for: {tests}. Please select the positive result level for each test variable.", 
                                   tests = paste(missing_positives, collapse = ", ")))
                }
            },
            
            # Data preparation and cleaning
            .prepareData = function() {
                # Clean data and handle missing values
                mydata <- jmvcore::naOmit(self$data)
                
                if (nrow(mydata) < nrow(self$data)) {
                    warning(jmvcore::.("Removed {count} rows with missing values", 
                                      count = nrow(self$data) - nrow(mydata)))
                }
                
                if (nrow(mydata) == 0) {
                    stop(jmvcore::.("Data contains no complete rows after removing missing values"))
                }
                
                # Extract variable names
                goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive) %>%
                             jmvcore::decomposeFormula() %>% unlist()
                goldVariable <- jmvcore::constructFormula(terms = self$options$gold) %>%
                               jmvcore::decomposeFormula() %>% unlist()
                
                # Convert to factors
                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])
                
                return(list(
                    data = mydata,
                    goldVariable = goldVariable,
                    goldPLevel = goldPLevel
                ))
            },
            
            # Initialize table structures
            .initializeTables = function() {
                # Add rows to contingency tables
                for (i in 1:3) {
                    table_name <- paste0("cTable", i)
                    if (table_name %in% names(self$results)) {
                        cTable <- self$results[[table_name]]
                        cTable$addRow(rowKey = "Test Positive", values = list(newtest = "Test Positive"))
                        cTable$addRow(rowKey = "Test Negative", values = list(newtest = "Test Negative"))
                        cTable$addRow(rowKey = "Total", values = list(newtest = "Total"))
                    }
                }
            },

            # Display original data tables
            .displayOriginalData = function(processed_data) {
                mydata <- processed_data$data
                goldVariable <- processed_data$goldVariable
                
                # Create frequency table for gold standard
                freq_table <- table(mydata[[goldVariable]])
                self$results$text1$setContent(freq_table)
                
                # Create cross-tabulation tables
                test_vars <- private$.getTestVariables()
                
                if (length(test_vars) > 0) {
                    html_tables <- ""
                    for (test_var in test_vars) {
                        cross_tab <- table(mydata[[test_var]], mydata[[goldVariable]])
                        html_table <- knitr::kable(
                            cross_tab,
                            format = "html",
                            caption = paste("Cross-tabulation of", test_var, "and", goldVariable)
                        )
                        html_tables <- paste(html_tables, html_table, "<br><br>")
                    }
                    self$results$text2$setContent(html_tables)
                }
            },

            # Get valid test variables
            .getTestVariables = function() {
                testVariables <- c(self$options$test1, self$options$test2, self$options$test3)
                testVariables <- testVariables[!is.null(testVariables) & testVariables != ""]
                return(testVariables)
            },
            
            # Get test positive levels mapping
            .getTestPositives = function() {
                testPositives <- list()
                if (!is.null(self$options$test1) && self$options$test1 != "") {
                    testPositives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    testPositives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    testPositives[[self$options$test3]] <- self$options$test3Positive
                }
                return(testPositives)
            },

            # Process all tests and calculate metrics
            .processAllTests = function(processed_data) {
                mydata <- processed_data$data
                goldVariable <- processed_data$goldVariable
                goldPLevel <- processed_data$goldPLevel
                
                testVariables <- private$.getTestVariables()
                testPositives <- private$.getTestPositives()
                
                test_results <- list()
                
                for (i in seq_along(testVariables)) {
                    testVariable <- testVariables[i]
                    testPLevel <- testPositives[[testVariable]]
                    
                    # Process individual test
                    result <- private$.processSingleTest(
                        mydata, testVariable, testPLevel, goldVariable, goldPLevel, i
                    )
                    
                    test_results[[testVariable]] <- result
                }
                
                return(test_results)
            },

            # Process a single test and calculate all metrics
            .processSingleTest = function(mydata, testVariable, testPLevel, goldVariable, goldPLevel, test_index) {
                # Convert to factor
                mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])
                
                # Recode data to positive/negative with efficient pipeline
                mydata2 <- mydata %>%
                    dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            .data[[testVariable]] == testPLevel ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        ),
                        goldVariable2 = dplyr::case_when(
                            .data[[goldVariable]] == goldPLevel ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        testVariable2 = forcats::fct_relevel(testVariable2, "Positive"),
                        goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive")
                    )
                
                # Create confusion table
                conf_table <- table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])
                
                # Extract confusion matrix values
                TP <- conf_table[1, 1]
                FP <- conf_table[1, 2]
                FN <- conf_table[2, 1]
                TN <- conf_table[2, 2]
                
                # Calculate basic metrics
                metrics <- private$.calculateDiagnosticMetrics(TP, FP, FN, TN)
                
                # Populate contingency table
                private$.populateContingencyTable(test_index, testVariable, TP, FP, FN, TN)
                
                # Calculate confidence intervals if requested
                if (self$options$ci) {
                    private$.populateConfidenceIntervals(test_index, testVariable, conf_table)
                }
                
                return(list(
                    variable = testVariable,
                    metrics = metrics,
                    conf_table = conf_table,
                    test_results = mydata2[["testVariable2"]],
                    TP = TP, FP = FP, FN = FN, TN = TN
                ))
            },

            # Helper function to calculate diagnostic metrics with prevalence logic
            .calculateDiagnosticMetrics = function(TP, FP, FN, TN) {
                TotalPop <- TP + TN + FP + FN
                DiseaseP <- TP + FN
                DiseaseN <- TN + FP
                TestP <- TP + FP
                TestN <- TN + FN
                
                # Basic metrics
                Sens <- TP / DiseaseP
                Spec <- TN / DiseaseN
                AccurT <- (TP + TN) / TotalPop
                
                # Calculate PPV and NPV based on prevalence setting
                if (self$options$pp && !is.na(self$options$pprob)) {
                    # Use provided prevalence with Bayes' theorem
                    PriorProb <- self$options$pprob
                    PPV <- (Sens * PriorProb) / ((Sens * PriorProb) + ((1 - Spec) * (1 - PriorProb)))
                    NPV <- (Spec * (1 - PriorProb)) / (((1 - Sens) * PriorProb) + (Spec * (1 - PriorProb)))
                } else {
                    # Use sample-based calculation
                    PPV <- TP / TestP
                    NPV <- TN / TestN
                    PriorProb <- DiseaseP / TotalPop
                }
                
                # Likelihood ratios
                LRP <- Sens / (1 - Spec)
                LRN <- (1 - Sens) / Spec
                
                return(list(
                    Sens = Sens, Spec = Spec, AccurT = AccurT,
                    PPV = PPV, NPV = NPV, LRP = LRP, LRN = LRN,
                    PriorProb = PriorProb
                ))
            },
            
            # Populate contingency table for a specific test
            .populateContingencyTable = function(test_index, testVariable, TP, FP, FN, TN) {
                table_name <- paste0("cTable", test_index)
                if (table_name %in% names(self$results)) {
                    cTable <- self$results[[table_name]]
                    
                    cTable$setRow(
                        rowKey = "Test Positive",
                        values = list(
                            newtest = jmvcore::.("Test Positive"),
                            GP = TP, GN = FP, Total = TP + FP
                        )
                    )
                    
                    cTable$setRow(
                        rowKey = "Test Negative",
                        values = list(
                            newtest = jmvcore::.("Test Negative"),
                            GP = FN, GN = TN, Total = FN + TN
                        )
                    )
                    
                    cTable$setRow(
                        rowKey = "Total",
                        values = list(
                            newtest = jmvcore::.("Total"),
                            GP = TP + FN, GN = FP + TN, Total = TP + FP + FN + TN
                        )
                    )
                    
                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addContingencyTableFootnotes(cTable)
                    }
                }
            },
            
            # Add footnotes to contingency tables
            .addContingencyTableFootnotes = function(cTable) {
                cTable$addFootnote(rowKey = "Test Positive", col = "GP", jmvcore::.("True Positive (TP)"))
                cTable$addFootnote(rowKey = "Test Positive", col = "GN", jmvcore::.("False Positive (FP)"))
                cTable$addFootnote(rowKey = "Test Negative", col = "GP", jmvcore::.("False Negative (FN)"))
                cTable$addFootnote(rowKey = "Test Negative", col = "GN", jmvcore::.("True Negative (TN)"))
            },

            # Populate comparison table with all test results
            .populateComparisonTable = function(test_results) {
                comparisonTable <- self$results$comparisonTable
                
                for (test_name in names(test_results)) {
                    result <- test_results[[test_name]]
                    metrics <- result$metrics
                    
                    comparisonTable$addRow(
                        rowKey = test_name,
                        values = list(
                            test = test_name,
                            Sens = metrics$Sens,
                            Spec = metrics$Spec,
                            AccurT = metrics$AccurT,
                            PPV = metrics$PPV,
                            NPV = metrics$NPV,
                            LRP = metrics$LRP,
                            LRN = metrics$LRN
                        )
                    )
                    
                    # Add clinical interpretation
                    clinical_interpretation <- private$.generateClinicalInterpretation(metrics)
                    comparisonTable$addFormat(rowKey = test_name, col = "test", format = jmvcore::Cell.BEGIN_GROUP)
                    comparisonTable$addRow(
                        rowKey = paste0(test_name, "_interp"),
                        values = list(
                            test = paste0("  → ", clinical_interpretation),
                            Sens = "", Spec = "", AccurT = "", PPV = "", NPV = "", LRP = "", LRN = ""
                        )
                    )
                    comparisonTable$addFormat(rowKey = paste0(test_name, "_interp"), col = "test", format = jmvcore::Cell.END_GROUP)
                    
                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addComparisonTableFootnotes(comparisonTable, test_name)
                    }
                }
            },
            
            # Generate clinical interpretation for test performance
            .generateClinicalInterpretation = function(metrics) {
                sens_pct <- metrics$Sens * 100
                spec_pct <- metrics$Spec * 100
                ppv_pct <- metrics$PPV * 100
                npv_pct <- metrics$NPV * 100
                lrp <- metrics$LRP
                lrn <- metrics$LRN
                
                # Determine primary clinical strength
                interpretations <- c()
                
                if (sens_pct >= 95 && spec_pct >= 95) {
                    interpretations <- c(interpretations, "Excellent overall performance")
                } else if (sens_pct >= 95) {
                    interpretations <- c(interpretations, "Excellent for screening (rule-out)")
                } else if (spec_pct >= 95) {
                    interpretations <- c(interpretations, "Excellent for confirmation (rule-in)")
                } else if (sens_pct >= 85 && spec_pct >= 85) {
                    interpretations <- c(interpretations, "Good balanced performance")
                } else if (sens_pct >= 85) {
                    interpretations <- c(interpretations, "Good sensitivity for screening")
                } else if (spec_pct >= 85) {
                    interpretations <- c(interpretations, "Good specificity for confirmation")
                }
                
                # Add likelihood ratio interpretation
                if (!is.na(lrp) && lrp >= 10) {
                    interpretations <- c(interpretations, "Strong positive evidence")
                } else if (!is.na(lrp) && lrp >= 5) {
                    interpretations <- c(interpretations, "Moderate positive evidence")
                }
                
                if (!is.na(lrn) && lrn <= 0.1) {
                    interpretations <- c(interpretations, "Strong negative evidence")
                } else if (!is.na(lrn) && lrn <= 0.2) {
                    interpretations <- c(interpretations, "Moderate negative evidence")
                }
                
                # Combine interpretations or provide fallback
                if (length(interpretations) > 0) {
                    return(paste(interpretations, collapse = "; "))
                } else {
                    return("Limited diagnostic utility - consider combining with other tests")
                }
            },
            
            # Add footnotes to comparison table
            .addComparisonTableFootnotes = function(comparisonTable, test_name) {
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "Sens",
                    "Sensitivity = TP/(TP+FN) = Probability of positive test among diseased"
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "Spec",
                    "Specificity = TN/(TN+FP) = Probability of negative test among healthy"
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "PPV",
                    "Positive Predictive Value = TP/(TP+FP) = Probability of disease when test is positive"
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "NPV",
                    "Negative Predictive Value = TN/(TN+FN) = Probability of being healthy when test is negative"
                )
            },

            # Populate confidence intervals for a specific test
            .populateConfidenceIntervals = function(test_index, testVariable, conf_table) {
                table_name <- paste0("epirTable", test_index)
                if (table_name %in% names(self$results)) {
                    epirTable <- self$results[[table_name]]
                    
                    tryCatch({
                        epirresult <- epiR::epi.tests(dat = conf_table)
                        epirresult2 <- summary(epirresult)
                        epirresult2 <- as.data.frame(epirresult2) %>%
                            tibble::rownames_to_column(.data = ., var = 'statsabv')
                        
                        epirresult2$statsnames <- c(
                            jmvcore::.("Apparent prevalence"), jmvcore::.("True prevalence"), jmvcore::.("Test sensitivity"),
                            jmvcore::.("Test specificity"), jmvcore::.("Diagnostic accuracy"), jmvcore::.("Diagnostic odds ratio"),
                            jmvcore::.("Number needed to diagnose"), jmvcore::.("Youden's index"),
                            jmvcore::.("Positive predictive value"), jmvcore::.("Negative predictive value"),
                            jmvcore::.("Likelihood ratio of a positive test"), jmvcore::.("Likelihood ratio of a negative test"),
                            jmvcore::.("Proportion of subjects with the outcome ruled out"),
                            jmvcore::.("Proportion of subjects with the outcome ruled in"),
                            jmvcore::.("Proportion of false positives"), jmvcore::.("Proportion of false negative"),
                            jmvcore::.("False Discovery Rate"), jmvcore::.("False Omission Rate")
                        )
                        
                        ratiorows <- c("ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                                      "p.tpdn", "p.tndp", "p.dntp", "p.dptn")
                        epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]
                        
                        for (i in seq_along(epirresult_ratio[, 1, drop = TRUE])) {
                            epirTable$addRow(rowKey = i, values = c(epirresult_ratio[i, ]))
                        }
                    }, error = function(e) {
                        n_sample <- sum(conf_table)
                        enhanced_msg <- jmvcore::.("Could not calculate confidence intervals for {test} (n={n}). This may be due to insufficient sample size or extreme values. Error: {error}", 
                                                  test = testVariable, n = n_sample, error = conditionMessage(e))
                        warning(enhanced_msg)
                    })
                }
            },

            # Perform statistical comparisons between tests with progress reporting
            .performStatisticalComparisons = function(test_results) {
                mcnemarTable <- self$results$mcnemarTable
                diffTable <- self$results$diffTable
                
                # Generate all pairwise combinations
                test_names <- names(test_results)
                test_pairs <- combn(test_names, 2, simplify = FALSE)
                
                # Progress reporting for multiple comparisons
                n_comparisons <- length(test_pairs)
                if (n_comparisons > 1) {
                    message(sprintf("Performing %d pairwise statistical comparisons...", n_comparisons))
                }
                
                for (i in seq_along(test_pairs)) {
                    pair <- test_pairs[[i]]
                    test1 <- pair[1]
                    test2 <- pair[2]
                    comparison_name <- paste(test1, "vs", test2)
                    
                    # Progress indicator for large numbers of comparisons
                    if (n_comparisons > 3 && i %% max(1, floor(n_comparisons/3)) == 0) {
                        message(sprintf("Completed %d/%d comparisons...", i, n_comparisons))
                    }
                    
                    # McNemar's test
                    private$.performMcNemarTest(test_results, test1, test2, comparison_name, mcnemarTable)
                    
                    # Confidence intervals for differences
                    private$.calculateDifferences(test_results, test1, test2, comparison_name, diffTable)
                }
                
                if (n_comparisons > 1) {
                    message(sprintf("Statistical comparisons completed successfully (%d total comparisons).", n_comparisons))
                }
            },
            
            # Perform McNemar's test with clinical interpretation
            .performMcNemarTest = function(test_results, test1, test2, comparison_name, mcnemarTable) {
                test1_results <- test_results[[test1]]$test_results
                test2_results <- test_results[[test2]]$test_results
                
                tryCatch({
                    mcnemar_table <- table(test1_results, test2_results)
                    mcnemar_result <- stats::mcnemar.test(mcnemar_table)
                    
                    # Clinical interpretation of p-value
                    interpretation <- dplyr::case_when(
                        mcnemar_result$p.value < 0.001 ~ "Highly significant difference (p<0.001)",
                        mcnemar_result$p.value < 0.01 ~ "Significant difference (p<0.01)", 
                        mcnemar_result$p.value < 0.05 ~ "Statistically significant difference (p<0.05)",
                        mcnemar_result$p.value < 0.1 ~ "Marginally significant difference (p<0.1)",
                        TRUE ~ "No significant difference (p≥0.1)"
                    )
                    
                    mcnemarTable$addRow(
                        rowKey = comparison_name,
                        values = list(
                            comparison = comparison_name,
                            stat = mcnemar_result$statistic,
                            df = mcnemar_result$parameter,
                            p = mcnemar_result$p.value,
                            interpretation = interpretation
                        )
                    )
                }, error = function(e) {
                    n1 <- length(test1_results)
                    n2 <- length(test2_results)
                    enhanced_msg <- jmvcore::.("Could not perform McNemar's test for {comparison} (n1={n1}, n2={n2}). This may be due to insufficient discordant pairs or identical test results. Error: {error}", 
                                              comparison = comparison_name, n1 = n1, n2 = n2, error = conditionMessage(e))
                    warning(enhanced_msg)
                })
            },
            
            # Calculate confidence intervals for metric differences
            .calculateDifferences = function(test_results, test1, test2, comparison_name, diffTable) {
                metrics <- c("Sens", "Spec", "PPV", "NPV")
                
                for (metric in metrics) {
                    tryCatch({
                        value1 <- test_results[[test1]]$metrics[[metric]]
                        value2 <- test_results[[test2]]$metrics[[metric]]
                        diff <- value1 - value2
                        
                        # Calculate sample sizes
                        n1 <- sum(test_results[[test1]]$conf_table)
                        n2 <- sum(test_results[[test2]]$conf_table)
                        
                        # Standard error for difference
                        se_diff <- sqrt((value1 * (1 - value1) / n1) + (value2 * (1 - value2) / n2))
                        
                        # 95% CI
                        z <- 1.96
                        lower <- diff - z * se_diff
                        upper <- diff + z * se_diff
                        
                        diffTable$addRow(
                            rowKey = paste(comparison_name, metric, sep = "_"),
                            values = list(
                                comparison = comparison_name,
                                metric = metric,
                                diff = diff,
                                lower = lower,
                                upper = upper
                            )
                        )
                    }, error = function(e) {
                        enhanced_msg <- jmvcore::.("Could not calculate {metric} difference for {comparison} (n1={n1}, n2={n2}). Check for extreme values or zero denominators. Error: {error}", 
                                                  metric = metric, comparison = comparison_name, n1 = n1, n2 = n2, error = conditionMessage(e))
                        warning(enhanced_msg)
                    })
                }
            },

            # Setup visualizations with improved data structure
            .setupVisualizations = function(test_results) {
                plotData <- list()
                
                for (test_name in names(test_results)) {
                    result <- test_results[[test_name]]
                    metrics <- result$metrics
                    
                    plotData[[test_name]] <- list(
                        test = test_name,
                        Sens = metrics$Sens,
                        Spec = metrics$Spec,
                        AccurT = metrics$AccurT,  # Include accuracy for radar plot
                        PPV = metrics$PPV,
                        NPV = metrics$NPV,
                        LRP = metrics$LRP,
                        LRN = metrics$LRN
                    )
                }
                
                if (self$options$plot) {
                    self$results$plot1$setState(plotData)
                }
                
                if (self$options$radarplot) {
                    self$results$plotRadar$setState(plotData)
                }
            },
            
            # Optimized bar plot data building - eliminates inefficient rbind operations
            .buildBarPlotData = function(plotData) {
                if (is.null(plotData) || length(plotData) == 0) return(data.frame())
                
                # Pre-calculate total rows needed
                n_tests <- length(plotData)
                n_metrics <- 4  # Sens, Spec, PPV, NPV
                total_rows <- n_tests * n_metrics
                
                # Pre-allocate vectors for efficiency
                test_names <- character(total_rows)
                metric_names <- character(total_rows)
                values <- numeric(total_rows)
                
                # Fill vectors efficiently
                row_idx <- 1
                metrics <- c("Sensitivity", "Specificity", "PPV", "NPV")
                metric_keys <- c("Sens", "Spec", "PPV", "NPV")
                
                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]
                    for (i in seq_along(metrics)) {
                        test_names[row_idx] <- test_name
                        metric_names[row_idx] <- metrics[i]
                        values[row_idx] <- test_data[[metric_keys[i]]]
                        row_idx <- row_idx + 1
                    }
                }
                
                return(data.frame(
                    test = test_names,
                    metric = metric_names,
                    value = values,
                    stringsAsFactors = FALSE
                ))
            },
            
            # Optimized radar plot data building with clinical scaling
            .buildRadarPlotData = function(plotData) {
                if (is.null(plotData) || length(plotData) == 0) return(data.frame())
                
                # Pre-calculate dimensions
                n_tests <- length(plotData)
                n_metrics <- 7  # 5 percentage metrics + 2 LR quality metrics
                total_rows <- n_tests * n_metrics
                
                # Pre-allocate vectors
                test_names <- character(total_rows)
                metric_names <- character(total_rows)
                values <- numeric(total_rows)
                scaled_values <- numeric(total_rows)
                
                # Define metrics and their processing
                pct_metrics <- list(
                    "Sensitivity" = "Sens",
                    "Specificity" = "Spec", 
                    "Accuracy" = "AccurT",
                    "PPV" = "PPV",
                    "NPV" = "NPV"
                )
                
                row_idx <- 1
                thresholds <- private$LR_CLINICAL_THRESHOLDS
                
                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]
                    
                    # Process percentage metrics (direct 0-100 scaling)
                    for (metric_label in names(pct_metrics)) {
                        metric_key <- pct_metrics[[metric_label]]
                        test_names[row_idx] <- test_name
                        metric_names[row_idx] <- metric_label
                        values[row_idx] <- test_data[[metric_key]]
                        scaled_values[row_idx] <- test_data[[metric_key]] * 100
                        row_idx <- row_idx + 1
                    }
                    
                    # Process LR+ with clinical thresholds
                    lrp_value <- test_data$LRP
                    lrp_scaled <- private$.scaleLikelihoodRatioPositive(lrp_value, thresholds)
                    
                    test_names[row_idx] <- test_name
                    metric_names[row_idx] <- "LR+ Quality"
                    values[row_idx] <- lrp_value
                    scaled_values[row_idx] <- lrp_scaled
                    row_idx <- row_idx + 1
                    
                    # Process LR- with clinical thresholds  
                    lrn_value <- test_data$LRN
                    lrn_scaled <- private$.scaleLikelihoodRatioNegative(lrn_value, thresholds)
                    
                    test_names[row_idx] <- test_name
                    metric_names[row_idx] <- "LR- Quality"
                    values[row_idx] <- lrn_value
                    scaled_values[row_idx] <- lrn_scaled
                    row_idx <- row_idx + 1
                }
                
                df <- data.frame(
                    test = test_names,
                    metric = factor(metric_names, 
                                   levels = c("Sensitivity", "Specificity", "Accuracy", 
                                             "PPV", "NPV", "LR+ Quality", "LR- Quality")),
                    value = values,
                    scaled_value = scaled_values,
                    stringsAsFactors = FALSE
                )
                
                return(df)
            },
            
            # Clinical scaling for positive likelihood ratios
            .scaleLikelihoodRatioPositive = function(lrp, thresholds) {
                if (is.na(lrp) || lrp <= 1) return(0)
                
                # Clinical scaling based on evidence strength
                if (lrp >= thresholds$excellent_pos) {
                    return(100)  # Excellent evidence
                } else if (lrp >= thresholds$good_pos) {
                    return(75 + (lrp - thresholds$good_pos) / (thresholds$excellent_pos - thresholds$good_pos) * 25)
                } else if (lrp >= thresholds$fair_pos) {
                    return(50 + (lrp - thresholds$fair_pos) / (thresholds$good_pos - thresholds$fair_pos) * 25)
                } else {
                    return(25 + (lrp - 1) / (thresholds$fair_pos - 1) * 25)
                }
            },
            
            # Clinical scaling for negative likelihood ratios
            .scaleLikelihoodRatioNegative = function(lrn, thresholds) {
                if (is.na(lrn) || lrn >= 1) return(0)
                
                # Clinical scaling - lower LR- values are better
                if (lrn <= thresholds$excellent_neg) {
                    return(100)  # Excellent evidence
                } else if (lrn <= thresholds$good_neg) {
                    return(75 + (thresholds$good_neg - lrn) / (thresholds$good_neg - thresholds$excellent_neg) * 25)
                } else if (lrn <= thresholds$fair_neg) {
                    return(50 + (thresholds$fair_neg - lrn) / (thresholds$fair_neg - thresholds$good_neg) * 25)
                } else {
                    return(max(0, 50 * (1 - lrn) / (1 - thresholds$fair_neg)))
                }
            },
            
            # Safe statistical calculation wrapper with enhanced error reporting
            .safeStatisticalCalculation = function(func, context_info, default_return = NULL, ...) {
                tryCatch(
                    func(...),
                    error = function(e) {
                        enhanced_msg <- jmvcore::.("Statistical calculation failed for {context}: {error}. Check data quality, sample sizes, and ensure no extreme values or division by zero.", 
                                                  context = context_info, error = conditionMessage(e))
                        warning(enhanced_msg)
                        return(default_return)
                    }
                )
            },
            
            # Generate comprehensive clinical report with copy-ready sentences
            .generateClinicalReport = function(test_results, processed_data) {
                if (length(test_results) == 0) return()
                
                # Find best performing test
                best_test <- private$.findBestTest(test_results)
                best_metrics <- test_results[[best_test]]$metrics
                
                # Generate report sections
                methods_section <- private$.generateMethodsSection(test_results, processed_data)
                results_section <- private$.generateResultsSection(test_results, best_test, best_metrics)
                clinical_recommendations <- private$.generateClinicalRecommendations(test_results, best_test)
                
                # Create comprehensive HTML report
                report_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px;">',
                    
                    '<h2 style="color: #2c3e50; border-bottom: 2px solid #3498db;">📋 Clinical Summary</h2>',
                    results_section,
                    
                    '<h3 style="color: #27ae60; margin-top: 30px;">📝 Copy-Ready Report Sentences</h3>',
                    '<div style="background-color: #f8f9fa; padding: 15px; border-left: 4px solid #28a745; margin: 15px 0;">',
                    '<h4 style="margin-top: 0;">Methods Section:</h4>',
                    '<p style="font-style: italic; line-height: 1.6;">', methods_section, '</p>',
                    '<button onclick="navigator.clipboard.writeText(this.parentNode.querySelector(\'p\').innerText)" ',
                    'style="background: #28a745; color: white; border: none; padding: 5px 10px; border-radius: 3px; cursor: pointer;">',
                    '📋 Copy Methods Text</button>',
                    '</div>',
                    
                    '<div style="background-color: #e8f4f8; padding: 15px; border-left: 4px solid #3498db; margin: 15px 0;">',
                    '<h4 style="margin-top: 0;">Results Section:</h4>',
                    '<p style="font-style: italic; line-height: 1.6;">', results_section, '</p>',
                    '<button onclick="navigator.clipboard.writeText(this.parentNode.querySelector(\'p\').innerText)" ',
                    'style="background: #3498db; color: white; border: none; padding: 5px 10px; border-radius: 3px; cursor: pointer;">',
                    '📋 Copy Results Text</button>',
                    '</div>',
                    
                    '<h3 style="color: #8e44ad; margin-top: 30px;">💡 Clinical Recommendations</h3>',
                    clinical_recommendations,
                    
                    '</div>'
                )
                
                self$results$clinicalReport$setContent(report_html)
            },
            
            # Find the best performing test based on overall metrics
            .findBestTest = function(test_results) {
                best_score <- -1
                best_test <- names(test_results)[1]
                
                for (test_name in names(test_results)) {
                    metrics <- test_results[[test_name]]$metrics
                    # Balanced scoring: Youden index + accuracy
                    score <- (metrics$Sens + metrics$Spec - 1) + metrics$AccurT
                    if (score > best_score) {
                        best_score <- score
                        best_test <- test_name
                    }
                }
                return(best_test)
            },
            
            # Generate methods section for manuscripts
            .generateMethodsSection = function(test_results, processed_data) {
                n_tests <- length(test_results)
                n_cases <- nrow(processed_data)
                test_names <- names(test_results)
                
                methods <- sprintf(
                    "We compared the diagnostic performance of %s tests (%s) against the gold standard reference using diagnostic accuracy analysis. The study included %d cases with complete data. Performance metrics calculated included sensitivity, specificity, positive and negative predictive values, likelihood ratios, and overall accuracy. %s",
                    n_tests,
                    paste(test_names, collapse = ", "),
                    n_cases,
                    if (n_tests >= 2 && self$options$statComp) "Statistical comparisons between tests were performed using McNemar's test for paired proportions." else ""
                )
                
                return(methods)
            },
            
            # Generate results section with key findings
            .generateResultsSection = function(test_results, best_test, best_metrics) {
                sens_pct <- round(best_metrics$Sens * 100, 1)
                spec_pct <- round(best_metrics$Spec * 100, 1)
                acc_pct <- round(best_metrics$AccurT * 100, 1)
                ppv_pct <- round(best_metrics$PPV * 100, 1)
                npv_pct <- round(best_metrics$NPV * 100, 1)
                
                # Determine statistical significance if McNemar was performed
                significance_note <- if (self$options$statComp && length(test_results) >= 2) {
                    " Statistical comparisons using McNemar's test revealed significant differences in test performance (detailed results in comparison tables)."
                } else {
                    ""
                }
                
                results <- sprintf(
                    "Among the tests evaluated, %s demonstrated optimal diagnostic performance with %s%% sensitivity (95%% CI: [see confidence interval table]), %s%% specificity (95%% CI: [see confidence interval table]), %s%% positive predictive value, %s%% negative predictive value, and %s%% overall accuracy.%s The likelihood ratio for positive results was %.2f and for negative results was %.2f.",
                    best_test,
                    sens_pct,
                    spec_pct, 
                    ppv_pct,
                    npv_pct,
                    acc_pct,
                    significance_note,
                    best_metrics$LRP,
                    best_metrics$LRN
                )
                
                return(results)
            },
            
            # Generate clinical recommendations
            .generateClinicalRecommendations = function(test_results, best_test) {
                best_metrics <- test_results[[best_test]]$metrics
                sens_pct <- best_metrics$Sens * 100
                spec_pct <- best_metrics$Spec * 100
                
                recommendations <- '<div style="background-color: #fff3cd; padding: 15px; border-radius: 8px;">'
                
                if (sens_pct >= 95 && spec_pct >= 95) {
                    recommendations <- paste0(recommendations, 
                        '<p><strong>Clinical Use:</strong> ', best_test, ' shows excellent performance for both screening and confirmatory testing.</p>')
                } else if (sens_pct >= 95) {
                    recommendations <- paste0(recommendations, 
                        '<p><strong>Screening Application:</strong> ', best_test, ' is excellent for initial screening due to high sensitivity (low false negative rate).</p>')
                } else if (spec_pct >= 95) {
                    recommendations <- paste0(recommendations, 
                        '<p><strong>Confirmatory Application:</strong> ', best_test, ' is excellent for confirming diagnosis due to high specificity (low false positive rate).</p>')
                } else {
                    recommendations <- paste0(recommendations, 
                        '<p><strong>Clinical Consideration:</strong> Consider using ', best_test, ' in combination with other tests for optimal diagnostic accuracy.</p>')
                }
                
                recommendations <- paste0(recommendations, 
                    '<p><strong>Implementation Note:</strong> Results should be interpreted in the context of disease prevalence in your clinical population. ',
                    'Consider local validation studies before implementation.</p></div>')
                
                return(recommendations)
            },
            
            # Generate comprehensive about this analysis panel
            .generateAboutPanel = function() {
                about_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px;">',
                    
                    '<h2 style="color: #2c3e50; text-align: center; border-bottom: 2px solid #3498db; padding-bottom: 10px;">',
                    '🔬 About Medical Decision Test Comparison</h2>',
                    
                    # What This Analysis Does
                    '<div style="background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 20px; border-radius: 10px; margin: 20px 0;">',
                    '<h3 style="color: #1565c0; margin-top: 0;">📊 What This Analysis Does</h3>',
                    '<p style="line-height: 1.6; color: #333;">',
                    'This tool compares the diagnostic performance of multiple medical tests against a gold standard reference. ',
                    'It systematically evaluates sensitivity, specificity, predictive values, likelihood ratios, and overall accuracy ',
                    'to help you determine which test performs best for your clinical scenario.',
                    '</p>',
                    '</div>',
                    
                    # When to Use
                    '<div style="background-color: #f1f8e9; border: 1px solid #8bc34a; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #4a7c59; margin-top: 0;">🎯 When to Use This Analysis</h3>',
                    '<ul style="line-height: 1.8; color: #4a7c59;">',
                    '<li><strong>Test Validation:</strong> Comparing new diagnostic methods against established standards</li>',
                    '<li><strong>Method Comparison:</strong> Evaluating which of several tests performs better</li>',
                    '<li><strong>Clinical Research:</strong> Validating biomarkers, imaging techniques, or clinical assessments</li>',
                    '<li><strong>Quality Assessment:</strong> Measuring agreement between different raters or methods</li>',
                    '<li><strong>Protocol Development:</strong> Optimizing diagnostic workflows</li>',
                    '</ul>',
                    '</div>',
                    
                    # How to Use
                    '<div style="background-color: #fff3e0; border: 1px solid #ff9800; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #e65100; margin-top: 0;">📝 How to Use This Analysis</h3>',
                    '<ol style="line-height: 1.8; color: #e65100;">',
                    '<li><strong>Select Gold Standard:</strong> Choose your most reliable reference test (e.g., biopsy, expert consensus)</li>',
                    '<li><strong>Choose Tests to Compare:</strong> Select 2-3 diagnostic tests you want to evaluate</li>',
                    '<li><strong>Define Positive Levels:</strong> Specify what constitutes a "positive" result for each test</li>',
                    '<li><strong>Configure Options:</strong> Enable statistical comparisons, confidence intervals, or visualizations as needed</li>',
                    '<li><strong>Run Analysis:</strong> Review results tables and clinical interpretations</li>',
                    '<li><strong>Copy Report:</strong> Use the auto-generated sentences for your documentation</li>',
                    '</ol>',
                    '</div>',
                    
                    # Key Metrics Explained
                    '<div style="background-color: #f3e5f5; border: 1px solid #9c27b0; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #6a1b9a; margin-top: 0;">📈 Key Metrics Explained</h3>',
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; color: #6a1b9a;">',
                    '<div>',
                    '<p><strong>Sensitivity:</strong> Probability test is positive when disease present (rule-out ability)</p>',
                    '<p><strong>Specificity:</strong> Probability test is negative when disease absent (rule-in ability)</p>',
                    '<p><strong>PPV:</strong> Probability of disease when test positive</p>',
                    '<p><strong>NPV:</strong> Probability of no disease when test negative</p>',
                    '</div>',
                    '<div>',
                    '<p><strong>LR+:</strong> How much positive test increases odds of disease</p>',
                    '<p><strong>LR-:</strong> How much negative test decreases odds of disease</p>',
                    '<p><strong>Accuracy:</strong> Overall probability of correct classification</p>',
                    '<p><strong>McNemar Test:</strong> Statistical comparison between paired tests</p>',
                    '</div>',
                    '</div>',
                    '</div>',
                    
                    # Clinical Guidelines
                    '<div style="background-color: #e8f5e8; border: 1px solid #4caf50; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #2e7d32; margin-top: 0;">⚕️ Clinical Interpretation Guidelines</h3>',
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; color: #2e7d32;">',
                    '<div>',
                    '<h4 style="margin-bottom: 5px;">Screening Tests (Rule-Out):</h4>',
                    '<p style="margin-top: 0;">• Sensitivity ≥95%: Excellent<br>• NPV ≥95%: High confidence<br>• Goal: Minimize false negatives</p>',
                    '</div>',
                    '<div>',
                    '<h4 style="margin-bottom: 5px;">Confirmatory Tests (Rule-In):</h4>',
                    '<p style="margin-top: 0;">• Specificity ≥95%: Excellent<br>• PPV ≥90%: High confidence<br>• Goal: Minimize false positives</p>',
                    '</div>',
                    '</div>',
                    '</div>',
                    
                    # Assumptions and Limitations
                    '<div style="background-color: #fff8e1; border: 1px solid #ffc107; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #f57f17; margin-top: 0;">⚠️ Important Assumptions & Limitations</h3>',
                    '<ul style="line-height: 1.6; color: #f57f17;">',
                    '<li><strong>Gold Standard:</strong> Assumes your reference test is truly accurate</li>',
                    '<li><strong>Sample Size:</strong> Results more reliable with larger, representative samples</li>',
                    '<li><strong>Prevalence Dependency:</strong> PPV and NPV vary with disease prevalence</li>',
                    '<li><strong>McNemar Test:</strong> Requires paired/matched data for statistical comparisons</li>',
                    '<li><strong>Missing Data:</strong> Cases with incomplete data are excluded from analysis</li>',
                    '<li><strong>Confidence Intervals:</strong> Calculated using Wilson method for better accuracy</li>',
                    '</ul>',
                    '</div>',
                    
                    '</div>'
                )
                
                self$results$aboutAnalysis$setContent(about_html)
            },

            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state
                
                # Optimized data frame building - pre-allocate and batch create
                df <- private$.buildBarPlotData(plotData)

                # Create plot
                plot <- ggplot2::ggplot(df, ggplot2::aes(x = metric, y = value, fill = test)) +
                    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
                    ggplot2::coord_flip() +
                    ggplot2::scale_y_continuous(labels = scales::percent) +
                    ggplot2::labs(
                        title = jmvcore::.("Comparison of Tests"),
                        x = "",
                        y = jmvcore::.("Value"),
                        fill = jmvcore::.("Test")
                    ) +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom")

                print(plot)
                TRUE
            },

            .plotRadar = function(imageRadar, ggtheme, ...) {
                plotData <- imageRadar$state
                
                # Optimized radar plot data building with clinical thresholds
                df <- private$.buildRadarPlotData(plotData)

                # Ensure factor ordering for consistent radar plot
                df$metric <- factor(df$metric, 
                                  levels = c("Sensitivity", "Specificity", "Accuracy", 
                                           "PPV", "NPV", "LR+ Quality", "LR- Quality"))

                # Create radar plot using ggplot2
                plot <- ggplot2::ggplot(df, ggplot2::aes(x = metric, y = scaled_value, 
                                                        group = test, color = test)) +
                    ggplot2::geom_line(size = 1.2) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::coord_polar() +
                    ggplot2::scale_y_continuous(limits = c(0, 100), 
                                               breaks = c(0, 25, 50, 75, 100),
                                               labels = c("0%", "25%", "50%", "75%", "100%")) +
                    ggplot2::labs(
                        title = jmvcore::.("Radar Plot: Decision Test Statistics Comparison"),
                        subtitle = jmvcore::.("All metrics scaled 0-100% (LR Quality: clinical performance scale)"),
                        x = "",
                        y = "",
                        color = jmvcore::.("Test")
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(size = 10),
                        axis.text.y = ggplot2::element_text(size = 8),
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9)
                    )

                print(plot)
                TRUE
            }
        )
    )
