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
                    
                }, error = function(e) {
                    stop(paste("Analysis failed:", conditionMessage(e), "Please check your data and variable selections."))
                })
            },

            
            # Comprehensive input validation with informative error messages
            .validateInputs = function() {
                # Check for required gold standard
                if (length(self$options$gold) == 0) {
                    stop("Gold standard variable is required. Please select a gold standard variable from your dataset.")
                }
                
                # Check for valid data
                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop("No data provided for analysis. Please ensure your dataset contains data.")
                }
                
                # Enhanced test validation with specific missing test identification
                private$.validateTestSelection()
                
                # Validate prevalence setting
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    stop(paste("Prior probability must be between 0 and 1. Current value:", self$options$pprob))
                }
                
                # Check for conflicting options
                if (self$options$pp && self$options$ci) {
                    stop("Prior probability and confidence intervals cannot both be enabled. Please choose one option.")
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
                        stop("No test variables selected. Please select at least Test 1 and Test 2 for comparison analysis.")
                    } else if (length(valid_tests) == 1) {
                        available <- paste(valid_tests, collapse = ", ")
                        needed <- paste(missing_tests[1:2], collapse = " or ")
                        stop(sprintf("Only %s is selected. Please also select %s to perform comparison analysis.", available, needed))
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
                    stop(sprintf("Positive level not specified for: %s. Please select the positive result level for each test variable.", 
                                paste(missing_positives, collapse = ", ")))
                }
            },
            
            # Data preparation and cleaning
            .prepareData = function() {
                # Clean data and handle missing values
                mydata <- jmvcore::naOmit(self$data)
                
                if (nrow(mydata) < nrow(self$data)) {
                    warning(sprintf("Removed %d rows with missing values", 
                                   nrow(self$data) - nrow(mydata)))
                }
                
                if (nrow(mydata) == 0) {
                    stop("Data contains no complete rows after removing missing values")
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
                            newtest = "Test Positive",
                            GP = TP, GN = FP, Total = TP + FP
                        )
                    )
                    
                    cTable$setRow(
                        rowKey = "Test Negative",
                        values = list(
                            newtest = "Test Negative",
                            GP = FN, GN = TN, Total = FN + TN
                        )
                    )
                    
                    cTable$setRow(
                        rowKey = "Total",
                        values = list(
                            newtest = "Total",
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
                cTable$addFootnote(rowKey = "Test Positive", col = "GP", "True Positive (TP)")
                cTable$addFootnote(rowKey = "Test Positive", col = "GN", "False Positive (FP)")
                cTable$addFootnote(rowKey = "Test Negative", col = "GP", "False Negative (FN)")
                cTable$addFootnote(rowKey = "Test Negative", col = "GN", "True Negative (TN)")
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
                    
                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addComparisonTableFootnotes(comparisonTable, test_name)
                    }
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
                            "Apparent prevalence", "True prevalence", "Test sensitivity",
                            "Test specificity", "Diagnostic accuracy", "Diagnostic odds ratio",
                            "Number needed to diagnose", "Youden's index",
                            "Positive predictive value", "Negative predictive value",
                            "Likelihood ratio of a positive test", "Likelihood ratio of a negative test",
                            "Proportion of subjects with the outcome ruled out",
                            "Proportion of subjects with the outcome ruled in",
                            "Proportion of false positives", "Proportion of false negative",
                            "False Discovery Rate", "False Omission Rate"
                        )
                        
                        ratiorows <- c("ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                                      "p.tpdn", "p.tndp", "p.dntp", "p.dptn")
                        epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]
                        
                        for (i in seq_along(epirresult_ratio[, 1, drop = TRUE])) {
                            epirTable$addRow(rowKey = i, values = c(epirresult_ratio[i, ]))
                        }
                    }, error = function(e) {
                        n_sample <- sum(conf_table)
                        enhanced_msg <- sprintf("Could not calculate confidence intervals for %s (n=%d). This may be due to insufficient sample size or extreme values. Error: %s", 
                                               testVariable, n_sample, conditionMessage(e))
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
                    enhanced_msg <- sprintf("Could not perform McNemar's test for %s (n1=%d, n2=%d). This may be due to insufficient discordant pairs or identical test results. Error: %s", 
                                           comparison_name, n1, n2, conditionMessage(e))
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
                        enhanced_msg <- sprintf("Could not calculate %s difference for %s (n1=%d, n2=%d). Check for extreme values or zero denominators. Error: %s", 
                                               metric, comparison_name, n1, n2, conditionMessage(e))
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
                        enhanced_msg <- sprintf("Statistical calculation failed for %s: %s. Check data quality, sample sizes, and ensure no extreme values or division by zero.", 
                                               context_info, conditionMessage(e))
                        warning(enhanced_msg)
                        return(default_return)
                    }
                )
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
                        title = "Comparison of Tests",
                        x = "",
                        y = "Value",
                        fill = "Test"
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
                        title = "Radar Plot: Decision Test Statistics Comparison",
                        subtitle = "All metrics scaled 0-100% (LR Quality: clinical performance scale)",
                        x = "",
                        y = "",
                        color = "Test"
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
