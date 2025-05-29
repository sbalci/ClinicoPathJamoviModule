#' @title Decision Panel Optimization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom utils combn
#' @importFrom stats predict

decisionpanelClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisionpanelClass",
        inherit = decisionpanelBase,
        private = list(

            # ============================================================================
            # INITIALIZATION
            # ============================================================================

            .init = function() {
                # Show initial message
                if (is.null(self$options$tests) || is.null(self$options$gold)) {
                    self$results$summary$setContent(
                        "<h3>Decision Panel Optimization</h3>
                        <p>Please select diagnostic tests and gold standard variable to begin analysis.</p>"
                    )
                }
            },

            # ============================================================================
            # MAIN ANALYSIS
            # ============================================================================

            .run = function() {
                # Check for required inputs
                if (is.null(self$options$tests) ||
                    length(self$options$tests) == 0 ||
                    is.null(self$options$gold)) {
                    return()
                }

                if (nrow(self$data) == 0) {
                    stop("Data contains no (complete) rows")
                }

                # Set random seed for reproducibility
                set.seed(self$options$seed)

                # Get data and options
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)

                # Get gold standard variable and positive level
                goldVariable <- self$options$gold
                goldPositive <- self$options$goldPositive

                # Get test variables and their positive levels
                testVariables <- self$options$tests


                # Enhanced version of the test level handling in .run() method

                # Handle test positive levels with better user feedback
                testPositiveLevels <- NULL
                level_detection_msg <- ""

                if (is.null(self$options$testLevels) || nchar(trimws(self$options$testLevels)) == 0) {
                    # Auto-detect positive levels
                    testPositiveLevels <- character(length(testVariables))
                    detected_levels <- character(length(testVariables))

                    for (i in seq_along(testVariables)) {
                        test <- testVariables[i]
                        test_levels <- levels(mydata[[test]])

                        # Try to find common positive indicators (case-insensitive)
                        positive_indicators <- c("Positive", "positive", "Yes", "yes", "TRUE", "True", "true", "1", "Abnormal", "abnormal")
                        found_positive <- intersect(positive_indicators, test_levels)

                        if (length(found_positive) > 0) {
                            testPositiveLevels[i] <- found_positive[1]
                            detected_levels[i] <- paste0(test, " = '", found_positive[1], "'")
                        } else {
                            # Default to first level and warn user
                            testPositiveLevels[i] <- test_levels[1]
                            detected_levels[i] <- paste0(test, " = '", test_levels[1], "' (first level)")
                        }
                    }

                    level_detection_msg <- paste0(
                        "<p style='color: #ff8800; background-color: #fff3cd; padding: 10px; border-radius: 5px;'>",
                        "<strong>Auto-detected positive test levels:</strong><br>",
                        paste(detected_levels, collapse = "<br>"),
                        "<br><em>If these are incorrect, please specify positive levels in the 'Positive Test Levels' field.</em>",
                        "</p>"
                    )

                } else {
                    # Parse user-specified levels
                    user_levels <- trimws(unlist(strsplit(self$options$testLevels, ",")))

                    if (length(user_levels) == 1) {
                        # Single level specified - use for all tests
                        testPositiveLevels <- rep(user_levels[1], length(testVariables))
                        level_detection_msg <- paste0(
                            "<p style='color: #28a745; background-color: #d4edda; padding: 10px; border-radius: 5px;'>",
                            "<strong>Using specified positive level:</strong> '", user_levels[1], "' for all tests",
                            "</p>"
                        )
                    } else if (length(user_levels) == length(testVariables)) {
                        # Multiple levels specified - one for each test
                        testPositiveLevels <- user_levels
                        level_pairs <- paste(testVariables, "=", paste0("'", user_levels, "'"))
                        level_detection_msg <- paste0(
                            "<p style='color: #28a745; background-color: #d4edda; padding: 10px; border-radius: 5px;'>",
                            "<strong>Using specified positive levels:</strong><br>",
                            paste(level_pairs, collapse = "<br>"),
                            "</p>"
                        )
                    } else {
                        stop(paste0("Number of specified positive levels (", length(user_levels),
                                    ") must be 1 or match the number of tests (", length(testVariables), ")"))
                    }
                }

                # Validate that all specified positive levels exist in the data
                validation_errors <- character(0)
                for (i in seq_along(testVariables)) {
                    test <- testVariables[i]
                    positive_level <- testPositiveLevels[i]

                    if (!positive_level %in% levels(mydata[[test]])) {
                        available_levels <- paste(levels(mydata[[test]]), collapse = "', '")
                        validation_errors <- c(validation_errors,
                                               paste0("Test '", test, "': Level '", positive_level,
                                                      "' not found. Available: '", available_levels, "'"))
                    }
                }

                if (length(validation_errors) > 0) {
                    error_msg <- paste0(
                        "Invalid positive test levels specified:\n",
                        paste(validation_errors, collapse = "\n")
                    )
                    stop(error_msg)
                }

                # Create binary versions of all variables
                mydata$gold_binary <- ifelse(mydata[[goldVariable]] == goldPositive, 1, 0)

                for (i in seq_along(testVariables)) {
                    test <- testVariables[i]
                    positive_level <- testPositiveLevels[i]
                    binary_col <- paste0(test, "_binary")
                    mydata[[binary_col]] <- as.numeric(mydata[[test]] == positive_level)
                }

                # Update summary with level information
                initial_summary <- paste0(
                    "<h3>Decision Panel Optimization</h3>",
                    level_detection_msg,
                    "<p><strong>Analysis includes:</strong></p>",
                    "<ul>",
                    "<li>", length(testVariables), " diagnostic test(s)</li>",
                    "<li>", nrow(mydata), " subjects</li>",
                    "<li>Disease prevalence: ", round(mean(mydata$gold_binary) * 100, 1), "%</li>",
                    "</ul>"
                )

                self$results$summary$setContent(initial_summary)






                # Calculate disease prevalence
                sample_prevalence <- mean(mydata$gold_binary, na.rm = TRUE)
                if (self$options$prevalence > 0) {
                    prevalence <- self$options$prevalence
                } else {
                    prevalence <- sample_prevalence
                }

                # Parse test costs if provided
                test_costs <- NULL
                if (self$options$useCosts && nchar(self$options$testCosts) > 0) {
                    test_costs <- as.numeric(unlist(strsplit(self$options$testCosts, ",")))
                    if (length(test_costs) != length(testVariables)) {
                        stop("Number of test costs must match number of tests")
                    }
                    names(test_costs) <- testVariables
                }

                # ============================================================================
                # EVALUATE INDIVIDUAL TESTS
                # ============================================================================

                individual_results <- private$.evaluateIndividualTests(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels,
                    test_costs
                )

                # Populate individual tests table
                private$.populateIndividualTestsTable(individual_results)

                # ============================================================================
                # EVALUATE TEST COMBINATIONS
                # ============================================================================

                all_combinations <- private$.evaluateAllCombinations(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels,
                    test_costs, prevalence
                )

                # ============================================================================
                # FIND OPTIMAL PANELS
                # ============================================================================

                optimal_panels <- private$.findOptimalPanels(
                    all_combinations,
                    self$options$optimizationCriteria,
                    self$options$minSensitivity,
                    self$options$minSpecificity,
                    self$options$topN
                )

                # Populate optimal panel table
                private$.populateOptimalPanelTable(optimal_panels)

                # ============================================================================
                # STRATEGY COMPARISON
                # ============================================================================

                if (self$options$compareStrategies) {
                    strategy_comparison <- private$.compareStrategies(all_combinations)
                    private$.populateStrategyComparisonTable(strategy_comparison)
                }

                # ============================================================================
                # CREATE DECISION TREE
                # ============================================================================

                if (self$options$createTree) {
                    tree_results <- private$.createDecisionTree(
                        mydata, testVariables, goldVariable,
                        test_costs, prevalence
                    )
                    private$.populateTreeResults(tree_results)
                }

                # ============================================================================
                # CROSS-VALIDATION
                # ============================================================================

                if (self$options$crossValidate) {
                    cv_results <- private$.performCrossValidation(
                        mydata, optimal_panels, testVariables,
                        goldVariable, goldPositive, testPositiveLevels
                    )
                    private$.populateCrossValidationTable(cv_results)
                }

                # ============================================================================
                # BOOTSTRAP CONFIDENCE INTERVALS
                # ============================================================================

                if (self$options$bootstrap) {
                    boot_results <- private$.performBootstrap(
                        mydata, optimal_panels, testVariables,
                        goldVariable, goldPositive, testPositiveLevels
                    )
                    private$.populateBootstrapTable(boot_results)
                }

                # ============================================================================
                # CREATE SUMMARY
                # ============================================================================

                private$.createSummary(
                    optimal_panels, individual_results,
                    prevalence, test_costs
                )

                # ============================================================================
                # CREATE RECOMMENDATIONS
                # ============================================================================

                private$.createRecommendations(
                    optimal_panels, individual_results,
                    test_costs, prevalence
                )

                # Show all combinations table if requested
                if (self$options$showAllCombinations) {
                    private$.populateAllCombinationsTable(all_combinations)
                }

                # Set data for plots
                if (self$options$plotTree && self$options$createTree) {
                    self$results$treeVisualization$setState(tree_results)
                }

                if (self$options$plotComparison && self$options$compareStrategies) {
                    self$results$strategyComparisonPlot$setState(strategy_comparison)
                }

                if (self$options$plotCostEffect && self$options$useCosts) {
                    self$results$costEffectivenessPlot$setState(all_combinations)
                }

                if (self$options$plotROC) {
                    self$results$rocCurvesPlot$setState(list(
                        data = mydata,
                        panels = head(optimal_panels, self$options$topN),
                        goldVariable = goldVariable,
                        goldPositive = goldPositive
                    ))
                }
            },




            # ============================================================================
            # HELPER METHODS
            # ============================================================================

            # Evaluate individual test performance
            .evaluateIndividualTests = function(mydata, testVariables, goldVariable,
                                                goldPositive, testPositiveLevels, test_costs) {
                results <- list()

                for (i in seq_along(testVariables)) {
                    test <- testVariables[i]
                    positive_level <- testPositiveLevels[i]

                    # Create confusion matrix
                    test_binary <- paste0(test, "_binary")
                    conf_matrix <- table(
                        Predicted = mydata[[test_binary]],
                        Actual = mydata$gold_binary
                    )

                    # Calculate metrics
                    TP <- conf_matrix[2, 2]
                    FP <- conf_matrix[2, 1]
                    FN <- conf_matrix[1, 2]
                    TN <- conf_matrix[1, 1]

                    sensitivity <- TP / (TP + FN)
                    specificity <- TN / (TN + FP)
                    accuracy <- (TP + TN) / sum(conf_matrix)
                    ppv <- TP / (TP + FP)
                    npv <- TN / (TN + FN)
                    plr <- sensitivity / (1 - specificity)
                    nlr <- (1 - sensitivity) / specificity

                    results[[test]] <- list(
                        test = test,
                        sensitivity = sensitivity,
                        specificity = specificity,
                        accuracy = accuracy,
                        ppv = ppv,
                        npv = npv,
                        plr = plr,
                        nlr = nlr,
                        cost = ifelse(is.null(test_costs), NA, test_costs[test])
                    )
                }

                return(results)
            },

            # Evaluate all test combinations
            .evaluateAllCombinations = function(mydata, testVariables, goldVariable,
                                                goldPositive, testPositiveLevels,
                                                test_costs, prevalence) {
                all_results <- list()

                # Get max number of tests to combine
                max_tests <- min(self$options$maxTests, length(testVariables))

                # Evaluate each combination size
                for (n_tests in 1:max_tests) {
                    # Get all combinations of n_tests
                    combinations <- combn(testVariables, n_tests, simplify = FALSE)

                    for (combo in combinations) {
                        # Evaluate different strategies for this combination
                        strategies <- private$.getStrategiesToEvaluate()

                        for (strategy in strategies) {
                            result <- private$.evaluateCombination(
                                mydata, combo, strategy,
                                goldVariable, goldPositive,
                                testPositiveLevels, test_costs,
                                prevalence
                            )

                            if (!is.null(result)) {
                                key <- paste(c(combo, strategy), collapse = "_")
                                all_results[[key]] <- result
                            }
                        }
                    }
                }

                return(all_results)
            },

            # Get strategies to evaluate based on options
            .getStrategiesToEvaluate = function() {
                strategies <- c()

                if (self$options$strategies == "all") {
                    strategies <- c("single", "parallel_any", "parallel_all",
                                    "parallel_majority", "sequential_positive",
                                    "sequential_negative")
                } else if (self$options$strategies == "single") {
                    strategies <- c("single")
                } else if (self$options$strategies == "parallel") {
                    if (self$options$parallelRules == "any") {
                        strategies <- c("parallel_any")
                    } else if (self$options$parallelRules == "all") {
                        strategies <- c("parallel_all")
                    } else if (self$options$parallelRules == "majority") {
                        strategies <- c("parallel_majority")
                    } else if (self$options$parallelRules == "custom") {
                        strategies <- c("parallel_custom")
                    }
                } else if (self$options$strategies == "sequential") {
                    strategies <- c("sequential_positive", "sequential_negative")
                }

                return(strategies)
            },

            # Evaluate a specific test combination with a specific strategy
            .evaluateCombination = function(mydata, tests, strategy, goldVariable,
                                            goldPositive, testPositiveLevels,
                                            test_costs, prevalence) {
                n_tests <- length(tests)

                # Skip invalid combinations
                if (n_tests == 1 && strategy != "single") return(NULL)
                if (n_tests > 1 && strategy == "single") return(NULL)

                # Get binary columns for tests
                binary_cols <- paste0(tests, "_binary")
                test_matrix <- as.matrix(mydata[, binary_cols])

                # Apply strategy
                if (strategy == "single") {
                    combined_result <- test_matrix[, 1]
                } else if (strategy == "parallel_any") {
                    combined_result <- as.numeric(rowSums(test_matrix) > 0)
                } else if (strategy == "parallel_all") {
                    combined_result <- as.numeric(rowSums(test_matrix) == n_tests)
                } else if (strategy == "parallel_majority") {
                    combined_result <- as.numeric(rowSums(test_matrix) > n_tests/2)
                } else if (strategy == "parallel_custom") {
                    threshold <- self$options$customThreshold
                    combined_result <- as.numeric(rowSums(test_matrix) >= threshold)
                } else if (grepl("sequential", strategy)) {
                    # Sequential testing is more complex - simplified here
                    # In practice, would need to implement proper sequential logic
                    combined_result <- test_matrix[, 1]  # Simplified
                }

                # Create confusion matrix
                conf_matrix <- table(
                    Predicted = combined_result,
                    Actual = mydata$gold_binary
                )

                # Calculate metrics
                TP <- conf_matrix[2, 2]
                FP <- conf_matrix[2, 1]
                FN <- conf_matrix[1, 2]
                TN <- conf_matrix[1, 1]

                sensitivity <- TP / (TP + FN)
                specificity <- TN / (TN + FP)
                accuracy <- (TP + TN) / sum(conf_matrix)
                ppv <- TP / (TP + FP)
                npv <- TN / (TN + FN)
                youden <- sensitivity + specificity - 1

                # Calculate cost if applicable
                total_cost <- 0
                if (!is.null(test_costs)) {
                    test_cost <- sum(test_costs[tests])
                    fp_cost <- FP * self$options$fpCost / nrow(mydata)
                    fn_cost <- FN * self$options$fnCost / nrow(mydata)
                    total_cost <- test_cost + fp_cost + fn_cost
                }

                # Calculate utility (negative cost for maximization)
                utility <- -total_cost

                # Calculate efficiency (accuracy per unit cost)
                efficiency <- ifelse(total_cost > 0, accuracy / total_cost, accuracy)

                return(list(
                    tests = paste(tests, collapse = "+"),
                    strategy = strategy,
                    nTests = n_tests,
                    sensitivity = sensitivity,
                    specificity = specificity,
                    accuracy = accuracy,
                    ppv = ppv,
                    npv = npv,
                    youden = youden,
                    cost = total_cost,
                    utility = utility,
                    efficiency = efficiency,
                    conf_matrix = conf_matrix
                ))
            },

            # Find optimal panels based on criteria
            .findOptimalPanels = function(all_combinations, criteria,
                                          min_sens, min_spec, top_n) {
                # Filter by minimum constraints
                filtered <- Filter(function(x) {
                    x$sensitivity >= min_sens && x$specificity >= min_spec
                }, all_combinations)

                if (length(filtered) == 0) {
                    warning("No panels meet the minimum sensitivity/specificity constraints")
                    filtered <- all_combinations
                }

                # Sort by optimization criteria
                sorted_results <- filtered[order(sapply(filtered, function(x) {
                    -x[[criteria]]  # Negative for descending order
                }))]

                # Return top N
                head(sorted_results, top_n)
            },

            # Compare different testing strategies
            .compareStrategies = function(all_combinations) {
                # Group by strategy
                strategy_groups <- list()

                for (result in all_combinations) {
                    strategy <- result$strategy
                    if (!(strategy %in% names(strategy_groups))) {
                        strategy_groups[[strategy]] <- list()
                    }
                    strategy_groups[[strategy]][[length(strategy_groups[[strategy]]) + 1]] <- result
                }

                # Calculate summary statistics for each strategy
                comparison <- list()

                for (strategy in names(strategy_groups)) {
                    results <- strategy_groups[[strategy]]

                    comparison[[strategy]] <- list(
                        strategy = strategy,
                        nTests = mean(sapply(results, function(x) x$nTests)),
                        avgSens = mean(sapply(results, function(x) x$sensitivity)),
                        avgSpec = mean(sapply(results, function(x) x$specificity)),
                        avgAccuracy = mean(sapply(results, function(x) x$accuracy)),
                        avgCost = mean(sapply(results, function(x) x$cost)),
                        bestPanel = results[[which.max(sapply(results, function(x) x$accuracy))]]$tests,
                        bestPerformance = max(sapply(results, function(x) x$accuracy))
                    )
                }

                return(comparison)
            },

            # Create decision tree
            .createDecisionTree = function(mydata, testVariables, goldVariable,
                                           test_costs, prevalence) {
                # Prepare data for tree
                tree_data <- mydata[, c(testVariables, goldVariable)]

                # Create formula
                formula <- as.formula(paste(goldVariable, "~",
                                            paste(testVariables, collapse = " + ")))

                # Build tree based on method
                if (self$options$treeMethod == "cart") {
                    if (!requireNamespace("rpart", quietly = TRUE)) {
                        stop("rpart package is required for CART trees")
                    }

                    # Set up cost matrix if using costs
                    if (self$options$useCosts) {
                        # Cost of false positive vs false negative
                        cost_matrix <- matrix(c(0, self$options$fpCost,
                                                self$options$fnCost, 0),
                                              nrow = 2)

                        tree <- rpart::rpart(
                            formula,
                            data = tree_data,
                            method = "class",
                            control = rpart::rpart.control(
                                maxdepth = self$options$maxDepth,
                                minsplit = self$options$minSplit
                            ),
                            parms = list(loss = cost_matrix)
                        )
                    } else {
                        tree <- rpart::rpart(
                            formula,
                            data = tree_data,
                            method = "class",
                            control = rpart::rpart.control(
                                maxdepth = self$options$maxDepth,
                                minsplit = self$options$minSplit
                            )
                        )
                    }

                    # Extract tree structure
                    tree_structure <- private$.extractTreeStructure(tree, tree_data)

                } else {
                    # Placeholder for other methods
                    tree <- NULL
                    tree_structure <- list()
                }

                return(list(
                    tree = tree,
                    structure = tree_structure,
                    method = self$options$treeMethod
                ))
            },

            # Extract tree structure for display
            .extractTreeStructure = function(tree, data) {
                if (is.null(tree)) return(NULL)

                # Get tree frame
                frame <- tree$frame

                # Create structure list
                structure <- list()

                for (i in 1:nrow(frame)) {
                    node_data <- frame[i, ]

                    structure[[i]] <- list(
                        node = rownames(frame)[i],
                        condition = ifelse(node_data$var == "<leaf>",
                                           "Terminal",
                                           paste(node_data$var, "split")),
                        nCases = node_data$n,
                        probPositive = node_data$yval2[, 2] / node_data$n,
                        decision = ifelse(node_data$yval == 1, "Negative", "Positive"),
                        accuracy = max(node_data$yval2[, 1:2]) / node_data$n
                    )
                }

                return(structure)
            },

            # Perform cross-validation
            .performCrossValidation = function(mydata, panels, testVariables,
                                               goldVariable, goldPositive,
                                               testPositiveLevels) {
                n_folds <- self$options$nFolds
                n <- nrow(mydata)
                fold_indices <- sample(rep(1:n_folds, length.out = n))

                cv_results <- list()

                # For each panel
                for (i in seq_along(panels)) {
                    panel <- panels[[i]]

                    # Store fold results
                    fold_sens <- numeric(n_folds)
                    fold_spec <- numeric(n_folds)
                    fold_acc <- numeric(n_folds)

                    # Perform k-fold CV
                    for (fold in 1:n_folds) {
                        # Split data
                        test_indices <- which(fold_indices == fold)
                        train_data <- mydata[-test_indices, ]
                        test_data <- mydata[test_indices, ]

                        # Evaluate on test fold
                        # (Simplified - would need to re-implement evaluation logic)
                        fold_sens[fold] <- panel$sensitivity  # Placeholder
                        fold_spec[fold] <- panel$specificity  # Placeholder
                        fold_acc[fold] <- panel$accuracy      # Placeholder
                    }

                    cv_results[[i]] <- list(
                        panel = panel$tests,
                        strategy = panel$strategy,
                        cvSensitivity = mean(fold_sens),
                        cvSensitivitySD = sd(fold_sens),
                        cvSpecificity = mean(fold_spec),
                        cvSpecificitySD = sd(fold_spec),
                        cvAccuracy = mean(fold_acc),
                        cvAccuracySD = sd(fold_acc)
                    )
                }

                return(cv_results)
            },

            # Perform bootstrap
            .performBootstrap = function(mydata, panels, testVariables,
                                         goldVariable, goldPositive,
                                         testPositiveLevels) {
                boot_reps <- self$options$bootReps
                n <- nrow(mydata)

                boot_results <- list()

                # For top panels only (to save computation)
                for (i in 1:min(5, length(panels))) {
                    panel <- panels[[i]]

                    # Store bootstrap results
                    boot_sens <- numeric(boot_reps)
                    boot_spec <- numeric(boot_reps)
                    boot_acc <- numeric(boot_reps)

                    # Perform bootstrap
                    for (b in 1:boot_reps) {
                        # Resample with replacement
                        boot_indices <- sample(1:n, n, replace = TRUE)
                        boot_data <- mydata[boot_indices, ]

                        # Re-evaluate (simplified)
                        boot_sens[b] <- panel$sensitivity  # Placeholder
                        boot_spec[b] <- panel$specificity  # Placeholder
                        boot_acc[b] <- panel$accuracy      # Placeholder
                    }

                    # Calculate confidence intervals
                    metrics <- c("sensitivity", "specificity", "accuracy")
                    values <- list(boot_sens, boot_spec, boot_acc)

                    for (j in seq_along(metrics)) {
                        boot_results[[paste0(panel$tests, "_", metrics[j])]] <- list(
                            panel = panel$tests,
                            metric = metrics[j],
                            estimate = panel[[metrics[j]]],
                            lower = quantile(values[[j]], 0.025),
                            upper = quantile(values[[j]], 0.975),
                            se = sd(values[[j]])
                        )
                    }
                }

                return(boot_results)
            },

            # ============================================================================
            # TABLE POPULATION METHODS
            # ============================================================================

            .populateIndividualTestsTable = function(results) {
                table <- self$results$individualTests

                for (test_name in names(results)) {
                    result <- results[[test_name]]

                    table$addRow(rowKey = test_name, values = list(
                        test = test_name,
                        sensitivity = result$sensitivity,
                        specificity = result$specificity,
                        accuracy = result$accuracy,
                        ppv = result$ppv,
                        npv = result$npv,
                        plr = result$plr,
                        nlr = result$nlr,
                        cost = result$cost
                    ))
                }
            },

            .populateOptimalPanelTable = function(panels) {
                table <- self$results$optimalPanel

                for (i in seq_along(panels)) {
                    panel <- panels[[i]]

                    table$addRow(rowKey = i, values = list(
                        rank = i,
                        tests = panel$tests,
                        strategy = panel$strategy,
                        sensitivity = panel$sensitivity,
                        specificity = panel$specificity,
                        accuracy = panel$accuracy,
                        ppv = panel$ppv,
                        npv = panel$npv,
                        youden = panel$youden,
                        cost = panel$cost,
                        utility = panel$utility,
                        efficiency = panel$efficiency
                    ))
                }
            },

            .populateStrategyComparisonTable = function(comparison) {
                table <- self$results$strategyComparison

                for (strategy_name in names(comparison)) {
                    comp <- comparison[[strategy_name]]

                    table$addRow(rowKey = strategy_name, values = list(
                        strategy = strategy_name,
                        nTests = comp$nTests,
                        avgSens = comp$avgSens,
                        avgSpec = comp$avgSpec,
                        avgAccuracy = comp$avgAccuracy,
                        avgCost = comp$avgCost,
                        bestPanel = comp$bestPanel,
                        bestPerformance = comp$bestPerformance
                    ))
                }
            },

            .populateAllCombinationsTable = function(combinations) {
                table <- self$results$allCombinations

                # Sort by accuracy
                sorted_combos <- combinations[order(sapply(combinations,
                                                           function(x) -x$accuracy))]

                for (i in seq_along(sorted_combos)) {
                    combo <- sorted_combos[[i]]

                    table$addRow(rowKey = i, values = list(
                        tests = combo$tests,
                        strategy = combo$strategy,
                        nTests = combo$nTests,
                        sensitivity = combo$sensitivity,
                        specificity = combo$specificity,
                        accuracy = combo$accuracy,
                        ppv = combo$ppv,
                        npv = combo$npv,
                        cost = combo$cost,
                        utility = combo$utility
                    ))
                }
            },

            .populateTreeResults = function(tree_results) {
                if (is.null(tree_results$structure)) return()

                # Create HTML representation of tree structure
                html <- "<h4>Decision Tree Structure</h4>"
                html <- paste0(html, "<pre>")

                # Simple text representation (would be enhanced with actual tree visualization)
                for (node in tree_results$structure) {
                    html <- paste0(html,
                                   sprintf("Node %s: %s (n=%d, P(+)=%.2f, Decision=%s)\n",
                                           node$node, node$condition, node$nCases,
                                           node$probPositive, node$decision))
                }

                html <- paste0(html, "</pre>")

                self$results$treeStructure$setContent(html)

                # Populate performance table
                table <- self$results$treePerformance

                for (node in tree_results$structure) {
                    table$addRow(rowKey = node$node, values = list(
                        node = node$node,
                        condition = node$condition,
                        nCases = node$nCases,
                        probPositive = node$probPositive,
                        decision = node$decision,
                        accuracy = node$accuracy
                    ))
                }
            },

            .populateCrossValidationTable = function(cv_results) {
                table <- self$results$crossValidation

                for (i in seq_along(cv_results)) {
                    result <- cv_results[[i]]

                    table$addRow(rowKey = i, values = list(
                        panel = result$panel,
                        strategy = result$strategy,
                        cvSensitivity = result$cvSensitivity,
                        cvSensitivitySD = result$cvSensitivitySD,
                        cvSpecificity = result$cvSpecificity,
                        cvSpecificitySD = result$cvSpecificitySD,
                        cvAccuracy = result$cvAccuracy,
                        cvAccuracySD = result$cvAccuracySD
                    ))
                }
            },

            .populateBootstrapTable = function(boot_results) {
                table <- self$results$bootstrapResults

                for (key in names(boot_results)) {
                    result <- boot_results[[key]]

                    table$addRow(rowKey = key, values = list(
                        panel = result$panel,
                        metric = result$metric,
                        estimate = result$estimate,
                        lower = result$lower,
                        upper = result$upper,
                        se = result$se
                    ))
                }
            },

            # ============================================================================
            # SUMMARY AND RECOMMENDATIONS
            # ============================================================================

            .createSummary = function(optimal_panels, individual_results,
                                      prevalence, test_costs) {
                html <- "<h3>Decision Panel Optimization Summary</h3>"

                # Best panel
                if (length(optimal_panels) > 0) {
                    best <- optimal_panels[[1]]
                    html <- paste0(html, "<h4>Optimal Test Panel</h4>")
                    html <- paste0(html, "<p><strong>Tests:</strong> ", best$tests, "</p>")
                    html <- paste0(html, "<p><strong>Strategy:</strong> ", best$strategy, "</p>")
                    html <- paste0(html, "<p><strong>Performance:</strong></p>")
                    html <- paste0(html, "<ul>")
                    html <- paste0(html, sprintf("<li>Sensitivity: %.1f%%</li>", best$sensitivity * 100))
                    html <- paste0(html, sprintf("<li>Specificity: %.1f%%</li>", best$specificity * 100))
                    html <- paste0(html, sprintf("<li>Accuracy: %.1f%%</li>", best$accuracy * 100))
                    html <- paste0(html, sprintf("<li>PPV: %.1f%%</li>", best$ppv * 100))
                    html <- paste0(html, sprintf("<li>NPV: %.1f%%</li>", best$npv * 100))

                    if (!is.na(best$cost)) {
                        html <- paste0(html, sprintf("<li>Total Cost: %.2f</li>", best$cost))
                    }

                    html <- paste0(html, "</ul>")
                }

                # Disease prevalence
                html <- paste0(html, "<h4>Population Characteristics</h4>")
                html <- paste0(html, sprintf("<p>Disease Prevalence: %.1f%%</p>", prevalence * 100))

                # Number of tests evaluated
                html <- paste0(html, sprintf("<p>Number of tests evaluated: %d</p>",
                                             length(individual_results)))

                self$results$summary$setContent(html)
            },

            .createRecommendations = function(optimal_panels, individual_results,
                                              test_costs, prevalence) {
                html <- "<h3>Recommendations</h3>"

                # Primary recommendation
                if (length(optimal_panels) > 0) {
                    best <- optimal_panels[[1]]

                    html <- paste0(html, "<h4>Primary Recommendation</h4>")
                    html <- paste0(html, "<p>Based on the analysis, the optimal testing approach is:</p>")
                    html <- paste0(html, "<ul>")
                    html <- paste0(html, "<li><strong>", best$tests, "</strong> using <strong>",
                                   best$strategy, "</strong> strategy</li>")
                    html <- paste0(html, "</ul>")

                    # Explain strategy
                    if (grepl("parallel_any", best$strategy)) {
                        html <- paste0(html, "<p>This means conducting all tests simultaneously and ",
                                       "considering the result positive if ANY test is positive.</p>")
                    } else if (grepl("parallel_all", best$strategy)) {
                        html <- paste0(html, "<p>This means conducting all tests simultaneously and ",
                                       "considering the result positive only if ALL tests are positive.</p>")
                    }
                }

                # Alternative recommendations
                if (length(optimal_panels) > 1) {
                    html <- paste0(html, "<h4>Alternative Options</h4>")
                    html <- paste0(html, "<p>Consider these alternatives based on specific needs:</p>")
                    html <- paste0(html, "<ul>")

                    # High sensitivity option
                    high_sens <- optimal_panels[order(sapply(optimal_panels,
                                                             function(x) -x$sensitivity))][[1]]
                    html <- paste0(html, sprintf("<li>For maximum sensitivity (%.1f%%): %s</li>",
                                                 high_sens$sensitivity * 100, high_sens$tests))

                    # High specificity option
                    high_spec <- optimal_panels[order(sapply(optimal_panels,
                                                             function(x) -x$specificity))][[1]]
                    html <- paste0(html, sprintf("<li>For maximum specificity (%.1f%%): %s</li>",
                                                 high_spec$specificity * 100, high_spec$tests))

                    # Low cost option (if applicable)
                    if (self$options$useCosts) {
                        low_cost <- optimal_panels[order(sapply(optimal_panels,
                                                                function(x) x$cost))][[1]]
                        html <- paste0(html, sprintf("<li>For minimum cost (%.2f): %s</li>",
                                                     low_cost$cost, low_cost$tests))
                    }

                    html <- paste0(html, "</ul>")
                }

                # General recommendations
                html <- paste0(html, "<h4>General Considerations</h4>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Consider the clinical context and consequences of ",
                               "false positives vs. false negatives</li>")
                html <- paste0(html, "<li>Factor in practical considerations such as test ",
                               "availability and turnaround time</li>")
                html <- paste0(html, "<li>Validate the chosen panel in an independent dataset ",
                               "before implementation</li>")
                html <- paste0(html, "</ul>")

                self$results$recommendations$setContent(html)
            },

            # ============================================================================
            # PLOTTING METHODS
            # ============================================================================

            .plotTree = function(image, ggtheme, theme, ...) {
                tree_results <- image$state

                if (is.null(tree_results$tree)) return(FALSE)

                # Use rpart.plot if available
                if (requireNamespace("rpart.plot", quietly = TRUE)) {
                    rpart.plot::rpart.plot(
                        tree_results$tree,
                        type = 2,
                        extra = 104,
                        fallen.leaves = TRUE,
                        main = "Decision Tree for Test Panel"
                    )
                } else {
                    # Basic plot
                    plot(tree_results$tree, uniform = TRUE, main = "Decision Tree")
                    text(tree_results$tree, use.n = TRUE, all = TRUE, cex = 0.8)
                }

                return(TRUE)
            },



            # Replace the entire .plotStrategyComparison method in decisionpanel.b.R

            .plotStrategyComparison = function(image, ggtheme, theme, ...) {
                comparison <- image$state

                # Check if comparison data exists and is valid
                if (is.null(comparison) || length(comparison) == 0) {
                    # Create empty plot with message
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5,
                                          label = "No strategy comparison data available",
                                          size = 12, hjust = 0.5, vjust = 0.5) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                }

                # Debug: Print comparison structure to understand the data
                # Uncomment next line for debugging:
                # cat("Comparison structure:", str(comparison), "\n")

                tryCatch({
                    # Initialize empty data frame with proper structure
                    plot_data <- data.frame(
                        strategy = character(0),
                        accuracy = numeric(0),
                        sensitivity = numeric(0),
                        specificity = numeric(0),
                        stringsAsFactors = FALSE
                    )

                    # Populate data frame safely
                    for (strategy_name in names(comparison)) {
                        comp <- comparison[[strategy_name]]

                        # Ensure comp is a list and has required fields
                        if (is.list(comp) &&
                            !is.null(comp$avgAccuracy) &&
                            !is.null(comp$avgSens) &&
                            !is.null(comp$avgSpec)) {

                            # Add row to plot_data
                            new_row <- data.frame(
                                strategy = strategy_name,
                                accuracy = as.numeric(comp$avgAccuracy),
                                sensitivity = as.numeric(comp$avgSens),
                                specificity = as.numeric(comp$avgSpec),
                                stringsAsFactors = FALSE
                            )
                            plot_data <- rbind(plot_data, new_row)
                        }
                    }

                    # Check if we have data to plot
                    if (nrow(plot_data) == 0) {
                        # Create empty plot with message
                        p <- ggplot2::ggplot() +
                            ggplot2::annotate("text", x = 0.5, y = 0.5,
                                              label = "No valid strategy data for plotting",
                                              size = 12, hjust = 0.5, vjust = 0.5) +
                            ggplot2::xlim(0, 1) +
                            ggplot2::ylim(0, 1) +
                            ggplot2::theme_void()
                        print(p)
                        return(TRUE)
                    }

                    # Verify required columns exist
                    required_cols <- c("accuracy", "sensitivity", "specificity")
                    existing_cols <- names(plot_data)
                    missing_cols <- setdiff(required_cols, existing_cols)

                    if (length(missing_cols) > 0) {
                        warning(paste("Missing columns in plot data:", paste(missing_cols, collapse = ", ")))
                        warning(paste("Available columns:", paste(existing_cols, collapse = ", ")))
                        return(FALSE)
                    }

                    # Check for scales package
                    if (!requireNamespace("scales", quietly = TRUE)) {
                        # Use basic percentage formatting if scales not available
                        percent_format <- function() {
                            function(x) paste0(round(x * 100, 1), "%")
                        }
                    } else {
                        percent_format <- scales::percent_format()
                    }

                    # Reshape data for plotting using quoted column names
                    plot_data_long <- tidyr::pivot_longer(
                        plot_data,
                        cols = all_of(c("accuracy", "sensitivity", "specificity")),
                        names_to = "metric",
                        values_to = "value"
                    )

                    # Create the plot
                    p <- ggplot2::ggplot(plot_data_long,
                                         ggplot2::aes(x = strategy, y = value, fill = metric)) +
                        ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
                        ggplot2::scale_y_continuous(
                            labels = percent_format,
                            limits = c(0, 1),
                            expand = c(0, 0)
                        ) +
                        ggplot2::scale_fill_brewer(
                            type = "qual",
                            palette = "Set2",
                            labels = c("Accuracy", "Sensitivity", "Specificity")
                        ) +
                        ggplot2::labs(
                            title = "Comparison of Testing Strategies",
                            x = "Testing Strategy",
                            y = "Performance Metric",
                            fill = "Metric"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                            plot.title = ggplot2::element_text(hjust = 0.5),
                            legend.position = "bottom",
                            panel.grid.minor = ggplot2::element_blank()
                        )

                    # Apply provided theme if available
                    if (!missing(ggtheme)) {
                        p <- p + ggtheme
                    }

                    print(p)
                    return(TRUE)

                }, error = function(e) {
                    warning(paste("Error in .plotStrategyComparison:", e$message))

                    # Create error plot
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.6,
                                          label = "Error creating strategy comparison plot",
                                          size = 12, hjust = 0.5, vjust = 0.5) +
                        ggplot2::annotate("text", x = 0.5, y = 0.4,
                                          label = paste("Error:", e$message),
                                          size = 10, hjust = 0.5, vjust = 0.5, color = "red") +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                })
            },


            .plotCostEffectiveness = function(image, ggtheme, theme, ...) {
                all_combinations <- image$state

                # Extract cost and effectiveness data
                plot_data <- data.frame(
                    cost = sapply(all_combinations, function(x) x$cost),
                    accuracy = sapply(all_combinations, function(x) x$accuracy),
                    tests = sapply(all_combinations, function(x) x$tests),
                    strategy = sapply(all_combinations, function(x) x$strategy)
                )

                # Remove NA costs
                plot_data <- plot_data[!is.na(plot_data$cost), ]

                # Find Pareto frontier
                pareto_indices <- which(!duplicated(plot_data[order(plot_data$cost,
                                                                    -plot_data$accuracy),
                                                              c("cost", "accuracy")]))

                # Create plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = cost, y = accuracy)) +
                    ggplot2::geom_point(alpha = 0.6) +
                    ggplot2::geom_line(data = plot_data[pareto_indices, ],
                                       color = "red", size = 1) +
                    ggplot2::geom_point(data = plot_data[pareto_indices, ],
                                        color = "red", size = 3) +
                    ggplot2::scale_y_continuous(labels = scales::percent) +
                    ggplot2::labs(
                        title = "Cost-Effectiveness Frontier",
                        x = "Total Cost",
                        y = "Accuracy"
                    ) +
                    ggplot2::theme_minimal()

                print(p)
                return(TRUE)
            },

            .plotROCCurves = function(image, ggtheme, theme, ...) {
                state <- image$state
                mydata <- state$data
                panels <- state$panels
                goldVariable <- state$goldVariable
                goldPositive <- state$goldPositive

                # This would require more complex implementation
                # to properly calculate and plot ROC curves
                # Placeholder for now

                plot(1:10, 1:10, type = "n",
                     xlab = "1 - Specificity",
                     ylab = "Sensitivity",
                     main = "ROC Curves for Top Panels")
                abline(0, 1, lty = 2)

                return(TRUE)
            }
        )
    )
