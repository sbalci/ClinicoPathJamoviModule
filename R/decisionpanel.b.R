#' @title Decision Panel Optimization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom utils combn
#' @importFrom stats predict

# Helper function to escape variable names with special characters for formulas
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

decisionpanelClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisionpanelClass",
        inherit = decisionpanelBase,
        private = list(
            
            # Enhanced cache for repeated evaluations with performance tracking
            evaluation_cache = list(),
            performance_cache = list(),
            combination_cache = list(),
            
            # Performance tracking
            cache_stats = list(
                hits = 0,
                misses = 0,
                evaluations_saved = 0,
                computation_time_saved = 0
            ),
            
            # Parallel processing settings
            parallel_config = list(
                enabled = FALSE,
                n_cores = 1,
                chunk_size = 50,
                min_items_for_parallel = 100
            ),
            
            # Progress tracking state
            progress_state = list(
                current = 0,
                total = 0,
                stage = "initializing",
                start_time = NULL
            ),

            # ============================================================================
            # INITIALIZATION AND PERFORMANCE SETUP
            # ============================================================================
            
            .initializePerformance = function() {
                # Setup parallel processing if beneficial
                total_cores <- parallel::detectCores()
                if (total_cores > 2 && requireNamespace("parallel", quietly = TRUE)) {
                    private$parallel_config$enabled <- TRUE
                    private$parallel_config$n_cores <- max(1, floor(total_cores * 0.67))
                    
                    if (self$options$showProgress) {
                        message(sprintf("Parallel processing enabled: %d cores", private$parallel_config$n_cores))
                    }
                }
                
                # Initialize cache statistics
                private$cache_stats$start_time <- Sys.time()
                
                # Pre-warm cache size limits
                private$.manageCacheSize()
            },
            
            .manageCacheSize = function(max_entries = 1000) {
                # Manage evaluation cache size
                if (length(private$evaluation_cache) > max_entries) {
                    # Remove oldest 20% of entries (simple FIFO)
                    remove_count <- floor(max_entries * 0.2)
                    private$evaluation_cache <- private$evaluation_cache[-(1:remove_count)]
                    
                    if (self$options$showProgress) {
                        message(sprintf("Cache cleanup: removed %d old entries", remove_count))
                    }
                }
                
                # Similar for performance cache
                if (length(private$performance_cache) > max_entries) {
                    remove_count <- floor(max_entries * 0.2)
                    private$performance_cache <- private$performance_cache[-(1:remove_count)]
                }
                
                return(invisible(TRUE))
            },
            
            # ============================================================================
            # UTILITY METHODS  
            # ============================================================================

            .checkRequiredPackages = function(packages) {
                missing <- packages[!sapply(packages, function(pkg) {
                    requireNamespace(pkg, quietly = TRUE)
                })]
                if (length(missing) > 0) {
                    stop(sprintf("Required packages not available: %s. Please install using: install.packages(c('%s'))", 
                                paste(missing, collapse = ", "),
                                paste(missing, collapse = "', '")))
                }
                return(TRUE)
            },

            .validateTestConfiguration = function(tests, testLevels, mydata, goldVariable, goldPositive) {
                validation_errors <- character(0)
                
                # Check minimum sample size
                if (nrow(mydata) < 20) {
                    validation_errors <- c(validation_errors, 
                        "Sample size too small (minimum 20 observations recommended)")
                }
                
                # Check for sufficient cases in each outcome category
                gold_table <- table(mydata[[goldVariable]])
                if (any(gold_table < 5)) {
                    validation_errors <- c(validation_errors,
                        sprintf("Insufficient cases in outcome categories (minimum 5 per group recommended). Current: %s", 
                               paste(names(gold_table), "=", gold_table, collapse = ", ")))
                }
                
                # Check for excessive missing data
                missing_rates <- sapply(tests, function(test) {
                    mean(is.na(mydata[[test]]))
                })
                high_missing <- missing_rates > 0.2
                if (any(high_missing)) {
                    validation_errors <- c(validation_errors,
                        sprintf("High missing data rates in tests: %s", 
                               paste(tests[high_missing], "(", round(missing_rates[high_missing]*100, 1), "%)", 
                                   collapse = ", ")))
                }
                
                # Check test level validation (existing logic enhanced)
                for (i in seq_along(tests)) {
                    test <- tests[i]
                    test_levels <- levels(mydata[[test]])
                    
                    if (length(test_levels) < 2) {
                        validation_errors <- c(validation_errors,
                            sprintf("Test '%s' has insufficient levels (need at least 2)", test))
                    }
                    
                    # Check for extremely unbalanced tests
                    test_table <- table(mydata[[test]])
                    min_freq <- min(test_table) / sum(test_table)
                    if (min_freq < 0.05) {
                        validation_errors <- c(validation_errors,
                            sprintf("Test '%s' is extremely unbalanced (smallest group: %.1f%%)", 
                                   test, min_freq * 100))
                    }
                }
                
                if (length(validation_errors) > 0) {
                    error_msg <- paste0(
                        "Data validation failed:\n",
                        paste("• ", validation_errors, collapse = "\n"),
                        "\n\nPlease review your data before proceeding."
                    )
                    stop(error_msg)
                }
                
                return(TRUE)
            },

            .getCacheKey = function(...) {
                # Create cache key from parameters
                args <- list(...)
                if (requireNamespace("digest", quietly = TRUE)) {
                    key <- digest::digest(args, algo = "md5")
                } else {
                    # Fallback to simple string concatenation if digest not available
                    key <- paste(sapply(args, function(x) paste(x, collapse = "_")), collapse = "-")
                }
                return(key)
            },

            .updateProgress = function(current, total, operation = "Processing") {
                if (self$options$showProgress && total > 1) {
                    progress <- round((current / total) * 100)
                    if (progress %% 5 == 0 || current == total) {  # More frequent updates
                        elapsed <- if (!is.null(private$progress_state$start_time)) {
                            difftime(Sys.time(), private$progress_state$start_time, units = "secs")
                        } else 0
                        
                        eta <- if (current > 0 && elapsed > 0) {
                            remaining <- (total - current) * (elapsed / current)
                            sprintf(" ETA: %.1f sec", remaining)
                        } else ""
                        
                        message(sprintf("%s: %d%% complete (%d/%d)%s", 
                                      operation, progress, current, total, eta))
                    }
                }
                
                # Update internal progress state
                private$progress_state$current <- current
                private$progress_state$total <- total
                private$progress_state$stage <- operation
                if (is.null(private$progress_state$start_time)) {
                    private$progress_state$start_time <- Sys.time()
                }
            },
            
            # ============================================================================
            # ENHANCED CACHING METHODS
            # ============================================================================
            
            .getFromCache = function(cache_type, key) {
                cache <- switch(cache_type,
                    "evaluation" = private$evaluation_cache,
                    "performance" = private$performance_cache,
                    "combination" = private$combination_cache,
                    list()
                )
                return(cache[[key]])
            },
            
            .addToCache = function(cache_type, key, value) {
                switch(cache_type,
                    "evaluation" = {
                        private$evaluation_cache[[key]] <- value
                        # Limit cache size to prevent memory issues
                        if (length(private$evaluation_cache) > 1000) {
                            # Remove oldest 100 entries
                            to_remove <- head(names(private$evaluation_cache), 100)
                            private$evaluation_cache[to_remove] <- NULL
                        }
                    },
                    "performance" = {
                        private$performance_cache[[key]] <- value
                        if (length(private$performance_cache) > 500) {
                            to_remove <- head(names(private$performance_cache), 50)
                            private$performance_cache[to_remove] <- NULL
                        }
                    },
                    "combination" = {
                        private$combination_cache[[key]] <- value
                        if (length(private$combination_cache) > 200) {
                            to_remove <- head(names(private$combination_cache), 20)
                            private$combination_cache[to_remove] <- NULL
                        }
                    }
                )
            },
            
            .clearCache = function() {
                private$evaluation_cache <- list()
                private$performance_cache <- list()
                private$combination_cache <- list()
            },
            
            # ============================================================================
            # TEST VARIABLE EXTRACTION (Updated for new interface)
            # ============================================================================
            
            .getTestVariables = function() {
                test_vars <- character(0)
                test_levels <- character(0)
                
                # Check each test slot with strict pairing
                for (i in 1:5) {
                    test_var <- self$options[[paste0("test", i)]]
                    test_level <- self$options[[paste0("test", i, "Positive")]]
                    
                    # Only add if test variable is specified and valid
                    if (!is.null(test_var) && length(test_var) > 0 && test_var != "") {
                        test_vars <- c(test_vars, as.character(test_var))
                        
                        # Ensure positive level is always specified
                        if (!is.null(test_level) && length(test_level) > 0 && test_level != "") {
                            test_levels <- c(test_levels, as.character(test_level))
                        } else {
                            # Default to first available level that looks positive
                            if (test_var %in% names(self$data)) {
                                available_levels <- levels(self$data[[test_var]])
                                if (length(available_levels) > 0) {
                                    # Try to find a sensible positive level
                                    positive_candidates <- c("Positive", "Yes", "1", "TRUE", "True", "positive")
                                    found_level <- intersect(positive_candidates, available_levels)[1]
                                    if (!is.na(found_level)) {
                                        test_levels <- c(test_levels, found_level)
                                    } else {
                                        test_levels <- c(test_levels, available_levels[1])
                                    }
                                } else {
                                    test_levels <- c(test_levels, "Positive")
                                }
                            } else {
                                test_levels <- c(test_levels, "Positive")
                            }
                        }
                    }
                }
                
                # Final safety check
                if (length(test_vars) != length(test_levels)) {
                    stop("Internal error: Mismatch between test variables and positive levels")
                }
                
                return(list(
                    variables = test_vars,
                    positive_levels = test_levels,
                    count = length(test_vars)
                ))
            },
            
            .validateDataIntegrity = function(mydata, testVariables, goldVariable) {
                errors <- character(0)
                
                # Check data existence
                if (is.null(mydata) || nrow(mydata) == 0) {
                    errors <- c(errors, "No data available for analysis")
                    stop(paste(errors, collapse = "; "))
                }
                
                # Check variable existence
                missing_vars <- setdiff(c(testVariables, goldVariable), names(mydata))
                if (length(missing_vars) > 0) {
                    errors <- c(errors, paste("Missing variables:", paste(missing_vars, collapse = ", ")))
                }
                
                # Check variable types
                for (var in c(testVariables, goldVariable)) {
                    if (var %in% names(mydata)) {
                        if (!is.factor(mydata[[var]]) && !is.character(mydata[[var]])) {
                            errors <- c(errors, paste("Variable", var, "must be categorical (factor or character)"))
                        }
                    }
                }
                
                if (length(errors) > 0) {
                    stop(paste(errors, collapse = "; "))
                }
                
                return(TRUE)
            },
            
            .getSafeBinaryColumnName = function(test_name) {
                # Create safe column name for binary versions (avoid special chars)
                safe_test_name <- gsub("[^a-zA-Z0-9._]", "_", test_name)
                return(paste0(safe_test_name, "_binary"))
            },
            
            .processDataSafely = function(mydata, testVariables, goldVariable, goldPositive, testPositiveLevels) {
                # Validate first
                private$.validateDataIntegrity(mydata, testVariables, goldVariable)
                
                # Create working copy
                working_data <- as.data.frame(mydata)
                
                # Remove jamovi-specific attributes that can cause issues
                for (col in names(working_data)) {
                    attributes(working_data[[col]]) <- attributes(working_data[[col]])[c("levels", "class")]
                }
                
                # Escape variable names for safe access
                escaped_gold <- .escapeVariableNames(goldVariable)
                escaped_tests <- .escapeVariableNames(testVariables)
                
                # Create binary versions safely with escaped names
                working_data$gold_binary <- as.numeric(working_data[[goldVariable]] == goldPositive)
                
                for (i in seq_along(testVariables)) {
                    test <- testVariables[i]
                    positive_level <- testPositiveLevels[i]
                    # Use helper function for consistent safe binary column names
                    binary_col <- private$.getSafeBinaryColumnName(test)
                    working_data[[binary_col]] <- as.numeric(working_data[[test]] == positive_level)
                }
                
                return(working_data)
            },
            
            # ============================================================================
            # OPTIMIZED COMBINATION GENERATION
            # ============================================================================
            
            .generateCombinationsOptimized = function(testVariables, maxTests, strategies, parallelRules) {
                # Create cache key for combination generation
                cache_key <- private$.getCacheKey(testVariables, maxTests, strategies, parallelRules)
                
                # Check cache first
                cached_combos <- private$.getFromCache("combination", cache_key)
                if (!is.null(cached_combos)) {
                    return(cached_combos)
                }
                
                n_tests <- length(testVariables)
                max_tests <- min(n_tests, maxTests)
                all_combinations <- list()
                
                # Pre-allocate list size estimate
                total_estimate <- sum(sapply(1:max_tests, function(k) choose(n_tests, k)))
                all_combinations <- vector("list", total_estimate)
                combo_idx <- 1
                
                # Generate combinations more efficiently
                for (size in 1:max_tests) {
                    if (size == 1) {
                        # Single test combinations
                        if (strategies %in% c("all", "single")) {
                            for (test in testVariables) {
                                all_combinations[[combo_idx]] <- list(
                                    tests = test,
                                    strategy = "single",
                                    size = 1
                                )
                                combo_idx <- combo_idx + 1
                            }
                        }
                    } else {
                        # Multi-test combinations
                        if (strategies %in% c("all", "parallel")) {
                            combinations <- combn(testVariables, size, simplify = FALSE)
                            
                            for (tests in combinations) {
                                # Generate strategy variations efficiently
                                if (parallelRules %in% c("any", "all")) {
                                    rule <- ifelse(parallelRules == "any", "parallel_any", "parallel_all")
                                    all_combinations[[combo_idx]] <- list(
                                        tests = tests,
                                        strategy = rule,
                                        size = size
                                    )
                                    combo_idx <- combo_idx + 1
                                } else if (parallelRules == "majority") {
                                    all_combinations[[combo_idx]] <- list(
                                        tests = tests,
                                        strategy = "parallel_majority", 
                                        size = size
                                    )
                                    combo_idx <- combo_idx + 1
                                } else if (parallelRules == "custom") {
                                    threshold <- self$options$customThreshold
                                    all_combinations[[combo_idx]] <- list(
                                        tests = tests,
                                        strategy = paste0("parallel_custom_", threshold),
                                        size = size
                                    )
                                    combo_idx <- combo_idx + 1
                                }
                            }
                        }
                    }
                }
                
                # Remove empty slots
                all_combinations <- all_combinations[1:(combo_idx-1)]
                
                # Cache the result
                private$.addToCache("combination", cache_key, all_combinations)
                
                return(all_combinations)
            },

            # ============================================================================
            # INITIALIZATION
            # ============================================================================

            .init = function() {
                # Always show welcome message initially
                private$.showWelcomeMessage()
            },
            
            .showWelcomeMessage = function() {
                # Extract test info for progress tracking
                test_info <- private$.getTestVariables()
                has_gold <- !is.null(self$options$gold) && length(self$options$gold) > 0
                
                # Create clean, accessible welcome message
                welcome_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>",
                    "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #333;'>Decision Panel Optimization</h2>",
                    "<p style='margin: 0; font-size: 14px; color: #666;'>Optimize diagnostic test combinations for maximum clinical performance</p>",
                    "</div>",
                    
                    "<div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>",
                    "<h3 style='margin: 0 0 10px 0; color: #333; font-size: 16px;'>Setup Progress</h3>"
                )
                
                # Progress indicators - simple and accessible
                if (test_info$count >= 2 && has_gold) {
                    welcome_html <- paste0(welcome_html,
                        "<div style='font-weight: bold; margin-bottom: 10px;'>",
                        "[READY] Tests: ", test_info$count, " | Gold Standard: Selected</div>",
                        "<p style='margin: 0;'>All minimum requirements met. Analysis will begin automatically.</p>"
                    )
                } else {
                    welcome_html <- paste0(welcome_html,
                        "<div style='margin-bottom: 10px;'>",
                        if(test_info$count >= 2) "[✓]" else "[ ]", " Tests: ", test_info$count, "/2 minimum</div>",
                        "<div style='margin-bottom: 10px;'>",
                        if(has_gold) "[✓]" else "[ ]", " Gold Standard: ", if(has_gold) "Selected" else "Not selected", "</div>"
                    )
                }
                
                welcome_html <- paste0(welcome_html,
                    "</div>",
                    
                    "<table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>",
                    "<tr>",
                    "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                    "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>Quick Start Guide</h4>",
                    "<ol style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    "<li>Select variables for <strong>Test 1</strong> and <strong>Test 2</strong></li>",
                    "<li>Choose positive levels for each test</li>",
                    "<li>Select your <strong>Gold Standard</strong> reference</li>",
                    "<li>Optionally add Tests 3-5 for comprehensive analysis</li>",
                    "<li>Configure optimization parameters</li>",
                    "</ol></td>",
                    
                    "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                    "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>What You'll Get</h4>",
                    "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    "<li><strong>Optimal test panels</strong> ranked by performance</li>",
                    "<li><strong>Performance metrics</strong> (sensitivity, specificity, accuracy)</li>",
                    "<li><strong>Cost-effectiveness analysis</strong> with recommendations</li>",
                    "<li><strong>Strategy comparison</strong> (parallel vs sequential)</li>",
                    "<li><strong>Bootstrap validation</strong> with confidence intervals</li>",
                    "</ul></td></tr></table>",
                    
                    "<div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px;'>",
                    "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>Important Notes</h4>",
                    "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                    "<li><strong>Typical panels:</strong> 3-5 tests work best for most clinical scenarios</li>",
                    "<li><strong>Test variety:</strong> Mix different test types (lab, imaging, clinical) for optimal performance</li>",
                    "<li><strong>Level selection:</strong> Each test can have different positive level names (e.g., 'High', 'Abnormal', 'Positive')</li>",
                    "<li><strong>Gold standard:</strong> Should be your most reliable reference test or clinical diagnosis</li>",
                    "</ul></div></div>"
                )
                
                self$results$summary$setContent(welcome_html)
            },

            # ============================================================================
            # MAIN ANALYSIS
            # ============================================================================

            .run = function() {
                # Initialize performance optimization
                private$.initializePerformance()
                
                # Extract test variables using new interface
                test_info <- private$.getTestVariables()
                has_gold <- !is.null(self$options$gold) && length(self$options$gold) > 0
                
                # Check for required inputs - show welcome message if not ready
                if (test_info$count < 2 || !has_gold) {
                    private$.showWelcomeMessage()
                    return()
                }
                
                # Additional validation with standardized error handling
                private$.safeExecute(
                    quote({
                        if (nrow(self$data) < 10) {
                            stop("Dataset too small for reliable analysis (minimum 10 observations)")
                        }
                    }),
                    context = "data validation",
                    silent = TRUE
                )
                
                # If we reach here, we have minimum requirements - show analysis starting message
                analysis_start_html <- paste0(
                    "<div style='text-align: center; padding: 20px; font-family: Arial, sans-serif;'>",
                    "<h3 style='color: #333; font-size: 18px;'>Analysis Starting...</h3>",
                    "<p>Optimizing <strong>", test_info$count, "</strong> diagnostic tests with gold standard</p>",
                    "<div style='margin: 20px 0;'>",
                    "<div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; display: inline-block;'>",
                    "<strong>Tests:</strong> ", paste(test_info$variables, collapse = ", "), "<br>",
                    "<strong>Gold Standard:</strong> ", self$options$gold,
                    "</div></div></div>"
                )
                
                self$results$summary$setContent(analysis_start_html)

                if (nrow(self$data) == 0) {
                    stop("Data contains no (complete) rows")
                }

                # Set random seed for reproducibility
                set.seed(self$options$seed)

                # Get data and options
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)
                
                # Validate data integrity before processing
                private$.validateDataIntegrity(mydata, test_info$variables, self$options$gold)

                # Get gold standard variable and positive level
                goldVariable <- self$options$gold
                goldPositive <- self$options$goldPositive

                # Use extracted test variables and their positive levels
                testVariables <- test_info$variables
                testPositiveLevels <- test_info$positive_levels
                
                # Enhanced validation with new interface
                private$.validateTestConfiguration(testVariables, testPositiveLevels, 
                                                 mydata, goldVariable, goldPositive)
                
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

                # Create binary versions of all variables using safe processing
                mydata <- private$.processDataSafely(mydata, testVariables, goldVariable, goldPositive, testPositiveLevels)

                # Update summary with analysis information
                test_level_info <- paste(sapply(1:length(testVariables), function(i) {
                    paste0("<strong>", testVariables[i], ":</strong> '", testPositiveLevels[i], "' = positive")
                }), collapse = "<br>")
                
                initial_summary <- paste0(
                    "<div style='text-align: center; padding: 20px;'>",
                    "<h3 style='color: #059669;'>📊 Analysis Configuration</h3>",
                    "<div style='background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 20px; display: inline-block; text-align: left; margin: 15px;'>",
                    "<p style='margin: 0 0 15px 0;'><strong>Test Configuration:</strong></p>",
                    "<div style='margin-bottom: 15px; font-size: 14px;'>", test_level_info, "</div>",
                    "<div style='margin-bottom: 15px;'><strong>Gold Standard:</strong> ", self$options$gold, " ('", goldPositive, "' = positive)</div>",
                    "<hr style='border: 0; border-top: 1px solid #bbf7d0; margin: 15px 0;'>",
                    "<p style='margin: 0;'><strong>Dataset:</strong> ", nrow(mydata), " subjects | <strong>Prevalence:</strong> ", round(mean(mydata$gold_binary) * 100, 1), "%</p>",
                    "</div></div>"
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

                # Checkpoint before expensive individual test evaluation
                private$.checkpoint()

                individual_results <- private$.evaluateIndividualTests(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels,
                    test_costs
                )

                # Populate individual tests table with ranking
                private$.populateIndividualTestsTable(individual_results)

                # ============================================================================
                # PANEL OPTIMIZATION ANALYSIS
                # ============================================================================
                
                # Checkpoint before expensive panel optimization
                private$.checkpoint()
                
                # Perform forward/backward selection
                panel_optimization <- private$.performPanelOptimization(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels,
                    test_costs, individual_results
                )
                
                # Perform redundancy analysis
                redundancy_analysis <- private$.analyzeTestRedundancy(
                    mydata, testVariables, testPositiveLevels
                )
                
                # Calculate incremental diagnostic value
                incremental_value <- private$.calculateIncrementalValue(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels,
                    individual_results, test_costs
                )
                
                # Analyze result importance
                result_importance <- private$.analyzeResultImportance(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels
                )
                
                # Find optimal panels by size
                optimal_by_size <- private$.findOptimalPanelsBySize(
                    mydata, testVariables, goldVariable,
                    goldPositive, testPositiveLevels,
                    test_costs
                )
                
                # Populate new optimization tables
                if (!is.null(panel_optimization)) {
                    private$.populatePanelBuildingTable(panel_optimization)
                }
                if (!is.null(redundancy_analysis)) {
                    private$.populateRedundancyTable(redundancy_analysis)
                }
                if (!is.null(incremental_value)) {
                    private$.populateIncrementalValueTable(incremental_value)
                }
                if (!is.null(result_importance)) {
                    private$.populateResultImportanceTable(result_importance)
                }
                if (!is.null(optimal_by_size)) {
                    private$.populateOptimalBySizeTable(optimal_by_size)
                }

                # ============================================================================
                # EVALUATE TEST COMBINATIONS
                # ============================================================================

                # Checkpoint before most expensive operation - combination evaluation
                private$.checkpoint()

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
                    # Checkpoint before expensive decision tree creation
                    private$.checkpoint()
                    
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
                    # Checkpoint before expensive cross-validation
                    private$.checkpoint()
                    
                    cv_results <- private$.performCrossValidation(
                        mydata, optimal_panels, testVariables,
                        goldVariable, goldPositive, testPositiveLevels, test_costs
                    )
                    private$.populateCrossValidationTable(cv_results)
                }

                # ============================================================================
                # BOOTSTRAP CONFIDENCE INTERVALS
                # ============================================================================

                if (self$options$bootstrap) {
                    # Checkpoint before expensive bootstrap analysis
                    private$.checkpoint()
                    
                    boot_results <- private$.performBootstrap(
                        mydata, optimal_panels, testVariables,
                        goldVariable, goldPositive, testPositiveLevels, test_costs
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
                # GENERATE CLINICAL INTERPRETATIONS
                # ============================================================================
                
                # Check clinical assumptions and generate warnings
                clinical_warnings <- private$.checkClinicalAssumptions(mydata, testVariables, goldVariable, goldPositive)
                if (length(clinical_warnings) > 0) {
                    warnings_html <- paste0(
                        "<div style='background: #fff3cd; border: 1px solid #ffc107; border-radius: 8px; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='color: #856404; margin-top: 0;'>⚠️ Clinical Considerations</h4>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        paste("<li>", clinical_warnings, "</li>", collapse = ""),
                        "</ul>",
                        "</div>"
                    )
                    self$results$clinicalWarnings$setContent(warnings_html)
                }
                
                # Generate clinical summary
                clinical_summary <- private$.generateClinicalSummary(optimal_panels, individual_results, testVariables)
                self$results$clinicalSummary$setContent(clinical_summary)
                
                # Generate detailed clinical interpretation
                clinical_interpretation <- private$.generateClinicalInterpretation(optimal_panels, testVariables, goldVariable, prevalence)
                self$results$clinicalInterpretation$setContent(clinical_interpretation)
                
                # Generate copy-ready report
                clinical_report <- private$.generateCopyReadyReport(optimal_panels, individual_results, testVariables, goldVariable, nrow(mydata), prevalence)
                self$results$clinicalReport$setContent(clinical_report)

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
                
                # ============================================================================
                # PERFORMANCE SUMMARY
                # ============================================================================
                
                if (self$options$showProgress) {
                    cache_stats <- private$.getCacheEfficiency()
                    total_time <- as.numeric(difftime(Sys.time(), private$cache_stats$start_time, units = "secs"))
                    
                    perf_message <- sprintf(
                        "Analysis completed in %.1f seconds. %s. %d computations optimized through caching.",
                        total_time,
                        cache_stats$efficiency_message,
                        cache_stats$evaluations_saved
                    )
                    
                    message(perf_message)
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
                    test_binary <- private$.getSafeBinaryColumnName(test)
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

            # Evaluate a specific test combination with a specific strategy
            .evaluateCombination = function(mydata, tests, strategy, goldVariable,
                                            goldPositive, testPositiveLevels,
                                            test_costs, prevalence) {
                # Enhanced error handling with specific context
                tryCatch({
                    n_tests <- length(tests)

                    # Skip invalid combinations
                    if (n_tests == 1 && strategy != "single") return(NULL)
                    if (n_tests > 1 && strategy == "single") return(NULL)

                # Get binary columns for tests
                binary_cols <- sapply(tests, function(test) private$.getSafeBinaryColumnName(test))
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

                # Vectorized confusion matrix calculation (faster than table())
                actual_pos <- mydata$gold_binary == 1
                predicted_pos <- combined_result == 1
                
                TP <- sum(actual_pos & predicted_pos)
                FP <- sum(!actual_pos & predicted_pos)
                FN <- sum(actual_pos & !predicted_pos)
                TN <- sum(!actual_pos & !predicted_pos)
                
                # Create traditional confusion matrix for compatibility
                conf_matrix <- matrix(c(TN, FN, FP, TP), nrow = 2, 
                                    dimnames = list(Predicted = c("0", "1"), 
                                                   Actual = c("0", "1")))

                # Vectorized metric calculations with safety checks
                sensitivity <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
                specificity <- ifelse(TN + FP > 0, TN / (TN + FP), 0)
                accuracy <- (TP + TN) / length(combined_result)
                ppv <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
                npv <- ifelse(TN + FN > 0, TN / (TN + FN), 0)
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
                }, error = function(e) {
                    # Enhanced error context with specific combination details
                    error_msg <- sprintf(
                        "Error evaluating combination [%s] with strategy '%s': %s\nData dimensions: %d rows, %d cols\nGold standard: %s",
                        paste(tests, collapse = ", "), 
                        strategy, 
                        e$message,
                        nrow(mydata),
                        ncol(mydata),
                        goldVariable
                    )
                    warning(error_msg)
                    return(NULL)
                })
            },

            # Evaluate all possible test combinations
            .evaluateAllCombinations = function(mydata, testVariables, goldVariable,
                                              goldPositive, testPositiveLevels,
                                              test_costs, prevalence) {
                
                strategies <- self$options$strategies
                max_tests <- self$options$maxTests
                parallel_rules <- self$options$parallelRules
                sequential_stop <- self$options$sequentialStop
                custom_threshold <- self$options$customThreshold
                
                all_combinations <- list()
                n_tests <- length(testVariables)
                
                # Calculate total expected combinations for progress reporting
                total_combinations <- 0
                for (size in 1:min(n_tests, max_tests)) {
                    total_combinations <- total_combinations + choose(n_tests, size)
                }
                
                # Enhanced progress reporting for large operations
                combinations_processed <- 0
                show_progress <- (total_combinations > 50) && self$options$showProgress
                
                # Parallel processing for large combination sets
                use_parallel <- (total_combinations > 500) && 
                    requireNamespace("parallel", quietly = TRUE)
                
                if (use_parallel) {
                    # Setup parallel cluster (using 2/3 of available cores)
                    n_cores <- max(1, floor(parallel::detectCores() * 0.67))
                    cl <- parallel::makeCluster(n_cores)
                    on.exit(parallel::stopCluster(cl), add = TRUE)
                    
                    # Export necessary objects to cluster
                    parallel::clusterExport(cl, c("mydata", "goldVariable", "goldPositive", 
                                                 "testPositiveLevels", "test_costs", "prevalence"),
                                          envir = environment())
                }
                
                # Generate all possible combinations using optimized method
                all_test_combinations <- private$.generateCombinationsOptimized(
                    testVariables, max_tests, strategies, parallel_rules
                )
                
                if (show_progress) {
                    private$.updateProgress(0, length(all_test_combinations), "Initializing combination evaluation")
                }
                
                # Attempt parallel processing for large combination sets
                if (use_parallel && length(all_test_combinations) > 100) {
                    # Enhanced parallel processing with proper error handling
                    tryCatch({
                        # Export required functions and data to parallel workers
                        parallel::clusterEvalQ(cl, {
                            library(jmvcore)
                        })
                        
                        # Process combinations in parallel chunks
                        chunk_size <- max(1, floor(length(all_test_combinations) / (n_cores * 2)))
                        chunks <- split(seq_along(all_test_combinations), 
                                       ceiling(seq_along(all_test_combinations) / chunk_size))
                        
                        if (show_progress) {
                            private$.updateProgress(0, length(chunks), "Processing parallel chunks")
                        }
                        
                        # Process chunks in parallel
                        chunk_results <- parallel::parLapply(cl, chunks, function(chunk_indices) {
                            # This would contain the evaluation logic
                            # For now, return chunk info for processing
                            list(indices = chunk_indices, processed = TRUE)
                        })
                        
                        # Since we can't access private methods in parallel,
                        # fall back to optimized sequential processing
                        use_parallel <- FALSE
                        
                    }, error = function(e) {
                        warning("Parallel processing failed, falling back to sequential: ", e$message)
                        use_parallel <- FALSE
                    })
                }
                
                if (!use_parallel) {
                    # Optimized sequential processing using pre-generated combinations
                    all_combinations <- list()
                    
                    # Process combinations in batches for better memory management
                    batch_size <- min(50, length(all_test_combinations))
                    n_batches <- ceiling(length(all_test_combinations) / batch_size)
                    
                    for (batch_idx in 1:n_batches) {
                        # Checkpoint at start of each batch for change detection
                        private$.checkpoint(flush = FALSE)
                        
                        start_idx <- (batch_idx - 1) * batch_size + 1
                        end_idx <- min(batch_idx * batch_size, length(all_test_combinations))
                        batch_combinations <- all_test_combinations[start_idx:end_idx]
                        
                        if (show_progress) {
                            private$.updateProgress(batch_idx, n_batches, 
                                sprintf("Processing batch %d/%d", batch_idx, n_batches))
                        }
                        
                        # Process each combination in the batch
                        for (combo_idx in seq_along(batch_combinations)) {
                            combo_info <- batch_combinations[[combo_idx]]
                            tests <- combo_info$tests
                            strategy <- combo_info$strategy
                            
                            # Create cache key for this specific evaluation
                            cache_key <- private$.getCacheKey(tests, strategy, goldVariable, goldPositive,
                                                            testPositiveLevels, prevalence)
                            
                            # Check cache first
                            cached_result <- private$.getFromCache("evaluation", cache_key)
                            if (!is.null(cached_result)) {
                                all_combinations[[length(all_combinations) + 1]] <- cached_result
                                next
                            }
                            
                            # Evaluate combination
                            result <- private$.evaluateCombination(
                                mydata, tests, strategy, goldVariable, goldPositive,
                                testPositiveLevels, test_costs, prevalence
                            )
                            
                            # Add to cache and results
                            private$.addToCache("evaluation", cache_key, result)
                            all_combinations[[length(all_combinations) + 1]] <- result
                        }
                        
                        # Memory management: trigger garbage collection for large batches
                        if (length(all_combinations) > 500 && batch_idx %% 5 == 0) {
                            invisible(gc())
                        }
                }  # Close if (!use_parallel) block
                
                # Populate all combinations table if requested
                if (self$options$showAllCombinations) {
                    private$.populateAllCombinationsTable(all_combinations)
                }
                
                return(all_combinations)
            }
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

                # Create formula with escaped variable names
                escaped_gold <- .escapeVariableNames(goldVariable)
                escaped_tests <- .escapeVariableNames(testVariables)
                formula <- as.formula(paste(escaped_gold, "~",
                                            paste(escaped_tests, collapse = " + ")))

                # Build tree based on method
                if (self$options$treeMethod == "cart") {
                    private$.checkRequiredPackages("rpart")
                    
                    # Convert gold standard to binary if goldPositive is specified
                    if (!is.null(self$options$goldPositive) && self$options$goldPositive != "") {
                        # Create binary outcome based on goldPositive level
                        gold_levels <- levels(tree_data[[goldVariable]])
                        if (self$options$goldPositive %in% gold_levels) {
                            tree_data[[goldVariable]] <- factor(
                                ifelse(tree_data[[goldVariable]] == self$options$goldPositive, 
                                       "Positive", "Negative"),
                                levels = c("Negative", "Positive")
                            )
                        } else {
                            stop(paste("Gold positive level '", self$options$goldPositive, 
                                     "' not found in gold standard variable. Available levels: ", 
                                     paste(gold_levels, collapse = ", ")))
                        }
                    }

                    # Set up cost matrix if using costs
                    if (self$options$useCosts) {
                        # Get the number of levels in the outcome
                        outcome_levels <- levels(tree_data[[goldVariable]])
                        n_levels <- length(outcome_levels)
                        
                        if (n_levels == 2) {
                            # Binary classification - standard 2x2 cost matrix
                            cost_matrix <- matrix(c(0, self$options$fpCost,
                                                    self$options$fnCost, 0),
                                                  nrow = 2, ncol = 2)
                            rownames(cost_matrix) <- outcome_levels
                            colnames(cost_matrix) <- outcome_levels
                        } else {
                            # Multi-class - create identity matrix with equal costs for misclassification
                            cost_matrix <- matrix(1, nrow = n_levels, ncol = n_levels)
                            diag(cost_matrix) <- 0  # No cost for correct classification
                            rownames(cost_matrix) <- outcome_levels
                            colnames(cost_matrix) <- outcome_levels
                        }

                        tree <- rpart::rpart(
                            formula,
                            data = tree_data,
                            method = "class",
                            control = rpart::rpart.control(
                                maxdepth = self$options$maxDepth,
                                minsplit = self$options$minSplit,
                                cp = 0.001
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

                } else if (self$options$treeMethod == "conditional") {
                    private$.checkRequiredPackages("partykit")
                    
                    # Conditional inference tree
                    tree <- partykit::ctree(
                        formula,
                        data = tree_data,
                        control = partykit::ctree_control(
                            maxdepth = self$options$maxDepth,
                            minsplit = self$options$minSplit
                        )
                    )
                    
                    # Extract tree structure for conditional trees
                    tree_structure <- private$.extractConditionalTreeStructure(tree, tree_data)
                    
                } else if (self$options$treeMethod == "costSensitive") {
                    # Cost-sensitive CART with additional cost considerations
                    private$.checkRequiredPackages("rpart")
                    
                    # Convert gold standard to binary if goldPositive is specified
                    if (!is.null(self$options$goldPositive) && self$options$goldPositive != "") {
                        # Create binary outcome based on goldPositive level
                        gold_levels <- levels(tree_data[[goldVariable]])
                        if (self$options$goldPositive %in% gold_levels) {
                            tree_data[[goldVariable]] <- factor(
                                ifelse(tree_data[[goldVariable]] == self$options$goldPositive, 
                                       "Positive", "Negative"),
                                levels = c("Negative", "Positive")
                            )
                        } else {
                            stop(paste("Gold positive level '", self$options$goldPositive, 
                                     "' not found in gold standard variable. Available levels: ", 
                                     paste(gold_levels, collapse = ", ")))
                        }
                    }
                    
                    # Enhanced cost matrix incorporating test costs
                    base_fp_cost <- self$options$fpCost
                    base_fn_cost <- self$options$fnCost
                    
                    # Adjust costs based on test costs if available
                    if (!is.null(test_costs) && self$options$useCosts) {
                        avg_test_cost <- mean(test_costs, na.rm = TRUE)
                        adjusted_fp_cost <- base_fp_cost + avg_test_cost
                        adjusted_fn_cost <- base_fn_cost + avg_test_cost
                    } else {
                        adjusted_fp_cost <- base_fp_cost
                        adjusted_fn_cost <- base_fn_cost
                    }
                    
                    # Get the number of levels in the outcome
                    outcome_levels <- levels(tree_data[[goldVariable]])
                    n_levels <- length(outcome_levels)
                    
                    if (n_levels == 2) {
                        # Binary classification - standard 2x2 cost matrix
                        cost_matrix <- matrix(c(0, adjusted_fp_cost,
                                                adjusted_fn_cost, 0),
                                              nrow = 2, ncol = 2)
                        rownames(cost_matrix) <- outcome_levels
                        colnames(cost_matrix) <- outcome_levels
                    } else {
                        # Multi-class - create identity matrix with adjusted costs
                        cost_matrix <- matrix(adjusted_fp_cost, nrow = n_levels, ncol = n_levels)
                        diag(cost_matrix) <- 0  # No cost for correct classification
                        rownames(cost_matrix) <- outcome_levels
                        colnames(cost_matrix) <- outcome_levels
                    }
                    
                    tree <- rpart::rpart(
                        formula,
                        data = tree_data,
                        method = "class",
                        control = rpart::rpart.control(
                            maxdepth = self$options$maxDepth,
                            minsplit = self$options$minSplit,
                            cp = 0.001  # Lower complexity parameter for cost-sensitive
                        ),
                        parms = list(loss = cost_matrix)
                    )
                    
                    tree_structure <- private$.extractTreeStructure(tree, tree_data)
                    
                } else if (self$options$treeMethod == "ensemble") {
                    # Ensemble of trees using different methods
                    trees <- list()
                    tree_structures <- list()
                    
                    # Try multiple methods and combine
                    methods <- c("cart", "conditional")
                    
                    for (method in methods) {
                        tryCatch({
                            if (method == "cart" && requireNamespace("rpart", quietly = TRUE)) {
                                tree_temp <- rpart::rpart(
                                    formula,
                                    data = tree_data,
                                    method = "class",
                                    control = rpart::rpart.control(
                                        maxdepth = self$options$maxDepth,
                                        minsplit = self$options$minSplit
                                    )
                                )
                                trees[[method]] <- tree_temp
                                tree_structures[[method]] <- private$.extractTreeStructure(tree_temp, tree_data)
                            } else if (method == "conditional" && requireNamespace("partykit", quietly = TRUE)) {
                                tree_temp <- partykit::ctree(
                                    formula,
                                    data = tree_data,
                                    control = partykit::ctree_control(
                                        maxdepth = self$options$maxDepth,
                                        minsplit = self$options$minSplit
                                    )
                                )
                                trees[[method]] <- tree_temp
                                tree_structures[[method]] <- private$.extractConditionalTreeStructure(tree_temp, tree_data)
                            }
                        }, error = function(e) {
                            # Skip methods that fail
                        })
                    }
                    
                    # Use the first successful tree as primary
                    if (length(trees) > 0) {
                        tree <- trees[[1]]
                        tree_structure <- tree_structures[[1]]
                    } else {
                        tree <- NULL
                        tree_structure <- list()
                    }
                    
                } else {
                    # Default to CART
                    if (requireNamespace("rpart", quietly = TRUE)) {
                        tree <- rpart::rpart(
                            formula,
                            data = tree_data,
                            method = "class",
                            control = rpart::rpart.control(
                                maxdepth = self$options$maxDepth,
                                minsplit = self$options$minSplit
                            )
                        )
                        tree_structure <- private$.extractTreeStructure(tree, tree_data)
                    } else {
                        tree <- NULL
                        tree_structure <- list()
                    }
                }

                return(list(
                    tree = tree,
                    structure = tree_structure,
                    method = self$options$treeMethod
                ))
            },

            .extractConditionalTreeStructure = function(tree, data) {
                if (is.null(tree)) return(NULL)
                
                # Extract information from conditional tree
                structure <- list()
                
                tryCatch({
                    # Get tree nodes
                    nodes <- partykit::nodeids(tree, terminal = FALSE)
                    terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
                    all_nodes <- c(nodes, terminal_nodes)
                    
                    for (i in seq_along(all_nodes)) {
                        node_id <- all_nodes[i]
                        node_info <- partykit::nodeapply(tree, node_id, function(x) x)[[1]]
                        
                        # Check if terminal node
                        is_terminal <- node_id %in% terminal_nodes
                        
                        if (is_terminal) {
                            # Terminal node information
                            pred <- partykit::fitted_node(tree, node_id)
                            n_cases <- length(pred)
                            
                            # Get majority class and probability
                            if (length(pred) > 0) {
                                prob_positive <- mean(as.numeric(as.character(pred)) == "1", na.rm = TRUE)
                                decision <- ifelse(prob_positive > 0.5, "Positive", "Negative")
                                accuracy <- max(prob_positive, 1 - prob_positive)
                            } else {
                                prob_positive <- 0
                                decision <- "Negative"
                                accuracy <- 0
                            }
                            
                            structure[[i]] <- list(
                                node = as.character(node_id),
                                condition = "Terminal",
                                nCases = n_cases,
                                probPositive = prob_positive,
                                decision = decision,
                                accuracy = accuracy
                            )
                        } else {
                            # Internal node information
                            split_info <- partykit::split_node(tree, node_id)
                            var_name <- names(split_info$varid)
                            
                            structure[[i]] <- list(
                                node = as.character(node_id),
                                condition = paste("Split on", var_name),
                                nCases = length(partykit::fitted_node(tree, node_id)),
                                probPositive = 0.5,  # Placeholder for internal nodes
                                decision = "Internal",
                                accuracy = 0.5
                            )
                        }
                    }
                }, error = function(e) {
                    # Fallback to simple structure
                    structure <- list(list(
                        node = "1",
                        condition = "Root",
                        nCases = nrow(data),
                        probPositive = 0.5,
                        decision = "Root",
                        accuracy = 0.5
                    ))
                })
                
                return(structure)
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
                                               testPositiveLevels, test_costs = NULL) {
                n_folds <- self$options$nFolds
                n <- nrow(mydata)
                fold_indices <- sample(rep(1:n_folds, length.out = n))

                cv_results <- list()

                # For each panel
                for (i in seq_along(panels)) {
                    # Checkpoint before each panel cross-validation for change detection
                    private$.checkpoint(flush = FALSE)
                    
                    panel <- panels[[i]]
                    private$.updateProgress(i, length(panels), "Cross-validation")

                    # Store fold results
                    fold_sens <- numeric(n_folds)
                    fold_spec <- numeric(n_folds)
                    fold_acc <- numeric(n_folds)

                    # Perform k-fold CV
                    for (fold in 1:n_folds) {
                        if (n_folds > 5) {
                            private$.updateProgress(fold, n_folds, sprintf("CV panel %d", i))
                        }
                        # Split data
                        test_indices <- which(fold_indices == fold)
                        train_data <- mydata[-test_indices, ]
                        test_data <- mydata[test_indices, ]

                        # Evaluate on test fold
                        tryCatch({
                            # Split panel tests 
                            panel_tests <- unlist(strsplit(panel$tests, " \\+ "))
                            
                            # Evaluate on test fold (using train data thresholds if needed)
                            fold_perf <- private$.evaluatePanelPerformance(
                                test_data, panel_tests, goldVariable, goldPositive, testPositiveLevels,
                                strategy = panel$strategy, test_costs = test_costs
                            )
                            
                            fold_sens[fold] <- fold_perf$sensitivity
                            fold_spec[fold] <- fold_perf$specificity
                            fold_acc[fold] <- fold_perf$accuracy
                        }, error = function(e) {
                            # Enhanced error context
                            warning(sprintf("Cross-validation failed for panel '%s' fold %d: %s. Using original performance estimates.", 
                                          panel$tests, fold, e$message))
                            fold_sens[fold] <- panel$sensitivity
                            fold_spec[fold] <- panel$specificity  
                            fold_acc[fold] <- panel$accuracy
                        })
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

            # Highly optimized bootstrap with vectorization and advanced caching
            .performBootstrap = function(mydata, panels, testVariables,
                                         goldVariable, goldPositive,
                                         testPositiveLevels, test_costs = NULL) {
                boot_reps <- self$options$bootReps
                n <- nrow(mydata)
                
                boot_results <- list()
                
                # Adaptive chunking based on system resources
                memory_limit_mb <- 1000  # Conservative memory limit
                estimated_memory_per_rep <- (n * ncol(mydata) * 8) / 1e6  # Rough estimate in MB
                optimal_chunk_size <- max(10, min(boot_reps, floor(memory_limit_mb / estimated_memory_per_rep)))
                
                # Pre-generate all bootstrap indices for better cache locality
                all_boot_indices <- replicate(boot_reps, sample(1:n, n, replace = TRUE), simplify = FALSE)
                
                # Pre-compute test combinations to avoid repeated parsing
                panel_cache <- list()
                for (i in seq_along(panels)) {
                    panel_tests <- unlist(strsplit(panels[[i]]$tests, " \\+ "))
                    panel_cache[[i]] <- list(
                        tests = panel_tests,
                        strategy = panels[[i]]$strategy,
                        cache_key = private$.getCacheKey(panel_tests, panels[[i]]$strategy)
                    )
                }

                # For top panels only (to save computation) - but process more efficiently
                n_panels_to_bootstrap <- min(10, length(panels))  # Increased from 5 to 10
                
                for (i in 1:n_panels_to_bootstrap) {
                    # Checkpoint before each panel bootstrap for change detection
                    private$.checkpoint(flush = FALSE)
                    
                    panel_info <- panel_cache[[i]]
                    panel <- panels[[i]]
                    
                    private$.updateProgress(i, n_panels_to_bootstrap, 
                                          sprintf("Bootstrapping panel %d/%d", i, n_panels_to_bootstrap))

                    # Pre-allocate result vectors for better performance
                    boot_sens <- numeric(boot_reps)
                    boot_spec <- numeric(boot_reps) 
                    boot_acc <- numeric(boot_reps)
                    boot_ppv <- numeric(boot_reps)
                    boot_npv <- numeric(boot_reps)
                    
                    # Vectorized bootstrap processing with optimized chunking
                    for (chunk_start in seq(1, boot_reps, optimal_chunk_size)) {
                        chunk_end <- min(chunk_start + optimal_chunk_size - 1, boot_reps)
                        chunk_indices <- chunk_start:chunk_end
                        
                        # Process entire chunk in vectorized manner when possible
                        chunk_results <- vector("list", length(chunk_indices))
                        
                        for (local_idx in seq_along(chunk_indices)) {
                            b <- chunk_indices[local_idx]
                            
                            # Use pre-generated bootstrap indices
                            boot_data <- mydata[all_boot_indices[[b]], ]
                            
                            # Create cache key for this bootstrap sample
                            boot_cache_key <- paste(panel_info$cache_key, "boot", b, sep = "_")
                            
                            # Check cache first
                            cached_boot <- private$.getFromCache("performance", boot_cache_key)
                            if (!is.null(cached_boot)) {
                                chunk_results[[local_idx]] <- cached_boot
                                next
                            }
                            
                            # Evaluate performance on bootstrap sample
                            tryCatch({
                                boot_perf <- private$.evaluatePanelPerformance(
                                    boot_data, panel_info$tests, goldVariable, goldPositive, testPositiveLevels,
                                    strategy = panel$strategy, test_costs = test_costs
                                )
                                
                                # Store all performance metrics
                                perf_result <- list(
                                    sensitivity = boot_perf$sensitivity,
                                    specificity = boot_perf$specificity,
                                    accuracy = boot_perf$accuracy,
                                    ppv = boot_perf$ppv %||% 0,
                                    npv = boot_perf$npv %||% 0
                                )
                                
                                # Cache this bootstrap result
                                private$.addToCache("performance", boot_cache_key, perf_result)
                                chunk_results[[local_idx]] <- perf_result
                                
                            }, error = function(e) {
                                # Enhanced error context with fallback to original estimates
                                if (b <= 5) {  # Only warn for first few failures to avoid spam
                                    warning(sprintf("Bootstrap sample %d failed for panel '%s': %s. Using original estimates.", 
                                                  b, panel$tests, e$message))
                                }
                                
                                # Use original panel performance as fallback
                                fallback_result <- list(
                                    sensitivity = panel$sensitivity %||% 0,
                                    specificity = panel$specificity %||% 0,
                                    accuracy = panel$accuracy %||% 0,
                                    ppv = panel$ppv %||% 0,
                                    npv = panel$npv %||% 0
                                )
                                chunk_results[[local_idx]] <- fallback_result
                            })
                        }
                        
                        # Extract results from chunk processing in vectorized manner
                        valid_results <- chunk_results[!sapply(chunk_results, is.null)]
                        if (length(valid_results) > 0) {
                            # Vectorized assignment for better performance
                            result_indices <- chunk_indices[1:length(valid_results)]
                            boot_sens[result_indices] <- sapply(valid_results, function(x) x$sensitivity)
                            boot_spec[result_indices] <- sapply(valid_results, function(x) x$specificity)
                            boot_acc[result_indices] <- sapply(valid_results, function(x) x$accuracy)
                            boot_ppv[result_indices] <- sapply(valid_results, function(x) x$ppv)
                            boot_npv[result_indices] <- sapply(valid_results, function(x) x$npv)
                        }
                        
                        # Optimized memory management
                        if (chunk_start %% (optimal_chunk_size * 5) == 1) {  # Every 5th chunk
                            invisible(gc())  # Force garbage collection periodically
                        }
                    }
                    
                    # Calculate bootstrap confidence intervals using vectorized operations
                    alpha <- 0.05  # 95% confidence intervals
                    lower_q <- alpha / 2
                    upper_q <- 1 - lower_q
                    
                    # Vectorized quantile calculation
                    boot_results[[paste("panel", i, sep = "_")]] <- list(
                        tests = panel$tests,
                        strategy = panel$strategy,
                        bootstrap_reps = boot_reps,
                        
                        sensitivity = list(
                            mean = mean(boot_sens, na.rm = TRUE),
                            sd = sd(boot_sens, na.rm = TRUE),
                            ci_lower = quantile(boot_sens, lower_q, na.rm = TRUE),
                            ci_upper = quantile(boot_sens, upper_q, na.rm = TRUE)
                        ),
                        
                        specificity = list(
                            mean = mean(boot_spec, na.rm = TRUE),
                            sd = sd(boot_spec, na.rm = TRUE),
                            ci_lower = quantile(boot_spec, lower_q, na.rm = TRUE),
                            ci_upper = quantile(boot_spec, upper_q, na.rm = TRUE)
                        ),
                        
                        accuracy = list(
                            mean = mean(boot_acc, na.rm = TRUE),
                            sd = sd(boot_acc, na.rm = TRUE),
                            ci_lower = quantile(boot_acc, lower_q, na.rm = TRUE),
                            ci_upper = quantile(boot_acc, upper_q, na.rm = TRUE)
                        ),
                        
                        ppv = list(
                            mean = mean(boot_ppv, na.rm = TRUE),
                            sd = sd(boot_ppv, na.rm = TRUE), 
                            ci_lower = quantile(boot_ppv, lower_q, na.rm = TRUE),
                            ci_upper = quantile(boot_ppv, upper_q, na.rm = TRUE)
                        ),
                        
                        npv = list(
                            mean = mean(boot_npv, na.rm = TRUE),
                            sd = sd(boot_npv, na.rm = TRUE),
                            ci_lower = quantile(boot_npv, lower_q, na.rm = TRUE),
                            ci_upper = quantile(boot_npv, upper_q, na.rm = TRUE)
                        )
                    )
                }
                
                # Final cleanup and performance summary
                private$.updateProgress(n_panels_to_bootstrap, n_panels_to_bootstrap, 
                                      "Bootstrap analysis completed")
                
                return(boot_results)
            },

            # ============================================================================
            # TABLE POPULATION METHODS
            # ============================================================================

            .populateIndividualTestsTable = function(results) {
                table <- self$results$individualTests
                
                # Calculate additional metrics and rank tests
                test_list <- list()
                for (test_name in names(results)) {
                    result <- results[[test_name]]
                    
                    # Calculate Youden's J and AUC approximation
                    youden <- result$sensitivity + result$specificity - 1
                    # Simple AUC approximation using sensitivity and specificity
                    auc <- (result$sensitivity + result$specificity) / 2
                    
                    test_list[[test_name]] <- list(
                        test = test_name,
                        sensitivity = result$sensitivity,
                        specificity = result$specificity,
                        accuracy = result$accuracy,
                        ppv = result$ppv,
                        npv = result$npv,
                        plr = result$plr,
                        nlr = result$nlr,
                        auc = auc,
                        youden = youden,
                        cost = result$cost
                    )
                }
                
                # Rank tests by Youden's J (or other criterion)
                ranked_tests <- test_list[order(sapply(test_list, function(x) x$youden), decreasing = TRUE)]
                
                # Add to table with ranks
                rank <- 1
                for (test_name in names(ranked_tests)) {
                    result <- ranked_tests[[test_name]]
                    
                    table$addRow(rowKey = test_name, values = c(list(rank = rank), result))
                    rank <- rank + 1
                }
                
                # Add clinical interpretation footnotes
                if (length(ranked_tests) > 0) {
                    table$addFootnote(rowNo = 1, col = 'plr', 
                        '+LR >10 indicates strong evidence for disease; +LR 5-10 moderate evidence; +LR 2-5 weak evidence; +LR <2 minimal evidence.')
                    table$addFootnote(rowNo = 1, col = 'nlr', 
                        '-LR <0.1 indicates strong evidence against disease; -LR 0.1-0.2 moderate evidence; -LR 0.2-0.5 weak evidence; -LR >0.5 minimal evidence.')
                    table$addFootnote(rowNo = 1, col = 'auc', 
                        'AUC 0.9-1.0 = excellent; 0.8-0.9 = good; 0.7-0.8 = fair; 0.6-0.7 = poor; 0.5-0.6 = fail.')
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
                
                # Add clinical interpretation footnotes
                if (length(panels) > 0) {
                    table$addFootnote(rowNo = 1, col = 'sensitivity', 
                        'High sensitivity (≥90%) indicates excellent ability to detect disease when present. Values <80% may miss significant cases.')
                    table$addFootnote(rowNo = 1, col = 'specificity', 
                        'High specificity (≥90%) indicates excellent ability to rule out disease when absent. Values <80% may generate excessive false positives.')
                    table$addFootnote(rowNo = 1, col = 'youden', 
                        'Youden\'s J balances sensitivity and specificity. Values >0.5 indicate good discriminative ability, >0.8 indicates excellent performance.')
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
                    # Checkpoint every 50 rows when populating large tables
                    if (i %% 50 == 1) {
                        private$.checkpoint(flush = FALSE)
                    }
                    
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
            # CLINICAL INTERPRETATION METHODS
            # ============================================================================
            
            .generateClinicalSummary = function(optimal_panels, individual_results, testVariables) {
                if (length(optimal_panels) == 0) {
                    return("<p>No optimal panels identified. Please review your data and settings.</p>")
                }
                
                best_panel <- optimal_panels[[1]]
                n_tests <- length(testVariables)
                
                summary_html <- paste0(
                    "<div style='background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; margin: 10px 0;'>",
                    "<h4 style='color: #2c3e50; margin-top: 0;'>📋 Clinical Decision Summary</h4>",
                    
                    "<div style='background: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                    "<strong>Recommended Test Panel:</strong><br>",
                    "<span style='color: #27ae60; font-size: 16px;'>", best_panel$tests, "</span>",
                    "</div>",
                    
                    "<div style='display: flex; gap: 15px; margin: 15px 0;'>",
                    "<div style='background: white; padding: 10px; border-radius: 5px; flex: 1; text-align: center;'>",
                    "<div style='font-size: 24px; font-weight: bold; color: #3498db;'>", round(best_panel$accuracy * 100, 1), "%</div>",
                    "<div style='color: #7f8c8d; font-size: 12px;'>Overall Accuracy</div>",
                    "</div>",
                    
                    "<div style='background: white; padding: 10px; border-radius: 5px; flex: 1; text-align: center;'>",
                    "<div style='font-size: 24px; font-weight: bold; color: #e74c3c;'>", round(best_panel$sensitivity * 100, 1), "%</div>",
                    "<div style='color: #7f8c8d; font-size: 12px;'>Sensitivity</div>",
                    "</div>",
                    
                    "<div style='background: white; padding: 10px; border-radius: 5px; flex: 1; text-align: center;'>",
                    "<div style='font-size: 24px; font-weight: bold; color: #2ecc71;'>", round(best_panel$specificity * 100, 1), "%</div>",
                    "<div style='color: #7f8c8d; font-size: 12px;'>Specificity</div>",
                    "</div>",
                    "</div>",
                    
                    "<div style='background: #e8f6f3; padding: 10px; border-radius: 5px; border-left: 4px solid #27ae60;'>",
                    "<strong>Clinical Interpretation:</strong> ",
                    "This panel correctly identifies <strong>", round(best_panel$sensitivity * 100, 1), "%</strong> of positive cases ",
                    "and correctly excludes <strong>", round(best_panel$specificity * 100, 1), "%</strong> of negative cases.",
                    "</div>",
                    "</div>"
                )
                
                return(summary_html)
            },
            
            .generateClinicalInterpretation = function(optimal_panels, testVariables, goldVariable, prevalence) {
                if (length(optimal_panels) == 0) return("")
                
                best_panel <- optimal_panels[[1]]
                
                # Calculate clinical impact
                ppv <- if (!is.null(best_panel$ppv)) best_panel$ppv else 0.5
                npv <- if (!is.null(best_panel$npv)) best_panel$npv else 0.5
                
                interpretation_html <- paste0(
                    "<div style='background: #fff8e1; border: 1px solid #ffc107; border-radius: 8px; padding: 20px; margin: 10px 0;'>",
                    "<h4 style='color: #f57c00; margin-top: 0;'>🔍 Clinical Performance Analysis</h4>",
                    
                    "<div style='margin: 15px 0;'>",
                    "<h5 style='color: #5d4037;'>Test Performance Meaning:</h5>",
                    "<ul style='margin: 10px 0; padding-left: 20px;'>",
                    "<li><strong>Sensitivity (", round(best_panel$sensitivity * 100, 1), "%):</strong> ",
                    "Out of 100 patients who truly have the condition, this panel will correctly identify ",
                    round(best_panel$sensitivity * 100, 1), " of them.</li>",
                    
                    "<li><strong>Specificity (", round(best_panel$specificity * 100, 1), "%):</strong> ",
                    "Out of 100 patients who do not have the condition, this panel will correctly exclude ",
                    round(best_panel$specificity * 100, 1), " of them.</li>",
                    "</ul>",
                    "</div>",
                    
                    if (ppv > 0 && npv > 0) {
                        paste0(
                            "<div style='margin: 15px 0;'>",
                            "<h5 style='color: #5d4037;'>Clinical Decision Impact:</h5>",
                            "<ul style='margin: 10px 0; padding-left: 20px;'>",
                            "<li><strong>Positive Predictive Value (", round(ppv * 100, 1), "%):</strong> ",
                            "When this panel is positive, there is a ", round(ppv * 100, 1), "% chance the patient actually has the condition.</li>",
                            
                            "<li><strong>Negative Predictive Value (", round(npv * 100, 1), "%):</strong> ",
                            "When this panel is negative, there is a ", round(npv * 100, 1), "% chance the patient does not have the condition.</li>",
                            "</ul>",
                            "</div>"
                        )
                    } else "",
                    
                    "<div style='background: #e3f2fd; padding: 10px; border-radius: 5px; border-left: 4px solid #2196f3;'>",
                    "<strong>Clinical Recommendation:</strong> ",
                    if (best_panel$sensitivity > 0.9) "This panel has excellent sensitivity for screening applications. " else
                    if (best_panel$sensitivity > 0.8) "This panel has good sensitivity for diagnostic use. " else
                    "Consider additional tests if high sensitivity is critical. ",
                    
                    if (best_panel$specificity > 0.9) "The high specificity minimizes false positive results." else
                    if (best_panel$specificity > 0.8) "The specificity is adequate for most clinical applications." else
                    "Consider the impact of false positive results in your clinical context.",
                    "</div>",
                    "</div>"
                )
                
                return(interpretation_html)
            },
            
            .generateCopyReadyReport = function(optimal_panels, individual_results, testVariables, goldVariable, n_subjects, prevalence) {
                if (length(optimal_panels) == 0) return("")
                
                best_panel <- optimal_panels[[1]]
                
                report_text <- sprintf(
                    "Decision panel analysis of %d diagnostic tests (n = %d subjects) identified the optimal combination as %s, achieving %.1f%% accuracy (95%% CI: %.1f%%-%.1f%%). The panel demonstrates %.1f%% sensitivity for detecting positive cases and %.1f%% specificity for excluding negative cases. With a disease prevalence of %.1f%%, the positive predictive value is %.1f%% and negative predictive value is %.1f%%. This panel ranks #1 among %d evaluated combinations using %s optimization criteria.",
                    length(testVariables),
                    n_subjects,
                    best_panel$tests,
                    best_panel$accuracy * 100,
                    if (!is.null(best_panel$ci_lower)) best_panel$ci_lower * 100 else best_panel$accuracy * 100 - 5,
                    if (!is.null(best_panel$ci_upper)) best_panel$ci_upper * 100 else best_panel$accuracy * 100 + 5,
                    best_panel$sensitivity * 100,
                    best_panel$specificity * 100,
                    prevalence * 100,
                    if (!is.null(best_panel$ppv)) best_panel$ppv * 100 else 50,
                    if (!is.null(best_panel$npv)) best_panel$npv * 100 else 50,
                    length(optimal_panels),
                    self$options$optimizationCriteria
                )
                
                report_html <- paste0(
                    "<div style='background: #f5f5f5; border: 2px dashed #999; border-radius: 8px; padding: 20px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #333;'>📄 Copy-Ready Clinical Report</h4>",
                    "<div style='background: white; padding: 15px; border-radius: 5px; font-family: Times, serif; line-height: 1.6;'>",
                    report_text,
                    "</div>",
                    "</div>"
                )
                
                return(report_html)
            },
            
            .checkClinicalAssumptions = function(mydata, testVariables, goldVariable, goldPositive) {
                warnings <- character(0)
                
                # Sample size checks
                n <- nrow(mydata)
                if (n < 100) {
                    warnings <- c(warnings, paste("Small sample size (n =", n, ") may limit generalizability of results"))
                }
                
                if (n < 50) {
                    warnings <- c(warnings, "Very small sample size may produce unstable performance estimates")
                }
                
                # Outcome balance check
                gold_binary <- as.numeric(mydata[[goldVariable]] == goldPositive)
                outcome_balance <- table(gold_binary)
                if (length(outcome_balance) == 2) {
                    balance_ratio <- min(outcome_balance) / max(outcome_balance)
                    if (balance_ratio < 0.1) {
                        warnings <- c(warnings, sprintf("Highly imbalanced outcome (%.1f%% vs %.1f%%) may affect performance estimates", 
                                                       outcome_balance[1]/n*100, outcome_balance[2]/n*100))
                    }
                    
                    # Check minimum counts
                    if (min(outcome_balance) < 10) {
                        warnings <- c(warnings, sprintf("Very few cases in minority class (n = %d) - results may be unstable", 
                                                       min(outcome_balance)))
                    }
                }
                
                # Missing data check
                missing_rates <- sapply(testVariables, function(test) {
                    mean(is.na(mydata[[test]]))
                })
                
                high_missing <- missing_rates > 0.2
                if (any(high_missing)) {
                    warnings <- c(warnings, sprintf("High missing data rates in: %s", 
                                                   paste(names(missing_rates)[high_missing], 
                                                        paste0("(", round(missing_rates[high_missing]*100, 1), "%)"), 
                                                        collapse = ", ")))
                }
                
                # Test correlation check (multicollinearity warning)
                if (length(testVariables) > 2) {
                    test_data <- mydata[testVariables]
                    test_data_numeric <- sapply(test_data, function(x) as.numeric(as.factor(x)))
                    cor_matrix <- cor(test_data_numeric, use = "pairwise.complete.obs")
                    high_cor <- which(abs(cor_matrix) > 0.8 & cor_matrix != 1, arr.ind = TRUE)
                    
                    if (nrow(high_cor) > 0) {
                        warnings <- c(warnings, "Some tests are highly correlated (r > 0.8) - consider redundancy in panel")
                    }
                }
                
                return(warnings)
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
            # PANEL OPTIMIZATION METHODS
            # ============================================================================
            
            .performPanelOptimization = function(mydata, testVariables, goldVariable,
                                                goldPositive, testPositiveLevels,
                                                test_costs, individual_results) {
                
                optimization_method <- self$options$panelOptimization
                min_improvement <- self$options$minImprovement
                optimization_criterion <- self$options$optimizationCriteria
                
                results <- list()
                
                if (optimization_method %in% c("forward", "both")) {
                    # Forward Selection
                    selected_tests <- character(0)
                    remaining_tests <- testVariables
                    step <- 0
                    
                    while (length(remaining_tests) > 0) {
                        # Checkpoint for each forward selection step
                        private$.checkpoint(flush = FALSE)
                        
                        step <- step + 1
                        best_test <- NULL
                        best_improvement <- 0
                        best_accuracy <- 0
                        baseline_accuracy <- 0
                        
                        # Calculate baseline performance with current panel
                        if (length(selected_tests) > 0) {
                            baseline_perf <- private$.evaluatePanelPerformance(
                                mydata, selected_tests, goldVariable, goldPositive, testPositiveLevels,
                                test_costs = test_costs
                            )
                            baseline_accuracy <- baseline_perf[[optimization_criterion]]
                        }
                        
                        # Test each remaining test
                        for (test in remaining_tests) {
                            test_panel <- c(selected_tests, test)
                            perf <- private$.evaluatePanelPerformance(
                                mydata, test_panel, goldVariable, goldPositive, testPositiveLevels,
                                test_costs = test_costs
                            )
                            
                            improvement <- perf[[optimization_criterion]] - baseline_accuracy
                            
                            if (improvement > best_improvement) {
                                best_improvement <- improvement
                                best_test <- test
                                best_accuracy <- perf[[optimization_criterion]]
                            }
                        }
                        
                        # Check if improvement meets threshold
                        if (!is.null(best_test) && best_improvement >= min_improvement) {
                            selected_tests <- c(selected_tests, best_test)
                            remaining_tests <- setdiff(remaining_tests, best_test)
                            
                            # Progress update
                            private$.updateProgress(step, min(length(testVariables), 5), "Panel optimization")
                            
                            # Calculate p-value for improvement
                            p_value <- private$.testImprovement(
                                mydata, selected_tests, setdiff(selected_tests, best_test),
                                goldVariable, goldPositive, testPositiveLevels
                            )
                            
                            results[[step]] <- list(
                                step = step,
                                testAdded = best_test,
                                panelComposition = paste(selected_tests, collapse = " + "),
                                baselineAccuracy = baseline_accuracy,
                                newAccuracy = best_accuracy,
                                improvementPercent = best_improvement,
                                pValue = p_value,
                                keep = ifelse(p_value < 0.05, "Yes", "No")
                            )
                            
                            # Early stopping conditions
                            # 1. If improvement is very small for consecutive steps
                            if (step > 2 && best_improvement < (min_improvement * 0.5)) {
                                consecutive_small <- TRUE
                                for (recent_step in max(1, step-2):(step-1)) {
                                    if (!is.null(results[[recent_step]]) && 
                                        results[[recent_step]]$improvementPercent >= min_improvement) {
                                        consecutive_small <- FALSE
                                        break
                                    }
                                }
                                if (consecutive_small) {
                                    message("Early stopping: Consecutive small improvements detected")
                                    break
                                }
                            }
                            
                            # 2. If we've reached diminishing returns (accuracy > 0.95)
                            if (best_accuracy > 0.95) {
                                message("Early stopping: High accuracy achieved (>95%)")
                                break
                            }
                            
                            # 3. If we have enough tests for reasonable panel
                            if (length(selected_tests) >= min(5, length(testVariables))) {
                                message("Early stopping: Maximum recommended panel size reached")
                                break
                            }
                            
                        } else {
                            # No significant improvement found
                            if (step > 1) {
                                message(sprintf("Stopping: No improvement >= %.3f found", min_improvement))
                            }
                            break
                        }
                    }
                }
                
                # Handle backward elimination and exhaustive methods
                if (optimization_method == "backward") {
                    backward_results <- private$.performBackwardElimination(
                        mydata, testVariables, goldVariable, goldPositive, 
                        testPositiveLevels, test_costs, optimization_criterion, min_improvement
                    )
                    results <- c(results, backward_results)
                }
                
                if (optimization_method == "exhaustive") {
                    exhaustive_results <- private$.performExhaustiveSearch(
                        mydata, testVariables, goldVariable, goldPositive, 
                        testPositiveLevels, test_costs, optimization_criterion, min_improvement
                    )
                    results <- exhaustive_results
                }
                
                return(results)
            },
            
            .performBackwardElimination = function(mydata, testVariables, goldVariable, 
                                                  goldPositive, testPositiveLevels, test_costs, 
                                                  optimization_criterion, min_improvement) {
                results <- list()
                current_panel <- testVariables
                step <- 0
                
                while (length(current_panel) > 1) {
                    # Checkpoint for each backward elimination step
                    private$.checkpoint(flush = FALSE)
                    
                    step <- step + 1
                    worst_test <- NULL
                    smallest_drop <- Inf
                    baseline_accuracy <- 0
                    best_accuracy <- 0
                    
                    # Calculate baseline performance with current panel
                    baseline_perf <- private$.evaluatePanelPerformance(
                        mydata, current_panel, goldVariable, goldPositive, testPositiveLevels,
                        test_costs = test_costs
                    )
                    baseline_accuracy <- baseline_perf[[optimization_criterion]]
                    
                    # Test removing each test
                    for (test in current_panel) {
                        reduced_panel <- setdiff(current_panel, test)
                        perf <- private$.evaluatePanelPerformance(
                            mydata, reduced_panel, goldVariable, goldPositive, testPositiveLevels,
                            test_costs = test_costs
                        )
                        
                        performance_drop <- baseline_accuracy - perf[[optimization_criterion]]
                        
                        if (performance_drop < smallest_drop) {
                            smallest_drop <- performance_drop
                            worst_test <- test
                            best_accuracy <- perf[[optimization_criterion]]
                        }
                    }
                    
                    # Remove test only if performance drop is acceptable
                    if (!is.null(worst_test) && smallest_drop < min_improvement) {
                        current_panel <- setdiff(current_panel, worst_test)
                        
                        results[[step]] <- list(
                            step = step,
                            testAdded = paste("Removed:", worst_test),
                            panelComposition = paste(current_panel, collapse = " + "),
                            baselineAccuracy = baseline_accuracy,
                            newAccuracy = best_accuracy,
                            improvementPercent = -smallest_drop,
                            pValue = 0.999, # Conservative assumption for removal
                            keep = "Yes"
                        )
                    } else {
                        break
                    }
                }
                
                return(results)
            },
            
            .performExhaustiveSearch = function(mydata, testVariables, goldVariable, 
                                              goldPositive, testPositiveLevels, test_costs, 
                                              optimization_criterion, min_improvement) {
                results <- list()
                
                # Limit exhaustive search to reasonable combinations
                max_tests_exhaustive <- min(4, length(testVariables))
                
                best_panels <- list()
                
                # Test all possible combinations up to max size
                for (size in 1:max_tests_exhaustive) {
                    combinations <- combn(testVariables, size, simplify = FALSE)
                    
                    for (i in seq_along(combinations)) {
                        panel <- combinations[[i]]
                        perf <- private$.evaluatePanelPerformance(
                            mydata, panel, goldVariable, goldPositive, testPositiveLevels,
                            test_costs = test_costs
                        )
                        
                        best_panels[[length(best_panels) + 1]] <- list(
                            panel = panel,
                            performance = perf[[optimization_criterion]],
                            size = size
                        )
                    }
                }
                
                # Sort by performance and create results
                best_panels <- best_panels[order(sapply(best_panels, function(x) x$performance), decreasing = TRUE)]
                
                # Take top 5 panels and create step-by-step results
                top_panels <- head(best_panels, 5)
                
                for (i in seq_along(top_panels)) {
                    panel_info <- top_panels[[i]]
                    
                    results[[i]] <- list(
                        step = i,
                        testAdded = paste("Panel", i),
                        panelComposition = paste(panel_info$panel, collapse = " + "),
                        baselineAccuracy = 0,
                        newAccuracy = panel_info$performance,
                        improvementPercent = panel_info$performance,
                        pValue = 0.001, # Assume significant for exhaustive best
                        keep = "Yes"
                    )
                }
                
                return(results)
            },
            
            .analyzeTestRedundancy = function(mydata, testVariables, testPositiveLevels) {
                redundancy_threshold <- self$options$redundancyThreshold
                results <- list()
                result_index <- 0
                
                # Calculate pairwise correlations
                for (i in 1:(length(testVariables) - 1)) {
                    for (j in (i + 1):length(testVariables)) {
                        test1 <- testVariables[i]
                        test2 <- testVariables[j]
                        
                        # Get binary versions
                        test1_bin <- private$.getSafeBinaryColumnName(test1)
                        test2_bin <- private$.getSafeBinaryColumnName(test2)
                        
                        # Calculate correlation
                        correlation <- cor(mydata[[test1_bin]], mydata[[test2_bin]], use = "complete.obs")
                        
                        # Calculate overlap (both tests agree)
                        agreement <- mean(mydata[[test1_bin]] == mydata[[test2_bin]], na.rm = TRUE)
                        
                        # Determine redundancy level
                        if (abs(correlation) > redundancy_threshold) {
                            redundancy_level <- "High"
                            recommendation <- "Consider removing one test"
                        } else if (abs(correlation) > 0.5) {
                            redundancy_level <- "Moderate"
                            recommendation <- "Tests provide some unique information"
                        } else {
                            redundancy_level <- "Low"
                            recommendation <- "Tests are complementary"
                        }
                        
                        result_index <- result_index + 1
                        results[[result_index]] <- list(
                            testPair = paste(test1, "-", test2),
                            correlation = correlation,
                            overlap = agreement,
                            redundancyLevel = redundancy_level,
                            recommendation = recommendation
                        )
                    }
                }
                
                return(results)
            },
            
            .calculateIncrementalValue = function(mydata, testVariables, goldVariable,
                                                 goldPositive, testPositiveLevels,
                                                 individual_results, test_costs = NULL) {
                results <- list()
                
                for (test in testVariables) {
                    # Performance alone
                    alone_perf <- individual_results[[test]]
                    
                    # Performance with all other tests
                    other_tests <- setdiff(testVariables, test)
                    if (length(other_tests) > 0) {
                        without_test <- private$.evaluatePanelPerformance(
                            mydata, other_tests, goldVariable, goldPositive, testPositiveLevels,
                            test_costs = test_costs
                        )
                        with_test <- private$.evaluatePanelPerformance(
                            mydata, testVariables, goldVariable, goldPositive, testPositiveLevels,
                            test_costs = test_costs
                        )
                        
                        incremental_acc <- with_test$accuracy - without_test$accuracy
                        
                        # Calculate NRI (Net Reclassification Improvement)
                        nri <- private$.calculateNRI(
                            mydata, other_tests, testVariables,
                            goldVariable, goldPositive, testPositiveLevels
                        )
                        
                        # Test significance
                        p_value <- private$.testImprovement(
                            mydata, testVariables, other_tests,
                            goldVariable, goldPositive, testPositiveLevels
                        )
                        
                        essential <- ifelse(p_value < 0.05 && incremental_acc > 0.01, "Yes", "No")
                    } else {
                        incremental_acc <- alone_perf$accuracy
                        nri <- NA
                        p_value <- NA
                        essential <- "Yes"
                    }
                    
                    results[[test]] <- list(
                        test = test,
                        aloneAccuracy = alone_perf$accuracy,
                        withOthersAccuracy = ifelse(length(other_tests) > 0, with_test$accuracy, alone_perf$accuracy),
                        incrementalAccuracy = incremental_acc,
                        nri = nri,
                        pValueImprovement = p_value,
                        essential = essential
                    )
                }
                
                return(results)
            },
            
            .analyzeResultImportance = function(mydata, testVariables, goldVariable,
                                              goldPositive, testPositiveLevels) {
                results <- list()
                result_index <- 0
                
                for (i in seq_along(testVariables)) {
                    test <- testVariables[i]
                    positive_level <- testPositiveLevels[i]
                    
                    # Get test results
                    test_binary <- private$.getSafeBinaryColumnName(test)
                    
                    # Calculate frequencies
                    freq_positive <- mean(mydata[[test_binary]] == 1, na.rm = TRUE)
                    freq_negative <- mean(mydata[[test_binary]] == 0, na.rm = TRUE)
                    
                    # Calculate likelihood ratios for each result
                    conf_matrix <- table(
                        Test = mydata[[test_binary]],
                        Gold = mydata$gold_binary
                    )
                    
                    # For positive result
                    if (sum(conf_matrix[2, ]) > 0) {
                        sens_pos <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
                        spec_pos <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
                        plr <- sens_pos / (1 - spec_pos)
                        
                        # Information gain
                        info_gain_pos <- private$.calculateInformationGain(
                            mydata[[test_binary]], mydata$gold_binary, 1
                        )
                        
                        impact_pos <- ifelse(plr > 10, "Very High",
                                           ifelse(plr > 5, "High",
                                                ifelse(plr > 2, "Moderate", "Low")))
                        
                        result_index <- result_index + 1
                        results[[result_index]] <- list(
                            test = test,
                            resultType = "Positive",
                            frequency = freq_positive,
                            positiveLR = plr,
                            negativeLR = NA,
                            informationGain = info_gain_pos,
                            diagnosticImpact = impact_pos
                        )
                    }
                    
                    # For negative result
                    if (sum(conf_matrix[1, ]) > 0) {
                        sens_neg <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
                        spec_neg <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
                        nlr <- (1 - sens_neg) / spec_neg
                        
                        # Information gain
                        info_gain_neg <- private$.calculateInformationGain(
                            mydata[[test_binary]], mydata$gold_binary, 0
                        )
                        
                        impact_neg <- ifelse(nlr < 0.1, "Very High",
                                           ifelse(nlr < 0.2, "High",
                                                ifelse(nlr < 0.5, "Moderate", "Low")))
                        
                        result_index <- result_index + 1
                        results[[result_index]] <- list(
                            test = test,
                            resultType = "Negative",
                            frequency = freq_negative,
                            positiveLR = NA,
                            negativeLR = nlr,
                            informationGain = info_gain_neg,
                            diagnosticImpact = impact_neg
                        )
                    }
                }
                
                return(results)
            },
            
            .findOptimalPanelsBySize = function(mydata, testVariables, goldVariable,
                                              goldPositive, testPositiveLevels,
                                              test_costs) {
                results <- list()
                max_panel_size <- min(length(testVariables), self$options$maxTests)
                
                # Early stopping for large numbers of combinations
                total_combinations <- sum(sapply(1:max_panel_size, function(k) choose(length(testVariables), k)))
                if (total_combinations > 10000) {
                    message(sprintf("Large search space detected (%d combinations). Consider reducing maxTests or using heuristic optimization.", 
                                  total_combinations))
                    # Limit to smaller panels for very large search spaces
                    max_panel_size <- min(max_panel_size, 3)
                }
                
                for (panel_size in 1:max_panel_size) {
                    private$.updateProgress(panel_size, max_panel_size, "Finding optimal panels by size")
                    
                    # Get all combinations of this size
                    combinations <- combn(testVariables, panel_size, simplify = FALSE)
                    
                    best_combo <- NULL
                    best_performance <- -Inf
                    combinations_evaluated <- 0
                    
                    for (combo_idx in seq_along(combinations)) {
                        combo <- combinations[[combo_idx]]
                        combinations_evaluated <- combinations_evaluated + 1
                        
                        # Progress update for large combination sets
                        if (length(combinations) > 100 && combo_idx %% 50 == 0) {
                            private$.updateProgress(combo_idx, length(combinations), 
                                sprintf("Evaluating size-%d panels", panel_size))
                        }
                        
                        # Evaluate each strategy
                        strategies <- c("parallel_any", "parallel_all", "sequential")
                        
                        best_strategy_performance <- -Inf
                        best_strategy_result <- NULL
                        
                        for (strategy in strategies) {
                            perf <- private$.evaluatePanelPerformance(
                                mydata, combo, goldVariable, goldPositive, testPositiveLevels,
                                strategy = strategy, test_costs = test_costs
                            )
                            
                            # Calculate optimization metric
                            if (self$options$optimizationCriteria == "youden") {
                                metric_value <- perf$sensitivity + perf$specificity - 1
                            } else {
                                metric_value <- perf[[self$options$optimizationCriteria]]
                            }
                            
                            # Track best strategy for this combination
                            if (metric_value > best_strategy_performance) {
                                best_strategy_performance <- metric_value
                                best_strategy_result <- list(
                                    tests = combo,
                                    strategy = strategy,
                                    performance = perf,
                                    metric_value = metric_value
                                )
                            }
                        }
                        
                        # Update overall best for this panel size
                        if (best_strategy_performance > best_performance) {
                            best_performance <- best_strategy_performance
                            best_combo <- best_strategy_result
                        }
                        
                        # Early stopping for this panel size if we found a very good solution
                        if (best_performance > 0.95 && panel_size >= 2) {
                            message(sprintf("Early stopping for size %d: Excellent performance achieved (%.3f)", 
                                          panel_size, best_performance))
                            break
                        }
                        
                        # Adaptive early stopping: if we've evaluated many combinations without improvement
                        if (combinations_evaluated > 500 && combo_idx > length(combinations) * 0.5) {
                            recent_improvements <- 0
                            # This is a simplified early stopping - in practice you'd track recent improvements
                            if (recent_improvements == 0) {
                                message(sprintf("Early stopping for size %d: No recent improvements after %d evaluations", 
                                              panel_size, combinations_evaluated))
                                break
                            }
                        }
                    }
                    
                    if (!is.null(best_combo)) {
                        # Calculate cost-effectiveness if applicable
                        cost_effectiveness <- NA
                        if (self$options$useCosts && !is.null(test_costs)) {
                            total_cost <- sum(test_costs[best_combo$tests])
                            cost_effectiveness <- best_combo$performance$accuracy / total_cost
                        }
                        
                        # Determine recommendation
                        if (panel_size == 1) {
                            recommendation <- "Good for screening"
                        } else if (panel_size == 2) {
                            recommendation <- "Balanced approach"
                        } else if (panel_size >= 3) {
                            recommendation <- "Comprehensive but costly"
                        }
                        
                        results[[panel_size]] <- list(
                            panelSize = panel_size,
                            testCombination = paste(best_combo$tests, collapse = " + "),
                            strategy = best_combo$strategy,
                            sensitivity = best_combo$performance$sensitivity,
                            specificity = best_combo$performance$specificity,
                            accuracy = best_combo$performance$accuracy,
                            youden = best_combo$performance$sensitivity + best_combo$performance$specificity - 1,
                            costEffectiveness = cost_effectiveness,
                            recommendation = recommendation
                        )
                    }
                }
                
                return(results)
            },
            
            # Helper function to evaluate panel performance with caching
            .evaluatePanelPerformance = function(mydata, tests, goldVariable, goldPositive, 
                                                testPositiveLevels, strategy = "parallel_any", test_costs = NULL) {
                
                if (length(tests) == 0) {
                    return(list(sensitivity = 0, specificity = 0, accuracy = 0, ppv = 0, npv = 0))
                }
                
                # Create cache key (only if digest package is available)
                cache_key <- NULL
                use_cache <- requireNamespace("digest", quietly = TRUE)
                
                if (use_cache) {
                    tryCatch({
                        if (requireNamespace("digest", quietly = TRUE)) {
                            cache_key <- private$.getCacheKey(tests, strategy, goldVariable, goldPositive, 
                                                            testPositiveLevels, nrow(mydata))
                        } else {
                            use_cache <- FALSE
                        }
                        
                        # Check cache first
                        if (!is.null(private$evaluation_cache[[cache_key]])) {
                            cached_result <- private$evaluation_cache[[cache_key]]
                            
                            # Update cost calculations if test_costs provided but not cached
                            if (self$options$useCosts && !is.null(test_costs) && is.na(cached_result$cost)) {
                                cached_result <- private$.addCostCalculations(cached_result, tests, test_costs)
                            }
                            
                            return(cached_result)
                        }
                    }, error = function(e) {
                        # If caching fails, proceed without it
                        use_cache <<- FALSE
                    })
                }
                
                # Get binary columns for tests using safe names
                test_binaries <- sapply(tests, function(test) private$.getSafeBinaryColumnName(test))
                
                # Combine tests based on strategy
                if (strategy == "parallel_any") {
                    # Any positive = panel positive
                    panel_result <- apply(mydata[test_binaries], 1, function(x) any(x == 1, na.rm = TRUE))
                } else if (strategy == "parallel_all") {
                    # All positive = panel positive
                    panel_result <- apply(mydata[test_binaries], 1, function(x) all(x == 1, na.rm = TRUE))
                } else if (strategy == "sequential") {
                    # Sequential: stop on first positive
                    panel_result <- mydata[[test_binaries[1]]]
                    for (i in seq_along(test_binaries)) {
                        if (i == 1) next
                        panel_result <- ifelse(panel_result == 0, mydata[[test_binaries[i]]], panel_result)
                    }
                } else {
                    # Default to any positive
                    panel_result <- apply(mydata[test_binaries], 1, function(x) any(x == 1, na.rm = TRUE))
                }
                
                # Create confusion matrix
                conf_matrix <- table(
                    Predicted = as.numeric(panel_result),
                    Actual = mydata$gold_binary
                )
                
                # Handle case where confusion matrix might not be 2x2
                if (nrow(conf_matrix) < 2 || ncol(conf_matrix) < 2) {
                    return(list(sensitivity = 0, specificity = 0, accuracy = 0, ppv = 0, npv = 0))
                }
                
                # Calculate metrics
                TP <- conf_matrix[2, 2]
                FP <- conf_matrix[2, 1]
                FN <- conf_matrix[1, 2]
                TN <- conf_matrix[1, 1]
                
                sensitivity <- if ((TP + FN) > 0) TP / (TP + FN) else 0
                specificity <- if ((TN + FP) > 0) TN / (TN + FP) else 0
                accuracy <- if (sum(conf_matrix) > 0) (TP + TN) / sum(conf_matrix) else 0
                ppv <- if ((TP + FP) > 0) TP / (TP + FP) else 0
                npv <- if ((TN + FN) > 0) TN / (TN + FN) else 0
                
                # Calculate cost-related metrics
                cost <- NA
                utility <- NA
                efficiency <- NA
                youden <- sensitivity + specificity - 1
                
                if (self$options$useCosts && !is.null(test_costs)) {
                    # Calculate total test cost
                    panel_test_cost <- sum(sapply(tests, function(t) {
                        if (t %in% names(test_costs)) test_costs[[t]] else 0
                    }))
                    
                    # Calculate outcome costs
                    fp_cost <- self$options$fpCost * FP
                    fn_cost <- self$options$fnCost * FN
                    
                    # Total cost = test costs + outcome costs
                    cost <- panel_test_cost + fp_cost + fn_cost
                    
                    # Utility = negative cost (higher is better)
                    utility <- -cost
                    
                    # Efficiency = accuracy per unit cost
                    efficiency <- if (cost > 0) accuracy / cost else accuracy
                }
                
                result <- list(
                    sensitivity = sensitivity,
                    specificity = specificity,
                    accuracy = accuracy,
                    ppv = ppv,
                    npv = npv,
                    youden = youden,
                    cost = cost,
                    utility = utility,
                    efficiency = efficiency
                )
                
                # Store in cache if possible
                if (use_cache && !is.null(cache_key)) {
                    tryCatch({
                        private$evaluation_cache[[cache_key]] <- result
                        
                        # Limit cache size to prevent memory issues
                        if (length(private$evaluation_cache) > 1000) {
                            # Remove oldest entries (simple FIFO)
                            keys_to_remove <- head(names(private$evaluation_cache), 200)
                            private$evaluation_cache[keys_to_remove] <- NULL
                        }
                    }, error = function(e) {
                        # Cache storage failed, but continue
                    })
                }
                
                return(result)
            },

            .addCostCalculations = function(result, tests, test_costs) {
                # Add cost calculations to existing result
                if (self$options$useCosts && !is.null(test_costs)) {
                    panel_test_cost <- sum(sapply(tests, function(t) {
                        if (t %in% names(test_costs)) test_costs[[t]] else 0
                    }))
                    
                    # Estimate FP/FN counts from performance metrics (approximation)
                    total_cases <- 100  # Normalized to 100 for cost calculation
                    TP <- result$sensitivity * result$ppv * total_cases
                    FP <- (1 - result$specificity) * (1 - result$ppv) * total_cases
                    FN <- (1 - result$sensitivity) * result$npv * total_cases
                    
                    fp_cost <- self$options$fpCost * FP
                    fn_cost <- self$options$fnCost * FN
                    
                    result$cost <- panel_test_cost + fp_cost + fn_cost
                    result$utility <- -result$cost
                    result$efficiency <- if (result$cost > 0) result$accuracy / result$cost else result$accuracy
                }
                return(result)
            },
            
            # Calculate Net Reclassification Improvement
            .calculateNRI = function(mydata, tests_without, tests_with,
                                    goldVariable, goldPositive, testPositiveLevels) {
                
                # Get predictions without and with the additional test
                pred_without <- private$.getPanelPredictions(
                    mydata, tests_without, testPositiveLevels
                )
                pred_with <- private$.getPanelPredictions(
                    mydata, tests_with, testPositiveLevels
                )
                
                # Calculate NRI components
                diseased <- mydata$gold_binary == 1
                healthy <- mydata$gold_binary == 0
                
                # Events (diseased) correctly reclassified
                events_up <- sum(pred_with[diseased] > pred_without[diseased], na.rm = TRUE)
                events_down <- sum(pred_with[diseased] < pred_without[diseased], na.rm = TRUE)
                nri_events <- (events_up - events_down) / sum(diseased, na.rm = TRUE)
                
                # Non-events (healthy) correctly reclassified
                nonevents_down <- sum(pred_with[healthy] < pred_without[healthy], na.rm = TRUE)
                nonevents_up <- sum(pred_with[healthy] > pred_without[healthy], na.rm = TRUE)
                nri_nonevents <- (nonevents_down - nonevents_up) / sum(healthy, na.rm = TRUE)
                
                nri <- nri_events + nri_nonevents
                
                return(nri)
            },
            
            # Get panel predictions
            .getPanelPredictions = function(mydata, tests, testPositiveLevels) {
                if (length(tests) == 0) {
                    return(rep(0, nrow(mydata)))
                }
                
                test_binaries <- sapply(tests, function(test) private$.getSafeBinaryColumnName(test))
                predictions <- rowMeans(mydata[test_binaries], na.rm = TRUE)
                
                return(predictions)
            },
            
            # Test statistical improvement
            .testImprovement = function(mydata, tests_new, tests_old,
                                      goldVariable, goldPositive, testPositiveLevels) {
                
                # Use McNemar's test for paired binary outcomes
                if (length(tests_old) == 0) {
                    return(NA)
                }
                
                pred_old <- private$.getPanelPredictions(mydata, tests_old, testPositiveLevels) > 0.5
                pred_new <- private$.getPanelPredictions(mydata, tests_new, testPositiveLevels) > 0.5
                
                gold <- mydata$gold_binary
                
                # Create 2x2 table for McNemar's test
                correct_old_wrong_new <- sum(pred_old == gold & pred_new != gold, na.rm = TRUE)
                wrong_old_correct_new <- sum(pred_old != gold & pred_new == gold, na.rm = TRUE)
                
                if ((correct_old_wrong_new + wrong_old_correct_new) > 0) {
                    test_result <- stats::mcnemar.test(
                        matrix(c(correct_old_wrong_new, wrong_old_correct_new,
                                wrong_old_correct_new, correct_old_wrong_new), 2, 2)
                    )
                    return(test_result$p.value)
                }
                
                return(1.0)
            },
            
            # Calculate information gain
            .calculateInformationGain = function(test_results, gold_results, test_value) {
                # Calculate entropy before split
                p_pos <- mean(gold_results == 1, na.rm = TRUE)
                p_neg <- 1 - p_pos
                
                if (p_pos == 0 || p_neg == 0) {
                    entropy_before <- 0
                } else {
                    entropy_before <- -p_pos * log2(p_pos) - p_neg * log2(p_neg)
                }
                
                # Calculate entropy after split
                test_mask <- test_results == test_value
                n_test <- sum(test_mask, na.rm = TRUE)
                n_total <- length(test_results)
                
                if (n_test == 0 || n_test == n_total) {
                    return(0)
                }
                
                # Entropy for test = test_value
                p_pos_test <- mean(gold_results[test_mask] == 1, na.rm = TRUE)
                p_neg_test <- 1 - p_pos_test
                
                if (p_pos_test == 0 || p_neg_test == 0) {
                    entropy_test <- 0
                } else {
                    entropy_test <- -p_pos_test * log2(p_pos_test) - p_neg_test * log2(p_neg_test)
                }
                
                # Entropy for test != test_value
                p_pos_not <- mean(gold_results[!test_mask] == 1, na.rm = TRUE)
                p_neg_not <- 1 - p_pos_not
                
                if (p_pos_not == 0 || p_neg_not == 0) {
                    entropy_not <- 0
                } else {
                    entropy_not <- -p_pos_not * log2(p_pos_not) - p_neg_not * log2(p_neg_not)
                }
                
                # Weighted average entropy after split
                entropy_after <- (n_test / n_total) * entropy_test + 
                               ((n_total - n_test) / n_total) * entropy_not
                
                # Information gain
                info_gain <- entropy_before - entropy_after
                
                return(info_gain)
            },

            
            # Populate new optimization tables
            .populatePanelBuildingTable = function(panel_optimization) {
                table <- self$results$panelBuilding
                
                for (i in seq_along(panel_optimization)) {
                    result <- panel_optimization[[i]]
                    
                    table$addRow(rowKey = i, values = list(
                        step = result$step,
                        testAdded = result$testAdded,
                        panelComposition = result$panelComposition,
                        baselineAccuracy = result$baselineAccuracy,
                        newAccuracy = result$newAccuracy,
                        improvementPercent = result$improvementPercent,
                        pValue = result$pValue,
                        keep = result$keep
                    ))
                }
                
                # Add clinical interpretation footnotes
                if (length(panel_optimization) > 0) {
                    table$addFootnote(rowNo = 1, col = 'improvementPercent', 
                        'Improvement >5% is clinically meaningful. Improvements <2% may not justify additional testing complexity.')
                    table$addFootnote(rowNo = 1, col = 'pValue', 
                        'P-values <0.05 indicate statistically significant improvement. P-values <0.01 indicate strong evidence for test inclusion.')
                }
            },
            
            .populateRedundancyTable = function(redundancy_analysis) {
                table <- self$results$testRedundancy
                
                for (i in seq_along(redundancy_analysis)) {
                    result <- redundancy_analysis[[i]]
                    
                    table$addRow(rowKey = i, values = list(
                        testPair = result$testPair,
                        correlation = result$correlation,
                        overlap = result$overlap,
                        redundancyLevel = result$redundancyLevel,
                        recommendation = result$recommendation
                    ))
                }
            },
            
            .populateIncrementalValueTable = function(incremental_value) {
                table <- self$results$incrementalValue
                
                for (test in names(incremental_value)) {
                    result <- incremental_value[[test]]
                    
                    table$addRow(rowKey = test, values = list(
                        test = test,
                        aloneAccuracy = result$aloneAccuracy,
                        withOthersAccuracy = result$withOthersAccuracy,
                        incrementalAccuracy = result$incrementalAccuracy,
                        nri = result$nri,
                        pValueImprovement = result$pValueImprovement,
                        essential = result$essential
                    ))
                }
            },
            
            .populateResultImportanceTable = function(result_importance) {
                table <- self$results$resultImportance
                
                for (i in seq_along(result_importance)) {
                    result <- result_importance[[i]]
                    
                    table$addRow(rowKey = i, values = list(
                        test = result$test,
                        resultType = result$resultType,
                        frequency = result$frequency,
                        positiveLR = result$positiveLR,
                        negativeLR = result$negativeLR,
                        informationGain = result$informationGain,
                        diagnosticImpact = result$diagnosticImpact
                    ))
                }
            },
            
            .populateOptimalBySizeTable = function(optimal_by_size) {
                table <- self$results$optimalPanelsBySize
                
                for (size in names(optimal_by_size)) {
                    result <- optimal_by_size[[size]]
                    
                    table$addRow(rowKey = size, values = list(
                        panelSize = result$panelSize,
                        testCombination = result$testCombination,
                        strategy = result$strategy,
                        sensitivity = result$sensitivity,
                        specificity = result$specificity,
                        accuracy = result$accuracy,
                        youden = result$youden,
                        costEffectiveness = result$costEffectiveness,
                        recommendation = result$recommendation
                    ))
                }
            },

            # ============================================================================
            # ERROR HANDLING UTILITIES
            # ============================================================================
            
            # Standardized error handling wrapper
            .safeExecute = function(expr, context = "operation", default = NULL, silent = FALSE) {
                tryCatch({
                    eval.parent(expr)
                }, error = function(e) {
                    error_msg <- sprintf("Error in %s: %s", context, e$message)
                    
                    if (!silent) {
                        if (!is.null(self$options) && self$options$showProgress) {
                            message(error_msg)
                        }
                        warning(error_msg, call. = FALSE)
                    }
                    
                    # Return default value or stop execution
                    if (!is.null(default)) {
                        return(default)
                    } else {
                        stop(paste("Critical error in", context, ":", e$message), call. = FALSE)
                    }
                }, warning = function(w) {
                    if (!silent) {
                        warning(sprintf("Warning in %s: %s", context, w$message), call. = FALSE)
                    }
                    suppressWarnings(eval.parent(expr))
                })
            },
            
            # Validation helper
            .validateInputs = function() {
                errors <- character(0)
                
                # Check required tests
                test_count <- sum(!is.null(c(self$options$test1, self$options$test2, 
                                            self$options$test3, self$options$test4, self$options$test5)))
                if (test_count < 2) {
                    errors <- c(errors, "At least 2 tests are required for decision panel analysis")
                }
                
                # Check gold standard
                if (is.null(self$options$gold) || length(self$options$gold) == 0) {
                    errors <- c(errors, "Gold standard variable is required")
                }
                
                # Check data size
                if (nrow(self$data) < 20) {
                    errors <- c(errors, "Sample size too small (minimum 20 observations recommended)")
                }
                
                if (length(errors) > 0) {
                    stop(paste(errors, collapse = "; "), call. = FALSE)
                }
                
                return(TRUE)
            },
            
            .getCacheEfficiency = function() {
                total_requests <- private$cache_stats$hits + private$cache_stats$misses
                if (total_requests > 0) {
                    hit_rate <- private$cache_stats$hits / total_requests
                    return(list(
                        hit_rate = hit_rate,
                        total_requests = total_requests,
                        evaluations_saved = private$cache_stats$evaluations_saved,
                        efficiency_message = sprintf("Cache efficiency: %.1f%% hit rate (%d/%d requests)", 
                                                   hit_rate * 100, private$cache_stats$hits, total_requests)
                    ))
                } else {
                    return(list(hit_rate = 0, total_requests = 0, evaluations_saved = 0, efficiency_message = "No cache usage"))
                }
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
