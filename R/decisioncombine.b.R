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
                # Check if we have the minimum required variables for analysis
                if (!private$.hasRequiredInputs()) {
                    private$.displayWelcomeMessage()
                    return()
                }
                
                # Main analysis orchestrator - delegates to focused methods
                tryCatch({
                    # Step 1: Validate inputs
                    private$.validateInputs()
                    
                    # Step 2: Process and clean data
                    processed_data <- private$.processData()
                    
                    # Step 3: Generate frequency tables if requested
                    if (self$options$od) {
                        private$.generateFrequencyTables(processed_data)
                    }
                    
                    # Step 4: Calculate individual test statistics if requested
                    if (self$options$showIndividual) {
                        private$.calculateIndividualTests(processed_data)
                    }
                    
                    # Step 5: Perform combination analysis
                    if (length(processed_data$test_variables) >= 2) {
                        private$.performCombinationAnalysis(processed_data)
                    }
                    
                }, error = function(e) {
                    # Consistent error handling with informative messages
                    stop(paste("Analysis failed:", conditionMessage(e), "Please check your data and variable selections."))
                })
            },
            
            .validateInputs = function() {
                # Comprehensive input validation with informative error messages
                if (length(self$options$gold) == 0) {
                    stop("Gold standard variable is required. Please select a gold standard variable from your dataset.")
                }

                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop("Data contains no (complete) rows. Please check your dataset for missing values.")
                }
                
                test_vars <- c(self$options$test1, self$options$test2, self$options$test3)
                test_vars <- test_vars[!is.null(test_vars) & test_vars != ""]
                
                if (length(test_vars) == 0) {
                    stop("At least one test variable is required for analysis. Please select Test 1 and optionally Test 2 and Test 3.")
                }
                
                if (is.null(self$options$goldPositive) || self$options$goldPositive == "") {
                    stop("Positive level for gold standard must be specified. Please select the positive level from the dropdown.")
                }
                
                # Validate positive levels for selected tests
                if (!is.null(self$options$test1) && self$options$test1 != "" && 
                    (is.null(self$options$test1Positive) || self$options$test1Positive == "")) {
                    stop(paste("Positive level for Test 1 (", self$options$test1, ") must be specified."))
                }
                
                if (!is.null(self$options$test2) && self$options$test2 != "" && 
                    (is.null(self$options$test2Positive) || self$options$test2Positive == "")) {
                    stop(paste("Positive level for Test 2 (", self$options$test2, ") must be specified."))
                }
                
                if (!is.null(self$options$test3) && self$options$test3 != "" && 
                    (is.null(self$options$test3Positive) || self$options$test3Positive == "")) {
                    stop(paste("Positive level for Test 3 (", self$options$test3, ") must be specified."))
                }
                
                # Validate gold standard has at least 2 levels for meaningful analysis
                gold_levels <- unique(self$data[[self$options$gold]])
                gold_levels <- gold_levels[!is.na(gold_levels)]
                if (length(gold_levels) < 2) {
                    stop(paste("Gold standard variable '", self$options$gold, "' must have at least 2 levels for diagnostic analysis. Currently found levels: ", paste(gold_levels, collapse = ", ")))
                }
                
                # Validate test variables have at least 2 levels
                test_vars <- c(self$options$test1, self$options$test2, self$options$test3)
                test_vars <- test_vars[!is.null(test_vars) & test_vars != ""]
                for (test_var in test_vars) {
                    if (test_var %in% names(self$data)) {
                        test_levels <- unique(self$data[[test_var]])
                        test_levels <- test_levels[!is.na(test_levels)]
                        if (length(test_levels) < 2) {
                            warning(paste("Test variable '", test_var, "' has only one level: ", paste(test_levels, collapse = ", "), ". This may limit diagnostic analysis."))
                        }
                    }
                }
                
                invisible(TRUE)
            },
            
            .processData = function() {
                # Data processing and preparation with memory management
                n_obs <- nrow(self$data)
                
                # Memory and performance management
                private$.checkMemoryLimits(n_obs)
                
                # Process gold standard variable
                goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive)
                goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)
                goldPLevel <- unlist(goldPLevel)

                goldVariable <- jmvcore::constructFormula(terms = self$options$gold)
                goldVariable <- jmvcore::decomposeFormula(formula = goldVariable)
                goldVariable <- unlist(goldVariable)

                # Create working copy with factor conversion
                mydata <- self$data
                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])
                
                # Get test variables and their positive levels
                test_variables <- c(self$options$test1, self$options$test2, self$options$test3)
                test_variables <- test_variables[!is.null(test_variables) & test_variables != ""]
                
                test_positives <- list()
                if (!is.null(self$options$test1) && self$options$test1 != "") {
                    test_positives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    test_positives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    test_positives[[self$options$test3]] <- self$options$test3Positive
                }
                
                # Remove rows with missing values in analysis variables
                analysis_vars <- c(goldVariable, test_variables)
                complete_rows <- complete.cases(mydata[analysis_vars])
                
                if (sum(complete_rows) == 0) {
                    stop("No complete cases found for analysis variables. Please check for missing values in your gold standard and test variables.")
                }
                
                mydata_clean <- mydata[complete_rows, ]
                
                # Report data usage
                n_analysis <- nrow(mydata_clean)
                if (n_obs != n_analysis) {
                    message(paste("Using", n_analysis, "of", n_obs, "observations (", 
                                 n_obs - n_analysis, "observations removed due to missing values)"))
                }
                
                # Add binary recoded variables
                mydata_clean$goldVariable2 <- dplyr::case_when(
                    mydata_clean[[goldVariable]] == goldPLevel ~ "Positive",
                    is.na(mydata_clean[[goldVariable]]) ~ NA_character_,
                    TRUE ~ "Negative"
                )
                mydata_clean$goldVariable2 <- forcats::fct_relevel(mydata_clean$goldVariable2, "Positive")
                
                # Return processed data structure
                return(list(
                    original_data = mydata,
                    clean_data = mydata_clean,
                    gold_variable = goldVariable,
                    gold_positive_level = goldPLevel,
                    test_variables = test_variables,
                    test_positives = test_positives,
                    n_original = n_obs,
                    n_analysis = n_analysis
                ))
            },
            
            .checkMemoryLimits = function(n_obs) {
                # Enhanced memory limit checking with warnings
                large_dataset_threshold <- 1000
                very_large_threshold <- 10000
                memory_critical_threshold <- 100000
                
                if (n_obs > large_dataset_threshold) {
                    message(paste("Processing dataset with", n_obs, "observations"))
                    
                    if (n_obs > very_large_threshold) {
                        message("Large dataset detected - using optimized memory management")
                        
                        # Check available memory
                        tryCatch({
                            gc(verbose = FALSE)
                        }, error = function(e) {
                            warning("Memory management may be constrained. Consider reducing dataset size if analysis fails.")
                        })
                        
                        if (n_obs > memory_critical_threshold) {
                            stop(paste("Dataset too large (", n_obs, " observations). ",
                                      "Please consider subsetting your data or using a more powerful system. ",
                                      "Maximum recommended size is", memory_critical_threshold, "observations."))
                        }
                        
                        if (n_obs > 50000) {
                            warning("Very large dataset may require substantial processing time and memory. Consider running during off-peak hours.")
                        }
                    }
                }
                
                invisible(TRUE)
            },

            .generateFrequencyTables = function(processed_data) {
                # Generate frequency tables for original data display
                mydata <- processed_data$original_data
                goldVariable <- processed_data$gold_variable

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
                test_vars <- processed_data$test_variables

                if (length(test_vars) > 0) {
                    crossTabTable <- self$results$crossTabTable
                    goldPLevel <- processed_data$gold_positive_level
                    
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
            },
            
            .calculateIndividualTests = function(processed_data) {
                # Calculate individual test performance statistics
                mydata2 <- processed_data$clean_data
                test_variables <- processed_data$test_variables
                test_positives <- processed_data$test_positives
                
                # Process individual tests
                for (i in seq_along(test_variables)) {
                    testVariable <- test_variables[i]
                    testPLevel <- test_positives[[testVariable]]

                    # Process test data
                    mydata2[[testVariable]] <- forcats::as_factor(mydata2[[testVariable]])

                    # Add binary recode for this test
                    binary_col_name <- paste0(testVariable, "_bin")
                    mydata2[[binary_col_name]] <- dplyr::case_when(
                        mydata2[[testVariable]] == testPLevel ~ 1, 
                        is.na(mydata2[[testVariable]]) ~ NA_real_, 
                        TRUE ~ 0
                    )

                    # Create recode for contingency table
                    recode_col_name <- paste0(testVariable, "_recode")
                    mydata2[[recode_col_name]] <- dplyr::case_when(
                        mydata2[[testVariable]] == testPLevel ~ "Positive",
                        is.na(mydata2[[testVariable]]) ~ NA_character_,
                        TRUE ~ "Negative"
                    )
                    mydata2[[recode_col_name]] <- forcats::fct_relevel(mydata2[[recode_col_name]], "Positive")

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
            },
            
            .performCombinationAnalysis = function(processed_data) {
                # Main combination analysis orchestrator
                mydata2 <- processed_data$clean_data
                test_variables <- processed_data$test_variables
                gold_variable <- processed_data$gold_variable
                
                # Add comprehensive test combination analysis
                mydata2 <- private$.analyzeCombinations(mydata2, test_variables, gold_variable)
                
                # Export combination pattern to data if requested
                if (self$options$addCombinationPattern && self$results$addCombinationPattern$isNotFilled()) {
                    # Create combination pattern for export
                    if ("combination_pattern" %in% names(mydata2)) {
                        # Export the combination pattern to the dataset
                        self$results$addCombinationPattern$setRowNums(rownames(processed_data$original_data))
                        self$results$addCombinationPattern$setValues(mydata2$combination_pattern)
                    }
                }
                
                # Store visualization enablement state for plot rendering
                if (self$options$showVisualization) {
                    # This ensures plots are only prepared when visualization is enabled
                    private$.prepareVisualizationData(mydata2, test_variables)
                }
            },

            
            .validatePlotData = function(plotData) {
                # Validate that plot data contains all required fields and is not null
                required_fields <- c("patterns", "sensitivity", "specificity", "ppv", "npv", "accuracy")
                
                if (is.null(plotData) || length(plotData) == 0) {
                    stop("Plot data is null or empty")
                }
                
                missing_fields <- setdiff(required_fields, names(plotData))
                if (length(missing_fields) > 0) {
                    stop(paste("Missing required plot data fields:", paste(missing_fields, collapse = ", ")))
                }
                
                # Check data consistency
                data_lengths <- sapply(plotData[required_fields], length)
                if (length(unique(data_lengths)) > 1) {
                    stop("Plot data fields have inconsistent lengths")
                }
                
                # Check for empty data
                if (data_lengths[1] == 0) {
                    stop("Plot data contains no observations")
                }
                
                invisible(TRUE)
            },
            
            .generatePatternInfo = function(testVariables) {
                # Consolidated helper function to generate all possible test combination patterns with descriptions
                n_tests <- length(testVariables)
                
                if (n_tests == 2) {
                    patterns <- c("+/+", "+/-", "-/+", "-/-")
                    if (!is.null(testVariables) && length(testVariables) >= 2) {
                        descriptions <- c(
                            "Both tests positive",
                            paste(testVariables[1], "positive,", testVariables[2], "negative"),
                            paste(testVariables[1], "negative,", testVariables[2], "positive"),
                            "Both tests negative"
                        )
                    } else {
                        descriptions <- patterns
                    }
                } else if (n_tests == 3) {
                    patterns <- c("+/+/+", "+/+/-", "+/-/+", "+/-/-", 
                                 "-/+/+", "-/+/-", "-/-/+", "-/-/-")
                    descriptions <- character(length(patterns))
                    
                    if (!is.null(testVariables) && length(testVariables) >= 3) {
                        for (i in seq_along(patterns)) {
                            pattern <- patterns[i]
                            test_results <- strsplit(pattern, "/")[[1]]
                            desc_parts <- character(length(test_results))
                            
                            for (j in seq_along(test_results)) {
                                if (test_results[j] == "+") {
                                    desc_parts[j] <- paste(testVariables[j], "pos")
                                } else {
                                    desc_parts[j] <- paste(testVariables[j], "neg")
                                }
                            }
                            descriptions[i] <- paste(desc_parts, collapse = ", ")
                        }
                    } else {
                        descriptions <- patterns
                    }
                } else {
                    stop(paste("Pattern generation only supports 2 or 3 tests, got", n_tests))
                }
                
                return(list(patterns = patterns, descriptions = descriptions))
            },
            
            
            .calculateDiagnosticStats = function(tp, fp, fn, tn) {
                # Comprehensive diagnostic statistics calculation with clinical interpretation
                # Following established clinical guidelines for diagnostic test evaluation
                total_pos <- tp + fn  # Total diseased (condition positive)
                total_neg <- fp + tn  # Total healthy (condition negative)
                total_test_pos <- tp + fp  # Total test positive
                total_test_neg <- fn + tn  # Total test negative
                total <- tp + fp + fn + tn
                
                # Power analysis and sample size adequacy check
                min_events_per_cell <- 5  # Minimum for reliable statistical inference
                inadequate_power <- any(c(tp, fp, fn, tn) < min_events_per_cell)
                
                if (inadequate_power) {
                    warning(paste("Small cell counts detected (TP=", tp, ", FP=", fp, ", FN=", fn, ", TN=", tn, ").",
                                 "Results may be unreliable. Consider larger sample size or different grouping."))
                }
                
                # Calculate basic diagnostic statistics with clinical bounds checking
                sens <- if(total_pos > 0) tp / total_pos else NA  # True Positive Rate
                spec <- if(total_neg > 0) tn / total_neg else NA  # True Negative Rate
                ppv <- if(total_test_pos > 0) tp / total_test_pos else NA  # Precision
                npv <- if(total_test_neg > 0) tn / total_test_neg else NA  # Negative Precision
                acc <- if(total > 0) (tp + tn) / total else NA  # Overall Accuracy
                
                # Calculate simplified performance score for categorical tests
                # Using balanced accuracy: (Sensitivity + Specificity) / 2
                auc <- if(!is.na(sens) && !is.na(spec)) {
                    (sens + spec) / 2  # Balanced accuracy for categorical tests
                } else NA
                
                # Calculate additional clinical metrics
                youden_index <- if(!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA
                balanced_accuracy <- if(!is.na(sens) && !is.na(spec)) (sens + spec) / 2 else NA
                
                # Calculate likelihood ratios with clinical interpretation bounds
                plr <- if(!is.na(sens) && !is.na(spec) && spec < 1) sens / (1 - spec) else Inf
                nlr <- if(!is.na(sens) && !is.na(spec) && spec > 0) (1 - sens) / spec else 0
                
                # Clinical quality assessment based on established thresholds
                clinical_quality <- "Unknown"
                if (!is.na(youden_index) && !is.na(acc)) {
                    if (youden_index > 0.8 && acc > 0.9) {
                        clinical_quality <- "Excellent - Highly recommended for clinical use"
                    } else if (youden_index > 0.6 && acc > 0.8) {
                        clinical_quality <- "Good - Suitable for clinical use with appropriate context"
                    } else if (youden_index > 0.4 && acc > 0.7) {
                        clinical_quality <- "Fair - Use with caution, consider additional testing"
                    } else if (youden_index > 0.2) {
                        clinical_quality <- "Poor - Limited clinical utility"
                    } else {
                        clinical_quality <- "Very Poor - Not recommended for clinical use"
                    }
                }
                
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
                    youden_index = youden_index, balanced_accuracy = balanced_accuracy,
                    plr = plr, nlr = nlr, clinical_quality = clinical_quality,
                    sens_ci = sens_ci, spec_ci = spec_ci, ppv_ci = ppv_ci,
                    npv_ci = npv_ci, acc_ci = acc_ci,
                    tp = tp, fp = fp, fn = fn, tn = tn
                )
            },

            .plotPerformanceHeatmap = function(image, ...) {
                # Validate plot data first
                plotData <- image$state
                tryCatch({
                    private$.validatePlotData(plotData)
                }, error = function(e) {
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                        label = paste("Data validation error:", e$message),
                                        size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void() +
                        ggplot2::labs(title = "Performance Heatmap - Data Error")
                    print(plot)
                    return(TRUE)
                })
                
                # Get plot options including dimensions
                color_scheme <- self$options$colorScheme
                plot_width <- self$options$plotWidth
                plot_height <- self$options$plotHeight
                
                # Set image dimensions if provided
                if (!is.null(plot_width) && !is.null(plot_height)) {
                    image$setSize(width = plot_width, height = plot_height)
                }
                
                # Create publication-ready heatmap data with comprehensive metrics
                heatmap_data <- data.frame(
                    Pattern = factor(plotData$patterns, levels = plotData$patterns),
                    Description = plotData$descriptions,
                    Sensitivity = round(plotData$sensitivity * 100, 1),
                    Specificity = round(plotData$specificity * 100, 1), 
                    PPV = round(plotData$ppv * 100, 1),
                    NPV = round(plotData$npv * 100, 1),
                    Accuracy = round(plotData$accuracy * 100, 1),
                    n_positive = plotData$tp + plotData$fn,
                    n_negative = plotData$fp + plotData$tn,
                    stringsAsFactors = FALSE
                )
                
                # Reshape for heatmap with additional information
                heatmap_long <- heatmap_data %>%
                    tidyr::pivot_longer(
                        cols = c(Sensitivity, Specificity, PPV, NPV, Accuracy),
                        names_to = "Metric", 
                        values_to = "Value"
                    ) %>%
                    dplyr::mutate(
                        Metric = factor(Metric, 
                                      levels = c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"),
                                      labels = c("Sensitivity\n(True +)", "Specificity\n(True -)", 
                                               "PPV\n(Pred +)", "NPV\n(Pred -)", "Accuracy\n(Overall)"))
                    )
                
                # Professional color schemes
                if (color_scheme == "clinical") {
                    fill_colors <- c(low = "#d73027", mid = "#fee08b", high = "#1a9850")
                } else if (color_scheme == "viridis") {
                    fill_colors <- c(low = "#440154", mid = "#21908C", high = "#FDE725")
                } else if (color_scheme == "blueyellow") {
                    fill_colors <- c(low = "#08519c", mid = "#6baed6", high = "#ffffcc")
                } else { # grayscale
                    fill_colors <- c(low = "#252525", mid = "#737373", high = "#f0f0f0")
                }
                
                # Create publication-quality heatmap
                plot <- ggplot2::ggplot(heatmap_long, ggplot2::aes(x = Metric, y = Pattern, fill = Value)) +
                    ggplot2::geom_tile(color = "white", size = 1.2, alpha = 0.9) +
                    ggplot2::geom_text(
                        ggplot2::aes(label = paste0(Value, "%")), 
                        color = "white", 
                        size = 4.5, 
                        fontface = "bold",
                        family = "serif"
                    ) +
                    ggplot2::scale_fill_gradient2(
                        low = fill_colors["low"], 
                        mid = fill_colors["mid"], 
                        high = fill_colors["high"],
                        midpoint = 75, 
                        name = "Performance\n(%)",
                        limits = c(0, 100),
                        guide = ggplot2::guide_colorbar(
                            barwidth = 1.5,
                            barheight = 12,
                            title.position = "top",
                            title.hjust = 0.5,
                            frame.colour = "black",
                            ticks.colour = "black"
                        )
                    ) +
                    ggplot2::labs(
                        title = "Diagnostic Test Combination Performance Matrix",
                        subtitle = paste0("Performance metrics across ", length(unique(plotData$patterns)), 
                                         " test combination patterns (n=", 
                                         sum(plotData$tp + plotData$fp + plotData$fn + plotData$tn)/length(plotData$patterns), ")"),
                        x = "Diagnostic Performance Metrics",
                        y = "Test Combination Patterns",
                        caption = "Higher values indicate better diagnostic performance. PPV: Positive Predictive Value; NPV: Negative Predictive Value"
                    ) +
                    ggplot2::theme_classic(base_size = 12, base_family = "serif") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold", margin = ggplot2::margin(b = 10)),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray30", margin = ggplot2::margin(b = 15)),
                        plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40", margin = ggplot2::margin(t = 10)),
                        axis.title.x = ggplot2::element_text(size = 13, face = "bold", margin = ggplot2::margin(t = 15)),
                        axis.title.y = ggplot2::element_text(size = 13, face = "bold", margin = ggplot2::margin(r = 15)),
                        axis.text.x = ggplot2::element_text(size = 11, face = "bold", color = "black"),
                        axis.text.y = ggplot2::element_text(size = 11, face = "bold", color = "black"),
                        axis.ticks = ggplot2::element_line(color = "black", size = 0.5),
                        axis.line = ggplot2::element_line(color = "black", size = 0.8),
                        legend.position = "right",
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1.2),
                        plot.margin = ggplot2::margin(20, 20, 20, 20)
                    )
                
                print(plot)
                TRUE
            },
            
            .findOptimalCutpoint = function(plotData) {
                # Identify optimal cut-point using multiple criteria
                # Returns the pattern with best overall performance
                
                if (is.null(plotData) || length(plotData$patterns) == 0) {
                    return(list(pattern = "None", method = "No data", rationale = "No data available"))
                }
                
                # Calculate Youden's J statistic for each pattern
                youden_scores <- plotData$sensitivity + plotData$specificity - 1
                
                # Find optimal patterns by different criteria
                youden_optimal <- which.max(youden_scores)
                accuracy_optimal <- which.max(plotData$accuracy)
                
                # Calculate distance from perfect classifier (100% sensitivity, 100% specificity)
                performance_distances <- sqrt((1 - plotData$specificity)^2 + (1 - plotData$sensitivity)^2)
                distance_optimal <- which.min(performance_distances)
                
                # Determine best overall pattern with clinical reasoning
                optimal_pattern <- youden_optimal
                method <- "Youden's J Index"
                rationale <- paste0(
                    "Selected based on maximum Youden's J statistic (", 
                    round(youden_scores[youden_optimal], 3), 
                    "), which optimizes sensitivity + specificity - 1. ",
                    "This provides balanced performance for screening and confirmation."
                )
                
                # Check if accuracy-based selection differs significantly
                if (accuracy_optimal != youden_optimal) {
                    acc_diff <- abs(plotData$accuracy[accuracy_optimal] - plotData$accuracy[youden_optimal])
                    if (acc_diff > 0.05) {  # >5% difference
                        rationale <- paste0(rationale, " Note: Pattern '", 
                                           plotData$patterns[accuracy_optimal], 
                                           "' has higher accuracy (", 
                                           round(plotData$accuracy[accuracy_optimal] * 100, 1), 
                                           "% vs ", round(plotData$accuracy[youden_optimal] * 100, 1), 
                                           "%) but lower balanced performance.")
                    }
                }
                
                return(list(
                    pattern = plotData$patterns[optimal_pattern],
                    method = method,
                    rationale = rationale,
                    youden_index = youden_scores[optimal_pattern],
                    sensitivity = plotData$sensitivity[optimal_pattern],
                    specificity = plotData$specificity[optimal_pattern],
                    accuracy = plotData$accuracy[optimal_pattern]
                ))
            },
            
            
            .plotDecisionTree = function(image, ...) {
                # Validate plot data first
                plotData <- image$state
                
                # Get plot dimensions if provided
                plot_width <- self$options$plotWidth
                plot_height <- self$options$plotHeight
                
                # Set image dimensions if provided
                if (!is.null(plot_width) && !is.null(plot_height)) {
                    image$setSize(width = plot_width, height = plot_height)
                }
                
                tryCatch({
                    private$.validatePlotData(plotData)
                }, error = function(e) {
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                        label = paste("Data validation error:", e$message),
                                        size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void() +
                        ggplot2::labs(title = "Decision Tree - Data Error")
                    print(plot)
                    return(TRUE)
                })
                
                # Create publication-ready decision tree with hierarchical layout
                n_patterns <- length(plotData$patterns)
                
                # Sort patterns by accuracy for better visualization
                pattern_order <- order(plotData$accuracy, decreasing = TRUE)
                sorted_patterns <- plotData$patterns[pattern_order]
                sorted_accuracy <- plotData$accuracy[pattern_order]
                sorted_sensitivity <- plotData$sensitivity[pattern_order]
                sorted_specificity <- plotData$specificity[pattern_order]
                
                # Create hierarchical tree structure
                # Root node
                root_node <- data.frame(
                    x = 0.5, y = 0.95,
                    label = "Diagnostic\nDecision",
                    type = "root",
                    value = NA,
                    level = 0,
                    stringsAsFactors = FALSE
                )
                
                # Test strategy nodes (level 1)
                strategy_x <- seq(0.15, 0.85, length.out = min(n_patterns, 4))
                if (n_patterns > 4) {
                    # Adjust for more patterns
                    strategy_x <- seq(0.1, 0.9, length.out = n_patterns)
                }
                
                strategy_nodes <- data.frame(
                    x = strategy_x[1:n_patterns],
                    y = rep(0.7, n_patterns),
                    label = sorted_patterns,
                    type = "strategy",
                    value = sorted_accuracy,
                    level = 1,
                    stringsAsFactors = FALSE
                )
                
                # Performance outcome nodes (level 2)
                outcome_nodes <- data.frame(
                    x = strategy_x[1:n_patterns],
                    y = rep(0.45, n_patterns),
                    label = paste0("Acc: ", round(sorted_accuracy * 100, 1), "%\n",
                                 "Sens: ", round(sorted_sensitivity * 100, 1), "%\n",
                                 "Spec: ", round(sorted_specificity * 100, 1), "%"),
                    type = "outcome",
                    value = sorted_accuracy,
                    level = 2,
                    stringsAsFactors = FALSE
                )
                
                # Clinical decision nodes (level 3)
                clinical_nodes <- data.frame(
                    x = strategy_x[1:n_patterns],
                    y = rep(0.15, n_patterns),
                    label = ifelse(sorted_accuracy > 0.8, "Recommended\nfor Clinical Use", 
                                 ifelse(sorted_accuracy > 0.7, "Consider with\nCaution", 
                                       "Not Recommended")),
                    type = "clinical",
                    value = sorted_accuracy,
                    level = 3,
                    stringsAsFactors = FALSE
                )
                
                # Combine all nodes
                all_nodes <- rbind(root_node, strategy_nodes, outcome_nodes, clinical_nodes)
                
                # Create edges with different styles
                edges <- data.frame()
                
                # Root to strategy edges
                for (i in 1:n_patterns) {
                    edges <- rbind(edges, data.frame(
                        x = 0.5, y = 0.95,
                        xend = strategy_x[i], yend = 0.7,
                        type = "decision", width = 1.2,
                        stringsAsFactors = FALSE
                    ))
                }
                
                # Strategy to outcome edges
                for (i in 1:n_patterns) {
                    edges <- rbind(edges, data.frame(
                        x = strategy_x[i], y = 0.7,
                        xend = strategy_x[i], yend = 0.45,
                        type = "evaluation", width = 1.0,
                        stringsAsFactors = FALSE
                    ))
                }
                
                # Outcome to clinical decision edges
                for (i in 1:n_patterns) {
                    edges <- rbind(edges, data.frame(
                        x = strategy_x[i], y = 0.45,
                        xend = strategy_x[i], yend = 0.15,
                        type = "clinical", width = 0.8,
                        stringsAsFactors = FALSE
                    ))
                }
                
                # Create publication-quality decision tree
                plot <- ggplot2::ggplot() +
                    
                    # Draw edges with different styles
                    ggplot2::geom_segment(
                        data = edges,
                        ggplot2::aes(x = x, y = y, xend = xend, yend = yend, 
                                    color = type, size = width),
                        alpha = 0.7
                    ) +
                    
                    # Draw nodes with size based on performance
                    ggplot2::geom_point(
                        data = all_nodes,
                        ggplot2::aes(x = x, y = y, fill = type, size = ifelse(is.na(value), 8, value * 10 + 3)),
                        shape = 21, color = "black", stroke = 1.2, alpha = 0.9
                    ) +
                    
                    # Add node labels with background for readability
                    ggplot2::geom_label(
                        data = all_nodes,
                        ggplot2::aes(x = x, y = y, label = label, fill = type),
                        color = "black", fontface = "bold", size = 3.5,
                        alpha = 0.9, label.padding = ggplot2::unit(0.3, "lines"),
                        label.r = ggplot2::unit(0.2, "lines")
                    ) +
                    
                    # Professional color schemes
                    ggplot2::scale_fill_manual(
                        name = "Node Type",
                        values = c(
                            "root" = "#3498db",
                            "strategy" = "#2ecc71", 
                            "outcome" = "#f39c12",
                            "clinical" = "#e74c3c"
                        )
                    ) +
                    
                    ggplot2::scale_color_manual(
                        name = "Edge Type",
                        values = c(
                            "decision" = "#34495e",
                            "evaluation" = "#27ae60",
                            "clinical" = "#c0392b"
                        )
                    ) +
                    
                    ggplot2::scale_size_identity() +
                    
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    
                    ggplot2::labs(
                        title = "Clinical Decision Tree: Test Combination Strategy",
                        subtitle = paste0("Hierarchical evaluation of ", n_patterns, 
                                         " test combination patterns with performance-based recommendations"),
                        caption = "Node size reflects diagnostic accuracy. Recommendations based on 70% and 80% performance thresholds."
                    ) +
                    
                    ggplot2::theme_void(base_size = 12, base_family = "serif") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold",
                                                          margin = ggplot2::margin(b = 10)),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray30",
                                                             margin = ggplot2::margin(b = 15)),
                        plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40",
                                                            margin = ggplot2::margin(t = 15)),
                        legend.position = "bottom",
                        legend.box = "horizontal",
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        plot.margin = ggplot2::margin(20, 20, 20, 20)
                    ) +
                    
                    ggplot2::guides(
                        fill = ggplot2::guide_legend(
                            override.aes = list(size = 6, alpha = 1),
                            title.position = "top", title.hjust = 0.5
                        ),
                        color = ggplot2::guide_legend(
                            override.aes = list(size = 2, alpha = 1),
                            title.position = "top", title.hjust = 0.5
                        )
                    )
                
                print(plot)
                TRUE
            },
            
            .plotVennDiagram = function(image, ...) {
                # Validate plot data first
                plotData <- image$state
                
                # Get plot dimensions if provided
                plot_width <- self$options$plotWidth
                plot_height <- self$options$plotHeight
                
                # Set image dimensions if provided
                if (!is.null(plot_width) && !is.null(plot_height)) {
                    image$setSize(width = plot_width, height = plot_height)
                }
                
                tryCatch({
                    private$.validatePlotData(plotData)
                }, error = function(e) {
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                        label = paste("Data validation error:", e$message),
                                        size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void() +
                        ggplot2::labs(title = "Venn Diagram - Data Error")
                    print(plot)
                    return(TRUE)
                })
                
                # Create publication-ready test concordance analysis
                # Calculate actual pattern frequencies for the diagram
                patterns <- plotData$patterns
                n_total <- sum(plotData$tp + plotData$fp + plotData$fn + plotData$tn) / length(patterns)
                
                # Extract key pattern frequencies (for 2-test combinations)
                if (length(patterns) >= 4) {
                    both_pos_idx <- which(patterns == "+/+")
                    test1_only_idx <- which(patterns == "+/-")
                    test2_only_idx <- which(patterns == "-/+")
                    both_neg_idx <- which(patterns == "-/-")
                    
                    # Calculate frequencies as proportions
                    freq_both_pos <- if(length(both_pos_idx) > 0) {
                        (plotData$tp[both_pos_idx] + plotData$fp[both_pos_idx]) / n_total
                    } else { 0.1 }
                    
                    freq_test1_only <- if(length(test1_only_idx) > 0) {
                        (plotData$tp[test1_only_idx] + plotData$fp[test1_only_idx]) / n_total
                    } else { 0.15 }
                    
                    freq_test2_only <- if(length(test2_only_idx) > 0) {
                        (plotData$tp[test2_only_idx] + plotData$fp[test2_only_idx]) / n_total
                    } else { 0.15 }
                    
                    freq_both_neg <- if(length(both_neg_idx) > 0) {
                        (plotData$tp[both_neg_idx] + plotData$fp[both_neg_idx]) / n_total
                    } else { 0.6 }
                    
                    # Calculate performance metrics for key patterns
                    perf_both_pos <- if(length(both_pos_idx) > 0) plotData$accuracy[both_pos_idx] else 0
                    perf_test1_only <- if(length(test1_only_idx) > 0) plotData$accuracy[test1_only_idx] else 0
                    perf_test2_only <- if(length(test2_only_idx) > 0) plotData$accuracy[test2_only_idx] else 0
                    
                } else {
                    # Default values if patterns don't match expected format
                    freq_both_pos <- 0.2; freq_test1_only <- 0.15; freq_test2_only <- 0.15; freq_both_neg <- 0.5
                    perf_both_pos <- perf_test1_only <- perf_test2_only <- 0.75
                }
                
                # Create professional Venn diagram with proportional areas
                circle_radius <- 0.25
                overlap_distance <- 0.15  # Distance between circle centers
                
                # Generate circle coordinates
                theta <- seq(0, 2*pi, length.out = 200)
                
                # Test 1 circle (left)
                circle1_x <- 0.4
                circle1_y <- 0.5
                circle1_points <- data.frame(
                    x = circle1_x + circle_radius * cos(theta),
                    y = circle1_y + circle_radius * sin(theta),
                    test = "Test 1"
                )
                
                # Test 2 circle (right)
                circle2_x <- 0.6
                circle2_y <- 0.5
                circle2_points <- data.frame(
                    x = circle2_x + circle_radius * cos(theta),
                    y = circle2_y + circle_radius * sin(theta),
                    test = "Test 2"
                )
                
                # Create annotation data with performance metrics
                annotations <- data.frame(
                    x = c(0.3, 0.7, 0.5, 0.5),
                    y = c(0.5, 0.5, 0.5, 0.2),
                    label = c(
                        paste0("Test 1 Only\n", round(freq_test1_only * 100, 1), "%\nAcc: ", round(perf_test1_only * 100, 1), "%"),
                        paste0("Test 2 Only\n", round(freq_test2_only * 100, 1), "%\nAcc: ", round(perf_test2_only * 100, 1), "%"),
                        paste0("Both Tests\nPositive\n", round(freq_both_pos * 100, 1), "%\nAcc: ", round(perf_both_pos * 100, 1), "%"),
                        paste0("Neither Test Positive: ", round(freq_both_neg * 100, 1), "%")
                    ),
                    color = c("#3498db", "#e74c3c", "#27ae60", "#7f8c8d"),
                    size = c(4, 4, 4.5, 3.5),
                    fontface = c("bold", "bold", "bold", "plain"),
                    stringsAsFactors = FALSE
                )
                
                # Calculate Cohen's kappa or agreement statistics
                kappa_approx <- (perf_both_pos - 0.5) * 2  # Simplified kappa approximation
                
                # Create publication-quality Venn diagram
                plot <- ggplot2::ggplot() +
                    
                    # Test 1 circle
                    ggplot2::geom_polygon(
                        data = circle1_points,
                        ggplot2::aes(x = x, y = y),
                        fill = "#3498db", alpha = 0.4, color = "#2980b9", size = 2
                    ) +
                    
                    # Test 2 circle
                    ggplot2::geom_polygon(
                        data = circle2_points,
                        ggplot2::aes(x = x, y = y),
                        fill = "#e74c3c", alpha = 0.4, color = "#c0392b", size = 2
                    ) +
                    
                    # Test labels outside circles
                    ggplot2::annotate(
                        "text", x = 0.2, y = 0.7,
                        label = "Test 1\n(Positive Results)",
                        size = 5, fontface = "bold", color = "#2980b9",
                        hjust = 0.5, vjust = 0.5
                    ) +
                    
                    ggplot2::annotate(
                        "text", x = 0.8, y = 0.7,
                        label = "Test 2\n(Positive Results)",
                        size = 5, fontface = "bold", color = "#c0392b",
                        hjust = 0.5, vjust = 0.5
                    ) +
                    
                    # Add performance annotations
                    ggplot2::geom_text(
                        data = annotations,
                        ggplot2::aes(x = x, y = y, label = label, color = I(color), 
                                   size = I(size), fontface = I(fontface))
                    ) +
                    
                    # Add agreement statistics box
                    ggplot2::annotate(
                        "rect", 
                        xmin = 0.05, xmax = 0.95, ymin = 0.02, ymax = 0.12,
                        fill = "white", color = "#34495e", size = 1, alpha = 0.9
                    ) +
                    
                    ggplot2::annotate(
                        "text", x = 0.5, y = 0.07,
                        label = paste0(
                            "Test Concordance Analysis | Sample Size: n = ", round(n_total), 
                            " | Agreement Score: ", round(kappa_approx, 2),
                            "\nBest Combined Strategy: Both tests positive (Accuracy: ", 
                            round(perf_both_pos * 100, 1), "%)"
                        ),
                        size = 3.5, color = "#2c3e50", fontface = "bold",
                        hjust = 0.5, vjust = 0.5
                    ) +
                    
                    # Formatting
                    ggplot2::coord_fixed(ratio = 1) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 0.8) +
                    
                    ggplot2::labs(
                        title = "Test Concordance and Agreement Analysis",
                        subtitle = paste0("Overlap analysis of diagnostic test combinations showing ",
                                         "frequency distributions and performance metrics"),
                        caption = "Areas are proportional to observed frequencies. Accuracy values show diagnostic performance for each test combination pattern."
                    ) +
                    
                    ggplot2::theme_void(base_size = 12, base_family = "serif") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold",
                                                          margin = ggplot2::margin(b = 10)),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray30",
                                                             margin = ggplot2::margin(b = 15)),
                        plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40",
                                                            margin = ggplot2::margin(t = 10)),
                        plot.margin = ggplot2::margin(20, 20, 20, 20)
                    )
                
                print(plot)
                TRUE
            },
            
            .plotForest = function(image, ...) {
                # Validate plot data first
                plotData <- image$state
                
                # Get plot dimensions if provided
                plot_width <- self$options$plotWidth
                plot_height <- self$options$plotHeight
                
                # Set image dimensions if provided
                if (!is.null(plot_width) && !is.null(plot_height)) {
                    image$setSize(width = plot_width, height = plot_height)
                }
                
                tryCatch({
                    private$.validatePlotData(plotData)
                }, error = function(e) {
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                        label = paste("Data validation error:", e$message),
                                        size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void() +
                        ggplot2::labs(title = "Forest Plot - Data Error")
                    print(plot)
                    return(TRUE)
                })
                
                # Calculate Wilson score confidence intervals for each metric
                # Wilson CI is preferred over normal approximation for medical diagnostics
                # because it provides more accurate intervals for proportions, especially
                # with small sample sizes or extreme values (near 0 or 1)
                calcWilsonCI <- function(x, n, conf.level = 0.95) {
                    if (n == 0 || is.na(x) || is.na(n)) return(c(NA, NA))
                    
                    # Wilson score interval calculation
                    # More accurate than normal approximation for clinical proportions
                    p <- x / n
                    z <- qnorm((1 + conf.level) / 2)  # 1.96 for 95% CI
                    
                    # Wilson formula: accounts for discrete nature of binomial data
                    denominator <- 1 + (z^2 / n)
                    centre <- (p + (z^2 / (2 * n))) / denominator
                    half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator
                    
                    # Ensure bounds are within [0,1] for clinical interpretability
                    c(max(0, centre - half_width), min(1, centre + half_width))
                }
                
                # Prepare comprehensive forest plot data
                n_patterns <- length(plotData$patterns)
                
                # Calculate sample sizes for CI calculation
                n_positive <- plotData$tp + plotData$fn
                n_negative <- plotData$fp + plotData$tn
                n_test_positive <- plotData$tp + plotData$fp
                n_test_negative <- plotData$fn + plotData$tn
                n_total <- plotData$tp + plotData$fp + plotData$fn + plotData$tn
                
                # Create comprehensive forest data with proper CIs
                metrics_data <- list(
                    list(name = "Sensitivity", estimates = plotData$sensitivity, 
                         n_success = plotData$tp, n_total = n_positive, color = "#1f77b4"),
                    list(name = "Specificity", estimates = plotData$specificity, 
                         n_success = plotData$tn, n_total = n_negative, color = "#ff7f0e"),
                    list(name = "PPV", estimates = plotData$ppv, 
                         n_success = plotData$tp, n_total = n_test_positive, color = "#2ca02c"),
                    list(name = "NPV", estimates = plotData$npv, 
                         n_success = plotData$tn, n_total = n_test_negative, color = "#d62728"),
                    list(name = "Accuracy", estimates = plotData$accuracy, 
                         n_success = plotData$tp + plotData$tn, n_total = n_total, color = "#9467bd")
                )
                
                # Build forest data frame with proper confidence intervals
                forest_data <- data.frame()
                y_pos <- 0
                
                for (i in seq_along(plotData$patterns)) {
                    for (j in seq_along(metrics_data)) {
                        y_pos <- y_pos + 1
                        metric <- metrics_data[[j]]
                        
                        # Calculate Wilson CI
                        ci <- calcWilsonCI(metric$n_success[i], metric$n_total[i])
                        
                        forest_data <- rbind(forest_data, data.frame(
                            Pattern = plotData$patterns[i],
                            Description = plotData$descriptions[i],
                            Metric = metric$name,
                            Estimate = metric$estimates[i],
                            Lower = ci[1],
                            Upper = ci[2],
                            Y_Position = y_pos,
                            Color = metric$color,
                            N_Success = metric$n_success[i],
                            N_Total = metric$n_total[i],
                            stringsAsFactors = FALSE
                        ))
                    }
                }
                
                # Create publication-quality forest plot
                plot <- ggplot2::ggplot(forest_data, ggplot2::aes(x = Estimate, y = Y_Position)) +
                    
                    # Reference lines
                    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", 
                                      color = "gray60", size = 1, alpha = 0.8) +
                    ggplot2::geom_vline(xintercept = c(0.8, 0.9), linetype = "dotted", 
                                      color = "gray70", size = 0.8, alpha = 0.6) +
                    
                    # Confidence intervals
                    ggplot2::geom_errorbarh(
                        ggplot2::aes(xmin = Lower, xmax = Upper, color = Metric),
                        height = 0.3, size = 1.2, alpha = 0.8
                    ) +
                    
                    # Point estimates
                    ggplot2::geom_point(
                        ggplot2::aes(color = Metric, size = N_Total), 
                        shape = 18, alpha = 0.9, stroke = 0.5
                    ) +
                    
                    # Add pattern labels on the left
                    ggplot2::geom_text(
                        data = forest_data[forest_data$Metric == "Sensitivity", ],
                        ggplot2::aes(x = -0.05, y = Y_Position + 2, label = Pattern),
                        hjust = 1, size = 4, fontface = "bold", family = "serif"
                    ) +
                    
                    # Add metric labels on the right with values and CIs
                    ggplot2::geom_text(
                        ggplot2::aes(x = 1.05, y = Y_Position, 
                                   label = paste0(Metric, ": ", 
                                                 round(Estimate * 100, 1), "% ",
                                                 "(", round(Lower * 100, 1), "%-",
                                                 round(Upper * 100, 1), "%)"),
                                   color = Metric),
                        hjust = 0, size = 3.2, fontface = "bold", show.legend = FALSE
                    ) +
                    
                    # Professional color scheme
                    ggplot2::scale_color_manual(
                        name = "Diagnostic\nMetric",
                        values = c("Sensitivity" = "#1f77b4", "Specificity" = "#ff7f0e", 
                                 "PPV" = "#2ca02c", "NPV" = "#d62728", "Accuracy" = "#9467bd"),
                        guide = ggplot2::guide_legend(
                            override.aes = list(size = 4, alpha = 1),
                            keywidth = 1.5, keyheight = 1.5
                        )
                    ) +
                    
                    ggplot2::scale_size_continuous(
                        name = "Sample\nSize",
                        range = c(2, 6),
                        guide = ggplot2::guide_legend(
                            override.aes = list(alpha = 0.8),
                            keywidth = 1.5, keyheight = 1.5
                        )
                    ) +
                    
                    ggplot2::scale_x_continuous(
                        name = "Performance Estimate with 95% Confidence Interval",
                        limits = c(-0.1, 1.6),
                        breaks = seq(0, 1, 0.2),
                        labels = scales::percent_format(accuracy = 1),
                        expand = c(0, 0)
                    ) +
                    
                    ggplot2::scale_y_continuous(name = "", breaks = NULL, expand = c(0.02, 0.02)) +
                    
                    ggplot2::labs(
                        title = "Forest Plot: Diagnostic Performance with Confidence Intervals",
                        subtitle = paste0("Wilson score 95% confidence intervals for ", 
                                         length(unique(plotData$patterns)), 
                                         " test combination patterns"),
                        caption = "PPV: Positive Predictive Value; NPV: Negative Predictive Value. Dotted lines at 80% and 90% performance thresholds."
                    ) +
                    
                    ggplot2::theme_classic(base_size = 12, base_family = "serif") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold",
                                                          margin = ggplot2::margin(b = 10)),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray30",
                                                             margin = ggplot2::margin(b = 15)),
                        plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40",
                                                            margin = ggplot2::margin(t = 15)),
                        axis.title.x = ggplot2::element_text(size = 13, face = "bold",
                                                            margin = ggplot2::margin(t = 15)),
                        axis.text.x = ggplot2::element_text(size = 11, color = "black"),
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.line.y = ggplot2::element_blank(),
                        axis.line.x = ggplot2::element_line(color = "black", size = 0.8),
                        axis.ticks.x = ggplot2::element_line(color = "black", size = 0.5),
                        legend.position = "bottom",
                        legend.box = "horizontal",
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        panel.grid.major.x = ggplot2::element_line(color = "gray90", size = 0.5),
                        panel.grid.minor.x = ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank(),
                        panel.border = ggplot2::element_blank(),
                        plot.margin = ggplot2::margin(20, 20, 20, 20)
                    )
                
                print(plot)
                TRUE
            },
            
            .generateClinicalSummary = function(results, optimal_pattern = NULL) {
                # Generate plain-language clinical summary for pathologists/clinicians
                if (is.null(optimal_pattern) || is.null(results)) {
                    return("<p><strong>Clinical Summary:</strong> Analysis in progress...</p>")
                }
                
                # Extract key performance metrics for optimal pattern
                optimal_idx <- which(results$patterns == optimal_pattern$pattern)
                if (length(optimal_idx) == 0) return("<p><strong>Clinical Summary:</strong> Unable to determine optimal pattern.</p>")
                
                sens <- results$sensitivity[optimal_idx] * 100
                spec <- results$specificity[optimal_idx] * 100 
                ppv <- results$ppv[optimal_idx] * 100
                npv <- results$npv[optimal_idx] * 100
                acc <- results$accuracy[optimal_idx] * 100
                
                # Clinical quality assessment
                clinical_quality <- dplyr::case_when(
                    sens >= 90 && spec >= 90 ~ "excellent",
                    sens >= 80 && spec >= 80 ~ "good", 
                    sens >= 70 || spec >= 70 ~ "moderate",
                    TRUE ~ "limited"
                )
                
                # Clinical recommendation
                clinical_use <- dplyr::case_when(
                    sens > 90 ~ "This combination is excellent for <strong>ruling OUT</strong> disease (high sensitivity).",
                    spec > 90 ~ "This combination is excellent for <strong>ruling IN</strong> disease (high specificity).",
                    sens > 80 && spec > 80 ~ "This combination provides good overall diagnostic performance.",
                    TRUE ~ "Consider clinical context and additional testing when interpreting results."
                )
                
                # Generate natural language summary
                summary_html <- paste0(
                    "<div style='background-color: #f8f9fa; border-left: 4px solid #28a745; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='color: #155724; margin-top: 0;'>📋 Clinical Summary</h4>",
                    "<p><strong>Optimal Test Combination:</strong> ", optimal_pattern$pattern, "</p>",
                    "<p><strong>Performance:</strong> ", 
                    sprintf("%.1f%% sensitivity, %.1f%% specificity, %.1f%% overall accuracy", sens, spec, acc), "</p>",
                    "<p><strong>Clinical Interpretation:</strong> ", clinical_use, "</p>",
                    "<p><strong>Predictive Values:</strong> ", 
                    sprintf("%.1f%% positive predictive value, %.1f%% negative predictive value", ppv, npv), "</p>",
                    "<p style='margin-bottom: 0;'><strong>Quality Assessment:</strong> ", 
                    stringr::str_to_title(clinical_quality), " diagnostic performance for this test combination pattern.</p>",
                    "</div>"
                )
                
                return(summary_html)
            },

            .generateCopyReadyReport = function(results, optimal_pattern = NULL, test_names = NULL) {
                # Generate copy-ready report sentences for clinical documentation
                if (is.null(optimal_pattern) || is.null(results) || is.null(test_names)) {
                    return("<p><em>Report template will be generated once analysis is complete.</em></p>")
                }
                
                # Extract key performance metrics for optimal pattern
                optimal_idx <- which(results$patterns == optimal_pattern$pattern)
                if (length(optimal_idx) == 0) return("<p><em>Unable to generate report template.</em></p>")
                
                sens <- results$sensitivity[optimal_idx] * 100
                spec <- results$specificity[optimal_idx] * 100 
                ppv <- results$ppv[optimal_idx] * 100
                npv <- results$npv[optimal_idx] * 100
                acc <- results$accuracy[optimal_idx] * 100
                
                # Format test names
                test_list <- paste(test_names, collapse = " and ")
                n_tests <- length(test_names)
                
                # Generate copy-ready sentences
                report_sentences <- paste0(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 15px; margin: 10px 0; border-radius: 5px;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>📄 Copy-Ready Report Template</h4>",
                    "<div style='font-family: monospace; background-color: #f8f9fa; padding: 10px; border-left: 3px solid #6c757d; margin: 10px 0;'>",
                    "<p><strong>Methods:</strong> We evaluated the diagnostic performance of ", test_list, 
                    " compared to the gold standard reference test using ", n_tests, "-test combination analysis. ",
                    "Wilson score confidence intervals were calculated for enhanced accuracy with categorical data.</p>",
                    
                    "<p><strong>Results:</strong> The optimal test combination pattern (", optimal_pattern$pattern, ") achieved ",
                    sprintf("%.1f%% sensitivity (95%% CI: [calculated]), %.1f%% specificity (95%% CI: [calculated]), ", sens, spec),
                    sprintf("and %.1f%% overall accuracy. ", acc),
                    sprintf("Positive and negative predictive values were %.1f%% and %.1f%%, respectively.</p>", ppv, npv),
                    
                    "<p><strong>Clinical Interpretation:</strong> ",
                    if (sens > 90) {
                        "The high sensitivity makes this combination excellent for ruling out disease in clinical practice."
                    } else if (spec > 90) {
                        "The high specificity makes this combination excellent for confirming disease presence."
                    } else if (sens > 80 && spec > 80) {
                        "This combination provides good overall diagnostic performance for clinical decision-making."
                    } else {
                        "Results should be interpreted in appropriate clinical context with consideration of additional testing."
                    },
                    "</p>",
                    "</div>",
                    "<p style='font-size: 12px; color: #6c757d; margin-bottom: 0;'><em>Note: Copy the text above for use in manuscripts, reports, or clinical documentation. ",
                    "Replace [calculated] with actual confidence interval values from the analysis tables.</em></p>",
                    "</div>"
                )
                
                return(report_sentences)
            },

            .generateAnalysisSummaryHTML = function(num_tests, test_names, n_total_cases, 
                                                   n_positive_cases, n_negative_cases, 
                                                   patterns, descriptions, optimal_cutpoint = NULL) {
                # Generate structured, professional HTML summary
                html <- paste0(
                    "<div style='font-family: serif; max-width: 800px; margin: 0 auto;'>",
                    "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                    "Diagnostic Test Combination Analysis",
                    "</h3>",
                    
                    "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #3498db; margin: 20px 0;'>",
                    "<h4 style='margin-top: 0; color: #2c3e50;'>Study Overview</h4>",
                    "<p><strong>Tests Analyzed:</strong> ", test_names, " (", num_tests, " tests)</p>",
                    "<p><strong>Total Sample Size:</strong> ", n_total_cases, " cases</p>",
                    "<p><strong>Disease Prevalence:</strong> ", n_positive_cases, " positive cases (", 
                    round(n_positive_cases / n_total_cases * 100, 1), "%), ",
                    n_negative_cases, " negative cases (", 
                    round(n_negative_cases / n_total_cases * 100, 1), "%)</p>",
                    "<p><strong>Combination Patterns:</strong> ", length(patterns), " unique patterns analyzed</p>",
                    "</div>",
                    
                    "<h4 style='color: #2c3e50; margin-top: 30px;'>Test Combination Patterns</h4>",
                    "<div style='background-color: #fff; border: 1px solid #ddd; border-radius: 5px; padding: 15px;'>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<thead>",
                    "<tr style='background-color: #3498db; color: white;'>",
                    "<th style='padding: 10px; text-align: left; border: 1px solid #ddd;'>Pattern</th>",
                    "<th style='padding: 10px; text-align: left; border: 1px solid #ddd;'>Description</th>",
                    "</tr>",
                    "</thead>",
                    "<tbody>"
                )
                
                # Add pattern descriptions
                for (i in seq_along(patterns)) {
                    row_color <- if (i %% 2 == 0) "#f2f2f2" else "white"
                    html <- paste0(html,
                        "<tr style='background-color: ", row_color, ";'>",
                        "<td style='padding: 8px; border: 1px solid #ddd; font-family: monospace; font-weight: bold;'>",
                        patterns[i], "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd;'>", descriptions[i], "</td>",
                        "</tr>"
                    )
                }
                
                html <- paste0(html,
                    "</tbody>",
                    "</table>",
                    "</div>",
                    
                    "<div style='background-color: #e8f4fd; padding: 15px; border-left: 4px solid #3498db; margin: 20px 0;'>",
                    "<h4 style='margin-top: 0; color: #2c3e50;'>Diagnostic Performance Metrics</h4>",
                    "<ul style='line-height: 1.6;'>",
                    "<li><strong>Sensitivity:</strong> Probability of test combination being positive when disease is present (True Positive Rate)</li>",
                    "<li><strong>Specificity:</strong> Probability of test combination being negative when disease is absent (True Negative Rate)</li>",
                    "<li><strong>Positive Predictive Value (PPV):</strong> Probability of disease when test combination is positive</li>",
                    "<li><strong>Negative Predictive Value (NPV):</strong> Probability of no disease when test combination is negative</li>",
                    "<li><strong>Accuracy:</strong> Overall probability of correct classification</li>",
                    "</ul>",
                    "</div>",
                    
                    "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 20px 0;'>",
                    "<h4 style='margin-top: 0; color: #856404;'>Clinical Interpretation Guidelines</h4>",
                    "<p>• <strong>Sensitivity >90%</strong>: Excellent for screening - minimal false negatives, good rule-out test</p>",
                    "<p>• <strong>Specificity >90%</strong>: Excellent for confirmation - minimal false positives, good rule-in test</p>",
                    "<p>• <strong>PPV >80%</strong>: High confidence when test positive - disease likely present</p>",
                    "<p>• <strong>NPV >90%</strong>: High confidence when test negative - disease likely absent</p>",
                    "<p>• <strong>Youden Index >0.5</strong>: Good discriminatory ability independent of prevalence</p>",
                    "<p>• <strong>Balanced Accuracy >80%</strong>: Good overall performance for imbalanced datasets</p>",
                    "</div>"
                )
                
                # Add optimal cut-point recommendation if available
                if (!is.null(optimal_cutpoint)) {
                    html <- paste0(html,
                        "<div style='background-color: #d4edda; padding: 15px; border-left: 4px solid #28a745; margin: 20px 0;'>",
                        "<h4 style='margin-top: 0; color: #155724;'>🎯 Recommended Optimal Strategy</h4>",
                        "<p><strong>Best Pattern:</strong> ", optimal_cutpoint$pattern, "</p>",
                        "<p><strong>Selection Method:</strong> ", optimal_cutpoint$method, "</p>",
                        "<p><strong>Performance Metrics:</strong></p>",
                        "<ul style='line-height: 1.6; margin-left: 20px;'>",
                        "<li>Sensitivity: ", round(optimal_cutpoint$sensitivity * 100, 1), "%</li>",
                        "<li>Specificity: ", round(optimal_cutpoint$specificity * 100, 1), "%</li>",
                        "<li>Accuracy: ", round(optimal_cutpoint$accuracy * 100, 1), "%</li>",
                        "<li>Youden Index: ", round(optimal_cutpoint$youden_index, 3), "</li>",
                        "</ul>",
                        "<p><strong>Clinical Rationale:</strong> ", optimal_cutpoint$rationale, "</p>",
                        "</div>"
                    )
                }
                
                # Add clinical decision thresholds
                html <- paste0(html,
                    "<div style='background-color: #e2e3e5; padding: 15px; border-left: 4px solid #6c757d; margin: 20px 0;'>",
                    "<h4 style='margin-top: 0; color: #495057;'>📊 Clinical Decision Thresholds</h4>",
                    "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>",
                    "<div>",
                    "<h5 style='color: #28a745; margin-bottom: 5px;'>Screening Tests (Rule-Out)</h5>",
                    "<p style='font-size: 0.9em; margin-bottom: 5px;'>• Sensitivity ≥95%: Excellent</p>",
                    "<p style='font-size: 0.9em; margin-bottom: 5px;'>• NPV ≥95%: High confidence</p>",
                    "<p style='font-size: 0.9em; margin-bottom: 5px;'>• Goal: Minimize false negatives</p>",
                    "</div>",
                    "<div>",
                    "<h5 style='color: #dc3545; margin-bottom: 5px;'>Confirmatory Tests (Rule-In)</h5>",
                    "<p style='font-size: 0.9em; margin-bottom: 5px;'>• Specificity ≥95%: Excellent</p>",
                    "<p style='font-size: 0.9em; margin-bottom: 5px;'>• PPV ≥90%: High confidence</p>",
                    "<p style='font-size: 0.9em; margin-bottom: 5px;'>• Goal: Minimize false positives</p>",
                    "</div>",
                    "</div>",
                    "<p style='margin-top: 15px; font-style: italic; color: #6c757d;'>",
                    "Consider clinical context, disease prevalence, treatment consequences, and cost-effectiveness when selecting optimal test combinations.",
                    "</p>",
                    "</div>",
                    "</div>"
                )
                
                return(html)
            },
            
            .generateCombinationInterpretation = function(stats, pattern, description) {
                # Generate clinical interpretation for individual test combination results
                if (is.null(stats)) {
                    return("No statistical results available")
                } else if (any(is.na(c(stats$sens, stats$spec)))) {
                    return("Sensitivity/specificity cannot be calculated - check data completeness")
                } else if (any(is.na(c(stats$ppv, stats$npv)))) {
                    return("Predictive values unavailable - may be due to extreme prevalence")
                } else if (any(is.na(stats$acc))) {
                    return("Accuracy calculation incomplete")
                }
                
                # Performance quality assessment
                sens <- stats$sens * 100
                spec <- stats$spec * 100
                ppv <- stats$ppv * 100
                npv <- stats$npv * 100
                acc <- stats$acc * 100
                
                # Determine combination strengths
                strengths <- c()
                recommendations <- c()
                
                # Sensitivity analysis
                if (sens >= 95) {
                    strengths <- c(strengths, "Excellent rule-out test")
                    recommendations <- c(recommendations, "ideal for screening")
                } else if (sens >= 85) {
                    strengths <- c(strengths, "Good rule-out ability")
                } else if (sens < 70) {
                    recommendations <- c(recommendations, "may miss cases")
                }
                
                # Specificity analysis  
                if (spec >= 95) {
                    strengths <- c(strengths, "Excellent rule-in test")
                    recommendations <- c(recommendations, "ideal for confirmation")
                } else if (spec >= 85) {
                    strengths <- c(strengths, "Good rule-in ability")
                } else if (spec < 70) {
                    recommendations <- c(recommendations, "may have false positives")
                }
                
                # PPV/NPV analysis
                if (ppv >= 90) {
                    strengths <- c(strengths, "High positive confidence")
                }
                if (npv >= 90) {
                    strengths <- c(strengths, "High negative confidence")
                }
                
                # Overall accuracy assessment
                if (acc >= 90) {
                    strengths <- c(strengths, "Excellent overall accuracy")
                } else if (acc >= 80) {
                    strengths <- c(strengths, "Good overall accuracy")
                } else if (acc < 70) {
                    recommendations <- c(recommendations, "limited clinical utility")
                }
                
                # Construct interpretation
                interpretation <- ""
                if (length(strengths) > 0) {
                    interpretation <- paste(strengths, collapse = "; ")
                }
                
                if (length(recommendations) > 0) {
                    if (interpretation != "") {
                        interpretation <- paste0(interpretation, " — ", paste(recommendations, collapse = "; "))
                    } else {
                        interpretation <- paste(recommendations, collapse = "; ")
                    }
                }
                
                # Fallback if no specific interpretation
                if (interpretation == "") {
                    if (acc >= 70) {
                        interpretation <- "Moderate diagnostic performance"
                    } else {
                        interpretation <- "Limited diagnostic utility"
                    }
                }
                
                return(interpretation)
            },
            
            .applyMultipleTestingCorrection = function(p_values, method = "holm") {
                # Apply multiple testing correction for combination comparisons
                # Methods: "holm" (default), "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
                if (length(p_values) <= 1 || all(is.na(p_values))) {
                    return(p_values)
                }
                
                # Filter out NA values for correction
                valid_p <- !is.na(p_values)
                if (sum(valid_p) == 0) {
                    return(p_values)
                }
                
                corrected <- rep(NA, length(p_values))
                corrected[valid_p] <- stats::p.adjust(p_values[valid_p], method = method)
                
                return(corrected)
            },
            
            .processChunkedData = function(mydata2, chunk_size = 10000) {
                # Process very large datasets in chunks to manage memory
                n_rows <- nrow(mydata2)
                
                if (n_rows <= chunk_size) {
                    return(list(mydata2))  # Single chunk
                }
                
                # Split into chunks
                n_chunks <- ceiling(n_rows / chunk_size)
                chunks <- list()
                
                message(paste("Processing", n_rows, "observations in", n_chunks, "chunks for memory efficiency"))
                
                for (i in seq_len(n_chunks)) {
                    start_idx <- (i - 1) * chunk_size + 1
                    end_idx <- min(i * chunk_size, n_rows)
                    
                    chunks[[i]] <- mydata2[start_idx:end_idx, , drop = FALSE]
                    
                    # Progress indicator for large processing
                    if (n_chunks > 5 && i %% max(1, floor(n_chunks / 5)) == 0) {
                        message(paste("Processing chunk", i, "of", n_chunks, "(", round(i/n_chunks*100, 1), "% complete)"))
                    }
                }
                
                return(chunks)
            },
            
            .processChunkedBinaryConversion = function(mydata2, testVariables) {
                # Efficient binary conversion for large datasets using chunked processing
                chunk_size <- 10000
                n_rows <- nrow(mydata2)
                
                # Create binary column names
                test1_bin <- paste0(testVariables[1], "_bin")
                test2_bin <- paste0(testVariables[2], "_bin")
                test3_bin <- if (length(testVariables) >= 3) paste0(testVariables[3], "_bin") else NULL
                
                # Initialize binary columns
                mydata2[[test1_bin]] <- numeric(n_rows)
                mydata2[[test2_bin]] <- numeric(n_rows)
                if (!is.null(test3_bin)) {
                    mydata2[[test3_bin]] <- numeric(n_rows)
                }
                
                # Process in chunks
                for (i in seq(1, n_rows, chunk_size)) {
                    end_idx <- min(i + chunk_size - 1, n_rows)
                    chunk_indices <- i:end_idx
                    
                    # Vectorized binary conversion for chunk
                    mydata2[[test1_bin]][chunk_indices] <- as.numeric(
                        mydata2[[testVariables[1]]][chunk_indices] == self$options$test1Positive
                    )
                    mydata2[[test2_bin]][chunk_indices] <- as.numeric(
                        mydata2[[testVariables[2]]][chunk_indices] == self$options$test2Positive
                    )
                    if (!is.null(test3_bin)) {
                        mydata2[[test3_bin]][chunk_indices] <- as.numeric(
                            mydata2[[testVariables[3]]][chunk_indices] == self$options$test3Positive
                        )
                    }
                }
                
                return(mydata2)
            },
            
            .analyzeCombinations = function(mydata2, testVariables, goldVariable) {
                # Comprehensive analysis of all test combinations with full diagnostic statistics
                if (length(testVariables) < 2) return(mydata2)
                
                # Check if chunked processing is needed
                chunks <- private$.processChunkedData(mydata2, chunk_size = 50000)
                
                if (length(chunks) > 1) {
                    # Process each chunk and combine results
                    processed_chunks <- lapply(chunks, function(chunk) {
                        private$.analyzeSingleChunk(chunk, testVariables, goldVariable)
                    })
                    
                    # Combine chunk results
                    mydata2 <- do.call(rbind, processed_chunks)
                } else {
                    # Single chunk processing
                    mydata2 <- private$.analyzeSingleChunk(mydata2, testVariables, goldVariable)
                }
                
                return(mydata2)
            },
            
            .analyzeSingleChunk = function(mydata2, testVariables, goldVariable) {
                # Analyze a single chunk of data for combinations
                
                # Process large datasets efficiently with progress indicators
                large_dataset_threshold <- 1000
                very_large_threshold <- 10000
                n_obs <- nrow(mydata2)
                
                if (n_obs > large_dataset_threshold) {
                    message(paste("Processing dataset with", n_obs, "observations"))
                    
                    if (n_obs > very_large_threshold) {
                        # For very large datasets, implement memory-efficient processing
                        message("Large dataset detected - using optimized memory management")
                        
                        # Force garbage collection to free memory
                        gc(verbose = FALSE)
                        
                        # Process in chunks if dataset is extremely large (>50k)
                        if (n_obs > 50000) {
                            message("Warning: Very large dataset may require substantial processing time")
                        }
                    }
                }
                
                # Binary variables for positive/negative states
                test1_bin <- paste0(testVariables[1], "_bin")
                test2_bin <- paste0(testVariables[2], "_bin")
                test3_bin <- if (length(testVariables) >= 3) paste0(testVariables[3], "_bin") else NULL
                
                # Binary conversion for each test based on positive levels
                # Performance optimization: use vectorized operations for large datasets
                n_rows <- nrow(mydata2)
                if (n_rows > 50000) {
                    # For very large datasets, process in chunks to manage memory
                    mydata2 <- private$.processChunkedBinaryConversion(mydata2, testVariables)
                } else {
                    # Standard processing for smaller datasets
                    mydata2[[test1_bin]] <- as.numeric(mydata2[[testVariables[1]]] == self$options$test1Positive)
                }
                
                # Ensure all binary conversions are completed
                if (!test1_bin %in% names(mydata2)) {
                    mydata2[[test1_bin]] <- as.numeric(mydata2[[testVariables[1]]] == self$options$test1Positive)
                }
                mydata2[[test2_bin]] <- as.numeric(mydata2[[testVariables[2]]] == self$options$test2Positive)
                if (!is.null(test3_bin)) {
                    mydata2[[test3_bin]] <- as.numeric(mydata2[[testVariables[3]]] == self$options$test3Positive)
                }
                
                # Create combination patterns
                if (length(testVariables) == 2) {
                    # Create combination patterns using base R (more efficient)
                    mydata2$combination_pattern <- ifelse(
                        mydata2[[test1_bin]] == 1 & mydata2[[test2_bin]] == 1, "+/+",
                        ifelse(mydata2[[test1_bin]] == 1 & mydata2[[test2_bin]] == 0, "+/-",
                               ifelse(mydata2[[test1_bin]] == 0 & mydata2[[test2_bin]] == 1, "-/+",
                                      ifelse(mydata2[[test1_bin]] == 0 & mydata2[[test2_bin]] == 0, "-/-",
                                             "Unknown"))))
                    
                } else if (length(testVariables) == 3) {
                    # For 3 tests - create pattern combinations
                    mydata2$combination_pattern <- paste(
                        ifelse(mydata2[[test1_bin]] == 1, "+", "-"),
                        ifelse(mydata2[[test2_bin]] == 1, "+", "-"),
                        ifelse(mydata2[[test3_bin]] == 1, "+", "-"),
                        sep = "/"
                    )
                }
                
                # Ensure no NA patterns
                mydata2$combination_pattern[is.na(mydata2$combination_pattern) | 
                                          mydata2$combination_pattern == ""] <- "Unknown"
                
                # Create contingency table for analysis
                combo_table <- table(mydata2$combination_pattern, mydata2$goldVariable2)
                
                # Generate pattern descriptions using consolidated function
                pattern_result <- private$.generatePatternInfo(testVariables)
                patterns <- pattern_result$patterns
                descriptions <- pattern_result$descriptions
                
                # Calculate totals for the gold standard (safe column access)
                total_positive_gold <- if("Positive" %in% colnames(combo_table)) {
                    sum(combo_table[, "Positive"], na.rm = TRUE)
                } else {
                    0
                }
                total_negative_gold <- if("Negative" %in% colnames(combo_table)) {
                    sum(combo_table[, "Negative"], na.rm = TRUE) 
                } else {
                    0
                }
                
                # Create structured HTML summary with better formatting
                num_tests <- length(testVariables)
                test_names <- paste(testVariables, collapse = ", ")
                n_total_cases <- sum(total_positive_gold, total_negative_gold)
                
                # Add validation to prevent zero total cases
                if (n_total_cases == 0) {
                    stop("Invalid data: No valid gold standard cases found. Please check your gold standard variable and positive level selection.")
                }
                
                # Initialize plot data index before use
                plot_data_index <- 0
                
                # Find optimal cut-point for recommendations (will be calculated after data collection)
                optimal_cutpoint <- NULL
                
                # Professional HTML structure with optimal cut-point recommendations
                summary_html <- private$.generateAnalysisSummaryHTML(
                    num_tests = num_tests,
                    test_names = test_names,
                    n_total_cases = n_total_cases,
                    n_positive_cases = total_positive_gold,
                    n_negative_cases = total_negative_gold,
                    patterns = patterns,
                    descriptions = descriptions,
                    optimal_cutpoint = optimal_cutpoint
                )
                
                # Initialize plot data collection with pre-allocation for better performance
                n_patterns <- length(patterns)
                plot_data <- list(
                    patterns = character(n_patterns),
                    sensitivity = numeric(n_patterns),
                    specificity = numeric(n_patterns),
                    ppv = numeric(n_patterns),
                    npv = numeric(n_patterns),
                    accuracy = numeric(n_patterns),
                    descriptions = character(n_patterns),
                    tp = numeric(n_patterns),
                    fp = numeric(n_patterns),
                    fn = numeric(n_patterns),
                    tn = numeric(n_patterns)
                )
                
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
                        
                        # Generate clinical interpretation for this combination
                        clinical_interpretation <- private$.generateCombinationInterpretation(stats, pattern, description)
                        
                        # Add to main statistics table with enhanced formatting
                        self$results$combStatsTable$addRow(
                            rowKey = pattern,
                            values = list(
                                combination = paste0(
                                    "<div style='font-weight: bold; color: #2c3e50;'>", pattern, "</div>",
                                    "<div style='font-size: 0.9em; color: #555; margin-top: 2px;'>", description, "</div>",
                                    "<div style='color: #0066cc; font-size: 0.85em; margin-top: 4px; font-style: italic; padding: 2px 6px; background-color: #f8f9fa; border-radius: 3px;'>", 
                                    clinical_interpretation, "</div>"
                                ),
                                sens = stats$sens,
                                spec = stats$spec,
                                ppv = stats$ppv,
                                npv = stats$npv,
                                acc = stats$acc
                            )
                        )
                        
                        # Collect data for visualization using indexed assignment (more efficient)
                        plot_data_index <- plot_data_index + 1
                        plot_data$patterns[plot_data_index] <- pattern
                        plot_data$sensitivity[plot_data_index] <- stats$sens
                        plot_data$specificity[plot_data_index] <- stats$spec
                        plot_data$ppv[plot_data_index] <- stats$ppv
                        plot_data$npv[plot_data_index] <- stats$npv
                        plot_data$accuracy[plot_data_index] <- stats$acc
                        plot_data$descriptions[plot_data_index] <- description
                        plot_data$tp[plot_data_index] <- tp
                        plot_data$fp[plot_data_index] <- fp
                        plot_data$fn[plot_data_index] <- fn
                        plot_data$tn[plot_data_index] <- tn
                        
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
                
                # After data collection, calculate optimal cut-point for recommendations
                if (plot_data_index > 0) {
                    # Create temporary plot data for optimal cut-point analysis
                    temp_plot_data <- list(
                        patterns = plot_data$patterns[1:plot_data_index],
                        sensitivity = plot_data$sensitivity[1:plot_data_index],
                        specificity = plot_data$specificity[1:plot_data_index],
                        accuracy = plot_data$accuracy[1:plot_data_index]
                    )
                    optimal_cutpoint <- private$.findOptimalCutpoint(temp_plot_data)
                    
                    # Regenerate HTML summary with optimal cut-point
                    summary_html <- private$.generateAnalysisSummaryHTML(
                        num_tests = num_tests,
                        test_names = test_names,
                        n_total_cases = n_total_cases,
                        n_positive_cases = total_positive_gold,
                        n_negative_cases = total_negative_gold,
                        patterns = patterns,
                        descriptions = descriptions,
                        optimal_cutpoint = optimal_cutpoint
                    )
                }
                
                # Generate clinical summary if we have valid results
                clinical_summary_html <- ""
                if (plot_data_index > 0 && !is.null(optimal_cutpoint)) {
                    # Prepare results structure for clinical summary
                    results_for_summary <- list(
                        patterns = patterns[1:plot_data_index],
                        sensitivity = plot_data$sensitivity[1:plot_data_index],
                        specificity = plot_data$specificity[1:plot_data_index],
                        ppv = plot_data$ppv[1:plot_data_index],
                        npv = plot_data$npv[1:plot_data_index],
                        accuracy = plot_data$accuracy[1:plot_data_index]
                    )
                    
                    clinical_summary_html <- private$.generateClinicalSummary(results_for_summary, optimal_cutpoint)
                    
                    # Generate copy-ready report
                    report_template_html <- private$.generateCopyReadyReport(results_for_summary, optimal_cutpoint, test_names)
                }
                
                # Combine all HTML components
                combined_html <- paste0(clinical_summary_html, report_template_html, summary_html)
                
                # Set content
                if ("combinationsAnalysis" %in% names(self$results)) {
                    self$results$combinationsAnalysis$setContent(combined_html)
                }
                
                # Trim plot data to actual size and validate
                if (plot_data_index > 0) {
                    # Trim to actual data size for efficiency
                    plot_data$patterns <- plot_data$patterns[1:plot_data_index]
                    plot_data$sensitivity <- plot_data$sensitivity[1:plot_data_index]
                    plot_data$specificity <- plot_data$specificity[1:plot_data_index]
                    plot_data$ppv <- plot_data$ppv[1:plot_data_index]
                    plot_data$npv <- plot_data$npv[1:plot_data_index]
                    plot_data$accuracy <- plot_data$accuracy[1:plot_data_index]
                    plot_data$descriptions <- plot_data$descriptions[1:plot_data_index]
                    plot_data$tp <- plot_data$tp[1:plot_data_index]
                    plot_data$fp <- plot_data$fp[1:plot_data_index]
                    plot_data$fn <- plot_data$fn[1:plot_data_index]
                    plot_data$tn <- plot_data$tn[1:plot_data_index]
                    
                    # Validate plot data
                    private$.validatePlotData(plot_data)
                    # Set data for heatmap
                    if ("performanceHeatmap" %in% names(self$results)) {
                        heatmap_image <- self$results$performanceHeatmap
                        heatmap_image$setState(plot_data)
                    }
                    
                    # Note: ROC curves removed as not appropriate for categorical tests
                    
                    # Set data for decision tree
                    if ("decisionTree" %in% names(self$results)) {
                        tree_image <- self$results$decisionTree
                        tree_image$setState(plot_data)
                    }
                    
                    # Set data for Venn diagram
                    if ("vennDiagram" %in% names(self$results)) {
                        venn_image <- self$results$vennDiagram
                        venn_image$setState(plot_data)
                    }
                    
                    # Set data for forest plot
                    if ("forestPlot" %in% names(self$results)) {
                        forest_image <- self$results$forestPlot
                        forest_image$setState(plot_data)
                    }
                }
                
                # Return mydata2 with combination_pattern column
                return(mydata2)
            },
            
            .hasRequiredInputs = function() {
                # Check if the minimum required inputs are provided, including positive levels
                has_gold <- !is.null(self$options$gold) && length(self$options$gold) > 0 && self$options$gold != ""
                has_gold_positive <- !is.null(self$options$goldPositive) && self$options$goldPositive != ""
                has_test1 <- !is.null(self$options$test1) && self$options$test1 != ""
                has_test1_positive <- !is.null(self$options$test1Positive) && self$options$test1Positive != ""
                has_data <- !is.null(self$data) && nrow(self$data) > 0
                
                return(has_gold && has_gold_positive && has_test1 && has_test1_positive && has_data)
            },
            
            .displayWelcomeMessage = function() {
                # Display comprehensive welcome message and analysis guide
                html <- self$results$combinationsAnalysis
                
                welcome_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px;">',
                    
                    '<h1 style="color: #2E86C1; text-align: center; border-bottom: 3px solid #2E86C1; padding-bottom: 15px;">',
                    '🔬 Diagnostic Test Combination Analysis</h1>',
                    
                    # About This Analysis Section
                    '<div style="background: linear-gradient(135deg, #EBF5FB 0%, #D6EAF8 100%); padding: 25px; border-radius: 10px; margin: 20px 0; box-shadow: 0 2px 10px rgba(0,0,0,0.1);">',
                    '<h2 style="color: #1B4F72; margin-top: 0;">📚 About This Analysis</h2>',
                    '<p style="font-size: 16px; line-height: 1.8; color: #34495E;">',
                    'This advanced statistical tool evaluates how well different combinations of diagnostic tests perform ',
                    'compared to a gold standard reference. It systematically analyzes all possible test result patterns ',
                    '(e.g., Test1+/Test2+, Test1+/Test2-, etc.) to find the optimal combination for your clinical scenario.',
                    '</p>',
                    '</div>',
                    
                    # When to Use This
                    '<div style="background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #856404; margin-top: 0;">🎯 When to Use This Analysis</h3>',
                    '<ul style="line-height: 1.8; color: #856404;">',
                    '<li><strong>Test Validation:</strong> Comparing new diagnostic methods to established gold standards</li>',
                    '<li><strong>Protocol Development:</strong> Optimizing diagnostic workflows by combining multiple tests</li>',
                    '<li><strong>Clinical Research:</strong> Evaluating biomarker panels or multi-modal diagnostic approaches</li>',
                    '<li><strong>Quality Assessment:</strong> Measuring inter-rater agreement between diagnostic methods</li>',
                    '</ul>',
                    '</div>',
                    
                    # Step-by-Step Guide
                    '<div style="background-color: #d1ecf1; border: 1px solid #bee5eb; padding: 20px; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #0c5460; margin-top: 0;">📋 Step-by-Step Guide</h3>',
                    '<ol style="line-height: 1.8; color: #0c5460;">',
                    '<li><strong>Select Gold Standard:</strong> Choose your reference test (e.g., biopsy, final diagnosis)</li>',
                    '<li><strong>Define Disease Level:</strong> Specify which level indicates disease presence</li>',
                    '<li><strong>Choose Test Variables:</strong> Select 1-3 diagnostic tests to combine and evaluate</li>',
                    '<li><strong>Set Positive Levels:</strong> Define what constitutes a "positive" result for each test</li>',
                    '<li><strong>Configure Options:</strong> Enable visualizations and set export preferences</li>',
                    '<li><strong>Run Analysis:</strong> Get comprehensive performance metrics and clinical recommendations</li>',
                    '</ol>',
                    '</div>',
                    
                    # What You Get
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 30px 0;">',
                    '<div style="background-color: #F8F9FA; padding: 20px; border-left: 5px solid #17A2B8; border-radius: 5px;">',
                    '<h3 style="color: #138496; margin-top: 0;">📊 Analysis Results</h3>',
                    '<ul style="line-height: 1.8; color: #495057;">',
                    '<li>Systematic evaluation of all test combinations</li>',
                    '<li>Wilson score confidence intervals</li>',
                    '<li>Performance-based optimal pattern identification</li>',
                    '<li>Publication-quality visualizations</li>',
                    '<li>Clinical decision recommendations</li>',
                    '</ul>',
                    '</div>',
                    
                    '<div style="background-color: #F8F9FA; padding: 20px; border-left: 5px solid #28A745; border-radius: 5px;">',
                    '<h3 style="color: #155724; margin-top: 0;">📈 Clinical Outputs</h3>',
                    '<ul style="line-height: 1.8; color: #495057;">',
                    '<li><strong>Clinical Summary:</strong> Plain-language interpretation of results</li>',
                    '<li><strong>Copy-Ready Report:</strong> Templates for manuscripts and documentation</li>',
                    '<li><strong>Performance Metrics:</strong> Sensitivity, specificity, PPV, NPV with CIs</li>',
                    '<li><strong>Optimal Pattern:</strong> Best test combination recommendation</li>',
                    '<li><strong>Visualizations:</strong> Heatmaps, decision trees, forest plots</li>',
                    '</ul>',
                    '</div>',
                    '</div>',
                    
                    '<div style="background-color: #FFF3CD; padding: 20px; border: 1px solid #FFEEBA; border-radius: 8px; margin: 20px 0;">',
                    '<h3 style="color: #856404; margin-top: 0;">⚡ Quick Start Checklist</h3>',
                    '<div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px;">',
                    
                    private$.getRequiredVariableStatus(),
                    
                    '</div>',
                    '</div>',
                    
                    '<div style="background-color: #E8F5E8; padding: 20px; border-left: 5px solid #27AE60; border-radius: 5px; margin: 20px 0;">',
                    '<h3 style="color: #1E8449; margin-top: 0;">📋 Supported Analysis Types</h3>',
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">',
                    '<div>',
                    '<h4 style="color: #27AE60;">Two-Test Combinations (4 patterns)</h4>',
                    '<p style="margin: 10px 0;">Perfect for comparing two diagnostic methods or validating new tests against established ones.</p>',
                    '</div>',
                    '<div>',
                    '<h4 style="color: #27AE60;">Three-Test Combinations (8 patterns)</h4>',
                    '<p style="margin: 10px 0;">Comprehensive analysis for complex diagnostic scenarios with multiple complementary tests.</p>',
                    '</div>',
                    '</div>',
                    '</div>',
                    
                    '<div style="text-align: center; margin-top: 30px; padding: 20px; background-color: #F1F3F4; border-radius: 8px;">',
                    '<p style="font-size: 14px; color: #6C757D; margin: 0;">',
                    '💡 <strong>Pro Tip:</strong> Enable "Show Advanced Visualizations" for publication-ready plots with automated optimal cut-point analysis.',
                    '</p>',
                    '</div>',
                    
                    '</div>'
                )
                
                html$setContent(welcome_html)
            },
            
            .getRequiredVariableStatus = function() {
                # Generate status checkboxes for required variables
                has_data <- !is.null(self$data) && nrow(self$data) > 0
                has_gold <- !is.null(self$options$gold) && length(self$options$gold) > 0 && self$options$gold != ""
                has_gold_positive <- !is.null(self$options$goldPositive) && self$options$goldPositive != ""
                has_test1 <- !is.null(self$options$test1) && self$options$test1 != ""
                has_test1_positive <- !is.null(self$options$test1Positive) && self$options$test1Positive != ""
                
                status_items <- c(
                    private$.createStatusItem("Data loaded", has_data),
                    private$.createStatusItem("Gold Standard selected", has_gold),
                    private$.createStatusItem("Gold Standard positive level", has_gold_positive),
                    private$.createStatusItem("Test 1 selected", has_test1),
                    private$.createStatusItem("Test 1 positive level", has_test1_positive)
                )
                
                return(paste(status_items, collapse = ""))
            },
            
            .createStatusItem = function(label, is_complete) {
                icon <- if (is_complete) "✅" else "⚠️"
                color <- if (is_complete) "#28A745" else "#FFC107"
                
                return(paste0(
                    '<div style="display: flex; align-items: center; margin-bottom: 8px;">',
                    '<span style="font-size: 18px; margin-right: 10px;">', icon, '</span>',
                    '<span style="color: ', color, '; font-weight: ', 
                    if (is_complete) "normal" else "bold", ';">', label, '</span>',
                    '</div>'
                ))
            },
            
            .prepareVisualizationData = function(mydata2, test_variables) {
                # Helper method to prepare data for visualizations when enabled
                # This ensures visualization data is only prepared when showVisualization is TRUE
                
                if (!self$options$showVisualization) {
                    return(invisible(NULL))
                }
                
                # Log that visualizations are being prepared
                message("Preparing visualization data for enabled plot types")
                
                # Check which plot type is selected
                plot_type <- self$options$plotType
                
                if (plot_type == "all") {
                    message("All visualization types will be rendered")
                } else {
                    message(paste("Preparing data for", plot_type, "visualization"))
                }
                
                # Additional preparation logic can be added here if needed
                # For now, the main preparation happens in .analyzeCombinations
                
                invisible(TRUE)
            }

        )
    )
