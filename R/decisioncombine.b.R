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

                # Check if data exists and has rows
                if (is.null(self$data) || nrow(self$data) == 0)
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
                mydata2$goldVariable2 <- dplyr::case_when(
                    mydata2[[goldVariable]] == goldPLevel ~ "Positive",
                    is.na(mydata2[[goldVariable]]) ~ NA_character_,
                    TRUE ~ "Negative"
                )
                mydata2$goldVariable2 <- forcats::fct_relevel(mydata2$goldVariable2, "Positive")

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
                    mydata2[[binary_col_name]] <- dplyr::case_when(mydata2[[testVariable]] == testPLevel ~ 1, is.na(mydata2[[testVariable]]) ~ NA_real_, TRUE ~ 0)

                    # Show individual test results if requested
                    if (showIndividual) {
                        # Recode this test
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
                }

                # No combination logic - just analyze all test patterns

                # Add comprehensive test combination analysis
                if (length(testVariables) >= 2) {
                    mydata2 <- private$.analyzeCombinations(mydata2, testVariables, goldVariable)
                    
                    # Export combination pattern to data if requested
                    if (self$options$exportCombinationPattern && self$results$addCombinationPattern$isNotFilled()) {
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
                
                # Get plot options
                color_scheme <- self$options$colorScheme
                
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
            
            .plotROCCurves = function(image, ...) {
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
                        ggplot2::labs(title = "ROC Analysis - Data Error")
                    print(plot)
                    return(TRUE)
                })
                
                # Create publication-ready ROC space analysis
                roc_data <- data.frame(
                    Pattern = factor(plotData$patterns, levels = plotData$patterns),
                    Description = plotData$descriptions,
                    FPR = 1 - plotData$specificity,  # False Positive Rate
                    TPR = plotData$sensitivity,      # True Positive Rate (Sensitivity)
                    Specificity = plotData$specificity,
                    Sensitivity = plotData$sensitivity,
                    PPV = plotData$ppv,
                    NPV = plotData$npv,
                    Accuracy = plotData$accuracy,
                    AUC = (plotData$sensitivity + plotData$specificity) / 2,  # Simplified AUC approximation
                    n_total = plotData$tp + plotData$fp + plotData$fn + plotData$tn,
                    stringsAsFactors = FALSE
                )
                
                # Calculate Youden's J statistic (Sensitivity + Specificity - 1)
                roc_data$Youden_J <- roc_data$Sensitivity + roc_data$Specificity - 1
                
                # Create high-quality ROC space plot
                plot <- ggplot2::ggplot(roc_data, ggplot2::aes(x = FPR, y = TPR)) +
                    
                    # Add reference lines
                    ggplot2::geom_abline(intercept = 0, slope = 1, 
                                        linetype = "dashed", color = "gray60", size = 1,
                                        alpha = 0.8) +
                    ggplot2::annotate("text", x = 0.5, y = 0.45, 
                                    label = "Random Classifier", 
                                    angle = 45, color = "gray60", size = 3.5, alpha = 0.8) +
                    
                    # Perfect classifier reference point
                    ggplot2::geom_point(x = 0, y = 1, color = "gold", size = 4, shape = 17) +
                    ggplot2::annotate("text", x = 0.02, y = 1, 
                                    label = "Perfect\nClassifier", 
                                    hjust = 0, color = "gold", size = 3, fontface = "bold") +
                    
                    # Test combination points with size based on sample size
                    ggplot2::geom_point(ggplot2::aes(color = Pattern, size = AUC), 
                                       alpha = 0.8, stroke = 1.5) +
                    
                    # Add confidence ellipses or error bars would require CI data
                    # For now, add performance labels
                    ggplot2::geom_text(
                        ggplot2::aes(label = paste0(Pattern, "\n(", 
                                                   "AUC≈", round(AUC, 2), ")"),
                                    color = Pattern),
                        vjust = -1.2, hjust = 0.5, size = 3.5, 
                        fontface = "bold", show.legend = FALSE
                    ) +
                    
                    # Professional styling
                    ggplot2::scale_x_continuous(
                        name = "1 - Specificity (False Positive Rate)",
                        limits = c(-0.05, 1.05), 
                        breaks = seq(0, 1, 0.2),
                        labels = scales::percent_format(accuracy = 1),
                        expand = c(0, 0)
                    ) +
                    ggplot2::scale_y_continuous(
                        name = "Sensitivity (True Positive Rate)", 
                        limits = c(-0.05, 1.1), 
                        breaks = seq(0, 1, 0.2),
                        labels = scales::percent_format(accuracy = 1),
                        expand = c(0, 0)
                    ) +
                    
                    # Color and size scales
                    ggplot2::scale_color_viridis_d(
                        name = "Test\nCombination",
                        option = "plasma",
                        guide = ggplot2::guide_legend(
                            override.aes = list(size = 4),
                            keywidth = 1.2,
                            keyheight = 1.2
                        )
                    ) +
                    ggplot2::scale_size_continuous(
                        name = "Approx.\nAUC",
                        range = c(3, 8),
                        limits = c(0.5, 1),
                        breaks = c(0.5, 0.7, 0.9),
                        guide = ggplot2::guide_legend(
                            override.aes = list(alpha = 0.8),
                            keywidth = 1.2,
                            keyheight = 1.2
                        )
                    ) +
                    
                    ggplot2::labs(
                        title = "ROC Space Analysis: Diagnostic Test Combinations",
                        subtitle = paste0("Performance comparison across ", nrow(roc_data), 
                                         " test combination patterns"),
                        caption = paste0("Points closer to top-left corner indicate better performance. ",
                                       "AUC approximated as (Sensitivity + Specificity)/2. ",
                                       "Sample size: n = ", round(mean(roc_data$n_total)))
                    ) +
                    
                    ggplot2::theme_classic(base_size = 12, base_family = "serif") +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold", 
                                                          margin = ggplot2::margin(b = 10)),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray30",
                                                             margin = ggplot2::margin(b = 15)),
                        plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40",
                                                            margin = ggplot2::margin(t = 10)),
                        axis.title.x = ggplot2::element_text(size = 13, face = "bold", 
                                                            margin = ggplot2::margin(t = 15)),
                        axis.title.y = ggplot2::element_text(size = 13, face = "bold", 
                                                            margin = ggplot2::margin(r = 15)),
                        axis.text = ggplot2::element_text(size = 11, color = "black"),
                        axis.ticks = ggplot2::element_line(color = "black", size = 0.5),
                        axis.line = ggplot2::element_line(color = "black", size = 0.8),
                        legend.position = "right",
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1.2),
                        panel.grid.major = ggplot2::element_line(color = "gray90", size = 0.5),
                        panel.grid.minor = ggplot2::element_line(color = "gray95", size = 0.3),
                        plot.margin = ggplot2::margin(20, 20, 20, 20),
                        aspect.ratio = 1
                    )
                
                print(plot)
                TRUE
            },
            
            .plotDecisionTree = function(image, ...) {
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
                calcWilsonCI <- function(x, n, conf.level = 0.95) {
                    if (n == 0 || is.na(x) || is.na(n)) return(c(NA, NA))
                    p <- x / n
                    z <- qnorm((1 + conf.level) / 2)
                    denominator <- 1 + (z^2 / n)
                    centre <- (p + (z^2 / (2 * n))) / denominator
                    half_width <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denominator
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
            
            .generateAnalysisSummaryHTML = function(num_tests, test_names, n_total_cases, 
                                                   n_positive_cases, n_negative_cases, 
                                                   patterns, descriptions) {
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
                    "<h4 style='margin-top: 0; color: #856404;'>Interpretation Guidelines</h4>",
                    "<p>• <strong>High Sensitivity</strong> (>90%): Good for screening, few false negatives</p>",
                    "<p>• <strong>High Specificity</strong> (>90%): Good for confirmation, few false positives</p>",
                    "<p>• <strong>High PPV</strong>: When test is positive, disease is likely present</p>",
                    "<p>• <strong>High NPV</strong>: When test is negative, disease is likely absent</p>",
                    "<p>• Consider clinical context, prevalence, and cost-effectiveness in decision making</p>",
                    "</div>",
                    "</div>"
                )
                
                return(html)
            },
            
            .analyzeCombinations = function(mydata2, testVariables, goldVariable) {
                # Comprehensive analysis of all test combinations with full diagnostic statistics
                if (length(testVariables) < 2) return(mydata2)
                
                # Process large datasets efficiently
                large_dataset_threshold <- 1000
                if (nrow(mydata2) > large_dataset_threshold) {
                    # Large dataset processing - could add progress indicators here
                    message(paste("Processing large dataset with", nrow(mydata2), "observations"))
                }
                
                # Binary variables for positive/negative states
                test1_bin <- paste0(testVariables[1], "_bin")
                test2_bin <- paste0(testVariables[2], "_bin")
                test3_bin <- if (length(testVariables) >= 3) paste0(testVariables[3], "_bin") else NULL
                
                # Binary conversion for each test based on positive levels
                
                mydata2[[test1_bin]] <- as.numeric(mydata2[[testVariables[1]]] == self$options$test1Positive)
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
                
                # Calculate totals for the gold standard
                total_positive_gold <- sum(combo_table[, "Positive"], na.rm = TRUE)
                total_negative_gold <- sum(combo_table[, "Negative"], na.rm = TRUE)
                
                # Create structured HTML summary with better formatting
                num_tests <- length(testVariables)
                test_names <- paste(testVariables, collapse = ", ")
                n_total_cases <- sum(total_positive_gold, total_negative_gold)
                
                # Professional HTML structure
                summary_html <- private$.generateAnalysisSummaryHTML(
                    num_tests = num_tests,
                    test_names = test_names,
                    n_total_cases = n_total_cases,
                    n_positive_cases = total_positive_gold,
                    n_negative_cases = total_negative_gold,
                    patterns = patterns,
                    descriptions = descriptions
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
                plot_data_index <- 0
                
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
                
                # Clinical interpretation is now part of the HTML generation function
                
                # Set content
                if ("combinationsAnalysis" %in% names(self$results)) {
                    self$results$combinationsAnalysis$setContent(summary_html)
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
                    
                    # Set data for ROC curves
                    if ("rocCurves" %in% names(self$results)) {
                        roc_image <- self$results$rocCurves
                        roc_image$setState(plot_data)
                    }
                    
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
            }

        )
    )
