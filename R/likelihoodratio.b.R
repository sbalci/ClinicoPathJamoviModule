# Likelihood Ratio Analysis - Backend Implementation
likelihoodratioClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "likelihoodratioClass",
    inherit = likelihoodratioBase,
    private = list(
        
        .init = function() {
            if (is.null(self$data))
                return()
                
            private$.initInstructions()
            private$.initClinicalInterpretation()
        },
        
        .run = function() {
            if (is.null(self$data))
                return()
                
            # Check for required variables
            if (is.null(self$options$testVariable) || is.null(self$options$referenceStandard)) {
                return()
            }
            
            data <- self$data
            opts <- self$options
            `%||%` <- function(x, y) if (is.null(x)) y else x
            
            # Extract variables
            testVar <- data[[opts$testVariable]]
            refStd <- data[[opts$referenceStandard]]
            
            # Remove missing values
            complete_cases <- complete.cases(testVar, refStd)
            testVar <- testVar[complete_cases]
            refStd <- refStd[complete_cases]
            
            if (length(testVar) == 0) {
                self$results$instructions$setContent("<p><b>Error:</b> No complete cases found.</p>")
                return()
            }
            
            # Ensure reference standard is binary
            if (is.factor(refStd)) {
                refStd_levels <- levels(refStd)
                if (length(refStd_levels) != 2) {
                    self$results$instructions$setContent("<p><b>Error:</b> Reference standard must be binary (2 levels).</p>")
                    return()
                }
                # Convert to 0/1 (0 = negative, 1 = positive)
                refStd_binary <- as.numeric(refStd) - 1
            } else {
                refStd_unique <- sort(unique(refStd))
                if (length(refStd_unique) != 2) {
                    self$results$instructions$setContent("<p><b>Error:</b> Reference standard must have exactly 2 values.</p>")
                    return()
                }
                # Map to 0/1
                refStd_binary <- ifelse(refStd == refStd_unique[2], 1, 0)
            }
            
            # Handle different analysis types
            analysisType <- opts$analysisType %||% "binary"
            
            if (analysisType == "binary") {
                private$.runBinaryAnalysis(testVar, refStd_binary)
            } else if (analysisType == "continuous") {
                private$.runContinuousAnalysis(testVar, refStd_binary)
            } else if (analysisType == "multilevel") {
                private$.runMultilevelAnalysis(testVar, refStd_binary)
            }
        },
        
        .runBinaryAnalysis = function(testVar, refStd) {
            # Convert test variable to binary if needed
            if (is.factor(testVar)) {
                test_levels <- levels(testVar)
                if (length(test_levels) != 2) {
                    self$results$instructions$setContent("<p><b>Error:</b> Binary test variable must have exactly 2 levels.</p>")
                    return()
                }
                testVar_binary <- as.numeric(testVar) - 1
            } else {
                test_unique <- sort(unique(testVar))
                if (length(test_unique) != 2) {
                    self$results$instructions$setContent("<p><b>Error:</b> Binary test variable must have exactly 2 values.</p>")
                    return()
                }
                testVar_binary <- ifelse(testVar == test_unique[2], 1, 0)
            }
            
            # Create 2x2 contingency table
            contingency <- table(testVar_binary, refStd)
            
            # Extract cell counts
            tn <- contingency[1,1]  # True negative
            fp <- contingency[1,2]  # False positive  
            fn <- contingency[2,1]  # False negative
            tp <- contingency[2,2]  # True positive
            
            # Apply continuity correction if needed
            if (any(c(tn, fp, fn, tp) == 0) && self$options$correctContinuity) {
                tn <- tn + 0.5
                fp <- fp + 0.5
                fn <- fn + 0.5
                tp <- tp + 0.5
            }
            
            # Calculate diagnostic measures
            results <- private$.calculateDiagnosticMeasures(tp, fp, fn, tn)
            
            # Populate results tables
            private$.populateTables(results, testVar, refStd)
            
        },
        
        .runContinuousAnalysis = function(testVar, refStd) {
            if (!is.numeric(testVar)) {
                self$results$instructions$setContent("<p><b>Error:</b> Test variable must be numeric for continuous analysis.</p>")
                return()
            }
            
            # Determine optimal cutpoint
            cutpoint <- private$.findOptimalCutpoint(testVar, refStd)
            
            # Convert to binary using cutpoint
            testDirection <- self$options$testDirection %||% "higher"
            if (testDirection == "higher") {
                testVar_binary <- ifelse(testVar >= cutpoint$value, 1, 0)
            } else {
                testVar_binary <- ifelse(testVar <= cutpoint$value, 1, 0)
            }
            
            # Create 2x2 table and calculate measures
            contingency <- table(testVar_binary, refStd)
            tn <- contingency[1,1]
            fp <- contingency[1,2]
            fn <- contingency[2,1]
            tp <- contingency[2,2]
            
            results <- private$.calculateDiagnosticMeasures(tp, fp, fn, tn)
            results$cutpoint <- cutpoint
            
            # Populate results
            private$.populateTables(results, testVar_binary, refStd)
            private$.populateOptimalCutpoint(cutpoint)
        },
        
        .runMultilevelAnalysis = function(testVar, refStd) {
            # Handle ordinal/multilevel test variables
            if (!is.factor(testVar)) {
                testVar <- as.factor(testVar)
            }
            
            test_levels <- levels(testVar)
            n_levels <- length(test_levels)
            
            # Calculate likelihood ratios for each level
            lr_results <- data.frame(
                level = character(n_levels),
                lr = numeric(n_levels),
                lr_lower = numeric(n_levels),
                lr_upper = numeric(n_levels),
                stringsAsFactors = FALSE
            )
            
            for (i in 1:n_levels) {
                level <- test_levels[i]
                # Positive if test result is this level
                testVar_level <- ifelse(testVar == level, 1, 0)
                
                # Calculate for this level
                contingency <- table(testVar_level, refStd)
                if (nrow(contingency) == 2 && ncol(contingency) == 2) {
                    tn <- contingency[1,1]
                    fp <- contingency[1,2]
                    fn <- contingency[2,1]
                    tp <- contingency[2,2]
                    
                    measures <- private$.calculateDiagnosticMeasures(tp, fp, fn, tn)
                    lr_results[i, ] <- c(level, measures$lr_positive, measures$lr_pos_lower, measures$lr_pos_upper)
                }
            }
            
            # Store multilevel results (simplified for this implementation)
            private$.populateMultilevelResults(lr_results)
        },
        
        .calculateDiagnosticMeasures = function(tp, fp, fn, tn) {
            # Basic diagnostic measures
            sensitivity <- tp / (tp + fn)
            specificity <- tn / (tn + fp)
            ppv <- tp / (tp + fp)
            npv <- tn / (tn + fn)
            
            # Likelihood ratios
            lr_positive <- sensitivity / (1 - specificity)
            lr_negative <- (1 - sensitivity) / specificity
            
            # Diagnostic odds ratio
            dor <- (tp * tn) / (fp * fn)
            
            # Confidence intervals
            conf_level <- self$options$confidenceLevel %||% 0.95
            alpha <- 1 - conf_level
            z <- qnorm(1 - alpha/2)
            
            # LR+ CI (log transformation)
            if (lr_positive > 0) {
                log_lr_pos <- log(lr_positive)
                se_log_lr_pos <- sqrt(1/tp + 1/fp)
                lr_pos_lower <- exp(log_lr_pos - z * se_log_lr_pos)
                lr_pos_upper <- exp(log_lr_pos + z * se_log_lr_pos)
            } else {
                lr_pos_lower <- 0
                lr_pos_upper <- Inf
            }
            
            # LR- CI
            if (lr_negative > 0) {
                log_lr_neg <- log(lr_negative)
                se_log_lr_neg <- sqrt(1/fn + 1/tn)
                lr_neg_lower <- exp(log_lr_neg - z * se_log_lr_neg)
                lr_neg_upper <- exp(log_lr_neg + z * se_log_lr_neg)
            } else {
                lr_neg_lower <- 0
                lr_neg_upper <- Inf
            }
            
            # DOR CI
            if (dor > 0) {
                log_dor <- log(dor)
                se_log_dor <- sqrt(1/tp + 1/fp + 1/fn + 1/tn)
                dor_lower <- exp(log_dor - z * se_log_dor)
                dor_upper <- exp(log_dor + z * se_log_dor)
            } else {
                dor_lower <- 0
                dor_upper <- Inf
            }
            
            list(
                tp = tp, fp = fp, fn = fn, tn = tn,
                sensitivity = sensitivity, specificity = specificity,
                ppv = ppv, npv = npv,
                lr_positive = lr_positive, lr_negative = lr_negative,
                lr_pos_lower = lr_pos_lower, lr_pos_upper = lr_pos_upper,
                lr_neg_lower = lr_neg_lower, lr_neg_upper = lr_neg_upper,
                dor = dor, dor_lower = dor_lower, dor_upper = dor_upper
            )
        },
        
        .findOptimalCutpoint = function(testVar, refStd) {
            method <- self$options$cutpointMethod %||% "youden"
            
            if (method == "manual") {
                return(list(value = self$options$manualCutpoint, method = "Manual"))
            }
            
            # Sort test values and calculate sensitivity/specificity for each
            sorted_values <- sort(unique(testVar))
            n_cutpoints <- length(sorted_values)
            
            best_cutpoint <- sorted_values[1]
            best_criterion <- -Inf
            
            for (i in 1:n_cutpoints) {
                cutpoint <- sorted_values[i]
                
                # Test both directions
                for (direction in c("higher", "lower")) {
                    if (direction == "higher") {
                        test_binary <- ifelse(testVar >= cutpoint, 1, 0)
                    } else {
                        test_binary <- ifelse(testVar <= cutpoint, 1, 0)
                    }
                    
                    # Calculate 2x2 table
                    contingency <- table(test_binary, refStd)
                    if (nrow(contingency) == 2 && ncol(contingency) == 2) {
                        tn <- contingency[1,1]
                        fp <- contingency[1,2]
                        fn <- contingency[2,1]
                        tp <- contingency[2,2]
                        
                        sens <- tp / (tp + fn)
                        spec <- tn / (tn + fp)
                        
                        # Calculate criterion based on method
                        if (method == "youden") {
                            criterion <- sens + spec - 1
                        } else if (method == "roc01") {
                            criterion <- -sqrt((1 - sens)^2 + (1 - spec)^2)
                        } else if (method == "cost") {
                            cost_ratio <- self$options$costRatio %||% 1
                            criterion <- -(fp * cost_ratio + fn)
                        } else {
                            criterion <- sens + spec - 1  # Default to Youden
                        }
                        
                        if (criterion > best_criterion) {
                            best_criterion <- criterion
                            best_cutpoint <- cutpoint
                            best_direction <- direction
                        }
                    }
                }
            }
            
            list(
                value = best_cutpoint,
                method = method,
                direction = best_direction,
                criterion = best_criterion
            )
        },
        
        .populateTables = function(results, testVar, refStd) {
            # Summary table
            if (self$options$showSummaryTable) {
                summary <- self$results$summaryTable
                summary$addRow(rowKey = 1, values = list(measure = "Sensitivity", value = results$sensitivity, 
                                     interpretation = private$.interpretSensitivity(results$sensitivity)))
                summary$addRow(rowKey = 2, values = list(measure = "Specificity", value = results$specificity,
                                     interpretation = private$.interpretSpecificity(results$specificity)))
                summary$addRow(rowKey = 3, values = list(measure = "Positive Predictive Value", value = results$ppv,
                                     interpretation = private$.interpretPPV(results$ppv)))
                summary$addRow(rowKey = 4, values = list(measure = "Negative Predictive Value", value = results$npv,
                                     interpretation = private$.interpretNPV(results$npv)))
            }
            
            # Likelihood ratios table
            if (self$options$showLikelihoodRatios) {
                lr_table <- self$results$likelihoodRatios
                lr_table$addRow(rowKey = 1, values = list(
                    ratio_type = "LR+ (Positive)",
                    value = results$lr_positive,
                    ci_lower = results$lr_pos_lower,
                    ci_upper = results$lr_pos_upper,
                    interpretation = private$.interpretLRPositive(results$lr_positive)
                ))
                lr_table$addRow(rowKey = 2, values = list(
                    ratio_type = "LR- (Negative)", 
                    value = results$lr_negative,
                    ci_lower = results$lr_neg_lower,
                    ci_upper = results$lr_neg_upper,
                    interpretation = private$.interpretLRNegative(results$lr_negative)
                ))
            }
            
            # Contingency table
            if (self$options$showCrosstabulation) {
                crosstab <- self$results$crosstabulation
                crosstab$addRow(rowKey = 1, values = list(test_result = "Positive", 
                                      disease_positive = results$tp,
                                      disease_negative = results$fp,
                                      total = results$tp + results$fp))
                crosstab$addRow(rowKey = 2, values = list(test_result = "Negative",
                                      disease_positive = results$fn, 
                                      disease_negative = results$tn,
                                      total = results$fn + results$tn))
            }
            
            # Diagnostic odds ratio
            if (self$options$showDiagnosticOdds) {
                dor_table <- self$results$diagnosticOddsRatio
                dor_table$addRow(rowKey = 1, values = list(
                    measure = "Diagnostic Odds Ratio",
                    value = results$dor,
                    ci_lower = results$dor_lower, 
                    ci_upper = results$dor_upper,
                    p_value = private$.calculateDORpValue(results)
                ))
            }
            
            # Post-test probabilities
            if (self$options$showPostTestProbs && self$options$calculatePostTest) {
                private$.calculatePostTestProbabilities(results$lr_positive, results$lr_negative)
            }
        },
        
        .populateOptimalCutpoint = function(cutpoint) {
            if (self$options$showOptimalCutpoint) {
                opt_table <- self$results$optimalCutpoint
                opt_table$addRow(rowKey = 1, values = list(
                    method = paste(cutpoint$method, "Method"),
                    cutpoint = cutpoint$value,
                    criterion_value = cutpoint$criterion
                ))
            }
        },
        
        .populateMultilevelResults = function(lr_results) {
            # Simplified multilevel results population
            if (self$options$showLikelihoodRatios) {
                lr_table <- self$results$likelihoodRatios
                for (i in 1:nrow(lr_results)) {
                    lr_table$addRow(rowKey = i, values = list(
                        ratio_type = paste("LR for", lr_results$level[i]),
                        value = as.numeric(lr_results$lr[i]),
                        ci_lower = as.numeric(lr_results$lr_lower[i]),
                        ci_upper = as.numeric(lr_results$lr_upper[i]),
                        interpretation = private$.interpretLRPositive(as.numeric(lr_results$lr[i]))
                    ))
                }
            }
        },
        
        .calculatePostTestProbabilities = function(lr_pos, lr_neg) {
            # Parse specific prevalences
            prev_string <- self$options$specificPrevalences %||% "10, 25, 50"
            prevalences <- as.numeric(strsplit(prev_string, ",\\s*")[[1]]) / 100
            
            post_table <- self$results$postTestProbabilities
            
            for (i in seq_along(prevalences)) {
                prev <- prevalences[i]
                
                # Post-test probability if positive
                post_pos <- (lr_pos * prev) / (lr_pos * prev + (1 - prev))
                
                # Post-test probability if negative  
                post_neg <- (lr_neg * prev) / (lr_neg * prev + (1 - prev))
                
                post_table$addRow(rowKey = i, values = list(
                    prevalence = prev * 100,
                    positive_test_post = post_pos * 100,
                    negative_test_post = post_neg * 100,
                    clinical_utility = private$.assessClinicalUtility(post_pos, post_neg)
                ))
            }
        },
        
        .calculateDORpValue = function(results) {
            # Chi-square test for DOR
            chi_sq <- log(results$dor)^2 / (1/results$tp + 1/results$fp + 1/results$fn + 1/results$tn)
            p_value <- 1 - pchisq(chi_sq, df = 1)
            return(p_value)
        },
        
        # Interpretation helper functions
        .interpretSensitivity = function(sens) {
            if (sens >= 0.9) return("Excellent")
            if (sens >= 0.8) return("Good")
            if (sens >= 0.7) return("Moderate")
            return("Poor")
        },
        
        .interpretSpecificity = function(spec) {
            if (spec >= 0.9) return("Excellent")
            if (spec >= 0.8) return("Good") 
            if (spec >= 0.7) return("Moderate")
            return("Poor")
        },
        
        .interpretPPV = function(ppv) {
            if (ppv >= 0.8) return("High")
            if (ppv >= 0.6) return("Moderate")
            return("Low")
        },
        
        .interpretNPV = function(npv) {
            if (npv >= 0.9) return("High")
            if (npv >= 0.8) return("Moderate")
            return("Low")
        },
        
        .interpretLRPositive = function(lr) {
            if (lr > 10) return("Large increase in disease probability")
            if (lr > 5) return("Moderate increase in disease probability")
            if (lr > 2) return("Small increase in disease probability")
            if (lr > 1) return("Minimal increase in disease probability")
            return("Decreases disease probability")
        },
        
        .interpretLRNegative = function(lr) {
            if (lr < 0.1) return("Large decrease in disease probability")
            if (lr < 0.2) return("Moderate decrease in disease probability")
            if (lr < 0.5) return("Small decrease in disease probability")
            if (lr < 1) return("Minimal decrease in disease probability")
            return("Increases disease probability")
        },
        
        .assessClinicalUtility = function(post_pos, post_neg) {
            if (post_pos > 0.8 && post_neg < 0.2) return("High utility")
            if (post_pos > 0.6 && post_neg < 0.4) return("Moderate utility")
            return("Limited utility")
        },
        
        .initInstructions = function() {
            html <- '
            <div style="background-color: #f0f8ff; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>Likelihood Ratio Analysis</h4>
            <p>This module calculates likelihood ratios and diagnostic performance measures for evaluating diagnostic tests.</p>
            
            <h5>Required Variables:</h5>
            <ul>
            <li><b>Test Variable:</b> The diagnostic test results (binary, continuous, or ordinal)</li>
            <li><b>Reference Standard:</b> The gold standard or true disease status (binary)</li>
            </ul>
            
            <h5>Analysis Types:</h5>
            <ul>
            <li><b>Binary Test:</b> Test results are already categorized (positive/negative)</li>
            <li><b>Continuous Test:</b> Numeric test results requiring optimal cutpoint determination</li>
            <li><b>Multi-level Test:</b> Ordinal test results with multiple categories</li>
            </ul>
            
            <p><em>Select your variables and analysis type to begin.</em></p>
            </div>'
            
            self$results$instructions$setContent(html)
        },
        
        .initClinicalInterpretation = function() {
            html <- '
            <div style="background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>Clinical Interpretation Guidelines</h4>
            
            <h5>Likelihood Ratios (LR):</h5>
            <ul>
            <li><b>LR+ > 10:</b> Large increase in disease probability (strong evidence)</li>
            <li><b>LR+ 5-10:</b> Moderate increase (moderate evidence)</li>
            <li><b>LR+ 2-5:</b> Small increase (weak evidence)</li>
            <li><b>LR+ 1-2:</b> Minimal change</li>
            <li><b>LR- < 0.1:</b> Large decrease in disease probability</li>
            <li><b>LR- 0.1-0.2:</b> Moderate decrease</li>
            <li><b>LR- 0.2-0.5:</b> Small decrease</li>
            </ul>
            
            <h5>Post-test Probability:</h5>
            <p>Post-test probability = (LR × Pre-test odds) / (LR × Pre-test odds + 1)</p>
            <p>Where Pre-test odds = Pre-test probability / (1 - Pre-test probability)</p>
            
            <h5>Clinical Decision Making:</h5>
            <ul>
            <li>High post-test probability (>80%): Consider treatment</li>
            <li>Low post-test probability (<20%): Rule out disease</li>
            <li>Intermediate probability: Additional testing needed</li>
            </ul>
            
            <h5>Diagnostic Odds Ratio (DOR):</h5>
            <p>DOR = (TP × TN) / (FP × FN). Higher values indicate better discriminative ability.</p>
            </div>'
            
            self$results$clinicalInterpretation$setContent(html)
        },
        
        # Plotting functions (simplified implementations)
        .plotROC = function(image, ggtheme, theme, ...) {
            # ROC curve implementation would go here
            # This is a simplified placeholder
            plot(1, type = "n", xlab = "1 - Specificity", ylab = "Sensitivity", 
                 main = "ROC Curve", xlim = c(0,1), ylim = c(0,1))
            text(0.5, 0.5, "ROC Plot\n(Implementation needed)")
        },
        
        .plotPredictiveValues = function(image, ggtheme, theme, ...) {
            # Predictive value curves implementation
            plot(1, type = "n", xlab = "Disease Prevalence", ylab = "Predictive Value",
                 main = "Predictive Value Curves", xlim = c(0,1), ylim = c(0,1))
            text(0.5, 0.5, "Predictive Value Curves\n(Implementation needed)")
        },
        
        .plotNomogram = function(image, ggtheme, theme, ...) {
            # Likelihood ratio nomogram
            plot(1, type = "n", main = "Likelihood Ratio Nomogram")
            text(1, 1, "LR Nomogram\n(Implementation needed)")
        },
        
        .plotPostTest = function(image, ggtheme, theme, ...) {
            # Post-test probability plot
            plot(1, type = "n", xlab = "Pre-test Probability", ylab = "Post-test Probability",
                 main = "Post-test Probability")
            text(1, 1, "Post-test Plot\n(Implementation needed)")
        },
        
        .plotDistributions = function(image, ggtheme, theme, ...) {
            # Test result distributions by disease status
            plot(1, type = "n", main = "Test Result Distributions by Disease Status")
            text(1, 1, "Distribution Plot\n(Implementation needed)")
        }
    )
)