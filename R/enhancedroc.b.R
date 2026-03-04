#' Enhanced ROC Analysis Class
#'
#' Comprehensive ROC analysis implementation inspired by BlueSky's createROCTable
#' with enhanced features for clinical diagnostic performance evaluation.
#'
#' @import jmvcore
#' @import pROC
#' @importFrom caret confusionMatrix
#' @importFrom stats quantile binom.test glm predict
#' @export

enhancedROCClass <- R6::R6Class(
    "enhancedROCClass",
    inherit = enhancedROCBase,
    private = list(
        .rocResults = NULL,
        .data = NULL,
        .binaryData = NULL,
        .outcome = NULL,
        .predictors = NULL,
        .rocObjects = NULL,
        .positiveClass = NULL,
        .multiClassOutcome = NULL,
        .presetConfig = NULL,
        .instructionsHtml = "",  # Accumulator for instructions panel content
        .analysisSummaryHtml = "",  # Accumulator for analysis summary content

        # Variable name escaping utility for special characters
        .escapeVar = function(x) {
            if (is.character(x)) {
                # Mirror modelbuilder/jmvcore behavior
                x <- gsub("[^A-Za-z0-9_]", "_", make.names(x))
            }
            return(x)
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
          if (is.null(text) || length(text) == 0) return("")
          text <- as.character(text)
          # Sanitize potentially dangerous characters
          text <- gsub("&", "&amp;", text, fixed = TRUE)
          text <- gsub("<", "&lt;", text, fixed = TRUE)
          text <- gsub(">", "&gt;", text, fixed = TRUE)
          text <- gsub("\"", "&quot;", text, fixed = TRUE)
          text <- gsub("'", "&#x27;", text, fixed = TRUE)
          return(text)
        },

        # Initialize notice collection list
        .noticeList = list(),

        # Add a notice to the collection
        .addNotice = function(type, title, content) {
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            return()
          }

          # Map notice types to colors and icons
          typeStyles <- list(
            ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = ""),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = ""),
            WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = ""),
            INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "")
          )

          html <- "<div style='margin: 10px 0;'>"

          for (notice in private$.noticeList) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong style='color: ", style$color, ";'>",
              style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
              "<span style='color: #374151;'>", private$.safeHtmlOutput(notice$content), "</span>",
              "</div>"
            )
          }

          html <- paste0(html, "</div>")

          self$results$results$notices$setContent(html)
        },

        .init = function() {
            # Initialize error handling
            if (exists("clinicopath_init", mode = "function")) {
                clinicopath_init("enhancedROC", self$options$clinicalContext)
            }

            private$.data <- self$data
            private$.outcome <- self$options$outcome
            private$.predictors <- self$options$predictors

            # Initialize instructions accumulator from base template
            private$.instructionsHtml <- private$.getInstructions()
            self$results$results$instructions$setContent(private$.instructionsHtml)

            # Initialize ROC objects list
            private$.rocObjects <- list()
        },

        .run = function() {
            # Reset notice list, instructions, summary, and preset config at start of every run
            private$.noticeList <- list()
            private$.instructionsHtml <- private$.getInstructions()
            private$.presetConfig <- NULL
            private$.analysisSummaryHtml <- ""

            # Check if we have required data
            if (is.null(private$.outcome) || is.null(private$.predictors) || length(private$.predictors) == 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Missing Variables",
                    content = "Please select an outcome variable and at least one predictor variable for ROC analysis. • Outcome variable: required (binary/factor). • Predictor variables: at least one numeric variable required for ROC curve calculation."
                )
                private$.renderNotices()
                return()
            }

            # Apply clinical presets if selected
            private$.applyClinicalPresets()

            # Prepare and validate data
            analysisData <- private$.prepareData()
            if (is.null(analysisData)) {
                private$.renderNotices()
                return()
            }

            # CRITICAL FIX: Store cleaned data in private$.data
            # This ensures all downstream confusion matrix and metric calculations
            # use the same sample as the ROC analysis (same NA removal, same outcome re-leveling)
            private$.data <- analysisData
            private$.binaryData <- analysisData[[private$.outcome]]

            # Checkpoint before expensive ROC analysis
            private$.checkpoint()

            # Run ROC analysis for each predictor
            private$.runROCAnalysis(analysisData)

            # Check for class imbalance
            if (self$options$detectImbalance) {
                private$.checkClassImbalance(analysisData)
                private$.populatePrecisionRecall(analysisData)
            }

            # Checkpoint after ROC analysis, before populating tables
            private$.checkpoint()

            # Populate results tables
            if (self$options$aucTable) {
                private$.populateAUCSummary()
            }

            if (self$options$optimalCutoffs) {
                private$.checkpoint()
                private$.populateOptimalCutoffs()
            }

            if (self$options$cutoffTable) {
                private$.checkpoint()
                private$.populateCutoffAnalysis()
            }

            if (self$options$diagnosticMetrics) {
                private$.checkpoint()
                private$.populateDiagnosticPerformance()
            }

            if (self$options$clinicalMetrics) {
                private$.checkpoint()
                private$.populateClinicalMetrics()
            }

            if (self$options$pairwiseComparisons && self$options$analysisType == "comparative") {
                private$.checkpoint()
                private$.populateROCComparisons()
            }

            if (self$options$showMetricsDiff && self$options$analysisType == "comparative") {
                private$.checkpoint()
                private$.populateDetailedComparison()
            }

            if (self$options$statisticalComparison && self$options$analysisType == "comparative") {
                private$.checkpoint()
                private$.populateStatisticalSummary()
            }

            if (self$options$partialAuc) {
                private$.checkpoint()
                private$.populatePartialAUC()
            }

            if (self$options$crocAnalysis) {
                private$.checkpoint()
                private$.populateCROCAnalysis()
            }

            if (self$options$convexHull) {
                private$.checkpoint()
                private$.populateConvexHull()
            }

            if (self$options$comprehensive_output) {
                private$.checkpoint()
                private$.populateComprehensiveAnalysis()
                private$.populateMethodsExplanation()
            }

            if (self$options$clinical_interpretation) {
                private$.checkpoint()
                private$.populateClinicalInterpretation()
            }

            if (self$options$calibrationAnalysis) {
                private$.checkpoint()
                private$.populateCalibrationAnalysis()
            }

            if (self$options$multiClassROC) {
                private$.checkpoint()
                private$.populateMultiClassROC()
            }

            if (self$options$clinicalImpact) {
                private$.checkpoint()
                private$.populateClinicalImpact()
            }


            # Notify user about unimplemented features that are toggled on
            unimplemented <- c()
            if (isTRUE(self$options$harrellCIndex)) unimplemented <- c(unimplemented, "Harrell C-Index")
            if (isTRUE(self$options$unoCStatistic)) unimplemented <- c(unimplemented, "Uno C-Statistic")
            if (isTRUE(self$options$incidentDynamic)) unimplemented <- c(unimplemented, "Incident/Dynamic AUC")
            if (isTRUE(self$options$cumulativeDynamic)) unimplemented <- c(unimplemented, "Cumulative/Dynamic AUC")
            if (isTRUE(self$options$competingRisksConcordance)) unimplemented <- c(unimplemented, "Competing Risks Concordance")
            if (isTRUE(self$options$splineCalibration)) unimplemented <- c(unimplemented, "Spline Calibration")
            if (isTRUE(self$options$eoRatio)) unimplemented <- c(unimplemented, "E/O Ratio")
            if (isTRUE(self$options$namDagostino)) unimplemented <- c(unimplemented, "Nam-D'Agostino Test")
            if (isTRUE(self$options$greenwoodNam)) unimplemented <- c(unimplemented, "Greenwood-Nam-D'Agostino Test")
            if (isTRUE(self$options$calibrationBelt)) unimplemented <- c(unimplemented, "Calibration Belt")
            if (isTRUE(self$options$calibrationDensity)) unimplemented <- c(unimplemented, "Calibration Density")
            if (isTRUE(self$options$optimismCorrection)) unimplemented <- c(unimplemented, "Optimism Correction")
            if (isTRUE(self$options$externalValidation)) unimplemented <- c(unimplemented, "External Validation")
            if (isTRUE(self$options$decisionImpactCurves)) unimplemented <- c(unimplemented, "Decision Impact Curves")
            if (isTRUE(self$options$netBenefitRegression)) unimplemented <- c(unimplemented, "Net Benefit Regression")
            if (isTRUE(self$options$modelUpdating)) unimplemented <- c(unimplemented, "Model Updating")
            if (isTRUE(self$options$transportability)) unimplemented <- c(unimplemented, "Transportability")
            if (isTRUE(self$options$bootstrapPartialAUC)) unimplemented <- c(unimplemented, "Bootstrap CI for Partial AUC")
            if (isTRUE(self$options$bootstrapCutoffCI)) unimplemented <- c(unimplemented, "Bootstrap CI for Cutoffs")
            if (self$options$multiClassAveraging != "macro" && isTRUE(self$options$multiClassROC)) unimplemented <- c(unimplemented, "Weighted/Micro Multi-Class AUC Averaging")

            if (length(unimplemented) > 0) {
                private$.addNotice(
                    type = "INFO",
                    title = "Planned Features",
                    content = paste0("The following selected features are planned but not yet implemented: ",
                                     paste(unimplemented, collapse = ", "),
                                     ". They will be available in a future release.")
                )
            }
            
            # Generate natural language summary
            if (length(private$.rocResults) > 0) {
                private$.generateAnalysisSummary()
                private$.generateClinicalReport()
            }

            if (self$options$internalValidation) {
                private$.checkpoint()
                private$.populateInternalValidation()
            }

            # Success completion notice
            if (length(private$.rocResults) > 0) {
                n_predictors <- length(private$.rocResults)
                n_obs <- nrow(private$.data)

                # Get best AUC
                best_auc <- 0
                best_predictor <- ""
                for (pred in names(private$.rocResults)) {
                    auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                    if (auc_val > best_auc) {
                        best_auc <- auc_val
                        best_predictor <- pred
                    }
                }

                predictor_text <- if (n_predictors == 1) {
                    paste0("1 predictor (", best_predictor, ", AUC=", round(best_auc, 3), ")")
                } else {
                    paste0(n_predictors, " predictors (best: ", best_predictor, ", AUC=", round(best_auc, 3), ")")
                }

                private$.addNotice(
                    type = "INFO",
                    title = "Analysis Complete",
                    content = paste0("ROC analysis completed successfully. • Analyzed ", predictor_text, " with n=", n_obs, " observations. • Review AUC values, confidence intervals, and optimal cutoffs in results tables below. • Check any warnings or recommendations above for data quality concerns.")
                )
            }

            # Render all collected notices as HTML (must be last step)
            private$.renderNotices()
        },

        .prepareData = function() {
            # Validate data using enhanced error handling if available
            if (exists("validate_clinical_data", mode = "function")) {
                validation_result <- validate_clinical_data(
                    data = self$data,
                    required_vars = c(private$.outcome, private$.predictors),
                    min_observations = 20,
                    clinical_checks = TRUE
                )

                if (!validation_result$valid) {
                    self$results$results$instructions$setContent(
                        paste("<p>Data validation failed:",
                              private$.safeHtmlOutput(paste(validation_result$errors, collapse = "; ")), "</p>")
                    )
                    return(NULL)
                }
            }

            # Validate partialRange if specified
            if (!is.null(self$options$partialRange) && self$options$partialRange != "") {
                range_parts <- trimws(strsplit(self$options$partialRange, ",")[[1]])
                if (length(range_parts) != 2) {
                    self$results$results$instructions$setContent(
                        "<p><strong>Error:</strong> Partial AUC range must contain exactly two comma-separated numbers (e.g., '0.8,1.0').</p>"
                    )
                    return(NULL)
                }
                
                range_values <- suppressWarnings(as.numeric(range_parts))
                if (any(is.na(range_values))) {
                    self$results$results$instructions$setContent(
                        "<p><strong>Error:</strong> Partial AUC range values must be numeric (e.g., '0.8,1.0').</p>"
                    )
                    return(NULL)
                }
                
                if (range_values[1] >= range_values[2] || range_values[1] < 0 || range_values[2] > 1) {
                    self$results$results$instructions$setContent(
                        "<p><strong>Error:</strong> Partial AUC range must be valid: first value < second value, both between 0 and 1.</p>"
                    )
                    return(NULL)
                }
            }

            # Show warnings if any
            if (length(validation_result$warnings) > 0) {
                warning_msg <- paste("<p><strong>Warnings:</strong><br>",
                                   private$.safeHtmlOutput(paste(validation_result$warnings, collapse = "; ")), "</p>")
                self$results$results$instructions$setContent(
                    paste(private$.getInstructions(), warning_msg)
                )
            }

            # Prepare analysis dataset
            vars <- c(private$.outcome, private$.predictors)
            data <- self$data[vars]
            data <- na.omit(data)

            # Convert outcome to binary factor with proper positive class handling
            outcome_var <- data[[private$.outcome]]
            
            # Convert to factor if numeric
            if (is.numeric(outcome_var)) {
                unique_vals <- length(unique(outcome_var))
                if (unique_vals != 2) {
                    self$results$results$instructions$setContent(
                        paste0("<p><strong>Error:</strong> Outcome variable '", private$.safeHtmlOutput(private$.outcome),
                               "' must be binary (exactly 2 unique values). Found ", unique_vals,
                               " unique values. Please recode your outcome variable to have exactly two categories.</p>")
                    )
                    return(NULL)
                }
                outcome_var <- as.factor(outcome_var)
            } else if (!is.factor(outcome_var)) {
                outcome_var <- as.factor(outcome_var)
            }
            
            # Validate binary nature
            levels_count <- length(levels(outcome_var))
            positive_class <- NULL
            if (levels_count < 2) {
                self$results$results$instructions$setContent(
                    paste0("<div style='padding: 10px; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;'>",
                           "<h4 style='color: #721c24; margin-top: 0;'>Insufficient Outcome Variable Levels</h4>",
                           "<p><strong>Error:</strong> Outcome variable '<code>", private$.safeHtmlOutput(private$.outcome),
                           "</code>' has only 1 unique value: '<code>", private$.safeHtmlOutput(levels(outcome_var)[1]), "</code>'</p>",
                           "<p><strong>ROC analysis requires:</strong> At least 2 different outcome values (e.g., Disease/No Disease, Positive/Negative)</p>",
                           "<p><strong>Solutions:</strong></p>",
                           "<ul>",
                           "<li>Check if your data filtering removed one of the outcome categories</li>",
                           "<li>Verify the outcome variable contains the expected values</li>",
                           "<li>Consider using a different outcome variable with multiple categories</li>",
                           "</ul>",
                           "</div>")
                )
                return(NULL)
            } else if (levels_count > 2) {
                # Check if Multi-Class ROC is enabled
                if (self$options$multiClassROC) {
                    # Allow multi-level outcome
                    available_levels <- levels(outcome_var)
                    positive_class <- self$options$positiveClass
                    
                    # Store positive class for potential binary fallback
                    if (!is.null(positive_class) && positive_class != "" && positive_class %in% available_levels) {
                        private$.positiveClass <- positive_class
                    } else {
                        private$.positiveClass <- available_levels[2]
                    }
                    
                    # Inform user
                    info_msg <- paste0("<div style='padding: 10px; background: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-top: 10px;'>",
                                     "<h4 style='color: #0c5460; margin-top: 0;'> Multi-Class Analysis Enabled</h4>",
                                     "<p>Outcome variable '<code>", private$.safeHtmlOutput(private$.outcome), "</code>' has ", levels_count, " levels.</p>",
                                     "<p>Multi-Class ROC metrics will be calculated.</p>",
                                     "<p>Standard ROC tables will show performance for Positive Class: '<code>", private$.safeHtmlOutput(private$.positiveClass), "</code>' vs Others.</p>",
                                     "</div>")
                    self$results$results$instructions$setContent(
                        paste(private$.getInstructions(), info_msg)
                    )
                    
                    # Keep original outcome for multi-class calculations
                    private$.multiClassOutcome <- outcome_var
                    # Binary fallback for downstream metrics
                    binary_outcome <- factor(ifelse(outcome_var == private$.positiveClass, private$.positiveClass, "Other"),
                                             levels = c("Other", private$.positiveClass))
                    data[[private$.outcome]] <- binary_outcome
                    private$.binaryData <- binary_outcome
                    outcome_var <- binary_outcome
                } else {
                    # Convert multi-level outcome to binary using selected positive class
                    available_levels <- levels(outcome_var)
                    positive_class <- self$options$positiveClass
                    
                    if (is.null(positive_class) || positive_class == "" || !positive_class %in% available_levels) {
                        self$results$results$instructions$setContent(
                            paste0("<div style='padding: 10px; background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;'>",
                                   "<h4 style='color: #856404; margin-top: 0;'>Multi-level Outcome Variable Detected</h4>",
                                   "<p><strong>Issue:</strong> Outcome variable '<code>", private$.safeHtmlOutput(private$.outcome),
                                   "</code>' has ", levels_count, " levels for ROC analysis: <strong>", private$.safeHtmlOutput(paste(available_levels, collapse = ", ")), "</strong></p>",
                                   "<p><strong>Solution:</strong> ROC analysis requires a binary outcome. Please:</p>",
                                   "<ol>",
                                   "<li><strong>Select a Positive Class:</strong> Choose which level represents the 'positive' outcome (e.g., disease present) from the <em>Positive Class</em> dropdown above. All other levels will be combined as 'negative'.</li>",
                                   "<li><strong>Alternative:</strong> Pre-process your data to create a binary version of this variable before analysis.</li>",
                                   "<li><strong>Enable Multi-Class ROC:</strong> If you intend to perform multi-class analysis, enable the 'Multi-Class ROC' option.</li>",
                                   "</ol>",
                                   "<p><em>Example:</em> If analyzing mortality, you might select 'DOD' (Dead of Disease) as positive, combining 'DOOC', 'AWD', 'AWOD' as negative cases.</p>",
                                   "</div>")
                        )
                        return(NULL)
                    }
                    
                    # Convert to binary: positive class vs all others
                    binary_outcome <- ifelse(outcome_var == positive_class, positive_class, "Other")
                    outcome_var <- factor(binary_outcome, levels = c("Other", positive_class))
                    
                    # Update data with binary outcome
                    data[[private$.outcome]] <- outcome_var
                    
                    # Inform user about the conversion
                    info_msg <- paste0("<div style='padding: 10px; background: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; margin-top: 10px;'>",
                                     "<h4 style='color: #155724; margin-top: 0;'> Outcome Variable Converted to Binary</h4>",
                                     "<p><strong>Positive Class:</strong> '<code>", private$.safeHtmlOutput(positive_class), "</code>' (cases of interest)</p>",
                                     "<p><strong>Negative Class:</strong> '<code>Other</code>' (combined: ",
                                     private$.safeHtmlOutput(paste(available_levels[available_levels != positive_class], collapse = ", ")), ")</p>",
                                     "<p><em>ROC analysis will evaluate how well the predictor(s) distinguish between these two groups.</em></p>",
                                     "</div>")
                    self$results$results$instructions$setContent(
                        paste(private$.getInstructions(), info_msg)
                    )
                }
            }
            
            # Handle positive class selection for binary variables
            if (levels_count == 2) {
                available_levels <- levels(outcome_var)
                positive_class <- self$options$positiveClass
                
                # Enhanced validation for positive class selection
                if (!is.null(positive_class) && positive_class != "") {
                    if (!positive_class %in% available_levels) {
                        self$results$results$instructions$setContent(
                            paste0("<div style='padding: 10px; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;'>",
                                   "<h4 style='color: #721c24; margin-top: 0;'>Invalid Positive Class Selection</h4>",
                                   "<p><strong>Error:</strong> Selected positive class '<code>", private$.safeHtmlOutput(positive_class),
                                   "</code>' not found in outcome variable '<code>", private$.safeHtmlOutput(private$.outcome), "</code>'.</p>",
                                   "<p><strong>Available options:</strong> ", paste0("<code>", vapply(available_levels, private$.safeHtmlOutput, character(1)), "</code>", collapse = ", "), "</p>",
                                   "<p><strong>Action needed:</strong> Please select a valid level from the <em>Positive Class</em> dropdown above.</p>",
                                   "</div>")
                        )
                        return(NULL)
                    }
                    
                    # Reorder levels so positive class is second (reference level is first)
                    negative_class <- available_levels[available_levels != positive_class]
                    outcome_var <- factor(outcome_var, levels = c(negative_class, positive_class))
                    
                } else {
                    # If no positive class specified, use default order and inform user
                    positive_class <- available_levels[2]  # Second level is positive by default
                    negative_class <- available_levels[1]
                    
                    info_msg <- paste0("<p><strong>Default Configuration:</strong> Using '", private$.safeHtmlOutput(positive_class),
                                     "' as positive class and '", private$.safeHtmlOutput(negative_class),
                                     "' as negative class. You can change this in the Positive Class dropdown.</p>")
                    current_instructions <- private$.getInstructions()
                    self$results$results$instructions$setContent(paste0(current_instructions, info_msg))
                }
            }

            data[[private$.outcome]] <- outcome_var

            # CRITICAL FIX: Store the determined positive class for use in imbalance detection
            # positive_class was set in the multi-level (line 255) or binary (lines 297, 320) branches above
            private$.positiveClass <- positive_class
            if (is.null(private$.binaryData)) {
                private$.binaryData <- data[[private$.outcome]]
            }

            # Check predictor variables are numeric
            non_numeric_preds <- c()
            for (pred in private$.predictors) {
                if (!is.numeric(data[[pred]])) {
                    non_numeric_preds <- c(non_numeric_preds, pred)
                }
            }
            
            if (length(non_numeric_preds) > 0) {
                self$results$results$instructions$setContent(
                    paste0("<div style='padding: 10px; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;'>",
                           "<h4 style='color: #721c24; margin-top: 0;'>Non-numeric Predictor Variables</h4>",
                           "<p><strong>Error:</strong> The following predictor variable(s) are not numeric: <code>",
                           paste(vapply(non_numeric_preds, private$.safeHtmlOutput, character(1)), collapse = "</code>, <code>"),
                           "</code></p>",
                           "<p><strong>ROC analysis requires:</strong> Continuous or ordinal numeric predictors (e.g., biomarker levels, test scores, measurements)</p>",
                           "<p><strong>Solutions:</strong></p>",
                           "<ul>",
                           "<li>Convert categorical variables to numeric (e.g., ordinal scales: Low=1, Medium=2, High=3)</li>",
                           "<li>Use different numeric variables as predictors</li>",
                           "<li>For categorical predictors, consider using contingency table analysis instead</li>",
                           "</ul>",
                           "</div>")
                )
                return(NULL)
            }
            
            # Check for insufficient data after removing missing values
            if (nrow(data) < 10) {
                self$results$results$instructions$setContent(
                    paste0("<p><strong>Error:</strong> Insufficient data for ROC analysis after removing missing values. ", 
                           "Found only ", nrow(data), " complete observations. At least 10 observations are recommended for reliable ROC analysis.</p>")
                )
                return(NULL)
            }
            
            # Add clinical assumption validation warnings
            warnings <- private$.validateClinicalAssumptions(data)
            if (length(warnings) > 0) {
                warning_html <- paste0(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>", .("Clinical Assumptions & Recommendations"), "</h4>",
                    paste(warnings, collapse = ""),
                    "</div>"
                )
                private$.instructionsHtml <- paste0(private$.instructionsHtml, warning_html)
                self$results$results$instructions$setContent(private$.instructionsHtml)
            }

            return(data)
        },

        .runROCAnalysis = function(data) {
            private$.rocResults <- list()
            
            # Clean up old ROC objects to manage memory
            private$.cleanupROCObjects()

            for (predictor in private$.predictors) {
                # Checkpoint before each expensive ROC computation
                private$.checkpoint()
                
                # Comprehensive error handling for ROC analysis
                tryCatch({
                    # CRITICAL FIX: Determine direction
                    # Let pROC auto-detect direction when "auto" is selected
                    # This prevents AUC inversion for biomarkers where higher values indicate disease
                    direction_param <- self$options$direction
                    if (direction_param == "auto") {
                        direction <- "auto"  # Let pROC::roc() auto-detect based on AUC maximization
                    } else if (direction_param == "higher") {
                        direction <- "<"   # pROC: "<" means controls < cases (higher values = positive)
                    } else if (direction_param == "lower") {
                        direction <- ">"   # pROC: ">" means controls > cases (lower values = positive)
                    } else {
                        direction <- "auto"  # Fallback to auto-detection
                    }

                    # Handle Multi-Class Outcome for Standard ROC
                    outcome_col <- data[[private$.outcome]]
                    if (is.factor(outcome_col) && nlevels(outcome_col) > 2) {
                        # Convert to binary: Positive Class vs Others
                        pos_class <- private$.positiveClass
                        # Fallback if pos_class not valid
                        if (is.null(pos_class) || !pos_class %in% levels(outcome_col)) {
                            pos_class <- levels(outcome_col)[2]
                        }
                        
                        binary_outcome <- factor(ifelse(outcome_col == pos_class, pos_class, "Other"),
                                               levels = c("Other", pos_class))
                        response_var <- binary_outcome
                    } else {
                        response_var <- outcome_col
                    }

                    # Configure CI method and bootstrap behavior
                    use_boot <- isTRUE(self$options$useBootstrap)
                    boot_n <- if (use_boot) (self$options$bootstrapSamples %||% 200) else 0
                    boot_strat <- if (use_boot) {
                        if (is.null(self$options$stratifiedBootstrap)) TRUE else self$options$stratifiedBootstrap
                    } else {
                        TRUE
                    }
                    boot_type <- if (use_boot) {
                        switch(self$options$bootstrapMethod,
                            bca = "bca",
                            percentile = "perc",
                            basic = "basic",
                            "bca"
                        )
                    } else {
                        "bca"
                    }

                    # Warn when custom tied handling is requested (not supported by pROC)
                    if (!is.null(self$options$tiedScoreHandling) && self$options$tiedScoreHandling != "average") {
                        private$.addNotice(type = "INFO",
                            title = "Tied Score Handling",
                            content = "Custom tied score handling is not supported by pROC; using default averaging.")
                    }

                    # Create ROC object — always use DeLong CI initially
                    # Bootstrap CI is computed separately via pROC::ci.auc() so
                    # the user's bootstrap method choice (BCa/percentile/basic) takes effect
                    roc_obj <- pROC::roc(
                        response = response_var,
                        predictor = data[[predictor]],
                        direction = direction,
                        ci = TRUE,
                        conf.level = self$options$confidenceLevel / 100
                    )

                    # If bootstrap CI requested, recompute CI via ci.auc() with correct type
                    if (use_boot) {
                        tryCatch({
                            roc_obj$ci <- pROC::ci.auc(roc_obj,
                                method = "bootstrap",
                                boot.n = boot_n,
                                boot.stratified = boot_strat,
                                boot.ci.type = boot_type,
                                conf.level = self$options$confidenceLevel / 100
                            )
                        }, error = function(e) {
                            private$.addNotice(type = "WARNING",
                                title = paste0("Bootstrap CI Failed: ", predictor),
                                content = paste0("Bootstrap CI computation failed for ", predictor, ": ", e$message, ". Falling back to DeLong CI."))
                        })
                    }

                    # Apply smoothing if requested
                    if (self$options$smoothMethod != "none") {
                        tryCatch({
                            if (self$options$smoothMethod == "binormal") {
                                roc_obj <- pROC::smooth(roc_obj, method = "binormal")
                            } else if (self$options$smoothMethod == "kernel") {
                                roc_obj <- pROC::smooth(roc_obj, method = "density")
                            }
                        }, error = function(e) {
                            private$.addNotice(type = "INFO", title = paste0("Smoothing Skipped: ", predictor), content = paste0("ROC smoothing failed for ", predictor, " - using unsmoothed ROC curve instead."))
                        })
                    }

                    private$.rocObjects[[predictor]] <- roc_obj

                    # Surface the auto-detected direction for clinical safety
                    if (direction_param == "auto") {
                        dir_label <- if (roc_obj$direction == "<") "higher predictor values classify as positive (disease)" else "lower predictor values classify as positive (disease)"
                        private$.addNotice(type = "INFO",
                            title = paste0("Direction Auto-Detected: ", predictor),
                            content = paste0("ROC direction for ", predictor, ": ", dir_label, ". Verify this matches your biomarker's expected behavior."))
                    }

                    # Calculate optimal cutoff using Youden Index
                    optimal_cutoff <- private$.calculateOptimalCutoff(roc_obj, data, predictor)

                    # Evaluate custom cutoffs if specified
                    custom_cutoffs <- private$.evaluateCustomCutoffs(roc_obj, data, predictor)

                    # Store results
                    private$.rocResults[[predictor]] <- list(
                        roc = roc_obj,
                        optimal_cutoff = optimal_cutoff,
                        custom_cutoffs = custom_cutoffs,
                        predictor = predictor
                    )

                    # Clinical threshold notices for this predictor
                    auc_value <- as.numeric(roc_obj$auc)
                    n_obs <- length(roc_obj$response)
                    n_positive <- sum(roc_obj$response == levels(roc_obj$response)[2])
                    n_negative <- sum(roc_obj$response == levels(roc_obj$response)[1])
                    prevalence <- n_positive / n_obs

                    # Per-class event count guard
                    if (n_positive < 10 || n_negative < 10) {
                        private$.addNotice(
                            type = "STRONG_WARNING",
                            title = paste0("Low Per-Class Count: ", predictor),
                            content = paste0("Very few events in one or both classes for ", predictor,
                                ": ", n_positive, " positive, ", n_negative, " negative. ",
                                "ROC estimates, confidence intervals, and optimal cutoffs are unreliable with fewer than 10 events per class. ",
                                "Collect more data before drawing clinical conclusions.")
                        )
                    }

                    # Small sample size notice
                    if (n_obs < 30) {
                        notice_type <- if (n_obs < 10) {
                            "STRONG_WARNING"
                        } else {
                            "WARNING"
                        }

                        private$.addNotice(
                            type = notice_type,
                            title = paste0("Small Sample Size: ", predictor),
                            content = paste0("Small sample size for ", predictor, ": n=", n_obs, " (", n_positive, " positive, ", n_negative, " negative). • ROC curve confidence intervals may be unreliable with limited data. • Consider collecting more data or using bootstrap resampling for more stable estimates. • Results should be interpreted cautiously and validated in larger samples.")
                        )
                    }

                    # Low AUC notice
                    if (auc_value < 0.7) {
                        notice_type <- if (auc_value < 0.5) {
                            "ERROR"
                        } else {
                            "STRONG_WARNING"
                        }

                        interpretation <- if (auc_value < 0.5) {
                            "AUC below 0.5 indicates worse than random performance - verify ROC direction is correct"
                        } else {
                            "AUC below 0.7 indicates limited discriminative ability"
                        }

                        private$.addNotice(
                            type = notice_type,
                            title = paste0("Limited Diagnostic Performance: ", predictor),
                            content = paste0("Limited diagnostic performance for ", predictor, ": AUC=", round(auc_value, 3), ". • ", interpretation, ". • Consider: (1) Adding predictor variables or interaction terms, (2) Verifying data quality and coding, (3) Using multivariate models to improve discrimination, (4) Checking if direction setting is appropriate for your biomarker.")
                        )
                    }

                    # Extreme prevalence notice
                    if (prevalence < 0.05 || prevalence > 0.95) {
                        prev_direction <- if (prevalence < 0.05) "low" else "high"
                        metric_concern <- if (prevalence < 0.05) {
                            "Positive Predictive Value (PPV) will be unreliable even with high sensitivity/specificity"
                        } else {
                            "Negative Predictive Value (NPV) will be unreliable even with high sensitivity/specificity"
                        }

                        private$.addNotice(
                            type = "STRONG_WARNING",
                            title = paste0("Extreme Prevalence: ", predictor),
                            content = paste0("Extreme prevalence for ", predictor, ": ", round(prevalence * 100, 1), "% (", prev_direction, "). • ", metric_concern, ". • ROC/AUC analysis may be misleading - consider Precision-Recall Curve (PRC) instead. • Sensitivity and specificity remain valid, but predictive values (PPV/NPV) are heavily influenced by prevalence.")
                        )
                    }

                }, error = function(e) {
                    # Provide specific error messages based on common ROC analysis issues
                    safe_predictor <- private$.safeHtmlOutput(predictor)
                    error_msg <- ""
                    if (grepl("No controls", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("No control observations found for predictor"), " '", safe_predictor,
                                          "'. ", .("Check that your outcome variable has both positive and negative cases."))
                    } else if (grepl("No cases", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("No case observations found for predictor"), " '", safe_predictor,
                                          "'. ", .("Check that your outcome variable has both positive and negative cases."))
                    } else if (grepl("identical", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("Predictor"), " '", safe_predictor,
                                          "' ", .("has identical values across all observations. ROC analysis requires variation in predictor values."))
                    } else if (grepl("missing", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("Missing values detected in predictor"), " '", safe_predictor,
                                          "' ", .("or outcome variable. Please check your data."))
                    } else {
                        error_msg <- paste0(.("ROC analysis failed for predictor"), " '", safe_predictor, "': ",
                                          private$.safeHtmlOutput(e$message), ". ", .("Please check your data quality and variable selection."))
                    }
                    
                    # Handle errors with enhanced error handling if available
                    if (exists("clinicopath_error_handler")) {
                        clinicopath_error_handler(e, "enhancedROC", error_msg)
                    }
                    
                    # Show user-friendly error in results
                    private$.instructionsHtml <- paste0(private$.instructionsHtml,
                        "<p><strong>", .("Warning"), ":</strong> ", error_msg, "</p>")
                    self$results$results$instructions$setContent(private$.instructionsHtml
                    )
                })
            }
        },

        # Helper function to apply cutoff respecting pROC direction convention
        .applyDirectionCutoff = function(predictor_values, cutoff, roc_direction) {
            # pROC direction "<" means controls < cases: HIGHER values = positive (disease)
            # pROC direction ">" means controls > cases: LOWER values = positive (disease)
            if (roc_direction == "<") {
                return(predictor_values >= cutoff)
            } else {
                return(predictor_values < cutoff)
            }
        },

        .calculateOptimalCutoff = function(roc_obj, data, predictor) {
            # Determine thresholds based on presets or options
            sens_threshold <- self$options$sensitivityThreshold %||% 0
            spec_threshold <- self$options$specificityThreshold %||% 0
            use_optimization <- isTRUE(self$options$youdenOptimization)

            if (!is.null(private$.presetConfig)) {
                config <- private$.presetConfig
                # Force optimization when using presets
                use_optimization <- TRUE
                
                if (config$focus == "sensitivity") {
                    sens_threshold <- config$threshold
                    spec_threshold <- 0
                } else if (config$focus == "specificity") {
                    sens_threshold <- 0
                    spec_threshold <- config$threshold
                } else if (config$focus == "balanced") {
                    sens_threshold <- config$sens_threshold
                    spec_threshold <- config$spec_threshold
                }
            }

            # Check if optimization is enabled (either by option or preset)
            if (!use_optimization) {
                # If Youden optimization is disabled, return a simple result based on best threshold
                coords_result <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

                # CRITICAL FIX: Calculate confusion matrix respecting ROC direction
                predictions <- ifelse(
                    private$.applyDirectionCutoff(data[[predictor]], coords_result$threshold, roc_obj$direction),
                    levels(data[[private$.outcome]])[2],
                    levels(data[[private$.outcome]])[1]
                )
                predictions <- factor(predictions, levels = levels(data[[private$.outcome]]))

                cm <- caret::confusionMatrix(predictions, data[[private$.outcome]],
                                            positive = levels(data[[private$.outcome]])[2])

                # Access confusion matrix by level name for robustness
                pos_lvl <- levels(data[[private$.outcome]])[2]
                neg_lvl <- levels(data[[private$.outcome]])[1]
                return(list(
                    cutoff = coords_result$threshold,
                    sensitivity = coords_result$sensitivity,
                    specificity = coords_result$specificity,
                    youden_index = coords_result$sensitivity + coords_result$specificity - 1,
                    accuracy = as.numeric(cm$overall["Accuracy"]),
                    confusion_matrix = cm,
                    true_positive = cm$table[pos_lvl, pos_lvl],
                    true_negative = cm$table[neg_lvl, neg_lvl],
                    false_positive = cm$table[pos_lvl, neg_lvl],
                    false_negative = cm$table[neg_lvl, pos_lvl]
                ))
            }
            
            # Calculate coordinates for all thresholds
            coords_result <- pROC::coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))

            # Apply sensitivity/specificity threshold constraints if specified
            valid_indices <- rep(TRUE, length(coords_result$threshold))

            if (sens_threshold > 0) {
                valid_indices <- valid_indices & (coords_result$sensitivity >= sens_threshold)
            }

            if (spec_threshold > 0) {
                valid_indices <- valid_indices & (coords_result$specificity >= spec_threshold)
            }

            # If no thresholds meet constraints, use unconstrained optimization
            if (!any(valid_indices)) {
                private$.addNotice(type = "WARNING", title = paste0("Constraints Not Met: ", predictor), content = paste0("No cutoffs meet the specified sensitivity/specificity constraints for ", predictor, ". Using unconstrained Youden optimization instead."))
                valid_indices <- rep(TRUE, length(coords_result$threshold))
            }

            # Calculate Youden Index for valid thresholds only
            youden_indices <- coords_result$sensitivity + coords_result$specificity - 1
            youden_indices[!valid_indices] <- -Inf  # Exclude invalid indices

            # Find optimal cutoff (maximum Youden Index among valid options)
            optimal_idx <- which.max(youden_indices)

            optimal_cutoff <- coords_result$threshold[optimal_idx]
            optimal_sens <- coords_result$sensitivity[optimal_idx]
            optimal_spec <- coords_result$specificity[optimal_idx]
            optimal_youden <- youden_indices[optimal_idx]

            # CRITICAL FIX: Calculate confusion matrix at optimal cutoff respecting ROC direction
            predictions <- ifelse(
                private$.applyDirectionCutoff(data[[predictor]], optimal_cutoff, roc_obj$direction),
                levels(data[[private$.outcome]])[2],
                levels(data[[private$.outcome]])[1]
            )
            predictions <- factor(predictions, levels = levels(data[[private$.outcome]]))

            cm <- caret::confusionMatrix(predictions, data[[private$.outcome]],
                                        positive = levels(data[[private$.outcome]])[2])

            # Access confusion matrix by level name for robustness
            pos_lvl <- levels(data[[private$.outcome]])[2]
            neg_lvl <- levels(data[[private$.outcome]])[1]
            return(list(
                cutoff = optimal_cutoff,
                sensitivity = optimal_sens,
                specificity = optimal_spec,
                youden_index = optimal_youden,
                accuracy = as.numeric(cm$overall["Accuracy"]),
                confusion_matrix = cm,
                true_positive = cm$table[pos_lvl, pos_lvl],
                true_negative = cm$table[neg_lvl, neg_lvl],
                false_positive = cm$table[pos_lvl, neg_lvl],
                false_negative = cm$table[neg_lvl, pos_lvl]
            ))
        },

        .evaluateCustomCutoffs = function(roc_obj, data, predictor) {
            # Evaluate custom cutoffs if specified
            if (is.null(self$options$customCutoffs) || self$options$customCutoffs == "") {
                return(NULL)
            }

            tryCatch({
                # Parse custom cutoffs
                custom_cuts <- as.numeric(trimws(strsplit(self$options$customCutoffs, ",")[[1]]))
                custom_cuts <- custom_cuts[!is.na(custom_cuts)]  # Remove invalid values

                if (length(custom_cuts) == 0) return(NULL)

                # Evaluate each custom cutoff
                custom_results <- data.frame(
                    cutoff = numeric(0),
                    sensitivity = numeric(0),
                    specificity = numeric(0),
                    youden_index = numeric(0),
                    accuracy = numeric(0),
                    ppv = numeric(0),
                    npv = numeric(0)
                )

                outcome_var <- data[[private$.outcome]]
                predictor_var <- data[[predictor]]

                for (cutoff in custom_cuts) {
                    # Calculate sensitivity and specificity at this cutoff
                    coords <- pROC::coords(roc_obj, cutoff, ret = c("sensitivity", "specificity", "ppv", "npv"))

                    # CRITICAL FIX: Create predictions respecting ROC direction
                    predictions <- ifelse(
                        private$.applyDirectionCutoff(predictor_var, cutoff, roc_obj$direction),
                        levels(outcome_var)[2],
                        levels(outcome_var)[1]
                    )
                    predictions <- factor(predictions, levels = levels(outcome_var))

                    cm <- caret::confusionMatrix(predictions, outcome_var,
                                               positive = levels(outcome_var)[2])

                    # Store results
                    custom_results <- rbind(custom_results, data.frame(
                        cutoff = cutoff,
                        sensitivity = coords$sensitivity,
                        specificity = coords$specificity,
                        youden_index = coords$sensitivity + coords$specificity - 1,
                        accuracy = as.numeric(cm$overall["Accuracy"]),
                        ppv = coords$ppv,
                        npv = coords$npv
                    ))
                }

                return(custom_results)

            }, error = function(e) {
                private$.addNotice(type = "WARNING", title = paste0("Custom Cutoff Error: ", predictor), content = paste0("Failed to evaluate custom cutoffs for ", predictor, ": ", e$message))
                return(NULL)
            })
        },

        .populateAUCSummary = function() {
            aucTable <- self$results$results$aucSummary

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                roc_obj <- result$roc

                # AUC interpretation
                auc_value <- as.numeric(roc_obj$auc)
                auc_interp <- private$.interpretAUC(auc_value)
                clinical_utility <- private$.assessClinicalUtility(auc_value, self$options$clinicalContext)

                row <- list(
                    predictor = predictor,
                    auc = auc_value,
                    auc_lower = if(!is.null(roc_obj$ci)) roc_obj$ci[1] else NA,
                    auc_upper = if(!is.null(roc_obj$ci)) roc_obj$ci[3] else NA,
                    std_error = tryCatch({
                        if (!self$options$useBootstrap) {
                            sqrt(pROC::var(roc_obj))
                        } else {
                            NA_real_
                        }
                    }, error = function(e) NA_real_),
                    auc_interpretation = auc_interp,
                    clinical_utility = clinical_utility
                )

                aucTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }
        },

        .populateOptimalCutoffs = function() {
            cutoffTable <- self$results$results$optimalCutoffSummary

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                optimal <- result$optimal_cutoff

                # Clinical recommendation based on context and performance
                clinical_rec <- private$.generateClinicalRecommendation(
                    optimal, self$options$clinicalContext, predictor)

                row <- list(
                    predictor = predictor,
                    optimal_cutoff = optimal$cutoff,
                    youden_index = optimal$youden_index,
                    sensitivity = optimal$sensitivity,
                    specificity = optimal$specificity,
                    accuracy = optimal$accuracy,
                    clinical_recommendation = clinical_rec
                )

                cutoffTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }
        },

        # Helper function to calculate confusion matrix values for a given cutoff
        .calculateConfusionMatrix = function(predictor, cutoff) {
            # Get the actual data for this predictor  
            predictor_data <- private$.data[[predictor]]
            outcome_data <- private$.data[[private$.outcome]]
            
            # Get the ROC result to determine direction
            roc_result <- private$.rocResults[[predictor]]
            roc_obj <- roc_result$roc
            
            # Use ROC direction to determine how to apply cutoff
            # pROC direction "<" means controls < cases: higher values = positive
            # pROC direction ">" means controls > cases: lower values = positive
            if (roc_obj$direction == "<") {
                predicted_positive <- predictor_data >= cutoff
            } else {
                predicted_positive <- predictor_data < cutoff
            }
            
            # Determine actual positive based on positive class selection
            positive_class <- private$.positiveClass
            if (is.null(positive_class) || positive_class == "") {
                # Default to second level
                actual_positive <- outcome_data == levels(outcome_data)[2]
            } else {
                actual_positive <- outcome_data == positive_class
            }
            
            # Calculate confusion matrix components
            tp <- sum(predicted_positive & actual_positive, na.rm = TRUE)
            tn <- sum(!predicted_positive & !actual_positive, na.rm = TRUE)
            fp <- sum(predicted_positive & !actual_positive, na.rm = TRUE)
            fn <- sum(!predicted_positive & actual_positive, na.rm = TRUE)
            
            return(list(
                true_positive = as.integer(tp),
                true_negative = as.integer(tn),
                false_positive = as.integer(fp),
                false_negative = as.integer(fn)
            ))
        },

        .populateCutoffAnalysis = function() {
            cutoffTable <- self$results$results$cutoffAnalysis

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                roc_obj <- result$roc
                optimal <- result$optimal_cutoff
                custom_cutoffs <- result$custom_cutoffs

                # Calculate confusion matrix for optimal cutoff
                optimal_cm <- private$.calculateConfusionMatrix(predictor, optimal$cutoff)

                # Add optimal cutoff first
                row <- list(
                    predictor = predictor,
                    cutoff = optimal$cutoff,
                    cutoff_type = .("Optimal (Youden)"),
                    sensitivity = optimal$sensitivity,
                    specificity = optimal$specificity,
                    one_minus_specificity = 1 - optimal$specificity,
                    youden_index = optimal$youden_index,
                    true_positive = optimal_cm$true_positive,
                    true_negative = optimal_cm$true_negative,
                    false_positive = optimal_cm$false_positive,
                    false_negative = optimal_cm$false_negative
                )
                cutoffTable$addRow(rowKey = paste(private$.escapeVar(predictor), "optimal", sep = "_"), values = row)

                # Add custom cutoffs if available
                if (!is.null(custom_cutoffs) && nrow(custom_cutoffs) > 0) {
                    for (i in 1:nrow(custom_cutoffs)) {
                        custom_cm <- private$.calculateConfusionMatrix(predictor, custom_cutoffs$cutoff[i])
                        
                        custom_row <- list(
                            predictor = predictor,
                            cutoff = custom_cutoffs$cutoff[i],
                            cutoff_type = paste("Custom", i),
                            sensitivity = custom_cutoffs$sensitivity[i],
                            specificity = custom_cutoffs$specificity[i],
                            one_minus_specificity = 1 - custom_cutoffs$specificity[i],
                            youden_index = custom_cutoffs$youden_index[i],
                            true_positive = custom_cm$true_positive,
                            true_negative = custom_cm$true_negative,
                            false_positive = custom_cm$false_positive,
                            false_negative = custom_cm$false_negative
                        )
                        cutoffTable$addRow(rowKey = paste(private$.escapeVar(predictor), "custom", i, sep = "_"), values = custom_row)
                    }
                }

                # Get additional representative cutoffs from ROC curve
                coords_result <- pROC::coords(roc_obj, "all",
                                            ret = c("threshold", "sensitivity", "specificity"))

                # Sample additional cutoffs for display (max 15 more rows per predictor)
                n_cutoffs <- min(nrow(coords_result), 15)
                sample_indices <- seq(1, nrow(coords_result), length.out = n_cutoffs)
                sample_indices <- round(sample_indices)

                for (i in sample_indices) {
                    sens <- coords_result$sensitivity[i]
                    spec <- coords_result$specificity[i]
                    cutoff <- coords_result$threshold[i]

                    # Calculate Youden Index
                    youden <- sens + spec - 1
                    
                    # Calculate confusion matrix values using helper function
                    cm <- private$.calculateConfusionMatrix(predictor, cutoff)

                    row <- list(
                        predictor = predictor,
                        cutoff = cutoff,
                        cutoff_type = .("Sampled"),
                        sensitivity = sens,
                        specificity = spec,
                        one_minus_specificity = 1 - spec,
                        youden_index = youden,
                        true_positive = cm$true_positive,
                        true_negative = cm$true_negative,
                        false_positive = cm$false_positive,
                        false_negative = cm$false_negative
                    )

                    cutoffTable$addRow(rowKey = paste(private$.escapeVar(predictor), i, sep = "_"), values = row)
                }
            }
        },

        .populateDiagnosticPerformance = function() {
            diagTable <- self$results$results$diagnosticPerformance

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                optimal <- result$optimal_cutoff
                cm <- optimal$confusion_matrix

                # Calculate confidence intervals for sensitivity and specificity (exact)
                sens_ci <- private$.calculateBinomialCI(optimal$true_positive,
                                                      optimal$true_positive + optimal$false_negative)
                spec_ci <- private$.calculateBinomialCI(optimal$true_negative,
                                                      optimal$true_negative + optimal$false_positive)

                # Calculate balanced accuracy
                balanced_acc <- (optimal$sensitivity + optimal$specificity) / 2

                row <- list(
                    predictor = predictor,
                    cutoff = optimal$cutoff,
                    sensitivity = optimal$sensitivity,
                    sensitivity_ci = paste0("(", round(sens_ci[1], 3), ", ", round(sens_ci[2], 3), ")"),
                    specificity = optimal$specificity,
                    specificity_ci = paste0("(", round(spec_ci[1], 3), ", ", round(spec_ci[2], 3), ")"),
                    accuracy = optimal$accuracy,
                    balanced_accuracy = balanced_acc
                )

                diagTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }
        },

        .populateClinicalMetrics = function() {
            clinTable <- self$results$results$clinicalApplicationMetrics

            observed_prevalence <- mean(private$.binaryData == levels(private$.binaryData)[2])
            prevalence <- if (isTRUE(self$options$useObservedPrevalence)) {
                observed_prevalence
            } else if (!is.null(self$options$prevalence) && !is.na(self$options$prevalence)) {
                self$options$prevalence
            } else {
                observed_prevalence
            }
            if (!is.na(prevalence) && abs(prevalence - observed_prevalence) > 0.05 && !isTRUE(self$options$useObservedPrevalence)) {
                note_html <- sprintf("<p><strong>Prevalence note:</strong> Calculations use a user-specified prevalence of %.3f (observed %.3f).</p>",
                                     prevalence, observed_prevalence)
                private$.instructionsHtml <- paste0(private$.instructionsHtml, note_html)
                self$results$results$instructions$setContent(private$.instructionsHtml)
            }

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                optimal <- result$optimal_cutoff

                # Calculate predictive values
                sens <- optimal$sensitivity
                spec <- optimal$specificity

                ppv_denom <- (sens * prevalence) + ((1 - spec) * (1 - prevalence))
                ppv <- if (ppv_denom > 1e-10) (sens * prevalence) / ppv_denom else NA_real_
                npv_denom <- ((1 - sens) * prevalence) + (spec * (1 - prevalence))
                npv <- if (npv_denom > 1e-10) (spec * (1 - prevalence)) / npv_denom else NA_real_

                # Calculate likelihood ratios (with division-by-zero guards)
                lr_pos <- if (abs(1 - spec) < 1e-10) Inf else sens / (1 - spec)
                lr_neg <- if (abs(spec) < 1e-10 && abs(1 - sens) < 1e-10) NA_real_
                         else if (abs(spec) < 1e-10) Inf
                         else (1 - sens) / spec

                # Diagnostic odds ratio
                dor <- if (is.na(lr_pos) || is.na(lr_neg)) {
                    NA_real_
                } else if (is.infinite(lr_pos) && is.infinite(lr_neg)) {
                    NA_real_  # Inf/Inf is indeterminate
                } else if (is.infinite(lr_pos) && lr_neg > 0) {
                    Inf  # Perfect rule-in (spec=1)
                } else if (!is.infinite(lr_neg) && abs(lr_neg) < 1e-10 && lr_pos > 0) {
                    Inf  # Perfect discrimination (sens=1)
                } else if (is.infinite(lr_neg)) {
                    0  # LR+/Inf = 0
                } else {
                    lr_pos / lr_neg
                }

                # Clinical interpretation
                clinical_interp <- private$.interpretClinicalMetrics(ppv, npv, lr_pos, lr_neg,
                                                                   self$options$clinicalContext)

                # Handle Inf LR values for numeric table columns
                # Cap at large value and annotate clinical interpretation
                lr_pos_display <- lr_pos
                lr_neg_display <- lr_neg
                lr_notes <- ""
                if (is.infinite(lr_pos) && lr_pos > 0) {
                    lr_pos_display <- 9999
                    lr_notes <- paste0(lr_notes, " LR+ is infinite (perfect rule-in: specificity = 100%).")
                }
                if (is.infinite(lr_neg)) {
                    lr_neg_display <- 9999
                    lr_notes <- paste0(lr_notes, " LR- is infinite (test never correctly classifies negatives: specificity = 0%).")
                }
                dor_display <- dor
                if (!is.na(dor) && is.infinite(dor)) {
                    dor_display <- 9999
                    lr_notes <- paste0(lr_notes, " DOR is infinite (perfect discrimination at this cutoff).")
                }
                if (nchar(lr_notes) > 0) {
                    clinical_interp <- paste0(clinical_interp, lr_notes)
                }

                row <- list(
                    predictor = predictor,
                    prevalence = prevalence,
                    ppv = ppv,
                    npv = npv,
                    lr_positive = lr_pos_display,
                    lr_negative = lr_neg_display,
                    diagnostic_odds_ratio = dor_display,
                    clinical_interpretation = clinical_interp
                )

                clinTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }
        },

        .populateROCComparisons = function() {
            if (length(private$.rocResults) < 2) {
                private$.addNotice(type = "WARNING", title = "Insufficient Predictors",
                    content = "Pairwise ROC comparisons require at least 2 predictors.")
                return()
            }

            compTable <- self$results$results$rocComparisons
            predictors <- names(private$.rocResults)

            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]

                    tryCatch({
                    roc1 <- private$.rocResults[[pred1]]$roc
                    roc2 <- private$.rocResults[[pred2]]$roc

                    comparison <- pROC::roc.test(roc1, roc2, method = self$options$comparisonMethod)

                    auc_diff <- as.numeric(roc1$auc) - as.numeric(roc2$auc)

                    result <- if (comparison$p.value < 0.05) .("Significantly different") else .("Not significantly different")

                    clinical_sig <- private$.assessClinicalSignificanceDifference(
                        abs(auc_diff), self$options$clinicalContext)

                    row <- list(
                        predictor1 = pred1,
                        predictor2 = pred2,
                        auc_difference = auc_diff,
                        test_statistic = comparison$statistic,
                        p_value = comparison$p.value,
                        result = result,
                        clinical_significance = clinical_sig
                    )

                    compTable$addRow(rowKey = paste(private$.escapeVar(pred1), private$.escapeVar(pred2), sep = "_vs_"), values = row)
                    }, error = function(e) {
                        private$.addNotice(type = "WARNING", title = paste0("Comparison Error: ", pred1, " vs ", pred2),
                            content = paste0("ROC comparison failed: ", e$message))
                    })
                }
            }
        },

        .populateDetailedComparison = function() {
            if (length(private$.rocResults) < 2) {
                return()
            }
            
            detailTable <- self$results$results$detailedComparison
            
            # Get all predictor pairs
            predictors <- names(private$.rocResults)
            
            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]
                    
                    tryCatch({
                    result1 <- private$.rocResults[[pred1]]
                    result2 <- private$.rocResults[[pred2]]

                    # Compare various metrics
                    metrics <- c("AUC", "Sensitivity", "Specificity", "Accuracy")

                    for (metric in metrics) {
                        if (metric == "AUC") {
                            val1 <- as.numeric(result1$roc$auc)
                            val2 <- as.numeric(result2$roc$auc)
                        } else if (metric == "Sensitivity") {
                            val1 <- result1$optimal_cutoff$sensitivity
                            val2 <- result2$optimal_cutoff$sensitivity
                        } else if (metric == "Specificity") {
                            val1 <- result1$optimal_cutoff$specificity
                            val2 <- result2$optimal_cutoff$specificity
                        } else if (metric == "Accuracy") {
                            val1 <- result1$optimal_cutoff$accuracy
                            val2 <- result2$optimal_cutoff$accuracy
                        }

                        if (!is.na(val1) && !is.na(val2)) {
                            diff <- val2 - val1
                            percent_change <- if (abs(val1) < 1e-10) NA_real_ else (diff / val1) * 100

                            # Calculate p-value using appropriate statistical test
                            p_value <- private$.calculateMetricPValue(metric, result1, result2, pred1, pred2)

                            # Determine clinical interpretation
                            interpretation <- if (!is.na(percent_change) && abs(percent_change) > 20) {
                                .("Clinically significant difference")
                            } else if (!is.na(percent_change) && abs(percent_change) > 10) {
                                .("Moderate difference")
                            } else {
                                .("Minimal difference")
                            }

                            row <- list(
                                metric = paste(metric, "(", pred1, "vs", pred2, ")"),
                                model1_value = val1,
                                model2_value = val2,
                                difference = diff,
                                percent_change = percent_change,
                                p_value = p_value,
                                effect_size = abs(diff),
                                interpretation = interpretation
                            )

                            detailTable$addRow(rowKey = paste(private$.escapeVar(pred1), private$.escapeVar(pred2), metric, sep = "_"), values = row)
                        }
                    }
                    }, error = function(e) {
                        private$.addNotice(type = "WARNING", title = paste0("Detailed Comparison Error: ", pred1, " vs ", pred2),
                            content = paste0("Detailed comparison failed: ", e$message))
                    })
                }
            }
        },

        .populateStatisticalSummary = function() {
            if (length(private$.rocResults) < 2) {
                return()
            }
            
            statSummaryTable <- self$results$results$statisticalSummary
            
            # Get all predictor pairs
            predictors <- names(private$.rocResults)
            
            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]
                    
                    tryCatch({
                    roc1 <- private$.rocResults[[pred1]]$roc
                    roc2 <- private$.rocResults[[pred2]]$roc

                    # Perform statistical test for AUC comparison
                    if (self$options$comparisonMethod == "delong") {
                        test_result <- pROC::roc.test(roc1, roc2, method = "delong")
                        test_name <- .("DeLong Test for AUC Comparison")
                        test_stat <- test_result$statistic
                        p_val <- test_result$p.value
                    } else if (self$options$comparisonMethod == "bootstrap") {
                        test_result <- pROC::roc.test(roc1, roc2, method = "bootstrap",
                                                    boot.n = self$options$bootstrapSamples)
                        test_name <- .("Bootstrap Test for AUC Comparison")
                        test_stat <- test_result$statistic
                        p_val <- test_result$p.value
                    } else {
                        # Venkatraman test
                        test_result <- pROC::roc.test(roc1, roc2, method = "venkatraman",
                                                    boot.n = self$options$bootstrapSamples)
                        test_name <- .("Venkatraman Test for AUC Comparison")
                        test_stat <- test_result$statistic
                        p_val <- test_result$p.value
                    }

                    # Determine statistical conclusion
                    conclusion <- if (p_val < 0.001) {
                        .("Highly significant difference (p < 0.001)")
                    } else if (p_val < 0.01) {
                        .("Significant difference (p < 0.01)")
                    } else if (p_val < 0.05) {
                        .("Significant difference (p < 0.05)")
                    } else {
                        .("No significant difference (p >= 0.05)")
                    }

                    # Determine effect magnitude
                    auc_diff <- abs(as.numeric(roc2$auc) - as.numeric(roc1$auc))
                    effect_magnitude <- if (auc_diff > 0.2) {
                        .("Large effect")
                    } else if (auc_diff > 0.1) {
                        .("Medium effect")
                    } else if (auc_diff > 0.05) {
                        .("Small effect")
                    } else {
                        .("Minimal effect")
                    }

                    row <- list(
                        test_name = paste(test_name, "(", pred1, "vs", pred2, ")"),
                        test_statistic = test_stat,
                        p_value = p_val,
                        conclusion = conclusion,
                        effect_magnitude = effect_magnitude
                    )

                    statSummaryTable$addRow(rowKey = paste(private$.escapeVar(pred1), private$.escapeVar(pred2), "stat", sep = "_"), values = row)
                    }, error = function(e) {
                        private$.addNotice(type = "WARNING", title = paste0("Statistical Summary Error: ", pred1, " vs ", pred2),
                            content = paste0("Statistical comparison failed: ", e$message))
                    })
                }
            }
        },

        .populatePartialAUC = function() {
            if (is.null(self$options$partialRange) || self$options$partialRange == "") return()

            paTable <- self$results$results$partialAucAnalysis

            # Parse partial range
            range_parts <- strsplit(self$options$partialRange, ",")[[1]]
            if (length(range_parts) != 2) return()

            range_min <- as.numeric(trimws(range_parts[1]))
            range_max <- as.numeric(trimws(range_parts[2]))

            # Determine if using sensitivity or specificity range
            range_type_name <- self$options$partialAucType

            for (predictor in names(private$.rocResults)) {
                tryCatch({
                    roc_obj <- private$.rocObjects[[predictor]]

                    # Calculate partial AUC based on type
                    # pROC requires partial.auc in DESCENDING order: c(max, min)
                    if (range_type_name == "sensitivity") {
                        pauc <- pROC::auc(roc_obj,
                                         partial.auc = c(range_max, range_min),
                                         partial.auc.focus = "sensitivity")
                        range_display <- "Sensitivity"
                    } else {
                        pauc <- pROC::auc(roc_obj,
                                         partial.auc = c(range_max, range_min),
                                         partial.auc.focus = "specificity")
                        range_display <- "Specificity"
                    }

                    # McClish (1989) normalized partial AUC via pROC's built-in correction
                    # This maps pAUC to [0.5, 1.0] scale where 0.5 = chance, 1.0 = perfect
                    normalized_pauc <- tryCatch({
                        pauc_corrected <- pROC::auc(roc_obj,
                                                    partial.auc = c(range_max, range_min),
                                                    partial.auc.focus = range_type_name,
                                                    partial.auc.correct = TRUE)
                        as.numeric(pauc_corrected)
                    }, error = function(e) NA_real_)

                    clinical_relevance <- private$.assessPartialAUCRelevance(
                        pauc, range_min, range_max, self$options$clinicalContext, range_type_name)

                    row <- list(
                        predictor = predictor,
                        range_type = range_display,
                        range_min = range_min,
                        range_max = range_max,
                        partial_auc = as.numeric(pauc),
                        normalized_pauc = as.numeric(normalized_pauc),
                        clinical_relevance = clinical_relevance
                    )

                    paTable$addRow(rowKey = private$.escapeVar(predictor), values = row)

                }, error = function(e) {
                    private$.addNotice(type = "WARNING", title = paste0("Partial AUC Error: ", predictor), content = paste0("Partial AUC calculation failed for ", predictor, ": ", e$message, ". Check that the partial range is valid and data has sufficient variation."))
                })
            }
        },

        .populateCROCAnalysis = function() {
            crocTable <- self$results$results$crocAnalysisTable
            alpha <- self$options$crocAlpha

            for (predictor in names(private$.rocResults)) {
                tryCatch({
                    roc_obj <- private$.rocObjects[[predictor]]

                    # Calculate CROC curve using exponential magnifier
                    croc_result <- private$.calculateCROC(roc_obj, alpha)

                    # Get original ROC AUC
                    roc_auc <- as.numeric(roc_obj$auc)

                    # Calculate early retrieval emphasis (CROC AUC - standard AUC)
                    # Note: this is a measure of how much the classifier benefits from
                    # early-retrieval weighting, not a direct AUC gain
                    early_retrieval_gain <- croc_result$croc_auc - roc_auc

                    # Interpretation
                    interpretation <- private$.interpretCROC(croc_result$croc_auc, early_retrieval_gain)

                    row <- list(
                        predictor = predictor,
                        croc_auc = croc_result$croc_auc,
                        roc_auc = roc_auc,
                        alpha = alpha,
                        early_retrieval_gain = early_retrieval_gain,
                        interpretation = interpretation
                    )

                    crocTable$addRow(rowKey = private$.escapeVar(predictor), values = row)

                }, error = function(e) {
                    private$.addNotice(type = "WARNING", title = paste0("CROC Error: ", predictor), content = paste0("CROC calculation failed for ", predictor, ": ", e$message))
                })
            }
        },

        .populateConvexHull = function() {
            hullTable <- self$results$results$convexHullTable

            for (predictor in names(private$.rocResults)) {
                tryCatch({
                    roc_obj <- private$.rocObjects[[predictor]]

                    # Calculate convex hull
                    hull_result <- private$.calculateConvexHull(roc_obj)

                    # Get empirical AUC
                    empirical_auc <- as.numeric(roc_obj$auc)

                    # Calculate performance gap
                    performance_gap <- hull_result$hull_auc - empirical_auc

                    # Interpretation
                    interpretation <- private$.interpretConvexHull(performance_gap, hull_result$n_hull_points)

                    row <- list(
                        predictor = predictor,
                        hull_auc = hull_result$hull_auc,
                        empirical_auc = empirical_auc,
                        performance_gap = performance_gap,
                        n_hull_points = hull_result$n_hull_points,
                        interpretation = interpretation
                    )

                    hullTable$addRow(rowKey = private$.escapeVar(predictor), values = row)

                }, error = function(e) {
                    private$.addNotice(type = "WARNING", title = paste0("Convex Hull Error: ", predictor), content = paste0("Convex hull calculation failed for ", predictor, ": ", e$message))
                })
            }
        },

        .calculateCROC = function(roc_obj, alpha = 7.0) {
            # Extract ROC coordinates
            fpr <- 1 - roc_obj$specificities  # False Positive Rate
            tpr <- roc_obj$sensitivities      # True Positive Rate

            # Apply exponential magnifier function: f(x) = (1 - exp(-alpha*x))/(1 - exp(-alpha))
            exp_alpha <- exp(-alpha)
            croc_fpr <- (1 - exp(-alpha * fpr)) / (1 - exp_alpha)

            # Calculate CROC AUC using trapezoidal rule
            # Sort by croc_fpr for proper integration
            ord <- order(croc_fpr)
            croc_fpr_sorted <- croc_fpr[ord]
            croc_tpr_sorted <- tpr[ord]

            # Trapezoidal integration
            croc_auc <- sum(diff(croc_fpr_sorted) *
                           (croc_tpr_sorted[-1] + croc_tpr_sorted[-length(croc_tpr_sorted)]) / 2)

            # Guard: negative integration indicates data ordering issue
            if (croc_auc < -1e-10) {
                private$.addNotice(type = "WARNING",
                    title = "CROC Integration Warning",
                    content = "CROC AUC integration yielded a negative value, which may indicate data ordering issues. Using absolute value.")
            }
            return(list(
                croc_auc = max(croc_auc, 0),
                croc_fpr = croc_fpr,
                croc_tpr = tpr,
                alpha = alpha
            ))
        },

        .calculateConvexHull = function(roc_obj) {
            # Extract ROC coordinates
            fpr <- 1 - roc_obj$specificities
            tpr <- roc_obj$sensitivities

            # Add corner points (0,0) and (1,1)
            fpr <- c(0, fpr, 1)
            tpr <- c(0, tpr, 1)

            # Calculate full convex hull
            points <- cbind(fpr, tpr)
            hull_indices <- grDevices::chull(points)

            # Extract hull points and sort by FPR
            hull_fpr <- fpr[hull_indices]
            hull_tpr <- tpr[hull_indices]
            ord <- order(hull_fpr)
            hull_fpr_sorted <- hull_fpr[ord]
            hull_tpr_sorted <- hull_tpr[ord]

            # Extract UPPER hull only: keep points above or on the diagonal
            # The upper hull connects (0,0) to (1,1) via points above the diagonal
            # For ROC, we need points where TPR >= FPR (above diagonal)
            # Plus the endpoints (0,0) and (1,1) which are on the diagonal
            upper_mask <- hull_tpr_sorted >= hull_fpr_sorted - 1e-10
            # Always include the endpoints
            upper_mask[1] <- TRUE
            upper_mask[length(upper_mask)] <- TRUE
            hull_fpr_upper <- hull_fpr_sorted[upper_mask]
            hull_tpr_upper <- hull_tpr_sorted[upper_mask]

            # Remove duplicates at same FPR, keeping max TPR
            if (length(hull_fpr_upper) > 1) {
                agg <- aggregate(hull_tpr_upper, by = list(hull_fpr_upper), FUN = max)
                hull_fpr_upper <- agg[, 1]
                hull_tpr_upper <- agg[, 2]
            }

            # Calculate upper hull AUC using trapezoidal rule
            hull_auc <- sum(diff(hull_fpr_upper) *
                           (hull_tpr_upper[-1] + hull_tpr_upper[-length(hull_tpr_upper)]) / 2)

            return(list(
                hull_auc = hull_auc,
                hull_fpr = hull_fpr_upper,
                hull_tpr = hull_tpr_upper,
                n_hull_points = length(hull_fpr_upper)
            ))
        },

        .interpretCROC = function(croc_auc, early_retrieval_gain) {
            if (early_retrieval_gain > 0.1) {
                return(.("Excellent early retrieval performance; classifier performs well at low FPR"))
            } else if (early_retrieval_gain > 0.05) {
                return(.("Good early retrieval performance"))
            } else if (early_retrieval_gain > 0) {
                return(.("Moderate early retrieval performance"))
            } else {
                return(.("Limited early retrieval advantage"))
            }
        },

        .interpretConvexHull = function(performance_gap, n_hull_points) {
            if (performance_gap < 0.01) {
                return(.("Classifier is near-optimal; convex hull shows minimal improvement potential"))
            } else if (performance_gap < 0.05) {
                return(.("Classifier performance is good; small optimization potential remains"))
            } else if (performance_gap < 0.1) {
                return(.("Moderate optimization potential; consider threshold tuning"))
            } else {
                return(.("Significant optimization potential; classifier may benefit from recalibration"))
            }
        },

        .checkClassImbalance = function(analysisData) {
            # Get outcome variable
            outcome <- private$.binaryData
            if (is.null(outcome)) {
                outcome <- analysisData[[private$.outcome]]
            }

            # Convert to factor if needed
            if (!is.factor(outcome)) {
                outcome <- as.factor(outcome)
            }

            # Count classes
            class_counts <- table(outcome)
            if (length(class_counts) != 2) {
                return()  # Only check binary classification
            }

            # CRITICAL FIX: Use stored positive class instead of option (which may be empty)
            # private$.positiveClass was set in .prepareData() after determining positive class
            positive_class_label <- if (!is.null(private$.positiveClass) &&
                                       length(private$.positiveClass) > 0 &&
                                       private$.positiveClass != "") {
                private$.positiveClass
            } else {
                # Fallback: assume second level is positive
                levels(outcome)[2]
            }

            if (!positive_class_label %in% names(class_counts)) {
                positive_class_label <- names(class_counts)[2]
            }

            n_positive <- as.integer(class_counts[positive_class_label])
            n_negative <- sum(class_counts) - n_positive

            # If counts are missing or zero, exit gracefully
            if (length(n_positive) == 0 || length(n_negative) == 0 ||
                is.na(n_positive) || is.na(n_negative) ||
                n_positive == 0 || n_negative == 0) {
                return()
            }

            # Calculate ratio (always express as larger:smaller)
            if (n_positive > n_negative) {
                ratio_value <- n_positive / n_negative
                ratio_text <- sprintf("%.2f:1", ratio_value)
            } else {
                ratio_value <- n_negative / n_positive
                ratio_text <- sprintf("1:%.2f", ratio_value)
            }

            # Calculate prevalence and PRC baseline
            prevalence <- n_positive / (n_positive + n_negative)
            prc_baseline <- prevalence

            # Determine if imbalanced
            threshold <- self$options$imbalanceThreshold
            if (is.null(threshold) || length(threshold) == 0 || is.na(threshold)) {
                threshold <- 3
            }
            is_imbalanced <- ratio_value >= threshold

            # Determine severity
            if (ratio_value >= 10) {
                severity <- .("Severe imbalance")
            } else if (ratio_value >= 5) {
                severity <- .("High imbalance")
            } else if (ratio_value >= threshold) {
                severity <- .("Moderate imbalance")
            } else {
                severity <- .("Balanced")
            }

            # Generate recommendation
            if (is_imbalanced && self$options$recommendPRC) {
                recommendation <- .("Use Precision-Recall Curve (PRC) analysis instead of ROC")
            } else if (is_imbalanced) {
                recommendation <- .("Consider class imbalance in interpretation")
            } else {
                recommendation <- .("ROC analysis is appropriate")
            }

            # Populate metrics table
            metricsTable <- self$results$results$imbalanceMetrics
            metricsTable$setRow(rowNo = 1, values = list(
                n_positive = n_positive,
                n_negative = n_negative,
                ratio = ratio_text,
                prevalence = prevalence,
                prc_baseline = prc_baseline,
                imbalance_severity = severity,
                recommendation = recommendation
            ))

            # Generate Notice if imbalanced (replaces HTML warning)
            if (is_imbalanced && self$options$showImbalanceWarning) {
                # Determine notice type based on severity
                notice_type <- if (ratio_value >= 10) {
                    "STRONG_WARNING"
                } else if (ratio_value >= 5) {
                    "STRONG_WARNING"
                } else {
                    "WARNING"
                }

                # Build concise, single-line notice content
                prc_recommendation <- if (self$options$recommendPRC) {
                    " • Consider using Precision-Recall Curve (PRC) analysis instead of ROC for more reliable performance assessment with imbalanced data."
                } else {
                    " • Interpret ROC results cautiously given class imbalance."
                }

                private$.addNotice(
                    type = notice_type,
                    title = "Class Imbalance Detected",
                    content = paste0("Class imbalance detected: ", ratio_text, " ratio (", n_positive, " positive, ", n_negative, " negative, prevalence ", round(prevalence * 100, 1), "%). • ", severity, ". • ROC curves may be optimistic because specificity is dominated by majority class. • High AUC may mask poor minority class performance.", prc_recommendation)
                )
            }
        },

        .populateComprehensiveAnalysis = function() {
            compTable <- self$results$results$comprehensiveAnalysisSummary

            # Summary statistics
            n_predictors <- length(private$.rocResults)
            n_observations <- nrow(private$.data)

            measures <- list(
                list(.("Number of Predictors"), as.character(n_predictors),
                     .("Number of predictor variables analyzed")),
                list(.("Sample Size"), as.character(n_observations),
                     .("Total number of observations in analysis")),
                list(.("Clinical Context"), self$options$clinicalContext,
                     .("Clinical application context for interpretation")),
                list(.("Analysis Type"), self$options$analysisType,
                     .("Type of ROC analysis performed"))
            )

            # Add best performing predictor
            if (n_predictors > 0) {
                best_auc <- 0
                best_predictor <- ""
                for (pred in names(private$.rocResults)) {
                    auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                    if (auc_val > best_auc) {
                        best_auc <- auc_val
                        best_predictor <- pred
                    }
                }
                measures[[length(measures) + 1]] <- list(
                    .("Best Predictor"), paste(best_predictor, "(AUC =", round(best_auc, 3), ")"),
                    .("Predictor with highest AUC value")
                )
            }

            for (i in 1:length(measures)) {
                measure <- measures[[i]]
                row <- list(
                    measure = measure[[1]],
                    value = measure[[2]],
                    interpretation = measure[[3]],
                    clinical_significance = .("Descriptive statistic")
                )
                compTable$addRow(rowKey = i, values = row)
            }
        },

        .populateMethodsExplanation = function() {
            html <- self$results$results$methodsExplanation
            methods_text <- paste0(
                "<h3>", .("Statistical Methods and References"), "</h3>",
                "<p><b>", .("ROC Curve Analysis"), ":</b> ",
                .("Receiver Operating Characteristic (ROC) curves were generated using the pROC package (Robin et al., 2011). AUC and confidence intervals were computed using DeLong's method (DeLong et al., 1988) unless bootstrap CIs were requested."), "</p>",
                "<p><b>", .("Optimal Cutoff Selection"), ":</b> ",
                .("Optimal cutpoints were determined by maximizing the Youden Index (J = Sensitivity + Specificity - 1; Youden, 1950). Custom cutoffs and sensitivity/specificity thresholds are also supported."), "</p>",
                "<p><b>", .("Partial AUC"), ":</b> ",
                .("Partial AUC was computed over user-specified specificity/sensitivity ranges and normalized using the McClish correction (McClish, 1989) to a 0.5-1.0 scale for comparability."), "</p>",
                "<p><b>", .("Calibration"), ":</b> ",
                .("Model calibration was assessed using the Hosmer-Lemeshow goodness-of-fit test (Hosmer & Lemeshow, 2000) and Brier score. Calibration plots show observed vs. predicted probabilities."), "</p>",
                "<p><b>", .("Internal Validation"), ":</b> ",
                .("Bootstrap validation uses Harrell's optimism-correction method (Harrell, 2015): corrected AUC = original AUC - mean(optimism), where optimism = apparent - test performance across bootstrap resamples. Cross-validation fits logistic models on training folds and evaluates on test folds."), "</p>",
                "<p><b>", .("Clinical Metrics"), ":</b> ",
                .("Predictive values (PPV, NPV) were adjusted for prevalence using Bayes' theorem. Likelihood ratios and diagnostic odds ratios follow standard definitions."), "</p>",
                "<h4>", .("References"), "</h4>",
                "<ul>",
                "<li>Robin X, et al. (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12:77.</li>",
                "<li>DeLong ER, et al. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves. Biometrics, 44(3):837-845.</li>",
                "<li>Youden WJ (1950). Index for rating diagnostic tests. Cancer, 3(1):32-35.</li>",
                "<li>McClish DK (1989). Analyzing a portion of the ROC curve. Medical Decision Making, 9(3):190-195.</li>",
                "<li>Hosmer DW, Lemeshow S (2000). Applied Logistic Regression. 2nd ed. Wiley.</li>",
                "<li>Harrell FE (2015). Regression Modeling Strategies. 2nd ed. Springer.</li>",
                "</ul>"
            )
            html$setContent(methods_text)
        },

        .populateClinicalInterpretation = function() {
            html <- self$results$results$clinicalInterpretationGuide

            interpretation <- "<h3>Clinical Application Guidance</h3>"

            if (length(private$.rocResults) > 0) {
                context <- self$options$clinicalContext

                interpretation <- paste0(interpretation,
                    "<p><strong>Clinical Context:</strong> ", private$.safeHtmlOutput(paste0(toupper(substring(context, 1, 1)),
                    substring(context, 2))), " application</p>"
                )

                interpretation <- paste0(interpretation,
                    "<h4>AUC Interpretation Guidelines:</h4>",
                    "<ul>",
                    "<li><strong>Excellent:</strong> AUC ≥ 0.90 - High clinical utility</li>",
                    "<li><strong>Good:</strong> AUC 0.80-0.89 - Moderate clinical utility</li>",
                    "<li><strong>Fair:</strong> AUC 0.70-0.79 - Limited clinical utility</li>",
                    "<li><strong>Poor:</strong> AUC 0.60-0.69 - Minimal clinical utility</li>",
                    "<li><strong>No discrimination:</strong> AUC ≤ 0.59 - No clinical utility</li>",
                    "</ul>"
                )

                # Context-specific guidance
                if (context == "screening") {
                    interpretation <- paste0(interpretation,
                        "<h4>Screening Application Guidelines:</h4>",
                        "<ul>",
                        "<li>High sensitivity (>90%) preferred to minimize false negatives</li>",
                        "<li>Specificity should be balanced to avoid excessive false positives</li>",
                        "<li>Consider prevalence effects on positive predictive value</li>",
                        "</ul>"
                    )
                } else if (context == "diagnosis") {
                    interpretation <- paste0(interpretation,
                        "<h4>Diagnostic Application Guidelines:</h4>",
                        "<ul>",
                        "<li>Balance sensitivity and specificity based on clinical consequences</li>",
                        "<li>High specificity preferred for confirmatory testing</li>",
                        "<li>Consider likelihood ratios for clinical decision-making</li>",
                        "</ul>"
                    )
                }

                interpretation <- paste0(interpretation,
                    "<h4>Clinical Decision Points:</h4>",
                    "<ul>",
                    "<li><strong>Youden Index:</strong> Optimal balance of sensitivity and specificity</li>",
                    "<li><strong>Clinical Thresholds:</strong> Consider cost of false positives vs. false negatives</li>",
                    "<li><strong>Prevalence:</strong> Higher prevalence increases positive predictive value</li>",
                    "</ul>"
                )
            }

            html$setContent(interpretation)
        },

        # Helper functions for interpretations
        .interpretAUC = function(auc) {
            if (auc >= 0.90) return(.("Excellent"))
            if (auc >= 0.80) return(.("Good"))
            if (auc >= 0.70) return(.("Fair"))
            if (auc >= 0.60) return(.("Poor"))
            return(.("No discrimination"))
        },

        .assessClinicalUtility = function(auc, context) {
            utility_level <- if (auc >= 0.80) .("High")
                           else if (auc >= 0.70) .("Moderate")
                           else if (auc >= 0.60) .("Limited")
                           else .("Minimal")

            if (context == "screening" && auc < 0.75) {
                return(paste(utility_level, .("- May not meet screening standards")))
            } else if (context == "diagnosis" && auc < 0.80) {
                return(paste(utility_level, .("- Consider combining with other markers")))
            } else {
                return(paste(utility_level, .("clinical utility")))
            }
        },

        .generateClinicalRecommendation = function(optimal, context, predictor) {
            sens <- optimal$sensitivity
            spec <- optimal$specificity

            if (context == "screening") {
                if (sens >= 0.90 && spec >= 0.70) {
                    return(.("Suitable for screening - high sensitivity with acceptable specificity"))
                } else if (sens >= 0.85) {
                    return(.("Consider for screening - good sensitivity but monitor false positives"))
                } else {
                    return(.("Not recommended for screening - insufficient sensitivity"))
                }
            } else if (context == "diagnosis") {
                if (sens >= 0.80 && spec >= 0.80) {
                    return(.("Good diagnostic performance - balanced sensitivity and specificity"))
                } else if (spec >= 0.90) {
                    return(.("Suitable for confirmatory testing - high specificity"))
                } else {
                    return(.("Consider combining with additional markers"))
                }
            } else {
                youden <- optimal$youden_index
                if (youden >= 0.6) {
                    return("Strong discriminatory performance")
                } else if (youden >= 0.4) {
                    return("Moderate discriminatory performance")
                } else {
                    return("Limited discriminatory performance")
                }
            }
        },

        .interpretClinicalMetrics = function(ppv, npv, lr_pos, lr_neg, context) {
            interpretation <- ""

            # Likelihood ratio interpretation
            if (lr_pos >= 10) {
                interpretation <- .("Strong evidence for positive diagnosis")
            } else if (lr_pos >= 5) {
                interpretation <- .("Moderate evidence for positive diagnosis")
            } else if (lr_pos >= 2) {
                interpretation <- .("Weak evidence for positive diagnosis")
            } else {
                interpretation <- .("Limited diagnostic value")
            }

            # Add NPV/PPV context
            if (context == "screening") {
                if (npv >= 0.95) {
                    interpretation <- paste(interpretation, .("- Excellent rule-out capability"))
                } else {
                    interpretation <- paste(interpretation, .("- Consider NPV for screening adequacy"))
                }
            }

            return(interpretation)
        },

        .assessClinicalSignificanceDifference = function(auc_diff, context) {
            if (auc_diff >= 0.1) {
                return(.("Clinically meaningful difference"))
            } else if (auc_diff >= 0.05) {
                return(.("Potentially meaningful difference"))
            } else {
                return(.("Minimal clinical difference"))
            }
        },

        .assessPartialAUCRelevance = function(pauc, range_min, range_max, context, range_type = "specificity") {
            # Provide context-aware relevance assessment
            if (range_type == "sensitivity") {
                if (context == "screening" && range_min >= 0.8) {
                    return(.("Relevant for high-sensitivity screening applications (minimizing false negatives)"))
                } else if (range_min >= 0.9) {
                    return(.("Very high sensitivity range - critical for screening/case-finding"))
                } else {
                    return(.("Partial AUC over sensitivity range"))
                }
            } else {
                # Specificity-based assessment
                if (context == "screening" && range_min >= 0.8) {
                    return(.("Relevant for high-specificity screening applications (minimizing false positives)"))
                } else if (context == "diagnosis" && range_min >= 0.9) {
                    return(.("Confirmatory diagnostic range with high specificity"))
                } else if (context == "diagnosis") {
                    return(.("Partial AUC for specific diagnostic range"))
                } else {
                    return(.("Partial area under curve analysis"))
                }
            }
        },

        .calculateBinomialCI = function(successes, n) {
            # Exact (Clopper-Pearson) confidence interval
            bt <- suppressWarnings(binom.test(successes, n))
            return(bt$conf.int)
        },

        .computePRMetrics = function(scores, labels, positive_label, roc_direction = "<") {
            # Simple PR curve and AUPRC calculation without extra dependencies
            y <- as.integer(labels == positive_label)
            if (length(unique(y)) < 2) {
                return(NULL)
            }
            # pROC direction "<" means higher values = positive (sort descending)
            # pROC direction ">" means lower values = positive (negate to sort descending)
            if (roc_direction == ">") scores <- -scores
            ord <- order(scores, decreasing = TRUE)
            y_sorted <- y[ord]
            tp_cum <- cumsum(y_sorted)
            fp_cum <- cumsum(1 - y_sorted)
            total_pos <- sum(y_sorted)
            recall <- tp_cum / total_pos
            precision <- tp_cum / (tp_cum + fp_cum)

            # prepend (0,1) for proper step interpolation
            recall <- c(0, recall)
            precision <- c(1, precision)

            auprc <- sum(diff(recall) * precision[-1], na.rm = TRUE)
            f1_denom <- precision + recall
            f1_scores <- ifelse(f1_denom > 0, 2 * precision * recall / f1_denom, NA_real_)
            best_f1 <- max(f1_scores, na.rm = TRUE)
            if (is.infinite(best_f1)) best_f1 <- NA_real_

            list(
                auprc = auprc,
                max_f1 = best_f1,
                precision = ifelse(length(precision) > 1, precision[2], NA),
                recall = ifelse(length(recall) > 1, recall[2], NA),
                avg_precision = auprc
            )
        },

        .populatePrecisionRecall = function(analysisData) {
            if (!self$options$detectImbalance) return()

            prTable <- self$results$results$precisionRecallTable
            outcome <- analysisData[[private$.outcome]]

            for (predictor in private$.predictors) {
                scores <- analysisData[[predictor]]
                roc_dir <- if (!is.null(private$.rocObjects[[predictor]])) private$.rocObjects[[predictor]]$direction else "<"
                pr <- private$.computePRMetrics(scores, outcome,
                                               private$.positiveClass %||% levels(outcome)[2],
                                               roc_dir)
                if (!is.null(pr)) {
                    prTable$addRow(rowKey = private$.escapeVar(predictor), values = list(
                        predictor = predictor,
                        auc_pr = pr$auprc,
                        f1_score = pr$max_f1,
                        precision = pr$precision,
                        recall = pr$recall,
                        average_precision = pr$avg_precision
                    ))
                }
            }
        },

        .cleanupROCObjects = function() {
            # Manage memory by cleaning up old ROC objects
            if (length(private$.rocObjects) > 20) {
                # Keep only the most recent 15 objects
                keep_indices <- max(1, length(private$.rocObjects) - 14):length(private$.rocObjects)
                private$.rocObjects <- private$.rocObjects[keep_indices]
                
                # Force garbage collection to free memory
                gc(verbose = FALSE)
            }
        },
        
        .getColorblindSafePalette = function(n) {
            # Colorblind-safe palette based on Cynthia Brewer's research
            # These colors are distinguishable for most types of color vision deficiency
            cb_palette <- c(
                "#0173B2",  # Blue
                "#DE8F05",  # Orange  
                "#029E73",  # Green
                "#CC78BC",  # Pink
                "#CA9161",  # Brown
                "#FBAFE4",  # Light pink
                "#949494",  # Grey
                "#ECE133",  # Yellow
                "#56B4E9"   # Light blue
            )
            
            if (n <= length(cb_palette)) {
                return(cb_palette[1:n])
            } else {
                # For more than 9 colors, interpolate
                return(colorRampPalette(cb_palette)(n))
            }
        },
        
        .applyClinicalPresets = function() {
            # Apply predefined clinical settings based on selected preset
            preset <- self$options$clinicalPresets
            
            if (preset == "custom") {
                return()  # No changes for custom configuration
            }
            
            # Apply preset-specific configurations (these would be applied programmatically)
            # Note: In jamovi, we can't directly modify options from backend, 
            # but we can use the preset to inform our analysis approach
            
            if (preset == "biomarker_screening") {
                # High sensitivity configuration
                private$.presetConfig <- list(
                    focus = "sensitivity",
                    threshold = 0.90,
                    clinical_priority = "screening",
                    recommendation = .("Optimized for biomarker screening with high sensitivity")
                )
            } else if (preset == "diagnostic_validation") {
                # Balanced sensitivity and specificity
                private$.presetConfig <- list(
                    focus = "balanced",
                    sens_threshold = 0.80,
                    spec_threshold = 0.80,
                    clinical_priority = "diagnosis", 
                    recommendation = .("Balanced approach for diagnostic test validation")
                )
            } else if (preset == "confirmatory_testing") {
                # High specificity configuration
                private$.presetConfig <- list(
                    focus = "specificity",
                    threshold = 0.95,
                    clinical_priority = "diagnosis",
                    recommendation = .("Optimized for confirmatory testing with high specificity")
                )
            } else if (preset == "research_comprehensive") {
                # Comprehensive analysis
                private$.presetConfig <- list(
                    focus = "comprehensive",
                    clinical_priority = "research",
                    recommendation = .("Comprehensive analysis for research applications")
                )
            }
        },

        .generateAnalysisSummary = function() {
            html <- self$results$results$analysisSummary
            
            # Generate plain language summary
            n_predictors <- length(private$.rocResults)
            n_obs <- nrow(private$.data)
            context <- self$options$clinicalContext

            # Find best performing predictor
            best_auc <- 0
            best_predictor <- ""
            for (pred in names(private$.rocResults)) {
                auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                if (auc_val > best_auc) {
                    best_auc <- auc_val
                    best_predictor <- pred
                }
            }
            
            # Generate summary paragraph
            summary_text <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
                "<h4 style='margin-top: 0; color: #007bff;'>", .("Analysis Summary"), "</h4>",
                "<p><strong>", .("ROC Analysis Results:"), "</strong> ",
                sprintf(.("This analysis evaluated %d predictor(s) using %d observations in a %s context."),
                        n_predictors, n_obs, private$.safeHtmlOutput(context)), " "
            )

            if (best_auc > 0) {
                interpretation <- private$.interpretAUC(best_auc)
                clinical_utility <- private$.assessClinicalUtility(best_auc, context)

                summary_text <- paste0(summary_text,
                    sprintf(.("The best performing predictor was '%s' with an AUC of %.3f (%s performance, %s)."),
                            private$.safeHtmlOutput(best_predictor), best_auc, private$.safeHtmlOutput(interpretation), private$.safeHtmlOutput(clinical_utility)),
                    "</p>"
                )
                
                # Add clinical recommendation
                if (context == "screening" && best_auc >= 0.8) {
                    summary_text <- paste0(summary_text, 
                        "<p><strong>", .("Clinical Recommendation"), ":</strong> ",
                        .("This predictor shows good potential for screening applications."), "</p>")
                } else if (context == "diagnosis" && best_auc >= 0.8) {
                    summary_text <- paste0(summary_text,
                        "<p><strong>", .("Clinical Recommendation"), ":</strong> ",
                        .("This predictor demonstrates good diagnostic performance."), "</p>")
                } else {
                    summary_text <- paste0(summary_text,
                        "<p><strong>", .("Note"), ":</strong> ",
                        .("Consider combining with additional markers or clinical information."), "</p>")
                }
            }
            
            summary_text <- paste0(summary_text, "</div>")
            private$.analysisSummaryHtml <- summary_text
            html$setContent(summary_text)
        },
        
        .generateClinicalReport = function() {
            html <- self$results$results$clinicalReport
            
            # Generate copy-ready clinical report sentences
            n_predictors <- length(private$.rocResults)
            n_obs <- nrow(private$.data)
            context <- self$options$clinicalContext

            # Find best performing predictor
            best_auc <- 0
            best_predictor <- ""
            best_cutoff <- NULL
            for (pred in names(private$.rocResults)) {
                auc_val <- as.numeric(private$.rocObjects[[pred]]$auc)
                if (auc_val > best_auc) {
                    best_auc <- auc_val
                    best_predictor <- pred
                    best_cutoff <- private$.rocResults[[pred]]$optimal_cutoff
                }
            }
            
            if (best_auc == 0) {
                private$.addNotice(
                    type = "WARNING",
                    title = "No Valid ROC Results",
                    content = "No valid ROC results available for report generation. • ROC analysis may have failed for all predictors. • Check that outcome variable is binary and predictors are numeric. • Verify sufficient data for each predictor-outcome combination."
                )
                return()
            }
            
            # Calculate confidence interval for best predictor
            ci <- private$.rocObjects[[best_predictor]]$ci
            ci_lower <- if (!is.null(ci)) round(as.numeric(ci)[1], 3) else "N/A"
            ci_upper <- if (!is.null(ci)) round(as.numeric(ci)[3], 3) else "N/A"
            
            # Generate report sections
            report_html <- "<div style='background-color: #f0f8ff; border: 1px solid #0066cc; padding: 15px; margin: 10px 0;'>"
            report_html <- paste0(report_html, "<h4 style='color: #0066cc; margin-top: 0;'>", .("Clinical Report Sentences"), "</h4>")
            report_html <- paste0(report_html, "<p><em>", .("Copy and paste the sections below into your clinical reports or publications"), ":</em></p>")
            
            # Methods section
            report_html <- paste0(report_html, "<h5>", .("Methods Section"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #0066cc; margin: 5px 0;'>")
            methods_text <- sprintf(.("ROC analysis was performed to evaluate the diagnostic performance of %s in predicting %s using %d observations. The analysis was conducted using the pROC package in R, with AUC calculation and %d%% confidence intervals determined using %s methodology."),
                                   ifelse(n_predictors == 1, paste0("'", private$.safeHtmlOutput(best_predictor), "'"), paste(n_predictors, "predictors")),
                                   private$.safeHtmlOutput(private$.outcome),
                                   n_obs,
                                   self$options$confidenceLevel,
                                   ifelse(self$options$useBootstrap, "bootstrap", "DeLong"))
            report_html <- paste0(report_html, methods_text, "</div>")
            
            # Results section  
            report_html <- paste0(report_html, "<h5>", .("Results Section"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #28a745; margin: 5px 0;'>")
            
            results_text <- sprintf(.("The %s predictor demonstrated %s diagnostic performance with an AUC of %.3f (95%% CI: %s--%s). At the optimal cutoff of %.3f, the test achieved %s sensitivity (%.1f%%) and %s specificity (%.1f%%), resulting in a Youden Index of %.3f."),
                                   private$.safeHtmlOutput(best_predictor),
                                   private$.interpretAUC(best_auc),
                                   best_auc,
                                   ci_lower, ci_upper,
                                   best_cutoff$cutoff,
                                   ifelse(best_cutoff$sensitivity >= 0.8, .("high"), ifelse(best_cutoff$sensitivity >= 0.7, .("moderate"), .("low"))),
                                   best_cutoff$sensitivity * 100,
                                   ifelse(best_cutoff$specificity >= 0.8, .("high"), ifelse(best_cutoff$specificity >= 0.7, .("moderate"), .("low"))),
                                   best_cutoff$specificity * 100,
                                   best_cutoff$youden_index)

            # Add NND (Number Needed to Diagnose) = 1 / Youden Index
            nnd_text <- ""
            if (!is.na(best_cutoff$youden_index) && best_cutoff$youden_index > 0) {
                nnd <- ceiling(1 / best_cutoff$youden_index)
                nnd_text <- sprintf(.(" The Number Needed to Diagnose (NND) was %d, meaning on average %d patients need to be tested to correctly identify one additional true positive beyond chance."),
                                   nnd, nnd)
            }

            report_html <- paste0(report_html, results_text, nnd_text, "</div>")
            
            # Clinical interpretation
            report_html <- paste0(report_html, "<h5>", .("Clinical Interpretation"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #ffc107; margin: 5px 0;'>")
            
            clinical_utility <- private$.assessClinicalUtility(best_auc, context)
            interpretation_text <- sprintf(.("These findings suggest that %s has %s for %s applications. The observed AUC indicates %s discriminatory ability, which %s for clinical implementation in this context."),
                                         private$.safeHtmlOutput(best_predictor),
                                         private$.safeHtmlOutput(clinical_utility),
                                         private$.safeHtmlOutput(context),
                                         ifelse(best_auc >= 0.8, .("good to excellent"), ifelse(best_auc >= 0.7, .("fair to good"), .("limited"))),
                                         ifelse(best_auc >= 0.8, .("supports consideration"), .("requires careful evaluation")))
            
            report_html <- paste0(report_html, interpretation_text, "</div>")
            
            # Statistical reporting
            if (n_predictors > 1) {
                report_html <- paste0(report_html, "<h5>", .("Comparative Analysis"), ":</h5>")
                report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #dc3545; margin: 5px 0;'>")
                if (isTRUE(self$options$pairwiseComparisons)) {
                    comparative_text <- sprintf(.("Among the %d predictors evaluated, %s demonstrated superior performance. Pairwise comparisons were conducted using %s methodology to assess statistical significance of observed differences."),
                                               n_predictors,
                                               private$.safeHtmlOutput(best_predictor),
                                               private$.safeHtmlOutput(self$options$comparisonMethod))
                } else {
                    comparative_text <- sprintf(.("Among the %d predictors evaluated, %s demonstrated superior performance (AUC = %.3f). Enable pairwise comparisons to test whether observed AUC differences are statistically significant."),
                                               n_predictors,
                                               private$.safeHtmlOutput(best_predictor),
                                               best_auc)
                }
                report_html <- paste0(report_html, comparative_text, "</div>")
            }
            
            report_html <- paste0(report_html, "</div>")
            
            html$setContent(report_html)
        },

        .validateClinicalAssumptions = function(data) {
            warnings <- c()
            
            # Check sample size adequacy
            n_obs <- nrow(data)
            if (n_obs < 30) {
                warnings <- c(warnings, 
                    paste0("<p><strong>", .("Sample Size Warning"), ":</strong> ",
                           sprintf(.("With only %d observations, results may be unstable. Consider %d+ observations for reliable ROC analysis."), n_obs, 50),
                           "</p>"))
            }
            
            # Check outcome balance
            outcome_table <- table(data[[private$.outcome]])
            min_group_size <- min(outcome_table)
            min_group_pct <- (min_group_size / n_obs) * 100
            
            if (min_group_pct < 10) {
                warnings <- c(warnings,
                    paste0("<p><strong>", .("Outcome Balance Warning"), ":</strong> ",
                           sprintf(.("The smaller outcome group has only %.1f%% of observations (%d cases). This may affect ROC reliability."), 
                                   min_group_pct, min_group_size), "</p>"))
            }
            
            # Check for extreme prevalence in clinical context
            if (self$options$clinicalContext == "screening" && min_group_pct > 30) {
                warnings <- c(warnings,
                    paste0("<p><strong>", .("Screening Context Note"), ":</strong> ",
                           .("High disease prevalence (>30%) is unusual for screening populations. Consider if this reflects your target population."),
                           "</p>"))
            }
            
            # Check predictor variability
            for (pred in private$.predictors) {
                pred_var <- var(data[[pred]], na.rm = TRUE)
                if (is.na(pred_var) || pred_var == 0) {
                    warnings <- c(warnings,
                        paste0("<p><strong>", .("Predictor Warning"), ":</strong> ",
                               sprintf(.("Predictor '%s' has no variation. ROC analysis requires variable predictors."), private$.safeHtmlOutput(pred)),
                               "</p>"))
                }
                
                # Check for extreme skewness (using basic calculation)
                tryCatch({
                    pred_values <- data[[pred]][!is.na(data[[pred]])]
                    if (length(pred_values) > 3) {
                        # Simple skewness calculation
                        mean_val <- mean(pred_values)
                        sd_val <- sd(pred_values)
                        if (sd_val > 0) {
                            skew_values <- ((pred_values - mean_val) / sd_val)^3
                            pred_skew <- abs(mean(skew_values))
                            if (pred_skew > 2) {
                                warnings <- c(warnings,
                                    paste0("<p><strong>", .("Distribution Note"), ":</strong> ",
                                           sprintf(.("Predictor '%s' appears highly skewed. Consider transformation for better clinical interpretation."), private$.safeHtmlOutput(pred)),
                                           "</p>"))
                            }
                        }
                    }
                }, error = function(e) {
                    # Skip skewness check if calculation fails
                })
            }
            
            return(warnings)
        },

        .calculateMetricPValue = function(metric, result1, result2, pred1, pred2) {
            # Calculate p-value for metric comparison based on the metric type
            tryCatch({
                if (metric == "AUC") {
                    # For AUC comparison, use ROC test
                    roc_test <- pROC::roc.test(result1$roc, result2$roc, method = self$options$comparisonMethod)
                    return(roc_test$p.value)
                } else {
                    # For paired metrics (same subjects, different predictors), use McNemar's test
                    if (metric == "Sensitivity" || metric == "Specificity" || metric == "Accuracy") {
                        cutoff1 <- result1$optimal_cutoff
                        cutoff2 <- result2$optimal_cutoff

                        # Build paired classification table from raw data
                        data <- private$.data
                        outcome <- data[[private$.outcome]]
                        pred1_vals <- data[[pred1]]
                        pred2_vals <- data[[pred2]]

                        # Classify each subject by both predictors at their optimal cutoffs
                        # pROC: "<" means controls < cases (higher values = positive)
                        dir1 <- result1$roc$direction
                        dir2 <- result2$roc$direction
                        if (dir1 == "<") {
                            class1 <- pred1_vals >= cutoff1$cutoff
                        } else {
                            class1 <- pred1_vals < cutoff1$cutoff
                        }
                        if (dir2 == "<") {
                            class2 <- pred2_vals >= cutoff2$cutoff
                        } else {
                            class2 <- pred2_vals < cutoff2$cutoff
                        }

                        # Subset based on metric
                        pos_class <- levels(result1$roc$response)[2]
                        if (metric == "Sensitivity") {
                            # Restrict to true positives
                            mask <- outcome == pos_class
                        } else if (metric == "Specificity") {
                            # Restrict to true negatives; correct = pred NOT positive
                            mask <- outcome != pos_class
                            class1 <- !class1
                            class2 <- !class2
                        } else {
                            # Accuracy: all subjects, correct = match
                            mask <- rep(TRUE, length(outcome))
                            true_pos <- outcome == pos_class
                            class1 <- (class1 & true_pos) | (!class1 & !true_pos)
                            class2 <- (class2 & true_pos) | (!class2 & !true_pos)
                        }

                        c1 <- class1[mask]
                        c2 <- class2[mask]

                        if (length(c1) >= 2) {
                            # McNemar's test: are the two predictors equally accurate on paired data?
                            cont_table <- table(Pred1 = c1, Pred2 = c2)
                            if (nrow(cont_table) == 2 && ncol(cont_table) == 2) {
                                mcnemar_result <- stats::mcnemar.test(cont_table, correct = FALSE)
                                return(mcnemar_result$p.value)
                            }
                        }
                    }
                }
                
                # If specific test fails, return NA
                return(NA)
                
            }, error = function(e) {
                # Return NA if statistical test fails
                return(NA)
            })
        },

        .getInstructions = function() {
            instructions <- "<div style='font-family: Arial, sans-serif;'>"
            instructions <- paste0(instructions,
                "<h2 style='color: #2c5530;'>Enhanced ROC Analysis</h2>",
                "<div style='background: #f8f9fa; padding: 15px; border-left: 4px solid #28a745; margin: 10px 0;'>",
                "<p><strong>Purpose:</strong> ROC (Receiver Operating Characteristic) analysis evaluates how well continuous variables (biomarkers, test scores) can distinguish between two outcome groups (e.g., disease vs. healthy).</p>",
                "</div>",
                
                "<h3 style='color: #2c5530;'> Setup Instructions:</h3>",
                "<ol style='margin-left: 20px;'>",
                "<li><strong>Outcome Variable:</strong> Select your binary outcome (e.g., Disease Status, Survival Status)</li>",
                "<ul style='margin-left: 20px; color: #6c757d;'>",
                "<li>If you have >2 levels (e.g., DOD, DOOC, AWD, AWOD), select which level represents 'positive' cases</li>",
                "</ul>",
                "<li><strong>Predictor Variables:</strong> Choose continuous predictors (e.g., Age, Biomarker levels, Test scores)</li>",
                "<li><strong>Positive Class:</strong> Specify which outcome level represents the condition of interest</li>",
                "<li><strong>Clinical Context:</strong> Select appropriate context for tailored interpretation</li>",
                "</ol>",
                
                "<h3 style='color: #2c5530;'> Quick Start Presets:</h3>",
                "<div style='background: #e7f3ff; padding: 10px; border-radius: 4px; margin: 10px 0;'>",
                "<p><strong>Clinical Presets</strong> automatically configure analysis settings:</p>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>Biomarker Screening:</strong> High sensitivity (catch all cases)</li>",
                "<li><strong>Diagnostic Validation:</strong> Balanced sensitivity/specificity</li>",
                "<li><strong>Confirmatory Testing:</strong> High specificity (minimize false positives)</li>",
                "<li><strong>Research Comprehensive:</strong> Full statistical analysis</li>",
                "</ul>",
                "</div>",
                
                "<h3 style='color: #2c5530;'> Key Output Metrics:</h3>",
                "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin: 10px 0;'>",
                "<div style='background: #fff3cd; padding: 10px; border-radius: 4px; flex: 1; min-width: 250px;'>",
                "<strong>AUC (Area Under Curve):</strong><br>",
                "• Below 0.60 = No discrimination<br>",
                "• 0.60-0.69 = Poor<br>",
                "• 0.70-0.79 = Fair<br>",
                "• 0.80-0.89 = Good<br>",
                "• 0.90-1.00 = Excellent",
                "</div>",
                "<div style='background: #d1ecf1; padding: 10px; border-radius: 4px; flex: 1; min-width: 250px;'>",
                "<strong>Youden Index:</strong><br>",
                "Optimal cutoff that maximizes<br>",
                "(Sensitivity + Specificity - 1)<br>",
                "Best balance of true/false rates",
                "</div>",
                "</div>",
                
                "<details style='margin: 15px 0; padding: 10px; background: #f8f9fa; border-radius: 4px;'>",
                "<summary style='cursor: pointer; font-weight: bold; color: #495057;'> Statistical Terms Glossary</summary>",
                "<div style='margin-top: 10px; padding-left: 20px;'>",
                "<p><strong>Sensitivity (True Positive Rate):</strong> Proportion of actual positives correctly identified</p>",
                "<p><strong>Specificity (True Negative Rate):</strong> Proportion of actual negatives correctly identified</p>",
                "<p><strong>PPV (Positive Predictive Value):</strong> When test is positive, probability patient has condition</p>",
                "<p><strong>NPV (Negative Predictive Value):</strong> When test is negative, probability patient is healthy</p>",
                "<p><strong>LR+ (Positive Likelihood Ratio):</strong> How much a positive test increases odds of disease</p>",
                "<p><strong>LR- (Negative Likelihood Ratio):</strong> How much a negative test decreases odds of disease</p>",
                "</div>",
                "</details>",
                
                "<div style='background: #d4edda; padding: 10px; border: 1px solid #c3e6cb; border-radius: 4px; margin: 10px 0;'>",
                "<p style='margin: 0;'><strong> Tip:</strong> For clinical decision making, consider both statistical significance and clinical relevance of the cutoff thresholds.</p>",
                "</div>",
                
                "</div>"
            )
            return(instructions)
        },

        # Plotting functions
        .plotROCCurve = function(image, ggtheme, theme, ...) {
            # Check if ROC curve plotting is enabled
            if (!self$options$rocCurve) {
                return(FALSE)
            }
            
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # Create ROC curve plot
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    return(FALSE)
                }

                # Prepare data for plotting
                plot_data <- data.frame()

                for (predictor in names(private$.rocResults)) {
                    roc_obj <- private$.rocResults[[predictor]]$roc

                    # Validate ROC object
                    if (is.null(roc_obj) || !inherits(roc_obj, "roc")) {
                        next
                    }

                    # Extract ROC curve coordinates
                    coords <- data.frame(
                        FPR = 1 - roc_obj$specificities,
                        TPR = roc_obj$sensitivities,
                        Predictor = predictor
                    )
                    plot_data <- rbind(plot_data, coords)
                }

                # Check if we have valid plot data
                if (nrow(plot_data) == 0) {
                    return(FALSE)
                }

                # Create the plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor)) +
                    ggplot2::geom_line(linewidth = 1.2) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    ggplot2::labs(
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)",
                        title = "ROC Curve Analysis",
                        color = "Predictor"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                # Apply colorblind-safe palette
                cb_colors <- private$.getColorblindSafePalette(length(unique(plot_data$Predictor)))
                p <- p + ggplot2::scale_color_manual(values = cb_colors)

                # Add confidence bands if requested
                if (self$options$showConfidenceBands) {
                    for (predictor in names(private$.rocResults)) {
                        roc_obj <- private$.rocResults[[predictor]]$roc
                        
                        # Only add confidence bands if CI is available
                        if (!is.null(roc_obj$ci)) {
                            # Use specificity values from the ROC curve for ci.coords
                            specificity_values <- seq(0.1, 0.9, by = 0.1)  # 9 points from 0.1 to 0.9
                            
                            ci_data_list <- list()
                            for (i in seq_along(specificity_values)) {
                                # Checkpoint before expensive CI calculation
                                private$.checkpoint()
                                
                                spec_val <- specificity_values[i]
                                
                                # Get CI for sensitivity at this specificity level
                                ci_result <- pROC::ci.coords(roc_obj, x = spec_val, 
                                                           input = "specificity", 
                                                           ret = "sensitivity",
                                                           conf.level = self$options$confidenceLevel / 100)
                                
                                if (!is.null(ci_result) && !is.null(ci_result$sensitivity) && 
                                    is.matrix(ci_result$sensitivity) && nrow(ci_result$sensitivity) >= 1) {
                                    
                                    ci_data_list[[i]] <- data.frame(
                                        FPR = 1 - spec_val,  # 1 - specificity
                                        TPR_lower = ci_result$sensitivity[1, 1],   # lower CI
                                        TPR_median = ci_result$sensitivity[1, 2],  # median
                                        TPR_upper = ci_result$sensitivity[1, 3],   # upper CI
                                        Predictor = predictor
                                    )
                                }
                            }
                            
                            if (length(ci_data_list) > 0) {
                                ci_data <- do.call(rbind, ci_data_list)
                                ci_data <- ci_data[complete.cases(ci_data), ]  # Remove any NA rows
                                
                                if (nrow(ci_data) > 0) {
                                    # Add confidence bands as ribbons
                                    p <- p + ggplot2::geom_ribbon(data = ci_data, 
                                                           ggplot2::aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper, 
                                                               fill = Predictor),
                                                           alpha = 0.2, show.legend = FALSE, inherit.aes = FALSE)
                                }
                            }
                        }
                    }
                }

                # Add optimal cutoff points if requested
                if (self$options$showCutoffPoints) {
                    cutoff_data <- data.frame()
                    for (predictor in names(private$.rocResults)) {
                        optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                        cutoff_data <- rbind(cutoff_data, data.frame(
                            FPR = 1 - optimal$specificity,
                            TPR = optimal$sensitivity,
                            Predictor = predictor
                        ))
                    }
                    p <- p + ggplot2::geom_point(data = cutoff_data, size = 3, shape = 16)
                }

                # Apply clinical theme if selected
                if (self$options$plotTheme == "clinical") {
                    p <- p + ggplot2::theme(
                        panel.grid.major = ggplot2::element_line(color = "gray90"),
                        panel.grid.minor = ggplot2::element_line(color = "gray95"),
                        panel.background = ggplot2::element_rect(fill = "white"),
                        strip.background = ggplot2::element_rect(fill = "lightblue")
                    )
                }

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "ROC Curve Plot Error",
                    content = paste0("Failed to create ROC curve plot: ", e$message))
                FALSE
            })
        },

        .plotComparativeROC = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) < 2) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 800
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(FALSE)
            }

            # Prepare comparative data for plotting
            plot_data <- data.frame()
            auc_data <- data.frame()

            for (predictor in names(private$.rocResults)) {
                roc_obj <- private$.rocResults[[predictor]]$roc
                
                # Use the ROC object's built-in coordinates instead of coords()
                predictor_data <- data.frame(
                    FPR = 1 - roc_obj$specificities,
                    TPR = roc_obj$sensitivities,
                    Predictor = predictor
                )
                
                plot_data <- rbind(plot_data, predictor_data)
                    
                    # Store AUC for legend
                    auc_data <- rbind(auc_data, data.frame(
                        Predictor = predictor,
                        AUC = round(as.numeric(roc_obj$auc), 3)
                    ))
                }

                # Create legend labels with AUC values
                auc_data$Legend <- paste0(auc_data$Predictor, " (AUC = ", auc_data$AUC, ")")
                plot_data <- merge(plot_data, auc_data[c("Predictor", "Legend")], by = "Predictor")

                # Create comparative ROC plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Legend)) +
                    ggplot2::geom_line(linewidth = 1.2, alpha = 0.8) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 0.7) +
                    ggplot2::coord_equal() +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::labs(
                        x = "1 - Specificity (False Positive Rate)",
                        y = "Sensitivity (True Positive Rate)",
                        title = "Comparative ROC Analysis",
                        subtitle = "ROC curves with AUC values for comparison",
                        color = "Predictor (AUC)"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11),
                        legend.position = "right",
                        legend.title = ggplot2::element_text(size = 10, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11),
                        panel.grid.minor = ggplot2::element_line(color = "gray95", linewidth = 0.3)
                    )

                # Add optimal cutoff points if enabled
                if (self$options$showCutoffPoints) {
                    optimal_points <- data.frame()
                    for (predictor in names(private$.rocResults)) {
                        optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                        if (!is.null(optimal)) {
                            optimal_points <- rbind(optimal_points, data.frame(
                                FPR = 1 - optimal$specificity,
                                TPR = optimal$sensitivity,
                                Predictor = predictor,
                                Legend = paste0(predictor, " (AUC = ", round(as.numeric(private$.rocResults[[predictor]]$roc$auc), 3), ")")
                            ))
                        }
                    }
                    
                    if (nrow(optimal_points) > 0) {
                        p <- p + ggplot2::geom_point(data = optimal_points, 
                                          ggplot2::aes(x = FPR, y = TPR, color = Legend),
                                          size = 3, shape = 16, alpha = 0.8, inherit.aes = FALSE)
                    }
                }

                # Add comparison annotations
                if (length(auc_data$AUC) >= 2) {
                    # Find best and worst AUC
                    best_idx <- which.max(auc_data$AUC)
                    worst_idx <- which.min(auc_data$AUC)
                    
                    if (best_idx != worst_idx) {
                        auc_diff <- auc_data$AUC[best_idx] - auc_data$AUC[worst_idx]
                        comparison_text <- paste0("Best: ", auc_data$Predictor[best_idx], 
                                                " (AUC = ", auc_data$AUC[best_idx], ")\n",
                                                "Difference: +", round(auc_diff, 3))
                        
                        p <- p + ggplot2::annotate("text", x = 0.7, y = 0.2, label = comparison_text,
                                        hjust = 0, vjust = 0, size = 3.5, 
                                        color = "darkblue", fontface = "italic")
                    }
                }

                # Apply clinical theme if selected
                if (self$options$plotTheme == "clinical") {
                    p <- p + ggplot2::theme(
                        panel.grid.major = ggplot2::element_line(color = "gray90"),
                        panel.grid.minor = ggplot2::element_line(color = "gray95"),
                        panel.background = ggplot2::element_rect(fill = "white"),
                        legend.background = ggplot2::element_rect(fill = "white", color = "gray80")
                    )
                }

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Comparative ROC Plot Error",
                    content = paste0("Failed to create comparative ROC plot: ", e$message))
                FALSE
            })
        },

        .plotCutoffAnalysis = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # ggplot2 available via package Imports

                # Create cutoff analysis plot showing sensitivity/specificity vs threshold
                plot_data <- data.frame()

                for (predictor in names(private$.rocResults)) {
                    roc_obj <- private$.rocResults[[predictor]]$roc
                    coords <- pROC::coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))

                    # Prepare data for long format
                    threshold_data <- data.frame(
                        Threshold = coords$threshold,
                        Sensitivity = coords$sensitivity,
                        Specificity = coords$specificity,
                        Predictor = predictor
                    )

                    # Convert to long format for plotting (base R, no reshape2 dependency)
                    sens_df <- data.frame(Threshold = threshold_data$Threshold, Predictor = threshold_data$Predictor, Metric = "Sensitivity", Value = threshold_data$Sensitivity, stringsAsFactors = FALSE)
                    spec_df <- data.frame(Threshold = threshold_data$Threshold, Predictor = threshold_data$Predictor, Metric = "Specificity", Value = threshold_data$Specificity, stringsAsFactors = FALSE)
                    threshold_long <- rbind(sens_df, spec_df)

                    plot_data <- rbind(plot_data, threshold_long)
                }

                # Create the plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Threshold, y = Value, color = Metric)) +
                    ggplot2::geom_line(linewidth = 1) +
                    ggplot2::labs(
                        x = "Threshold",
                        y = "Performance Metric",
                        title = "Sensitivity and Specificity vs Threshold",
                        color = "Metric"
                    ) +
                    ggplot2::ylim(0, 1) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                if (length(names(private$.rocResults)) > 1) {
                    p <- p + ggplot2::facet_wrap(~Predictor, scales = "free_x")
                }

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Cutoff Analysis Plot Error",
                    content = paste0("Failed to create cutoff analysis plot: ", e$message))
                FALSE
            })
        },

        .plotYoudenIndex = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # ggplot2 available via package Imports

                # Create Youden Index plot
                plot_data <- data.frame()

                for (predictor in names(private$.rocResults)) {
                    roc_obj <- private$.rocResults[[predictor]]$roc
                    coords <- pROC::coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))
                    youden <- coords$sensitivity + coords$specificity - 1

                    youden_data <- data.frame(
                        Threshold = coords$threshold,
                        YoudenIndex = youden,
                        Predictor = predictor
                    )

                    plot_data <- rbind(plot_data, youden_data)
                }

                # Create the plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Threshold, y = YoudenIndex, color = Predictor)) +
                    ggplot2::geom_line(linewidth = 1.2) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                    ggplot2::labs(
                        x = "Threshold",
                        y = "Youden Index (Sensitivity + Specificity - 1)",
                        title = "Youden Index vs Threshold",
                        color = "Predictor"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                # Mark optimal points
                optimal_data <- data.frame()
                for (predictor in names(private$.rocResults)) {
                    optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                    optimal_data <- rbind(optimal_data, data.frame(
                        Threshold = optimal$cutoff,
                        YoudenIndex = optimal$youden_index,
                        Predictor = predictor
                    ))
                }
                p <- p + ggplot2::geom_point(data = optimal_data, size = 3, shape = 16)

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Youden Index Plot Error",
                    content = paste0("Failed to create Youden Index plot: ", e$message))
                FALSE
            })
        },

        .plotClinicalDecision = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # ggplot2 available via package Imports

                # Create clinical decision plot showing PPV/NPV vs prevalence
                plot_data <- data.frame()
                prevalence_range <- seq(0.01, 0.99, 0.01)

                for (predictor in names(private$.rocResults)) {
                    optimal <- private$.rocResults[[predictor]]$optimal_cutoff
                    sens <- optimal$sensitivity
                    spec <- optimal$specificity

                    # Calculate PPV and NPV across prevalence range
                    ppv_vals <- (sens * prevalence_range) /
                                (sens * prevalence_range + (1 - spec) * (1 - prevalence_range))
                    npv_vals <- (spec * (1 - prevalence_range)) /
                                (spec * (1 - prevalence_range) + (1 - sens) * prevalence_range)

                    ppv_data <- data.frame(
                        Prevalence = prevalence_range,
                        Value = ppv_vals,
                        Metric = "PPV",
                        Predictor = predictor
                    )

                    npv_data <- data.frame(
                        Prevalence = prevalence_range,
                        Value = npv_vals,
                        Metric = "NPV",
                        Predictor = predictor
                    )

                    plot_data <- rbind(plot_data, ppv_data, npv_data)
                }

                # Create the plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Prevalence, y = Value, color = Metric, linetype = Predictor)) +
                    ggplot2::geom_line(linewidth = 1) +
                    ggplot2::labs(
                        x = "Disease Prevalence",
                        y = "Predictive Value",
                        title = "Predictive Values vs Disease Prevalence",
                        color = "Metric",
                        linetype = "Predictor"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                # Add current prevalence line if specified
                if (!is.null(self$options$prevalence)) {
                    p <- p + ggplot2::geom_vline(xintercept = self$options$prevalence,
                                       linetype = "dashed", color = "red", alpha = 0.7)
                }

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Clinical Decision Plot Error",
                    content = paste0("Failed to create clinical decision plot: ", e$message))
                FALSE
            })
        },

        .plotPRC = function(image, ggtheme, theme, ...) {
            if (!self$options$detectImbalance) return(FALSE)

            # Set custom plot dimensions
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # ggplot2 available via package Imports
                
                plot_data <- data.frame()
                baseline <- 0.5 # Default baseline

                for (predictor in names(private$.rocResults)) {
                    # Get data again
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    scores <- data[[predictor]]
                    
                    # Ensure outcome is binary 0/1, where 1 is the positive class
                    # We need to respect the private$.positiveClass logic
                    posClass <- private$.positiveClass %||% levels(outcome)[2]
                    y_binary <- as.numeric(outcome == posClass)
                    
                    baseline <- mean(y_binary) # Actual baseline is prevalence

                    # We can use pROC to get coords, but pROC doesn't do PRC natively in the same way.
                    # However, we can calculate Precision (PPV) and Recall (Sensitivity) from coords.
                    roc_obj <- private$.rocResults[[predictor]]$roc
                    
                    # Get all coordinates
                    coords <- pROC::coords(roc_obj, x="all", ret=c("threshold", "sensitivity", "specificity"))
                    
                    # Calculate Precision: TP / (TP + FP)
                    # Sensitivity = TP / (TP + FN) -> TP = Sens * Positives
                    # Specificity = TN / (TN + FP) -> FP = (1 - Spec) * Negatives
                    n_pos <- sum(y_binary == 1)
                    n_neg <- sum(y_binary == 0)
                    
                    tp <- coords$sensitivity * n_pos
                    fp <- (1 - coords$specificity) * n_neg
                    
                    precision <- tp / (tp + fp)
                    recall <- coords$sensitivity
                    
                    # Handle division by zero/NaNs at extremes
                    # When TP+FP=0 (no positive predictions), precision is undefined.
                    # Convention: set to 1 (no false positives were made) per scikit-learn standard.
                    precision[is.na(precision)] <- 1
                    
                    pr_df <- data.frame(
                        Recall = recall,
                        Precision = precision,
                        Predictor = predictor
                    )
                    plot_data <- rbind(plot_data, pr_df)
                }
                
                if (nrow(plot_data) == 0) return(FALSE)

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Recall, y = Precision, color = Predictor)) +
                    ggplot2::geom_line(linewidth = 1.2) +
                    ggplot2::geom_hline(yintercept = baseline, linetype = "dashed", color = "gray50") +
                    ggplot2::labs(
                        x = "Recall (Sensitivity)",
                        y = "Precision (PPV)",
                        title = "Precision-Recall Curve",
                        subtitle = paste("Baseline (Prevalence) =", round(baseline, 3))
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Precision-Recall Plot Error",
                    content = paste0("Failed to create Precision-Recall plot: ", e$message))
                FALSE
            })
        },

        .plotCROC = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # ggplot2 available via package Imports

                # Prepare CROC data for plotting
                plot_data <- data.frame()
                alpha <- self$options$crocAlpha

                for (predictor in names(private$.rocResults)) {
                    roc_obj <- private$.rocObjects[[predictor]]

                    # Calculate CROC curve
                    croc_result <- private$.calculateCROC(roc_obj, alpha)

                    # Create data frame for this predictor
                    croc_df <- data.frame(
                        CROC_FPR = croc_result$croc_fpr,
                        TPR = croc_result$croc_tpr,
                        Predictor = predictor,
                        Type = "CROC"
                    )

                    # Also add original ROC for comparison
                    roc_df <- data.frame(
                        CROC_FPR = (1 - roc_obj$specificities),
                        TPR = roc_obj$sensitivities,
                        Predictor = predictor,
                        Type = "ROC"
                    )

                    plot_data <- rbind(plot_data, croc_df, roc_df)
                }

                # Create the plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = CROC_FPR, y = TPR, color = Predictor, linetype = Type)) +
                    ggplot2::geom_line(linewidth = 1) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    ggplot2::labs(
                        x = paste0("Transformed FPR (CROC, \u03b1=", alpha, ")"),
                        y = "True Positive Rate (Sensitivity)",
                        title = "CROC (Concentrated ROC) Curve Analysis",
                        subtitle = "Exponential magnifier emphasizes early retrieval performance",
                        color = "Predictor",
                        linetype = "Curve Type"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::coord_fixed() +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                        legend.position = "bottom",
                        panel.grid.minor = ggplot2::element_line(color = "gray95")
                    )

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "CROC Plot Error",
                    content = paste0("Failed to create CROC plot: ", e$message))
                FALSE
            })
        },

        .plotConvexHull = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) == 0) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            tryCatch({
                # ggplot2 available via package Imports

                # Prepare convex hull data for plotting
                plot_data <- data.frame()
                hull_data <- data.frame()

                for (predictor in names(private$.rocResults)) {
                    roc_obj <- private$.rocObjects[[predictor]]

                    # Calculate convex hull
                    hull_result <- private$.calculateConvexHull(roc_obj)

                    # Original ROC curve
                    roc_df <- data.frame(
                        FPR = 1 - roc_obj$specificities,
                        TPR = roc_obj$sensitivities,
                        Predictor = predictor,
                        Type = "Empirical ROC"
                    )

                    # Convex hull
                    hull_df <- data.frame(
                        FPR = hull_result$hull_fpr,
                        TPR = hull_result$hull_tpr,
                        Predictor = predictor,
                        Type = "Convex Hull"
                    )

                    plot_data <- rbind(plot_data, roc_df)
                    hull_data <- rbind(hull_data, hull_df)
                }

                # Create the plot
                p <- ggplot2::ggplot() +
                    ggplot2::geom_line(data = plot_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor),
                             linewidth = 1, alpha = 0.7) +
                    ggplot2::geom_polygon(data = hull_data, ggplot2::aes(x = FPR, y = TPR, fill = Predictor),
                                alpha = 0.2) +
                    ggplot2::geom_line(data = hull_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor),
                             linewidth = 1.2, linetype = "dashed") +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    ggplot2::labs(
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)",
                        title = "ROC Curve with Convex Hull",
                        subtitle = "Convex hull represents optimal achievable performance",
                        color = "Predictor",
                        fill = "Predictor"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::coord_fixed() +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                        legend.position = "bottom",
                        panel.grid.minor = ggplot2::element_line(color = "gray95")
                    )

                print(p)
                TRUE

            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Convex Hull Plot Error",
                    content = paste0("Failed to create convex hull plot: ", e$message))
                FALSE
            })
        },

        .populateCalibrationAnalysis = function() {
            if (!self$options$calibrationAnalysis) return()
            
            calTable <- self$results$results$calibrationSummary
            hlTable <- self$results$results$hosmerLemeshowTable
            
            for (predictor in names(private$.rocResults)) {
                tryCatch({
                    # Get data
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    
                    # Ensure outcome is binary 0/1
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    
                    # Get probabilities: if predictor is already in [0,1] range,
                    # use values directly (assumes pre-calibrated probabilities).
                    # Otherwise, fit logistic regression to map to probabilities.
                    pred_range <- range(pred_vals, na.rm = TRUE)
                    if (pred_range[1] >= 0 && pred_range[2] <= 1) {
                        probs <- pred_vals
                        private$.addNotice(type = "INFO",
                            title = paste0("Calibration: ", predictor),
                            content = "Predictor values are in [0,1] range - treated as pre-calibrated probabilities.")
                    } else {
                        model <- glm(y_binary ~ pred_vals, family = binomial)
                        probs <- predict(model, type = "response")
                        private$.addNotice(type = "STRONG_WARNING",
                            title = paste0("Calibration: Non-Probability Predictor '", predictor, "'"),
                            content = paste0(
                                "The predictor '", predictor, "' values are not in the [0,1] probability range ",
                                "(observed range: ", round(pred_range[1], 2), " to ", round(pred_range[2], 2), "). ",
                                "A logistic regression model was fitted to obtain predicted probabilities for calibration assessment. ",
                                "These calibration results reflect the fitted model's calibration, NOT the raw biomarker's calibration. ",
                                "For proper calibration assessment, provide predicted probabilities from your prediction model."
                            ))
                    }
                    
                    # Brier Score (only compute if brierScore option is enabled)
                    brier <- NA
                    scaled_brier <- NA
                    if (isTRUE(self$options$brierScore)) {
                        brier <- mean((probs - y_binary)^2)
                        # Scaled Brier Score (1 - Brier / Brier_max)
                        prev <- mean(y_binary)
                        brier_max <- prev * (1 - prev)^2 + (1 - prev) * prev^2
                        scaled_brier <- if (brier_max > 1e-10) 1 - (brier / brier_max) else NA_real_
                    }

                    # Calibration Slope, Intercept, and Calibration-in-the-large
                    slope <- NA
                    intercept <- NA
                    cal_in_large <- NA
                    interpretation <- ""
                    if (isTRUE(self$options$calibrationMetrics)) {
                        logit_probs <- qlogis(probs)
                        # Handle infinite logits if probs are 0 or 1
                        logit_probs[probs == 0] <- -10
                        logit_probs[probs == 1] <- 10

                        cal_model <- glm(y_binary ~ logit_probs, family = binomial)
                        intercept <- coef(cal_model)[1]
                        slope <- coef(cal_model)[2]

                        # Calibration-in-the-large (intercept when slope is fixed to 1)
                        cal_large_model <- glm(y_binary ~ offset(logit_probs), family = binomial)
                        cal_in_large <- coef(cal_large_model)[1]

                        if (slope > 1.1) interpretation <- "Under-fitting (slope > 1)"
                        else if (slope < 0.9) interpretation <- "Over-fitting (slope < 1)"
                        else interpretation <- "Good calibration slope"
                    }

                    # Populate Calibration Summary Table
                    row <- list(
                        predictor = predictor,
                        brier_score = brier,
                        scaled_brier = scaled_brier,
                        calibration_slope = slope,
                        calibration_intercept = intercept,
                        calibration_in_large = cal_in_large,
                        interpretation = interpretation
                    )
                    calTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
                    
                    # Hosmer-Lemeshow Test
                    if (self$options$hosmerLemeshow) {
                        n_groups <- self$options$hlGroups
                        
                        # Create groups based on quantiles
                        # Use cut2 or manual quantile cutting
                        # Handle potential duplicate quantiles
                        breaks <- unique(quantile(probs, probs = seq(0, 1, length.out = n_groups + 1), na.rm = TRUE))
                        if (length(breaks) < 3) {
                             # Too few unique predicted probability values for meaningful grouping
                             private$.addNotice(type = "WARNING",
                                 title = paste0("HL Test Skipped: ", predictor),
                                 content = "Insufficient unique predicted probability values for Hosmer-Lemeshow grouping (need at least 3 distinct break points). This typically occurs when the predictor has very few unique values.")
                             next
                        }
                        groups <- cut(probs, breaks = breaks, include.lowest = TRUE, labels = FALSE)
                        actual_groups <- length(levels(factor(groups)))
                        
                        obs <- tapply(y_binary, groups, sum)
                        exp <- tapply(probs, groups, sum)
                        n_cnt <- tapply(y_binary, groups, length)
                        
                        # Calculate Chi-Square statistic (guard against zero denominators)
                        denom <- exp * (1 - exp/n_cnt)
                        valid <- !is.na(denom) & abs(denom) > 1e-10
                        hl_stat <- if (any(valid)) sum((obs[valid] - exp[valid])^2 / denom[valid]) else NA
                        df <- actual_groups - 2
                        p_val <- if (!is.na(hl_stat) && df > 0) 1 - pchisq(hl_stat, df = df) else NA

                        conclusion <- if (!is.na(p_val) && p_val < 0.05) "Significant lack of fit" else if (!is.na(p_val)) "Good fit" else "Could not compute"
                        
                        hl_row <- list(
                            predictor = predictor,
                            chi_square = hl_stat,
                            df = df,
                            p_value = p_val,
                            n_groups = actual_groups,
                            conclusion = conclusion
                        )
                        hlTable$addRow(rowKey = private$.escapeVar(predictor), values = hl_row)
                    }
                    
                }, error = function(e) {
                    private$.addNotice(type = "WARNING", title = paste0("Calibration Error: ", predictor), content = paste0("Calibration analysis failed for ", predictor, ": ", e$message))
                })
            }
        },

        .plotCalibration = function(image, ggtheme, theme, ...) {
            if (!self$options$calibrationAnalysis || !self$options$calibrationPlot) return(FALSE)
            
            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))
            
            tryCatch({
                # ggplot2 available via package Imports
                
                plot_data <- data.frame()
                
                for (predictor in names(private$.rocResults)) {
                    # Get data
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    
                    # Fit model
                    model <- glm(y_binary ~ pred_vals, family = binomial)
                    probs <- predict(model, type = "response")
                    
                    # Create bins for plotting (deciles)
                    data_df <- data.frame(prob = probs, outcome = y_binary)
                    data_df$bin <- cut(data_df$prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
                    
                    # Calculate observed vs expected in each bin
                    bin_stats <- aggregate(cbind(outcome, prob) ~ bin, data = data_df, FUN = mean)
                    bin_stats$Predictor <- predictor
                    
                    plot_data <- rbind(plot_data, bin_stats)
                }
                
                # Create plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = prob, y = outcome, color = Predictor, group = Predictor)) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    ggplot2::geom_point(size = 3, alpha = 0.7) +
                    ggplot2::geom_line(linewidth = 1, alpha = 0.7) +
                    ggplot2::labs(
                        x = "Predicted Probability",
                        y = "Observed Proportion",
                        title = "Calibration Plot",
                        subtitle = "Observed vs Predicted Probabilities (Deciles)",
                        color = "Predictor"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::coord_fixed() +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                        legend.position = "bottom"
                    )
                
                # Apply clinical theme if selected
                if (self$options$plotTheme == "clinical") {
                    p <- p + ggplot2::theme(
                        panel.grid.major = ggplot2::element_line(color = "gray90"),
                        panel.grid.minor = ggplot2::element_line(color = "gray95"),
                        panel.background = ggplot2::element_rect(fill = "white"),
                        legend.background = ggplot2::element_rect(fill = "white", color = "gray80")
                    )
                }
                
                print(p)
                TRUE
                
            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Calibration Plot Error",
                    content = paste0("Failed to create calibration plot: ", e$message))
                FALSE
            })
        },

        .populateMultiClassROC = function() {
            if (!self$options$multiClassROC) return()
            
            aucTable <- self$results$results$multiClassAUC
            avgTable <- self$results$results$multiClassAverage
            
            # Check if outcome has > 2 levels
            outcome <- if (!is.null(private$.multiClassOutcome)) {
                private$.multiClassOutcome
            } else {
                private$.data[[private$.outcome]]
            }
            if (nlevels(outcome) < 3) {
                self$results$results$instructions$setContent(
                    "<p><b>Note:</b> Multi-class ROC analysis requires an outcome variable with 3 or more levels.</p>"
                )
                return()
            }
            
            for (predictor in private$.predictors) {
                tryCatch({
                    # Get data
                    data <- private$.data
                    pred_vals <- data[[predictor]]
                    
                    # Run Multi-class ROC
                    mc_roc <- pROC::multiclass.roc(
                        response = outcome,
                        predictor = pred_vals,
                        levels = levels(outcome)
                    )
                    
                    # Hand-Till pairwise AUC from pROC::multiclass.roc
                    mc_auc_val <- as.numeric(mc_roc$auc)
                    mc_interp <- private$.interpretAUC(mc_auc_val)

                    # If we want One-vs-Rest, compute OVR macro average separately
                    if (self$options$multiClassStrategy == "ovr") {
                        ovr_aucs <- numeric(nlevels(outcome))
                        for (j in seq_along(levels(outcome))) {
                            lvl <- levels(outcome)[j]
                            # Create binary outcome: Class vs Rest
                            binary_outcome <- factor(ifelse(outcome == lvl, "Positive", "Negative"),
                                                   levels = c("Negative", "Positive"))

                            # Calculate ROC
                            roc_obj <- pROC::roc(response = binary_outcome, predictor = pred_vals,
                                               direction = "auto", quiet = TRUE)
                            ovr_aucs[j] <- as.numeric(roc_obj$auc)

                            row <- list(
                                class = lvl,
                                strategy = "One-vs-Rest",
                                auc = ovr_aucs[j],
                                auc_lower = if(self$options$useBootstrap) as.numeric(pROC::ci.auc(roc_obj)[1]) else NA,
                                auc_upper = if(self$options$useBootstrap) as.numeric(pROC::ci.auc(roc_obj)[3]) else NA,
                                n_positive = sum(outcome == lvl),
                                n_negative = sum(outcome != lvl)
                            )
                            aucTable$addRow(rowKey = paste0(private$.escapeVar(predictor), "_", private$.escapeVar(lvl)), values = row)
                        }

                        # OVR Macro Average
                        ovr_macro_auc <- mean(ovr_aucs, na.rm = TRUE)
                        ovr_interp <- private$.interpretAUC(ovr_macro_auc)
                        avgTable$setRow(rowNo = 1, values = list(
                            averaging_method = "OVR Macro Average",
                            macro_auc = ovr_macro_auc,
                            weighted_auc = mc_auc_val,  # Hand-Till pairwise for reference
                            micro_auc = NA,
                            interpretation = paste0(ovr_interp, " (Hand-Till pairwise: ", round(mc_auc_val, 3), ")")
                        ))
                    } else {
                        # One-vs-One (Pairwise) — Hand-Till method
                        avgTable$setRow(rowNo = 1, values = list(
                            averaging_method = "Hand-Till Pairwise",
                            macro_auc = mc_auc_val,
                            weighted_auc = NA,
                            micro_auc = NA,
                            interpretation = mc_interp
                        ))

                        # Per-pair AUC breakdown
                        pairs <- combn(levels(outcome), 2)
                        for (i in 1:ncol(pairs)) {
                            class1 <- pairs[1, i]
                            class2 <- pairs[2, i]
                            
                            # Subset data
                            subset_idx <- outcome %in% c(class1, class2)
                            subset_outcome <- factor(outcome[subset_idx], levels = c(class1, class2))
                            subset_pred <- pred_vals[subset_idx]
                            
                            roc_obj <- pROC::roc(response = subset_outcome, predictor = subset_pred,
                                               direction = "auto", quiet = TRUE)
                            
                            row <- list(
                                class = paste(class1, "vs", class2),
                                strategy = "One-vs-One",
                                auc = as.numeric(roc_obj$auc),
                                auc_lower = NA, # CI for pairwise might be overkill to compute for all
                                auc_upper = NA,
                                n_positive = sum(outcome == class2), # Assuming class2 is 'positive' in the pair
                                n_negative = sum(outcome == class1)
                            )
                            aucTable$addRow(rowKey = paste0(private$.escapeVar(predictor), "_", private$.escapeVar(class1), "_", private$.escapeVar(class2)), values = row)
                        }
                    }
                    
                }, error = function(e) {
                    private$.addNotice(type = "WARNING", title = paste0("Multi-Class ROC Error: ", predictor), content = paste0("Multi-class ROC analysis failed for ", predictor, ": ", e$message))
                })
            }
        },

        .plotMultiClassROC = function(image, ggtheme, theme, ...) {
            if (!self$options$multiClassROC) return(FALSE)
            
            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))
            
            tryCatch({
                # ggplot2 available via package Imports
                
                plot_data <- data.frame()
                outcome <- if (!is.null(private$.multiClassOutcome)) {
                    private$.multiClassOutcome
                } else {
                    private$.data[[private$.outcome]]
                }
                
                if (nlevels(outcome) < 3) return(FALSE)

                if (self$options$multiClassStrategy == "ovo") {
                    private$.addNotice(type = "INFO",
                        title = "Multi-Class ROC Plot",
                        content = "One-vs-One (OVO) ROC plots are not yet supported. Switch to One-vs-Rest (OVR) strategy for ROC curve visualization.")
                    return(FALSE)
                }

                for (predictor in private$.predictors) {
                    pred_vals <- private$.data[[predictor]]

                    if (self$options$multiClassStrategy == "ovr") {
                        for (lvl in levels(outcome)) {
                            binary_outcome <- factor(ifelse(outcome == lvl, "Positive", "Negative"), 
                                                   levels = c("Negative", "Positive"))
                            
                            roc_obj <- pROC::roc(response = binary_outcome, predictor = pred_vals, 
                                               direction = "auto", quiet = TRUE)
                            
                            coords <- pROC::coords(roc_obj, "all", ret = c("specificity", "sensitivity"))
                            
                            df <- data.frame(
                                FPR = 1 - coords$specificity,
                                TPR = coords$sensitivity,
                                Class = lvl,
                                Predictor = predictor
                            )
                            plot_data <- rbind(plot_data, df)
                        }
                    }
                }
                
                if (nrow(plot_data) == 0) return(FALSE)
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Class)) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    ggplot2::geom_line(linewidth = 1) +
                    ggplot2::labs(
                        x = "False Positive Rate",
                        y = "True Positive Rate",
                        title = "Multi-Class ROC Curves (One-vs-Rest)",
                        color = "Class"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::coord_fixed() +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                if (length(private$.predictors) > 1) {
                    p <- p + ggplot2::facet_wrap(~Predictor)
                }
                
                print(p)
                TRUE
                
            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Multi-Class ROC Plot Error",
                    content = paste0("Failed to create multi-class ROC plot: ", e$message))
                FALSE
            })
        },

        .populateClinicalImpact = function() {
            if (!self$options$clinicalImpact) return()
            
            impactTable <- self$results$results$clinicalImpactTable
            decisionTable <- self$results$results$decisionImpactSummary
            
            for (predictor in names(private$.rocResults)) {
                tryCatch({
                    # Get data
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    n <- length(y_binary)
                    prevalence <- if (isTRUE(self$options$useObservedPrevalence)) {
                        mean(y_binary)
                    } else if (!is.null(self$options$prevalence) && !is.na(self$options$prevalence)) {
                        self$options$prevalence
                    } else {
                        mean(y_binary)
                    }
                    
                    # Get predicted probabilities (use raw if already probabilities)
                    probs <- if (all(pred_vals >= 0 & pred_vals <= 1, na.rm = TRUE)) {
                        pred_vals
                    } else {
                        stats::predict(glm(y_binary ~ pred_vals, family = binomial), type = "response")
                    }
                    
                    # Decision threshold based on prevalence (user provided or observed)
                    threshold_prob <- min(max(self$options$prevalence %||% prevalence, 0.01), 0.99)
                    classified_positive <- probs >= threshold_prob
                    
                    tp <- sum(classified_positive & y_binary == 1)
                    fp <- sum(classified_positive & y_binary == 0)
                    fn <- sum(!classified_positive & y_binary == 1)
                    tn <- sum(!classified_positive & y_binary == 0)
                    
                    sens <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
                    spec <- ifelse((tn + fp) > 0, tn / (tn + fp), NA)
                    youden <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA
                    nnd <- if (!is.na(youden) && youden > 0) 1 / youden else NA

                    # NNT (Number Needed to Test/Screen): inverse of net benefit
                    # NNT = 1 / (TP/n - FP/n * pt/(1-pt)) where pt = threshold probability
                    # This is the number of patients that need to be tested for one
                    # additional true positive at the given decision threshold
                    net_benefit_local <- (tp / n) - (fp / n) * (threshold_prob / (1 - threshold_prob))
                    nnt <- if (!is.na(net_benefit_local) && net_benefit_local > 0) {
                        round(1 / net_benefit_local)
                    } else {
                        NA
                    }

                    tested_positive <- (tp + fp) / n
                    false_positive_rate <- 1 - spec
                    net_benefit <- net_benefit_local

                    row <- list(
                        predictor = predictor,
                        threshold = threshold_prob,
                        nnt = nnt,
                        nnd = nnd,
                        tested_positive = tested_positive,
                        true_positive_rate = sens,
                        false_positive_rate = false_positive_rate,
                        net_benefit_per_100 = net_benefit * 100
                    )
                    impactTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
                    
                    if (self$options$decisionImpactTable) {
                        thresholds <- unique(pmin(pmax(c(0.1, 0.2, 0.3, threshold_prob), 0.01), 0.99))
                        for (thr in thresholds) {
                            pred_pos <- probs >= thr
                            tp_thr <- sum(pred_pos & y_binary == 1)
                            fp_thr <- sum(pred_pos & y_binary == 0)
                            fn_thr <- sum(!pred_pos & y_binary == 1)
                            tn_thr <- sum(!pred_pos & y_binary == 0)
                            
                            ppv <- ifelse((tp_thr + fp_thr) > 0, tp_thr / (tp_thr + fp_thr), NA)
                            npv <- ifelse((tn_thr + fn_thr) > 0, tn_thr / (tn_thr + fn_thr), NA)
                            
                            decisionTable$addRow(rowKey = paste0(private$.escapeVar(predictor), "_", thr), values = list(
                                threshold = thr,
                                n_high_risk = tp_thr + fp_thr,
                                n_high_risk_with_event = tp_thr,
                                n_high_risk_without_event = fp_thr,
                                ppv = ppv,
                                npv = npv
                            ))
                        }
                    }
                    
                }, error = function(e) {
                    private$.addNotice(type = "WARNING", title = paste0("Clinical Impact Error: ", predictor), content = paste0("Clinical impact analysis failed for ", predictor, ": ", e$message))
                })
            }
        },

        .plotClinicalUtility = function(image, ggtheme, theme, ...) {
            if (!self$options$clinicalImpact || !self$options$clinicalUtilityCurve) return(FALSE)
            
            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 600
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))
            
            tryCatch({
                # ggplot2 available via package Imports
                
                plot_data <- data.frame()
                thresholds <- seq(0.01, 0.99, by = 0.01)
                
                for (predictor in names(private$.rocResults)) {
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    n <- length(y_binary)
                    
                    # Use raw values if already in [0,1] probability range, else fit logistic model
                    if (all(pred_vals >= 0 & pred_vals <= 1, na.rm = TRUE)) {
                        probs <- pred_vals
                    } else {
                        model <- glm(y_binary ~ pred_vals, family = binomial)
                        probs <- predict(model, type = "response")
                    }
                    
                    net_benefits <- numeric(length(thresholds))
                    
                    for (i in seq_along(thresholds)) {
                        thresh <- thresholds[i]
                        tp <- sum(probs >= thresh & y_binary == 1)
                        fp <- sum(probs >= thresh & y_binary == 0)
                        
                        # Net Benefit
                        nb <- (tp / n) - (fp / n) * (thresh / (1 - thresh))
                        net_benefits[i] <- nb
                    }
                    
                    df <- data.frame(
                        Threshold = thresholds,
                        NetBenefit = net_benefits,
                        Predictor = predictor,
                        Type = "Model"
                    )
                    plot_data <- rbind(plot_data, df)
                }
                
                # Add "Treat All" line
                prevalence <- mean(as.numeric(private$.data[[private$.outcome]] == levels(private$.data[[private$.outcome]])[2]))
                treat_all_nb <- prevalence - (1 - prevalence) * (thresholds / (1 - thresholds))
                treat_all_df <- data.frame(
                    Threshold = thresholds,
                    NetBenefit = treat_all_nb,
                    Predictor = "Treat All",
                    Type = "Reference"
                )
                
                # Add "Treat None" line (Net Benefit = 0)
                treat_none_df <- data.frame(
                    Threshold = thresholds,
                    NetBenefit = 0,
                    Predictor = "Treat None",
                    Type = "Reference"
                )
                
                plot_data <- rbind(plot_data, treat_all_df, treat_none_df)
                
                # Filter out negative net benefits for cleaner plot (optional, but standard DCA often shows them)
                # Usually we limit y-axis to reasonable range
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Threshold, y = NetBenefit, color = Predictor, linetype = Type)) +
                    ggplot2::geom_line(linewidth = 1) +
                    ggplot2::labs(
                        x = "Threshold Probability",
                        y = "Net Benefit",
                        title = "Decision Curve Analysis",
                        subtitle = "Net Benefit vs Threshold Probability",
                        color = "Strategy",
                        linetype = "Type"
                    ) +
                    ggplot2::coord_cartesian(ylim = c(-0.05, prevalence + 0.05)) + # Zoom in on relevant range
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                print(p)
                TRUE
                
            }, error = function(e) {
                private$.addNotice(type = "WARNING",
                    title = "Clinical Utility Plot Error",
                    content = paste0("Failed to create clinical utility plot: ", e$message))
                FALSE
            })
        },

        .populateInternalValidation = function() {
            if (!self$options$internalValidation) return()

            private$.addNotice(
                type = "INFO",
                title = "Internal Validation Note",
                content = paste0(
                    "Bootstrap optimism correction is most informative when evaluating a fitted prediction model. ",
                    "For a single raw biomarker, optimism from model fitting is minimal. ",
                    "The correction shown reflects optimism from a logistic regression model fitted to the data."
                )
            )

            val_method <- self$options$validationMethod

            summary_text <- paste0("<h3>Internal Validation (", private$.safeHtmlOutput(val_method), ")</h3>")

            for (predictor in names(private$.rocResults)) {
                result <- tryCatch({
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    n <- length(y_binary)

                    original_auc <- as.numeric(private$.rocResults[[predictor]]$roc$auc)
                    pred_text <- ""

                    if (val_method == "bootstrap" || val_method == "both") {
                        # Optimism-corrected bootstrap validation (Harrell 2015)
                        # 1. Fit model on bootstrap sample
                        # 2. Apparent AUC = model predictions on bootstrap sample
                        # 3. Test AUC = same model predictions on original sample
                        # 4. Optimism = apparent - test
                        # 5. Corrected AUC = original AUC - mean(optimism)
                        B <- min(self$options$bootstrapSamples %||% 100, 200)
                        optimism_vals <- numeric(B)
                        boot_aucs <- numeric(B)

                        for (i in 1:B) {
                            indices <- sample(1:n, replace = TRUE)
                            # Fit logistic model on bootstrap sample
                            boot_df <- data.frame(y = y_binary[indices], x = pred_vals[indices])
                            boot_fit <- tryCatch(
                                glm(y ~ x, data = boot_df, family = binomial),
                                error = function(e) NULL
                            )
                            if (is.null(boot_fit)) {
                                optimism_vals[i] <- NA
                                boot_aucs[i] <- NA
                                next
                            }
                            # Apparent: model predictions evaluated on bootstrap sample
                            boot_preds <- predict(boot_fit, newdata = boot_df, type = "response")
                            roc_apparent <- pROC::roc(y_binary[indices], boot_preds,
                                                      direction = "auto", quiet = TRUE)
                            apparent_auc <- as.numeric(roc_apparent$auc)

                            # Test: same model predictions evaluated on original sample
                            orig_df <- data.frame(x = pred_vals)
                            test_preds <- predict(boot_fit, newdata = orig_df, type = "response")
                            roc_test <- pROC::roc(y_binary, test_preds,
                                                  direction = roc_apparent$direction, quiet = TRUE)
                            test_auc <- as.numeric(roc_test$auc)

                            optimism_vals[i] <- apparent_auc - test_auc
                            boot_aucs[i] <- apparent_auc
                        }

                        mean_optimism <- mean(optimism_vals, na.rm = TRUE)
                        corrected_auc <- original_auc - mean_optimism
                        # SE-based CI for the corrected AUC (using SD of optimism estimates)
                        se_optimism <- sd(optimism_vals, na.rm = TRUE)
                        ci_lower <- corrected_auc - 1.96 * se_optimism
                        ci_upper <- corrected_auc + 1.96 * se_optimism

                        pred_text <- paste0(pred_text,
                            "<p><b>", private$.safeHtmlOutput(predictor), " (Bootstrap):</b> Original AUC = ", round(original_auc, 3),
                            ", Optimism = ", round(mean_optimism, 4),
                            ", Corrected AUC = ", round(corrected_auc, 3),
                            ", 95% CI [", round(ci_lower, 3), ", ", round(ci_upper, 3), "]</p>")
                    }

                    if (val_method == "cv" || val_method == "both") {
                        # Cross-validation: train logistic regression on training fold,
                        # predict on test fold, compute AUC on test predictions
                        k <- 10
                        # Stratified CV: maintain class proportions in each fold
                        pos_idx <- which(y_binary == 1)
                        neg_idx <- which(y_binary == 0)
                        folds <- integer(n)
                        folds[pos_idx] <- sample(rep(1:k, length.out = length(pos_idx)))
                        folds[neg_idx] <- sample(rep(1:k, length.out = length(neg_idx)))

                        cv_aucs <- numeric(k)

                        for (i in 1:k) {
                            test_idx <- which(folds == i)
                            train_idx <- which(folds != i)

                            # Fit logistic model on training fold
                            train_df <- data.frame(y = y_binary[train_idx], x = pred_vals[train_idx])
                            test_df <- data.frame(x = pred_vals[test_idx])

                            fit <- tryCatch(
                                glm(y ~ x, data = train_df, family = binomial),
                                error = function(e) NULL
                            )

                            if (!is.null(fit)) {
                                # Predict on test fold
                                test_preds <- predict(fit, newdata = test_df, type = "response")
                                roc_cv <- pROC::roc(y_binary[test_idx], test_preds,
                                                    direction = "auto", quiet = TRUE)
                                cv_aucs[i] <- as.numeric(roc_cv$auc)
                            } else {
                                cv_aucs[i] <- NA
                            }
                        }

                        mean_cv_auc <- mean(cv_aucs, na.rm = TRUE)
                        sd_cv_auc <- sd(cv_aucs, na.rm = TRUE)

                        pred_text <- paste0(pred_text,
                            "<p><b>", private$.safeHtmlOutput(predictor), " (", k, "-Fold CV):</b> Mean AUC = ", round(mean_cv_auc, 3),
                            " (SD = ", round(sd_cv_auc, 3), ")</p>")
                    }

                    pred_text  # return the text for this predictor

                }, error = function(e) {
                    paste0("<p>Validation failed for ", private$.safeHtmlOutput(predictor), ": ", private$.safeHtmlOutput(e$message), "</p>")
                })

                summary_text <- paste0(summary_text, result)
            }

            # Append validation results to existing analysis summary (not overwrite)
            separator <- if (nchar(private$.analysisSummaryHtml) > 0) "<br>" else ""
            private$.analysisSummaryHtml <- paste0(private$.analysisSummaryHtml, separator, summary_text)
            self$results$results$analysisSummary$setContent(private$.analysisSummaryHtml)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for enhancedROC analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            outcome <- self$options$outcome
            predictors <- self$options$predictors

            if (is.null(outcome) || is.null(predictors) || length(predictors) == 0)
                return('')

            # Get arguments
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::enhancedROC(\n    data = data', args, ')')
        }
    ) # End of public list
)
