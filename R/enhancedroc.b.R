#' Enhanced ROC Analysis Class
#'
#' Comprehensive ROC analysis implementation inspired by BlueSky's createROCTable
#' with enhanced features for clinical diagnostic performance evaluation.
#'
#' @import jmvcore
#' @import pROC
#' @importFrom caret confusionMatrix
#' @importFrom stats quantile binom.test glm predict
#' @importFrom dplyr arrange desc
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
          text <- gsub("/", "&#x2F;", text, fixed = TRUE)
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
            ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
            WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
            INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
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
            if (exists("clinicopath_init")) {
                clinicopath_init("enhancedROC", self$options$clinicalContext)
            }

            private$.data <- self$data
            private$.outcome <- self$options$outcome
            private$.predictors <- self$options$predictors

            # Initialize instructions
            html <- self$results$results$instructions
            html$setContent(private$.getInstructions())

            # Initialize ROC objects list
            private$.rocObjects <- list()
        },

        .run = function() {
            # Check if we have required data
            if (is.null(private$.outcome) || is.null(private$.predictors) || length(private$.predictors) == 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Missing Variables",
                    content = "Please select an outcome variable and at least one predictor variable for ROC analysis. • Outcome variable: required (binary/factor). • Predictor variables: at least one numeric variable required for ROC curve calculation."
                )
                return()
            }

            # Apply clinical presets if selected
            private$.applyClinicalPresets()

            # Prepare and validate data
            analysisData <- private$.prepareData()
            if (is.null(analysisData)) return()

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

            # Determine if optimization should be run
            use_optimization <- self$options$youdenOptimization
            if (!is.null(private$.presetConfig)) {
                use_optimization <- TRUE
            }

            if (self$options$optimalCutoffs && use_optimization) {
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


            # NOT IMPLEMENTED - Time-Dependent ROC Warning (commented out)
            # if (self$options$timeDependentROC || self$options$survivalROC) {
            #     private$.addNotice(
            #         type = "WARNING",
            #         title = "Time-Dependent ROC Not Supported",
            #         content = "Time-Dependent and Survival ROC analyses are not currently supported. • These features require time-to-event input variables (event time, censoring indicator) which are not yet available in this interface. • For survival ROC analysis, please use standard ROC analysis with risk scores or predicted probabilities from survival models."
            #     )
            # }
            
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
                n_obs <- nrow(self$data)

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
            validation_result <- if (exists("validate_clinical_data")) {
                validate_clinical_data(
                    data = self$data,
                    required_vars = c(private$.outcome, private$.predictors),
                    min_observations = 20,
                    clinical_checks = TRUE
                )
            } else {
                # Basic validation
                list(
                    valid = !is.null(self$data) && nrow(self$data) >= 20,
                    errors = if(is.null(self$data) || nrow(self$data) < 20) c(.("Insufficient data")) else c(),
                    warnings = c(),
                    n_observations = ifelse(is.null(self$data), 0, nrow(self$data))
                )
            }

            if (!validation_result$valid) {
                self$results$results$instructions$setContent(
                    paste("<p>Data validation failed:",
                          paste(validation_result$errors, collapse = "; "), "</p>")
                )
                return(NULL)
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
                                   paste(validation_result$warnings, collapse = "<br>"), "</p>")
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
                        paste0("<p><strong>Error:</strong> Outcome variable '", private$.outcome, 
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
                           "<p><strong>Error:</strong> Outcome variable '<code>", private$.outcome, 
                           "</code>' has only 1 unique value: '<code>", levels(outcome_var)[1], "</code>'</p>",
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
                                     "<h4 style='color: #0c5460; margin-top: 0;'>ℹ Multi-Class Analysis Enabled</h4>",
                                     "<p>Outcome variable '<code>", private$.outcome, "</code>' has ", levels_count, " levels.</p>",
                                     "<p>Multi-Class ROC metrics will be calculated.</p>",
                                     "<p>Standard ROC tables will show performance for Positive Class: '<code>", private$.positiveClass, "</code>' vs Others.</p>",
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
                                   "<p><strong>Issue:</strong> Outcome variable '<code>", private$.outcome, 
                                   "</code>' has ", levels_count, " levels for ROC analysis: <strong>", paste(available_levels, collapse = ", "), "</strong></p>",
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
                                     "<h4 style='color: #155724; margin-top: 0;'>✓ Outcome Variable Converted to Binary</h4>",
                                     "<p><strong>Positive Class:</strong> '<code>", positive_class, "</code>' (cases of interest)</p>",
                                     "<p><strong>Negative Class:</strong> '<code>Other</code>' (combined: ", 
                                     paste(available_levels[available_levels != positive_class], collapse = ", "), ")</p>",
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
                                   "<p><strong>Error:</strong> Selected positive class '<code>", positive_class, 
                                   "</code>' not found in outcome variable '<code>", private$.outcome, "</code>'.</p>",
                                   "<p><strong>Available options:</strong> ", paste0("<code>", available_levels, "</code>", collapse = ", "), "</p>",
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
                    
                    info_msg <- paste0("<p><strong>Default Configuration:</strong> Using '", positive_class, 
                                     "' as positive class and '", negative_class, 
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
                           paste(non_numeric_preds, collapse = "</code>, <code>"), 
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
                current_msg <- self$results$results$instructions$content
                if (is.null(current_msg) || current_msg == "") {
                    current_msg <- private$.getInstructions()
                }
                self$results$results$instructions$setContent(paste0(current_msg, warning_html))
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
                        direction <- ">"
                    } else if (direction_param == "lower") {
                        direction <- "<"
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
                    ci_method <- if (use_boot) "bootstrap" else "delong"
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
                        current_msg <- self$results$results$instructions$content %||% private$.getInstructions()
                        self$results$results$instructions$setContent(
                            paste0(current_msg,
                                   "<p><strong>Note:</strong> Custom tied score handling is not supported by pROC; using default averaging.</p>")
                        )
                    }

                    # Create ROC object with optional smoothing
                    roc_obj <- pROC::roc(
                        response = response_var,
                        predictor = data[[predictor]],
                        direction = direction,
                        ci = TRUE,
                        conf.level = self$options$confidenceLevel / 100,
                        ci.method = ci_method,
                        boot.n = boot_n,
                        boot.stratified = boot_strat,
                        boot.ci.type = boot_type
                    )

                    # Apply smoothing if requested
                    if (self$options$smoothMethod != "none") {
                        tryCatch({
                            if (self$options$smoothMethod == "binormal") {
                                roc_obj <- pROC::smooth(roc_obj, method = "binormal")
                            } else if (self$options$smoothMethod == "kernel") {
                                roc_obj <- pROC::smooth(roc_obj, method = "density")
                            }
                        }, error = function(e) {
                            warning(paste("Smoothing failed for", predictor, "- using unsmoothed ROC"))
                        })
                    }

                    private$.rocObjects[[predictor]] <- roc_obj

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
                        direction <- if (prevalence < 0.05) "low" else "high"
                        metric_concern <- if (prevalence < 0.05) {
                            "Positive Predictive Value (PPV) will be unreliable even with high sensitivity/specificity"
                        } else {
                            "Negative Predictive Value (NPV) will be unreliable even with high sensitivity/specificity"
                        }

                        private$.addNotice(
                            type = "STRONG_WARNING",
                            title = paste0("Extreme Prevalence: ", predictor),
                            content = paste0("Extreme prevalence for ", predictor, ": ", round(prevalence * 100, 1), "% (", direction, "). • ", metric_concern, ". • ROC/AUC analysis may be misleading - consider Precision-Recall Curve (PRC) instead. • Sensitivity and specificity remain valid, but predictive values (PPV/NPV) are heavily influenced by prevalence.")
                        )
                    }

                }, error = function(e) {
                    # Provide specific error messages based on common ROC analysis issues
                    error_msg <- ""
                    if (grepl("No controls", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("No control observations found for predictor"), " '", predictor, 
                                          "'. ", .("Check that your outcome variable has both positive and negative cases."))
                    } else if (grepl("No cases", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("No case observations found for predictor"), " '", predictor, 
                                          "'. ", .("Check that your outcome variable has both positive and negative cases."))
                    } else if (grepl("identical", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("Predictor"), " '", predictor, 
                                          "' ", .("has identical values across all observations. ROC analysis requires variation in predictor values."))
                    } else if (grepl("missing", e$message, ignore.case = TRUE)) {
                        error_msg <- paste0(.("Missing values detected in predictor"), " '", predictor, 
                                          "' ", .("or outcome variable. Please check your data."))
                    } else {
                        error_msg <- paste0(.("ROC analysis failed for predictor"), " '", predictor, "': ", 
                                          e$message, ". ", .("Please check your data quality and variable selection."))
                    }
                    
                    # Handle errors with enhanced error handling if available
                    if (exists("clinicopath_error_handler")) {
                        clinicopath_error_handler(e, "enhancedROC", error_msg)
                    }
                    
                    # Show user-friendly error in results
                    current_msg <- self$results$results$instructions$content
                    if (is.null(current_msg) || current_msg == "") {
                        current_msg <- private$.getInstructions()
                    }
                    self$results$results$instructions$setContent(
                        paste0(current_msg, "<p><strong>", .("Warning"), ":</strong> ", error_msg, "</p>")
                    )
                })
            }
        },

        # CRITICAL FIX: Helper function to apply cutoff respecting ROC direction
        .applyDirectionCutoff = function(predictor_values, cutoff, roc_direction) {
            # When direction is "<", lower values predict positive outcome
            # When direction is ">", higher values predict positive outcome
            if (roc_direction == "<") {
                return(predictor_values < cutoff)
            } else {
                return(predictor_values >= cutoff)
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

                return(list(
                    cutoff = coords_result$threshold,
                    sensitivity = coords_result$sensitivity,
                    specificity = coords_result$specificity,
                    youden_index = coords_result$sensitivity + coords_result$specificity - 1,
                    accuracy = as.numeric(cm$overall["Accuracy"]),
                    confusion_matrix = cm,
                    true_positive = cm$table[2, 2],
                    true_negative = cm$table[1, 1],
                    false_positive = cm$table[2, 1],
                    false_negative = cm$table[1, 2]
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
                warning(paste("No cutoffs meet the specified sensitivity/specificity constraints for", predictor, ". Using unconstrained optimization."))
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

            return(list(
                cutoff = optimal_cutoff,
                sensitivity = optimal_sens,
                specificity = optimal_spec,
                youden_index = optimal_youden,
                accuracy = as.numeric(cm$overall["Accuracy"]),
                confusion_matrix = cm,
                true_positive = cm$table[2, 2],
                true_negative = cm$table[1, 1],
                false_positive = cm$table[2, 1],
                false_negative = cm$table[1, 2]
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
                warning(paste("Failed to evaluate custom cutoffs for", predictor, ":", e$message))
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
                    std_error = if(!is.null(roc_obj$ci)) (roc_obj$ci[3] - roc_obj$ci[1]) / 3.92 else NA,
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
            if (roc_obj$direction == "<") {
                # Lower values predict positive outcome
                predicted_positive <- predictor_data < cutoff
            } else {
                # Higher values predict positive outcome  
                predicted_positive <- predictor_data >= cutoff
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
                cutoffTable$addRow(rowKey = paste(predictor, "optimal", sep = "_"), values = row)

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
                        cutoffTable$addRow(rowKey = paste(predictor, "custom", i, sep = "_"), values = custom_row)
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

                    cutoffTable$addRow(rowKey = paste(predictor, i, sep = "_"), values = row)
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
                current_msg <- self$results$results$instructions$content %||% private$.getInstructions()
                self$results$results$instructions$setContent(paste0(current_msg, note_html))
            }

            for (predictor in names(private$.rocResults)) {
                result <- private$.rocResults[[predictor]]
                optimal <- result$optimal_cutoff

                # Calculate predictive values
                sens <- optimal$sensitivity
                spec <- optimal$specificity

                ppv <- (sens * prevalence) / ((sens * prevalence) + ((1 - spec) * (1 - prevalence)))
                npv <- (spec * (1 - prevalence)) / (((1 - sens) * prevalence) + (spec * (1 - prevalence)))

                # Calculate likelihood ratios
                lr_pos <- sens / (1 - spec)
                lr_neg <- (1 - sens) / spec

                # Diagnostic odds ratio
                dor <- lr_pos / lr_neg

                # Clinical interpretation
                clinical_interp <- private$.interpretClinicalMetrics(ppv, npv, lr_pos, lr_neg,
                                                                   self$options$clinicalContext)

                row <- list(
                    predictor = predictor,
                    prevalence = prevalence,
                    ppv = ppv,
                    npv = npv,
                    lr_positive = lr_pos,
                    lr_negative = lr_neg,
                    diagnostic_odds_ratio = dor,
                    clinical_interpretation = clinical_interp
                )

                clinTable$addRow(rowKey = private$.escapeVar(predictor), values = row)
            }
        },

        .populateROCComparisons = function() {
            if (length(private$.rocResults) < 2) {
                stop("ROC comparisons require at least 2 predictors. Found: ", length(private$.rocResults))
            }

            compTable <- self$results$results$rocComparisons
            predictors <- names(private$.rocResults)

            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]

                    # Remove tryCatch to expose errors
                    # Perform ROC comparison
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

                    compTable$addRow(rowKey = paste(pred1, pred2, sep = "_vs_"), values = row)
                }
            }
        },

        .populateDetailedComparison = function() {
            if (length(private$.rocResults) < 2) {
                stop("Detailed comparison requires at least 2 predictors. Found: ", length(private$.rocResults))
            }
            
            detailTable <- self$results$results$detailedComparison
            
            # Get all predictor pairs
            predictors <- names(private$.rocResults)
            
            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]
                    
                    # Remove tryCatch to expose errors
                    # Get results for both predictors
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
                            percent_change <- (diff / val1) * 100
                            
                            # Calculate p-value using appropriate statistical test
                            p_value <- private$.calculateMetricPValue(metric, result1, result2, pred1, pred2)
                            
                            # Determine clinical interpretation
                            interpretation <- if (abs(percent_change) > 20) {
                                .("Clinically significant difference")
                            } else if (abs(percent_change) > 10) {
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
                            
                            detailTable$addRow(rowKey = paste(pred1, pred2, metric, sep = "_"), values = row)
                        }
                    }
                }
            }
        },

        .populateStatisticalSummary = function() {
            if (length(private$.rocResults) < 2) {
                stop("Statistical summary requires at least 2 predictors. Found: ", length(private$.rocResults))
            }
            
            statSummaryTable <- self$results$results$statisticalSummary
            
            # Get all predictor pairs
            predictors <- names(private$.rocResults)
            
            for (i in 1:(length(predictors) - 1)) {
                for (j in (i + 1):length(predictors)) {
                    pred1 <- predictors[i]
                    pred2 <- predictors[j]
                    
                    # Remove tryCatch to expose errors
                    # Get ROC objects for comparison
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
                        .("No significant difference (p ≥ 0.05)")
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
                    
                    statSummaryTable$addRow(rowKey = paste(pred1, pred2, "stat", sep = "_"), values = row)
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
                    if (range_type_name == "sensitivity") {
                        # Calculate pAUC over sensitivity range
                        # pROC uses partial.auc.focus to specify sensitivity-based pAUC
                        pauc <- pROC::auc(roc_obj,
                                         partial.auc = c(range_min, range_max),
                                         partial.auc.focus = "sensitivity")
                        range_display <- "Sensitivity"
                    } else {
                        # Calculate pAUC over specificity range (default)
                        pauc <- pROC::auc(roc_obj,
                                         partial.auc = c(range_min, range_max),
                                         partial.auc.focus = "specificity")
                        range_display <- "Specificity"
                    }

                    # Normalize partial AUC
                    range_width <- range_max - range_min
                    normalized_pauc <- pauc / range_width

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
                    warning(paste("Partial AUC calculation failed for", predictor, ":", e$message))
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

                    # Calculate early retrieval gain
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
                    warning(paste("CROC calculation failed for", predictor, ":", e$message))
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
                    warning(paste("Convex hull calculation failed for", predictor, ":", e$message))
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

            return(list(
                croc_auc = abs(croc_auc),
                croc_fpr = croc_fpr,
                croc_tpr = tpr,
                alpha = alpha
            ))
        },

        .calculateConvexHull = function(roc_obj) {
            # Extract ROC coordinates
            fpr <- 1 - roc_obj$specificities
            tpr <- roc_obj$sensitivities

            # Add corner points
            fpr <- c(0, fpr, 1)
            tpr <- c(0, tpr, 1)

            # Calculate convex hull using Graham scan
            points <- cbind(fpr, tpr)
            hull_indices <- grDevices::chull(points)

            # Extract hull points
            hull_fpr <- fpr[hull_indices]
            hull_tpr <- tpr[hull_indices]

            # Sort by FPR for AUC calculation
            ord <- order(hull_fpr)
            hull_fpr_sorted <- hull_fpr[ord]
            hull_tpr_sorted <- hull_tpr[ord]

            # Calculate hull AUC using trapezoidal rule
            hull_auc <- sum(diff(hull_fpr_sorted) *
                           (hull_tpr_sorted[-1] + hull_tpr_sorted[-length(hull_tpr_sorted)]) / 2)

            return(list(
                hull_auc = abs(hull_auc),
                hull_fpr = hull_fpr_sorted,
                hull_tpr = hull_tpr_sorted,
                n_hull_points = length(hull_indices)
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
            n_observations <- nrow(self$data)

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

        .populateClinicalInterpretation = function() {
            html <- self$results$results$clinicalInterpretationGuide

            interpretation <- "<h3>Clinical Application Guidance</h3>"

            if (length(private$.rocResults) > 0) {
                context <- self$options$clinicalContext

                interpretation <- paste0(interpretation,
                    "<p><strong>Clinical Context:</strong> ", toupper(substring(context, 1, 1)),
                    substring(context, 2), " application</p>"
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

        .computePRMetrics = function(scores, labels, positive_label) {
            # Simple PR curve and AUPRC calculation without extra dependencies
            y <- as.integer(labels == positive_label)
            if (length(unique(y)) < 2) {
                return(NULL)
            }
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
            f1_scores <- 2 * precision * recall / (precision + recall)
            best_f1 <- max(f1_scores, na.rm = TRUE)

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
                pr <- private$.computePRMetrics(scores, outcome,
                                               private$.positiveClass %||% levels(outcome)[2])
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
            n_obs <- nrow(self$data)
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
                        n_predictors, n_obs, context), " "
            )
            
            if (best_auc > 0) {
                interpretation <- private$.interpretAUC(best_auc)
                clinical_utility <- private$.assessClinicalUtility(best_auc, context)
                
                summary_text <- paste0(summary_text,
                    sprintf(.("The best performing predictor was '%s' with an AUC of %.3f (%s performance, %s)."), 
                            best_predictor, best_auc, interpretation, clinical_utility), 
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
            html$setContent(summary_text)
        },
        
        .generateClinicalReport = function() {
            html <- self$results$results$clinicalReport
            
            # Generate copy-ready clinical report sentences
            n_predictors <- length(private$.rocResults)
            n_obs <- nrow(self$data)
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
            methods_text <- sprintf(.("ROC analysis was performed to evaluate the diagnostic performance of %s in predicting [outcome] using %d observations. The analysis was conducted using the pROC package in R, with AUC calculation and %d%% confidence intervals determined using %s methodology."), 
                                   ifelse(n_predictors == 1, paste0("'", best_predictor, "'"), paste(n_predictors, "predictors")),
                                   n_obs, 
                                   self$options$confidenceLevel,
                                   ifelse(self$options$useBootstrap, "bootstrap", "DeLong"))
            report_html <- paste0(report_html, methods_text, "</div>")
            
            # Results section  
            report_html <- paste0(report_html, "<h5>", .("Results Section"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #28a745; margin: 5px 0;'>")
            
            results_text <- sprintf(.("The %s predictor demonstrated %s diagnostic performance with an AUC of %.3f (95%% CI: %s--%s). At the optimal cutoff of %.3f, the test achieved %s sensitivity (%.1f%%) and %s specificity (%.1f%%), resulting in a Youden Index of %.3f."),
                                   best_predictor,
                                   private$.interpretAUC(best_auc),
                                   best_auc,
                                   ci_lower, ci_upper,
                                   best_cutoff$cutoff,
                                   ifelse(best_cutoff$sensitivity >= 0.8, .("high"), ifelse(best_cutoff$sensitivity >= 0.7, .("moderate"), .("low"))),
                                   best_cutoff$sensitivity * 100,
                                   ifelse(best_cutoff$specificity >= 0.8, .("high"), ifelse(best_cutoff$specificity >= 0.7, .("moderate"), .("low"))),
                                   best_cutoff$specificity * 100,
                                   best_cutoff$youden_index)
            
            report_html <- paste0(report_html, results_text, "</div>")
            
            # Clinical interpretation
            report_html <- paste0(report_html, "<h5>", .("Clinical Interpretation"), ":</h5>")
            report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #ffc107; margin: 5px 0;'>")
            
            clinical_utility <- private$.assessClinicalUtility(best_auc, context)
            interpretation_text <- sprintf(.("These findings suggest that %s has %s for %s applications. The observed AUC indicates %s discriminatory ability, which %s for clinical implementation in this context."),
                                         best_predictor,
                                         clinical_utility,
                                         context,
                                         ifelse(best_auc >= 0.8, .("good to excellent"), ifelse(best_auc >= 0.7, .("fair to good"), .("limited"))),
                                         ifelse(best_auc >= 0.8, .("supports consideration"), .("requires careful evaluation")))
            
            report_html <- paste0(report_html, interpretation_text, "</div>")
            
            # Statistical reporting
            if (n_predictors > 1) {
                report_html <- paste0(report_html, "<h5>", .("Comparative Analysis"), ":</h5>")
                report_html <- paste0(report_html, "<div style='background-color: white; padding: 10px; border-left: 4px solid #dc3545; margin: 5px 0;'>")
                comparative_text <- sprintf(.("Among the %d predictors evaluated, %s demonstrated superior performance. Pairwise comparisons were conducted using %s methodology to assess statistical significance of observed differences."),
                                           n_predictors,
                                           best_predictor,
                                           self$options$comparisonMethod)
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
                               sprintf(.("Predictor '%s' has no variation. ROC analysis requires variable predictors."), pred),
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
                                           sprintf(.("Predictor '%s' appears highly skewed. Consider transformation for better clinical interpretation."), pred),
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
        
        .createClinicalInterpreter = function(context) {
            # Unified clinical interpretation system
            list(
                context = context,
                
                interpretAUC = function(auc) {
                    if (auc >= 0.90) return(list(level = .("Excellent"), utility = .("High")))
                    if (auc >= 0.80) return(list(level = .("Good"), utility = .("Moderate")))
                    if (auc >= 0.70) return(list(level = .("Fair"), utility = .("Limited")))
                    if (auc >= 0.60) return(list(level = .("Poor"), utility = .("Minimal")))
                    return(list(level = .("No discrimination"), utility = .("None")))
                },
                
                assessUtility = function(auc) {
                    interp <- self$interpretAUC(auc)
                    base_utility <- interp$utility
                    
                    if (context == "screening" && auc < 0.75) {
                        return(paste(base_utility, .("- May not meet screening standards")))
                    } else if (context == "diagnosis" && auc < 0.80) {
                        return(paste(base_utility, .("- Consider combining with other markers")))
                    } else {
                        return(paste(base_utility, .("clinical utility")))
                    }
                },
                
                generateRecommendation = function(sensitivity, specificity, youden_index = NULL) {
                    if (context == "screening") {
                        if (sensitivity >= 0.90 && specificity >= 0.70) {
                            return(.("Suitable for screening - high sensitivity with acceptable specificity"))
                        } else if (sensitivity >= 0.85) {
                            return(.("Consider for screening - good sensitivity but monitor false positives"))
                        } else {
                            return(.("Not recommended for screening - insufficient sensitivity"))
                        }
                    } else if (context == "diagnosis") {
                        if (sensitivity >= 0.80 && specificity >= 0.80) {
                            return(.("Good diagnostic performance - balanced sensitivity and specificity"))
                        } else if (specificity >= 0.90) {
                            return(.("Suitable for confirmatory testing - high specificity"))
                        } else {
                            return(.("Consider combining with additional markers"))
                        }
                    } else {
                        if (!is.null(youden_index) && youden_index >= 0.6) {
                            return(.("Strong discriminatory performance"))
                        } else {
                            return(.("Moderate discriminatory performance"))
                        }
                    }
                }
            )
        },

        .calculateMetricPValue = function(metric, result1, result2, pred1, pred2) {
            # Calculate p-value for metric comparison based on the metric type
            tryCatch({
                if (metric == "AUC") {
                    # For AUC comparison, use ROC test
                    roc_test <- pROC::roc.test(result1$roc, result2$roc, method = self$options$comparisonMethod)
                    return(roc_test$p.value)
                } else {
                    # For other metrics (Sensitivity, Specificity, Accuracy), use bootstrap or approximation
                    # Since we don't have raw data for McNemar's test, use bootstrap approach
                    n_bootstrap <- min(self$options$bootstrapSamples, 1000)  # Limit for performance
                    
                    if (metric == "Sensitivity" || metric == "Specificity" || metric == "Accuracy") {
                        # Get optimal cutoffs for both predictors
                        cutoff1 <- result1$optimal_cutoff
                        cutoff2 <- result2$optimal_cutoff
                        
                        # For proportions like sensitivity/specificity, use normal approximation
                        if (metric == "Sensitivity") {
                            p1 <- cutoff1$sensitivity
                            p2 <- cutoff2$sensitivity
                            # Estimate sample sizes from ROC objects
                            n1 <- length(result1$roc$cases)
                            n2 <- length(result2$roc$cases)
                        } else if (metric == "Specificity") {
                            p1 <- cutoff1$specificity  
                            p2 <- cutoff2$specificity
                            n1 <- length(result1$roc$controls)
                            n2 <- length(result2$roc$controls)
                        } else {  # Accuracy
                            p1 <- cutoff1$accuracy
                            p2 <- cutoff2$accuracy
                            n1 <- length(result1$roc$response)
                            n2 <- length(result2$roc$response)
                        }
                        
                        # Two-sample proportion test
                        if (!is.na(p1) && !is.na(p2) && n1 > 0 && n2 > 0) {
                            # Calculate pooled proportion
                            x1 <- round(p1 * n1)
                            x2 <- round(p2 * n2)
                            
                            if (x1 >= 0 && x1 <= n1 && x2 >= 0 && x2 <= n2) {
                                prop_test <- stats::prop.test(c(x1, x2), c(n1, n2), correct = TRUE)
                                return(prop_test$p.value)
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
                
                "<h3 style='color: #2c5530;'>📋 Setup Instructions:</h3>",
                "<ol style='margin-left: 20px;'>",
                "<li><strong>Outcome Variable:</strong> Select your binary outcome (e.g., Disease Status, Survival Status)</li>",
                "<ul style='margin-left: 20px; color: #6c757d;'>",
                "<li>If you have >2 levels (e.g., DOD, DOOC, AWD, AWOD), select which level represents 'positive' cases</li>",
                "</ul>",
                "<li><strong>Predictor Variables:</strong> Choose continuous predictors (e.g., Age, Biomarker levels, Test scores)</li>",
                "<li><strong>Positive Class:</strong> Specify which outcome level represents the condition of interest</li>",
                "<li><strong>Clinical Context:</strong> Select appropriate context for tailored interpretation</li>",
                "</ol>",
                
                "<h3 style='color: #2c5530;'>🔧 Quick Start Presets:</h3>",
                "<div style='background: #e7f3ff; padding: 10px; border-radius: 4px; margin: 10px 0;'>",
                "<p><strong>Clinical Presets</strong> automatically configure analysis settings:</p>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>Biomarker Screening:</strong> High sensitivity (catch all cases)</li>",
                "<li><strong>Diagnostic Validation:</strong> Balanced sensitivity/specificity</li>",
                "<li><strong>Confirmatory Testing:</strong> High specificity (minimize false positives)</li>",
                "<li><strong>Research Comprehensive:</strong> Full statistical analysis</li>",
                "</ul>",
                "</div>",
                
                "<h3 style='color: #2c5530;'>📊 Key Output Metrics:</h3>",
                "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin: 10px 0;'>",
                "<div style='background: #fff3cd; padding: 10px; border-radius: 4px; flex: 1; min-width: 250px;'>",
                "<strong>AUC (Area Under Curve):</strong><br>",
                "• 0.5 = No discrimination<br>",
                "• 0.7-0.8 = Acceptable<br>",
                "• 0.8-0.9 = Excellent<br>",
                "• >0.9 = Outstanding",
                "</div>",
                "<div style='background: #d1ecf1; padding: 10px; border-radius: 4px; flex: 1; min-width: 250px;'>",
                "<strong>Youden Index:</strong><br>",
                "Optimal cutoff that maximizes<br>",
                "(Sensitivity + Specificity - 1)<br>",
                "Best balance of true/false rates",
                "</div>",
                "</div>",
                
                "<details style='margin: 15px 0; padding: 10px; background: #f8f9fa; border-radius: 4px;'>",
                "<summary style='cursor: pointer; font-weight: bold; color: #495057;'>📖 Statistical Terms Glossary</summary>",
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
                "<p style='margin: 0;'><strong>💡 Tip:</strong> For clinical decision making, consider both statistical significance and clinical relevance of the cutoff thresholds.</p>",
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

            # Remove tryCatch to expose plot errors immediately
            # tryCatch({
                # Create ROC curve plot
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    stop("ggplot2 package not available - required for ROC plotting")
                }
                
                # Prepare data for plotting
                plot_data <- data.frame()

                for (predictor in names(private$.rocResults)) {
                    roc_obj <- private$.rocResults[[predictor]]$roc
                    
                    # Validate ROC object
                    if (is.null(roc_obj) || !inherits(roc_obj, "roc")) {
                        stop(paste("Invalid ROC object for predictor:", predictor, "- ROC analysis may have failed"))
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
                    stop("No valid plot data generated - ROC analysis may have failed")
                }

                # Create the plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = FPR, y = TPR, color = Predictor)) +
                    ggplot2::geom_line(size = 1.2) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    ggplot2::labs(
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)",
                        title = "ROC Curve Analysis",
                        color = "Predictor"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_minimal() +
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
                                private$.checkpoint(flush = FALSE)
                                
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
                return(TRUE)

            # }, error = function(e) {
            #     stop(paste("Failed to create ROC curve plot:", e$message))
            # })
        },

        .plotComparativeROC = function(image, ggtheme, theme, ...) {
            if (is.null(private$.rocResults) || length(private$.rocResults) < 2) {
                return(FALSE)
            }

            # Set custom plot dimensions if specified
            width <- self$options$plotWidth %||% 800
            height <- self$options$plotHeight %||% 600
            image$setState(list(width = width, height = height))

            # Remove tryCatch to expose errors
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                stop("ggplot2 package not available for comparative ROC plot")
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
                    ggplot2::geom_line(size = 1.2, alpha = 0.8) +
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
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11),
                        legend.position = "right",
                        legend.title = ggplot2::element_text(size = 10, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11),
                        panel.grid.minor = ggplot2::element_line(color = "gray95", size = 0.3)
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
                return(TRUE)
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
                library(ggplot2)

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

                    # Convert to long format for plotting
                    threshold_long <- reshape2::melt(threshold_data,
                                                   id.vars = c("Threshold", "Predictor"),
                                                   variable.name = "Metric",
                                                   value.name = "Value")

                    plot_data <- rbind(plot_data, threshold_long)
                }

                # Create the plot
                p <- ggplot(plot_data, aes(x = Threshold, y = Value, color = Metric)) +
                    geom_line(size = 1) +
                    labs(
                        x = "Threshold",
                        y = "Performance Metric",
                        title = "Sensitivity and Specificity vs Threshold",
                        color = "Metric"
                    ) +
                    ylim(0, 1) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                if (length(names(private$.rocResults)) > 1) {
                    p <- p + facet_wrap(~Predictor, scales = "free_x")
                }

                print(p)
                TRUE

            }, error = function(e) {
                warning(paste("Failed to create cutoff analysis plot:", e$message))
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
                library(ggplot2)

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
                p <- ggplot(plot_data, aes(x = Threshold, y = YoudenIndex, color = Predictor)) +
                    geom_line(size = 1.2) +
                    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
                    labs(
                        x = "Threshold",
                        y = "Youden Index (Sensitivity + Specificity - 1)",
                        title = "Youden Index vs Threshold",
                        color = "Predictor"
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
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
                p <- p + geom_point(data = optimal_data, size = 3, shape = 16)

                print(p)
                TRUE

            }, error = function(e) {
                warning(paste("Failed to create Youden Index plot:", e$message))
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
                library(ggplot2)

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
                p <- ggplot(plot_data, aes(x = Prevalence, y = Value, color = Metric, linetype = Predictor)) +
                    geom_line(size = 1) +
                    labs(
                        x = "Disease Prevalence",
                        y = "Predictive Value",
                        title = "Predictive Values vs Disease Prevalence",
                        color = "Metric",
                        linetype = "Predictor"
                    ) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                # Add current prevalence line if specified
                if (!is.null(self$options$prevalence)) {
                    p <- p + geom_vline(xintercept = self$options$prevalence,
                                       linetype = "dashed", color = "red", alpha = 0.7)
                }

                print(p)
                TRUE

            }, error = function(e) {
                warning(paste("Failed to create clinical decision plot:", e$message))
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
                library(ggplot2)
                
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
                    precision[is.na(precision)] <- 1 # Usually at threshold 1 where TP=0, FP=0? 
                    # If TP+FP=0, it means no positive predictions. Precision is undefined but often treated as 1 or 0.
                    
                    pr_df <- data.frame(
                        Recall = recall,
                        Precision = precision,
                        Predictor = predictor
                    )
                    plot_data <- rbind(plot_data, pr_df)
                }
                
                if (nrow(plot_data) == 0) return(FALSE)

                p <- ggplot(plot_data, aes(x = Recall, y = Precision, color = Predictor)) +
                    geom_line(size = 1.2) +
                    geom_hline(yintercept = baseline, linetype = "dashed", color = "gray50") +
                    labs(
                        x = "Recall (Sensitivity)",
                        y = "Precision (PPV)",
                        title = "Precision-Recall Curve",
                        subtitle = paste("Baseline (Prevalence) =", round(baseline, 3))
                    ) +
                    xlim(0, 1) + ylim(0, 1) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )

                print(p)
                TRUE

            }, error = function(e) {
                warning(paste("Failed to create Precision-Recall plot:", e$message))
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
                library(ggplot2)

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
                p <- ggplot(plot_data, aes(x = CROC_FPR, y = TPR, color = Predictor, linetype = Type)) +
                    geom_line(size = 1) +
                    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    labs(
                        x = paste0("Transformed FPR (CROC, α=", alpha, ")"),
                        y = "True Positive Rate (Sensitivity)",
                        title = "CROC (Concentrated ROC) Curve Analysis",
                        subtitle = "Exponential magnifier emphasizes early retrieval performance",
                        color = "Predictor",
                        linetype = "Curve Type"
                    ) +
                    xlim(0, 1) + ylim(0, 1) +
                    coord_fixed() +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 10),
                        legend.position = "bottom",
                        panel.grid.minor = element_line(color = "gray95")
                    )

                print(p)
                TRUE

            }, error = function(e) {
                warning(paste("Failed to create CROC plot:", e$message))
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
                library(ggplot2)

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
                p <- ggplot() +
                    geom_line(data = plot_data, aes(x = FPR, y = TPR, color = Predictor),
                             size = 1, alpha = 0.7) +
                    geom_polygon(data = hull_data, aes(x = FPR, y = TPR, fill = Predictor),
                                alpha = 0.2) +
                    geom_line(data = hull_data, aes(x = FPR, y = TPR, color = Predictor),
                             size = 1.2, linetype = "dashed") +
                    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    labs(
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)",
                        title = "ROC Curve with Convex Hull",
                        subtitle = "Convex hull represents optimal achievable performance",
                        color = "Predictor",
                        fill = "Predictor"
                    ) +
                    xlim(0, 1) + ylim(0, 1) +
                    coord_fixed() +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 10),
                        legend.position = "bottom",
                        panel.grid.minor = element_line(color = "gray95")
                    )

                print(p)
                TRUE

            }, error = function(e) {
                warning(paste("Failed to create convex hull plot:", e$message))
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
                    
                    # Get probabilities
                    # If predictor is already a probability (0-1), use it directly?
                    # For safety, we'll fit a logistic regression to get calibrated probabilities
                    # unless the user explicitly provides probabilities (which we can't easily check here)
                    # So we will fit a model.
                    
                    model <- glm(y_binary ~ pred_vals, family = binomial)
                    probs <- predict(model, type = "response")
                    
                    # Brier Score
                    brier <- mean((probs - y_binary)^2)
                    
                    # Scaled Brier Score (1 - Brier / Brier_max)
                    # Brier_max is Brier score of a null model (prevalence)
                    prev <- mean(y_binary)
                    brier_max <- prev * (1 - prev)^2 + (1 - prev) * prev^2
                    scaled_brier <- 1 - (brier / brier_max)
                    
                    # Calibration Slope and Intercept
                    # Fit logistic regression of outcome on logit(probs)
                    logit_probs <- qlogis(probs)
                    # Handle infinite logits if probs are 0 or 1
                    logit_probs[probs == 0] <- -10 
                    logit_probs[probs == 1] <- 10
                    
                    cal_model <- glm(y_binary ~ logit_probs, family = binomial)
                    intercept <- coef(cal_model)[1]
                    slope <- coef(cal_model)[2]
                    
                    # Calibration-in-the-large (intercept when slope is fixed to 1)
                    # offset(logit_probs)
                    cal_large_model <- glm(y_binary ~ offset(logit_probs), family = binomial)
                    cal_in_large <- coef(cal_large_model)[1]
                    
                    # Interpretation
                    interpretation <- ""
                    if (slope > 1.1) interpretation <- "Under-fitting (slope > 1)"
                    else if (slope < 0.9) interpretation <- "Over-fitting (slope < 1)"
                    else interpretation <- "Good calibration slope"
                    
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
                        breaks <- unique(quantile(probs, probs = seq(0, 1, length.out = n_groups + 1)))
                        if (length(breaks) < 3) {
                             # Fallback for very few unique values
                             groups <- as.numeric(as.factor(probs))
                             actual_groups <- length(unique(groups))
                        } else {
                             groups <- cut(probs, breaks = breaks, include.lowest = TRUE, labels = FALSE)
                             actual_groups <- length(unique(groups))
                        }
                        
                        obs <- tapply(y_binary, groups, sum)
                        exp <- tapply(probs, groups, sum)
                        n_cnt <- tapply(y_binary, groups, length)
                        
                        # Calculate Chi-Square statistic
                        hl_stat <- sum((obs - exp)^2 / (exp * (1 - exp/n_cnt)), na.rm = TRUE)
                        df <- actual_groups - 2
                        p_val <- 1 - pchisq(hl_stat, df = df)
                        
                        conclusion <- if (p_val < 0.05) "Significant lack of fit" else "Good fit"
                        
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
                    warning(paste("Calibration analysis failed for", predictor, ":", e$message))
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
                library(ggplot2)
                
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
                p <- ggplot(plot_data, aes(x = prob, y = outcome, color = Predictor, group = Predictor)) +
                    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    geom_point(size = 3, alpha = 0.7) +
                    geom_line(size = 1, alpha = 0.7) +
                    labs(
                        x = "Predicted Probability",
                        y = "Observed Proportion",
                        title = "Calibration Plot",
                        subtitle = "Observed vs Predicted Probabilities (Deciles)",
                        color = "Predictor"
                    ) +
                    xlim(0, 1) + ylim(0, 1) +
                    coord_fixed() +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 10),
                        legend.position = "bottom"
                    )
                
                # Apply clinical theme if selected
                if (self$options$plotTheme == "clinical") {
                    p <- p + theme(
                        panel.grid.major = element_line(color = "gray90"),
                        panel.grid.minor = element_line(color = "gray95"),
                        panel.background = element_rect(fill = "white"),
                        legend.background = element_rect(fill = "white", color = "gray80")
                    )
                }
                
                print(p)
                TRUE
                
            }, error = function(e) {
                warning(paste("Failed to create calibration plot:", e$message))
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
                    
                    # Populate Average Table
                    avgTable$setRow(rowNo = 1, values = list(
                        averaging_method = "Hand & Till (Pairwise)",
                        macro_auc = as.numeric(mc_roc$auc),
                        weighted_auc = NA, # Not directly provided by pROC default
                        micro_auc = NA
                    ))
                    
                    # Populate Pairwise/Class-specific AUCs if available
                    # pROC returns pairwise comparisons
                    # We can iterate through them
                    
                    # Note: pROC multiclass.roc returns a list of ROC curves for pairwise comparisons
                    # We can try to map these to the table structure
                    
                    # The table structure expects "class" and "strategy" (e.g. One-vs-Rest)
                    # pROC does pairwise (One-vs-One) by default
                    
                    # If we want One-vs-Rest, we need to calculate it manually
                    if (self$options$multiClassStrategy == "ovr") {
                        for (lvl in levels(outcome)) {
                            # Create binary outcome: Class vs Rest
                            binary_outcome <- factor(ifelse(outcome == lvl, "Positive", "Negative"), 
                                                   levels = c("Negative", "Positive"))
                            
                            # Calculate ROC
                            roc_obj <- pROC::roc(response = binary_outcome, predictor = pred_vals, 
                                               direction = "auto", quiet = TRUE)
                            
                            row <- list(
                                class = lvl,
                                strategy = "One-vs-Rest",
                                auc = as.numeric(roc_obj$auc),
                                auc_lower = if(self$options$useBootstrap) as.numeric(pROC::ci.auc(roc_obj)[1]) else NA,
                                auc_upper = if(self$options$useBootstrap) as.numeric(pROC::ci.auc(roc_obj)[3]) else NA,
                                n_positive = sum(outcome == lvl),
                                n_negative = sum(outcome != lvl)
                            )
                            aucTable$addRow(rowKey = paste0(predictor, "_", lvl), values = row)
                        }
                    } else {
                        # One-vs-One (Pairwise)
                        # We can extract from mc_roc$rocs if available, or re-calculate
                        # mc_roc$rocs is a list of lists of roc objects
                        
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
                            aucTable$addRow(rowKey = paste0(predictor, "_", class1, "_", class2), values = row)
                        }
                    }
                    
                }, error = function(e) {
                    warning(paste("Multi-class ROC analysis failed for", predictor, ":", e$message))
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
                library(ggplot2)
                
                plot_data <- data.frame()
                outcome <- if (!is.null(private$.multiClassOutcome)) {
                    private$.multiClassOutcome
                } else {
                    private$.data[[private$.outcome]]
                }
                
                if (nlevels(outcome) < 3) return(FALSE)
                
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
                
                p <- ggplot(plot_data, aes(x = FPR, y = TPR, color = Class)) +
                    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                    geom_line(size = 1) +
                    labs(
                        x = "False Positive Rate",
                        y = "True Positive Rate",
                        title = "Multi-Class ROC Curves (One-vs-Rest)",
                        color = "Class"
                    ) +
                    xlim(0, 1) + ylim(0, 1) +
                    coord_fixed() +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                if (length(private$.predictors) > 1) {
                    p <- p + facet_wrap(~Predictor)
                }
                
                print(p)
                TRUE
                
            }, error = function(e) {
                warning(paste("Failed to create multi-class ROC plot:", e$message))
                FALSE
            })
        },

        # NOT IMPLEMENTED - Time-Dependent ROC Stub Functions (commented out)
        # .plotTimeDependentROC = function(image, ggtheme, theme, ...) {
        #     # Stub for Time-Dependent ROC Plot
        #     # Currently not supported, as indicated by the warning in .run()
        #     # Returns FALSE to indicate no plot generated, or generates a placeholder
        #
        #     if (!self$options$timeDependentROC) return(FALSE)
        #
        #     width <- self$options$plotWidth %||% 600
        #     height <- self$options$plotHeight %||% 600
        #     image$setState(list(width = width, height = height))
        #
        #     tryCatch({
        #         library(ggplot2)
        #
        #         # Create a placeholder plot
        #         df <- data.frame(x = 0.5, y = 0.5, label = "Time-Dependent ROC\nCurrently Not Supported")
        #         p <- ggplot(df, aes(x=x, y=y, label=label)) +
        #             geom_text(size = 6) +
        #             theme_void() +
        #             theme(plot.background = element_rect(fill = "white", color = NA))
        #
        #         print(p)
        #         TRUE
        #     }, error = function(e) {
        #         FALSE
        #     })
        # },
        #
        # .plotAUCTimeCourse = function(image, ggtheme, theme, ...) {
        #     # Stub for AUC Time Course Plot
        #     if (!self$options$timeDependentROC) return(FALSE)
        #
        #     width <- self$options$plotWidth %||% 600
        #     height <- self$options$plotHeight %||% 600
        #     image$setState(list(width = width, height = height))
        #
        #     tryCatch({
        #         library(ggplot2)
        #
        #         # Create a placeholder plot
        #         df <- data.frame(x = 0.5, y = 0.5, label = "AUC Time Course\nCurrently Not Supported")
        #         p <- ggplot(df, aes(x=x, y=y, label=label)) +
        #             geom_text(size = 6) +
        #             theme_void() +
        #             theme(plot.background = element_rect(fill = "white", color = NA))
        #
        #         print(p)
        #         TRUE
        #     }, error = function(e) {
        #         FALSE
        #     })
        # },

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
                    
                    tested_positive <- (tp + fp) / n
                    false_positive_rate <- 1 - spec
                    net_benefit <- (tp / n) - (fp / n) * (threshold_prob / (1 - threshold_prob))
                    
                    row <- list(
                        predictor = predictor,
                        threshold = threshold_prob,
                        nnt = NA,
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
                            
                            decisionTable$addRow(rowKey = paste0(predictor, "_", thr), values = list(
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
                    warning(paste("Clinical impact analysis failed for", predictor, ":", e$message))
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
                library(ggplot2)
                
                plot_data <- data.frame()
                thresholds <- seq(0.01, 0.99, by = 0.01)
                
                for (predictor in names(private$.rocResults)) {
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    n <- length(y_binary)
                    
                    # Fit model to get probabilities if needed, or use raw if 0-1
                    # Assuming raw for now if 0-1, else fit
                    if (all(pred_vals >= 0 & pred_vals <= 1)) {
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
                
                p <- ggplot(plot_data, aes(x = Threshold, y = NetBenefit, color = Predictor, linetype = Type)) +
                    geom_line(size = 1) +
                    labs(
                        x = "Threshold Probability",
                        y = "Net Benefit",
                        title = "Decision Curve Analysis",
                        subtitle = "Net Benefit vs Threshold Probability",
                        color = "Strategy",
                        linetype = "Type"
                    ) +
                    coord_cartesian(ylim = c(-0.05, prevalence + 0.05)) + # Zoom in on relevant range
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                print(p)
                TRUE
                
            }, error = function(e) {
                warning(paste("Failed to create clinical utility plot:", e$message))
                FALSE
            })
        },

        .populateInternalValidation = function() {
            if (!self$options$internalValidation) return()
            
            val_method <- self$options$validationMethod
            
            summary_text <- paste0("<h3>Internal Validation (", val_method, ")</h3>")
            
            for (predictor in names(private$.rocResults)) {
                tryCatch({
                    data <- private$.data
                    outcome <- data[[private$.outcome]]
                    pred_vals <- data[[predictor]]
                    y_binary <- as.numeric(outcome == levels(outcome)[2])
                    
                    original_auc <- as.numeric(private$.rocResults[[predictor]]$roc$auc)
                    
                    if (val_method == "bootstrap" || val_method == "both") {
                        # Bootstrap Validation (Optimism)
                        B <- 100 # Use fewer samples for speed
                        boot_aucs <- numeric(B)
                        
                        for (i in 1:B) {
                            indices <- sample(1:length(y_binary), replace = TRUE)
                            roc_boot <- pROC::roc(outcome[indices], pred_vals[indices], direction="auto", quiet=TRUE)
                            boot_aucs[i] <- as.numeric(roc_boot$auc)
                        }
                        
                        ci_lower <- quantile(boot_aucs, 0.025)
                        ci_upper <- quantile(boot_aucs, 0.975)
                        
                        summary_text <- paste0(summary_text, 
                            "<p><b>", predictor, ":</b> Original AUC = ", round(original_auc, 3), 
                            ", Bootstrap 95% CI [", round(ci_lower, 3), ", ", round(ci_upper, 3), "]</p>")
                    }
                    
                    if (val_method == "cv" || val_method == "both") {
                        # Cross-Validation
                        k <- 10
                        folds <- cut(seq(1, length(y_binary)), breaks = k, labels = FALSE)
                        folds <- sample(folds)
                        
                        cv_aucs <- numeric(k)
                        
                        for (i in 1:k) {
                            test_idx <- which(folds == i)
                            roc_cv <- pROC::roc(outcome[test_idx], pred_vals[test_idx], direction="auto", quiet=TRUE)
                            cv_aucs[i] <- as.numeric(roc_cv$auc)
                        }
                        
                        mean_cv_auc <- mean(cv_aucs, na.rm = TRUE)
                        
                        summary_text <- paste0(summary_text, 
                            "<p><b>", predictor, ":</b> ", k, "-Fold CV AUC = ", round(mean_cv_auc, 3), "</p>")
                    }
                    
                }, error = function(e) {
                    summary_text <- paste0(summary_text, "<p>Validation failed for ", predictor, ": ", e$message, "</p>")
                })
            }
            
            # Append to analysis summary
            current_content <- self$results$results$analysisSummary$content
            self$results$results$analysisSummary$setContent(paste(current_content, summary_text, sep = "<br>"))
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
