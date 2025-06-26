#' @title Medical Decision Tree
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#' @description Enhanced decision tree analysis for medical research, pathology and oncology
#'

treeClass <- if (requireNamespace('jmvcore'))
    R6::R6Class("treeClass",
                inherit = treeBase,
                private = list(

                    # Store prevalence for later use
                    .prevalence = NULL,

                    # Medical data validation ----
                    .validateMedicalData = function() {

                        # Check minimum sample size for reliable clinical decision trees
                        if (nrow(self$data) < 50) {
                            stop("Minimum 50 cases required for reliable medical decision trees")
                        }

                        targetName <- self$options$target
                        targetLevel <- self$options$targetLevel

                        if (is.null(targetName) || is.null(targetLevel)) {
                            return(FALSE)
                        }

                        # Check class imbalance - critical for medical diagnosis
                        diseaseCount <- sum(self$data[[targetName]] == targetLevel, na.rm=TRUE)
                        totalCount <- sum(!is.na(self$data[[targetName]]))
                        prevalence <- diseaseCount / totalCount

                        # Store prevalence for reporting
                        private$.prevalence <- prevalence

                        if (prevalence < 0.05) {
                            warning("Very low disease prevalence (<5%). Consider oversampling or different approach.")
                        } else if (prevalence > 0.95) {
                            warning("Very high disease prevalence (>95%). Consider different approach.")
                        }

                        # Check for unrealistic biomarker values
                        varsName <- if(!is.null(self$options$vars)) trimws(self$options$vars) else NULL
                        for (var in varsName) {
                            values <- jmvcore::toNumeric(self$data[[var]])

                            # Check for negative values in typically positive biomarkers
                            if (any(values < 0, na.rm=TRUE)) {
                                warning(paste("Warning:", var, "contains negative values - check data quality"))
                            }

                            # Check for extreme outliers (>5 SD from mean)
                            outliers <- abs(scale(values)) > 5
                            if (any(outliers, na.rm=TRUE)) {
                                warning(paste("Warning:", var, "contains extreme outliers"))
                            }
                        }

                        return(TRUE)
                    },

                    # Enhanced data preparation for medical research ----
                    .prepareData = function() {

                        # Validate medical data first
                        if (!private$.validateMedicalData()) {
                            return(NULL)
                        }

                        # Trim variable names to handle spaces in variable names (Issue #12)
                        varsName <- if(!is.null(self$options$vars)) trimws(self$options$vars) else NULL
                        facsName <- if(!is.null(self$options$facs)) trimws(self$options$facs) else NULL
                        targetName <- if(!is.null(self$options$target)) trimws(self$options$target) else NULL
                        trainName <- if(!is.null(self$options$train)) trimws(self$options$train) else NULL

                        # Select variables
                        allVars <- c(varsName, facsName, targetName)
                        if (!is.null(trainName)) {
                            allVars <- c(allVars, trainName)
                        }

                        mydata <- jmvcore::select(self$data, allVars)

                        # Convert factors
                        mydata[[targetName]] <- as.factor(mydata[[targetName]])
                        for (fac in facsName) {
                            mydata[[fac]] <- as.factor(mydata[[fac]])
                        }

                        # Convert continuous variables
                        for (var in varsName) {
                            mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                        }

                        # Handle missing values with medical-appropriate methods
                        if (self$options$imputeMissing) {
                            mydata <- private$.imputeMedicalData(mydata)
                        } else {
                            # Report missing data patterns before removal
                            missingReport <- private$.analyzeMissing(mydata)

                            # Populate missing data report table
                            for (i in 1:nrow(missingReport)) {
                                self$results$missingDataReport$addRow(rowKey=i, values=list(
                                    variable = missingReport$Variable[i],
                                    missing_count = missingReport$Missing_Count[i],
                                    missing_percent = missingReport$Missing_Percent[i],
                                    complete_cases = missingReport$Complete_Cases[i]
                                ))
                            }
                        }

                        # ALWAYS remove NA values - FFTrees requires complete data
                        mydata <- jmvcore::naOmit(mydata)

                        # Check if we have enough data after removing NAs
                        if (nrow(mydata) < 20) {
                            stop("Insufficient data after removing missing values. Need at least 20 complete cases.")
                        }

                        # Create binary target for medical classification
                        targetLevel <- self$options$targetLevel
                        mydata[["outcome"]] <- factor(
                            ifelse(mydata[[targetName]] == targetLevel, "Disease", "Control"),
                            levels = c("Control", "Disease")
                        )

                        # Handle train/test split
                        if (!is.null(trainName)) {
                            mydata[[trainName]] <- as.factor(mydata[[trainName]])
                            trainLevel <- self$options$trainLevel

                            trainData <- mydata[mydata[[trainName]] == trainLevel, ]
                            testData <- mydata[mydata[[trainName]] != trainLevel, ]
                        } else {
                            # Create stratified split maintaining disease prevalence
                            set.seed(123) # For reproducibility
                            trainIndices <- caret::createDataPartition(
                                mydata$outcome,
                                p = 0.7,
                                list = FALSE
                            )
                            trainData <- mydata[trainIndices, ]
                            testData <- mydata[-trainIndices, ]
                        }

                        # Balance classes if requested (important for rare diseases)
                        if (self$options$balanceClasses && nrow(trainData) > 20) {
                            trainData <- private$.balanceData(trainData)
                        }

                        # Feature scaling for continuous variables
                        if (self$options$scaleFeatures) {
                            for (var in varsName) {
                                if (var %in% names(trainData)) {
                                    # Scale based on training data only
                                    trainMean <- mean(trainData[[var]], na.rm = TRUE)
                                    trainSD <- sd(trainData[[var]], na.rm = TRUE)

                                    trainData[[var]] <- (trainData[[var]] - trainMean) / trainSD
                                    if (nrow(testData) > 0) {
                                        testData[[var]] <- (testData[[var]] - trainMean) / trainSD
                                    }
                                }
                            }
                        }

                        return(list(
                            "mydata" = mydata,
                            "trainData" = trainData,
                            "testData" = testData,
                            "prevalence" = private$.prevalence
                        ))
                    },

                    # Missing data analysis ----
                    .analyzeMissing = function(data) {
                        missingPattern <- data.frame(
                            Variable = names(data),
                            Missing_Count = sapply(data, function(x) sum(is.na(x))),
                            Missing_Percent = sapply(data, function(x) round(mean(is.na(x)) * 100, 1))
                        )
                        missingPattern$Complete_Cases <- nrow(data) - missingPattern$Missing_Count

                        return(missingPattern)
                    },

                    # Medical-appropriate missing data imputation ----
                    .imputeMedicalData = function(data) {
                        # Trim variable names to handle spaces (Issue #12 fix)
                        varsName <- if(!is.null(self$options$vars)) trimws(self$options$vars) else NULL
                        facsName <- if(!is.null(self$options$facs)) trimws(self$options$facs) else NULL
                        targetName <- if(!is.null(self$options$target)) trimws(self$options$target) else NULL

                        # For continuous biomarkers: median imputation within disease groups
                        for (var in varsName) {
                            if (sum(is.na(data[[var]])) > 0) {
                                # Calculate median by target group, handling cases where group might have no data
                                medians_by_group <- tapply(data[[var]], data[[targetName]], median, na.rm = TRUE)

                                # Impute missing values
                                for (level in names(medians_by_group)) {
                                    mask <- is.na(data[[var]]) & data[[targetName]] == level
                                    if (any(mask)) {
                                        replacement_value <- medians_by_group[level]
                                        # If group median is NA, use overall median
                                        if (is.na(replacement_value)) {
                                            replacement_value <- median(data[[var]], na.rm = TRUE)
                                        }
                                        data[mask, var] <- replacement_value
                                    }
                                }
                            }
                        }

                        # For categorical variables: mode imputation within disease groups
                        for (fac in facsName) {
                            if (sum(is.na(data[[fac]])) > 0) {
                                # Calculate mode by target group
                                for (level in unique(data[[targetName]])) {
                                    mask <- is.na(data[[fac]]) & data[[targetName]] == level
                                    if (any(mask)) {
                                        # Get mode for this group
                                        group_data <- data[data[[targetName]] == level & !is.na(data[[fac]]), fac]
                                        if (length(group_data) > 0) {
                                            mode_val <- names(sort(table(group_data), decreasing = TRUE))[1]
                                            data[mask, fac] <- mode_val
                                        } else {
                                            # If no data in group, use overall mode
                                            overall_mode <- names(sort(table(data[[fac]], useNA = "no"), decreasing = TRUE))[1]
                                            data[mask, fac] <- overall_mode
                                        }
                                    }
                                }
                                # Convert back to factor
                                data[[fac]] <- as.factor(data[[fac]])
                            }
                        }

                        return(data)
                    },

                    # Balance data for rare diseases ----
                    .balanceData = function(data) {
                        # Simple upsampling of minority class
                        minorityClass <- names(sort(table(data$outcome)))[1]
                        majorityClass <- names(sort(table(data$outcome)))[2]

                        minorityData <- data[data$outcome == minorityClass, ]
                        majorityData <- data[data$outcome == majorityClass, ]

                        # Upsample minority class
                        nMajority <- nrow(majorityData)
                        nMinority <- nrow(minorityData)

                        if (nMinority < nMajority) {
                            minorityUpsampled <- minorityData[sample(nrow(minorityData),
                                                                     nMajority,
                                                                     replace = TRUE), ]
                            balancedData <- rbind(majorityData, minorityUpsampled)
                        } else {
                            balancedData <- data
                        }

                        return(balancedData)
                    },

                    # Calculate comprehensive clinical metrics ----
                    .calculateClinicalMetrics = function(predictions, actual, probabilities = NULL) {

                        # Confusion matrix
                        cm <- table(Predicted = predictions, Actual = actual)

                        # Basic metrics
                        TP <- cm[2, 2]  # True Positives
                        TN <- cm[1, 1]  # True Negatives
                        FP <- cm[2, 1]  # False Positives
                        FN <- cm[1, 2]  # False Negatives

                        # Calculate clinical metrics
                        sensitivity <- TP / (TP + FN)  # Recall/True Positive Rate
                        specificity <- TN / (TN + FP)  # True Negative Rate
                        precision <- TP / (TP + FP)    # Positive Predictive Value
                        npv <- TN / (TN + FN)          # Negative Predictive Value
                        accuracy <- (TP + TN) / sum(cm)
                        f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

                        # Likelihood ratios (important for clinical decision making)
                        lr_positive <- sensitivity / (1 - specificity)
                        lr_negative <- (1 - sensitivity) / specificity

                        # Diagnostic odds ratio
                        dor <- (TP * TN) / (FP * FN)

                        # Youden's J statistic (optimal threshold)
                        youden_j <- sensitivity + specificity - 1

                        # Number needed to diagnose
                        nnd <- 1 / (sensitivity - (1 - specificity))

                        metrics <- list(
                            "Sensitivity" = sensitivity,
                            "Specificity" = specificity,
                            "PPV" = precision,
                            "NPV" = npv,
                            "Accuracy" = accuracy,
                            "F1_Score" = f1_score,
                            "LR_Positive" = lr_positive,
                            "LR_Negative" = lr_negative,
                            "DOR" = dor,
                            "Youden_J" = youden_j,
                            "NND" = nnd
                        )

                        # Add AUC if probabilities available
                        if (!is.null(probabilities)) {
                            roc_obj <- pROC::roc(actual, probabilities)
                            auc <- as.numeric(pROC::auc(roc_obj))
                            metrics[["AUC"]] <- auc
                        }

                        return(metrics)
                    },

                    # Feature importance analysis ----
                    .analyzeFeatureImportance = function(model, data) {
                        # Get feature importance from FFTrees
                        if (class(model)[1] == "FFTrees") {
                            # Extract cues used in best tree
                            bestTree <- model$trees$best
                            cues <- model$trees$definitions[bestTree, ]

                            # Get frequency of use across all trees
                            allCues <- unlist(model$trees$definitions[, "cues"])
                            cueImportance <- table(allCues)

                            importance_df <- data.frame(
                                Feature = names(cueImportance),
                                Importance = as.numeric(cueImportance),
                                Normalized_Importance = as.numeric(cueImportance) / max(cueImportance)
                            )

                            return(importance_df[order(importance_df$Importance, decreasing = TRUE), ])
                        }

                        return(NULL)
                    },

                    # Generate clinical interpretation ----
                    .generateClinicalInterpretation = function(metrics, prevalence) {
                        interpretation <- c()

                        # Sensitivity interpretation
                        if (metrics$Sensitivity >= 0.9) {
                            interpretation <- c(interpretation, "Excellent sensitivity - very few cases will be missed")
                        } else if (metrics$Sensitivity >= 0.8) {
                            interpretation <- c(interpretation, "Good sensitivity - acceptable miss rate for screening")
                        } else if (metrics$Sensitivity >= 0.7) {
                            interpretation <- c(interpretation, "Moderate sensitivity - consider additional testing")
                        } else {
                            interpretation <- c(interpretation, "Low sensitivity - high risk of missed diagnoses")
                        }

                        # Specificity interpretation
                        if (metrics$Specificity >= 0.9) {
                            interpretation <- c(interpretation, "Excellent specificity - very few false alarms")
                        } else if (metrics$Specificity >= 0.8) {
                            interpretation <- c(interpretation, "Good specificity - acceptable false positive rate")
                        } else {
                            interpretation <- c(interpretation, "Moderate specificity - many false positives expected")
                        }

                        # Likelihood ratio interpretation
                        if (metrics$LR_Positive >= 10) {
                            interpretation <- c(interpretation, "Positive test strongly increases disease probability")
                        } else if (metrics$LR_Positive >= 5) {
                            interpretation <- c(interpretation, "Positive test moderately increases disease probability")
                        } else if (metrics$LR_Positive >= 2) {
                            interpretation <- c(interpretation, "Positive test slightly increases disease probability")
                        } else {
                            interpretation <- c(interpretation, "Positive test has little diagnostic value")
                        }

                        if (metrics$LR_Negative <= 0.1) {
                            interpretation <- c(interpretation, "Negative test strongly decreases disease probability")
                        } else if (metrics$LR_Negative <= 0.2) {
                            interpretation <- c(interpretation, "Negative test moderately decreases disease probability")
                        } else if (metrics$LR_Negative <= 0.5) {
                            interpretation <- c(interpretation, "Negative test slightly decreases disease probability")
                        } else {
                            interpretation <- c(interpretation, "Negative test has little diagnostic value")
                        }

                        return(paste(interpretation, collapse = "<br>"))
                    },

                    # Main run function ----
                    .run = function() {

                        # Error checking
                        if (nrow(self$data) == 0) {
                            stop("Data contains no (complete) rows")
                        }

                        if (is.null(self$options$vars) && is.null(self$options$facs)) {
                            todo <- "
                            <br><b>Welcome to ClinicoPath Medical Decision Tree Analysis</b>
                            <br><br>
                            This tool creates decision trees optimized for medical diagnosis and prognosis.
                            <br><br>
                            <b>Designed specifically for:</b><br>
                            • Pathology diagnosis support<br>
                            • Oncology staging and prognosis<br>
                            • Biomarker panel optimization<br>
                            • Clinical risk stratification<br>
                            • Treatment response prediction<br>
                            <br>
                            <b>Key Features:</b><br>
                            • Clinical performance metrics (Sensitivity, Specificity, PPV, NPV)<br>
                            • Likelihood ratios for evidence-based medicine<br>
                            • Missing data handling appropriate for clinical data<br>
                            • Class balancing for rare diseases<br>
                            • Feature importance for biomarker selection<br>
                            • Clinical interpretation guidelines<br>
                            <br>
                            <b>Required:</b><br>
                            • Target Variable: Disease outcome or classification<br>
                            • Explanatory Variables: Clinical features, biomarkers, or pathological findings<br>
                            <br>
                            <b>Optional:</b><br>
                            • Train/Test Variable: For validation (recommended)<br>
                            • Missing data imputation<br>
                            • Class balancing for rare diseases<br>
                            <hr>
                            "

                            html <- self$results$todo
                            html$setContent(todo)
                            return()
                        }

                        # Prepare data
                        results <- private$.prepareData()
                        if (is.null(results)) {
                            return()
                        }

                        trainData <- results$trainData
                        testData <- results$testData
                        prevalence <- results$prevalence

                        # Build medical decision tree
                        targetName <- if(!is.null(self$options$target)) trimws(self$options$target) else NULL

                        # Prepare formula with trimmed variable names (Issue #12 fix)
                        trimmed_vars <- if(!is.null(self$options$vars)) trimws(self$options$vars) else NULL
                        trimmed_facs <- if(!is.null(self$options$facs)) trimws(self$options$facs) else NULL
                        varsFormula <- paste(c(trimmed_vars, trimmed_facs), collapse = " + ")
                        myformula <- as.formula(paste("outcome ~", varsFormula))

                        # Progress indicator
                        self$results$text1$setContent("Building decision tree model...")

                        # Build FFTrees model optimized for medical decisions
                        mytree.fft <- FFTrees::FFTrees(
                            formula = myformula,
                            data = trainData,
                            data.test = if(nrow(testData) > 0) testData else NULL,
                            goal = "bacc",  # Balanced accuracy for medical decisions
                            goal.chase = "bacc",
                            goal.threshold = "bacc",
                            main = "Medical Decision Tree",
                            decision.labels = c("Control", "Disease"),
                            do.comp = FALSE,  # Skip time-consuming competitor algorithms
                            do.cart = TRUE,   # Include CART for comparison
                            do.lr = TRUE,     # Include logistic regression
                            do.rf = FALSE,    # Skip random forest to save time
                            do.svm = FALSE    # Skip SVM to save time
                        )

                        # Update progress
                        self$results$text1$setContent("Model training completed. Calculating performance metrics...")

                        # Get predictions for test set
                        if (nrow(testData) > 0) {
                            predictions <- predict(mytree.fft, testData)
                            probabilities <- predict(mytree.fft, testData, type = "prob")

                            # Calculate comprehensive clinical metrics
                            metrics <- private$.calculateClinicalMetrics(
                                predictions,
                                testData$outcome,
                                probabilities[, "Disease"]
                            )

                            # Populate clinical metrics table
                            metricNames <- c("Sensitivity", "Specificity", "PPV", "NPV",
                                             "Accuracy", "F1_Score", "LR_Positive", "LR_Negative",
                                             "AUC", "Youden_J")

                            for (i in seq_along(metricNames)) {
                                if (metricNames[i] %in% names(metrics)) {
                                    value <- metrics[[metricNames[i]]]
                                    formatted_value <- if(is.finite(value)) {
                                        sprintf("%.3f", value)
                                    } else {
                                        "Not calculable"
                                    }

                                    self$results$clinicalMetrics$addRow(rowKey=i, values=list(
                                        metric = metricNames[i],
                                        value = formatted_value,
                                        interpretation = private$.interpretMetric(metricNames[i], value)
                                    ))
                                }
                            }

                            # Generate clinical interpretation
                            interpretation <- private$.generateClinicalInterpretation(metrics, prevalence)
                            self$results$clinicalInterpretation$setContent(interpretation)

                            # Feature importance analysis
                            importance <- private$.analyzeFeatureImportance(mytree.fft, trainData)
                            if (!is.null(importance)) {
                                for (i in 1:min(nrow(importance), 10)) {  # Top 10 features
                                    self$results$featureImportance$addRow(rowKey=i, values=list(
                                        feature = importance$Feature[i],
                                        importance = round(importance$Normalized_Importance[i], 3),
                                        rank = i
                                    ))
                                }
                            }
                        }

                        # Store model summary
                        self$results$modelSummary$setContent(
                            paste0("<h3>Model Summary</h3>",
                                   "<p>Decision tree successfully created with FFTrees algorithm.</p>",
                                   "<p><strong>Training samples:</strong> ", nrow(trainData), "</p>",
                                   "<p><strong>Test samples:</strong> ", nrow(testData), "</p>",
                                   "<p><strong>Disease prevalence:</strong> ", round(prevalence * 100, 1), "%</p>")
                        )

                        # Data quality report - simplified version
                        dataQuality <- private$.generateDataQualityReport(results$mydata)
                        self$results$dataQuality$setContent(dataQuality)

                        # Update final status
                        self$results$text1$setContent("Analysis completed successfully!")
                        self$results$text3$setContent("Model ready for clinical evaluation")

                        # Perform additional validations if requested (Issue #12 enhancements)
                        if (self$options$crossValidation) {
                            private$.performCrossValidation(results$mydata, myformula)
                        }
                        
                        if (self$options$bootstrapValidation) {
                            private$.performBootstrapValidation(trainData, testData, myformula)
                        }
                        
                        if (self$options$compareModels) {
                            private$.compareModels(trainData, testData, myformula)
                        }

                        # Store plot data
                        plotData <- list(
                            model = mytree.fft,
                            testData = testData,
                            trainData = trainData
                        )

                        # Store plot data for all plot types
                        self$results$plot$setState(plotData)
                        self$results$partitionPlot$setState(plotData)
                        self$results$rocPlot$setState(plotData)
                        self$results$calibrationPlot$setState(plotData)
                        self$results$clinicalUtilityPlot$setState(plotData)
                    },

                    # Assess clinical relevance of features ----
                    .assessClinicalRelevance = function(feature) {
                        # Define clinical relevance based on feature names/types
                        clinical_keywords <- c(
                            "age", "grade", "stage", "size", "marker", "level",
                            "count", "ratio", "score", "index", "status"
                        )

                        feature_lower <- tolower(feature)

                        if (any(sapply(clinical_keywords, function(x) grepl(x, feature_lower)))) {
                            return("High clinical relevance")
                        } else {
                            return("Moderate clinical relevance")
                        }
                    },

                    # Risk stratification analysis ----
                    .performRiskStratification = function(probabilities, outcomes) {
                        # Create risk groups based on probability tertiles
                        risk_breaks <- quantile(probabilities, probs = c(0, 0.33, 0.67, 1))
                        risk_groups <- cut(probabilities, breaks = risk_breaks,
                                           labels = c("Low Risk", "Medium Risk", "High Risk"),
                                           include.lowest = TRUE)

                        # Calculate metrics for each risk group
                        risk_summary <- data.frame(
                            Risk_Group = levels(risk_groups),
                            N_Patients = as.numeric(table(risk_groups)),
                            Event_Rate = as.numeric(tapply(outcomes == "Disease", risk_groups, mean) * 100),
                            stringsAsFactors = FALSE
                        )

                        # Calculate relative risk compared to low risk group
                        baseline_rate <- risk_summary$Event_Rate[1] / 100
                        risk_summary$Relative_Risk <- risk_summary$Event_Rate / 100 / baseline_rate

                        # Add clinical actions
                        risk_summary$Clinical_Action <- c(
                            "Routine follow-up",
                            "Increased surveillance",
                            "Immediate intervention"
                        )

                        return(risk_summary)
                    },

                    # Adjust metrics for different prevalence ----
                    .adjustForPrevalence = function(metrics, study_prev, target_prev) {
                        sensitivity <- metrics$Sensitivity
                        specificity <- metrics$Specificity

                        # Adjusted PPV and NPV for target prevalence
                        adjusted_ppv <- (sensitivity * target_prev) /
                            (sensitivity * target_prev + (1 - specificity) * (1 - target_prev))

                        adjusted_npv <- (specificity * (1 - target_prev)) /
                            (specificity * (1 - target_prev) + (1 - sensitivity) * target_prev)

                        return(list(
                            PPV = adjusted_ppv,
                            NPV = adjusted_npv
                        ))
                    },

                    # Generate deployment guidelines ----
                    .generateDeploymentGuidelines = function(metrics, clinical_context) {
                        guidelines <- paste0(
                            "<h3>Clinical Implementation Guidelines</h3>",
                            "<p><strong>Clinical Context:</strong> ", clinical_context, "</p>",
                            "<p><strong>Model Performance Summary:</strong></p>",
                            "<ul>",
                            "<li>Sensitivity: ", sprintf("%.1f%%", metrics$Sensitivity * 100), "</li>",
                            "<li>Specificity: ", sprintf("%.1f%%", metrics$Specificity * 100), "</li>",
                            "<li>Positive Predictive Value: ", sprintf("%.1f%%", metrics$PPV * 100), "</li>",
                            "<li>Negative Predictive Value: ", sprintf("%.1f%%", metrics$NPV * 100), "</li>",
                            "</ul>"
                        )

                        # Add context-specific recommendations
                        if (clinical_context == "screening") {
                            guidelines <- paste0(guidelines,
                                                 "<p><strong>Screening Recommendations:</strong></p>",
                                                 "<ul>",
                                                 "<li>Use as first-line screening tool</li>",
                                                 "<li>Follow positive results with confirmatory testing</li>",
                                                 "<li>Consider lowering threshold for high-risk populations</li>",
                                                 "</ul>"
                            )
                        } else if (clinical_context == "diagnosis") {
                            guidelines <- paste0(guidelines,
                                                 "<p><strong>Diagnostic Recommendations:</strong></p>",
                                                 "<ul>",
                                                 "<li>Use as diagnostic aid, not replacement for clinical judgment</li>",
                                                 "<li>Consider additional testing for borderline cases</li>",
                                                 "<li>Document rationale for clinical decisions</li>",
                                                 "</ul>"
                            )
                        }

                        return(guidelines)
                    },
                    .interpretMetric = function(metric, value) {
                        if (!is.finite(value)) return("Not calculable")

                        switch(metric,
                               "Sensitivity" = if(value >= 0.9) "Excellent" else if(value >= 0.8) "Good" else if(value >= 0.7) "Adequate" else "Poor",
                               "Specificity" = if(value >= 0.9) "Excellent" else if(value >= 0.8) "Good" else if(value >= 0.7) "Adequate" else "Poor",
                               "PPV" = if(value >= 0.8) "High confidence" else if(value >= 0.6) "Moderate confidence" else "Low confidence",
                               "NPV" = if(value >= 0.9) "High confidence" else if(value >= 0.8) "Moderate confidence" else "Low confidence",
                               "LR_Positive" = if(value >= 10) "Strong evidence" else if(value >= 5) "Moderate evidence" else if(value >= 2) "Weak evidence" else "Minimal evidence",
                               "LR_Negative" = if(value <= 0.1) "Strong evidence" else if(value <= 0.2) "Moderate evidence" else if(value <= 0.5) "Weak evidence" else "Minimal evidence",
                               "AUC" = if(value >= 0.9) "Excellent discrimination" else if(value >= 0.8) "Good discrimination" else if(value >= 0.7) "Fair discrimination" else "Poor discrimination",
                               ""
                        )
                    },

                    # Generate data quality report ----
                    .generateDataQualityReport = function(data) {
                        report <- list(
                            "Total_Cases" = nrow(data),
                            "Disease_Cases" = sum(data$outcome == "Disease"),
                            "Control_Cases" = sum(data$outcome == "Control"),
                            "Disease_Prevalence" = round(mean(data$outcome == "Disease") * 100, 1),
                            "Complete_Cases" = sum(complete.cases(data))
                        )

                        return(report)
                    },

                    # Plot function ----
                    .plot = function(image, ggtheme, theme, ...) {

                        plotData <- image$state

                        if (is.null(plotData$model)) {
                            return()
                        }

                        # Create the FFTrees plot
                        plot(plotData$model,
                             main = "Medical Decision Tree",
                             data = if(nrow(plotData$testData) > 0) "test" else "train")

                        TRUE
                    },

                    # ROC curve plot ----
                    .plotROC = function(image, ggtheme, theme, ...) {

                        plotData <- image$state

                        if (is.null(plotData$model) || nrow(plotData$testData) == 0) {
                            return()
                        }

                        # Get predictions and probabilities
                        probabilities <- predict(plotData$model, plotData$testData, type = "prob")
                        actual <- plotData$testData$outcome

                        # Create ROC curve
                        roc_obj <- pROC::roc(actual, probabilities[, "Disease"])

                        # Plot ROC curve with clinical context
                        plot(roc_obj,
                             main = "ROC Curve - Clinical Performance",
                             col = "#2E86AB",
                             lwd = 2,
                             legacy.axes = TRUE)

                        # Add AUC to plot
                        auc_value <- as.numeric(pROC::auc(roc_obj))
                        text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), cex = 1.2)

                        # Add clinical interpretation lines
                        abline(a = 0, b = 1, lty = 2, col = "gray", lwd = 1)

                        # Add optimal cutoff point
                        coords_obj <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
                        points(1 - coords_obj$specificity, coords_obj$sensitivity,
                               col = "red", pch = 19, cex = 1.5)
                        text(1 - coords_obj$specificity + 0.1, coords_obj$sensitivity,
                             paste("Optimal\nCutoff"), cex = 0.8)

                        TRUE
                    },

                    # Calibration plot ----
                    .plotCalibration = function(image, ggtheme, theme, ...) {

                        plotData <- image$state

                        if (is.null(plotData$model) || nrow(plotData$testData) == 0) {
                            return()
                        }

                        # Get predicted probabilities
                        probabilities <- predict(plotData$model, plotData$testData, type = "prob")
                        actual <- as.numeric(plotData$testData$outcome == "Disease")
                        pred_prob <- probabilities[, "Disease"]

                        # Create calibration bins
                        breaks <- seq(0, 1, by = 0.1)
                        bins <- cut(pred_prob, breaks = breaks, include.lowest = TRUE)

                        # Calculate observed vs predicted for each bin
                        obs_freq <- tapply(actual, bins, mean, na.rm = TRUE)
                        pred_freq <- tapply(pred_prob, bins, mean, na.rm = TRUE)
                        bin_counts <- table(bins)

                        # Remove bins with no data
                        valid_bins <- !is.na(obs_freq) & !is.na(pred_freq)
                        obs_freq <- obs_freq[valid_bins]
                        pred_freq <- pred_freq[valid_bins]
                        bin_counts <- bin_counts[valid_bins]

                        # Create calibration plot
                        plot(pred_freq, obs_freq,
                             xlim = c(0, 1), ylim = c(0, 1),
                             xlab = "Predicted Probability",
                             ylab = "Observed Frequency",
                             main = "Probability Calibration",
                             pch = 19, cex = sqrt(bin_counts/max(bin_counts)) * 2,
                             col = "#2E86AB")

                        # Add perfect calibration line
                        abline(a = 0, b = 1, lty = 2, col = "gray", lwd = 2)

                        # Add legend
                        legend("topleft",
                               legend = c("Perfect Calibration", "Observed vs Predicted", "Bubble size ∝ N cases"),
                               lty = c(2, NA, NA),
                               pch = c(NA, 19, 1),
                               col = c("gray", "#2E86AB", "black"),
                               cex = 0.8)

                        TRUE
                    },

                    # Clinical utility curve ----
                    .plotClinicalUtility = function(image, ggtheme, theme, ...) {

                        plotData <- image$state

                        if (is.null(plotData$model) || nrow(plotData$testData) == 0) {
                            return()
                        }

                        # Get predictions and probabilities
                        probabilities <- predict(plotData$model, plotData$testData, type = "prob")
                        actual <- plotData$testData$outcome

                        # Calculate clinical utility across different thresholds
                        thresholds <- seq(0.01, 0.99, by = 0.01)
                        utilities <- sapply(thresholds, function(thresh) {
                            predictions <- ifelse(probabilities[, "Disease"] >= thresh, "Disease", "Control")

                            # Calculate confusion matrix
                            cm <- table(predictions, actual)
                            if (nrow(cm) == 2 && ncol(cm) == 2) {
                                TP <- cm[2, 2]
                                TN <- cm[1, 1]
                                FP <- cm[2, 1]
                                FN <- cm[1, 2]

                                # Clinical utility (can be customized based on clinical context)
                                utility <- (TP - self$options$costRatio * FP) / nrow(plotData$testData)
                                return(utility)
                            } else {
                                return(0)
                            }
                        })

                        # Plot clinical utility curve
                        plot(thresholds, utilities,
                             type = "l", lwd = 2, col = "#2E86AB",
                             xlab = "Decision Threshold",
                             ylab = "Clinical Utility",
                             main = "Clinical Utility Curve")

                        # Add optimal threshold
                        optimal_idx <- which.max(utilities)
                        optimal_thresh <- thresholds[optimal_idx]
                        optimal_utility <- utilities[optimal_idx]

                        points(optimal_thresh, optimal_utility, col = "red", pch = 19, cex = 1.5)
                        text(optimal_thresh, optimal_utility + 0.02,
                             paste("Optimal:", round(optimal_thresh, 3)),
                             cex = 0.8, pos = 3)

                        # Add horizontal line at utility = 0
                        abline(h = 0, lty = 2, col = "gray")

                        TRUE
                    },

                    # Partition plot function using parttree ----
                    .plotPartition = function(image, ggtheme, theme, ...) {
                        
                        plotData <- image$state
                        
                        if (is.null(plotData$model)) {
                            return()
                        }
                        
                        # Check if parttree package is available
                        if (!requireNamespace("parttree", quietly = TRUE)) {
                            plot.new()
                            text(0.5, 0.5, "parttree package required for partition plots\nInstall with: install.packages('parttree')", 
                                 cex = 1.2, col = "red", adj = 0.5)
                            return(TRUE)
                        }
                        
                        # Check if we have exactly 2 continuous variables for optimal visualization
                        continuous_vars <- self$options$vars
                        
                        if (length(continuous_vars) < 2) {
                            plot.new()
                            text(0.5, 0.5, "Partition plot requires at least 2 continuous variables\nPlease select additional variables", 
                                 cex = 1.2, col = "orange", adj = 0.5)
                            return(TRUE)
                        }
                        
                        if (length(continuous_vars) > 2) {
                            # Use first 2 variables and warn user
                            continuous_vars <- continuous_vars[1:2]
                            message("Using first 2 continuous variables for partition plot: ", 
                                   paste(continuous_vars, collapse = ", "))
                        }
                        
                        # Get the appropriate dataset
                        plot_data <- if(nrow(plotData$testData) > 0) plotData$testData else plotData$trainData
                        
                        # Create formula for rpart (parttree requires rpart model)
                        formula_vars <- paste(continuous_vars, collapse = " + ")
                        partition_formula <- as.formula(paste("outcome ~", formula_vars))
                        
                        # Fit rpart model for partition visualization
                        rpart_model <- rpart::rpart(partition_formula, data = plot_data,
                                                   method = "class",
                                                   control = rpart::rpart.control(
                                                       minsplit = max(10, self$options$minCases),
                                                       maxdepth = self$options$maxDepth
                                                   ))
                        
                        # Create parttree plot
                        tryCatch({
                            library(ggplot2)
                            
                            # Create the base ggplot
                            p <- ggplot(plot_data, aes(x = .data[[continuous_vars[1]]], 
                                                      y = .data[[continuous_vars[2]]], 
                                                      color = outcome)) +
                                geom_point(alpha = 0.7, size = 2) +
                                parttree::geom_parttree(data = rpart_model, alpha = 0.3) +
                                labs(
                                    title = "Decision Tree Partition Plot",
                                    subtitle = paste("Variables:", paste(continuous_vars, collapse = " vs ")),
                                    x = continuous_vars[1],
                                    y = continuous_vars[2],
                                    color = "Outcome"
                                ) +
                                scale_color_manual(values = c("Control" = "#2E86AB", "Disease" = "#A23B72")) +
                                theme_minimal() +
                                theme(
                                    plot.title = element_text(size = 14, face = "bold"),
                                    plot.subtitle = element_text(size = 12),
                                    legend.position = "bottom"
                                )
                            
                            # Apply user theme if provided
                            if (!missing(ggtheme)) {
                                p <- p + ggtheme
                            }
                            
                            print(p)
                            
                        }, error = function(e) {
                            plot.new()
                            text(0.5, 0.5, paste("Error creating partition plot:", e$message), 
                                 cex = 1, col = "red", adj = 0.5)
                        })
                        
                        TRUE
                    },

                    # Cross-validation implementation (Issue #12 enhancement) ----
                    .performCrossValidation = function(data, formula) {
                        tryCatch({
                            folds <- self$options$cvFolds
                            set.seed(123)  # For reproducibility
                            
                            # Create folds
                            fold_indices <- caret::createFolds(data$outcome, k = folds, list = TRUE)
                            
                            cv_results <- data.frame()
                            
                            for (i in 1:folds) {
                                # Split data
                                test_idx <- fold_indices[[i]]
                                cv_train <- data[-test_idx, ]
                                cv_test <- data[test_idx, ]
                                
                                # Build model
                                cv_model <- FFTrees::FFTrees(
                                    formula = formula,
                                    data = cv_train,
                                    data.test = cv_test,
                                    goal = "bacc",
                                    do.comp = FALSE,
                                    do.cart = FALSE,
                                    do.lr = FALSE,
                                    do.rf = FALSE,
                                    do.svm = FALSE
                                )
                                
                                # Get predictions and calculate metrics
                                predictions <- predict(cv_model, cv_test)
                                probabilities <- predict(cv_model, cv_test, type = "prob")
                                
                                metrics <- private$.calculateClinicalMetrics(
                                    predictions, cv_test$outcome, probabilities[, "Disease"]
                                )
                                
                                # Add to results
                                cv_results <- rbind(cv_results, data.frame(
                                    fold = i,
                                    sensitivity = metrics$Sensitivity,
                                    specificity = metrics$Specificity,
                                    accuracy = metrics$Accuracy,
                                    auc = metrics$AUC
                                ))
                            }
                            
                            # Populate CV results table
                            for (i in 1:nrow(cv_results)) {
                                self$results$crossValidationResults$addRow(rowKey = i, values = list(
                                    fold = cv_results$fold[i],
                                    sensitivity = cv_results$sensitivity[i],
                                    specificity = cv_results$specificity[i],
                                    accuracy = cv_results$accuracy[i],
                                    auc = cv_results$auc[i]
                                ))
                            }
                            
                        }, error = function(e) {
                            warning(paste("Cross-validation failed:", e$message))
                        })
                    },

                    # Bootstrap validation implementation (Issue #12 enhancement) ----
                    .performBootstrapValidation = function(trainData, testData, formula) {
                        tryCatch({
                            n_boot <- self$options$bootstrapSamples
                            set.seed(123)
                            
                            boot_results <- list()
                            
                            for (i in 1:n_boot) {
                                # Bootstrap sample
                                boot_idx <- sample(nrow(trainData), replace = TRUE)
                                boot_train <- trainData[boot_idx, ]
                                
                                # Build model
                                boot_model <- FFTrees::FFTrees(
                                    formula = formula,
                                    data = boot_train,
                                    data.test = testData,
                                    goal = "bacc",
                                    do.comp = FALSE,
                                    do.cart = FALSE,
                                    do.lr = FALSE,
                                    do.rf = FALSE,
                                    do.svm = FALSE
                                )
                                
                                # Get predictions and calculate metrics
                                if (nrow(testData) > 0) {
                                    predictions <- predict(boot_model, testData)
                                    probabilities <- predict(boot_model, testData, type = "prob")
                                    
                                    metrics <- private$.calculateClinicalMetrics(
                                        predictions, testData$outcome, probabilities[, "Disease"]
                                    )
                                    
                                    boot_results[[i]] <- metrics
                                }
                            }
                            
                            # Calculate summary statistics
                            if (length(boot_results) > 0) {
                                metric_names <- names(boot_results[[1]])
                                summary_stats <- list()
                                
                                for (metric in metric_names) {
                                    values <- sapply(boot_results, function(x) x[[metric]])
                                    values <- values[is.finite(values)]
                                    
                                    if (length(values) > 0) {
                                        summary_stats[[metric]] <- list(
                                            mean = mean(values),
                                            ci_lower = quantile(values, 0.025),
                                            ci_upper = quantile(values, 0.975),
                                            bias = mean(values) - boot_results[[1]][[metric]]
                                        )
                                    }
                                }
                                
                                # Populate bootstrap results table
                                i <- 1
                                for (metric in names(summary_stats)) {
                                    stats <- summary_stats[[metric]]
                                    self$results$bootstrapResults$addRow(rowKey = i, values = list(
                                        metric = metric,
                                        mean = stats$mean,
                                        ci_lower = stats$ci_lower,
                                        ci_upper = stats$ci_upper,
                                        bias = stats$bias
                                    ))
                                    i <- i + 1
                                }
                            }
                            
                        }, error = function(e) {
                            warning(paste("Bootstrap validation failed:", e$message))
                        })
                    },

                    # Model comparison implementation (Issue #12 enhancement) ----
                    .compareModels = function(trainData, testData, formula) {
                        tryCatch({
                            if (nrow(testData) == 0) return()
                            
                            models <- list()
                            results <- data.frame()
                            
                            # FFTrees model (already built, simplified version for comparison)
                            fft_model <- FFTrees::FFTrees(
                                formula = formula,
                                data = trainData,
                                data.test = testData,
                                goal = self$options$modelComparisonMetric,
                                do.comp = FALSE,
                                do.cart = TRUE,
                                do.lr = TRUE,
                                do.rf = FALSE,
                                do.svm = FALSE
                            )
                            
                            # Get FFTrees predictions
                            fft_pred <- predict(fft_model, testData)
                            fft_prob <- predict(fft_model, testData, type = "prob")
                            fft_metrics <- private$.calculateClinicalMetrics(
                                fft_pred, testData$outcome, fft_prob[, "Disease"]
                            )
                            
                            # Logistic regression
                            lr_model <- glm(formula, data = trainData, family = binomial)
                            lr_prob <- predict(lr_model, testData, type = "response")
                            lr_pred <- factor(ifelse(lr_prob > 0.5, "Disease", "Control"), levels = c("Control", "Disease"))
                            lr_metrics <- private$.calculateClinicalMetrics(
                                lr_pred, testData$outcome, lr_prob
                            )
                            
                            # CART model
                            cart_model <- rpart::rpart(formula, data = trainData, method = "class")
                            cart_pred <- predict(cart_model, testData, type = "class")
                            cart_prob <- predict(cart_model, testData, type = "prob")[, "Disease"]
                            cart_metrics <- private$.calculateClinicalMetrics(
                                cart_pred, testData$outcome, cart_prob
                            )
                            
                            # Initialize model lists
                            model_metrics <- list(fft_metrics, lr_metrics, cart_metrics)
                            model_names <- c("FFTrees", "Logistic Regression", "CART")
                            primary_values <- c(
                                fft_metrics[[switch(comparison_metric, 
                                                   "bacc" = "Accuracy", "auc" = "AUC", "sens" = "Sensitivity", 
                                                   "spec" = "Specificity", "f1" = "F1_Score")]],
                                lr_metrics[[switch(comparison_metric, 
                                                  "bacc" = "Accuracy", "auc" = "AUC", "sens" = "Sensitivity", 
                                                  "spec" = "Specificity", "f1" = "F1_Score")]],
                                cart_metrics[[switch(comparison_metric, 
                                                    "bacc" = "Accuracy", "auc" = "AUC", "sens" = "Sensitivity", 
                                                    "spec" = "Specificity", "f1" = "F1_Score")]]
                            )
                            spatial_fits <- c("N/A", "N/A", "N/A")
                            
                            # Add autocart if spatial coordinates and option enabled
                            if (self$options$useAutocart && length(self$options$spatialCoords) >= 2) {
                                autocart_result <- private$.performAutocartAnalysis(trainData, testData, formula)
                                if (!is.null(autocart_result)) {
                                    model_names <- c(model_names, "Autocart (Spatial)")
                                    model_metrics <- c(model_metrics, list(autocart_result$metrics))
                                    primary_values <- c(primary_values, autocart_result$metrics[[switch(comparison_metric, 
                                                       "bacc" = "Accuracy", "auc" = "AUC", "sens" = "Sensitivity", 
                                                       "spec" = "Specificity", "f1" = "F1_Score")]])
                                    spatial_fits <- c(spatial_fits, autocart_result$spatial_fit)
                                }
                            }
                            
                            best_idx <- which.max(primary_values)
                            
                            # Populate comparison table
                            for (i in seq_along(model_names)) {
                                metrics <- model_metrics[[i]]
                                self$results$modelComparison$addRow(rowKey = i, values = list(
                                    model = model_names[i],
                                    primary_metric = primary_values[i],
                                    sensitivity = metrics$Sensitivity,
                                    specificity = metrics$Specificity,
                                    auc = metrics$AUC,
                                    spatial_metric = spatial_fits[i],
                                    best_model = if(i == best_idx) "★ Best" else ""
                                ))
                            }
                            
                        }, error = function(e) {
                            warning(paste("Model comparison failed:", e$message))
                        })
                    },

                    # Autocart spatial analysis implementation ----
                    .performAutocartAnalysis = function(trainData, testData, formula) {
                        tryCatch({
                            # Check if autocart package is available
                            if (!requireNamespace("autocart", quietly = TRUE)) {
                                warning("autocart package not available. Install with: devtools::install_github('ethanancell/autocart')")
                                return(NULL)
                            }
                            
                            # Get spatial coordinates
                            coord_vars <- self$options$spatialCoords[1:2]
                            x_var <- coord_vars[1]
                            y_var <- coord_vars[2]
                            
                            # Prepare spatial data for autocart
                            spatial_train <- trainData
                            spatial_test <- testData
                            
                            # Check if coordinates exist
                            if (!all(c(x_var, y_var) %in% names(spatial_train))) {
                                warning("Spatial coordinates not found in data")
                                return(NULL)
                            }
                            
                            # Build autocart model
                            autocart_model <- autocart::autocart(
                                formula = formula,
                                data = spatial_train,
                                xcoord = x_var,
                                ycoord = y_var,
                                alpha = self$options$spatialAlpha,
                                beta = self$options$spatialBeta
                            )
                            
                            # Get predictions
                            autocart_pred <- predict(autocart_model, spatial_test)
                            
                            # Calculate metrics
                            # Note: autocart may return different prediction format, handle accordingly
                            if (is.factor(autocart_pred)) {
                                pred_class <- autocart_pred
                                pred_prob <- as.numeric(autocart_pred == "Disease")
                            } else {
                                pred_prob <- autocart_pred
                                pred_class <- factor(ifelse(pred_prob > 0.5, "Disease", "Control"), 
                                                   levels = c("Control", "Disease"))
                            }
                            
                            metrics <- private$.calculateClinicalMetrics(
                                pred_class, spatial_test$outcome, pred_prob
                            )
                            
                            # Calculate spatial fit measure (simplified)
                            spatial_fit <- paste0("α=", self$options$spatialAlpha, ", β=", self$options$spatialBeta)
                            
                            # Update spatial analysis table
                            private$.updateSpatialAnalysis(autocart_model, spatial_fit)
                            
                            return(list(
                                model = autocart_model,
                                metrics = metrics,
                                spatial_fit = spatial_fit
                            ))
                            
                        }, error = function(e) {
                            warning(paste("Autocart analysis failed:", e$message))
                            return(NULL)
                        })
                    },

                    # Update spatial analysis results ----
                    .updateSpatialAnalysis = function(autocart_model, spatial_fit) {
                        tryCatch({
                            # Clear existing rows
                            self$results$spatialAnalysis$setNote("", "note")
                            
                            # Add spatial parameter rows
                            self$results$spatialAnalysis$addRow(rowKey = 1, values = list(
                                parameter = "Spatial Autocorrelation Weight (α)",
                                value = self$options$spatialAlpha,
                                interpretation = if(self$options$spatialAlpha > 0.7) "High spatial clustering emphasis" 
                                               else if(self$options$spatialAlpha > 0.3) "Moderate spatial clustering"
                                               else "Low spatial clustering emphasis"
                            ))
                            
                            self$results$spatialAnalysis$addRow(rowKey = 2, values = list(
                                parameter = "Spatial Compactness Weight (β)",
                                value = self$options$spatialBeta,
                                interpretation = if(self$options$spatialBeta > 0.7) "High compactness preference"
                                               else if(self$options$spatialBeta > 0.3) "Moderate compactness preference"
                                               else "Low compactness preference"
                            ))
                            
                            # Add spatial interpretation
                            spatial_interpretation <- paste0(
                                "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                                "<h4 style='color: #1565C0; margin-top: 0;'>Spatial Autocart Analysis</h4>",
                                "<p><strong>Spatial Parameters:</strong> α=", self$options$spatialAlpha, 
                                ", β=", self$options$spatialBeta, "</p>",
                                "<p><strong>Clinical Relevance:</strong> Autocart considers spatial relationships ",
                                "between samples, which is valuable for:</p>",
                                "<ul>",
                                "<li>Tissue microarray analysis with coordinate information</li>",
                                "<li>Multi-center studies with geographic clustering</li>",
                                "<li>Spatial pathology where location matters for diagnosis</li>",
                                "<li>Epidemiological studies with spatial disease patterns</li>",
                                "</ul>",
                                "<p><strong>Interpretation:</strong> Higher α values emphasize spatial autocorrelation ",
                                "(similar outcomes in nearby locations), while higher β values favor spatially ",
                                "compact decision regions.</p>",
                                "</div>"
                            )
                            
                            self$results$spatialInterpretation$setContent(spatial_interpretation)
                            
                        }, error = function(e) {
                            warning(paste("Spatial analysis update failed:", e$message))
                        })
                    }
                ))
