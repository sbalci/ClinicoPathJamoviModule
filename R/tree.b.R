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
                        varsName <- self$options$vars
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

                        varsName <- self$options$vars
                        facsName <- self$options$facs
                        targetName <- self$options$target
                        trainName <- self$options$train

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
                        varsName <- self$options$vars
                        facsName <- self$options$facs
                        targetName <- self$options$target

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
                        targetName <- self$options$target

                        # Prepare formula
                        varsFormula <- paste(c(self$options$vars, self$options$facs), collapse = " + ")
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

                        # Store plot data
                        plotData <- list(
                            model = mytree.fft,
                            testData = testData,
                            trainData = trainData
                        )

                        image <- self$results$plot
                        image$setState(plotData)
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
                    }
                ))
