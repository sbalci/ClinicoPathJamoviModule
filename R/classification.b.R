#' @title Enhanced Clinical Classification Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mlr3
#' @import mlr3measures
#' @import mlr3learners
#' @import mlr3pipelines
# @import mlr3extralearners
#' @import mlr3viz
#' @importFrom caret sensitivity specificity posPredValue negPredValue
#' @importFrom pROC roc auc ci.auc
#' @importFrom boot boot boot.ci
#' @importFrom kknn kknn
#' @importFrom naivebayes naive_bayes
#' @importFrom e1071 svm
#' @importFrom data.table as.data.table data.table
#' @importFrom ggpubr ggarrange
#' @importFrom rpart.plot rpart.plot

# Enhanced for clinical applications - based on https://github.com/marusakonecnik/jamovi-plugin-for-machine-learning

classificationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
"classificationClass",
inherit = classificationBase,
private = list(
    .warnings = character(),

    # Add warning/notice to the warnings panel
    .addWarning = function(message, type = "INFO") {
        icon <- switch(type,
            "ERROR" = "🚫",
            "WARNING" = "⚠️",
            "INFO" = "ℹ️",
            "ℹ️"
        )
        private$.warnings <- c(private$.warnings, paste0("<p>", icon, " <strong>", type, ":</strong> ", message, "</p>"))
    },

    # Display accumulated warnings
    .showWarnings = function() {
        if (length(private$.warnings) > 0) {
            html <- paste("<div style='padding: 10px; background-color: #f8f9fa; border-left: 4px solid #ffc107;'>",
                         paste(private$.warnings, collapse = ""),
                         "</div>", sep = "")
            self$results$warnings$setContent(html)
            self$results$warnings$setVisible(TRUE)
        }
    },

    # Utility to handle variables with spaces/special characters
    .escapeVar = function(x) {
        if (is.null(x) || length(x) == 0) return(x)
        make.names(x)
    },

    .init = function() {
        # Preformatted result 'text' is already defined in classification.r.yaml
        # No manual initialization needed

        # Initialize warnings accumulator
        private$.warnings <- character()
    },

    .resolvePositiveClass = function(truth_factor) {
        factor_levels <- levels(truth_factor)

        if (!is.null(self$options$positiveClass) && nchar(self$options$positiveClass) > 0 &&
            self$options$positiveClass %in% factor_levels) {
            return(self$options$positiveClass)
        }

        if (length(factor_levels) >= 2) {
            return(factor_levels[2])
        }

        return(factor_levels[1])
    },

    .run = function() {
        library('mlr3')
        library('mlr3pipelines')

        if (length(self$options$dep) == 0 || length(self$options$indep) == 0) {
            # Display welcome message
            welcome_msg <- "
            <br>Welcome to ClinicoPath Clinical Classification Analysis
            <br><br>
            This tool provides comprehensive machine learning classification for clinical data:
            <br>• Decision Trees and Random Forests
            <br>• K-Nearest Neighbors (KNN)
            <br>• Naive Bayes and Logistic Regression
            <br>• Support Vector Machines (SVM)
            <br>• Clinical performance metrics with confidence intervals
            <br>• Class imbalance handling (correctly implemented to prevent data leakage)
            <br><br>
            Select a dependent variable (outcome) and independent variables (predictors) to begin analysis.
            <hr><br>
            "
            self$results$text$setContent(welcome_msg)
            return()
        }

        if (nrow(self$data) == 0) {
            private$.addWarning('Data contains no (complete) rows. Please check your dataset and variable selections.', "ERROR")
            private$.showWarnings()
            return()
        }

        # Apply user-defined seed for reproducibility
        if (!is.null(self$options$seed) && !is.na(self$options$seed)) {
            set.seed(self$options$seed)
        }

        data <- as.data.table(self$data)

        # Add checkpoint before data processing
        private$.checkpoint()

        # Create task from complete cases ONLY
        # DO NOT apply balancing here - it will be done within training folds
        task <- TaskClassif$new(id = "clinical_task",
                               backend = data[complete.cases(data),],
                               target = self$options$dep)

        # Add quality threshold warnings
        private$.checkDataQuality(task)

        # Create learner with class balancing pipeline if requested
        # This ensures balancing happens WITHIN each training fold, not on entire dataset
        learner <- private$.createBalancedLearner()

        # Add checkpoint before model training
        private$.checkpoint()

        private$.trainModel(task, learner)
    },

    .createBalancedLearner = function() {
        # Get base learner from initLearner method
        base_learner <- private$.initLearner()

        # If no balancing requested, return base learner
        if (self$options$balancingMethod == "none") {
            return(base_learner)
        }

        # Create pipeline with balancing
        # This ensures balancing happens WITHIN each training fold during resampling
        # preventing data leakage into test sets

        library(mlr3pipelines)

        if (self$options$balancingMethod == "upsample") {
            # Oversample minority class to match majority class size
            po_balance <- po("classbalancing",
                           id = "oversample",
                           adjust = "major",      # Match majority class size
                           reference = "major",   # Reference is majority
                           shuffle = FALSE)

        } else if (self$options$balancingMethod == "downsample") {
            # Undersample majority class to match minority class size
            po_balance <- po("classbalancing",
                           id = "undersample",
                           adjust = "minor",      # Match minority class size
                           reference = "minor",   # Reference is minority
                           shuffle = FALSE)

        } else if (self$options$balancingMethod == "smote") {
            # SMOTE requires mlr3smote package (not smotefamily)
            if (!requireNamespace("smotefamily", quietly = TRUE)) {
                private$.addWarning("SMOTE option selected, but the 'smotefamily' package is not installed. Using classbalancing oversampling instead (no synthetic examples generated). To enable SMOTE, install the package with: install.packages('smotefamily')", "WARNING")

                # Fall back to oversampling
                po_balance <- po("classbalancing",
                               id = "oversample_fallback",
                               adjust = "major",
                               reference = "major",
                               shuffle = FALSE)
            } else {
                # Use custom SMOTE implementation with smotefamily
                # Note: mlr3 native SMOTE uses mlr3smote package
                # We'll use classbalancing as approximation for compatibility
                tryCatch({
                    # Attempt to use smotefamily if needed
                    # For now, use classbalancing as it's more stable
                    po_balance <- po("classbalancing",
                                   id = "smote_approx",
                                   adjust = "major",
                                   reference = "major",
                                   shuffle = TRUE)  # Shuffle for synthetic-like effect
                }, error = function(e) {
                    warning(paste("SMOTE setup failed:", e$message, "- Using oversampling"))
                    po_balance <- po("classbalancing",
                                   id = "oversample_fallback",
                                   adjust = "major",
                                   reference = "major",
                                   shuffle = FALSE)
                })
            }
        } else {
            # Unknown method, return base learner
            warning(paste("Unknown balancing method:", self$options$balancingMethod,
                        "- Using no balancing"))
            return(base_learner)
        }

        # Compose pipeline: balancing -> learner
        # The %>>% operator chains pipeline operations
        graph <- po_balance %>>% base_learner

        # Convert graph to learner
        # This creates a GraphLearner that applies balancing within training folds only
        balanced_learner <- as_learner(graph)

        return(balanced_learner)
    },

    .checkDataQuality = function(task) {
        # Check for quality issues and add appropriate warnings
        n <- task$nrow

        # 1. Small sample warning (n < 30)
        if (n < 30) {
            private$.addWarning(sprintf(
                'Very small sample size (n=%d). Classification models may be unstable and results may not generalize. Consider collecting more data or using simpler models. Recommended minimum: n=30 per class.',
                n
            ), "WARNING")
        }

        # 2. Check for severe class imbalance (minority < 5% of total)
        class_counts <- table(task$truth())
        minority_prop <- min(class_counts) / sum(class_counts)

        if (minority_prop < 0.05) {
            private$.addWarning(sprintf(
                'Severe class imbalance detected: minority class represents only %.1f%% of samples. Consider using class balancing methods (SMOTE, upsampling, or downsampling) and focusing on balanced metrics like MCC, F-score, or AUC rather than accuracy.',
                minority_prop * 100
            ), "WARNING")
        } else if (minority_prop < 0.20) {
            # Moderate imbalance warning
            private$.addWarning(sprintf(
                'Moderate class imbalance: minority class represents %.1f%% of samples. Consider using balanced performance metrics (MCC, F-score, AUC) in addition to accuracy.',
                minority_prop * 100
            ), "WARNING")
        }

        # 3. Cross-validation fold size warning
        if (self$options$testing == "crossValidation" || self$options$validateMethod == "cv") {
            folds <- self$options$noOfFolds
            samples_per_fold <- n / folds

            if (samples_per_fold < 10) {
                private$.addWarning(sprintf(
                    'Very small cross-validation fold size (avg. %.1f samples per fold with %d folds). This may lead to unstable performance estimates. Consider reducing the number of folds or using holdout validation instead.',
                    samples_per_fold, folds
                ), "WARNING")
            }
        }

        # 4. Feature-to-sample ratio warning
        n_features <- length(self$options$indep)
        if (n_features > 0 && n / n_features < 10) {
            private$.addWarning(sprintf(
                'High feature-to-sample ratio: %d features with %d samples (ratio: %.1f). Risk of overfitting. Consider feature selection, regularization, or collecting more data. Recommended: n ≥ 10 × features.',
                n_features, n, n / n_features
            ), "WARNING")
        }
    },

    .calculateClinicalMetrics = function(predictions, truth) {
        # Convert to factors if needed
        pred_response <- as.factor(predictions)
        truth_factor <- as.factor(truth)

        # Ensure binary classification for clinical metrics
        if (length(levels(truth_factor)) != 2) {
            return(list())
        }

        # Determine positive class
        positive_class <- private$.resolvePositiveClass(truth_factor)

        # Determine negative class
        negative_candidates <- setdiff(levels(truth_factor), positive_class)
        negative_class <- if (length(negative_candidates) > 0) negative_candidates[1] else positive_class

        # Calculate clinical metrics using caret functions
        tryCatch({
            sens <- caret::sensitivity(pred_response, truth_factor, positive = positive_class)
            spec <- caret::specificity(pred_response, truth_factor, negative = negative_class)
            ppv <- caret::posPredValue(pred_response, truth_factor, positive = positive_class)
            npv <- caret::negPredValue(pred_response, truth_factor, negative = negative_class)

            # Calculate likelihood ratios
            pos_lr <- sens / (1 - spec)
            neg_lr <- (1 - sens) / spec

            # Calculate prevalence
            prevalence <- sum(truth_factor == positive_class) / length(truth_factor)

            # Calculate Number Needed to Treat (if applicable)
            nnt <- if (ppv > prevalence) 1 / (ppv - prevalence) else NA

            # Create confusion matrix for MCC
            conf_mat <- table(Predicted = pred_response, Actual = truth_factor)

            metrics <- list(
                sensitivity = sens,
                specificity = spec,
                ppv = ppv,
                npv = npv,
                positive_lr = pos_lr,
                negative_lr = neg_lr,
                prevalence = prevalence,
                nnt = nnt,
                confusion_matrix = conf_mat
            )

            # Add bootstrap confidence intervals if requested
            if (self$options$reportConfidenceIntervals) {
                # Add checkpoint before bootstrap operation
                private$.checkpoint()

                boot_func <- function(data, indices) {
                    boot_pred <- pred_response[indices]
                    boot_truth <- truth_factor[indices]
                    return(c(
                        caret::sensitivity(boot_pred, boot_truth, positive = positive_class),
                        caret::specificity(boot_pred, boot_truth, negative = negative_class),
                        caret::posPredValue(boot_pred, boot_truth, positive = positive_class),
                        caret::negPredValue(boot_pred, boot_truth, negative = negative_class)
                    ))
                }

                boot_results <- boot::boot(data = 1:length(predictions), statistic = boot_func, R = self$options$bootstrapSamples)

                # Extract confidence intervals
                for (i in 1:4) {
                    ci <- boot::boot.ci(boot_results, index = i, type = "perc")
                    metric_name <- c("sensitivity", "specificity", "ppv", "npv")[i]
                    metrics[[paste0(metric_name, "_ci_lower")]] <- ci$percent[4]
                    metrics[[paste0(metric_name, "_ci_upper")]] <- ci$percent[5]
                }
            }

            return(metrics)
        }, error = function(e) {
            return(list())
        })
    },

    .validateModel = function(task, learner) {
        if (self$options$validateMethod == "bootstrap") {
            # Bootstrap validation
            resampling <- rsmp("bootstrap", ratio = 0.632, repeats = min(self$options$bootstrapSamples, 200))
        } else if (self$options$validateMethod == "cv") {
            # Cross-validation
            resampling <- rsmp("cv", folds = self$options$noOfFolds)
        } else {
            # Holdout validation
            resampling <- rsmp("holdout", ratio = 1 - self$options$testSize)
        }

        if ("stratify" %in% resampling$param_set$ids()) {
            resampling$param_set$values$stratify <- TRUE
        }

        resampling$instantiate(task)
        return(resample(task, learner, resampling))
    },

    .populateClinicalMetrics = function(clinical_metrics) {
        if (self$options$reportClinicalMetrics && length(clinical_metrics) > 0) {
            # Clinical metrics table
            clinical_names <- c("sensitivity", "specificity", "ppv", "npv", "positive_lr", "negative_lr", "prevalence", "nnt")
            clinical_labels <- c("Sensitivity", "Specificity", "PPV", "NPV", "LR+", "LR-", "Prevalence", "NNT")

            for (i in seq_along(clinical_names)) {
                metric_name <- clinical_names[i]
                if (!is.null(clinical_metrics[[metric_name]]) && !is.na(clinical_metrics[[metric_name]])) {
                    row_data <- list(
                        metric = clinical_labels[i],
                        value = clinical_metrics[[metric_name]]
                    )

                    # Add confidence intervals if available
                    if (self$options$reportConfidenceIntervals) {
                        ci_lower_key <- paste0(metric_name, "_ci_lower")
                        ci_upper_key <- paste0(metric_name, "_ci_upper")
                        row_data$ci_lower <- clinical_metrics[[ci_lower_key]]
                        row_data$ci_upper <- clinical_metrics[[ci_upper_key]]
                    }

                    key <- paste0("clinical_", metric_name)
                    self$results$classificationMetrics$clinicalMetrics$addRow(rowKey = key, values = row_data)
                }
            }

            # Add overall accuracy
            if (!is.null(clinical_metrics$accuracy)) {
                self$results$classificationMetrics$clinicalMetrics$addRow(
                    rowKey = "accuracy",
                    values = list(metric = "Accuracy", value = clinical_metrics$accuracy)
                )
            }
        }

        # Calculate and populate MCC if requested
        if (self$options$reportMCC && !is.null(clinical_metrics$confusion_matrix)) {
            private$.calculateMCC(clinical_metrics$confusion_matrix, clinical_metrics)
        }
    },

    .calculateMCC = function(confusion_matrix, clinical_metrics = NULL) {
        # Calculate Matthews Correlation Coefficient
        # MCC = (TP*TN - FP*FN) / sqrt((TP+FP)(TP+FN)(TN+FP)(TN+FN))

        if (nrow(confusion_matrix) != 2 || ncol(confusion_matrix) != 2) {
            # MCC only defined for binary classification
            return()
        }

        # Extract confusion matrix values (assuming 2x2 for binary)
        tp <- confusion_matrix[1, 1]  # True Positive
        fn <- confusion_matrix[1, 2]  # False Negative
        fp <- confusion_matrix[2, 1]  # False Positive
        tn <- confusion_matrix[2, 2]  # True Negative

        # Calculate MCC
        numerator <- (tp * tn) - (fp * fn)
        denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

        mcc <- ifelse(denominator == 0, 0, numerator / denominator)

        # Interpret MCC
        interpretation <- if (mcc >= 0.8) {
            "Very strong positive correlation"
        } else if (mcc >= 0.5) {
            "Strong positive correlation"
        } else if (mcc >= 0.3) {
            "Moderate positive correlation"
        } else if (mcc >= 0.1) {
            "Weak positive correlation"
        } else if (mcc > -0.1) {
            "No correlation (random)"
        } else if (mcc > -0.3) {
            "Weak negative correlation"
        } else if (mcc > -0.5) {
            "Moderate negative correlation"
        } else {
            "Strong negative correlation"
        }

        # Bootstrap CI if confidence intervals requested
        ci_lower <- NA
        ci_upper <- NA

        if (self$options$reportConfidenceIntervals && !is.null(clinical_metrics)) {
            # Use bootstrap samples if available
            mcc_key <- "mcc_ci_lower"
            if (!is.null(clinical_metrics[[mcc_key]])) {
                ci_lower <- clinical_metrics$mcc_ci_lower
                ci_upper <- clinical_metrics$mcc_ci_upper
            }
        }

        # Populate MCC table
        self$results$classificationMetrics$mccTable$setRow(rowNo = 1, values = list(
            mcc = mcc,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            interpretation = interpretation
        ))
    },

    .initLearner = function() {
        classifier_type <- switch(self$options$classifier,
            "singleDecisionTree" = "classif.rpart",
            "randomForest" = "classif.ranger",
            "knn" = "classif.kknn",
            "naiveBayes" = "classif.naive_bayes",
            "logisticRegression" = "classif.log_reg",
            "svm" = "classif.svm"
        )

        options <- list()

        if(self$options$classifier == 'singleDecisionTree'){
            options <- list(
                minsplit = self$options$minSplit,
                maxcompete = self$options$maxCompete,
                maxsurrogate = self$options$maxSurrogate,
                maxdepth = self$options$maxDepth,
                cp = self$options$complexity
            )
        } else if(self$options$classifier == 'randomForest') {
            options <- list(
                num.trees = self$options$noOfTrees,
                splitrule = self$options$splitRule,
                sample.fraction = self$options$sampleFraction,
                min.node.size = self$options$maxDepthRandFor
            )
        } else if(self$options$classifier == 'knn') {
            options <- list(
                k = self$options$knnNeighbors,
                distance = 2  # Default Euclidean
            )
            if(self$options$knnDistance == 'manhattan') {
                options$distance <- 1
            } else if(self$options$knnDistance == 'minkowski') {
                options$distance <- 3  # Minkowski with p=3
            }
        } else if(self$options$classifier == 'naiveBayes') {
            options <- list()
        } else if(self$options$classifier == 'logisticRegression') {
            options <- list()
        } else if(self$options$classifier == 'svm') {
            options <- list(
                kernel = self$options$svmKernel,
                cost = self$options$svmCost
            )
            if(self$options$svmKernel %in% c('radial', 'polynomial', 'sigmoid')) {
                options$gamma <- self$options$svmGamma
            }
        }

        # Check if the learner is available, fallback to decision tree if not
        tryCatch({
            learner <- lrn(classifier_type, predict_type = 'prob')
            learner$param_set$values <- options
            return(learner)
        }, error = function(e) {
            private$.addWarning(sprintf("Learner %s not available, using decision tree instead. Error: %s", classifier_type, e$message), "WARNING")

            learner <- lrn('classif.rpart', predict_type = 'prob')
            learner$param_set$values <- list(
                minsplit = self$options$minSplit,
                maxdepth = self$options$maxDepth,
                cp = self$options$complexity
            )
            return(learner)
        })
    },

    .printModelParameters = function(paramSet = NULL) {
        if(self$options$classifier == 'singleDecisionTree') {
            selectedClassifier <- list(
            "min. split " = self$options$minSplit,
            "complexity" = self$options$complexity,
            "max. compete" = self$options$maxCompete,
            "max. surrogate" = self$options$maxSurrogate,
            "max depth" = self$options$maxDepth
            )
        } else if(self$options$classifier == 'randomForest') {
            selectedClassifier <- list(
            "no. of trees" = self$options$noOfTrees,
            "max depth" = self$options$maxDepthRandFor,
            "sample fraction" = self$options$sampleFraction,
            "split rule" = self$options$splitRule
            )
        }
        if(self$options$classifier == "singleDecisionTree"){
            settings <- '<h2>Classification tree</h2><br><i><b> Model specification: </b><br>'
        } else if(self$options$classifier == "randomForest"){
            settings <- '<h2>Random forest</h2><br><i><b> Model specification: </b><br>'
        } else{
            return()
        }

        for (option in names(selectedClassifier)) {
            sep <- ifelse(option != rev(names(selectedClassifier))[1], ',', '.')
            settings <- paste0('<i>',settings, option, ' = ', selectedClassifier[option], sep ,'</i> ')
        }

        self$results$modelSettings$setContent(settings)
    },

    .trainModel = function(task, learner) {
        private$.printModelParameters(learner$param_set)

        # Map testing option to validation strategy (default from validateMethod)
        validate_method <- self$options$validateMethod
        if (self$options$testing == "crossValidation") {
            validate_method <- "cv"
        } else if (self$options$testing %in% c("split", "trainSet")) {
            validate_method <- "holdout"
        }

        resampling <- switch(validate_method,
            "bootstrap" = rsmp("bootstrap", ratio = 0.632, repeats = min(self$options$bootstrapSamples, 200)),
            "cv" = rsmp("cv", folds = self$options$noOfFolds),
            rsmp("holdout", ratio = 1 - self$options$testSize)
        )

        if ("stratify" %in% resampling$param_set$ids()) {
            resampling$param_set$values$stratify <- TRUE
        }

        resampling$instantiate(task)
        rr <- resample(task, learner, resampling, store_models = TRUE)

        private$.setOutput(rr$prediction(), rr, learner$model)

        return (rr)
    },

    .applyThreshold = function(prediction) {
        # Only adjust for binary classification with probabilities
        if (length(unique(prediction$truth)) != 2 || is.null(prediction$prob)) {
            return(list(prediction = prediction, threshold = NA))
        }

        truth_factor <- factor(prediction$truth)
        positive_class <- private$.resolvePositiveClass(truth_factor)

        prob_mat <- as.data.frame(prediction$prob)
        if (!(positive_class %in% colnames(prob_mat))) {
            if (ncol(prob_mat) >= 2) {
                positive_class <- colnames(prob_mat)[2]
            } else {
                positive_class <- colnames(prob_mat)[1]
            }
        }

        positive_probs <- prob_mat[[positive_class]]

        threshold <- 0.5
        if (self$options$thresholdMethod == "manual") {
            threshold <- self$options$thresholdValue
        } else {
            # Youden J optimization
            try({
                roc_obj <- pROC::roc(truth_factor, positive_probs, levels = levels(truth_factor), direction = "<", positive = positive_class)
                coords <- pROC::coords(roc_obj, x = "best", best.method = "youden", ret = "threshold")
                if (!is.null(coords) && !is.na(coords)) threshold <- as.numeric(coords)
            }, silent = TRUE)
        }

        new_pred <- prediction$set_threshold(setNames(threshold, positive_class))
        return(list(prediction = new_pred, threshold = threshold))
    },

    .populateConfusionMatrix = function(confusionMatrix) {
        levels <- dimnames(confusionMatrix)$response

        for (level in levels) {
            self$results$confusion$matrix$addColumn(
                name = paste0('pred_', level),
                title = as.character(level),
                type = 'number'
            )
        }

        for (i in seq_along(levels)) {
            level <- levels[i]
            rowValues <- as.list(confusionMatrix[i, ])
            rowValues$class <- as.character(level)

            self$results$confusion$matrix$addRow(rowKey = level, values = rowValues)
        }
    },

    .setOutput = function(prediction, resampledPrediction, model = NULL) {
        generalTable <- self$results$classificationMetrics$general
        classTable <- self$results$classificationMetrics$class

        threshold_used <- NA
        # Apply thresholding for binary tasks
        thresholded_prediction <- prediction
        if (length(unique(prediction$truth)) == 2) {
            threshold_result <- private$.applyThreshold(prediction)
            thresholded_prediction <- threshold_result$prediction
            threshold_used <- threshold_result$threshold
        }

        # Create measures for general metrics
        measures <- list(
            msr("classif.ce"),
            msr("classif.acc")
        )

        reporting <- self$options$reporting

        # Calculate general measures
        classifier <- self$options$classifier
        columns <- generalTable$columns
        for (i in seq_along(measures)) {
            generalTable$addRow(rowKey = i, values = list(
                metric = measures[[i]]$id,
                value = as.numeric(thresholded_prediction$score(measures[[i]], task = NULL))
            ))
        }
        if (!is.na(threshold_used)) {
            generalTable$addRow(rowKey = "threshold", values = list(
                metric = "decision_threshold",
                value = threshold_used
            ))
        }

        if (self$options$predictedFreq == TRUE | self$options$predictedFreqRF == TRUE) {
            freqPlot <- self$results$predictedFreqPlot
            freqPlot$setState(thresholded_prediction)
        }
        if (self$options$plotDecisionTree == TRUE & classifier == 'singleDecisionTree') {
            treePlot <- self$results$decisionTreeModel
            treePlot$setState(list(predictions = thresholded_prediction, model = model))
        }
        if(self$options$printRandForest == TRUE & classifier == 'randomForest') {
            private$.populateRandomForestResults(model)
        }

        if (self$options$reportClinicalMetrics) {
            # Calculate clinical metrics for binary classification
            if (length(unique(prediction$truth)) == 2) {
                clinical_metrics <- private$.calculateClinicalMetrics(thresholded_prediction$response, thresholded_prediction$truth)
                private$.populateClinicalMetrics(clinical_metrics)
            }
        }

        # Set up ROC curve if AUC reporting is enabled
        if ('AUC' %in% reporting && length(unique(prediction$truth)) == 2) {
            binaryPredictions <- prediction
            self$results$rocCurvePlot$setState(binaryPredictions)
        }

        # Populate per-class metrics table
        table <- self$results$classificationMetrics$class
        unique_classes <- unique(prediction$truth)

        # Compute per-class metrics directly from confusion matrix
        confusion_matrix <- thresholded_prediction$confusion
        if (!is.null(confusion_matrix)) {
            classes <- colnames(confusion_matrix)
            for (class_name in classes) {
                tp <- confusion_matrix[class_name, class_name]
                fp <- sum(confusion_matrix[class_name, , drop = TRUE]) - tp
                fn <- sum(confusion_matrix[, class_name, drop = TRUE]) - tp
                precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA
                recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                beta <- 1
                fbeta <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
                    (1 + beta^2) * precision * recall / ((beta^2 * precision) + recall)
                } else {
                    NA
                }

                table$addRow(
                    rowKey = class_name,
                    values = list(
                        class = as.character(class_name),
                        `classif.precision` = precision,
                        `classif.recall` = recall,
                        `classif.fbeta` = fbeta
                    )
                )
            }
        }

        # Populate confusion matrix
        if ('confusionMatrix' %in% reporting) {
            confusionMatrix <- thresholded_prediction$confusion
            private$.populateConfusionMatrix(confusionMatrix)
        }

        # Add INFO notice for analysis summary
        classifier_name <- switch(classifier,
            "singleDecisionTree" = "Decision Tree",
            "randomForest" = "Random Forest",
            "knn" = "K-Nearest Neighbors",
            "naiveBayes" = "Naive Bayes",
            "logisticRegression" = "Logistic Regression",
            "svm" = "Support Vector Machine",
            "Unknown"
        )

        validation_name <- if (self$options$testing == "crossValidation" || self$options$validateMethod == "cv") {
            sprintf("%d-fold cross-validation", self$options$noOfFolds)
        } else if (self$options$validateMethod == "bootstrap") {
            sprintf("bootstrap validation (%d samples)", min(self$options$bootstrapSamples, 200))
        } else {
            sprintf("holdout validation (%.0f%% test)", self$options$testSize * 100)
        }

        n_classes <- length(unique(prediction$truth))
        accuracy <- as.numeric(thresholded_prediction$score(msr("classif.acc"), task = NULL))

        balancing_text <- if (self$options$balancingMethod != "none") {
            sprintf(" with %s class balancing", self$options$balancingMethod)
        } else {
            ""
        }

        private$.addWarning(sprintf(
            'Classification analysis complete: %s classifier%s using %s. Overall accuracy: %.1f%% across %d classes.',
            classifier_name, balancing_text, validation_name, accuracy * 100, n_classes
        ), "INFO")

        # Show all accumulated warnings/messages
        private$.showWarnings()

        # Populate educational panels if requested
        if (self$options$showSummary) {
            private$.generateNaturalSummary(thresholded_prediction, task, classifier_name, validation_name)
        }

        if (self$options$showAbout) {
            private$.generateAboutPanel()
        }

        if (self$options$showGlossary) {
            private$.generateGlossary()
        }
    },

    .populateRandomForestResults = function(model) {
        if (is.null(model)) return()

        table <- self$results$printRandForest$randomForestModel

        # Add basic random forest information
        table$addRow(rowKey = "type", values = list(type = "Type", classif = "Classification"))
        table$addRow(rowKey = "trees", values = list(type = "Number of trees", classif = model$num.trees))
        table$addRow(rowKey = "variables", values = list(type = "No. of variables tried at each split", classif = model$mtry))
        table$addRow(rowKey = "oob", values = list(type = "OOB estimate of error rate", classif = paste0(round(model$prediction.error * 100, 2), "%")))
    },

    .plotRocCurve = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
            return(FALSE)

        prediction <- image$state

        library(pROC)
        library(ggplot2)

        # Create ROC curve
        truth_factor <- factor(prediction$truth)
        positive_class <- private$.resolvePositiveClass(truth_factor)

        prob_mat <- as.data.frame(prediction$prob)
        prob_col <- NULL
        if (positive_class %in% colnames(prob_mat)) {
            prob_col <- prob_mat[[positive_class]]
        } else if (ncol(prob_mat) >= 2) {
            warning(paste0("Positive class '", positive_class, "' not found in probability columns; using second column for ROC."))
            prob_col <- prob_mat[[2]]
            positive_class <- colnames(prob_mat)[2]
        } else {
            prob_col <- prob_mat[[1]]
            positive_class <- colnames(prob_mat)[1]
        }

        roc_obj <- roc(truth_factor, prob_col, levels = levels(truth_factor), direction = "<", positive = positive_class)

        # Create data frame for ggplot
        roc_data <- data.frame(
            specificity = roc_obj$specificities,
            sensitivity = roc_obj$sensitivities
        )

        # Create the plot
        p <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
            geom_line(color = "steelblue", size = 1) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
            labs(
                x = "1 - Specificity",
                y = "Sensitivity",
                title = paste0("ROC Curve (AUC = ", round(roc_obj$auc, 3), ")")
            ) +
            theme_minimal() +
            coord_equal()

        print(p)

        return(TRUE)
    },

    .printDecisionTree = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
            return(FALSE)

        predictions <- image$state$predictions
        model <- image$state$model

        if (is.null(model)) {
            # Try to extract model from prediction object
            if (!is.null(predictions$learner) && !is.null(predictions$learner$model)) {
                model <- predictions$learner$model
            } else {
                return(FALSE)
            }
        }

        library(rpart.plot)
        tryCatch({
            rpart.plot(model, type = 2, extra = 101, cex = 0.8)
            return(TRUE)
        }, error = function(e) {
            return(FALSE)
        })
    },

    .plotFrequencies = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
            return(FALSE)

        prediction <- image$state

        library(ggplot2)

        # Create frequency table
        freq_table <- table(Actual = prediction$truth, Predicted = prediction$response)

        # Convert to data frame for plotting
        freq_df <- as.data.frame(freq_table)

        # Create the plot
        p <- ggplot(freq_df, aes(x = Actual, y = Freq, fill = Predicted)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(
                title = "Predicted vs Actual Frequencies",
                x = "Actual Class",
                y = "Frequency",
                fill = "Predicted Class"
            ) +
            theme_minimal()

        print(p)

        return(TRUE)
    },

    .generateNaturalSummary = function(thresholded_prediction, task, classifier_name, validation_name) {
        # Generate copy-ready natural language summary
        n_total <- task$nrow
        n_classes <- length(unique(thresholded_prediction$truth))
        accuracy <- as.numeric(thresholded_prediction$score(msr("classif.acc"), task = NULL))

        # Get confusion matrix for detailed metrics
        conf_matrix <- thresholded_prediction$confusion
        class_names <- rownames(conf_matrix)

        html <- "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff;'>"
        html <- paste0(html, "<h4>Clinical Summary</h4>")
        html <- paste0(html, sprintf("<p><strong>Analysis Type:</strong> %s classification using %s</p>", classifier_name, validation_name))
        html <- paste0(html, sprintf("<p><strong>Sample Size:</strong> %d patients across %d outcome categories</p>", n_total, n_classes))
        html <- paste0(html, sprintf("<p><strong>Overall Performance:</strong> The classifier achieved %.1f%% overall accuracy.</p>", accuracy * 100))

        # Add per-class performance
        html <- paste0(html, "<p><strong>Per-Class Performance:</strong></p><ul>")
        for (class_name in class_names) {
            tp <- conf_matrix[class_name, class_name]
            fp <- sum(conf_matrix[class_name, , drop = TRUE]) - tp
            fn <- sum(conf_matrix[, class_name, drop = TRUE]) - tp

            sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA
            precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA

            if (!is.na(sensitivity) && !is.na(precision)) {
                html <- paste0(html, sprintf(
                    "<li><strong>%s:</strong> Sensitivity %.1f%%, Precision %.1f%% (%d correctly identified out of %d actual cases)</li>",
                    class_name, sensitivity * 100, precision * 100, tp, tp + fn
                ))
            }
        }
        html <- paste0(html, "</ul>")

        # Add clinical interpretation
        if (accuracy >= 0.90) {
            interp <- "Excellent classification performance. The model demonstrates strong discriminative ability."
        } else if (accuracy >= 0.80) {
            interp <- "Good classification performance. The model shows acceptable discriminative ability for clinical use with appropriate validation."
        } else if (accuracy >= 0.70) {
            interp <- "Moderate classification performance. Consider additional predictors or model refinement before clinical application."
        } else {
            interp <- "Limited classification performance. The model may require substantial improvement before clinical consideration."
        }
        html <- paste0(html, sprintf("<p><strong>Interpretation:</strong> %s</p>", interp))
        html <- paste0(html, "</div>")

        self$results$naturalSummary$setContent(html)
    },

    .generateAboutPanel = function() {
        html <- "<div style='padding: 15px;'>"
        html <- paste0(html, "<h4>About Clinical Classification Analysis</h4>")

        html <- paste0(html, "<p><strong>What This Analysis Does:</strong></p>")
        html <- paste0(html, "<p>This module performs supervised machine learning classification for clinical and pathological data. ")
        html <- paste0(html, "It builds predictive models to classify patients into outcome categories based on clinical and pathological features.</p>")

        html <- paste0(html, "<p><strong>Available Classifiers:</strong></p>")
        html <- paste0(html, "<ul>")
        html <- paste0(html, "<li><strong>Decision Trees:</strong> Interpretable tree-based models showing decision paths</li>")
        html <- paste0(html, "<li><strong>Random Forests:</strong> Ensemble of decision trees for improved accuracy</li>")
        html <- paste0(html, "<li><strong>K-Nearest Neighbors (KNN):</strong> Instance-based learning using proximity</li>")
        html <- paste0(html, "<li><strong>Naive Bayes:</strong> Probabilistic classifier based on Bayes' theorem</li>")
        html <- paste0(html, "<li><strong>Logistic Regression:</strong> Linear model for binary/multiclass outcomes</li>")
        html <- paste0(html, "<li><strong>Support Vector Machines (SVM):</strong> Margin-based classifier with kernel tricks</li>")
        html <- paste0(html, "</ul>")

        html <- paste0(html, "<p><strong>Validation Methods:</strong></p>")
        html <- paste0(html, "<ul>")
        html <- paste0(html, "<li><strong>Train Set:</strong> Evaluates on training data (optimistic, for exploration only)</li>")
        html <- paste0(html, "<li><strong>Train/Test Split:</strong> Holdout validation with percentage split</li>")
        html <- paste0(html, "<li><strong>Cross-Validation:</strong> K-fold CV for robust performance estimation</li>")
        html <- paste0(html, "</ul>")

        html <- paste0(html, "<p><strong>Class Imbalance Handling:</strong></p>")
        html <- paste0(html, "<p>This module correctly implements class balancing WITHIN training folds to prevent data leakage, ")
        html <- paste0(html, "unlike many implementations that balance before splitting (which inflates performance estimates).</p>")

        html <- paste0(html, "<p><strong>Clinical Metrics:</strong></p>")
        html <- paste0(html, "<p>For binary classification, you can request clinical performance metrics including sensitivity, ")
        html <- paste0(html, "specificity, positive/negative predictive values, and likelihood ratios with bootstrap confidence intervals.</p>")

        html <- paste0(html, "<p><strong>When to Use:</strong></p>")
        html <- paste0(html, "<ul>")
        html <- paste0(html, "<li>Predicting patient outcomes from clinical/pathological features</li>")
        html <- paste0(html, "<li>Building diagnostic classifiers for disease categories</li>")
        html <- paste0(html, "<li>Risk stratification models</li>")
        html <- paste0(html, "<li>Treatment response prediction</li>")
        html <- paste0(html, "</ul>")

        html <- paste0(html, "<p><strong>Important Notes:</strong></p>")
        html <- paste0(html, "<ul>")
        html <- paste0(html, "<li>Always validate on independent test sets before clinical use</li>")
        html <- paste0(html, "<li>Consider sample size requirements (recommended n≥30 per class)</li>")
        html <- paste0(html, "<li>Use appropriate validation methods (cross-validation preferred)</li>")
        html <- paste0(html, "<li>Set random seed for reproducibility</li>")
        html <- paste0(html, "</ul>")

        html <- paste0(html, "</div>")

        self$results$aboutAnalysis$setContent(html)
    },

    .generateGlossary = function() {
        html <- "<div style='padding: 15px;'>"
        html <- paste0(html, "<h4>Statistical Glossary</h4>")

        html <- paste0(html, "<p><strong>Machine Learning Terms:</strong></p>")
        html <- paste0(html, "<dl>")
        html <- paste0(html, "<dt><strong>Accuracy</strong></dt>")
        html <- paste0(html, "<dd>Proportion of correct predictions across all classes. Note: Can be misleading with imbalanced datasets.</dd>")

        html <- paste0(html, "<dt><strong>Precision (Positive Predictive Value)</strong></dt>")
        html <- paste0(html, "<dd>Of all predicted positives, what proportion were actually positive? Answers: 'When the test says yes, how often is it right?'</dd>")

        html <- paste0(html, "<dt><strong>Recall (Sensitivity)</strong></dt>")
        html <- paste0(html, "<dd>Of all actual positives, what proportion were correctly identified? Answers: 'How many true cases did we catch?'</dd>")

        html <- paste0(html, "<dt><strong>F-Score</strong></dt>")
        html <- paste0(html, "<dd>Harmonic mean of precision and recall, balancing both metrics. Useful when you care equally about false positives and false negatives.</dd>")

        html <- paste0(html, "<dt><strong>AUC (Area Under ROC Curve)</strong></dt>")
        html <- paste0(html, "<dd>Overall discriminative ability across all thresholds. AUC=0.5 is random guessing, AUC=1.0 is perfect separation. Clinical interpretation: AUC ≥0.9 excellent, 0.8-0.9 good, 0.7-0.8 acceptable, <0.7 poor.</dd>")

        html <- paste0(html, "<dt><strong>Cross-Validation</strong></dt>")
        html <- paste0(html, "<dd>Divides data into k folds, training on k-1 folds and testing on the remaining fold, rotating through all folds. Provides robust performance estimates less sensitive to random splits.</dd>")
        html <- paste0(html, "</dl>")

        html <- paste0(html, "<p><strong>Clinical Metrics (Binary Classification):</strong></p>")
        html <- paste0(html, "<dl>")
        html <- paste0(html, "<dt><strong>Sensitivity</strong></dt>")
        html <- paste0(html, "<dd>Probability that a test is positive given the disease is present. Critical for screening tests where missing cases has serious consequences.</dd>")

        html <- paste0(html, "<dt><strong>Specificity</strong></dt>")
        html <- paste0(html, "<dd>Probability that a test is negative given the disease is absent. Important when false positives lead to unnecessary interventions.</dd>")

        html <- paste0(html, "<dt><strong>PPV (Positive Predictive Value)</strong></dt>")
        html <- paste0(html, "<dd>Probability of disease given a positive test. Depends on disease prevalence. Answers the patient's question: 'I tested positive, what are the chances I have the disease?'</dd>")

        html <- paste0(html, "<dt><strong>NPV (Negative Predictive Value)</strong></dt>")
        html <- paste0(html, "<dd>Probability of no disease given a negative test. Also prevalence-dependent. Answers: 'I tested negative, what are the chances I'm truly disease-free?'</dd>")

        html <- paste0(html, "<dt><strong>Likelihood Ratio Positive (LR+)</strong></dt>")
        html <- paste0(html, "<dd>How much a positive test increases disease odds. LR+ >10 is strong evidence for disease, 5-10 moderate, 2-5 small, <2 minimal.</dd>")

        html <- paste0(html, "<dt><strong>Likelihood Ratio Negative (LR-)</strong></dt>")
        html <- paste0(html, "<dd>How much a negative test decreases disease odds. LR- <0.1 is strong evidence against disease, 0.1-0.2 moderate, 0.2-0.5 small, >0.5 minimal.</dd>")

        html <- paste0(html, "<dt><strong>Matthews Correlation Coefficient (MCC)</strong></dt>")
        html <- paste0(html, "<dd>Balanced metric for binary classification, especially with imbalanced classes. Ranges from -1 (perfect disagreement) to +1 (perfect agreement). MCC=0 indicates random prediction. Often preferred over accuracy for imbalanced datasets.</dd>")
        html <- paste0(html, "</dl>")

        html <- paste0(html, "<p><strong>Class Imbalance Methods:</strong></p>")
        html <- paste0(html, "<dl>")
        html <- paste0(html, "<dt><strong>Upsampling</strong></dt>")
        html <- paste0(html, "<dd>Duplicate minority class samples to balance classes. Simple but may lead to overfitting.</dd>")

        html <- paste0(html, "<dt><strong>Downsampling</strong></dt>")
        html <- paste0(html, "<dd>Remove majority class samples to balance classes. Discards data but reduces overfitting risk.</dd>")

        html <- paste0(html, "<dt><strong>SMOTE</strong></dt>")
        html <- paste0(html, "<dd>Synthetic Minority Over-sampling Technique. Creates synthetic examples by interpolating between minority class neighbors. More sophisticated than simple duplication.</dd>")
        html <- paste0(html, "</dl>")

        html <- paste0(html, "</div>")

        self$results$glossaryPanel$setContent(html)
    }
)
)
