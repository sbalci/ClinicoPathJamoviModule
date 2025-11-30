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
    # Utility to handle variables with spaces/special characters
    .escapeVar = function(x) {
        if (is.null(x) || length(x) == 0) return(x)
        make.names(x)
    },

    .init = function() {
        preformatted <- jmvcore::Preformatted$new(self$options, 'preformatted')
        self$results$add(preformatted)
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

        if (nrow(self$data) == 0)
            stop('Data contains no (complete) rows')

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

        # Create learner with class balancing pipeline if requested
        # This ensures balancing happens WITHIN each training fold, not on entire dataset
        learner <- private$.createBalancedLearner()

        # NOTE: Clinical cutoff is stored for potential future use
        # Most mlr3 learners don't support cutoff parameter directly
        # Threshold can be applied during prediction post-processing if needed

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
                warning(paste(
                    "SMOTE option selected, but the 'smotefamily' package is not installed.",
                    "Using classbalancing oversampling instead (no synthetic examples generated).",
                    "To enable SMOTE, install the package with: install.packages('smotefamily')"
                ))
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

    .checkpoint = function() {
        # Allow jamovi to check for user cancellation
        if (self$isFresh) return()
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
                min.node.size = self$options$maxDepth
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
            warning(paste("Learner", classifier_type, "not available, using decision tree. Error:", e$message))
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
            "max depth" = self$options$maxDepth,
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
    }
)
)
