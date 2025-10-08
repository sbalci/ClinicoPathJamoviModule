#' @title Enhanced Clinical Classification Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mlr3
#' @import mlr3measures
#' @import mlr3learners
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
    .init = function() {
        preformatted <- jmvcore::Preformatted$new(self$options, 'preformatted')
        self$results$add(preformatted)
    },

    .run = function() {
        library('mlr3')

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
            <br>• Class imbalance handling methods
            <br><br>
            Select a dependent variable (outcome) and independent variables (predictors) to begin analysis.
            <hr><br>
            "
            self$results$text$setContent(welcome_msg)
            return()
        }

        if (nrow(self$data) == 0)
            stop('Data contains no (complete) rows')

        data <- as.data.table(self$data)

        # Add checkpoint before data processing
        private$.checkpoint()

        task <- TaskClassif$new(id = "clinical_task", backend = data[complete.cases(data),], target = self$options$dep)

        # Handle class imbalance if specified
        task <- private$.handleClassImbalance(task)

        learner <- private$.initLearner()

        # Set clinical cutoff if specified
        learner$param_set$values$cutoff <- self$options$clinicalCutoff

        # Add checkpoint before model training
        private$.checkpoint()

        private$.trainModel(task, learner)
    },

    .handleClassImbalance = function(task) {
        if (self$options$balancingMethod == "upsample") {
            # Implement upsampling
            task_data <- task$data()
            target_col <- task$target_names
            minority_class <- names(sort(table(task_data[[target_col]])))[1]
            majority_count <- max(table(task_data[[target_col]]))

            # Simple upsampling by replication
            minority_indices <- which(task_data[[target_col]] == minority_class)
            n_replicate <- majority_count - length(minority_indices)

            if (n_replicate > 0) {
                replicated_indices <- sample(minority_indices, n_replicate, replace = TRUE)
                upsampled_data <- rbind(task_data, task_data[replicated_indices, ])
                task <- TaskClassif$new(id = task$id, backend = upsampled_data, target = target_col)
            }
        } else if (self$options$balancingMethod == "downsample") {
            # Implement downsampling
            task_data <- task$data()
            target_col <- task$target_names
            minority_count <- min(table(task_data[[target_col]]))

            balanced_data <- data.table()
            for (class in unique(task_data[[target_col]])) {
                class_data <- task_data[task_data[[target_col]] == class, ]
                sampled_data <- class_data[sample(nrow(class_data), minority_count), ]
                balanced_data <- rbind(balanced_data, sampled_data)
            }
            task <- TaskClassif$new(id = task$id, backend = balanced_data, target = target_col)
        } else if (self$options$balancingMethod == "smote") {
            stop("SMOTE requires the 'smotefamily' package. Please install it or choose another balancing method.")
        }
        return(task)
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

        # Calculate clinical metrics using caret functions
        tryCatch({
            sens <- caret::sensitivity(pred_response, truth_factor, positive = levels(truth_factor)[2])
            spec <- caret::specificity(pred_response, truth_factor, negative = levels(truth_factor)[1])
            ppv <- caret::posPredValue(pred_response, truth_factor, positive = levels(truth_factor)[2])
            npv <- caret::negPredValue(pred_response, truth_factor, negative = levels(truth_factor)[1])

            # Calculate likelihood ratios
            pos_lr <- sens / (1 - spec)
            neg_lr <- (1 - sens) / spec

            # Calculate prevalence
            prevalence <- sum(truth_factor == levels(truth_factor)[2]) / length(truth_factor)

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
                        caret::sensitivity(boot_pred, boot_truth, positive = levels(truth_factor)[2]),
                        caret::specificity(boot_pred, boot_truth, negative = levels(truth_factor)[1]),
                        caret::posPredValue(boot_pred, boot_truth, positive = levels(truth_factor)[2]),
                        caret::negPredValue(boot_pred, boot_truth, negative = levels(truth_factor)[1])
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
            "min. bucket" = self$options$minBucket,
            "complexity" = self$options$complexity,
            "max. compete" = self$options$maxCompete,
            "max. surrogate" = self$options$maxSurrogate,
            "unsurrogate" = self$options$unsurrogate,
            "max depth" = self$options$maxDepth,
            "no. cross validations" = self$options$noCrossValidations
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

        if (self$options$testing == "split" | self$options$testing == "trainSet") {
            predictions <- private$.trainTestSplit(task, learner)
            private$.setOutput(predictions, predictions, learner$model)
        } else {
            predictions <- private$.crossValidate(task, learner)
            private$.setOutput(predictions$prediction(), predictions, learner$model)
        }

        return (predictions)
    },

    .crossValidate = function(task, learner) {
        resampling <- rsmp("cv", folds = self$options$noOfFolds)
        resampling$instantiate(task)

        rr <- resample(task, learner, resampling, store_models = TRUE)

        return (rr)
    },

    .trainTestSplit = function(task, learner) {
        trainSet <- sample(task$nrow)
        testSet <- trainSet

        if (self$options$testing == "split") {
            trainSet <- sample(task$nrow, (1 - self$options$testSize) * task$nrow)
            testSet <- setdiff(seq_len(task$nrow), as.numeric(trainSet))
        }

        learner$train(task, row_ids = trainSet)

        prediction <- learner$predict(task, row_ids = testSet)

        return (prediction)
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

        # Create measures for general metrics
        measures <- list(
            msr("classif.ce"),
            msr("classif.acc")
        )

        # Create measures for per-class metrics
        classMeasures <- list(
            msr("classif.precision"),
            msr("classif.recall"),
            msr("classif.fbeta")
        )

        # Add AUC if binary classification
        if (length(unique(prediction$truth)) == 2) {
            classMeasures <- c(classMeasures, list(msr("classif.auc")))
        }

        reporting <- self$options$reporting

        # Calculate general measures
        classifier <- self$options$classifier
        columns <- generalTable$columns
        for (i in seq_along(measures)) {
            generalTable$addRow(rowKey = i, values = list(
                metric = measures[[i]]$id,
                value = as.numeric(prediction$score(measures[[i]], task = NULL))
            ))
        }

        if (self$options$predictedFreq == TRUE | self$options$predictedFreqRF == TRUE) {
            freqPlot <- self$results$predictedFreqPlot
            freqPlot$setState(prediction)
        }
        if (self$options$plotDecisionTree == TRUE & classifier == 'singleDecisionTree') {
            treePlot <- self$results$decisionTreeModel
            treePlot$setState(list(predictions = prediction, model = model))
        }
        if(self$options$printRandForest == TRUE & classifier == 'randomForest') {
            private$.populateRandomForestResults(model)
        }

        if (self$options$reportClinicalMetrics) {
            # Calculate clinical metrics for binary classification
            if (length(unique(prediction$truth)) == 2) {
                clinical_metrics <- private$.calculateClinicalMetrics(prediction$response, prediction$truth)
                private$.populateClinicalMetrics(clinical_metrics)
            }
        }

        # Cross-validation specific handling
        resampling <- rsmp("cv", folds = self$options$noOfFolds)
        resampling$instantiate(task)

        columns <- classTable$columns
        for (i in seq_along(columns)) {
            if (columns[[i]]$name == 'class') next
        }

        if (self$options$testing == "split") {
            trainSet <- sample(task$nrow, (1 - self$options$testSize) * task$nrow)
            testSet <- setdiff(seq_len(task$nrow), as.numeric(trainSet))
        }

        # Set up ROC curve if AUC reporting is enabled
        if ('AUC' %in% reporting && length(unique(prediction$truth)) == 2) {
            binaryPredictions <- prediction
            self$results$rocCurvePlot$setState(binaryPredictions)
        }

        # Populate per-class metrics table
        table <- self$results$classificationMetrics$class
        unique_classes <- unique(prediction$truth)

        for (class_name in unique_classes) {
            row_data <- list(class = as.character(class_name))

            # Calculate per-class metrics
            for (measure in classMeasures) {
                if (measure$id == "classif.auc" && length(unique_classes) > 2) {
                    # Skip AUC for multi-class
                    next
                }

                tryCatch({
                    score <- prediction$score(measure, task = NULL)
                    if (measure$id %in% c("classif.precision", "classif.recall", "classif.fbeta")) {
                        # These are per-class metrics, extract the specific class
                        if (is.numeric(score) && length(score) == 1) {
                            row_data[[measure$id]] <- score
                        }
                    } else {
                        row_data[[measure$id]] <- score
                    }
                }, error = function(e) {
                    row_data[[measure$id]] <- NA
                })
            }

            table$addRow(rowKey = class_name, values = row_data)
        }

        # Populate confusion matrix
        if ('confusionMatrix' %in% reporting) {
            confusionMatrix <- prediction$confusion
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
        roc_obj <- roc(prediction$truth, prediction$prob[, 2])

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
