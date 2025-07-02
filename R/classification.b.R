#' @title Enhanced Clinical Classification Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mlr3
#' @import mlr3measures  
#' @import mlr3learners
#' @import mlr3extralearners
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
        }
        # Note: SMOTE would require additional packages like smotefamily
        return(task)
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
            
            metrics <- list(
                sensitivity = sens,
                specificity = spec,
                ppv = ppv,
                npv = npv,
                positive_lr = pos_lr,
                negative_lr = neg_lr,
                prevalence = prevalence,
                nnt = nnt
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
            metric_names <- c("Sensitivity", "Specificity", "Positive Predictive Value", 
                            "Negative Predictive Value", "Positive Likelihood Ratio", 
                            "Negative Likelihood Ratio", "Prevalence")
            metric_keys <- c("sensitivity", "specificity", "ppv", "npv", 
                           "positive_lr", "negative_lr", "prevalence")
            
            for (i in seq_along(metric_names)) {
                key <- metric_keys[i]
                if (key %in% names(clinical_metrics)) {
                    row_data <- list(
                        metric = metric_names[i],
                        value = clinical_metrics[[key]]
                    )
                    
                    # Add confidence intervals if available
                    if (self$options$reportConfidenceIntervals) {
                        ci_lower_key <- paste0(key, "_ci_lower")
                        ci_upper_key <- paste0(key, "_ci_upper")
                        
                        if (ci_lower_key %in% names(clinical_metrics)) {
                            row_data$ci_lower <- clinical_metrics[[ci_lower_key]]
                            row_data$ci_upper <- clinical_metrics[[ci_upper_key]]
                        }
                    }
                    
                    self$results$classificationMetrics$clinicalMetrics$addRow(rowKey = key, values = row_data)
                }
            }
            
            # Add NNT if available and valid
            if ("nnt" %in% names(clinical_metrics) && !is.na(clinical_metrics$nnt) && clinical_metrics$nnt > 0) {
                self$results$classificationMetrics$clinicalMetrics$addRow(
                    rowKey = "nnt", 
                    values = list(
                        metric = "Number Needed to Treat",
                        value = clinical_metrics$nnt
                    )
                )
            }
        }
    },

    .initLearner = function() {
        classifier_type <- switch(self$options$classifier,
            'singleDecisionTree' = 'classif.rpart',
            'randomForest' = 'classif.ranger',
            'knn' = 'classif.kknn',
            'naiveBayes' = 'classif.naive_bayes',
            'logisticRegression' = 'classif.log_reg',
            'svm' = 'classif.svm',
            'classif.rpart'  # default
        )

        # Initialize options based on classifier type
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
                distance = 2  # Euclidean distance
            )
            if(self$options$knnDistance == 'manhattan') {
                options$distance <- 1
            } else if(self$options$knnDistance == 'minkowski') {
                options$distance <- 3
                options$p <- 3
            }
        } else if(self$options$classifier == 'naiveBayes') {
            # Naive Bayes typically doesn't need many hyperparameters
            options <- list()
        } else if(self$options$classifier == 'logisticRegression') {
            # Logistic regression parameters
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

    .printModelParameters = function(parameters) {
        singleTreeOptions <- c(
            "type" = "single decision tree",
            "min. split " = self$options$minSplit,
            "min. bucket" = self$options$minBucket,
            "complexity" = self$options$complexity,
            "max. compete" = self$options$maxCompete,
            "max. surrogate" = self$options$maxSurrogate,
            "unsurrogate" = self$options$unsurrogate,
            "max depth" = self$options$maxDepth,
            "no. cross validations" = self$options$noCrossValidations
        )

        randomForestOptions <- c(
            "type" =  "random forest",
            "no. of trees" = self$options$noOfTrees,
            "max depth" = self$options$maxDepth,
            "sample fraction" = self$options$sampleFraction,
            "split rule" = self$options$splitRule
        )

        if(self$options$classifier == "singleDecisionTree"){
            selectedClassifier <- singleTreeOptions
        }
        else {
            selectedClassifier <- randomForestOptions
        }

        settings <- "Decision tree with "
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

    .printRandomForestModel = function(model) {
        table <- self$results$printRandForest$randomForestModel

        table$setRow(rowNo = 1, values = list(type = 'No. of trees', classif = model$forest$num.trees))
        table$setRow(rowNo = 2, values = list(type = 'Sample size', classif = model$num.samples))
        table$setRow(rowNo = 3, values = list(type = 'Number of indep. variables', classif = model$num.independent.variables))
        table$setRow(rowNo = 4, values = list(type = 'Mtyri', classif = model$mtry))
        table$setRow(rowNo = 5, values = list(type = 'Target node size', classif = model$min.node.size))
        table$setRow(rowNo = 6, values = list(type = 'Variable importance mode', classif = model$importance.mode))
        table$setRow(rowNo = 7, values = list(type = 'Split rule', classif = model$splitrule))
        table$setRow(rowNo = 8, values = list(type = 'OOB prediction error', classif = model$prediction.error))

    },

    .setOutput = function(predictions, plotData, model) {
        levels <- levels(predictions$truth)
        reporting <- self$options$reporting
        freqPlot <- self$results$predictedFreqPlot
        treePlot <- self$results$decisionTreeModel
        classifier <- self$options$classifier

        if (any(reporting == 'confusionMatrix')) {
            private$.populateConfusionMatrix(predictions$confusion)
        }

        if (any(reporting == 'AUC')) {
            binaryPredictions <- sapply(levels, function(level) private$.convertToBinary(level, predictions), USE.NAMES = TRUE)
            self$results$rocCurvePlot$setState(binaryPredictions)
        }

        if (any(reporting == "classifMetrices")) {
            table <- self$results$classificationMetrics$class
            columns <- private$.getTableColumns(table)

            private$.populateClassificationMetrices(predictions, names(columns))
        }

        if (self$options$predictedFreq == TRUE | self$options$predictedFreqRF == TRUE) {
            freqPlot$setState(plotData)
        }

        if (self$options$plotDecisionTree == TRUE & classifier == 'singleDecisionTree') {
            treePlot$setState(model)
        }

        if(self$options$printRandForest == TRUE & classifier == 'randomForest') {
            private$.printRandomForestModel(model)
        }

        # Calculate and populate clinical metrics if requested
        if (self$options$reportClinicalMetrics) {
            clinical_metrics <- private$.calculateClinicalMetrics(predictions$response, predictions$truth)
            private$.populateClinicalMetrics(clinical_metrics)
        }
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
        levels <- colnames(confusionMatrix)[!is.na(colnames(confusionMatrix))] # get levels, removes .na values if any

        lapply(levels, function(level) { # add columns
            self$results$confusion$matrix$addColumn(
                name = level,
                superTitle = 'truth',
                title = level,
                type = 'integer')
        })

        lapply(levels, function(level) { # add rows
            rowValues <- as.list(confusionMatrix[level,])
            rowValues[['class']] <- level
            self$results$confusion$matrix$addRow(rowKey = level, values = rowValues)
        })

        return (confusionMatrix)
    },

    .populateClassificationMetrices = function(predictions, outputScores) {
        generalTable <- self$results$classificationMetrics$general
        classTable <- self$results$classificationMetrics$class
        levels <- levels(predictions$truth)

        scores <- private$.calculateScores(predictions, outputScores)
        general <- scores[['general']]
        class <- scores[['class']]

        metricesDict <- c(
            classif.acc = 'Accuracy',
            classif.bacc = 'Balanced accuracy',
            classif.ce = 'Error rate',
            classif.recall = 'Macro recall',
            classif.precision = 'Macro precision',
            classif.fbeta = 'Macro F-score',
            classif.auc = 'Macro AUC'
        )

        lapply(names(general), function(name) generalTable$addRow(rowKey = name, values = list(metric = metricesDict[name], value = general[[name]])))

        lapply(levels, function(level) {
            class[[level]][['class']] <- level
            classTable$addRow(rowKey = level, values = class[[level]])
        })
    },

    .convertToBinary = function(class, predictions) {
        transformed <- transform(as.data.table(predictions), truth = ifelse(truth != class, "negative", as.character(truth)))
        transformed <- transform(as.data.table(transformed), response = ifelse(response != class, "negative", as.character(response)))

        probName <- gsub("[[:space:]]", "", paste('prob.', class)) #name of prob column, deletes space
        prob <- as.matrix(data.frame(transformed[[probName]], 1 - transformed[[probName]]))
        colnames(prob) <- c(class, "negative")

        binaryPrediction <- PredictionClassif$new(
            truth = as.factor(transformed$truth),
            response = transformed$response,
            prob = prob,
            row_ids = 1:length(transformed$truth)
        )

        return (binaryPrediction)
    },

    .getTableColumns = function(table) {
        columnsToPlot <- table$columns[c(-1)]

        columns <- lapply(columnsToPlot, function(column) {
            ifelse(column$visible, column$title, NA)
        })

        return(columns[!is.na(columns)])
    },

    .calculateScores = function(predictions, outputScores) {
        macros <- numeric(0)
        levels <- levels(predictions$truth)

        binaryPredictions <- sapply(levels, USE.NAMES = TRUE, function(level) private$.convertToBinary(level, predictions))

        classScores <- lapply(levels, function(level) {
            prob <- as.data.table(binaryPredictions[[level]])[[paste('prob', level, sep = '.')]]

            c(
                'classif.recall' = recall(binaryPredictions[[level]]$truth, binaryPredictions[[level]]$response, positive = level),
                'classif.precision' = precision(binaryPredictions[[level]]$truth, binaryPredictions[[level]]$response, positive = level),
                'classif.fbeta' = fbeta(binaryPredictions[[level]]$truth, binaryPredictions[[level]]$response, positive = level),
                'classif.auc' = auc(binaryPredictions[[level]]$truth, prob = prob, positive = level)
            )
        })

        names(classScores) <- levels
        classScores <- lapply(classScores, round , 3)

        #calculate macro scores
        for (scoreName in outputScores) {
            macros[scoreName] <- mean(sapply(levels, function(class) classScores[[class]][scoreName]))
        }

        return(list(
            general = c(predictions$score(msrs(c('classif.acc', 'classif.ce', 'classif.bacc'))), macros),
            class = classScores
        ))
    },

    .plotRocCurve = function(image, ...) {
        plotData <- image$state

        plotList <- lapply(names(plotData), function(class) {
            mlr3viz::autoplot(plotData[[class]], type = 'roc')
        })

        plot <- ggpubr::ggarrange(plotlist = plotList,
                                  labels = names(plotData),
                                  font.label = list(size = 8, color = "black", face = "bold", family = NULL),
                                  ncol = 2, nrow = round(length(names(plotData)) / 2))
        print(plot)
    },

    .printDecisionTree = function(image, ...) {
        plot <- rpart.plot::rpart.plot(image$state)
        print(plot)
    },

    .plotFrequencies = function(image, ...) {
        plot <- mlr3viz::autoplot(image$state)
        print(plot)
    })
)
