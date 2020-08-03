# from https://github.com/marusakonecnik/jamovi-plugin-for-machine-learning

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

        if (length(self$options$dep) == 0 || length(self$options$indep) == 0)
            return()

        data <- as.data.table(self$data)

        task <- TaskClassif$new(id = "task", backend = data[complete.cases(data),], target = self$options$dep)

        learner <- private$.initLearner()

        private$.trainModel(task, learner)
    },

    .initLearner = function() {
        classifier <- ifelse(self$options$classifier == 'singleDecisionTree', 'classif.rpart', 'classif.ranger')

        if(self$options$classifier == 'singleDecisionTree'){
            classifierType <- 'classif.rpart'
            options <- list(
                   minsplit = self$options$minSplit,
                   maxcompete = self$options$maxCompete,
                   maxsurrogate = self$options$maxSurrogate,
                   maxdepth = self$options$maxDepth,
                   cp = self$options$complecity
            )
        } else {
            classifierType <- 'classif.ranger'
            options <- list(
                        num.trees = self$options$noOfTrees,
                        splitrule = self$options$splitRule,
                        sample.fraction = self$options$sampleFraction,
                        min.node.size = self$options$maxDepth
            )
        }

        learner <- lrn(classifierType, predict_type = 'prob')
        learner$param_set$values <- options

        return (learner)
    },

    .printModelParameters = function(parameters) {
        singleTreeOptions <- c(
            "type" = "single decision tree",
            "min. split " = self$options$minSplit,
            "min. bucket" = self$options$minBucket,
            "complecity" = self$options$complecity,
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
