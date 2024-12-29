# from https://github.com/marusakonecnik/jamovi-plugin-for-machine-learning

experimenterClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "experimenterClass",
    inherit = experimenterBase,
    private = list(
        .init = function() {
            preformatted <- jmvcore::Preformatted$new(self$options, 'preformatted')
            self$results$add(preformatted)
        },
        .run = function() {
            library(mlr3) #can't use some mlr3 functions without this for some reason

            if (length(self$options$dep) == 0 || length(self$options$indep) == 0)
                return()

            data <- as.data.table(self$data)

            task <- TaskClassif$new(id = "task", backend = data[complete.cases(data),], target = self$options$dep)

            private$.setOutput(task)
        },

        .populateOverallMetrics = function(classifier, predictions) {
            if(length(self$options$classifiersToUse) == 0)
                return()

            table <- self$results$overallMetrics$overallMetricsTable
            columns <- private$.getTableColumns(table)

            scores <- private$.calculateScores(predictions, names(columns))[['general']]

            row <- as.list(sapply(names(scores), function(name) scores[[name]], USE.NAMES = TRUE ))
            row[['classifier']] <- classifier

            table$addRow(rowKey = classifier, values = row)
        },

        .populatePerClassMetrics = function(classifier, predictions, levels) {
            if(length(self$options$classifiersToUse) == 0)
                return();

            tables <- self$results$perClassMetrics
            table <- tables$get(key = classifier)

            columns <- private$.getTableColumns(table)

            scores <- private$.calculateScores(predictions, names(columns))[['class']]

            for(level in levels) {
               scores[[level]][['class']] <- level
               table$addRow(rowKey = level, values = scores[[level]])
            }
        },

        .populateRocCurvePlots = function(classifier, predictions, levels) {
            if(length(self$options$classifiersToUse) == 0)
                return()

            plot <- self$results$rocCurvePlots$get(key = classifier)

            binaryPredictions <- sapply(levels, function(level) private$.convertToBinary(level, predictions), USE.NAMES = TRUE)

            plot$setState(binaryPredictions)
        },

        .populateMetricComparisonPlot = function(classifier, predictions, plotData) {
            classifiers <- self$options$classifiersToUse

            if(length(classifiers) == 0)
                return()

            table <- self$results$overallMetrics$overallMetricsTable
            columns <- private$.getTableColumns(table)

            scores <- private$.calculateScores(predictions, names(columns))[['general']]

            for (name in names(scores)) {
                plotData[nrow(plotData) + 1,] = c(columns[[name]], classifier, round(scores[[name]], 2))
            }

            if(classifier == classifiers[[length(classifiers)]]) {
                plot <- self$results$metricComparison$metricComparisonPlot
                plot$setState(plotData)
                return()
            }
            else {
                return (plotData)
            }
        },

        .populatePerClassComparisonPlot = function(classifier, predictions, plotsData) {
            classifiers <- self$options$classifiersToUse

            if(length(classifiers) == 0)
                return();

            table <- self$results$perClassMetrics$get(key = classifiers[[1]])
            columns <- private$.getTableColumns(table)

            scores <- private$.calculateScores(predictions, names(columns))[['class']]

            for (name in names(plotsData)) {
                for(score in names(columns)) {
                     insertIndex <- nrow(plotsData[[name]]) + 1
                     scoreName <- columns[[score]]
                     scoreValue <- scores[[name]][[score]]
                     plotsData[[name]][insertIndex, ] <- c(scoreName, classifier, scoreValue)
                }
             }

            if(classifier == classifiers[[length(classifiers)]]) {
                plot <- self$results$metricComparison$perClassComparisonPlot
                plot$setState(plotsData)
                return()
            }
            else {
                return (plotsData)
            }
        },

        .getTableColumns = function(table) {
            columnsToPlot <- table$columns[c(-1)]

            columns <- lapply(columnsToPlot, function(column) {
                ifelse(column$visible, column$title, NA)
            })

            return(columns[!is.na(columns)])
        },

        .getSettings = function(classifier) {
            settings <- regmatches(classifier, gregexpr("\\(.+?\\)", classifier))

            if (!identical(settings[[1]], character(0))) {
                classifierOptions <- substr(settings, 2, nchar(settings) - 1)

                if (private$.isSettingsValid(classifierOptions)) {
                    settings <- private$.getOptions(classifierOptions)
                }
            }
            return(settings)
        },

        .getPredictions = function(task, classifier, settings) {
            learner <- private$.initLearner(classifier, settings)
            predictions <- private$.trainModel(task, learner)

            return(predictions)
        },

        .isSettingsValid = function(settings) {
            errors <- list(
                missingComma = "Settings input not valid. Did you forget to put ',' between options? Example of a valid input: k = 3, distance = 2",
                missingEquals = "Settings input not valid. Did you forget to put '=' when assigning value to an option? Example of a valid input: k = 3, distance = 2",
                unknown = "Settings input not valid. Please check again if your input is as requested. Example of a valid input: k = 3, distance = 2"
            )

            settingsValid <- regmatches(settings, regexpr("^(\\S+ ?= ?\\S+(, ?|$))+$", settings))

            equalsCount <- stringr::str_count(settings, "=") # number of equals in user input settings
            commasCount <- stringr::str_count(settings, ",") # number of commas in user input settings

            if (identical(settingsValid, character(0))) {
                if (equalsCount > commasCount + 1) {
                    stop(errors$missingComma)
                } else if (commasCount + 1 > equalsCount) {
                    stop(errors$missingEquals)
                } else {
                    stop(errors$unknown)
                }
            }
            return(TRUE)
        },

        .getOptions = function(settings) {
            settings <- gsub("[[:space:]]", "", settings)
            splitted <- strsplit(settings, ',')[[1]]
            options <- list()

            for (option in splitted) {
                splittedOption <- strsplit(option, '=')[[1]]
                optionName <- splittedOption[1]
                optionValue <- splittedOption[2]

                if (!is.na(as.numeric((splittedOption[2])))) {
                    optionValue <- as.numeric(optionValue)
                }

                if (optionValue == "TRUE" ||
                    optionValue == "FALSE" ||
                    optionValue == 'true' ||
                    optionValue == 'false') {
                    optionValue <- as.logical(optionValue)
                }
                options[[optionName]] <- optionValue
            }
            return(options)
        },

        .initLearner = function(classifier, options) {
            if (grepl("KNN", classifier, fixed = TRUE)) {
                learner <- lrn("classif.kknn", predict_type = 'prob')
            } else if (grepl("Decision tree", classifier, fixed = TRUE)) {
                learner <- lrn("classif.rpart", predict_type = 'prob')
            } else if (grepl("Random forest", classifier, fixed = TRUE)) {
                learner <- lrn("classif.ranger", predict_type = 'prob')
            } else if (grepl("Naive bayes", classifier, fixed = TRUE)) {
                learner <- lrn("classif.naive_bayes", predict_type = 'prob')
            } else if (grepl("Logistic regression", classifier, fixed = TRUE)) {
                learner <- lrn("classif.log_reg", predict_type = 'prob')
            }

            if (!identical(options[[1]], character(0))) {
                learner$param_set$values <- options
            }

            return(learner)
        },

        .trainModel = function(task, learner) {
            if (self$options$testing == "split" | self$options$testing == "trainSet") {
                predictions <- private$.trainTestSplit(task, learner)
                return(predictions)
            } else {
                predictions <- private$.crossValidate(task, learner)
                return(predictions$prediction())
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

            return(prediction)
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
            classScores <- lapply(classScores, round, 3)

            #calculate macro scores
            for (scoreName in outputScores) {
                macros[scoreName] <- mean(sapply(levels, function(class) classScores[[class]][scoreName]))
            }

            return(list(
                general = c(predictions$score(msrs(c('classif.acc', 'classif.ce', 'classif.bacc'))), macros),
                class = classScores
            ))
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

            binaryPrediction
        },

        .setOutput = function(task) {
            reporting <- self$options$reporting
            classifiers <- self$options$classifiersToUse
            levels <- levels(task$truth())

            overallPlotData <- data.frame(
                    matrix(vector(), 0, 3, dimnames=list(c(), c("metric", "classifier", "value")))
            )

            perClassPlotData <- lapply(levels, function(level) {
                    data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("metric", "classifier", "value"))))
            })
            names(perClassPlotData) <- levels

            for (classifier in classifiers) {
                settings <- private$.getSettings(classifier)
                predictions <- private$.getPredictions(task, classifier, settings)

                if (any(reporting == 'AUC')) {
                    private$.populateRocCurvePlots(classifier, predictions, levels)
                }

                if (any(reporting == "classifMetrices") ) {
                    private$.populateOverallMetrics(classifier, predictions)
                }

                if (any(reporting == 'perClass')) {
                    private$.populatePerClassMetrics(classifier, predictions, levels)
                }

                if (any(reporting == 'plotMetricComparison')) {
                    overallPlotData <- private$.populateMetricComparisonPlot(classifier, predictions, overallPlotData)
                    perClassPlotData <- private$.populatePerClassComparisonPlot(classifier, predictions, perClassPlotData)
                }
            }
        },

        .rocCurve = function(image, ...) {
            plotData <- image$state

            plotList <- lapply(names(plotData), function(class) {
                mlr3viz::autoplot(plotData[[class]], type = 'roc', title = 'lala')
            })

            plot <- ggpubr::ggarrange(plotlist = plotList,
                                      labels = names(plotData),
                                      font.label = list(size = 14, color = "black", family = NULL),
                                      ncol = length(plotList),
                                      nrow = ceiling(length(names(plotData)) / 3))

            print(plot)
        },

        .plotMetricComparison = function(image, ...) {
            plotData <- image$state

            plot <- ggplot(
                data = plotData,
                aes(x = metric,
                    y = value,
                    fill = classifier)) +
                labs(title = 'Overall metrics') +
                geom_bar(stat = "identity",
                         position = position_dodge()
                )

            print(plot)
        },

        .perClassMetricComparison = function(image, ...) {
            plotData <- image$state

            plotList <- lapply(names(plotData), function(class) {
                ggplot(
                    data = plotData[[class]],
                    aes(x = metric,
                        y = value,
                        fill = classifier)) +
                    labs(title = class) +
                    geom_bar(stat = "identity",
                             position = position_dodge()
                    )
            })

            plot <- ggpubr::ggarrange(plotlist = plotList,
                                      ncol = 1,
                                      nrow = length(plotList))
            print(plot)
        }
    )
)
