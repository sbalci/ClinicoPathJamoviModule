#' @title Decision Tree
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#'

treeClass <- if (requireNamespace('jmvcore'))
    R6::R6Class("treeClass",
                inherit = treeBase,
                private = list(

                    # prepare data ----

                    .prepareData = function() {

                        varsName <- self$options$vars

                        facsName <- self$options$facs

                        targetName <- self$options$target

                        trainName <- self$options$train


                        mydata <- jmvcore::select(self$data, c(varsName,
                                                               facsName,
                                                               targetName,
                                                               trainName))

                        mydata <- jmvcore::naOmit(mydata)


                        self$results$text1$setContent(head(mydata, n = 20))




                        mydata[[targetName]] <- as.factor(mydata[[targetName]])

                        mydata[[trainName]] <- as.factor(mydata[[trainName]])


                        targetLevel <- self$options$targetLevel

                        trainLevel <- self$options$trainLevel

                        mydata[[targetName]] <-
                            as.factor(mydata[[targetName]])

                        mydata[["targetName2"]] <- ifelse(mydata[[targetName]] == targetLevel, "Disease", "Healthy")


                        mydata[[trainName]] <-
                            as.factor(mydata[[trainName]])

                        mydata[["trainName2"]] <- ifelse(mydata[[trainName]] == trainLevel, "Train", "Test")



                        mydata <- jmvcore::naOmit(mydata)

                        self$results$text2$setContent(head(mydata, n = 20))


                        trainData <- mydata %>%
                            dplyr::filter(trainName2 == "Train") %>%
                            dplyr::select(varsName,
                                          facsName,
                                          targetName)


                        testData <- mydata %>%
                            dplyr::filter(trainName2 == "Test") %>%
                            dplyr::select(varsName,
                                          facsName,
                                          targetName)


                        self$results$text2a$setContent(head(trainData))

                        self$results$text2b$setContent(head(testData))



                        ## Return Data ----

                        return(
                            list(
                                "mydata" = mydata,
                                "trainData" = trainData,
                                "testData" = testData

                            )
                        )





                    },



                    # explore ----

                    .treeExplore = function() {},

                    # FFTrees ----

                    .treeFFTrees = function() {},


                    # rpart


                    .treerpart = function() {},




                    # run function ----

                    .run = function() {
                        # Error Message ----

                        if (nrow(self$data) == 0)
                            stop("Data contains no (complete) rows")

                #         if ((is.null(self$options$vars) ||
                #              is.null(self$options$facs)) && is.null(self$options$target)) {
                #             # ToDo Message ----
                #             todo <- "
                # <br>Welcome to ClinicoPath
                #           <br><br>
                #           This tool will help you form Decision Trees.
                #           "
                #             html <- self$results$todo
                #             html$setContent(todo)
                #             return()
                #
                #         } else {
                #             todo <- ""
                #             html <- self$results$todo
                #             html$setContent(todo)
                #
                #         }


                        # Get Clean Data ----
                        results <- private$.prepareData()

                        mydata <- results$mydata

                        testData <- results$testData

                        trainData <- results$trainData




                        # Select Graph Style ----

                        # sty <- self$options$sty


                        # explore ----
                        # if (sty == "explore") {



                        # sumdata <- list(typeof(mydata),
                        #              class(mydata),
                        #              head(mydata),
                        #              head(as.data.frame(mydata)),
                        #              summary(mydata))


                        # self$results$text1$setContent(sumdata)





                        # rpart ----

                        # } else if (sty == "rpart") {

                        # Prepare formula ----

                        # myformula <- jmvcore::constructFormula(terms = self$options$target)
                        #
                        # myformula <- paste(myformula, '~ .')
                        #
                        # myformula <- as.formula(myformula)


                        # rpart function ----

                        # tree <- rpart::rpart(myformula, data = mydata, cp = .02)
                        #
                        # self$results$text2$setContent(tree)


                        # FFTrees ----
                        # } else if (sty == "fftrees") {


                        # Prepare Data ----






                        # Prepare formula ----

                        myformula <- jmvcore::constructFormula(terms = self$options$target)

                        myformula <- paste0(myformula, ' ~ .')

                        myformula <- as.formula(myformula)

                        # FFTrees ----
                        # https://ndphillips.github.io/useR2017_pres/#1


                        mytree.fft <- FFTrees::FFTrees(
                            formula = myformula,
                            data = trainData,
                            data.test = testData
                        #     # ,
                        #     # decision.labels = c("Healthy", "Disease")
                        )
                        #
                        # Sys.sleep(10)


                        # heart.test <- FFTrees::heart.test
                        #
                        # heart.train <- FFTrees::heart.train
                        #
                        # heart.fft <- FFTrees(formula = diagnosis ~.,
                        #                      data = heart.train,
                        #                      data.test = heart.test,
                        #                      decision.labels = c("Healthy", "Disease"))

                        # myoutput <- capture.output(
                        #     print(mytree.fft)
                        # )

                        self$results$text3$setContent(exists("mytree.fft"))

                        self$results$text4$setContent(mytree.fft)

                        }
                    # }

                # ,
                # .plot1 = function(image, ggtheme, theme, ...) {  # <-- the plot1 function ----
                #
                #
                #     # explore ----
                #
                #     # Error Message ----
                #
                #     if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
                #
                #     if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) )
                #         return()
                #
                #
                #     # Prepare Data ----
                #
                #     varsName <- self$options$vars
                #
                #     facsName <- self$options$facs
                #
                #     targetName <- self$options$target
                #
                #     mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))
                #
                #     mydata[[targetName]] <- as.factor(mydata[[targetName]])
                #
                #
                #     for (fac in facsName)
                #         mydata[[fac]] <- as.factor(mydata[[fac]])
                #
                #     for (cov in varsName)
                #         mydata[[cov]] <- jmvcore::toNumeric(mydata[[cov]])
                #
                #
                #     mydata <- jmvcore::naOmit(mydata)
                #
                #
                #     # Explore ----
                #
                #
                #     plot1 <-
                #         explore::explain_tree(data = mydata,
                #                               target = .data[[targetName]]
                #                               )
                #
                #     print(plot1)
                #     TRUE
                # }
                #
                # ,
                # .plot2 = function(image, ggtheme, theme, ...) {  # <-- the plot2 function ----
                #
                #
                #     # FFTrees ----
                #
                #     # Error Message ----
                #
                #     if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
                #
                #     if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) )
                #         return()
                #
                #     if ( !self$options$showPlot )
                #         return()
                #
                #
                #     # Prepare Data ----
                #
                #     varsName <- self$options$vars
                #
                #     facsName <- self$options$facs
                #
                #     targetName <- self$options$target
                #
                #     targetLevel <- self$options$targetLevel
                #
                #     trainName <- self$options$train
                #
                #     trainLevel <- self$options$trainLevel
                #
                #
                #     mydata <- jmvcore::select(self$data, c(varsName,
                #                                            facsName,
                #                                            targetName,
                #                                            trainName
                #                                            )
                #                               )
                #
                #     mydata[[targetName]] <- as.factor(mydata[[targetName]])
                #
                #     mydata[["targetName2"]] <- ifelse(
                #         mydata[[targetName]] == targetLevel, TRUE, FALSE)
                #
                #
                #     mydata[[trainName]] <- as.factor(mydata[[trainName]])
                #
                #     mydata[["trainName2"]] <- ifelse(
                #         mydata[[trainName]] == trainLevel, TRUE, FALSE)
                #
                #
                #     mydata <- jmvcore::naOmit(mydata)
                #
                #
                #     trainData <- mydata[mydata[["trainName2"]] == TRUE, ]
                #
                #     testData <- mydata[mydata[["trainName2"]] == FALSE, ]
                #
                #     # Prepare formula ----
                #
                #     myformula <- jmvcore::constructFormula(terms = self$options$target)
                #
                #     myformula <- paste("targetName2", '~ .')
                #
                #     myformula <- as.formula(myformula)
                #
                #     # FFTrees ----
                #     # https://ndphillips.github.io/useR2017_pres/#1
                #
                #     mytree.fft <- FFTrees::FFTrees(
                #         formula = myformula,
                #         data = trainData,
                #         data.test = testData
                #         )
                #
                #     plot2 <- plot(mytree.fft,
                #          data = "test"
                #          )
                #
                #     print(plot2)
                #     TRUE
                #
                # }
                #
                # ,
                # .plot3 = function(image, ggtheme, theme, ...) {  # <-- the plot3 function ----
                #
                #
                #     # rpart ----
                #
                #
                #     # Error Message ----
                #
                #     if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
                #
                #     if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) )
                #         return()
                #
                #     # Prepare Data ----
                #
                #     varsName <- self$options$vars
                #
                #     facsName <- self$options$facs
                #
                #     targetName <- self$options$target
                #
                #     targetLevel <- self$options$targetLevel
                #
                #     mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))
                #
                #     mydata[[targetName]] <- as.factor(mydata[[targetName]])
                #
                #     mydata[[targetName]] <- ifelse(
                #         mydata[[targetName]] == targetLevel, "Target", "Others")
                #
                #     for (fac in facsName)
                #         mydata[[fac]] <- as.factor(mydata[[fac]])
                #
                #     for (cov in varsName)
                #         mydata[[cov]] <- jmvcore::toNumeric(mydata[[cov]])
                #
                #
                #
                #     mydata <- jmvcore::naOmit(mydata)
                #
                #     # rpart ----
                #
                #     myformula <- jmvcore::constructFormula(terms = self$options$target)
                #
                #     myformula <- paste(myformula, '~ .')
                #
                #     myformula <- as.formula(myformula)
                #
                #
                #     tree <- rpart::rpart(formula = myformula,
                #                          data = mydata,
                #
                #                          cp = .02
                #                          )
                #
                #
                #     plot3 <- rpart.plot::rpart.plot(tree,
                #                                     box.palette = "RdBu",
                #                                     shadow.col = "gray",
                #                                     nn = TRUE)
                #
                #     print(plot3)
                #     TRUE
                #
                # }



                ))
