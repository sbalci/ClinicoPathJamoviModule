#' @title Decision Tree
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# @import explore
#'


treeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(


        # run function ----

        .run = function() {


            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
                # ToDo Message ----
                todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form Decision Trees.
                          "
                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

            }


            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata <- jmvcore::naOmit(mydata)

            self$results$text1$setContent(head(mydata, n = 20))


            # Select Graph Style ----

            sty <- self$options$sty


            # explore ----
            if (sty == "explore") {



            # sumdata <- list(typeof(mydata),
            #              class(mydata),
            #              head(mydata),
            #              head(as.data.frame(mydata)),
            #              summary(mydata))


            # self$results$text1$setContent(sumdata)





            # rpart ----

            } else if (sty == "rpart") {

            # Prepare formula ----

            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)


            # rpart function ----

            tree <- rpart::rpart(myformula, data = mydata, cp = .02)

            self$results$text2$setContent(tree)


            # FFTrees ----
            } else if (sty == "fftrees") {


                # Prepare Data ----

                varsName <- self$options$vars

                facsName <- self$options$facs

                targetName <- self$options$target

                targetLevel <- self$options$targetLevel

                mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

                mydata[[targetName]] <- as.factor(mydata[[targetName]])

                mydata[["targetName2"]] <- ifelse(
                    mydata[[targetName]] == targetLevel, TRUE, FALSE)

                mydata <- jmvcore::naOmit(mydata)



                self$results$text2$setContent(mydata)


            }
        }

        ,
        .plot1 = function(image, ggtheme, theme, ...) {  # <-- the plot1 function ----


            # explore ----

            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) )
                return()


            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])


            for (fac in facsName)
                mydata[[fac]] <- as.factor(mydata[[fac]])

            for (cov in varsName)
                mydata[[cov]] <- jmvcore::toNumeric(mydata[[cov]])


            mydata <- jmvcore::naOmit(mydata)


            # Explore ----


            plot1 <-
                explore::explain_tree(data = mydata,
                                      target = .data[[targetName]]
                                      )

            print(plot1)
            TRUE
        }

        ,
        .plot2 = function(image, ggtheme, theme, ...) {  # <-- the plot2 function ----


            # FFTrees ----

            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) )
                return()

            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            targetLevel <- self$options$targetLevel

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata[[targetName]] <- ifelse(
                mydata[[targetName]] == targetLevel, TRUE, FALSE)

            mydata <- jmvcore::naOmit(mydata)

            # Prepare formula ----

            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)

            # FFTrees ----
            # https://ndphillips.github.io/useR2017_pres/#1

            mytree.fft <- FFTrees::FFTrees(
                formula = myformula,
                data = mydata,
                data.test = mydata
                )

            plot2 <- plot(mytree.fft,
                 data = "test"
                 )

            print(plot2)
            TRUE

        }

        ,
        .plot3 = function(image, ggtheme, theme, ...) {  # <-- the plot3 function ----


            # rpart ----


            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) )
                return()

            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            targetLevel <- self$options$targetLevel

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata[[targetName]] <- ifelse(
                mydata[[targetName]] == targetLevel, "Target", "Others")

            for (fac in facsName)
                mydata[[fac]] <- as.factor(mydata[[fac]])

            for (cov in varsName)
                mydata[[cov]] <- jmvcore::toNumeric(mydata[[cov]])



            mydata <- jmvcore::naOmit(mydata)

            # rpart ----

            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)


            tree <- rpart::rpart(formula = myformula,
                                 data = mydata,

                                 cp = .02
                                 )


            plot3 <- rpart.plot::rpart.plot(tree,
                                            box.palette = "RdBu",
                                            shadow.col = "gray",
                                            nn = TRUE)

            print(plot3)
            TRUE

        }
        )
)

