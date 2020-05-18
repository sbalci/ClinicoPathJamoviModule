#' Decision Tree
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#'


treeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(


        # run function ----

        .run = function() {


            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata <- jmvcore::naOmit(mydata)


            sumdata <- list(typeof(mydata),
                         class(mydata),
                         head(mydata),
                         head(as.data.frame(mydata)),
                         summary(mydata))


            self$results$text1$setContent(sumdata)

        }

        ,
        .plot1 = function(image, ...) {  # <-- the plot1 function ----


            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata <- jmvcore::naOmit(mydata)


            # Tree function explore ----


            plot1 <-
                explore::explain_tree(data = mydata,
                                      target = .data[[targetName]]
                                      )

            print(plot1)
            TRUE

        }

        ,
        .plot2 = function(image, ...) {  # <-- the plot2 function ----


            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata <- jmvcore::naOmit(mydata)


            # Prepare formula ----

            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)


            # Tree function FFTrees ----

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
        .plot3 = function(image, ...) {  # <-- the plot3 function ----

            # Prepare Data ----

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            mydata <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            mydata[[targetName]] <- as.factor(mydata[[targetName]])

            mydata <- jmvcore::naOmit(mydata)


            # Prepare formula ----


            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)


            tree <- rpart::rpart(myformula, data = mydata, cp = .02)


            plot3 <- rpart.plot::rpart.plot(tree,
                                            box.palette = "RdBu",
                                            shadow.col = "gray",
                                            nn = TRUE)

            print(plot3)
            TRUE

        }


        )
)

