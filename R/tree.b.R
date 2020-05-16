#' Decision Tree
#'
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#'


treeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(

        # cleandata ----

        .cleanData = function() {

            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            data <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            data <- jmvcore::naOmit(data)

            data
        },



        # run function ----

        .run = function() {

            data <- private$.cleanData()


            data <- list(head(data),
                         head(as.data.frame(data)),
                         summary(data))


            self$results$text1$setContent(data)




        }

        ,
        .plot1 = function(image, ...) {  # <-- the plot1 function ----


            varsName <- self$options$vars

            facsName <- self$options$facs

            targetName <- self$options$target

            data <- jmvcore::select(self$data, c(varsName, facsName, targetName))

            for (fac in facsName)
                data[[fac]] <- as.factor(data[[fac]])

            data <- jmvcore::naOmit(data)


            targetName <- jmvcore::composeTerm(targetName)

            tree1 <- data %>%
                explore::explain_tree(target = targetName)


            plot1 <- tree1

            print(plot1)
            TRUE


        }

        ,
        .plot2 = function(image, ...) {  # <-- the plot2 function ----

            # if (is.null(self$options$vars) || is.null(self$options$target))
            #     return()

            mydata <- self$data
            myvars <- self$options$vars
            mytarget <- self$options$target

            mydata <- jmvcore::naOmit(mydata)

            mydata <- mydata %>%
                dplyr::select(mytarget, myvars)

            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)

            # mytree <- FFTrees::FFTrees(
            #     formula = myformula,
            #     data = mydata,
            #     data.test = mydata
            #     )
            #
            # plot2 <- plot(mytree,
            #      data = mydata,
            #      main = mydata
            #      )


            iris1 <- iris
            iris1$target <- sample(x = c(TRUE,FALSE), size = dim(iris)[1], replace = TRUE)


            iris.fft <- FFTrees::FFTrees(formula = target ~.,
                                         data = iris1,
                                 data.test = iris1,
                                 force = TRUE)

            plot(iris.fft,
                 data = "test")

            TRUE
        }

        ,
        .plot3 = function(image, ...) {  # <-- the plot3 function ----

            mydata <- self$data
            myvars <- self$options$vars
            mytarget <- self$options$target

            mydata <- jmvcore::naOmit(mydata)

            mydata <- mydata %>%
                dplyr::select(mytarget, myvars)

            myformula <- jmvcore::constructFormula(terms = self$options$target)

            myformula <- paste(myformula, '~ .')

            myformula <- as.formula(myformula)


            # Load rpart and rpart.plot
            # Create a decision tree model
            tree <- rpart::rpart(Species~., data = iris, cp = .02)

            # Visualize the decision tree with rpart.plot
            plot3 <- rpart.plot::rpart.plot(tree,
                                            box.palette = "RdBu",
                                            shadow.col = "gray",
                                            nn = TRUE)





            print(plot3)
            TRUE


        }


        )
)

