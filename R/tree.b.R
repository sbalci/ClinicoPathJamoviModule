#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric


treeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(
        .run = function() {

            # TODO

            todo <- glue::glue(
                "This Module is still under development
                🔬🔬🔬🔬 UNDER CONSTRUCTION 🛠⛔️⚠️🔩
                -
                -
                "
            )

            self$results$todo$setContent(todo)


            # if (is.null(self$options$vars) || is.null(self$options$target))
            #     return()


            # prepare data for explore ----
            # https://cran.r-project.org/web/packages/explore/vignettes/explore.html


            # result1 <- iris %>% explore::explain_tree(target = Species)
            #
            # self$results$text1$setContent(result1)


            # image <- self$results$plot

            # image$setState(plotData)









        },


        .plot = function(image, ...) {  # <-- the plot function ----

            # if (is.null(self$options$vars) || is.null(self$options$target))
            #     return()

            mydata <- self$data
            myvars <- self$options$vars
            mytarget <- self$options$target

            # xtarget <- jmvcore::composeTerm(components = self$options$target)

            mydata <- jmvcore::naOmit(mydata)

            mydata <- mydata %>%
                dplyr::select(mytarget, myvars)

            # plot <- mydata %>%
            #     explore::explain_tree(data = .,
            #                           target = .data[[xtarget]])


            # plot <- iris %>% explore::explain_tree(target = Species)

            # if (length(self$options$dep) + length(self$options$group) < 2)

            #     return()


            tree1 <- iris %>% explore::explain_tree(target = Species)


            iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
            tree2 <- iris %>%
            dplyr::select(-Species) %>%
            explore::explain_tree(target = is_versicolor)

            tree3 <- iris %>%
            explore::explain_tree(target = Sepal.Length)

            plot <- tree1

            print(plot)
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
            iris1$target <- sample(x = c(0,1), size = dim(iris)[1], replace = TRUE)


            # Create an FFTrees object from the heartdisease data
            iris.fft <- FFTrees::FFTrees(formula = target ~.,
                                 data = iris1)
            # Plot the best tree applied to the test data
            plot2 <- plot(iris.fft,
                 data = "iris",
                 main = "iris")

            print(plot2)
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

