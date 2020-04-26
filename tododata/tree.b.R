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


            if (is.null(self$options$vars) || is.null(self$options$target))
                return()


            # prepare data for explore ----
            # https://cran.r-project.org/web/packages/explore/vignettes/explore.html


            # image <- self$results$plot

            # image$setState(plotData)

        },


        .plot = function(image, ...) {  # <-- the plot function ----

            if (is.null(self$options$vars) || is.null(self$options$target))
                return()

            mydata <- self$data
            myvars <- self$options$vars
            mytarget <- self$options$target

            xtarget <- jmvcore::composeTerm(components = self$options$target)

            mydata <- jmvcore::naOmit(mydata)

            mydata <- mydata %>%
                dplyr::select(mytarget, myvars)

            plot <- mydata %>%
                explore::explain_tree(data = .,
                                      target = .data[[xtarget]])

            print(plot)
            TRUE


        },


        .plot2 = function(image, ...) {  # <-- the plot2 function ----

            if (is.null(self$options$vars) || is.null(self$options$target))
                return()

            mydata <- self$data
            myvars <- self$options$vars
            mytarget <- self$options$target

            mydata <- jmvcore::naOmit(mydata)

            mydata <- mydata %>%
                dplyr::select(mytarget, myvars)

            # plot <- mydata %>%
            #     explore::explain_tree(data = .,
            #                           target = .data[[xtarget]])


            formula <- jmvcore::constructFormula(terms = self$options$target)

            formula <- paste(formula, '~ .')

            formula <- as.formula(formula)

            # Create an FFTrees object from the data
            FFTrees.fft <- FFTrees::FFTrees(
                formula = formula,
                data = mydata
                )

            # Plot the best tree applied to the test data
            plot2 <- plot(FFTrees.fft,
                 data = mydata
                 # ,
                 # main = "Heart Disease",
                 # decision.labels = c("Healthy", "Disease")
                 )


            # Create an FFTrees object from the heartdisease data
            # iris.fft <- FFTrees::FFTrees(formula = Species ~.,
            #                      data = iris)
            # Plot the best tree applied to the test data
            # plot <- plot(iris.fft,
            #      data = "iris",
            #      main = "iris")





            print(plot2)
            TRUE


        }




        )
)
