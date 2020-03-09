#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

treeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(
        .run = function() {



            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -
                -
                "
            )

            self$results$todo$setContent(todo)


            ####



            # if (length(self$options$dep) + length(self$options$group) < 2)
            #     return()



            # https://cran.r-project.org/web/packages/explore/vignettes/explore.html


            # mydata <- self$data

            # mydep <- self$data[[self$options$dep]]
            # mygroup <- self$data[[self$options$group]]


            # plotData <- data.frame(gr = mygroup, dp = jmvcore::toNumeric(mydep))
            # plotData <- jmvcore::naOmit(plotData)


            # image <- self$results$plot

            # image$setState(plotData)


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        },


        .plot=function(image, ...) {  # <-- the plot function

            # if (length(self$options$dep) + length(self$options$group) < 2)
            #     return()


            # plotData <- image$state



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


        })
)
