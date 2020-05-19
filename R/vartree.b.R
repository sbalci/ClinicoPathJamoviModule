#' Variable Tree
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


vartreeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "vartreeClass",
    inherit = vartreeBase,
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


            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')



            mydata <- self$data

            mydata <- jmvcore::naOmit(mydata)

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            mydata <- mydata %>%
                dplyr::select(myvars)

            myvars <- paste0(myvars, collapse = " ")

            results <- vtree::vtree(mydata, myvars)

            # diagram <- results[["x"]][["diagram"]]
            #
            # results <- DiagrammeR::grViz(diagram = diagram)



            results <-
                DiagrammeR::create_graph() %>%
                DiagrammeR::add_node() %>%
                DiagrammeR::add_node() %>%
                DiagrammeR::add_edge(from = 1, to = 2)

            results <- DiagrammeR::render_graph(results, layout = "nicely")


            self$results$text1$setContent(print(results))

            self$results$text2$setContent(print(results))







        },


        .plot = function(image, ...) {  # <-- the plot function ----

            mydata <- self$data

            mydata <- jmvcore::naOmit(mydata)

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            mydata <- mydata %>%
                dplyr::select(myvars)

            myvars <- paste0(myvars, collapse = " ")

            results <- vtree::vtree(mydata, myvars)

            # diagram <- results[["x"]][["diagram"]]

            # results <- DiagrammeR::grViz(diagram = diagram)

            plot <- results




            # plot <-
            #     DiagrammeR::create_graph() %>%
            #     DiagrammeR::add_node() %>%
            #     DiagrammeR::add_node() %>%
            #     DiagrammeR::add_edge(from = 1, to = 2)

            # plot <- DiagrammeR::render_graph(plot, layout = "nicely")


            print(plot)
            TRUE

        }


        )
)
