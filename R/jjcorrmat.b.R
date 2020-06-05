#' @title jjcorrmat
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjcorrmatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcorrmatClass",
    inherit = jjcorrmatBase,
    private = list(

        .run = function() {

            # Initial Message ----
            if (is.null(self$options$dep) ||
                is.null(self$options$group)) {
                # TODO ----

                todo <- glue::glue(
                    "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Plots.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>here</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>here</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                todo <- glue::glue(
                    "<br>You have selected to use a barplot to compare a categorical variable with another.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        ,
        .plot = function(image, ...) {
            # the plot function ----
            # Error messages ----

            if (is.null(self$options$dep) ||
                is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            direction <- self$options$direction

            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw


            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            mydep <- mydata[[self$options$dep]]
            mygroup <- mydata[[self$options$group]]

            if ( !is.null(self$options$grvar) ) {
                mygrvar <- mydata[[self$options$grvar]]
            }


            # ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html



            ggcorrmat(
                data,
                cor.vars = NULL,
                cor.vars.names = NULL,
                output = "plot",
                matrix.type = "full",
                matrix.method = "square",
                type = "parametric",
                beta = 0.1,
                k = 2L,
                sig.level = 0.05,
                conf.level = 0.95,
                bf.prior = 0.707,
                p.adjust.method = "none",
                pch = "cross",
                ggcorrplot.args = list(outline.color = "black"),
                package = "RColorBrewer",
                palette = "Dark2",
                colors = c("#E69F00", "white", "#009E73"),
                ggtheme = ggplot2::theme_bw(),
                ggstatsplot.layer = TRUE,
                ggplot.component = NULL,
                title = NULL,
                subtitle = NULL,
                caption = NULL,
                messages = TRUE,
                ...
            )






            # grouped_ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html


            grouped_ggcorrmat(
                data,
                cor.vars = NULL,
                cor.vars.names = NULL,
                grouping.var,
                title.prefix = NULL,
                output = "plot",
                ...,
                plotgrid.args = list(),
                title.text = NULL,
                title.args = list(size = 16, fontface = "bold"),
                caption.text = NULL,
                caption.args = list(size = 10),
                sub.text = NULL,
                sub.args = list(size = 12)
            )







            # Print Plot ----

            print(plot)
            TRUE

        }
    )
)









