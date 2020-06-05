#' @title jjcoefstats
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjcoefstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcoefstatsClass",
    inherit = jjcoefstatsBase,
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




            # ggcoefstats
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggcoefstats.html


            ggcoefstats(
                x,
                output = "plot",
                statistic = NULL,
                bf.message = TRUE,
                effsize = "eta",
                partial = TRUE,
                meta.analytic.effect = FALSE,
                meta.type = "parametric",
                conf.int = TRUE,
                conf.level = 0.95,
                k = 2,
                exclude.intercept = TRUE,
                exponentiate = FALSE,
                sort = "none",
                xlab = "regression coefficient",
                ylab = "term",
                title = NULL,
                subtitle = NULL,
                only.significant = FALSE,
                caption = NULL,
                caption.summary = TRUE,
                point.args = list(size = 3, color = "blue"),
                errorbar.args = list(height = 0),
                vline = TRUE,
                vline.args = list(size = 1, linetype = "dashed"),
                stats.labels = TRUE,
                stats.label.color = NULL,
                stats.label.args = list(size = 3, direction = "y"),
                package = "RColorBrewer",
                palette = "Dark2",
                ggtheme = ggplot2::theme_bw(),
                ggstatsplot.layer = TRUE,
                ...
            )







            # Print Plot ----

            print(plot)
            TRUE


        }
    )
)



