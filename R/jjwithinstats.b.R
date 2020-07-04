#' @title Violin Plots to Compare Within Group
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(

        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Violin Plots for repeated measurements.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.html' target='_blank'>grouped_ggwithinstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                "<br>You have selected to use a Violin Plots to Compare repeated measurements.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }



        ,
        .plot = function(image, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----


            # # direction, paired ----
            #
            # direction <- self$options$direction
            #
            # if (direction == "repeated") {
            #
            #     paired <- TRUE
            #
            # } else if (direction == "independent") {
            #
            #     paired <- FALSE
            #
            # }


            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw


            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            # mydep <- mydata[[self$options$dep]]
            # mygroup <- mydata[[self$options$group]]


            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggwithinstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html



            plot <-
                ggstatsplot::ggwithinstats(
                    data = mydata,
                    x = !!group,
                    y = !!dep,
                    type = "parametric",
                    pairwise.comparisons = FALSE,
                    pairwise.display = "significant",
                    p.adjust.method = "holm",
                    effsize.type = "unbiased",
                    partial = TRUE,
                    bf.prior = 0.707,
                    bf.message = TRUE,
                    sphericity.correction = TRUE,
                    results.subtitle = TRUE,
                    xlab = NULL,
                    ylab = NULL,
                    caption = NULL,
                    title = NULL,
                    subtitle = NULL,
                    sample.size.label = TRUE,
                    k = 2,
                    conf.level = 0.95,
                    nboot = 100,
                    tr = 0.1,
                    mean.plotting = TRUE,
                    mean.ci = FALSE,
                    mean.point.args = list(size = 5, color = "darkred"),
                    mean.label.args = list(size = 3),
                    point.path = TRUE,
                    point.path.args = list(alpha = 0.5, linetype = "dashed"),
                    mean.path = TRUE,
                    mean.path.args = list(color = "red", size = 1, alpha = 0.5),
                    notch = FALSE,
                    notchwidth = 0.5,
                    outlier.tagging = FALSE,
                    outlier.label = NULL,
                    outlier.coef = 1.5,
                    outlier.label.args = list(),
                    outlier.point.args = list(),
                    violin.args = list(width = 0.5, alpha = 0.2),
                    ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = TRUE,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE
                )



            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data


            # # direction, paired ----
            #
            # direction <- self$options$direction
            #
            # if (direction == "repeated") {
            #
            #     paired <- TRUE
            #
            # } else if (direction == "independent") {
            #
            #     paired <- FALSE
            #
            # }

            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)




            # grouped_ggwithinstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.html



            if ( !is.null(self$options$grvar) ) {
                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggwithinstats(
                    data = mydata,
                    x = !!group,
                    y = !!dep,
                    grouping.var = !!grvar,
                    outlier.label = NULL,
                    title.prefix = NULL,
                    output = "plot",
                    plotgrid.args = list(),
                    title.text = NULL,
                    title.args = list(size = 16, fontface = "bold"),
                    caption.text = NULL,
                    caption.args = list(size = 10),
                    sub.text = NULL,
                    sub.args = list(size = 12)
                )


            }

            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)
