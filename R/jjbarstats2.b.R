#' @title Bar Charts
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjbarstats2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbarstats2Class",
    inherit = jjbarstats2Base,
    private = list(

        .run = function() {


            todo <- glue::glue(
                "<br>You have selected to use a barplot to compare a categorical variable with another.<br><hr>")

            self$results$todo$setContent(todo)



            mydata <- self$data


            dep <- self$options$dep

            # group <- self$options$group



            dep <- unlist(dep)

            deneme <- paste0(dep)

            self$results$text$setContent(deneme)






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


            # direction, paired ----

            direction <- self$options$direction

            if (direction == "repeated") {

                paired <- TRUE

            } else if (direction == "independent") {

                paired <- FALSE

            }


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


            dep <- jmvcore::composeTerms(listOfComponents = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggbarstats ----
            # bar charts for categorical data
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html



            plot <-
                ggstatsplot::ggbarstats(
                    data = mydata,
                    main = !!dep,
                    condition = !!group,

                    paired = paired,


                    counts = NULL,
                    ratio = NULL,
                    results.subtitle = TRUE,
                    sample.size.label = TRUE,
                    label = "percentage",
                    perc.k = 0,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = TRUE,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = 0.95,
                    nboot = 100,
                    legend.title = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    k = 2,
                    proportion.test = TRUE,
                    ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = TRUE,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE,
                    x = NULL,
                    y = NULL
                )


            # Print Plot ----

            print(plot)
            TRUE

        }


        ,             .plot2 = function(image, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data


            # direction, paired ----

            direction <- self$options$direction

            if (direction == "repeated") {

                paired <- TRUE

            } else if (direction == "independent") {

                paired <- FALSE

            }

            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)




            # grouped_ggbarstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html



            if ( !is.null(self$options$grvar) ) {
                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggbarstats(
                    data = mydata,
                    main = !!dep,
                    condition = !!group,
                    grouping.var = !!grvar,

                    paired = paired,


                    counts = NULL,
                    title.prefix = NULL,
                    output = "plot",
                    x = NULL,
                    y = NULL,
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
