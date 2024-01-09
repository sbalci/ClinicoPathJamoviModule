#' @title Correlation Matrix
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjcorrmatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcorrmatClass",
    inherit = jjcorrmatBase,
    private = list(


            # init ----
            .init = function() {

                deplen <- length(self$options$dep)

                self$results$plot$setSize(600, 450)


                if (!is.null(self$options$grvar)) {

                    mydata <- self$data

                    grvar <-  self$options$grvar

                    num_levels <- nlevels(
                        as.factor(mydata[[grvar]])
                    )

                    self$results$plot2$setSize(num_levels * 600, 450)

                }



            }

            # run ----
            ,
            .run = function() {

            # Initial Message ----
            if ( length(self$options$dep) <= 1 ) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }



        ,
        .plot = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( length(self$options$dep) <= 1 )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----


            mydata <- self$data


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)





            # define main arguments ----

            myvars <- jmvcore::constructFormula(terms = self$options$dep)

            myvars <- jmvcore::decomposeFormula(formula = myvars)

            myvars <- unlist(myvars)

            # originaltheme <- self$options$originaltheme


            # ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html



            plot <- ggstatsplot::ggcorrmat(
                data = mydata,
                cor.vars = myvars,
                cor.vars.names = NULL,
                output = "plot",
                matrix.type = "full",
                matrix.method = "square",

                type = typestatistics,

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

                ggplot.component = NULL,
                title = NULL,
                subtitle = NULL,
                caption = NULL,
                messages = TRUE
            )


            # originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # } else {
            #     plot <- plot + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }



            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)




            # define main arguments ----

            myvars <- jmvcore::constructFormula(terms = self$options$dep)

            myvars <- jmvcore::decomposeFormula(formula = myvars)

            myvars <- unlist(myvars)

            # originaltheme <- self$options$originaltheme


            # grouped_ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html



            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggcorrmat(
                    data = mydata,
                    cor.vars = myvars,
                    cor.vars.names = NULL,
                    grouping.var = !!rlang::sym(grvar),
                    title.prefix = NULL,
                    output = "plot",
                    plotgrid.args = list(),
                    title.text = NULL,
                    title.args = list(size = 16, fontface = "bold"),
                    caption.text = NULL,
                    caption.args = list(size = 10),
                    sub.text = NULL,
                    sub.args = list(size = 12)
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = originaltheme
                    , type = typestatistics



                )


            }


            # originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # } else {
            #     plot <- plot + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }

            # Print Plot ----

            print(plot2)
            TRUE

        }

    )
)







