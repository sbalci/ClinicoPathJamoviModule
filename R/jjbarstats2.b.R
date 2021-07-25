#' @title Bar Charts
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'
#'

jjbarstats2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbarstats2Class",
    inherit = jjbarstats2Base,
    private = list(

        .run = function() {

            todo <- glue::glue(
                "<br>You have selected to use a barplot to compare a categorical variable with another.<br><hr>")

            self$results$todo$setContent(todo)

            # mydata <- self$data

            originaltheme <- self$options$originaltheme


            dep <- self$options$dep

            dep2 <- jmvcore::composeTerms(listOfComponents = dep)

            group <- self$options$group

            group <- jmvcore::composeTerm(components = group)

            group2 <- group %||% "NULL"

            deneme <- list(
                "self$options$dep" = self$options$dep,
                "typeof" = typeof(self$options$dep),
                "class" = class(self$options$dep),
                "print" = print(self$options$dep),
                "is.null" = is.null(self$options$dep),
                "as.vector" = as.vector(self$options$dep),
                "unlist" = unlist(self$options$dep),

                "dep" = dep,
                "typeof" = typeof(dep),
                "class" = class(dep),
                "print" = print(dep),
                "is.null" = is.null(dep),
                "as.vector" = as.vector(dep),
                "unlist" = unlist(dep),

                "dep2" = dep2,
                "typeof" = typeof(dep2),
                "class" = class(dep2),
                "print" = print(dep2),
                "is.null" = is.null(dep2),
                "as.vector" = as.vector(dep2),
                "unlist" = unlist(dep2),

                "self$options$group" = self$options$group,
                "typeof" = typeof(self$options$group),
                "class" = class(self$options$group),
                "print" = print(self$options$group),
                "is.null" = is.null(self$options$group),
                "as.vector" = as.vector(self$options$group),
                "unlist" = unlist(self$options$group),

                "group" = group,
                "typeof" = typeof(group),
                "class" = class(group),
                "print" = print(group),
                "is.null" = is.null(group),
                "as.vector" = as.vector(group),
                "unlist" = unlist(group),

                "group2" = group2,
                "typeof" = typeof(group2),
                "class" = class(group2),
                "print" = print(group2),
                "is.null" = is.null(group2),
                "as.vector" = as.vector(group2),
                "unlist" = unlist(group2),

                "paste" = paste0("dep: ", dep, " and ", "group: ", group)
            )

            self$results$text$setContent(deneme)

        }

        # ,
        # .plot = function(image, ggtheme, theme, ...) {
        #
        #     mydata <- self$data
        #
        #     dep <- self$options$dep
        #
        #     dep <- jmvcore::composeTerms(listOfComponents = dep)
        #
        #     group <- self$options$group
        #
        #     group <- jmvcore::composeTerm(components = group)
        #
        #     if (group == "") group <- NULL
        #
        #     plot <-
        #         ggstatsplot::ggpiestats(
        #             data = mydata,
        #             main = !!dep,
        #             condition = !!group
        #           , ggtheme = ggtheme
        #             )
        #
        #     # Print Plot ----
        #
        #     print(plot)
        #     TRUE
        #
        # }



        # Repeating function execution across multiple columns in a dataframe

        # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/purrr_examples.html#repeating-function-execution-across-multiple-columns-in-a-dataframe-1



        ,
        .plot2 = function(image, ggtheme, theme, ...) {

            mydata <- self$data

            dep <- self$options$dep

            dep1 <- jmvcore::composeTerms(listOfComponents = dep)


            group <- self$options$group

            group <- jmvcore::composeTerm(components = group)

            if (group == "") group <- NULL


            if ( length(self$options$dep) == 1 ) {

            plot2 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    main = !!dep1,
                    condition = !!group
                    , ggtheme = ggtheme

                )
            }


            if ( length(self$options$dep) > 1 ) {

                dep2 <- as.list(self$options$dep)

                # running the same analysis on two different columns (creates a list of plots)
                plotlist <-
                    purrr::pmap(
                        .l = list(
                            main = dep2,
                            # title = list(dep),
                            messages = FALSE
                        ),
                        .f = ggstatsplot::ggpiestats,
                        data = mydata,
                        condition = !!group
                    )

                # combine plots using `patchwork`
                # plot2 <- plotlist[[1]] + plotlist[[2]]

                # plotname <- "plotlist[[1]] + "

                # if ( length(plotlist) > 1 ) {
                    # for (i in 2:length(plotlist)) {
                    #     newplot <- paste0("plotlist[[", i, "]]")
                    #     plotname <- paste(plotname, newplot, sep = " + ")
                    # }

                # plot2 <- eval(parse(text = plotname))

                plot2 <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    nrow = 2
                    # ncol = 1
                    )
                                # }
                            }



            # movies_long <- ggstatsplot::movies_long

            # # running the same analysis on two different columns (creates a list of plots)
            # plotlist <-
            #     purrr::pmap(
            #         .l = list(
            #             data = list(movies_long),
            #             x = "mpaa",
            #             y = list("rating", "length"),
            #             title = list("IMDB score by MPAA rating", "Movie length by MPAA rating"),
            #             messages = FALSE
            #         ),
            #         .f = ggstatsplot::ggbetweenstats
            #     )
            #
            # # combine plots using `patchwork`
            # plot2 <- plotlist[[1]] + plotlist[[2]]

            # Print Plot ----
            print(plot2)
            TRUE
        }
    )
)
