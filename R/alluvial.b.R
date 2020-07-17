#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
#'

alluvialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "alluvialClass",
    inherit = alluvialBase,
    private = list(
        .run = function() {

            # Error Message ----


            if (is.null(self$options$vars)) {
                # ToDo Message ----
                todo <- "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you form Alluvial Diagrams (Alluvial Plots).
                <hr><br>
                "

                html <- self$results$todo
                html$setContent(todo)

            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)



                if (nrow(self$data) == 0) stop("Data contains no (complete) rows")



            }

        }

            ,

            .plot = function(image, ggtheme, theme, ...) {
                # the plot function ----


             #Errors ----

                if (is.null(self$options$vars) )
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            # Prepare Data ----

            varsName <- self$options$vars

            mydata <- jmvcore::select(self$data, c(varsName))

            fill <- jmvcore::composeTerm(self$options$fill)


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # verbose ----
            # verbose <- FALSE
            # verb <- self$options$verb
            # if (isTRUE(verb)) verbose <- TRUE

            # fill_by ----

            fill <- jmvcore::composeTerm(self$options$fill)


            #bin ----

            bin <- self$options$bin

            if (bin == "default") bin <- c("LL", "ML", "M", "MH", "HH")



            # easyalluvial ----
            # https://erblast.github.io/easyalluvial/

            plot <-
                easyalluvial::alluvial_wide( data = mydata,
                                             max_variables = 8,
                                             fill_by = fill,
                                             verbose = TRUE,
                                             # verbose = verb,
                                             bin_labels = bin
                )


            # marginal table ----

            marg <- self$options$marg

            if (marg) {
                plot <- easyalluvial::add_marginal_histograms(p = plot,
                                                              data_input = mydata,
                                                              keep_labels = TRUE,
                                                              top = TRUE,
                                                              plot = TRUE)
                    }


            # flip coordinates ----

            flip <- self$options$flip

            if (flip) {
                plot <- plot +
                    ggplot2::coord_flip()
                    # ggplot2::theme_minimal()
            }




            # select theme ----

            themex <- self$options$themex


            if (themex == "jamovi") {
                plot <- plot + ggtheme
            } else if (marg || themex == "easyalluvial") {
                plot <- plot
            # } else if (themex == "ipsum") {
            #     plot <- plot + hrbrthemes::theme_ipsum()
            } else if (themex == "grey") {
                plot <- plot + ggplot2::theme_grey()
            } else if (themex == "gray") {
                plot <- plot + ggplot2::theme_gray()
            } else if (themex == "bw") {
                plot <- plot + ggplot2::theme_bw()
            } else if (themex == "linedraw") {
                plot <- plot + ggplot2::theme_linedraw()
            } else if (themex == "light") {
                plot <- plot + ggplot2::theme_light()
            } else if (themex == "dark") {
                plot <- plot + ggplot2::theme_dark()
            } else if (themex == "minimal") {
                plot <- plot + ggplot2::theme_minimal()
            } else if (themex == "classic") {
                plot <- plot + ggplot2::theme_classic()
            } else if (themex == "void") {
                plot <- plot + ggplot2::theme_void()
            } else if (themex == "test") {
                plot <- plot + ggplot2::theme_test()
            }

            # originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # }


            # add title ----

            mytitle <- self$options$mytitle

            # mytitle <- jmvcore::composeTerm(components = mytitle)


            # use title ----

            usetitle <- self$options$usetitle

            if (usetitle) {
                plot <- plot +
                    ggplot2::ggtitle(mytitle)
            }



            # Print Plot ----
            print(plot)
            TRUE
        }

        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # the plot function ----

            #Errors ----

            if (is.null(self$options$condensationvar) || is.null(self$options$vars))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare Data ----

            condvarName <- self$options$condensationvar

            condvarName <- jmvcore::composeTerm(components = condvarName)

            mydata <- self$data


            # easyalluvial ----

            plot2 <-
                easyalluvial::plot_condensation(df = mydata,
                                                first = .data[[condvarName]])

            # Print Plot ----
            print(plot2)
            TRUE
        }
        )
)




# #Errors ----
#
# if (is.null(self$options$vars) )
#     return()
#
# if (nrow(self$data) == 0)
#     stop('Data contains no (complete) rows')
#
# # Prepare Data ----
#
# varsName <- self$options$vars
#
# mydata <- jmvcore::select(self$data, c(varsName))
#
#
#
# # Exclude NA ----
#
# excl <- self$options$excl
#
# if (excl) {mydata <- jmvcore::naOmit(mydata)}
#
#
# # easyalluvial ----
# # https://erblast.github.io/easyalluvial/
#
# plot <-
#     easyalluvial::alluvial_wide( data = mydata,
#                                  max_variables = 6,
#                                  fill_by = 'first_variable'
#     )
#
#
# # marginal table ----
#
# marg <- self$options$marg
#
# if (marg) {
#     plot <- plot %>%
#         easyalluvial::add_marginal_histograms(mydata)
# }
#
#
#
# # Interactive ----
#
# inter <- self$options$inter
#
# if (inter) {
#     plot <-
#         parcats::parcats(plot,
#                          data_input = mydata)
# }
#
#
# plothtml <- plot
#
# html <- self$results$plothtml
# html$setContent(plothtml)
#

# # Interactive ----
#
# inter <- self$options$inter
#
# if (inter) {
#     plot <-
#         parcats::parcats(plot,
#                          data_input = mydata)
# }




