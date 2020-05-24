#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
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
                          This tool will help you form an Alluvial Plots.
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

            .plot = function(image, ...) {
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



            # verbose ----

            verbose <- FALSE

            verb <- self$options$verb

            if (isTRUE(verb)) verbose <- TRUE

            # fill_by ----

            fill <- jmvcore::composeTerm(self$options$fill)


            #bin ----

            bin <- self$options$bin

            if (bin == "default") bin <- c("LL", "ML", "M", "MH", "HH")

            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # easyalluvial ----
            # https://erblast.github.io/easyalluvial/

            plot <-
                easyalluvial::alluvial_wide( data = mydata,
                                             max_variables = 6,
                                             fill_by = fill,
                                             verbose = verbose,
                                             bin_labels = bin
                )


            # marginal table ----

            marg <- self$options$marg

            if (marg) {
                plot <- plot %>%
                    easyalluvial::add_marginal_histograms(mydata)
                    }

            # Print Plot ----
            print(plot)
            TRUE
        }

        ,

        .plot2 = function(image, ...) {
            # the plot function ----

            #Errors ----

            if (is.null(self$options$condensationvar) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare Data ----

            condvarName <- self$options$condensationvar

            condvarName <- jmvcore::composeTerm(components = condvarName)

            mydata2 <- self$data


            # easyalluvial ----

            plot2 <-
                easyalluvial::plot_condensation(df = mydata2,
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




