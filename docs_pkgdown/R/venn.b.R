#' @title Venn Diagram
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggvenn


vennClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "vennClass",
        inherit = vennBase,
        private = list(
            .run = function() {
                # Error Message ----


                if (is.null(self$options$var1) || is.null(self$options$var2)) {
                    # ToDo Message ----
                    todo <- "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you form Venn Diagrams.
                <hr><br>
                "

                    html <- self$results$todo
                    html$setContent(todo)

                } else {
                    todo <- ""
                    html <- self$results$todo
                    html$setContent(todo)



                    if (nrow(self$data) == 0)
                        stop("Data contains no (complete) rows")


                    # Read data ----

                    mydata <- self$data

                    var1 <- self$options$var1
                    var1true <- self$options$var1true

                    var2 <- self$options$var2
                    var2true <- self$options$var2true

                    var3 <- self$options$var3
                    var3true <- self$options$var3true

                    var4 <- self$options$var4
                    var4true <- self$options$var4true

                    # mydata <- jmvcore::select(df = mydata,
                    #                           columnNames = c(var1, var2, var3, var4))

                    mydata <- jmvcore::naOmit(mydata)


                    if (!is.null(self$options$var1)) {
                        mydata[[var1]] <-
                            ifelse(test = mydata[[var1]] == var1true, TRUE, FALSE)
                    }

                    if (!is.null(self$options$var2)) {
                        mydata[[var2]] <-
                            ifelse(test = mydata[[var2]] == var2true, TRUE, FALSE)
                    }

                    if (!is.null(self$options$var3)) {
                        mydata[[var3]] <-
                            ifelse(test = mydata[[var3]] == var3true, TRUE, FALSE)
                    }

                    if (!is.null(self$options$var4)) {
                        mydata[[var4]] <-
                            ifelse(test = mydata[[var4]] == var4true, TRUE, FALSE)
                    }

                    # myoutput1 <- list(
                    #     "mydata" = head(mydata),
                    #     "names" = names(mydata)
                    # )


                    # self$results$output1$setContent(myoutput1)


                    plotData <- list("mydata" = mydata,
                                     "names" = names(mydata)
                                     )

                    image <- self$results$plot
                    image$setState(plotData)


                }

            }

            ,

            .plot = function(image, ggtheme, theme, ...) {
                # the plot function ----


                #Errors ----

                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # Prepare Data ----

                results <- image$state

                mydata2 <- results$mydata

                namescolumn2 <- results$names

                # Venn Diagram by ggplot2, with really easy-to-use API.
                # https://github.com/yanlinlin82/ggvenn

                plot <-
                    ggvenn::ggvenn(data = mydata2,
                                   columns = namescolumn2)


                plot <-
                    plot +
                    ggtheme +
                    ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                                   axis.text.x = ggplot2::element_blank(),
                                   axis.ticks.x = ggplot2::element_blank(),
                                   axis.title.x = ggplot2::element_blank(),
                                   axis.line.y = ggplot2::element_blank(),
                                   axis.text.y = ggplot2::element_blank(),
                                   axis.ticks.y = ggplot2::element_blank(),
                                   axis.title.y = ggplot2::element_blank()
                                   )

                # Print Plot ----
                print(plot)
                TRUE
            }
        )
    )
