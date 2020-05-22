#' @title ROC Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


rocClass <- if (requireNamespace("jmvcore")) R6::R6Class("rocClass", inherit = rocBase,
    private = list(.run = function() {

        # Error Message ----

        if (nrow(self$data) == 0) stop("Data contains no (complete) rows")



        if (is.null(self$options$measurement) || is.null(self$options$status) ) {

            # ToDo Message ----


            todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a ROC Curve.
                          "

            html <- self$results$todo
            html$setContent(todo)

        } else {

            todo <- ""
            html <- self$results$todo
            html$setContent(todo)



            #Errors ----


            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare Data ----


            varMeasure <- self$options$measurement
            varStatus <- self$options$status

            mydata <- jmvcore::select(self$data, c(varMeasure, varStatus))



            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            self$results$text$setContent(head(mydata))



            # plotROC
            #
            # http://sachsmc.github.io/plotROC/


            # plot <- mydata %>%
            #     ggplot2::ggplot(.,
            #                     ggplot2::aes(d = .data[[varStatus]], m = .data[[varMeasure]])
            #     ) +
            #     plotROC::geom_roc(
            #         labels = TRUE,
            #         n.cuts = 5,
            #         labelsize = 5,
            #         labelround = 2
            #     )


            # sty <- self$options$sty
            #
            # if (sty) {
            #
            #     plot <- plot +
            #         plotROC::style_roc(
            #             theme = theme_grey,
            #             xlab = "1 - Specificity"
            #         )
            #
            # }


            # quant <- self$options$quant
            # if (quant) {
            #     plot <- plot +
            #         plotROC::geom_rocci(
            #             sig.level = .01,
            #             ci.at = quantile(varMeasure, c(.1, .4, .5, .6, .9))
            #         )
            # }



            # label <- self$options$label
            #
            # if (label) {
            #     plot <-
            #         plotROC::direct_label(
            #             ggroc_p = plot,
            #             labels = "Biomarker",
            #             label.angle = 45,
            #             nudge_x = 0,
            #             nudge_y = -.1,
            #             size = 6
            #         )
            #     # +
            #     # plotROC::style_roc()
            #
            #
            # }


            # inter <- self$options$inter
            #
            # if (inter) {
            #
            # plot <- mydata %>%
            #     ggplot2::ggplot(.,
            #                     ggplot2::aes(d = .data[[varStatus]], m = .data[[varMeasure]])
            #     ) +
            #     plotROC::geom_roc(
            #         # labels = TRUE,
            #         # n.cuts = 5,
            #         # labelsize = 5,
            #         # labelround = 2
            #     )
            #
            #
            # # Interactive Plots
            #
            # plot <- plotROC::plot_interactive_roc(plot)
            #
            #
            # # opens in new html
            #
            #
            # # plot2 <- plotROC::export_interactive_roc(plot2)
            # # no output
            #
            #
            # # plot2 <- cat(plotROC::export_interactive_roc(plot2))
            # # no output
            #
            # # knitr::asis_output(plot2)
            #
            # }


            # plot <- knitr::asis_output(plot)


            # self$result$textplot$setContent(plot)



        # Prepare plot data

        # image <- self$results$plot
        # image$setState(plotData)


        # plot3 <- private$.plot2()
        # self$results$plot3$setContent(plot3)


}


    },

    .plot=function(image, ...) {


        # Error Messages

        if (is.null(self$options$measurement) || is.null(self$options$status) )
            return()

        if (nrow(self$data) == 0)
            stop('Data contains no (complete) rows')

        # Prepare Data ----


        varMeasure <- self$options$measurement
        varStatus <- self$options$status

        mydata <- jmvcore::select(self$data, c(varMeasure, varStatus))



        # Exclude NA ----

        excl <- self$options$excl

        if (excl) {mydata <- jmvcore::naOmit(mydata)}



        # set.seed(2529)
        # D.ex <- rbinom(250, size = 1, prob = .5)
        # M1 <- rnorm(250, mean = D.ex, sd = .65)
        # M2 <- rnorm(250, mean = D.ex, sd = 1.5)
        #
        # plotData <- data.frame(D = D.ex,
        #                        D.str = c("Healthy", "Ill")[D.ex + 1],
        #                        M1 = M1,
        #                        M2 = M2,
        #                        stringsAsFactors = FALSE)

        plot <- mydata %>%
            ggplot2::ggplot(.,
                            ggplot2::aes(d = .data[[varStatus]], m = .data[[varMeasure]])
                            ) +
            plotROC::geom_roc(
                labels = TRUE,
                n.cuts = 5,
                labelsize = 5,
                labelround = 2
            )


        sty <- self$options$sty

        if (sty) {

            plot <- plot +
            plotROC::style_roc(
                theme = theme_grey,
                xlab = "1 - Specificity"
            )

        }


        quant <- self$options$quant

        if (quant) {
            plot <- plot +
            plotROC::geom_rocci(
                sig.level = .01,
                ci.at = quantile(varMeasure, c(.1, .4, .5, .6, .9))
            )
        }



        label <- self$options$label

        if (label) {
            plot <-
        plotROC::direct_label(
            ggroc_p = plot,
            labels = "Biomarker",
            label.angle = 45,
            nudge_x = 0,
            nudge_y = -.1,
            size = 6
            )
            # +
            # plotROC::style_roc()


        }




        print(plot)
        TRUE
    }


    ,

    .plot2=function(image, ...) {



        # Error Messages

        if (is.null(self$options$measurement) || is.null(self$options$status) )
            return()

        if (nrow(self$data) == 0)
            stop('Data contains no (complete) rows')

        # Prepare Data ----


        varMeasure <- self$options$measurement
        varStatus <- self$options$status

        mydata <- jmvcore::select(self$data, c(varMeasure, varStatus))



        # Exclude NA ----

        excl <- self$options$excl

        if (excl) {mydata <- jmvcore::naOmit(mydata)}




        plot2 <- mydata %>%
            ggplot2::ggplot(.,
                            ggplot2::aes(d = .data[[varStatus]], m = .data[[varMeasure]])
            ) +
            plotROC::geom_roc(
                # labels = TRUE,
                # n.cuts = 5,
                # labelsize = 5,
                # labelround = 2
            )


    # Interactive Plots

        plot2 <- plotROC::plot_interactive_roc(plot2)


        # opens in new html


        # plot2 <- plotROC::export_interactive_roc(plot2)
        # no output


        # plot2 <- cat(plotROC::export_interactive_roc(plot2))
        # no output

        knitr::asis_output(plot2)

        # print(plot2)
        # TRUE


    }


    # Multiple ROC Curves
    # http://sachsmc.github.io/plotROC/

    # New Features

    # Advanced Options


    )
    )




# Other ROC Packages on CRAN
#
# AROC: Covariate-Adjusted Receiver Operating Characteristic Curve Inference
# https://cran.r-project.org/web/packages/AROC/index.html

