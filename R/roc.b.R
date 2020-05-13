#' ROC Analysis
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import plotROC


rocClass <- if (requireNamespace("jmvcore")) R6::R6Class("rocClass", inherit = rocBase,
    private = list(.run = function() {



        # TODO

        todo <- glue::glue("This Module is still under development
                -
                -
                ")

        self$results$todo$setContent(todo)


        # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

        # plotROC
        #
        # http://sachsmc.github.io/plotROC/


        set.seed(2529)
        D.ex <- rbinom(200, size = 1, prob = .5)
        M1 <- rnorm(200, mean = D.ex, sd = .65)
        M2 <- rnorm(200, mean = D.ex, sd = 1.5)

        plotData <- data.frame(D = D.ex,
                               D.str = c("Healthy", "Ill")[D.ex + 1],
                               M1 = M1,
                               M2 = M2,
                               stringsAsFactors = FALSE)



        # Prepare plot data

        image <- self$results$plot
        image$setState(plotData)


        plot3 <- private$.plot2()

        self$results$plot3$setContent(plot3)





    },

    .plot=function(image, ...) {

        plotData <- image$state


        set.seed(2529)
        D.ex <- rbinom(200, size = 1, prob = .5)
        M1 <- rnorm(200, mean = D.ex, sd = .65)
        M2 <- rnorm(200, mean = D.ex, sd = 1.5)

        plotData <- data.frame(D = D.ex,
                               D.str = c("Healthy", "Ill")[D.ex + 1],
                               M1 = M1,
                               M2 = M2,
                               stringsAsFactors = FALSE)

        plot <- plotData %>%
            ggplot2::ggplot(.,
                                ggplot2::aes(d = D, m = M1)
                                ) +
            plotROC::geom_roc(
                labels = TRUE,
                n.cuts = 5,
                labelsize = 5,
                labelround = 2
            ) +
            plotROC::style_roc(
                theme = theme_grey,
                xlab = "1 - Specificity"
            ) +
            plotROC::geom_rocci(
                sig.level = .01,
                ci.at = quantile(M1, c(.1, .4, .5, .6, .9))
            )



        plotROC::direct_label(
            ggroc_p = plot,
            labels = "Biomarker",
            label.angle = 45,
            nudge_x = 0,
            nudge_y = -.1,
            size = 6
            ) +
            plotROC::style_roc()



        print(plot)
        TRUE
    } ,

    .plot2=function() {

        # plotData <- image$state


        set.seed(2529)
        D.ex <- rbinom(200, size = 1, prob = .5)
        M1 <- rnorm(200, mean = D.ex, sd = .65)
        M2 <- rnorm(200, mean = D.ex, sd = 1.5)

        plotData <- data.frame(D = D.ex,
                               D.str = c("Healthy", "Ill")[D.ex + 1],
                               M1 = M1,
                               M2 = M2,
                               stringsAsFactors = FALSE)

        plot2 <- plotData %>%
            ggplot2::ggplot(.,
                            ggplot2::aes(d = D, m = M1)
            ) +
            plotROC::geom_roc()


    # Interactive Plots

        plot2 <- plotROC::plot_interactive_roc(plot2)
        # opens in new html


        # plot2 <- plotROC::export_interactive_roc(plot2)
        # no output


        # plot2 <- cat(plotROC::export_interactive_roc(plot2))
        # no output

        knitr::asis_output(plot2)
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

