#' @title Violin Plot
#' @return Violin Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjviolinClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjviolinClass",
    inherit = jjviolinBase,
    private = list(
        .run = function() {

            # Error Message ----

            if ( is.null(self$options$dep) || is.null(self$options$group) ) {

                # ToDo Message ----
                todo <- glue::glue("
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a Violin Plot.
                          <br><hr>
                          ")

                html <- self$results$todo
                html$setContent(todo)
                return()


            } else {



                # themex <- "theme_dark()"
                # self$options$themex
                # themex <- jmvcore::composeTerm(themex)
                # themex <- jmvcore::decomposeTerm(themex)
                # themex <- paste0("ggplot2::", themex)
                #
                # themex <- as.formula(themex)
                #
                # todo <- themex






                todo <- ""
                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


                # image <- self$results$plot
                # image$setState(plotData)

                }
        },


        .plot = function(image, ...) {

            # plotData <- image$state


            # Error Message ----

            if ( is.null(self$options$dep) || is.null(self$options$group) )
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")



            # Prepare Data ----

            mydata <- self$data

            # Exclude NA ----

            excl <- self$options$excl
            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Define variables for arguments ----

            dep <- self$options$dep
            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            if ( is.null(self$options$fill)) {

            fill <- group

            } else {

                fill2 <-  self$options$fill
                fill <- jmvcore::composeTerm(components = fill2)

            }


            if ( is.null(self$options$col)) {

                col <- group

            } else {

                col2 <-  self$options$col
                col <- jmvcore::composeTerm(components = col2)

            }



            # Plot function ----

            plot <- mydata %>%
                ggplot2::ggplot(.,
                                ggplot2::aes(x = .data[[group]]
                                             , y = .data[[dep]]
                                             , fill = .data[[fill]]
                                             , col = .data[[col]]

                                            )
                                    ) +
                ggplot2::geom_violin()



            # Flip Coordinates ----

            flip <-  self$options$flip

            if (flip) {

            plot <- plot +
                ggplot2::coord_flip()

            }


            # Themes ----

            themex <- self$options$themex

            # plot <- plot + eval(parse(text=self$options$themex))

            if (themex == "ipsum") {
                plot <- plot + hrbrthemes::theme_ipsum()
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



            # xlab ----

            usexlabel <- self$options$usexlabel

            if (usexlabel) {

            xlabel <- self$options$xlabel

            plot <- plot + xlab(xlabel)

            }




            # ylab ----

            #     ylab("Assigned Probability (%)")



            #     geom_violin(width=2.1, size=0.2) +
            #     scale_fill_viridis(discrete=TRUE) +
            #     scale_color_viridis(discrete=TRUE) +
            #     theme_ipsum() +



            # Grouped ----
            # # Libraries
            # library(ggplot2)
            # library(dplyr)
            # library(forcats)
            # library(hrbrthemes)
            # library(viridis)
            #
            # # Load dataset from github
            # data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/10_OneNumSevCatSubgroupsSevObs.csv", header=T, sep=",") %>%
            #     mutate(tip = round(tip/total_bill*100, 1))
            #
            # # Grouped
            # data %>%
            #     mutate(day = fct_reorder(day, tip)) %>%
            #     mutate(day = factor(day, levels=c("Thur", "Fri", "Sat", "Sun"))) %>%
            #     ggplot(aes(fill=sex, y=tip, x=day)) +
            #     geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
            #     scale_fill_viridis(discrete=T, name="") +
            #     theme_ipsum()  +
            #     xlab("") +
            #     ylab("Tip (%)") +
            #     ylim(0,40)




            # add boxplot ----

            # # Libraries
            # library(ggplot2)
            # library(dplyr)
            # library(hrbrthemes)
            # library(viridis)
            #
            # # create a dataset
            # data <- data.frame(
            #     name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
            #     value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
            # )
            #
            # # sample size
            # sample_size = data %>% group_by(name) %>% summarize(num=n())
            #
            # # Plot
            # data %>%
            #     left_join(sample_size) %>%
            #     mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
            #     ggplot( aes(x=myaxis, y=value, fill=name)) +
            #     geom_violin(width=1.4) +
            #     geom_boxplot(width=0.1, color="grey", alpha=0.2) +
            #     scale_fill_viridis(discrete = TRUE) +
            #     theme_ipsum() +
            #     theme(
            #         legend.position="none",
            #         plot.title = element_text(size=11)
            #     ) +
            #     ggtitle("A Violin wrapping a boxplot") +
            #     xlab("")



            # vioplot ----
            # library(vioplot)
            #
            # # Create data
            # treatment <- c(rep("A", 40) , rep("B", 40) , rep("C", 40) )
            # value <- c( sample(2:5, 40 , replace=T) , sample(c(1:5,12:17), 40 , replace=T), sample(1:7, 40 , replace=T) )
            # data <- data.frame(treatment,value)
            #
            # # Draw the plot
            # with(data , vioplot(
            #     value[treatment=="A"] , value[treatment=="B"], value[treatment=="C"],
            #     col=rgb(0.1,0.4,0.7,0.7) , names=c("A","B","C")
            # ))











            # Print plot

            print(plot)
            TRUE
        }
        )
)
