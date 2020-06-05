#' @title Violin Plot
#' @return Violin Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jviolinClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jviolinClass",
    inherit = jviolinBase,
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


            useylabel <- self$options$useylabel

            if (useylabel) {

                ylabel <- self$options$ylabel

                plot <- plot + xlab(ylabel)

            }


            # geom_violin options ----


            #     geom_violin(width=2.1, size=0.2)


#
#
#             geom_violin {ggplot2}	R Documentation
#             Violin plot
#             Description
#             A violin plot is a compact display of a continuous distribution. It is a blend of geom_boxplot() and geom_density(): a violin plot is a mirrored density plot displayed in the same way as a boxplot.
#
#             Usage
#             geom_violin(
#                 mapping = NULL,
#                 data = NULL,
#                 stat = "ydensity",
#                 position = "dodge",
#                 ...,
#                 draw_quantiles = NULL,
#                 trim = TRUE,
#                 scale = "area",
#                 na.rm = FALSE,
#                 orientation = NA,
#                 show.legend = NA,
#                 inherit.aes = TRUE
#             )
#
#             stat_ydensity(
#                 mapping = NULL,
#                 data = NULL,
#                 geom = "violin",
#                 position = "dodge",
#                 ...,
#                 bw = "nrd0",
#                 adjust = 1,
#                 kernel = "gaussian",
#                 trim = TRUE,
#                 scale = "area",
#                 na.rm = FALSE,
#                 orientation = NA,
#                 show.legend = NA,
#                 inherit.aes = TRUE
#             )
#             Arguments
#             mapping
#             Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#
#             data
#             The data to be displayed in this layer. There are three options:
#
#                 If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#
#             A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify() for which variables will be created.
#
#             A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#
#             position
#             Position adjustment, either as a string, or the result of a call to a position adjustment function.
#
#             ...
#             Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#
#             draw_quantiles
#             If not(NULL) (default), draw horizontal lines at the given quantiles of the density estimate.
#
#             trim
#             If TRUE (default), trim the tails of the violins to the range of the data. If FALSE, don't trim the tails.
#
# scale
# if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#
# na.rm
# If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#
# orientation
# The orientation of the layer. The default (NA) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting orientation to either "x" or "y". See the Orientation section for more detail.
#
# show.legend
# logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#
# inherit.aes
# If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#
#             geom, stat
#             Use to override the default connection between geom_violin and stat_ydensity.
#
#             bw
#             The smoothing bandwidth to be used. If numeric, the standard deviation of the smoothing kernel. If character, a rule to choose the bandwidth, as listed in stats::bw.nrd().
#
#             adjust
#             A multiplicate bandwidth adjustment. This makes it possible to adjust the bandwidth while still using the a bandwidth estimator. For example, adjust = 1/2 means use half of the default bandwidth.
#
#             kernel
#             Kernel. See list of available kernels in density().
#
#             Orientation
#             This geom treats each axis differently and, thus, can thus have two orientations. Often the orientation is easy to deduce from a combination of the given mappings and the types of positional scales in use. Thus, ggplot2 will by default try to guess which orientation the layer should have. Under rare circumstances, the orientation is ambiguous and guessing may fail. In that case the orientation can be specified directly using the orientation parameter, which can be either "x" or "y". The value gives the axis that the geom should run along, "x" being the default orientation you would expect for the geom.
#
#             Aesthetics
#             geom_violin() understands the following aesthetics (required aesthetics are in bold):
#
#                 x
#
#             y
#
#             alpha
#
#             colour
#
#             fill
#
#             group
#
#             linetype
#
#             size
#
#             weight
#
#             Learn more about setting these aesthetics in vignette("ggplot2-specs").
#
#             Computed variables
#             density
#             density estimate
#
#             scaled
#             density estimate, scaled to maximum of 1
#
#             count
#             density * number of points - probably useless for violin plots
#
#             violinwidth
#             density scaled for the violin plot, according to area, counts or to a constant maximum width
#
#             n
#             number of points
#
#             width
#             width of violin bounding box
#
#             References
#             Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box Plot-Density Trace Synergism. The American Statistician 52, 181-184.
#
#             See Also
#             geom_violin() for examples, and stat_density() for examples with data along the x axis.
#
#             Examples
#             p <- ggplot(mtcars, aes(factor(cyl), mpg))
#             p + geom_violin()
#
#             # Orientation follows the discrete axis
#             ggplot(mtcars, aes(mpg, factor(cyl))) +
#                 geom_violin()
#
#
#             p + geom_violin() + geom_jitter(height = 0, width = 0.1)
#
#             # Scale maximum width proportional to sample size:
#             p + geom_violin(scale = "count")
#
#             # Scale maximum width to 1 for all violins:
#             p + geom_violin(scale = "width")
#
#             # Default is to trim violins to the range of the data. To disable:
#             p + geom_violin(trim = FALSE)
#
#             # Use a smaller bandwidth for closer density fit (default is 1).
#             p + geom_violin(adjust = .5)
#
#             # Add aesthetic mappings
#             # Note that violins are automatically dodged when any aesthetic is
#             # a factor
#             p + geom_violin(aes(fill = cyl))
#             p + geom_violin(aes(fill = factor(cyl)))
#             p + geom_violin(aes(fill = factor(vs)))
#             p + geom_violin(aes(fill = factor(am)))
#
#             # Set aesthetics to fixed value
#             p + geom_violin(fill = "grey80", colour = "#3366FF")
#
#             # Show quartiles
#             p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
#
#             # Scales vs. coordinate transforms -------
#             if (require("ggplot2movies")) {
#                 # Scale transformations occur before the density statistics are computed.
#                 # Coordinate transformations occur afterwards.  Observe the effect on the
#                 # number of outliers.
#                 m <- ggplot(movies, aes(y = votes, x = rating, group = cut_width(rating, 0.5)))
#                 m + geom_violin()
#                 m + geom_violin() + scale_y_log10()
#                 m + geom_violin() + coord_trans(y = "log10")
#                 m + geom_violin() + scale_y_log10() + coord_trans(y = "log10")
#
#                 # Violin plots with continuous x:
#                 # Use the group aesthetic to group observations in violins
#                 ggplot(movies, aes(year, budget)) + geom_violin()
#                 ggplot(movies, aes(year, budget)) +
#                     geom_violin(aes(group = cut_width(year, 10)), scale = "width")
#             }
#
#             [Package ggplot2 version 3.3.0.9000 Index]






            #     scale_fill_viridis(discrete=TRUE) +
            #     scale_color_viridis(discrete=TRUE) +



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
