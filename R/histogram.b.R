# from https://github.com/ClinicoPath/PUBH5018-jamovi


histogramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "histogramClass",
    inherit = histogramBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

          image <- self$results$plot
          dep <- self$options$dep
          if(is.null(dep)) return(FALSE)
          data <- self$data

          plotData <- data.frame(x = data[[dep]])

          image$setState(plotData)
        },
        .plot=function(image,ggtheme,theme,...) {

          if (is.null(image$state))
            return(FALSE)

          fill <- theme$fill[2]
          color <- theme$color[1]

          plotData <- image$state
          plot <- ggplot(plotData, aes(x=x)) +
            #geom_histogram(binwidth = self$options$binwidth, boundary=self$options$anchor,color="black",fill="yellow") +
            geom_histogram(binwidth = self$options$binwidth, boundary=self$options$anchor,color=color,fill=fill) +
            xlab(self$options$dep) +
            ylab("Frequency")

          plot <- plot + ggtheme

          print(plot)
          TRUE
        })
)
