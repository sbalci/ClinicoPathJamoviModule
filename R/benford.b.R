
benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "benfordClass",
    inherit = benfordBase,
    private = list(
        .run = function() {



            todo <- glue::glue("
                               <br>
                               See
                               <a href = 'https://github.com/carloscinelli/benford.analysis'>Package documentation</a> for interpratation.
                               ",
                               # states that:
                               # <br>
                               # After that you have the main statistics of the log mantissa of the data. If the data follows Benfords Law, the numbers should be close to:
                               # <br>
                               #
                               # <table>
                               # <tr>
                               # <th>Statistics</th>
                               # <th>Value</th>
                               # </tr>
                               # <tr>
                               # <td>Mean</td>
                               # <td>0.5</td>
                               # </tr>
                               # <tr>
                               # <td>Variance</td>
                               # <td>1/12 (0.08333…)</td>
                               # </tr>
                               # <tr>
                               # <td>Ex. Kurtosis</td>
                               # <td>-1.2</td>
                               # </tr>
                               # <tr>
                               # <td>Skewness</td>
                               # <td>0</td>
                               # </tr>
                               # </table>
                               # <br>
                               # <hr>
                               # <br>,

                               "<br>
                               Also see <a href = 'https://www.iamnagdev.com/?p=926'>this blog post</a>.
                               <br>
                               <hr>
                               ")

            self$results$todo$setContent(todo)


            # Error Message ----

            if ( is.null(self$options$var) )
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            # Read data ----

            mydata <- self$data

            # var <- self$options$var

            # var <- jmvcore::composeTerm(components = var)

            var <- jmvcore::constructFormula(terms = self$options$var)

            var <- jmvcore::toNumeric(mydata[[self$options$var]])

            bfd.cp <- benford.analysis::benford(data = var)

            self$results$text$setContent(bfd.cp)

            # Suspects ----

            suspects <- benford.analysis::getSuspects(bfd = bfd.cp, data = mydata)

            self$results$text2$setContent(suspects)

            # Prepare Data for Plot ----

            plotData <- bfd.cp


            # li <- self$options$li
            #
            # if (li) {
            #
            #   # load image
            #   im <- load.image(file.choose()) %>% grayscale()
            #
            #   # perform (DCT)
            #   im_df <- DCT2D(im) %>% as.data.frame()
            #
            #   # apply benford law
            #   bfd.im = benford(im_df$value, number.of.digits = 1, discrete = T, round = 1, sign = "both")
            #   bfd.im
            #
            #
            #   self$results$text$setContent(bfd.im)
            #
            #
            #   # Prepare Data for Plot ----
            #
            #   plotData <- bfd.im
            #
            # }



            # Data for plot ----

            image <- self$results$plot
            image$setState(plotData)





        },

        .plot = function(image, ggtheme, theme, ...) {


            # Error Message ----
            if ( is.null(self$options$var) )
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # read data ----

            plotData <- image$state

            plot <- plot(plotData)

            print(plot)
            TRUE

        }


        )
)
