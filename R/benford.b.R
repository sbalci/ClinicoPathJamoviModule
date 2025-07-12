#' @title Benford's Law Analysis
#' @description This function performs a Benford's Law analysis on a numeric variable.
#' It returns the Benford's Law distribution and a list of potential suspects.
#' @details The Benford's Law analysis is a test to determine if the distribution of the first digits of a numeric variable follows Benford's Law.
#' The Benford's Law distribution is compared to the observed distribution of the first digits of the variable.
#' The analysis returns a list of potential suspects that deviate significantly from Benford's Law.
#' @param var The numeric variable to analyze.
#' @return A list with the Benford's Law distribution and a list of potential suspects.
#' @importFrom benford.analysis benford getSuspects
#' @importFrom glue glue
#' @importFrom jmvcore composeTerm constructFormula toNumeric
#'
#'
#' @returns A list with the Benford's Law distribution and a list of potential suspects.
#' @export benfordClass
#'


benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "benfordClass",
    inherit = benfordBase,
    private = list(
        .run = function() {

            todo <- glue::glue("
                               <br>
                               See
                               <a href = 'https://github.com/carloscinelli/benford.analysis'>Package documentation</a> for interpratation.
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
