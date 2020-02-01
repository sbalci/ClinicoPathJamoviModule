#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

writesummaryClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "writesummaryClass",
    inherit = writesummaryBase,
    private = list(
        .run = function() {

            if (length(self$options$vars) == 0)
                return()


            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -
                -
                "
            )

            self$results$todo$setContent(todo)


            ####



            mydata <- self$data

            myvars <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = myvars)

            myvars <- unlist(myvars)

            mysummary <- function(myvar) {

            mean_x <- round(mean(mydata[[myvar]], na.rm=TRUE), digits = 1)

            sd_x <- round(sd(x = mydata[[myvar]], na.rm = TRUE), digits = 1)

            median_x <- round(median(mydata[[myvar]], na.rm=TRUE), digits = 1)

            min_x <- round(min(mydata[[myvar]], na.rm=TRUE), digits = 1)

            max_x <- round(mean(mydata[[myvar]], na.rm=TRUE), digits = 1)

            print(
                paste0(
                    "Mean of ",
                    myvar,
                    " is: ",
                    mean_x,
                    " + ",
                    sd_x,
                    ". (Median: ",
                    median_x,
                    " [Min: ",
                    min_x,
                    " - ",
                    "Max: ",
                    max_x,
                    "])",
                    collapse = " "
                    )
            )
            }

            results <- purrr::map(.x = myvars, .f = mysummary)

            results <- unlist(results)

            self$results$text$setContent(results)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
