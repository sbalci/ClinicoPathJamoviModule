#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

frequenciesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "frequenciesClass",
    inherit = frequenciesBase,
    private = list(
        .run = function() {



            # TODO

            todo <- glue::glue(
                "This Module is still under development",
                " -  ",
                " -  "
            )

            self$results$todo$setContent(todo)



            if (length(self$options$vars) == 0)
                return()

            mydata <- self$data

            show_na <- self$options$show_na

            na_lev <- self$options$na_lev


            formula <- jmvcore::constructFormula(terms = self$options$vars)

            # formula <- jmvcore::constructFormula(terms=c('Sex', 'PreinvasiveComponent'))

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)


            results <-
                purrr::map(.x = myvars,
                           .f = ~ janitor::tabyl(dat = mydata,
                                                 .x,
                                                 show_na = show_na,
                                                 show_missing_levels = na_lev
                           ) %>%
                               janitor::adorn_pct_formatting(rounding = "half up",
                                                             digits = 1) %>%
                               janitor::adorn_totals()
                ) %>%

                purrr::set_names(myvars)


            self$results$text$setContent(results)


            # results1 <- knitr::kable(results)

            # results2 <- results

            # self$results$text2$setContent(results2)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
