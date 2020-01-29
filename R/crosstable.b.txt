#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

crosstableClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstableClass",
    inherit = crosstableBase,
    private = list(
        .run = function() {


            if (length(self$options$vars) == 0)
                return()

            formulaR <- jmvcore::constructFormula(terms = self$options$vars)

            formulaL <- jmvcore::constructFormula(terms = self$options$group)

            formula <- paste(formulaL, '~', formulaR)
            formula <- as.formula(formula)

            table1 <- arsenal::tableby(formula, self$data)

            results <- summary(table1)

            self$results$text$setContent(results)





            # mydata <- self$data
            #
            # show_na <- self$options$show_na
            #
            # na_lev <- self$options$na_lev
            #
            # formula <- jmvcore::constructFormula(terms = self$options$vars)
            #
            # # formula <- jmvcore::constructFormula(terms=c('Sex', 'PreinvasiveComponent'))
            #
            # myvars <- jmvcore::decomposeFormula(formula = formula)
            #
            # myvars <- unlist(myvars)
            #
            #
            # formula2 <- jmvcore::constructFormula(terms = self$options$group)
            #
            # mygroup <- jmvcore::decomposeFormula(formula = formula2)
            #
            # mygroup <- unlist(mygroup)
            #
            # results2 <-
            #     purrr::map(.x = myvars,
            #                .f = ~ janitor::tabyl(dat = mydata,
            #                                      mygroup,
            #                                      .x,
            #                                      show_na = show_na,
            #                                      show_missing_levels = na_lev
            #                ) %>%
            #                    janitor::adorn_pct_formatting(rounding = "half up",
            #                                                  digits = 1) %>%
            #                    janitor::adorn_totals()
            #     ) %>%
            #
            #     purrr::set_names(myvars)
            #
            #
            # self$results$text$setContent(results2)







            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
