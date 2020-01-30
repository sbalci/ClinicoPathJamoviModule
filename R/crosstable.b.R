#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

crosstableClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstableClass",
    inherit = crosstableBase,
    private = list(
        .run = function() {


            if (length(self$options$vars) == 0)
                return()

            # Arsenal Table

            formulaR <- jmvcore::constructFormula(terms = self$options$vars)

            formulaL <- jmvcore::constructFormula(terms = self$options$group)

            formula <- paste(formulaL, '~', formulaR)
            formula <- as.formula(formula)

            table1 <- arsenal::tableby(formula, self$data)

            results1 <- summary(table1)

            self$results$text1$setContent(results1)


            # Tangram Table

            table2 <-
                tangram::tangram(formula, self$data
                )

            results2 <- table2

            self$results$text2$setContent(results2)

            # Tangram Table NEJM

            table3 <-
                tangram::html5(
                    tangram::tangram(
                        formula, self$data),
                    fragment=TRUE,
                    inline="nejm.css",
                    caption = "Cross Table NEJM Style",
                    id="tbl3")

            results3 <- table3

            self$results$text3$setContent(results3)


            # Tangram Table Lancet

            table4 <-
                tangram::html5(
                    tangram::tangram(
                        formula, self$data),
                    fragment=TRUE,
                    inline="lancet.css",
                    caption = "Cross Table Lancet Style",
                    id="tbl4")

            results4 <- table4

            self$results$text4$setContent(results4)






            # table2 <-
            #     tangram::html5(tangram::tangram("drug ~ bili[2] + albumin + stage::Categorical + protime + sex + age + spiders", pbc),
            #       fragment=TRUE, inline="nejm.css", caption = "HTML5 Table NEJM Style", id="tbl3")




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
