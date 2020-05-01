#' @importFrom R6 R6Class
#' @import jmvcore
#'

crosstableClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstableClass",
    inherit = crosstableBase,
    private = list(
        .run = function() {


            if (length(self$options$vars) == 0 | length(self$options$group) == 0)
                return()


            # TODO

            # todo <- glue::glue(
            #     "This Module is still under development
            #     -
            #     -
            #     "
            # )
            #
            # self$results$todo$setContent(todo)


            # write explanation for the function


            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            formulaR <- jmvcore::constructFormula(terms = self$options$vars)

            formulaL <- jmvcore::constructFormula(terms = self$options$group)

            formula <- paste(formulaL, '~', formulaR)

            formula <- as.formula(formula)

            # Arsenal Table
            # table1 <- arsenal::tableby(formula, self$data)
            # results1 <- summary(table1)
            # results1 <- knitr::kable(results1,
            #              row.names = FALSE,
            #              align = c('l', 'l', 'r', 'r', 'r', 'r'),
            #              format = "html") %>%
            #     kableExtra::kable_styling(kable_input = .,
            #                               bootstrap_options = "striped",
            #                               full_width = F,
            #                               position = "left")
            # self$results$text1$setContent(results1)

            # Tangram Table
            # table2 <-
            #     tangram::tangram(formula, self$data
            #     )
            # results2 <- table2
            # self$results$text2$setContent(results2)

            # Tangram Table NEJM

            table3 <-
                tangram::html5(
                    tangram::tangram(
                        formula, self$data),
                    fragment = TRUE,
                    inline = "nejm.css",
                    caption = paste0("Cross Table NEJM Style for Dependent ", self$options$group),
                    id = "tbl3")

            results3 <- table3

            self$results$text3$setContent(results3)


            # Tangram Table Lancet

            # table4 <-
            #     tangram::html5(
            #         tangram::tangram(
            #             formula, self$data),
            #         fragment = TRUE,
            #         inline = "lancet.css",
            #         caption = "Cross Table Lancet Style",
            #         id = "tbl4")
            # results4 <- table4
            # self$results$text4$setContent(results4)



            # Table FinalFit
            # https://finalfit.org/articles/tables_gallery.html#cross-tables
            # myvars <- jmvcore::decomposeFormula(formula = formulaR)
            # myvars <- unlist(myvars)
            # self$data %>%
            #     summary_factorlist(dependent = self$options$group,
            #                        explanatory = myvars,
            #                        # column = TRUE,
            #                        total_col = TRUE,
            #                        p = TRUE,
            #                        add_dependent_label = TRUE,
            #                        na_include = FALSE
            #                        # catTest = catTestfisher
            # ) -> table5
            # knitr::kable(table5, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'))
            # results5 <- table5
            # self$results$text5$setContent(results5)

            # table2 <-
            #     tangram::html5(tangram::tangram("drug ~ bili[2] + albumin + stage::Categorical + protime + sex + age + spiders", pbc),
            #       fragment=TRUE, inline="nejm.css", caption = "HTML5 Table NEJM Style", id="tbl3")


        })
)
