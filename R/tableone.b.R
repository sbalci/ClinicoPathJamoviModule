#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

tableoneClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
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


            # formula <- jmvcore::constructFormula(terms = self$options$vars)
            # formula <- paste('~', formula)
            # formula <- as.formula(formula)
            # table1 <- arsenal::tableby(formula, self$data,
            #                            total = TRUE,
            #                            digits = 1,
            #                            digits.count = 1
            #                            )
            # myarsenal <- summary(table1, text = "html")
            # myarsenal <- kableExtra::kable(myarsenal, format = "html",
            #                                digits = 1,
            #                                escape = TRUE) %>%
            #     kableExtra::kable_styling(kable_input = .,
            #                               bootstrap_options = "striped",
            #                               full_width = F,
            #                               position = "left")

            # table one


            mytableone <- self$data %>%
                tableone::CreateTableOne(data = .)




            # results

            self$results$text1$setContent(mytableone)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
