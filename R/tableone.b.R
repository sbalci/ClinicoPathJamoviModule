#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

tableoneClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {


            if (length(self$options$vars) == 0)
                return()

            # formula <- jmvcore::constructFormula(terms = self$options$vars)
            # formula <- paste('~', formula)
            # formula <- as.formula(formula)

            # table1 <- arsenal::tableby(formula, self$data)

            # results1 <- summary(table1, text = "html")




            # result2 <- "You have entered"

            # result2 <- kableExtra::kable(results1, format = "html", digits = 1,
            #                              escape = TRUE) %>%
            #     kableExtra::kable_styling(kable_input = .,
            #                               bootstrap_options = "striped", full_width = F, position = "left")



            # table one


            tableo <- self$data %>%
                tableone::CreateTableOne(data = .)




            # results

            self$results$text$setContent(tableo)


            # html <- self$results$result2
            # html$setContent(result2)


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
