#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
# This file is a generated template, your changes will not be overwritten

tableoneClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {

            if (length(self$options$vars) == 0) {
                todo <- " This Module is still under development
                -
                -
                Welcome to Statkat!
                          <br><br>
                          This tool will help you to find an appropriate statistical method given the measurement level of your data.
                          Make sure you have correctly defined the measurement levels of your variables on the Data tab. You can change the measurement level
                          of a variable via the Setup button on the Data tab, or by double clicking on a column header of interest.
                          <br><br>
                          To get started, drop a variable in the white box below Variable. Our tool will then come up with a statistical method that may be appropriate for your data!
                          <br><br>
                          Note:<br>
                          Our advice is based on the measurement level of your data. There can be details related to your data, task, or assignment that may render the advice moot.
                          Always check the assumptions made by the statistical method before interpreting the results.
                          We always try to come up with the least complicated method that might be applicable given your data. Keep in mind that there may be other, more advanced,
                          methods that might be applicable as well.
                          "
                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {

                todo <- " This Module is still under development
                -
                -"

                html <- self$results$todo
                html$setContent(todo)


            # # TODO
            #
            # todo <- glue::glue(
            #     ""
            # )
            #
            # self$results$todo$setContent(todo)
            #
            #
            #
            # if (length(self$options$vars) == 0)
            #     return()


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


            }



        })
)
