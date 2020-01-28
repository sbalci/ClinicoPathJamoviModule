
# This file is a generated template, your changes will not be overwritten

statsplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "statsplotClass",
    inherit = statsplotBase,
    private = list(
        .run = function() {

            mydata <- self$data


            # dep <- jmvcore::constructFormula(terms = self$options$dep)
            #
            # dep <- jmvcore::decomposeFormula(formula = dep)
            #
            # dep <- unlist(dep)
            #
            # group <- jmvcore::constructFormula(terms = self$options$group)
            #
            # group <- jmvcore::decomposeFormula(formula = group)
            #
            # group <- unlist(group)



            dep <- self$options$dep

            # dep <- jmvcore::toNumeric(self$data[[dep]])

            # dep <- dplyr::enquo(dep)

            group <- self$options$group

            # group <- self$data[[group]]

            # group <- dplyr::enquo(group)


            mygroup <- jmvcore::constructFormula(terms = self$options$group)

            mygroup <- jmvcore::composeTerm(mygroup)

            mydep <- jmvcore::constructFormula(terms = self$options$dep)

            mydep <- jmvcore::composeTerm(mydep)


            # formula <- paste0('x = ', mygroup, 'y = ', mydep)
            # myformula <- as.formula(formula)

            # table1 <- arsenal::tableby(formula, self$data)

            # results1 <- summary(table1, text = "html")


            # myformula <- glue::glue( 'x = ' , {mygroup}, ', y = ' , {mydep})

            # myformula <- jmvcore::composeTerm(myformula)


            # myresults <- function(data, mygroup, mydep) {
            #     data %>%
            #         ggstatsplot::ggbetweenstats(
            #             data = .data,
            #             x = {{mygroup}},
            #             y = {{mydep}})
            # }
            #
            #
            # results <- myresults(mydata, mygroup, mydep)

            results <- mydata %>%
                ggstatsplot::ggbetweenstats(
                    # data = .data,
                    {{mygroup}},
                    {{mydep}}
                    #                         x = .data[[group]],
                    #                         y = .data[[dep]]
                    # x = quote(!!mygroup),
                    # y = quote(!!mydep)
                                            )




            # results <- group





            self$results$text$setContent(results)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
