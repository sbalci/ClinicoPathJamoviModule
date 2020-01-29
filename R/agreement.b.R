
# This file is a generated template, your changes will not be overwritten

agreementClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "agreementClass",
    inherit = agreementBase,
    private = list(
        .run = function() {



            if (length(self$options$vars) == 0)
                return()

            mydata <- self$data

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            ratings <- mydata %>% select(myvars)


            result1 <- irr::agree(ratings)


            result2 <- irr::kappa2(ratings)


            self$results$text1$setContent(result1)

            self$results$text2$setContent(result2)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
