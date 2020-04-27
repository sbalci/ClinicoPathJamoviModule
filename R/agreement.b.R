#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import irr

agreementClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "agreementClass",
    inherit = agreementBase,
    private = list(

        .run = function() {

            if (length(self$options$vars) < 2)
                return()


            # TODO

            todo <- glue::glue(
                "This Module is still under development
                🔬🔬🔬🔬 UNDER CONSTRUCTION 🛠⛔️⚠️🔩

                -
                -
                "
            )

            self$results$todo$setContent(todo)


            ####


            mydata <- self$data

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            ratings <- mydata %>%
                dplyr::select(myvars)


            result <- table(ratings[,1], ratings[,2])

            result1 <- irr::agree(ratings)

            result2 <- irr::kappa2(ratings)



            #>  Cohen's Kappa for 2 Raters (Weights: unweighted)
            #>
            #>  Subjects = 30
            #>    Raters = 2
            #>     Kappa = 0.651
            #>
            #>         z = 7
            #>   p-value = 2.63e-12




            self$results$text$setContent(result)

            self$results$text1$setContent(result1)

            self$results$text2$setContent(result2)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
