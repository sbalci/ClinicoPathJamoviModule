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



            # > result1[["method"]]
            # [1] "Percentage agreement (Tolerance=0)"
            # > result1[["subjects"]]
            # [1] 248
            # > result1[["raters"]]
            # [1] 2
            # > result1[["irr.name"]]
            # [1] "%-agree"
            # > result1[["value"]]
            # [1] 52.01613


# > result2[["method"]]
# [1] "Cohen's Kappa for 2 Raters (Weights: unweighted)"
# > result2[["subjects"]]
# [1] 248
# > result2[["raters"]]
# [1] 2
# > result2[["irr.name"]]
# [1] "Kappa"
# > result2[["value"]]
# [1] 0.003377009
# > result2[["stat.name"]]
# [1] "z"
# > result2[["statistic"]]
# [1] 0.05419615
# > result2[["p.value"]]
# [1] 0.9567789






            self$results$text$setContent(result)

            self$results$text1$setContent(result1)

            self$results$text2$setContent(result2)


            irrname <- result1[["irr.name"]]

            table <- self$results$irrtable
            table$setRow(rowNo=1, values=list(
                # var=self$options$vars,
                Method = result1[["method"]],
                Subjects=result1[["subjects"]],
                Raters = result1[["raters"]],
                peragree = result1[["value"]]
            ))

        })
)
