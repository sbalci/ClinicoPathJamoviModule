#' @title Interrater Reliability Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr


# See
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass",
    inherit = agreementBase, private = list(.run = function() {



        # # Error Message ----
        #
        # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
        #
        # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
        #     # ToDo Message ----
        #     todo <- "
        #         <br>Welcome to ClinicoPath
        #                   <br><br>
        #                   This tool will help you form an Alluvial Plots.
        #                   "
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        # } else {
        #     todo <- ""
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        #
        #
        # }









        # Data definition ----


        exct <- self$options$exct
        wght <- self$options$wght

        mydata <- self$data

        formula <- jmvcore::constructFormula(terms = self$options$vars)

        myvars <- jmvcore::decomposeFormula(formula = formula)

        myvars <- unlist(myvars)

        ratings <- mydata %>% dplyr::select(myvars)


        # psych::cohen.kappa ----

        # from https://github.com/kwongwh/Kappa
        # ratings2 <- mydata[c(self$options$vars)]
        #
        # result_cohen <- psych::cohen.kappa(x = ratings2)
        #
        # self$results$result_cohen$setContent(result_cohen)


        # irr.kappa <- kappa2(vars, weight = self$options$weights)
        # if(self$options$weights == "unweighted"){
        #     n = 1
        # } else {
        #     n = 2
        # }
        #
        # table <- self$results$ka
        #
        # table$setRow(rowNo=1, values=list(
        #     weights = self$options$weights,
        #     kappa=irr.kappa$value,
        #     upper_CI=results$confid[1,3],
        #     lower_CI=results$confid[1,1],
        #     p = irr.kappa$p.value
        # ))




        if (is.null(self$options$vars) || length(self$options$vars) < 2) {
            # No variables ----

            # todo <- glue::glue( 'This function ' )

            # self$results$todo$setContent(todo)

        } else {
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            # 2 & categorical ----

            if (length(self$options$vars) == 2) {
                # todo <- 'Cohen'

                # self$results$todo$setContent(todo)


                xorder <- unlist(lapply(ratings, is.ordered))

                if (wght %in% c("equal", "squared") && !all(xorder == TRUE)) stop("Use ordinal variables when using weight argument")

                if (exct == TRUE) stop("Use exact argument only >=3 variables")


                # irr::kappa2 ----

                result2 <- irr::kappa2(ratings = ratings, weight = wght)

                # self$results$text2$setContent(result2)


                # >=2 & categorical ----


            } else if (length(self$options$vars) >= 2) {
                # todo <- 'kappam.fleiss'

                # self$results$todo$setContent(todo)

                # irr::kappam.fleiss ----

                result2 <- irr::kappam.fleiss(ratings = ratings, exact = exct,
                  detail = TRUE)

                # self$results$text2$setContent(result2)

            }


            # irr::agree ----

            result <- table(ratings)

            self$results$text$setContent(result)


            result1 <- irr::agree(ratings)

            # self$results$text1$setContent(result1)

            # Table ----

            table2 <- self$results$irrtable
            table2$setRow(rowNo = 1, values = list(method = result2[["method"]],
                subjects = result1[["subjects"]], raters = result1[["raters"]],
                peragree = result1[["value"]], kappa = result2[["value"]],
                z = result2[["statistic"]], p = result2[["p.value"]]))

        }


    }))
