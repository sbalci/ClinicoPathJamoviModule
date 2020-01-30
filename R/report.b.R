#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

reportClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "reportClass",
    inherit = reportBase,
    private = list(
        .run = function() {

            if (length(self$options$vars) == 0)
                return()

            mydata <- self$data

            med <- self$options$med

            cent <- self$options$cent

            disp <- self$options$disp

            ran <- self$options$ran

            distr <- self$options$distr

            lev <- self$options$lev

            n_ch <- self$options$n_ch

            mis <- self$options$mis

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            myreport <- mydata %>%
                select(myvars) %>%
                report::report(.)
            # ,
            #                    median = med,
            #                    centrality = cent,
            #                    dispersion = disp,
            #                    range = ran,
            #                    distribution = distr,
            #                    levels_percentage = lev,
            #                    n_characters = n_ch,
            #                    missing_percentage = mis
            #                    )
            results1 <- myreport



            # results1 <- mydata %>%
            #     explore::describe(.) %>%
            #     dplyr::filter(na > 0)



            self$results$text$setContent(results1)


            if (self$options$gli == 0){

                return()

            } else{


                mydata <- self$data

                results2 <- mydata %>%
                    dplyr::select(myvars) %>%
                    dplyr::glimpse(.)

                self$results$text2$setContent(results2)


            }

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
