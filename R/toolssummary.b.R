#' @title Tools for data summary
#'
#' @return Table
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#'


toolssummaryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "toolssummaryClass",
    inherit = toolssummaryBase,
    private = list(
        .run = function() {


            # Prepare Data ----

            varsName <- self$options$vars

            data <- jmvcore::select(self$data, c(varsName))


            # summarytools requires tcltk
            # mytable <-
            #     summarytools::dfSummary(
            #     data
            #     )
            # mytable <- gtsummary::tbl_summary(data = data)


            # autoEDA shows output in XQuartz
            # mytable <- autoEDA::autoEDA(iris)



            # needs pandoc
            # mytable <- DataExplorer::create_report(iris)




            self$results$tablestyle4$setContent(mytable)


            self$results$tablestyle4html$setContent(mytable)



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
