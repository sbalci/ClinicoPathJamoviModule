
# This file is automatically generated, you probably don't want to edit this

lassocoxOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lassocoxOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function( ...) {

            super$initialize(
                package="ClinicoPath",
                name="lassocox",
                requiresData=TRUE,
                ...)


        }),
    active = list(),
    private = list()
)

lassocoxResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lassocoxResults",
    inherit = jmvcore::Group,
    active = list(
        modelSummary = function() private$.items[["modelSummary"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Lasso-Cox Regression")
            self$add(jmvcore::Table$new(
                options=options,
                name="modelSummary",
                title="Model Summary",
                rows=0,
                columns=list(
                    list(
                        `name`="statistic", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="value", 
                        `title`="Value", 
                        `type`="number"))))}))

lassocoxBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lassocoxBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "lassocox",
                version = c(1,0,0),
                options = options,
                results = lassocoxResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Lasso-Cox Regression
#'
#' 
#' @param data The data as a data frame.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$modelSummary} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$modelSummary$asDF}
#'
#' \code{as.data.frame(results$modelSummary)}
#'
#' @export
lassocox <- function(
    data) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("lassocox requires jmvcore to be installed (restart may be required)")

    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame())


    options <- lassocoxOptions$new()

    analysis <- lassocoxClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

