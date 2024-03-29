
# This file is automatically generated, you probably don't want to edit this

retractedOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "retractedOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            doi = NULL, ...) {

            super$initialize(
                package="ClinicoPath",
                name="retracted",
                requiresData=TRUE,
                ...)

            private$..doi <- jmvcore::OptionVariable$new(
                "doi",
                doi)
            private$..resids <- jmvcore::OptionOutput$new(
                "resids")
            private$..resids2 <- jmvcore::OptionOutput$new(
                "resids2")

            self$.addOption(private$..doi)
            self$.addOption(private$..resids)
            self$.addOption(private$..resids2)
        }),
    active = list(
        doi = function() private$..doi$value,
        resids = function() private$..resids$value,
        resids2 = function() private$..resids2$value),
    private = list(
        ..doi = NA,
        ..resids = NA,
        ..resids2 = NA)
)

retractedResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "retractedResults",
    inherit = jmvcore::Group,
    active = list(
        text4 = function() private$.items[["text4"]],
        text5 = function() private$.items[["text5"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Find Retracted Papers from DOI",
                refs=list(
                    "ClinicoPathJamoviModule"))
            self$add(jmvcore::Html$new(
                options=options,
                name="text4",
                title="id convert"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text5",
                title="text5"))}))

retractedBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "retractedBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "retracted",
                version = c(1,0,0),
                options = options,
                results = retractedResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'none')
        }))

#' Find Retracted Papers from DOI
#'
#' 
#' @param data .
#' @param doi .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text4} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$text5} \tab \tab \tab \tab \tab a preformatted \cr
#' }
#'
#' @export
retracted <- function(
    data,
    doi) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("retracted requires jmvcore to be installed (restart may be required)")

    if ( ! missing(doi)) doi <- jmvcore::resolveQuo(jmvcore::enquo(doi))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(doi), doi, NULL))


    options <- retractedOptions$new(
        doi = doi)

    analysis <- retractedClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

