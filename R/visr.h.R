
# This file is automatically generated, you probably don't want to edit this

visrOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "visrOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            group = NULL,
            alt = "notequal",
            varEq = TRUE, ...) {

            super$initialize(
                package="ClinicoPath",
                name="visr",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep)
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group)
            private$..alt <- jmvcore::OptionList$new(
                "alt",
                alt,
                options=list(
                    "notequal",
                    "onegreater",
                    "twogreater"),
                default="notequal")
            private$..varEq <- jmvcore::OptionBool$new(
                "varEq",
                varEq,
                default=TRUE)

            self$.addOption(private$..dep)
            self$.addOption(private$..group)
            self$.addOption(private$..alt)
            self$.addOption(private$..varEq)
        }),
    active = list(
        dep = function() private$..dep$value,
        group = function() private$..group$value,
        alt = function() private$..alt$value,
        varEq = function() private$..varEq$value),
    private = list(
        ..dep = NA,
        ..group = NA,
        ..alt = NA,
        ..varEq = NA)
)

visrResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "visrResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="visr")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="visr"))}))

visrBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "visrBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "visr",
                version = c(1,0,0),
                options = options,
                results = visrResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' visr
#'
#' 
#' @param data .
#' @param dep .
#' @param group .
#' @param alt .
#' @param varEq .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#' }
#'
#' @export
visr <- function(
    data,
    dep,
    group,
    alt = "notequal",
    varEq = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("visr requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(group), group, NULL))


    options <- visrOptions$new(
        dep = dep,
        group = group,
        alt = alt,
        varEq = varEq)

    analysis <- visrClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

