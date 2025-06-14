
# This file is automatically generated, you probably don't want to edit this

competingsurvivalOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "competingsurvivalOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            explanatory = NULL,
            overalltime = NULL,
            outcome = NULL,
            dod = NULL,
            dooc = NULL,
            awd = NULL,
            awod = NULL,
            analysistype = "overall", ...) {

            super$initialize(
                package="ClinicoPath",
                name="competingsurvival",
                requiresData=TRUE,
                ...)

            private$..explanatory <- jmvcore::OptionVariable$new(
                "explanatory",
                explanatory,
                suggested=list(
                    "ordinal",
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..overalltime <- jmvcore::OptionVariable$new(
                "overalltime",
                overalltime,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..outcome <- jmvcore::OptionVariable$new(
                "outcome",
                outcome,
                suggested=list(
                    "ordinal",
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..dod <- jmvcore::OptionLevel$new(
                "dod",
                dod,
                variable="(outcome)",
                allowNone=TRUE)
            private$..dooc <- jmvcore::OptionLevel$new(
                "dooc",
                dooc,
                variable="(outcome)",
                allowNone=TRUE)
            private$..awd <- jmvcore::OptionLevel$new(
                "awd",
                awd,
                variable="(outcome)",
                allowNone=TRUE)
            private$..awod <- jmvcore::OptionLevel$new(
                "awod",
                awod,
                variable="(outcome)",
                allowNone=TRUE)
            private$..analysistype <- jmvcore::OptionList$new(
                "analysistype",
                analysistype,
                options=list(
                    "overall",
                    "cause",
                    "compete"),
                default="overall")

            self$.addOption(private$..explanatory)
            self$.addOption(private$..overalltime)
            self$.addOption(private$..outcome)
            self$.addOption(private$..dod)
            self$.addOption(private$..dooc)
            self$.addOption(private$..awd)
            self$.addOption(private$..awod)
            self$.addOption(private$..analysistype)
        }),
    active = list(
        explanatory = function() private$..explanatory$value,
        overalltime = function() private$..overalltime$value,
        outcome = function() private$..outcome$value,
        dod = function() private$..dod$value,
        dooc = function() private$..dooc$value,
        awd = function() private$..awd$value,
        awod = function() private$..awod$value,
        analysistype = function() private$..analysistype$value),
    private = list(
        ..explanatory = NA,
        ..overalltime = NA,
        ..outcome = NA,
        ..dod = NA,
        ..dooc = NA,
        ..awd = NA,
        ..awod = NA,
        ..analysistype = NA)
)

competingsurvivalResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "competingsurvivalResults",
    inherit = jmvcore::Group,
    active = list(
        todo = function() private$.items[["todo"]],
        text1 = function() private$.items[["text1"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Overall, Cause Specific, and Competing Survival")
            self$add(jmvcore::Html$new(
                options=options,
                name="todo",
                title="To Do",
                clearWith=list(
                    "explanatory",
                    "outcome",
                    "overalltime")))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text1",
                title="Competing Survival"))}))

competingsurvivalBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "competingsurvivalBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "competingsurvival",
                version = c(0,0,3),
                options = options,
                results = competingsurvivalResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Overall, Cause Specific, and Competing Survival
#'
#' Overall, Cause Specific, and Competing Survival.
#'
#' @examples
#' \donttest{
#' # example will be added
#'}
#' @param data The data as a data frame.
#' @param explanatory .
#' @param overalltime .
#' @param outcome .
#' @param dod .
#' @param dooc .
#' @param awd .
#' @param awod .
#' @param analysistype .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$todo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$text1} \tab \tab \tab \tab \tab a preformatted \cr
#' }
#'
#' @export
competingsurvival <- function(
    data,
    explanatory,
    overalltime,
    outcome,
    dod,
    dooc,
    awd,
    awod,
    analysistype = "overall") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("competingsurvival requires jmvcore to be installed (restart may be required)")

    if ( ! missing(explanatory)) explanatory <- jmvcore::resolveQuo(jmvcore::enquo(explanatory))
    if ( ! missing(overalltime)) overalltime <- jmvcore::resolveQuo(jmvcore::enquo(overalltime))
    if ( ! missing(outcome)) outcome <- jmvcore::resolveQuo(jmvcore::enquo(outcome))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(explanatory), explanatory, NULL),
            `if`( ! missing(overalltime), overalltime, NULL),
            `if`( ! missing(outcome), outcome, NULL))

    for (v in explanatory) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in outcome) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- competingsurvivalOptions$new(
        explanatory = explanatory,
        overalltime = overalltime,
        outcome = outcome,
        dod = dod,
        dooc = dooc,
        awd = awd,
        awod = awod,
        analysistype = analysistype)

    analysis <- competingsurvivalClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

