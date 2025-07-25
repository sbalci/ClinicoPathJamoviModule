
# This file is automatically generated, you probably don't want to edit this

groupsummaryOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "groupsummaryOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            groupVars = NULL,
            sumVars = NULL,
            statistics = list(
                "sum",
                "mean",
                "n"),
            dateVar = NULL,
            dateFormat = "ymd",
            timeAggregation = "day",
            showMissing = FALSE,
            addPercentage = TRUE,
            sortBy = "groups", ...) {

            super$initialize(
                package="ClinicoPath",
                name="groupsummary",
                requiresData=TRUE,
                ...)

            private$..groupVars <- jmvcore::OptionVariables$new(
                "groupVars",
                groupVars,
                suggested=list(
                    "ordinal",
                    "nominal"),
                permitted=list(
                    "factor",
                    "numeric",
                    "id"))
            private$..sumVars <- jmvcore::OptionVariables$new(
                "sumVars",
                sumVars,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..statistics <- jmvcore::OptionNMXList$new(
                "statistics",
                statistics,
                options=list(
                    "sum",
                    "mean",
                    "median",
                    "n"),
                default=list(
                    "sum",
                    "mean",
                    "n"))
            private$..dateVar <- jmvcore::OptionVariable$new(
                "dateVar",
                dateVar,
                default=NULL,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric",
                    "factor",
                    "id"))
            private$..dateFormat <- jmvcore::OptionList$new(
                "dateFormat",
                dateFormat,
                options=list(
                    "ymd",
                    "dmy",
                    "mdy",
                    "ymd_hms",
                    "dmy_hms",
                    "mdy_hms"),
                default="ymd")
            private$..timeAggregation <- jmvcore::OptionList$new(
                "timeAggregation",
                timeAggregation,
                options=list(
                    "hour",
                    "day",
                    "week",
                    "month",
                    "year"),
                default="day")
            private$..showMissing <- jmvcore::OptionBool$new(
                "showMissing",
                showMissing,
                default=FALSE)
            private$..addPercentage <- jmvcore::OptionBool$new(
                "addPercentage",
                addPercentage,
                default=TRUE)
            private$..sortBy <- jmvcore::OptionList$new(
                "sortBy",
                sortBy,
                options=list(
                    "groups",
                    "first_desc",
                    "first_asc"),
                default="groups")

            self$.addOption(private$..groupVars)
            self$.addOption(private$..sumVars)
            self$.addOption(private$..statistics)
            self$.addOption(private$..dateVar)
            self$.addOption(private$..dateFormat)
            self$.addOption(private$..timeAggregation)
            self$.addOption(private$..showMissing)
            self$.addOption(private$..addPercentage)
            self$.addOption(private$..sortBy)
        }),
    active = list(
        groupVars = function() private$..groupVars$value,
        sumVars = function() private$..sumVars$value,
        statistics = function() private$..statistics$value,
        dateVar = function() private$..dateVar$value,
        dateFormat = function() private$..dateFormat$value,
        timeAggregation = function() private$..timeAggregation$value,
        showMissing = function() private$..showMissing$value,
        addPercentage = function() private$..addPercentage$value,
        sortBy = function() private$..sortBy$value),
    private = list(
        ..groupVars = NA,
        ..sumVars = NA,
        ..statistics = NA,
        ..dateVar = NA,
        ..dateFormat = NA,
        ..timeAggregation = NA,
        ..showMissing = NA,
        ..addPercentage = NA,
        ..sortBy = NA)
)

groupsummaryResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "groupsummaryResults",
    inherit = jmvcore::Group,
    active = list(
        todo = function() private$.items[["todo"]],
        dateInfo = function() private$.items[["dateInfo"]],
        summaryTable = function() private$.items[["summaryTable"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Group and Summarize",
                refs=list(
                    "ClinicoPathJamoviModule"))
            self$add(jmvcore::Html$new(
                options=options,
                name="todo",
                title="To Do",
                clearWith=list(
                    "groupVars",
                    "sumVars")))
            self$add(jmvcore::Html$new(
                options=options,
                name="dateInfo",
                title="Date Processing Information",
                clearWith=list(
                    "dateVar",
                    "dateFormat",
                    "timeAggregation"),
                visible="(dateVar)"))
            self$add(jmvcore::Table$new(
                options=options,
                name="summaryTable",
                title="Group Summary Statistics",
                rows=0,
                columns=list(),
                clearWith=list(
                    "groupVars",
                    "sumVars",
                    "statistics",
                    "showMissing",
                    "sortBy",
                    "dateVar",
                    "dateFormat",
                    "timeAggregation")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Summary Plot",
                width=800,
                height=600,
                renderFun=".plot",
                requiresData=TRUE,
                clearWith=list(
                    "groupVars",
                    "sumVars",
                    "statistics",
                    "showMissing",
                    "sortBy",
                    "dateVar",
                    "dateFormat",
                    "timeAggregation")))}))

groupsummaryBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "groupsummaryBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "groupsummary",
                version = c(0,0,3),
                options = options,
                results = groupsummaryResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Group and Summarize
#'
#' Group data by categorical variables and calculate summary statistics
#'
#' @examples
#' \donttest{
#' # Group by one or more categorical variables and sum numeric variables
#' groupsummary(
#'     data = mydata,
#'     groupVars = c("Category", "Group"),
#'     sumVars = c("Value1", "Value2")
#' )
#'}
#' @param data The data as a data frame.
#' @param groupVars Variables to group by (categorical or date).
#' @param sumVars Numeric variables to calculate statistics for each group.
#' @param statistics .
#' @param dateVar Select the date variable from group variables to apply date
#'   formatting.
#' @param dateFormat .
#' @param timeAggregation .
#' @param showMissing Include groups with missing values in the summary.
#' @param addPercentage Add percentage of total for sum values.
#' @param sortBy .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$todo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$dateInfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$summaryTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$summaryTable$asDF}
#'
#' \code{as.data.frame(results$summaryTable)}
#'
#' @export
groupsummary <- function(
    data,
    groupVars,
    sumVars,
    statistics = list(
                "sum",
                "mean",
                "n"),
    dateVar = NULL,
    dateFormat = "ymd",
    timeAggregation = "day",
    showMissing = FALSE,
    addPercentage = TRUE,
    sortBy = "groups") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("groupsummary requires jmvcore to be installed (restart may be required)")

    if ( ! missing(groupVars)) groupVars <- jmvcore::resolveQuo(jmvcore::enquo(groupVars))
    if ( ! missing(sumVars)) sumVars <- jmvcore::resolveQuo(jmvcore::enquo(sumVars))
    if ( ! missing(dateVar)) dateVar <- jmvcore::resolveQuo(jmvcore::enquo(dateVar))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(groupVars), groupVars, NULL),
            `if`( ! missing(sumVars), sumVars, NULL),
            `if`( ! missing(dateVar), dateVar, NULL))


    options <- groupsummaryOptions$new(
        groupVars = groupVars,
        sumVars = sumVars,
        statistics = statistics,
        dateVar = dateVar,
        dateFormat = dateFormat,
        timeAggregation = timeAggregation,
        showMissing = showMissing,
        addPercentage = addPercentage,
        sortBy = sortBy)

    analysis <- groupsummaryClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

