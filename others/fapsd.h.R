
# This file is automatically generated, you probably don't want to edit this

faPSDOptions <- if (requireNamespace('jmvcore')) R6::R6Class(
    "faPSDOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            y1 = NULL,
            standardise = "meanSD",
            removeTrend = "nodet",
            polydet_order = 1,
            fs = 1,
            nfft = "nyquist",
            userNFFT = 256, ...) {

            super$initialize(
                package='ClinicoPath',
                name='faPSD',
                requiresData=TRUE,
                ...)

            private$..y1 <- jmvcore::OptionVariable$new(
                "y1",
                y1)
            private$..standardise <- jmvcore::OptionList$new(
                "standardise",
                standardise,
                options=list(
                    "none",
                    "meanSD",
                    "medianMAD"),
                default="meanSD")
            private$..removeTrend <- jmvcore::OptionList$new(
                "removeTrend",
                removeTrend,
                options=list(
                    "nodet",
                    "polydet"),
                default="nodet")
            private$..polydet_order <- jmvcore::OptionInteger$new(
                "polydet_order",
                polydet_order,
                default=1)
            private$..fs <- jmvcore::OptionInteger$new(
                "fs",
                fs,
                default=1)
            private$..nfft <- jmvcore::OptionList$new(
                "nfft",
                nfft,
                options=list(
                    "nyquist",
                    "user"),
                default="nyquist")
            private$..userNFFT <- jmvcore::OptionInteger$new(
                "userNFFT",
                userNFFT,
                default=256)

            self$.addOption(private$..y1)
            self$.addOption(private$..standardise)
            self$.addOption(private$..removeTrend)
            self$.addOption(private$..polydet_order)
            self$.addOption(private$..fs)
            self$.addOption(private$..nfft)
            self$.addOption(private$..userNFFT)
        }),
    active = list(
        y1 = function() private$..y1$value,
        standardise = function() private$..standardise$value,
        removeTrend = function() private$..removeTrend$value,
        polydet_order = function() private$..polydet_order$value,
        fs = function() private$..fs$value,
        nfft = function() private$..nfft$value,
        userNFFT = function() private$..userNFFT$value),
    private = list(
        ..y1 = NA,
        ..standardise = NA,
        ..removeTrend = NA,
        ..polydet_order = NA,
        ..fs = NA,
        ..nfft = NA,
        ..userNFFT = NA)
)

faPSDResults <- if (requireNamespace('jmvcore')) R6::R6Class(
    inherit = jmvcore::Group,
    active = list(
        tblTS = function() private$.items[["tblTS"]],
        tblPSD = function() private$.items[["tblPSD"]],
        tsplot = function() private$.items[["tsplot"]],
        psdplot = function() private$.items[["psdplot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Power Spectral Density Slope")
            self$add(jmvcore::Table$new(
                options=options,
                name="tblTS",
                title="Time series descriptives",
                rows=2,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="N", 
                        `type`="integer", 
                        `title`="valid N"),
                    list(
                        `name`="na", 
                        `type`="integer", 
                        `title`="NA values"),
                    list(
                        `name`="median", 
                        `type`="number"),
                    list(
                        `name`="mad", 
                        `type`="number"),
                    list(
                        `name`="mean", 
                        `type`="number"),
                    list(
                        `name`="sd", 
                        `type`="number"),
                    list(
                        `name`="standardise", 
                        `type`="text"),
                    list(
                        `name`="detrending", 
                        `title`="detrended", 
                        `type`="text"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="tblPSD",
                title="PSD parameters and results",
                rows=2,
                columns=list(
                    list(
                        `name`="method", 
                        `title`="regression method", 
                        `type`="text"),
                    list(
                        `name`="antiper", 
                        `title`="antipersistent?", 
                        `type`="text"),
                    list(
                        `name`="intercept", 
                        `title`="log-log intercept", 
                        `type`="number"),
                    list(
                        `name`="slope", 
                        `title`="log-log slope", 
                        `type`="number"),
                    list(
                        `name`="H", 
                        `title`="Hurst", 
                        `type`="number"),
                    list(
                        `name`="FD", 
                        `title`="informed FD", 
                        `type`="number"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="tsplot",
                title="Time series",
                width=700,
                height=300,
                renderFun=".tsplot"))
            self$add(jmvcore::Image$new(
                options=options,
                name="psdplot",
                title="log-log regression",
                width=700,
                height=400,
                renderFun=".psdplot"))}))

faPSDBase <- if (requireNamespace('jmvcore')) R6::R6Class(
    "faPSDBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = 'ClinicoPath',
                name = 'faPSD',
                version = c(1,0,0),
                options = options,
                results = faPSDResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Power Spectral Density Slope
#'
#' 
#' @param data .
#' @param y1 .
#' @param standardise .
#' @param removeTrend .
#' @param polydet_order .
#' @param fs .
#' @param nfft .
#' @param userNFFT .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$tblTS} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$tblPSD} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$tsplot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$psdplot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$tblTS$asDF}
#'
#' \code{as.data.frame(results$tblTS)}
#'
#' @export
faPSD <- function(
    data,
    y1,
    standardise = "meanSD",
    removeTrend = "nodet",
    polydet_order = 1,
    fs = 1,
    nfft = "nyquist",
    userNFFT = 256) {

    if ( ! requireNamespace('jmvcore'))
        stop('faPSD requires jmvcore to be installed (restart may be required)')

    if ( ! missing(y1)) y1 <- jmvcore::resolveQuo(jmvcore::enquo(y1))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(y1), y1, NULL))


    options <- faPSDOptions$new(
        y1 = y1,
        standardise = standardise,
        removeTrend = removeTrend,
        polydet_order = polydet_order,
        fs = fs,
        nfft = nfft,
        userNFFT = userNFFT)

    analysis <- faPSDClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
