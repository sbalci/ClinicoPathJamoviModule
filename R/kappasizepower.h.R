
# This file is automatically generated, you probably don't want to edit this

kappaSizePowerOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "kappaSizePowerOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            outcome = "2",
            kappa0 = 0.4,
            kappa1 = 0.6,
            props = "0.20 , 0.80",
            raters = "2",
            alpha = 0.05,
            power = 0.8, ...) {

            super$initialize(
                package="ClinicoPath",
                name="kappaSizePower",
                requiresData=FALSE,
                ...)

            private$..outcome <- jmvcore::OptionList$new(
                "outcome",
                outcome,
                options=list(
                    "2",
                    "3",
                    "4",
                    "5"),
                default="2")
            private$..kappa0 <- jmvcore::OptionNumber$new(
                "kappa0",
                kappa0,
                default=0.4,
                min=0.01,
                max=0.99)
            private$..kappa1 <- jmvcore::OptionNumber$new(
                "kappa1",
                kappa1,
                default=0.6,
                min=0.01,
                max=0.99)
            private$..props <- jmvcore::OptionString$new(
                "props",
                props,
                default="0.20 , 0.80")
            private$..raters <- jmvcore::OptionList$new(
                "raters",
                raters,
                options=list(
                    "2",
                    "3",
                    "4",
                    "5"),
                default="2")
            private$..alpha <- jmvcore::OptionNumber$new(
                "alpha",
                alpha,
                default=0.05,
                min=0.01,
                max=0.99)
            private$..power <- jmvcore::OptionNumber$new(
                "power",
                power,
                default=0.8,
                min=0.01,
                max=0.99)

            self$.addOption(private$..outcome)
            self$.addOption(private$..kappa0)
            self$.addOption(private$..kappa1)
            self$.addOption(private$..props)
            self$.addOption(private$..raters)
            self$.addOption(private$..alpha)
            self$.addOption(private$..power)
        }),
    active = list(
        outcome = function() private$..outcome$value,
        kappa0 = function() private$..kappa0$value,
        kappa1 = function() private$..kappa1$value,
        props = function() private$..props$value,
        raters = function() private$..raters$value,
        alpha = function() private$..alpha$value,
        power = function() private$..power$value),
    private = list(
        ..outcome = NA,
        ..kappa0 = NA,
        ..kappa1 = NA,
        ..props = NA,
        ..raters = NA,
        ..alpha = NA,
        ..power = NA)
)

kappaSizePowerResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "kappaSizePowerResults",
    inherit = jmvcore::Group,
    active = list(
        text1 = function() private$.items[["text1"]],
        text2 = function() private$.items[["text2"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Power Approach for the Number of Subjects Required",
                refs=list(
                    "ClinicoPathJamoviModule",
                    "kappaSize"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text1",
                title="Analysis result"))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text2",
                title="Study Explanation"))}))

kappaSizePowerBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "kappaSizePowerBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "kappaSizePower",
                version = c(0,0,3),
                options = options,
                results = kappaSizePowerResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'na')
        }))

#' Power Approach for the Number of Subjects Required
#'
#' Power Analysis for Interobserver Agreement Analysis.
#' 
#'
#' @examples
#' \donttest{
#' # example will be added
#'}
#' @param outcome Number of outcome level.
#' @param kappa0 Expected value of kappa.
#' @param kappa1 Expected value of kappa.
#' @param props Proportions of outcome level.
#' @param raters Number of raters.
#' @param alpha Significance level.
#' @param power Power.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text1} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$text2} \tab \tab \tab \tab \tab a preformatted \cr
#' }
#'
#' @export
kappaSizePower <- function(
    outcome = "2",
    kappa0 = 0.4,
    kappa1 = 0.6,
    props = "0.20 , 0.80",
    raters = "2",
    alpha = 0.05,
    power = 0.8) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("kappaSizePower requires jmvcore to be installed (restart may be required)")


    options <- kappaSizePowerOptions$new(
        outcome = outcome,
        kappa0 = kappa0,
        kappa1 = kappa1,
        props = props,
        raters = raters,
        alpha = alpha,
        power = power)

    analysis <- kappaSizePowerClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

