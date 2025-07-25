
# This file is automatically generated, you probably don't want to edit this

timeintervalOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "timeintervalOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dx_date = NULL,
            fu_date = NULL,
            time_format = "auto",
            output_unit = "months",
            use_landmark = FALSE,
            landmark_time = 6,
            remove_negative = TRUE,
            remove_extreme = TRUE,
            add_times = TRUE,
            include_quality_metrics = TRUE,
            confidence_level = 95, ...) {

            super$initialize(
                package="ClinicoPath",
                name="timeinterval",
                requiresData=TRUE,
                ...)

            private$..dx_date <- jmvcore::OptionVariable$new(
                "dx_date",
                dx_date,
                suggested=list(
                    "continuous",
                    "nominal"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..fu_date <- jmvcore::OptionVariable$new(
                "fu_date",
                fu_date,
                suggested=list(
                    "continuous",
                    "nominal"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..time_format <- jmvcore::OptionList$new(
                "time_format",
                time_format,
                options=list(
                    "auto",
                    "ymdhms",
                    "ymd",
                    "dmy",
                    "mdy",
                    "ydm",
                    "myd",
                    "dym"),
                default="auto")
            private$..output_unit <- jmvcore::OptionList$new(
                "output_unit",
                output_unit,
                options=list(
                    "days",
                    "weeks",
                    "months",
                    "years"),
                default="months")
            private$..use_landmark <- jmvcore::OptionBool$new(
                "use_landmark",
                use_landmark,
                default=FALSE)
            private$..landmark_time <- jmvcore::OptionNumber$new(
                "landmark_time",
                landmark_time,
                default=6,
                min=0)
            private$..remove_negative <- jmvcore::OptionBool$new(
                "remove_negative",
                remove_negative,
                default=TRUE)
            private$..remove_extreme <- jmvcore::OptionBool$new(
                "remove_extreme",
                remove_extreme,
                default=TRUE)
            private$..add_times <- jmvcore::OptionBool$new(
                "add_times",
                add_times,
                default=TRUE)
            private$..include_quality_metrics <- jmvcore::OptionBool$new(
                "include_quality_metrics",
                include_quality_metrics,
                default=TRUE)
            private$..confidence_level <- jmvcore::OptionNumber$new(
                "confidence_level",
                confidence_level,
                default=95,
                min=90,
                max=99)

            self$.addOption(private$..dx_date)
            self$.addOption(private$..fu_date)
            self$.addOption(private$..time_format)
            self$.addOption(private$..output_unit)
            self$.addOption(private$..use_landmark)
            self$.addOption(private$..landmark_time)
            self$.addOption(private$..remove_negative)
            self$.addOption(private$..remove_extreme)
            self$.addOption(private$..add_times)
            self$.addOption(private$..include_quality_metrics)
            self$.addOption(private$..confidence_level)
        }),
    active = list(
        dx_date = function() private$..dx_date$value,
        fu_date = function() private$..fu_date$value,
        time_format = function() private$..time_format$value,
        output_unit = function() private$..output_unit$value,
        use_landmark = function() private$..use_landmark$value,
        landmark_time = function() private$..landmark_time$value,
        remove_negative = function() private$..remove_negative$value,
        remove_extreme = function() private$..remove_extreme$value,
        add_times = function() private$..add_times$value,
        include_quality_metrics = function() private$..include_quality_metrics$value,
        confidence_level = function() private$..confidence_level$value),
    private = list(
        ..dx_date = NA,
        ..fu_date = NA,
        ..time_format = NA,
        ..output_unit = NA,
        ..use_landmark = NA,
        ..landmark_time = NA,
        ..remove_negative = NA,
        ..remove_extreme = NA,
        ..add_times = NA,
        ..include_quality_metrics = NA,
        ..confidence_level = NA)
)

timeintervalResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "timeintervalResults",
    inherit = jmvcore::Group,
    active = list(
        todo = function() private$.items[["todo"]],
        personTimeInfo = function() private$.items[["personTimeInfo"]],
        qualityAssessment = function() private$.items[["qualityAssessment"]],
        summary = function() private$.items[["summary"]],
        calculated_time = function() private$.items[["calculated_time"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Comprehensive Time Interval Calculator",
                refs=list(
                    "lubridate"))
            self$add(jmvcore::Html$new(
                options=options,
                name="todo",
                title="Getting Started",
                visible="(dx_date === null || fu_date === null)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="personTimeInfo",
                title="Understanding Person-Time Analysis"))
            self$add(jmvcore::Html$new(
                options=options,
                name="qualityAssessment",
                title="Data Quality Assessment",
                visible="(dx_date !== null && fu_date !== null)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="summary",
                title="Statistical Summary & Person-Time Analysis",
                visible="(dx_date !== null && fu_date !== null)"))
            self$add(jmvcore::Output$new(
                options=options,
                name="calculated_time",
                title="Calculated Time Intervals",
                measureType="continuous",
                clearWith=list(
                    "dx_date",
                    "fu_date",
                    "time_format",
                    "output_unit",
                    "use_landmark",
                    "landmark_time",
                    "remove_negative",
                    "remove_extreme")))}))

timeintervalBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "timeintervalBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "timeinterval",
                version = c(1,0,0),
                options = options,
                results = timeintervalResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Comprehensive Time Interval Calculator
#'
#' Advanced time interval calculator designed for survival analysis, 
#' epidemiological studies,  and person-time analysis. Features intelligent 
#' date parsing, comprehensive data quality  assessment, landmark analysis, 
#' and robust statistical summaries. Time intervals form the  foundation of 
#' person-time follow-up calculations, capturing both participant counts and  
#' observation duration for accurate incidence rate calculations.
#'
#' @examples
#' # Basic time interval calculation:
#' timeinterval(
#'   data = study_data,
#'   dx_date = "diagnosis_date",
#'   fu_date = "followup_date",
#'   time_format = "ymd",
#'   output_unit = "months"
#' )
#'
#' # With landmark analysis:
#' timeinterval(
#'   data = study_data,
#'   dx_date = "start_date",
#'   fu_date = "end_date",
#'   use_landmark = TRUE,
#'   landmark_time = 6,
#'   output_unit = "months"
#' )
#'
#' @param data The data as a data frame containing date columns for interval
#'   calculation.
#' @param dx_date Column containing start dates (e.g., diagnosis date, study
#'   entry, treatment start). Supports various date formats including text and
#'   numeric representations.
#' @param fu_date Column containing end dates (e.g., follow-up date, event
#'   date, study exit). Must be in the same format as the start date variable.
#' @param time_format Date format specification. 'Auto-detect' attempts to
#'   identify the format automatically. Manual selection ensures accurate
#'   parsing for specific date formats.
#' @param output_unit Unit for calculated time intervals. Affects person-time
#'   calculations and  statistical summaries. Choose based on study duration and
#'   event frequency.
#' @param use_landmark Enables conditional analysis from a specific time
#'   point. Useful for  studying outcomes after a landmark time (e.g., 6-month
#'   survivors only).
#' @param landmark_time Time point for landmark analysis in the specified
#'   output units.  Only participants surviving past this time are included in
#'   analysis.
#' @param remove_negative Automatically exclude negative time intervals (end
#'   date before start date). Recommended for data quality assurance.
#' @param remove_extreme Identify and flag potentially extreme time intervals
#'   for quality review. Uses statistical outlier detection methods.
#' @param add_times Appends calculated time intervals as a new variable for
#'   downstream analysis. Useful for subsequent survival analysis or person-time
#'   calculations.
#' @param include_quality_metrics Provides comprehensive data quality
#'   assessment including missing values, negative intervals, and distribution
#'   statistics.
#' @param confidence_level Confidence level for statistical intervals (mean
#'   confidence intervals). Standard epidemiological practice uses 95\%
#'   confidence intervals.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$todo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$personTimeInfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$qualityAssessment} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$summary} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$calculated_time} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' @export
timeinterval <- function(
    data,
    dx_date,
    fu_date,
    time_format = "auto",
    output_unit = "months",
    use_landmark = FALSE,
    landmark_time = 6,
    remove_negative = TRUE,
    remove_extreme = TRUE,
    add_times = TRUE,
    include_quality_metrics = TRUE,
    confidence_level = 95) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("timeinterval requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dx_date)) dx_date <- jmvcore::resolveQuo(jmvcore::enquo(dx_date))
    if ( ! missing(fu_date)) fu_date <- jmvcore::resolveQuo(jmvcore::enquo(fu_date))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dx_date), dx_date, NULL),
            `if`( ! missing(fu_date), fu_date, NULL))


    options <- timeintervalOptions$new(
        dx_date = dx_date,
        fu_date = fu_date,
        time_format = time_format,
        output_unit = output_unit,
        use_landmark = use_landmark,
        landmark_time = landmark_time,
        remove_negative = remove_negative,
        remove_extreme = remove_extreme,
        add_times = add_times,
        include_quality_metrics = include_quality_metrics,
        confidence_level = confidence_level)

    analysis <- timeintervalClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

