
# This file is automatically generated, you probably don't want to edit this

jiwillsurviveOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jiwillsurviveOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            analysis_type = "survival_model",
            time_var = NULL,
            event_var = NULL,
            group_var = NULL,
            start_date_var = NULL,
            end_date_var = NULL,
            derive_followup = FALSE,
            followup_units = "days",
            confidence_level = 0.95,
            show_risk_table = TRUE,
            show_median_survival = TRUE,
            show_confidence_bands = TRUE,
            show_censoring_marks = TRUE,
            plot_style = "iwillsurvive",
            color_palette = "default",
            plot_title = "",
            x_label = "",
            y_label = "",
            time_breaks = "",
            legend_position = "right",
            followup_plot_type = "histogram",
            show_statistics = TRUE,
            show_survival_table = TRUE,
            show_interpretation = TRUE,
            time_points = "",
            export_data = FALSE, ...) {

            super$initialize(
                package="ClinicoPath",
                name="jiwillsurvive",
                requiresData=TRUE,
                ...)

            private$..analysis_type <- jmvcore::OptionList$new(
                "analysis_type",
                analysis_type,
                options=list(
                    "survival_model",
                    "followup_plot",
                    "data_prep",
                    "kaplan_meier"),
                default="survival_model")
            private$..time_var <- jmvcore::OptionVariable$new(
                "time_var",
                time_var,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..event_var <- jmvcore::OptionVariable$new(
                "event_var",
                event_var,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..group_var <- jmvcore::OptionVariable$new(
                "group_var",
                group_var,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..start_date_var <- jmvcore::OptionVariable$new(
                "start_date_var",
                start_date_var,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..end_date_var <- jmvcore::OptionVariable$new(
                "end_date_var",
                end_date_var,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..derive_followup <- jmvcore::OptionBool$new(
                "derive_followup",
                derive_followup,
                default=FALSE)
            private$..followup_units <- jmvcore::OptionList$new(
                "followup_units",
                followup_units,
                options=list(
                    "days",
                    "weeks",
                    "months",
                    "years"),
                default="days")
            private$..confidence_level <- jmvcore::OptionNumber$new(
                "confidence_level",
                confidence_level,
                default=0.95,
                min=0.5,
                max=0.99)
            private$..show_risk_table <- jmvcore::OptionBool$new(
                "show_risk_table",
                show_risk_table,
                default=TRUE)
            private$..show_median_survival <- jmvcore::OptionBool$new(
                "show_median_survival",
                show_median_survival,
                default=TRUE)
            private$..show_confidence_bands <- jmvcore::OptionBool$new(
                "show_confidence_bands",
                show_confidence_bands,
                default=TRUE)
            private$..show_censoring_marks <- jmvcore::OptionBool$new(
                "show_censoring_marks",
                show_censoring_marks,
                default=TRUE)
            private$..plot_style <- jmvcore::OptionList$new(
                "plot_style",
                plot_style,
                options=list(
                    "iwillsurvive",
                    "classic",
                    "modern",
                    "minimal",
                    "publication"),
                default="iwillsurvive")
            private$..color_palette <- jmvcore::OptionList$new(
                "color_palette",
                color_palette,
                options=list(
                    "default",
                    "colorblind",
                    "viridis",
                    "set1",
                    "dark2",
                    "pastel"),
                default="default")
            private$..plot_title <- jmvcore::OptionString$new(
                "plot_title",
                plot_title,
                default="")
            private$..x_label <- jmvcore::OptionString$new(
                "x_label",
                x_label,
                default="")
            private$..y_label <- jmvcore::OptionString$new(
                "y_label",
                y_label,
                default="")
            private$..time_breaks <- jmvcore::OptionString$new(
                "time_breaks",
                time_breaks,
                default="")
            private$..legend_position <- jmvcore::OptionList$new(
                "legend_position",
                legend_position,
                options=list(
                    "right",
                    "left",
                    "top",
                    "bottom",
                    "none"),
                default="right")
            private$..followup_plot_type <- jmvcore::OptionList$new(
                "followup_plot_type",
                followup_plot_type,
                options=list(
                    "histogram",
                    "timeline",
                    "swimmer",
                    "summary"),
                default="histogram")
            private$..show_statistics <- jmvcore::OptionBool$new(
                "show_statistics",
                show_statistics,
                default=TRUE)
            private$..show_survival_table <- jmvcore::OptionBool$new(
                "show_survival_table",
                show_survival_table,
                default=TRUE)
            private$..show_interpretation <- jmvcore::OptionBool$new(
                "show_interpretation",
                show_interpretation,
                default=TRUE)
            private$..time_points <- jmvcore::OptionString$new(
                "time_points",
                time_points,
                default="")
            private$..export_data <- jmvcore::OptionBool$new(
                "export_data",
                export_data,
                default=FALSE)

            self$.addOption(private$..analysis_type)
            self$.addOption(private$..time_var)
            self$.addOption(private$..event_var)
            self$.addOption(private$..group_var)
            self$.addOption(private$..start_date_var)
            self$.addOption(private$..end_date_var)
            self$.addOption(private$..derive_followup)
            self$.addOption(private$..followup_units)
            self$.addOption(private$..confidence_level)
            self$.addOption(private$..show_risk_table)
            self$.addOption(private$..show_median_survival)
            self$.addOption(private$..show_confidence_bands)
            self$.addOption(private$..show_censoring_marks)
            self$.addOption(private$..plot_style)
            self$.addOption(private$..color_palette)
            self$.addOption(private$..plot_title)
            self$.addOption(private$..x_label)
            self$.addOption(private$..y_label)
            self$.addOption(private$..time_breaks)
            self$.addOption(private$..legend_position)
            self$.addOption(private$..followup_plot_type)
            self$.addOption(private$..show_statistics)
            self$.addOption(private$..show_survival_table)
            self$.addOption(private$..show_interpretation)
            self$.addOption(private$..time_points)
            self$.addOption(private$..export_data)
        }),
    active = list(
        analysis_type = function() private$..analysis_type$value,
        time_var = function() private$..time_var$value,
        event_var = function() private$..event_var$value,
        group_var = function() private$..group_var$value,
        start_date_var = function() private$..start_date_var$value,
        end_date_var = function() private$..end_date_var$value,
        derive_followup = function() private$..derive_followup$value,
        followup_units = function() private$..followup_units$value,
        confidence_level = function() private$..confidence_level$value,
        show_risk_table = function() private$..show_risk_table$value,
        show_median_survival = function() private$..show_median_survival$value,
        show_confidence_bands = function() private$..show_confidence_bands$value,
        show_censoring_marks = function() private$..show_censoring_marks$value,
        plot_style = function() private$..plot_style$value,
        color_palette = function() private$..color_palette$value,
        plot_title = function() private$..plot_title$value,
        x_label = function() private$..x_label$value,
        y_label = function() private$..y_label$value,
        time_breaks = function() private$..time_breaks$value,
        legend_position = function() private$..legend_position$value,
        followup_plot_type = function() private$..followup_plot_type$value,
        show_statistics = function() private$..show_statistics$value,
        show_survival_table = function() private$..show_survival_table$value,
        show_interpretation = function() private$..show_interpretation$value,
        time_points = function() private$..time_points$value,
        export_data = function() private$..export_data$value),
    private = list(
        ..analysis_type = NA,
        ..time_var = NA,
        ..event_var = NA,
        ..group_var = NA,
        ..start_date_var = NA,
        ..end_date_var = NA,
        ..derive_followup = NA,
        ..followup_units = NA,
        ..confidence_level = NA,
        ..show_risk_table = NA,
        ..show_median_survival = NA,
        ..show_confidence_bands = NA,
        ..show_censoring_marks = NA,
        ..plot_style = NA,
        ..color_palette = NA,
        ..plot_title = NA,
        ..x_label = NA,
        ..y_label = NA,
        ..time_breaks = NA,
        ..legend_position = NA,
        ..followup_plot_type = NA,
        ..show_statistics = NA,
        ..show_survival_table = NA,
        ..show_interpretation = NA,
        ..time_points = NA,
        ..export_data = NA)
)

jiwillsurviveResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jiwillsurviveResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        survivalPlot = function() private$.items[["survivalPlot"]],
        survivalStats = function() private$.items[["survivalStats"]],
        survivalTable = function() private$.items[["survivalTable"]],
        interpretation = function() private$.items[["interpretation"]],
        kmPlot = function() private$.items[["kmPlot"]],
        kmStats = function() private$.items[["kmStats"]],
        kmTable = function() private$.items[["kmTable"]],
        followupPlot = function() private$.items[["followupPlot"]],
        prepText = function() private$.items[["prepText"]],
        dataOutput = function() private$.items[["dataOutput"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Intuitive Survival Analysis")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Analysis Instructions",
                visible=TRUE))
            self$add(jmvcore::Image$new(
                options=options,
                name="survivalPlot",
                title="Survival Plot",
                width=600,
                height=400,
                visible="(analysis_type:survival_model)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="survivalStats",
                title="Statistical Tests",
                visible="(analysis_type:survival_model && show_statistics)"))
            self$add(jmvcore::Table$new(
                options=options,
                name="survivalTable",
                title="Survival Summary Table",
                rows=0,
                columns=list(
                    list(
                        `name`="Time", 
                        `title`="Time", 
                        `type`="number"),
                    list(
                        `name`="N_Risk", 
                        `title`="N at Risk", 
                        `type`="integer"),
                    list(
                        `name`="N_Event", 
                        `title`="N Events", 
                        `type`="integer"),
                    list(
                        `name`="Survival", 
                        `title`="Survival", 
                        `type`="number"),
                    list(
                        `name`="SE", 
                        `title`="SE", 
                        `type`="number"),
                    list(
                        `name`="Lower_CI", 
                        `title`="Lower CI", 
                        `type`="number"),
                    list(
                        `name`="Upper_CI", 
                        `title`="Upper CI", 
                        `type`="number"),
                    list(
                        `name`="Group", 
                        `title`="Group", 
                        `type`="text")),
                visible="(analysis_type:survival_model && show_survival_table)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="interpretation",
                title="Clinical Interpretation",
                visible="(analysis_type:survival_model && show_interpretation)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="kmPlot",
                title="Kaplan-Meier Plot",
                width=600,
                height=400,
                visible="(analysis_type:kaplan_meier)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="kmStats",
                title="Kaplan-Meier Statistics",
                visible="(analysis_type:kaplan_meier)"))
            self$add(jmvcore::Table$new(
                options=options,
                name="kmTable",
                title="Kaplan-Meier Summary Table",
                rows=0,
                columns=list(
                    list(
                        `name`="Time", 
                        `title`="Time", 
                        `type`="number"),
                    list(
                        `name`="N_Risk", 
                        `title`="N at Risk", 
                        `type`="integer"),
                    list(
                        `name`="N_Event", 
                        `title`="N Events", 
                        `type`="integer"),
                    list(
                        `name`="Survival", 
                        `title`="Survival", 
                        `type`="number"),
                    list(
                        `name`="Lower_CI", 
                        `title`="Lower CI", 
                        `type`="number"),
                    list(
                        `name`="Upper_CI", 
                        `title`="Upper CI", 
                        `type`="number"),
                    list(
                        `name`="Group", 
                        `title`="Group", 
                        `type`="text")),
                visible="(analysis_type:kaplan_meier)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="followupPlot",
                title="Follow-up Visualization",
                width=600,
                height=400,
                visible="(analysis_type:followup_plot)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="prepText",
                title="Data Preparation Summary",
                visible="(analysis_type:data_prep)"))
            self$add(jmvcore::Html$new(
                options=options,
                name="dataOutput",
                title="Processed Data",
                visible="(analysis_type:data_prep && export_data)"))}))

jiwillsurviveBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jiwillsurviveBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "jiwillsurvive",
                version = c(0,0,3),
                options = options,
                results = jiwillsurviveResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Intuitive Survival Analysis
#'
#' Intuitive survival analysis using the iwillsurvive package with 
#' user-friendly interface, automatic data preparation, and comprehensive 
#' visualization options.
#'
#' @examples
#' \donttest{
#' # Example usage:
#' library(iwillsurvive)
#' # Derive follow-up columns
#' data <- derive_followup_days(data, start_date, end_date)
#' # Fit survival model
#' model <- iwillsurvive(time, event, data = data)
#' # Plot results
#' plot(model)
#'}
#' @param data The data as a data frame.
#' @param analysis_type Type of survival analysis to perform.
#' @param time_var Time-to-event or follow-up time variable.
#' @param event_var Event indicator variable (1=event, 0=censored).
#' @param group_var Variable for group comparison.
#' @param start_date_var Start date for follow-up calculation.
#' @param end_date_var End date for follow-up calculation.
#' @param derive_followup Whether to automatically derive follow-up time from
#'   dates.
#' @param followup_units Units for follow-up time calculation.
#' @param confidence_level Confidence level for survival estimates.
#' @param show_risk_table Whether to display risk table below survival plot.
#' @param show_median_survival Whether to display median survival times.
#' @param show_confidence_bands Whether to display confidence intervals around
#'   survival curves.
#' @param show_censoring_marks Whether to mark censored observations on
#'   survival curves.
#' @param plot_style Visual style for survival plots.
#' @param color_palette Color palette for group comparisons.
#' @param plot_title Custom title for the survival plot.
#' @param x_label Custom label for time axis.
#' @param y_label Custom label for survival probability axis.
#' @param time_breaks Custom time points for axis (comma-separated).
#' @param legend_position Position of the legend in the plot.
#' @param followup_plot_type Type of follow-up visualization.
#' @param show_statistics Whether to display statistical test results.
#' @param show_survival_table Whether to display survival summary table.
#' @param show_interpretation Whether to include clinical interpretation.
#' @param time_points Specific time points for survival estimates
#'   (comma-separated).
#' @param export_data Whether to include processed data in output.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$survivalPlot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$survivalStats} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$survivalTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$interpretation} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$kmPlot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$kmStats} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$kmTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$followupPlot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$prepText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$dataOutput} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$survivalTable$asDF}
#'
#' \code{as.data.frame(results$survivalTable)}
#'
#' @export
jiwillsurvive <- function(
    data,
    analysis_type = "survival_model",
    time_var,
    event_var,
    group_var,
    start_date_var,
    end_date_var,
    derive_followup = FALSE,
    followup_units = "days",
    confidence_level = 0.95,
    show_risk_table = TRUE,
    show_median_survival = TRUE,
    show_confidence_bands = TRUE,
    show_censoring_marks = TRUE,
    plot_style = "iwillsurvive",
    color_palette = "default",
    plot_title = "",
    x_label = "",
    y_label = "",
    time_breaks = "",
    legend_position = "right",
    followup_plot_type = "histogram",
    show_statistics = TRUE,
    show_survival_table = TRUE,
    show_interpretation = TRUE,
    time_points = "",
    export_data = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jiwillsurvive requires jmvcore to be installed (restart may be required)")

    if ( ! missing(time_var)) time_var <- jmvcore::resolveQuo(jmvcore::enquo(time_var))
    if ( ! missing(event_var)) event_var <- jmvcore::resolveQuo(jmvcore::enquo(event_var))
    if ( ! missing(group_var)) group_var <- jmvcore::resolveQuo(jmvcore::enquo(group_var))
    if ( ! missing(start_date_var)) start_date_var <- jmvcore::resolveQuo(jmvcore::enquo(start_date_var))
    if ( ! missing(end_date_var)) end_date_var <- jmvcore::resolveQuo(jmvcore::enquo(end_date_var))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(time_var), time_var, NULL),
            `if`( ! missing(event_var), event_var, NULL),
            `if`( ! missing(group_var), group_var, NULL),
            `if`( ! missing(start_date_var), start_date_var, NULL),
            `if`( ! missing(end_date_var), end_date_var, NULL))

    for (v in group_var) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- jiwillsurviveOptions$new(
        analysis_type = analysis_type,
        time_var = time_var,
        event_var = event_var,
        group_var = group_var,
        start_date_var = start_date_var,
        end_date_var = end_date_var,
        derive_followup = derive_followup,
        followup_units = followup_units,
        confidence_level = confidence_level,
        show_risk_table = show_risk_table,
        show_median_survival = show_median_survival,
        show_confidence_bands = show_confidence_bands,
        show_censoring_marks = show_censoring_marks,
        plot_style = plot_style,
        color_palette = color_palette,
        plot_title = plot_title,
        x_label = x_label,
        y_label = y_label,
        time_breaks = time_breaks,
        legend_position = legend_position,
        followup_plot_type = followup_plot_type,
        show_statistics = show_statistics,
        show_survival_table = show_survival_table,
        show_interpretation = show_interpretation,
        time_points = time_points,
        export_data = export_data)

    analysis <- jiwillsurviveClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

