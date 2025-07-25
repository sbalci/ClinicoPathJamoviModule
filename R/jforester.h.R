
# This file is automatically generated, you probably don't want to edit this

jforesterOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jforesterOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            study_labels = NULL,
            estimates = NULL,
            ci_lower = NULL,
            ci_upper = NULL,
            sample_sizes = NULL,
            events = NULL,
            effect_type = "or",
            confidence_level = "95",
            reference_line = 1,
            log_scale = TRUE,
            plot_title = "",
            x_axis_label = "",
            show_summary = FALSE,
            summary_estimate = 1,
            summary_ci_lower = 0.8,
            summary_ci_upper = 1.2,
            point_size_range = "medium",
            color_scheme = "default",
            custom_point_color = "#2166AC",
            custom_ci_color = "#4D4D4D",
            font_family = "Arial",
            plot_width = 10,
            plot_height = 8,
            dpi = 600,
            show_table = TRUE,
            table_position = "left",
            include_weights = FALSE,
            show_heterogeneity = FALSE,
            arrow_labels = FALSE,
            left_arrow_label = "Favors Control",
            right_arrow_label = "Favors Treatment",
            stripe_rows = TRUE,
            export_format = "png", ...) {

            super$initialize(
                package="ClinicoPath",
                name="jforester",
                requiresData=TRUE,
                ...)

            private$..study_labels <- jmvcore::OptionVariable$new(
                "study_labels",
                study_labels)
            private$..estimates <- jmvcore::OptionVariable$new(
                "estimates",
                estimates)
            private$..ci_lower <- jmvcore::OptionVariable$new(
                "ci_lower",
                ci_lower)
            private$..ci_upper <- jmvcore::OptionVariable$new(
                "ci_upper",
                ci_upper)
            private$..sample_sizes <- jmvcore::OptionVariable$new(
                "sample_sizes",
                sample_sizes,
                default=NULL)
            private$..events <- jmvcore::OptionVariable$new(
                "events",
                events,
                default=NULL)
            private$..effect_type <- jmvcore::OptionList$new(
                "effect_type",
                effect_type,
                options=list(
                    "or",
                    "rr",
                    "hr",
                    "md",
                    "smd",
                    "custom"),
                default="or")
            private$..confidence_level <- jmvcore::OptionList$new(
                "confidence_level",
                confidence_level,
                options=list(
                    "95",
                    "90",
                    "99"),
                default="95")
            private$..reference_line <- jmvcore::OptionNumber$new(
                "reference_line",
                reference_line,
                default=1)
            private$..log_scale <- jmvcore::OptionBool$new(
                "log_scale",
                log_scale,
                default=TRUE)
            private$..plot_title <- jmvcore::OptionString$new(
                "plot_title",
                plot_title,
                default="")
            private$..x_axis_label <- jmvcore::OptionString$new(
                "x_axis_label",
                x_axis_label,
                default="")
            private$..show_summary <- jmvcore::OptionBool$new(
                "show_summary",
                show_summary,
                default=FALSE)
            private$..summary_estimate <- jmvcore::OptionNumber$new(
                "summary_estimate",
                summary_estimate,
                default=1)
            private$..summary_ci_lower <- jmvcore::OptionNumber$new(
                "summary_ci_lower",
                summary_ci_lower,
                default=0.8)
            private$..summary_ci_upper <- jmvcore::OptionNumber$new(
                "summary_ci_upper",
                summary_ci_upper,
                default=1.2)
            private$..point_size_range <- jmvcore::OptionList$new(
                "point_size_range",
                point_size_range,
                options=list(
                    "small",
                    "medium",
                    "large"),
                default="medium")
            private$..color_scheme <- jmvcore::OptionList$new(
                "color_scheme",
                color_scheme,
                options=list(
                    "default",
                    "medical",
                    "forest",
                    "grayscale",
                    "custom"),
                default="default")
            private$..custom_point_color <- jmvcore::OptionString$new(
                "custom_point_color",
                custom_point_color,
                default="#2166AC")
            private$..custom_ci_color <- jmvcore::OptionString$new(
                "custom_ci_color",
                custom_ci_color,
                default="#4D4D4D")
            private$..font_family <- jmvcore::OptionList$new(
                "font_family",
                font_family,
                options=list(
                    "Arial",
                    "Times",
                    "Helvetica",
                    "Calibri"),
                default="Arial")
            private$..plot_width <- jmvcore::OptionNumber$new(
                "plot_width",
                plot_width,
                min=6,
                max=16,
                default=10)
            private$..plot_height <- jmvcore::OptionNumber$new(
                "plot_height",
                plot_height,
                min=4,
                max=20,
                default=8)
            private$..dpi <- jmvcore::OptionInteger$new(
                "dpi",
                dpi,
                min=150,
                max=1200,
                default=600)
            private$..show_table <- jmvcore::OptionBool$new(
                "show_table",
                show_table,
                default=TRUE)
            private$..table_position <- jmvcore::OptionList$new(
                "table_position",
                table_position,
                options=list(
                    "left",
                    "right",
                    "below"),
                default="left")
            private$..include_weights <- jmvcore::OptionBool$new(
                "include_weights",
                include_weights,
                default=FALSE)
            private$..show_heterogeneity <- jmvcore::OptionBool$new(
                "show_heterogeneity",
                show_heterogeneity,
                default=FALSE)
            private$..arrow_labels <- jmvcore::OptionBool$new(
                "arrow_labels",
                arrow_labels,
                default=FALSE)
            private$..left_arrow_label <- jmvcore::OptionString$new(
                "left_arrow_label",
                left_arrow_label,
                default="Favors Control")
            private$..right_arrow_label <- jmvcore::OptionString$new(
                "right_arrow_label",
                right_arrow_label,
                default="Favors Treatment")
            private$..stripe_rows <- jmvcore::OptionBool$new(
                "stripe_rows",
                stripe_rows,
                default=TRUE)
            private$..export_format <- jmvcore::OptionList$new(
                "export_format",
                export_format,
                options=list(
                    "png",
                    "pdf",
                    "svg",
                    "tiff"),
                default="png")

            self$.addOption(private$..study_labels)
            self$.addOption(private$..estimates)
            self$.addOption(private$..ci_lower)
            self$.addOption(private$..ci_upper)
            self$.addOption(private$..sample_sizes)
            self$.addOption(private$..events)
            self$.addOption(private$..effect_type)
            self$.addOption(private$..confidence_level)
            self$.addOption(private$..reference_line)
            self$.addOption(private$..log_scale)
            self$.addOption(private$..plot_title)
            self$.addOption(private$..x_axis_label)
            self$.addOption(private$..show_summary)
            self$.addOption(private$..summary_estimate)
            self$.addOption(private$..summary_ci_lower)
            self$.addOption(private$..summary_ci_upper)
            self$.addOption(private$..point_size_range)
            self$.addOption(private$..color_scheme)
            self$.addOption(private$..custom_point_color)
            self$.addOption(private$..custom_ci_color)
            self$.addOption(private$..font_family)
            self$.addOption(private$..plot_width)
            self$.addOption(private$..plot_height)
            self$.addOption(private$..dpi)
            self$.addOption(private$..show_table)
            self$.addOption(private$..table_position)
            self$.addOption(private$..include_weights)
            self$.addOption(private$..show_heterogeneity)
            self$.addOption(private$..arrow_labels)
            self$.addOption(private$..left_arrow_label)
            self$.addOption(private$..right_arrow_label)
            self$.addOption(private$..stripe_rows)
            self$.addOption(private$..export_format)
        }),
    active = list(
        study_labels = function() private$..study_labels$value,
        estimates = function() private$..estimates$value,
        ci_lower = function() private$..ci_lower$value,
        ci_upper = function() private$..ci_upper$value,
        sample_sizes = function() private$..sample_sizes$value,
        events = function() private$..events$value,
        effect_type = function() private$..effect_type$value,
        confidence_level = function() private$..confidence_level$value,
        reference_line = function() private$..reference_line$value,
        log_scale = function() private$..log_scale$value,
        plot_title = function() private$..plot_title$value,
        x_axis_label = function() private$..x_axis_label$value,
        show_summary = function() private$..show_summary$value,
        summary_estimate = function() private$..summary_estimate$value,
        summary_ci_lower = function() private$..summary_ci_lower$value,
        summary_ci_upper = function() private$..summary_ci_upper$value,
        point_size_range = function() private$..point_size_range$value,
        color_scheme = function() private$..color_scheme$value,
        custom_point_color = function() private$..custom_point_color$value,
        custom_ci_color = function() private$..custom_ci_color$value,
        font_family = function() private$..font_family$value,
        plot_width = function() private$..plot_width$value,
        plot_height = function() private$..plot_height$value,
        dpi = function() private$..dpi$value,
        show_table = function() private$..show_table$value,
        table_position = function() private$..table_position$value,
        include_weights = function() private$..include_weights$value,
        show_heterogeneity = function() private$..show_heterogeneity$value,
        arrow_labels = function() private$..arrow_labels$value,
        left_arrow_label = function() private$..left_arrow_label$value,
        right_arrow_label = function() private$..right_arrow_label$value,
        stripe_rows = function() private$..stripe_rows$value,
        export_format = function() private$..export_format$value),
    private = list(
        ..study_labels = NA,
        ..estimates = NA,
        ..ci_lower = NA,
        ..ci_upper = NA,
        ..sample_sizes = NA,
        ..events = NA,
        ..effect_type = NA,
        ..confidence_level = NA,
        ..reference_line = NA,
        ..log_scale = NA,
        ..plot_title = NA,
        ..x_axis_label = NA,
        ..show_summary = NA,
        ..summary_estimate = NA,
        ..summary_ci_lower = NA,
        ..summary_ci_upper = NA,
        ..point_size_range = NA,
        ..color_scheme = NA,
        ..custom_point_color = NA,
        ..custom_ci_color = NA,
        ..font_family = NA,
        ..plot_width = NA,
        ..plot_height = NA,
        ..dpi = NA,
        ..show_table = NA,
        ..table_position = NA,
        ..include_weights = NA,
        ..show_heterogeneity = NA,
        ..arrow_labels = NA,
        ..left_arrow_label = NA,
        ..right_arrow_label = NA,
        ..stripe_rows = NA,
        ..export_format = NA)
)

jforesterResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jforesterResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        forest_plot = function() private$.items[["forest_plot"]],
        data_table = function() private$.items[["data_table"]],
        summary_statistics = function() private$.items[["summary_statistics"]],
        interpretation = function() private$.items[["interpretation"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Forest Plot Visualization")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Analysis Instructions",
                visible=TRUE))
            self$add(jmvcore::Image$new(
                options=options,
                name="forest_plot",
                title="Forest Plot",
                width=800,
                height=600,
                renderFun=".plot_forest",
                visible=TRUE,
                clearWith=list(
                    "study_labels",
                    "estimates",
                    "ci_lower",
                    "ci_upper",
                    "effect_type",
                    "log_scale",
                    "reference_line",
                    "color_scheme",
                    "custom_point_color",
                    "custom_ci_color")))
            self$add(jmvcore::Table$new(
                options=options,
                name="data_table",
                title="Study Data",
                visible="(show_table)",
                columns=list(
                    list(
                        `name`="study", 
                        `title`="Study/Group", 
                        `type`="text"),
                    list(
                        `name`="estimate", 
                        `title`="Estimate", 
                        `type`="number", 
                        `format`="zto,dp:3"),
                    list(
                        `name`="ci_lower", 
                        `title`="Lower CI", 
                        `type`="number", 
                        `format`="zto,dp:3"),
                    list(
                        `name`="ci_upper", 
                        `title`="Upper CI", 
                        `type`="number", 
                        `format`="zto,dp:3"),
                    list(
                        `name`="sample_size", 
                        `title`="N", 
                        `type`="integer"),
                    list(
                        `name`="events", 
                        `title`="Events", 
                        `type`="integer"),
                    list(
                        `name`="weight", 
                        `title`="Weight (%)", 
                        `type`="number", 
                        `format`="zto,dp:1")),
                clearWith=list(
                    "study_labels",
                    "estimates",
                    "ci_lower",
                    "ci_upper",
                    "sample_sizes",
                    "events")))
            self$add(jmvcore::Table$new(
                options=options,
                name="summary_statistics",
                title="Summary Statistics",
                visible="(show_summary || show_heterogeneity)",
                columns=list(
                    list(
                        `name`="statistic", 
                        `title`="Statistic", 
                        `type`="text"),
                    list(
                        `name`="value", 
                        `title`="Value", 
                        `type`="text")),
                clearWith=list(
                    "estimates",
                    "ci_lower",
                    "ci_upper",
                    "show_summary",
                    "show_heterogeneity")))
            self$add(jmvcore::Html$new(
                options=options,
                name="interpretation",
                title="Forest Plot Interpretation",
                visible=TRUE))}))

jforesterBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jforesterBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "jforester",
                version = c(1,0,0),
                options = options,
                results = jforesterResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Forest Plot Visualization
#'
#' Create publication-ready forest plots for meta-analyses, subgroup analyses, 
#' and clinical trial results. Display point estimates with confidence 
#' intervals
#' in a professional format suitable for academic publications.
#' 
#' @param data Dataset for forest plot analysis
#' @param study_labels Variable containing study or subgroup names
#' @param estimates Point estimates (odds ratios, risk ratios, mean
#'   differences, etc.)
#' @param ci_lower Lower bounds of confidence intervals
#' @param ci_upper Upper bounds of confidence intervals
#' @param sample_sizes Sample sizes for each study/group (optional)
#' @param events Number of events or cases (optional)
#' @param effect_type Type of effect measure being displayed
#' @param confidence_level Confidence level for intervals
#' @param reference_line Position of vertical reference line
#' @param log_scale Display forest plot on logarithmic scale
#' @param plot_title Custom title for forest plot
#' @param x_axis_label Custom x-axis label
#' @param show_summary Display overall summary effect at bottom
#' @param summary_estimate Overall summary effect estimate
#' @param summary_ci_lower Lower bound of summary confidence interval
#' @param summary_ci_upper Upper bound of summary confidence interval
#' @param point_size_range Range of point sizes based on sample size
#' @param color_scheme Color scheme for points and confidence intervals
#' @param custom_point_color Custom color for points (hex code)
#' @param custom_ci_color Custom color for confidence intervals (hex code)
#' @param font_family Font family for plot text
#' @param plot_width Width of exported plot
#' @param plot_height Height of exported plot
#' @param dpi Resolution for exported plot
#' @param show_table Display accompanying data table
#' @param table_position Position of data table relative to plot
#' @param include_weights Display study weights in table (requires sample
#'   sizes)
#' @param show_heterogeneity Display I² and Q-test statistics
#' @param arrow_labels Add directional arrows with labels
#' @param left_arrow_label Label for left arrow
#' @param right_arrow_label Label for right arrow
#' @param stripe_rows Add alternating row colors for readability
#' @param export_format Format for plot export
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$forest_plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$data_table} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$summary_statistics} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$interpretation} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$data_table$asDF}
#'
#' \code{as.data.frame(results$data_table)}
#'
#' @export
jforester <- function(
    data,
    study_labels,
    estimates,
    ci_lower,
    ci_upper,
    sample_sizes = NULL,
    events = NULL,
    effect_type = "or",
    confidence_level = "95",
    reference_line = 1,
    log_scale = TRUE,
    plot_title = "",
    x_axis_label = "",
    show_summary = FALSE,
    summary_estimate = 1,
    summary_ci_lower = 0.8,
    summary_ci_upper = 1.2,
    point_size_range = "medium",
    color_scheme = "default",
    custom_point_color = "#2166AC",
    custom_ci_color = "#4D4D4D",
    font_family = "Arial",
    plot_width = 10,
    plot_height = 8,
    dpi = 600,
    show_table = TRUE,
    table_position = "left",
    include_weights = FALSE,
    show_heterogeneity = FALSE,
    arrow_labels = FALSE,
    left_arrow_label = "Favors Control",
    right_arrow_label = "Favors Treatment",
    stripe_rows = TRUE,
    export_format = "png") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jforester requires jmvcore to be installed (restart may be required)")

    if ( ! missing(study_labels)) study_labels <- jmvcore::resolveQuo(jmvcore::enquo(study_labels))
    if ( ! missing(estimates)) estimates <- jmvcore::resolveQuo(jmvcore::enquo(estimates))
    if ( ! missing(ci_lower)) ci_lower <- jmvcore::resolveQuo(jmvcore::enquo(ci_lower))
    if ( ! missing(ci_upper)) ci_upper <- jmvcore::resolveQuo(jmvcore::enquo(ci_upper))
    if ( ! missing(sample_sizes)) sample_sizes <- jmvcore::resolveQuo(jmvcore::enquo(sample_sizes))
    if ( ! missing(events)) events <- jmvcore::resolveQuo(jmvcore::enquo(events))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(study_labels), study_labels, NULL),
            `if`( ! missing(estimates), estimates, NULL),
            `if`( ! missing(ci_lower), ci_lower, NULL),
            `if`( ! missing(ci_upper), ci_upper, NULL),
            `if`( ! missing(sample_sizes), sample_sizes, NULL),
            `if`( ! missing(events), events, NULL))


    options <- jforesterOptions$new(
        study_labels = study_labels,
        estimates = estimates,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        sample_sizes = sample_sizes,
        events = events,
        effect_type = effect_type,
        confidence_level = confidence_level,
        reference_line = reference_line,
        log_scale = log_scale,
        plot_title = plot_title,
        x_axis_label = x_axis_label,
        show_summary = show_summary,
        summary_estimate = summary_estimate,
        summary_ci_lower = summary_ci_lower,
        summary_ci_upper = summary_ci_upper,
        point_size_range = point_size_range,
        color_scheme = color_scheme,
        custom_point_color = custom_point_color,
        custom_ci_color = custom_ci_color,
        font_family = font_family,
        plot_width = plot_width,
        plot_height = plot_height,
        dpi = dpi,
        show_table = show_table,
        table_position = table_position,
        include_weights = include_weights,
        show_heterogeneity = show_heterogeneity,
        arrow_labels = arrow_labels,
        left_arrow_label = left_arrow_label,
        right_arrow_label = right_arrow_label,
        stripe_rows = stripe_rows,
        export_format = export_format)

    analysis <- jforesterClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

