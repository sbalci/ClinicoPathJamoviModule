
# This file is automatically generated, you probably don't want to edit this

tinytableOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "tinytableOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            group_var = NULL,
            table_type = "summary",
            show_statistics = TRUE,
            show_counts = TRUE,
            show_missing = FALSE,
            table_theme = "clinical",
            table_title = "Data Summary Table",
            table_notes = "",
            output_format = "html",
            column_width = 0.8,
            precision_digits = 2,
            style_alternating = TRUE,
            style_borders = "all",
            font_size = "normal",
            show_interpretation = TRUE, ...) {

            super$initialize(
                package="ClinicoPath",
                name="tinytable",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "continuous",
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "numeric",
                    "factor"))
            private$..group_var <- jmvcore::OptionVariable$new(
                "group_var",
                group_var,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..table_type <- jmvcore::OptionList$new(
                "table_type",
                table_type,
                options=list(
                    "summary",
                    "descriptive",
                    "grouped",
                    "raw",
                    "custom"),
                default="summary")
            private$..show_statistics <- jmvcore::OptionBool$new(
                "show_statistics",
                show_statistics,
                default=TRUE)
            private$..show_counts <- jmvcore::OptionBool$new(
                "show_counts",
                show_counts,
                default=TRUE)
            private$..show_missing <- jmvcore::OptionBool$new(
                "show_missing",
                show_missing,
                default=FALSE)
            private$..table_theme <- jmvcore::OptionList$new(
                "table_theme",
                table_theme,
                options=list(
                    "clinical",
                    "modern",
                    "publication",
                    "minimal",
                    "bootstrap"),
                default="clinical")
            private$..table_title <- jmvcore::OptionString$new(
                "table_title",
                table_title,
                default="Data Summary Table")
            private$..table_notes <- jmvcore::OptionString$new(
                "table_notes",
                table_notes,
                default="")
            private$..output_format <- jmvcore::OptionList$new(
                "output_format",
                output_format,
                options=list(
                    "html",
                    "pdf",
                    "word",
                    "latex",
                    "markdown"),
                default="html")
            private$..column_width <- jmvcore::OptionNumber$new(
                "column_width",
                column_width,
                min=0.3,
                max=1,
                default=0.8)
            private$..precision_digits <- jmvcore::OptionNumber$new(
                "precision_digits",
                precision_digits,
                min=0,
                max=5,
                default=2)
            private$..style_alternating <- jmvcore::OptionBool$new(
                "style_alternating",
                style_alternating,
                default=TRUE)
            private$..style_borders <- jmvcore::OptionList$new(
                "style_borders",
                style_borders,
                options=list(
                    "all",
                    "horizontal",
                    "minimal",
                    "none"),
                default="all")
            private$..font_size <- jmvcore::OptionList$new(
                "font_size",
                font_size,
                options=list(
                    "small",
                    "normal",
                    "large"),
                default="normal")
            private$..show_interpretation <- jmvcore::OptionBool$new(
                "show_interpretation",
                show_interpretation,
                default=TRUE)

            self$.addOption(private$..vars)
            self$.addOption(private$..group_var)
            self$.addOption(private$..table_type)
            self$.addOption(private$..show_statistics)
            self$.addOption(private$..show_counts)
            self$.addOption(private$..show_missing)
            self$.addOption(private$..table_theme)
            self$.addOption(private$..table_title)
            self$.addOption(private$..table_notes)
            self$.addOption(private$..output_format)
            self$.addOption(private$..column_width)
            self$.addOption(private$..precision_digits)
            self$.addOption(private$..style_alternating)
            self$.addOption(private$..style_borders)
            self$.addOption(private$..font_size)
            self$.addOption(private$..show_interpretation)
        }),
    active = list(
        vars = function() private$..vars$value,
        group_var = function() private$..group_var$value,
        table_type = function() private$..table_type$value,
        show_statistics = function() private$..show_statistics$value,
        show_counts = function() private$..show_counts$value,
        show_missing = function() private$..show_missing$value,
        table_theme = function() private$..table_theme$value,
        table_title = function() private$..table_title$value,
        table_notes = function() private$..table_notes$value,
        output_format = function() private$..output_format$value,
        column_width = function() private$..column_width$value,
        precision_digits = function() private$..precision_digits$value,
        style_alternating = function() private$..style_alternating$value,
        style_borders = function() private$..style_borders$value,
        font_size = function() private$..font_size$value,
        show_interpretation = function() private$..show_interpretation$value),
    private = list(
        ..vars = NA,
        ..group_var = NA,
        ..table_type = NA,
        ..show_statistics = NA,
        ..show_counts = NA,
        ..show_missing = NA,
        ..table_theme = NA,
        ..table_title = NA,
        ..table_notes = NA,
        ..output_format = NA,
        ..column_width = NA,
        ..precision_digits = NA,
        ..style_alternating = NA,
        ..style_borders = NA,
        ..font_size = NA,
        ..show_interpretation = NA)
)

tinytableResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "tinytableResults",
    inherit = jmvcore::Group,
    active = list(
        todo = function() private$.items[["todo"]],
        table = function() private$.items[["table"]],
        interpretation = function() private$.items[["interpretation"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Modern Table Formatting")
            self$add(jmvcore::Html$new(
                options=options,
                name="todo",
                title="Instructions",
                visible=FALSE))
            self$add(jmvcore::Html$new(
                options=options,
                name="table",
                title="Modern Table"))
            self$add(jmvcore::Html$new(
                options=options,
                name="interpretation",
                title="Usage Guide",
                visible="(show_interpretation)"))}))

tinytableBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "tinytableBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "tinytable",
                version = c(0,0,3),
                options = options,
                results = tinytableResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Modern Table Formatting
#'
#' Creates modern, publication-ready tables using the tinytable package.
#' This module provides lightweight, zero-dependency table formatting with 
#' multiple output formats including HTML, LaTeX, PDF, and Word. Designed
#' to complement existing ClinicoPath table modules with modern styling,
#' flexible formatting, and clean aesthetics. Perfect for enhancing data
#' presentation in clinical research publications and reports.
#' 
#'
#' @examples
#' \donttest{
#' # Example:
#' # 1. Select variables for table display.
#' # 2. Choose grouping variables for organized presentation.
#' # 3. Customize styling, themes, and formatting options.
#' # 4. Generate publication-ready tables in multiple formats.
#'}
#' @param data The data as a data frame.
#' @param vars Variables to include in the table display.
#' @param group_var Optional variable for grouping rows or creating summary
#'   tables.
#' @param table_type Type of table to generate with appropriate formatting.
#' @param show_statistics If TRUE, includes statistical summaries (mean, SD,
#'   etc.) for numeric variables.
#' @param show_counts If TRUE, shows counts and percentages for categorical
#'   variables.
#' @param show_missing If TRUE, includes information about missing values.
#' @param table_theme Visual theme for table styling and appearance.
#' @param table_title Title for the table.
#' @param table_notes Optional notes to display below the table.
#' @param output_format Primary output format for the table.
#' @param column_width Width of the table as proportion of available space.
#' @param precision_digits Number of decimal places for numeric values.
#' @param style_alternating If TRUE, applies alternating row background
#'   colors.
#' @param style_borders Border style for the table.
#' @param font_size Font size for table text.
#' @param show_interpretation If TRUE, displays interpretation and usage
#'   guidelines.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$todo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$table} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$interpretation} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' @export
tinytable <- function(
    data,
    vars,
    group_var,
    table_type = "summary",
    show_statistics = TRUE,
    show_counts = TRUE,
    show_missing = FALSE,
    table_theme = "clinical",
    table_title = "Data Summary Table",
    table_notes = "",
    output_format = "html",
    column_width = 0.8,
    precision_digits = 2,
    style_alternating = TRUE,
    style_borders = "all",
    font_size = "normal",
    show_interpretation = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("tinytable requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if ( ! missing(group_var)) group_var <- jmvcore::resolveQuo(jmvcore::enquo(group_var))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL),
            `if`( ! missing(group_var), group_var, NULL))

    for (v in group_var) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- tinytableOptions$new(
        vars = vars,
        group_var = group_var,
        table_type = table_type,
        show_statistics = show_statistics,
        show_counts = show_counts,
        show_missing = show_missing,
        table_theme = table_theme,
        table_title = table_title,
        table_notes = table_notes,
        output_format = output_format,
        column_width = column_width,
        precision_digits = precision_digits,
        style_alternating = style_alternating,
        style_borders = style_borders,
        font_size = font_size,
        show_interpretation = show_interpretation)

    analysis <- tinytableClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

