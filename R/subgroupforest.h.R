
# This file is automatically generated, you probably don't want to edit this

subgroupforestOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "subgroupforestOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            outcome = NULL,
            treatment = NULL,
            subgroups = NULL,
            time = NULL,
            event = NULL,
            outcomeType = "survival",
            effectMeasure = "hr",
            confidenceLevel = "0.95",
            showOverall = TRUE,
            showInteraction = TRUE,
            sortBy = "effect",
            showSampleSizes = TRUE,
            logScale = TRUE,
            nullLine = 1, ...) {

            super$initialize(
                package="ClinicoPath",
                name="subgroupforest",
                requiresData=TRUE,
                ...)

            private$..outcome <- jmvcore::OptionVariable$new(
                "outcome",
                outcome,
                suggested=list(
                    "continuous",
                    "nominal"))
            private$..treatment <- jmvcore::OptionVariable$new(
                "treatment",
                treatment,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..subgroups <- jmvcore::OptionVariables$new(
                "subgroups",
                subgroups,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..time <- jmvcore::OptionVariable$new(
                "time",
                time,
                suggested=list(
                    "continuous"))
            private$..event <- jmvcore::OptionVariable$new(
                "event",
                event,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..outcomeType <- jmvcore::OptionList$new(
                "outcomeType",
                outcomeType,
                options=list(
                    "survival",
                    "binary",
                    "continuous"),
                default="survival")
            private$..effectMeasure <- jmvcore::OptionList$new(
                "effectMeasure",
                effectMeasure,
                options=list(
                    "hr",
                    "or",
                    "rr",
                    "md"),
                default="hr")
            private$..confidenceLevel <- jmvcore::OptionList$new(
                "confidenceLevel",
                confidenceLevel,
                options=list(
                    "0.95",
                    "0.90",
                    "0.99"),
                default="0.95")
            private$..showOverall <- jmvcore::OptionBool$new(
                "showOverall",
                showOverall,
                default=TRUE)
            private$..showInteraction <- jmvcore::OptionBool$new(
                "showInteraction",
                showInteraction,
                default=TRUE)
            private$..sortBy <- jmvcore::OptionList$new(
                "sortBy",
                sortBy,
                options=list(
                    "effect",
                    "n",
                    "alpha"),
                default="effect")
            private$..showSampleSizes <- jmvcore::OptionBool$new(
                "showSampleSizes",
                showSampleSizes,
                default=TRUE)
            private$..logScale <- jmvcore::OptionBool$new(
                "logScale",
                logScale,
                default=TRUE)
            private$..nullLine <- jmvcore::OptionNumber$new(
                "nullLine",
                nullLine,
                default=1)

            self$.addOption(private$..outcome)
            self$.addOption(private$..treatment)
            self$.addOption(private$..subgroups)
            self$.addOption(private$..time)
            self$.addOption(private$..event)
            self$.addOption(private$..outcomeType)
            self$.addOption(private$..effectMeasure)
            self$.addOption(private$..confidenceLevel)
            self$.addOption(private$..showOverall)
            self$.addOption(private$..showInteraction)
            self$.addOption(private$..sortBy)
            self$.addOption(private$..showSampleSizes)
            self$.addOption(private$..logScale)
            self$.addOption(private$..nullLine)
        }),
    active = list(
        outcome = function() private$..outcome$value,
        treatment = function() private$..treatment$value,
        subgroups = function() private$..subgroups$value,
        time = function() private$..time$value,
        event = function() private$..event$value,
        outcomeType = function() private$..outcomeType$value,
        effectMeasure = function() private$..effectMeasure$value,
        confidenceLevel = function() private$..confidenceLevel$value,
        showOverall = function() private$..showOverall$value,
        showInteraction = function() private$..showInteraction$value,
        sortBy = function() private$..sortBy$value,
        showSampleSizes = function() private$..showSampleSizes$value,
        logScale = function() private$..logScale$value,
        nullLine = function() private$..nullLine$value),
    private = list(
        ..outcome = NA,
        ..treatment = NA,
        ..subgroups = NA,
        ..time = NA,
        ..event = NA,
        ..outcomeType = NA,
        ..effectMeasure = NA,
        ..confidenceLevel = NA,
        ..showOverall = NA,
        ..showInteraction = NA,
        ..sortBy = NA,
        ..showSampleSizes = NA,
        ..logScale = NA,
        ..nullLine = NA)
)

subgroupforestResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "subgroupforestResults",
    inherit = jmvcore::Group,
    active = list(
        todo = function() private$.items[["todo"]],
        plot = function() private$.items[["plot"]],
        summary = function() private$.items[["summary"]],
        interactions = function() private$.items[["interactions"]],
        overall = function() private$.items[["overall"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Subgroup Analysis Forest Plot",
                refs=list(
                    "survival",
                    "forestplot",
                    "meta",
                    "ClinicoPathJamoviModule"))
            self$add(jmvcore::Html$new(
                options=options,
                name="todo",
                title="Instructions",
                clearWith=list(
                    "outcome",
                    "treatment",
                    "subgroups",
                    "outcomeType")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Subgroup Forest Plot",
                width=800,
                height=500,
                requiresData=TRUE,
                renderFun=".plot",
                clearWith=list(
                    "outcome",
                    "treatment",
                    "subgroups",
                    "outcomeType",
                    "effectMeasure",
                    "confidenceLevel",
                    "sortBy",
                    "showSampleSizes",
                    "logScale",
                    "nullLine")))
            self$add(jmvcore::Table$new(
                options=options,
                name="summary",
                title="Subgroup Analysis Summary",
                columns=list(
                    list(
                        `name`="subgroup", 
                        `title`="Subgroup", 
                        `type`="text"),
                    list(
                        `name`="n", 
                        `title`="N", 
                        `type`="integer"),
                    list(
                        `name`="events", 
                        `title`="Events", 
                        `type`="integer"),
                    list(
                        `name`="estimate", 
                        `title`="Effect Estimate", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="ci_lower", 
                        `title`="95% CI Lower", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="ci_upper", 
                        `title`="95% CI Upper", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="pvalue", 
                        `title`="P-value", 
                        `type`="number", 
                        `format`="zto,pvalue")),
                clearWith=list(
                    "outcome",
                    "treatment",
                    "subgroups",
                    "outcomeType",
                    "effectMeasure",
                    "confidenceLevel")))
            self$add(jmvcore::Table$new(
                options=options,
                name="interactions",
                title="Interaction Tests",
                columns=list(
                    list(
                        `name`="variable", 
                        `title`="Variable", 
                        `type`="text"),
                    list(
                        `name`="pvalue", 
                        `title`="Interaction P-value", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="interpretation", 
                        `title`="Interpretation", 
                        `type`="text")),
                visible="(showInteraction)",
                clearWith=list(
                    "outcome",
                    "treatment",
                    "subgroups",
                    "outcomeType",
                    "showInteraction")))
            self$add(jmvcore::Table$new(
                options=options,
                name="overall",
                title="Overall Treatment Effect",
                columns=list(
                    list(
                        `name`="measure", 
                        `title`="Measure", 
                        `type`="text"),
                    list(
                        `name`="estimate", 
                        `title`="Effect", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="ci_lower", 
                        `title`="95% CI Lower", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="ci_upper", 
                        `title`="95% CI Upper", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="pvalue", 
                        `title`="P-value", 
                        `type`="number", 
                        `format`="zto,pvalue"))))}))

subgroupforestBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "subgroupforestBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "subgroupforest",
                version = c(0,1,0),
                options = options,
                results = subgroupforestResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Subgroup Analysis Forest Plot
#'
#' Creates forest plots showing treatment effects across different patient 
#' subgroups. Performs subgroup analysis for clinical trials and observational 
#' studies, calculating treatment effects within patient subgroups and testing 
#' for interactions. Supports survival (time-to-event), binary, and continuous 
#' outcomes with comprehensive statistical validation and heterogeneity 
#' testing.
#' 
#'
#' @examples
#' # Survival outcome subgroup analysis
#' subgroupforest(
#'     data = clinical_trial,
#'     outcome = "time_to_event",
#'     treatment = "treatment_arm",
#'     subgroups = c("age_group", "gender", "stage"),
#'     time = "time_to_event",
#'     event = "event_occurred",
#'     outcomeType = "survival",
#'     effectMeasure = "hr"
#' )
#'
#' # Binary outcome analysis
#' subgroupforest(
#'     data = study_data,
#'     outcome = "response",
#'     treatment = "intervention",
#'     subgroups = c("age_category", "sex"),
#'     outcomeType = "binary",
#'     effectMeasure = "or"
#' )
#'
#' @param data The data as a data frame.
#' @param outcome Primary outcome variable. For survival analysis, this should
#'   be the time variable. For binary outcomes, use 0/1 or factor with 2 levels.
#'   For continuous outcomes, use numeric variables.
#' @param treatment Treatment or exposure variable (must be binary: 0/1,
#'   control/treatment). This variable defines the two groups being compared in
#'   the analysis.
#' @param subgroups Variables defining patient subgroups for analysis
#'   (categorical variables). Each variable will be analyzed separately to
#'   identify differential treatment effects. Examples: age groups, gender,
#'   disease stage, biomarker status.
#' @param time Time variable for survival analysis (numeric, required if
#'   outcomeType = "survival"). Should contain time to event or censoring in
#'   consistent units (days, months, years).
#' @param event Event indicator for survival analysis (binary: 1=event
#'   occurred, 0=censored). Required for survival analysis to distinguish
#'   between observed events and censored observations.
#' @param outcomeType Type of outcome variable: "survival" (time-to-event),
#'   "binary" (yes/no), "continuous". Determines the statistical method used for
#'   analysis (Cox regression, logistic regression, linear regression).
#' @param effectMeasure Statistical measure for treatment effect: "hr" (hazard
#'   ratio), "or" (odds ratio),  "rr" (risk ratio), "md" (mean difference).
#'   Should match the outcome type.
#' @param confidenceLevel Confidence level for intervals.
#' @param showOverall Display overall treatment effect across all patients.
#' @param showInteraction Perform statistical tests for subgroup interactions
#'   using likelihood ratio tests. Tests whether treatment effect varies
#'   significantly across subgroups.
#' @param sortBy Method for ordering subgroups in the plot: "effect" (by
#'   effect size),  "n" (by sample size), "alpha" (alphabetical). Affects visual
#'   presentation.
#' @param showSampleSizes Display sample sizes for each subgroup.
#' @param logScale Display effects on log scale (recommended for ratios: HR,
#'   OR, RR). Makes ratio effects more interpretable and symmetric around null
#'   value.
#' @param nullLine Value for null effect reference line (1 for ratios:
#'   HR/OR/RR, 0 for differences: MD). Vertical line indicating no treatment
#'   effect.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$todo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$summary} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$interactions} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$overall} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$summary$asDF}
#'
#' \code{as.data.frame(results$summary)}
#'
#' @export
subgroupforest <- function(
    data,
    outcome,
    treatment,
    subgroups,
    time,
    event,
    outcomeType = "survival",
    effectMeasure = "hr",
    confidenceLevel = "0.95",
    showOverall = TRUE,
    showInteraction = TRUE,
    sortBy = "effect",
    showSampleSizes = TRUE,
    logScale = TRUE,
    nullLine = 1) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("subgroupforest requires jmvcore to be installed (restart may be required)")

    if ( ! missing(outcome)) outcome <- jmvcore::resolveQuo(jmvcore::enquo(outcome))
    if ( ! missing(treatment)) treatment <- jmvcore::resolveQuo(jmvcore::enquo(treatment))
    if ( ! missing(subgroups)) subgroups <- jmvcore::resolveQuo(jmvcore::enquo(subgroups))
    if ( ! missing(time)) time <- jmvcore::resolveQuo(jmvcore::enquo(time))
    if ( ! missing(event)) event <- jmvcore::resolveQuo(jmvcore::enquo(event))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(outcome), outcome, NULL),
            `if`( ! missing(treatment), treatment, NULL),
            `if`( ! missing(subgroups), subgroups, NULL),
            `if`( ! missing(time), time, NULL),
            `if`( ! missing(event), event, NULL))

    for (v in treatment) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in subgroups) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in event) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- subgroupforestOptions$new(
        outcome = outcome,
        treatment = treatment,
        subgroups = subgroups,
        time = time,
        event = event,
        outcomeType = outcomeType,
        effectMeasure = effectMeasure,
        confidenceLevel = confidenceLevel,
        showOverall = showOverall,
        showInteraction = showInteraction,
        sortBy = sortBy,
        showSampleSizes = showSampleSizes,
        logScale = logScale,
        nullLine = nullLine)

    analysis <- subgroupforestClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

