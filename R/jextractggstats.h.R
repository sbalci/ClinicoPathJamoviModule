
# This file is automatically generated, you probably don't want to edit this

jextractggstatsOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jextractggstatsOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep_var = NULL,
            group_var = NULL,
            test_value = 0,
            analysis_type = "between_stats",
            extract_components = "all",
            statistical_test = "parametric",
            effect_size_type = "eta",
            pairwise_comparisons = FALSE,
            pairwise_correction = "holm",
            conf_level = 0.95,
            bf_prior = 0.707,
            centrality_plotting = TRUE,
            outlier_tagging = FALSE,
            output_format = "table",
            include_plot_data = TRUE,
            include_model_data = TRUE,
            detailed_results = TRUE,
            show_interpretation = TRUE, ...) {

            super$initialize(
                package="ClinicoPath",
                name="jextractggstats",
                requiresData=TRUE,
                ...)

            private$..dep_var <- jmvcore::OptionVariable$new(
                "dep_var",
                dep_var,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..group_var <- jmvcore::OptionVariable$new(
                "group_var",
                group_var,
                suggested=list(
                    "ordinal",
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..test_value <- jmvcore::OptionNumber$new(
                "test_value",
                test_value,
                default=0)
            private$..analysis_type <- jmvcore::OptionList$new(
                "analysis_type",
                analysis_type,
                options=list(
                    "between_stats",
                    "within_stats",
                    "correlation",
                    "histogram",
                    "scatterplot",
                    "bar_chart",
                    "contingency_stats",
                    "one_sample_stats"),
                default="between_stats")
            private$..extract_components <- jmvcore::OptionList$new(
                "extract_components",
                extract_components,
                options=list(
                    "all",
                    "subtitle_data",
                    "caption_data",
                    "pairwise_data",
                    "descriptive_data"),
                default="all")
            private$..statistical_test <- jmvcore::OptionList$new(
                "statistical_test",
                statistical_test,
                options=list(
                    "parametric",
                    "nonparametric",
                    "robust",
                    "bayes"),
                default="parametric")
            private$..effect_size_type <- jmvcore::OptionList$new(
                "effect_size_type",
                effect_size_type,
                options=list(
                    "eta",
                    "omega",
                    "cohens_d",
                    "hedges_g",
                    "cramers_v",
                    "phi"),
                default="eta")
            private$..pairwise_comparisons <- jmvcore::OptionBool$new(
                "pairwise_comparisons",
                pairwise_comparisons,
                default=FALSE)
            private$..pairwise_correction <- jmvcore::OptionList$new(
                "pairwise_correction",
                pairwise_correction,
                options=list(
                    "holm",
                    "bonferroni",
                    "fdr",
                    "none"),
                default="holm")
            private$..conf_level <- jmvcore::OptionNumber$new(
                "conf_level",
                conf_level,
                default=0.95,
                min=0.5,
                max=0.99)
            private$..bf_prior <- jmvcore::OptionNumber$new(
                "bf_prior",
                bf_prior,
                default=0.707,
                min=0.1,
                max=2)
            private$..centrality_plotting <- jmvcore::OptionBool$new(
                "centrality_plotting",
                centrality_plotting,
                default=TRUE)
            private$..outlier_tagging <- jmvcore::OptionBool$new(
                "outlier_tagging",
                outlier_tagging,
                default=FALSE)
            private$..output_format <- jmvcore::OptionList$new(
                "output_format",
                output_format,
                options=list(
                    "table",
                    "csv",
                    "dataframe",
                    "json"),
                default="table")
            private$..include_plot_data <- jmvcore::OptionBool$new(
                "include_plot_data",
                include_plot_data,
                default=TRUE)
            private$..include_model_data <- jmvcore::OptionBool$new(
                "include_model_data",
                include_model_data,
                default=TRUE)
            private$..detailed_results <- jmvcore::OptionBool$new(
                "detailed_results",
                detailed_results,
                default=TRUE)
            private$..show_interpretation <- jmvcore::OptionBool$new(
                "show_interpretation",
                show_interpretation,
                default=TRUE)

            self$.addOption(private$..dep_var)
            self$.addOption(private$..group_var)
            self$.addOption(private$..test_value)
            self$.addOption(private$..analysis_type)
            self$.addOption(private$..extract_components)
            self$.addOption(private$..statistical_test)
            self$.addOption(private$..effect_size_type)
            self$.addOption(private$..pairwise_comparisons)
            self$.addOption(private$..pairwise_correction)
            self$.addOption(private$..conf_level)
            self$.addOption(private$..bf_prior)
            self$.addOption(private$..centrality_plotting)
            self$.addOption(private$..outlier_tagging)
            self$.addOption(private$..output_format)
            self$.addOption(private$..include_plot_data)
            self$.addOption(private$..include_model_data)
            self$.addOption(private$..detailed_results)
            self$.addOption(private$..show_interpretation)
        }),
    active = list(
        dep_var = function() private$..dep_var$value,
        group_var = function() private$..group_var$value,
        test_value = function() private$..test_value$value,
        analysis_type = function() private$..analysis_type$value,
        extract_components = function() private$..extract_components$value,
        statistical_test = function() private$..statistical_test$value,
        effect_size_type = function() private$..effect_size_type$value,
        pairwise_comparisons = function() private$..pairwise_comparisons$value,
        pairwise_correction = function() private$..pairwise_correction$value,
        conf_level = function() private$..conf_level$value,
        bf_prior = function() private$..bf_prior$value,
        centrality_plotting = function() private$..centrality_plotting$value,
        outlier_tagging = function() private$..outlier_tagging$value,
        output_format = function() private$..output_format$value,
        include_plot_data = function() private$..include_plot_data$value,
        include_model_data = function() private$..include_model_data$value,
        detailed_results = function() private$..detailed_results$value,
        show_interpretation = function() private$..show_interpretation$value),
    private = list(
        ..dep_var = NA,
        ..group_var = NA,
        ..test_value = NA,
        ..analysis_type = NA,
        ..extract_components = NA,
        ..statistical_test = NA,
        ..effect_size_type = NA,
        ..pairwise_comparisons = NA,
        ..pairwise_correction = NA,
        ..conf_level = NA,
        ..bf_prior = NA,
        ..centrality_plotting = NA,
        ..outlier_tagging = NA,
        ..output_format = NA,
        ..include_plot_data = NA,
        ..include_model_data = NA,
        ..detailed_results = NA,
        ..show_interpretation = NA)
)

jextractggstatsResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jextractggstatsResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        extracted_data = function() private$.items[["extracted_data"]],
        statistical_summary = function() private$.items[["statistical_summary"]],
        interpretation = function() private$.items[["interpretation"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Statistical Data Extraction Results")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE,
                refs=list()))
            self$add(jmvcore::Html$new(
                options=options,
                name="extracted_data",
                title="Extracted Statistical Data",
                visible=TRUE,
                refs=list()))
            self$add(jmvcore::Html$new(
                options=options,
                name="statistical_summary",
                title="Statistical Summary",
                visible="(detailed_results)",
                refs=list()))
            self$add(jmvcore::Html$new(
                options=options,
                name="interpretation",
                title="Interpretation",
                visible="(show_interpretation)",
                refs=list()))}))

jextractggstatsBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jextractggstatsBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "jextractggstats",
                version = c(1,0,0),
                options = options,
                results = jextractggstatsResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Statistical Data Extraction from ggstatsplot
#'
#' 
#' @param data R object to use
#' @param dep_var Dependent variable for analysis
#' @param group_var Grouping variable for comparisons
#' @param test_value Reference value for one-sample test
#' @param analysis_type Type of ggstatsplot analysis to perform
#' @param extract_components Which statistical components to extract
#' @param statistical_test Type of statistical test to use
#' @param effect_size_type Type of effect size to calculate
#' @param pairwise_comparisons Perform pairwise comparisons for multiple
#'   groups
#' @param pairwise_correction Multiple comparison correction method
#' @param conf_level Confidence level for intervals
#' @param bf_prior Prior width for Bayesian analysis
#' @param centrality_plotting Display measures of central tendency
#' @param outlier_tagging Identify and tag outliers
#' @param output_format Format for extracted data
#' @param include_plot_data Include underlying plot data
#' @param include_model_data Include statistical model information
#' @param detailed_results Include detailed statistical output
#' @param show_interpretation Display result interpretation
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$extracted_data} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$statistical_summary} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$interpretation} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' @export
jextractggstats <- function(
    data,
    dep_var,
    group_var,
    test_value = 0,
    analysis_type = "between_stats",
    extract_components = "all",
    statistical_test = "parametric",
    effect_size_type = "eta",
    pairwise_comparisons = FALSE,
    pairwise_correction = "holm",
    conf_level = 0.95,
    bf_prior = 0.707,
    centrality_plotting = TRUE,
    outlier_tagging = FALSE,
    output_format = "table",
    include_plot_data = TRUE,
    include_model_data = TRUE,
    detailed_results = TRUE,
    show_interpretation = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jextractggstats requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep_var)) dep_var <- jmvcore::resolveQuo(jmvcore::enquo(dep_var))
    if ( ! missing(group_var)) group_var <- jmvcore::resolveQuo(jmvcore::enquo(group_var))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep_var), dep_var, NULL),
            `if`( ! missing(group_var), group_var, NULL))

    for (v in group_var) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- jextractggstatsOptions$new(
        dep_var = dep_var,
        group_var = group_var,
        test_value = test_value,
        analysis_type = analysis_type,
        extract_components = extract_components,
        statistical_test = statistical_test,
        effect_size_type = effect_size_type,
        pairwise_comparisons = pairwise_comparisons,
        pairwise_correction = pairwise_correction,
        conf_level = conf_level,
        bf_prior = bf_prior,
        centrality_plotting = centrality_plotting,
        outlier_tagging = outlier_tagging,
        output_format = output_format,
        include_plot_data = include_plot_data,
        include_model_data = include_model_data,
        detailed_results = detailed_results,
        show_interpretation = show_interpretation)

    analysis <- jextractggstatsClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

