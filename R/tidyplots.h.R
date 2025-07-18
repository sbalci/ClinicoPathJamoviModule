
# This file is automatically generated, you probably don't want to edit this

tidyplotsOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "tidyplotsOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            xvar = NULL,
            yvar = NULL,
            color = NULL,
            group = NULL,
            facet = NULL,
            plotType = "points",
            pointType = "basic",
            lineType = "direct",
            barType = "mean",
            showMean = FALSE,
            meanType = "dash",
            showMedian = FALSE,
            medianType = "dash",
            showSEM = FALSE,
            showSD = FALSE,
            showCI = FALSE,
            ciType = "errorbar",
            showRange = FALSE,
            showDistribution = FALSE,
            distributionType = "density",
            showOutliers = TRUE,
            violinPoints = FALSE,
            histogramBins = 30,
            areaType = "absolute",
            showPValue = FALSE,
            showSignificance = FALSE,
            colorScheme = "friendly",
            alpha = 1,
            fontSize = 12,
            plotTitle = "",
            xLabel = "",
            yLabel = "",
            legendTitle = "",
            removeLegend = FALSE,
            removePadding = FALSE,
            removeXAxis = FALSE,
            removeXAxisLabels = FALSE,
            removeXAxisTitle = FALSE,
            removeYAxis = FALSE,
            removeYAxisLabels = FALSE,
            removeYAxisTitle = FALSE, ...) {

            super$initialize(
                package="ClinicoPath",
                name="tidyplots",
                requiresData=TRUE,
                ...)

            private$..xvar <- jmvcore::OptionVariable$new(
                "xvar",
                xvar,
                suggested=list(
                    "continuous",
                    "ordinal",
                    "nominal"),
                permitted=list(
                    "numeric",
                    "factor"))
            private$..yvar <- jmvcore::OptionVariable$new(
                "yvar",
                yvar,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..color <- jmvcore::OptionVariable$new(
                "color",
                color,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..facet <- jmvcore::OptionVariable$new(
                "facet",
                facet,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..plotType <- jmvcore::OptionList$new(
                "plotType",
                plotType,
                options=list(
                    "points",
                    "line",
                    "bar",
                    "boxplot",
                    "violin",
                    "histogram",
                    "area",
                    "density"),
                default="points")
            private$..pointType <- jmvcore::OptionList$new(
                "pointType",
                pointType,
                options=list(
                    "basic",
                    "beeswarm",
                    "jitter"),
                default="basic")
            private$..lineType <- jmvcore::OptionList$new(
                "lineType",
                lineType,
                options=list(
                    "direct",
                    "mean",
                    "median",
                    "curve"),
                default="direct")
            private$..barType <- jmvcore::OptionList$new(
                "barType",
                barType,
                options=list(
                    "mean",
                    "median",
                    "count"),
                default="mean")
            private$..showMean <- jmvcore::OptionBool$new(
                "showMean",
                showMean,
                default=FALSE)
            private$..meanType <- jmvcore::OptionList$new(
                "meanType",
                meanType,
                options=list(
                    "dash",
                    "dot",
                    "value"),
                default="dash")
            private$..showMedian <- jmvcore::OptionBool$new(
                "showMedian",
                showMedian,
                default=FALSE)
            private$..medianType <- jmvcore::OptionList$new(
                "medianType",
                medianType,
                options=list(
                    "dash",
                    "dot",
                    "value"),
                default="dash")
            private$..showSEM <- jmvcore::OptionBool$new(
                "showSEM",
                showSEM,
                default=FALSE)
            private$..showSD <- jmvcore::OptionBool$new(
                "showSD",
                showSD,
                default=FALSE)
            private$..showCI <- jmvcore::OptionBool$new(
                "showCI",
                showCI,
                default=FALSE)
            private$..ciType <- jmvcore::OptionList$new(
                "ciType",
                ciType,
                options=list(
                    "errorbar",
                    "ribbon"),
                default="errorbar")
            private$..showRange <- jmvcore::OptionBool$new(
                "showRange",
                showRange,
                default=FALSE)
            private$..showDistribution <- jmvcore::OptionBool$new(
                "showDistribution",
                showDistribution,
                default=FALSE)
            private$..distributionType <- jmvcore::OptionList$new(
                "distributionType",
                distributionType,
                options=list(
                    "density",
                    "rug"),
                default="density")
            private$..showOutliers <- jmvcore::OptionBool$new(
                "showOutliers",
                showOutliers,
                default=TRUE)
            private$..violinPoints <- jmvcore::OptionBool$new(
                "violinPoints",
                violinPoints,
                default=FALSE)
            private$..histogramBins <- jmvcore::OptionInteger$new(
                "histogramBins",
                histogramBins,
                default=30,
                min=5,
                max=100)
            private$..areaType <- jmvcore::OptionList$new(
                "areaType",
                areaType,
                options=list(
                    "absolute",
                    "relative"),
                default="absolute")
            private$..showPValue <- jmvcore::OptionBool$new(
                "showPValue",
                showPValue,
                default=FALSE)
            private$..showSignificance <- jmvcore::OptionBool$new(
                "showSignificance",
                showSignificance,
                default=FALSE)
            private$..colorScheme <- jmvcore::OptionList$new(
                "colorScheme",
                colorScheme,
                options=list(
                    "friendly",
                    "seaside",
                    "apple",
                    "rainbow",
                    "viridis",
                    "inferno",
                    "magma",
                    "turbo",
                    "blue2red",
                    "blue2brown"),
                default="friendly")
            private$..alpha <- jmvcore::OptionNumber$new(
                "alpha",
                alpha,
                default=1,
                min=0.1,
                max=1)
            private$..fontSize <- jmvcore::OptionInteger$new(
                "fontSize",
                fontSize,
                default=12,
                min=8,
                max=24)
            private$..plotTitle <- jmvcore::OptionString$new(
                "plotTitle",
                plotTitle,
                default="")
            private$..xLabel <- jmvcore::OptionString$new(
                "xLabel",
                xLabel,
                default="")
            private$..yLabel <- jmvcore::OptionString$new(
                "yLabel",
                yLabel,
                default="")
            private$..legendTitle <- jmvcore::OptionString$new(
                "legendTitle",
                legendTitle,
                default="")
            private$..removeLegend <- jmvcore::OptionBool$new(
                "removeLegend",
                removeLegend,
                default=FALSE)
            private$..removePadding <- jmvcore::OptionBool$new(
                "removePadding",
                removePadding,
                default=FALSE)
            private$..removeXAxis <- jmvcore::OptionBool$new(
                "removeXAxis",
                removeXAxis,
                default=FALSE)
            private$..removeXAxisLabels <- jmvcore::OptionBool$new(
                "removeXAxisLabels",
                removeXAxisLabels,
                default=FALSE)
            private$..removeXAxisTitle <- jmvcore::OptionBool$new(
                "removeXAxisTitle",
                removeXAxisTitle,
                default=FALSE)
            private$..removeYAxis <- jmvcore::OptionBool$new(
                "removeYAxis",
                removeYAxis,
                default=FALSE)
            private$..removeYAxisLabels <- jmvcore::OptionBool$new(
                "removeYAxisLabels",
                removeYAxisLabels,
                default=FALSE)
            private$..removeYAxisTitle <- jmvcore::OptionBool$new(
                "removeYAxisTitle",
                removeYAxisTitle,
                default=FALSE)

            self$.addOption(private$..xvar)
            self$.addOption(private$..yvar)
            self$.addOption(private$..color)
            self$.addOption(private$..group)
            self$.addOption(private$..facet)
            self$.addOption(private$..plotType)
            self$.addOption(private$..pointType)
            self$.addOption(private$..lineType)
            self$.addOption(private$..barType)
            self$.addOption(private$..showMean)
            self$.addOption(private$..meanType)
            self$.addOption(private$..showMedian)
            self$.addOption(private$..medianType)
            self$.addOption(private$..showSEM)
            self$.addOption(private$..showSD)
            self$.addOption(private$..showCI)
            self$.addOption(private$..ciType)
            self$.addOption(private$..showRange)
            self$.addOption(private$..showDistribution)
            self$.addOption(private$..distributionType)
            self$.addOption(private$..showOutliers)
            self$.addOption(private$..violinPoints)
            self$.addOption(private$..histogramBins)
            self$.addOption(private$..areaType)
            self$.addOption(private$..showPValue)
            self$.addOption(private$..showSignificance)
            self$.addOption(private$..colorScheme)
            self$.addOption(private$..alpha)
            self$.addOption(private$..fontSize)
            self$.addOption(private$..plotTitle)
            self$.addOption(private$..xLabel)
            self$.addOption(private$..yLabel)
            self$.addOption(private$..legendTitle)
            self$.addOption(private$..removeLegend)
            self$.addOption(private$..removePadding)
            self$.addOption(private$..removeXAxis)
            self$.addOption(private$..removeXAxisLabels)
            self$.addOption(private$..removeXAxisTitle)
            self$.addOption(private$..removeYAxis)
            self$.addOption(private$..removeYAxisLabels)
            self$.addOption(private$..removeYAxisTitle)
        }),
    active = list(
        xvar = function() private$..xvar$value,
        yvar = function() private$..yvar$value,
        color = function() private$..color$value,
        group = function() private$..group$value,
        facet = function() private$..facet$value,
        plotType = function() private$..plotType$value,
        pointType = function() private$..pointType$value,
        lineType = function() private$..lineType$value,
        barType = function() private$..barType$value,
        showMean = function() private$..showMean$value,
        meanType = function() private$..meanType$value,
        showMedian = function() private$..showMedian$value,
        medianType = function() private$..medianType$value,
        showSEM = function() private$..showSEM$value,
        showSD = function() private$..showSD$value,
        showCI = function() private$..showCI$value,
        ciType = function() private$..ciType$value,
        showRange = function() private$..showRange$value,
        showDistribution = function() private$..showDistribution$value,
        distributionType = function() private$..distributionType$value,
        showOutliers = function() private$..showOutliers$value,
        violinPoints = function() private$..violinPoints$value,
        histogramBins = function() private$..histogramBins$value,
        areaType = function() private$..areaType$value,
        showPValue = function() private$..showPValue$value,
        showSignificance = function() private$..showSignificance$value,
        colorScheme = function() private$..colorScheme$value,
        alpha = function() private$..alpha$value,
        fontSize = function() private$..fontSize$value,
        plotTitle = function() private$..plotTitle$value,
        xLabel = function() private$..xLabel$value,
        yLabel = function() private$..yLabel$value,
        legendTitle = function() private$..legendTitle$value,
        removeLegend = function() private$..removeLegend$value,
        removePadding = function() private$..removePadding$value,
        removeXAxis = function() private$..removeXAxis$value,
        removeXAxisLabels = function() private$..removeXAxisLabels$value,
        removeXAxisTitle = function() private$..removeXAxisTitle$value,
        removeYAxis = function() private$..removeYAxis$value,
        removeYAxisLabels = function() private$..removeYAxisLabels$value,
        removeYAxisTitle = function() private$..removeYAxisTitle$value),
    private = list(
        ..xvar = NA,
        ..yvar = NA,
        ..color = NA,
        ..group = NA,
        ..facet = NA,
        ..plotType = NA,
        ..pointType = NA,
        ..lineType = NA,
        ..barType = NA,
        ..showMean = NA,
        ..meanType = NA,
        ..showMedian = NA,
        ..medianType = NA,
        ..showSEM = NA,
        ..showSD = NA,
        ..showCI = NA,
        ..ciType = NA,
        ..showRange = NA,
        ..showDistribution = NA,
        ..distributionType = NA,
        ..showOutliers = NA,
        ..violinPoints = NA,
        ..histogramBins = NA,
        ..areaType = NA,
        ..showPValue = NA,
        ..showSignificance = NA,
        ..colorScheme = NA,
        ..alpha = NA,
        ..fontSize = NA,
        ..plotTitle = NA,
        ..xLabel = NA,
        ..yLabel = NA,
        ..legendTitle = NA,
        ..removeLegend = NA,
        ..removePadding = NA,
        ..removeXAxis = NA,
        ..removeXAxisLabels = NA,
        ..removeXAxisTitle = NA,
        ..removeYAxis = NA,
        ..removeYAxisLabels = NA,
        ..removeYAxisTitle = NA)
)

tidyplotsResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "tidyplotsResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Comprehensive Tidy Plots",
                refs=list(
                    "tidyplots",
                    "ggplot2",
                    "dplyr",
                    "rlang",
                    "jmvcore",
                    "R6"))
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible="(xvar === null || yvar === null)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Plot",
                renderFun=".plot",
                requiresData=TRUE,
                width=800,
                height=600,
                clearWith=list(
                    "xvar",
                    "yvar",
                    "color",
                    "group",
                    "facet",
                    "plotType",
                    "pointType",
                    "lineType",
                    "barType",
                    "areaType",
                    "showMean",
                    "meanType",
                    "showMedian",
                    "medianType",
                    "showSEM",
                    "showSD",
                    "showCI",
                    "ciType",
                    "showRange",
                    "showDistribution",
                    "distributionType",
                    "showOutliers",
                    "violinPoints",
                    "histogramBins",
                    "showPValue",
                    "showSignificance",
                    "colorScheme",
                    "alpha",
                    "fontSize",
                    "plotTitle",
                    "xLabel",
                    "yLabel",
                    "legendTitle",
                    "removeLegend",
                    "removePadding",
                    "removeXAxis",
                    "removeXAxisLabels",
                    "removeXAxisTitle",
                    "removeYAxis",
                    "removeYAxisLabels",
                    "removeYAxisTitle")))}))

tidyplotsBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "tidyplotsBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "ClinicoPath",
                name = "tidyplots",
                version = c(1,0,0),
                options = options,
                results = tidyplotsResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Comprehensive Tidy Plots
#'
#' Create publication-ready plots using the tidyplots framework with extensive 
#' customization options, statistical features, and advanced visualization 
#' capabilities. Comprehensive implementation of tidyplots for jamovi with 
#' advanced statistical visualization features including multiple plot types, 
#' statistical testing, and extensive customization options.
#' 
#' @param data The data as a data frame.
#' @param xvar .
#' @param yvar .
#' @param color .
#' @param group .
#' @param facet .
#' @param plotType .
#' @param pointType .
#' @param lineType .
#' @param barType .
#' @param showMean .
#' @param meanType .
#' @param showMedian .
#' @param medianType .
#' @param showSEM .
#' @param showSD .
#' @param showCI .
#' @param ciType .
#' @param showRange .
#' @param showDistribution .
#' @param distributionType .
#' @param showOutliers .
#' @param violinPoints .
#' @param histogramBins .
#' @param areaType .
#' @param showPValue .
#' @param showSignificance .
#' @param colorScheme .
#' @param alpha .
#' @param fontSize .
#' @param plotTitle .
#' @param xLabel .
#' @param yLabel .
#' @param legendTitle .
#' @param removeLegend .
#' @param removePadding .
#' @param removeXAxis .
#' @param removeXAxisLabels .
#' @param removeXAxisTitle .
#' @param removeYAxis .
#' @param removeYAxisLabels .
#' @param removeYAxisTitle .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' @export
tidyplots <- function(
    data,
    xvar,
    yvar,
    color,
    group,
    facet,
    plotType = "points",
    pointType = "basic",
    lineType = "direct",
    barType = "mean",
    showMean = FALSE,
    meanType = "dash",
    showMedian = FALSE,
    medianType = "dash",
    showSEM = FALSE,
    showSD = FALSE,
    showCI = FALSE,
    ciType = "errorbar",
    showRange = FALSE,
    showDistribution = FALSE,
    distributionType = "density",
    showOutliers = TRUE,
    violinPoints = FALSE,
    histogramBins = 30,
    areaType = "absolute",
    showPValue = FALSE,
    showSignificance = FALSE,
    colorScheme = "friendly",
    alpha = 1,
    fontSize = 12,
    plotTitle = "",
    xLabel = "",
    yLabel = "",
    legendTitle = "",
    removeLegend = FALSE,
    removePadding = FALSE,
    removeXAxis = FALSE,
    removeXAxisLabels = FALSE,
    removeXAxisTitle = FALSE,
    removeYAxis = FALSE,
    removeYAxisLabels = FALSE,
    removeYAxisTitle = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("tidyplots requires jmvcore to be installed (restart may be required)")

    if ( ! missing(xvar)) xvar <- jmvcore::resolveQuo(jmvcore::enquo(xvar))
    if ( ! missing(yvar)) yvar <- jmvcore::resolveQuo(jmvcore::enquo(yvar))
    if ( ! missing(color)) color <- jmvcore::resolveQuo(jmvcore::enquo(color))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if ( ! missing(facet)) facet <- jmvcore::resolveQuo(jmvcore::enquo(facet))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(xvar), xvar, NULL),
            `if`( ! missing(yvar), yvar, NULL),
            `if`( ! missing(color), color, NULL),
            `if`( ! missing(group), group, NULL),
            `if`( ! missing(facet), facet, NULL))


    options <- tidyplotsOptions$new(
        xvar = xvar,
        yvar = yvar,
        color = color,
        group = group,
        facet = facet,
        plotType = plotType,
        pointType = pointType,
        lineType = lineType,
        barType = barType,
        showMean = showMean,
        meanType = meanType,
        showMedian = showMedian,
        medianType = medianType,
        showSEM = showSEM,
        showSD = showSD,
        showCI = showCI,
        ciType = ciType,
        showRange = showRange,
        showDistribution = showDistribution,
        distributionType = distributionType,
        showOutliers = showOutliers,
        violinPoints = violinPoints,
        histogramBins = histogramBins,
        areaType = areaType,
        showPValue = showPValue,
        showSignificance = showSignificance,
        colorScheme = colorScheme,
        alpha = alpha,
        fontSize = fontSize,
        plotTitle = plotTitle,
        xLabel = xLabel,
        yLabel = yLabel,
        legendTitle = legendTitle,
        removeLegend = removeLegend,
        removePadding = removePadding,
        removeXAxis = removeXAxis,
        removeXAxisLabels = removeXAxisLabels,
        removeXAxisTitle = removeXAxisTitle,
        removeYAxis = removeYAxis,
        removeYAxisLabels = removeYAxisLabels,
        removeYAxisTitle = removeYAxisTitle)

    analysis <- tidyplotsClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

