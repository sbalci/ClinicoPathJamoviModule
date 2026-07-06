# Comprehensive Tidy Plots

Create publication-ready plots using the tidyplots framework with
extensive customization options, statistical features, and advanced
visualization capabilities. Comprehensive implementation of tidyplots
for jamovi with advanced statistical visualization features including
multiple plot types, statistical testing, and extensive customization
options.

## Usage

``` r
tidyplots(
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
  stackType = "absolute",
  showMean = FALSE,
  meanType = "dash",
  showMedian = FALSE,
  medianType = "dash",
  showSum = FALSE,
  sumType = "dash",
  showCount = FALSE,
  countType = "dash",
  showSEM = FALSE,
  semType = "errorbar",
  showSD = FALSE,
  sdType = "errorbar",
  showCI = FALSE,
  ciType = "errorbar",
  showRange = FALSE,
  rangeType = "errorbar",
  showDistribution = FALSE,
  distributionType = "density",
  violinPoints = FALSE,
  histogramBins = 30,
  showPValue = FALSE,
  showSignificance = FALSE,
  showReferenceLines = FALSE,
  referenceX = "",
  referenceY = "",
  colorScheme = "friendly",
  alpha = 1,
  fontSize = 12,
  plotTitle = "",
  xLabel = "",
  yLabel = "",
  legendTitle = "",
  plotCaption = "",
  showHowTo = FALSE,
  showGlossary = FALSE,
  showSummary = FALSE,
  pointShape = 16,
  removeLegend = FALSE,
  removeLegendTitle = FALSE,
  removePadding = FALSE,
  removeXAxis = FALSE,
  removeXAxisLabels = FALSE,
  removeXAxisTitle = FALSE,
  removeYAxis = FALSE,
  removeYAxisLabels = FALSE,
  removeYAxisTitle = FALSE,
  sortXAxisLabels = FALSE,
  reverseXAxisLabels = FALSE,
  plotTheme = "default",
  removeXAxisLine = FALSE,
  removeYAxisLine = FALSE,
  showDataLabels = FALSE,
  dataLabelType = "direct",
  legendPosition = "default",
  flipOrientation = FALSE,
  paddingTop = 0.1,
  paddingBottom = 0.1,
  paddingLeft = 0.1,
  paddingRight = 0.1,
  colorSaturation = 1,
  dodgeWidth = 0.8,
  pointSize = 2.5,
  pointWhiteBorder = FALSE,
  lineWidth = 1,
  useRasterization = FALSE,
  rasterDPI = 300,
  setXLimits = FALSE,
  xMin = 0,
  xMax = 100,
  setYLimits = FALSE,
  yMin = 0,
  yMax = 100,
  xAxisTickCount = 5,
  yAxisTickCount = 5,
  fontFamily = "default",
  titleAlignment = "left",
  captionAlignment = "right",
  showMedianLine = FALSE,
  maxDataPoints = 10000,
  errorBarWidth = 0.1,
  violinScale = "area",
  boxplotOutliers = FALSE,
  boxplotNotch = FALSE,
  addSmoother = FALSE,
  smootherMethod = "loess",
  smootherSE = FALSE,
  plotWidth = 150,
  plotHeight = 100,
  useAutoSize = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- xvar:

  .

- yvar:

  .

- color:

  .

- group:

  .

- facet:

  .

- plotType:

  .

- pointType:

  .

- lineType:

  .

- barType:

  .

- stackType:

  .

- showMean:

  .

- meanType:

  .

- showMedian:

  .

- medianType:

  .

- showSum:

  .

- sumType:

  .

- showCount:

  .

- countType:

  .

- showSEM:

  .

- semType:

  .

- showSD:

  .

- sdType:

  .

- showCI:

  .

- ciType:

  .

- showRange:

  .

- rangeType:

  .

- showDistribution:

  .

- distributionType:

  .

- violinPoints:

  .

- histogramBins:

  .

- showPValue:

  .

- showSignificance:

  .

- showReferenceLines:

  .

- referenceX:

  .

- referenceY:

  .

- colorScheme:

  .

- alpha:

  .

- fontSize:

  .

- plotTitle:

  .

- xLabel:

  .

- yLabel:

  .

- legendTitle:

  .

- plotCaption:

  .

- showHowTo:

  .

- showGlossary:

  .

- showSummary:

  .

- pointShape:

  .

- removeLegend:

  .

- removeLegendTitle:

  .

- removePadding:

  .

- removeXAxis:

  .

- removeXAxisLabels:

  .

- removeXAxisTitle:

  .

- removeYAxis:

  .

- removeYAxisLabels:

  .

- removeYAxisTitle:

  .

- sortXAxisLabels:

  .

- reverseXAxisLabels:

  .

- plotTheme:

  .

- removeXAxisLine:

  .

- removeYAxisLine:

  .

- showDataLabels:

  .

- dataLabelType:

  .

- legendPosition:

  .

- flipOrientation:

  .

- paddingTop:

  .

- paddingBottom:

  .

- paddingLeft:

  .

- paddingRight:

  .

- colorSaturation:

  .

- dodgeWidth:

  .

- pointSize:

  .

- pointWhiteBorder:

  .

- lineWidth:

  .

- useRasterization:

  .

- rasterDPI:

  .

- setXLimits:

  .

- xMin:

  .

- xMax:

  .

- setYLimits:

  .

- yMin:

  .

- yMax:

  .

- xAxisTickCount:

  .

- yAxisTickCount:

  .

- fontFamily:

  .

- titleAlignment:

  .

- captionAlignment:

  .

- showMedianLine:

  .

- maxDataPoints:

  .

- errorBarWidth:

  .

- violinScale:

  .

- boxplotOutliers:

  .

- boxplotNotch:

  .

- addSmoother:

  .

- smootherMethod:

  .

- smootherSE:

  .

- plotWidth:

  .

- plotHeight:

  .

- useAutoSize:

  .

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$instructions` |     |     |     |     | a html   |
| `results$howto`        |     |     |     |     | a html   |
| `results$glossary`     |     |     |     |     | a html   |
| `results$summary`      |     |     |     |     | a html   |
| `results$plot`         |     |     |     |     | an image |
