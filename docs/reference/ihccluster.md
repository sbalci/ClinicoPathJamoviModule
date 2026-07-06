# IHC Clustering Analysis

Clusters cases based on immunohistochemistry (IHC) staining patterns.

## Usage

``` r
ihccluster(
  data,
  catVars = NULL,
  contVars = NULL,
  caseId = NULL,
  spatialCompartment = NULL,
  performSpatialAnalysis = FALSE,
  spatialComparisonMode = "both",
  method = "pam",
  distanceMethod = "gower",
  linkageMethod = "ward",
  nClusters = 3,
  autoSelectK = FALSE,
  kRange = "medium",
  scaleContVars = TRUE,
  weights = "",
  handleMissing = "pairwise",
  consensusClustering = FALSE,
  nBootstrap = 100,
  seed = 42,
  showSilhouette = TRUE,
  showHeatmap = TRUE,
  heatmapScale = "row",
  showDendrogram = FALSE,
  showPCAPlot = FALSE,
  showBoxplots = FALSE,
  markerSummary = TRUE,
  clusterProfiles = TRUE,
  associationTests = FALSE,
  multipleTestingCorrection = "bonferroni",
  markerOptimization = FALSE,
  showMarkerCorrelation = FALSE,
  performMarkerClustering = FALSE,
  markerClusteringMethod = "chisquared",
  markerLinkage = "ward",
  markerSignificanceTest = FALSE,
  markerCutHeight = FALSE,
  clusterQualityMetrics = TRUE,
  iterativeRefinement = FALSE,
  refinementIterations = 3,
  reproducibilityTest = FALSE,
  nSplits = 10,
  supervisedClustering = FALSE,
  supervisedVariable = NULL,
  calculateRatios = FALSE,
  ratioNumerator = NULL,
  ratioDenominator = NULL,
  ratioName = "Marker_Ratio",
  ratioClassification = FALSE,
  ratioLowCutoff = 1,
  ratioHighCutoff = 2,
  exportClusters = FALSE,
  knownDiagnosis = NULL,
  calculateDiagnosticMetrics = FALSE,
  identifyOptimalPanel = FALSE,
  panelSize = "pairs",
  flagOutliers = TRUE,
  outlierThreshold = 0.25,
  clinicalVars = NULL,
  survivalTime = NULL,
  survivalEvent = NULL,
  colorPalette = "default",
  fontSize = "medium",
  plotContrast = FALSE,
  showInterpretation = FALSE,
  showTechnicalNotes = FALSE,
  showDiagnosticGlossary = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- catVars:

  Binary (pos/neg) or ordinal (0/1/2/3) stain results

- contVars:

  H-scores (0-300), percent positivity (0-100), or other continuous
  measures

- caseId:

  Case identifier for tracking

- spatialCompartment:

  Groups cases by spatial location (e.g., Central/Invasive,
  Preinvasive/Invasive, Primary/Metastatic)

- performSpatialAnalysis:

  Compare clustering patterns across spatial compartments

- spatialComparisonMode:

  How to analyze spatial compartments

- method:

  Clustering algorithm to use

- distanceMethod:

  Distance metric for clustering

- linkageMethod:

  Hierarchical clustering linkage method

- nClusters:

  Leave blank for automatic selection using silhouette

- autoSelectK:

  Use silhouette width to find optimal number of clusters

- kRange:

  Range to test when auto-selecting k

- scaleContVars:

  Z-score continuous markers before clustering

- weights:

  Comma-separated weights for markers (e.g., "1,1,2,1" for 4 markers)

- handleMissing:

  How to handle missing values

- consensusClustering:

  Use bootstrap resampling for stable clusters

- nBootstrap:

  Number of bootstrap samples for consensus

- seed:

  For reproducibility

- showSilhouette:

  Show silhouette width plot for cluster quality

- showHeatmap:

  Show clustered heatmap of IHC expression

- heatmapScale:

  How to scale heatmap values

- showDendrogram:

  Show dendrogram for hierarchical clustering

- showPCAPlot:

  Show dimension reduction plot with clusters

- showBoxplots:

  Show boxplots of continuous markers by cluster

- markerSummary:

  Summary statistics for each marker by cluster

- clusterProfiles:

  Characteristic features of each cluster

- associationTests:

  Test marker-cluster associations

- multipleTestingCorrection:

  Correction method for marker association tests

- markerOptimization:

  Analyze marker importance and identify optimal panel

- showMarkerCorrelation:

  Display correlation structure between markers

- performMarkerClustering:

  Cluster IHC markers to identify co-expression patterns and redundancy

- markerClusteringMethod:

  Distance metric for marker clustering

- markerLinkage:

  Linkage method for marker dendrogram

- markerSignificanceTest:

  Perform statistical tests for marker-marker associations

- markerCutHeight:

  Automatically identify statistically distinct marker groups

- clusterQualityMetrics:

  Calculate PPV, purity, and cluster quality measures

- iterativeRefinement:

  Perform iterative clustering with marker selection (advanced)

- refinementIterations:

  Number of iterative refinement cycles

- reproducibilityTest:

  Random split validation with Cohen kappa (Sterlacci 2019)

- nSplits:

  Number of random splits for reproducibility testing

- supervisedClustering:

  Cluster within each known diagnosis group separately

- supervisedVariable:

  Variable defining groups (e.g., histotype, diagnosis)

- calculateRatios:

  Calculate ratios between continuous markers (e.g., CD4/CD8 ratio)

- ratioNumerator:

  Numerator marker for ratio calculation

- ratioDenominator:

  Denominator marker for ratio calculation

- ratioName:

  Name for the computed ratio variable

- ratioClassification:

  Classify ratio as Low/Intermediate/High using cutoffs

- ratioLowCutoff:

  Values \<= this are classified as Low

- ratioHighCutoff:

  Values \>= this are classified as High

- exportClusters:

  Save cluster assignments to dataset

- knownDiagnosis:

  Known diagnoses for calculating marker performance metrics
  (sensitivity, specificity, PPV, NPV)

- calculateDiagnosticMetrics:

  Compute sensitivity, specificity, PPV, NPV for each marker when
  diagnosis is known

- identifyOptimalPanel:

  Find minimal marker combinations with maximum diagnostic
  discrimination

- panelSize:

  Size of antibody panel combinations to evaluate

- flagOutliers:

  Identify cases with ambiguous/atypical immunoprofiles based on
  silhouette scores

- outlierThreshold:

  Silhouette score threshold below which cases are flagged as outliers

- clinicalVars:

  Clinical variables to compare across clusters

- survivalTime:

  Time variable for survival analysis

- survivalEvent:

  Event variable for survival analysis

- colorPalette:

  Color palette for plots (colorblind-safe options available)

- fontSize:

  Base font size for all text elements

- plotContrast:

  Enable high contrast mode for better visibility

- showInterpretation:

  Display clinical interpretation guide

- showTechnicalNotes:

  Display technical notes about the analysis

- showDiagnosticGlossary:

  Display explanations of sensitivity, specificity, PPV, NPV

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$notices` |  |  |  |  | a preformatted |
| `results$todo` |  |  |  |  | a html |
| `results$binaryConversionNote` |  |  |  |  | a html |
| `results$summary` |  |  |  |  | a html |
| `results$clusterSizes` |  |  |  |  | a table |
| `results$silhouetteStats` |  |  |  |  | a table |
| `results$clusterProfiles` |  |  |  |  | a table |
| `results$markerSummary` |  |  |  |  | a table |
| `results$associationTests` |  |  |  |  | a table |
| `results$clinicalComparison` |  |  |  |  | a table |
| `results$consensusStats` |  |  |  |  | a table |
| `results$consensusPlot` |  |  |  |  | an image |
| `results$reproducibilityStats` |  |  |  |  | a table |
| `results$supervisedSummary` |  |  |  |  | a table |
| `results$supervisedResults` |  |  |  |  | a html |
| `results$ratioSummary` |  |  |  |  | a table |
| `results$ratioClassificationTable` |  |  |  |  | a table |
| `results$markerImportance` |  |  |  |  | a table |
| `results$clusterQuality` |  |  |  |  | a table |
| `results$refinementHistory` |  |  |  |  | a table |
| `results$markerPerformance` |  |  |  |  | Sensitivity, specificity, PPV, NPV for each marker by diagnosis |
| `results$optimalPanels` |  |  |  |  | Ranked marker combinations for differential diagnosis |
| `results$outlierCases` |  |  |  |  | Cases with ambiguous cluster assignment or low silhouette scores |
| `results$silhouettePlot` |  |  |  |  | an image |
| `results$heatmapPlot` |  |  |  |  | an image |
| `results$dendrogramPlot` |  |  |  |  | an image |
| `results$pcaContributions` |  |  |  |  | a table |
| `results$pcaVariablePlot` |  |  |  |  | an image |
| `results$pcaPlot` |  |  |  |  | an image |
| `results$boxplotPlot` |  |  |  |  | an image |
| `results$markerCorrelationPlot` |  |  |  |  | an image |
| `results$markerAssociationTable` |  |  |  |  | Statistical tests for associations between IHC markers |
| `results$markerClusteringTree` |  |  |  |  | Hierarchical merging sequence showing which markers cluster together |
| `results$markerGroups` |  |  |  |  | Statistically distinct groups of co-expressed markers |
| `results$markerDendrogram` |  |  |  |  | an image |
| `results$survivalPlot` |  |  |  |  | an image |
| `results$medoidInfo` |  |  |  |  | a table |
| `results$interpretationGuide` |  |  |  |  | a html |
| `results$technicalNotes` |  |  |  |  | a html |
| `results$diagnosticGlossary` |  |  |  |  | a html |
| `results$executiveSummary` |  |  |  |  | a preformatted |
| `results$spatialCompartmentSummary` |  |  |  |  | a table |
| `results$spatialConcordance` |  |  |  |  | a table |
| `results$spatialClusterComparison` |  |  |  |  | a table |
| `results$spatialMarkerDifferences` |  |  |  |  | a table |
| `results$spatialHeatmapPlot` |  |  |  |  | an image |
| `results$sizes` |  |  |  |  | a table |
| `results$distr` |  |  |  |  | a table |
| `results$assoc` |  |  |  |  | a table |
| `results$text` |  |  |  |  | a preformatted |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$clusterSizes$asDF`

`as.data.frame(results$clusterSizes)`

## Details

**Variable Types:**

- Categorical: pos/neg, intensity levels (0/1/2/3), ordinal scales

- Continuous: H-scores (0-300), percent positivity (0-100), expression
  levels

- Mixed: Any combination of categorical and continuous markers

Uses Gower distance to handle mixed data types appropriately.
