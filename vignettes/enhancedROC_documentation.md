# Clinical ROC Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `enhancedROC`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `positiveClass` | UI Control `positiveClass` | `self$options$positiveClass` | Output item / Table |
| `predictors` | UI Control `predictors` | `self$options$predictors` | Output item / Table |
| `analysisType` | UI Control `analysisType` | `self$options$analysisType` | Output item / Table |
| `direction` | UI Control `direction` | `self$options$direction` | Output item / Table |
| `youdenOptimization` | UI Control `youdenOptimization` | `self$options$youdenOptimization` | Output item / Table |
| `customCutoffs` | UI Control `customCutoffs` | `self$options$customCutoffs` | Output item / Table |
| `sensitivityThreshold` | UI Control `sensitivityThreshold` | `self$options$sensitivityThreshold` | Output item / Table |
| `specificityThreshold` | UI Control `specificityThreshold` | `self$options$specificityThreshold` | Output item / Table |
| `confidenceLevel` | UI Control `confidenceLevel` | `self$options$confidenceLevel` | Output item / Table |
| `bootstrapSamples` | UI Control `bootstrapSamples` | `self$options$bootstrapSamples` | Output item / Table |
| `useBootstrap` | UI Control `useBootstrap` | `self$options$useBootstrap` | Output item / Table |
| `bootstrapMethod` | UI Control `bootstrapMethod` | `self$options$bootstrapMethod` | Output item / Table |
| `bootstrapCutoffCI` | UI Control `bootstrapCutoffCI` | `self$options$bootstrapCutoffCI` | Output item / Table |
| `bootstrapPartialAUC` | UI Control `bootstrapPartialAUC` | `self$options$bootstrapPartialAUC` | Output item / Table |
| `stratifiedBootstrap` | UI Control `stratifiedBootstrap` | `self$options$stratifiedBootstrap` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |
| `pairwiseComparisons` | UI Control `pairwiseComparisons` | `self$options$pairwiseComparisons` | Output item / Table |
| `comparisonMethod` | UI Control `comparisonMethod` | `self$options$comparisonMethod` | Output item / Table |
| `rocCurve` | UI Control `rocCurve` | `self$options$rocCurve` | Output item / Table |
| `aucTable` | UI Control `aucTable` | `self$options$aucTable` | Output item / Table |
| `cutoffTable` | UI Control `cutoffTable` | `self$options$cutoffTable` | Output item / Table |
| `optimalCutoffs` | UI Control `optimalCutoffs` | `self$options$optimalCutoffs` | Output item / Table |
| `diagnosticMetrics` | UI Control `diagnosticMetrics` | `self$options$diagnosticMetrics` | Output item / Table |
| `clinicalMetrics` | UI Control `clinicalMetrics` | `self$options$clinicalMetrics` | Output item / Table |
| `smoothMethod` | UI Control `smoothMethod` | `self$options$smoothMethod` | Output item / Table |
| `partialAuc` | UI Control `partialAuc` | `self$options$partialAuc` | Output item / Table |
| `partialAucType` | UI Control `partialAucType` | `self$options$partialAucType` | Output item / Table |
| `partialRange` | UI Control `partialRange` | `self$options$partialRange` | Output item / Table |
| `crocAnalysis` | UI Control `crocAnalysis` | `self$options$crocAnalysis` | Output item / Table |
| `crocAlpha` | UI Control `crocAlpha` | `self$options$crocAlpha` | Output item / Table |
| `convexHull` | UI Control `convexHull` | `self$options$convexHull` | Output item / Table |
| `tiedScoreHandling` | UI Control `tiedScoreHandling` | `self$options$tiedScoreHandling` | Output item / Table |
| `detectImbalance` | UI Control `detectImbalance` | `self$options$detectImbalance` | Output item / Table |
| `imbalanceThreshold` | UI Control `imbalanceThreshold` | `self$options$imbalanceThreshold` | Output item / Table |
| `showImbalanceWarning` | UI Control `showImbalanceWarning` | `self$options$showImbalanceWarning` | Output item / Table |
| `recommendPRC` | UI Control `recommendPRC` | `self$options$recommendPRC` | Output item / Table |
| `prevalence` | UI Control `prevalence` | `self$options$prevalence` | Output item / Table |
| `useObservedPrevalence` | UI Control `useObservedPrevalence` | `self$options$useObservedPrevalence` | Output item / Table |
| `clinicalContext` | UI Control `clinicalContext` | `self$options$clinicalContext` | Output item / Table |
| `clinicalPresets` | UI Control `clinicalPresets` | `self$options$clinicalPresets` | Output item / Table |
| `comprehensive_output` | UI Control `comprehensive_output` | `self$options$comprehensive_output` | Output item / Table |
| `clinical_interpretation` | UI Control `clinical_interpretation` | `self$options$clinical_interpretation` | Output item / Table |
| `plotTheme` | UI Control `plotTheme` | `self$options$plotTheme` | Output item / Table |
| `plotWidth` | UI Control `plotWidth` | `self$options$plotWidth` | Output item / Table |
| `plotHeight` | UI Control `plotHeight` | `self$options$plotHeight` | Output item / Table |
| `showCutoffPoints` | UI Control `showCutoffPoints` | `self$options$showCutoffPoints` | Output item / Table |
| `showConfidenceBands` | UI Control `showConfidenceBands` | `self$options$showConfidenceBands` | Output item / Table |
| `showMetricsDiff` | UI Control `showMetricsDiff` | `self$options$showMetricsDiff` | Output item / Table |
| `statisticalComparison` | UI Control `statisticalComparison` | `self$options$statisticalComparison` | Output item / Table |
| `calibrationAnalysis` | UI Control `calibrationAnalysis` | `self$options$calibrationAnalysis` | Output item / Table |
| `calibrationPlot` | UI Control `calibrationPlot` | `self$options$calibrationPlot` | Output item / Table |
| `hosmerLemeshow` | UI Control `hosmerLemeshow` | `self$options$hosmerLemeshow` | Output item / Table |
| `hlGroups` | UI Control `hlGroups` | `self$options$hlGroups` | Output item / Table |
| `brierScore` | UI Control `brierScore` | `self$options$brierScore` | Output item / Table |
| `calibrationMetrics` | UI Control `calibrationMetrics` | `self$options$calibrationMetrics` | Output item / Table |
| `splineCalibration` | UI Control `splineCalibration` | `self$options$splineCalibration` | Output item / Table |
| `splineKnots` | UI Control `splineKnots` | `self$options$splineKnots` | Output item / Table |
| `eoRatio` | UI Control `eoRatio` | `self$options$eoRatio` | Output item / Table |
| `namDagostino` | UI Control `namDagostino` | `self$options$namDagostino` | Output item / Table |
| `greenwoodNam` | UI Control `greenwoodNam` | `self$options$greenwoodNam` | Output item / Table |
| `calibrationBelt` | UI Control `calibrationBelt` | `self$options$calibrationBelt` | Output item / Table |
| `calibrationDensity` | UI Control `calibrationDensity` | `self$options$calibrationDensity` | Output item / Table |
| `multiClassROC` | UI Control `multiClassROC` | `self$options$multiClassROC` | Output item / Table |
| `multiClassStrategy` | UI Control `multiClassStrategy` | `self$options$multiClassStrategy` | Output item / Table |
| `multiClassAveraging` | UI Control `multiClassAveraging` | `self$options$multiClassAveraging` | Output item / Table |
| `clinicalImpact` | UI Control `clinicalImpact` | `self$options$clinicalImpact` | Output item / Table |
| `nntCalculation` | UI Control `nntCalculation` | `self$options$nntCalculation` | Output item / Table |
| `clinicalUtilityCurve` | UI Control `clinicalUtilityCurve` | `self$options$clinicalUtilityCurve` | Output item / Table |
| `decisionImpactTable` | UI Control `decisionImpactTable` | `self$options$decisionImpactTable` | Output item / Table |
| `harrellCIndex` | UI Control `harrellCIndex` | `self$options$harrellCIndex` | Output item / Table |
| `unoCStatistic` | UI Control `unoCStatistic` | `self$options$unoCStatistic` | Output item / Table |
| `incidentDynamic` | UI Control `incidentDynamic` | `self$options$incidentDynamic` | Output item / Table |
| `cumulativeDynamic` | UI Control `cumulativeDynamic` | `self$options$cumulativeDynamic` | Output item / Table |
| `competingRisksConcordance` | UI Control `competingRisksConcordance` | `self$options$competingRisksConcordance` | Output item / Table |
| `internalValidation` | UI Control `internalValidation` | `self$options$internalValidation` | Output item / Table |
| `validationMethod` | UI Control `validationMethod` | `self$options$validationMethod` | Output item / Table |
| `optimismCorrection` | UI Control `optimismCorrection` | `self$options$optimismCorrection` | Output item / Table |
| `externalValidation` | UI Control `externalValidation` | `self$options$externalValidation` | Output item / Table |
| `decisionImpactCurves` | UI Control `decisionImpactCurves` | `self$options$decisionImpactCurves` | Output item / Table |
| `netBenefitRegression` | UI Control `netBenefitRegression` | `self$options$netBenefitRegression` | Output item / Table |
| `modelUpdating` | UI Control `modelUpdating` | `self$options$modelUpdating` | Output item / Table |
| `transportability` | UI Control `transportability` | `self$options$transportability` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/enhancedROC.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

