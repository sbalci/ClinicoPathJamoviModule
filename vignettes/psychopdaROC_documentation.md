# Advanced ROC Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `psychopdaROC`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `manualRun` | UI Control `manualRun` | `self$options$manualRun` | Output item / Table |
| `run` | UI Control `run` | `self$options$run` | Output item / Table |
| `clinicalMode` | UI Control `clinicalMode` | `self$options$clinicalMode` | Output item / Table |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dependentVars` | UI Control `dependentVars` | `self$options$dependentVars` | Output item / Table |
| `classVar` | UI Control `classVar` | `self$options$classVar` | Output item / Table |
| `positiveClass` | UI Control `positiveClass` | `self$options$positiveClass` | Output item / Table |
| `subGroup` | UI Control `subGroup` | `self$options$subGroup` | Output item / Table |
| `method` | UI Control `method` | `self$options$method` | Output item / Table |
| `metric` | UI Control `metric` | `self$options$metric` | Output item / Table |
| `direction` | UI Control `direction` | `self$options$direction` | Output item / Table |
| `specifyCutScore` | UI Control `specifyCutScore` | `self$options$specifyCutScore` | Output item / Table |
| `tol_metric` | UI Control `tol_metric` | `self$options$tol_metric` | Output item / Table |
| `break_ties` | UI Control `break_ties` | `self$options$break_ties` | Output item / Table |
| `allObserved` | UI Control `allObserved` | `self$options$allObserved` | Output item / Table |
| `boot_runs` | UI Control `boot_runs` | `self$options$boot_runs` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |
| `usePriorPrev` | UI Control `usePriorPrev` | `self$options$usePriorPrev` | Output item / Table |
| `priorPrev` | UI Control `priorPrev` | `self$options$priorPrev` | Output item / Table |
| `costratioFP` | UI Control `costratioFP` | `self$options$costratioFP` | Output item / Table |
| `sensSpecTable` | UI Control `sensSpecTable` | `self$options$sensSpecTable` | Output item / Table |
| `showThresholdTable` | UI Control `showThresholdTable` | `self$options$showThresholdTable` | Output item / Table |
| `maxThresholds` | UI Control `maxThresholds` | `self$options$maxThresholds` | Output item / Table |
| `delongTest` | UI Control `delongTest` | `self$options$delongTest` | Output item / Table |
| `plotROC` | UI Control `plotROC` | `self$options$plotROC` | Output item / Table |
| `combinePlots` | UI Control `combinePlots` | `self$options$combinePlots` | Output item / Table |
| `cleanPlot` | UI Control `cleanPlot` | `self$options$cleanPlot` | Output item / Table |
| `showOptimalPoint` | UI Control `showOptimalPoint` | `self$options$showOptimalPoint` | Output item / Table |
| `displaySE` | UI Control `displaySE` | `self$options$displaySE` | Output item / Table |
| `smoothing` | UI Control `smoothing` | `self$options$smoothing` | Output item / Table |
| `showConfidenceBands` | UI Control `showConfidenceBands` | `self$options$showConfidenceBands` | Output item / Table |
| `legendPosition` | UI Control `legendPosition` | `self$options$legendPosition` | Output item / Table |
| `directLabel` | UI Control `directLabel` | `self$options$directLabel` | Output item / Table |
| `interactiveROC` | UI Control `interactiveROC` | `self$options$interactiveROC` | Output item / Table |
| `showCriterionPlot` | UI Control `showCriterionPlot` | `self$options$showCriterionPlot` | Output item / Table |
| `showPrevalencePlot` | UI Control `showPrevalencePlot` | `self$options$showPrevalencePlot` | Output item / Table |
| `showDotPlot` | UI Control `showDotPlot` | `self$options$showDotPlot` | Output item / Table |
| `precisionRecallCurve` | UI Control `precisionRecallCurve` | `self$options$precisionRecallCurve` | Output item / Table |
| `partialAUC` | UI Control `partialAUC` | `self$options$partialAUC` | Output item / Table |
| `partialAUCfrom` | UI Control `partialAUCfrom` | `self$options$partialAUCfrom` | Output item / Table |
| `partialAUCto` | UI Control `partialAUCto` | `self$options$partialAUCto` | Output item / Table |
| `rocSmoothingMethod` | UI Control `rocSmoothingMethod` | `self$options$rocSmoothingMethod` | Output item / Table |
| `bootstrapCI` | UI Control `bootstrapCI` | `self$options$bootstrapCI` | Output item / Table |
| `bootstrapReps` | UI Control `bootstrapReps` | `self$options$bootstrapReps` | Output item / Table |
| `quantileCIs` | UI Control `quantileCIs` | `self$options$quantileCIs` | Output item / Table |
| `quantiles` | UI Control `quantiles` | `self$options$quantiles` | Output item / Table |
| `compareClassifiers` | UI Control `compareClassifiers` | `self$options$compareClassifiers` | Output item / Table |
| `calculateIDI` | UI Control `calculateIDI` | `self$options$calculateIDI` | Output item / Table |
| `calculateNRI` | UI Control `calculateNRI` | `self$options$calculateNRI` | Output item / Table |
| `refVar` | UI Control `refVar` | `self$options$refVar` | Output item / Table |
| `nriThresholds` | UI Control `nriThresholds` | `self$options$nriThresholds` | Output item / Table |
| `idiNriBootRuns` | UI Control `idiNriBootRuns` | `self$options$idiNriBootRuns` | Output item / Table |
| `effectSizeAnalysis` | UI Control `effectSizeAnalysis` | `self$options$effectSizeAnalysis` | Output item / Table |
| `powerAnalysis` | UI Control `powerAnalysis` | `self$options$powerAnalysis` | Output item / Table |
| `powerAnalysisType` | UI Control `powerAnalysisType` | `self$options$powerAnalysisType` | Output item / Table |
| `expectedAUCDifference` | UI Control `expectedAUCDifference` | `self$options$expectedAUCDifference` | Output item / Table |
| `targetPower` | UI Control `targetPower` | `self$options$targetPower` | Output item / Table |
| `significanceLevel` | UI Control `significanceLevel` | `self$options$significanceLevel` | Output item / Table |
| `correlationROCs` | UI Control `correlationROCs` | `self$options$correlationROCs` | Output item / Table |
| `bayesianAnalysis` | UI Control `bayesianAnalysis` | `self$options$bayesianAnalysis` | Output item / Table |
| `priorAUC` | UI Control `priorAUC` | `self$options$priorAUC` | Output item / Table |
| `priorPrecision` | UI Control `priorPrecision` | `self$options$priorPrecision` | Output item / Table |
| `clinicalUtilityAnalysis` | UI Control `clinicalUtilityAnalysis` | `self$options$clinicalUtilityAnalysis` | Output item / Table |
| `treatmentThreshold` | UI Control `treatmentThreshold` | `self$options$treatmentThreshold` | Output item / Table |
| `harmBenefitRatio` | UI Control `harmBenefitRatio` | `self$options$harmBenefitRatio` | Output item / Table |
| `interventionCost` | UI Control `interventionCost` | `self$options$interventionCost` | Output item / Table |
| `fixedSensSpecAnalysis` | UI Control `fixedSensSpecAnalysis` | `self$options$fixedSensSpecAnalysis` | Output item / Table |
| `fixedAnalysisType` | UI Control `fixedAnalysisType` | `self$options$fixedAnalysisType` | Output item / Table |
| `fixedSensitivityValue` | UI Control `fixedSensitivityValue` | `self$options$fixedSensitivityValue` | Output item / Table |
| `fixedSpecificityValue` | UI Control `fixedSpecificityValue` | `self$options$fixedSpecificityValue` | Output item / Table |
| `showFixedROC` | UI Control `showFixedROC` | `self$options$showFixedROC` | Output item / Table |
| `fixedInterpolation` | UI Control `fixedInterpolation` | `self$options$fixedInterpolation` | Output item / Table |
| `showFixedExplanation` | UI Control `showFixedExplanation` | `self$options$showFixedExplanation` | Output item / Table |
| `metaAnalysis` | UI Control `metaAnalysis` | `self$options$metaAnalysis` | Output item / Table |
| `metaAnalysisMethod` | UI Control `metaAnalysisMethod` | `self$options$metaAnalysisMethod` | Output item / Table |
| `heterogeneityTest` | UI Control `heterogeneityTest` | `self$options$heterogeneityTest` | Output item / Table |
| `forestPlot` | UI Control `forestPlot` | `self$options$forestPlot` | Output item / Table |
| `overrideMetaAnalysisWarning` | UI Control `overrideMetaAnalysisWarning` | `self$options$overrideMetaAnalysisWarning` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/psychopdaROC.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

