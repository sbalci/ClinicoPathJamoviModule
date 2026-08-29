# Treatment Response: Patient-Level Burden - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `waterfall`
- **Module**: `OncoPath`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `patientID` | UI Control `patientID` | `self$options$patientID` | Output item / Table |
| `responseVar` | UI Control `responseVar` | `self$options$responseVar` | Output item / Table |
| `timeVar` | UI Control `timeVar` | `self$options$timeVar` | Output item / Table |
| `groupVar` | UI Control `groupVar` | `self$options$groupVar` | Output item / Table |
| `inputType` | UI Control `inputType` | `self$options$inputType` | Output item / Table |
| `sortBy` | UI Control `sortBy` | `self$options$sortBy` | Output item / Table |
| `sortDirection` | UI Control `sortDirection` | `self$options$sortDirection` | Output item / Table |
| `showBaseline` | UI Control `showBaseline` | `self$options$showBaseline` | Output item / Table |
| `confirmationVar` | UI Control `confirmationVar` | `self$options$confirmationVar` | Output item / Table |
| `ongoingVar` | UI Control `ongoingVar` | `self$options$ongoingVar` | Output item / Table |
| `responseCategoryVar` | UI Control `responseCategoryVar` | `self$options$responseCategoryVar` | Output item / Table |
| `showCategoryLabels` | UI Control `showCategoryLabels` | `self$options$showCategoryLabels` | Output item / Table |
| `showSpiderLabels` | UI Control `showSpiderLabels` | `self$options$showSpiderLabels` | Output item / Table |
| `annotationVars` | UI Control `annotationVars` | `self$options$annotationVars` | Output item / Table |
| `showThresholds` | UI Control `showThresholds` | `self$options$showThresholds` | Output item / Table |
| `labelOutliers` | UI Control `labelOutliers` | `self$options$labelOutliers` | Output item / Table |
| `showMedian` | UI Control `showMedian` | `self$options$showMedian` | Output item / Table |
| `showCI` | UI Control `showCI` | `self$options$showCI` | Output item / Table |
| `minResponseForLabel` | UI Control `minResponseForLabel` | `self$options$minResponseForLabel` | Output item / Table |
| `colorBy` | UI Control `colorBy` | `self$options$colorBy` | Output item / Table |
| `colorScheme` | UI Control `colorScheme` | `self$options$colorScheme` | Output item / Table |
| `barAlpha` | UI Control `barAlpha` | `self$options$barAlpha` | Output item / Table |
| `barWidth` | UI Control `barWidth` | `self$options$barWidth` | Output item / Table |
| `showWaterfallPlot` | UI Control `showWaterfallPlot` | `self$options$showWaterfallPlot` | Output item / Table |
| `showSpiderPlot` | UI Control `showSpiderPlot` | `self$options$showSpiderPlot` | Output item / Table |
| `spiderColorBy` | UI Control `spiderColorBy` | `self$options$spiderColorBy` | Output item / Table |
| `spiderColorScheme` | UI Control `spiderColorScheme` | `self$options$spiderColorScheme` | Output item / Table |
| `timeUnitLabel` | UI Control `timeUnitLabel` | `self$options$timeUnitLabel` | Output item / Table |
| `generateCopyReadyReport` | UI Control `generateCopyReadyReport` | `self$options$generateCopyReadyReport` | Output item / Table |
| `showClinicalSignificance` | UI Control `showClinicalSignificance` | `self$options$showClinicalSignificance` | Output item / Table |
| `showConfidenceIntervals` | UI Control `showConfidenceIntervals` | `self$options$showConfidenceIntervals` | Output item / Table |
| `enableGuidedMode` | UI Control `enableGuidedMode` | `self$options$enableGuidedMode` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `showResponseDuration` | UI Control `showResponseDuration` | `self$options$showResponseDuration` | Output item / Table |
| `addResponseCategory` | UI Control `addResponseCategory` | `self$options$addResponseCategory` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/waterfall.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

