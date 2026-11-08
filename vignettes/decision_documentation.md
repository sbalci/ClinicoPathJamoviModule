# Medical Decision - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `decision`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `referenceType` | UI Control `referenceType` | `self$options$referenceType` | `referenceDefinition`, comparator outputs |
| `referenceDescription` | UI Control `referenceDescription` | `self$options$referenceDescription` | `referenceDefinition`, `validationContext` |
| `targetDefinition` | UI Control `targetDefinition` | `self$options$targetDefinition` | `referenceDefinition` |
| `gold` | UI Control `gold` | `self$options$gold` | Output item / Table |
| `goldPositive` | UI Control `goldPositive` | `self$options$goldPositive` | Output item / Table |
| `newtest` | UI Control `newtest` | `self$options$newtest` | Output item / Table |
| `testPositive` | UI Control `testPositive` | `self$options$testPositive` | Output item / Table |
| `goldNegative` | UI Control `goldNegative` | `self$options$goldNegative` | Output item / Table |
| `testNegative` | UI Control `testNegative` | `self$options$testNegative` | Output item / Table |
| `excludeIndeterminate` | UI Control `excludeIndeterminate` | `self$options$excludeIndeterminate` | `levelMapping`, all estimates |
| `pp` | UI Control `pp` | `self$options$pp` | Output item / Table |
| `pprob` | UI Control `pprob` | `self$options$pprob` | Output item / Table |
| `od` | UI Control `od` | `self$options$od` | Output item / Table |
| `fnote` | UI Control `fnote` | `self$options$fnote` | Output item / Table |
| `ci` | UI Control `ci` | `self$options$ci` | Output item / Table |
| `fagan` | UI Control `fagan` | `self$options$fagan` | Output item / Table |
| `faganPalette` | UI Control `faganPalette` | `self$options$faganPalette` | `plot1` |
| `showNaturalLanguage` | UI Control `showNaturalLanguage` | `self$options$showNaturalLanguage` | Output item / Table |
| `showClinicalInterpretation` | UI Control `showClinicalInterpretation` | `self$options$showClinicalInterpretation` | Output item / Table |
| `showReportTemplate` | UI Control `showReportTemplate` | `self$options$showReportTemplate` | Output item / Table |
| `showAboutAnalysis` | UI Control `showAboutAnalysis` | `self$options$showAboutAnalysis` | Output item / Table |
| `showMisclassified` | UI Control `showMisclassified` | `self$options$showMisclassified` | Output item / Table |
| `maxCasesShow` | UI Control `maxCasesShow` | `self$options$maxCasesShow` | Output item / Table |
| `saveClassifications` | UI Control `saveClassifications` | `self$options$saveClassifications` | Output item / Table |
| `showValidationContext` | UI Control `showValidationContext` | `self$options$showValidationContext` | `validationContext` |
| `cohortRole` | UI Control `cohortRole` | `self$options$cohortRole` | `validationContext` |
| `samplingDesign` | UI Control `samplingDesign` | `self$options$samplingDesign` | `validationContext` |
| `thresholdSource` | UI Control `thresholdSource` | `self$options$thresholdSource` | `validationContext` |
| `modelVersion` | UI Control `modelVersion` | `self$options$modelVersion` | `validationContext` |
| `analysisUnit` | UI Control `analysisUnit` | `self$options$analysisUnit` | `validationContext` |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/decision.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

