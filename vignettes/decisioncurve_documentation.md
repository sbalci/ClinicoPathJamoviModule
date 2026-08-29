# Decision Curve Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `decisioncurve`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `outcomePositive` | UI Control `outcomePositive` | `self$options$outcomePositive` | Output item / Table |
| `models` | UI Control `models` | `self$options$models` | Output item / Table |
| `modelNames` | UI Control `modelNames` | `self$options$modelNames` | Output item / Table |
| `thresholdRange` | UI Control `thresholdRange` | `self$options$thresholdRange` | Output item / Table |
| `thresholdMin` | UI Control `thresholdMin` | `self$options$thresholdMin` | Output item / Table |
| `thresholdMax` | UI Control `thresholdMax` | `self$options$thresholdMax` | Output item / Table |
| `thresholdStep` | UI Control `thresholdStep` | `self$options$thresholdStep` | Output item / Table |
| `showTable` | UI Control `showTable` | `self$options$showTable` | Output item / Table |
| `selectedThresholds` | UI Control `selectedThresholds` | `self$options$selectedThresholds` | Output item / Table |
| `showPlot` | UI Control `showPlot` | `self$options$showPlot` | Output item / Table |
| `plotStyle` | UI Control `plotStyle` | `self$options$plotStyle` | Output item / Table |
| `showReferenceLinesLabels` | UI Control `showReferenceLinesLabels` | `self$options$showReferenceLinesLabels` | Output item / Table |
| `highlightRange` | UI Control `highlightRange` | `self$options$highlightRange` | Output item / Table |
| `highlightMin` | UI Control `highlightMin` | `self$options$highlightMin` | Output item / Table |
| `highlightMax` | UI Control `highlightMax` | `self$options$highlightMax` | Output item / Table |
| `calculateClinicalImpact` | UI Control `calculateClinicalImpact` | `self$options$calculateClinicalImpact` | Output item / Table |
| `populationSize` | UI Control `populationSize` | `self$options$populationSize` | Output item / Table |
| `showInterventionAvoided` | UI Control `showInterventionAvoided` | `self$options$showInterventionAvoided` | Output item / Table |
| `confidenceIntervals` | UI Control `confidenceIntervals` | `self$options$confidenceIntervals` | Output item / Table |
| `bootReps` | UI Control `bootReps` | `self$options$bootReps` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |
| `ciLevel` | UI Control `ciLevel` | `self$options$ciLevel` | Output item / Table |
| `showBenefitRange` | UI Control `showBenefitRange` | `self$options$showBenefitRange` | Output item / Table |
| `compareModels` | UI Control `compareModels` | `self$options$compareModels` | Output item / Table |
| `weightedAUC` | UI Control `weightedAUC` | `self$options$weightedAUC` | Output item / Table |
| `clinicalDecisionRule` | UI Control `clinicalDecisionRule` | `self$options$clinicalDecisionRule` | Output item / Table |
| `decisionRuleVar` | UI Control `decisionRuleVar` | `self$options$decisionRuleVar` | Output item / Table |
| `decisionRulePositive` | UI Control `decisionRulePositive` | `self$options$decisionRulePositive` | Output item / Table |
| `decisionRuleLabel` | UI Control `decisionRuleLabel` | `self$options$decisionRuleLabel` | Output item / Table |
| `showClinicalImpactPlot` | UI Control `showClinicalImpactPlot` | `self$options$showClinicalImpactPlot` | Output item / Table |
| `showNetBenefitCI` | UI Control `showNetBenefitCI` | `self$options$showNetBenefitCI` | Output item / Table |
| `costBenefitAnalysis` | UI Control `costBenefitAnalysis` | `self$options$costBenefitAnalysis` | Output item / Table |
| `testCost` | UI Control `testCost` | `self$options$testCost` | Output item / Table |
| `treatmentCost` | UI Control `treatmentCost` | `self$options$treatmentCost` | Output item / Table |
| `benefitCorrectTreatment` | UI Control `benefitCorrectTreatment` | `self$options$benefitCorrectTreatment` | Output item / Table |
| `harmFalseTreatment` | UI Control `harmFalseTreatment` | `self$options$harmFalseTreatment` | Output item / Table |
| `showStandardizedNetBenefit` | UI Control `showStandardizedNetBenefit` | `self$options$showStandardizedNetBenefit` | Output item / Table |
| `multiModelComparison` | UI Control `multiModelComparison` | `self$options$multiModelComparison` | Output item / Table |
| `comparisonMethod` | UI Control `comparisonMethod` | `self$options$comparisonMethod` | Output item / Table |
| `showDecisionConsequences` | UI Control `showDecisionConsequences` | `self$options$showDecisionConsequences` | Output item / Table |
| `resourceUtilization` | UI Control `resourceUtilization` | `self$options$resourceUtilization` | Output item / Table |
| `showRelativeUtility` | UI Control `showRelativeUtility` | `self$options$showRelativeUtility` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/decisioncurve.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

