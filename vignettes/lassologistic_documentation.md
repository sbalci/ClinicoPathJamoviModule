# LASSO Logistic Regression - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `lassologistic`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `outcomeLevel` | UI Control `outcomeLevel` | `self$options$outcomeLevel` | Output item / Table |
| `explanatory` | UI Control `explanatory` | `self$options$explanatory` | Output item / Table |
| `penalty` | UI Control `penalty` | `self$options$penalty` | Output item / Table |
| `alpha` | UI Control `alpha` | `self$options$alpha` | Output item / Table |
| `lambda` | UI Control `lambda` | `self$options$lambda` | Output item / Table |
| `nfolds` | UI Control `nfolds` | `self$options$nfolds` | Output item / Table |
| `random_seed` | UI Control `random_seed` | `self$options$random_seed` | Output item / Table |
| `standardize` | UI Control `standardize` | `self$options$standardize` | Output item / Table |
| `suitabilityCheck` | UI Control `suitabilityCheck` | `self$options$suitabilityCheck` | Output item / Table |
| `bootstrapValidation` | UI Control `bootstrapValidation` | `self$options$bootstrapValidation` | Output item / Table |
| `bootstrapN` | UI Control `bootstrapN` | `self$options$bootstrapN` | Output item / Table |
| `cv_plot` | UI Control `cv_plot` | `self$options$cv_plot` | Output item / Table |
| `coef_plot` | UI Control `coef_plot` | `self$options$coef_plot` | Output item / Table |
| `roc_plot` | UI Control `roc_plot` | `self$options$roc_plot` | Output item / Table |
| `scoringSystem` | UI Control `scoringSystem` | `self$options$scoringSystem` | Output item / Table |
| `scoringMethod` | UI Control `scoringMethod` | `self$options$scoringMethod` | Output item / Table |
| `scoringMaxPoints` | UI Control `scoringMaxPoints` | `self$options$scoringMaxPoints` | Output item / Table |
| `scoreCutMethod` | UI Control `scoreCutMethod` | `self$options$scoreCutMethod` | Output item / Table |
| `scoreCutPoints` | UI Control `scoreCutPoints` | `self$options$scoreCutPoints` | Output item / Table |
| `scoreLookupTable` | UI Control `scoreLookupTable` | `self$options$scoreLookupTable` | Output item / Table |
| `predictions` | UI Control `predictions` | `self$options$predictions` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `showMethodologyNotes` | UI Control `showMethodologyNotes` | `self$options$showMethodologyNotes` | Output item / Table |
| `includeClinicalGuidance` | UI Control `includeClinicalGuidance` | `self$options$includeClinicalGuidance` | Output item / Table |
| `showVariableImportance` | UI Control `showVariableImportance` | `self$options$showVariableImportance` | Output item / Table |
| `showModelComparison` | UI Control `showModelComparison` | `self$options$showModelComparison` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/lassologistic.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

