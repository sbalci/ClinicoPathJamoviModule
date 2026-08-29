# Odds Ratio Table and Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `oddsratio`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `explanatory` | UI Control `explanatory` | `self$options$explanatory` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `outcomeLevel` | UI Control `outcomeLevel` | `self$options$outcomeLevel` | Output item / Table |
| `diagnosticPredictor` | UI Control `diagnosticPredictor` | `self$options$diagnosticPredictor` | Output item / Table |
| `predictorLevel` | UI Control `predictorLevel` | `self$options$predictorLevel` | Output item / Table |
| `usePenalized` | UI Control `usePenalized` | `self$options$usePenalized` | Output item / Table |
| `showNomogram` | UI Control `showNomogram` | `self$options$showNomogram` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/oddsratio.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

