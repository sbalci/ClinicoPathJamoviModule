# Analysis Without Gold Standard - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `nogoldstandard`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `clinicalPreset` | UI Control `clinicalPreset` | `self$options$clinicalPreset` | Output item / Table |
| `test1` | UI Control `test1` | `self$options$test1` | Output item / Table |
| `test1Positive` | UI Control `test1Positive` | `self$options$test1Positive` | Output item / Table |
| `test2` | UI Control `test2` | `self$options$test2` | Output item / Table |
| `test2Positive` | UI Control `test2Positive` | `self$options$test2Positive` | Output item / Table |
| `test3` | UI Control `test3` | `self$options$test3` | Output item / Table |
| `test3Positive` | UI Control `test3Positive` | `self$options$test3Positive` | Output item / Table |
| `test4` | UI Control `test4` | `self$options$test4` | Output item / Table |
| `test4Positive` | UI Control `test4Positive` | `self$options$test4Positive` | Output item / Table |
| `test5` | UI Control `test5` | `self$options$test5` | Output item / Table |
| `test5Positive` | UI Control `test5Positive` | `self$options$test5Positive` | Output item / Table |
| `method` | UI Control `method` | `self$options$method` | Output item / Table |
| `bootstrap` | UI Control `bootstrap` | `self$options$bootstrap` | Output item / Table |
| `nboot` | UI Control `nboot` | `self$options$nboot` | Output item / Table |
| `alpha` | UI Control `alpha` | `self$options$alpha` | Output item / Table |
| `verbose` | UI Control `verbose` | `self$options$verbose` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showMethodGuide` | UI Control `showMethodGuide` | `self$options$showMethodGuide` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/nogoldstandard.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

