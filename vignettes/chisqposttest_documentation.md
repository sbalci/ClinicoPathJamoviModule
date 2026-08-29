# Chi-Square Post-Hoc Tests - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `chisqposttest`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `rows` | UI Control `rows` | `self$options$rows` | Output item / Table |
| `cols` | UI Control `cols` | `self$options$cols` | Output item / Table |
| `counts` | UI Control `counts` | `self$options$counts` | Output item / Table |
| `posthoc` | UI Control `posthoc` | `self$options$posthoc` | Output item / Table |
| `sig` | UI Control `sig` | `self$options$sig` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `exp` | UI Control `exp` | `self$options$exp` | Output item / Table |
| `plot` | UI Control `plot` | `self$options$plot` | Output item / Table |
| `showResiduals` | UI Control `showResiduals` | `self$options$showResiduals` | Output item / Table |
| `showEducational` | UI Control `showEducational` | `self$options$showEducational` | Output item / Table |
| `showDetailedTables` | UI Control `showDetailedTables` | `self$options$showDetailedTables` | Output item / Table |
| `residualsCriterion` | UI Control `residualsCriterion` | `self$options$residualsCriterion` | Output item / Table |
| `residualsCutoff` | UI Control `residualsCutoff` | `self$options$residualsCutoff` | Output item / Table |
| `phiCI` | UI Control `phiCI` | `self$options$phiCI` | Output item / Table |
| `testSelection` | UI Control `testSelection` | `self$options$testSelection` | Output item / Table |
| `exportResults` | UI Control `exportResults` | `self$options$exportResults` | Output item / Table |
| `showClinicalSummary` | UI Control `showClinicalSummary` | `self$options$showClinicalSummary` | Output item / Table |
| `copyReadySentences` | UI Control `copyReadySentences` | `self$options$copyReadySentences` | Output item / Table |
| `showAssumptionsCheck` | UI Control `showAssumptionsCheck` | `self$options$showAssumptionsCheck` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/chisqposttest.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

