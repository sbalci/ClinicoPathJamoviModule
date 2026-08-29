# Cross Tables - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `crosstable`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `sty` | UI Control `sty` | `self$options$sty` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `cont` | UI Control `cont` | `self$options$cont` | Output item / Table |
| `pcat` | UI Control `pcat` | `self$options$pcat` | Output item / Table |
| `p_adjust` | UI Control `p_adjust` | `self$options$p_adjust` | Output item / Table |
| `showSMD` | UI Control `showSMD` | `self$options$showSMD` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/crosstable.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

