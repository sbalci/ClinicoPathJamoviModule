# Correlation Matrix - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjcorrmat`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `typestatistics` | UI Control `typestatistics` | `self$options$typestatistics` | Output item / Table |
| `bayesseed` | UI Control `bayesseed` | `self$options$bayesseed` | Bayesian seed (Table & Plot) |
| `matrixtype` | UI Control `matrixtype` | `self$options$matrixtype` | Output item / Table |
| `matrixmethod` | UI Control `matrixmethod` | `self$options$matrixmethod` | Output item / Table |
| `siglevel` | UI Control `siglevel` | `self$options$siglevel` | Output item / Table |
| `conflevel` | UI Control `conflevel` | `self$options$conflevel` | Output item / Table |
| `padjustmethod` | UI Control `padjustmethod` | `self$options$padjustmethod` | Output item / Table |
| `k` | UI Control `k` | `self$options$k` | Plot label decimals |
| `partial` | UI Control `partial` | `self$options$partial` | Output item / Table |
| `naHandling` | UI Control `naHandling` | `self$options$naHandling` | Output item / Table |
| `lowcolor` | UI Control `lowcolor` | `self$options$lowcolor` | Plot palette |
| `midcolor` | UI Control `midcolor` | `self$options$midcolor` | Plot palette |
| `highcolor` | UI Control `highcolor` | `self$options$highcolor` | Plot palette |
| `originaltheme` | UI Control `originaltheme` | `self$options$originaltheme` | Plot theme |
| `title` | UI Control `title` | `self$options$title` | Output item / Table |
| `subtitle` | UI Control `subtitle` | `self$options$subtitle` | Output item / Table |
| `caption` | UI Control `caption` | `self$options$caption` | Output item / Table |
| `showexplanations` | UI Control `showexplanations` | `self$options$showexplanations` | Output item / Table |
| `plotwidth` | UI Control `plotwidth` | `self$options$plotwidth` | Output item / Table |
| `plotheight` | UI Control `plotheight` | `self$options$plotheight` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjcorrmat.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

