# Dot Chart (Summary vs Reference Value) - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjdotchart`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `testvalue` | UI Control `testvalue` | `self$options$testvalue` | Output item / Table |
| `typestatistics` | UI Control `typestatistics` | `self$options$typestatistics` | Output item / Table |
| `conflevel` | UI Control `conflevel` | `self$options$conflevel` | Output item / Table |
| `k` | UI Control `k` | `self$options$k` | Output item / Table |
| `resultssubtitle` | UI Control `resultssubtitle` | `self$options$resultssubtitle` | Output item / Table |
| `showSummaryTable` | UI Control `showSummaryTable` | `self$options$showSummaryTable` | Output item / Table |
| `centralityplotting` | UI Control `centralityplotting` | `self$options$centralityplotting` | Output item / Table |
| `centralitytype` | UI Control `centralitytype` | `self$options$centralitytype` | Output item / Table |
| `originaltheme` | UI Control `originaltheme` | `self$options$originaltheme` | Output item / Table |
| `mytitle` | UI Control `mytitle` | `self$options$mytitle` | Output item / Table |
| `xtitle` | UI Control `xtitle` | `self$options$xtitle` | Output item / Table |
| `ytitle` | UI Control `ytitle` | `self$options$ytitle` | Output item / Table |
| `plotwidth` | UI Control `plotwidth` | `self$options$plotwidth` | Output item / Table |
| `plotheight` | UI Control `plotheight` | `self$options$plotheight` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjdotchart.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

