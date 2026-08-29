# Pie Charts - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjpiestats`
- **Module**: `JJStatsPlot`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `typestatistics` | UI Control `typestatistics` | `self$options$typestatistics` | Output item / Table |
| `originaltheme` | UI Control `originaltheme` | `self$options$originaltheme` | Output item / Table |
| `counts` | UI Control `counts` | `self$options$counts` | Output item / Table |
| `ratio` | UI Control `ratio` | `self$options$ratio` | Output item / Table |
| `paired` | UI Control `paired` | `self$options$paired` | Output item / Table |
| `label` | UI Control `label` | `self$options$label` | Output item / Table |
| `digits` | UI Control `digits` | `self$options$digits` | Output item / Table |
| `conflevel` | UI Control `conflevel` | `self$options$conflevel` | Output item / Table |
| `proportiontest` | UI Control `proportiontest` | `self$options$proportiontest` | Output item / Table |
| `bfmessage` | UI Control `bfmessage` | `self$options$bfmessage` | Output item / Table |
| `messages` | UI Control `messages` | `self$options$messages` | Output item / Table |
| `clinicalpreset` | UI Control `clinicalpreset` | `self$options$clinicalpreset` | Output item / Table |
| `showexplanations` | UI Control `showexplanations` | `self$options$showexplanations` | Output item / Table |
| `resultssubtitle` | UI Control `resultssubtitle` | `self$options$resultssubtitle` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showAssumptions` | UI Control `showAssumptions` | `self$options$showAssumptions` | Output item / Table |
| `showInterpretation` | UI Control `showInterpretation` | `self$options$showInterpretation` | Output item / Table |
| `addGGPubrDonut` | UI Control `addGGPubrDonut` | `self$options$addGGPubrDonut` | Output item / Table |
| `ggpubrDonutPalette` | UI Control `ggpubrDonutPalette` | `self$options$ggpubrDonutPalette` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjpiestats.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

