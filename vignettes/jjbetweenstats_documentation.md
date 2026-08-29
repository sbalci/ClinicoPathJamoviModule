# Box-Violin Plots to Compare Between Groups - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjbetweenstats`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `centralityplotting` | UI Control `centralityplotting` | `self$options$centralityplotting` | Output item / Table |
| `centralitytype` | UI Control `centralitytype` | `self$options$centralitytype` | Output item / Table |
| `typestatistics` | UI Control `typestatistics` | `self$options$typestatistics` | Output item / Table |
| `pairwisecomparisons` | UI Control `pairwisecomparisons` | `self$options$pairwisecomparisons` | Output item / Table |
| `pairwisedisplay` | UI Control `pairwisedisplay` | `self$options$pairwisedisplay` | Output item / Table |
| `padjustmethod` | UI Control `padjustmethod` | `self$options$padjustmethod` | Output item / Table |
| `effsizetype` | UI Control `effsizetype` | `self$options$effsizetype` | Output item / Table |
| `mytitle` | UI Control `mytitle` | `self$options$mytitle` | Output item / Table |
| `xtitle` | UI Control `xtitle` | `self$options$xtitle` | Output item / Table |
| `ytitle` | UI Control `ytitle` | `self$options$ytitle` | Output item / Table |
| `originaltheme` | UI Control `originaltheme` | `self$options$originaltheme` | Output item / Table |
| `resultssubtitle` | UI Control `resultssubtitle` | `self$options$resultssubtitle` | Output item / Table |
| `bfmessage` | UI Control `bfmessage` | `self$options$bfmessage` | Output item / Table |
| `k` | UI Control `k` | `self$options$k` | Output item / Table |
| `conflevel` | UI Control `conflevel` | `self$options$conflevel` | Output item / Table |
| `varequal` | UI Control `varequal` | `self$options$varequal` | Output item / Table |
| `multiEndpointCorrection` | UI Control `multiEndpointCorrection` | `self$options$multiEndpointCorrection` | Output item / Table |
| `plotwidth` | UI Control `plotwidth` | `self$options$plotwidth` | Output item / Table |
| `plotheight` | UI Control `plotheight` | `self$options$plotheight` | Output item / Table |
| `colorblindSafe` | UI Control `colorblindSafe` | `self$options$colorblindSafe` | Output item / Table |
| `showexplanations` | UI Control `showexplanations` | `self$options$showexplanations` | Output item / Table |
| `addGGPubrPlot` | UI Control `addGGPubrPlot` | `self$options$addGGPubrPlot` | Output item / Table |
| `ggpubrPlotType` | UI Control `ggpubrPlotType` | `self$options$ggpubrPlotType` | Output item / Table |
| `ggpubrPalette` | UI Control `ggpubrPalette` | `self$options$ggpubrPalette` | Output item / Table |
| `ggpubrAddStats` | UI Control `ggpubrAddStats` | `self$options$ggpubrAddStats` | Output item / Table |
| `ggpubrAddPoints` | UI Control `ggpubrAddPoints` | `self$options$ggpubrAddPoints` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjbetweenstats.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

