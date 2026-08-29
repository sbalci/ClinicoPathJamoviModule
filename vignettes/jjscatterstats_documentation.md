# Scatter Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjscatterstats`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `colorvar` | UI Control `colorvar` | `self$options$colorvar` | Output item / Table |
| `sizevar` | UI Control `sizevar` | `self$options$sizevar` | Output item / Table |
| `shapevar` | UI Control `shapevar` | `self$options$shapevar` | Output item / Table |
| `alphavar` | UI Control `alphavar` | `self$options$alphavar` | Output item / Table |
| `labelvar` | UI Control `labelvar` | `self$options$labelvar` | Output item / Table |
| `showRugPlot` | UI Control `showRugPlot` | `self$options$showRugPlot` | Output item / Table |
| `marginalType` | UI Control `marginalType` | `self$options$marginalType` | Output item / Table |
| `smoothMethod` | UI Control `smoothMethod` | `self$options$smoothMethod` | Output item / Table |
| `typestatistics` | UI Control `typestatistics` | `self$options$typestatistics` | Output item / Table |
| `mytitle` | UI Control `mytitle` | `self$options$mytitle` | Output item / Table |
| `xtitle` | UI Control `xtitle` | `self$options$xtitle` | Output item / Table |
| `ytitle` | UI Control `ytitle` | `self$options$ytitle` | Output item / Table |
| `originaltheme` | UI Control `originaltheme` | `self$options$originaltheme` | Output item / Table |
| `resultssubtitle` | UI Control `resultssubtitle` | `self$options$resultssubtitle` | Output item / Table |
| `conflevel` | UI Control `conflevel` | `self$options$conflevel` | Output item / Table |
| `bfmessage` | UI Control `bfmessage` | `self$options$bfmessage` | Output item / Table |
| `k` | UI Control `k` | `self$options$k` | Output item / Table |
| `marginal` | UI Control `marginal` | `self$options$marginal` | Output item / Table |
| `xsidefill` | UI Control `xsidefill` | `self$options$xsidefill` | Output item / Table |
| `ysidefill` | UI Control `ysidefill` | `self$options$ysidefill` | Output item / Table |
| `pointsize` | UI Control `pointsize` | `self$options$pointsize` | Output item / Table |
| `pointalpha` | UI Control `pointalpha` | `self$options$pointalpha` | Output item / Table |
| `smoothlinesize` | UI Control `smoothlinesize` | `self$options$smoothlinesize` | Output item / Table |
| `smoothlinecolor` | UI Control `smoothlinecolor` | `self$options$smoothlinecolor` | Output item / Table |
| `plotwidth` | UI Control `plotwidth` | `self$options$plotwidth` | Output item / Table |
| `plotheight` | UI Control `plotheight` | `self$options$plotheight` | Output item / Table |
| `addGGPubrPlot` | UI Control `addGGPubrPlot` | `self$options$addGGPubrPlot` | Output item / Table |
| `ggpubrPalette` | UI Control `ggpubrPalette` | `self$options$ggpubrPalette` | Output item / Table |
| `ggpubrAddCorr` | UI Control `ggpubrAddCorr` | `self$options$ggpubrAddCorr` | Output item / Table |
| `ggpubrCorrMethod` | UI Control `ggpubrCorrMethod` | `self$options$ggpubrCorrMethod` | Output item / Table |
| `ggpubrAddSmooth` | UI Control `ggpubrAddSmooth` | `self$options$ggpubrAddSmooth` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `clinicalPreset` | UI Control `clinicalPreset` | `self$options$clinicalPreset` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjscatterstats.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

