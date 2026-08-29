# Histogram - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjhistostats`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `typestatistics` | UI Control `typestatistics` | `self$options$typestatistics` | Output item / Table |
| `centralityline` | UI Control `centralityline` | `self$options$centralityline` | Output item / Table |
| `changebinwidth` | UI Control `changebinwidth` | `self$options$changebinwidth` | Output item / Table |
| `binwidth` | UI Control `binwidth` | `self$options$binwidth` | Output item / Table |
| `resultssubtitle` | UI Control `resultssubtitle` | `self$options$resultssubtitle` | Output item / Table |
| `showInterpretation` | UI Control `showInterpretation` | `self$options$showInterpretation` | Output item / Table |
| `clinicalPreset` | UI Control `clinicalPreset` | `self$options$clinicalPreset` | Output item / Table |
| `enableOneSampleTest` | UI Control `enableOneSampleTest` | `self$options$enableOneSampleTest` | Output item / Table |
| `test.value` | UI Control `test.value` | `self$options$test.value` | Output item / Table |
| `conf.level` | UI Control `conf.level` | `self$options$conf.level` | Output item / Table |
| `bf.message` | UI Control `bf.message` | `self$options$bf.message` | Output item / Table |
| `digits` | UI Control `digits` | `self$options$digits` | Output item / Table |
| `xlab` | UI Control `xlab` | `self$options$xlab` | Output item / Table |
| `title` | UI Control `title` | `self$options$title` | Output item / Table |
| `subtitle` | UI Control `subtitle` | `self$options$subtitle` | Output item / Table |
| `caption` | UI Control `caption` | `self$options$caption` | Output item / Table |
| `centralitytype` | UI Control `centralitytype` | `self$options$centralitytype` | Output item / Table |
| `binfill` | UI Control `binfill` | `self$options$binfill` | Output item / Table |
| `bincolor` | UI Control `bincolor` | `self$options$bincolor` | Output item / Table |
| `binalpha` | UI Control `binalpha` | `self$options$binalpha` | Output item / Table |
| `centralitylinecolor` | UI Control `centralitylinecolor` | `self$options$centralitylinecolor` | Output item / Table |
| `centralitylinewidth` | UI Control `centralitylinewidth` | `self$options$centralitylinewidth` | Output item / Table |
| `centralitylinetype` | UI Control `centralitylinetype` | `self$options$centralitylinetype` | Output item / Table |
| `plotwidth` | UI Control `plotwidth` | `self$options$plotwidth` | Output item / Table |
| `plotheight` | UI Control `plotheight` | `self$options$plotheight` | Output item / Table |
| `addGGPubrPlot` | UI Control `addGGPubrPlot` | `self$options$addGGPubrPlot` | Output item / Table |
| `ggpubrPalette` | UI Control `ggpubrPalette` | `self$options$ggpubrPalette` | Output item / Table |
| `ggpubrAddDensity` | UI Control `ggpubrAddDensity` | `self$options$ggpubrAddDensity` | Output item / Table |
| `ggpubrAddMean` | UI Control `ggpubrAddMean` | `self$options$ggpubrAddMean` | Output item / Table |
| `addDistributionDiagnostics` | UI Control `addDistributionDiagnostics` | `self$options$addDistributionDiagnostics` | Output item / Table |
| `ggpubrDensityColor` | UI Control `ggpubrDensityColor` | `self$options$ggpubrDensityColor` | Output item / Table |
| `ggpubrShowQQ` | UI Control `ggpubrShowQQ` | `self$options$ggpubrShowQQ` | Output item / Table |
| `ggpubrShowECDF` | UI Control `ggpubrShowECDF` | `self$options$ggpubrShowECDF` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjhistostats.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

