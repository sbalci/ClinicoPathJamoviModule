# Box-Violin Plots to Compare Between Groups - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjbetweenstats`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding (`.u.yaml`) | Backend Handler (`R/jjbetweenstats.b.R`) | Target Result Item (`.r.yaml`) |
| :--- | :--- | :--- | :--- |
| `data` | Implicit dataset | `self$data` (column-scoped hash in `.prepareData()`) | All outputs |
| `dep` | `VariablesListBox` (`dep`) | `self$options$dep`, `.subsetVar()`, `.subsetStratifiedVar()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2`, `diagnostics`, `clinicalSummary`, `mecGuidance`, `summary`, `report` |
| `group` | `VariablesListBox` (`group`) | `self$options$group`, `.cleanFactor()` | All active outputs |
| `grvar` | `VariablesListBox` (`grvar`) | `self$options$grvar`, `.subsetStratifiedVar()`, `.fixGroupedParametricPairwise()` | `plot2`, `ggpubrPlot2`, `diagnostics`, `summary`, `report` |
| `centralityplotting` | `CheckBox` (`centralityplotting`) | `self$options$centralityplotting` | `plot`, `plot2` |
| `centralitytype` | `ComboBox` (`centralitytype`) | `self$options$centralitytype` | `plot`, `plot2` |
| `typestatistics` | `ComboBox` (`typestatistics`) | `self$options$typestatistics`, `.computeStatsResult()`, `.subtitleExpr()`, `.ggpubrStatLayer()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2`, `diagnostics`, `clinicalSummary`, `mecGuidance`, `summary`, `assumptions`, `interpretation`, `report` |
| `pairwisecomparisons` | `CheckBox` (`pairwisecomparisons`) | `self$options$pairwisecomparisons`, `.fixParametricPairwise()`, `.fixGroupedParametricPairwise()` | `plot`, `plot2`, `diagnostics`, `summary`, `report` |
| `pairwisedisplay` | `ComboBox` (`pairwisedisplay`) | `.pairwiseDisplay()` | `plot`, `plot2` |
| `padjustmethod` | `ComboBox` (`padjustmethod`) | `self$options$padjustmethod`, `.fixParametricPairwise()`, `.fixGroupedParametricPairwise()` | `plot`, `plot2`, `diagnostics`, `summary`, `report` |
| `effsizetype` | `ComboBox` (`effsizetype`) | `self$options$effsizetype`, `.computeStatsResult()` | `plot`, `plot2`, `diagnostics`, `summary`, `report` |
| `mytitle` | `TextBox` (`mytitle`) | `self$options$mytitle`, `.processTitle()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` |
| `xtitle` | `TextBox` (`xtitle`) | `self$options$xtitle`, `.processTitle()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` |
| `ytitle` | `TextBox` (`ytitle`) | `self$options$ytitle`, `.processTitle()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` |
| `originaltheme` | `CheckBox` (`originaltheme`) | `.applyTheme()` | `plot`, `plot2` |
| `resultssubtitle` | `CheckBox` (`resultssubtitle`) | `.computeStatsResult()`, `.subtitleExpr()`, `.formatAutoFilledReportLine()` | `plot`, `plot2`, `diagnostics`, `clinicalSummary`, `report` |
| `bfmessage` | `CheckBox` (`bfmessage`) | `.bfCaptionExpr()` | `plot`, `plot2`, `diagnostics` |
| `k` | `TextBox` (`k`) | `self$options$k`, `.fmtStatVal()` | `plot`, `plot2`, `report` |
| `conflevel` | `TextBox` (`conflevel`) | `self$options$conflevel` | `plot`, `plot2`, `summary`, `report` |
| `varequal` | `CheckBox` (`varequal`) | `self$options$varequal`, `.computeStatsResult()`, `.ggpubrStatLayer()`, `.syncValidityNotice()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2`, `diagnostics`, `clinicalSummary`, `summary`, `assumptions`, `report` |
| `multiEndpointCorrection` | `ComboBox` (`multiEndpointCorrection`) | `.endpointGuidanceHtml()` | `mecGuidance` |
| `plotwidth` | `TextBox` (`plotwidth`) | `.init()` `setSize()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` |
| `plotheight` | `TextBox` (`plotheight`) | `.init()` `setSize()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` |
| `colorblindSafe` | `CheckBox` (`colorblindSafe`) | `.applyTheme()`, `.ggpubrPaletteFor()`, `.plot2()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` |
| `showexplanations` | `CheckBox` (`showexplanations`) | `.run()` explanation generators | `about`, `summary`, `assumptions`, `interpretation`, `report` |
| `addGGPubrPlot` | `CheckBox` (`addGGPubrPlot`) | `.plotGGPubr()`, `.plotGGPubr2()` | `ggpubrPlot`, `ggpubrPlot2` |
| `ggpubrPlotType` | `ComboBox` (`ggpubrPlotType`) | `.buildOneGGPubrPanel()` | `ggpubrPlot`, `ggpubrPlot2` |
| `ggpubrPalette` | `ComboBox` (`ggpubrPalette`) | `.ggpubrPaletteFor()` | `ggpubrPlot`, `ggpubrPlot2` |
| `ggpubrAddStats` | `CheckBox` (`ggpubrAddStats`) | `.ggpubrStatLayer()` | `ggpubrPlot`, `ggpubrPlot2` |
| `ggpubrAddPoints` | `CheckBox` (`ggpubrAddPoints`) | `.buildOneGGPubrPanel()` | `ggpubrPlot`, `ggpubrPlot2` |
| `seed` | `TextBox` (`seed`) | `.safeSeed()`, `withr::local_seed()` | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2`, `report` |

## Verification Checklist

- [x] All 31 options defined in `.a.yaml` have matching UI bindings in `.u.yaml` (except implicit `data`) and full `R:`/`ui:` descriptions.
- [x] All `Bool` options in `.a.yaml` default to `false`.
- [x] Backend `R/jjbetweenstats.b.R` references all declared options safely, scopes data hashing, memoizes `statsExpressions` results, and escapes variable names.
- [x] All 13 results definitions in `.r.yaml` correspond to populated output containers.
