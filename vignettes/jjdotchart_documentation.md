# Dot Chart (Summary vs Reference Value) - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjdotchart`
- **Module**: `JJStatsPlot`

| Feature / Option | UI Binding (`.u.yaml`) | Backend Handler (`R/jjdotchart.b.R`) | Target Result Item (`.r.yaml`) |
| :--- | :--- | :--- | :--- |
| `data` | Implicit dataset | `self$data` in `.prepareData()`, `.run()` | `todo`, `notices`, `summary`, `plot`, `plot2` |
| `dep` | `VariablesListBox` (`dep`) | `.prepareData()`, `.groupTableFor()`, `.buildDotPanel()` | `todo`, `notices`, `summary`, `plot`, `plot2` |
| `group` | `VariablesListBox` (`group`) | `.cleanFactor()`, `.prepareData()`, `.groupTableFor()`, `.buildDotPanel()` | `todo`, `notices`, `summary`, `plot`, `plot2` |
| `grvar` | `VariablesListBox` (`grvar`) | `.prepareData()`, `.syncTableTitles()`, `.validateSplitBy()`, `.buildSplitPlot()` | `notices`, `summary` (title/note), `plot2` |
| `testvalue` | `TextBox` (`testvalue`) | `.oneSampleStats()`, `.validate()`, `.fillTable()`, `.buildDotPanel()` | `notices`, `summary` (`vsref`), `plot`, `plot2` |
| `typestatistics` | `ComboBox` (`typestatistics`) | `.summaryLabel*()`, `.groupSummaryOne()`, `.oneSampleStats()`, `.validate()` | `notices`, `summary`, `plot`, `plot2` |
| `conflevel` | `TextBox` (`conflevel`) | `.groupSummaryOne()`, `.oneSampleStats()`, `.fillTable()` | `summary` (`ci_low`, `ci_high`, note), `plot`, `plot2` |
| `k` | `TextBox` (`k`) | `.oneSampleStats()`, `.centralityStat()` | `plot`, `plot2` |
| `resultssubtitle` | `CheckBox` (`resultssubtitle`) | `.run()`, `.validateSplitBy()`, `.buildDotPanel()` | `notices`, `plot`, `plot2` |
| `showSummaryTable` | `CheckBox` (`showSummaryTable`) | Declarative visibility (`visible: (showSummaryTable)`) | `summary` |
| `centralityplotting` | `CheckBox` (`centralityplotting`) | `.captionText()`, `.buildDotPanel()` | `plot`, `plot2` |
| `centralitytype` | `ComboBox` (`centralitytype`) | `.centralityStat()` | `plot`, `plot2` |
| `originaltheme` | `CheckBox` (`originaltheme`) | `.buildDotPanel()` | `plot`, `plot2` |
| `mytitle` | `TextBox` (`mytitle`) | `.buildMainPlot()`, `.buildSplitPlot()` | `plot`, `plot2` |
| `xtitle` | `TextBox` (`xtitle`) | `.buildDotPanel()` | `plot`, `plot2` |
| `ytitle` | `TextBox` (`ytitle`) | `.buildDotPanel()` | `plot`, `plot2` |
| `plotwidth` | `TextBox` (`plotwidth`) | `.init()` (`setSize`) | `plot`, `plot2` |
| `plotheight` | `TextBox` (`plotheight`) | `.init()` (`setSize`) | `plot`, `plot2` |
| `seed` | `TextBox` (`seed`) | `.groupSeed()`, `.oneSampleStats()`, `.fillTable()`, `addSeedCaption()` | `summary` (note), `plot`, `plot2` |

## Verification Checklist

- [x] All 19 options defined in `.a.yaml` (including `seed`) have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjdotchart.b.R` references all declared options safely and updates dynamic column and table titles.
- [x] Results definitions in `.r.yaml` correspond to populated output containers and clear stale rows on invalid state transitions.
