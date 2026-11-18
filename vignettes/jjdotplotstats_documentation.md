# Horizontal Box-Violin Comparison - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjdotplotstats`
- **Module**: `JJStatsPlotT` (`v1.0.82`)

| Feature / Option | UI Binding (`.u.yaml`) | Backend Handler (`R/jjdotplotstats.b.R`) | Target Result Item(s) (`.r.yaml`) |
| :--- | :--- | :--- | :--- |
| `data` | Implicit dataset frame | `self$data` in `.prepareData()`, `.validateInputs()` | `todo`, `notices`, `explanation`, `plot`, `plot2` |
| `dep` | `VariablesListBox` `dep` | `jmvcore::toNumeric()`, finite filter in `.prepareData()` | `todo`, `notices`, `explanation`, `plot`, `plot2` |
| `group` | `VariablesListBox` `group` | `.cleanFactor()`, `droplevels()` in `.prepareData()` | `todo`, `notices`, `explanation`, `plot`, `plot2` |
| `grvar` | `VariablesListBox` `grvar` | `.prepareSplitData()`, `.validateSplitInputs()`, `.plot2()` | `todo`, `notices`, `plot2` (`visible: (grvar)`) |
| `typestatistics` | `ComboBox` `typestatistics` | `.subtitleExpr()`, `.captionExpr()`, `.renderExplanation()`, `.plot()`, `.plot2()` | `notices`, `explanation`, `plot`, `plot2` |
| `effsizetype` | `ComboBox` `effsizetype` (`enable: (typestatistics:parametric)`) | `.subtitleExpr()`, `.renderExplanation()`, `.run()` notices | `notices`, `explanation`, `plot` |
| `pairwisedisplay` | `ComboBox` `pairwisedisplay` | `.prepareOptions()`, `.fixParametricPairwise()`, `.renderExplanation()` | `notices`, `explanation`, `plot`, `plot2` |
| `padjustmethod` | `ComboBox` `padjustmethod` (`enable: (!pairwisedisplay:none && !typestatistics:bayes)`) | `.prepareOptions()`, `.fixParametricPairwise()`, `.run()` notices | `notices`, `explanation`, `plot`, `plot2` |
| `conflevel` | `TextBox` `conflevel` | `.subtitleExpr()`, `.kruskalBcaSubtitle()`, `.bayesAnovaSubtitle()`, `.plot()`, `.plot2()` | `explanation`, `plot`, `plot2` |
| `bfmessage` | `CheckBox` `bfmessage` (`enable: (resultssubtitle && typestatistics:parametric)`) | `.captionExpr()`, `.run()` notices, `.plot()`, `.plot2()` | `notices`, `plot`, `plot2` |
| `k` | `TextBox` `k` | `.fmtStatVal()`, `options_list$digits` in `.prepareOptions()` | `explanation`, `plot`, `plot2` |
| `testvalueline` | `CheckBox` `testvalueline` | `geom_hline()` gate in `.prepareOptions()` | `plot`, `plot2` |
| `testvalue` | `TextBox` `testvalue` (`enable: (testvalueline)`) | `geom_hline(yintercept = testvalue)` in `.prepareOptions()` | `plot`, `plot2` |
| `centralityplotting` | `CheckBox` `centralityplotting` | `centrality_plotting` in `.prepareOptions()`, `.validateCentralityOptions()` | `notices`, `plot`, `plot2` |
| `centralityparameter` | `ComboBox` `centralityparameter` (`enable: (centralityplotting)`) | Legacy suppression/disagreement check in `.prepareOptions()`, `.validateCentralityOptions()` | `notices`, `plot`, `plot2` |
| `centralitytype` | `ComboBox` `centralitytype` (`enable: (centralityplotting)`) | `centrality_type` in `.prepareOptions()`, `.validateCentralityOptions()` | `notices`, `plot`, `plot2` |
| `centralityk` | Hidden (`hidden: true`, deprecated) | Deprecation `INFO` notice in `.validateCentralityOptions()` | `notices` |
| `resultssubtitle` | `CheckBox` `resultssubtitle` | `.subtitleExpr()`, `.captionExpr()`, `.enhanceSplitPanels()`, `.plot()`, `.plot2()` | `notices`, `plot`, `plot2` |
| `showexplanation` | `CheckBox` `showexplanation` | `.renderExplanation()`, `.plainResultSentence()` | `explanation` (`visible: (showexplanation)`) |
| `originaltheme` | `CheckBox` `originaltheme` | `ggstatsplot::theme_ggstatsplot()` switch in `.plot()`, `.plot2()` | `plot`, `plot2` |
| `colorpalette` | `ComboBox` `colorpalette` | `.paletteColors()`, `.applyPalette()` in `.plot()`, `.plot2()` | `plot`, `plot2` |
| `mytitle` | `TextBox` `mytitle` | `options_list$mytitle` (`title` / `annotation.args`) | `plot`, `plot2` |
| `xtitle` | `TextBox` `xtitle` | `ylab <- xtitle` (matches `coord_flip()` horizontal values axis) | `plot`, `plot2` |
| `ytitle` | `TextBox` `ytitle` | `xlab <- ytitle` (matches `coord_flip()` vertical groups axis) | `plot`, `plot2` |
| `plotwidth` | `TextBox` `plotwidth` | `setSize()` in `.init()` (`min(L * plotwidth, 3000)` on `plot2`) | `plot`, `plot2` |
| `plotheight` | `TextBox` `plotheight` | `setSize()` in `.init()` | `plot`, `plot2` |
| `seed` | `TextBox` `seed` | `private$.seed()`, `withr::with_seed()`, `addSeedCaption()` | `explanation`, `plot`, `plot2` |

## Verification Checklist

- [x] All 26 options defined in `jamovi/jjdotplotstats.a.yaml` have matching UI bindings in `jamovi/jjdotplotstats.u.yaml` (except implicit `data` and `hidden: true` legacy `centralityk`) and `clearWith` entries in `jamovi/jjdotplotstats.r.yaml`.
- [x] Backend `R/jjdotplotstats.b.R` wires all declared options without dead code or hardcoded placeholders.
- [x] Results definitions in `jamovi/jjdotplotstats.r.yaml` (`todo`, `notices`, `explanation`, `plot2`, `plot`) correspond to populated output items with serialized `image$state` guards.
