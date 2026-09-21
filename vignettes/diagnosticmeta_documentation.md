# Diagnostic Test Meta-Analysis for Pathology - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `diagnosticmeta`
- **Module**: `OncoPath` (`jamovi/diagnosticmeta.a.yaml` currently declares `menuGroup: OncoPathT`, the JamoviTest routing suffix used while the analysis is under review)
- **Verified against**: `jamovi/diagnosticmeta.a.yaml`, `jamovi/diagnosticmeta.u.yaml`, `jamovi/diagnosticmeta.r.yaml` and `R/diagnosticmeta.b.R` on 2026-09-20

Three things to know before reading the table:

- **`data` is not read as an option.** `self$options$data` appears nowhere in the backend. jamovi hands the dataset over as `self$data`, which is what `R/diagnosticmeta.b.R` uses (lines 290, 325, 1510).
- **The four `show_*` options gate the work, not only the pane.** `show_interpretation`, `show_methodology`, `show_analysis_summary` and `show_plot_explanations` appear twice each: in the `visible:` expressions in `jamovi/diagnosticmeta.r.yaml`, and in the backend, where each one guards the call that builds its panel. Until 2026-09-20 they were visibility-only and the roughly 31 KB of HTML behind them was built on every run and written into every saved `.omv` whether or not anyone could see it.
- **Target Result Item** names the `.r.yaml` item(s) the option actually changes, not merely the item it is listed under in the UI.

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | none (supplied by jamovi) | `self$data` - **not** `self$options$data` | Every result item; it is the source frame for all models and plots |
| `study` | UI Control `study` | `self$options$study` | `individualstudies` (Study column), study labels on `forestplot`; also emitted by `asSource()` |
| `true_positives` | UI Control `true_positives` | `self$options$true_positives` | Every table and plot (one cell of the 2x2 every model is fitted to) |
| `false_positives` | UI Control `false_positives` | `self$options$false_positives` | Every table and plot (one cell of the 2x2 every model is fitted to) |
| `false_negatives` | UI Control `false_negatives` | `self$options$false_negatives` | Every table and plot (one cell of the 2x2 every model is fitted to) |
| `true_negatives` | UI Control `true_negatives` | `self$options$true_negatives` | Every table and plot (one cell of the 2x2 every model is fitted to) |
| `covariate` | UI Control `covariate` | `self$options$covariate` | `metaregression` (also half of that table's `visible:` expression); exclusions disclosed in `notices` |
| `bivariate_analysis` | UI Control `bivariate_analysis` | `self$options$bivariate_analysis` | `bivariateresults`; also `summary` (chooses `.generateSummary()` vs `.generateBasicSummary()`) and the pooled point on `forestplot` / `srocplot` |
| `hsroc_analysis` | UI Control `hsroc_analysis` | `self$options$hsroc_analysis` | `hsrocresults` |
| `meta_regression` | UI Control `meta_regression` | `self$options$meta_regression` | `metaregression`; `notices` when no covariate is set |
| `heterogeneity_analysis` | UI Control `heterogeneity_analysis` | `self$options$heterogeneity_analysis` | `heterogeneity` |
| `publication_bias` | UI Control `publication_bias` | `self$options$publication_bias` | `publicationbias` (row scaffolded in `.init()`); also gates `funnelplot` in both `.run()` and its `visible:` expression |
| `confidence_level` | UI Control `confidence_level` | `self$options$confidence_level` | CI columns of `bivariateresults`, `hsrocresults` and `individualstudies`; confidence/prediction regions on `srocplot`; intervals on `forestplot`; `summary` text |
| `method` | UI Control `method` | `self$options$method` | `bivariateresults`, `hsrocresults`, `heterogeneity`, `metaregression`; method name and fixed-effect caveats in `notices` and table notes |
| `zero_cell_correction` | UI Control `zero_cell_correction` | `self$options$zero_cell_correction` | Every model table and plot (the corrected counts); the correction is disclosed in `notices` |
| `forest_plot` | UI Control `forest_plot` | `self$options$forest_plot` | `forestplot` Image (`renderFun: .forestplot`); also gates `forestplot_explanation` visibility |
| `sroc_plot` | UI Control `sroc_plot` | `self$options$sroc_plot` | `srocplot` Image (`renderFun: .srocplot`); also gates `srocplot_explanation` visibility |
| `funnel_plot` | UI Control `funnel_plot` | `self$options$funnel_plot` | `funnelplot` Image (`renderFun: .funnelplot`); also gates `funnelplot_explanation` visibility |
| `show_individual_studies` | UI Control `show_individual_studies` | `self$options$show_individual_studies` | `individualstudies` |
| `show_interpretation` | UI Control `show_interpretation` | `self$options$show_interpretation` | `interpretation` (Html); `visible: (show_interpretation)` reveals the pane and the same option gates `.populateInterpretation()` in `.init()`, so nothing is built when it is off |
| `show_methodology` | UI Control `show_methodology` | `self$options$show_methodology` | `about` (Html); `visible: (show_methodology)` reveals the pane and the same option gates `.populateAboutPanel()` in `.init()` |
| `show_analysis_summary` | UI Control `show_analysis_summary` | `self$options$show_analysis_summary` | `summary` (Html); `visible: (show_analysis_summary)` reveals the pane and the same option gates `.generateSummary()` / `.generateBasicSummary()` in `.run()`. It also feeds the "Nothing selected to display" notice |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` (resolved by `.getColorPalette()`) | `forestplot`, `srocplot`, `funnelplot` |
| `show_plot_explanations` | UI Control `show_plot_explanations` | `self$options$show_plot_explanations` | `forestplot_explanation`, `srocplot_explanation`, `funnelplot_explanation`; the option reveals the three panes and gates all three `.populate*Explanation()` calls in `.init()` |

## Verification Checklist

- [x] All 23 user-facing options defined in `.a.yaml` have matching UI bindings in `.u.yaml` (`data` has no control, which is correct - jamovi supplies it).
- [x] Every `self$options$<name>` read in `R/diagnosticmeta.b.R` names an option that exists in `.a.yaml`; there are no phantom option reads.
- [x] Every declared option except `data` - 23 of 24 - is read in `R/diagnosticmeta.b.R`. `data` is not, and should not be: jamovi supplies the dataset as `self$data`.
- [x] All 17 result items defined in `.r.yaml` are populated by the backend (the three Images through `image$setState()`, the rest through `setContent()` / `setRow()` / `addRow()`).
