# Histogram (`jjhistostats`) Feature Mapping

> For architecture, Mermaid data-flow diagrams, validation rules, and execution sequence, see [Developer Documentation](jjhistostats-documentation.md). For QA scenarios, see [Testing Checklist](testing_jjhistostats.md).

This document maps every feature and option of `jjhistostats` (`version: 1.1.7`, `JJStatsPlot` / `JJStatsPlotT`) from its `.a.yaml` schema and `.u.yaml` UI control to its target `.r.yaml` output and `R/jjhistostats.b.R` handler.

## Feature Summary

`jjhistostats` provides single-variable, multi-variable, and grouped histogram analysis tailored to clinicopathological research. Features fall into six functional areas:

1. **Variables & Clinical Presets**: Continuous numeric outcome selection (`dep`), optional factor stratification (`grvar`), and four clinical presets (`lab_values`, `biomarkers`, `patient_chars`, `pathology_scores`) synchronized between client JavaScript and the R backend.
2. **One-Sample Location Inference**: Parametric Student *t* (exact Hedges' *g* + noncentral-*t* CI), nonparametric Wilcoxon signed-rank (rank-biserial *r*), robust bootstrap-*t* on trimmed means (two-sided via `WRS2`, one-sided Phipson–Smyth via `.buildOneSidedRobustExpr`), and Bayesian one-sample *t*-test (overflow-safe log-scale BF and unconstrained posterior via `.buildDirectionalBayesExpr`), with deterministic `seed` control.
3. **Histogram & Centrality Customization**: Manual/automatic bin widths (guarded at `max_bins = 5000`), bar fill/border/alpha aesthetics, vertical centrality line measure and styling, custom labels, dynamic sizing, and jamovi vs original `ggstatsplot` theming.
4. **Publication-Ready `ggpubr` & Diagnostics**: Optional density-aligned `ggpubr` histograms (overall and grouped) plus density, Q-Q, and ECDF distribution diagnostic plots.
5. **Clinical Warnings & Interpretation**: Severity-ranked advisories in `todo` (zero-null testing, Shapiro–Wilk non-normality, Wilcoxon normal approximation/pseudomedian/zero ties, robust trim/zero-SE, Bayes prior, missing data, small samples/groups, outliers, suppressed labels, pooled diagnostics) and an opt-in `interpretation` panel.

## Feature Details

| Feature | YAML Argument (`.a.yaml`) | UI Label (`.u.yaml`) | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Input Variables** | | | | |
| Dataset | `data` | — | All outputs | `.prepareData`, `.validateInputs` |
| Continuous variables | `dep` | Variables | All 9 outputs | `.init`, `.validateInputs`, `.prepareData`, `.run`, all `.plot*` |
| Grouping factor | `grvar` | Split by (optional) | `plot2`, `ggpubrPlot2`, `todo` | `.init`, `.validateInputs`, `.generateClinicalWarnings`, `.plot2`, `.plotGGPubr2` |
| **Clinical Presets** | | | | |
| Preset selector | `clinicalPreset` | Clinical preset | `plot`, `plot2`, `todo`, `interpretation` | `.applyClinicalPreset`, `.option` (JS: `onChange_clinicalPreset`) |
| **Statistical Analysis** | | | | |
| Location test family | `typestatistics` | Type of statistic | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Reproducibility seed | `seed` | Random seed | `plot`, `plot2` (caption) | `.plot`, `.plot2`, `.buildOneSidedRobustExpr`, `.buildDirectionalBayesExpr` |
| Statistical subtitle toggle | `resultssubtitle` | Statistical results | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Custom null switch | `enableOneSampleTest` | Custom null value | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateClinicalWarnings` |
| Null comparison value | `test.value` | Test value | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Test direction | `alternative` | Test direction | `plot`, `plot2`, `todo` | `.prepareOptions`, `.buildOneSidedRobustExpr`, `.buildDirectionalBayesExpr` |
| Confidence / credible level | `conf.level` | Confidence level | `plot`, `plot2` | `.validateInputs`, `.prepareOptions`, `.formatConfPct` |
| Parametric Bayes factor caption | `bf.message` | Bayes factor | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Decimal precision | `digits` | Decimal places | `plot`, `plot2` | `.validateInputs`, `.prepareOptions`, `.patchRobustZeroP` |
| Trimmed-mean proportion | `trimlevel` | Trim proportion | `plot`, `plot2`, `todo` | `.validateInputs`, `.prepareOptions`, `.buildOneSidedRobustExpr`, `.generateClinicalWarnings` |
| Cauchy prior scale *r* | `bfprior` | Bayes prior scale | `plot`, `plot2`, `todo` | `.validateInputs`, `.prepareOptions`, `.buildDirectionalBayesExpr`, `.generateClinicalWarnings` |
| Plain-language interpretation | `showInterpretation` | Clinical interpretation | `interpretation` | `.run`, `.generateClinicalInterpretation`, `.formatPWithOp` |
| **Histogram Bins & Appearance** | | | | |
| Manual bin width toggle | `changebinwidth` | Manual bin width | `plot`, `plot2` | `.validateInputs`, `.prepareOptions`, `.generateSingleHistogram` |
| Manual bin width value | `binwidth` | Bin width | `plot`, `plot2` | `.validateInputs`, `.prepareOptions`, `.generateSingleHistogram` |
| Bin fill color | `binfill` | Bin fill color | `plot`, `plot2` | `.prepareAesthetics` (JS: `onChange_binfill`) |
| Bin border color | `bincolor` | Bin border color | `plot`, `plot2` | `.prepareAesthetics` |
| Bin opacity | `binalpha` | Bin transparency | `plot`, `plot2` | `.validateInputs`, `.prepareAesthetics` |
| **Centrality Line** | | | | |
| Centrality line toggle | `centralityline` | Centrality line | `plot`, `plot2` | `.prepareOptions`, `.generateSingleHistogram` |
| Centrality estimator | `centralitytype` | Centrality measure | `plot`, `plot2` | `.prepareOptions`, `.generateSingleHistogram` |
| Centrality line color | `centralitylinecolor` | Centrality line color | `plot`, `plot2` | `.prepareAesthetics` |
| Centrality line width | `centralitylinewidth` | Centrality line width | `plot`, `plot2` | `.validateInputs`, `.prepareAesthetics` |
| Centrality line pattern | `centralitylinetype` | Centrality line type | `plot`, `plot2` | `.prepareAesthetics` |
| **Plot Labels, Size & Theme** | | | | |
| X-axis label | `xlab` | X-axis label | `plot`, `plot2` | `.prepareOptions`, `.generateSingleHistogram` |
| Plot title | `title` | Plot title | `plot`, `plot2` | `.prepareOptions`, `.generateSingleHistogram` |
| Plot subtitle (when stats off) | `subtitle` | Plot subtitle | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Plot caption | `caption` | Plot caption | `plot`, `plot2`, `todo` | `.prepareOptions`, `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Panel width (px) | `plotwidth` | Plot width | `plot`, `plot2`, `ggpubrPlot` | `.init` |
| Panel height (px) | `plotheight` | Plot height | `plot`, `plot2`, `ggpubrPlot`, `ggpubrPlot2` | `.init` |
| Original `ggstatsplot` theme | `originaltheme` | Original ggstatsplot theme | `plot`, `plot2` | `.cleanGgtheme`, `.generateSingleHistogram` |
| **Publication-Ready `ggpubr` Plots** | | | | |
| Enable `ggpubr` histogram | `addGGPubrPlot` | Publication-ready histogram | `ggpubrPlot`, `ggpubrPlot2` | `.init`, `.plotGGPubr`, `.plotGGPubr2` |
| `ggpubr` fill color | `ggpubrPalette` | Fill color | `ggpubrPlot`, `ggpubrPlot2` | `.plotGGPubr`, `.plotGGPubr2` |
| Density curve overlay | `ggpubrAddDensity` | Density curve | `ggpubrPlot`, `ggpubrPlot2` | `.plotGGPubr`, `.plotGGPubr2` (sets `y = "density"`) |
| Mean vertical line | `ggpubrAddMean` | Mean line | `ggpubrPlot`, `ggpubrPlot2` | `.plotGGPubr`, `.plotGGPubr2` |
| **Distribution Diagnostics (`ggpubr`)** | | | | |
| Diagnostics toggle | `addDistributionDiagnostics` | Distribution diagnostics | `densityPlot`, `qqPlot`, `ecdfPlot`, `todo` | `.init`, `.plotDensity`, `.plotQQ`, `.plotECDF`, `.generateClinicalWarnings` |
| Density fill color | `ggpubrDensityColor` | Density plot fill color | `densityPlot`, `qqPlot`, `ecdfPlot` | `.plotDensity`, `.plotQQ`, `.plotECDF` |
| Q-Q normality plot | `ggpubrShowQQ` | QQ plot | `qqPlot` | `.plotQQ` |
| Empirical CDF plot | `ggpubrShowECDF` | ECDF plot | `ecdfPlot` | `.plotECDF` |
| **Derived / Guarded Behaviors (No Direct Option)** | | | | |
| Welcome instructions | — | — | `todo` | `.run` (when `dep` is empty) |
| Severity-ranked advisories | — | — | `todo` | `.generateClinicalWarnings`, `.generatePerformanceWarnings`, `.run` |
| Degenerate subgroup guards (`n < 3`, constant, `trimse <= 0`) | — | — | `plot`, `plot2`, `todo` | `.generateSingleHistogram`, `.generateClinicalWarnings` |
| Reproducible R syntax export | — | — | jamovi Syntax mode | `public$asSource` |

## Changelog

- **2026-11-24**: Replaced generic 2026-08-29 stub with full categorized 40-option + derived-behavior mapping matching `version: 1.1.7` (`seed`, `alternative`, `trimlevel`, `bfprior`, `originaltheme`, and exact `.r.yaml` / `.b.R` handlers); deduplicated overview boilerplate against `jjhistostats-documentation.md`.
