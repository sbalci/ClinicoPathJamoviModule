# Enhanced ROC Analysis (timeroc) - Feature Mapping Table

Complete feature-to-code mapping for the `timeroc` jamovi function. Every feature is traced from YAML option through UI label, result item, and backend method.

**Module:** jsurvival (menuGroup: SurvivalT, menuSubgroup: ClinicoPath Survival)
**Version:** 1.1.0
**Packages:** timeROC, pROC, ggplot2, glue, scales

---

## 1. Input Variables

| Feature | a.yaml Option | Type | UI Label (u.yaml) | r.yaml Result | b.R Method |
|---|---|---|---|---|---|
| Follow-up time | `elapsedtime` | Variable (continuous) | Time Elapsed | (used in timedep mode) | `.cleanData()` -> `data$time` |
| Outcome variable | `outcome` | Variable (ordinal/nominal/continuous) | Outcome | (used in all) | `.cleanData()` -> `data$status` |
| Event level | `outcomeLevel` | Level (of outcome) | Event Level | (used in all) | `.cleanData()` -> ifelse == level |
| Primary marker | `marker` | Variable (continuous) | Marker Variable | (used in all) | `.cleanData()` -> `data$marker` |
| Input data | `data` | Data | (implicit) | (all) | `.cleanData()` |

---

## 2. Analysis Configuration

| Feature | a.yaml Option | Type / Default | UI Label (u.yaml) | r.yaml Result | b.R Method |
|---|---|---|---|---|---|
| Analysis type dispatch | `analysisType` | List / `timedep` | Analysis Type (ComboBox) | controls `visible:` on all items | `.run()` dispatches `.runTimeROC()` or `.runBinaryROC()` |
| Evaluation timepoints | `timepoints` | String / `"12, 36, 60"` | Evaluation Timepoints (TextBox) | `aucTable`, `cutoffTable` | `.parseTimepoints()` |
| IPCW weighting method | `method` | List / `marginal` | IPCW Weighting Method (ComboBox) | `aucTable`, `rocPlot`, `aucPlot` | `.runTimeROC()` -> `timeROC::timeROC(weighting=...)` |
| Confidence intervals | `bootstrapCI` | Bool / `false` | Confidence Intervals (Asymptotic) (CheckBox) | `aucTable` SE/CI columns, `aucPlot` ribbons | `.runTimeROC()` -> `timeROC::timeROC(iid=TRUE)` |
| Bootstrap samples (deprecated) | `nboot` | Integer / 100 | (hidden) | - | Retained for backward compat only; not used |
| Time unit display | `timetypeoutput` | List / `months` | Time Units (ComboBox) | axis labels on `rocPlot`, `aucPlot`, interpretation text | `.createInterpretation()`, `.plotAUC()`, `.plotROC()` |

**Legacy method mapping:** Values `incident`, `cumulative`, `static` from older saved analyses are silently remapped to `marginal` in `.runTimeROC()`.

---

## 3. Time-Dependent Results

| Feature | a.yaml Trigger | r.yaml Result | Type | Visible When | b.R Method |
|---|---|---|---|---|---|
| AUC table | (always in timedep) | `aucTable` | Table (timepoint, auc, se, ci_lower, ci_upper) | `analysisType:timedep` | `.runTimeROC()` |
| ROC curves plot | `plotROC` | `rocPlot` | Image 600x450 | `plotROC && analysisType:timedep` | `.plotROC()` (base graphics) |
| AUC over time plot | `plotAUC` | `aucPlot` | Image 600x450 | `plotAUC && analysisType:timedep` | `.plotAUC()` (ggplot2) |
| Marker statistics | `showMarkerStats` | `markerStats` | Table (statistic, value) | `showMarkerStats && analysisType:timedep` | `.calculateMarkerStats()` |
| Optimal cutoff values | `showOptimalCutoff` | `cutoffTable` | Table (timepoint, cutoff, sensitivity, specificity, youden) | `showOptimalCutoff && analysisType:timedep` | `.calculateOptimalCutoffs()` |
| Model comparison | `compareBaseline` | `modelComparison` | Html | `compareBaseline && analysisType:timedep` | `.compareToBaseline()` |
| Clinical interpretation | (always in timedep) | `clinicalInterpretation` | Html | `analysisType:timedep` | `.createClinicalInterpretation()` |
| Results text | (always) | `text` | Html | (always) | `.createInterpretation()` |

---

## 4. Binary ROC Results

| Feature | a.yaml Trigger | r.yaml Result | Type | Visible When | b.R Method |
|---|---|---|---|---|---|
| Binary ROC table | (always in binary) | `binaryROCTable` | Table (marker, auc, se, ci_lower, ci_upper, sensitivity, specificity, optimal_cutoff) | `analysisType:binary` | `.runBinaryROC()` |
| ROC comparison table | `compareROCs` | `rocComparisonTable` | Table (comparison, method, test_statistic, p_value, interpretation) | `compareROCs && analysisType:binary` | `.runROCComparison()` |
| Binary ROC plot | `plotROC` | `binaryROCPlot` | Image 600x450 | `analysisType:binary && plotROC` | `.plotBinaryROC()` |
| Diagnostic performance | (always in binary) | `diagnosticPerformance` | Html | `analysisType:binary` | `.generateDiagnosticSummary()` |

---

## 5. Display Options

| Feature | a.yaml Option | Type / Default | UI Label (u.yaml) | Effect on Output | b.R Method |
|---|---|---|---|---|---|
| Plot ROC curves | `plotROC` | Bool / `true` | Plot ROC Curves (CheckBox) | Controls `rocPlot` (timedep) and `binaryROCPlot` (binary) visibility | `.plotROC()`, `.plotBinaryROC()` |
| Plot AUC over time | `plotAUC` | Bool / `true` | Plot AUC Over Time (CheckBox) | Controls `aucPlot` visibility (timedep only) | `.plotAUC()` |
| Smooth AUC curve | `smoothAUC` | Bool / `false` | Smooth AUC Curve (CheckBox, enabled when plotAUC) | Adds loess smoothing to AUC plot (requires >2 timepoints) | `.plotAUC()` -> `geom_smooth(method="loess")` |
| Show optimal cutoff | `showOptimalCutoff` | Bool / `true` | Show Optimal Cutoff Values (CheckBox) | Controls `cutoffTable` visibility | `.calculateOptimalCutoffs()` |
| Show marker stats | `showMarkerStats` | Bool / `true` | Show Marker Statistics (CheckBox) | Controls `markerStats` table visibility | `.calculateMarkerStats()` |
| Compare to baseline | `compareBaseline` | Bool / `false` | Compare to Baseline Model (CheckBox) | Controls `modelComparison` visibility; requires `bootstrapCI` for p-values | `.compareToBaseline()` |
| Youden index | `youdenIndex` | Bool / `true` | Calculate Youden Index (CheckBox) | When false, optimal cutoff/sens/spec are NA in binary mode | `.runBinaryROC()` |

---

## 6. Comparison Options

| Feature | a.yaml Option | Type / Default | UI Label (u.yaml) | Effect on Output | b.R Method |
|---|---|---|---|---|---|
| Compare ROC curves | `compareROCs` | Bool / `false` | Compare ROC Curves (CheckBox) | Enables multi-marker comparison (binary mode only) | `.runROCComparison()`, `.plotBinaryROC()` |
| Additional markers | `markers` | Variables (continuous, numeric) / `[]` | Multiple Markers (for comparison) (VariablesListBox) | Additional markers to compare against primary | `.runROCComparison()` -> `pROC::roc.test()` |
| Comparison method | `rocComparison` | List / `delong` | ROC Comparison Method (ComboBox) | Statistical test: delong, bootstrap (n=1000), or venkatraman | `.runROCComparison()` -> `pROC::roc.test(method=...)` |

---

## 7. Notices & Interpretation

| Notice Type | Trigger Condition | Title | b.R Location |
|---|---|---|---|
| ERROR | No events in outcome (all status=0) | No Events Found | `.cleanData()` |
| ERROR | Analysis failure (any uncaught error) | Analysis Failed / Binary ROC Analysis Failed | `.runTimeROC()`, `.runBinaryROC()` |
| STRONG_WARNING | AUC < 0.5 at any timepoint / overall | AUC Below 0.5 (Reversed Marker?) | `.runTimeROC()`, `.runBinaryROC()` |
| WARNING | Few unique marker values (<5) | Few Unique Marker Values | `.cleanData()` |
| WARNING | Invalid timepoints string | Invalid Timepoints | `.parseTimepoints()` |
| WARNING | All timepoints exceed max follow-up | Timepoints Adjusted | `.parseTimepoints()` |
| WARNING | Timepoints exceed max follow-up (some) | Timepoint Warning | `.runTimeROC()` |
| WARNING | <5 events at a timepoint | Few Events at Timepoint | `.runTimeROC()` |
| WARNING | AUC mean < 0.70 | Limited Clinical Utility | `.runTimeROC()`, `.runBinaryROC()` |
| WARNING | Cutoff calculation fails | Cutoff Calculation Failed | `.calculateOptimalCutoffs()` |
| WARNING | <10 complete cases for comparison marker | Insufficient Data | `.runROCComparison()` |
| WARNING | ROC comparison fails for a marker | Comparison Failed | `.runROCComparison()` |
| INFO | Analysis completes successfully | Analysis Complete | `.runTimeROC()`, `.runBinaryROC()` |

**Notice rendering:** HTML-based with severity-graded styling (color, border, background). Rendered by `.renderNotices()` into `self$results$notices`.

---

## Complete Option Index (21 options)

| # | Option Name | Type | Default | Mode |
|---|---|---|---|---|
| 1 | `data` | Data | - | both |
| 2 | `elapsedtime` | Variable | null | timedep |
| 3 | `outcome` | Variable | null | both |
| 4 | `outcomeLevel` | Level | (auto) | both |
| 5 | `marker` | Variable | null | both |
| 6 | `timepoints` | String | "12, 36, 60" | timedep |
| 7 | `method` | List | marginal | timedep |
| 8 | `bootstrapCI` | Bool | false | timedep |
| 9 | `nboot` | Integer | 100 | (deprecated/hidden) |
| 10 | `plotROC` | Bool | true | both |
| 11 | `plotAUC` | Bool | true | timedep |
| 12 | `timetypeoutput` | List | months | timedep |
| 13 | `showOptimalCutoff` | Bool | true | timedep |
| 14 | `showMarkerStats` | Bool | true | timedep |
| 15 | `compareBaseline` | Bool | false | timedep |
| 16 | `smoothAUC` | Bool | false | timedep |
| 17 | `analysisType` | List | timedep | both |
| 18 | `compareROCs` | Bool | false | binary |
| 19 | `markers` | Variables | [] | binary |
| 20 | `rocComparison` | List | delong | binary |
| 21 | `youdenIndex` | Bool | true | binary |
