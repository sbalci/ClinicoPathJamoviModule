# decisioncompare: option-to-result mapping

The analysis accepts paired categorical diagnostic tests against a reference standard.
`data` is supplied by jamovi; every other option has a named UI control. Level options
are required arguments of the R wrapper, including optional-test levels passed as `NULL`.

| Option | UI control | Backend / calculation | Result |
|---|---|---|---|
| `data` | Dataset supplied by jamovi | self$data → .prepareData() | All data-dependent outputs |
| `gold` | VariablesListBox | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `goldPositive` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `goldNegative` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test1` | VariablesListBox | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test1Positive` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test1Negative` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test2` | VariablesListBox | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test2Positive` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test2Negative` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test3` | VariablesListBox | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test3Positive` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `test3Negative` | LevelSelector | .validateInputs() / .prepareData() → per-test coding and paired alignment | cTable1–3, epirTable1–3, accuracy/comparison tables, plots and prose |
| `pp` | CheckBox | .calculateDiagnosticMetrics() → prevalence adjustment | comparisonTable PPV/NPV, plots and descriptive prose |
| `pprob` | TextBox | .calculateDiagnosticMetrics() → Bayes predictive values | comparisonTable PPV/NPV, plots and descriptive prose |
| `od` | CheckBox | .displayOriginalData() → original categorical frequencies, subject to display-size limit | text1, text2, notices |
| `fnote` | CheckBox | .run() → educational definitions | cTable1–3 and comparisonTable notes |
| `ci` | CheckBox | .processSingleTest() → epiR exact intervals | epirTable1–3 |
| `plot` | CheckBox | .setupVisualizations() → stored summaries → .plot1() | plot1 |
| `excludeIndeterminate` | CheckBox | .prepareData() / .processSingleTest() → explicit negative-level exclusions | All data-dependent outputs and denominator notices |
| `radarplot` | CheckBox | .setupVisualizations() → stored summaries → .plotRadar() | plotRadar |
| `heatmap` | CheckBox | .buildHeatmapData() → stored correctness → .plotHeatmap() | plotHeatmap |
| `opa` | CheckBox | .populateOPATable() → per-test concordance | opaTable |
| `niMargin` | TextBox | OPA lower-bound comparison with descriptive threshold | opaTable criterion cells |
| `useOpaCriterion` | CheckBox | Enable descriptive OPA threshold assessment | opaTable criterion cells |
| `ciMethod` | ComboBox | Wilson / logit / exact OPA interval | opaTable |
| `stratify` | VariablesListBox | Subgroup-specific available-case calculations | stratifiedTable |
| `statComp` | CheckBox | .performStatisticalComparisons() / .calculateDifferences() | mcnemarTable, diffTable, optional inferential prose |
| `mcnemarMethod` | ComboBox | .mcnemarTest() → mid-p / uncorrected asymptotic tail | mcnemarTable, diffTable sensitivity/specificity p, prose |
| `showSummary` | CheckBox | .generateSummary() | summaryReport |
| `showExplanations` | CheckBox | .generateExplanations() | explanationsContent, aboutAnalysis |
| `showReportSentence` | CheckBox | .generateReportSentence() | reportSentence |
| `showDescriptiveReport` | CheckBox | Methods/results templates and descriptive ranking | clinicalReport |

No unused options were found. Some options operate through validation, visibility or plotting
rather than changing estimates. The `statComp` option controls inferential outputs; the
selected `mcnemarMethod` affects overall and class-specific paired comparisons, not Cochran's Q.

The optional original-data display drops unused factor levels. If any test-by-reference
cross-tabulation would exceed 10,000 cells, the original-data display is omitted with a
warning. Diagnostic estimates, comparisons, plots and reports still run normally.

Automatic behaviors without separate controls are intentional: available-case denominators,
Holm adjustment over informative overall-accuracy pairs, unadjusted class-specific comparisons,
Newcombe method-10 difference intervals, denominator-specific likelihood-ratio correction,
and exact conditional Cochran's Q for at most 30 informative cases. Their assumptions and
limits are described in the [developer documentation](decisioncompare-documentation.md).
