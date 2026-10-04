# decisioncompare: option-to-result mapping

The analysis accepts paired categorical diagnostic tests against a reference standard.
`data` is supplied by jamovi; every other option has a named UI control. Level options
are required arguments of the R wrapper, including optional-test levels passed as `NULL`.

| Option | UI control | Backend / calculation | Result |
|---|---|---|---|
| `data` | Dataset supplied by jamovi | self$data → .prepareData() | All data-dependent outputs |
| `referenceType` | ComboBox | .diagReferenceMode() → disease path (`legacy`, `disease`) or `.runComparator()` (`consensus`, `method`) | referenceDefinition; disease-path or comparator* outputs |
| `referenceDescription` | TextBox | .diagReferenceDefinition() → displayed only | referenceDefinition |
| `targetDefinition` | TextBox | .diagReferenceDefinition() → displayed only | referenceDefinition |
| `gold` | VariablesListBox | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `goldPositive` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `goldNegative` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test1` | VariablesListBox | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test1Positive` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test1Negative` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test2` | VariablesListBox | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test2Positive` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test2Negative` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test3` | VariablesListBox | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test3Positive` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `test3Negative` | LevelSelector | .validateMappings() → .mapBinary() explicit positive/negative coding | levelMapping, referenceDefinition, every disease-path and comparator output |
| `pp` | CheckBox | .calculateDiagnosticMetrics() → prevalence adjustment (disease path only; ignored in consensus/method) | comparisonTable PPV/NPV, plots and descriptive prose |
| `pprob` | TextBox | .calculateDiagnosticMetrics() → Bayes predictive values (bounds 1e-9 to 1 − 1e-9) | comparisonTable PPV/NPV, plots and descriptive prose |
| `od` | CheckBox | .displayOriginalData() → original categorical frequencies, subject to display-size limit | text1, text2, notices |
| `fnote` | CheckBox | .run() → educational definitions | cTable1–3 and comparisonTable notes |
| `ci` | CheckBox | .processSingleTest() → epiR exact intervals; comparator path → exact binomial / Newcombe intervals | epirTable1–3; comparatorMetrics exact CIs; comparatorDifferences Newcombe CIs |
| `plot` | CheckBox | .setupVisualizations() → stored summaries → .plot1() | plot1 |
| `excludeIndeterminate` | CheckBox | .diagComparatorBinary() → levels outside the selected pair are excluded (TRUE) or stop the analysis (FALSE); never pooled as negative | levelMapping counts, every estimate, exclusion notices |
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
| `showValidationContext` | CheckBox | .init() → .dvProvenance() → shows/fills the provenance table | validationContext |
| `cohortRole` | TextBox | .dvProvenance() → displayed only, not validated | validationContext |
| `samplingDesign` | TextBox | .dvProvenance() → displayed only, not validated | validationContext |
| `thresholdSource` | TextBox | .dvProvenance() → displayed only, not validated | validationContext |
| `modelVersion` | TextBox | .dvProvenance() → displayed only, not validated | validationContext |
| `analysisUnit` | TextBox | .dvProvenance() → displayed only, not validated | validationContext |

Every selected variable needs distinct, explicit positive and negative levels (analysis 1.1.0);
a binary variable is no exception and nothing is inferred. `consensus` and `method` report agreement
(PPA/NPA/OPA, paired McNemar with Newcombe intervals and a Holm family over every pair x metric)
through `comparatorCounts`, `comparatorPopulation`, `comparatorMetrics`, `comparatorReport`,
`comparatorDifferences`, `comparatorStrata` and the three comparator plots; disease-only options
(`pp`, `od`, `fnote`, `opa`, narrative panels) are disabled there.

The six validation-provenance options (`showValidationContext`, default FALSE; `cohortRole`,
`samplingDesign`, `thresholdSource`, `modelVersion`, `analysisUnit`, String, default `''`) are
user-declared context. `validationContext` echoes them, with `referenceType` and
`referenceDescription`, only when `showValidationContext` is ticked; nothing in them is
checked and no estimate depends on them.

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
