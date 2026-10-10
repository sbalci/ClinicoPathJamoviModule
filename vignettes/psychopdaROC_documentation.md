# Advanced ROC Analysis — Feature Mapping

Analysis 1.0.88, `meddecideT` (JamoviTest). This is a source/schema map, not evidence that every option combination was executed.

The results column includes both top-level and array-template `clearWith` and `visible` dependencies. Runtime computation can affect other results too. `data` is supplied by jamovi and read through `self$data`.

| Option | UI type | First backend read | Declared affected results |
|---|---|---|---|
| `manualRun` | CheckBox | [line 2288](../R/psychopdaROC.b.R#L2288) | Runtime guidance or helper computation; see backend |
| `run` | ActionButton | [line 2288](../R/psychopdaROC.b.R#L2288) | Runtime guidance or helper computation; see backend |
| `showExplanations` | CheckBox | [line 2479](../R/psychopdaROC.b.R#L2479) | `procedureNotes`, `clinicalInterpretationTable` |
| `clinicalMode` | ComboBox | [line 457](../R/psychopdaROC.b.R#L457) | Runtime guidance or helper computation; see backend |
| `data` | Dataset input | [line 2352](../R/psychopdaROC.b.R#L2352) | Runtime guidance or helper computation; see backend |
| `dependentVars` | VariablesListBox | [line 577](../R/psychopdaROC.b.R#L577) | `outcomeCoding`, `clinicalInterpretationTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `fixedSensSpecTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `idiTable`, `nriTable`, `effectSizeTable`, `powerAnalysisTable`, `bayesianROCTable`, `clinicalUtilityTable`, `metaAnalysisWarning`, `metaAnalysisTable`, `decisionCurveTable`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `effectSizePlot`, `powerCurvePlot`, `bayesianTracePlot`, `decisionCurvePlot`, `metaAnalysisForestPlot` |
| `classVar` | VariablesListBox | [line 447](../R/psychopdaROC.b.R#L447) | `outcomeCoding`, `clinicalInterpretationTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `fixedSensSpecTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `idiTable`, `nriTable`, `effectSizeTable`, `powerAnalysisTable`, `bayesianROCTable`, `clinicalUtilityTable`, `metaAnalysisWarning`, `metaAnalysisTable`, `decisionCurveTable`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `effectSizePlot`, `powerCurvePlot`, `bayesianTracePlot`, `decisionCurvePlot`, `metaAnalysisForestPlot` |
| `positiveClass` | LevelSelector | [line 1200](../R/psychopdaROC.b.R#L1200) | `outcomeCoding`, `clinicalInterpretationTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `fixedSensSpecTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `idiTable`, `nriTable`, `effectSizeTable`, `powerAnalysisTable`, `bayesianROCTable`, `clinicalUtilityTable`, `metaAnalysisWarning`, `metaAnalysisTable`, `decisionCurveTable`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `effectSizePlot`, `powerCurvePlot`, `bayesianTracePlot`, `decisionCurvePlot`, `metaAnalysisForestPlot` |
| `negativeClass` | LevelSelector | [line 1020](../R/psychopdaROC.b.R#L1020) | `outcomeCoding`, `clinicalInterpretationTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `fixedSensSpecTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `idiTable`, `nriTable`, `effectSizeTable`, `powerAnalysisTable`, `bayesianROCTable`, `clinicalUtilityTable`, `metaAnalysisWarning`, `metaAnalysisTable`, `decisionCurveTable`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `effectSizePlot`, `powerCurvePlot`, `bayesianTracePlot`, `decisionCurvePlot`, `metaAnalysisForestPlot` |
| `subGroup` | VariablesListBox | [line 2429](../R/psychopdaROC.b.R#L2429) | `outcomeCoding`, `clinicalInterpretationTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `fixedSensSpecTable`, `aucSummaryTable`, `plotROC`, `interactivePlot`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable` |
| `method` | ComboBox | [line 562](../R/psychopdaROC.b.R#L562) | `resultsTable`, `sensSpecTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `metric` | ComboBox | [line 1533](../R/psychopdaROC.b.R#L1533) | `resultsTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `direction` | ComboBox | [line 745](../R/psychopdaROC.b.R#L745) | `outcomeCoding`, `clinicalInterpretationTable`, `resultsTable`, `sensSpecTable`, `thresholdTable`, `fixedSensSpecTable`, `aucSummaryTable`, `delongComparisonTable`, `delongTest`, `plotROC`, `interactivePlot`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `precisionRecallPlot`, `idiTable`, `nriTable`, `effectSizeTable`, `powerAnalysisTable`, `bayesianROCTable`, `clinicalUtilityTable`, `metaAnalysisTable`, `decisionCurveTable`, `partialAUCTable`, `bootstrapCITable`, `rocComparisonTable`, `powerCurvePlot`, `bayesianTracePlot`, `metaAnalysisForestPlot` |
| `specifyCutScore` | TextBox | [line 2556](../R/psychopdaROC.b.R#L2556) | `resultsTable`, `sensSpecTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `tol_metric` | TextBox | [line 1520](../R/psychopdaROC.b.R#L1520) | `resultsTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `break_ties` | ComboBox | [line 1523](../R/psychopdaROC.b.R#L1523) | `resultsTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `allObserved` | CheckBox | [line 2208](../R/psychopdaROC.b.R#L2208) | `resultsTable`, `sensSpecTable` |
| `boot_runs` | TextBox | [line 2464](../R/psychopdaROC.b.R#L2464) | `resultsTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `seed` | TextBox | [line 1472](../R/psychopdaROC.b.R#L1472) | `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `partialAUCTable`, `bootstrapCITable`, `bayesianTracePlot` |
| `usePriorPrev` | CheckBox | [line 928](../R/psychopdaROC.b.R#L928) | `resultsTable`, `thresholdTable`, `fixedSensSpecTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `priorPrev` | TextBox | [line 929](../R/psychopdaROC.b.R#L929) | `resultsTable`, `thresholdTable`, `fixedSensSpecTable`, `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `costratioFP` | TextBox | [line 2821](../R/psychopdaROC.b.R#L2821) | `plotROC`, `criterionPlot`, `prevalencePlot`, `dotPlot` |
| `sensSpecTable` | CheckBox | [line 1559](../R/psychopdaROC.b.R#L1559) | `sensSpecTable` |
| `showThresholdTable` | CheckBox | [line 3415](../R/psychopdaROC.b.R#L3415) | `thresholdTable` |
| `maxThresholds` | TextBox | [line 3428](../R/psychopdaROC.b.R#L3428) | Runtime guidance or helper computation; see backend |
| `delongTest` | CheckBox | [line 3058](../R/psychopdaROC.b.R#L3058) | `delongComparisonTable`, `delongTest` |
| `plotROC` | CheckBox | [line 2391](../R/psychopdaROC.b.R#L2391) | `plotROC`, `interactivePlot` |
| `combinePlots` | CheckBox | [line 2753](../R/psychopdaROC.b.R#L2753) | `plotROC`, `fixedSensSpecROC`, `criterionPlot`, `prevalencePlot`, `dotPlot`, `dotPlotMessage`, `precisionRecallPlot` |
| `cleanPlot` | CheckBox | [line 3404](../R/psychopdaROC.b.R#L3404) | `plotROC` |
| `showOptimalPoint` | CheckBox | [line 4255](../R/psychopdaROC.b.R#L4255) | `plotROC` |
| `displaySE` | CheckBox | [line 4243](../R/psychopdaROC.b.R#L4243) | `plotROC` |
| `smoothing` | CheckBox | [line 4240](../R/psychopdaROC.b.R#L4240) | `plotROC` |
| `showConfidenceBands` | CheckBox | [line 3405](../R/psychopdaROC.b.R#L3405) | `plotROC` |
| `legendPosition` | ComboBox | [line 4142](../R/psychopdaROC.b.R#L4142) | `plotROC` |
| `directLabel` | CheckBox | [line 4341](../R/psychopdaROC.b.R#L4341) | `plotROC` |
| `interactiveROC` | CheckBox | [line 2981](../R/psychopdaROC.b.R#L2981) | `interactivePlot` |
| `showCriterionPlot` | CheckBox | [line 2758](../R/psychopdaROC.b.R#L2758) | `criterionPlot` |
| `showPrevalencePlot` | CheckBox | [line 2761](../R/psychopdaROC.b.R#L2761) | `prevalencePlot` |
| `showDotPlot` | CheckBox | [line 2764](../R/psychopdaROC.b.R#L2764) | `dotPlot`, `dotPlotMessage` |
| `precisionRecallCurve` | CheckBox | [line 3613](../R/psychopdaROC.b.R#L3613) | `precisionRecallPlot` |
| `partialAUC` | CheckBox | [line 3474](../R/psychopdaROC.b.R#L3474) | `partialAUCTable` |
| `partialAUCfrom` | TextBox | [line 3494](../R/psychopdaROC.b.R#L3494) | `partialAUCTable` |
| `partialAUCto` | TextBox | [line 3495](../R/psychopdaROC.b.R#L3495) | `partialAUCTable` |
| `rocSmoothingMethod` | ComboBox | [line 3513](../R/psychopdaROC.b.R#L3513) | `plotROC` |
| `bootstrapCI` | CheckBox | [line 1056](../R/psychopdaROC.b.R#L1056) | `partialAUCTable`, `bootstrapCITable` |
| `bootstrapReps` | TextBox | [line 1060](../R/psychopdaROC.b.R#L1060) | `partialAUCTable`, `bootstrapCITable` |
| `quantileCIs` | CheckBox | [line 2391](../R/psychopdaROC.b.R#L2391) | `plotROC` |
| `quantiles` | TextBox | [line 2392](../R/psychopdaROC.b.R#L2392) | `plotROC` |
| `compareClassifiers` | CheckBox | [line 3678](../R/psychopdaROC.b.R#L3678) | `rocComparisonTable` |
| `calculateIDI` | CheckBox | [line 2528](../R/psychopdaROC.b.R#L2528) | `idiTable` |
| `calculateNRI` | CheckBox | [line 2528](../R/psychopdaROC.b.R#L2528) | `nriTable` |
| `refVar` | VariablesListBox | [line 3732](../R/psychopdaROC.b.R#L3732) | `idiTable`, `nriTable` |
| `nriThresholds` | TextBox | [line 3832](../R/psychopdaROC.b.R#L3832) | `nriTable` |
| `idiNriBootRuns` | TextBox | [line 3751](../R/psychopdaROC.b.R#L3751) | `idiTable`, `nriTable` |
| `effectSizeAnalysis` | CheckBox | [line 1967](../R/psychopdaROC.b.R#L1967) | `effectSizeTable`, `effectSizePlot` |
| `powerAnalysis` | CheckBox | [line 3974](../R/psychopdaROC.b.R#L3974) | `powerAnalysisTable`, `powerCurvePlot` |
| `powerAnalysisType` | ComboBox | [line 5487](../R/psychopdaROC.b.R#L5487) | `powerAnalysisTable`, `powerCurvePlot` |
| `expectedAUCDifference` | TextBox | [line 5484](../R/psychopdaROC.b.R#L5484) | `powerAnalysisTable`, `powerCurvePlot` |
| `targetPower` | TextBox | [line 5483](../R/psychopdaROC.b.R#L5483) | `powerAnalysisTable`, `powerCurvePlot` |
| `significanceLevel` | TextBox | [line 5485](../R/psychopdaROC.b.R#L5485) | `powerAnalysisTable`, `powerCurvePlot` |
| `correlationROCs` | TextBox | [line 5486](../R/psychopdaROC.b.R#L5486) | `powerAnalysisTable`, `powerCurvePlot` |
| `bayesianAnalysis` | CheckBox | [line 3981](../R/psychopdaROC.b.R#L3981) | `bayesianROCTable`, `bayesianTracePlot` |
| `priorAUC` | TextBox | [line 5745](../R/psychopdaROC.b.R#L5745) | `bayesianROCTable`, `bayesianTracePlot` |
| `priorPrecision` | TextBox | [line 5746](../R/psychopdaROC.b.R#L5746) | `bayesianROCTable`, `bayesianTracePlot` |
| `clinicalUtilityAnalysis` | CheckBox | [line 3988](../R/psychopdaROC.b.R#L3988) | `clinicalUtilityTable`, `decisionCurveTable`, `decisionCurvePlot` |
| `treatmentThreshold` | TextBox | [line 5897](../R/psychopdaROC.b.R#L5897) | `clinicalUtilityTable`, `decisionCurveTable`, `decisionCurvePlot` |
| `harmBenefitRatio` | TextBox | [line 5894](../R/psychopdaROC.b.R#L5894) | `clinicalUtilityTable`, `decisionCurveTable`, `decisionCurvePlot` |
| `interventionCost` | CheckBox | [line 5895](../R/psychopdaROC.b.R#L5895) | `clinicalUtilityTable`, `decisionCurveTable`, `decisionCurvePlot` |
| `fixedSensSpecAnalysis` | CheckBox | [line 881](../R/psychopdaROC.b.R#L881) | `fixedSensSpecTable`, `fixedSensSpecExplanation`, `fixedSensSpecROC` |
| `fixedAnalysisType` | ComboBox | [line 885](../R/psychopdaROC.b.R#L885) | `fixedSensSpecTable`, `fixedSensSpecROC` |
| `fixedSensitivityValue` | TextBox | [line 887](../R/psychopdaROC.b.R#L887) | `fixedSensSpecTable`, `fixedSensSpecROC` |
| `fixedSpecificityValue` | TextBox | [line 889](../R/psychopdaROC.b.R#L889) | `fixedSensSpecTable`, `fixedSensSpecROC` |
| `showFixedROC` | CheckBox | [line 2879](../R/psychopdaROC.b.R#L2879) | `fixedSensSpecROC` |
| `fixedInterpolation` | ComboBox | [line 892](../R/psychopdaROC.b.R#L892) | `fixedSensSpecTable`, `fixedSensSpecROC` |
| `showFixedExplanation` | CheckBox | [line 980](../R/psychopdaROC.b.R#L980) | `fixedSensSpecExplanation` |
| `metaAnalysis` | CheckBox | [line 3994](../R/psychopdaROC.b.R#L3994) | `metaAnalysisWarning`, `metaAnalysisTable`, `metaAnalysisForestPlot` |
| `metaAnalysisMethod` | ComboBox | [line 2248](../R/psychopdaROC.b.R#L2248) | `metaAnalysisTable`, `metaAnalysisForestPlot` |
| `heterogeneityTest` | CheckBox | [line 6290](../R/psychopdaROC.b.R#L6290) | `metaAnalysisTable` |
| `forestPlot` | CheckBox | [line 6335](../R/psychopdaROC.b.R#L6335) | `metaAnalysisForestPlot` |
| `overrideMetaAnalysisWarning` | CheckBox | [line 6151](../R/psychopdaROC.b.R#L6151) | `metaAnalysisWarning`, `metaAnalysisTable`, `metaAnalysisForestPlot` |

See [developer documentation](psychopdaROC-documentation.md) for defaults and [release checks](testing_psychopdaROC.md) for executed coverage.
