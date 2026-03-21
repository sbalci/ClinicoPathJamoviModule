# Survival Analysis: Feature-to-Code Mapping

> Complete mapping of the `survival` jamovi function across all four architecture files.
> Last updated: 2026-03-22

---

## 1. Core Variables & Input

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Time elapsed (numeric) | `elapsedtime` (Variable, numeric) | VariablesListBox `elapsedtime` | -- | `.definemytime()` |
| Calculate time from dates | `tint` (Bool), `dxdate`, `fudate` (Variable) | CollapseBox "Advanced Elapsed Time Options" | `calculatedtime` (Output) | `.definemytime()` |
| Date format in data | `timetypedata` (List: ymdhms/ymd/ydm/mdy/myd/dmy/dym) | ComboBox `timetypedata` | -- | `.definemytime()` |
| Output time unit | `timetypeoutput` (List: days/weeks/months/years) | ComboBox `timetypeoutput` | -- | `.definemytime()` |
| Outcome variable | `outcome` (Variable, factor/numeric) | VariablesListBox `outcome` | `outcomeredefined` (Output) | `.definemyoutcome()` |
| Event level | `outcomeLevel` (Level of outcome) | LevelSelector `outcomeLevel` | -- | `.definemyoutcome()` |
| Explanatory variable | `explanatory` (Variable, factor) | VariablesListBox `explanatory` | `subtitle` (Preformatted) | `.definemyfactor()` |

---

## 2. Core Analysis: Kaplan-Meier & Median Survival

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Median survival (always runs) | (none -- always computed) | -- | `medianSurvivalHeading` (Preformatted), `medianSummary` (Preformatted), `medianTable` (Table: factor, records, events, rmean, se_rmean, median, x0_95lcl, x0_95ucl) | `.medianSurv()` |
| Median survival explanation | `showExplanations` (Bool) | -- | `medianSurvivalHeading3` (Preformatted), `medianSurvivalExplanation` (Html) | `.populateExplanations()` |
| Median survival summary | `showSummaries` (Bool) | -- | `medianSummary` (Preformatted) | `.medianSurv()` |

---

## 3. Core Analysis: Cox Regression

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Cox proportional hazards (always runs) | (none -- always computed unless competing risk) | -- | `coxRegressionHeading` (Preformatted), `coxSummary` (Preformatted), `coxTable` (Table: Explanatory, Levels, all, HR_univariable), `tCoxtext2` (Html) | `.cox()` |
| Cox regression explanation | `showExplanations` (Bool) | -- | `coxRegressionHeading3` (Preformatted), `coxRegressionExplanation` (Html) | `.populateExplanations()` |
| Cox regression summary | `showSummaries` (Bool) | -- | `coxSummary` (Preformatted) | `.cox()` |

---

## 4. Survival Probability Tables

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Survival at cutpoints (always runs) | `cutp` (String, default "12, 36, 60") | CollapseBox "Survival Tables" > TextBox `cutp` | `survivalTablesHeading` (Preformatted), `survTableSummary` (Preformatted), `survTable` (Table: strata, time, n.risk, n.event, surv, lower, upper) | `.survTable()` |
| Survival tables explanation | `showExplanations` (Bool) | -- | `survivalTablesHeading3` (Preformatted), `survivalTablesExplanation` (Html) | `.populateExplanations()` |

---

## 5. PH Assumption Testing

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Proportional hazards test | `ph_cox` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `ph_cox` | `cox_ph` (Preformatted), `phInterpretation` (Html), `plot8` (Image, renderFun: `.plot8`) | `.cox()` (within Cox), `.generatePHInterpretation()`, `.plot8()` |

---

## 6. Pairwise Comparisons

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Pairwise group comparisons | `pw` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `pw` | `pairwiseComparisonHeading` (Preformatted), `pairwiseSummary` (Preformatted), `pairwiseTable` (Table: rowname, name, value) | `.pairwise()` |
| P-value adjustment method | `padjustmethod` (List: holm/hochberg/hommel/bonferroni/BH/BY/fdr/none) | ComboBox `padjustmethod` | -- | `.pairwise()` |

---

## 7. RMST (Restricted Mean Survival Time)

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| RMST analysis | `rmst_analysis` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `rmst_analysis` | `rmstHeading` (Preformatted), `rmstTable` (Table: group, rmst, se, ci_lower, ci_upper, tau), `rmstSummary` (Preformatted) | `.calculateRMST()` |
| RMST time horizon | `rmst_tau` (Number, default 0 = auto 75th percentile) | TextBox `rmst_tau` | -- | `.calculateRMST()` |
| RMST explanation | `showExplanations` (Bool) | -- | `rmstExplanation` (Html) | `.populateExplanations()` |

---

## 8. Weighted Log-Rank Tests

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Fleming-Harrington family tests | `weightedLogRank` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `weightedLogRank` | `weightedLogRankTable` (Table: test, rho, chisq, df, pvalue, weighting) | `.calculateWeightedLogRank()` |
| Survival test type selection | `survivalTestType` (List: logrank/gehan_breslow/tarone_ware/peto_peto/fleming_harrington) | ComboBox `survivalTestType` | -- | `.calculateWeightedLogRank()` |
| Weighted log-rank explanation | `showSummaries` (Bool) | -- | `weightedLogRankExplanation` (Html) | `.populateWeightedLogRankExplanation()` |

---

## 9. Bootstrap Internal Validation

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Bootstrap optimism-corrected C-index | `bootstrapValidation` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `bootstrapValidation` | `bootstrapValidationTable` (Table: metric, apparent, optimism, corrected, n_bootstrap) | `.calculateBootstrapValidation()` |
| Number of bootstrap resamples | `bootstrapValN` (Integer, default 200, min 50, max 1000) | TextBox `bootstrapValN` | -- | `.calculateBootstrapValidation()` |
| Bootstrap explanation | `showSummaries` (Bool) | -- | `bootstrapValidationExplanation` (Html) | `.populateBootstrapValidationExplanation()` |

---

## 10. Calibration Curves

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Cox model calibration assessment | `calibration_curves` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `calibration_curves` | `calibrationTable` (Table: metric, value, ci_lower, ci_upper, ideal, interpretation), `calibrationGroupTable` (Table: group, n, events, predicted, observed, observed_lower, observed_upper), `calibrationPlot` (Image, renderFun: `.plotCalibration`) | `.calculateCalibration()`, `.plotCalibration()` |
| Calibration time point | `calibration_timepoint` (Number, default 0 = median) | TextBox `calibration_timepoint` | -- | `.calculateCalibration()` |
| Number of risk groups | `calibration_ngroups` (Integer, default 5, min 3, max 10) | TextBox `calibration_ngroups` | -- | `.calculateCalibration()` |
| Calibration explanation | `showExplanations` (Bool) | -- | `calibrationInterpretation` (Html) | `.populateExplanations()` |

---

## 11. RCS (Restricted Cubic Splines)

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Non-linearity assessment | `rcs_analysis` (Bool) | CollapseBox "Statistical Analysis" > CheckBox `rcs_analysis` | `rcsTestTable` (Table: model, df, loglik, aic, lr_chisq, lr_df, p_value, conclusion), `rcsPlot` (Image, renderFun: `.plotRCS`) | `.calculateRCS()`, `.plotRCS()` |
| Continuous variable for spline | `rcs_variable` (Variable, numeric) | VariablesListBox `rcs_variable` | -- | `.calculateRCS()` |
| Number of knots | `rcs_knots` (Integer, default 4, min 3, max 7) | TextBox `rcs_knots` | -- | `.calculateRCS()` |
| RCS explanation | `showExplanations` (Bool) | -- | `rcsInterpretation` (Html) | `.populateExplanations()` |

---

## 12. Age Adjustment

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Age-adjusted Cox regression | `age_adjustment` (Bool), `age_variable` (Variable, numeric) | Label "Age Adjustment" > CheckBox + VariablesListBox | `ageAdjustedCoxHeading` (Preformatted), `ageAdjustedCoxTable` (Table: variable, levels, n, hr_unadjusted, hr_age_adjusted), `ageAdjustedInterpretation` (Html) | `.ageAdjustedCox()` |
| Age x group interaction test | `age_interaction` (Bool) | CheckBox `age_interaction` | `ageInteractionTable` (Table: term, coef, hr, se, z, pvalue) | `.ageAdjustedCox()` |
| Age-stratified Cox model | `age_stratified_cox` (Bool), `age_group_cutpoints` (String, default "50, 65, 75") | CheckBox `age_stratified_cox`, TextBox `age_group_cutpoints` | (within `ageAdjustedCoxTable`) | `.ageAdjustedCox()` |
| Age as time scale | `age_time_scale` (Bool) | CheckBox `age_time_scale` | `ageTimeScaleTable` (Table: variable, levels, hr, ci_lower, ci_upper, pvalue), `ageTimeScaleInterpretation` (Html) | `.ageTimeScaleCox()` |
| Age standardization (SMR) | `age_standardization` (Bool), `age_standardization_method` (List: indirect/direct) | CheckBox + ComboBox | `ageStandardizationTable` (Table: group, observed, expected, smr, smr_ci_lower, smr_ci_upper, pvalue), `ageStandardizationInterpretation` (Html) | `.ageStandardization()` |
| Age-stratified KM plots | `age_stratified_km` (Bool) | CheckBox `age_stratified_km` | `ageStratifiedKMPlot` (Image, 700x500, renderFun: `.plotAgeStratifiedKM`) | `.plotAgeStratifiedKM()` |
| Adjusted survival curves | `adjusted_curves` (Bool) | CheckBox `adjusted_curves` | `adjustedCurvesPlot` (Image, 700x500, renderFun: `.plotAdjustedCurves`) | `.plotAdjustedCurves()` |
| Age adjustment explanation | `showExplanations` (Bool) | -- | `ageAdjustedExplanation` (Html) | `.populateExplanations()` |

---

## 13. Competing Risks & Multi-Event

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Multiple event levels | `multievent` (Bool) | CollapseBox "Analysis with Multiple Outcomes" > CheckBox `multievent` | `outcomeredefined` (Output) | `.definemyoutcome()` |
| Dead of Disease | `dod` (Level of outcome) | LevelSelector `dod` | -- | `.definemyoutcome()` |
| Dead of Other Causes | `dooc` (Level of outcome) | LevelSelector `dooc` | -- | `.definemyoutcome()` |
| Alive with Disease | `awd` (Level of outcome) | LevelSelector `awd` | -- | `.definemyoutcome()` |
| Alive without Disease | `awod` (Level of outcome) | LevelSelector `awod` | -- | `.definemyoutcome()` |
| Analysis type | `analysistype` (List: overall/cause/compete) | ComboBox `analysistype` | -- | `.definemyoutcome()`, `.isCompetingRisk()` |
| Competing risk cumulative incidence | (auto when `analysistype == "compete"`) | -- | (via plots) | `.competingRiskCumInc()` |

---

## 14. Person-Time Analysis

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Person-time metrics & incidence rates | `person_time` (Bool) | CollapseBox "Person-Time Analysis" > CheckBox `person_time` | `personTimeHeading` (Preformatted), `personTimeTable` (Table: interval, events, person_time, rate, rate_ci_lower, rate_ci_upper), `personTimeSummary` (Html) | `.personTimeAnalysis()` |
| Time intervals for stratification | `time_intervals` (String, default "12, 36, 60") | TextBox `time_intervals` | -- | `.personTimeAnalysis()` |
| Rate multiplier | `rate_multiplier` (Integer, default 100) | TextBox `rate_multiplier` | -- | `.personTimeAnalysis()` |
| Person-time explanation | `showExplanations` (Bool) | -- | `personTimeExplanation` (Html) | `.populateExplanations()` |

---

## 15. Stratified Cox & Landmark

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Stratified Cox model | `stratified_cox` (Bool), `strata_variable` (Variable, factor) | CheckBox `stratified_cox`, VariablesListBox `strata_variable` | (within `coxTable`) | `.cox()` |
| Landmark analysis | `uselandmark` (Bool), `landmark` (Integer, default 3) | Label "Landmark Analysis" > CheckBox + TextBox | -- | `.cleandata()` (filters data at landmark time) |

---

## 16. Parametric Models (Disabled in Current Release)

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Enable parametric models | `use_parametric` (Bool) | CollapseBox "Parametric Survival Models" | `parametricModelSummary` (Table), `parametricModelComparison` (Table), `parametricDiagnostics` (Html) | *disabled in .run()* |
| Distribution selection | `parametric_distribution` (List: exp/weibull/lnorm/llogis/gamma/gengamma/gompertz/survspline) | ComboBox | -- | *disabled* |
| Include covariates | `parametric_covariates` (Bool) | CheckBox | -- | *disabled* |
| Spline knots / scale | `spline_knots` (Int, 1-10), `spline_scale` (List) | TextBox, ComboBox | -- | *disabled* |
| Extrapolation | `parametric_extrapolation` (Bool), `extrapolation_time` (Number) | CheckBox, TextBox | `extrapolationPlot` (Image), `extrapolationTable` (Table) | *disabled* |
| Compare distributions | `compare_distributions` (Bool) | CheckBox | `parametricModelComparison` (Table) | *disabled* |
| Parametric survival plots | `parametric_survival_plots` (Bool) | CheckBox | `parametricSurvivalPlot` (Image) | `.plotParametricSurvival()` *disabled* |
| Hazard function plots | `hazard_plots` (Bool) | CheckBox | `hazardFunctionPlot` (Image) | `.plotHazardFunction()` *disabled* |

---

## 17. Residual Diagnostics

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Cox model residuals | `residual_diagnostics` (Bool) | CheckBox `residual_diagnostics` | `residualsTable` (Table: observation, martingale, deviance, score, schoenfeld), `residualsPlot` (Image, renderFun: `.plot9`) | `.calculateResiduals()`, `.plot9()` |
| Residual diagnostics explanation | `showExplanations` (Bool) | -- | `residualDiagnosticsExplanation` (Html) | `.populateExplanations()` |

---

## 18. Plots

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Survival curve (KM) | `sc` (Bool) | CollapseBox "Survival Plots" > CheckBox `sc` | `plot` (Image, 600x450, renderFun: `.plot`) | `.plot()` |
| KMunicate-style plot | `kmunicate` (Bool) | CheckBox `kmunicate` | `plot6` (Image, 600x450, renderFun: `.plot6`) | `.plot6()` |
| Cumulative events | `ce` (Bool) | CheckBox `ce` | `plot2` (Image, 600x450, renderFun: `.plot2`) | `.plot2()` |
| Cumulative hazard | `ch` (Bool) | CheckBox `ch` | `plot3` (Image, 600x450, renderFun: `.plot3`) | `.plot3()` |
| Log-log plot | `loglog` (Bool) | CheckBox `loglog` | `plot7` (Image, 600x450, renderFun: `.plot7`) | `.plot7()` |
| PH assumption plot | `ph_cox` (Bool) | CheckBox `ph_cox` | `plot8` (Image, 600x450, renderFun: `.plot8`) | `.plot8()` |
| Residuals diagnostic plot | `residual_diagnostics` (Bool) | CheckBox `residual_diagnostics` | `residualsPlot` (Image, 600x450, renderFun: `.plot9`) | `.plot9()` |
| Calibration plot | `calibration_curves` (Bool) | CheckBox `calibration_curves` | `calibrationPlot` (Image, 600x500, renderFun: `.plotCalibration`) | `.plotCalibration()` |
| RCS hazard ratio curve | `rcs_analysis` (Bool) | CheckBox `rcs_analysis` | `rcsPlot` (Image, 600x450, renderFun: `.plotRCS`) | `.plotRCS()` |
| Age-stratified KM curves | `age_stratified_km` (Bool) | CheckBox | `ageStratifiedKMPlot` (Image, 700x500, renderFun: `.plotAgeStratifiedKM`) | `.plotAgeStratifiedKM()` |
| Adjusted survival curves | `adjusted_curves` (Bool) | CheckBox | `adjustedCurvesPlot` (Image, 700x500, renderFun: `.plotAdjustedCurves`) | `.plotAdjustedCurves()` |

### Plot Appearance Options

| Feature | .a.yaml Option(s) | .u.yaml Section | Applies To |
|---------|-------------------|-----------------|------------|
| 95% confidence intervals | `ci95` (Bool) | Label "Plot Appearance" > CheckBox | `.plot()`, `.plot2()`, `.plot3()` |
| Risk table | `risktable` (Bool) | CheckBox | `.plot()`, `.plot2()`, `.plot3()` |
| Censored marks | `censored` (Bool) | CheckBox | `.plot()`, `.plot2()`, `.plot3()` |
| P-value on plot | `pplot` (Bool) | CheckBox | `.plot()` |
| Median survival line | `medianline` (List: none/h/v/hv) | ComboBox | `.plot()` |
| Plot end time | `endplot` (Integer, default 60) | TextBox | all plot functions |
| Time interval (axis breaks) | `byplot` (Integer, default 12) | TextBox | all plot functions |
| Y-axis start | `ybegin_plot` (Number, default 0.0) | TextBox | `.plot()`, `.plot2()`, `.plot3()` |
| Y-axis end | `yend_plot` (Number, default 1.0) | TextBox | `.plot()`, `.plot2()`, `.plot3()` |

---

## 19. UX Features: Explanations, Summaries & REMARK

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Analysis explanations | `showExplanations` (Bool) | CollapseBox "Analysis Explanations" > CheckBox | `medianSurvivalExplanation`, `coxRegressionExplanation`, `survivalTablesExplanation`, `personTimeExplanation`, `rmstExplanation`, `residualDiagnosticsExplanation`, `survivalPlotsExplanation`, `ageAdjustedExplanation`, `calibrationInterpretation`, `rcsInterpretation`, `parametricModelsExplanation` (all Html) | `.populateExplanations()` |
| Natural language summaries | `showSummaries` (Bool) | CheckBox | `medianSummary`, `coxSummary`, `survTableSummary`, `pairwiseSummary`, `rmstSummary` (Preformatted); `weightedLogRankExplanation`, `bootstrapValidationExplanation` (Html) | various methods |
| Clinical glossary | `showExplanations` (Bool) | -- | `clinicalGlossaryExplanation` (Html) | `.generateClinicalGlossary()` |
| Clinical interpretation | `showSummaries` (Bool) | -- | `clinicalInterpretationExplanation` (Html) | `.generateClinicalInterpretation()` |
| Copy-ready report sentences | `showSummaries` (Bool) | -- | `copyReadySentencesExplanation` (Html) | `.generateCopyReadySentences()` |
| REMARK checklist | `remark_checklist` (Bool) | CheckBox `remark_checklist` | `remarkChecklist` (Html) | `.generateRemarkChecklist()` |

---

## 20. Data Export

| Feature | .a.yaml Option(s) | .u.yaml Section | .r.yaml Output(s) | .b.R Method |
|---------|-------------------|-----------------|-------------------|-------------|
| Export survival estimates | `export_survival_data` (Output) | CollapseBox "Data Export Options" > Output | `survivalExport` (Output), `survivalExportSummary` (Html) | `.exportSurvivalData()` |
| Calculated time to data | `calculatedtime` (Output) | Output `calculatedtime` | `calculatedtime` (Output) | `.run()` (direct assignment) |
| Redefined outcome to data | `outcomeredefined` (Output) | Output `outcomeredefined` | `outcomeredefined` (Output) | `.run()` (direct assignment) |

---

## 21. Safety Checks & Clinical Warnings

| Check | Location | Condition | Behavior |
|-------|----------|-----------|----------|
| **Critical: <10 events** | `.run()` | `n_events < 10` | `stop()` -- blocks analysis entirely |
| **Warning: 10-19 events** | `.run()` | `n_events >= 10 && n_events < 20` | Table note on `medianTable` warning about unreliability |
| **Note: 20-49 events** | `.run()` | `n_events >= 20 && n_events < 50` | Table note on `medianTable` noting limitations for complex models |
| **Missing outcome** | `.validateInputs()` | `outcome == NULL` | `jmvcore::reject()` |
| **Missing explanatory** | `.validateInputs()` | `explanatory == NULL` | `jmvcore::reject()` |
| **Missing time** | `.validateInputs()` | No `elapsedtime` and no dates | `jmvcore::reject()` |
| **Incomplete dates** | `.validateInputs()` | `tint == TRUE` but missing dx/fu date | `jmvcore::reject()` |
| **Missing multievent config** | `.validateInputs()` | `multievent == TRUE` but no dod/dooc | `jmvcore::reject()` |
| **Empty data** | `.validateInputs()` | `nrow(data) == 0` | `jmvcore::reject()` |
| **Competing risk skips** | `.run()` | `multievent && analysistype == "compete"` | Skips Cox, pairwise, RMST, calibration, RCS, bootstrap, person-time |

---

## 22. Internal Architecture: Key Private Methods

| Method | Purpose | Called By |
|--------|---------|----------|
| `.init()` | Visibility control for all outputs based on options | jamovi framework |
| `.run()` | Main orchestrator: validate -> clean -> analyze -> populate | jamovi framework |
| `.getData()` | Data retrieval with janitor name cleaning and label mapping | `.cleandata()` |
| `.cleandata()` | Data prep: time/outcome/explanatory extraction, NA removal, factor coercion | `.run()` |
| `.definemytime()` | Time variable construction (numeric or date-based) | `.cleandata()` |
| `.definemyoutcome()` | Outcome variable construction (binary, cause-specific, competing) | `.cleandata()` |
| `.definemyfactor()` | Explanatory variable extraction | `.cleandata()` |
| `.buildSurvFormula()` | Safe formula builder with variable name escaping | multiple methods |
| `.validateInputs()` | Pre-run validation of required inputs | `.run()` |
| `.populateTableSafely()` | Generic table population helper | multiple methods |
| `.safeAnalysis()` | Try-catch wrapper for analysis steps | multiple methods |
| `.isCompetingRisk()` | Check if competing risk analysis is active | `.run()`, plot methods |
| `.competingRiskCumInc()` | Competing risk cumulative incidence calculation | plot methods |
| `.getDefaultCutpoints()` | Parse cutpoints string into numeric vector | `.survTable()` |
| `.setExplanationContent()` | Safe Html content setter | `.populateExplanations()` |
| `.populateExplanations()` | Populate all explanation Html outputs | `.run()` |
| `.populateEnhancedClinicalContent()` | Clinical glossary + interpretation + copy-ready sentences | `.run()` |
| `.addAgeVariable()` | Pull age variable from `self$data` and align with cleaned data | `.ageAdjustedCox()`, `.ageTimeScaleCox()`, `.ageStandardization()` |

---

## 23. References Declared in .r.yaml

- `ClinicoPathJamoviModule`, `survival`, `survminer`, `finalfit`, `flexsurv`, `cmprsk`
- `KMunicate`, `KMunicate2`
- `survivaltutorial`, `survivalrwnahhas`, `survivalrviews`, `appliedsurvivalanalysisR`
- `lubridate`, `padjust`

---

## 24. Execution Flow (.run() method)

```
validate inputs
  -> .cleandata() [.definemytime() -> .definemyoutcome() -> .definemyfactor()]
    -> event count safety checks (stop if <10)
    -> .medianSurv()
    -> .calculateRMST() [if rmst_analysis]
    -> .cox() [unless competing risk]
      -> .calculateResiduals() [if residual_diagnostics]
      -> .generatePHInterpretation() [if ph_cox]
    -> .ageAdjustedCox() [if age_adjustment]
    -> .ageTimeScaleCox() [if age_time_scale]
    -> .ageStandardization() [if age_standardization]
    -> .survTable()
    -> .exportSurvivalData()
    -> .calculateCalibration() [if calibration_curves]
    -> .calculateRCS() [if rcs_analysis]
    -> .pairwise() [if pw]
    -> .calculateWeightedLogRank() [if weightedLogRank]
    -> .calculateBootstrapValidation() [if bootstrapValidation]
    -> .generateRemarkChecklist() [if remark_checklist]
    -> .personTimeAnalysis() [if person_time]
    -> export calculated time / redefined outcome
    -> .populateExplanations()
    -> .populateEnhancedClinicalContent()
```
