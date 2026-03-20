# Survival Analysis - Feature Mapping Table

Complete feature-to-code mapping for the `survival` jamovi function. Every feature is traced from YAML option through UI label, result item, and backend method.

---

## 1. Input Variables

| Feature | a.yaml Option | Type | UI Label (u.yaml) | r.yaml Result | b.R Method |
|---|---|---|---|---|---|
| Survival time | `elapsedtime` | Variable (numeric) | Time Elapsed | (used in all) | `.definemytime()` |
| Outcome variable | `outcome` | Variable (factor/numeric) | Outcome | (used in all) | `.definemyoutcome()` |
| Event level | `outcomeLevel` | Level | Event Level | (used in all) | `.definemyoutcome()` |
| Explanatory group | `explanatory` | Variable (factor) | Explanatory Variable | `subtitle` | `.definemyfactor()` |
| Input data | `data` | Data | (implicit) | (all) | `.getData()` |

---

## 2. Time Calculation

| Feature | a.yaml Option | Type/Default | UI Label (u.yaml) | r.yaml Result | b.R Method |
|---|---|---|---|---|---|
| Use dates for time | `tint` | Bool / false | Calculate time from dates | - | `.definemytime()` |
| Diagnosis date | `dxdate` | Variable / null | Diagnosis Date | - | `.definemytime()` |
| Follow-up date | `fudate` | Variable / null | Follow-up Date | - | `.definemytime()` |
| Date input format | `timetypedata` | List / ymd | Input Time Type | - | `.definemytime()` (lubridate parse) |
| Time output unit | `timetypeoutput` | List / months | Output Time Type | - | `.definemytime()` (conversion) |
| Export calculated time | `calculatedtime` | Output | Add Calculated Time to Data | `calculatedtime` (Output) | `.run()` -> setValues |
| Landmark enable | `uselandmark` | Bool / false | Use Landmark Time | - | `.cleandata()` |
| Landmark time point | `landmark` | Integer / 3 | Landmark Time | `medianTable` note | `.cleandata()` -> dplyr::filter |

---

## 3. Outcome Configuration

| Feature | a.yaml Option | Type/Default | UI Label (u.yaml) | r.yaml Result | b.R Method |
|---|---|---|---|---|---|
| Multiple events | `multievent` | Bool / false | Enable Multiple Event Analysis | - | `.definemyoutcome()` |
| Dead of Disease | `dod` | Level | Dead of Disease | - | `.definemyoutcome()` |
| Dead of Other | `dooc` | Level | Dead of Other Causes | - | `.definemyoutcome()` |
| Alive w Disease | `awd` | Level | Alive with Disease | - | `.definemyoutcome()` |
| Alive w/o Disease | `awod` | Level | Alive without Disease | - | `.definemyoutcome()` |
| Analysis type | `analysistype` | List / overall | Analysis Type | - | `.definemyoutcome()` |
| Export redefined outcome | `outcomeredefined` | Output | Add Redefined Outcome to Data | `outcomeredefined` (Output) | `.run()` -> setValues |

---

## 4. Core Survival Analysis (Always Runs)

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Median survival | (always) | - | - | `medianSurvivalHeading` (Preformatted) | `.medianSurv()` |
| Median summary text | showSummaries | Bool | Show Natural Language Summaries | `medianSummary` (Preformatted) | `.medianSurv()` |
| Median table | (always) | - | - | `medianTable` (Table: factor, records, events, rmean, se_rmean, median, x0_95lcl, x0_95ucl) | `.medianSurv()` |
| Survival probabilities | `cutp` | String / '12, 36, 60' | Time Points for Table | `survivalTablesHeading` (Preformatted) | `.survTable()` |
| Survival table | `cutp` | String | Time Points for Table | `survTable` (Table: strata, time, n.risk, n.event, surv, lower, upper) | `.survTable()` |
| Survival table summary | showSummaries | Bool | Show Natural Language Summaries | `survTableSummary` (Preformatted) | `.survTable()` |

---

## 5. Cox Regression

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Cox regression | (always, unless competing) | - | - | `coxRegressionHeading` (Preformatted) | `.cox()` |
| Cox table | (always) | - | - | `coxTable` (Table: Explanatory, Levels, all, HR_univariable) | `.cox()` via finalfit |
| Cox forest plot | (always) | - | - | `tCoxtext2` (Html) | `.cox()` |
| Cox summary text | showSummaries | Bool | Show Natural Language Summaries | `coxSummary` (Preformatted) | `.cox()` |
| PH assumption test | `ph_cox` | Bool / false | Proportional Hazards Test | `cox_ph` (Preformatted) | `.cox()` -> cox.zph |
| PH interpretation | `ph_cox` | Bool / false | Proportional Hazards Test | `phInterpretation` (Html) | `.generatePHInterpretation()` |
| PH Schoenfeld plot | `ph_cox` | Bool / false | Proportional Hazards Test | `plot8` (Image 600x450) | `.plot8()` |
| Stratified Cox | `stratified_cox` | Bool / false | Stratified Cox Model | (within coxTable) | `.cox()` with strata() |
| Stratification var | `strata_variable` | Variable / null | Stratification Variable | (within coxTable) | `.cox()` |
| Residual diagnostics | `residual_diagnostics` | Bool / false | Residual Diagnostics | `residualsTable` (Table: observation, martingale, deviance, score, schoenfeld) | `.calculateResiduals()` |
| Residuals plot | `residual_diagnostics` | Bool / false | Residual Diagnostics | `residualsPlot` (Image 600x450) | `.plot9()` |
| Export survival data | `export_survival_data` | Output | Export Estimated Survival | `survivalExport` (Output), `survivalExportSummary` (Html) | `.exportSurvivalData()` |

---

## 6. Age Adjustment (NEW)

All features require `age_adjustment=true` and `age_variable` set. Skipped for competing risk.

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Age-adjusted Cox | `age_adjustment` | Bool / false | Age-Adjusted Cox Regression | `ageAdjustedCoxHeading` (Preformatted) | `.ageAdjustedCox()` |
| Age-adjusted table | `age_adjustment` | Bool / false | Age-Adjusted Cox Regression | `ageAdjustedCoxTable` (Table: variable, levels, n, hr_unadjusted, hr_age_adjusted) | `.ageAdjustedCox()` |
| Age interpretation | `age_adjustment` | Bool / false | Age-Adjusted Cox Regression | `ageAdjustedInterpretation` (Html) | `.ageAdjustedCox()` |
| Age explanation | `age_adjustment` + `showExplanations` | Bool | - | `ageAdjustedExplanation` (Html) | `.ageAdjustedCox()` |
| Age x Group interaction | `age_interaction` | Bool / false | Test Age x Group Interaction | `ageInteractionTable` (Table: term, coef, hr, se, z, pvalue) | `.ageAdjustedCox()` |
| Age-stratified Cox | `age_stratified_cox` | Bool / false | Stratify Cox by Age Groups | (within ageAdjustedCoxTable) | `.ageAdjustedCox()` |
| Age group cutpoints | `age_group_cutpoints` | String / '50, 65, 75' | Age Group Cutpoints | - | `.ageAdjustedCox()`, `.ageStandardization()` |
| Age as time scale | `age_time_scale` | Bool / false | Age as Time Scale | `ageTimeScaleTable` (Table: variable, levels, hr, ci_lower, ci_upper, pvalue) | `.ageTimeScaleCox()` |
| Age time scale interp. | `age_time_scale` | Bool / false | - | `ageTimeScaleInterpretation` (Html) | `.ageTimeScaleCox()` |
| Age standardization | `age_standardization` | Bool / false | Age Standardization (SMR) | `ageStandardizationTable` (Table: group, observed, expected, smr, smr_ci, pvalue) | `.ageStandardization()` |
| Standardization method | `age_standardization_method` | List / indirect | Standardization Method | (within ageStandardizationTable) | `.ageStandardization()` |
| SMR interpretation | `age_standardization` | Bool / false | - | `ageStandardizationInterpretation` (Html) | `.ageStandardization()` |
| Age-stratified KM | `age_stratified_km` | Bool / false | Age-Stratified KM Plots | `ageStratifiedKMPlot` (Image 700x500) | `.plotAgeStratifiedKM()` |
| Adjusted curves | `adjusted_curves` | Bool / false | Adjusted Survival Curves | `adjustedCurvesPlot` (Image 700x500) | `.plotAdjustedCurves()` |

---

## 7. Plots

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result | b.R renderFun |
|---|---|---|---|---|---|
| Survival curve | `sc` | Bool / false | Survival Curves | `plot` (Image 600x450) | `.plot()` |
| KMunicate plot | `kmunicate` | Bool / false | KMunicate Plot | `plot6` (Image 600x450) | `.plot6()` |
| Cumulative events | `ce` | Bool / false | Cumulative Events | `plot2` (Image 600x450) | `.plot2()` |
| Cumulative hazard | `ch` | Bool / false | Cumulative Hazard | `plot3` (Image 600x450) | `.plot3()` |
| Log-log plot | `loglog` | Bool / false | Log-Log Plot | `plot7` (Image 600x450) | `.plot7()` |
| 95% CI | `ci95` | Bool / false | 95% Confidence Intervals | (modifies plots) | All plot fns |
| Risk table | `risktable` | Bool / false | Risk Table | (modifies plots) | `.plot()` |
| Censored marks | `censored` | Bool / false | Show Censored Points | (modifies plots) | `.plot()` |
| P-value display | `pplot` | Bool / false | Show P-values | (modifies plots) | `.plot()` |
| Median line | `medianline` | List / none | Median Survival Line | (modifies plots) | `.plot()` |
| Plot end time | `endplot` | Integer / 60 | Plot End Time | (modifies plots) | All plot fns |
| Time interval | `byplot` | Integer / 12 | Time Interval | (modifies plots) | All plot fns |
| Y-axis start | `ybegin_plot` | Number / 0.00 | Y-axis Start | (modifies plots) | `.plot()` |
| Y-axis end | `yend_plot` | Number / 1.00 | Y-axis End | (modifies plots) | `.plot()` |

---

## 8. Statistical Tests

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Pairwise comparisons | `pw` | Bool / false | Perform Pairwise Comparisons | `pairwiseComparisonHeading`, `pairwiseTable` (Table: rowname, name, value), `pairwiseSummary` | `.pairwise()` |
| P-value adjustment | `padjustmethod` | List / holm | P-value Adjustment Method | (within pairwiseTable) | `.pairwise()` via survminer |
| Weighted log-rank | `weightedLogRank` | Bool / false | Perform Weighted Log-Rank Tests | `weightedLogRankTable` (Table: test, rho, chisq, df, pvalue, weighting) | `.calculateWeightedLogRank()` |
| Test type | `survivalTestType` | List / logrank | Test Type for Pairwise Comparisons | (modifies pairwise) | `.calculateWeightedLogRank()` |

### survivalTestType Values to rho Mapping

| Test Name | survivalTestType value | rho Parameter | Weighting Emphasis |
|---|---|---|---|
| Log-Rank (standard) | logrank | 0 | Equal weighting |
| Gehan-Breslow-Wilcoxon | gehan_breslow | 1 | Early differences |
| Tarone-Ware | tarone_ware | 0.5 | Moderate early |
| Peto-Peto | peto_peto | (special) | Early, more robust |
| Fleming-Harrington | fleming_harrington | (configurable) | Flexible |

---

## 9. Validation & Calibration

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Calibration curves | `calibration_curves` | Bool / false | Calibration Curves | `calibrationTable`, `calibrationGroupTable`, `calibrationPlot` (Image 600x500) | `.calculateCalibration()`, `.plotCalibration()` |
| Calibration timepoint | `calibration_timepoint` | Number / 0 | Time Point (0 = median) | (config for calibration) | `.calculateCalibration()` |
| Calibration groups | `calibration_ngroups` | Integer / 5 | Number of Risk Groups | (config for calibration) | `.calculateCalibration()` |
| Bootstrap validation | `bootstrapValidation` | Bool / false | Bootstrap Internal Validation | `bootstrapValidationTable` (Table: metric, apparent, optimism, corrected, n_bootstrap) | `.calculateBootstrapValidation()` |
| Bootstrap resamples | `bootstrapValN` | Integer / 200 | Bootstrap Resamples | (config) | `.calculateBootstrapValidation()` |
| RCS non-linearity | `rcs_analysis` | Bool / false | Restricted Cubic Splines | `rcsTestTable`, `rcsPlot` (Image 600x450) | `.calculateRCS()`, `.plotRCS()` |
| RCS variable | `rcs_variable` | Variable / null | Continuous Variable for Spline | (config) | `.calculateRCS()` |
| RCS knots | `rcs_knots` | Integer / 4 | Number of Knots (3-7) | (config) | `.calculateRCS()` |

---

## 10. Person-Time Analysis

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Enable person-time | `person_time` | Bool / false | Calculate Person-Time Rates | `personTimeHeading`, `personTimeTable` (Table: interval, events, person_time, rate, rate_ci_lower, rate_ci_upper) | `.personTimeAnalysis()` |
| Time intervals | `time_intervals` | String / '12, 36, 60' | Time Intervals | (config) | `.personTimeAnalysis()` |
| Rate multiplier | `rate_multiplier` | Integer / 100 | Rate Multiplier | (config - per N person-time) | `.personTimeAnalysis()` |
| Person-time summary | `person_time` + showSummaries | Bool | - | `personTimeSummary` (Html) | `.personTimeAnalysis()` |

---

## 11. RMST (Restricted Mean Survival Time)

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Enable RMST | `rmst_analysis` | Bool / false | RMST Analysis | `rmstHeading`, `rmstTable` (Table: group, rmst, se, ci_lower, ci_upper, tau) | `.calculateRMST()` |
| Time horizon | `rmst_tau` | Number / 0 | Tau (Restriction Time) | (config - 0 = 75th percentile) | `.calculateRMST()` |
| RMST summary | rmst_analysis + showSummaries | Bool | - | `rmstSummary` (Preformatted) | `.calculateRMST()` |

---

## 12. Explanations & Reporting

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Analysis explanations | `showExplanations` | Bool / false | Show Analysis Explanations | `medianSurvivalExplanation`, `coxRegressionExplanation`, `survivalTablesExplanation`, `survivalPlotsExplanation`, `personTimeExplanation`, `rmstExplanation`, `residualDiagnosticsExplanation`, `calibrationInterpretation`, `rcsInterpretation`, `parametricModelsExplanation`, `clinicalGlossaryExplanation`, `ageAdjustedExplanation` | `.populateExplanations()` |
| Natural language summaries | `showSummaries` | Bool / false | Show Natural Language Summaries | `medianSummary`, `coxSummary`, `survTableSummary`, `pairwiseSummary`, `rmstSummary`, `weightedLogRankExplanation`, `bootstrapValidationExplanation`, `clinicalInterpretationExplanation`, `copyReadySentencesExplanation` | `.populateEnhancedClinicalContent()` |
| REMARK checklist | `remark_checklist` | Bool / false | REMARK Reporting Checklist | `remarkChecklist` (Html) | `.generateRemarkChecklist()` |
| Clinical glossary | showExplanations | Bool | - | `clinicalGlossaryExplanation` (Html) | `.generateClinicalGlossary()` |
| Clinical interpretation | showSummaries | Bool | - | `clinicalInterpretationExplanation` (Html) | `.generateClinicalInterpretation()` |
| Copy-ready sentences | showSummaries | Bool | - | `copyReadySentencesExplanation` (Html) | `.generateCopyReadySentences()` |

---

## 13. Parametric Survival Models (DISABLED in Current Release)

All code is present but commented out in `.init()` and `.run()`. UI controls exist but are non-functional.

| Feature | a.yaml Option | Type/Default | UI Label | r.yaml Result(s) | b.R Method |
|---|---|---|---|---|---|
| Enable parametric | `use_parametric` | Bool / false | Enable Parametric Survival Models | - | `.parametricSurvival()` (commented) |
| Distribution | `parametric_distribution` | List / weibull | Parametric Distribution | `parametricModelSummary` | (commented) |
| Include covariates | `parametric_covariates` | Bool / true | Include Covariates | - | (commented) |
| Spline knots | `spline_knots` | Integer / 3 | Number of Spline Knots | - | (commented) |
| Spline scale | `spline_scale` | List / hazard | Spline Scale | - | (commented) |
| Extrapolation | `parametric_extrapolation` | Bool / false | Extrapolation Analysis | `extrapolationPlot`, `extrapolationTable` | `.plotExtrapolation()` (stub) |
| Extrapolation time | `extrapolation_time` | Number / 0 | Extrapolation Time Horizon | - | (commented) |
| Diagnostics | `parametric_diagnostics` | Bool / true | Model Diagnostics | `parametricDiagnostics` | (commented) |
| Compare distributions | `compare_distributions` | Bool / false | Compare Multiple Distributions | `parametricModelComparison` | (commented) |
| Parametric plots | `parametric_survival_plots` | Bool / false | Parametric Survival Plots | `parametricSurvivalPlot` | `.plotParametricSurvival()` (stub) |
| Hazard plots | `hazard_plots` | Bool / false | Hazard Function Plots | `hazardFunctionPlot` | `.plotHazardFunction()` (stub) |

Supported distributions (for future activation):
- Exponential (`exp`)
- Weibull (`weibull`)
- Log-Normal (`lnorm`)
- Log-Logistic (`llogis`)
- Gamma (`gamma`)
- Generalized Gamma (`gengamma`)
- Gompertz (`gompertz`)
- Spline / Royston-Parmar (`survspline`)

---

## Data Flow Diagram

```
User Input
    |
    v
.validateInputs()  -->  reject with message if missing
    |
    v
.getData()  -->  fetch from self$data, cache
    |
    v
.definemytime()      \
.definemyoutcome()    }-->  .cleandata()  -->  joined data.frame
.definemyfactor()    /         |
                               |-- Landmark filter (if uselandmark)
                               |-- Rename columns
                               |-- naOmit
                               |-- setState for plots
                               |
                               v
                        results list:
                        { name1time, name2outcome, name3explanatory,
                          cleanData, *_labelled }
                               |
    +-----------+-----------+--+--+-------+------+------+---+
    |           |           |     |       |      |      |   |
    v           v           v     v       v      v      v   v
.medianSurv  .cox      .survTable .pairwise .rmst .person .boot .age*
    |           |           |     |       |      |      |
    v           v           v     v       v      v      v
 medianTable  coxTable   survTable  pw   rmst   pt    boot
              tCoxtext2           Table  Table  Table  Table
              cox_ph
```

---

## Test Dataset: `survival_comprehensive`

**File:** `data/survival_comprehensive.rda`
**Dimensions:** 200 rows x 15 columns

| Column | Type | Description | Use As |
|---|---|---|---|
| PatientID | integer | 1-200 | - |
| FollowUpMonths | numeric | Survival time in months | `elapsedtime` |
| Status | Factor (4 levels) | Alive without Disease, Alive with Disease, Dead of Disease, Dead of Other Causes | `outcome` |
| TumorStage | Factor (4 levels) | Stage I-IV | `explanatory` |
| Age | numeric | Patient age | `age_variable`, `rcs_variable` |
| Sex | Factor (2 levels) | Female, Male | `explanatory` (alt) |
| TumorGrade | Factor (3 levels) | Grade 1-3 | `explanatory` (alt) |
| Ki67 | numeric | Proliferation index (with NAs) | `rcs_variable` |
| TumorSize | numeric | Tumor size in cm (with NAs) | `rcs_variable` |
| LymphovascularInvasion | Factor (2 levels) | Absent, Present | `explanatory` (alt) |
| MarginStatus | Factor (3 levels) | Negative, Close, Positive | `explanatory` (alt), `strata_variable` |
| DiagnosisDate | character | YYYY-MM-DD format | `dxdate` |
| LastFollowUpDate | character | YYYY-MM-DD format | `fudate` |
| HER2Status | Factor (3 levels) | Negative, Equivocal, Positive | `explanatory` (alt) |
| ERStatus | Factor (2 levels) | Negative, Positive | `explanatory` (alt) |

**Status distribution:** Alive without Disease (80), Alive with Disease (25), Dead of Disease (73), Dead of Other Causes (22)
