# Survival Analysis - Developer Documentation

## Overview

The `survival` function is the flagship analysis in the **ClinicoPath** jamovi module. It performs comprehensive univariate and stratified survival analysis, including Kaplan-Meier estimation, Cox proportional hazards regression, competing risks, age adjustment, person-time analysis, RMST, calibration, bootstrap validation, restricted cubic splines, parametric models, and publication-ready plots.

**Version:** 0.0.33
**Menu:** SurvivalT > ClinicoPath Survival
**Backend:** `R/survival.b.R` (survivalClass, ~6400 lines)
**Dataset for testing:** `data/survival_comprehensive.rda` (200 patients, 15 variables, 4-level Status)

---

## UI Controls to Options Mapping

### Main Panel (Always Visible)

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| VariablesListBox | Time Elapsed | `elapsedtime` | Variable (numeric) | null |
| VariablesListBox | Outcome | `outcome` | Variable (factor/numeric) | null |
| LevelSelector | Event Level | `outcomeLevel` | Level | (none) |
| VariablesListBox | Explanatory Variable | `explanatory` | Variable (factor) | null |

### CollapseBox: Advanced Elapsed Time Options

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Calculate time from dates | `tint` | Bool | false |
| VariablesListBox | Diagnosis Date | `dxdate` | Variable | null |
| VariablesListBox | Follow-up Date | `fudate` | Variable | null |
| ComboBox | Input Time Type | `timetypedata` | List | ymd |
| ComboBox | Output Time Type | `timetypeoutput` | List | months |
| CheckBox | Use Landmark Time | `uselandmark` | Bool | false |
| TextBox | Landmark Time | `landmark` | Integer | 3 |

### CollapseBox: Analysis with Multiple Outcomes

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Enable Multiple Event Analysis | `multievent` | Bool | false |
| LevelSelector | Dead of Disease | `dod` | Level | (none) |
| LevelSelector | Dead of Other Causes | `dooc` | Level | (none) |
| LevelSelector | Alive with Disease | `awd` | Level | (none) |
| LevelSelector | Alive without Disease | `awod` | Level | (none) |
| ComboBox | Analysis Type | `analysistype` | List | overall |

### CollapseBox: Statistical Analysis

#### Cox Regression Analysis

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Proportional Hazards Test | `ph_cox` | Bool | false |
| CheckBox | Stratified Cox Model | `stratified_cox` | Bool | false |
| CheckBox | Residual Diagnostics | `residual_diagnostics` | Bool | false |
| CheckBox | Log-Log Plot | `loglog` | Bool | false |
| VariablesListBox | Stratification Variable | `strata_variable` | Variable (factor) | null |

#### Age Adjustment

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Age-Adjusted Cox Regression | `age_adjustment` | Bool | false |
| VariablesListBox | Age Variable (Continuous) | `age_variable` | Variable (numeric) | null |
| CheckBox | Test Age x Group Interaction | `age_interaction` | Bool | false |
| CheckBox | Stratify Cox by Age Groups | `age_stratified_cox` | Bool | false |
| TextBox | Age Group Cutpoints | `age_group_cutpoints` | String | '50, 65, 75' |
| CheckBox | Age as Time Scale | `age_time_scale` | Bool | false |
| CheckBox | Age Standardization (SMR) | `age_standardization` | Bool | false |
| ComboBox | Standardization Method | `age_standardization_method` | List | indirect |
| CheckBox | Age-Stratified KM Plots | `age_stratified_km` | Bool | false |
| CheckBox | Adjusted Survival Curves | `adjusted_curves` | Bool | false |

#### Model Validation

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Calibration Curves | `calibration_curves` | Bool | false |
| TextBox | Time Point (0 = median) | `calibration_timepoint` | Number | 0 |
| TextBox | Number of Risk Groups | `calibration_ngroups` | Integer | 5 |
| CheckBox | Bootstrap Internal Validation | `bootstrapValidation` | Bool | false |
| TextBox | Bootstrap Resamples (50-1000) | `bootstrapValN` | Integer | 200 |

#### Non-Linearity Assessment

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Restricted Cubic Splines (RCS) | `rcs_analysis` | Bool | false |
| VariablesListBox | Continuous Variable for Spline | `rcs_variable` | Variable (numeric) | null |
| TextBox | Number of Knots (3-7) | `rcs_knots` | Integer | 4 |

#### Pairwise Comparisons

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Perform Pairwise Comparisons | `pw` | Bool | false |
| ComboBox | P-value Adjustment Method | `padjustmethod` | List | holm |

#### Weighted Log-Rank Tests

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Perform Weighted Log-Rank Tests | `weightedLogRank` | Bool | false |
| ComboBox | Test Type for Pairwise Comparisons | `survivalTestType` | List | logrank |

#### Restricted Mean Survival Time

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | RMST Analysis | `rmst_analysis` | Bool | false |
| TextBox | Tau (Restriction Time) | `rmst_tau` | Number | 0 |

### CollapseBox: Survival Plots

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Survival Curves | `sc` | Bool | false |
| CheckBox | KMunicate Plot | `kmunicate` | Bool | false |
| CheckBox | Cumulative Events | `ce` | Bool | false |
| CheckBox | Cumulative Hazard | `ch` | Bool | false |
| CheckBox | 95% Confidence Intervals | `ci95` | Bool | false |
| CheckBox | Risk Table | `risktable` | Bool | false |
| CheckBox | Show Censored Points | `censored` | Bool | false |
| CheckBox | Show P-values | `pplot` | Bool | false |
| ComboBox | Median Survival Line | `medianline` | List | none |
| TextBox | Plot End Time | `endplot` | Integer | 60 |
| TextBox | Time Interval | `byplot` | Integer | 12 |
| TextBox | Y-axis Start | `ybegin_plot` | Number | 0.00 |
| TextBox | Y-axis End | `yend_plot` | Number | 1.00 |

### CollapseBox: Survival Tables

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| TextBox | Time Points for Table | `cutp` | String | '12, 36, 60' |

### CollapseBox: Person-Time Analysis

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Calculate Person-Time Rates | `person_time` | Bool | false |
| TextBox | Time Intervals | `time_intervals` | String | '12, 36, 60' |
| TextBox | Rate Multiplier | `rate_multiplier` | Integer | 100 |

### CollapseBox: Data Export Options

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| Output | Export Estimated Survival | `export_survival_data` | Output | - |
| Output | Add Calculated Time to Data | `calculatedtime` | Output | - |
| Output | Add Redefined Outcome to Data | `outcomeredefined` | Output | - |

### CollapseBox: Analysis Explanations

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| CheckBox | Show Analysis Explanations | `showExplanations` | Bool | false |
| CheckBox | Show Natural Language Summaries | `showSummaries` | Bool | false |
| CheckBox | REMARK Reporting Checklist | `remark_checklist` | Bool | false |
| CheckBox | Enable Parametric Survival Models | `use_parametric` | Bool | false |

### Parametric Models (UI present but DISABLED in current release)

| UI Control | UI Label | Option Name | Type | Default |
|---|---|---|---|---|
| ComboBox | Parametric Distribution | `parametric_distribution` | List | weibull |
| CheckBox | Include Covariates | `parametric_covariates` | Bool | true |
| TextBox | Number of Spline Knots | `spline_knots` | Integer | 3 |
| ComboBox | Spline Scale | `spline_scale` | List | hazard |
| CheckBox | Extrapolation Analysis | `parametric_extrapolation` | Bool | false |
| TextBox | Extrapolation Time Horizon | `extrapolation_time` | Number | 0 |
| CheckBox | Model Diagnostics | `parametric_diagnostics` | Bool | true |
| CheckBox | Compare Multiple Distributions | `compare_distributions` | Bool | false |
| CheckBox | Parametric Survival Plots | `parametric_survival_plots` | Bool | false |
| CheckBox | Hazard Function Plots | `hazard_plots` | Bool | false |

---

## Options Reference (All Active Options)

Total: ~75 options defined in `survival.a.yaml`.

| # | Option | Type | Default | Description | Downstream Effect |
|---|--------|------|---------|-------------|-------------------|
| 1 | `data` | Data | - | Input data frame | Required |
| 2 | `elapsedtime` | Variable (numeric) | null | Survival time variable | `.definemytime()` |
| 3 | `tint` | Bool | false | Calculate time from dates | Enables dxdate/fudate flow |
| 4 | `dxdate` | Variable | null | Diagnosis date | `.definemytime()` when tint=true |
| 5 | `fudate` | Variable | null | Follow-up date | `.definemytime()` when tint=true |
| 6 | `calculatedtime` | Output | - | Export calculated time to dataset | `.run()` end |
| 7 | `explanatory` | Variable (factor) | null | Group comparison variable | `.definemyfactor()` |
| 8 | `outcome` | Variable (factor/numeric) | null | Event indicator | `.definemyoutcome()` |
| 9 | `outcomeLevel` | Level | (none) | Event level | `.definemyoutcome()` binary coding |
| 10 | `dod` | Level | (none) | Dead of Disease level | Competing risks |
| 11 | `dooc` | Level | (none) | Dead of Other Causes level | Competing risks |
| 12 | `awd` | Level | (none) | Alive with Disease level | Competing risks |
| 13 | `awod` | Level | (none) | Alive without Disease level | Competing risks |
| 14 | `analysistype` | List | overall | overall/cause/compete | `.definemyoutcome()` recoding |
| 15 | `outcomeredefined` | Output | - | Export redefined outcome | `.run()` end |
| 16 | `cutp` | String | '12, 36, 60' | Time points for survival table | `.survTable()` |
| 17 | `timetypedata` | List | ymd | Date parsing format | `.definemytime()` lubridate call |
| 18 | `timetypeoutput` | List | months | Output time unit | `.definemytime()` conversion |
| 19 | `uselandmark` | Bool | false | Enable landmark analysis | `.cleandata()` filtering |
| 20 | `landmark` | Integer | 3 | Landmark time point | `.cleandata()` time shift |
| 21 | `pw` | Bool | false | Pairwise comparisons | `.pairwise()` |
| 22 | `padjustmethod` | List | holm | Multiple testing correction | `.pairwise()` p.adjust |
| 23 | `weightedLogRank` | Bool | false | Weighted log-rank tests | `.calculateWeightedLogRank()` |
| 24 | `survivalTestType` | List | logrank | Test type for pairwise | `.calculateWeightedLogRank()` rho |
| 25 | `ph_cox` | Bool | false | PH assumption test | `.cox()` -> cox.zph |
| 26 | `sc` | Bool | false | Survival curve plot | `.plot()` |
| 27 | `kmunicate` | Bool | false | KMunicate-style plot | `.plot6()` |
| 28 | `ce` | Bool | false | Cumulative events plot | `.plot2()` |
| 29 | `ch` | Bool | false | Cumulative hazard plot | `.plot3()` |
| 30 | `endplot` | Integer | 60 | Plot end time | All plot functions |
| 31 | `ybegin_plot` | Number | 0.00 | Y-axis start | `.plot()` ylim |
| 32 | `yend_plot` | Number | 1.00 | Y-axis end | `.plot()` ylim |
| 33 | `byplot` | Integer | 12 | X-axis break interval | All plot functions |
| 34 | `multievent` | Bool | false | Enable multi-event analysis | `.definemyoutcome()` recoding |
| 35 | `ci95` | Bool | false | Show 95% CI on plots | `.plot()` conf.int |
| 36 | `risktable` | Bool | false | Show risk table | `.plot()` risk.table |
| 37 | `censored` | Bool | false | Show censoring marks | `.plot()` censor |
| 38 | `pplot` | Bool | false | Show p-value on plot | `.plot()` pval |
| 39 | `medianline` | List | none | Median line on plot | `.plot()` surv.median.line |
| 40 | `person_time` | Bool | false | Person-time metrics | `.personTimeAnalysis()` |
| 41 | `time_intervals` | String | '12, 36, 60' | Interval stratification | `.personTimeAnalysis()` |
| 42 | `rate_multiplier` | Integer | 100 | Rate per N person-time | `.personTimeAnalysis()` |
| 43 | `rmst_analysis` | Bool | false | RMST calculation | `.calculateRMST()` |
| 44 | `rmst_tau` | Number | 0 | RMST time horizon | `.calculateRMST()` tau |
| 45 | `stratified_cox` | Bool | false | Stratified Cox model | `.cox()` strata() formula |
| 46 | `strata_variable` | Variable (factor) | null | Stratification variable | `.cox()` strata term |
| 47 | `age_adjustment` | Bool | false | Age-adjusted Cox | `.ageAdjustedCox()` |
| 48 | `age_variable` | Variable (numeric) | null | Age covariate | `.ageAdjustedCox()` |
| 49 | `age_interaction` | Bool | false | Age x group interaction | `.ageAdjustedCox()` interaction term |
| 50 | `age_stratified_cox` | Bool | false | Stratify by age groups | `.ageAdjustedCox()` strata |
| 51 | `age_group_cutpoints` | String | '50, 65, 75' | Age group cutpoints | `.ageAdjustedCox()` / `.ageStandardization()` |
| 52 | `age_time_scale` | Bool | false | Age as time axis | `.ageTimeScaleCox()` |
| 53 | `age_standardization` | Bool | false | SMR analysis | `.ageStandardization()` |
| 54 | `age_standardization_method` | List | indirect | Direct/indirect | `.ageStandardization()` |
| 55 | `age_stratified_km` | Bool | false | Age-stratified KM curves | `.plotAgeStratifiedKM()` |
| 56 | `adjusted_curves` | Bool | false | Adjusted survival curves | `.plotAdjustedCurves()` |
| 57 | `remark_checklist` | Bool | false | REMARK checklist | `.generateRemarkChecklist()` |
| 58 | `residual_diagnostics` | Bool | false | Cox residuals | `.calculateResiduals()` |
| 59 | `export_survival_data` | Output | - | Export survival estimates | `.exportSurvivalData()` |
| 60 | `loglog` | Bool | false | Log-log plot | `.plot7()` |
| 61 | `showExplanations` | Bool | false | Show method explanations | `.populateExplanations()` |
| 62 | `showSummaries` | Bool | false | Show NL summaries | `.populateEnhancedClinicalContent()` |
| 63 | `use_parametric` | Bool | false | Parametric models (DISABLED) | `.parametricSurvival()` |
| 64 | `parametric_distribution` | List | weibull | Distribution choice | `.parametricSurvival()` |
| 65 | `parametric_covariates` | Bool | true | Include covariates | `.parametricSurvival()` |
| 66 | `spline_knots` | Integer | 3 | Spline knots count | `.parametricSurvival()` |
| 67 | `spline_scale` | List | hazard | Spline scale | `.parametricSurvival()` |
| 68 | `parametric_extrapolation` | Bool | false | Extrapolation | `.parametricSurvival()` |
| 69 | `extrapolation_time` | Number | 0 | Extrapolation horizon | `.parametricSurvival()` |
| 70 | `parametric_diagnostics` | Bool | true | Parametric diagnostics | `.parametricSurvival()` |
| 71 | `compare_distributions` | Bool | false | Compare distributions | `.parametricSurvival()` |
| 72 | `parametric_survival_plots` | Bool | false | Parametric plots | `.plotParametricSurvival()` |
| 73 | `hazard_plots` | Bool | false | Hazard function plots | `.plotHazardFunction()` |
| 74 | `calibration_curves` | Bool | false | Calibration assessment | `.calculateCalibration()` |
| 75 | `calibration_timepoint` | Number | 0 | Calibration time point | `.calculateCalibration()` |
| 76 | `calibration_ngroups` | Integer | 5 | Risk group count | `.calculateCalibration()` |
| 77 | `rcs_analysis` | Bool | false | Non-linearity assessment | `.calculateRCS()` |
| 78 | `rcs_variable` | Variable (numeric) | null | Continuous predictor | `.calculateRCS()` |
| 79 | `rcs_knots` | Integer | 4 | RCS knot count | `.calculateRCS()` |
| 80 | `bootstrapValidation` | Bool | false | Bootstrap validation | `.calculateBootstrapValidation()` |
| 81 | `bootstrapValN` | Integer | 200 | Bootstrap resamples | `.calculateBootstrapValidation()` |

---

## Results Definition (r.yaml)

Total: ~72 result items defined in `survival.r.yaml`.

### Always-Visible Core Results

| Result Name | Type | Title | Populated By |
|---|---|---|---|
| `subtitle` | Preformatted | Survival Analysis - ${explanatory} | `.run()` |
| `todo` | Html | To Do | `.todo()` |
| `medianSurvivalHeading` | Preformatted | Median Survival Analysis | `.init()` |
| `medianTable` | Table | Median Survival Table | `.medianSurv()` |
| `coxRegressionHeading` | Preformatted | Cox Regression Analysis | `.init()` |
| `coxTable` | Table | Cox Table | `.cox()` |
| `tCoxtext2` | Html | (Cox forest plot HTML) | `.cox()` |
| `survivalTablesHeading` | Preformatted | Survival Probability Tables | `.init()` |
| `survTable` | Table | 1, 3, 5 year Survival | `.survTable()` |

### Conditional: showSummaries

| Result Name | Type | Visibility | Populated By |
|---|---|---|---|
| `medianSummary` | Preformatted | showSummaries | `.medianSurv()` |
| `coxSummary` | Preformatted | showSummaries | `.cox()` |
| `survTableSummary` | Preformatted | showSummaries | `.survTable()` |
| `rmstSummary` | Preformatted | rmst_analysis && showSummaries | `.calculateRMST()` |
| `pairwiseSummary` | Preformatted | pw && showSummaries | `.pairwise()` |
| `weightedLogRankExplanation` | Html | weightedLogRank && showSummaries | `.calculateWeightedLogRank()` |
| `bootstrapValidationExplanation` | Html | bootstrapValidation && showSummaries | `.calculateBootstrapValidation()` |
| `clinicalInterpretationExplanation` | Html | showSummaries | `.populateEnhancedClinicalContent()` |
| `copyReadySentencesExplanation` | Html | showSummaries | `.populateEnhancedClinicalContent()` |

### Conditional: showExplanations

| Result Name | Type | Visibility | Populated By |
|---|---|---|---|
| `medianSurvivalHeading3` | Preformatted | showExplanations | `.init()` |
| `medianSurvivalExplanation` | Html | showExplanations | `.populateExplanations()` |
| `coxRegressionHeading3` | Preformatted | showExplanations | `.init()` |
| `coxRegressionExplanation` | Html | showExplanations | `.populateExplanations()` |
| `survivalTablesHeading3` | Preformatted | showExplanations | `.init()` |
| `survivalTablesExplanation` | Html | showExplanations | `.populateExplanations()` |
| `personTimeExplanation` | Html | person_time && showExplanations | `.populateExplanations()` |
| `rmstExplanation` | Html | rmst_analysis && showExplanations | `.populateExplanations()` |
| `residualDiagnosticsExplanation` | Html | residual_diagnostics && showExplanations | `.populateExplanations()` |
| `survivalPlotsHeading3` | Preformatted | (sc\|ce\|ch\|kmunicate\|loglog) && showExplanations | `.init()` |
| `survivalPlotsExplanation` | Html | (sc\|ce\|ch\|kmunicate\|loglog) && showExplanations | `.populateExplanations()` |
| `calibrationInterpretation` | Html | calibration_curves && showExplanations | `.calculateCalibration()` |
| `rcsInterpretation` | Html | rcs_analysis && showExplanations | `.calculateRCS()` |
| `parametricModelsExplanation` | Html | use_parametric && showExplanations | (DISABLED) |
| `clinicalGlossaryExplanation` | Html | showExplanations | `.populateExplanations()` |
| `ageAdjustedExplanation` | Html | age_adjustment && showExplanations | `.ageAdjustedCox()` |

### Conditional: Feature-Specific Results

#### Age Adjustment (visible when age_adjustment = true)

| Result Name | Type | Populated By |
|---|---|---|
| `ageAdjustedCoxHeading` | Preformatted | `.ageAdjustedCox()` |
| `ageAdjustedCoxTable` | Table (variable, levels, n, hr_unadjusted, hr_age_adjusted) | `.ageAdjustedCox()` |
| `ageInteractionTable` | Table (term, coef, hr, se, z, pvalue) | `.ageAdjustedCox()` |
| `ageAdjustedInterpretation` | Html | `.ageAdjustedCox()` |
| `ageTimeScaleTable` | Table (variable, levels, hr, ci_lower, ci_upper, pvalue) | `.ageTimeScaleCox()` |
| `ageTimeScaleInterpretation` | Html | `.ageTimeScaleCox()` |
| `ageStandardizationTable` | Table (group, observed, expected, smr, ci, pvalue) | `.ageStandardization()` |
| `ageStandardizationInterpretation` | Html | `.ageStandardization()` |
| `ageStratifiedKMPlot` | Image 700x500 | `.plotAgeStratifiedKM()` |
| `adjustedCurvesPlot` | Image 700x500 | `.plotAdjustedCurves()` |

#### Proportional Hazards (visible when ph_cox = true)

| Result Name | Type | Populated By |
|---|---|---|
| `cox_ph` | Preformatted | `.cox()` |
| `phInterpretation` | Html | `.generatePHInterpretation()` |
| `plot8` | Image 600x450 | `.plot8()` |

#### Pairwise Comparisons (visible when pw = true)

| Result Name | Type | Populated By |
|---|---|---|
| `pairwiseComparisonHeading` | Preformatted | `.init()` |
| `pairwiseTable` | Table (rowname, name, value) | `.pairwise()` |
| `pairwiseSummary` | Preformatted | `.pairwise()` |

#### Weighted Log-Rank (visible when weightedLogRank = true)

| Result Name | Type | Populated By |
|---|---|---|
| `weightedLogRankTable` | Table (test, rho, chisq, df, pvalue, weighting) | `.calculateWeightedLogRank()` |

#### Person-Time (visible when person_time = true)

| Result Name | Type | Populated By |
|---|---|---|
| `personTimeHeading` | Preformatted | `.init()` |
| `personTimeTable` | Table (interval, events, person_time, rate, rate_ci) | `.personTimeAnalysis()` |
| `personTimeSummary` | Html | `.personTimeAnalysis()` |

#### RMST (visible when rmst_analysis = true)

| Result Name | Type | Populated By |
|---|---|---|
| `rmstHeading` | Preformatted | `.init()` |
| `rmstTable` | Table (group, rmst, se, ci_lower, ci_upper, tau) | `.calculateRMST()` |

#### Calibration (visible when calibration_curves = true)

| Result Name | Type | Populated By |
|---|---|---|
| `calibrationTable` | Table (metric, value, ci, ideal, interpretation) | `.calculateCalibration()` |
| `calibrationGroupTable` | Table (group, n, events, predicted, observed, ci) | `.calculateCalibration()` |
| `calibrationPlot` | Image 600x500 | `.plotCalibration()` |

#### RCS Non-Linearity (visible when rcs_analysis = true)

| Result Name | Type | Populated By |
|---|---|---|
| `rcsTestTable` | Table (model, df, loglik, aic, lr_chisq, lr_df, p_value, conclusion) | `.calculateRCS()` |
| `rcsPlot` | Image 600x450 | `.plotRCS()` |

#### Bootstrap Validation (visible when bootstrapValidation = true)

| Result Name | Type | Populated By |
|---|---|---|
| `bootstrapValidationTable` | Table (metric, apparent, optimism, corrected, n_bootstrap) | `.calculateBootstrapValidation()` |

#### Residual Diagnostics (visible when residual_diagnostics = true)

| Result Name | Type | Populated By |
|---|---|---|
| `residualsTable` | Table (observation, martingale, deviance, score, schoenfeld) | `.calculateResiduals()` |
| `residualsPlot` | Image 600x450 | `.plot9()` |

#### REMARK Checklist (visible when remark_checklist = true)

| Result Name | Type | Populated By |
|---|---|---|
| `remarkChecklist` | Html | `.generateRemarkChecklist()` |

#### Plots (visible per individual option)

| Result Name | Visible When | renderFun | Size |
|---|---|---|---|
| `plot` | sc | `.plot()` | 600x450 |
| `plot2` | ce | `.plot2()` | 600x450 |
| `plot3` | ch | `.plot3()` | 600x450 |
| `plot6` | kmunicate | `.plot6()` | 600x450 |
| `plot7` | loglog | `.plot7()` | 600x450 |
| `plot8` | ph_cox | `.plot8()` | 600x450 |
| `residualsPlot` | residual_diagnostics | `.plot9()` | 600x450 |
| `calibrationPlot` | calibration_curves | `.plotCalibration()` | 600x500 |
| `rcsPlot` | rcs_analysis | `.plotRCS()` | 600x450 |
| `ageStratifiedKMPlot` | age_stratified_km | `.plotAgeStratifiedKM()` | 700x500 |
| `adjustedCurvesPlot` | adjusted_curves | `.plotAdjustedCurves()` | 700x500 |
| `parametricSurvivalPlot` | use_parametric && parametric_survival_plots | `.plotParametricSurvival()` | 600x450 |
| `hazardFunctionPlot` | use_parametric && hazard_plots | `.plotHazardFunction()` | 600x450 |
| `extrapolationPlot` | use_parametric && parametric_extrapolation | `.plotExtrapolation()` | 600x450 |

#### Parametric Models (DISABLED in current release)

| Result Name | Type | Visibility | Populated By |
|---|---|---|---|
| `parametricModelComparison` | Table | use_parametric && compare_distributions | (DISABLED) |
| `parametricModelSummary` | Table | use_parametric | (DISABLED) |
| `parametricDiagnostics` | Html | use_parametric && parametric_diagnostics | (DISABLED) |
| `extrapolationTable` | Table | use_parametric && parametric_extrapolation | (DISABLED) |

#### Data Exports (Output type)

| Result Name | Type | Populated By |
|---|---|---|
| `calculatedtime` | Output | `.run()` when tint=true |
| `outcomeredefined` | Output | `.run()` when multievent=true |
| `survivalExport` | Output | `.exportSurvivalData()` |
| `survivalExportSummary` | Html | `.exportSurvivalData()` |

---

## Key Feature Groups

### 1. Core Survival (Always Runs)

- **Median Survival**: `.medianSurv()` -- Kaplan-Meier fit, median + CI per group
- **Cox Regression**: `.cox()` -- univariate Cox via `finalfit::finalfit()`, forest plot HTML
- **Survival Table**: `.survTable()` -- 1,3,5-year (or custom cutpoints) survival probabilities
- **Data Cleaning**: `.cleandata()` -- joins time/outcome/factor, applies landmark, renames columns

### 2. Age Adjustment (New Feature)

Activated by `age_adjustment=true` + `age_variable` set. Six sub-features:

| Sub-feature | Option | Method | Output |
|---|---|---|---|
| Age-adjusted HR | age_adjustment | `.ageAdjustedCox()` | ageAdjustedCoxTable |
| Age x Group interaction | age_interaction | `.ageAdjustedCox()` | ageInteractionTable |
| Stratify by age groups | age_stratified_cox | `.ageAdjustedCox()` | (within Cox) |
| Age as time scale | age_time_scale | `.ageTimeScaleCox()` | ageTimeScaleTable |
| Age standardization (SMR) | age_standardization | `.ageStandardization()` | ageStandardizationTable |
| Age-stratified KM | age_stratified_km | `.plotAgeStratifiedKM()` | ageStratifiedKMPlot |
| Adjusted curves | adjusted_curves | `.plotAdjustedCurves()` | adjustedCurvesPlot |

### 3. Competing Risks

Activated by `multievent=true` + `analysistype=compete`. When active:
- Cox regression is **skipped** (competing risks use cumulative incidence)
- `.medianSurv()` uses `.competingRiskCumInc()` instead of standard KM
- Pairwise, person-time, RMST, calibration, RCS, bootstrap are all **skipped**

### 4. Validation & Diagnostics

| Feature | Trigger | Method |
|---|---|---|
| PH assumption | ph_cox | cox.zph within `.cox()` |
| Residual diagnostics | residual_diagnostics | `.calculateResiduals()` |
| Log-log plot | loglog | `.plot7()` |
| Calibration | calibration_curves | `.calculateCalibration()` |
| Bootstrap validation | bootstrapValidation | `.calculateBootstrapValidation()` |
| RCS non-linearity | rcs_analysis | `.calculateRCS()` |

### 5. Plots

All plots receive `(image, ggtheme, theme, ...)`. State is set by `.cleandata()` as `plotData` list containing `name1time`, `name2outcome`, `name3explanatory`, `cleanData`.

### 6. Person-Time Analysis

Calculates incidence rates per interval using Poisson exact CIs. Stratified by `time_intervals` option.

### 7. RMST

Uses `survRM2::rmst2()` if available, else manual area-under-KM-curve calculation. Tau defaults to 75th percentile of follow-up time if not specified.

### 8. Explanations & Reporting

- `.populateExplanations()` -- HTML explanations for each analysis section
- `.populateEnhancedClinicalContent()` -- clinical interpretation, glossary, copy-ready sentences
- `.generateRemarkChecklist()` -- REMARK reporting compliance check

---

## Backend Architecture (survival.b.R)

### Class Structure

```
survivalClass (R6)
  inherits: survivalBase (auto-generated from .yaml)
  private fields:
    .parametric_model, .parametric_model_name, .parametric_results
    .cachedGetData
  private methods:
    .init()                         # Visibility management
    .getData()                      # Raw data fetch + caching
    .todo()                         # Welcome/help HTML
    .validateInputs()               # Input validation
    .populateTableSafely()          # Generic table population helper
    .definemytime()                 # Time variable processing
    .definemyoutcome()              # Outcome variable processing
    .definemyfactor()               # Explanatory variable processing
    .cleandata()                    # Data join + landmark + rename
    .run()                          # Main orchestrator
    .medianSurv()                   # Median survival analysis
    .cox()                          # Cox regression + PH test + residuals
    .survTable()                    # Survival probability tables
    .pairwise()                     # Pairwise group comparisons
    .calculateRMST()                # Restricted Mean Survival Time
    .calculateResiduals()           # Cox model residuals
    .generatePHInterpretation()     # PH test interpretation
    .calculateWeightedLogRank()     # Weighted log-rank tests
    .calculateBootstrapValidation() # Bootstrap internal validation
    .personTimeAnalysis()           # Person-time rates
    .calculateCalibration()         # Calibration assessment
    .calculateRCS()                 # Non-linearity assessment
    .ageAdjustedCox()               # Age-adjusted Cox regression
    .ageTimeScaleCox()              # Age as time scale
    .ageStandardization()           # Age standardization (SMR)
    .generateRemarkChecklist()      # REMARK checklist
    .generateClinicalInterpretation() # Clinical interpretation
    .generateClinicalGlossary()     # Terminology glossary
    .generateCopyReadySentences()   # Copy-ready report sentences
    .populateExplanations()         # Explanation HTML
    .populateEnhancedClinicalContent() # Enhanced content
    .plot() through .plot9()        # Plot render functions
    .plotCalibration()              # Calibration plot
    .plotRCS()                      # RCS spline plot
    .plotParametricSurvival()       # Parametric survival (DISABLED)
    .plotHazardFunction()           # Hazard function (DISABLED)
    .plotExtrapolation()            # Extrapolation (DISABLED)
    .plotAgeStratifiedKM()          # Age-stratified KM curves
    .plotAdjustedCurves()           # Adjusted survival curves
```

### Execution Flow (.run())

1. Reset cached data
2. Validate inputs (`.validateInputs()`)
3. Clean data (`.cleandata()`)
4. Check event count (< 10 = ERROR, 10-19 = caution, 20-49 = note)
5. Median survival (`.medianSurv()`)
6. RMST if enabled (`.calculateRMST()`)
7. Cox regression if not competing risk (`.cox()`)
8. Age-adjusted Cox if enabled (`.ageAdjustedCox()`)
9. Age as time scale if enabled (`.ageTimeScaleCox()`)
10. Age standardization if enabled (`.ageStandardization()`)
11. Survival table (`.survTable()`)
12. Export survival data (`.exportSurvivalData()`)
13. Calibration if enabled (`.calculateCalibration()`)
14. RCS if enabled (`.calculateRCS()`)
15. Pairwise if enabled (`.pairwise()`)
16. Weighted log-rank if enabled (`.calculateWeightedLogRank()`)
17. Bootstrap validation if enabled (`.calculateBootstrapValidation()`)
18. REMARK checklist if enabled (`.generateRemarkChecklist()`)
19. Person-time if enabled (`.personTimeAnalysis()`)
20. Data exports (calculatedtime, outcomeredefined)
21. Explanations (`.populateExplanations()`)
22. Enhanced clinical content (`.populateEnhancedClinicalContent()`)

### Clinical Safety

- Minimum 10 events required (hard stop)
- 10-19 events: caution note on median table
- 20-49 events: moderate note on median table
- All features except core KM/Cox are skipped for competing risk analysis

---

## References (from r.yaml)

- ClinicoPathJamoviModule
- survival (R package)
- survminer
- finalfit
- flexsurv
- cmprsk
- KMunicate / KMunicate2
- survivaltutorial / survivalrwnahhas / survivalrviews / appliedsurvivalanalysisR
