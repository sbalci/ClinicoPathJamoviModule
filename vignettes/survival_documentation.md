# Survival Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `survival`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `elapsedtime` | UI Control `elapsedtime` | `self$options$elapsedtime` | Output item / Table |
| `tint` | UI Control `tint` | `self$options$tint` | Output item / Table |
| `dxdate` | UI Control `dxdate` | `self$options$dxdate` | Output item / Table |
| `fudate` | UI Control `fudate` | `self$options$fudate` | Output item / Table |
| `calculatedtime` | UI Control `calculatedtime` | `self$options$calculatedtime` | Output item / Table |
| `explanatory` | UI Control `explanatory` | `self$options$explanatory` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `outcomeLevel` | UI Control `outcomeLevel` | `self$options$outcomeLevel` | Output item / Table |
| `dod` | UI Control `dod` | `self$options$dod` | Output item / Table |
| `dooc` | UI Control `dooc` | `self$options$dooc` | Output item / Table |
| `awd` | UI Control `awd` | `self$options$awd` | Output item / Table |
| `awod` | UI Control `awod` | `self$options$awod` | Output item / Table |
| `analysistype` | UI Control `analysistype` | `self$options$analysistype` | Output item / Table |
| `outcomeredefined` | UI Control `outcomeredefined` | `self$options$outcomeredefined` | Output item / Table |
| `cutp` | UI Control `cutp` | `self$options$cutp` | Output item / Table |
| `timetypedata` | UI Control `timetypedata` | `self$options$timetypedata` | Output item / Table |
| `timetypeoutput` | UI Control `timetypeoutput` | `self$options$timetypeoutput` | Output item / Table |
| `uselandmark` | UI Control `uselandmark` | `self$options$uselandmark` | Output item / Table |
| `landmark` | UI Control `landmark` | `self$options$landmark` | Output item / Table |
| `pw` | UI Control `pw` | `self$options$pw` | Output item / Table |
| `padjustmethod` | UI Control `padjustmethod` | `self$options$padjustmethod` | Output item / Table |
| `weightedLogRank` | UI Control `weightedLogRank` | `self$options$weightedLogRank` | Output item / Table |
| `survivalTestType` | UI Control `survivalTestType` | `self$options$survivalTestType` | Output item / Table |
| `ph_cox` | UI Control `ph_cox` | `self$options$ph_cox` | Output item / Table |
| `sc` | UI Control `sc` | `self$options$sc` | Output item / Table |
| `kmunicate` | UI Control `kmunicate` | `self$options$kmunicate` | Output item / Table |
| `ce` | UI Control `ce` | `self$options$ce` | Output item / Table |
| `ch` | UI Control `ch` | `self$options$ch` | Output item / Table |
| `endplot` | UI Control `endplot` | `self$options$endplot` | Output item / Table |
| `ybegin_plot` | UI Control `ybegin_plot` | `self$options$ybegin_plot` | Output item / Table |
| `yend_plot` | UI Control `yend_plot` | `self$options$yend_plot` | Output item / Table |
| `byplot` | UI Control `byplot` | `self$options$byplot` | Output item / Table |
| `multievent` | UI Control `multievent` | `self$options$multievent` | Output item / Table |
| `ci95` | UI Control `ci95` | `self$options$ci95` | Output item / Table |
| `risktable` | UI Control `risktable` | `self$options$risktable` | Output item / Table |
| `censored` | UI Control `censored` | `self$options$censored` | Output item / Table |
| `pplot` | UI Control `pplot` | `self$options$pplot` | Output item / Table |
| `medianline` | UI Control `medianline` | `self$options$medianline` | Output item / Table |
| `person_time` | UI Control `person_time` | `self$options$person_time` | Output item / Table |
| `time_intervals` | UI Control `time_intervals` | `self$options$time_intervals` | Output item / Table |
| `rate_multiplier` | UI Control `rate_multiplier` | `self$options$rate_multiplier` | Output item / Table |
| `rmst_analysis` | UI Control `rmst_analysis` | `self$options$rmst_analysis` | Output item / Table |
| `rmst_tau` | UI Control `rmst_tau` | `self$options$rmst_tau` | Output item / Table |
| `stratified_cox` | UI Control `stratified_cox` | `self$options$stratified_cox` | Output item / Table |
| `strata_variable` | UI Control `strata_variable` | `self$options$strata_variable` | Output item / Table |
| `age_adjustment` | UI Control `age_adjustment` | `self$options$age_adjustment` | Output item / Table |
| `age_variable` | UI Control `age_variable` | `self$options$age_variable` | Output item / Table |
| `age_interaction` | UI Control `age_interaction` | `self$options$age_interaction` | Output item / Table |
| `age_stratified_cox` | UI Control `age_stratified_cox` | `self$options$age_stratified_cox` | Output item / Table |
| `age_group_cutpoints` | UI Control `age_group_cutpoints` | `self$options$age_group_cutpoints` | Output item / Table |
| `age_time_scale` | UI Control `age_time_scale` | `self$options$age_time_scale` | Output item / Table |
| `age_standardization` | UI Control `age_standardization` | `self$options$age_standardization` | Output item / Table |
| `age_standardization_method` | UI Control `age_standardization_method` | `self$options$age_standardization_method` | Output item / Table |
| `age_stratified_km` | UI Control `age_stratified_km` | `self$options$age_stratified_km` | Output item / Table |
| `adjusted_curves` | UI Control `adjusted_curves` | `self$options$adjusted_curves` | Output item / Table |
| `remark_checklist` | UI Control `remark_checklist` | `self$options$remark_checklist` | Output item / Table |
| `residual_diagnostics` | UI Control `residual_diagnostics` | `self$options$residual_diagnostics` | Output item / Table |
| `export_survival_data` | UI Control `export_survival_data` | `self$options$export_survival_data` | Output item / Table |
| `loglog` | UI Control `loglog` | `self$options$loglog` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `showSummaries` | UI Control `showSummaries` | `self$options$showSummaries` | Output item / Table |
| `use_parametric` | UI Control `use_parametric` | `self$options$use_parametric` | Output item / Table |
| `parametric_distribution` | UI Control `parametric_distribution` | `self$options$parametric_distribution` | Output item / Table |
| `parametric_covariates` | UI Control `parametric_covariates` | `self$options$parametric_covariates` | Output item / Table |
| `spline_knots` | UI Control `spline_knots` | `self$options$spline_knots` | Output item / Table |
| `spline_scale` | UI Control `spline_scale` | `self$options$spline_scale` | Output item / Table |
| `compare_distributions` | UI Control `compare_distributions` | `self$options$compare_distributions` | Output item / Table |
| `parametric_survival_plots` | UI Control `parametric_survival_plots` | `self$options$parametric_survival_plots` | Output item / Table |
| `calibration_curves` | UI Control `calibration_curves` | `self$options$calibration_curves` | Output item / Table |
| `calibration_timepoint` | UI Control `calibration_timepoint` | `self$options$calibration_timepoint` | Output item / Table |
| `calibration_ngroups` | UI Control `calibration_ngroups` | `self$options$calibration_ngroups` | Output item / Table |
| `rcs_analysis` | UI Control `rcs_analysis` | `self$options$rcs_analysis` | Output item / Table |
| `rcs_variable` | UI Control `rcs_variable` | `self$options$rcs_variable` | Output item / Table |
| `rcs_knots` | UI Control `rcs_knots` | `self$options$rcs_knots` | Output item / Table |
| `bootstrapValidation` | UI Control `bootstrapValidation` | `self$options$bootstrapValidation` | Output item / Table |
| `bootstrapValN` | UI Control `bootstrapValN` | `self$options$bootstrapValN` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/survival.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

