# Survival Analysis for Continuous Variable - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `survivalcont`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `elapsedtime` | UI Control `elapsedtime` | `self$options$elapsedtime` | Output item / Table |
| `tint` | UI Control `tint` | `self$options$tint` | Output item / Table |
| `dxdate` | UI Control `dxdate` | `self$options$dxdate` | Output item / Table |
| `fudate` | UI Control `fudate` | `self$options$fudate` | Output item / Table |
| `calculatedtime` | UI Control `calculatedtime` | `self$options$calculatedtime` | Output item / Table |
| `contexpl` | UI Control `contexpl` | `self$options$contexpl` | Output item / Table |
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
| `sc` | UI Control `sc` | `self$options$sc` | Output item / Table |
| `kmunicate` | UI Control `kmunicate` | `self$options$kmunicate` | Output item / Table |
| `ce` | UI Control `ce` | `self$options$ce` | Output item / Table |
| `ch` | UI Control `ch` | `self$options$ch` | Output item / Table |
| `endplot` | UI Control `endplot` | `self$options$endplot` | Output item / Table |
| `ybegin_plot` | UI Control `ybegin_plot` | `self$options$ybegin_plot` | Output item / Table |
| `yend_plot` | UI Control `yend_plot` | `self$options$yend_plot` | Output item / Table |
| `byplot` | UI Control `byplot` | `self$options$byplot` | Output item / Table |
| `findcut` | UI Control `findcut` | `self$options$findcut` | Output item / Table |
| `multiple_cutoffs` | UI Control `multiple_cutoffs` | `self$options$multiple_cutoffs` | Output item / Table |
| `num_cutoffs` | UI Control `num_cutoffs` | `self$options$num_cutoffs` | Output item / Table |
| `cutoff_method` | UI Control `cutoff_method` | `self$options$cutoff_method` | Output item / Table |
| `min_group_size` | UI Control `min_group_size` | `self$options$min_group_size` | Output item / Table |
| `calculatedcutoff` | UI Control `calculatedcutoff` | `self$options$calculatedcutoff` | Output item / Table |
| `calculatedmulticut` | UI Control `calculatedmulticut` | `self$options$calculatedmulticut` | Output item / Table |
| `multievent` | UI Control `multievent` | `self$options$multievent` | Output item / Table |
| `ci95` | UI Control `ci95` | `self$options$ci95` | Output item / Table |
| `risktable` | UI Control `risktable` | `self$options$risktable` | Output item / Table |
| `censored` | UI Control `censored` | `self$options$censored` | Output item / Table |
| `medianline` | UI Control `medianline` | `self$options$medianline` | Output item / Table |
| `person_time` | UI Control `person_time` | `self$options$person_time` | Output item / Table |
| `time_intervals` | UI Control `time_intervals` | `self$options$time_intervals` | Output item / Table |
| `rate_multiplier` | UI Control `rate_multiplier` | `self$options$rate_multiplier` | Output item / Table |
| `rmst_analysis` | UI Control `rmst_analysis` | `self$options$rmst_analysis` | Output item / Table |
| `rmst_tau` | UI Control `rmst_tau` | `self$options$rmst_tau` | Output item / Table |
| `residual_diagnostics` | UI Control `residual_diagnostics` | `self$options$residual_diagnostics` | Output item / Table |
| `stratified_cox` | UI Control `stratified_cox` | `self$options$stratified_cox` | Output item / Table |
| `strata_variable` | UI Control `strata_variable` | `self$options$strata_variable` | Output item / Table |
| `loglog` | UI Control `loglog` | `self$options$loglog` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `showSummaries` | UI Control `showSummaries` | `self$options$showSummaries` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/survivalcont.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

