# Multivariable Survival Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `multisurvival`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `elapsedtime` | UI Control `elapsedtime` | `self$options$elapsedtime` | Output item / Table |
| `tint` | UI Control `tint` | `self$options$tint` | Output item / Table |
| `dxdate` | UI Control `dxdate` | `self$options$dxdate` | Output item / Table |
| `fudate` | UI Control `fudate` | `self$options$fudate` | Output item / Table |
| `timetypedata` | UI Control `timetypedata` | `self$options$timetypedata` | Output item / Table |
| `timetypeoutput` | UI Control `timetypeoutput` | `self$options$timetypeoutput` | Output item / Table |
| `uselandmark` | UI Control `uselandmark` | `self$options$uselandmark` | Output item / Table |
| `landmark` | UI Control `landmark` | `self$options$landmark` | Output item / Table |
| `calculatedtime` | UI Control `calculatedtime` | `self$options$calculatedtime` | Output item / Table |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `outcomeLevel` | UI Control `outcomeLevel` | `self$options$outcomeLevel` | Output item / Table |
| `dod` | UI Control `dod` | `self$options$dod` | Output item / Table |
| `dooc` | UI Control `dooc` | `self$options$dooc` | Output item / Table |
| `awd` | UI Control `awd` | `self$options$awd` | Output item / Table |
| `awod` | UI Control `awod` | `self$options$awod` | Output item / Table |
| `analysistype` | UI Control `analysistype` | `self$options$analysistype` | Output item / Table |
| `outcomeredefined` | UI Control `outcomeredefined` | `self$options$outcomeredefined` | Output item / Table |
| `explanatory` | UI Control `explanatory` | `self$options$explanatory` | Output item / Table |
| `contexpl` | UI Control `contexpl` | `self$options$contexpl` | Output item / Table |
| `interactions` | UI Control `interactions` | `self$options$interactions` | Output item / Table |
| `multievent` | UI Control `multievent` | `self$options$multievent` | Output item / Table |
| `hr` | UI Control `hr` | `self$options$hr` | Output item / Table |
| `sty` | UI Control `sty` | `self$options$sty` | Output item / Table |
| `ph_cox` | UI Control `ph_cox` | `self$options$ph_cox` | Output item / Table |
| `km` | UI Control `km` | `self$options$km` | Output item / Table |
| `endplot` | UI Control `endplot` | `self$options$endplot` | Output item / Table |
| `byplot` | UI Control `byplot` | `self$options$byplot` | Output item / Table |
| `ci95` | UI Control `ci95` | `self$options$ci95` | Output item / Table |
| `risktable` | UI Control `risktable` | `self$options$risktable` | Output item / Table |
| `censored` | UI Control `censored` | `self$options$censored` | Output item / Table |
| `medianline` | UI Control `medianline` | `self$options$medianline` | Output item / Table |
| `pplot` | UI Control `pplot` | `self$options$pplot` | Output item / Table |
| `cutp` | UI Control `cutp` | `self$options$cutp` | Output item / Table |
| `calculateRiskScore` | UI Control `calculateRiskScore` | `self$options$calculateRiskScore` | Output item / Table |
| `numRiskGroups` | UI Control `numRiskGroups` | `self$options$numRiskGroups` | Output item / Table |
| `plotRiskGroups` | UI Control `plotRiskGroups` | `self$options$plotRiskGroups` | Output item / Table |
| `ci_optimism` | UI Control `ci_optimism` | `self$options$ci_optimism` | Output item / Table |
| `ci_optimism_boot` | UI Control `ci_optimism_boot` | `self$options$ci_optimism_boot` | Output item / Table |
| `addRiskScore` | UI Control `addRiskScore` | `self$options$addRiskScore` | Output item / Table |
| `addRiskGroup` | UI Control `addRiskGroup` | `self$options$addRiskGroup` | Output item / Table |
| `ac` | UI Control `ac` | `self$options$ac` | Output item / Table |
| `adjexplanatory` | UI Control `adjexplanatory` | `self$options$adjexplanatory` | Output item / Table |
| `ac_method` | UI Control `ac_method` | `self$options$ac_method` | Output item / Table |
| `ac_summary` | UI Control `ac_summary` | `self$options$ac_summary` | Output item / Table |
| `showNomogram` | UI Control `showNomogram` | `self$options$showNomogram` | Output item / Table |
| `compare_models` | UI Control `compare_models` | `self$options$compare_models` | Output item / Table |
| `use_stratify` | UI Control `use_stratify` | `self$options$use_stratify` | Output item / Table |
| `stratvar` | UI Control `stratvar` | `self$options$stratvar` | Output item / Table |
| `person_time` | UI Control `person_time` | `self$options$person_time` | Output item / Table |
| `time_intervals` | UI Control `time_intervals` | `self$options$time_intervals` | Output item / Table |
| `rate_multiplier` | UI Control `rate_multiplier` | `self$options$rate_multiplier` | Output item / Table |
| `show_survmetrics` | UI Control `show_survmetrics` | `self$options$show_survmetrics` | Output item / Table |
| `survmetrics_timepoints` | UI Control `survmetrics_timepoints` | `self$options$survmetrics_timepoints` | Output item / Table |
| `survmetrics_show_plots` | UI Control `survmetrics_show_plots` | `self$options$survmetrics_show_plots` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `showSummaries` | UI Control `showSummaries` | `self$options$showSummaries` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/multisurvival.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

