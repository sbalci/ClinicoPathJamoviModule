# Outlier Detection - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `outlierdetection`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `method_category` | UI Control `method_category` | `self$options$method_category` | Output item / Table |
| `univariate_methods` | UI Control `univariate_methods` | `self$options$univariate_methods` | Output item / Table |
| `multivariate_methods` | UI Control `multivariate_methods` | `self$options$multivariate_methods` | Output item / Table |
| `composite_threshold` | UI Control `composite_threshold` | `self$options$composite_threshold` | Output item / Table |
| `zscore_threshold` | UI Control `zscore_threshold` | `self$options$zscore_threshold` | Output item / Table |
| `iqr_multiplier` | UI Control `iqr_multiplier` | `self$options$iqr_multiplier` | Output item / Table |
| `confidence_level` | UI Control `confidence_level` | `self$options$confidence_level` | Output item / Table |
| `show_outlier_table` | UI Control `show_outlier_table` | `self$options$show_outlier_table` | Output item / Table |
| `show_method_comparison` | UI Control `show_method_comparison` | `self$options$show_method_comparison` | Output item / Table |
| `show_exclusion_summary` | UI Control `show_exclusion_summary` | `self$options$show_exclusion_summary` | Output item / Table |
| `show_visualization` | UI Control `show_visualization` | `self$options$show_visualization` | Output item / Table |
| `show_interpretation` | UI Control `show_interpretation` | `self$options$show_interpretation` | Output item / Table |
| `sampleThreshold` | UI Control `sampleThreshold` | `self$options$sampleThreshold` | Output item / Table |
| `sampleSize` | UI Control `sampleSize` | `self$options$sampleSize` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/outlierdetection.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

