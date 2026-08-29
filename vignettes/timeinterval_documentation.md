# Comprehensive Time Interval Calculator - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `timeinterval`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dx_date` | UI Control `dx_date` | `self$options$dx_date` | Output item / Table |
| `fu_date` | UI Control `fu_date` | `self$options$fu_date` | Output item / Table |
| `time_format` | UI Control `time_format` | `self$options$time_format` | Output item / Table |
| `output_unit` | UI Control `output_unit` | `self$options$output_unit` | Output item / Table |
| `time_basis` | UI Control `time_basis` | `self$options$time_basis` | Output item / Table |
| `use_landmark` | UI Control `use_landmark` | `self$options$use_landmark` | Output item / Table |
| `landmark_time` | UI Control `landmark_time` | `self$options$landmark_time` | Output item / Table |
| `remove_negative` | UI Control `remove_negative` | `self$options$remove_negative` | Output item / Table |
| `remove_extreme` | UI Control `remove_extreme` | `self$options$remove_extreme` | Output item / Table |
| `extreme_multiplier` | UI Control `extreme_multiplier` | `self$options$extreme_multiplier` | Output item / Table |
| `add_times` | UI Control `add_times` | `self$options$add_times` | Output item / Table |
| `include_quality_metrics` | UI Control `include_quality_metrics` | `self$options$include_quality_metrics` | Output item / Table |
| `confidence_level` | UI Control `confidence_level` | `self$options$confidence_level` | Output item / Table |
| `show_summary` | UI Control `show_summary` | `self$options$show_summary` | Output item / Table |
| `show_glossary` | UI Control `show_glossary` | `self$options$show_glossary` | Output item / Table |
| `timezone` | UI Control `timezone` | `self$options$timezone` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/timeinterval.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

