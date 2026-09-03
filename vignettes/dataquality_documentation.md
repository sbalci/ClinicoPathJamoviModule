# Multi-Variable Visual Quality - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `dataquality`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `check_duplicates` | UI Control `check_duplicates` | `self$options$check_duplicates` | Output item / Table |
| `check_missing` | UI Control `check_missing` | `self$options$check_missing` | Output item / Table |
| `row_level_duplicates` | UI Control `row_level_duplicates` | `self$options$row_level_duplicates` | Output item / Table |
| `plot_data_overview` | UI Control `plot_data_overview` | `self$options$plot_data_overview` | Output item / Table |
| `plot_missing_patterns` | UI Control `plot_missing_patterns` | `self$options$plot_missing_patterns` | Output item / Table |
| `plot_data_types` | UI Control `plot_data_types` | `self$options$plot_data_types` | Output item / Table |
| `missing_threshold_visual` | UI Control `missing_threshold_visual` | `self$options$missing_threshold_visual` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showRecommendations` | UI Control `showRecommendations` | `self$options$showRecommendations` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/dataquality.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

