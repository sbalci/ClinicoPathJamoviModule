# IHC Heterogeneity Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `ihcheterogeneity`
- **Module**: `OncoPath`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `wholesection` | UI Control `wholesection` | `self$options$wholesection` | Output item / Table |
| `biopsy1` | UI Control `biopsy1` | `self$options$biopsy1` | Output item / Table |
| `biopsy2` | UI Control `biopsy2` | `self$options$biopsy2` | Output item / Table |
| `biopsy3` | UI Control `biopsy3` | `self$options$biopsy3` | Output item / Table |
| `biopsy4` | UI Control `biopsy4` | `self$options$biopsy4` | Output item / Table |
| `biopsies` | UI Control `biopsies` | `self$options$biopsies` | Output item / Table |
| `spatial_id` | UI Control `spatial_id` | `self$options$spatial_id` | Output item / Table |
| `compareCompartments` | UI Control `compareCompartments` | `self$options$compareCompartments` | Output item / Table |
| `compartmentTests` | UI Control `compartmentTests` | `self$options$compartmentTests` | Output item / Table |
| `analysis_type` | UI Control `analysis_type` | `self$options$analysis_type` | Output item / Table |
| `sampling_strategy` | UI Control `sampling_strategy` | `self$options$sampling_strategy` | Output item / Table |
| `cv_threshold` | UI Control `cv_threshold` | `self$options$cv_threshold` | Output item / Table |
| `correlation_threshold` | UI Control `correlation_threshold` | `self$options$correlation_threshold` | Output item / Table |
| `show_variability_plots` | UI Control `show_variability_plots` | `self$options$show_variability_plots` | Output item / Table |
| `variance_components` | UI Control `variance_components` | `self$options$variance_components` | Output item / Table |
| `power_analysis` | UI Control `power_analysis` | `self$options$power_analysis` | Output item / Table |
| `generate_recommendations` | UI Control `generate_recommendations` | `self$options$generate_recommendations` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showGlossary` | UI Control `showGlossary` | `self$options$showGlossary` | Output item / Table |
| `showReportSentences` | UI Control `showReportSentences` | `self$options$showReportSentences` | Output item / Table |
| `showAssumptions` | UI Control `showAssumptions` | `self$options$showAssumptions` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/ihcheterogeneity.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

