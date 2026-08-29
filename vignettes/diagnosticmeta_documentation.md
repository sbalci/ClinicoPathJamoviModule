# Diagnostic Test Meta-Analysis for Pathology - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `diagnosticmeta`
- **Module**: `OncoPath`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `study` | UI Control `study` | `self$options$study` | Output item / Table |
| `true_positives` | UI Control `true_positives` | `self$options$true_positives` | Output item / Table |
| `false_positives` | UI Control `false_positives` | `self$options$false_positives` | Output item / Table |
| `false_negatives` | UI Control `false_negatives` | `self$options$false_negatives` | Output item / Table |
| `true_negatives` | UI Control `true_negatives` | `self$options$true_negatives` | Output item / Table |
| `covariate` | UI Control `covariate` | `self$options$covariate` | Output item / Table |
| `bivariate_analysis` | UI Control `bivariate_analysis` | `self$options$bivariate_analysis` | Output item / Table |
| `hsroc_analysis` | UI Control `hsroc_analysis` | `self$options$hsroc_analysis` | Output item / Table |
| `meta_regression` | UI Control `meta_regression` | `self$options$meta_regression` | Output item / Table |
| `heterogeneity_analysis` | UI Control `heterogeneity_analysis` | `self$options$heterogeneity_analysis` | Output item / Table |
| `publication_bias` | UI Control `publication_bias` | `self$options$publication_bias` | Output item / Table |
| `confidence_level` | UI Control `confidence_level` | `self$options$confidence_level` | Output item / Table |
| `method` | UI Control `method` | `self$options$method` | Output item / Table |
| `zero_cell_correction` | UI Control `zero_cell_correction` | `self$options$zero_cell_correction` | Output item / Table |
| `forest_plot` | UI Control `forest_plot` | `self$options$forest_plot` | Output item / Table |
| `sroc_plot` | UI Control `sroc_plot` | `self$options$sroc_plot` | Output item / Table |
| `funnel_plot` | UI Control `funnel_plot` | `self$options$funnel_plot` | Output item / Table |
| `show_individual_studies` | UI Control `show_individual_studies` | `self$options$show_individual_studies` | Output item / Table |
| `show_interpretation` | UI Control `show_interpretation` | `self$options$show_interpretation` | Output item / Table |
| `show_methodology` | UI Control `show_methodology` | `self$options$show_methodology` | Output item / Table |
| `show_analysis_summary` | UI Control `show_analysis_summary` | `self$options$show_analysis_summary` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `show_plot_explanations` | UI Control `show_plot_explanations` | `self$options$show_plot_explanations` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/diagnosticmeta.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

