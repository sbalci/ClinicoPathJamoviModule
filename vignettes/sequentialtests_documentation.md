# Sequential Testing Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `sequentialtests`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `preset` | UI Control `preset` | `self$options$preset` | Output item / Table |
| `test1_name` | UI Control `test1_name` | `self$options$test1_name` | Output item / Table |
| `test1_sens` | UI Control `test1_sens` | `self$options$test1_sens` | Output item / Table |
| `test1_spec` | UI Control `test1_spec` | `self$options$test1_spec` | Output item / Table |
| `test1_cost` | UI Control `test1_cost` | `self$options$test1_cost` | Output item / Table |
| `test2_name` | UI Control `test2_name` | `self$options$test2_name` | Output item / Table |
| `test2_sens` | UI Control `test2_sens` | `self$options$test2_sens` | Output item / Table |
| `test2_spec` | UI Control `test2_spec` | `self$options$test2_spec` | Output item / Table |
| `test2_cost` | UI Control `test2_cost` | `self$options$test2_cost` | Output item / Table |
| `strategy` | UI Control `strategy` | `self$options$strategy` | Output item / Table |
| `prevalence` | UI Control `prevalence` | `self$options$prevalence` | Output item / Table |
| `population_size` | UI Control `population_size` | `self$options$population_size` | Output item / Table |
| `show_explanation` | UI Control `show_explanation` | `self$options$show_explanation` | Output item / Table |
| `show_formulas` | UI Control `show_formulas` | `self$options$show_formulas` | Output item / Table |
| `show_cost_analysis` | UI Control `show_cost_analysis` | `self$options$show_cost_analysis` | Output item / Table |
| `show_plots` | UI Control `show_plots` | `self$options$show_plots` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/sequentialtests.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

