# Co-Testing Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `cotest`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `test1_name` | UI Control `test1_name` | `self$options$test1_name` | Output item / Table |
| `test2_name` | UI Control `test2_name` | `self$options$test2_name` | Output item / Table |
| `test1_sens` | UI Control `test1_sens` | `self$options$test1_sens` | Output item / Table |
| `test1_spec` | UI Control `test1_spec` | `self$options$test1_spec` | Output item / Table |
| `test2_sens` | UI Control `test2_sens` | `self$options$test2_sens` | Output item / Table |
| `test2_spec` | UI Control `test2_spec` | `self$options$test2_spec` | Output item / Table |
| `indep` | UI Control `indep` | `self$options$indep` | Output item / Table |
| `cond_dep_pos` | UI Control `cond_dep_pos` | `self$options$cond_dep_pos` | Output item / Table |
| `cond_dep_neg` | UI Control `cond_dep_neg` | `self$options$cond_dep_neg` | Output item / Table |
| `prevalence` | UI Control `prevalence` | `self$options$prevalence` | Output item / Table |
| `showGuidance` | UI Control `showGuidance` | `self$options$showGuidance` | Output item / Table |
| `fnote` | UI Control `fnote` | `self$options$fnote` | Output item / Table |
| `fagan` | UI Control `fagan` | `self$options$fagan` | Output item / Table |
| `preset` | UI Control `preset` | `self$options$preset` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/cotest.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

