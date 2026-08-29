# Lowest Expected Value for a fixed sample size - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `kappaSizeFixedN`
- **Module**: `PowerT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `outcome` | UI Control `outcome` | `self$options$outcome` | Output item / Table |
| `kappa0` | UI Control `kappa0` | `self$options$kappa0` | Output item / Table |
| `props` | UI Control `props` | `self$options$props` | Output item / Table |
| `raters` | UI Control `raters` | `self$options$raters` | Output item / Table |
| `alpha` | UI Control `alpha` | `self$options$alpha` | Output item / Table |
| `n` | UI Control `n` | `self$options$n` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/kappaSizeFixedN.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

