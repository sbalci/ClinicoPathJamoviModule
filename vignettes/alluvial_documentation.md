# Alluvial Diagrams - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `alluvial`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `condensationvar` | UI Control `condensationvar` | `self$options$condensationvar` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `marg` | UI Control `marg` | `self$options$marg` | Output item / Table |
| `fill` | UI Control `fill` | `self$options$fill` | Output item / Table |
| `fillGgalluvial` | UI Control `fillGgalluvial` | `self$options$fillGgalluvial` | Output item / Table |
| `orient` | UI Control `orient` | `self$options$orient` | Output item / Table |
| `usetitle` | UI Control `usetitle` | `self$options$usetitle` | Output item / Table |
| `mytitle` | UI Control `mytitle` | `self$options$mytitle` | Output item / Table |
| `maxvars` | UI Control `maxvars` | `self$options$maxvars` | Output item / Table |
| `colorPalette` | UI Control `colorPalette` | `self$options$colorPalette` | Output item / Table |
| `showCounts` | UI Control `showCounts` | `self$options$showCounts` | Output item / Table |
| `themeStyle` | UI Control `themeStyle` | `self$options$themeStyle` | Output item / Table |
| `enhancedGradients` | UI Control `enhancedGradients` | `self$options$enhancedGradients` | Output item / Table |
| `plotSubtitle` | UI Control `plotSubtitle` | `self$options$plotSubtitle` | Output item / Table |
| `weight` | UI Control `weight` | `self$options$weight` | Output item / Table |
| `sankeyStyle` | UI Control `sankeyStyle` | `self$options$sankeyStyle` | Output item / Table |
| `curveType` | UI Control `curveType` | `self$options$curveType` | Output item / Table |
| `flowDirection` | UI Control `flowDirection` | `self$options$flowDirection` | Output item / Table |
| `engine` | UI Control `engine` | `self$options$engine` | Output item / Table |
| `labelNodes` | UI Control `labelNodes` | `self$options$labelNodes` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/alluvial.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

