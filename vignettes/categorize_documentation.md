# Categorize Continuous Variables - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `categorize`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `var` | UI Control `var` | `self$options$var` | Output item / Table |
| `method` | UI Control `method` | `self$options$method` | Output item / Table |
| `nbins` | UI Control `nbins` | `self$options$nbins` | Output item / Table |
| `breaks` | UI Control `breaks` | `self$options$breaks` | Output item / Table |
| `sdmult` | UI Control `sdmult` | `self$options$sdmult` | Output item / Table |
| `labels` | UI Control `labels` | `self$options$labels` | Output item / Table |
| `customlabels` | UI Control `customlabels` | `self$options$customlabels` | Output item / Table |
| `newvarname` | UI Control `newvarname` | `self$options$newvarname` | Output item / Table |
| `addtodata` | UI Control `addtodata` | `self$options$addtodata` | Output item / Table |
| `excludeoutofrange` | UI Control `excludeoutofrange` | `self$options$excludeoutofrange` | Output item / Table |
| `includelowest` | UI Control `includelowest` | `self$options$includelowest` | Output item / Table |
| `rightclosed` | UI Control `rightclosed` | `self$options$rightclosed` | Output item / Table |
| `ordered` | UI Control `ordered` | `self$options$ordered` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `showcode` | UI Control `showcode` | `self$options$showcode` | Output item / Table |
| `showplot` | UI Control `showplot` | `self$options$showplot` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/categorize.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

