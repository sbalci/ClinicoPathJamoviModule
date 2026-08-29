# Single Variable Quality Check - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `checkdata`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `var` | UI Control `var` | `self$options$var` | Output item / Table |
| `showOutliers` | UI Control `showOutliers` | `self$options$showOutliers` | Output item / Table |
| `showDistribution` | UI Control `showDistribution` | `self$options$showDistribution` | Output item / Table |
| `showDuplicates` | UI Control `showDuplicates` | `self$options$showDuplicates` | Output item / Table |
| `showPatterns` | UI Control `showPatterns` | `self$options$showPatterns` | Output item / Table |
| `rareCategoryThreshold` | UI Control `rareCategoryThreshold` | `self$options$rareCategoryThreshold` | Output item / Table |
| `clinicalValidation` | UI Control `clinicalValidation` | `self$options$clinicalValidation` | Output item / Table |
| `unitSystem` | UI Control `unitSystem` | `self$options$unitSystem` | Output item / Table |
| `outlierTransform` | UI Control `outlierTransform` | `self$options$outlierTransform` | Output item / Table |
| `mcarTest` | UI Control `mcarTest` | `self$options$mcarTest` | Output item / Table |
| `cvMinMean` | UI Control `cvMinMean` | `self$options$cvMinMean` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showAbout` | UI Control `showAbout` | `self$options$showAbout` | Output item / Table |
| `showCaveats` | UI Control `showCaveats` | `self$options$showCaveats` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/checkdata.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

