# Lollipop Chart - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `lollipop`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `useHighlight` | UI Control `useHighlight` | `self$options$useHighlight` | Output item / Table |
| `highlight` | UI Control `highlight` | `self$options$highlight` | Output item / Table |
| `aggregation` | UI Control `aggregation` | `self$options$aggregation` | Output item / Table |
| `sortBy` | UI Control `sortBy` | `self$options$sortBy` | Output item / Table |
| `orientation` | UI Control `orientation` | `self$options$orientation` | Output item / Table |
| `showValues` | UI Control `showValues` | `self$options$showValues` | Output item / Table |
| `showMean` | UI Control `showMean` | `self$options$showMean` | Output item / Table |
| `colorScheme` | UI Control `colorScheme` | `self$options$colorScheme` | Output item / Table |
| `theme` | UI Control `theme` | `self$options$theme` | Output item / Table |
| `pointSize` | UI Control `pointSize` | `self$options$pointSize` | Output item / Table |
| `lineWidth` | UI Control `lineWidth` | `self$options$lineWidth` | Output item / Table |
| `lineType` | UI Control `lineType` | `self$options$lineType` | Output item / Table |
| `baseline` | UI Control `baseline` | `self$options$baseline` | Output item / Table |
| `conditionalColor` | UI Control `conditionalColor` | `self$options$conditionalColor` | Output item / Table |
| `colorThreshold` | UI Control `colorThreshold` | `self$options$colorThreshold` | Output item / Table |
| `xlabel` | UI Control `xlabel` | `self$options$xlabel` | Output item / Table |
| `ylabel` | UI Control `ylabel` | `self$options$ylabel` | Output item / Table |
| `title` | UI Control `title` | `self$options$title` | Output item / Table |
| `width` | UI Control `width` | `self$options$width` | Output item / Table |
| `height` | UI Control `height` | `self$options$height` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/lollipop.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

