# Line Chart - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `linechart`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `xvar` | UI Control `xvar` | `self$options$xvar` | Output item / Table |
| `yvar` | UI Control `yvar` | `self$options$yvar` | Output item / Table |
| `groupby` | UI Control `groupby` | `self$options$groupby` | Output item / Table |
| `confidence` | UI Control `confidence` | `self$options$confidence` | Output item / Table |
| `trendline` | UI Control `trendline` | `self$options$trendline` | Output item / Table |
| `points` | UI Control `points` | `self$options$points` | Output item / Table |
| `smooth` | UI Control `smooth` | `self$options$smooth` | Output item / Table |
| `showRefline` | UI Control `showRefline` | `self$options$showRefline` | Output item / Table |
| `refline` | UI Control `refline` | `self$options$refline` | Output item / Table |
| `reflineLabel` | UI Control `reflineLabel` | `self$options$reflineLabel` | Output item / Table |
| `colorPalette` | UI Control `colorPalette` | `self$options$colorPalette` | Output item / Table |
| `theme` | UI Control `theme` | `self$options$theme` | Output item / Table |
| `xlabel` | UI Control `xlabel` | `self$options$xlabel` | Output item / Table |
| `ylabel` | UI Control `ylabel` | `self$options$ylabel` | Output item / Table |
| `title` | UI Control `title` | `self$options$title` | Output item / Table |
| `width` | UI Control `width` | `self$options$width` | Output item / Table |
| `height` | UI Control `height` | `self$options$height` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/linechart.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

