# Waffle Charts - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jwaffle`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `counts` | UI Control `counts` | `self$options$counts` | Output item / Table |
| `groups` | UI Control `groups` | `self$options$groups` | Output item / Table |
| `facet` | UI Control `facet` | `self$options$facet` | Output item / Table |
| `rows` | UI Control `rows` | `self$options$rows` | Output item / Table |
| `flip` | UI Control `flip` | `self$options$flip` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `show_legend` | UI Control `show_legend` | `self$options$show_legend` | Output item / Table |
| `mytitle` | UI Control `mytitle` | `self$options$mytitle` | Output item / Table |
| `legendtitle` | UI Control `legendtitle` | `self$options$legendtitle` | Output item / Table |
| `showSummaries` | UI Control `showSummaries` | `self$options$showSummaries` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jwaffle.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

