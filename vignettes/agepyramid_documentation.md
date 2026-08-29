# Age Pyramid - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `agepyramid`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `age` | UI Control `age` | `self$options$age` | Output item / Table |
| `gender` | UI Control `gender` | `self$options$gender` | Output item / Table |
| `female` | UI Control `female` | `self$options$female` | Output item / Table |
| `male` | UI Control `male` | `self$options$male` | Output item / Table |
| `age_groups` | UI Control `age_groups` | `self$options$age_groups` | Output item / Table |
| `age_interval` | UI Control `age_interval` | `self$options$age_interval` | Output item / Table |
| `bin_width` | UI Control `bin_width` | `self$options$bin_width` | Output item / Table |
| `custom_breaks` | UI Control `custom_breaks` | `self$options$custom_breaks` | Output item / Table |
| `plot_title` | UI Control `plot_title` | `self$options$plot_title` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `female_color` | UI Control `female_color` | `self$options$female_color` | Output item / Table |
| `male_color` | UI Control `male_color` | `self$options$male_color` | Output item / Table |
| `originaltheme` | UI Control `originaltheme` | `self$options$originaltheme` | Output item / Table |
| `enableGGCharts` | UI Control `enableGGCharts` | `self$options$enableGGCharts` | Output item / Table |
| `ggcharts_sort` | UI Control `ggcharts_sort` | `self$options$ggcharts_sort` | Output item / Table |
| `ggcharts_colors` | UI Control `ggcharts_colors` | `self$options$ggcharts_colors` | Output item / Table |
| `ggcharts_color1` | UI Control `ggcharts_color1` | `self$options$ggcharts_color1` | Output item / Table |
| `ggcharts_color2` | UI Control `ggcharts_color2` | `self$options$ggcharts_color2` | Output item / Table |
| `ggcharts_title` | UI Control `ggcharts_title` | `self$options$ggcharts_title` | Output item / Table |
| `ggcharts_xlab` | UI Control `ggcharts_xlab` | `self$options$ggcharts_xlab` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/agepyramid.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

