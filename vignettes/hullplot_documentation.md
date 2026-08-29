# Hull Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `hullplot`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `x_var` | UI Control `x_var` | `self$options$x_var` | Output item / Table |
| `y_var` | UI Control `y_var` | `self$options$y_var` | Output item / Table |
| `group_var` | UI Control `group_var` | `self$options$group_var` | Output item / Table |
| `color_var` | UI Control `color_var` | `self$options$color_var` | Output item / Table |
| `size_var` | UI Control `size_var` | `self$options$size_var` | Output item / Table |
| `hull_concavity` | UI Control `hull_concavity` | `self$options$hull_concavity` | Output item / Table |
| `hull_alpha` | UI Control `hull_alpha` | `self$options$hull_alpha` | Output item / Table |
| `show_labels` | UI Control `show_labels` | `self$options$show_labels` | Output item / Table |
| `point_size` | UI Control `point_size` | `self$options$point_size` | Output item / Table |
| `point_alpha` | UI Control `point_alpha` | `self$options$point_alpha` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `plot_theme` | UI Control `plot_theme` | `self$options$plot_theme` | Output item / Table |
| `plot_title` | UI Control `plot_title` | `self$options$plot_title` | Output item / Table |
| `x_label` | UI Control `x_label` | `self$options$x_label` | Output item / Table |
| `y_label` | UI Control `y_label` | `self$options$y_label` | Output item / Table |
| `hull_expand` | UI Control `hull_expand` | `self$options$hull_expand` | Output item / Table |
| `show_statistics` | UI Control `show_statistics` | `self$options$show_statistics` | Output item / Table |
| `outlier_detection` | UI Control `outlier_detection` | `self$options$outlier_detection` | Output item / Table |
| `confidence_ellipses` | UI Control `confidence_ellipses` | `self$options$confidence_ellipses` | Output item / Table |
| `show_summary` | UI Control `show_summary` | `self$options$show_summary` | Output item / Table |
| `show_assumptions` | UI Control `show_assumptions` | `self$options$show_assumptions` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/hullplot.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

