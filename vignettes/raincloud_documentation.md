# Raincloud Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `raincloud`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep_var` | UI Control `dep_var` | `self$options$dep_var` | Output item / Table |
| `group_var` | UI Control `group_var` | `self$options$group_var` | Output item / Table |
| `facet_var` | UI Control `facet_var` | `self$options$facet_var` | Output item / Table |
| `color_var` | UI Control `color_var` | `self$options$color_var` | Output item / Table |
| `show_violin` | UI Control `show_violin` | `self$options$show_violin` | Output item / Table |
| `show_boxplot` | UI Control `show_boxplot` | `self$options$show_boxplot` | Output item / Table |
| `show_dots` | UI Control `show_dots` | `self$options$show_dots` | Output item / Table |
| `dots_side` | UI Control `dots_side` | `self$options$dots_side` | Output item / Table |
| `violin_width` | UI Control `violin_width` | `self$options$violin_width` | Output item / Table |
| `box_width` | UI Control `box_width` | `self$options$box_width` | Output item / Table |
| `dots_size` | UI Control `dots_size` | `self$options$dots_size` | Output item / Table |
| `alpha_violin` | UI Control `alpha_violin` | `self$options$alpha_violin` | Output item / Table |
| `alpha_dots` | UI Control `alpha_dots` | `self$options$alpha_dots` | Output item / Table |
| `orientation` | UI Control `orientation` | `self$options$orientation` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `plot_theme` | UI Control `plot_theme` | `self$options$plot_theme` | Output item / Table |
| `plot_title` | UI Control `plot_title` | `self$options$plot_title` | Output item / Table |
| `x_label` | UI Control `x_label` | `self$options$x_label` | Output item / Table |
| `y_label` | UI Control `y_label` | `self$options$y_label` | Output item / Table |
| `show_statistics` | UI Control `show_statistics` | `self$options$show_statistics` | Output item / Table |
| `show_outliers` | UI Control `show_outliers` | `self$options$show_outliers` | Output item / Table |
| `outlier_method` | UI Control `outlier_method` | `self$options$outlier_method` | Output item / Table |
| `normality_test` | UI Control `normality_test` | `self$options$normality_test` | Output item / Table |
| `comparison_test` | UI Control `comparison_test` | `self$options$comparison_test` | Output item / Table |
| `comparison_method` | UI Control `comparison_method` | `self$options$comparison_method` | Output item / Table |
| `adjust_method` | UI Control `adjust_method` | `self$options$adjust_method` | Output item / Table |
| `effect_size` | UI Control `effect_size` | `self$options$effect_size` | Output item / Table |
| `log_transform` | UI Control `log_transform` | `self$options$log_transform` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/raincloud.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

