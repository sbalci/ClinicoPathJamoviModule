# Ridge Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjridges`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `x_var` | UI Control `x_var` | `self$options$x_var` | Output item / Table |
| `y_var` | UI Control `y_var` | `self$options$y_var` | Output item / Table |
| `fill_var` | UI Control `fill_var` | `self$options$fill_var` | Output item / Table |
| `facet_var` | UI Control `facet_var` | `self$options$facet_var` | Output item / Table |
| `plot_type` | UI Control `plot_type` | `self$options$plot_type` | Output item / Table |
| `scale` | UI Control `scale` | `self$options$scale` | Output item / Table |
| `bandwidth` | UI Control `bandwidth` | `self$options$bandwidth` | Output item / Table |
| `bandwidth_value` | UI Control `bandwidth_value` | `self$options$bandwidth_value` | Output item / Table |
| `binwidth` | UI Control `binwidth` | `self$options$binwidth` | Output item / Table |
| `add_boxplot` | UI Control `add_boxplot` | `self$options$add_boxplot` | Output item / Table |
| `add_points` | UI Control `add_points` | `self$options$add_points` | Output item / Table |
| `point_alpha` | UI Control `point_alpha` | `self$options$point_alpha` | Output item / Table |
| `add_quantiles` | UI Control `add_quantiles` | `self$options$add_quantiles` | Output item / Table |
| `quantiles` | UI Control `quantiles` | `self$options$quantiles` | Output item / Table |
| `add_mean` | UI Control `add_mean` | `self$options$add_mean` | Output item / Table |
| `add_median` | UI Control `add_median` | `self$options$add_median` | Output item / Table |
| `show_stats` | UI Control `show_stats` | `self$options$show_stats` | Output item / Table |
| `test_type` | UI Control `test_type` | `self$options$test_type` | Output item / Table |
| `p_adjust_method` | UI Control `p_adjust_method` | `self$options$p_adjust_method` | Output item / Table |
| `effsize_type` | UI Control `effsize_type` | `self$options$effsize_type` | Output item / Table |
| `alpha` | UI Control `alpha` | `self$options$alpha` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `custom_colors` | UI Control `custom_colors` | `self$options$custom_colors` | Output item / Table |
| `gradient_low` | UI Control `gradient_low` | `self$options$gradient_low` | Output item / Table |
| `gradient_high` | UI Control `gradient_high` | `self$options$gradient_high` | Output item / Table |
| `fill_ridges` | UI Control `fill_ridges` | `self$options$fill_ridges` | Output item / Table |
| `reverse_order` | UI Control `reverse_order` | `self$options$reverse_order` | Output item / Table |
| `show_fill_legend` | UI Control `show_fill_legend` | `self$options$show_fill_legend` | Output item / Table |
| `show_facet_legend` | UI Control `show_facet_legend` | `self$options$show_facet_legend` | Output item / Table |
| `theme_style` | UI Control `theme_style` | `self$options$theme_style` | Output item / Table |
| `grid_lines` | UI Control `grid_lines` | `self$options$grid_lines` | Output item / Table |
| `expand_panels` | UI Control `expand_panels` | `self$options$expand_panels` | Output item / Table |
| `legend_position` | UI Control `legend_position` | `self$options$legend_position` | Output item / Table |
| `plot_title` | UI Control `plot_title` | `self$options$plot_title` | Output item / Table |
| `plot_subtitle` | UI Control `plot_subtitle` | `self$options$plot_subtitle` | Output item / Table |
| `plot_caption` | UI Control `plot_caption` | `self$options$plot_caption` | Output item / Table |
| `x_label` | UI Control `x_label` | `self$options$x_label` | Output item / Table |
| `y_label` | UI Control `y_label` | `self$options$y_label` | Output item / Table |
| `add_sample_size` | UI Control `add_sample_size` | `self$options$add_sample_size` | Output item / Table |
| `add_density_values` | UI Control `add_density_values` | `self$options$add_density_values` | Output item / Table |
| `custom_annotations` | UI Control `custom_annotations` | `self$options$custom_annotations` | Output item / Table |
| `width` | UI Control `width` | `self$options$width` | Output item / Table |
| `height` | UI Control `height` | `self$options$height` | Output item / Table |
| `clinicalPreset` | UI Control `clinicalPreset` | `self$options$clinicalPreset` | Output item / Table |
| `showAboutPanel` | UI Control `showAboutPanel` | `self$options$showAboutPanel` | Output item / Table |
| `showAssumptions` | UI Control `showAssumptions` | `self$options$showAssumptions` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjridges.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

