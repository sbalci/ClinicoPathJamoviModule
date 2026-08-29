# Segmented Total Bar Charts - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `jjsegmentedtotalbar`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `analysis_preset` | UI Control `analysis_preset` | `self$options$analysis_preset` | Output item / Table |
| `x_var` | UI Control `x_var` | `self$options$x_var` | Output item / Table |
| `y_var` | UI Control `y_var` | `self$options$y_var` | Output item / Table |
| `fill_var` | UI Control `fill_var` | `self$options$fill_var` | Output item / Table |
| `facet_var` | UI Control `facet_var` | `self$options$facet_var` | Output item / Table |
| `show_plot` | UI Control `show_plot` | `self$options$show_plot` | Output item / Table |
| `plot_type` | UI Control `plot_type` | `self$options$plot_type` | Output item / Table |
| `chart_style` | UI Control `chart_style` | `self$options$chart_style` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `show_percentages` | UI Control `show_percentages` | `self$options$show_percentages` | Output item / Table |
| `percentage_format` | UI Control `percentage_format` | `self$options$percentage_format` | Output item / Table |
| `show_counts` | UI Control `show_counts` | `self$options$show_counts` | Output item / Table |
| `label_threshold` | UI Control `label_threshold` | `self$options$label_threshold` | Output item / Table |
| `orientation` | UI Control `orientation` | `self$options$orientation` | Output item / Table |
| `sort_categories` | UI Control `sort_categories` | `self$options$sort_categories` | Output item / Table |
| `plot_title` | UI Control `plot_title` | `self$options$plot_title` | Output item / Table |
| `x_title` | UI Control `x_title` | `self$options$x_title` | Output item / Table |
| `y_title` | UI Control `y_title` | `self$options$y_title` | Output item / Table |
| `legend_title` | UI Control `legend_title` | `self$options$legend_title` | Output item / Table |
| `legend_position` | UI Control `legend_position` | `self$options$legend_position` | Output item / Table |
| `bar_width` | UI Control `bar_width` | `self$options$bar_width` | Output item / Table |
| `plot_width` | UI Control `plot_width` | `self$options$plot_width` | Output item / Table |
| `plot_height` | UI Control `plot_height` | `self$options$plot_height` | Output item / Table |
| `add_outline` | UI Control `add_outline` | `self$options$add_outline` | Output item / Table |
| `outline_color` | UI Control `outline_color` | `self$options$outline_color` | Output item / Table |
| `export_ready` | UI Control `export_ready` | `self$options$export_ready` | Output item / Table |
| `flerlage_show_labels` | UI Control `flerlage_show_labels` | `self$options$flerlage_show_labels` | Output item / Table |
| `flerlage_label_size` | UI Control `flerlage_label_size` | `self$options$flerlage_label_size` | Output item / Table |
| `flerlage_label_color` | UI Control `flerlage_label_color` | `self$options$flerlage_label_color` | Output item / Table |
| `flerlage_alpha` | UI Control `flerlage_alpha` | `self$options$flerlage_alpha` | Output item / Table |
| `flerlage_box_color` | UI Control `flerlage_box_color` | `self$options$flerlage_box_color` | Output item / Table |
| `y_is_count` | UI Control `y_is_count` | `self$options$y_is_count` | Output item / Table |
| `show_statistical_tests` | UI Control `show_statistical_tests` | `self$options$show_statistical_tests` | Output item / Table |
| `confidence_level` | UI Control `confidence_level` | `self$options$confidence_level` | Output item / Table |
| `exclude_missing` | UI Control `exclude_missing` | `self$options$exclude_missing` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/jjsegmentedtotalbar.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

