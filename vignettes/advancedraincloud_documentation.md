# Advanced Raincloud Plot - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `advancedraincloud`
- **Module**: `JJStatsPlotT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `y_var` | UI Control `y_var` | `self$options$y_var` | Output item / Table |
| `x_var` | UI Control `x_var` | `self$options$x_var` | Output item / Table |
| `fill_var` | UI Control `fill_var` | `self$options$fill_var` | Output item / Table |
| `id_var` | UI Control `id_var` | `self$options$id_var` | Output item / Table |
| `cov_var` | UI Control `cov_var` | `self$options$cov_var` | Output item / Table |
| `rain_side` | UI Control `rain_side` | `self$options$rain_side` | Output item / Table |
| `likert_mode` | UI Control `likert_mode` | `self$options$likert_mode` | Output item / Table |
| `show_longitudinal` | UI Control `show_longitudinal` | `self$options$show_longitudinal` | Output item / Table |
| `point_size` | UI Control `point_size` | `self$options$point_size` | Output item / Table |
| `point_alpha` | UI Control `point_alpha` | `self$options$point_alpha` | Output item / Table |
| `violin_alpha` | UI Control `violin_alpha` | `self$options$violin_alpha` | Output item / Table |
| `boxplot_width` | UI Control `boxplot_width` | `self$options$boxplot_width` | Output item / Table |
| `jitter_seed` | UI Control `jitter_seed` | `self$options$jitter_seed` | Output item / Table |
| `color_palette` | UI Control `color_palette` | `self$options$color_palette` | Output item / Table |
| `plot_title` | UI Control `plot_title` | `self$options$plot_title` | Output item / Table |
| `x_label` | UI Control `x_label` | `self$options$x_label` | Output item / Table |
| `y_label` | UI Control `y_label` | `self$options$y_label` | Output item / Table |
| `show_statistics` | UI Control `show_statistics` | `self$options$show_statistics` | Output item / Table |
| `show_comparisons` | UI Control `show_comparisons` | `self$options$show_comparisons` | Output item / Table |
| `show_interpretation` | UI Control `show_interpretation` | `self$options$show_interpretation` | Output item / Table |
| `clinical_cutoff` | UI Control `clinical_cutoff` | `self$options$clinical_cutoff` | Output item / Table |
| `reference_range_min` | UI Control `reference_range_min` | `self$options$reference_range_min` | Output item / Table |
| `reference_range_max` | UI Control `reference_range_max` | `self$options$reference_range_max` | Output item / Table |
| `show_mcid` | UI Control `show_mcid` | `self$options$show_mcid` | Output item / Table |
| `mcid_value` | UI Control `mcid_value` | `self$options$mcid_value` | Output item / Table |
| `show_effect_size` | UI Control `show_effect_size` | `self$options$show_effect_size` | Output item / Table |
| `effect_size_type` | UI Control `effect_size_type` | `self$options$effect_size_type` | Output item / Table |
| `show_change_scores` | UI Control `show_change_scores` | `self$options$show_change_scores` | Output item / Table |
| `baseline_group` | UI Control `baseline_group` | `self$options$baseline_group` | Output item / Table |
| `responder_threshold` | UI Control `responder_threshold` | `self$options$responder_threshold` | Output item / Table |
| `show_sample_size` | UI Control `show_sample_size` | `self$options$show_sample_size` | Output item / Table |
| `show_missing_info` | UI Control `show_missing_info` | `self$options$show_missing_info` | Output item / Table |
| `trial_arms` | UI Control `trial_arms` | `self$options$trial_arms` | Output item / Table |
| `time_labels` | UI Control `time_labels` | `self$options$time_labels` | Output item / Table |
| `population_type` | UI Control `population_type` | `self$options$population_type` | Output item / Table |
| `log_transform` | UI Control `log_transform` | `self$options$log_transform` | Output item / Table |
| `outlier_method` | UI Control `outlier_method` | `self$options$outlier_method` | Output item / Table |
| `show_cv_bands` | UI Control `show_cv_bands` | `self$options$show_cv_bands` | Output item / Table |
| `cv_band_1` | UI Control `cv_band_1` | `self$options$cv_band_1` | Output item / Table |
| `cv_band_2` | UI Control `cv_band_2` | `self$options$cv_band_2` | Output item / Table |
| `p_value_position` | UI Control `p_value_position` | `self$options$p_value_position` | Output item / Table |
| `journal_style` | UI Control `journal_style` | `self$options$journal_style` | Output item / Table |
| `generate_report` | UI Control `generate_report` | `self$options$generate_report` | Output item / Table |
| `include_methods` | UI Control `include_methods` | `self$options$include_methods` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/advancedraincloud.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

