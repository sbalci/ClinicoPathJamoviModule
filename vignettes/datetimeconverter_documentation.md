# DateTime Converter - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `datetimeconverter`
- **Module**: `SurvivalT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `datetime_var` | UI Control `datetime_var` | `self$options$datetime_var` | Output item / Table |
| `datetime_format` | UI Control `datetime_format` | `self$options$datetime_format` | Output item / Table |
| `timezone` | UI Control `timezone` | `self$options$timezone` | Output item / Table |
| `preview_rows` | UI Control `preview_rows` | `self$options$preview_rows` | Output item / Table |
| `extract_year` | UI Control `extract_year` | `self$options$extract_year` | Output item / Table |
| `extract_month` | UI Control `extract_month` | `self$options$extract_month` | Output item / Table |
| `extract_monthname` | UI Control `extract_monthname` | `self$options$extract_monthname` | Output item / Table |
| `extract_day` | UI Control `extract_day` | `self$options$extract_day` | Output item / Table |
| `extract_hour` | UI Control `extract_hour` | `self$options$extract_hour` | Output item / Table |
| `extract_minute` | UI Control `extract_minute` | `self$options$extract_minute` | Output item / Table |
| `extract_second` | UI Control `extract_second` | `self$options$extract_second` | Output item / Table |
| `extract_dayname` | UI Control `extract_dayname` | `self$options$extract_dayname` | Output item / Table |
| `extract_weeknum` | UI Control `extract_weeknum` | `self$options$extract_weeknum` | Output item / Table |
| `extract_quarter` | UI Control `extract_quarter` | `self$options$extract_quarter` | Output item / Table |
| `extract_dayofyear` | UI Control `extract_dayofyear` | `self$options$extract_dayofyear` | Output item / Table |
| `show_quality_metrics` | UI Control `show_quality_metrics` | `self$options$show_quality_metrics` | Output item / Table |
| `show_summary` | UI Control `show_summary` | `self$options$show_summary` | Output item / Table |
| `show_explanations` | UI Control `show_explanations` | `self$options$show_explanations` | Output item / Table |
| `show_glossary` | UI Control `show_glossary` | `self$options$show_glossary` | Output item / Table |
| `corrected_datetime_char` | UI Control `corrected_datetime_char` | `self$options$corrected_datetime_char` | Output item / Table |
| `corrected_datetime_numeric` | UI Control `corrected_datetime_numeric` | `self$options$corrected_datetime_numeric` | Output item / Table |
| `year_out` | UI Control `year_out` | `self$options$year_out` | Output item / Table |
| `month_out` | UI Control `month_out` | `self$options$month_out` | Output item / Table |
| `monthname_out` | UI Control `monthname_out` | `self$options$monthname_out` | Output item / Table |
| `day_out` | UI Control `day_out` | `self$options$day_out` | Output item / Table |
| `hour_out` | UI Control `hour_out` | `self$options$hour_out` | Output item / Table |
| `minute_out` | UI Control `minute_out` | `self$options$minute_out` | Output item / Table |
| `second_out` | UI Control `second_out` | `self$options$second_out` | Output item / Table |
| `dayname_out` | UI Control `dayname_out` | `self$options$dayname_out` | Output item / Table |
| `weeknum_out` | UI Control `weeknum_out` | `self$options$weeknum_out` | Output item / Table |
| `quarter_out` | UI Control `quarter_out` | `self$options$quarter_out` | Output item / Table |
| `dayofyear_out` | UI Control `dayofyear_out` | `self$options$dayofyear_out` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/datetimeconverter.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

