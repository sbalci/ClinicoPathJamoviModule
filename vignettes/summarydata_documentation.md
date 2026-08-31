# Summary of Continuous Variables - Feature Mapping

## Feature-to-code map

| Feature | UI/schema option | Backend use | Result item(s) |
| :--- | :--- | :--- | :--- |
| Input data | `data` | `self$data` | all computed outputs |
| Variable selection | `vars` | validation, `dataset[[var]]` | `notices`, `todo`, `text`, `text1`, interpretation panels |
| Distribution diagnostics | `distr` | cached `stats::shapiro.test()`, `moments::skewness()`, `moments::kurtosis()` | `text`, `reportSentences`, `glossary` |
| Display precision | `decimal_places` | `.fmtNum()` and `gt::fmt_number()` | `text`, `text1`, `outlierReport`, `reportSentences` |
| Outlier screening | `outliers` | `.detectOutliers()` with 1.5 x IQR fences | `outlierReport`, `glossary` |
| Draft prose | `report_sentences` | `.generateReportSentences()` | `reportSentences` |

## Missing-data rule

Each variable is summarized from its own non-missing values. The output reports
available and missing counts; no imputation is performed. Missingness above 20%
produces a strong warning because the available observations may not represent
the full dataset.

## Verification checklist

- [x] All declared options are consumed by the backend.
- [x] All backend-populated result items exist in `summarydata.r.yaml`.
- [x] Display precision applies to the text, visual summary, IQR report, and
  draft sentences.
- [x] Shapiro-Wilk conclusions use the unrounded p-value and avoid claiming that
  normality was established.
- [x] IQR bounds are labelled as fences rather than expected/reference ranges.
- [x] Draft sentences disclose missingness and require units and study context.
