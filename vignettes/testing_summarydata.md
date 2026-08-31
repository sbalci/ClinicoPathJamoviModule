# Testing Checklist: Summary of Continuous Variables (`summarydata`)

## Test scenarios

| Scenario | Input | Expected result |
| :--- | :--- | :--- |
| Default execution | Numeric variable with valid observations | Text and visual summaries are populated. |
| Missing values | Numeric variables with different missingness patterns | Variable-specific available-case calculations; no imputation; available and missing counts reported. |
| High missingness | More than 20% missing in a selected variable | Strong warning naming the variable and counts. |
| Very small sample | One or two non-missing observations | Strong warning; SD is identified as undefined for one value; normality is not assessed. |
| All missing | Numeric column containing only `NA` | Variable excluded with a strong warning; no stale output. |
| Non-numeric selection | Factor or character column passed through R | Rejected by the generated variable-type validation or excluded defensively by the backend. |
| Zero-row data | Selected numeric column in a zero-row data frame | `jmvcore::reject()` explains that the dataset has no rows. |
| Special variable names | Names containing spaces, braces, ampersands, or angle brackets | Names remain intact and are HTML-escaped. |
| Precision | `decimal_places` from 0 through 5 | Text, visual summary, outlier fences, and draft sentences use the selected precision. |
| Distribution diagnostics | Constant, small, normal-like, and skewed inputs | Correct Shapiro-Wilk eligibility and cautious interpretation; moments use the documented convention. |
| Outlier screening | Hand-checkable data | 1.5 x IQR fences and flagged values match independent calculations. |
| Rendering fallback | Forced `gtExtras` rendering failure | Numeric fallback appears and a visible notice says inline plots are unavailable. |

## Focused execution

```r
files <- list.files(
  "tests/testthat",
  pattern = "^test-summarydata.*[.]R$",
  full.names = TRUE
)
invisible(lapply(files, testthat::test_file))
```

## Sign-off criteria

- No test failures in the focused suite.
- R and YAML sources parse successfully.
- All result HTML is nonempty where applicable and contains no unescaped user
  labels or invalid state.
- Hand-calculated mean, sample SD, median, quartiles, IQR fences, and missing
  counts agree with the output.
- Clinical wording distinguishes descriptive summaries, outlier screening, and
  reference-interval estimation.
- Translation extraction is rerun when user-facing strings change; completion
  of every locale is a separate release task and is not implied by this check.
