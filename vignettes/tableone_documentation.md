# Table One — feature map

Detailed behavior is in [the developer documentation](tableone-documentation.md).

| Input | UI | Backend | Results affected |
| --- | --- | --- | --- |
| `data` | Active dataset, no separate control | `self$data` | All data-dependent output |
| `vars` | VariablesListBox | `.prepareVariables()` and `.prepareCohort()` | Table, omission notices, quality, summary, report |
| `sty` | ComboBox | `.renderTable()`; t4 eligibility before exclusion | One of `tablestyle1`–`tablestyle4`; cohort-dependent supplementary output |
| `excl` | CheckBox | `.prepareCohort()` calls `jmvcore::naOmit()` after eligibility | Table, quality, summary, report |
| `nonnormal` | CheckBox, t1 only | `print.TableOne(nonnormal = names(data))` | `tablestyle1`; guidance |
| `showSummary` | CheckBox | `.generateSummary()` | `summary`, declaratively visible |
| `showAbout` | CheckBox | `.setAboutContent()`, only when requested | `about`, declaratively visible |
| `showReportSentence` | CheckBox | `.setReportSentence()` after rendering | `reportSentence`, declaratively visible |

Other handlers: `.clearOutputs()` prevents restored output from surviving early
returns; `.checkDataQuality()` populates `assumptions` before the tables in the
result layout; `.normalizeArsenalHtml()` protects text export; `.sourcifyOption()`
and `asSource()` generate reproducible R calls. There are no plot, model or
stratification handlers.

`init()` rejects selected matrix/array/list columns before framework flattening.
`.prepareVariables()` normalizes actual NA factor levels before cohort selection.
`.htmlSafeTableData()` preserves factor codes and missingness while escaping
labels; arsenal rendering checks that missingness is unchanged. Janitor hides
unused levels, and gtsummary labels the counted dichotomous level.
`.formatText()` supports translated named placeholders without interpreting
braces or backslashes supplied in variable names.
