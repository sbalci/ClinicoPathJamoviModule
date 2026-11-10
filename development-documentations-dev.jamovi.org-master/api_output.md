<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/output.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Output
description: "Reference for the Output results element, which defines the column metadata and R methods for writing computed values back to the jamovi spreadsheet."
type: article
---

The `Output` element is declared in `.r.yaml` and defines *what* gets written to the spreadsheet. Its companion [`Output` option](/api/option-output) is declared in `.a.yaml` and controls the Save checkbox in the UI. Together they form the save-to-spreadsheet feature: the option in `.a.yaml` captures the user's intent, while this element specifies what the column looks like and how it is populated.

> **Note:** The Output element writes to the **spreadsheet**, not the results panel. Values only appear when the user has ticked the corresponding Save checkbox.

## YAML Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `name` | string | — | Internal name. Must match the `Output` option name in `.a.yaml`. Accessed in R as `self$results$name`. |
| `type` | string | — | Must be `Output`. |
| `title` | string | — | Display label shown in the Save section of the analysis UI. |
| `varTitle` | string | — | Column header written to the spreadsheet. |
| `varDescription` | string | — | Tooltip or description for the column in the spreadsheet. |
| `measureType` | string | `continuous` | Variable type for the new column. One of `continuous`, `nominal`, `ordinal`, or `id`. |
| `clearWith` | list | `*` | List of option names whose change invalidates this output. Defaults to `*` (cleared by any option change). |
| `items` | integer or expression | `1` | Number of output columns to produce. Defaults to `1`. For multi-column output, set to the number of columns or an R expression evaluated against the options (e.g. `(length(factors))`). |

## R Methods

All methods are accessed via `self$results$name`, where `name` matches the `name` property in `.r.yaml`.

| Method | Description |
|--------|-------------|
| `setRowNums(rowNums)` | Sets which spreadsheet rows to write to. **Must be `rownames(data)`, not `1:nrow(data)`** — see important note below. |
| `setValues(values, index, key)` | Sets the vector of values for the column. For a single output (`items: 1`) pass a plain vector. For multi-output use `index` (integer position) or `key` (name) to target a specific column. |
| `isFilled(key)` | Returns `TRUE` if this output has already been populated in the current run. |
| `isNotFilled(key)` | Returns `TRUE` if this output has **not** yet been populated. Use as a guard to avoid redundant computation. |
| `set(keys, titles, descriptions, measureTypes)` | Configures all columns at once. Each argument is a vector with one entry per column. Convenient for dynamic multi-column output where the number of columns depends on the data or options. |
| `setTitle(title, index, key)` | Overrides `varTitle` at runtime for a single column. |
| `setDescription(desc, index, key)` | Overrides `varDescription` at runtime for a single column. |

**Note on `key` and `index`:** These parameters are only relevant when `items > 1` (multi-column output). For the default single-output case (`items: 1`) you can omit them entirely — e.g. `self$results$myOutput$setValues(myVector)` is sufficient.

> [!IMPORTANT]
> **`setRowNums` must receive `rownames()`, not `1:n`**
>
> When jamovi applies row filters or handles missing values, it drops rows from the cleaned data frame but **preserves the original row names**. If you pass `1:nrow(data)` instead of `rownames(data)`, the values are written to the wrong spreadsheet rows — silently, with no error. Always use:
>
> ```r
> self$results$residsOV$setRowNums(rownames(data))
> ```

## Examples

### 1. Define in `.r.yaml`

```yaml
- name: residsOV
  type: Output
  title: Residuals
  varTitle: Residuals
  varDescription: Ordinary residuals from the fitted model
  measureType: continuous
  clearWith:
    - dep
    - factors
```

### 2. Populate in `.b.R`

```r
if (self$options$residsOV && self$results$residsOV$isNotFilled()) {
    self$results$residsOV$setRowNums(rownames(data))
    self$results$residsOV$setValues(residuals(model))
}
```

Here `data` is the cleaned data frame (after `na.omit()`) and `model` is the fitted model — both computed earlier in `.run()`. See the [Output Variables tutorial](/tutorial/tuts0202a-output-variables) for a full worked example.

### 3. Configure multiple columns with `set()`

When the number of output columns depends on the data or options (`items` set to an expression), use `set()` to define all columns in one call rather than overriding each `title` and `description` individually:

```r
keys         <- seq_len(nModels)
titles       <- vapply(keys, function(k) paste('Residuals', k), '')
descriptions <- vapply(keys, function(k) paste('Residuals of model', k), '')
measureTypes <- rep('continuous', nModels)

self$results$residsOV$set(keys, titles, descriptions, measureTypes)
```

Each argument is a vector with one entry per column. After `set()`, populate each column by passing its `key` or `index` to `setValues()`.
