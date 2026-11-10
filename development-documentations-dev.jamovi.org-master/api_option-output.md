<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/option-output.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Output Option
description: "Reference for the Output option, which lets a jamovi analysis write values back to the spreadsheet as a new column."
type: article
---

The `Output` option declares that an analysis can write a computed value (e.g. residuals, predicted values) back to the jamovi spreadsheet as a new column.

## Description

In the jamovi UI, an `Output` option is represented as a **Checkbox**. When ticked, jamovi creates a new column in the user's spreadsheet populated with values from the analysis.

This option only captures user intent — the column metadata and the R code that populates it are defined in the companion [Output results element](/api/output) in `.r.yaml`.

## YAML Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | The unique name of the option. Used in R as `self$options$name`. |
| `type` | string | Must be `Output`. |
| `title` | string | The label shown next to the checkbox in the UI. |

The `Output` option has no `default` property; the checkbox is always unchecked by default.

## R Implementation

In R, an `Output` option is represented as a `logical` value (`TRUE` or `FALSE`). It is `TRUE` when the user has ticked the checkbox.

## Examples

### 1. Define in YAML

```yaml
- name: residsOV
  type: Output
  title: Residuals
```

### 2. Implementation in R

```r
if (self$options$residsOV) {
    # User has enabled saving — populate the Output results element
}
```

See the [Output results element](/api/output) reference for how to define the column and write values to it from R.
