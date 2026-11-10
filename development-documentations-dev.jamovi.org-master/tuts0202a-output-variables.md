<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0202a-output-variables.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Output Variables
type: article
description: "Learn how to write values from your R analysis back to the jamovi spreadsheet as new columns using the Output element."
---

Every result you have produced in this tutorial series so far has gone to the **results panel** — tables and plots that appear on the right-hand side of the jamovi interface. The `Output` element lets you go in a different direction: instead of displaying a value in the results panel, you write it back to the **spreadsheet** as a new column. Concrete examples include ordinary residuals, standardised residuals, predicted values, factor scores, and Mahalanobis distances. In jamovi terminology, a column produced this way is called an **output variable**.

## What is an Output Variable?

When a user enables an output variable (by ticking its checkbox), jamovi appends a new column to their open dataset. The column is tied to the analysis: if the user changes a relevant option, jamovi clears the column and re-populates it on the next run. The column persists in the dataset for the life of the session and can be saved with the file.

From the developer's perspective, you declare what the column looks like (its title, description, and variable type) and then write a small amount of R code that pushes a vector of values into it.

## The Three-File Pattern

Computed columns follow the same three-file structure as the rest of a jamovi analysis. Each file plays a distinct role:

| File | Role |
| :--- | :--- |
| `.a.yaml` | Declares an `Output` option — jamovi creates a checkbox in the UI |
| `.r.yaml` | Defines the column metadata: title, description, variable type, and which options invalidate it |
| `.b.R` | Populates the column at runtime by writing the computed values into the Output element |

## Step-by-Step Example

The following example adds a single `residsOV` residuals column to a hypothetical regression analysis. Each sub-section maps directly to one of the three files above.

### 1. Declare the option in `.a.yaml`

**Add** the following three lines to your `.a.yaml` options list:

```yaml
- name: residsOV
  type: Output
  title: Residuals
```

That is all that is needed here. jamovi recognises `type: Output` and creates a checkbox for saving residuals to the spreadsheet. In R, `self$options$residsOV` will be `TRUE` when the user has ticked the box and `FALSE` otherwise.

### 2. Define the column in `.r.yaml`

**Add** the following block to your `.r.yaml` results list:

```yaml
- name: residsOV
  type: Output
  title: Residuals
  varTitle: Residuals
  varDescription: Ordinary residuals from the fitted model
  measureType: continuous
```

A few details worth noting:

- **`name` must match the option name** in `.a.yaml`. This is how jamovi links the checkbox to the column. It is also how you access the element in R via `self$results$residsOV`.
- **`varTitle`** is the column header written to the spreadsheet (as opposed to `title`, which labels the checkbox in the UI).
- **`measureType: continuous`** marks the new column as a continuous variable in the spreadsheet. `measureType` describes how jamovi classifies the column — use `continuous` for numeric values, `nominal` or `ordinal` for categories, and `id` for identifier columns. This is distinct from the column `type` and format options used in results tables.
### 3. Populate the column in `.b.R`

**Add** the output population code to your `.run()` method, after the main computation:

```r
.run = function() {
    dep  <- self$options$dep
    covs <- self$options$covs
    data <- self$data

    # convert variables to numeric (see Handling Data tutorial)
    data[[dep]] <- jmvcore::toNumeric(data[[dep]])
    for (cov in covs)
        data[[cov]] <- jmvcore::toNumeric(data[[cov]])

    # remove rows with missing values — row names are preserved by na.omit()
    data <- na.omit(data)

    # fit the model
    formula <- jmvcore::constructFormula(dep, covs)
    model   <- lm(formula, data = data)

    # ... populate tables and other results here ...

    # write residuals to the spreadsheet
    if (self$options$residsOV && self$results$residsOV$isNotFilled()) {
        self$results$residsOV$setRowNums(rownames(data))
        self$results$residsOV$setValues(residuals(model))
    }
}
```

## Row Numbers Matter

This is the most common source of bugs with output variables, and it is entirely silent — jamovi will not warn you if you get it wrong.

When a user applies a row filter or jamovi excludes rows containing missing values, those rows are removed from the data frame passed to your R code. However, the remaining rows keep their **original row names** from the full dataset. A data frame with 90 rows after filtering will have row names like `"1"`, `"3"`, `"5"`, ... (skipping the excluded rows), not `"1"`, `"2"`, `"3"`, ....

`setRowNums()` uses these row names to map each computed value back to the correct row in the spreadsheet. If you pass sequential integers instead, every value lands in the wrong row for any dataset that has excluded rows.

> [!IMPORTANT]
> **Always pass `rownames()`, never `1:nrow()`**
>
> `1:nrow(data)` produces the wrong mapping whenever rows have been filtered or excluded. The error is silent — jamovi writes the values without complaint, but they end up in the wrong spreadsheet rows.

```r
# Correct — preserves original row indices through filtering
self$results$residsOV$setRowNums(rownames(data))

# Wrong — breaks silently when any rows are excluded
self$results$residsOV$setRowNums(1:nrow(data))
```

## The `isNotFilled()` Guard

The `isNotFilled()` check prevents redundant computation. If the output column has already been populated in the current run, there is no point recomputing and rewriting the values — so the entire block is skipped.

The complement — *when* jamovi considers an output stale and clears it — is controlled by `clearWith` in `.r.yaml`, which is covered in the [State tutorial](/tutorial/tuts0203-state).

**Next Step:** Now that you can write data back to the spreadsheet, let's look at how to manage [complex analysis state](/tutorial/tuts0203-state).
