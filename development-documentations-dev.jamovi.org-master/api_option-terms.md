<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/option-terms.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Terms Option
description: "Technical reference for the Terms option, used for model building, main effects, and interactions."
type: article
---

The `Terms` option is used to define model structures, including main effects and interactions between variables.

## Description

In the jamovi UI, the `Terms` option is typically paired with a `ListBox` and a `TermLabel`. It allows users to construct complex models (e.g., ANOVA, Linear Regression) by combining variables into interaction terms.

## YAML Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | The unique name of the option. |
| `type` | string | Must be `Terms`. |
| `title` | string | The label displayed in the UI. |
| `sources` | array | A list of option names (of type `Variable` or `Variables`) that provide the components for these terms. |

## R Implementation

In R, a `Terms` option is represented as a **list of character vectors**. Each character vector in the list represents a single term (e.g., `list("A", "B", c("A", "B"))` for main effects A and B, and their interaction).

## Examples

### 1. Define in YAML

```yaml
- name: modelTerms
  type: Terms
  title: Model Terms
  sources:
    - factors
    - covariates
```

### 2. Implementation in R

Access the terms using `self$options$name`:

```r
terms <- self$options$modelTerms

# Example: Constructing a formula string
formula_parts <- sapply(terms, function(term) {
    paste0(jmvcore::composeTerm(term))
})
full_formula <- paste0("dep ~ ", paste(formula_parts, collapse=" + "))
```
