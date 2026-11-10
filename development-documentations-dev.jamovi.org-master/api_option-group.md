<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/option-group.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Group Option
description: "Documentation for the Group option, used to organize analysis parameters into logical UI sections."
type: article
---

The `Group` option is a container used to organize related options into logical sections within the jamovi analysis panel.

## Description

In the jamovi UI, a `Group` option does not represent a data value itself. Instead, it acts as a visual container (often with a title) for other controls. This is essential for keeping complex analyses organized and readable.

## YAML Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | The unique name of the group. |
| `type` | string | Must be `Group`. |
| `title` | string | The heading displayed above the grouped options. |
| `items` | array | A list of option definitions that belong to this group. |

## R Implementation

The `Group` option has **no direct R implementation** for data processing, as it is a UI-only construct. The options contained within the group are accessed directly via `self$options$optionName`, regardless of their nesting in a group.

## Examples

### 1. Define in YAML

```yaml
- name: optionsGroup
  type: Group
  title: Analysis Options
  items:
    - name: check1
      type: Bool
      title: Perform normality test
    - name: check2
      type: Bool
      title: Perform homogeneity test
```

### 2. Implementation in R

Access the nested options as if they were at the top level:

```r
if (self$options$check1) {
    # Normality test logic
}
```
