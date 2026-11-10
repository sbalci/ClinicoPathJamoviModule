<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/notice.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Notice
description: "Documentation for the Notice results element, used for informational messages, warnings, and errors in jamovi."
type: article
---

The `Notice` element is used to display informational messages, warnings, or errors to the user within the jamovi results panel. They are particularly useful for alerting users to issues with their data, violations of assumptions, or providing additional context about the analysis.

![Jamovi screenshot](@assets/reference/api/notices-example-1.png)

![Jamovi screenshot](@assets/reference/api/notices-example-2.png)

## Notice Types

The `jmvcore::NoticeType` object provides the following constants for setting the notice type:

- `jmvcore::NoticeType$INFO`: Blue background, informational icon.
- `jmvcore::NoticeType$WARNING`: Yellow background, warning icon.
- `jmvcore::NoticeType$STRONG_WARNING`: Orange background, strong warning icon.
- `jmvcore::NoticeType$ERROR`: Red background, error icon.

## Examples

### 1. Define the Notice in YAML

A `Notice` can be defined in the `.r.yaml` file as part of the results structure.

```yaml
results:
    - name: dataNote
      type: Notice
```

### 2. Implementation in R

#### Static Definition
Setting the content of a notice defined in the YAML:

```r
self$results$dataNote$setContent("Note: 5 cases were excluded due to missing values.")
self$results$dataNote$setType(jmvcore::NoticeType$INFO)
```

#### Dynamic Creation
Notices can also be created dynamically in R and inserted into the results tree.

```r
if (violation) {
    notice <- jmvcore::Notice$new(
        options=self$options,
        name='violationNotice',
        type=jmvcore::NoticeType$STRONG_WARNING
    )
    notice$setContent("Assumption of normality was violated.")
    self$results$insert(1, notice)
}
```
