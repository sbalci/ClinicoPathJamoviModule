<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/preformatted.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Preformatted
description: "Learn how to use the Preformatted results element for raw, monospaced text and R console output."
type: article
---

The `Preformatted` element is used for displaying raw, monospaced text. This is useful for showing R console output, model summaries, or any text where whitespace and alignment are important.

## Methods

### setStatus(status)

sets the element's status, should be one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### setVisible(visible=TRUE)

overrides the element's default visibility.

### setTitle(title)

sets the element's title.

### setError(message)

sets the element's status to 'error', and assigns the error message.

### setState(object)

sets the state object on the element.

### setContent(content)

sets the text content of the element. `content` should be a string.

## Examples

### 1. Define the Preformatted element in YAML

A `Preformatted` element is defined in the `.r.yaml` file:

```yaml
- name: debugOutput
  title: Model Summary
  type: Preformatted
```

### 2. Set Content in R

In the `.run()` function, you can capture R output and display it in a `Preformatted` element:

```r
.run = function() {
    # Access a preformatted element named 'debugOutput'
    pre <- self$results$debugOutput
    
    # Perform some calculation and capture the output
    model <- lm(y ~ x, data = self$data)
    output <- capture.output(summary(model))
    
    # Join the lines and set the content
    pre$setContent(paste(output, collapse="\n"))
}
```
