<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/html.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Html
description: "Reference for the Html results element, allowing for rich, custom HTML output in jamovi analyses."
type: article
---

The `Html` element is used for displaying rich HTML output. This allows for custom formatting, links, and other HTML features that are not available in standard jamovi tables or text elements.

> [!WARNING]
> While `Html` elements provide great flexibility, they should be used sparingly to maintain a consistent look and feel across jamovi analyses. For standard statistical results, `Table` and `Image` elements are preferred. For narrative or explanatory text, prefer [Text](/api/text), which supports basic inline formatting without the overhead and inconsistency of hand-rolled HTML.

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

sets the HTML content of the element. `content` should be a string containing valid HTML.

## Examples

### 1. Define the Html element in YAML

An `Html` element is defined in the `.r.yaml` file:

```yaml
- name: summaryHtml
  title: Analysis Summary
  type: Html
```

### 2. Set Content in R

In the `.run()` function, you can generate HTML content and display it:

```r
.run = function() {
    # Access an HTML element named 'summaryHtml'
    html <- self$results$summaryHtml
    
    # Create some HTML content
    content <- "
        <h3>Analysis Summary</h3>
        <p>The results indicate a <b>significant</b> effect.</p>
        <ul>
            <li>Factor A: p < .001</li>
            <li>Factor B: p = .045</li>
        </ul>
    "
    
    # Set the content
    html$setContent(content)
}
```
