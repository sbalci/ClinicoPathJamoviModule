<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/group.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Group
description: "Learn how to use the Group results element to organize and hierarchicalize your jamovi analysis output."
type: article
---

The `Group` element is a container used to organize other results elements (like tables, images, or other groups) into a logical hierarchy.

## Methods

### setStatus(status)

sets the group's status, should be one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### setVisible(visible=TRUE)

overrides the group's default visibility.

### setTitle(title)

sets the group's title.

### setError(message)

sets the group's status to 'error', and assigns the error message.

### setState(object)

sets the state object on the group.

### get(name)

retrieves a child element by its name.

## Examples

### 1. Define the Group in YAML

A `Group` is defined in the `.r.yaml` file and can contain other results elements in its `items` property.

```yaml
- name: descriptives
  title: Descriptives
  type: Group
  items:
    - name: statsTable
      type: Table
      title: Statistics
```

### 2. Access the Group in R

In the `.run()` function, you can access the group and its children:

```r
.run = function() {
    # Access a group named 'descriptives'
    group <- self$results$descriptives
    
    # Access a table inside that group
    table <- group$get('statsTable')
    
    # Alternatively, using the $ operator (shorthand)
    table <- group$statsTable
    
    # Set the group title dynamically
    group$setTitle(paste("Descriptives for", self$options$variable))
}
```
