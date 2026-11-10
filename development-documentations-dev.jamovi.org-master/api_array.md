<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/reference/api/array.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: Array
description: "Detailed reference for the jamovi Array results element, used for repeating output elements like tables or plots."
type: article
---

The `Array` element is a dynamic container used for repeating elements. It is particularly useful when you need to generate a set of results (like a table or a plot) for each level of a factor or for each variable in a list.

The values of properties can be accessed using the `$` operator. For example, to retrieve the title of an array, one can go:

```r
array$title
```

The methods of an array object are called using the `$` operator as well. For example:

```r
array$setVisible(FALSE)
```

## Properties

### name

a string specifying the name of the array.

### title

a string specifying the title of the array.

### visible

`TRUE` or `FALSE`, whether the array is visible or not.

### status

a string, one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### state

the state object associated with the array.

## Methods

### setStatus(status)

sets the array's status, should be one of `'complete'`, `'error'`, `'inited'`, `'running'`.

### setVisible(visible=TRUE)

overrides the array's default visibility.

### setTitle(title)

sets the array's title.

### setError(message)

sets the array's status to 'error', and assigns the error message.

### setState(object)

sets the state object on the array.

### addItem(key)

adds a new item to the array. The `key` should be a unique identifier for this item. This method returns the newly created element (the template defined in `.r.yaml`).

### get(key)

retrieves an item from the array by its `key`.

## Examples

### 1. Define the Array in YAML

In your `.r.yaml` file, you define an `Array` and provide an `items` template. This template defines what each element in the array will look like (e.g., a `Table`).

```yaml
- name: modelResults
  title: Model Results
  type: Array
  items:
    type: Table
    title: Results for Variable
    columns:
      - name: var
        title: Variable
        type: text
      - name: value
        title: Value
        type: number
```

### 2. Implementation in R

In the `.run()` function of your `.b.R` file, you can dynamically add items to the array using unique **keys**. Keys are typically strings (like variable names or factor levels) that uniquely identify each element in the array.

```r
.run = function() {
    array <- self$results$modelResults
    
    # Suppose we want a table for each dependent variable
    deps <- self$options$deps
    
    for (dep in deps) {
        # Create a new instance of the template for this variable
        # The 'key' can be any string that uniquely identifies this item
        table <- array$addItem(key = dep)
        
        # You can now interact with the new table object
        table$setTitle(paste("Results for", dep))
        
        # Add a row to the new table
        table$setRow(rowNo=1, values=list(
            var=dep,
            value=mean(self$data[[dep]], na.rm=TRUE)
        ))
    }
}
```

> [!TIP]
> Arrays are the standard way in jamovi to handle "Split By" functionality or any situation where the number of output elements depends on the data or user options.
