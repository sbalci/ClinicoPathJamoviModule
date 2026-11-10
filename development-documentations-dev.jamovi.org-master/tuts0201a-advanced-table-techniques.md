<!-- Vendored verbatim from https://github.com/jamovi/dev.jamovi.org/blob/7145eb8/src/content/docs/learn/tutorial/tuts0201a-advanced-table-techniques.md (fetched 2026-11-09; published at https://dev.jamovi.org). -->

---
layout: ../layouts/BaseLayout.astro
title: "Advanced Table Techniques"
type: article
description: "Master complex results using the Three Levels of Table Initialization, Grouped Results, and Table Folding."
---

Building on the [Analysis Lifecycle](/tutorial/tuts0200-analysis-lifecycle), this section explores how to handle tables that don't have a fixed structure.

## The Three Levels of Table Initialization

In jamovi, you can define a table's structure at three different stages, depending on how much information you have.

### 1. YAML (Static/Data-bound)
The simplest way. Define the columns and rows directly in your `.r.yaml`. Use `columns` for fixed columns and `rows: (deps)` for data-bound rows.

```yaml
- name: mainTable
  type: Table
  rows: (deps)
  columns:
    - name: var
      title: Variable
      type: text
      content: ($key)
    - name: p
      title: p-value
      type: number
      format: zto,pvalue
```

### 2. `.init()` (Dynamic Structure)
Use this when the table structure depends on **User Options**, but not on the data values themselves. 

A perfect example is an **ANOVA table**. While the columns are fixed, the number of rows depends on how many factors the user selects and how they've defined their model (main effects, interactions, etc.). By initializing these rows in `.init()`, the table structure appears instantly, providing immediate feedback before the calculation even starts.

```r
.init = function() {
    table <- self$results$mainTable
    
    # Get the model terms from options (e.g., list(c("A"), c("B"), c("A", "B")))
    terms <- self$options$modelTerms
    
    for (term in terms) {
        # Create a nice label like "A ✻ B" for interactions
        label <- paste0(term, collapse = " ✻ ")
        
        # Add a row for each term using the term itself as a unique key
        table$addRow(rowKey = term, values = list(
            var = label
        ))
    }
    
    # Always add a row for the Error/Residuals
    table$addRow(rowKey = ".error", values = list(
        var = "Residuals"
    ))
}
```

> [!TIP]
> **Why not YAML?** While you can bind rows 1-to-1 to an option in YAML (e.g., `rows: (deps)`), an ANOVA table often needs to generate rows for *combinations* of options (interactions) or add extra rows (Residuals, Total). `.init()` gives you the programmatic flexibility to build this complex structure while still maintaining a responsive UI.

### 3. `.run()` (Result-based Structure)
Use this only when the structure depends on the **Results of the Calculation**. For example, in an Exploratory Factor Analysis where the number of columns depends on how many factors were actually extracted from the data.

```r
.run = function() {
    table <- self$results$factorTable
    
    # Imagine a function that calculates the optimal number of factors
    nFactors <- calculateOptimalFactors(self$data)
    
    # Dynamically add a column for each extracted factor
    for (i in 1:nFactors) {
        table$addColumn(
            name = paste0("factor", i),
            title = paste("Factor", i),
            type = "number"
        )
    }
}
```

> [!CAUTION]
> **Avoid Level 3 whenever possible.** Using `.run()` to define table structure causes a "UI Jump." Because the `.init()` phase doesn't know the structure, it creates a generic placeholder. When `.run()` finishes, the table "jumps" as columns and rows are suddenly added. Level 1 and 2 ensure a smooth, professional-feeling UI where the table structure is visible immediately.

---

## Nesting Results (Groups and Arrays)

In complex analyses, a single flat list of items is often insufficient. jamovi provides two specialized types for nesting and dynamically generating results: `Group` and `Array`.

> [!NOTE]
> **Nesting vs. Folding:**
> While they sound similar, they serve different purposes: **Nesting** is for organizing different objects (multiple tables/images), while **Folding** is for organizing data within a single table.

### 1. The `Group` Type
Use a `Group` to nest related tables or images under a shared title in the results panel.

```yaml
# .r.yaml
items:
  - name: mainTable
    type: Table
    ...
  - name: assum  # A logical group for assumption checks
    title: Assumptions
    type: Group
    items:
      - name: norm
        title: Normality Test
        type: Table
      - name: eqv
        title: Homogeneity of Variances
        type: Table
```

In the R API, you access these nested items through the group name: `self$results$assum$norm` and `self$results$assum$eqv`.

### 2. The `Array` Type
Use an `Array` when you need to generate a dynamic list of items based on a user's selection (e.g., one Q-Q plot for every dependent variable).

```yaml
# .r.yaml
items:
  - name: plots
    title: Plots
    type: Array
    items: (deps)  # Binds the array size to the 'deps' option
    template:      # The template defines the structure of every item in the array
      title: $key  # Uses the variable name as the title
      type: Group
      items:
        - name: descPlot
          type: Image
        - name: qqPlot
          type: Image
```

In the R API, you access array elements by their key (the variable name): `self$results$plots$get(depName)$descPlot`.

---

## Table Folding (Columns to Rows)

**Table Folding** is a powerful feature that allows you to store results in a "flat" structure while rendering them in a "long" format (multiple rows). This is a core technique used in the `jmv` t-test to show multiple tests (Student's t, Welch's t, etc.) for each variable.

### The Bracket Convention for Columns
When you add bracketed suffixes to column names, jamovi automatically creates a "fold" for each unique bracket.

```yaml
# .r.yaml
columns:
  - name: var[stud]
    title: Variable
    type: text
    content: ($key)
    combineBelow: true
  - name: name[stud]
    title: ''
    content: Student's t
  - name: stat[stud]
    title: Statistic
    type: number

  - name: var[welc]
    title: Variable
    type: text
    content: ($key)
    combineBelow: true
  - name: name[welc]
    title: ''
    content: Welch's t
  - name: stat[welc]
    title: Statistic
    type: number
```

### How it works
Even though you've defined 6 columns in YAML, jamovi will render this as **3 visual columns** and **2 visual rows** for every row key.

1.  **Grouping:** jamovi sees that `var[stud]` and `var[welc]` share the same base name (`var`) and treats them as the same visual column.
2.  **Folding:** jamovi creates one visual row for the `[stud]` fold and one for the `[welc]` fold.
3.  **Flat Storage:** In your R code, you populate the table with a single `setRow` call:

```r
# Populating folded rows in .b.R
table$setRow(rowKey="Weight", list(
    "stat[stud]" = 2.31,
    "stat[welc]" = 2.28
))
```

### Combining with `combineBelow`
By setting `combineBelow: true` on the `var` columns, the variable name is only shown once and merged across all folds, creating a clean, professional "grouped" look.

> [!TIP]
> This technique is extremely dynamic. If you bind the `visible` property of all `[welc]` columns to a "Welch's t" checkbox, the second row for each variable will simply vanish or reappear as the user toggles the option, without any complex logic in your `.run()` function.

**Next Step:** Now that you've mastered tables, let's look at [Handling Data](/tutorial/tuts0202-handling-data) more efficiently.
