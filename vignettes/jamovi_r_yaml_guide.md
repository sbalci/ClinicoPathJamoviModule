# A Comprehensive Guide to Writing `.r.yaml` Files for jamovi Development

This document provides a comprehensive guide to writing `.r.yaml` files for jamovi module development. These files are essential for defining the structure and content of the results that your analysis will produce.

## 1. Introduction: The Role of `.r.yaml`

The `.r.yaml` file is a YAML file that specifies the results definition for a jamovi analysis. It defines the output elements that will be displayed to the user, such as tables, plots, and text. Each analysis in a jamovi module has an `.r.yaml` file, which works in conjunction with the `.a.yaml` (analysis definition) and `.u.yaml` (UI definition) files.

The `.r.yaml` file acts as a blueprint for the results, specifying the structure and properties of each output element. The actual content of these elements is then populated by your R code during the analysis.

## 2. Core Components of an `.r.yaml` File

Let's examine the top-level keys in a typical `.r.yaml` file, using `gtsummary.r.yaml` as a starting point:

```yaml
---
name:  gtsummary
title: Publication-Ready Tables with gtsummary
jrs:   '1.1'

items:
    - name:  todo
      title: Instructions
      type:  Html
      visible: false
# ... more items
```

*   `name`: This should match the `name` in the corresponding `.a.yaml` file. It uniquely identifies the analysis.
*   `title`: The title of the results. This can be a static string or can be dynamically generated using variables from the analysis options (e.g., `` `Survival Analysis - ${explanatory}` ``).
*   `jrs`: The jamovi results specification version. `'1.1'` is a common value.
*   `items`: This is the core of the `.r.yaml` file. It's a list of all the result elements that will be part of the output.

## 3. The `items` Section: Defining Result Elements

The `items` section is a list of dictionaries, where each dictionary defines a single result element. Here are the common keys for an item:

*   `name`: A unique name for the result item. This name is used in your R code to access and populate the item.
*   `title`: The title of the result item as it will appear in the jamovi output.
*   `type`: The type of result element. This determines how the item will be rendered. We'll explore the different types below.
*   `visible`: A condition that determines whether the item is visible in the output. This is often tied to an option from the `.a.yaml` file (e.g., `(addPValue)`).
*   `clearWith`: A list of option names from the `.a.yaml` file. If any of these options change, the content of the result item will be cleared. This is important for ensuring that the results are always up-to-date with the current analysis settings.
*   `refs`: A list of references to be cited for this result.

### Common Result Element Types

Here are some of the most common result element types you'll use:

#### `Html`

This type is used to render HTML content. It's very flexible and can be used for instructions, notes, or complex, custom-formatted output.

```yaml
- name:  todo
  title: Instructions
  type:  Html
  visible: false
```

#### `Preformatted`

This type is used to display preformatted text, such as the output from a statistical test or a block of R code. The text will be rendered in a monospace font, preserving whitespace.

```yaml
- name:  code_output
  title: R Code
  type:  Preformatted
  visible: "(showCode)"
```

#### `Table`

This is one of the most important result types. It's used to display tabular data.

```yaml
- name:  medianTable
  title: '`Median Survival Table: Levels for ${explanatory}`'
  type:  Table
  rows: 0
  columns:
    - name: factor
      title: "Levels"
      type: text
    - name: records
      title: "Records"
      type: integer
    - name: events
      title: "Events"
      type: integer
    - name: median
      title: "Median"
      type: number
    - name: x0_95lcl
      title: "Lower"
      superTitle: '95% Confidence Interval'
      type: number
    - name: x0_95ucl
      title: "Upper"
      superTitle: '95% Confidence Interval'
      type: number
```

*   `rows`: The number of rows in the table. `0` means the number of rows is determined by the data.
*   `columns`: A list of dictionaries, where each dictionary defines a column in the table.
    *   `name`: The name of the column.
    *   `title`: The column header.
    *   `type`: The data type of the column (`text`, `integer`, `number`).
    *   `format`: A string specifying the format for numeric values (e.g., `pc` for percentage, `zto` for zero-to-one, `pvalue`).
    *   `superTitle`: A title that spans across multiple columns.

#### `Image`

This type is used to display plots and other images.

```yaml
- name: plot
  title: '`Survival Plot - ${explanatory}`'
  type: Image
  width:  600
  height: 450
  renderFun: .plot
  visible: (sc)
  requiresData: true
```

*   `width` and `height`: The dimensions of the plot in pixels.
*   `renderFun`: The name of the R function that will generate the plot. This function is defined in your analysis's R code.
*   `requiresData`: Whether the plot requires data to be rendered.

#### `Output`

This type is used to create a new variable in the dataset.

```yaml
- name: calculatedtime
  title: Add Calculated Time to Data
  type: Output
  varTitle: '`Calculated Time - from ${ dxdate } to { fudate }`'
  varDescription: '`Calculated Time from Given Dates - from ${ dxdate } to { fudate } in Survival Analysis`'
  measureType: continuous
```

*   `varTitle`: The title of the new variable.
*   `varDescription`: A description of the new variable.
*   `measureType`: The measure type of the new variable (`continuous`, `nominal`, `ordinal`).

## 4. A Complete Example: `survival.r.yaml`

The `survival.r.yaml` file provides an excellent example of a complex results definition with multiple tables, plots, and other elements.

```yaml
---
name:  survival
title: Survival Analysis
jrs:   '1.1'

items:
    - name: subtitle
      title: '`Survival Analysis - ${explanatory}`'
      type:  Preformatted

    - name:  medianTable
      title: '`Median Survival Table: Levels for ${explanatory}`'
      type:  Table
      # ... (column definitions)

    - name: plot
      title: '`Survival Plot - ${explanatory}`'
      type: Image
      width:  600
      height: 450
      renderFun: .plot
      visible: (sc)
      requiresData: true

    - name: calculatedtime
      title: Add Calculated Time to Data
      type: Output
      # ... (output variable definitions)
```

This example showcases several key features:

*   **Dynamic Titles:** The titles of the results items are dynamically generated using the `${explanatory}` variable from the analysis options.
*   **Conditional Visibility:** The visibility of the plot is controlled by the `sc` option.
*   **Multiple Result Types:** The analysis produces a variety of outputs, including preformatted text, tables, plots, and new data variables.

## 5. Best Practices

*   **Clear and Descriptive Names:** Use clear and descriptive names for your result items. This will make your R code easier to read and maintain.
*   **Use `clearWith`:** Always use the `clearWith` property to ensure that your results are updated when the user changes the analysis options.
*   **Organize Your Results:** Use `Group` elements (not shown in the examples, but available) to organize your results into logical sections.
*   **Provide Informative Titles:** Use dynamic titles to provide context for your results.
*   **Refer to the Documentation:** The official [jamovi results definition documentation](https://dev.jamovi.org/api_results-definition.html) and [results elements documentation](https://dev.jamovi.org/api_results-elements.html) are invaluable resources.

By following these guidelines and studying the examples in this project, you can create well-structured and informative results for your jamovi analyses.
