# A Comprehensive Guide to Creating and Populating Tables in jamovi

This document provides a comprehensive guide to creating and populating tables in jamovi module development. Tables are a fundamental way to present statistical results, and this guide will walk you through the process from definition to population.

## 1. Introduction: The Two-Part Process of Creating Tables

Creating tables in jamovi is a two-part process that involves two different files:

1.  **`.r.yaml` file:** This is where you **define the structure** of your table. You specify the name of the table, its title, and the columns it will contain. This acts as a blueprint for your table.
2.  **`.b.R` file:** This is where you **populate the table** with data. You write R code to perform your analysis, and then you use functions provided by the `jmvcore` package to fill the table with the results.

## 2. Step 1: Defining the Table in `.r.yaml`

The first step is to define the table in your analysis's `.r.yaml` file. A table is defined as an item of type `Table`. Let's look at an example from the `survival.r.yaml` file:

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

Here's a breakdown of the key properties:

*   `name`: A unique name for the table (e.g., `medianTable`). You'll use this name to access the table in your R code.
*   `title`: The title of the table. You can use `${...}` to insert the value of an option from your `.a.yaml` file, making the title dynamic.
*   `type`: Must be `Table`.
*   `rows`: The number of rows in the table. Setting `rows: 0` is a common practice for tables where the number of rows is determined by the data.
*   `columns`: A list of dictionaries, where each dictionary defines a column.
    *   `name`: A unique name for the column.
    *   `title`: The text that will appear in the column header.
    *   `type`: The data type of the column. This can be `text`, `integer`, or `number`.
    *   `format`: (Optional) A string to format numeric values. For example, `pc` for percentage, or `zto,pvalue` for p-values.
    *   `superTitle`: (Optional) A title that spans across multiple columns, creating a higher-level header. In the example, "95% Confidence Interval" spans the "Lower" and "Upper" columns.

## 3. Step 2: Accessing and Populating the Table in `.b.R`

Once you've defined the table in your `.r.yaml` file, you can access it in your `.b.R` file and populate it with data.

### Accessing the Table

You can get a reference to a table object using `self$results$get()` and the name you gave the table in the `.r.yaml` file:

```R
medianTable <- self$results$medianTable
```

### Populating the Table

There are several ways to populate a table. The most common method is to loop through a data frame of results and add rows one by one.

Let's look at the `survival.b.R` file to see how the `medianTable` is populated. The code first performs the survival analysis and creates a data frame called `results2table` with the results. Then, it iterates through this data frame and adds rows to the table:

```R
# ... (code to generate results2table data frame) ...

medianTable <- self$results$medianTable
data_frame <- results2table
for (i in seq_along(data_frame[, 1, drop = T])) {
    medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
}
```

*   `medianTable$addRow(rowKey = i, values = ...)`: This is the key function.
    *   `rowKey`: A unique identifier for the row. A simple counter `i` is usually sufficient.
    *   `values`: A named list or vector containing the values for each column in the row. The names must match the `name` of the columns you defined in the `.r.yaml` file. The `c(data_frame[i,])` syntax is a concise way to create a named vector from a row of a data frame.

### Other Population Methods

While `addRow()` is common, `jmvcore` provides other functions for populating tables:

*   `setRow(rowNo = ..., values = ...)`: Sets the values for an entire row at a specific row number.
*   `setCellValue(rowKey = ..., col = ..., value = ...)`: Sets the value of a single cell.
*   `addColumn(name = ..., title = ..., type = ...)`: Adds a new column to the table dynamically.

## 4. A Complete Example: `gtsummary`

The `gtsummary` function in this project provides a different approach. Instead of manually populating a table, it uses the `gtsummary` R package to create a rich HTML table and then places that HTML into an `Html` result item.

**`.r.yaml`:**

```yaml
- name:  maintable
  title: Summary Table
  type:  Html
  visible: true
```

**`.b.R`:**

```R
# ... (code to create the gtsummary table object `main_table`) ...

# Convert to HTML format
html_table <- gtsummary::as_kable_extra(main_table, format = "html")

self$results$maintable$setContent(html_table)
```

This approach is very powerful for creating complex and highly customized tables, but it bypasses the standard jamovi table-populating mechanism.

## 5. Advanced Features

### Footnotes

You can add footnotes to your tables to provide additional information.

```R
pairwiseTable$setNote(
    key = padjustmethod,
    note = paste0("p-value adjustement method: ",
           padjustmethod)
)
```

*   `key`: A unique key for the note.
*   `note`: The text of the footnote.

### State Management and `clearWith`

Remember to use the `clearWith` property in your `.r.yaml` file to specify which options should trigger the table to be cleared. This ensures that the table is always up-to-date with the current analysis settings.

## 6. Best Practices

*   **Match Names:** Ensure that the `name` of the table and its columns in the `.r.yaml` file match the names you use to access them in your `.b.R` file.
*   **Data Types:** Make sure the data you're inserting into a column matches the `type` you defined for that column in the `.r.yaml` file.
*   **Dynamic Sizing:** Use `rows: 0` in your `.r.yaml` file for tables where the number of rows depends on the data.
*   **Refer to the Documentation:** The official jamovi documentation on the [Table class](https://dev.jamovi.org/api_table.html) and [dynamic tables](https://dev.jamovi.org/tuts0201-dynamic-tables.html) are excellent resources.

By following these steps and best practices, you can effectively create and populate tables to present your statistical results clearly and professionally in your jamovi modules.
