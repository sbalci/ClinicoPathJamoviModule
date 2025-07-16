# Enhanced Tables Documentation

This document provides a comprehensive overview of the Enhanced Tables module (enhancedtables), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The enhancedtables module is designed to create highly customizable and publication-ready tables from various data sources and statistical analyses. It extends basic table functionalities by offering advanced formatting, styling, and content integration options.

The module's features can be broadly categorized as follows:

*   **Core Table Generation:** Create structured tables from data frames or statistical outputs.
*   **Advanced Formatting:** Apply custom styles, fonts, colors, and conditional formatting to table cells.
*   **Content Integration:** Embed plots, sparklines, or other visual elements within table cells.
*   **Interactive Features:** Options for interactive tables (e.g., sorting, filtering, searching).
*   **Export Options:** Capabilities to save tables in various high-quality formats (e.g., HTML, PDF, Word, LaTeX).

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Data Input                       | `dataInput`                    | Data Input                             | `tableOverview`                     | `.prepareTableData`                  |
| **Table Customization**          |                                |                                        |                                     |                                      |
| Show Table                       | `showTable`                    | Show Table                             | `enhancedTable`                     | `.renderEnhancedTable`               |
| Table Title                      | `tableTitle`                   | Table Title                            | `enhancedTable`                     | `.setTableTitle`                     |
| Column Labels                    | `columnLabels`                 | Column Labels                          | `enhancedTable`                     | `.setColumnLabels`                   |
| Row Grouping                     | `rowGrouping`                  | Row Grouping                           | `enhancedTable`                     | `.applyRowGrouping`                  |
| Conditional Formatting           | `conditionalFormatting`        | Conditional Formatting                 | `enhancedTable`                     | `.applyConditionalFormatting`        |
| **Content Integration**          |                                |                                        |                                     |                                      |
| Add Sparkline                    | `addSparkline`                 | Add Sparkline                          | `enhancedTable`                     | `.addSparkline`                      |
| Add Plot                         | `addPlot`                      | Add Plot                               | `enhancedTable`                     | `.addPlotToCell`                     |
| **Interactive Options**          |                                |                                        |                                     |                                      |
| Enable Interactivity             | `enableInteractivity`          | Enable Interactivity                   | `enhancedTable`                     | `.enableInteractiveFeatures`         |
| Search Box                       | `showSearchBox`                | Show Search Box                        | `enhancedTable`                     | `.showSearchBox`                     |
| Pagination                       | `enablePagination`             | Enable Pagination                      | `enhancedTable`                     | `.enablePagination`                  |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Table                     | `exportTable`                  | Export Table                           | `exportOptions`                     | `.exportTable`                       |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportTable`                       |