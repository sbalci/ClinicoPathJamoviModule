# Treemap (jjtreemap) Documentation

This document provides a comprehensive overview of the `jjtreemap` module, detailing its features, user interface elements, and the underlying R functions. This module is designed to create treemap visualizations for hierarchical categorical data.

## Feature Summary

The `jjtreemap` module is a versatile tool for visualizing hierarchical data as nested rectangles. It allows users to represent quantitative values through rectangle size, categorize data using color, and display hierarchical relationships through nested structures. The module leverages the `treemap` and `ggplot2` R packages for its functionality.

The module's features can be broadly categorized as follows:

*   **Data Input:** Specifies group, size, and optional color variables for treemap generation.
*   **Layout and Appearance:** Controls aspect ratio, border widths, and colors for different levels of the treemap.
*   **Labeling:** Offers extensive options for customizing labels, including size, color, font style, background, alignment, and overlap tolerance.
*   **Plot Titles:** Allows custom titles, subtitles, and captions for comprehensive plot annotation.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| :------------------------------- | :----------------------------- | :------------------------------------- | :---------------------------------- | :----------------------------------- |
| **Data Input**                   |                                |                                        |                                     |                                      |
| Group Variable                   | `group`                        | Group Variable                         | `plot`                              | `.run`, `.plot`                      |
| Size Variable                    | `size`                         | Size Variable                          | `plot`                              | `.run`, `.plot`                      |
| Color Variable (Optional)        | `color`                        | Color Variable (Optional)              | `plot`                              | `.run`, `.plot`                      |
| **Layout and Appearance**        |                                |                                        |                                     |                                      |
| Aspect Ratio                     | `aspectRatio`                  | Aspect Ratio                           | `plot`                              | `.plot`                              |
| Border Width                     | `borderWidth`                  | Border Width                           | `plot`                              | `.plot`                              |
| Level 1 Border Width             | `borderLevel1Width`            | Level 1 Border Width                   | `plot`                              | `.plot`                              |
| Level 2 Border Width             | `borderLevel2Width`            | Level 2 Border Width                   | `plot`                              | `.plot`                              |
| Level 1 Border Color             | `borderLevel1Color`            | Level 1 Border Color                   | `plot`                              | `.plot`                              |
| Level 2 Border Color             | `borderLevel2Color`            | Level 2 Border Color                   | `plot`                              | `.plot`                              |
| **Labeling**                     |                                |                                        |                                     |                                      |
| Show Labels                      | `showLabels`                   | Show Labels                            | `plot`                              | `.plot`                              |
| Minimum Label Size               | `labelSize`                    | Minimum Label Size                     | `plot`                              | `.plot`                              |
| Level 1 Label Size               | `labelLevel1Size`              | Level 1 Label Size                     | `plot`                              | `.plot`                              |
| Level 2 Label Size               | `labelLevel2Size`              | Level 2 Label Size                     | `plot`                              | `.plot`                              |
| Level 1 Label Color              | `labelLevel1Color`             | Level 1 Label Color                    | `plot`                              | `.plot`                              |
| Level 2 Label Color              | `labelLevel2Color`             | Level 2 Label Color                    | `plot`                              | `.plot`                              |
| Label Font Style                 | `labelFontFace`                | Label Font Style                       | `plot`                              | `.plot`                              |
| Label Background                 | `labelBackground`              | Label Background                       | `plot`                              | `.plot`                              |
| Label Horizontal Alignment       | `labelAlignH`                  | Label Horizontal Alignment             | `plot`                              | `.plot`                              |
| Label Vertical Alignment         | `labelAlignV`                  | Label Vertical Alignment               | `plot`                              | `.plot`                              |
| Label Overlap Tolerance          | `labelOverlap`                 | Label Overlap Tolerance                | `plot`                              | `.plot`                              |
| **Plot Titles**                  |                                |                                        |                                     |                                      |
| Plot Title                       | `title`                        | Plot Title                             | `plot`                              | `.plot`                              |
| Plot Subtitle                    | `subtitle`                     | Plot Subtitle                          | `plot`                              | `.plot`                              |
| Plot Caption                     | `caption`                      | Plot Caption                           | `plot`                              | `.plot`                              |