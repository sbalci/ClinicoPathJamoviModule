# Advanced Heatmap Visualization

Advanced Heatmap Visualization

## Usage

``` r
jggheatmap(
  data,
  matrix_vars,
  row_var = NULL,
  col_var = NULL,
  value_var = NULL,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "complete",
  distance_method = "euclidean",
  scaling = "none",
  color_scheme = "blue_red",
  cell_shape = "square",
  show_values = FALSE,
  value_format = "auto",
  text_size = 8,
  show_row_labels = TRUE,
  show_col_labels = TRUE,
  row_label_size = 10,
  col_label_size = 10,
  show_dendrograms = TRUE,
  dendrogram_height = 0.2,
  annotation_var = NULL,
  annotation_colors = "default",
  plot_title = "",
  plot_width = 600,
  plot_height = 600,
  show_colorbar = TRUE,
  colorbar_title = "Value",
  border_color = "white",
  na_color = "grey90",
  output_format = "plot_only"
)
```

## Arguments

- data:

  .

- matrix_vars:

  Variables to include in the heatmap matrix

- row_var:

  Variable defining row groupings (for pivot format)

- col_var:

  Variable defining column groupings (for pivot format)

- value_var:

  Variable containing matrix values (for pivot format)

- cluster_rows:

  Perform hierarchical clustering on rows

- cluster_cols:

  Perform hierarchical clustering on columns

- clustering_method:

  .

- distance_method:

  .

- scaling:

  .

- color_scheme:

  .

- cell_shape:

  .

- show_values:

  Display numeric values in cells

- value_format:

  .

- text_size:

  Size of text in cells

- show_row_labels:

  .

- show_col_labels:

  .

- row_label_size:

  .

- col_label_size:

  .

- show_dendrograms:

  Display clustering dendrograms

- dendrogram_height:

  Height of dendrogram relative to plot

- annotation_var:

  Variable for row/column annotations

- annotation_colors:

  .

- plot_title:

  .

- plot_width:

  .

- plot_height:

  .

- show_colorbar:

  Display color scale legend

- colorbar_title:

  .

- border_color:

  .

- na_color:

  .

- output_format:

  .

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$plot`           |     |     |     |     | an image |
| `results$matrixtab`      |     |     |     |     | a table  |
| `results$clustertab`     |     |     |     |     | a table  |
| `results$interpretation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$matrixtab$asDF`

`as.data.frame(results$matrixtab)`
