# Complex UpSet Plot Visualization

Complex UpSet Plot Visualization

## Usage

``` r
jcomplexupset(
  data,
  set_vars,
  value_var = NULL,
  min_size = 0,
  max_degree = 4,
  sort_by = "cardinality",
  sort_order = "descending",
  keep_empty_groups = FALSE,
  set_size_show = TRUE,
  intersection_size_show = TRUE,
  show_percentages = FALSE,
  annotations = "intersection_size",
  base_annotations_height = 2,
  intersections_height = 3,
  sets_width = 2,
  color_palette = "Set2",
  theme_style = "theme_minimal",
  matrix_color = "black",
  bar_color = "steelblue",
  text_size = 12,
  plot_title = "",
  plot_subtitle = "",
  show_statistics = TRUE,
  show_interpretation = TRUE,
  width_ratio = 0.3,
  height_ratio = 0.7
)
```

## Arguments

- data:

  R object to use

- set_vars:

  Variables representing set membership (binary columns)

- value_var:

  Optional variable for weighting intersections

- min_size:

  Minimum size for intersections to be displayed

- max_degree:

  Maximum number of sets in intersection

- sort_by:

  Method for sorting intersections

- sort_order:

  Order for sorting intersections

- keep_empty_groups:

  Show intersections with zero size

- set_size_show:

  Display individual set sizes

- intersection_size_show:

  Display intersection size values

- show_percentages:

  Display percentages instead of counts

- annotations:

  Type of annotation to add

- base_annotations_height:

  Height of annotation panels

- intersections_height:

  Height of intersections panel

- sets_width:

  Width of sets panel

- color_palette:

  Color palette for visualization

- theme_style:

  ggplot2 theme style

- matrix_color:

  Color for intersection matrix dots

- bar_color:

  Color for bars in plots

- text_size:

  Size of text elements

- plot_title:

  Title for the plot

- plot_subtitle:

  Subtitle for the plot

- show_statistics:

  Display summary statistics

- show_interpretation:

  Display result interpretation

- width_ratio:

  Ratio of sets width to total width

- height_ratio:

  Ratio of intersections height to total height

## Value

A results object containing:

|                          |     |     |     |     |          |
|--------------------------|-----|-----|-----|-----|----------|
| `results$instructions`   |     |     |     |     | a html   |
| `results$plot`           |     |     |     |     | an image |
| `results$statistics`     |     |     |     |     | a html   |
| `results$interpretation` |     |     |     |     | a html   |
