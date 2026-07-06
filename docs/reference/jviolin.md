# Professional Violin Plot

Create professional violin plots for visualizing the distribution of
continuous data across groups. Violin plots combine density estimation
with boxplot elements to show both distribution shape and summary
statistics.

## Usage

``` r
jviolin(
  data,
  dep = NULL,
  group = NULL,
  col = NULL,
  fill = NULL,
  excl = FALSE,
  flip = FALSE,
  add_boxplot = FALSE,
  add_points = FALSE,
  add_mean = FALSE,
  draw_quantiles = FALSE,
  quantile_lines = "0.25,0.5,0.75",
  trim_violin = TRUE,
  scale_violin = "area",
  violin_width = 1,
  violin_alpha = 0.7,
  boxplot_width = 0.1,
  boxplot_alpha = 0.8,
  point_size = 1.5,
  point_alpha = 0.6,
  point_jitter = TRUE,
  color_palette = "default",
  manual_colors = "",
  themex = "minimal",
  usexlabel = FALSE,
  xlabel = "",
  useylabel = FALSE,
  ylabel = "",
  showExplanations = TRUE,
  clinicalPreset = "custom"
)
```

## Arguments

- data:

  The data to be analyzed

- dep:

  Continuous variable for y-axis (violin height)

- group:

  Categorical variable for x-axis (groups)

- col:

  Optional variable for color mapping (uses grouping variable if empty)

- fill:

  Optional variable for fill mapping (uses grouping variable if empty)

- excl:

  Exclude observations with missing data from analysis

- flip:

  Swap x and y axes for horizontal violin plots

- add_boxplot:

  Add boxplot inside violin for summary statistics

- add_points:

  Add individual data points over violin

- add_mean:

  Add mean values as red diamond points

- draw_quantiles:

  Draw horizontal lines at specified quantiles

- quantile_lines:

  Comma-separated quantile values (e.g., 0.25,0.5,0.75)

- trim_violin:

  Trim violin tails to data range

- scale_violin:

  How to scale violin widths

- violin_width:

  Width multiplier for violins

- violin_alpha:

  Transparency level for violin fill

- boxplot_width:

  Width of boxplot overlay

- boxplot_alpha:

  Transparency level for boxplot

- point_size:

  Size of individual data points

- point_alpha:

  Transparency level for points

- point_jitter:

  Add horizontal jitter to prevent point overlap

- color_palette:

  Color palette for violin fills and outlines

- manual_colors:

  Comma-separated color values (e.g., red,blue,green)

- themex:

  Overall plot theme style

- usexlabel:

  Override default x-axis label

- xlabel:

  Custom label for x-axis

- useylabel:

  Override default y-axis label

- ylabel:

  Custom label for y-axis

- showExplanations:

  Show explanations of the statistical results

- clinicalPreset:

  Clinical analysis preset

## Value

A results object containing:

|                |     |     |     |     |          |
|----------------|-----|-----|-----|-----|----------|
| `results$todo` |     |     |     |     | a html   |
| `results$plot` |     |     |     |     | an image |
