# Age Pyramid

Age Pyramid

## Usage

``` r
agepyramid(
  data,
  age,
  gender,
  female,
  male,
  age_groups = "custom",
  bin_width = 5,
  custom_breaks = "",
  plot_title = "Age Pyramid",
  color_palette = "standard",
  female_color = "#E91E63",
  male_color = "#2196F3",
  originaltheme = FALSE,
  enableGGCharts = FALSE,
  ggcharts_sort = "no",
  ggcharts_colors = "default",
  ggcharts_color1 = "#1F77B4",
  ggcharts_color2 = "#FF7F0E",
  ggcharts_title = "Age Pyramid (ggcharts)",
  ggcharts_xlab = "Population"
)
```

## Arguments

- data:

  The data as a data frame.

- age:

  a string naming the variable from `data` that contains the continuous
  values used for the report

- gender:

  a string naming the variable from `data` that contains the categorical
  values used for the report

- female:

  a string naming the level from `gender` that contains the level female

- male:

  a string naming the level from `gender` that contains the level male

- age_groups:

  Predefined age group categories. Choose 'custom' to use bin_width, or
  select a preset.

- bin_width:

  The width of the age bins in years. Adjust this to change the
  granularity of the age groups.

- custom_breaks:

  Comma-separated age break points (e.g., "0,18,25,50,65,100"). Only
  used when age_groups is set to 'custom'. Leave empty to use bin_width.

- plot_title:

  The title displayed on the age pyramid plot.

- color_palette:

  Color palette for gender visualization. Choose 'custom' to specify
  your own colors.

- female_color:

  Custom color for female bars (hex code). Only used when color_palette
  is 'custom'.

- male_color:

  Custom color for male bars (hex code). Only used when color_palette is
  'custom'.

- originaltheme:

  Whether to apply the original custom theme (theme_minimal with custom
  tweaks) instead of jamovi's default theme.

- enableGGCharts:

  Enable the ggcharts pyramid_chart() visualization as a second plot.
  This provides an alternative visualization style using the ggcharts
  package.

- ggcharts_sort:

  Sort bars by population count. Options: 'no' (default order),
  'ascending' (smallest to largest), 'descending' (largest to smallest).

- ggcharts_colors:

  Color scheme for ggcharts pyramid. 'default' uses ggcharts defaults,
  or select from preset palettes or custom colors.

- ggcharts_color1:

  Custom color for first group (hex code). Used when ggcharts_colors is
  'custom'.

- ggcharts_color2:

  Custom color for second group (hex code). Used when ggcharts_colors is
  'custom'.

- ggcharts_title:

  Title for the ggcharts pyramid plot.

- ggcharts_xlab:

  X-axis label for ggcharts pyramid.

## Value

A results object containing:

|                        |     |     |     |     |          |
|------------------------|-----|-----|-----|-----|----------|
| `results$welcome`      |     |     |     |     | a html   |
| `results$dataInfo`     |     |     |     |     | a html   |
| `results$pyramidTable` |     |     |     |     | a table  |
| `results$plot`         |     |     |     |     | an image |
| `results$plotGGCharts` |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$pyramidTable$asDF`

`as.data.frame(results$pyramidTable)`
