# Radar Plot

'Create radar plots (spider plots) for multivariate data visualization.
This function allows comparison of multiple variables across different
categories.'

## Usage

``` r
jjradarplot(
  data,
  vars,
  categoryVar,
  splitBy = NULL,
  scaleData = TRUE,
  aggregationMethod = "mean",
  alpha = 0.25,
  lineSize = 1,
  pointSize = 2,
  title = "Radar Plot",
  legendPosition = "right",
  axisLabelSize = 10,
  facetLabelSize = 11,
  excl = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  A list of numeric variables to be used as radar axes (minimum 3
  required).

- categoryVar:

  A categorical variable that defines different radar polygons.

- splitBy:

  Optional variable to create separate radar plots in facets.

- scaleData:

  Whether to scale all variables to 0-1 range for better comparison.

- aggregationMethod:

  How to aggregate data for each category.

- alpha:

  Transparency level for polygon fill (0 = transparent, 1 = opaque).

- lineSize:

  Width of polygon outline.

- pointSize:

  Size of points at vertices.

- title:

  Title for the plot.

- legendPosition:

  Position of the legend.

- axisLabelSize:

  Size of axis labels.

- facetLabelSize:

  Size of facet labels (when using Split By).

- excl:

  TRUE or FALSE (default), exclude cases analysis by analysis

## Value

A results object containing:

|                |     |     |     |     |          |
|----------------|-----|-----|-----|-----|----------|
| `results$todo` |     |     |     |     | a html   |
| `results$plot` |     |     |     |     | an image |
