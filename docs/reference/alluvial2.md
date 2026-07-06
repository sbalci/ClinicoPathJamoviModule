# Alluvial Diagrams

Alluvial Diagrams

## Usage

``` r
alluvial2(
  data,
  vars,
  condensationvar = NULL,
  excl = FALSE,
  marg = FALSE,
  fill = "first_variable",
  bin = "default",
  orient = "vert",
  usetitle = FALSE,
  mytitle = "Alluvial Plot"
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  a string naming the variables from `data` that contains the values
  used for the Alluvial Diagram.

- condensationvar:

  The primary variable to be used for condensation.

- excl:

  Exclude missing values from the analysis.

- marg:

  Include marginal plots.

- fill:

  A list for the argument fill for selecting the variable to be
  represented by color. Default is 'first_variable'.

- bin:

  labels for the bins from low to high

- orient:

  Orientation of the plot. Default is 'vertical'.

- usetitle:

  Use a custom title for the plot.

- mytitle:

  Title for the plot.

## Value

A results object containing:

|                 |     |     |     |     |          |
|-----------------|-----|-----|-----|-----|----------|
| `results$todo`  |     |     |     |     | a html   |
| `results$plot`  |     |     |     |     | an image |
| `results$plot2` |     |     |     |     | an image |
