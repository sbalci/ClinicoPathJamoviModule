# Circos Chord Diagram

Draws a circular chord diagram visualizing the flow or co-occurrence
between categories - for example transitions between disease states,
co-mutation of genes, referral patterns between sites, or migration
between diagnostic categories. Each category occupies a sector on a
circle and ribbons connect categories with a width proportional to the
strength of their relationship. Input may be an edge list (from / to /
value) or the two categorical variables whose cross-tabulation defines
the links. Built on the circlize package.

## Usage

``` r
circos(
  data,
  inputMode = "edges",
  fromVar,
  toVar,
  valueVar = NULL,
  directional = TRUE,
  symmetric = FALSE,
  gridPalette = "Set2",
  transparency = 0.5,
  showLabels = TRUE,
  showMatrix = TRUE,
  showExplanation = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- inputMode:

  Whether links are given as an edge list with an optional value column,
  or derived by cross-tabulating two categorical variables.

- fromVar:

  Source category for each link.

- toVar:

  Target category for each link.

- valueVar:

  Link weight (edge-list mode). If omitted, each row counts as 1 and
  identical from/to pairs are summed.

- directional:

  Draw ribbons with directionality (arrows from source to target).

- symmetric:

  Treat the relationship as undirected (combine A-\>B and B-\>A). Useful
  for co-occurrence such as co-mutation.

- gridPalette:

  Colour palette for the category sectors.

- transparency:

  Transparency of the connecting ribbons (0 = opaque, higher = more
  transparent).

- showLabels:

  Label each category sector.

- showMatrix:

  Report the from-to matrix underlying the diagram.

- showExplanation:

  Display an explanation of the chord diagram.

## Value

A results object containing:

|                       |     |     |     |     |          |
|-----------------------|-----|-----|-----|-----|----------|
| `results$todo`        |     |     |     |     | a html   |
| `results$plot`        |     |     |     |     | an image |
| `results$matrixTable` |     |     |     |     | a table  |
| `results$explanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$matrixTable$asDF`

`as.data.frame(results$matrixTable)`

## Examples

``` r
# \donttest{
circos(
    data = mydata,
    fromVar = "referring_site",
    toVar = "treating_site",
    valueVar = "n_patients")
#> Error: object 'mydata' not found
# }
```
