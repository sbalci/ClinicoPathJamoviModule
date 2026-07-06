# Automatic Plot Selection

Automatically selects and generates the most appropriate statistical
visualization based on variable data types. Features enhanced error
messages with contextual guidance, robust data validation, and
comprehensive fallback options. Supports both independent and repeated
measures designs with various plot types including violin plots, scatter
plots, bar charts, and alluvial diagrams.

## Usage

``` r
statsplot2(
  data,
  dep,
  group,
  grvar = NULL,
  direction = "independent",
  distribution = "p",
  alluvsty = "t1",
  excl = FALSE,
  sampleLarge = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- dep:

  The dependent variable (y-axis, 1st measurement). Can be continuous or
  categorical.

- group:

  The grouping variable (x-axis, 2nd measurement). Can be continuous or
  categorical.

- grvar:

  Optional grouping variable for creating grouped plots across multiple
  panels.

- direction:

  Measurement design type. "independent" for between-subjects
  comparisons, "repeated" for within-subjects/repeated measures
  comparisons.

- distribution:

  Statistical approach: "p" = parametric, "np" = nonparametric, "r" =
  robust, "bf" = Bayes factor.

- alluvsty:

  Style for alluvial diagrams: "t1" = ggalluvial with stratum labels,
  "t2" = easyalluvial with automatic variable selection.

- excl:

  If TRUE, excludes rows with missing values before analysis.

- sampleLarge:

  If TRUE, automatically samples large datasets (\>10,000 rows) to 5,000
  rows for improved performance.

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`            |     |     |     |     | a preformatted |
| `results$todo`               |     |     |     |     | a html         |
| `results$ExplanationMessage` |     |     |     |     | a html         |
| `results$plot`               |     |     |     |     | an image       |
