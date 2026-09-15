# Cross Tables

Function for making Cross Tables.

## Usage

``` r
crosstable2(
  data,
  vars,
  group,
  sty = "nejm",
  excl = FALSE,
  cont = "mean",
  pcat = "chisq"
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  The variable(s) that will appear as rows in the cross table.

- group:

  The variable that will appear as columns (groups) in the table.

- sty:

  .

- excl:

  Exclude rows with missing values.

- cont:

  .

- pcat:

  .

## Value

A results object containing:

|                       |     |     |     |     |                |
|-----------------------|-----|-----|-----|-----|----------------|
| `results$subtitle`    |     |     |     |     | a preformatted |
| `results$todo`        |     |     |     |     | a html         |
| `results$todo2`       |     |     |     |     | a html         |
| `results$tablestyle1` |     |     |     |     | a html         |
| `results$tablestyle2` |     |     |     |     | a html         |
| `results$tablestyle3` |     |     |     |     | a html         |
| `results$tablestyle4` |     |     |     |     | a html         |
