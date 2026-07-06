# Contingency Tables

The X² test of association (not to be confused with the X² goodness of
fit) is used to test whether two categorical variables are independent
or associated. If the p-value is low, it suggests the variables are not
independent, and that there is a relationship between the two variables.

## Usage

``` r
contTables(
  data,
  rows,
  cols,
  counts = NULL,
  layers = NULL,
  chiSq = TRUE,
  chiSqCorr = FALSE,
  likeRat = FALSE,
  fisher = FALSE,
  contCoef = FALSE,
  phiCra = FALSE,
  logOdds = FALSE,
  odds = FALSE,
  relRisk = FALSE,
  riskDiff = FALSE,
  nnt = FALSE,
  ci = FALSE,
  ciWidth = 95,
  gamma = FALSE,
  taub = FALSE,
  trendTest = FALSE,
  trendDirection = "twosided",
  obs = TRUE,
  exp = FALSE,
  pcRow = FALSE,
  pcCol = FALSE,
  pcTot = FALSE,
  formula
)
```

## Arguments

- data:

  the data as a data frame

- rows:

  the variable to use as the rows in the contingency table (not
  necessary when providing a formula, see the examples)

- cols:

  the variable to use as the columns in the contingency table (not
  necessary when providing a formula, see the examples)

- counts:

  the variable to use as the counts in the contingency table (not
  necessary when providing a formula, see the examples)

- layers:

  the variables to use to split the contingency table (not necessary
  when providing a formula, see the examples)

- chiSq:

  `TRUE` (default) or `FALSE`, provide X²

- chiSqCorr:

  `TRUE` or `FALSE` (default), provide X² with continuity correction

- likeRat:

  `TRUE` or `FALSE` (default), provide the likelihood ratio

- fisher:

  `TRUE` or `FALSE` (default), provide Fisher's exact test

- contCoef:

  `TRUE` or `FALSE` (default), provide the contingency coefficient

- phiCra:

  `TRUE` or `FALSE` (default), provide Phi and Cramer's V

- logOdds:

  `TRUE` or `FALSE` (default), provide the log odds ratio (only
  available for 2x2 tables)

- odds:

  `TRUE` or `FALSE` (default), provide the odds ratio (only available
  for 2x2 tables)

- relRisk:

  `TRUE` or `FALSE` (default), provide the relative risk (only available
  for 2x2 tables)

- riskDiff:

  `TRUE` or `FALSE` (default), provide the risk difference (only
  available for 2x2 tables)

- nnt:

  `TRUE` or `FALSE` (default), provide the number needed to treat (only
  available for 2x2 tables)

- ci:

  `TRUE` or `FALSE` (default), provide confidence intervals for the
  comparative measures

- ciWidth:

  a number between 50 and 99.9 (default: 95), width of the confidence
  intervals to provide

- gamma:

  `TRUE` or `FALSE` (default), provide gamma

- taub:

  `TRUE` or `FALSE` (default), provide Kendall's tau-b

- trendTest:

  `TRUE` or `FALSE` (default), provide Cochran-Armitage test for trend
  for ordered categorical variables

- trendDirection:

  Direction of trend test: two-sided (default), increasing, or
  decreasing

- obs:

  `TRUE` or `FALSE` (default), provide the observed counts

- exp:

  `TRUE` or `FALSE` (default), provide the expected counts

- pcRow:

  `TRUE` or `FALSE` (default), provide row percentages

- pcCol:

  `TRUE` or `FALSE` (default), provide column percentages

- pcTot:

  `TRUE` or `FALSE` (default), provide total percentages

- formula:

  (optional) the formula to use, see the examples

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$freqs` |  |  |  |  | a table of proportions |
| `results$chiSq` |  |  |  |  | a table of X² test results |
| `results$odds` |  |  |  |  | a table of comparative measures |
| `results$nom` |  |  |  |  | a table of the 'nominal' test results |
| `results$gamma` |  |  |  |  | a table of the gamma test results |
| `results$taub` |  |  |  |  | a table of the Kendall's tau-b test results |
| `results$trendTest` |  |  |  |  | Cochran-Armitage trend test results |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$freqs$asDF`

`as.data.frame(results$freqs)`
