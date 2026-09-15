# Recursive Partitioning Analysis for Survival

Recursive Partitioning Analysis (RPA) for survival data using CART
methodology. Builds a decision tree to identify optimal cut-points for
risk stratification. Automatically performs cross-validation and tree
pruning. Creates new variable with risk group assignments. Useful for
developing prognostic staging systems by integrating multiple
predictors.

## Usage

``` r
rpasurvival(
  data,
  time = NULL,
  event = NULL,
  predictors = NULL,
  eventValue = "1",
  time_unit = "months",
  minbucket = 20,
  cp = 0.01,
  maxdepth = 3,
  nfolds = 10,
  prunetree = TRUE,
  riskgrouplabels = "auto",
  treeplot = TRUE,
  kmplot = TRUE,
  kmci = FALSE,
  risktable = TRUE,
  pval = TRUE,
  riskgrouptable = TRUE,
  cptable = FALSE,
  variableimportance = TRUE,
  createnewvar = FALSE,
  newvarname = "rpa_stage",
  showSummary = TRUE,
  showInterpretation = FALSE,
  showReport = TRUE
)
```

## Arguments

- data:

  .

- time:

  a (non-negative valued) vector of survival times containing the
  (possibly censored) time to the event or time of last observation

- event:

  the status indicator; normally 0=alive/censored, 1=dead/event. Other
  choices are TRUE/FALSE (TRUE = death/event) or 1/2 (2=death/event)

- predictors:

  variables to use in recursive partitioning analysis for developing
  risk stratification groups

- eventValue:

  the value in the event variable that represents an event
  (death/failure)

- time_unit:

  unit of measurement for survival time. Used for calculating 5-year
  survival estimates in the risk group summary table.

- minbucket:

  the minimum number of observations in any terminal (leaf) node.
  Smaller values create more detailed trees but may overfit.

- cp:

  any split that does not decrease overall lack of fit by a factor of cp
  is not attempted. Smaller values grow larger trees.

- maxdepth:

  maximum depth of any node of the final tree, with the root node
  counted as depth 0. Values greater than 30 are unlikely.

- nfolds:

  number of cross-validation folds for pruning the tree. Use 0 to
  suppress cross-validation.

- prunetree:

  prune the tree using cross-validation to select optimal complexity
  parameter

- riskgrouplabels:

  labeling scheme for terminal nodes (risk groups)

- treeplot:

  .

- kmplot:

  .

- kmci:

  .

- risktable:

  .

- pval:

  .

- riskgrouptable:

  .

- cptable:

  .

- variableimportance:

  .

- createnewvar:

  .

- newvarname:

  name for the new variable containing RPA stage assignments

- showSummary:

  .

- showInterpretation:

  .

- showReport:

  .

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$summary` |  |  |  |  | a html |
| `results$interpretation` |  |  |  |  | a html |
| `results$report` |  |  |  |  | a html |
| `results$treeplot` |  |  |  |  | Decision tree showing recursive partitioning splits for survival risk stratification |
| `results$riskgrouptable` |  |  |  |  | a table |
| `results$kmplot` |  |  |  |  | Kaplan-Meier survival curves stratified by RPA-derived risk groups |
| `results$logranktest` |  |  |  |  | a table |
| `results$cptable` |  |  |  |  | a table |
| `results$varimp` |  |  |  |  | a table |
| `results$coxmodel` |  |  |  |  | a table |
| `results$notices` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$riskgrouptable$asDF`

`as.data.frame(results$riskgrouptable)`
