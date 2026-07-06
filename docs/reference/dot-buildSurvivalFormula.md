# Build a survival model formula from variable names

Consolidated helper used across the survival-analysis backends (e.g.
`multisurvival.b.R`) to assemble a `survival::Surv(...) ~ ...` formula
from raw variable names, with safe escaping of non-syntactic names via
[`.escapeVariableNames()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/dot-escapeVariableNames.md)
and safe parsing via
[`.asSurvivalFormula()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/dot-asSurvivalFormula.md).

## Usage

``` r
.buildSurvivalFormula(
  time_var,
  outcome_var,
  predictors,
  survival_type = "standard",
  start_var = NULL,
  stop_var = NULL,
  strata_vars = NULL,
  interaction_terms = NULL
)
```

## Arguments

- time_var:

  Character. Time variable name (or start time for
  `"standard"`/`"interval"` types).

- outcome_var:

  Character. Event/outcome variable name.

- predictors:

  Character vector of predictor (main-effect) variable names.

- survival_type:

  One of `"standard"`, `"counting"`, `"interval"`.

- start_var:

  Character. Start-time variable name (required when
  `survival_type = "counting"`).

- stop_var:

  Character. Stop-time variable name (required when `survival_type` is
  `"counting"` or `"interval"`).

- strata_vars:

  Character vector of stratification variable names.

- interaction_terms:

  Character vector of already-escaped, `:`-joined interaction terms
  (e.g. `` "`Arm`:`Bio`" ``) appended to the right-hand side after main
  effects and before strata.

## Value

A parsed formula object (see
[`.asSurvivalFormula()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/dot-asSurvivalFormula.md)).
