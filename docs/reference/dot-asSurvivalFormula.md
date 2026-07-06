# Build a survival formula safely via jmvcore::asFormula

Wraps
[`jmvcore::asFormula`](https://rdrr.io/pkg/jmvcore/man/asFormula.html)
with the function allow-list extended to cover common survival modelling
helpers (`Surv`, `strata`, `cluster`, `frailty`, `tt`, `pspline`, `ns`,
`bs`, `I`, `const`, `finegray`). Use this instead of base
[`stats::as.formula()`](https://rdrr.io/r/stats/formula.html) in
survival / Cox / Fine-Gray paths so the formula goes through jmvcore's
allow-listed parser.

## Usage

``` r
.asSurvivalFormula(x)
```

## Arguments

- x:

  A character formula string (e.g. `"survival::Surv(t, d) ~ x"`).

## Value

A parsed formula object.
