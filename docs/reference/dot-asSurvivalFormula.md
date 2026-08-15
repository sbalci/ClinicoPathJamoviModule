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
.asSurvivalFormula(x, env = parent.frame())
```

## Arguments

- x:

  A character formula string (e.g. `"survival::Surv(t, d) ~ x"`).

- env:

  Environment to attach to the returned formula. Defaults to the calling
  frame, which is what downstream re-evaluation needs.

## Value

A parsed formula object.

## Details

The returned formula's environment is set to the CALLER's frame.
`asFormula` otherwise leaves it pointing at its own evaluation frame,
which does not hold the caller's `mydata`. Model fitting still works,
because `coxph(fml, data=)` is handed the data directly — but any method
that later re-evaluates the model call does not get it. `cox.zph()` is
the visible casualty: ticking "Proportional hazards assumption" aborted
the entire analysis with "object 'mydata' not found", taking every other
result down with it.
