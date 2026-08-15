# Message shown when an output is unavailable in competing-risks mode

[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) does
not reject a 0/1/2 status vector. It emits only a warning and remaps 1
to censored, 2 to event and 0 to `NA`, which jamovi never surfaces. Any
output that cannot handle competing risks must therefore be blocked
explicitly and say why, rather than silently rendering inverted results.

## Usage

``` r
.competingRiskUnavailable(feature)
```

## Arguments

- feature:

  Display name of the output being blocked.

## Value

A character string.
