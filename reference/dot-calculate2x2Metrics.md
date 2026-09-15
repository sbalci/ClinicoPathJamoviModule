# Comprehensive 2x2 Diagnostic Metrics Calculator

Calculates all standard diagnostic accuracy metrics from a 2x2 confusion
matrix

## Usage

``` r
.calculate2x2Metrics(tp, fp, fn, tn, prevalence = NULL)
```

## Arguments

- tp:

  Number of true positives

- fp:

  Number of false positives

- fn:

  Number of false negatives

- tn:

  Number of true negatives

- prevalence:

  Optional disease prevalence (for adjusted PPV/NPV)

## Value

List containing all diagnostic metrics
