# Calculate Confidence Interval for Specificity

Calculate Confidence Interval for Specificity

## Usage

``` r
.specificityCI(tn, fp, conf_level = 0.95, method = "wilson")
```

## Arguments

- tn:

  Number of true negatives

- fp:

  Number of false positives

- conf_level:

  Confidence level (default 0.95)

- method:

  Method ('wilson', 'exact', 'asymptotic')

## Value

List with estimate, lower, and upper bounds
