# Calculate Confidence Interval for Sensitivity

Calculate Confidence Interval for Sensitivity

## Usage

``` r
.sensitivityCI(tp, fn, conf_level = 0.95, method = "wilson")
```

## Arguments

- tp:

  Number of true positives

- fn:

  Number of false negatives

- conf_level:

  Confidence level (default 0.95)

- method:

  Method ('wilson', 'exact', 'asymptotic')

## Value

List with estimate, lower, and upper bounds
