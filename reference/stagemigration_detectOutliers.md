# Outlier Detection for Survival Times

Identifies potential outliers in survival times using IQR method.

## Usage

``` r
stagemigration_detectOutliers(time_var, method = "iqr", threshold = 3)
```

## Arguments

- time_var:

  Numeric vector of survival times

- method:

  Method for outlier detection ("iqr", "zscore")

- threshold:

  Threshold multiplier (default 3.0 for IQR, 3.0 for z-score)

## Value

List with outlier indices and summary
