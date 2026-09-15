# Format Diagnostic Metrics Table for Display

Creates a formatted data frame of diagnostic metrics with confidence
intervals

## Usage

``` r
.formatDiagnosticTable(metrics, ci_method = "wilson", conf_level = 0.95)
```

## Arguments

- metrics:

  List of calculated metrics (from calculate2x2Metrics)

- ci_method:

  Method for confidence intervals ('wilson', 'exact', 'asymptotic')

- conf_level:

  Confidence level (default 0.95)

## Value

Formatted data frame ready for table display
