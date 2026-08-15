# Enhanced T-Test with Robust Error Handling

Enhanced t-test implementation with clinical context error handling

## Usage

``` r
enhanced_ttest(
  data,
  dependent,
  group = NULL,
  test_value = 0,
  alternative = "two.sided",
  conf_level = 0.95,
  clinical_context = "general"
)
```

## Arguments

- data:

  Dataset for analysis

- dependent:

  Dependent variable name

- group:

  Grouping variable name (optional for one-sample test)

- test_value:

  Test value for one-sample test (default 0)

- alternative:

  Alternative hypothesis ("two.sided", "less", "greater")

- conf_level:

  Confidence level (default 0.95)

- clinical_context:

  Clinical research context

## Value

An enhanced result object (as produced by `create_enhanced_result`)
containing the analysis success flag, formatted t-test results, clinical
interpretation, and any accumulated warnings or errors.
