# Calculate Effect Size for T-Test

Calculate Cohen's d and other effect size measures

## Usage

``` r
calculate_effect_size(ttest_result, clinical_context)
```

## Arguments

- ttest_result:

  T-test result object

- clinical_context:

  Clinical research context

## Value

A list with the calculated Cohen's d (`cohens_d`), a qualitative
`magnitude` label, and a `clinical_relevance` assessment string.
