# Intraclass Correlation Coefficient

Calculate Intraclass Correlation Coefficient (ICC) for inter-rater
reliability and agreement studies. Provides multiple ICC types with
confidence intervals and APA-style formatting for clinical research
applications.

## Usage

``` r
icccoeff(
  data,
  vars,
  icc_type = "icc2_1",
  agreement_type = "consistency",
  confidence_level = "0.95",
  missing_values = "complete",
  show_apa_format = TRUE,
  show_interpretation = TRUE,
  show_confidence_intervals = TRUE,
  show_f_test = TRUE,
  show_descriptive_stats = TRUE,
  decimal_places = 3,
  alpha_level = 0.05
)
```

## Arguments

- data:

  Dataset containing ratings from multiple raters

- vars:

  Variables representing ratings from different raters (subjects as
  rows, raters as columns)

- icc_type:

  Type of ICC calculation based on study design

- agreement_type:

  Whether to assess consistency or absolute agreement

- confidence_level:

  Confidence level for ICC confidence intervals

- missing_values:

  How to handle missing data

- show_apa_format:

  Display results in APA style format

- show_interpretation:

  Include interpretation guide for ICC values

- show_confidence_intervals:

  Display confidence intervals for ICC estimates

- show_f_test:

  Include F-test for significance of ICC

- show_descriptive_stats:

  Display descriptive statistics for each rater

- decimal_places:

  Number of decimal places in output

- alpha_level:

  Alpha level for statistical tests

## Value

A results object containing:

|                                  |     |     |     |     |         |
|----------------------------------|-----|-----|-----|-----|---------|
| `results$instructions`           |     |     |     |     | a html  |
| `results$icc_results`            |     |     |     |     | a table |
| `results$apa_format`             |     |     |     |     | a html  |
| `results$descriptive_stats`      |     |     |     |     | a table |
| `results$reliability_assessment` |     |     |     |     | a table |
| `results$interpretation`         |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$icc_results$asDF`

`as.data.frame(results$icc_results)`
