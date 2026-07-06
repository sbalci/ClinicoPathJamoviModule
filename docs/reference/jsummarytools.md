# Summary Statistics with summarytools

Comprehensive descriptive statistics using the summarytools package.
Provides professional data frame summaries, frequency tables, and
descriptive statistics with publication-ready output for clinical
research.

## Usage

``` r
jsummarytools(
  data,
  analysis_type = "dfsummary",
  vars,
  group_var = NULL,
  weights_var = NULL,
  cross_var1 = NULL,
  cross_var2 = NULL,
  show_labels = TRUE,
  show_variable_numbers = FALSE,
  show_graphs = TRUE,
  show_valid_counts = TRUE,
  show_na_counts = TRUE,
  round_digits = 2,
  max_distinct_values = 10,
  include_cumulative = FALSE,
  report_missing = TRUE,
  transpose_output = FALSE,
  stats_to_include = "all",
  include_mean = TRUE,
  include_median = TRUE,
  include_mode = FALSE,
  include_sd = TRUE,
  include_var = FALSE,
  include_range = TRUE,
  include_quartiles = TRUE,
  include_skewness = FALSE,
  include_kurtosis = FALSE,
  cross_proportions = "none",
  output_style = "grid",
  plain_ascii = FALSE,
  headings = TRUE,
  escape_pipe = TRUE,
  bootstrap_css = TRUE,
  custom_css = "",
  show_interpretation = TRUE
)
```

## Arguments

- data:

  Dataset for descriptive analysis

- analysis_type:

  Type of summarytools analysis to perform

- vars:

  Variables for analysis (all variables if empty)

- group_var:

  Optional grouping variable for stratified analysis

- weights_var:

  Optional variable containing sample weights

- cross_var1:

  First variable for cross-tabulation

- cross_var2:

  Second variable for cross-tabulation

- show_labels:

  Display variable labels when available

- show_variable_numbers:

  Display variable numbers in output

- show_graphs:

  Include histograms and bar charts in dfSummary

- show_valid_counts:

  Display valid observation counts and proportions

- show_na_counts:

  Display missing data information

- round_digits:

  Number of decimal places for numeric output

- max_distinct_values:

  Maximum number of distinct values to show for categorical variables

- include_cumulative:

  Include cumulative frequencies in frequency tables

- report_missing:

  Include missing values in frequency tables

- transpose_output:

  Transpose descriptive statistics table (variables as rows)

- stats_to_include:

  Which descriptive statistics to include

- include_mean:

  Include mean in descriptive statistics

- include_median:

  Include median in descriptive statistics

- include_mode:

  Include mode in descriptive statistics

- include_sd:

  Include standard deviation in descriptive statistics

- include_var:

  Include variance in descriptive statistics

- include_range:

  Include min, max, and range in descriptive statistics

- include_quartiles:

  Include Q1, Q3, and IQR in descriptive statistics

- include_skewness:

  Include skewness in descriptive statistics

- include_kurtosis:

  Include kurtosis in descriptive statistics

- cross_proportions:

  Type of proportions to calculate in cross-tabulation

- output_style:

  HTML output styling

- plain_ascii:

  Use plain ASCII instead of HTML formatting

- headings:

  Include section headings in output

- escape_pipe:

  Escape pipe characters for markdown compatibility

- bootstrap_css:

  Include Bootstrap CSS styling in HTML output

- custom_css:

  Custom CSS styling for HTML output

- show_interpretation:

  Display interpretation guidance for results

## Value

A results object containing:

|                              |     |     |     |     |         |
|------------------------------|-----|-----|-----|-----|---------|
| `results$instructions`       |     |     |     |     | a html  |
| `results$summary_output`     |     |     |     |     | a html  |
| `results$data_summary_table` |     |     |     |     | a table |
| `results$frequency_table`    |     |     |     |     | a table |
| `results$descriptive_stats`  |     |     |     |     | a table |
| `results$crosstab_output`    |     |     |     |     | a html  |
| `results$interpretation`     |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$data_summary_table$asDF`

`as.data.frame(results$data_summary_table)`
