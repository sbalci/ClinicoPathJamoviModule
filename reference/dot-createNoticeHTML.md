# Cross Table for Clinicopathological Comparisons

This function generates cross tables comparing a dependent variable
(rows) with a grouping variable (columns) and automatically selects
hypothesis tests appropriate for clinical research. The output tables
are rendered in various styles (e.g., arsenal, finalfit, gtsummary,
NEJM, Lancet, hmisc) and are intended for pathologists and oncologists.

This function prepares and cleans data for single-arm survival analysis
by calculating survival time, filtering based on landmark time, and
merging survival outcomes with other factors.

## Usage

``` r
.createNoticeHTML(
  message,
  type = c("ERROR", "STRONG_WARNING", "WARNING", "INFO")
)

.createNoticeHTML(
  message,
  type = c("ERROR", "STRONG_WARNING", "WARNING", "INFO")
)
```

## Arguments

- data:

  A data frame containing the study data.

- vars:

  A set of variables used as the dependent variables (rows).

- group:

  A variable (factor) used as the grouping variable (columns).

- sty:

  A string indicating the desired table style. Options include:
  "arsenal", "finalfit", "gtsummary", "nejm", "lancet", "hmisc".

- excl:

  Logical. If TRUE, rows with missing values will be excluded.

- cont:

  A string ("mean" or "median") to specify the central tendency metric.

- pcat:

  A string ("chisq" or "fisher") to specify the primary test.

## Value

The function produces an HTML table output in the selected style.

A list containing cleaned data and metadata for plotting and analysis.

## Details

The function cleans variable names and applies original labels. It then
builds a formula based on the cleaned data and performs the appropriate
statistical test (e.g. chi-square or Fisher's exact test). Detailed user
guidance is provided via HTML messages.

Currently implemented features:

- Multiple table styles (arsenal, finalfit, gtsummary, NEJM, Lancet,
  hmisc)

- Automatic test selection (chi-square, Fisher's exact, t-test, ANOVA)

- Multiple testing correction (Bonferroni, Holm, BH, BY)

- Variable name safety (handles spaces and special characters)

- Data quality validation warnings

Note: Advanced features including pairwise comparisons, effect size
measures, residual analysis, correspondence analysis, and mosaic plots
are planned but not currently implemented.

## Note

Ensure the input data contains the required variables (elapsed time,
outcome) and meets specified formatting criteria.
