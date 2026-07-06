# Pathology Composition Analysis

Semi-quantitative analysis of histologic components and their
association with clinical outcomes. Based on advanced pathology research
methodologies, particularly gastric cancer composition analysis.
Performs component risk assessment, composition pattern analysis, and
optimal threshold identification.

## Usage

``` r
pathologycomposition(
  data,
  outcome_variable,
  component1,
  component2,
  component3,
  component4,
  composition_analysis = TRUE,
  optimal_composition = TRUE,
  trend_test = TRUE,
  confidence_level = 0.95,
  low_risk_threshold = 0.05,
  high_risk_threshold = 0.2,
  min_group_size = 10,
  quantitative_categories = "gastric_cancer",
  composition_plot = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- outcome_variable:

  Clinical outcome variable for composition analysis

- component1:

  First histologic component (proportion or category)

- component2:

  Second histologic component (proportion or category)

- component3:

  Third histologic component (proportion or category)

- component4:

  Fourth histologic component (proportion or category)

- composition_analysis:

  Analyze risk based on component composition patterns

- optimal_composition:

  Identify optimal low-risk and high-risk composition patterns

- trend_test:

  Perform trend tests for dose-response relationships

- confidence_level:

  Confidence level for risk estimates and intervals

- low_risk_threshold:

  Maximum risk probability for low-risk classification (5 percent =
  0.05)

- high_risk_threshold:

  Minimum risk probability for high-risk classification (20 percent =
  0.20)

- min_group_size:

  Minimum number of cases required for composition pattern analysis

- quantitative_categories:

  Semi-quantitative categorization system for components

- composition_plot:

  Generate scatter plot of component composition vs risk

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`        |     |     |     |     | a html   |
| `results$componentanalysis`   |     |     |     |     | a table  |
| `results$compositionrisk`     |     |     |     |     | a table  |
| `results$optimalcompositions` |     |     |     |     | a table  |
| `results$compositionplot`     |     |     |     |     | an image |
| `results$interpretation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$componentanalysis$asDF`

`as.data.frame(results$componentanalysis)`
