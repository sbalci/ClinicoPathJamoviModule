# Entropy and Mutual Information Analysis

Entropy and Mutual Information analysis quantifies uncertainty in AI
predictions and measures information gain from diagnostic tests or
features.

## Usage

``` r
entropyanalysis(
  data,
  outcome,
  probability_vars,
  predictor_var,
  calculate_entropy = TRUE,
  calculate_conditional_entropy = TRUE,
  calculate_mutual_information = TRUE,
  calculate_kl_divergence = FALSE,
  uncertainty_threshold = 0.5,
  normalize_entropy = TRUE,
  binning_method = "equal_width",
  n_bins = 10,
  show_case_level = FALSE,
  flag_uncertain = TRUE,
  plot_entropy_distribution = TRUE,
  plot_uncertainty_by_class = TRUE,
  plot_mi_heatmap = FALSE,
  random_seed = 42
)
```

## Arguments

- data:

  the data as a data frame

- outcome:

  a string naming the true outcome/class variable

- probability_vars:

  vector of predicted probability variables (one per class)

- predictor_var:

  optional single predictor for mutual information calculation

- calculate_entropy:

  calculate Shannon entropy for each prediction

- calculate_conditional_entropy:

  calculate conditional entropy H(Y\|X)

- calculate_mutual_information:

  calculate mutual information I(X;Y)

- calculate_kl_divergence:

  calculate Kullback-Leibler divergence from uniform distribution

- uncertainty_threshold:

  entropy threshold for flagging high-uncertainty predictions

- normalize_entropy:

  normalize entropy to (0 to 1) scale (divide by log(n_classes))

- binning_method:

  binning method for continuous variables in MI calculation

- n_bins:

  number of bins for discretizing continuous variables

- show_case_level:

  show entropy for each individual case

- flag_uncertain:

  identify cases exceeding uncertainty threshold

- plot_entropy_distribution:

  plot histogram of entropy values

- plot_uncertainty_by_class:

  plot entropy distribution for each true class

- plot_mi_heatmap:

  plot heatmap of pairwise mutual information (for multiple features)

- random_seed:

  random seed for reproducibility

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructionsText`        |     |     |     |     | a html   |
| `results$summaryTable`            |     |     |     |     | a table  |
| `results$entropyTable`            |     |     |     |     | a table  |
| `results$mutualInfoTable`         |     |     |     |     | a table  |
| `results$conditionalEntropyTable` |     |     |     |     | a table  |
| `results$klDivergenceTable`       |     |     |     |     | a table  |
| `results$caseLevelTable`          |     |     |     |     | a table  |
| `results$entropyDistPlot`         |     |     |     |     | an image |
| `results$uncertaintyByClassPlot`  |     |     |     |     | an image |
| `results$interpretationText`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`

## Details

Shannon Entropy quantifies prediction uncertainty (higher = more
uncertain). Mutual Information measures how much knowing one variable
reduces uncertainty about another.

Applications: AI triage systems ("defer to pathologist" decisions),
feature selection, test ordering optimization, multi-class uncertainty
quantification.
