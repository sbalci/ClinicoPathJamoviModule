# Create Tree Analysis Progress Steps

Defines the progress steps for tree analysis

## Usage

``` r
getTreeAnalysisSteps(
  algorithm = "rpart",
  validation = FALSE,
  hyperparameter_tuning = FALSE,
  model_comparison = FALSE
)
```

## Arguments

- algorithm:

  Algorithm being used ("rpart", "fftrees", etc.)

- validation:

  Whether cross-validation is enabled

- hyperparameter_tuning:

  Whether hyperparameter tuning is enabled

- model_comparison:

  Whether model comparison is enabled

## Value

List containing step definitions
