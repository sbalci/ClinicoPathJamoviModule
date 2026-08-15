# Lasso-Cox Regression for Variable Selection in Survival Analysis

Performs Lasso-penalized Cox proportional hazards regression for
variable selection in survival analysis. This function uses penalized
likelihood to identify the most important predictors while preventing
overfitting, making it ideal for high-dimensional survival data where
the number of potential predictors may approach or exceed the sample
size.

## Value

An `R6` class generator object for the `lassocoxClass` backend; used
internally by the jamovi analysis wrapper and not called directly.

## Details

The Lasso-Cox regression combines the Cox proportional hazards model
with L1 regularization (Lasso penalty) to perform automatic variable
selection. The method minimizes the partial likelihood penalized by the
L1 norm of the coefficient vector, effectively shrinking less important
coefficients toward zero and setting some exactly to zero.

Key features:

- Automatic variable selection through L1 regularization

- Cross-validation for optimal tuning parameter selection

- Risk score calculation and stratification

- Comprehensive model performance evaluation

- Survival curve visualization by risk groups

The function uses the glmnet package for efficient computation and
supports both lambda.min (minimum cross-validation error) and lambda.1se
(1 standard error rule) for tuning parameter selection.

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`lassocoxBase` -\> `lassocoxClass`

## Methods

### Public methods

- [`lassocoxClass$clone()`](#method-lassocoxClass-clone)

Inherited methods

- [`jmvcore::Analysis$.createImage()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImage)
- [`jmvcore::Analysis$.createImages()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImages)
- [`jmvcore::Analysis$.createPlotObject()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createPlotObject)
- [`jmvcore::Analysis$.getSessionTemp()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.getSessionTemp)
- [`jmvcore::Analysis$.load()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.load)
- [`jmvcore::Analysis$.render()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.render)
- [`jmvcore::Analysis$.save()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.save)
- [`jmvcore::Analysis$.savePart()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.savePart)
- [`jmvcore::Analysis$.setCheckpoint()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setCheckpoint)
- [`jmvcore::Analysis$.setParent()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setParent)
- [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetHeaderSource)
- [`jmvcore::Analysis$.setReadDatasetSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetSource)
- [`jmvcore::Analysis$.setResourcesPathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setResourcesPathSource)
- [`jmvcore::Analysis$.setStatePathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setStatePathSource)
- [`jmvcore::Analysis$addAddon()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-addAddon)
- [`jmvcore::Analysis$asProtoBuf()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asProtoBuf)
- [`jmvcore::Analysis$asSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asSource)
- [`jmvcore::Analysis$check()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-check)
- [`jmvcore::Analysis$init()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-init)
- [`jmvcore::Analysis$optionsChangedHandler()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-optionsChangedHandler)
- [`jmvcore::Analysis$postInit()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-postInit)
- [`jmvcore::Analysis$print()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-print)
- [`jmvcore::Analysis$readDataset()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-readDataset)
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `lassocoxBase$initialize()`

------------------------------------------------------------------------

### `lassocoxClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    lassocoxClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic Lasso-Cox regression
result <- lassocox(
  data = survival_data,
  elapsedtime = "time",
  outcome = "status", 
  outcomeLevel = "1",
  explanatory = c("age", "gender", "stage", "grade"),
  lambda = "lambda.1se",
  nfolds = 10
)

# High-dimensional scenario
result <- lassocox(
  data = genomic_data,
  elapsedtime = "survival_time",
  outcome = "event",
  outcomeLevel = "death",
  explanatory = gene_variables,
  lambda = "lambda.min",
  nfolds = 5,
  standardize = TRUE
)
} # }
```
